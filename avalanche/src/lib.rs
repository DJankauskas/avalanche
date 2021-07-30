/// A trait providing platform-specific rendering code.
pub mod renderer;
/// A reference-counted interior-mutable type designed to reduce runtime borrow rule violations.
pub mod shared;
/// A `Vec`-backed implementation of a tree, with a relatively friendly mutation api.
pub(crate) mod tree;
/// An in-memory representation of the current component tree.
pub mod vdom;
pub mod tracked;

use downcast_rs::{impl_downcast, Downcast};
use std::{any::Any, rc::Rc};

use renderer::{NativeType, Scheduler};
use shared::Shared;
use tree::NodeId;
use vdom::{update_vnode, VDom, VNode};

pub use tracked::Tracked;
pub use tracked::UseVec;

/// An attribute macro used to define [Component](Component)s.
///
/// # Basic usage
/// The macro must be applied to a function that returns a [View](View).
/// It generates a `struct` implementing [Component](Component) with the name of the input function.
/// Thus, the function's name should start with a capital ASCII character to comply with
/// Rust type name conventions.
///
/// The function can optionally take parameters; users of the component will then be required to provide them,
/// as described below. Parameter types must be `'static`: that is, they cannot contain non-`'static` references.
/// The function body then receives each parameter as a reference, in order to avoid moving params
/// and allow components to be rendered multiple times. Parameters must have concrete types: they cannot use the `impl Trait`
/// syntax. Components cannot be `async` or generic.
///
/// A component must return a [View] describing what it will render.
/// A [View] contains an instance of a [Component]. The simplest possible [Component] to return is the `()` type:
/// ```rust
/// use avalanche::{component, View};
///
/// #[component]
/// pub fn Void() -> View {
///     ().into()
/// }
/// ```
/// The `()` type signifies that there is nothing to render. While this is sometimes useful, usually you will want
/// to render more complex components. Components are invoked with the same syntax as `struct` init expressions, except with
/// the macro `!` after the type name:
/// ```rust
/// use avalanche::{component, tracked, View};
/// use avalanche_web::components::{H1, Text};
///
/// const class: &str = "hello-world";
///
/// #[component]
/// pub fn HelloWorld(name: String) -> View {
///     H1!(
///         id: class,
///         class: class,
///         child: Text!(text: format!("Hi there, {}!", tracked!(name)))
///     )
/// }
/// ```
/// `id`, `class`, and `child` are all parameters of the `H1` component provided by the `avalanche_web` crate.
/// In order to create an instance of `H1`, we provide it its parameters as struct fields; this allows us to
/// provide the `class` parameter concisely instead of typing out `class: class`.
///
/// In the case of `H1`, all of its parameters are optional and have default values, but for components generated by this
/// macro, all parameters must be provided, or component instantiation will panic at runtime. In the future, this will
/// instead be a compile-time error, and specifying default values will be allowed.
///
/// Note that all macro invocations beginning with a capital ASCII character will be interpreted as component invocations
/// within the function. If you need to invoke a macro beginning with a capital letter, consider using it with an alias
/// beginning with `_`.
///
/// # Hooks
/// Pure components (those whose output only depends on their inputs) can be useful, but oftentimes you'll want
/// components with state to enable more complex behaviors. Hooks are composeable abstractions that enable you
/// to introduce state, side effects, and reusable functionality in your components.
///
/// Hooks are specified within the component attribute like this:
/// `#[component(name1 = HookType1, name2 = HookType2<u8>)]`
/// This injects two values, usually functions, with names `name1` and `name2` into your component's render code.
///
/// Unlike in some other frameworks, hook values can be used in any order and within any language construct,
/// although hooks often implement [`FnOnce`](std::ops::FnOnce) and thus can only be called once. A very commonly used
/// hook is [UseState<T>](UseState), as it allows injecting state into your component; check the linked docs for more details.
/// Custom hooks, defined by the `hook` attribute macro, will be introduced in a future version.
///
/// # Updates and dependency tracking
/// On each rerender, `avalanche` calculates whether each parameter of each component has been updated.
/// In order to enable this, the `component` attribute macro analyzes the dependency flow of parameters within UI code.
/// In each instance where a given `Component` is created with the syntax `Component !`, avalanche calculates which hooks
/// and parent parameter values contribute to the value of each child parameter. This enables efficienct updates,
/// but has a few caveats that creates two major rules to follow:
///
/// ## Avoid side effects and interior mutability
/// `avalanche` considers a parameter updated if one of the parameters or hooks that influence it change, but
/// a function like [rand::thread_rng](https://docs.rs/rand/0.8/rand/fn.thread_rng.html) has a different value on every call
/// despite having no parameter or hook dependencies.
/// Using values from functions and methods that are not pure or have interior mutability will lead to missed updates.
///
/// ## Eschew third-party macros
/// Unfortunately, macros have custom syntaxes and `component` cannot calculate the dependencies of most of them.
/// All `std` macros (like [vec!](std::vec!()) and [format!](std::format!())) and `avalanche` macros (like [enclose!()])
/// work well, but any others may lead to parameters being incorrectly marked as not updated.
#[doc(inline)]
pub use avalanche_macro::component;

/// Takes a list of identifiers terminated by a semicolon and expression. Each identifier is
/// cloned, and made available to the expression. The macro evaluates to that expression.
/// This is useful for passing things like state and setters to multiple component props.
/// # Example
/// ```rust
/// # use avalanche::enclose;
/// let message = "Enclose me!".to_owned();
/// let closure1 = enclose!(message; move || println!("{}", message));
/// let closure2 = enclose!(message; move || eprintln!("{}", message));
/// ```
// Note: code derived from stdweb
// TODO: appropriately license project and/or code snippet as MIT or Apache license
#[macro_export]
macro_rules! enclose {
    ( $( $x:ident ),*; $y:expr ) => {
        {
            $(let $x = $x.clone();)*
            $y
        }
    };
}

/// For internal use only, does not obey semver and is unsupported
/// A hack used to work around a seeming syn limitation
/// syn will interpret some macro calls as items rather than expressions
/// syn expects an item to be replaced by another, so when parsing Component! type macros,
/// `#[component]` will replace that call with an expression within this macro
/// to syn, this is replacing one macro item with another
#[doc(hidden)]
#[macro_export]
macro_rules! __internal_identity {
    ($e:expr) => {
        $e
    };
}

/// A reference-counted type that holds an instance of a component.
/// Component functions must return a [`View`].
#[derive(Clone)]
pub struct View {
    rc: Rc<dyn DynComponent>,
}

impl View {
    fn new<T: DynComponent>(val: T) -> Self {
        Self { rc: Rc::new(val) }
    }
}

impl std::ops::Deref for View {
    type Target = dyn DynComponent;

    fn deref(&self) -> &dyn DynComponent {
        &*self.rc
    }
}

impl<T: DynComponent> From<T> for View {
    fn from(val: T) -> Self {
        Self::new(val)
    }
}

impl<T: DynComponent> From<Option<T>> for View {
    fn from(val: Option<T>) -> Self {
        match val {
            Some(val) => return View::new(val),
            None => return View::new(()),
        }
    }
}

impl From<Option<View>> for View {
    fn from(val: Option<View>) -> Self {
        match val {
            Some(val) => return val,
            None => return View::new(()),
        }
    }
}

/// The trait representing a component. Except for renderer libraries,
/// users should not implement this trait manually but instead use the `component` attribute.
pub trait Component: 'static {
    type Builder;

    fn init_state(&self) -> Box<dyn Any> {
        Box::new(())
    }

    fn render(&self, context: InternalContext) -> View;

    fn updated(&self) -> bool;

    fn native_type(&self) -> Option<NativeType> {
        None
    }

    fn location(&self) -> Option<(u32, u32)> {
        None
    }

    fn key(&self) -> Option<&str> {
        None
    }
}

impl Component for () {
    // TODO: make ! when never stabilizes
    type Builder = ();

    fn render(&self, _: InternalContext) -> View {
        unreachable!()
    }
    fn updated(&self) -> bool {
        false
    }
}

/// An internal trait implemented for all [`Component`]s. This should not be
/// implemented manually.
#[doc(hidden)]
pub trait DynComponent: Downcast + 'static {
    fn init_state(&self) -> Box<dyn Any>;

    fn render(&self, context: InternalContext) -> View;

    fn native_type(&self) -> Option<NativeType>;

    fn updated(&self) -> bool;

    fn location(&self) -> Option<(u32, u32)>;

    fn key(&self) -> Option<&str>;
}

impl_downcast!(DynComponent);

impl<T: Component> DynComponent for T {
    fn init_state(&self) -> Box<dyn Any> {
        Component::init_state(self)
    }

    fn render(&self, context: InternalContext) -> View {
        Component::render(self, context)
    }

    fn native_type(&self) -> Option<NativeType> {
        Component::native_type(self)
    }

    fn updated(&self) -> bool {
        Component::updated(self)
    }

    fn location(&self) -> Option<(u32, u32)> {
        Component::location(self)
    }

    fn key(&self) -> Option<&str> {
        Component::key(self)
    }
}

#[doc(hidden)]
/// Provided as an argument to the [`Component::render`] method
/// to provide hooks access to component state and other data.
pub struct InternalContext<'a> {
    pub state: &'a mut Box<dyn Any>,
    pub component_pos: ComponentPos<'a>,
    pub scheduler: &'a Shared<dyn Scheduler>,
}

#[doc(hidden)]
#[derive(Clone)]
pub struct ComponentNodeId {
    pub(crate) id: NodeId<VNode>
}

impl From<NodeId<VNode>> for ComponentNodeId {
    fn from(node: NodeId<VNode>) -> Self {
        Self {
            id: node
        }
    }
}

#[doc(hidden)]
#[derive(Clone)]
/// Internal data structure that stores what tree a component
/// belongs to, and its position within it
pub struct ComponentPos<'a> {
    /// Shared value ONLY for passing to UseState
    /// within the render function this value is mutably borrowed,
    /// so exec and exec_mut will panic
    pub node_id: ComponentNodeId,
    /// Shared container to the VDom of which the [`vnode`] is a part.
    pub vdom: &'a Shared<VDom>,
}

/// A hook that allows a component to keep persistent state across renders.
///
/// [UseState<T>](UseState) takes a type parameter specifying the type of the state variable the hook manages.
/// This hook injects a function `impl FnOnce(T) -> (&T, UseStateSetter)` into a component.
/// To use it, call the function with your desired default value for `T`. If the function has not been called before,
/// then the state will be initialized to this value. The return value contains a reference to the current state,
/// and the setter [UseStateSetter<T>](UseStateSetter). `&T`'s lifetime is only valid within the component's render
/// function, but [UseStateSetter<T>](UseStateSetter) may be freely moved and cloned.
///
/// To update the state, use the [set](UseStateSetter::set) or [update](UseStateSetter::update) methods on the setter variable.
///
/// # Example
/// ```rust
/// use avalanche::{component, tracked, View, UseState};
/// use avalanche_web::components::{Div, H2, Button, Text};
///
/// #[component(count = UseState<u64>)]
/// fn Counter() -> View {
///     let (count, set_count) = count(0);
///     Div!(
///         children: [
///             H2!(
///                 child: Text!("Counter!"),
///             ),
///             Button!(
///                 on_click: move |_| set_count.update(|count| *count += 1),
///                 child: Text!("+")
///             ),
///             Text!(tracked!(count))
///         ]
///     )
/// }
/// ```
/// __Adapted from the `avalanche_web`
/// [counter example.](https://github.com/DJankauskas/avalanche/blob/38ec4ccb83f93550c7d444351fa395708505d053/avalanche-web/examples/counter/src/lib.rs)__
pub struct UseState<T: 'static> {
    state: Option<T>,
    updated: bool,
}

impl<T> Default for UseState<T> {
    fn default() -> Self {
        Self {
            state: None,
            updated: false,
        }
    }
}

impl<T> UseState<T> {
    /// This function is called by the framework. Users gain access to a reference to the
    /// state type and a setter when they call it.
    #[doc(hidden)]
    pub fn hook<'a>(
        &'a mut self,
        component_pos: ComponentPos<'a>,
        scheduler: &Shared<dyn Scheduler>,
        get_self: fn(&mut Box<dyn Any>) -> &mut UseState<T>,
    ) -> impl FnOnce(T) -> (Tracked<&'a T>, UseStateSetter<T>) {
        let scheduler = scheduler.clone();
        let closure = move |val| {
            let updated = self.updated;
            if self.updated {
                self.updated = false;
            }
            if let None = self.state {
                self.state = Some(val);
                self.updated = true;
            };
            let state_ref = Tracked::new(self.state.as_ref().unwrap(), updated);
            let setter = UseStateSetter::new(component_pos, scheduler, Rc::new(get_self));
            (state_ref, setter)
        };
        closure
    }
}

/// Provides a setter for a piece of state managed by [UseState<T>](UseState).
pub struct UseStateSetter<T: 'static> {
    vdom: Shared<VDom>,
    vnode: NodeId<VNode>,
    scheduler: Shared<dyn Scheduler>,
    get_mut: Rc<dyn Fn(&mut Box<dyn Any>) -> &mut UseState<T>>,
}

impl<T: 'static> Clone for UseStateSetter<T> {
    fn clone(&self) -> Self {
        Self {
            vdom: self.vdom.clone(),
            vnode: self.vnode,
            scheduler: self.scheduler.clone(),
            get_mut: self.get_mut.clone(),
        }
    }
}

impl<T: 'static> UseStateSetter<T> {
    fn new(
        component_pos: ComponentPos,
        scheduler: Shared<dyn Scheduler>,
        get_mut: Rc<dyn Fn(&mut Box<dyn Any>) -> &mut UseState<T>>,
    ) -> Self {
        Self {
            vdom: component_pos.vdom.clone(),
            vnode: component_pos.node_id.id,
            scheduler,
            get_mut,
        }
    }

    /// Takes a function that modifies the state associated with [UseStateSetter] and
    /// triggers a rerender of its associated component.
    ///
    /// The update is not performed immediately; its effect will only be accessible
    /// on its component's rerender. Note that `update` always triggers a rerender, and the state value
    /// is marked as updated, even if the given function performs no mutations.
    pub fn update<F: FnOnce(&mut T) + 'static>(&self, f: F) {
        let get_mut = self.get_mut.clone();
        let vdom_clone = self.vdom.clone();
        let vdom_clone_2 = vdom_clone.clone();
        let vnode_copy = self.vnode;
        let scheduler_clone = self.scheduler.clone();

        self.scheduler.exec_mut(move |scheduler| {
            scheduler.schedule_on_ui_thread(Box::new(move || {
                vdom_clone.exec_mut(|vdom| {
                    let vnode = vnode_copy.get_mut(&mut vdom.tree);
                    vnode.dirty = true;
                    let mut_ref_to_state = get_mut(vnode.state.as_mut().unwrap());
                    f(mut_ref_to_state.state.as_mut().unwrap());
                    mut_ref_to_state.updated = true;
                    update_vnode(
                        None,
                        vnode_copy,
                        &mut vdom.tree,
                        &mut vdom.renderer,
                        &vdom_clone_2,
                        &scheduler_clone,
                    );
                })
            }));
        });
    }

    /// Sets the state to the given value.
    ///
    /// The update is not performed immediately; its effect will only be accessible
    /// on its component's rerender. Note that `set` always triggers a rerender, and the state value
    /// is marked as updated, even if the new state is equal to the old.
    pub fn set(&self, val: T) {
        self.update(move |state| *state = val);
    }
}
