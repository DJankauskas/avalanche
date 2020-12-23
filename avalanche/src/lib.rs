///A trait providing platform-specific rendering code.
pub mod renderer;
///A reference-counted interior-mutable type designed to reduce runtime borrow rule violations.
pub mod shared;
///An in-memory representation of the current component tree.
pub mod vdom;

pub mod tree;

pub use avalanche_macro::component;
use downcast_rs::{impl_downcast, Downcast};
use renderer::NativeType;
use shared::Shared;
use std::{any::Any, rc::Rc};
use tree::NodeId;
use vdom::{update_vnode, VDom, VNode};

#[macro_export]
macro_rules! reactive_assert {
    ( $( $($dept:ident),+ => $rec:ident );* ) => {};
}

///For internal use only, does not obey semver and is unsupported
///A hack used to work around a seeming syn limitation
///syn will interpret some macro calls as items rather than expressions
///syn expects an item to be replaced by another, so when parsing Component! type macros,
///`#[reactive]` will replace that call with an expression within this macro
///to syn, this is replacing one macro item with another
#[doc(hidden)]
#[macro_export]
macro_rules! __internal_identity {
    ($e:expr) => {
        $e
    };
}

///A reference-counted type that holds an instance of a component.
///Component functions must return a view.
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

///The trait representing a component. Except for renderer libraries,
///users should not implement this trait manually but instead use the `component` attribute.
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
    //TODO: make ! when never stabilizes
    type Builder = ();

    fn render(&self, _: InternalContext) -> View {
        unreachable!()
    }
    fn updated(&self) -> bool {
        false
    }
}

///An internal trait implemented for all `Component`s. This should not be
///implemented manually.
pub trait DynComponent: Downcast + 'static {
    #[doc(hidden)]
    fn init_state(&self) -> Box<dyn Any>;

    #[doc(hidden)]
    fn render(&self, context: InternalContext) -> View;

    #[doc(hidden)]
    fn native_type(&self) -> Option<NativeType>;

    #[doc(hidden)]
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
///Provided as an argument to the [`Component::render`] method
///to provide hooks access to component state and other data.
pub struct InternalContext<'a> {
    pub state: &'a mut Box<dyn Any>,
    pub component_pos: ComponentPos,
}

#[doc(hidden)]
#[derive(Clone)]
///Internal data structure that stores what tree a component
///belongs to, and its position within it
pub struct ComponentPos {
    ///Shared value ONLY for passing to UseState
    ///within the render function this value is mutably borrowed,
    ///so exec and exec_mut will panic
    pub vnode: NodeId<VNode>,
    ///shared container to the VDom of which
    pub vdom: Shared<VDom>,
}

///A hook that allows a component to keep persistent state across renders.
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
    ///This function is called by the framework. Users gain access to a reference to the
    ///state type and a setter when they call it.
    pub fn hook<'a>(
        &'a mut self,
        component_pos: ComponentPos,
        get_self: fn(&mut Box<dyn Any>) -> &mut UseState<T>,
    ) -> (
        impl FnOnce(T) -> (&'a T, UseStateSetter<T>),
        UseStateUpdates,
    ) {
        let updates = UseStateUpdates {
            update: self.updated,
        };
        let closure = move |val| {
            if self.updated {
                self.updated = false;
            }
            if let None = self.state {
                self.state = Some(val);
                self.updated = true;
            };
            let state_ref = self.state.as_ref().unwrap();
            let setter = UseStateSetter::new(component_pos, get_self);
            (state_ref, setter)
        };
        (closure, updates)
    }
}

///Provides a setter for a piece of state managed by `UseState<T>`.
pub struct UseStateSetter<T: 'static> {
    component_pos: ComponentPos,
    get_mut: fn(&mut Box<dyn Any>) -> &mut UseState<T>,
}

impl<T: 'static> Clone for UseStateSetter<T> {
    fn clone(&self) -> Self {
        Self {
            component_pos: self.component_pos.clone(),
            get_mut: self.get_mut
        }
    }
}

impl<T: 'static> UseStateSetter<T> {
    fn new(
        component_pos: ComponentPos,
        get_mut: fn(&mut Box<dyn Any>) -> &mut UseState<T>,
    ) -> Self {
        Self {
            component_pos,
            get_mut,
        }
    }

    ///Takes a function that modifies its attached state and triggers a rerender of its component.
    ///The update is not performed immediately; its effect will only be accessible
    ///on its component's rerender.
    pub fn call<F: FnOnce(&mut T)>(&self, f: F) {
        let get_mut = self.get_mut;
        let vdom_clone = self.component_pos.vdom.clone();
        let vdom_clone_2 = vdom_clone.clone();
        let vnode_copy = self.component_pos.vnode;

        self.component_pos.vdom.exec_mut(move |vdom| {
            let vnode = vnode_copy.get_mut(&mut vdom.tree);
            let mut_ref_to_state = get_mut(vnode.state.as_mut().unwrap());
            f(mut_ref_to_state.state.as_mut().unwrap());
            mut_ref_to_state.updated = true;

            vdom.renderer.schedule_on_ui_thread(Box::new(move || {
                vdom_clone.exec_mut(|vdom| {
                    vnode_copy.get_mut(&mut vdom.tree).dirty = true;
                    update_vnode(
                        None, 
                        vnode_copy, 
                        &mut vdom.tree, 
                        &mut vdom.renderer, 
                        vdom_clone_2
                    );
                })
            }));
        });
        // let vnode_clone = self.vnode.clone();
        // let vdom_clone = self.vnode.exec(|vnode| vnode.vdom.clone());
        // let get_mut = self.get_mut;

        // self.vnode.exec_mut(move |vnode| {
        //     let mut_ref_to_state = get_mut(vnode.state.as_mut().unwrap());
        //     f(mut_ref_to_state.state.as_mut().unwrap());
        //     mut_ref_to_state.updated = true;

        //     vnode.vdom.exec_mut(|vdom| {
        //         vdom.renderer.schedule_on_ui_thread(
        //             Box::new(move || {
        //                 vnode_clone.exec_mut(|vnode| vnode.dirty = true);
        //                 vdom_clone.exec_mut(|vdom| {
        //                     update_vnode(vnode_clone, None, &mut vdom.renderer);
        //                 });
        //             })
        //         )
        //     });
        // });
    }
}

#[doc(hidden)]
pub struct UseStateUpdates {
    update: bool,
}

impl UseStateUpdates {
    #[doc(hidden)]
    ///Used to get the status of a portion of returned state
    ///Usage: if the return type is an array or tuple, passing a &[num] will
    ///yield whether that element has been updated. If that element is also
    //a tuple or array, this logic applies recursively.
    /// TODO: actually use this model!
    /// Or replace it with something else!
    /// For now, each element corresponds to an index within
    pub fn index(&self, idx: &[usize]) -> bool {
        // match idx {
        //     //corresponds to &'a T in hook()
        //     [0, ..] => self.update,
        //     //corresponds to UseStateSetter
        //     //this never meaningfully changes
        //     [1, ..] => false,
        //     _ => self.update,
        // }
        for elem in idx {
            if *elem == 0 && self.update {
                return true;
            }
        }
        false
    }
}
