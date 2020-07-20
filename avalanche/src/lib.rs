#[cfg(test)]
#[allow(unused_variables)]
#[allow(dead_code)]
#[allow(unused_assignments)]
mod tests;

///A reference-counted interior-mutable type designed to reduce runtime borrow rule violations.
pub mod shared;
///An in-memory representation of the current component tree.
pub mod vdom;
///A trait providing platform-specific rendering code.
pub mod renderer;

use std::{any::Any, rc::Rc};
use downcast_rs::{Downcast, impl_downcast};
pub use avalanche_macro::component;
use renderer::NativeType;
use vdom::{VNode, update_vnode};
use shared::Shared;

#[macro_export]
macro_rules! reactive_assert {
    ( $( $($dept:ident),+ => $rec:ident );* ) => {}
}

///For internal use only, does not obey semver and is unsupported
///A hack used to work around a seeming syn limitation
///syn will interpret some macro calls as items rather than expressions
///syn expects an item to be replaced by another, so when parsing Component! type macros,
///#[reactive] will replace that call with an expression within this macro
///to syn, this is replacing one macro item with another
#[doc(hidden)]
#[macro_export]
macro_rules! __internal_identity {
    ($e:expr) => {$e}
}

///A reference-counted type that holds an instance of a component.
///Component functions must return a view.
#[derive(Clone)]
pub struct View {
    rc: Rc<dyn DynComponent>
}

impl View {
    fn new<T: DynComponent>(val: T) -> Self {
        Self {
            rc: Rc::new(val)
        }
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

    fn updates(&self) -> &[u64];
    
    fn native_type(&self) -> Option<NativeType> {
        None
    }
}

impl Component for () {
    //TODO: make ! when never stabilizes
    type Builder = ();

    fn render(&self, _: InternalContext) -> View {
        unreachable!()
    }
    fn updates(&self) -> &[u64] {
        &[]
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
    fn updates(&self) -> &[u64];
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

    fn updates(&self) -> &[u64] {
        Component::updates(self)
    }
}

#[doc(hidden)]
pub struct InternalContext<'a> {
    pub state: &'a mut Box<dyn Any>,
    //Shared value ONLY for passing to UseState
    //within the render function this value is mutably borrowed,
    //so exec and exec_mut will panic 
    pub vnode: Shared<VNode>
}

///A hook that allows a component to keep persistent state across renders.
pub struct UseState<T: 'static> {
    state: Option<T>
}

impl<T> Default for UseState<T> {
    fn default() -> Self {
        Self {
            state: None
        }
    }
}

impl<T> UseState<T> {
    ///This function is called by the framework. Users gain access to a reference to the
    ///state type and a setter when they call it.
    pub fn hook<'a>(
        &'a mut self, 
        vnode: Shared<VNode>, 
        get_self: fn(&mut Box<dyn Any>) -> &mut UseState<T>
    ) -> impl FnOnce(T) -> (&'a T, SetStateSetter<T>) + 'a {
        move |val| {
            if let None = self.state {
                self.state = Some(val)
            };
            let state_ref = self.state.as_ref().unwrap();
            let setter = SetStateSetter::new(vnode, get_self);
            (state_ref, setter)
        }
    }
}

///Provides a setter for a piece of state managed by `UseState<T>`.
#[derive(Clone)]
pub struct SetStateSetter<T: 'static> {
    vnode: Shared<VNode>,
    get_mut: fn(&mut Box<dyn Any>) -> &mut UseState<T>,
}

impl<T: 'static> SetStateSetter<T> {
    fn new(vnode: Shared<VNode>, get_mut: fn(&mut Box<dyn Any>) -> &mut UseState<T>) -> Self {
        Self {
            vnode,
            get_mut
        }
    }

    ///Takes a function that modifies its attached state and triggers a rerender of its component.
    ///The update is not performed immediately; its effect will only be accessible 
    ///on its component's rerender.
    pub fn call<F: FnOnce(&mut T)>(&mut self, f: F) {
        let mut vnode_clone = self.vnode.clone();
        let mut vdom_clone = self.vnode.exec(|vnode| vnode.vdom.clone());
        let get_mut = self.get_mut;

        self.vnode.exec_mut(move |vnode| {
            let mut_ref_to_state = get_mut(vnode.state.as_mut().unwrap());
            f(mut_ref_to_state.state.as_mut().unwrap());

            vnode.vdom.exec_mut(|vdom| {
                vdom.renderer.schedule_on_ui_thread(
                    Box::new(move || {
                        vnode_clone.exec_mut(|vnode| vnode.dirty = true);
                        vdom_clone.exec_mut(|vdom| {
                            update_vnode(vnode_clone, None, &mut vdom.renderer);
                        });
                    })
                )
            });
        });
    }
}


