use std::mem::ManuallyDrop;
use std::{any::Any, cell::RefCell, panic::Location, rc::Rc};

use rustc_hash::FxHashMap;

use crate::alloc::{Box as BumpBox, Bump};
use crate::hooks::{HookContext, RenderContext};
use crate::renderer::{DispatchNativeEvent, NativeEvent};
use crate::shared::Shared;
use crate::tracked::Gen;
use crate::{
    renderer::{NativeHandle, Renderer, Scheduler},
    tracked::InternalGen,
};
use crate::{Component, DefaultComponent, View};

mod algos;
pub(crate) mod data;
mod dyn_component;
// Separate wrapper types for maintaining and mutating component state during rendering.
// Separated into a separate module to keep data structures maintaining safety invariants
// as isolated as possible.
pub(crate) mod state;

pub use algos::render_child;
use algos::render_vdom;
use data::{ComponentId, NativeComponent, VDom, VNode};
use state::SharedBox;

pub(crate) type ComponentState = FxHashMap<Location<'static>, SharedBox<dyn Any>>;

/// Contains the data structures necessary to support the avalanche vdom abstraction. This struct
/// should only be used by renderer implementation libraries.
///
/// # Usage
///
/// In order to render an avalanche `View`, a renderer library should accept a `View` from the user, then
/// use the `new` method to create a `Root` instance.
pub struct Root {
    vdom: ManuallyDrop<Shared<VDom>>,
    scheduler: Shared<dyn Scheduler>,
}

impl Root {
    /// Creates a new UI tree rooted at `native_handle`. That handle will be used
    /// in order to allow rooting an avalanche tree upon
    /// an existing UI component created externally. Renders `child` as the child of `native_handle`.
    ///
    /// Removes all current children of `native_handle`. After calling `new`, only avalanche should
    /// manipulate or insert descendents of `native_handle` until and if the created instance of `Root`'s
    /// `unmount` method is called. Modifying those children before `unmount` is called will likely result
    /// in panics.
    pub fn new<'a, R: Renderer + 'static, S: Scheduler + 'static, C: DefaultComponent>(
        mut native_handle: NativeHandle,
        mut renderer: R,
        scheduler: S,
    ) -> Self {
        // Remove all the children of `native_handle`.
        renderer.truncate_children(&mut native_handle, 0);

        let mut curr_component_id = ComponentId::new();
        let root_component_id = curr_component_id.create_next();
        let body_children = FxHashMap::with_capacity_and_hasher(1, Default::default());
        let mut children = FxHashMap::with_capacity_and_hasher(16, Default::default());
        let vnode = VNode {
            body_parent: None,
            body_children,
            native_component: Some(NativeComponent {
                native_handle,
                native_parent: None,
                native_children: Vec::new(),
            }),
            state: FxHashMap::default(),
            dirty: false,
            view: View {
                id: Some(root_component_id),
                native_component_id: Some(root_component_id),
            },
        };
        children.insert(root_component_id, vnode);
        let vdom = VDom {
            children,
            curr_component_id,
            renderer: Box::new(renderer),
            gen: InternalGen::new(),
            update_vdom: render_vdom::<C>,
            bump: Bump::new(),
        };
        let vdom = Shared::new(vdom);
        let vdom_clone = vdom.clone();
        let scheduler: Shared<dyn Scheduler> = Shared::new_dyn(Rc::new(RefCell::new(scheduler)));
        vdom.exec_mut(|vdom| {
            render_vdom::<C>(vdom, &vdom_clone, &scheduler, None);
        });
        Root {
            vdom: ManuallyDrop::new(vdom),
            scheduler,
        }
    }

    /// Unmounts the tree created by `new`, clearing its root native handle of all the
    /// children created by avalanche. This method also drops all the state of the tree.
    pub fn unmount(self) {
        let vdom = ManuallyDrop::into_inner(self.vdom);
        let is_vdom_borrowed = vdom.borrowed();
        let exec_unmount = move || {
            vdom.exec_mut(|vdom| {
                let root_vnode = vdom.children.get_mut(&ComponentId::new());
                let native_root = root_vnode
                    .and_then(|node| node.native_component.as_mut())
                    .unwrap();

                // Clear children
                vdom.renderer
                    .truncate_children(&mut native_root.native_handle, 0);
            });
        };
        if is_vdom_borrowed {
            self.scheduler.exec_mut(|scheduler| {
                scheduler.schedule_on_ui_thread(Box::new(exec_unmount));
            })
        } else {
            exec_unmount();
        }
    }
}
