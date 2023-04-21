use std::marker::PhantomData;

use super::*;

type NativeUpdateType =
    unsafe fn(*mut (), &mut dyn Renderer, &NativeHandle, Gen, Option<NativeEvent>);

/// Contains pointers for executing methods on an instance of a
/// `Component` behind a `*mut ()`.
struct DynComponentVTable {
    native_create: unsafe fn(*mut (), &mut dyn Renderer, DispatchNativeEvent) -> NativeHandle,
    get_render: unsafe fn(*mut (), RenderContext, HookContext) -> View,
    native_update: NativeUpdateType,
    drop: unsafe fn(*mut ()),
}

/// A type-erased container for `Component` allowing virtualization
/// to avoid code bloat.
pub(crate) struct DynComponent<'a, 'b> {
    updated: bool,
    location: Option<(u32, u32)>,
    is_native: bool,
    /// A pointer to a `BumpBox`-allocated instance of a Component<'a>.
    /// It must never be null and always be valid when dropped.
    inner: *mut (),
    vtable: &'static DynComponentVTable,
    native_children: unsafe fn(*mut ()) -> &'a [View],
    /// whether the component pointed to by inner has been dropped
    component_dropped: bool,
    /// Ensures the `DynComponent` cannot outlive lifetime of the data
    /// held in `inner`.
    phantom: PhantomData<(&'a (), &'b ())>,
}

trait ComponentVTable {
    const VTABLE: DynComponentVTable;
}

unsafe fn native_create<'a, C: Component<'a>>(
    c: *mut (),
    renderer: &mut dyn Renderer,
    dispatch_native_event: DispatchNativeEvent,
) -> NativeHandle {
    // SAFETY: by the conditions of `native_update`.
    let component_ref = unsafe { &*c.cast::<C>() };
    component_ref.native_create(renderer, dispatch_native_event)
}

impl<'a, C: Component<'a>> ComponentVTable for C {
    const VTABLE: DynComponentVTable = DynComponentVTable {
        native_create: native_create::<C>,
        get_render: get_render::<C>,
        native_update: native_update::<C>,
        drop: drop::<C>,
    };
}

/// SAFETY: `c` must be a valid pointer to an instance of `C`.
unsafe fn native_update<'a, C: Component<'a>>(
    c: *mut (),
    renderer: &mut dyn Renderer,
    native_handle: &NativeHandle,
    curr_gen: Gen,
    event: Option<NativeEvent>,
) {
    // SAFETY: by the conditions of `native_update`.
    let component_ref = unsafe { &*c.cast::<C>() };
    component_ref.native_update(renderer, native_handle, curr_gen, event);
}

/// SAFETY: `c` must be a valid pointer to an instance of `C`.
/// The value pointed to by `c` must not be used after this
/// function is called.
unsafe fn native_children<'a, C: Component<'a>>(c: *mut ()) -> &'a [View] {
    let component = unsafe { c.cast::<C>().read() };
    component.native_children()
}

/// SAFETY: `c` must be a valid pointer to an instance of `C`.
/// The value pointed to by `c` must not be used after this
/// function is called.
unsafe fn get_render<'a, C: Component<'a>>(
    c: *mut (),
    render_ctx: RenderContext,
    hook_ctx: HookContext,
) -> View {
    // SAFETY: by the conditions of `get_render`.
    let component = unsafe { c.cast::<C>().read() };
    component.render(render_ctx, hook_ctx)
}

/// SAFETY: `c` must be a valid pointer to an instance of `C`.
/// The value pointed to by `c` must not be used after this
/// function is called.
unsafe fn drop<'a, C: Component<'a>>(c: *mut ()) {
    let _ = unsafe { c.cast::<C>().read() };
}

impl<'a, 'b> DynComponent<'a, 'b> {
    pub(super) fn new_in<C: Component<'a>>(component: C, gen: InternalGen, bump: &'b Bump) -> Self {
        Self {
            updated: component.updated(gen.into()),
            location: component.location(),
            is_native: component.is_native(),
            inner: BumpBox::into_raw(BumpBox::new_in(component, bump)).cast(),
            vtable: &C::VTABLE,
            native_children: native_children::<C>,
            component_dropped: false,
            phantom: PhantomData,
        }
    }

    pub(super) fn updated(&self) -> bool {
        self.updated
    }

    pub(super) fn location(&self) -> Option<(u32, u32)> {
        self.location
    }

    pub(super) fn is_native(&self) -> bool {
        self.is_native
    }

    pub(super) fn native_create(
        &self,
        renderer: &mut dyn Renderer,
        dispatch_native_event: DispatchNativeEvent,
    ) -> NativeHandle {
        // SAFETY: self.inner is valid.
        unsafe { (self.vtable.native_create)(self.inner, renderer, dispatch_native_event) }
    }

    pub(super) fn native_update(
        &self,
        renderer: &mut dyn Renderer,
        native_handle: &NativeHandle,
        curr_gen: Gen,
        event: Option<NativeEvent>,
    ) {
        // SAFETY: self.inner is valid.
        unsafe {
            (self.vtable.native_update)(self.inner, renderer, native_handle, curr_gen, event)
        };
    }

    pub(super) fn native_children(mut self) -> &'a [View] {
        self.component_dropped = true;
        // SAFETY: self.inner is valid and is not dereferenced after this call.
        unsafe { (self.native_children)(self.inner) }
    }

    pub(super) fn render(mut self, render_ctx: RenderContext, hook_ctx: HookContext) -> View {
        self.component_dropped = true;
        // SAFETY: self.inner is valid and is not dereferenced after this call.
        let ret = unsafe { (self.vtable.get_render)(self.inner, render_ctx, hook_ctx) };

        ret
    }
}

impl<'a, 'b> Drop for DynComponent<'a, 'b> {
    fn drop(&mut self) {
        // SAFETY: inner must always be a valid allocation created by a
        // Box that holds a valid Component instance.
        unsafe {
            if !self.component_dropped {
                (self.vtable.drop)(self.inner);
            }
        }
    }
}
