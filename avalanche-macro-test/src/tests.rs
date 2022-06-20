use avalanche::any_ref::DynRef;
use avalanche::renderer::{
    DispatchNativeEvent, NativeEvent, NativeHandle, NativeType, Renderer, Scheduler,
};
use avalanche::tracked::Gen;
use avalanche::vdom::Root;
use avalanche::{component, enclose, tracked, updated, Component, Tracked, View};

/// A renderer that does nothing, to test render functions only
struct TestRenderer;

impl Renderer for TestRenderer {
    fn create_component(
        &mut self,
        _native_type: &NativeType,
        _component: DynRef,
        _dispatch_native_event: DispatchNativeEvent,
    ) -> NativeHandle {
        Box::new(())
    }

    fn append_child(
        &mut self,
        _parent_type: &NativeType,
        _parent_handle: &mut NativeHandle,
        _child_type: &NativeType,
        _child_handle: &NativeHandle,
    ) {
    }

    fn insert_child(
        &mut self,
        _parent_type: &NativeType,
        _parent_handle: &mut NativeHandle,
        _index: usize,
        _child_type: &NativeType,
        _child_handle: &NativeHandle,
    ) {
    }

    fn replace_child(
        &mut self,
        _parent_type: &NativeType,
        _parent_handle: &mut NativeHandle,
        _index: usize,
        _child_type: &NativeType,
        _child_handle: &NativeHandle,
    ) {
    }

    fn swap_children(
        &mut self,
        _parent_type: &NativeType,
        _parent_handle: &mut NativeHandle,
        _a: usize,
        _b: usize,
    ) {
    }

    fn truncate_children(
        &mut self,
        parent_type: &NativeType,
        parent_handle: &mut NativeHandle,
        len: usize,
    ) {
    }

    fn update_component(
        &mut self,
        _native_type: &NativeType,
        _native_handle: &mut NativeHandle,
        _component: DynRef,
        _curr_gen: Gen,
        _native_event: Option<NativeEvent>,
    ) {
    }
}

/// A scheduler that does nothing, used for testing only
struct TestScheduler;

impl Scheduler for TestScheduler {
    fn schedule_on_ui_thread(&mut self, _f: Box<dyn FnOnce()>) {}
}

struct TestChildren {
    children: Vec<View>,
    location: (u32, u32),
}

impl TestChildren {
    fn new() -> Self {
        Self {
            children: Vec::new(),
            location: (0, 0),
        }
    }

    fn __last(mut self, children: Vec<View>, _gen: Gen<'_>) -> Self {
        self.children = children;
        self
    }

    fn build(mut self, location: (u32, u32)) -> Self {
        self.location = location;
        self
    }
}

avalanche::impl_any_ref! { TestChildren }

impl<'a> Component<'a> for TestChildren {
    type Builder = Self;

    fn render(self, _: avalanche::RenderContext, _: avalanche::HookContext) -> View {
        unimplemented!()
    }

    fn children(self) -> Vec<View> {
        self.children
    }

    fn updated(&self, _curr_gen: Gen) -> bool {
        true
    }

    fn native_type(&self) -> Option<NativeType> {
        Some(NativeType {
            handler: "",
            name: "",
        })
    }

    fn location(&self) -> Option<(u32, u32)> {
        Some(self.location)
    }
}

#[test]
fn test() {
    let native_parent = TestChildren {
        children: Vec::new(),
        location: (0, 0),
    };
    Root::new::<_, _, Test>(
        NativeType {
            handler: "",
            name: "",
        },
        Box::new(()),
        TestRenderer,
        TestScheduler,
    );
}

#[component]
fn Test() -> View {
    let gen_updated = Gen::escape_hatch_new(true);
    let gen_not_updated = Gen::escape_hatch_new(false);
    let a = Tracked::new(0u8, gen_updated);
    let b = Tracked::new(0u8, gen_not_updated);
    let c = Tracked::new(0u8, gen_updated);

    TestChildren!(vec![
        Bare!(),
        Identity!(a: tracked!(a)),
        ArrayIndex!(a: tracked!(a), b: tracked!(b), c: tracked!(c)),
        Binary!(a: tracked!(a), b: tracked!(b), c: tracked!(c)),
        Block!(a: tracked!(a), b: tracked!(b)),
        FnCall!(a: tracked!(a)),
        Cast!(a: tracked!(a)),
        Closure!(a: tracked!(a), b: tracked!(b)),
        Field!(a: (tracked!(a), tracked!(a)), b: tracked!(b)),
        If!(a: tracked!(a), b: tracked!(b), c: tracked!(c)),
        Loop!(a: tracked!(a), b: tracked!(b), c: tracked!(c)),
        Match!(a: tracked!(a), b: tracked!(b), c: tracked!(c)),
        Unary!(a: tracked!(a)),
        Tuple!(a: tracked!(a), b: tracked!(b), c: tracked!(c)),
        Macros!(a: tracked!(a), b: tracked!(b), c: tracked!(c)),
        NestedBlocks!(a: tracked!(a)),
        NestedTracked!(a: tracked!(a), b: tracked!(b)),
        Updated!(a: tracked!(a), b: tracked!(b), c: tracked!(c)),
        BasicRef!(a: &tracked!(a)),
        ComplexRef!(a: &tracked!(a), bc: &[&tracked!(b), &tracked!(c)]),
        ComplexTrait!(b: &tracked!(&b)),
        ParameterizedRef!(a: vec![&tracked!(a)]),
        ExplicitLifetime!(b: &tracked!(b)),
        MixedLifetimes!(a: &tracked!(a), c: &tracked!(c))
    ])
}

#[derive(Default)]
struct HasFields {
    field_one: u8,
    field_two: u8,
}

#[component]
fn Bare() -> View {
    ().into()
}

#[component]
fn Identity(a: u8) -> View {
    assert!(updated!(a));
    let a = tracked!(a);
    assert!(updated!(a));

    ().into()
}

#[component]
fn ArrayIndex(a: u8, b: u8, c: u8) -> View {
    let arr = [tracked!(b)];
    assert!(!updated!(arr));

    let arr = [tracked!(a), tracked!(b)];
    assert!(updated!(arr));

    let arr = [tracked!(c), tracked!(b)];
    assert!(updated!(arr));

    let arr = [tracked!(a), tracked!(b), tracked!(c)];
    assert!(updated!(arr));

    {
        let second = tracked!(arr)[1];
        assert!(updated!(second));
    }

    let arr2 = [tracked!(a); 3];
    assert!(updated!(arr2));
    let from2 = tracked!(arr2)[0];
    assert!(updated!(from2));

    let arr3 = [1u8];

    let indexed = arr3[tracked!(c) as usize];
    assert!(updated!(indexed));

    ().into()
}

#[component]
fn Binary(a: u8, b: u8, c: u8) -> View {
    let x = tracked!(a) ^ tracked!(b);
    assert!(updated!(x));

    let y = tracked!(b) & tracked!(c);
    assert!(updated!(y));

    let z = tracked!(b) * tracked!(b);
    assert!(!updated!(z));

    ().into()
}

#[component]
fn Block(a: u8, b: u8) -> View {
    let x = {
        let x = tracked!(a) + tracked!(b);
        x
    };
    assert!(updated!(x));

    ().into()
}

#[component]
fn FnCall(a: u8) -> View {
    let b = std::convert::identity(tracked!(a));
    assert!(updated!(b));

    ().into()
}

#[component]
fn Cast(a: u8) -> View {
    let ret = tracked!(a) as u16;
    assert!(updated!(ret));

    ().into()
}

#[component]
fn Closure(a: u8, b: u8) -> View {
    let closure1 = || {
        let _ = (tracked!(a), tracked!(b));
    };
    assert!(updated!(closure1));

    let closure2 = || {
        tracked!(b);
    };
    assert!(!updated!(closure2));

    let closure3 = || {
        while false {
            updated!(a);
        }
    };
    assert!(updated!(a));

    let closure4 = || {
        if true {
            match 1 {
                1 => updated!(a),
                _ => unreachable!(),
            };
        }
    };
    assert!(updated!(closure4));

    let closure5 = || {
        let closure = || tracked!(a);
    };
    assert!(updated!(closure5));

    ().into()
}

#[component]
fn Field(a: (u8, u8), b: u8) -> View {
    let ret = tracked!(a).0;
    assert!(updated!(ret));

    ().into()
}

#[component]
fn If(a: u8, b: u8, c: u8) -> View {
    let x = if tracked!(a) == 0 { tracked!(b) } else { 5 };
    assert!(updated!(x));

    let y = if tracked!(b) == 0 {
        tracked!(a)
    } else {
        tracked!(c)
    };
    assert!(updated!(y));

    ().into()
}

#[component]
fn Loop(a: u8, b: u8, c: u8) -> View {
    let x = loop {
        break tracked!(a);
    };
    assert!(updated!(x));

    let y = loop {
        if tracked!(c) == 0 {
            break 0;
        }
    };
    assert!(updated!(y));

    ().into()
}

#[component]
fn Match(a: u8, b: u8, c: u8) -> View {
    let option = Some(tracked!(a));
    let x = match tracked!(option) {
        Some(var) => var,
        None => tracked!(b),
    };
    assert!(updated!(option));

    let y = match tracked!(b) {
        0 => "zero",
        1 => "one",
        _ => "other",
    };
    assert!(!updated!(y));

    let z = match tracked!(b) {
        0 if tracked!(c) == 0 => "zero",
        _ => "other",
    };
    assert!(updated!(z));

    ().into()
}

#[component]
fn Unary(a: u8) -> View {
    let b = !tracked!(a);

    assert!(updated!(b));

    ().into()
}

#[component]
fn Tuple(a: u8, b: u8, c: u8) -> View {
    let tuple = (tracked!(a), tracked!(b));
    assert!(updated!(tuple));

    let tuple = (tracked!(b), tracked!(c));
    assert!(updated!(tuple));

    let tuple = (tracked!(b), tracked!(b));
    assert!(!updated!(tuple));

    ().into()
}

#[component]
fn Macros(a: u8, b: u8, c: u8) -> View {
    // testing assert
    let result = assert!(updated!(a));
    assert!(updated!(result));

    // testing dbg!
    let a_prime = dbg!(tracked!(a));
    assert!(updated!(a));

    let ab_prime = dbg!(tracked!(b));
    assert!(!updated!(b));

    // testing enclose!
    let updated_a = enclose!(; updated!(a));
    assert!(updated!(updated_a));
    let cloned_a = enclose!(a; tracked!(a));
    assert!(updated!(cloned_a));

    let cloned_a = enclose!(a; a);
    assert!(updated!(cloned_a));

    // testing format!
    let formatted = format!("{} {}", tracked!(a), tracked!(b));
    assert!(updated!(formatted));

    let formatted2 = format!("{} {d}", tracked!(b), d = tracked!(c));
    assert!(updated!(formatted2));

    // testing matches!
    let matched = matches!(tracked!(a), 1);
    assert!(updated!(matched));

    let matched = matches!(tracked!(b), 1 | 2);
    assert!(!updated!(matched));

    let matched = matches!(tracked!(b), 0 | 1 if tracked!(c) > 2,);
    assert!(updated!(matched));

    // testing vec!
    let vec = vec![tracked!(a)];
    assert!(updated!(vec));

    let vec = vec![tracked!(b), tracked!(c)];
    assert!(updated!(vec));

    let vec = vec![5; tracked!(c) as usize];
    assert!(updated!(vec));

    // testing try!
    // TODO: adjust closure handling or remove this test
    // let ret: Result<u8, ()> = (|| {
    //     let a = Ok(tracked!(a));
    //     Ok(r#try!(tracked!(a)))
    // })();
    // assert!(updated!(ret.unwrap()));

    ().into()
}

#[component]
fn NestedBlocks(a: u8) -> View {
    let x = loop {
        break loop {
            break tracked!(a);
        };
    };
    assert!(updated!(x));

    // Check assignment within nesting
    let y = loop {
        let y = loop {
            break tracked!(a);
        };
        break y;
    };
    assert!(updated!(y));

    let closure = || loop {
        break tracked!(a);
    };
    assert!(updated!(closure));

    ().into()
}

#[component]
fn NestedTracked(a: u8, b: u8) -> View {
    let nested = Tracked::new(a, Gen::escape_hatch_new(true));
    let nested_val = tracked!(tracked!(nested));
    assert!(updated!(nested_val));

    // if a non updated value is nested within an updated one, accessing the non updated value
    // should report an updated value of false
    let nested = Tracked::new(b, Gen::escape_hatch_new(true));
    let nested_val = tracked!(tracked!(nested));
    assert!(!updated!(nested_val));

    ().into()
}

#[component]
fn Updated(a: u8, b: u8, c: u8) -> View {
    assert!(updated!(a));
    assert!(!updated!(b));
    assert!(updated!(c));

    let a = updated!(a);
    let b = updated!(b);
    let c = updated!(c);

    assert!(updated!(a));
    assert!(!updated!(b));
    assert!(updated!(c));

    let x = updated!(a) && updated!(b);
    let y = updated!(b) || updated!(b);
    assert!(updated!(x));
    assert!(!updated!(y));

    ().into()
}

#[component]
fn BasicRef(a: &u8) -> View {
    assert!(updated!(a));
    ().into()
}

#[component]
fn ComplexRef(a: &u8, bc: &[&u8]) -> View {
    assert!(updated!(a));
    assert!(updated!(bc));

    ().into()
}

#[component]
fn ComplexTrait(b: &<&u8 as std::ops::Deref>::Target) -> View {
    assert!(!updated!(b));
    ().into()
}

#[component]
fn ParameterizedRef(a: Vec<&u8>) -> View {
    assert!(updated!(a));

    ().into()
}

#[component]
fn ExplicitLifetime<'a>(b: &'a u8) -> View {
    assert!(!updated!(b));

    ().into()
}

#[component]
fn MixedLifetimes<'a>(a: &u8, c: &'a u8) -> View {
    assert!(updated!(a) && updated!(c));

    ().into()
}
