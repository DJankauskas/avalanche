use avalanche::renderer::{HasChildrenMarker, NativeHandle, NativeType, Renderer, Scheduler};
use avalanche::vdom::Root;
use avalanche::{Component, Tracked, View, component, enclose, tracked};

/// A renderer that does nothing, to test render functions only
struct TestRenderer;

impl Renderer for TestRenderer {
    fn create_component(&mut self, _native_type: &NativeType, _component: &View) -> NativeHandle {
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

    fn move_child(
        &mut self,
        _parent_type: &NativeType,
        _parent_handle: &mut NativeHandle,
        _old: usize,
        _new: usize,
    ) {
    }

    fn remove_child(
        &mut self,
        _parent_type: &NativeType,
        _parent_handle: &mut NativeHandle,
        _index: usize,
    ) {
    }

    fn update_component(
        &mut self,
        _native_type: &NativeType,
        _native_handle: &mut NativeHandle,
        _component: &View,
    ) {
    }

    fn remove_component(&mut self, _vnode: &mut avalanche::vdom::VNode) {}
}

/// A scheduler that does nothing, used for testing only
struct TestScheduler;

impl Scheduler for TestScheduler {
    fn schedule_on_ui_thread(&mut self, _f: Box<dyn FnOnce()>) {}
}

struct TestChildren {
    children: Vec<View>,
}

impl Component for TestChildren {
    type Builder = ();

    fn render(&self, context: avalanche::InternalContext) -> View {
        HasChildrenMarker {
            children: self.children.clone(),
        }
        .into()
    }

    fn updated(&self) -> bool {
        true
    }

    fn native_type(&self) -> Option<NativeType> {
        Some(NativeType {
            handler: "",
            name: "",
        })
    }
}

#[test]
fn test() {
    let native_parent = TestChildren {
        children: Vec::new(),
    };
    Root::new(
        Test::default().into(),
        native_parent.into(),
        Box::new(()),
        TestRenderer,
        TestScheduler,
    );
}

#[component]
fn Test() -> View {
    let a = Tracked::new(0u8, true);
    let b = Tracked::new(0u8, false);
    let c = Tracked::new(0u8, true);

    TestChildren { children: vec![
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
        StdMacros!(a: tracked!(a), b: tracked!(b), c: tracked!(c)),
        Nested!(a: tracked!(a))

    ] }.into()
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
    assert!(a.updated());
    let a = tracked!(a);
    assert!(a.updated());
    
    ().into()
}

#[component]
fn ArrayIndex(a: u8, b: u8, c: u8) -> View {
    assert!(a.updated());
    assert!(!b.updated());
    assert!(c.updated());

    let arr = [tracked!(b)];
    assert!(!arr.updated());
    
    let arr = [tracked!(a), tracked!(b)];
    assert!(arr.updated());

    let arr = [tracked!(c), tracked!(b)];
    assert!(arr.updated());

    let arr = [tracked!(a), tracked!(b), tracked!(c)];
    assert!(arr.updated());

    {
        let second = tracked!(arr)[1];
        assert!(second.updated());
    }

    let arr2 = [tracked!(a); 3];
    assert!(arr2.updated());
    let from2 = tracked!(arr2)[0];
    assert!(from2.updated());

    let arr3 = [1u8];

    let indexed = arr3[*tracked!(c) as usize];
    assert!(indexed.updated());

    ().into()
}

#[component]
fn Binary(a: u8, b: u8, c: u8) -> View {
    let x = tracked!(a) ^ tracked!(b);
    assert!(x.updated());

    let y = tracked!(b) & tracked!(c);
    assert!(y.updated());

    let z = tracked!(b) * tracked!(b);
    assert!(!z.updated());

    ().into()
}

#[component]
fn Block(a: u8, b: u8) -> View {
    let x = {
        let x = tracked!(a) + tracked!(b);
        x
    };
    assert!(x.updated());

    ().into()
}

#[component]
fn FnCall(a: u8) -> View {
    let b = std::convert::identity(tracked!(a));
    assert!(b.updated());

    ().into()
}

#[component]
fn Cast(a: u8) -> View {
    let ret = *tracked!(a) as u16;
    assert!(ret.updated());

    ().into()
}

#[component]
fn Closure(a: u8, b: u8) -> View {
    let closure1 = || {
        let _ = (*tracked!(a), *tracked!(b));
    };
    assert!(closure1.updated());

    let closure2 = || {
        tracked!(b);
    };
    assert!(!closure2.updated());

    ().into()
}

#[component]
fn Field(a: (u8, u8), b: u8) -> View {
    let ret = tracked!(a).0;
    assert!(ret.updated());

    ().into()
}

#[component]
fn If(a: u8, b: u8, c: u8) -> View {
    let x = if *tracked!(a) == 0 { tracked!(b) } else { &5 };
    assert!(x.updated());

    let y = if *tracked!(b) == 0 { tracked!(a) } else {tracked!(c)};
    assert!(y.updated());

    ().into()
}

#[component]
fn Loop(a: u8, b: u8, c: u8) -> View {
    let x = loop {
        break tracked!(a);
    };
    assert!(x.updated());

    let y = loop {
        if *tracked!(c) == 0 {
            break 0;
        }
    };
    assert!(y.updated());

    ().into()
}

#[component]
fn Match(a: u8, b: u8, c: u8) -> View {
    let option = Some(tracked!(a));
    let x = match tracked!(option) {
        Some(var) => var,
        None => tracked!(b),
    };
    assert!(option.updated());


    let y = match *tracked!(b) {
        0 => "zero",
        1 => "one",
        _ => "other",
    };
    assert!(!y.updated());
    
    let z = match tracked!(b) {
        0 if *tracked!(c) == 0 => "zero",
        _ => "other"
    };

    ().into()
}

#[component]
fn Unary(a: u8) -> View {
    let b = !*tracked!(a);
    
    assert!(b.updated());

    ().into()
}

#[component]
fn Tuple(a: u8, b: u8, c: u8) -> View {
    let tuple = (tracked!(a), tracked!(b));
    assert!(tuple.updated());

    let tuple = (tracked!(b), tracked!(c));
    assert!(tuple.updated());

    let tuple = (tracked!(b), tracked!(b));
    assert!(!tuple.updated());

    ().into()
}

#[component]
fn StdMacros(a: u8, b: u8, c: u8) -> View {
    // testing dbg!
    let a_prime = dbg!(tracked!(a));
    assert!(a.updated());

    let ab_prime = dbg!(tracked!(b));
    assert!(!b.updated());

    // testing enclose!
    let cloned_a = enclose!(a; tracked!(a));
    assert!(cloned_a.updated());

    let cloned_a = enclose!(a; a);
    assert!(cloned_a.updated());

    // testing format!
    let formatted = format!("{} {}", tracked!(a), tracked!(b));
    assert!(formatted.updated());

    let formatted2 = format!("{} {d}", tracked!(b), d=tracked!(c));
    assert!(formatted2.updated());

    // testing matches!
    let matched = matches!(tracked!(a), 1);
    assert!(matched.updated());

    let matched = matches!(tracked!(b), 1 | 2);
    assert!(!matched.updated());

    let matched = matches!(tracked!(b), 0 | 1 if *tracked!(c) > 2,);
    assert!(matched.updated());


    // testing vec!
    let vec = vec![tracked!(a)];
    assert!(vec.updated());

    let vec = vec![tracked!(b), tracked!(c)];
    assert!(vec.updated());

    let vec = vec![5; *tracked!(c) as usize];
    assert!(vec.updated());

    // testing try!
    // TODO: adjust closure handling or remove this test
    // let ret: Result<u8, ()> = (|| {
    //     let a = Ok(*tracked!(a));
    //     Ok(r#try!(tracked!(a)))
    // })();

    ().into()
}

#[component]
fn Nested(a: u8) -> View {
    let x = loop {
        break loop {
            break tracked!(a);
        }
    };
    assert!(x.updated());

    // Check assignment within nesting
    let y = loop {
        let y = loop {
            break tracked!(a);
        };
        break y;
    };
    assert!(y.updated());

    let closure = || {
        loop {
            break tracked!(a);
        }
    };
    assert!(closure.updated());

    ().into()
}
