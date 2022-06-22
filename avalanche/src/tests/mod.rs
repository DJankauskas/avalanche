mod native_mock;
mod native_repr;
mod renderer;

use std::{collections::VecDeque, vec};

use native_mock::Root;
use native_repr::Repr;

use crate::{
    component,
    renderer::{NativeType, Scheduler},
    shared::Shared,
    state, store, tracked, updated, Component, Tracked, View,
};

use self::{native_mock::Native, renderer::TestRenderer};

/// Implements a functional scheduler implementing a basic
/// event queue for testing purposes. avalanche-scheduled callbacks always run before any scheduled click
/// events to ensure future click events only occur when past ones have been fully processed.
#[derive(Clone)]
struct TestScheduler {
    /// Events scheduled by avalanche, in FIFO order.
    scheduled_events: Shared<VecDeque<Box<dyn FnOnce()>>>,
    /// Remaining clicks in the event loop, to be popped off in descending order.
    click_events: Vec<String>,
    /// The root of the component tree, used to deliver clicks.
    root: Shared<Root>,
}

impl TestScheduler {
    /// Creates a scheduler initialized with which clicks will be performed over the lifetime
    /// of the test.
    fn new(click_events: Vec<&str>, root: Shared<Root>) -> Self {
        let click_events = click_events
            .into_iter()
            .rev()
            .map(ToOwned::to_owned)
            .collect();
        Self {
            scheduled_events: Shared::new(VecDeque::new()),
            click_events,
            root,
        }
    }

    fn run(mut self) {
        while !(self.click_events.is_empty() && self.scheduled_events.exec(|e| e.is_empty())) {
            if let Some(event) = self.scheduled_events.exec_mut(|e| e.pop_front()) {
                event();
                continue;
            } else {
                let name = self.click_events.pop().unwrap();
                let node = self.root.exec(|root| root.get_node(&name));
                node.click();
            }
        }
    }
}

impl Scheduler for TestScheduler {
    fn schedule_on_ui_thread(&mut self, f: Box<dyn FnOnce()>) {
        self.scheduled_events.exec_mut(|events| events.push_back(f));
    }
}

/// Render the given component, apply click events to the components with the given names,
/// then check if the final component tree is equivalent to the expected one.
fn test<C: Component<'static> + Default>(events: Vec<&str>, expected: Vec<Repr>) {
    let mut root = Root::new();
    let root_node = root.create_node("root");
    let root = Shared::new(root);
    let renderer = TestRenderer::new(root.clone());

    let scheduler = TestScheduler::new(events, root.clone());

    let _avalanche_root = crate::vdom::Root::new::<_, _, C>(
        NativeType {
            handler: "",
            name: "",
        },
        Box::new(root_node.clone()),
        renderer,
        scheduler.clone(),
    );

    scheduler.run();

    let expected_repr = Repr {
        children: expected,
        name: "root".to_string(),
        value: String::new(),
        has_on_click: false,
    };
    let actual_repr = root_node.to_repr();
    assert_eq!(expected_repr, actual_repr);
}

#[component]
fn Minimal() -> View {
    ().into()
}

#[test]
fn minimal() {
    test::<Minimal>(vec![], vec![]);
}

#[component]
fn MinimalNative() -> View {
    Native!(name: "minimal", value: "a")
}

#[test]
fn minimal_native() {
    test::<MinimalNative>(
        vec![],
        vec![Repr {
            children: vec![],
            name: "minimal".to_string(),
            value: "a".to_string(),
            has_on_click: false,
        }],
    )
}

#[component]
fn MinimalChildren() -> View {
    Native!(
        name: "minimal children",
        vec![
            ().into(),
            Native!(name: "a"),
            ().into(),
            Native!(name: "b", vec![Native!(name: "c", value: "nested child", on_click: || {})])
        ]
    )
}

#[test]
fn minimal_children() {
    test::<MinimalChildren>(
        vec![],
        vec![Repr {
            name: "minimal children".to_string(),
            value: String::new(),
            has_on_click: false,
            children: vec![
                Repr {
                    name: "a".to_string(),
                    value: String::new(),
                    has_on_click: false,
                    children: vec![],
                },
                Repr {
                    name: "b".to_string(),
                    value: String::new(),
                    has_on_click: false,
                    children: vec![Repr {
                        name: "c".to_string(),
                        value: "nested child".to_string(),
                        has_on_click: true,
                        children: vec![],
                    }],
                },
            ],
        }],
    )
}

#[component]
fn BasicStateEvent() -> View {
    let (status, set_status) = state(self, || "default");
    Native!(name: "a", value: tracked!(status), on_click: move || set_status.set("clicked"))
}

#[test]
fn basic_state_event() {
    test::<BasicStateEvent>(
        vec!["a"],
        vec![Repr {
            name: "a".to_string(),
            value: "clicked".to_string(),
            has_on_click: true,
            children: vec![],
        }],
    );
}

#[component]
fn AddChildren() -> View {
    let (children, update_children) = store(self, |gen| vec![Tracked::new("c", gen)]);
    println!(
        "children updated? - {}, {}",
        updated!(children),
        updated!(tracked!(children).iter().next().unwrap())
    );
    Native!(
        name: "a",
        on_click: move || update_children.update(|children, gen| { children.insert(0, Tracked::new("b", gen)); children.insert(2, Tracked::new("d", gen)) }),
        tracked!(children)
            .iter()
            .map(|child| { println!("Child {} is updated: {}", tracked!(child), updated!(child)); Native!(key: tracked!(child).to_string(), name: tracked!(child)) })
            .collect()
    )
}

#[test]
fn add_children() {
    test::<AddChildren>(
        vec!["a"],
        vec![Repr {
            name: "a".to_string(),
            value: String::new(),
            has_on_click: true,
            children: vec![
                Repr {
                    name: "b".to_string(),
                    value: String::new(),
                    has_on_click: false,
                    children: vec![],
                },
                Repr {
                    name: "c".to_string(),
                    value: String::new(),
                    has_on_click: false,
                    children: vec![],
                },
                Repr {
                    name: "d".to_string(),
                    value: String::new(),
                    has_on_click: false,
                    children: vec![],
                },
            ],
        }],
    )
}

#[component]
fn ReparentChild() -> View {
    let (cond, set_cond) = state(self, || true);

    let child = Native!(name: "child");

    let changing_parent = if *tracked!(cond) {
        Native!(name: "old_parent",  vec![child])
    } else {
        Native!(name: "new_parent", vec![child])
    };

    let changing_child = Native!(name: "changing_child");

    let (child_left, child_right) = if *tracked!(cond) {
        (None, Some(changing_child))
    } else {
        (Some(changing_child), None)
    };

    Native!(name: "container", on_click: || set_cond.set(false), vec![
        tracked!(changing_parent),
        Native!(
            name: "stable_parent_left",
            vec![tracked!(child_left).into()]
        ),
        Native!(
            name: "stable_parent_right",
            vec![tracked!(child_right).into()]
        ),
    ])
}

#[test]
fn reparent_child() {
    test::<ReparentChild>(
        vec!["container"],
        vec![Repr {
            name: "container".into(),
            value: String::new(),
            has_on_click: true,
            children: vec![
                Repr {
                    name: "new_parent".into(),
                    value: String::new(),
                    has_on_click: false,
                    children: vec![Repr {
                        name: "child".into(),
                        value: String::new(),
                        has_on_click: false,
                        children: vec![],
                    }],
                },
                Repr {
                    name: "stable_parent_left".into(),
                    value: String::new(),
                    has_on_click: false,
                    children: vec![Repr {
                        name: "changing_child".into(),
                        value: String::new(),
                        has_on_click: false,
                        children: vec![],
                    }],
                },
                Repr {
                    name: "stable_parent_right".into(),
                    value: String::new(),
                    has_on_click: false,
                    children: vec![],
                },
            ],
        }],
    )
}
