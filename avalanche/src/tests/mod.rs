mod native_mock;
mod native_repr;
mod renderer;

use std::{collections::VecDeque, vec};

use native_mock::Root;
use native_repr::Repr;

use crate::{
    component, keyed,
    renderer::{NativeType, Scheduler},
    shared::{Shared, WeakShared},
    state, store, tracked, updated, DefaultComponent, Tracked, View,
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
    root: WeakShared<Root>,
}

impl TestScheduler {
    /// Creates a scheduler initialized with which clicks will be performed over the lifetime
    /// of the test.
    fn new(click_events: Vec<&str>, root: WeakShared<Root>) -> Self {
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
                let node = self
                    .root
                    .upgrade()
                    .unwrap()
                    .exec(|root| root.get_node(&name));
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
pub fn test<C: DefaultComponent>(events: Vec<&str>, expected: Vec<Repr>) {
    let mut root = Root::new();
    let root_node = root.create_node("root");
    let root = Shared::new(root);
    let renderer = TestRenderer::new(root.clone());

    let scheduler = TestScheduler::new(events, root.downgrade());

    let avalanche_root = crate::vdom::Root::new::<_, _, C>(
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

    avalanche_root.unmount();
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
    Native(self, name = "minimal", value = "a")
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
    Native(
        self,
        name = "minimal children",
        vec![
            ().into(),
            Native(self, name = "a"),
            ().into(),
            Native(
                self,
                name = "b",
                vec![Native(
                    self,
                    name = "c",
                    value = "nested child",
                    on_click = || {},
                )],
            ),
        ],
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
    Native(
        self,
        name = "a",
        value = tracked!(status),
        on_click = move || set_status.set("clicked"),
    )
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
fn RepeatHookCalls() -> View {
    // 5 state hook calls to cause a resize of the component's state HashMap,
    // testing to ensure this is memory safe and works correctly
    let (a, set_a) = state(self, || "a");
    let (b, _) = state(self, || "b");
    let (c, _) = state(self, || "c");
    let (d, _) = state(self, || "d");
    let (e, _) = state(self, || "e");

    Native(
        self,
        name = "repeat",
        value = &format!(
            "{}{}{}{}{}",
            tracked!(a),
            tracked!(b),
            tracked!(c),
            tracked!(d),
            tracked!(e)
        ),
        on_click = || set_a.set(""),
    )
}

#[test]
fn repeat_hook_calls() {
    test::<RepeatHookCalls>(
        vec!["repeat"],
        vec![Repr {
            name: "repeat".into(),
            value: "bcde".into(),
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
    Native(
        self,
        name = "a",
        on_click = move || {
            update_children.update(|children, gen| {
                children.insert(0, Tracked::new("b", gen));
                children.insert(2, Tracked::new("d", gen))
            })
        },
        tracked!(children)
            .iter()
            .map(|child| {
                println!("Child {} is updated: {}", tracked!(child), updated!(child));
                keyed(self, tracked!(child), || {
                    Native(self, name = tracked!(child))
                })
            })
            .collect(),
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
fn OptionalChildren() -> View {
    let (children, update_children) = store(self, |gen| {
        vec![
            Some(Tracked::new("a", gen)),
            None,
            Some(Tracked::new("c", gen)),
        ]
    });

    Native(
        self,
        name = "container",
        on_click = || {
            update_children.update(|children, gen| {
                children.swap(0, 1);
                children[0] = Some(Tracked::new("b", gen));
                children[2] = None;
            })
        },
        tracked!(children)
            .iter()
            .map(|child| {
                child
                    .map(|child| {
                        keyed(self, tracked!(child), || {
                            Native(self, name = tracked!(child))
                        })
                    })
                    .into()
            })
            .collect(),
    )
}

#[test]
fn optional_children() {
    test::<OptionalChildren>(
        vec!["container"],
        vec![Repr {
            name: "container".into(),
            value: String::new(),
            has_on_click: true,
            children: vec![
                Repr {
                    name: "b".into(),
                    value: String::new(),
                    has_on_click: false,
                    children: vec![],
                },
                Repr {
                    name: "a".into(),
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

    let child = Native(self, name = "child");

    let changing_parent = if *tracked!(cond) {
        Native(self, name = "old_parent", vec![child])
    } else {
        Native(self, name = "new_parent", vec![child])
    };

    let changing_child = Native(self, name = "changing_child");

    let (child_left, child_right) = if *tracked!(cond) {
        (None, Some(changing_child))
    } else {
        (Some(changing_child), None)
    };

    Native(
        self,
        name = "container",
        on_click = || set_cond.set(false),
        vec![
            tracked!(changing_parent),
            Native(
                self,
                name = "stable_parent_left",
                vec![tracked!(child_left).into()],
            ),
            Native(
                self,
                name = "stable_parent_right",
                vec![tracked!(child_right).into()],
            ),
        ],
    )
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

#[component]
fn NestedKeyed() -> View {
    let (data, _) = state(self, || vec!["a", "b", "c"]);
    Native(
        self,
        name = "container",
        tracked!(data)
            .iter()
            .map(|d| {
                keyed(self, d, || {
                    Native(
                        self,
                        name = d,
                        vec![Native(self, name = &format!("{d}-child"))],
                    )
                })
            })
            .collect(),
    )
}

#[test]
fn nested_keyed() {
    test::<NestedKeyed>(
        vec![],
        vec![Repr {
            name: "container".into(),
            value: String::new(),
            has_on_click: false,
            children: vec![
                Repr {
                    name: "a".into(),
                    value: String::new(),
                    has_on_click: false,
                    children: vec![Repr {
                        name: "a-child".into(),
                        value: String::new(),
                        has_on_click: false,
                        children: vec![],
                    }],
                },
                Repr {
                    name: "b".into(),
                    value: String::new(),
                    has_on_click: false,
                    children: vec![Repr {
                        name: "b-child".into(),
                        value: String::new(),
                        has_on_click: false,
                        children: vec![],
                    }],
                },
                Repr {
                    name: "c".into(),
                    value: String::new(),
                    has_on_click: false,
                    children: vec![Repr {
                        name: "c-child".into(),
                        value: String::new(),
                        has_on_click: false,
                        children: vec![],
                    }],
                },
            ],
        }],
    )
}
