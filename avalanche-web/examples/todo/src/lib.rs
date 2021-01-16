use avalanche::{component, enclose, reactive_assert, UseState};
use avalanche_web::components::{
    Button, Div, Footer, Header, Input, Label, Li, Section, Span, Strong, Text, Ul, A, H1,
};
use wasm_bindgen::prelude::*;

// When the `wee_alloc` feature is enabled, this uses `wee_alloc` as the global
// allocator.
//
// If you don't want to use `wee_alloc`, you can safely delete this.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

const ENTER_KEY: u32 = 13;
const ESCAPE_KEY: u32 = 27;

#[derive(Debug, Clone)]
struct Item {
    text: String,
    completed: bool,
    id: u32,
}

#[derive(Clone, Copy, PartialEq)]
enum Filter {
    All,
    Active,
    Completed,
}

impl Filter {
    fn selected(&self, other: Filter) -> Option<&'static str> {
        if *self == other {
            Some("selected")
        } else {
            Some("")
        }
    }
}

#[component(items = UseState<Vec<Item>>, monotonic_id = UseState<u32>, editing = UseState<Option<u32>>, filter = UseState<Filter>)]
fn Todo() {
    let (editing, set_editing) = editing(None);
    let (filter, set_filter) = filter(Filter::All);
    let (items, update_items) = items(Vec::new());
    let (monotonic_id, update_monotonic_id) = monotonic_id(0);
    let monotonic_id = *monotonic_id;

    let num_completed = items.iter().filter(|item| item.completed).count();
    let num_active = items.len() - num_completed;

    let clear_completed = enclose!(update_items; move |_| {
        update_items.call(|items| {
            items.retain(|item| !item.completed);
        })
    });

    let children = items
        .iter()
        .filter(|item| {
            match filter {
                Filter::All => true,
                Filter::Completed => item.completed,
                Filter::Active => !item.completed
            }
        })
        .enumerate()
        .map(|(i, item)| {
            reactive_assert!(items => i);
            let id = item.id;
            Li! {
                // TODO: replace with then method
                class: Some(format!(
                    "{} {}",
                    if item.completed {
                        "completed"
                    } else {
                        ""
                    },
                    if *editing == Some(item.id) {
                        "editing"
                    } else {
                        ""
                    }
                )),
                key: item.id,
                children: [
                    Div!{
                        class: Some("view"),
                        children: [
                            Input!{
                                class: Some("toggle"),
                                type_: Some("checkbox"),
                                checked: item.completed,
                                on_click: enclose!(update_items; move |_| {
                                    update_items.call(|items| items[i].completed = !items[i].completed)
                                })
                            },
                            Label!{
                                child: Text!{text: item.text.clone()},
                                on_double_click: enclose!(set_editing; move |_| {
                                    set_editing.call(|editing| *editing = Some(id))
                                })
                            },
                            Button!{
                                class: Some("destroy"),
                                on_click: enclose!(update_items; move |_| {
                                    update_items.call(|items| {
                                        items.remove(i);
                                    })
                                })
                            }
                        ]
                    },
                    if *editing == Some(item.id) {
                        Input!{
                            class: Some("edit"),
                            id: Some("edit"),
                            auto_focus: true,
                            value: item.text.clone(),
                            on_key_down: enclose!(set_editing; move |e| {
                                let which = e.which();
                                if which == ENTER_KEY {
                                    e.current_target().unwrap().blur().expect("blur");
                                } else if which == ESCAPE_KEY {
                                    set_editing.call(|editing| *editing = None);
                                }
                            }),
                            on_blur: enclose!(update_items, set_editing; move |e| {
                                update_items.call(|items| items[i].text = e.current_target().unwrap().value());
                                set_editing.call(|editing| *editing = None);
                            })
                        }
                    } else {
                        ().into()
                    }
                ]
            }
        })
        .collect::<Vec<_>>();

    reactive_assert!(items => children);

    Div! {
        children: [
            Header!{
                class: Some("header"),
                children: [
                    H1!{
                        child: Text!{text: "todos"}
                    },
                    Input!{
                        class: Some("new-todo"),
                        placeholder: Some("What needs to be done?"),
                        auto_focus: true,
                        on_key_down: enclose!(update_items; move |e| {
                            if e.which() == ENTER_KEY {
                                let current_target = e.current_target().unwrap();
                                update_items.call(|items| {
                                    let new_item = Item {
                                        text: current_target.value(),
                                        completed: false,
                                        id: monotonic_id
                                    };
                                    items.push(new_item);
                                });
                                update_monotonic_id.call(|id| *id += 1);
                                current_target.set_value("");
                            }
                        })
                    }
                ]
            },
            if items.len() > 0 {
                Section!{
                    class: Some("main"),
                    children: [
                        Input!{
                            id: Some("toggle-all"),
                            class: Some("toggle-all"),
                            type_: Some("checkbox"),
                            on_change: enclose!(update_items; move |e| {
                                update_items.call(|items| {
                                    let checked = e.current_target().unwrap().checked();
                                    for item in items.iter_mut() {
                                        item.completed = checked;
                                    }
                                })
                            })
                        },
                        Label!{
                            for_: Some("toggle-all"),
                            child: Text!{text: "Mark all as complete"}
                        },

                        Ul!{
                            class: Some("todo-list"),
                            children
                        },

                        Footer!{
                            class: Some("footer"),
                            children: [
                                Span!{
                                    class: Some("todo-count"),
                                    children: [
                                        Strong! {
                                            child: Text!{text: num_active}
                                        },
                                        Text!{text: if num_active == 1 { " item" } else { " items" }}
                                    ]
                                },

                                Ul! {
                                    class: Some("filters"),
                                    children: [
                                        Li! {
                                            child: A! {
                                                class: filter.selected(Filter::All),
                                                href: Some("#/"),
                                                child: Text!{text: "All"},
                                                on_click: enclose!(set_filter; move |_| {
                                                    set_filter.call(|filter| *filter = Filter::All);
                                                })
                                            }
                                        },
                                        Li! {
                                            child: A! {
                                                class: filter.selected(Filter::Active),
                                                href: Some("#/active"),
                                                child: Text!{text: "Active"},
                                                on_click: enclose!(set_filter; move |_| {
                                                    set_filter.call(|filter| *filter = Filter::Active);
                                                })
                                            }
                                        },
                                        Li! {
                                            child: A! {
                                                class: filter.selected(Filter::Completed),
                                                href: Some("#/completed"),
                                                child: Text!{text: "Completed"},
                                                on_click: enclose!(set_filter; move |_| {
                                                    set_filter.call(|filter| *filter = Filter::Completed);
                                                })
                                            }
                                        }
                                    ]
                                },

                                if num_completed > 0 {
                                    Button!{
                                        class: Some("clear-completed"),
                                        on_click: clear_completed,
                                        child: Text!{text: "Clear completed"}
                                    }
                                } else {
                                    ().into()
                                }
                            ]
                        }
                    ]
                }
            } else {
                ().into()
            },
        ]
    }
}

// This is like the `main` function, except for JavaScript.
#[wasm_bindgen(start)]
pub fn main_js() {
    // This provides better error messages in debug mode.
    // It's disabled in release mode so it doesn't bloat up the file size.
    #[cfg(debug_assertions)]
    console_error_panic_hook::set_once();

    //TODO: the App initialization is ugly, provide a Default impl for unit struct components?
    avalanche_web::mount_to_body::<Todo>();
}
