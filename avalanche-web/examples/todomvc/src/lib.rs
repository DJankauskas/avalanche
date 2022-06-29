use avalanche::{component, enclose, state, tracked, updated, store, View, Tracked};
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
    fn selected(&self, other: Filter) -> &'static str {
        if *self == other {
            "selected"
        } else {
            ""
        }
    }
}

#[component]
fn Todo() -> View {
    let (editing, set_editing) = state::<Option<u32>>(self, || None);
    let (filter, set_filter) = state(self, || Filter::All);
    let (items, update_items) = store::<Vec<Tracked<Item>>>(self, |_| vec![]);
    let (monotonic_id, update_monotonic_id) = state(self, || 0);

    let monotonic_id = *tracked!(monotonic_id);

    let num_completed = tracked!(items)
        .iter()
        .filter(|item| tracked!(item).completed)
        .count();
    let num_active = tracked!(items).len() - tracked!(num_completed);

    let clear_completed = enclose!(update_items; move |_| {
        update_items.update(|items, _| {
            items.retain(|item| !tracked!(item).completed);
        })
    });

    let children = tracked!(items)
        .iter()
        .enumerate()
        .filter(|(_, item)| {
            match *tracked!(filter) {
                Filter::All => true,
                Filter::Completed => tracked!(item).completed,
                Filter::Active => !tracked!(item).completed
            }
        })
        .map(|(i, item)| {
            let id = tracked!(item).id;
            TodoItem!(
                key: tracked!(id),
                item: &tracked!(item),
                is_editing: *tracked!(editing) == Some(tracked!(id)),
                toggle_completed: &enclose!(update_items; move || {
                    updated!(items);
                    update_items.update(move |items, gen| items[i].mutate(gen).completed = !tracked!(&items[i]).completed)
                }),
                set_editing: &move |editing| {
                    set_editing.set(editing.then(|| tracked!(id)));
                },
                update_item: &enclose!(update_items; move |item| {
                    updated!(items);
                    match item {
                        Some(item) => update_items.update(move |items, gen| items[i] = Tracked::new(item, gen)),
                        None => update_items.update(move |items, _| { items.remove(i); }),
                    }
                })
                
            )
        })
        .collect::<Vec<_>>();

    Div!(
        [
            Header!(
                class: "header",
                children: [
                    H1!(
                        child: Text!("todos")
                    ),
                    Input!(
                        class: "new-todo",
                        placeholder: "What needs to be done?",
                        auto_focus: true,
                        on_key_down: enclose!(update_items; move |e| {
                            if e.which() == ENTER_KEY {
                                let current_target = e.current_target().unwrap();
                                let value = current_target.value();
                                update_items.update(move |items, _| {
                                    let new_item = Item {
                                        text: value,
                                        completed: false,
                                        id: tracked!(monotonic_id)
                                    };
                                    items.push(new_item);
                                });
                                update_monotonic_id.update(|id| *id += 1);
                                current_target.set_value("");
                            }
                        })
                    )
                ]
            ),
            (tracked!(items).len() > 0).then(|| Section!(
                class: "main",
                [
                    Input!(
                        id: "toggle-all",
                        class: "toggle-all",
                        type_: "checkbox",
                        on_change: enclose!(update_items; move |e| {
                            let checked = e.current_target().unwrap().checked();
                            update_items.update(move |items, gen| {
                                for item in items.iter_mut() {
                                    item.mutate(gen).completed = checked;
                                }
                            })
                        })
                    ),
                    Label!(
                        for_: "toggle-all",
                        child: Text!("Mark all as complete")
                    ),
                    Ul!(
                        class: "todo-list",
                        tracked!(children)
                    ),
                    Footer!(
                        class: "footer",
                        [
                            Span!(
                                class: "todo-count",
                                [
                                    Strong! (
                                        child: Text!(tracked!(num_active).to_string())
                                    ),
                                    Text!(if tracked!(num_active) == 1 { " item" } else { " items" })
                                ]
                            ),
                            Ul!(
                                class: "filters",
                                [
                                    Li!(
                                        child: A!(
                                            class: tracked!(filter).selected(Filter::All),
                                            href: "#/",
                                            child: Text!("All"),
                                            on_click: move |_| {
                                                set_filter.set(Filter::All);
                                            }
                                        )
                                    ),
                                    Li!(
                                        child: A!(
                                            class: tracked!(filter).selected(Filter::Active),
                                            href: "#/active",
                                            child: Text!("Active"),
                                            on_click: move |_| {
                                                set_filter.set(Filter::Active);
                                            }
                                        )
                                    ),
                                    Li!(
                                        child: A!(
                                            class: tracked!(filter).selected(Filter::Completed),
                                            href: "#/completed",
                                            child: Text!("Completed"),
                                            on_click: move |_| {
                                                set_filter.set(Filter::Completed);
                                            }
                                        )
                                    )
                                ]
                            ),

                            (tracked!(num_completed) > 0).then(|| Button!(
                                class: "clear-completed",
                                on_click: clear_completed,
                                child: Text!("Clear completed")
                            )).into()
                        ]
                    )
                ]
            )).into()
        ]
    )
}

#[component]
fn TodoItem(item: &Item, is_editing: bool, toggle_completed: &dyn Fn(), set_editing: &dyn Fn(bool), update_item: &dyn Fn(Option<Item>)) -> View {
    Li!(
        class: format!(
            "{} {}",
            if tracked!(item).completed {
                "completed"
            } else {
                ""
            },
            if tracked!(is_editing) {
                "editing"
            } else {
                ""
            }
        ),
        [
            Div!(
                class: "view",
                [
                    Input!(
                        class: "toggle",
                        type_: "checkbox",
                        checked: tracked!(item).completed,
                        on_click: move |_| {
                            tracked!(toggle_completed)();
                        }
                    ),
                    Label!(
                        child: Text!(&tracked!(item).text),
                        on_double_click: move |_| {
                            tracked!(set_editing)(true);
                        }
                    ),
                    Button!(
                        class: "destroy",
                        on_click: move |_| {
                            tracked!(update_item)(None);
                        }
                    )
                ]
            ),
            tracked!(is_editing).then(|| Input!(
                class: "edit",
                id: "edit",
                auto_focus: true,
                value: &tracked!(item).text,
                on_key_down: move |e| {
                    let which = e.which();
                    if which == ENTER_KEY {
                        e.current_target().unwrap().blur().expect("blur");
                    } else if which == ESCAPE_KEY {
                        tracked!(set_editing)(false);
                    }
                },
                on_blur: move |e| {
                    let item = Item {
                        text: e.current_target().unwrap().value(),
                        ..tracked!(item).clone()
                    };
                    tracked!(update_item)(Some(tracked!(item)));
                    tracked!(set_editing)(false);
                }
            )).into()
        ]
    )
}

// This is like the `main` function, except for JavaScript.
#[wasm_bindgen(start)]
pub fn main_js() {
    // This provides better error messages in debug mode.
    // It's disabled in release mode so it doesn't bloat up the file size.
    #[cfg(debug_assertions)]
    console_error_panic_hook::set_once();

    avalanche_web::mount::<Todo>(
        web_sys::window()
            .expect("window")
            .document()
            .expect("document")
            .query_selector(".todoapp")
            .expect("body")
            .unwrap()
    );
}
