use avalanche::{component, reactive_assert, enclose, UseState};
use avalanche_web::components::{Button, Div, Input, Text, H2};
use wasm_bindgen::prelude::*;
use wasm_bindgen::{JsCast};
use web_sys::{HtmlInputElement};

// When the `wee_alloc` feature is enabled, this uses `wee_alloc` as the global
// allocator.
//
// If you don't want to use `wee_alloc`, you can safely delete this.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[derive(Debug, Clone)]
struct Item {
    text: String,
    id: u32,
}

#[component(text = UseState<String>, items = UseState<Vec<Item>>, monotonic_id = UseState<u32>)]
fn Todo() {
    let (text, set_text) = text(String::new());
    let (items, update_items) = items(Vec::new());
    let (monotonic_id, update_monotonic_id) = monotonic_id(0);
    let monotonic_id = *monotonic_id;

    let children = items
        .iter()
        .enumerate()
        .map(|(i, item)| {
            reactive_assert!(items => i);
            Div!{
                children: [
                    Text! {
                        text: "Item ".to_owned() + &item.text,
                    },
                    Button!{
                        child: Text!{
                            text: "x"
                        },
                        on_click: enclose!(update_items; move |_| {
                            update_items.call(|items| {
                                items.remove(i);
                            });
                        })
                    },
                ],
                key: item.id
            }
        })
        .collect::<Vec<_>>();

    reactive_assert!(items => children);

    Div! {
        children: [
            H2!{
                child: Text!{text: "Todo!"},
            },
            Input!{
                value: text.to_owned(),
                on_input: enclose!(set_text; move |e| {
                    let input = e.current_target().unwrap().dyn_into::<HtmlInputElement>().unwrap();
                    set_text.call(|text| *text = input.value());
                })
            },
            Div!{
                children: [
                    Text!{text: "id: "},
                    Text!{text: monotonic_id},
                    Text!{text: " text: "},
                    Text!{text: text.clone()}
                ]
            },
            Div!{
                style: Some("display: flex; flex-direction: column;".to_owned()),
                children
            },
            Button!{
                // on_click: move |_| set_count.call(|count| *count += 1),
                child: Text!{text: "Create"},
                on_click: enclose!(text, update_items; move |_| {
                    let text = text.clone();
                    update_items.call(|items| items.push(Item {
                        text,
                        id: monotonic_id
                    }));
                    set_text.call(|text| text.clear());
                    update_monotonic_id.call(|id| *id += 1);
                })
            },
            Button!{
                child: Text!{text: "Sort alphabetically"},
                on_click: move |_| {
                    update_items.call(|items| {
                        items.sort_by(|a, b| a.text.cmp(&b.text))
                    })
                }
            }
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
    avalanche_web::mount_to_body(
        <<Todo as avalanche::Component>::Builder>::new()
            .build((line!(), column!()))
            .into(),
    );
}
