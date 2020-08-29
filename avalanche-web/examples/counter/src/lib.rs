use wasm_bindgen::prelude::*;
use avalanche::{component, UseState};
use avalanche_web::{Text, Div, Button, H2};

// When the `wee_alloc` feature is enabled, this uses `wee_alloc` as the global
// allocator.
//
// If you don't want to use `wee_alloc`, you can safely delete this.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[component(count = UseState<i64>)]
fn App() {
    let (count, set_count) = count(0);
    Div! {
        children: [
            H2!{
                child: Text!{text: "Counter!"}
            },
            Button!{
                on_click: move |_| set_count.call(|count| *count += 1),
                child: Text!{text: "+"}
            },
            Text!{text: count}
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
    avalanche_web::mount_to_body(<<App as avalanche::Component>::Builder>::new().build().into());
}
