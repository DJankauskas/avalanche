use avalanche::{component, UseState, View, tracked};
use avalanche_web::components::{Button, Div, Text, H2};
use wasm_bindgen::prelude::*;

// When the `wee_alloc` feature is enabled, this uses `wee_alloc` as the global
// allocator.
//
// If you don't want to use `wee_alloc`, you can safely delete this.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[component(count = UseState<u64>)]
fn Counter() -> View {
    let (count, set_count) = count(0);
    Div!([
        H2!(
            child: Text!("Counter!"),
        ),
        Button!(
            on_click: move |_| set_count.update(|count| *count += 1),
            child: Text!("+")
        ),
        Text!(count)
    ])
}

// This is like the `main` function, except for JavaScript.
#[wasm_bindgen(start)]
pub fn main_js() {
    avalanche_web::mount_to_body::<Counter>();
}
