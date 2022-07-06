use avalanche::{component, state, tracked, View};
use avalanche_web::components::{Button, Div, Text, H2};
use wasm_bindgen::prelude::*;

// When the `wee_alloc` feature is enabled, this uses `wee_alloc` as the global
// allocator.
//
// If you don't want to use `wee_alloc`, you can safely delete this.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[component]
fn Counter() -> View {
    let (count, set_count) = state(self, || 0);
    Div(
        self,
        [
            H2(self, child = Text(self, "Counter!")),
            Button(
                self,
                on_click = move |_| set_count.update(|count| *count += 1),
                child = Text(self, "+"),
            ),
            Text(self, tracked!(count).to_string()),
        ],
    )
}

// This is like the `main` function, except for JavaScript.
#[wasm_bindgen(start)]
pub fn main_js() {
    // This provides better error messages in debug mode.
    // It's disabled in release mode so it doesn't bloat up the file size.
    #[cfg(debug_assertions)]
    console_error_panic_hook::set_once();

    avalanche_web::mount_to_body::<Counter>();
}
