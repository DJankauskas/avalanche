use avalanche::{component, enclose, reactive_assert, UseState, View};
use avalanche_web::components::{Br, Div, Input, Text};
use core::sync::atomic::{AtomicUsize, Ordering};
use wasm_bindgen::prelude::*;

// When the `wee_alloc` feature is enabled, this uses `wee_alloc` as the global
// allocator.
//
// If you don't want to use `wee_alloc`, you can safely delete this.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

const ENTER_KEY: u32 = 13;

static RENDER_COUNT: AtomicUsize = AtomicUsize::new(0);
static INTERNAL_RENDERS: AtomicUsize = AtomicUsize::new(0);

#[component]
fn BunchaBois(width: usize) -> View {
    const SUB_BY: usize = 1;
    INTERNAL_RENDERS.fetch_add(1, Ordering::SeqCst);

    if *width < SUB_BY {
        return Div! {
            style: format!("border-style: solid; border-width: 2px;"),
        };
    }

    return Div! {
        style: format!("padding: 2px; border-style: solid; border-width: 2px;"),
        child: BunchaBois!{width: width - SUB_BY},
    };
}

#[component(width = UseState<usize>, trailing = UseState<usize>)]
fn BottomText() -> View {
    let (width, update_width) = width(100);
    let (trailing, update_trailing) = trailing(0);

    if *width != *trailing {
        RENDER_COUNT.fetch_add(1, Ordering::SeqCst);
        update_trailing.set(*width);
    }

    let bois = BunchaBois! {width: *width};

    return Div! {
        style: format!("float: left"),
        children: [
            Input!{
                on_key_down: enclose!(update_width; move |e| {
                    if e.which() == ENTER_KEY {
                        let current_target = e.current_target().unwrap();
                        let width = current_target.value().parse::<usize>();
                        update_width.update(move |w| {
                            if let Ok(width) = width {
                                *w = width;
                            }
                        });
                        current_target.set_value("");
                    }
                })
            },
            Br! {},
            Text!{text: format!("renders: {} (width: {})", RENDER_COUNT.load(Ordering::SeqCst), width + trailing - trailing)},
            Br! {},
            Text!{text: format!("internal: {} (trailing: {})\n", INTERNAL_RENDERS.load(Ordering::SeqCst), trailing + width - width)},
            bois,
        ]
    };
}

// This is like the `main` function, except for JavaScript.
#[wasm_bindgen(start)]
pub fn main_js() {
    // This provides better error messages in debug mode.
    // It's disabled in release mode so it doesn't bloat up the file size.
    #[cfg(debug_assertions)]
    console_error_panic_hook::set_once();

    let document = web_sys::window()
        .expect("window")
        .document()
        .expect("document");

    let body = document.query_selector("body").unwrap().unwrap();
    avalanche_web::mount::<BottomText>(body.into());
}
