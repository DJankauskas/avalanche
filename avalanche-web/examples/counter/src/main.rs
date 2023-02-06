use avalanche::{component, state, tracked, View};
use avalanche_web::components::{Button, Div, Text, H2};

#[component]
fn Counter() -> View {
    let (count, set_count) = state(self, || 0);
    Div(
        self,
        [
            H2(self, child = Text(self, "Counter!")),
            Button(
                self,
                on_click = |_| set_count.update(|count| *count += 1),
                child = Text(self, "+"),
            ),
            Text(self, tracked!(count).to_string()),
        ],
    )
}

pub fn main() {
    // This provides better error messages in debug mode.
    // It's disabled in release mode so it doesn't bloat up the file size.
    #[cfg(debug_assertions)]
    console_error_panic_hook::set_once();

    avalanche_web::mount_to_body::<Counter>();
}
