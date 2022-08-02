mod puzzle;

use avalanche::{component, store, tracked, Tracked, View};
use avalanche_web::{
    components::{Div, Img},
    mount_to_body,
};
use puzzle::{Puzzle, TileState, Type};

fn main() {
    // This provides better error messages in debug mode.
    // It's disabled in release mode so it doesn't bloat up the file size.
    // #[cfg(debug_assertions)]
    console_error_panic_hook::set_once();

    mount_to_body::<Main>();
}

#[component]
fn Main() -> View {
    Board(self)
}

enum Msg {
    PrimaryAction { x: usize, y: usize },
    SecondaryAction { x: usize, y: usize },
}

#[component]
fn Board() -> View {
    let (puzzle, update_puzzle) = store(self, |gen| Puzzle::new(8, 8, 10, gen));

    let reducer = |msg: Msg| match msg {
        Msg::PrimaryAction { x, y } => {
            update_puzzle.update(move |puzzle, gen| {
                // TODO: handle clicking on bomb
                puzzle.click(x, y, gen);
            });
        }
        Msg::SecondaryAction { x, y } => {
            update_puzzle.update(move |puzzle, gen| {
                puzzle.toggle_flag(x, y, gen);
            });
        }
    };
    let width = tracked!(puzzle).width();
    let height = tracked!(puzzle).height();

    let tiles: Tracked<Vec<_>> = tracked!(puzzle)
        .tiles()
        .iter()
        .flatten()
        .enumerate()
        .map(|(num, tile)| {
            let x = num % tracked!(width);
            let y = num / tracked!(width);
            Tile(
                self,
                key = num.to_string(),
                tile = tracked!(tile),
                on_primary = &|| {
                    web_sys::console::log_1(&format!("{} {}", tracked!(x), tracked!(y)).into());
                    reducer(Msg::PrimaryAction {
                        x: tracked!(x),
                        y: tracked!(y),
                    })
                },
                on_secondary = &|| {
                    reducer(Msg::SecondaryAction {
                        x: tracked!(x),
                        y: tracked!(y),
                    })
                },
            )
        })
        .collect();
    let style = format!("display: grid; grid-template-rows: repeat({}, 0fr); grid-template-columns: repeat({}, 0fr);", tracked!(height), tracked!(width));
    Div(self, style = tracked!(style), tracked!(tiles))
}

/// Takes in a bomb count, returns an asset to use for it along with an alt label.
fn count_to_href(count: usize) -> (&'static str, &'static str) {
    match count {
        0 => ("assets/0.png", "0"),
        1 => ("assets/1.png", "1"),
        2 => ("assets/2.png", "2"),
        3 => ("assets/3.png", "3"),
        4 => ("assets/4.png", "4"),
        5 => ("assets/5.png", "5"),
        6 => ("assets/6.png", "6"),
        7 => ("assets/7.png", "7"),
        8 => ("assets/8.png", "8"),
        _ => unreachable!("counts should be between 0 and 8"),
    }
}

#[component]
fn Tile(tile: puzzle::Tile, on_primary: &dyn Fn(), on_secondary: &dyn Fn()) -> View {
    let (src, alt) = match tracked!(tile).state {
        TileState::Covered => ("assets/unexplored.png", "Unexplored tile"),
        TileState::Flagged => ("assets/flag.png", "Flagged tile"),
        TileState::Uncovered => match tracked!(tile).variant {
            Type::Bomb => ("assets/bomb.png", "Bomb"),
            Type::Safe { adjacent_bombs } => count_to_href(adjacent_bombs),
        },
    };

    Img(
        self,
        on_click = |_| tracked!(on_primary)(),
        on_context_menu = |e| {
            e.prevent_default();
            tracked!(on_secondary)()
        },
        src = tracked!(src),
        alt = tracked!(alt),
        width = 61,
        height = 61,
    )
}
