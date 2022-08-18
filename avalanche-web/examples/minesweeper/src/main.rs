mod puzzle;

use std::fmt::Display;

use avalanche::{component, state, store, tracked, Tracked, View};
use avalanche_web::{
    components::{Button, Div, Img, Text},
    mount_to_body,
};
use puzzle::{GameStatus, Puzzle, TileState, TileVariant};

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

#[allow(unused)]
#[derive(Clone, Copy, Debug)]
enum PuzzleType {
    Beginner,
    Intermediate,
    Expert,
}

impl PuzzleType {
    /// Returns the width, height, and starting bomb count of a puzzle size.
    fn size(self) -> (usize, usize, usize) {
        match self {
            PuzzleType::Beginner => (8, 8, 10),
            PuzzleType::Intermediate => (16, 16, 40),
            PuzzleType::Expert => (30, 16, 99),
        }
    }
}

impl Display for PuzzleType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            PuzzleType::Beginner => "Beginner",
            PuzzleType::Intermediate => "Intermediate",
            PuzzleType::Expert => "Expert",
        })
    }
}

enum Msg {
    PrimaryAction { x: usize, y: usize },
    SecondaryAction { x: usize, y: usize },
    NewGame(PuzzleType),
}

fn reset_text(status: GameStatus) -> &'static str {
    match status {
        GameStatus::New => "Reset",
        GameStatus::InProgress => "Reset",
        GameStatus::Won => "New game",
        GameStatus::Lost => "New game",
    }
}

// TODO: implement selecting different puzzle sizes
#[component]
fn Board() -> View {
    let (puzzle, update_puzzle) = store(self, |gen| Puzzle::new(8, 8, 10, gen));
    let (status, set_status) = state(self, || GameStatus::New);
    let (puzzle_type, _) = state(self, || PuzzleType::Beginner);

    let reducer = |msg: Msg| match msg {
        Msg::PrimaryAction { x, y } => {
            if tracked!(status).puzzle_ended() {
                return;
            };
            let set_state = set_status.clone();
            update_puzzle.update(move |puzzle, gen| {
                set_state.set(puzzle.click(x, y, gen));
            });
        }
        Msg::SecondaryAction { x, y } => {
            if tracked!(status).puzzle_ended() {
                return;
            };
            update_puzzle.update(move |puzzle, gen| {
                puzzle.toggle_flag(x, y, gen);
            });
        }
        Msg::NewGame(puzzle_type) => {
            let (width, height, mine_count) = puzzle_type.size();
            update_puzzle
                .update(move |puzzle, gen| *puzzle = Puzzle::new(width, height, mine_count, gen));
            set_status.set(GameStatus::New);
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
    let style = format!("display: grid; grid-template-rows: repeat({}, 0fr); grid-template-columns: repeat({}, 0fr); grid-auto-layout: column;", tracked!(height), tracked!(width));
    Div(
        self,
        [
            Div(
                self,
                [Button(
                    self,
                    on_click = |_| reducer(Msg::NewGame(*tracked!(puzzle_type))),
                    [Text(self, reset_text(*tracked!(status)))],
                )],
            ),
            true.then(|| Div(self)).into(),
            Text(
                self,
                format!("{:?}, {}", tracked!(status), tracked!(puzzle).mine_count()),
            ),
            Div(self, style = tracked!(style), tracked!(tiles)),
        ],
    )
}

/// Takes in a mine count, returns an asset to use for it along with an alt label.
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
            TileVariant::Mine => ("assets/mine_hit.png", "Mine"),
            TileVariant::Safe { adjacent_mines } => count_to_href(adjacent_mines),
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
