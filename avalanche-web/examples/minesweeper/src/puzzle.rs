use std::collections::VecDeque;

use avalanche::tracked;
use avalanche::tracked::{Gen, Tracked};
use rand::{seq::SliceRandom, thread_rng};

#[derive(Clone, Copy, Debug)]
pub struct Tile {
    pub state: TileState,
    pub variant: Type,
}

impl Default for Tile {
    fn default() -> Self {
        Self {
            state: TileState::Covered,
            variant: Type::Safe { adjacent_bombs: 0 },
        }
    }
}

impl Tile {
    /// Returns 1 if the tile is a bomb, 0 otherwise
    fn bomb_count(self) -> usize {
        matches!(self.variant, Type::Bomb) as usize
    }
}

#[derive(Copy, Clone, Debug)]
pub enum TileState {
    Covered,
    Flagged,
    Uncovered,
}

#[derive(Copy, Clone, Debug)]
pub enum Type {
    Bomb,
    Safe { adjacent_bombs: usize },
}

pub struct Puzzle {
    data: Vec<Vec<Tracked<Tile>>>,
    bomb_count: isize,
    been_clicked: bool,
    width: usize,
    height: usize,
}

pub struct BombDetonated;

impl Puzzle {
    pub fn new(width: usize, height: usize, bomb_count: usize, gen: Gen) -> Self {
        let mut puzzle = Vec::with_capacity(width);
        for _ in 0..width {
            puzzle.push(vec![Tracked::new(Tile::default(), gen); height]);
        }

        Puzzle {
            data: puzzle,
            bomb_count: bomb_count as isize,
            been_clicked: false,
            width,
            height,
        }
    }

    pub fn tiles(&self) -> &[Vec<Tracked<Tile>>] {
        &self.data
    }

    pub fn _bomb_count(&self) -> isize {
        self.bomb_count
    }

    pub fn width(&self) -> usize {
        self.width
    }

    pub fn height(&self) -> usize {
        self.height
    }

    /// Returns Some(BombDetonated) if bomb(s) were pressed
    /// TODO: support clicking on numbers
    pub fn click(&mut self, x: usize, y: usize, gen: Gen) -> Option<BombDetonated> {
        if !self.been_clicked {
            populate_puzzle(self, (x, y), gen);
            self.been_clicked = true;
        }
        let tile = tracked!(self.data[x][y]);

        match tile.state {
            TileState::Covered => match tile.variant {
                Type::Bomb => return Some(BombDetonated),
                Type::Safe { .. } => {
                    // self.data[x][y].mutate(gen).state = TileState::Uncovered;
                    let mut to_uncover = VecDeque::with_capacity(16);
                    to_uncover.push_back((x, y));
                    while let Some((x, y)) = to_uncover.pop_front() {
                        let tile = tracked!(self.data[x][y]);
                        if let Type::Safe { adjacent_bombs } = tile.variant {
                            if matches!(tile.state, TileState::Covered) {
                                self.data[x][y].mutate(gen).state = TileState::Uncovered;
                                if adjacent_bombs == 0 {
                                    to_uncover.extend(self.neighbors(x, y));
                                }
                            }
                        }
                    }
                }
            },
            TileState::Flagged => {}
            TileState::Uncovered => { /* TODO: handle number clicking */ }
        }

        None
    }

    /// Adds neighbors of the given tile that meet the given condition.
    fn neighbors(
        &self,
        x: usize,
        y: usize,
    ) -> impl Iterator<Item = (usize, usize)> + DoubleEndedIterator + ExactSizeIterator + Clone
    {
        let x = x as isize;
        let y = y as isize;
        let potential_positions = [
            (x - 1, y - 1),
            (x - 1, y),
            (x - 1, y + 1),
            (x, y - 1),
            (x, y + 1),
            (x + 1, y - 1),
            (x + 1, y),
            (x + 1, y + 1),
        ];

        let mut neighbors = Vec::with_capacity(8);

        for (x, y) in potential_positions {
            if self.try_tile(x, y).is_some() {
                neighbors.push((x as usize, y as usize))
            }
        }

        neighbors.into_iter()
    }

    pub fn toggle_flag(&mut self, x: usize, y: usize, gen: Gen) {
        let state = &mut self.data[x][y].mutate(gen).state;
        *state = match state {
            TileState::Covered => TileState::Flagged,
            TileState::Flagged => TileState::Covered,
            TileState::Uncovered => TileState::Uncovered,
        }
    }

    fn try_tile(&self, x: isize, y: isize) -> Option<Tile> {
        if x < 0 || y < 0 {
            return None;
        }
        let x = x as usize;
        let y = y as usize;
        self.data
            .get(x)
            .and_then(|row| row.get(y))
            .map(|tile| tracked!(tile))
    }

    /// Gets the tile at `x` and `y`, but if those are out of
    /// bounds, returns `Tile::default` instead.
    fn tile_or_default(&self, x: isize, y: isize) -> Tile {
        self.try_tile(x, y).unwrap_or_default()
    }
}

fn populate_puzzle(puzzle: &mut Puzzle, (clicked_x, clicked_y): (usize, usize), gen: Gen) {
    let num_tiles = puzzle.width * puzzle.height;
    let mut positions = Vec::with_capacity(num_tiles);
    for x in 0..puzzle.width {
        for y in 0..puzzle.height {
            if (x, y) != (clicked_x, clicked_y) {
                positions.push((x, y));
            }
        }
    }

    let mut rng = thread_rng();

    for (x, y) in positions.choose_multiple(&mut rng, puzzle.bomb_count as usize) {
        puzzle.data[*x][*y].mutate(gen).variant = Type::Bomb;
    }
    // Set adjacent bomb counts.
    for x in 0..puzzle.width {
        for y in 0..puzzle.height {
            let x = x as isize;
            let y = y as isize;

            let mut count = 0;
            count += puzzle.tile_or_default(x - 1, y - 1).bomb_count();
            count += puzzle.tile_or_default(x - 1, y).bomb_count();
            count += puzzle.tile_or_default(x - 1, y + 1).bomb_count();
            count += puzzle.tile_or_default(x, y - 1).bomb_count();
            count += puzzle.tile_or_default(x, y + 1).bomb_count();
            count += puzzle.tile_or_default(x + 1, y - 1).bomb_count();
            count += puzzle.tile_or_default(x + 1, y).bomb_count();
            count += puzzle.tile_or_default(x + 1, y + 1).bomb_count();
            if let Type::Safe { adjacent_bombs } =
                &mut puzzle.data[x as usize][y as usize].mutate(gen).variant
            {
                *adjacent_bombs = count;
            }
        }
    }
}
