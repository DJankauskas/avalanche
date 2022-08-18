use std::collections::VecDeque;

use avalanche::tracked;
use avalanche::tracked::{Gen, Tracked};
use rand::{seq::SliceRandom, thread_rng};

#[derive(Clone, Copy, Debug)]
pub struct Tile {
    pub state: TileState,
    pub variant: TileVariant,
}

impl Default for Tile {
    fn default() -> Self {
        Self {
            state: TileState::Covered,
            variant: TileVariant::Safe { adjacent_mines: 0 },
        }
    }
}

impl Tile {
    /// Returns 1 if the tile is a mine, 0 otherwise
    fn mine_count(self) -> usize {
        if matches!(self.variant, TileVariant::Mine) {
            1
        } else {
            0
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum TileState {
    Covered,
    Flagged,
    Uncovered,
}

#[derive(Copy, Clone, Debug)]
pub enum TileVariant {
    Mine,
    Safe { adjacent_mines: usize },
}

pub struct Puzzle {
    data: Vec<Vec<Tracked<Tile>>>,
    unflagged_mine_count: isize,
    total_mine_count: usize,
    num_unclicked: usize,
    num_covered: usize,
    been_clicked: bool,
    width: usize,
    height: usize,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum GameStatus {
    New,
    InProgress,
    Won,
    Lost,
}

impl GameStatus {
    pub fn puzzle_ended(self) -> bool {
        matches!(self, GameStatus::Won | GameStatus::Lost)
    }
}

impl Puzzle {
    pub fn new(width: usize, height: usize, mine_count: usize, gen: Gen) -> Self {
        let mut puzzle = Vec::with_capacity(width);
        for _ in 0..height {
            puzzle.push(vec![Tracked::new(Tile::default(), gen); width]);
        }

        Puzzle {
            data: puzzle,
            unflagged_mine_count: mine_count as isize,
            total_mine_count: mine_count,
            num_unclicked: width * height,
            num_covered: width * height,
            been_clicked: false,
            width,
            height,
        }
    }
    
    pub fn tiles(&self) -> &[Vec<Tracked<Tile>>] {
        &self.data
    }

    pub fn mine_count(&self) -> isize {
        self.unflagged_mine_count
    }

    pub fn width(&self) -> usize {
        self.width
    }

    pub fn height(&self) -> usize {
        self.height
    }

    /// Returns Some(MineDetonated) if mine(s) were pressed
    pub fn click(&mut self, x: usize, y: usize, gen: Gen) -> GameStatus {
        if !self.been_clicked {
            populate_puzzle(self, (x, y), gen);
            self.been_clicked = true;
        }
        let tile = tracked!(self.data[y][x]);

        match tile.state {
            TileState::Covered => {
                match tile.variant {
                    TileVariant::Mine => {
                        self.num_unclicked -= 1;
                        self.num_covered -= 1;
                        self.data[y][x].mutate(gen).state = TileState::Uncovered;
                        return GameStatus::Lost;
                    }
                    TileVariant::Safe { .. } => {
                        // self.data[x][y].mutate(gen).state = TileState::Uncovered;
                        let mut to_uncover = VecDeque::with_capacity(16);
                        to_uncover.push_back((x, y));
                        while let Some((x, y)) = to_uncover.pop_front() {
                            let tile = tracked!(self.data[y][x]);
                            if let TileVariant::Safe { adjacent_mines } = tile.variant {
                                if matches!(tile.state, TileState::Covered) {
                                    self.data[y][x].mutate(gen).state = TileState::Uncovered;
                                    self.num_unclicked -= 1;
                                    self.num_covered -= 1;
                                    if adjacent_mines == 0 {
                                        to_uncover.extend(self.neighbors(x, y));
                                    }
                                }
                            }
                        }
                    }
                }
            }
            TileState::Flagged => {}
            TileState::Uncovered => {
                if let TileVariant::Safe { adjacent_mines } = tile.variant {
                    if adjacent_mines > 0 {
                        let neighbors = self.neighbors(x, y);
                        let mut num_flagged = 0;
                        for (x, y) in &neighbors {
                            if let TileState::Flagged = tracked!(self.data[*y][*x]).state {
                                num_flagged += 1;
                            }
                        }
                        if num_flagged >= adjacent_mines {
                            let mut game_status = GameStatus::InProgress;
                            for (x, y) in neighbors {
                                if let TileState::Covered = tracked!(self.data[y][x]).state {
                                    game_status = std::cmp::max(game_status, self.click(x, y, gen));
                                }
                            }
                            return game_status;
                        }
                    }
                }
            }
        }

        // All the remaining unchecked tiles must be mines, so we flag them and end the game
        if self.num_covered as isize == self.unflagged_mine_count {
            for x in 0..self.width {
                for y in 0..self.height {
                    if let TileState::Covered = tracked!(self.data[y][x]).state {
                        self.toggle_flag(x, y, gen);
                    }
                }
            }
        }

        if self.num_unclicked == self.total_mine_count {
            GameStatus::Won
        } else {
            GameStatus::InProgress
        }
    }

    /// Adds neighbors of the given tile that meet the given condition.
    fn neighbors(&self, x: usize, y: usize) -> Vec<(usize, usize)> {
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

        neighbors
    }

    pub fn toggle_flag(&mut self, x: usize, y: usize, gen: Gen) {
        let state = &mut self.data[y][x].mutate(gen).state;
        *state = match state {
            TileState::Covered => {
                self.num_covered -= 1;
                self.unflagged_mine_count -= 1;
                TileState::Flagged
            }
            TileState::Flagged => {
                self.num_covered += 1;
                self.unflagged_mine_count += 1;
                TileState::Covered
            }
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
            .get(y)
            .and_then(|row| row.get(x))
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

    for (x, y) in positions.choose_multiple(&mut rng, puzzle.unflagged_mine_count as usize) {
        puzzle.data[*y][*x].mutate(gen).variant = TileVariant::Mine;
    }
    // Set adjacent mine counts.
    for x in 0..puzzle.width {
        for y in 0..puzzle.height {
            let x = x as isize;
            let y = y as isize;

            let mut count = 0;
            count += puzzle.tile_or_default(x - 1, y - 1).mine_count();
            count += puzzle.tile_or_default(x - 1, y).mine_count();
            count += puzzle.tile_or_default(x - 1, y + 1).mine_count();
            count += puzzle.tile_or_default(x, y - 1).mine_count();
            count += puzzle.tile_or_default(x, y + 1).mine_count();
            count += puzzle.tile_or_default(x + 1, y - 1).mine_count();
            count += puzzle.tile_or_default(x + 1, y).mine_count();
            count += puzzle.tile_or_default(x + 1, y + 1).mine_count();
            if let TileVariant::Safe { adjacent_mines } =
                &mut puzzle.data[y as usize][x as usize].mutate(gen).variant
            {
                *adjacent_mines = count;
            }
        }
    }
}
