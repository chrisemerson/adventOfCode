use std::cmp::max;
use std::cmp::min;
use std::collections::HashSet;
use std::fs;

#[derive(Hash, PartialEq)]
struct Tile {
    x: i32,
    y: i32
}

impl std::cmp::Eq for Tile {}

fn main() {
    let input = fs::read_to_string("input.txt")
        .expect("Something went wrong reading the file");

    let mut flipped_tiles:HashSet<Tile> = HashSet::new();

    for line in input.lines() {
        let coords = get_tile_coords_from_path(line.to_string());

        if !flipped_tiles.contains(&coords) {
            flipped_tiles.insert(coords);
        } else {
            flipped_tiles.remove(&coords);
        }
    }

    println!("Number of flipped tiles: {}", flipped_tiles.len());

    for _ in 0 .. 100 {
        flipped_tiles = get_new_tile_state(&flipped_tiles);
    }

    println!("Number of flipped tiles after 100 times: {}", flipped_tiles.len());
}

fn get_tile_coords_from_path(path: String) -> Tile {
    if path == "" {
        return Tile { x: 0, y: 0 }
    }

    let mut remaining_path = path;
    let mut x = 0;
    let mut y = 0;

    loop {
        if remaining_path == "" {
            return Tile{ x, y };
        } else if &remaining_path[0 .. 1] == "e" {
            x += 1;
            remaining_path = remaining_path[1 ..].to_string();
        } else if &remaining_path[0 .. 1] == "w" {
            x -= 1;
            remaining_path = remaining_path[1 ..].to_string();
        } else if &remaining_path[0 .. 1] == "n" || &remaining_path[0 .. 1] == "s" {
            if &remaining_path[0 .. 2] == "nw" {
                if y % 2 == 0 {
                    x -= 1;
                }

                y -= 1;

                remaining_path = remaining_path[2 ..].to_string();
            } else if &remaining_path[0 .. 2] == "sw" {
                if y % 2 == 0 {
                    x -= 1;
                }

                y += 1;

                remaining_path = remaining_path[2 ..].to_string();
            } else if &remaining_path[0 .. 2] == "ne" {
                if y % 2 != 0 {
                    x += 1;
                }

                y -= 1;

                remaining_path = remaining_path[2 ..].to_string();
            } else if &remaining_path[0 .. 2] == "se" {
                if y % 2 != 0 {
                    x += 1;
                }

                y += 1;

                remaining_path = remaining_path[2 ..].to_string();
            }
        }
    }
}

fn get_new_tile_state(current_tile_state: &HashSet<Tile>) -> HashSet<Tile> {
    let mut new_tile_state:HashSet<Tile> = HashSet::new();
    let (min_x, max_x, min_y, max_y) = get_min_max_x_y(current_tile_state);

    for y in min_y - 1 .. max_y + 2 {
        for x in min_x - 2 .. max_x + 3 {
            let flipped_neighbours = get_adjacent_black_tiles(current_tile_state, x, y);

            if current_tile_state.contains(&Tile {x, y}) && flipped_neighbours == 1 {
                new_tile_state.insert(Tile {x, y});
            }

            if flipped_neighbours == 2 {
                new_tile_state.insert(Tile {x, y});
            }
        }
    }

    return new_tile_state;
}

fn get_min_max_x_y(tiles: &HashSet<Tile>) -> (i32, i32, i32, i32) {
    let mut min_x = 0;
    let mut max_x = 0;
    let mut min_y = 0;
    let mut max_y = 0;

    for tile in tiles {
        min_x = min(min_x, tile.x);
        max_x = max(max_x, tile.x);
        min_y = min(min_y, tile.y);
        max_y = max(max_y, tile.y);
    }

    return (min_x, max_x, min_y, max_y);
}

fn get_adjacent_black_tiles(tiles: &HashSet<Tile>, x: i32, y: i32) -> i32 {
    let mut adjacent_black_tiles:i32 = 0;

    if tiles.contains(&Tile {x: x - 1, y}) {
        adjacent_black_tiles += 1;
    }

    if tiles.contains(&Tile {x: x + 1, y}) {
        adjacent_black_tiles += 1;
    }

    if y % 2 == 0 {
        if tiles.contains(&Tile {x: x - 1, y: y - 1 }) {
            adjacent_black_tiles += 1;
        }

        if tiles.contains(&Tile {x, y: y - 1 }) {
            adjacent_black_tiles += 1;
        }

        if tiles.contains(&Tile {x: x - 1, y: y + 1 }) {
            adjacent_black_tiles += 1;
        }

        if tiles.contains(&Tile {x, y: y + 1 }) {
            adjacent_black_tiles += 1;
        }
    } else {
        if tiles.contains(&Tile {x, y: y - 1 }) {
            adjacent_black_tiles += 1;
        }

        if tiles.contains(&Tile {x: x + 1, y: y - 1 }) {
            adjacent_black_tiles += 1;
        }

        if tiles.contains(&Tile {x, y: y + 1 }) {
            adjacent_black_tiles += 1;
        }

        if tiles.contains(&Tile {x: x + 1, y: y + 1 }) {
            adjacent_black_tiles += 1;
        }
    }

    return adjacent_black_tiles;
}