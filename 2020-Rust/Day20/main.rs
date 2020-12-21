use std::fs;
use std::cmp::min;

#[derive(Debug, PartialEq, Copy, Clone)]
enum PieceType {
    Unknown,
    Corner,
    Edge,
    Middle
}

#[derive(Debug)]
struct Tile {
    id: u32,
    contents: Vec<String>,

    piece_type: PieceType,

    top_edge_code: u32,
    bottom_edge_code: u32,
    left_edge_code: u32,
    right_edge_code: u32
}

fn main() {
    let input = fs::read_to_string("input.txt")
        .expect("Something went wrong reading the file");

    let tiles = determine_piece_types(
        &parse_input_into_tiles(input)
    );

    let product_of_corners = &tiles
        .iter()
        .filter(|t| t.piece_type == PieceType::Corner)
        .map(|t| t.id)
        .fold(1, |a, b| a * b as i64);

    println!("Product of corner pieces is {}", product_of_corners);

    let puzzle = solve_puzzle(&tiles);
    let mut flattened_puzzle = flatten_puzzle(&puzzle);

    let sea_monster = vec!["                  # ".to_string(),
                           "#    ##    ##    ###".to_string(),
                           " #  #  #  #  #  #   ".to_string()];

    let no_of_hashes:i32 = flattened_puzzle
        .iter()
        .map(|line| {
            line
                .chars()
                .filter(|c| c == &'#')
                .count() as i32
        })
        .fold(0, |a, b| a + b);

    let mut sea_monsters_found = false;

    for _ in 0..2 {
        for _ in 0..4 {
            // print_flattened_puzzle(&flattened_puzzle);
            let no_of_sea_monsters = find_no_of_sea_monsters(&flattened_puzzle, &sea_monster);

            if no_of_sea_monsters > 0 {
                println!("Sea monsters ahoy! Found {}, so rough sea factor is: {}", no_of_sea_monsters, no_of_hashes - (15 * no_of_sea_monsters));
                sea_monsters_found = true;
            }

            flattened_puzzle = rotate_contents_clockwise(flattened_puzzle);
        }

        flattened_puzzle = flip_contents_vertically(flattened_puzzle);
    }

    if !sea_monsters_found {
        println!("Didn't find any sea monsters :(");
    }
}

fn find_no_of_sea_monsters(grid: &Vec<String>, sea_monster: &Vec<String>) -> i32 {
    let mut sea_monsters_found = 0;

    for grid_y in 0 .. grid.len() - sea_monster.len() + 1 {
        'startingpos: for grid_x in 0 .. grid.get(grid_y).unwrap().len() - sea_monster.get(0).unwrap().len() + 1 {
            for sm_y in 0 .. sea_monster.len() {
                for sm_x in 0 .. sea_monster.get(sm_y).unwrap().len() {
                    let sm_cell = sea_monster.get(sm_y).unwrap().chars().collect::<Vec<char>>()[sm_x];
                    let grid_cell = grid.get(grid_y + sm_y).unwrap().chars().collect::<Vec<char>>()[grid_x + sm_x];

                    if sm_cell == '#' && grid_cell != '#' {
                        continue 'startingpos;
                    }
                }
            }

            sea_monsters_found += 1;
        }
    }

    return sea_monsters_found;
}

fn solve_puzzle(tiles: &Vec<Tile>) -> Vec<Vec<Tile>> {
    let mut puzzle:Vec<Vec<Tile>> = Vec::new();
    let square_size = 12;

    let mut top_row: Vec<Tile> = Vec::new();
    let top_left_corner = find_and_orient_top_left_corner(&tiles);
    let mut exposed_right_edge = top_left_corner.right_edge_code;
    let mut last_tile_id = top_left_corner.id;

    top_row.push(top_left_corner);

    for i in 0..square_size - 1 {
        let mut piece_type = PieceType::Edge;

        if i == square_size - 2 {
            piece_type = PieceType::Corner;
        }

        let next_tile = find_and_orient_piece_with_left_and_top_edge(&tiles, piece_type, Some(&exposed_right_edge), None, &last_tile_id);

        exposed_right_edge = next_tile.right_edge_code;
        last_tile_id = next_tile.id;
        top_row.push(next_tile);
    }

    let mut top_tile_ids:Vec<u32> = Vec::new();
    let mut top_tile_bottom_edges:Vec<u32> = Vec::new();

    for tile in &top_row {
        top_tile_ids.push(tile.id);
        top_tile_bottom_edges.push(tile.bottom_edge_code);
    }

    puzzle.push(top_row);

    for _ in 0..square_size - 2 {
        let mut row: Vec<Tile> = Vec::new();

        let left_tile = find_and_orient_piece_with_left_and_top_edge(&tiles, PieceType::Edge, None, Some(&top_tile_bottom_edges.get(0).unwrap()), &top_tile_ids.get(0).unwrap());
        let mut exposed_right_edge = left_tile.right_edge_code;
        let mut last_tile_id = left_tile.id;

        row.push(left_tile);

        for j in 0..square_size - 1 {
            let exposed_bottom_edge = top_tile_bottom_edges.get(j + 1).unwrap();
            let mut piece_type = PieceType::Middle;

            if j == square_size - 2 {
                piece_type = PieceType::Edge;
            }

            let next_tile = find_and_orient_piece_with_left_and_top_edge(&tiles, piece_type, Some(&exposed_right_edge), Some(&exposed_bottom_edge), &last_tile_id);

            exposed_right_edge = next_tile.right_edge_code;
            last_tile_id = next_tile.id;
            row.push(next_tile);
        }

        top_tile_ids.clear();
        top_tile_bottom_edges.clear();

        for tile in &row {
            top_tile_ids.push(tile.id);
            top_tile_bottom_edges.push(tile.bottom_edge_code);
        }

        puzzle.push(row);
    }

    let mut bottom_row: Vec<Tile> = Vec::new();

    let bottom_left_tile = find_and_orient_piece_with_left_and_top_edge(&tiles, PieceType::Corner, None, Some(&top_tile_bottom_edges.get(0).unwrap()), &top_tile_ids.get(0).unwrap());
    let mut exposed_right_edge = bottom_left_tile.right_edge_code;
    let mut last_tile_id = bottom_left_tile.id;

    bottom_row.push(bottom_left_tile);

    for j in 0..square_size - 1 {
        let exposed_bottom_edge = top_tile_bottom_edges.get(j + 1).unwrap();
        let mut piece_type = PieceType::Edge;

        if j == square_size - 2 {
            piece_type = PieceType::Corner;
        }

        let next_tile = find_and_orient_piece_with_left_and_top_edge(&tiles, piece_type, Some(&exposed_right_edge), Some(&exposed_bottom_edge), &last_tile_id);

        exposed_right_edge = next_tile.right_edge_code;
        last_tile_id = next_tile.id;
        bottom_row.push(next_tile);
    }

    puzzle.push(bottom_row);

    return puzzle;
}

fn flatten_puzzle(puzzle: &Vec<Vec<Tile>>) -> Vec<String> {
    let mut flattened_puzzle:Vec<String> = Vec::new();

    for row in puzzle {
        for i in 1..9 {
            let mut row_string = "".to_string();

            for tile in row {
                row_string.push_str(&tile.contents.get(i).unwrap()[1..9]);
            }

            flattened_puzzle.push(row_string.to_string());
        }
    }

    return flattened_puzzle;
}

fn find_and_orient_top_left_corner(tiles: &Vec<Tile>) -> Tile {
    for tile in tiles {
        if tile.piece_type == PieceType::Corner {
            let mut top_left_corner = Tile {
                id: tile.id,
                contents: tile.contents.clone(),
                piece_type: tile.piece_type,
                top_edge_code: tile.top_edge_code,
                bottom_edge_code: tile.bottom_edge_code,
                left_edge_code: tile.left_edge_code,
                right_edge_code: tile.right_edge_code,
            };

            while find_edges_that_match(top_left_corner.right_edge_code, tiles, top_left_corner.id) != 1 || find_edges_that_match(top_left_corner.bottom_edge_code, tiles, top_left_corner.id) != 1 {
                //Rotate tile

                top_left_corner = Tile {
                    id: top_left_corner.id,
                    contents: rotate_contents_clockwise(top_left_corner.contents),
                    piece_type: top_left_corner.piece_type,
                    top_edge_code: top_left_corner.left_edge_code,
                    bottom_edge_code: top_left_corner.right_edge_code,
                    left_edge_code: top_left_corner.bottom_edge_code,
                    right_edge_code: top_left_corner.top_edge_code,
                };
            }

            return top_left_corner;
        }
    }

    panic!("No corner tile found!");
}

fn find_and_orient_piece_with_left_and_top_edge(tiles: &Vec<Tile>, piece_type:PieceType, left_edge_code: Option<&u32>, top_edge_code: Option<&u32>, exclude_id: &u32) -> Tile {
    for tile in tiles {
        if tile.piece_type == piece_type && &tile.id != exclude_id {
            match left_edge_code {
                Some(lec) => {
                    if &tile.top_edge_code == lec || &tile.bottom_edge_code == lec || &tile.left_edge_code == lec || &tile.right_edge_code == lec {
                        let mut candidate_tile = Tile {
                            id: tile.id,
                            contents: tile.contents.clone(),
                            piece_type: tile.piece_type,
                            top_edge_code: tile.top_edge_code,
                            bottom_edge_code: tile.bottom_edge_code,
                            left_edge_code: tile.left_edge_code,
                            right_edge_code: tile.right_edge_code,
                        };

                        while &candidate_tile.left_edge_code != lec {
                            //Rotate tile
                            candidate_tile = Tile {
                                id: candidate_tile.id,
                                contents: rotate_contents_clockwise(candidate_tile.contents),
                                piece_type: candidate_tile.piece_type,
                                top_edge_code: candidate_tile.left_edge_code,
                                bottom_edge_code: candidate_tile.right_edge_code,
                                left_edge_code: candidate_tile.bottom_edge_code,
                                right_edge_code: candidate_tile.top_edge_code,
                            };
                        }

                        //Finally, check whether the tile needs flipping based on the required top_edge_code
                        if (top_edge_code.is_none() && find_edges_that_match(candidate_tile.top_edge_code, tiles, candidate_tile.id) != 0) || (top_edge_code.is_some() && Some(&candidate_tile.top_edge_code) != top_edge_code) {
                            //Flip tile vertically - we've matched the left edge so this has to stay
                            candidate_tile = Tile {
                                id: candidate_tile.id,
                                contents: flip_contents_vertically(candidate_tile.contents),
                                piece_type: candidate_tile.piece_type,
                                top_edge_code: candidate_tile.bottom_edge_code,
                                bottom_edge_code: candidate_tile.top_edge_code,
                                left_edge_code: candidate_tile.left_edge_code,
                                right_edge_code: candidate_tile.right_edge_code,
                            }
                        }

                        return candidate_tile;
                    }
                },
                None => match top_edge_code {
                    Some (tec) => {
                        if &tile.top_edge_code == tec || &tile.bottom_edge_code == tec || &tile.left_edge_code == tec || &tile.right_edge_code == tec {
                            let mut candidate_tile = Tile {
                                id: tile.id,
                                contents: tile.contents.clone(),
                                piece_type: tile.piece_type,
                                top_edge_code: tile.top_edge_code,
                                bottom_edge_code: tile.bottom_edge_code,
                                left_edge_code: tile.left_edge_code,
                                right_edge_code: tile.right_edge_code,
                            };

                            while &candidate_tile.left_edge_code != tec {
                                //Rotate tile

                                candidate_tile = Tile {
                                    id: candidate_tile.id,
                                    contents: rotate_contents_clockwise(candidate_tile.contents),
                                    piece_type: candidate_tile.piece_type,
                                    top_edge_code: candidate_tile.left_edge_code,
                                    bottom_edge_code: candidate_tile.right_edge_code,
                                    left_edge_code: candidate_tile.bottom_edge_code,
                                    right_edge_code: candidate_tile.top_edge_code,
                                };
                            }

                            //Finally, check whether the tile needs flipping based on the required left_edge_code
                            if find_edges_that_match(candidate_tile.bottom_edge_code, tiles, candidate_tile.id) != 0 {
                                //Flip tile vertically - we've matched the top (right currently) edge so this has to stay
                                candidate_tile = Tile {
                                    id: candidate_tile.id,
                                    contents: flip_contents_vertically(candidate_tile.contents.clone()),
                                    piece_type: candidate_tile.piece_type,
                                    top_edge_code: candidate_tile.bottom_edge_code,
                                    bottom_edge_code: candidate_tile.top_edge_code,
                                    left_edge_code: candidate_tile.left_edge_code,
                                    right_edge_code: candidate_tile.right_edge_code,
                                }
                            }

                            //Rotate once more to get the right edge to the top after potential flip
                            return Tile {
                                id: candidate_tile.id,
                                contents: rotate_contents_clockwise(candidate_tile.contents),
                                piece_type: candidate_tile.piece_type,
                                top_edge_code: candidate_tile.left_edge_code,
                                bottom_edge_code: candidate_tile.right_edge_code,
                                left_edge_code: candidate_tile.bottom_edge_code,
                                right_edge_code: candidate_tile.top_edge_code,
                            };
                        }
                    },
                    None => panic!("Must supply either a Top or Left edge")
                }
            }
        }
    }

    panic!("No suitable tile found!");
}

fn rotate_contents_clockwise(contents: Vec<String>) -> Vec<String> {
    let mut new_contents:Vec<String> = Vec::new();

    for i in 0 .. contents.get(0).unwrap().len() {
        new_contents.push(contents
            .iter()
            .map(|s| s.chars().collect::<Vec<char>>()[i])
            .rev()
            .collect::<String>()
        );
    }

    return new_contents;
}

fn flip_contents_vertically(contents: Vec<String>) -> Vec<String> {
    let mut new_contents:Vec<String> = Vec::new();

    for i in (0 .. contents.len()).rev() {
        new_contents.push(contents.get(i).unwrap().to_string());
    }

    return new_contents;
}

fn determine_piece_types(tiles: &Vec<Tile>) -> Vec<Tile> {
    let mut tiles_with_piece_types:Vec<Tile> = Vec::new();

    for tile in tiles {
        let mut matching_edges = 0;

        matching_edges += find_edges_that_match(tile.top_edge_code, tiles, tile.id);
        matching_edges += find_edges_that_match(tile.bottom_edge_code, tiles, tile.id);
        matching_edges += find_edges_that_match(tile.left_edge_code, tiles, tile.id);
        matching_edges += find_edges_that_match(tile.right_edge_code, tiles, tile.id);

        let piece_type = match matching_edges {
            2 => PieceType::Corner,
            3 => PieceType::Edge,
            4 => PieceType::Middle,
            _ => PieceType::Unknown
        };

        tiles_with_piece_types.push(Tile {
            id: tile.id,
            contents: tile.contents.clone(),
            piece_type,
            top_edge_code: tile.top_edge_code,
            bottom_edge_code: tile.bottom_edge_code,
            left_edge_code: tile.left_edge_code,
            right_edge_code: tile.right_edge_code
        });
    }

    return tiles_with_piece_types;
}

fn find_edges_that_match(edge_code: u32, tiles: &Vec<Tile>, exclude_id: u32) -> i32 {
    let mut matching_edges = 0;

    for tile in tiles {
        if tile.id != exclude_id {
            if tile.top_edge_code == edge_code {
                matching_edges += 1;
            }

            if tile.bottom_edge_code == edge_code {
                matching_edges += 1;
            }

            if tile.left_edge_code == edge_code {
                matching_edges += 1;
            }

            if tile.right_edge_code == edge_code {
                matching_edges += 1;
            }
        }
    }

    return matching_edges;
}

fn parse_input_into_tiles(input: String) -> Vec<Tile> {
    let mut tiles:Vec<Tile> = Vec::new();

    for tile in input.replace("\r", "").split("\n\n") {
        let mut contents:Vec<String> = Vec::new();
        let mut id:u32 = 0;

        for line in tile.lines().filter(|l| l != &"") {
            if line[0..4] == "Tile".to_string() {
                id = line[5 .. line.len() - 1].parse::<u32>().unwrap();
            } else {
                contents.push(line.to_string());
            }
        }

        let top_edge_code = min(
            u32::from_str_radix(&contents[0].replace('#', "1").replace('.', "0"), 2).unwrap(),
            u32::from_str_radix(&contents[0].chars().rev().collect::<String>().replace('#', "1").replace('.', "0"), 2).unwrap()
        );

        let bottom_edge_code = min(
            u32::from_str_radix(&contents[contents.len() - 1].replace('#', "1").replace('.', "0"), 2).unwrap(),
            u32::from_str_radix(&contents[contents.len() - 1].chars().rev().collect::<String>().replace('#', "1").replace('.', "0"), 2).unwrap()
        );

        let left_edge_str = contents
            .iter()
            .map(|s| s.chars().collect::<Vec<char>>()[0])
            .collect::<String>();

        let right_edge_str = contents
            .iter()
            .map(|s| s.chars().collect::<Vec<char>>()[s.len() - 1])
            .collect::<String>();

        let left_edge_code = min(
            u32::from_str_radix(&left_edge_str.replace('#', "1").replace('.', "0"), 2).unwrap(),
            u32::from_str_radix(&left_edge_str.chars().rev().collect::<String>().replace('#', "1").replace('.', "0"), 2).unwrap()
        );

        let right_edge_code = min(
            u32::from_str_radix(&right_edge_str.replace('#', "1").replace('.', "0"), 2).unwrap(),
            u32::from_str_radix(&right_edge_str.chars().rev().collect::<String>().replace('#', "1").replace('.', "0"), 2).unwrap()
        );

        tiles.push(Tile { id, contents, piece_type: PieceType::Unknown, top_edge_code, bottom_edge_code, left_edge_code, right_edge_code });
    }

    return tiles;
}
