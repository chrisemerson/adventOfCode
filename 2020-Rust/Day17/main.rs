use std::cmp::max;
use std::cmp::min;
use std::collections::HashMap;
use std::fs;

#[derive(Debug, Clone)]
struct Cell {
    x: i32,
    y: i32,
    z: i32,
    w: i32,
    current_state: bool,
    next_state: bool
}

fn main() {
    let input = fs::read_to_string("input.txt")
        .expect("Something went wrong reading the file");

    let mut cells3d:HashMap<String, Cell> = HashMap::new();

    let mut y = 0;
    let z = 0;
    let w = 0;

    for line in input.lines() {
        let mut x = 0;

        for char in line.chars()
        {
            cells3d.insert(get_cell_key_from_coords(&x, &y, &z, &w), Cell { x, y, z, w, current_state: (char == '#'), next_state: false});
            x += 1;
        }

        y += 1;
    }

    let mut cycles = 6;

    let mut state = cells3d.clone();
    let mut state_4d = cells3d.clone();

    while cycles > 0 {
        state = get_next_state(&state);
        state_4d = get_next_state_4d(&state_4d);
        cycles -= 1;
    }

    let active_cells = state
        .iter()
        .filter(|(_, c)| c.current_state )
        .collect::<HashMap<&String, &Cell>>()
        .len();

    let active_cells_4d = state_4d
        .iter()
        .filter(|(_, c)| c.current_state )
        .collect::<HashMap<&String, &Cell>>()
        .len();

    println!("There are {} active cells after 6 cycles.", active_cells);
    println!("There are {} active cells after 6 cycles in 4 dimensions.", active_cells_4d);
}

fn get_next_state(cells: &HashMap<String, Cell>) -> HashMap<String, Cell> {
    let mut new_state:HashMap<String, Cell> = HashMap::new();
    let (min_x, max_x, min_y, max_y, min_z, max_z, _, _) = get_min_max(cells);
    let w = 0;

    for x in min_x - 1 .. max_x + 2 {
        for y in min_y - 1 .. max_y + 2 {
            for z in min_z - 1 .. max_z + 2 {
                let cell_key = get_cell_key_from_coords(&x, &y, &z, &w);
                let next_state:bool = match get_active_neighbours(cells, &x, &y, &z, &w) {
                    3 => true,
                    2 => match cells.get(&cell_key) {
                        Some(c) => c.current_state,
                        _ => false
                    },
                    _ => false
                };

                new_state.insert(cell_key, Cell { x, y, z, w, current_state: false, next_state });
            }
        }
    }

    return process_next_state(&new_state);
}

fn get_next_state_4d(cells: &HashMap<String, Cell>) -> HashMap<String, Cell> {
    let mut new_state:HashMap<String, Cell> = HashMap::new();
    let (min_x, max_x, min_y, max_y, min_z, max_z, min_w, max_w) = get_min_max(cells);

    for x in min_x - 1 .. max_x + 2 {
        for y in min_y - 1 .. max_y + 2 {
            for z in min_z - 1 .. max_z + 2 {
                for w in min_w - 1 .. max_w + 2 {
                    let cell_key = get_cell_key_from_coords(&x, &y, &z, &w);
                    let next_state: bool = match get_active_neighbours(cells, &x, &y, &z, &w) {
                        3 => true,
                        2 => match cells.get(&cell_key) {
                            Some(c) => c.current_state,
                            _ => false
                        },
                        _ => false
                    };

                    new_state.insert(cell_key, Cell { x, y, z, w, current_state: false, next_state });
                }
            }
        }
    }

    return process_next_state(&new_state);
}

fn get_active_neighbours(cells: &HashMap<String, Cell>, x:&i32, y:&i32, z:&i32, w:&i32) -> i32 {
    let mut active_neighbours = 0;

    for (_, candidate_cell) in cells {
        if candidate_cell.current_state
            && (x - &candidate_cell.x).abs() <= 1 && (y - &candidate_cell.y).abs() <= 1 && (z - &candidate_cell.z).abs() <= 1 && (w - &candidate_cell.w).abs() <= 1
            && (&candidate_cell.x != x || &candidate_cell.y != y || &candidate_cell.z != z || &candidate_cell.w != w)
        {
            active_neighbours += 1;
        }
    }

    return active_neighbours;
}

fn get_cell_key_from_coords(x:&i32, y:&i32, z:&i32, w:&i32) -> String {
    return format!("{}.{}.{}.{}", x, y, z, w);
}

fn process_next_state(state: &HashMap<String, Cell>) -> HashMap<String, Cell> {
    let mut processed_state:HashMap<String, Cell> = HashMap::new();

    for (idx, cell) in state {
        processed_state.insert(idx.to_string(), Cell { x: cell.x, y: cell.y, z: cell.z, w: cell.w, current_state: cell.next_state, next_state: false });
    }

    return processed_state;
}

fn get_min_max(cells: &HashMap<String, Cell>) -> (i32, i32, i32, i32, i32, i32, i32, i32) {
    let mut min_x = 0;
    let mut max_x = 0;
    let mut min_y = 0;
    let mut max_y = 0;
    let mut min_z = 0;
    let mut max_z = 0;
    let mut min_w = 0;
    let mut max_w = 0;

    for (_, cell) in cells {
        min_x = min(min_x, cell.x);
        min_y = min(min_y, cell.y);
        min_z = min(min_z, cell.z);
        min_w = min(min_w, cell.w);
        max_x = max(max_x, cell.x);
        max_y = max(max_y, cell.y);
        max_z = max(max_z, cell.z);
        max_w = max(max_w, cell.w);
    }

    return (min_x, max_x, min_y, max_y, min_z, max_z, min_w, max_w);
}
