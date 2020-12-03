use std::fs;

#[derive(Clone)]
struct Position {
    x: i32,
    y: i32
}

struct Velocity {
    x: i32,
    y: i32
}

fn main() {
    let input = fs::read_to_string("input.txt")
        .expect("Something went wrong reading the file");

    let map: Vec<Vec<char>> = input
        .lines()
        .map(|str| str.chars().collect())
        .collect();

    let start = Position { x: 0, y: 0};
    let velocity = Velocity { x: 3, y: 1};

    println!("Part 1: You encountered {} trees", count_trees_encountered(map.clone(), start.clone(), &velocity));

    let mut tree_product: i64 = 1;

    println!("Part 2: You encountered {} trees (1, 1)", count_trees_encountered(map.clone(), start.clone(), &Velocity {x: 1, y: 1}));
    tree_product *= count_trees_encountered(map.clone(), start.clone(), &Velocity {x: 1, y: 1});

    println!("Part 2: You encountered {} trees (3, 1)", count_trees_encountered(map.clone(), start.clone(), &Velocity {x: 3, y: 1}));
    tree_product *= count_trees_encountered(map.clone(), start.clone(), &Velocity {x: 3, y: 1});

    println!("Part 2: You encountered {} trees (5, 1)", count_trees_encountered(map.clone(), start.clone(), &Velocity {x: 5, y: 1}));
    tree_product *= count_trees_encountered(map.clone(), start.clone(), &Velocity {x: 5, y: 1});

    println!("Part 2: You encountered {} trees (7, 1)", count_trees_encountered(map.clone(), start.clone(), &Velocity {x: 7, y: 1}));
    tree_product *= count_trees_encountered(map.clone(), start.clone(), &Velocity {x: 7, y: 1});

    println!("Part 2: You encountered {} trees (1, 2)", count_trees_encountered(map.clone(), start.clone(), &Velocity {x: 1, y: 2}));
    tree_product *= count_trees_encountered(map.clone(), start.clone(), &Velocity {x: 1, y: 2});

    println!("Part 2: Product of trees encountered was {}", tree_product);
}

fn update_position(position: Position, velocity: &Velocity, width: usize) -> Position {
    return Position{ x: (position.x + velocity.x) % (width as i32), y: position.y + velocity.y };
}

fn count_trees_encountered(map: Vec<Vec<char>>, start: Position, velocity: &Velocity) -> i64 {
    let width = map
        .iter()
        .map(|vec| vec.len())
        .max()
        .expect("Couldn't find width");

    let height = map
        .len();

    let mut tree_count = 0;
    let mut position = start;

    for _row in 0..height {
        if position.y < height as i32 {
            if *(map.iter().nth(position.y as usize).unwrap().iter().nth(position.x as usize).unwrap()) == '#' {
                tree_count += 1;
            }

            position = update_position(position, &velocity, width);
        }
    }

    return tree_count;
}