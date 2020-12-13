use std::fs;

#[derive(Debug)]
struct ShipState {
    x: i32,
    y: i32,
    h: i32
}

#[derive(Debug)]
struct WaypointPosition {
    x: i32,
    y: i32
}

#[derive(Debug)]
struct ShipPosition {
    x: i32,
    y: i32
}

#[derive(Debug)]
struct FerryJourney {
    ship: ShipPosition,
    waypoint: WaypointPosition
}

fn main() {
    let input = fs::read_to_string("input.txt")
        .expect("Something went wrong reading the file");

    let directions = input
        .lines()
        .map(|line| line.trim().to_string())
        .filter(|line| line != "")
        .collect::<Vec<String>>();

    let ship_state = ShipState {
        x: 0,
        y: 0,
        h: 90
    };

    let final_ship_state = directions
        .iter()
        .fold(
            ship_state,
            |ship_state, instruction| update_ship_state(&ship_state, instruction)
        );

    println!("Manhattan distance of final ship state is {}", final_ship_state.x.abs() + final_ship_state.y.abs());

    let ferry_journey = FerryJourney {
        ship: ShipPosition {
            x: 0,
            y: 0
        },
        waypoint: WaypointPosition {
            x: 10,
            y: 1
        }
    };

    let final_ship_state = directions
        .iter()
        .fold(
            ferry_journey,
            |ferry_journey, instruction| update_journey_state(&ferry_journey, instruction)
        );

    println!("Manhattan distance of final ship after waypoint navigation is {}", final_ship_state.ship.x.abs() + final_ship_state.ship.y.abs());
}

fn update_ship_state(ship_state: &ShipState, instruction: &String) -> ShipState {
    let mut x = ship_state.x;
    let mut y = ship_state.y;
    let mut h = ship_state.h;

    let operation = &instruction[..1];
    let value = &instruction[1..].parse::<i32>().unwrap();

    match operation {
        "N" => y += value,
        "S" => y -= value,
        "E" => x += value,
        "W" => x -= value,
        "F" => match h {
            0 => y += value,
            90 => x += value,
            180 => y -= value,
            270 => x -= value,
            _ => panic!("Ship got itself into an unsupported heading: {}", h)
        },
        "L" => h = (h + (360 - value)) % 360,
        "R" => h = (h + value) % 360,
        _ => panic!("Operation not found - {}", operation)
    }

    return ShipState { x, y, h };
}

fn update_journey_state(ferry_journey: &FerryJourney, instruction: &String) -> FerryJourney {
    let mut ship_x = ferry_journey.ship.x;
    let mut ship_y = ferry_journey.ship.y;
    let mut waypoint_x = ferry_journey.waypoint.x;
    let mut waypoint_y = ferry_journey.waypoint.y;

    let operation = &instruction[..1];
    let value = &instruction[1..].parse::<i32>().unwrap();

    match operation {
        "N" => waypoint_y += value,
        "S" => waypoint_y -= value,
        "E" => waypoint_x += value,
        "W" => waypoint_x -= value,
        "F" => {
            ship_x += waypoint_x * value;
            ship_y += waypoint_y * value;
        },
        "L" | "R" => match instruction.as_str() {
            "L90" | "R270" => {
                let old_waypoint_x = waypoint_x;

                waypoint_x = -waypoint_y;
                waypoint_y = old_waypoint_x;
            },
            "L180" | "R180" => {
                waypoint_x = -waypoint_x;
                waypoint_y = -waypoint_y;
            },
            "L270" | "R90" => {
                let old_waypoint_x = waypoint_x;

                waypoint_x = waypoint_y;
                waypoint_y = -old_waypoint_x;
            },
            _ => panic!("Unsupported instruction: {}", instruction)
        },
        _ => panic!("Operation not found - {}", operation)
    }

    return FerryJourney { ship: ShipPosition { x: ship_x, y: ship_y }, waypoint: WaypointPosition { x: waypoint_x, y: waypoint_y }};
}
