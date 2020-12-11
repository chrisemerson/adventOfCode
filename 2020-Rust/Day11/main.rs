use std::cmp;
use std::fmt;
use std::fs;

struct AOCPart {
    name: String,
    seat_occupancy_counting_strategy: fn(&AirportLounge, i32, i32) -> i32,
    seat_occupancy_count_threshold: i32
}

#[derive(Clone)]
struct AirportLounge {
    seats: Vec<Vec<char>>
}

impl fmt::Display for AirportLounge {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for line in &self.seats {
            match write!(f, "{}\n", line.into_iter().collect::<String>()) {
                Err(error) => return Err(error),
                Ok(_) => ()
            }
        }

        return Ok(())
    }
}

impl std::cmp::PartialEq for AirportLounge {
    fn eq(&self, other: &AirportLounge) -> bool {
        if self.seats.len() != other.seats.len() {
            return false;
        }

        if self.seats.get(0).unwrap().len() != other.seats.get(0).unwrap().len() {
            return false;
        }

        for x in 0..self.seats.len() {
            for y in 0..self.seats.get(x).unwrap().len() {
                let this_seat = self.seats.get(x).unwrap().get(y).unwrap();
                let other_seat = other.seats.get(x).unwrap().get(y).unwrap();

                if this_seat != other_seat {
                    return false;
                }
            }
        }

        return true;
    }
}

fn main() {
    let input = fs::read_to_string("input.txt")
        .expect("Something went wrong reading the file");

    let airport_lounge = AirportLounge {
        seats: input
            .lines()
            .map(|line| line.chars().collect::<Vec<char>>())
            .collect::<Vec<Vec<char>>>()
    };

    let parts = [
        AOCPart { name: "Part 1".to_string(), seat_occupancy_counting_strategy: get_number_of_occupied_adjacent_seats, seat_occupancy_count_threshold: 4 },
        AOCPart { name: "Part 2".to_string(), seat_occupancy_counting_strategy: get_number_of_occupied_visible_seats, seat_occupancy_count_threshold: 5 }
    ];

    for part in &parts {
        let mut prev_state = airport_lounge.clone();

        loop {
            let new_state = update_state(&prev_state, part.seat_occupancy_counting_strategy, part.seat_occupancy_count_threshold);

            if new_state == prev_state {
                println!("{}: When the state stabilises, there are {} occupied seats.", part.name, count_occupied_seats(&new_state));
                break;
            }

            prev_state = new_state;
        }
    }
}

fn update_state(airport_lounge: &AirportLounge, seat_occupation_strategy: fn(&AirportLounge, i32, i32) -> i32, occupied_seats_seen_needed_to_empty_this_seat: i32) -> AirportLounge {
    let mut seats:Vec<Vec<char>> = Vec::new();

    for row in 0..airport_lounge.seats.len() {
        let mut new_seat_row:Vec<char> = Vec::new();

        for col in 0..airport_lounge.seats.get(row).unwrap().len() {
            let occupied_adjacent_seats = seat_occupation_strategy(airport_lounge, row as i32, col as i32);
            let current_seat_state = airport_lounge.seats.get(row).unwrap().get(col).unwrap().to_owned();

            if occupied_adjacent_seats == 0 && current_seat_state != '.' {
                // If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
                new_seat_row.push('#');
            } else if occupied_adjacent_seats >= occupied_seats_seen_needed_to_empty_this_seat && current_seat_state != '.' {
                // If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
                new_seat_row.push('L');
            } else {
                // Otherwise, the seat's state does not change.
                new_seat_row.push(current_seat_state);
            }
        }

        seats.push(new_seat_row);
    }

    return AirportLounge { seats };
}

fn get_number_of_occupied_adjacent_seats(airport_lounge: &AirportLounge, row: i32, column: i32) -> i32 {
    let mut occupied_adjacent_seats:i32 = 0;

    for r in cmp::max(&row - 1, 0) .. &row + 2 {
        for c in cmp::max(&column - 1, 0) .. &column + 2 {
            if r != row || c != column {
                match get_seat_at(airport_lounge, r, c) {
                    Some(&'#') => occupied_adjacent_seats += 1,
                    _ => ()
                }
            }
        }
    }

    return occupied_adjacent_seats;
}

fn get_number_of_occupied_visible_seats(airport_lounge: &AirportLounge, row: i32, column: i32) -> i32 {
    let mut occupied_visible_seats = 0;

    let directions = [
        (-1, 0),
        (-1, 1),
        (0, 1),
        (1, 1),
        (1, 0),
        (1, -1),
        (0, -1),
        (-1, -1)
    ];

    for direction in &directions {
        let mut r = row;
        let mut c = column;

        loop {
            r += direction.0;
            c += direction.1;

            match get_seat_at(airport_lounge, r, c) {
                Some(&'.') => (),
                Some(&'L') => {
                    break;
                }
                Some(&'#') => {
                    occupied_visible_seats += 1;
                    break;
                },
                _ => break
            };
        }
    }

    return occupied_visible_seats;
}

fn get_seat_at(airport_lounge: &AirportLounge, row: i32, column: i32) -> Option<&char> {
    return match airport_lounge.seats.get(row as usize) {
        Some(seats_row) => seats_row.get(column as usize),
        _ => None
    }
}

fn count_occupied_seats(airport_lounge: &AirportLounge) -> i32 {
    let mut occupied_seats = 0;

    for row in 0..airport_lounge.seats.len() {
        for col in 0..airport_lounge.seats.get(row).unwrap().len() {
            let current_seat_state = airport_lounge.seats.get(row).unwrap().get(col).unwrap().to_owned();

            if current_seat_state == '#' {
                occupied_seats += 1;
            }
        }
    }

    return occupied_seats;
}
