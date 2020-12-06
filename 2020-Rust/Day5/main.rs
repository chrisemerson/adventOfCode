use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt")
        .expect("Something went wrong reading the file");

    let seats = input
        .split("\n")
        .map(|str|
            i32::from_str_radix(
                &str
                    .replace("F", "0")
                    .replace("B", "1")
                    .replace("L", "0")
                    .replace("R", "1"),
                2)
                .unwrap())
        .collect::<Vec<i32>>();

    let max_seat_id = seats
        .iter()
        .max()
        .unwrap();

    println!("The highest seat number is {}", max_seat_id);

    for seat in 0..1023 {
        if !seats.contains(&seat) && seats.contains(&(seat - 1)) && seats.contains(&(seat + 1)) {
            println!("Our seat is {}", seat);
        }
    }
}