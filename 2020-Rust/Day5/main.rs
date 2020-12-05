use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt")
        .expect("Something went wrong reading the file");

    let seats = input
        .split("\n")
        .map(|str|
            isize::from_str_radix(
                &str
                    .replace("F", "0")
                    .replace("B", "1")
                    .replace("L", "0")
                    .replace("R", "1"),
                2)
                .unwrap())
        .collect::<Vec<isize>>();

    let max_seat_id = seats
        .iter()
        .max()
        .unwrap();

    println!("The highest seat number is {}", max_seat_id);

    for seat in 0..1023 {
        if !seats.contains(&(seat as isize)) && seats.contains(&(seat - 1 as isize)) && seats.contains(&(seat + 1 as isize)) {
            println!("Our seat is {}", seat);
        }
    }
}