use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt")
        .expect("Something went wrong reading the file");

    let integers:Vec<i64> = input.lines().map(|line| line.trim().parse::<i64>().unwrap()).collect();

    for x in integers.iter() {
        for y in integers.iter() {
            if x + y == 2020 && x < y {
                println!("Numbers are {} and {}, and their product is {}", x, y, x * y);
            }
        }
    }

    for x in integers.iter() {
        for y in integers.iter() {
            for z in integers.iter() {
                if x + y + z == 2020 && x < y && y < z {
                    println!("Numbers are {}, {} and {}, and their product is {}", x, y, z, x * y * z);
                }
            }
        }
    }
}