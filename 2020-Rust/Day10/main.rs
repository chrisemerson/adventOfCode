use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt")
        .expect("Something went wrong reading the file");

    let mut adapters = input
        .lines()
        .filter(|line| line != &"")
        .map(|number| number.parse::<i32>().unwrap())
        .collect::<Vec<i32>>();

    adapters.sort();

    let mut prev_joltage = 0;
    let mut prev_prev_joltage = 0;

    let mut differences_of_1 = 0;
    let mut differences_of_3 = 1;

    let mut lynchpin_adapters:Vec<i32> = Vec::new();

    lynchpin_adapters.push(0);

    for i in &adapters {
        if i - &prev_joltage == 1 {
            differences_of_1 += 1;
        }

        if i - &prev_joltage == 3 {
            differences_of_3 += 1;

            if &prev_joltage - &prev_prev_joltage == 3 {
                lynchpin_adapters.push(prev_joltage);
            }
        }

        prev_prev_joltage = prev_joltage;
        prev_joltage = *i;
    }

    lynchpin_adapters.push(adapters.iter().max().unwrap().to_owned() + 3);

    println!("There were {} differences of 1 and {} differences of 3. Their product is {}", differences_of_1, differences_of_3, differences_of_1 * differences_of_3);

    let chains = lynchpin_adapters
        .windows(2)
        .map(|w| get_number_of_chains_with_available_adapters(
            w[1] - w[0],
            adapters
                .iter()
                .filter(|&a| a > &w[0] && a < &w[1])
                .map(|a| a - w[0])
                .collect::<Vec<i32>>()
        ))
        .fold(1, |a, b| a * b);

    println!("There are {} possible ways to use the adapters to get from 0 jolts to the device joltage", chains);
}

fn get_number_of_chains_with_available_adapters(target: i32, adapters: Vec<i32>) -> i64 {
    if target == 0 {
        return 0;
    }

    if adapters.len() == 0 {
        if target <= 3 {
            return 1;
        }

        return 0;
    }

    let mut number_of_chains:i64 = 0;
    let eligible_adapters = adapters.iter().filter(|a| **a >= 1 && **a <= 3).map(|a| *a).collect::<Vec<i32>>();

    for adapter in eligible_adapters {
        let new_adapters = adapters
            .clone()
            .iter()
            .map(|a| (*a - adapter))
            .filter(|a| *a >= 1)
            .collect::<Vec<i32>>();

        number_of_chains += get_number_of_chains_with_available_adapters(target.clone() - adapter, new_adapters);
    }

    return number_of_chains;
}