use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt")
        .expect("Something went wrong reading the file");

    let numbers = input
        .lines()
        .filter(|line| line != &"")
        .map(|number| number.parse::<i64>().unwrap())
        .collect::<Vec<i64>>();

    let slice_size = 25;
    let mut sum:i64 = 0;

    for i in 0..(numbers.len() - &slice_size - 1) {
        let prev_numbers:Vec<i64> = numbers[i..i + &slice_size].to_vec();
        let number:i64 = numbers[i + &slice_size];

        if !can_number_be_sum_of_previous_numbers(&number, &prev_numbers) {
            sum = number;

            println!("First number that cannot be the sum of previous {} numbers: {}", slice_size, number);
            break;
        }
    }

    let mut start:usize = 0;
    let mut end:usize = 1;

    loop {
        let slice = &numbers[start..end];
        let sum_of_contiguous_numbers:i64 = slice.iter().sum();

        if sum_of_contiguous_numbers == sum {
            let min = slice.iter().min().unwrap();
            let max = slice.iter().max().unwrap();

            println!("Sum of start and end of group ({} and {}) is {}", min, max, min + max);

            break;
        }

        if sum_of_contiguous_numbers < sum {
            end += 1;
        }

        if sum_of_contiguous_numbers > sum {
            start += 1;
        }
    }
}

fn can_number_be_sum_of_previous_numbers(number: &i64, prev_numbers: &Vec<i64>) -> bool {
    for i in 0..prev_numbers.len() {
        for j in 0..prev_numbers.len() {
            if i != j && prev_numbers[i] + prev_numbers[j] == number.to_owned() {
                return true;
            }
        }
    }

    return false;
}