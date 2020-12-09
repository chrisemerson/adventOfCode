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

    for start in 0..numbers.len() {
        let mut sum_of_contiguous_numbers:i64 = 0;

        for end in start..numbers.len() {
            sum_of_contiguous_numbers += numbers[end];

            if sum_of_contiguous_numbers > sum {
                break;
            }

            if end - start != 1 && sum_of_contiguous_numbers == sum {
                let contiguous_numbers:Vec<i64> = numbers[start..end + 1].to_vec();

                println!("Contiguous group found that sum to {}: {:?}", &sum, contiguous_numbers);

                let min_number = contiguous_numbers.iter().min().unwrap();
                let max_number = contiguous_numbers.iter().max().unwrap();

                println!(
                    "Sum of start and end of group ({} and {}) is {}",
                    min_number,
                    max_number,
                    min_number + max_number
                );
            }
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