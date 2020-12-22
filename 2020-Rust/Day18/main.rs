use std::collections::VecDeque;
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt")
        .expect("Something went wrong reading the file");

    let sum_pt1 = input
        .lines()
        .filter(|line| line != &"")
        .map(|calculation| perform_calculation(calculation.to_string(), false))
        .fold(0, |a, b| a + b);

    println!("The sum of all the calculations is {}", sum_pt1);

    let sum_pt2 = input
        .lines()
        .filter(|line| line != &"")
        .map(|calculation| perform_calculation(calculation.to_string(), true))
        .fold(0, |a, b| a + b);

    println!("The sum of all the calculations with the advanced precedence rules is {}", sum_pt2);
}

fn perform_calculation(calculation: String, advanced_precedence_rules: bool) -> u64 {
    let output_queue = shunt_some_trains(&calculation, advanced_precedence_rules);
    let mut calc_stack:Vec<u64> = Vec::new();

    for token in &output_queue {
        match token {
            '+' => {
                let val1 = calc_stack.pop().unwrap();
                let val2 = calc_stack.pop().unwrap();

                calc_stack.push(val1 + val2)
            },
            '*' => {
                let val1 = calc_stack.pop().unwrap();
                let val2 = calc_stack.pop().unwrap();

                calc_stack.push(val1 * val2)
            },
            _ => calc_stack.push(token.to_string().parse::<u64>().unwrap())
        }
    }

    return calc_stack.pop().unwrap();
}

fn shunt_some_trains(calculation: &String, advanced_precedence_rules: bool) -> VecDeque<char> {
    let tokens = calculation.chars().filter(|c| c != &' ').collect::<Vec<char>>();

    let mut output_queue:VecDeque<char> = VecDeque::new();
    let mut operator_stack:Vec<char> = Vec::new();

    for token in tokens {
        match token {
            '+' | '*' => loop {
                if operator_stack.len() == 0 {
                    operator_stack.push(token);
                    break;
                }

                let top_of_stack = operator_stack.pop().unwrap();

                if top_of_stack == '(' || (advanced_precedence_rules && top_of_stack != '+') {
                    operator_stack.push(top_of_stack);
                    operator_stack.push(token);
                    break;
                } else {
                    output_queue.push_back(top_of_stack);
                }
            },
            '(' => operator_stack.push(token),
            ')' => loop {
                let top_of_stack = operator_stack.pop().unwrap();

                if top_of_stack == '(' {
                    break;
                } else {
                    output_queue.push_back(top_of_stack);
                }
            },
            _ => output_queue.push_back(token)
        }
    }

    while operator_stack.len() > 0 {
        output_queue.push_back(operator_stack.pop().unwrap());
    }

    return output_queue;
}
