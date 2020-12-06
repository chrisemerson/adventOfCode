use std::fs;
use std::collections::HashSet;
use std::iter::FromIterator;

fn main() {
    let input = fs::read_to_string("input.txt")
        .expect("Something went wrong reading the file");

    let answers_groups:Vec<Vec<HashSet<char>>> = input
        .split("\n\n")
        .map(|ag|
            ag
                .split("\n")
                .filter(|a| a != &"")
                .map(|a| HashSet::from_iter(a.chars()))
                .collect::<Vec<HashSet<char>>>()
        )
        .collect::<Vec<Vec<HashSet<char>>>>();

    let mut part1total = 0;

    for answers_group in &answers_groups {
        let mut questions_answered = HashSet::new();

        for answers in answers_group {
            questions_answered = HashSet::from_iter(questions_answered.union(&answers).map(|i| *i));
        }

        part1total += questions_answered.len();
    }

    println!("Part 1: {}", part1total);

    let mut part2total = 0;

    for answers_group in &answers_groups {
        let mut questions_everyone_answered = answers_group[0].clone();

        for answers in answers_group {
            questions_everyone_answered = HashSet::from_iter(questions_everyone_answered.intersection(&answers).map(|i| *i));
        }

        part2total += questions_everyone_answered.len();
    }

    println!("Part 2: {}", part2total);
}