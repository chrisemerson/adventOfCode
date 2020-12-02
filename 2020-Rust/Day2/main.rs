use std::collections::btree_map::BTreeMap;
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt")
        .expect("Something went wrong reading the file");

    let passwords = input
        .lines()
        .map(|str| str.to_string());

    println!(
        "There are {} valid passwords for Part 1",
        passwords
            .clone()
            .filter(|password| password_valid_pt1(password))
            .collect::<Vec<String>>()
            .len()
    );

    println!(
        "There are {} valid passwords for Part 2",
        passwords
            .clone()
            .filter(|password| password_valid_pt2(password))
            .collect::<Vec<String>>()
            .len()
    );
}

fn password_valid_pt1(password: &String) -> bool {
    let (min, max, char, password_text) = parse_password_line(password);

    let mut letter_counts = BTreeMap::new();

    for c in password_text.chars() {
        *letter_counts.entry(c).or_insert(0) += 1;
    }

    let mut count_of_char = 0;

    if letter_counts.contains_key(&char) {
        count_of_char = letter_counts[&char];
    }

    return count_of_char >= min && count_of_char <= max;
}

fn password_valid_pt2(password: &String) -> bool {
    let (a, b, char, password_text) = parse_password_line(password);

    let char_at_a = password_text.chars().nth(a as usize - 1).unwrap();
    let char_at_b = password_text.chars().nth(b as usize - 1).unwrap();

    return (char_at_a == char || char_at_b == char) && !(char_at_a == char && char_at_b == char);
}

fn parse_password_line(password: &String) -> (i64, i64, char, String) {
    let parts:Vec<&str> = password.split(":").collect();
    let range:String = parts[0].to_string();
    let range_parts:Vec<&str> = range.split(" ").collect();
    let min_max:Vec<&str> = range_parts[0].split("-").collect();

    return (
        min_max[0].parse::<i64>().unwrap(),
        min_max[1].parse::<i64>().unwrap(),
        range_parts[1].trim().to_string().chars().next().unwrap(),
        parts[1].trim().to_string()
    );
}