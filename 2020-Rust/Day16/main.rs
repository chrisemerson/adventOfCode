use std::fs;
use std::collections::HashMap;

#[derive(Debug)]
struct Range {
    low: i32,
    high: i32,
}

#[derive(Debug)]
struct Requirement {
    name: String,
    ranges: Vec<Range>,
}

fn main() {
    let input = fs::read_to_string("input.txt")
        .expect("Something went wrong reading the file");

    let (requirements, your_ticket, nearby_tickets) = parse_input(input);

    let ticket_scanning_error_rate: i32 = nearby_tickets
        .iter()
        .map(|ticket| find_invalid_value_in_ticket(ticket, &requirements))
        .filter(|invalid_value| invalid_value.is_some())
        .map(|invalid_value| invalid_value.unwrap())
        .fold(0, |a, b| a + b);

    println!("Ticket scanning error rate is: {}", ticket_scanning_error_rate);

    let mut valid_tickets: Vec<Vec<i32>> = nearby_tickets
        .iter()
        .filter(|ticket| find_invalid_value_in_ticket(ticket, &requirements).is_none())
        .map(|ticket| ticket.to_owned())
        .collect::<Vec<Vec<i32>>>();

    valid_tickets.push(your_ticket.clone());
    let mut possible_requirements:HashMap<usize, Vec<String>> = HashMap::new();
    let mut final_requirements:HashMap<usize, String> = HashMap::new();

    for i in 0..your_ticket.clone().len() {
        let field_values = valid_tickets
            .iter()
            .map(|ticket| ticket.get(i).unwrap().to_owned())
            .collect::<Vec<i32>>();

        let possible_requirements_for_field = get_possible_requirements_for_field(&field_values, &requirements);

        possible_requirements.insert(i, possible_requirements_for_field);
    }

    loop {
        for (i, possible_requirement_list) in &possible_requirements {
            if possible_requirement_list.len() == 1 {
                let requirement = possible_requirement_list.get(0).unwrap().to_string();
                final_requirements.insert(*i, requirement.clone());
                possible_requirements = remove_item_from_lists(&requirement, &possible_requirements);
                break;
            }
        }

        if final_requirements.len() == possible_requirements.len() {
            break;
        }
    }

    let pt2_answer = final_requirements
        .iter()
        .filter(|(_k, v)| v.len() >= 9 && v[0..9] == "departure".to_string())
        .map(|(k, _v)| *your_ticket.get(*k as usize).unwrap() as i64)
        .fold(1, |a, b| a * b);

    println!("Multiplying all the departure fields gives: {}", pt2_answer);
}

fn find_invalid_value_in_ticket(ticket: &Vec<i32>, requirements: &Vec<Requirement>) -> Option<i32> {
   return requirements
        .iter()
        .map(|requirement|
            find_invalid_value_in_ticket_by_single_requirement(ticket, requirement)
        )
        .fold(Some(0), |a, b| return if a.is_none() { a } else { b });
}

fn find_invalid_value_in_ticket_by_single_requirement(ticket: &Vec<i32>, requirement: &Requirement) -> Option<i32> {
    return ticket
        .iter()
        .map(|ticket_value| if requirement.ranges
                .iter()
                .map(|range| ticket_value >= &range.low && ticket_value <= &range.high)
                .fold(false, |a, b| if b { b } else { a }) {
                None
            } else {
                Some(*ticket_value)
            }
        )
        .fold(None, |a, b| return if b.is_some() { b } else { a });
}

fn get_possible_requirements_for_field(field_values: &Vec<i32>, requirements: &Vec<Requirement>) -> Vec<String> {
    return requirements
        .iter()
        .filter(|requirement| find_invalid_value_in_ticket_by_single_requirement(field_values, requirement).is_none())
        .map(|r| r.name.clone())
        .collect::<Vec<String>>();
}

fn remove_item_from_lists(requirement: &String, possible_requirements: &HashMap<usize, Vec<String>>) -> HashMap<usize, Vec<String>> {
    let mut new_possible_requirements:HashMap<usize, Vec<String>> = HashMap::new();

    for (i, list) in possible_requirements {
        new_possible_requirements.insert(*i, list.iter().filter(|r| r != &requirement).map(|r| r.to_string()).collect::<Vec<String>>());
    }

    return new_possible_requirements;
}

fn parse_input(input: String) -> (Vec<Requirement>, Vec<i32>, Vec<Vec<i32>>) {
    let mut requirements: Vec<Requirement> = Vec::new();
    let mut your_ticket: Vec<i32> = Vec::new();
    let mut nearby_tickets: Vec<Vec<i32>> = Vec::new();
    let mut your_ticket_next = false;

    for line in input.lines().map(|line| line.trim().to_string()) {
        if line != "" && line != "nearby tickets:" {
            if your_ticket.len() != 0 {
                nearby_tickets.push(line.split(",").map(|x| x.parse::<i32>().unwrap()).collect());
            } else if your_ticket_next {
                your_ticket = line.split(",").map(|x| x.parse::<i32>().unwrap()).collect();
                your_ticket_next = false;
            } else if line == "your ticket:" {
                your_ticket_next = true;
            } else {
                let requirement_parts = line.split(": ").map(|x| x.to_string()).collect::<Vec<String>>();
                let mut ranges: Vec<Range> = Vec::new();

                for range in requirement_parts.get(1).unwrap().split(" or ") {
                    let range_parts = range.split("-").map(|x| x.parse::<i32>().unwrap()).collect::<Vec<i32>>();
                    ranges.push(Range { low: *range_parts.get(0).unwrap(), high: *range_parts.get(1).unwrap() });
                }

                requirements.push(Requirement { name: requirement_parts.get(0).unwrap().to_string(), ranges });
            }
        }
    }

    return (requirements, your_ticket, nearby_tickets);
}
