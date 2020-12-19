use std::fs;
use std::collections::HashMap;
use regex::Regex;

fn main() {
    let input = fs::read_to_string("input.txt")
        .expect("Something went wrong reading the file")
        .replace("\r", "");

    let input_parts = input.split("\n\n").collect::<Vec<&str>>();

    let rules = parse_rules_into_map(input_parts[0].lines().map(|x| x.to_string()).collect::<Vec<String>>());
    let messages = input_parts[1].lines().map(|x| x.to_string()).collect::<Vec<String>>();

    let re = Regex::new(&format!("^{}$", get_regex(&rules, 0))).unwrap();

    let valid_messages = messages
        .iter()
        .filter(|m| re.is_match(m))
        .map(|x| x.to_string())
        .collect::<Vec<String>>()
        .len();

    println!("{:?}", valid_messages);
}

fn parse_rules_into_map(rules: Vec<String>) -> HashMap<i32, String> {
    let mut map:HashMap<i32, String> = HashMap::new();

    for rule in rules {
        let rule_parts = rule.split(": ").map(|x| x.trim().to_string()).collect::<Vec<String>>();
        map.insert(rule_parts[0].parse::<i32>().unwrap(), rule_parts[1].to_string());
    }

    return map;
}

fn get_regex(rules: &HashMap<i32, String>, rule_no: i32) -> String {
    let rule = rules.get(&rule_no).unwrap();
    let chars = rule.chars().collect::<Vec<char>>();

    //If single character, return it
    if chars[0] == '"' && chars[chars.len() - 1] == '"' {
        return rule[1 .. rule.len() - 1].to_string();
    }

    //If there is a | in the string, construct a regex to match it
    let or_sides = rule.split(" | ").map(|x| x.trim().to_string()).collect::<Vec<String>>();
    if or_sides.len() > 1 {
        let mut part_0 = "".to_string();
        let mut part_1 = "".to_string();

        for part in or_sides[0].split(" ").map(|x| x.trim().to_string()).collect::<Vec<String>>() {
            part_0.push_str(&get_regex(rules, part.parse::<i32>().unwrap()));
        }

        for part in or_sides[1].split(" ").map(|x| x.trim().to_string()).collect::<Vec<String>>() {
            part_1.push_str(&get_regex(rules, part.parse::<i32>().unwrap()));
        }

        return format!("(?:{}|{})", part_0, part_1);
    }

    let mut concat_string = "".to_string();

    for part in rule.split(" ").map(|x| x.trim().to_string()).collect::<Vec<String>>() {
        concat_string.push_str(&get_regex(rules, part.parse::<i32>().unwrap()));
    }

    return concat_string;
}