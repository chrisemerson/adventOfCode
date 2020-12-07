use std::collections::HashMap;
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt")
        .expect("Something went wrong reading the file");

    let mut bags:HashMap<String, HashMap<String, i32>> = HashMap::new();

    for line in input.lines() {
        let requirement_parts:Vec<&str> = line.split(" bags contain ").collect();
        let bag_color = requirement_parts[0];
        let bag_requirements:Vec<String> = requirement_parts[1].split(",").map(|bag| String::from(bag.trim())).collect();

        let mut inner_bags:HashMap<String, i32> = HashMap::new();

        if bag_requirements[0] != "no other bags." {
            for bag_requirement in bag_requirements {
                let mut bag_requirement_words:Vec<&str> = bag_requirement.split(" ").filter(|word| word != &"bags." && word != &"bags" && word != &"bag." && word != &"bag").collect();

                let bag_quantity = bag_requirement_words[0].parse::<i32>().unwrap();
                bag_requirement_words.remove(0);

                let inner_bag_color = bag_requirement_words.join(" ");

                inner_bags.insert(inner_bag_color, bag_quantity);
            }
        }

        bags.insert(String::from(bag_color), inner_bags);
    }

    println!("The shiny gold bag can eventually appear within {} other coloured bags", get_all_bags_that_can_eventually_contain(&bags, &"shiny gold".to_string()).len());
    println!("The shiny gold bag must contain {} other coloured bags", get_number_of_bags_that_must_be_contained_within(&bags, &"shiny gold".to_string()));
}

fn get_bags_that_can_contain(bags: &HashMap<String, HashMap<String, i32>>, color: &String) -> Vec<String> {
    let mut result:Vec<String> = Vec::new();

    for (outer_color, inner_bags) in bags {
        for (inner_color, _inner_qty) in inner_bags {
            if inner_color == color {
                result.push(outer_color.to_string());
            }
        }
    }

    return result;
}

fn get_all_bags_that_can_eventually_contain(bags: &HashMap<String, HashMap<String, i32>>, color: &String) -> Vec<String> {
    let mut result:Vec<String> = Vec::new();
    let parent_bags = get_bags_that_can_contain(bags, color);

    for parent_bag in &parent_bags {
        result.push(parent_bag.to_string());

        let parent_parent_bags = get_all_bags_that_can_eventually_contain(bags, parent_bag);

        for parent_parent_bag in parent_parent_bags {
            result.push(parent_parent_bag.clone());
        }
    }

    result.sort_unstable();
    result.dedup();

    return result;
}

fn get_number_of_bags_that_must_be_contained_within(bags: &HashMap<String, HashMap<String, i32>>, color: &String) -> i32 {
    return match bags.get(color) {
        Some(inner_bags) => {
            let mut total_bags = 0;

            for (inner_color, inner_qty) in inner_bags {
                total_bags += inner_qty * (get_number_of_bags_that_must_be_contained_within(bags, inner_color) + 1);
            }

            total_bags
        },
        _ => 0
    };
}