use std::fs;
use std::collections::HashMap;
use std::collections::HashSet;

fn main() {
    let input = fs::read_to_string("input.txt")
        .expect("Something went wrong reading the file");

    let possible_allergens = parse_input(&input);
    let mut foods_with_possible_allergens:HashSet<String> = HashSet::new();

    for foods in possible_allergens.values() {
        foods_with_possible_allergens = foods_with_possible_allergens.union(foods).map(|x| x.to_string()).collect();
    }

    let mut food_without_allergens_count = 0;

    for line in input.lines() {
        if line != "" {
            let food_parts = line.split(" (contains ").map(|x| x.to_string()).collect::<Vec<String>>();
            let ingredients = food_parts[0].split(" ").map(|x| x.to_string()).collect::<Vec<String>>();

            for ingredient in ingredients {
                match foods_with_possible_allergens.get(&ingredient) {
                    Some(_) => (),
                    None => {
                        food_without_allergens_count += 1;
                    }
                }
            }
        }
    }

    println!("There are {} foods listed that cannot have an allergen", food_without_allergens_count);

    let mut possible_allergens_by_food = get_possible_allergens_by_food(possible_allergens);
    let mut final_allergy_list:HashMap<String, String> = HashMap::new();

    loop {
        let pabf = possible_allergens_by_food.clone();

        for (food, allergens) in pabf {
            if allergens.len() == 1 {
                let allergen = allergens.iter().next().unwrap();
                final_allergy_list.insert(allergen.to_string(), food.to_string());
                possible_allergens_by_food = remove_allergen_from_list(&possible_allergens_by_food, &allergen);
            }
        }

        if possible_allergens_by_food.len() == 0 {
            break;
        }
    }

    let mut final_allergen_list_sorted:Vec<String> = Vec::new();

    let mut sorted_allergens = final_allergy_list.keys().map(|x| x.to_string()).collect::<Vec<String>>();
    sorted_allergens.sort();

    for allergen in sorted_allergens {
        match final_allergy_list.get(&allergen) {
            Some(res) => final_allergen_list_sorted.push(res.to_string()),
            _ => ()
        }
    }

    println!("The final food list sorted alphabetically by allergy is {:?}", final_allergen_list_sorted.join(","));
}

fn remove_allergen_from_list(possible_allergens_by_food: &HashMap<String, HashSet<String>>, allergen: &String) -> HashMap<String, HashSet<String>>{
    let mut result:HashMap<String, HashSet<String>> = HashMap::new();

    for (food, allergens) in possible_allergens_by_food {
        let mut new_allergens = allergens.clone();

        match allergens.get(allergen) {
            Some(_) => {
                new_allergens.remove(allergen);

                if new_allergens.len() > 0 {
                    result.insert(food.to_string(), new_allergens);
                }
            },
            _ => {
                result.insert(food.to_string(), new_allergens);
            }
        };
    }

    return result;
}

fn get_possible_allergens_by_food(possible_allergens: HashMap<String, HashSet<String>>) -> HashMap<String, HashSet<String>> {
    let mut possible_allergens_per_food:HashMap<String, HashSet<String>> = HashMap::new();

    for (allergen, foods) in possible_allergens {
        for food in foods {
            let this_allergen = allergen.clone();

            match possible_allergens_per_food.get(&food) {
                Some(result) => {
                    let mut new_list = result.clone();
                    new_list.insert(this_allergen);
                    possible_allergens_per_food.insert(food, new_list);
                },
                None => {
                    let mut list: HashSet<String> = HashSet::new();
                    list.insert(this_allergen);
                    possible_allergens_per_food.insert(food, list);
                }
            }
        }
    }

    return possible_allergens_per_food;
}

fn parse_input(input: &String) -> HashMap<String, HashSet<String>>
{
    let mut allergens:HashMap<String, HashSet<String>> = HashMap::new();

    for line in input.lines() {
        if line != "" {
            let food_parts = line.split(" (contains ").map(|x| x.to_string()).collect::<Vec<String>>();
            let ingredients = food_parts[0].split(" ").map(|x| x.to_string()).collect::<HashSet<String>>();
            let allergens_list = food_parts[1][.. food_parts[1].len() - 1].split(", ").map(|x| x.to_string()).collect::<Vec<String>>();

            for allergen in allergens_list {
                match allergens.get(&allergen) {
                    Some(result) => allergens.insert(allergen, ingredients.intersection(result).map(|x| x.to_string()).collect()),
                    None => allergens.insert(allergen, ingredients.clone())
                };
            }
        }
    }

    return allergens;
}