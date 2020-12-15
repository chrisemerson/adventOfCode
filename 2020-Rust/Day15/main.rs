use std::collections::HashMap;

fn main() {
    let numbers:Vec<i32> = vec![0,14,6,20,1,4];

    let mut number_age:HashMap<i32, i32> = HashMap::new();
    let mut counter:i32 = 1;
    let mut number_to_say:i32 = 0;
    let mut prev_number_to_say:i32 = 0;

    loop {
        if counter as usize <= numbers.len() {
           number_to_say = *numbers.get(counter as usize - 1).unwrap();
        } else {
            number_to_say = match number_age.get(&number_to_say) {
                Some(n) => counter - *n,
                _ => 0
            };
        }

        number_age.insert(prev_number_to_say, counter);

        if counter == 2020 {
            println!("The 2020th number is {}", number_to_say);
        }

        if counter == 30000000 {
            println!("The 2020th number is {}", number_to_say);
            break;
        }

        counter += 1;
        prev_number_to_say = number_to_say;
    }
}