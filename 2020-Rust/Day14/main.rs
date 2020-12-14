use std::fs;
use std::collections::HashMap;

fn main() {
    let input = fs::read_to_string("input.txt")
        .expect("Something went wrong reading the file");

    let bits_n_bobs = input
        .lines()
        .map(|line| line.trim().to_string())
        .collect::<Vec<String>>();

    let mut memory:HashMap<i64, i64> = HashMap::new();
    let mut mask_or:i64 = 0;
    let mut mask_and:i64 = 0;

    for bit_or_bob in &bits_n_bobs {
        let bits_of_bit_or_bob = bit_or_bob.split(" = ").map(|x| x.to_string()).collect::<Vec<String>>();
        let value = &bits_of_bit_or_bob[1];

        if &bits_of_bit_or_bob[0] == "mask" {
            mask_and = i64::from_str_radix(&value.replace("X", "1"), 2).unwrap();
            mask_or = i64::from_str_radix(&value.replace("X", "0"), 2).unwrap();
        } else {
            let mem_location = &bits_of_bit_or_bob[0][4..bits_of_bit_or_bob[0].len() - 1].parse::<i64>().unwrap();
            let mem_value = &value.parse::<i64>().unwrap() & &mask_and | &mask_or;

            memory.insert(*mem_location, mem_value);
        }
    }

    let memory_sum:i64 = memory.iter().fold(0, |a, (_k, v)| a + v);

    println!("The sum of all the memory values is: {}", memory_sum);

    let mut memory:HashMap<i64, i64> = HashMap::new();
    let mut mask:String = "".to_string();

    for bit_or_bob in &bits_n_bobs {
        let bits_of_bit_or_bob = bit_or_bob.split(" = ").map(|x| x.to_string()).collect::<Vec<String>>();
        let value = &bits_of_bit_or_bob[1];

        if &bits_of_bit_or_bob[0] == "mask" {
            mask = value.to_string();
        } else {
            let mut this_address_mask = "".to_string();
            let base_address = format!("{:036b}", bits_of_bit_or_bob[0][4..bits_of_bit_or_bob[0].len() - 1].parse::<i64>().unwrap());

            for (ix, char) in mask.chars().enumerate() {
                match char {
                    'X' => this_address_mask.push_str("X"),
                    '1' => this_address_mask.push_str("1"),
                    '0' => this_address_mask.push_str(&base_address.chars().nth(ix).unwrap().to_string()),
                    _ => panic!("Unknown character in mask: {}", char)
                }
            }

            let memory_addresses:Vec<i64> = get_memory_addresses_from_mask(this_address_mask.to_string());
            let mem_value = value.parse::<i64>().unwrap();

            for memory_address in memory_addresses {
                memory.insert(memory_address, mem_value);
            }
        }
    }

    let memory_sum:i64 = memory.iter().fold(0, |a, (_k, v)| a + v);

    println!("The sum of all the memory values is: {}", memory_sum);
}

fn get_memory_addresses_from_mask(mask: String) -> Vec<i64> {
    let mut memory_addresses:Vec<i64> = Vec::new();

    match mask.find("X") {
        Some(pos) => {
            let mut mask_0 = mask[..pos].to_owned();
            let mut mask_1 = mask[..pos].to_owned();

            let rest_of_mask = &mask[pos+1..];

            mask_0.push_str("0");
            mask_0.push_str(&rest_of_mask);

            mask_1.push_str("1");
            mask_1.push_str(&rest_of_mask);

            memory_addresses.extend(get_memory_addresses_from_mask(
                mask_0
            ));

            memory_addresses.extend(get_memory_addresses_from_mask(
                mask_1
            ));
        },
        None => return vec![i64::from_str_radix(&mask, 2).unwrap()]
    }

    return memory_addresses;
}