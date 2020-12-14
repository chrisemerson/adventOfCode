use std::fs;
use std::collections::HashMap;

fn main() {
    let input = fs::read_to_string("input.txt")
        .expect("Something went wrong reading the file");

    let bits_n_bobs = input
        .lines()
        .map(|line| line.trim().to_string())
        .collect::<Vec<String>>();

    let mut memory:HashMap<i32, i64> = HashMap::new();
    let mut mask_or:i64 = 0;
    let mut mask_and:i64 = 0;

    for bit_or_bob in bits_n_bobs {
        let bits_of_bit_or_bob = bit_or_bob.split(" = ").map(|x| x.to_string()).collect::<Vec<String>>();
        let value = &bits_of_bit_or_bob[1];

        if &bits_of_bit_or_bob[0] == "mask" {
            mask_and = i64::from_str_radix(&value.replace("X", "1"), 2).unwrap();
            mask_or = i64::from_str_radix(&value.replace("X", "0"), 2).unwrap();
        } else {
            let mem_location = &bits_of_bit_or_bob[0][4..bits_of_bit_or_bob[0].len() - 1].parse::<i32>().unwrap();
            let mem_value = &value.parse::<i64>().unwrap() & &mask_and | &mask_or;

            memory.insert(*mem_location, mem_value);
        }
    }

    let memory_sum:i64 = memory.iter().fold(0, |a, (_k, v)| a + v);

    println!("The sum of all the memory values is: {}", memory_sum);
}
