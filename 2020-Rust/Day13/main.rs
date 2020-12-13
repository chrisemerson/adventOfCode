use std::fs;
use std::collections::HashMap;

fn main() {
    let input = fs::read_to_string("input.txt")
        .expect("Something went wrong reading the file");

    let bus_info = input
        .lines()
        .map(|line| line.trim().to_string())
        .collect::<Vec<String>>();

    let original_timestamp = bus_info
        .get(0)
        .unwrap()
        .parse::<i32>()
        .unwrap();

    let buses = bus_info
        .get(1)
        .unwrap()
        .split(",")
        .filter(|bus| bus != &"x")
        .map(|bus| bus
            .parse::<i32>()
            .unwrap()
        )
        .collect::<Vec<i32>>();

    let mut timestamp = original_timestamp;

    'busloop: loop {
        for bus in &buses {
            if timestamp % bus == 0 {
                println!("Bus {} is the earliest to depart, after {} minutes at time {}. The answer is {}.", bus, timestamp - original_timestamp, timestamp, bus * (timestamp - original_timestamp));
                break 'busloop;
            }
        }

        timestamp += 1;
    }

    let buses = bus_info
        .get(1)
        .unwrap()
        .split(",")
        .map(|x| x.to_string())
        .collect::<Vec<String>>();

    let mut bus_times:HashMap<i64, i64> = HashMap::new();

    for (t, bus) in buses.iter().enumerate() {
        if bus != "x" {
            let bus_id = bus.parse::<i64>().unwrap();
            bus_times.insert(t as i64, bus_id);
        }
    }

    let product_of_moduli: i64 = bus_times.iter().fold(1, |a, (_t, b)| a * b );

    let mut timestamp: i64 = bus_times
        .iter()
        .map(|(t, b)| (0 - t) * (product_of_moduli / b) * mod_inv(product_of_moduli / b, *b))
        .fold(0, |a, b| a + b);

    while timestamp > 0 {
        timestamp -= product_of_moduli;
    }

    while timestamp < 0 {
        timestamp += product_of_moduli;
    }

    println!("The amazing bus sequence starts at time {}", timestamp);
}

fn mod_inv(a: i64, module: i64) -> i64 {
    let mut mn = (module, a);
    let mut xy = (0, 1);

    while mn.1 != 0 {
        xy = (xy.1, xy.0 - (mn.0 / mn.1) * xy.1);
        mn = (mn.1, mn.0 % mn.1);
    }

    while xy.0 < 0 {
        xy.0 += module;
    }
    xy.0
}
