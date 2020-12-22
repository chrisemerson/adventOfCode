use std::collections::HashMap;
use std::fs;

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

    let mut skip_time:i64 = 1;
    let mut time:i64 = 0;

    loop {
        let mut biggest_bus_not_in_skip_time:i64 = 0;

        for (_t, bus) in &bus_times {
            if bus > &biggest_bus_not_in_skip_time && &skip_time % bus != 0 {
                biggest_bus_not_in_skip_time = bus.to_owned();
            }
        }

        if biggest_bus_not_in_skip_time == 0 {
            println!("The amazing bus sequence starts at time {}", time);
            break;
        }

        let biggest_bus_offsets = bus_times
            .iter()
            .filter(|(_k, v)| *v == &biggest_bus_not_in_skip_time)
            .map(|(k, _v)| *k)
            .collect::<Vec<i64>>();

        let biggest_bus_offset = biggest_bus_offsets.get(0).unwrap();

        loop {
            if (time + biggest_bus_offset) % biggest_bus_not_in_skip_time == 0 {
                skip_time *= biggest_bus_not_in_skip_time;
                break;
            }

            time += skip_time;
        }
    }
}
