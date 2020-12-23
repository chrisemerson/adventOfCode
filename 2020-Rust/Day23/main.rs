fn main() {
    let cups:Vec<u32> = vec![9, 5, 2, 3, 1, 6, 4, 8, 7];

    let final_cups_pt1 = run_game(cups.clone(), cups.len(), 100);

    let mut cup = get_next_cup(&final_cups_pt1, 1);

    print!("Final cup order: ");

    loop {
        if cup == 1 {
            break;
        }

        print!("{}", cup);

        cup = get_next_cup(&final_cups_pt1, cup as usize);
    }

    println!();

    let final_cups_pt2 = run_game(cups.clone(), 1000000, 10000000);

    let cup_after_1 = get_next_cup(&final_cups_pt2, 1);
    let cup_after_that = get_next_cup(&final_cups_pt2, cup_after_1 as usize);

    println!("Product of 2 cups after 1 with 10,000,000 turns and 1,000,000 cups: {}", cup_after_1 as u64 * cup_after_that as u64)
}

fn run_game(initial_cups: Vec<u32>, num_cups: usize, turns: u32) -> Vec<u32> {
    let mut current_turn = 1;

    let mut cups = create_cups_linked_list(&initial_cups, num_cups);
    let mut current_cup = initial_cups[0];

    loop {
        let moved_cup_1 = get_next_cup(&cups, current_cup as usize);
        let moved_cup_2 = get_next_cup(&cups, moved_cup_1 as usize);
        let moved_cup_3 = get_next_cup(&cups, moved_cup_2 as usize);
        let cup_after_moved_cups = get_next_cup(&cups, moved_cup_3 as usize);

        let mut destination_cup_label = current_cup - 1;

        while destination_cup_label == 0 || destination_cup_label == moved_cup_1 || destination_cup_label == moved_cup_2 || destination_cup_label == moved_cup_3 {
            if destination_cup_label <= 1 {
                destination_cup_label = num_cups as u32;
            } else {
                destination_cup_label -= 1;
            }
        }

        let cup_after_destination_cup = get_next_cup(&cups, destination_cup_label as usize);

        cups[current_cup as usize] = cup_after_moved_cups;
        cups[destination_cup_label as usize] = moved_cup_1;
        cups[moved_cup_3 as usize] = cup_after_destination_cup;

        current_cup = get_next_cup(&cups, current_cup as usize);

        if current_turn == turns {
            break;
        }

        current_turn += 1;
    }

    return cups;
}

fn get_next_cup(cups: &Vec<u32>, index: usize) -> u32 {
    return cups[index];
}

fn create_cups_linked_list(initial_cups: &Vec<u32>, num_cups: usize) -> Vec<u32>
{
    let mut cups:Vec<u32> = vec![0; num_cups + 1];
    let mut last_label = 0;

    for cup in initial_cups {
        cups[last_label] = *cup;
        last_label = *cup as usize;
    }

    for i in initial_cups.len() + 1 .. num_cups + 1 {
        cups[last_label] = i as u32;
        last_label = i;
    }

    cups[last_label] = initial_cups[0];

    return cups;
}
