use std::collections::HashSet;
use std::collections::VecDeque;
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt")
        .expect("Something went wrong reading the file");

    let (player1deck, player2deck) = parse_input(input);

    let mut player1cards = player1deck.clone();
    let mut player2cards = player2deck.clone();

    loop {
        let (player1result, player2result) = play_round_combat(player1cards, player2cards);

        player1cards = player1result;
        player2cards = player2result;

        if player1cards.len() == 0 || player2cards.len() == 0 {
            if player2cards.len() == 0 {
                println!("The score of the winning player's hand in Combat is {}", calculate_score(player1cards));
            } else {
                println!("The score of the winning player's hand in Combat is {}", calculate_score(player2cards));
            }

            break;
        }
    }

    let mut player1result = VecDeque::new();
    let mut player2result = VecDeque::new();

    match play_round_recursive_combat(&player1deck, &player2deck) {
        Ok(result) => {
            player1result = result.0;
            player2result = result.1;
        },
        Err(_) => ()
    };

    if player2result.len() == 0 {
        println!("The score of the winning player's hand in Recursive Combat is {}", calculate_score(player1result));
    } else {
        println!("The score of the winning player's hand in Recursive Combat is {}", calculate_score(player2result));
    }
}

fn calculate_score(cards: VecDeque<u32>) -> u32
{
    let mut multiplier:u32 = cards.len() as u32;
    let mut score = 0;

    for card in cards {
        score += multiplier * card;
        multiplier -= 1;
    }

    return score;
}

fn play_round_combat(player1cards: VecDeque<u32>, player2cards:VecDeque<u32>) -> (VecDeque<u32>, VecDeque<u32>)
{
    let mut player1cardsnew = player1cards.clone();
    let mut player2cardsnew = player2cards.clone();

    let player1card = player1cardsnew.pop_front().unwrap();
    let player2card = player2cardsnew.pop_front().unwrap();

    if player1card > player2card {
        player1cardsnew.push_back(player1card);
        player1cardsnew.push_back(player2card);
    } else {
        player2cardsnew.push_back(player2card);
        player2cardsnew.push_back(player1card);
    }

    return (player1cardsnew, player2cardsnew);
}

fn play_round_recursive_combat(player1cards: &VecDeque<u32>, player2cards: &VecDeque<u32>) -> Result<(VecDeque<u32>, VecDeque<u32>), u32>
{
    let mut previous_hands = HashSet::new();

    let mut player1cardsnew = player1cards.clone();
    let mut player2cardsnew = player2cards.clone();

    loop {
        let hash_of_game_state = get_hash_for_game_state(&player1cardsnew, &player2cardsnew);

        if previous_hands.contains(&hash_of_game_state) {
            //Player 1 wins
            return Err(1);
        }

        previous_hands.insert(hash_of_game_state);

        if player1cardsnew.len() == 0 || player2cardsnew.len() == 0 {
            return Ok((player1cardsnew.clone(), player2cardsnew.clone()));
        }

        let player1card = player1cardsnew.pop_front().unwrap();
        let player2card = player2cardsnew.pop_front().unwrap();

        let winner:i32;

        if player1card > player1cardsnew.len() as u32 || player2card > player2cardsnew.len() as u32 {
            //higher value card wins
            if player1card > player2card {
                winner = 1;
            } else {
                winner = 2;
            }
        } else {
            //New subgame needed with fresh 'previous hands' set
            let new_player1_deck = player1cardsnew.iter().map(|x| x.to_owned()).collect::<Vec<u32>>();
            let new_player2_deck = player2cardsnew.iter().map(|x| x.to_owned()).collect::<Vec<u32>>();

            winner = match play_round_recursive_combat(
                &new_player1_deck[0..player1card as usize].iter().map(|x| x.to_owned()).collect::<VecDeque<u32>>(),
                &new_player2_deck[0..player2card as usize].iter().map(|x| x.to_owned()).collect::<VecDeque<u32>>()) {
                Ok(result) => {
                    if result.0.len() == 0 {
                        2
                    } else if result.1.len() == 0 {
                        1
                    } else {
                        0
                    }
                },
                Err(_) => 1
            }
        }

        if winner == 1 {
            player1cardsnew.push_back(player1card);
            player1cardsnew.push_back(player2card);
        } else if winner == 2 {
            player2cardsnew.push_back(player2card);
            player2cardsnew.push_back(player1card);
        }
    }
}

fn get_hash_for_game_state(player1cards: &VecDeque<u32>, player2cards: &VecDeque<u32>) -> String
{
    let mut hash_string = "".to_string();

    hash_string.push_str(&player1cards.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(","));
    hash_string.push_str(&"|");
    hash_string.push_str(&player2cards.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(","));

    return hash_string;
}

fn parse_input(input: String) -> (VecDeque<u32>, VecDeque<u32>)
{
    let mut player1cards = VecDeque::new();
    let mut player2cards = VecDeque::new();

    let mut player1 = true;


    for line in input.lines() {
        if line == "Player 2:" {
            player1 = false;
        } else {
            if line != "Player 1:" && line != "" {
                if player1 {
                    player1cards.push_back(line.parse::<u32>().unwrap());
                } else {
                    player2cards.push_back(line.parse::<u32>().unwrap());
                }
            }
        }
    }

    return (player1cards, player2cards);
}
