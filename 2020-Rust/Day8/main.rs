use std::fs;

#[derive(Clone)]
struct Instruction {
    operation: String,
    value: i32
}

struct ProgramState {
    visited_lines: Vec<i32>,
    current_instruction: i32,
    accumulator: i32
}

fn main() {
    let input = fs::read_to_string("input.txt")
        .expect("Something went wrong reading the file");

    let mut program_state = ProgramState {
        visited_lines: Vec::new(),
        current_instruction: 0,
        accumulator: 0
    };

    let instructions = parse_instructions(&input.lines().collect::<Vec<&str>>());

    'program_pt1: loop {
        match process_instruction(&program_state, &instructions) {
            Ok(new_state) => program_state = new_state,
            Err(accumulator) => {
                println!("Line about to be executed twice. Accumulator is {}", accumulator);
                break 'program_pt1;
            }
        }
    }

    'meta_program: for i in 0..instructions.len() {
        let mut altered_instructions = instructions.clone();

        //Change instructions on line i if jmp or nop
        match instructions[i].operation.as_str() {
            "nop" => {
                altered_instructions[i].operation = "jmp".to_string();
            },
            "jmp" => {
                altered_instructions[i].operation = "nop".to_string();
            }
            "acc" => continue,
            _ => ()
        }

        let mut program_state = ProgramState {
            visited_lines: Vec::new(),
            current_instruction: 0,
            accumulator: 0
        };

        'program_pt2: loop {
            match process_instruction(&program_state, &altered_instructions) {
                Ok(new_state) => {
                    program_state = new_state;

                    if program_state.current_instruction == instructions.len() as i32 {
                        println!("End of program reached after altering line {}. Accumulator value is {}", i, program_state.accumulator);
                        break 'meta_program;
                    }
                },
                Err(_acc) => break 'program_pt2
            }
        }
    }
}

fn parse_instructions(input_lines: &Vec<&str>) -> Vec<Instruction> {
    let mut instructions: Vec<Instruction> = Vec::new();

    for input_line in input_lines {
        let instruction_parts = input_line.split(" ").collect::<Vec<&str>>();

        let sign = &instruction_parts[1][..1];
        let value = instruction_parts[1][1..].parse::<i32>().unwrap();

        instructions.push(Instruction {
            operation: instruction_parts[0].to_string(),
            value: match sign.as_ref() {
                "+" => value,
                "-" => 0 - value,
                _ => 0
            }
        });
    }

    return instructions;
}

fn process_instruction(current_state: &ProgramState, instructions: &Vec<Instruction>) -> Result<ProgramState, i32> {
    let instruction = &instructions[current_state.current_instruction as usize];

    if current_state.visited_lines.contains(&current_state.current_instruction) {
        return Err(current_state.accumulator);
    }

    let mut new_visited_lines = current_state.visited_lines.clone();
    new_visited_lines.push(current_state.current_instruction);
    let mut new_current_instruction = current_state.current_instruction + 1;
    let mut new_accumulator = current_state.accumulator;

    match instruction.operation.as_str() {
        "acc" => new_accumulator += instruction.value,
        "jmp" => new_current_instruction += instruction.value - 1,
        "nop" => (),
        _ => println!("Invalid instruction found: {}", instruction.operation)
    }

    return Ok(ProgramState {
        visited_lines: new_visited_lines,
        current_instruction: new_current_instruction,
        accumulator: new_accumulator
    });
}
