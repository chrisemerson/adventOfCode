fn main() {
    let public_key_1:i64 = 14788856;
    let public_key_2:i64 = 19316454;

    let mut loop_size = 0;
    let mut current = 1;

    loop {
        loop_size += 1;

        current = (current * 7) % 20201227;

        if current == public_key_1 {
            break;
        }
    }

    println!("Final door encryption key: {}", transform_subject_number(public_key_2, loop_size));
}

fn transform_subject_number(subject_number: i64, loop_size: i64) -> i64 {
    let mut current:i64 = 1;

    for _ in 0 .. loop_size {
        current = (current * subject_number) % 20201227;
    }

    return current;
}