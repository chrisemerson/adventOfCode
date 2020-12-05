use std::fs;

#[derive(Debug)]
struct Passport {
    byr: Option<String>,
    iyr: Option<String>,
    eyr: Option<String>,
    hgt: Option<String>,
    hcl: Option<String>,
    ecl: Option<String>,
    pid: Option<String>,
    cid: Option<String>
}

fn main() {
    let input = fs::read_to_string("input.txt")
        .expect("Something went wrong reading the file");

    let parsed_passports:Vec<Passport> = input
        .split("\n\n")
        .map(|pp| parse_passport(&pp))
        .collect();

    println!(
        "There are {} passports, of which {} have all required fields, and {} have valid data",
        parsed_passports.len(),
        parsed_passports.iter().filter(|passport| passport_has_required_fields(passport)).collect::<Vec<&Passport>>().len(),
        parsed_passports.iter().filter(|passport| passport_is_valid(passport)).collect::<Vec<&Passport>>().len()
    );
}

fn parse_passport(passport_text: &str) -> Passport {
    let mut byr:Option<String> = None;
    let mut iyr:Option<String> = None;
    let mut eyr:Option<String> = None;
    let mut hgt:Option<String> = None;
    let mut hcl:Option<String> = None;
    let mut ecl:Option<String> = None;
    let mut pid:Option<String> = None;
    let mut cid:Option<String> = None;

    let passport_text_singleline = passport_text.replace("\n", " ");
    let passport_parts:Vec<&str> = passport_text_singleline.split(" ").collect();

    for passport_part in passport_parts {
        let passport_part_components:Vec<&str> = passport_part.split(":").collect();

        match passport_part_components[0].trim() {
            "byr" => byr = Some(passport_part_components[1].to_string()),
            "iyr" => iyr = Some(passport_part_components[1].to_string()),
            "eyr" => eyr = Some(passport_part_components[1].to_string()),
            "hgt" => hgt = Some(passport_part_components[1].to_string()),
            "hcl" => hcl = Some(passport_part_components[1].to_string()),
            "ecl" => ecl = Some(passport_part_components[1].to_string()),
            "pid" => pid = Some(passport_part_components[1].to_string()),
            "cid" => cid = Some(passport_part_components[1].to_string()),

            _ => println!("Unknown passport component: {}", passport_part_components[0])
        }
    }

    return Passport { byr, iyr, eyr, hgt, hcl, ecl, pid, cid }
}

fn passport_has_required_fields(passport: &Passport) -> bool {
    return
        passport.byr.is_some()
            && passport.iyr.is_some()
            && passport.eyr.is_some()
            && passport.hgt.is_some()
            && passport.hcl.is_some()
            && passport.ecl.is_some()
            && passport.pid.is_some()
}

fn passport_is_valid(passport: &Passport) -> bool {
    if !passport_has_required_fields(passport) {
        return false;
    }

    return
        validate_year(passport.byr.as_ref().unwrap(), 1920, 2020)
            && validate_year(passport.iyr.as_ref().unwrap(), 2010, 2020)
            && validate_year(passport.eyr.as_ref().unwrap(), 2020, 2030)
            && validate_height(passport.hgt.as_ref().unwrap())
            && validate_hair_color(passport.hcl.as_ref().unwrap())
            && validate_eye_color(passport.ecl.as_ref().unwrap())
            && validate_passport_id(passport.pid.as_ref().unwrap());
}

fn validate_year(year: &str, min: u32, max: u32) -> bool {
    let parsed_year = year.parse::<u32>();

    return match parsed_year {
        Ok(y) => y >= min && y <= max,
        Err(_e) => false
    };
}

fn validate_height(height: &str) -> bool {
    let unit = &height[(height.len() - 2)..];
    let value = &height[..(height.len() - 2)].parse::<i32>();

    return match value {
        Err(_e) => false,
        Ok(v) => match unit {
            "cm" => v >= &150 && v <= &193,
            "in" => v >= &59 && v <= &76,
            _ => false
        }
    };
}

fn validate_hair_color(hair_color: &str) -> bool {
    let first_char = &hair_color[..1];
    let remaining_chars = &hair_color[1..];

    if first_char != "#" || remaining_chars.len() != 6 {
        return false;
    }

    for char in remaining_chars.chars() {
        match char {
            'a' | 'b' | 'c' | 'd' | 'e' | 'f' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => (),
            _ => return false
        }
    }

    return true;
}

fn validate_eye_color(eye_color: &str) -> bool {
    return match eye_color {
        "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" => true,
        _ => false
    }
}

fn validate_passport_id(passport_id: &str) -> bool {
    if passport_id.len() != 9 {
        return false;
    }

    for char in passport_id.chars() {
        match char {
            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => (),
            _ => return false
        }
    }

    return true;
}