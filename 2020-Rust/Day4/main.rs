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

    return Passport {
        byr: byr,
        iyr: iyr,
        eyr: eyr,
        hgt: hgt,
        hcl: hcl,
        ecl: ecl,
        pid: pid,
        cid: cid
    }
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

    return true;
}