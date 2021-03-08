
#[derive(Debug)]
pub struct ParseSuccess<'a> {
    pub num: u32,
    pub rest: &'a str,
}

#[derive(Debug)]
pub struct ParseFailure<'a> {
    pub rest: &'a str
}

pub type ParseResult<'a> = Result<
    ParseSuccess<'a>,
    ParseFailure<'a>
>;


pub fn parse_roman_numerals(input: &str) -> ParseResult {
    let mut num_ = 0;
    let mut rest_ = &input[..];
    
    //
    if rest_.starts_with('M') {
        rest_ = &rest_[1..];
        num_ += 1000;
    }
    
    //
    let ParseSuccess{ num, rest } = parse_one(rest_, 'C', 'D', 'M')?;
    num_ += 100 * num;
    rest_ = rest;
    
    //
    let ParseSuccess{ num, rest } = parse_one(rest_, 'X', 'L', 'C')?;
    num_ += 10 * num;
    rest_ = rest;
    
    //
    let ParseSuccess{ num, rest } = parse_one(rest_, 'I', 'V', 'X')?;
    num_ += num;
    rest_ = rest;

    //
    Ok(ParseSuccess{ num: num_, rest: rest_ })
}

fn parse_one(input: &str, one: char, five: char, ten: char) -> ParseResult {
    let mut num = 0;
    let mut rest = input;
    
    if rest.starts_with(one) { // I...
        rest = &rest[1..];
        if rest.starts_with(one) { // II...
            rest = &rest[1..];
            if rest.starts_with(one) { // III...
                rest = &rest[1..];
                num = 3;
            }
            else {
                num = 2;
            }
        }
        else if rest.starts_with(five) { // IV...
            rest = &rest[1..];
            num = 4;
        }
        else if rest.starts_with(ten) { // IX...
            rest = &rest[1..];
            num = 9;
        }
        else {
            num = 1;
        }
    }
    else if rest.starts_with(five) { // V...
        rest = &rest[1..];
        if rest.starts_with(one) { // VI...
            rest = &rest[1..];
            if rest.starts_with(one) { // VII...
                rest = &rest[1..];
                if rest.starts_with(one) { // VIII...
                    rest = &rest[1..];
                    num = 8;
                }
                else {
                    num = 7;
                }
            } else {
                num = 6;
            }
        } else {
            num = 5;
        }
    }
     
    
    Ok(ParseSuccess{ num, rest })
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_symbols_parse_correctly() {
        let digit = parse_roman_numerals("I");
        assert_eq!(digit.unwrap().num, 1);
        
        let digit = parse_roman_numerals("V");
        assert_eq!(digit.unwrap().num, 5);
        
        let digit = parse_roman_numerals("X");
        assert_eq!(digit.unwrap().num, 10);
        
        let digit = parse_roman_numerals("L");
        assert_eq!(digit.unwrap().num, 50);
        
        let digit = parse_roman_numerals("C");
        assert_eq!(digit.unwrap().num, 100);
        
        let digit = parse_roman_numerals("D");
        assert_eq!(digit.unwrap().num, 500);
        
        let digit = parse_roman_numerals("M");
        assert_eq!(digit.unwrap().num, 1000);
    }
    
    #[test]
    fn ones_parse_correctly() {
        ones_parse_correctly_helper(1, 'I', 'V', 'X');
    }
    
    #[test]
    fn tens_parse_correctly() {
        ones_parse_correctly_helper(10, 'X', 'L', 'C');
    }
    
    #[test]
    fn hundredds_parse_correctly() {
        ones_parse_correctly_helper(100, 'C', 'D', 'M');
    }
    
    #[rustfmt::skip]
    fn ones_parse_correctly_helper(scale: u32, one:char, five: char, ten: char) {
        let _1: String = [  one                 ].iter().collect();
        let _2: String = [  one,  one           ].iter().collect();
        let _3: String = [  one,  one, one      ].iter().collect();
        let _4: String = [  one, five           ].iter().collect();
        let _5: String = [ five                 ].iter().collect();
        let _6: String = [ five,  one           ].iter().collect();
        let _7: String = [ five,  one, one      ].iter().collect();
        let _8: String = [ five,  one, one, one ].iter().collect();
        let _9: String = [  one,  ten           ].iter().collect();
        
        assert_eq!(parse_roman_numerals(&_1).unwrap().num, 1 * scale);
        assert_eq!(parse_roman_numerals(&_2).unwrap().num, 2 * scale);
        assert_eq!(parse_roman_numerals(&_3).unwrap().num, 3 * scale);
        assert_eq!(parse_roman_numerals(&_4).unwrap().num, 4 * scale);
        assert_eq!(parse_roman_numerals(&_5).unwrap().num, 5 * scale);
        assert_eq!(parse_roman_numerals(&_6).unwrap().num, 6 * scale);
        assert_eq!(parse_roman_numerals(&_7).unwrap().num, 7 * scale);
        assert_eq!(parse_roman_numerals(&_8).unwrap().num, 8 * scale);
        assert_eq!(parse_roman_numerals(&_9).unwrap().num, 9 * scale);
    }
    
    #[test]
    fn try_some_numbers() {
        try_number_helper(     "XI",  11);
        try_number_helper(     "XV",  15);
        try_number_helper(    "XIV",  14);
        try_number_helper(  "XVIII",  18);
        try_number_helper("CMXXXII", 932);
    }
    
    fn try_number_helper(input: &str, expected: u32) {
        assert_eq!(parse_roman_numerals(input).unwrap().num, expected);
    }
    
}