// Copyright © 2018 Cormac O'Brien
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software
// and associated documentation files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or
// substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
// BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

use crate::common::parse::quoted;

use hashbrown::HashMap;
use nom::{
    Parser,
    branch::alt,
    bytes::complete::tag,
    character::complete::{line_ending, not_line_ending, space0},
    combinator::{all_consuming, map, opt, recognize},
    multi::many0,
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
};

fn empty_line(input: &str) -> nom::IResult<&str, &str> {
    recognize(tuple((
        space0,
        opt(preceded(tag("//"), not_line_ending)),
        line_ending,
    )))(input)
}

// "name" "value"\n
pub fn entity_attribute(input: &str) -> nom::IResult<&str, (&str, &str)> {
    terminated(separated_pair(quoted, space0, quoted), line_ending)(input)
}

// {
// "name1" "value1"
// "name2" "value2"
// "name3" "value3"
// }
pub fn entity(input: &str) -> nom::IResult<&str, HashMap<&str, &str>> {
    delimited(
        terminated(delimited(space0, tag("{"), space0), line_ending),
        map(
            many0(alt((
                delimited(space0, entity_attribute, space0).map(Some),
                empty_line.map(|_| None),
            ))),
            |attrs| attrs.into_iter().flatten().collect(),
        ),
        terminated(delimited(space0, tag("}"), space0), line_ending),
    )(input)
}

pub fn entities(
    input: &str,
) -> Result<Vec<HashMap<&str, &str>>, nom::Err<nom::error::Error<&str>>> {
    let input = input.strip_suffix('\0').unwrap_or(input);
    match all_consuming(many0(alt((empty_line.map(|_| None), entity.map(Some)))))(input) {
        Ok(("", entities)) => Ok(entities.into_iter().flatten().collect()),
        Ok(_) => unreachable!(),
        Err(e) => Err(e),
    }
}
