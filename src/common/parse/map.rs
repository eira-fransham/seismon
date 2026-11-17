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

use bevy::log::info;
use pest::Parser as _;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "common/parse/map.pest"]
struct MapEntitiesParser;

pub fn entities(
    input: &str,
) -> Result<impl Iterator<Item = impl Iterator<Item = (&str, &str)>>, Box<pest::error::Error<Rule>>>
{
    let input = input.strip_suffix('\0').unwrap_or(input);

    Ok(MapEntitiesParser::parse(Rule::map, input)?
        .filter_map(|p| match p.as_rule() {
            Rule::map => Some(p.into_inner()),
            rule => {
                info!("Found {rule:?}");
                None
            }
        })
        .flatten()
        .filter_map(|p| match p.as_rule() {
            Rule::object => Some(p.into_inner()),
            rule => {
                info!("Found {rule:?}");
                None
            }
        })
        .map(|pair| {
            pair.filter_map(|p| match p.as_rule() {
                Rule::key_value => {
                    let mut kv =
                        p.into_inner().filter(|pair| matches!(pair.as_rule(), Rule::string));

                    let k = kv
                        .next()
                        .unwrap()
                        .into_inner()
                        .filter_map(|pair| match pair.as_rule() {
                            Rule::string_inner => Some(pair.as_str()),
                            _ => None,
                        })
                        .next()
                        .unwrap();
                    let v = kv
                        .next()
                        .unwrap()
                        .into_inner()
                        .filter_map(|pair| match pair.as_rule() {
                            Rule::string_inner => Some(pair.as_str()),
                            _ => None,
                        })
                        .next()
                        .unwrap();

                    Some((k, v))
                }
                rule => {
                    info!("Found {rule:?}");
                    None
                }
            })
        }))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(input: &str) -> Vec<Vec<(&str, &str)>> {
        entities(input).unwrap().map(|entity| entity.collect()).collect()
    }

    #[test]
    fn backslashes_and_newlines_in_values() {
        // From `start.bsp`: backslashes must not end the parse (and drop the rest of the map).
        let map = "{\n\"classname\" \"trigger_onlyregistered\"\n\"message\" \"For registered users only!\\nPlay episode 1\"\n}\n{\n\"classname\" \"trigger_changelevel\"\n}\n";
        assert_eq!(
            parse(map),
            [
                vec![
                    ("classname", "trigger_onlyregistered"),
                    ("message", r"For registered users only!\nPlay episode 1"),
                ],
                vec![("classname", "trigger_changelevel")],
            ]
        );

        // As in `COM_Parse`, quoted strings may span lines.
        assert_eq!(parse("{ \"a\" \"b\nc\" }"), [vec![("a", "b\nc")]]);
    }

    #[test]
    fn syntax_error_fails() {
        assert!(entities("{ \"a\" \"b\" } { \"unterminated").is_err());
    }
}
