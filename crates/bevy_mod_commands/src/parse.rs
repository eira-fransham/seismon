//! [`nom`] parsers for command scripts.
//!
//! Currently exposed so that consumers can use these parsers if a command should
//! take other commands as arguments (e.g. `bind`).

use nom::{
    Parser,
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{line_ending, not_line_ending, one_of, space0},
    combinator::{opt, recognize},
    multi::many0,
    sequence::{delimited, pair, preceded, terminated, tuple},
};

use crate::{CmdName, RunCmd, Trigger};

fn string_contents(input: &str) -> nom::IResult<&str, &str> {
    take_while1(|c: char| !"\"".contains(c) && c.is_ascii() && !c.is_ascii_control())(input)
}

/// Match a double-quoted string, returning its contents.
pub fn quoted(input: &str) -> nom::IResult<&str, &str> {
    delimited(tag("\""), string_contents, tag("\""))(input)
}

/// Match a line comment.
///
/// A line comment is considered to be composed of:
/// - Two forward slashes (`"//"`)
/// - Zero or more characters, excluding line endings (`"\n"` or `"\r\n"`)
pub fn line_comment(input: &str) -> nom::IResult<&str, &str> {
    recognize(preceded(tag("//"), not_line_ending))(input)
}

/// Match an empty line.
///
/// An empty line is considered to be composed of:
/// - Zero or more spaces or tabs
/// - An optional line comment
/// - A line ending (`"\n"` or `"\r\n"`)
pub fn empty_line(input: &str) -> nom::IResult<&str, &str> {
    recognize(tuple((space0, opt(line_comment), line_ending)))(input)
}

/// Match a basic argument terminator.
///
/// Basic (unquoted) arguments can be terminated by any of:
/// - A non-newline whitespace character (`" "` or `"\t"`)
/// - The beginning of a line comment (`"//"`)
/// - A line ending (`"\r\n"` or `"\n"`)
/// - A semicolon (`";"`)
pub fn basic_arg_terminator(input: &str) -> nom::IResult<&str, &str> {
    alt((recognize(one_of(" \t;")), line_ending, tag("//")))(input)
}

/// Match a sequence of any non-whitespace, non-line-ending ASCII characters,
/// ending with whitespace, a line comment or a line terminator.
pub fn basic_arg(input: &str) -> nom::IResult<&str, &str> {
    // break on comment, semicolon, quote, or whitespace
    let patterns = ["//", ";", "\"", ")", "(", " ", "\t", "\r\n", "\n"];

    // length in bytes of matched sequence
    let mut match_len = 0;

    // consume characters not matching any of the patterns
    loop {
        let (_, remaining) = input.split_at(match_len);
        let terminator = patterns.iter().any(|p| remaining.starts_with(*p));

        let chr = match remaining.chars().next() {
            Some(c) => c,
            None => break,
        };

        if terminator || !chr.is_ascii() || chr.is_ascii_control() {
            break;
        }

        match_len += chr.len_utf8();
    }

    match match_len {
        // TODO: more descriptive error?
        0 => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Many1))),
        len => {
            let (matched, rest) = input.split_at(len);
            Ok((rest, matched))
        }
    }
}

pub fn list(input: &str) -> nom::IResult<&str, &str> {
    recognize(tuple((opt(tag("#")), tag("("), arg_list, tag(")"))))(input)
}

/// Match a basic argument or a quoted string.
pub fn arg(input: &str) -> nom::IResult<&str, &str> {
    alt((quoted, list, basic_arg))(input)
}

/// Match a command terminator.
///
/// Commands can be terminated by either:
/// - Zero or more spaces or tabs followed by a semicolon (`";"`), or
/// - An empty line, which may contain a line comment (see `empty_line`)
pub fn command_terminator(input: &str) -> nom::IResult<&str, &str> {
    alt((empty_line, recognize(pair(space0, tag(";")))))(input)
}

/// Match whatever may follow a command's arguments: either a single command terminator, or
/// optional whitespace and an optional line comment at the end of the input.
///
/// Always succeeds.
fn command_end(input: &str) -> nom::IResult<&str, &str> {
    alt((command_terminator, recognize(pair(space0, opt(line_comment)))))(input)
}

/// Match anything between commands: blank lines, comment lines, stray semicolons, and a trailing
/// line comment at the end of the input.
///
/// Always succeeds.
fn separators(input: &str) -> nom::IResult<&str, &str> {
    recognize(tuple((many0(command_terminator), space0, opt(line_comment))))(input)
}

pub fn trigger(input: &str) -> nom::IResult<&str, Trigger> {
    alt((tag("+").map(|_| Trigger::Positive), tag("-").map(|_| Trigger::Negative)))(input)
}

pub fn command_name(input: &str) -> nom::IResult<&str, CmdName<'_>> {
    tuple((opt(trigger), arg))
        .map(|(trigger, name)| CmdName { name: name.into(), trigger })
        .parse(input)
}

pub fn arg_list(input: &str) -> nom::IResult<&str, Vec<String>> {
    many0(preceded(space0, arg.map(|arg| arg.to_owned())))(input)
}

/// Match a command name followed by its arguments, without anything that follows them.
fn command_body(input: &str) -> nom::IResult<&str, RunCmd<'_>> {
    tuple((command_name, arg_list)).map(|(cmd, rest)| RunCmd(cmd, rest.into())).parse(input)
}

/// Match a single command.
///
/// A command is considered to be composed of:
/// - A command name, optionally prefixed with `+` or `-`
/// - Zero or more arguments, separated by non-newline whitespace characters
/// - Optionally, a single command terminator (see `command_terminator`), or a line comment at the
///   end of the input
pub fn command(input: &str) -> nom::IResult<&str, RunCmd<'_>> {
    terminated(command_body, command_end)(input)
}

/// Match a sequence of commands, separated by semicolons or line endings, ignoring blank lines
/// and line comments.
pub fn commands(input: &str) -> nom::IResult<&str, Vec<RunCmd<'_>>> {
    preceded(separators, many0(terminated(command, separators)))(input)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_quoted() {
        let s = "\"hello\"";
        assert_eq!(quoted(s), Ok(("", "hello")))
    }

    #[test]
    fn test_line_comment() {
        let result = line_comment("// a comment\nnext line");
        assert_eq!(result, Ok(("\nnext line", "// a comment")));
    }

    #[test]
    fn test_empty_line() {
        let result = empty_line("  \t \t // a comment\nnext line");
        assert_eq!(result, Ok(("next line", "  \t \t // a comment\n")));
    }

    #[test]
    fn test_basic_arg_space_terminated() {
        let result = basic_arg("space_terminated ");
        assert_eq!(result, Ok((" ", "space_terminated")));
    }

    #[test]
    fn test_basic_arg_newline_terminated() {
        let result = basic_arg("newline_terminated\n");
        assert_eq!(result, Ok(("\n", "newline_terminated")));
    }

    #[test]
    fn test_basic_arg_semicolon_terminated() {
        let result = basic_arg("semicolon_terminated;");
        assert_eq!(result, Ok((";", "semicolon_terminated")));
    }

    #[test]
    fn test_arg_basic() {
        let result = arg("basic_arg \t;");
        assert_eq!(result, Ok((" \t;", "basic_arg")));
    }

    #[test]
    fn test_quoted_arg() {
        let result = arg("\"quoted argument\";\n");
        assert_eq!(result, Ok((";\n", "quoted argument")));
    }

    #[test]
    fn test_command_basic() {
        let result = command("arg_0 arg_1;\n");
        assert_eq!(result, Ok(("\n", RunCmd("arg_0".into(), vec!["arg_1".to_owned()].into()))));
    }

    #[test]
    fn test_command_quoted() {
        let result = command("bind \"space\" \"+jump\";\n");
        assert_eq!(
            result,
            Ok(("\n", RunCmd("bind".into(), vec!["space".to_owned(), "+jump".to_owned()].into())))
        );
    }

    #[test]
    fn test_command_comment() {
        let result = command("bind \"space\" \"+jump\" // bind space to jump\n\n");
        assert_eq!(
            result,
            Ok(("\n", RunCmd("bind".into(), vec!["space".to_owned(), "+jump".to_owned()].into())))
        );
    }

    #[test]
    fn test_command_comment_at_eof() {
        let result = command("a // c");
        assert_eq!(result, Ok(("", RunCmd("a".into(), vec![].into()))));
    }

    #[test]
    fn test_command_space_before_semicolon() {
        let result = command("a ;b");
        assert_eq!(result, Ok(("b", RunCmd("a".into(), vec![].into()))));
    }

    #[test]
    fn test_commands_space_before_semicolon_and_trailing_comment() {
        let result = commands("a ; b // c");
        assert_eq!(
            result,
            Ok(("", vec![RunCmd("a".into(), vec![].into()), RunCmd("b".into(), vec![].into())]))
        );
    }

    #[test]
    fn test_commands_mixed_terminators() {
        let result = commands("a;b\nc");
        assert_eq!(
            result,
            Ok((
                "",
                vec![
                    RunCmd("a".into(), vec![].into()),
                    RunCmd("b".into(), vec![].into()),
                    RunCmd("c".into(), vec![].into()),
                ]
            ))
        );
    }

    #[test]
    fn test_commands_empty() {
        assert_eq!(commands(""), Ok(("", vec![])));
        assert_eq!(commands("// only a comment"), Ok(("", vec![])));
        assert_eq!(commands(" ; ;\n// c\n  "), Ok(("", vec![])));
    }

    #[test]
    fn test_run_cmd_parse_terminated() {
        let expected = RunCmd("map".into(), vec!["start".to_owned()].into());
        assert_eq!(RunCmd::parse("map start;"), Ok(expected.clone()));
        assert_eq!(RunCmd::parse("map start // c"), Ok(expected.clone()));
        assert_eq!(RunCmd::parse("map start\n"), Ok(expected));
        assert!(RunCmd::parse("map start; next").is_err());
    }

    #[test]
    fn test_commands_script() {
        let script = "
// load the base configuration
exec default.cfg

// load the last saved configuration
exec config.cfg

// run a user script file if present
exec autoexec.cfg

//
// stuff command line statements
//
stuffcmds

// start demos if not already running a server
startdemos demo1 demo2 demo3
";
        let expected = vec![
            RunCmd("exec".into(), vec!["default.cfg".to_owned()].into()),
            RunCmd("exec".into(), vec!["config.cfg".to_owned()].into()),
            RunCmd("exec".into(), vec!["autoexec.cfg".to_owned()].into()),
            RunCmd("stuffcmds".into(), vec![].into()),
            RunCmd(
                "startdemos".into(),
                vec!["demo1".to_owned(), "demo2".to_owned(), "demo3".to_owned()].into(),
            ),
        ];

        let result = commands(script);
        assert_eq!(result, Ok(("", expected)));
    }
}
