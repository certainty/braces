use rustc_hash::FxHashSet;
use rustyline::completion::Completer;
use rustyline::completion::Pair;
use rustyline::Context;

pub struct StringCompleter {
    strings: FxHashSet<String>,
}

impl StringCompleter {
    pub fn from<I: Into<String>>(input: Vec<I>) -> Self {
        let mut strings = FxHashSet::default();

        for s in input {
            strings.insert(s.into());
        }

        Self { strings }
    }

    fn complete_string(&self, pos: usize, input: &str) -> rustyline::Result<(usize, Vec<Pair>)> {
        let mut all_matches: Vec<Pair> = self
            .strings
            .iter()
            .filter_map(|known| {
                if known.starts_with(input) {
                    Some(Pair {
                        display: String::from(input),
                        replacement: known.clone(),
                    })
                } else {
                    None
                }
            })
            .collect();

        all_matches.sort_by(|a, b| a.display.cmp(&b.display));
        Ok((pos - input.len(), all_matches))
    }
}

impl Completer for StringCompleter {
    type Candidate = Pair;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Pair>)> {
        self.complete_string(pos, line)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn find_longest_matches() {
        let completer = StringCompleter::from(vec!["foobar", "foo", "baz", "yo"]);

        let (pos, matches) = completer.complete_string(10, "foo").unwrap();

        assert_eq!(pos, 7);
        assert_eq!(matches.len(), 2);

        assert_eq!(
            matches
                .iter()
                .map(|e| e.replacement.clone())
                .collect::<Vec<_>>(),
            vec![String::from("foobar"), String::from("foo")]
        );

        let (_, matches) = completer.complete_string(10, "nope").unwrap();
        assert!(matches.len() == 0, "Expected no matches");
    }
}
