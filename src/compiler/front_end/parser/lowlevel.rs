use pest::error::Error;
use pest::iterators::Pairs;
use pest::Parser;

#[derive(Parser)]
#[grammar = "compiler/front_end/parser/syntax.pest"]
pub struct Implementation;

pub fn parse_datum(input: &str) -> Result<Pairs<Rule>, Error<Rule>> {
    Implementation::parse(Rule::datum, input)
}
