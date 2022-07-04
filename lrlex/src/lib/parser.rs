use cfgrammar::Span;
use std::collections::HashMap;
use try_from::TryFrom;

use crate::{lexer::Rule, LexBuildError, LexBuildResult, LexErrorKind};

type LexInternalBuildResult<T> = Result<T, LexBuildError>;
#[derive(Debug)]
pub struct StartState {
    pub(super) id: usize,
    pub(super) name: String,
    pub(super) exclusive: bool,
}

impl StartState {
    pub fn new(id: usize, name: &str, exclusive: bool) -> Self {
        Self {
            id,
            name: name.to_string(),
            exclusive,
        }
    }
}

pub(super) struct LexParser<StorageT> {
    src: String,
    pub(super) rules: Vec<Rule<StorageT>>,
    duplicate_names: HashMap<Span, Vec<Span>>,
    pub(super) start_states: Vec<StartState>,
}

impl<StorageT: TryFrom<usize>> LexParser<StorageT> {
    pub(super) fn new(src: String) -> LexBuildResult<LexParser<StorageT>> {
        let mut p = LexParser {
            src,
            rules: Vec::new(),
            duplicate_names: HashMap::new(),
            start_states: vec![StartState::new(0, "INITIAL", false)],
        };
        p.parse()?;
        Ok(p)
    }

    fn mk_error(&self, kind: LexErrorKind, off: usize) -> LexBuildError {
        let span = Span::new(off, off);
        LexBuildError { kind, span }
    }

    fn parse(&mut self) -> LexBuildResult<usize> {
        let mut i = self.parse_declarations(0).map_err(|e| vec![e])?;
        i = self.parse_rules(i).map_err(|e| vec![e]).and_then(|i| {
            if let Some((orig_span, spans)) = self.duplicate_names.iter().next() {
                return Err(vec![LexBuildError {
                    span: *orig_span,
                    kind: LexErrorKind::DuplicateName(spans.clone()),
                }]);
            }
            Ok(i)
        })?;
        // We don't currently support the subroutines part of a specification. One day we might...
        match self.lookahead_is("%%", i) {
            Some(j) => {
                let k = self.parse_ws(j).map_err(|e| vec![e])?;
                if k == self.src.len() {
                    Ok(i)
                } else {
                    Err(vec![self.mk_error(LexErrorKind::RoutinesNotSupported, i)])
                }
            }
            None => {
                assert_eq!(i, self.src.len());
                Ok(i)
            }
        }
    }

    fn parse_declarations(&mut self, mut i: usize) -> LexInternalBuildResult<usize> {
        loop {
            i = self.parse_ws(i)?;
            if i == self.src.len() {
                break Err(self.mk_error(LexErrorKind::PrematureEnd, i - 1));
            }
            if let Some(j) = self.lookahead_is("%%", i) {
                break Ok(j);
            }
            i = self.parse_declaration(i)?;
        }
    }

    fn parse_declaration(&mut self, i: usize) -> LexInternalBuildResult<usize> {
        let line_len = self.src[i..]
            .find(|c| c == '\n')
            .unwrap_or(self.src.len() - i);
        let line = self.src[i..i + line_len].trim_end();
        let declaration_len = line.find(|c: char| c.is_whitespace()).unwrap_or(line_len);
        let declaration = self.src[i..i + declaration_len].trim_end();
        match declaration {
            "%x" => {
                for name in self.src[i + declaration_len..i + line_len]
                    .trim()
                    .split_whitespace()
                {
                    let id = self.start_states.len();
                    let start_state = StartState::new(id, name, true);
                    self.start_states.push(start_state);
                }
                Ok(i + line_len)
            }
            "%s" => {
                for name in self.src[i + declaration_len..i + line_len]
                    .trim()
                    .split_whitespace()
                {
                    let id = self.start_states.len();
                    let start_state = StartState::new(id, name, false);
                    self.start_states.push(start_state);
                }
                Ok(i + line_len)
            }
            _ => Err(self.mk_error(LexErrorKind::UnknownDeclaration, i)),
        }
    }

    fn parse_rules(&mut self, mut i: usize) -> LexInternalBuildResult<usize> {
        loop {
            i = self.parse_ws(i)?;
            if i == self.src.len() {
                break;
            }
            if self.lookahead_is("%%", i).is_some() {
                break;
            }
            i = self.parse_rule(i)?;
        }
        Ok(i)
    }

    fn parse_rule(&mut self, i: usize) -> LexInternalBuildResult<usize> {
        let line_len = self.src[i..]
            .find(|c| c == '\n')
            .unwrap_or(self.src.len() - i);
        let line = self.src[i..i + line_len].trim_end();
        let rspace = match line.rfind(' ') {
            Some(j) => j,
            None => return Err(self.mk_error(LexErrorKind::MissingSpace, i)),
        };

        let name;
        let target_state;
        let orig_name = if line[rspace + 1..].starts_with('<') {
            match line[rspace + 1..].find('>') {
                Some(l) => {
                    target_state = self
                        .get_start_state_by_name(&line[rspace + 2..rspace + 1 + l])
                        .map(|s| s.id);
                    if target_state.is_none() {
                        &line[rspace + 1..]
                    } else {
                        &line[rspace + 1 + l + 1..]
                    }
                }
                None => {
                    target_state = None;
                    &line[rspace + 1..]
                }
            }
        } else {
            target_state = None;
            &line[rspace + 1..]
        };
        let name_span;
        let dupe = if orig_name == ";" {
            name = None;
            let pos = i + rspace + 1;
            name_span = Span::new(pos, pos);
            false
        } else {
            debug_assert!(!orig_name.is_empty());
            if !((orig_name.starts_with('\'') && orig_name.ends_with('\''))
                || (orig_name.starts_with('\"') && orig_name.ends_with('"')))
            {
                return Err(self.mk_error(LexErrorKind::InvalidName, i + rspace + 1));
            }
            name = Some(orig_name[1..orig_name.len() - 1].to_string());
            name_span = Span::new(i + rspace + 2, i + rspace + orig_name.len());
            self.rules.iter().any(|r| {
                let dupe = r
                    .name
                    .as_ref()
                    .map_or(false, |n| n == name.as_ref().unwrap());
                if dupe {
                    self.duplicate_names
                        .entry(r.name_span)
                        .or_insert_with(Vec::new)
                        .push(name_span);
                }
                dupe
            })
        };

        if !dupe {
            let (start_states, re_str) = self.parse_start_states(line[..rspace].trim_end());
            //let re_str = line[..rspace].trim_end().to_string();
            let rules_len = self.rules.len();
            let tok_id = StorageT::try_from(rules_len)
                           .unwrap_or_else(|_| panic!("StorageT::try_from failed on {} (if StorageT is an unsigned integer type, this probably means that {} exceeds the type's maximum value)", rules_len, rules_len));

            let rule = Rule::new(
                Some(tok_id),
                name,
                name_span,
                re_str.to_string(),
                start_states,
                target_state,
            )
            .map_err(|_| self.mk_error(LexErrorKind::RegexError, i))?;
            self.rules.push(rule);
        }
        Ok(i + line_len)
    }

    // TODO Should return LexInternalBuildResult<?>
    fn parse_start_states<'a>(&self, re_str: &'a str) -> (Vec<usize>, &'a str) {
        if !re_str.starts_with('<') {
            (vec![], re_str)
        } else {
            match re_str.find('>') {
                None => (vec![], re_str),
                Some(j) => {
                    let start_states = re_str[1..j]
                        .split(',')
                        .map(|s| s.trim())
                        .map(|s| self.get_start_state_by_name(s))
                        // TODO: Handle the case where start state DOESN'T exist
                        .filter_map(|s| s.map(|ss| ss.id))
                        .collect();
                    (start_states, &re_str[j + 1..])
                }
            }
        }
    }

    fn get_start_state_by_name(&self, state: &str) -> Option<&StartState> {
        self.start_states.iter().find(|r| r.name == state)
    }

    fn parse_ws(&mut self, i: usize) -> LexInternalBuildResult<usize> {
        let mut j = i;
        for c in self.src[i..].chars() {
            match c {
                ' ' | '\t' | '\n' | '\r' => (),
                _ => break,
            }
            j += c.len_utf8();
        }
        Ok(j)
    }

    fn lookahead_is(&self, s: &'static str, i: usize) -> Option<usize> {
        if self.src[i..].starts_with(s) {
            Some(i + s.len())
        } else {
            None
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        lexer::{LRNonStreamingLexerDef, LexerDef},
        DefaultLexeme,
    };

    macro_rules! incorrect_errs {
        ($src:ident, $errs:expr) => {{
            for e in $errs {
                let mut line_cache = ::cfgrammar::newlinecache::NewlineCache::new();
                line_cache.feed(&$src);
                if let Some((line, column)) =
                    line_cache.byte_to_line_num_and_col_num(&$src, e.span.start())
                {
                    panic!(
                        "Incorrect error returned {} at line {line} column {column}",
                        e
                    )
                } else {
                    panic!("{}", e)
                }
            }
        }};
    }

    macro_rules! line_col {
        ($src:ident, $span: ident) => {{
            let mut line_cache = ::cfgrammar::newlinecache::NewlineCache::new();
            line_cache.feed(&$src);
            line_cache
                .byte_to_line_num_and_col_num(&$src, $span.start())
                .unwrap()
        }};
    }

    #[test]
    fn test_nooptions() {
        let src = "
%option nounput
        "
        .to_string();
        assert!(LRNonStreamingLexerDef::<DefaultLexeme<u8>, u8>::from_str(&src).is_err());
    }

    #[test]
    fn test_minimum() {
        let src = "%%".to_string();
        assert!(LRNonStreamingLexerDef::<DefaultLexeme<u8>, u8>::from_str(&src).is_ok());
    }

    #[test]
    fn test_rules() {
        let src = "%%
[0-9]+ 'int'
[a-zA-Z]+ 'id'
\\+ '+'
"
        .to_string();
        let ast = LRNonStreamingLexerDef::<DefaultLexeme<u8>, u8>::from_str(&src).unwrap();
        let intrule = ast.get_rule_by_name("int").unwrap();
        assert_eq!("int", intrule.name.as_ref().unwrap());
        assert_eq!("[0-9]+", intrule.re_str);
        let idrule = ast.get_rule_by_name("id").unwrap();
        assert_eq!("id", idrule.name.as_ref().unwrap());
        assert_eq!("[a-zA-Z]+", idrule.re_str);
        let plusrule = ast.get_rule_by_name("+").unwrap();
        assert_eq!("+", plusrule.name.as_ref().unwrap());
        assert_eq!("\\+", plusrule.re_str);
    }

    #[test]
    fn test_no_name() {
        let src = "%%
[0-9]+ ;
"
        .to_string();
        let ast = LRNonStreamingLexerDef::<DefaultLexeme<u8>, u8>::from_str(&src).unwrap();
        let intrule = ast.get_rule(0).unwrap();
        assert!(intrule.name.is_none());
        assert_eq!("[0-9]+", intrule.re_str);
    }

    #[test]
    fn test_broken_rule() {
        let src = "%%
[0-9]
'int'"
            .to_string();
        assert!(LRNonStreamingLexerDef::<DefaultLexeme<u8>, u8>::from_str(&src).is_err());
        match LRNonStreamingLexerDef::<DefaultLexeme<u8>, u8>::from_str(&src)
            .as_ref()
            .map_err(Vec::as_slice)
        {
            Ok(_) => panic!("Broken rule parsed"),
            Err(
                [LexBuildError {
                    kind: LexErrorKind::MissingSpace,
                    span,
                }],
            ) if line_col!(src, span) == (2, 1) => (),
            Err(e) => incorrect_errs!(src, e),
        }
    }

    #[test]
    fn test_broken_rule2() {
        let src = "%%
[0-9] "
            .to_string();
        assert!(LRNonStreamingLexerDef::<DefaultLexeme<u8>, u8>::from_str(&src).is_err());
        match LRNonStreamingLexerDef::<DefaultLexeme<u8>, u8>::from_str(&src)
            .as_ref()
            .map_err(Vec::as_slice)
        {
            Ok(_) => panic!("Broken rule parsed"),
            Err(
                [LexBuildError {
                    kind: LexErrorKind::MissingSpace,
                    span,
                }],
            ) if line_col!(src, span) == (2, 1) => (),
            Err(e) => incorrect_errs!(src, e),
        }
    }

    #[test]
    fn test_broken_rule3() {
        let src = "%%
[0-9] int"
            .to_string();
        assert!(LRNonStreamingLexerDef::<DefaultLexeme<u8>, u8>::from_str(&src).is_err());
        match LRNonStreamingLexerDef::<DefaultLexeme<u8>, u8>::from_str(&src)
            .as_ref()
            .map_err(Vec::as_slice)
        {
            Ok(_) => panic!("Broken rule parsed"),
            Err(
                [LexBuildError {
                    kind: LexErrorKind::InvalidName,
                    span,
                }],
            ) if line_col!(src, span) == (2, 7) => (),
            Err(e) => incorrect_errs!(src, e),
        }
    }

    #[test]
    fn test_broken_rule4() {
        let src = "%%
[0-9] 'int"
            .to_string();
        assert!(LRNonStreamingLexerDef::<DefaultLexeme<u8>, u8>::from_str(&src).is_err());
        match LRNonStreamingLexerDef::<DefaultLexeme<u8>, u8>::from_str(&src)
            .as_ref()
            .map_err(Vec::as_slice)
        {
            Ok(_) => panic!("Broken rule parsed"),
            Err(
                [LexBuildError {
                    kind: LexErrorKind::InvalidName,
                    span,
                }],
            ) if line_col!(src, span) == (2, 7) => (),
            Err(e) => incorrect_errs!(src, e),
        }
    }

    #[test]
    fn test_duplicate_rule() {
        let src = "%%
[0-9] 'int'
[0-9] 'int'
[0-9] 'int'"
            .to_string();
        match LRNonStreamingLexerDef::<DefaultLexeme<u8>, u8>::from_str(&src)
            .as_ref()
            .map_err(Vec::as_slice)
        {
            Ok(_) => panic!("Duplicate rule parsed"),
            Err(
                [LexBuildError {
                    kind: LexErrorKind::DuplicateName(spans),
                    span,
                }],
            ) if line_col!(src, span) == (2, 8) => {
                assert_eq!(spans, &[Span::new(22, 25), Span::new(34, 37)])
            }

            Err(e) => incorrect_errs!(src, e),
        }
    }

    #[test]
    #[should_panic]
    fn exceed_tok_id_capacity() {
        let mut src = "%%
"
        .to_string();
        for i in 0..257 {
            src.push_str(&format!("x 'x{}'\n", i));
        }
        LRNonStreamingLexerDef::<DefaultLexeme<u8>, u8>::from_str(&src).ok();
    }
}
