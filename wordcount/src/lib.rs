//! wordcount はシンプルな文字、単語、行の出現頻度の計測機能を提供します。
//! 詳しくは[`count`](fn.count.html)関数のドキュメントを見てください。
#![warn(missing_docs)]

use regex::Regex;
use std::collections::HashMap;
use std::io::BufRead;

/// input から 1 行ずつ UTF-8 文字列を読み込み、頻度を数える
///
/// 頻度を数える対象はオプションによって制御される
/// * [`CountOption::Char`](enum.CountOption.html#variant.Char): Unicode の 1 文字ごと
/// * [`CountOption::Word`](enum.CountOption.html#variant.Word): 正規表現 \w+ にマッチする単語ごと
/// * [`CountOption::Line`](enum.CountOption.html#variant.Line): \n または \r\n で区切られた 1 行ごと
///
/// # Panics
///
///入力が UTF-8 でフォーマットされていない場合にパニックする
pub fn count(input: impl BufRead, option: CountOption) -> HashMap<String, usize> {
    let regex = Regex::new(r"\w+").unwrap();
    let mut counts = HashMap::new();

    for line in input.lines() {
        let line = line.unwrap();

        match option {
            CountOption::Char => {
                for c in line.chars() {
                    *counts.entry(c.to_string()).or_insert(0) += 1;
                }
            }
            CountOption::Word => {
                for m in regex.find_iter(&line) {
                    let word = m.as_str().to_string();
                    *counts.entry(word).or_insert(0) += 1;
                }
            }
            CountOption::Line => {
                *counts.entry(line.to_string()).or_insert(0) += 1;
            }
        }
    }

    counts
}

/// [`count`](fn.count.html)で使うオプション
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CountOption {
    /// 文字ごとに頻度を数える
    Char,
    /// 単語ごとに頻度を数える
    Word,
    /// 行ごとに頻度を数える
    Line,
}

/// オプションのデフォルトは[`Word`](enum.CountOption.html#variant.Word)
impl Default for CountOption {
    fn default() -> Self {
        CountOption::Word
    }
}

#[test]
fn word_count_works() {
    use std::io::Cursor;

    let mut exp = HashMap::new();
    exp.insert("aa".to_string(), 1);
    exp.insert("bb".to_string(), 2);
    exp.insert("cc".to_string(), 1);

    assert_eq!(count(Cursor::new("aa bb cc bb"), CountOption::Word), exp);
}

#[test]
#[should_panic]
fn word_count_do_not_unknown_words() {
    use std::io::Cursor;

    count(
        Cursor::new([b'a', 0xf0, 0x90, 0x80, 0xe3, 0x81, 0x82]),
        CountOption::Word,
    );
}
