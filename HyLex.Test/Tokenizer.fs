module HyLex.Test

open HyLex.Core
open System.Globalization

// Represents language keywords.
// 表示语言的关键字。
type Keyword =
    // Variables and bindings | 变量与绑定
    | Let
    | Mutable
    // Functions and control flow | 函数与控制流
    | If
    | Then
    | Else
    | While
    | Do
    | Return
    // Types and modules | 类型与模块
    | Module
    | Import
    | Type
    | Struct
    // Logic and booleans | 逻辑与布尔
    | And
    | Or
    | Not
    | True
    | False
    // Modifiers | 修饰符
    | Public

// Represents punctuation and operators.
// 表示标点符号与运算符。
type Symbol =
    // Parentheses and delimiters | 括号与分隔符
    | LeftParen      // (
    | RightParen     // )
    | LeftBrace      // {
    | RightBrace     // }
    | LeftBracket    // [
    | RightBracket   // ]
    | Comma          // ,
    | Colon          // :
    | Semicolon      // ;
    | Dot            // .
    // Basic arithmetic operators | 基础算术运算符
    | Plus           // +
    | Minus          // -
    | Multiply       // *
    | Slash          // /
    | Modulo         // %
    // Comparison and logic operators | 比较与逻辑运算符
    | Equal          // ==
    | NotEqual       // !=
    | GreaterThan    // >
    | LessThan       // <
    | GreaterEqual   // >=
    | LessEqual      // <=
    | BitwiseAnd     // &
    | LogicalAnd     // &&
    | LogicalOr      // ||
    | BitwiseOr      // | 
    | Exclamation    // !
    // Assignment and arrows | 赋值与箭头
    | Assign         // =
    | Arrow          // ->
    // Others | 其他
    | Underscore     // _

// Represents the universal category of a token.
// 表示词法单元的全局分类。
type TokenKind = 
    | Keyword of Keyword
    | Symbol of Symbol
    | NumberLiteral
    | Whitespace
    | EndOfLine
    | Identifier
    | CharLiteral
    | StringLiteral
    | Comment
    | Unknown

// Keyword string-to-enum mapping.
// 关键字字符串到枚举的映射。
let Keywords =
    [
        ("module", Keyword.Module)
        ("import", Keyword.Import)
        ("let", Keyword.Let)
        ("mutable", Keyword.Mutable)
        ("type", Keyword.Type)
        ("struct", Keyword.Struct)
        ("do", Keyword.Do)
        ("and", Keyword.And)
        ("or", Keyword.Or)
        ("not", Keyword.Not)
        ("if", Keyword.If)
        ("then", Keyword.Then)
        ("else", Keyword.Else)
        ("while", Keyword.While)
        ("return", Keyword.Return)
        ("public", Keyword.Public)
        ("true", Keyword.True)
        ("false", Keyword.False)
    ]

// Symbol string-to-enum mapping.
// 符号字符串到枚举的映射。
let Symbols =
    [
        ("(", Symbol.LeftParen)
        (")", Symbol.RightParen)
        ("{", Symbol.LeftBrace)
        ("}", Symbol.RightBrace)
        ("[", Symbol.LeftBracket)
        ("]", Symbol.RightBracket)
        (",", Symbol.Comma)
        ("!", Symbol.Exclamation)
        ("->", Symbol.Arrow)
        (":", Symbol.Colon)
        (";", Symbol.Semicolon)
        ("_", Symbol.Underscore)
        ("+", Symbol.Plus)
        ("-", Symbol.Minus)
        ("*", Symbol.Multiply)
        ("/", Symbol.Slash)
        ("%", Symbol.Modulo)
        ("=", Symbol.Assign)
        ("==", Symbol.Equal)
        ("!=", Symbol.NotEqual)
        (">", Symbol.GreaterThan)
        ("<", Symbol.LessThan)
        (">=", Symbol.GreaterEqual)
        ("<=", Symbol.LessEqual)
        ("&&", Symbol.LogicalAnd)
        ("&", Symbol.BitwiseAnd)
        ("||", Symbol.LogicalOr)
        ("|", Symbol.BitwiseOr)
        (".", Symbol.Dot)
    ]

// Private lexer instance containing all compiled automata rules.
// 包含所有已编译自动机规则的私有词法分析器实例。
let private Instance =
    let ruleBuilder = LexicalRuleBuilder()

    // Identifier rules - Lowest priority.
    // 标识符规则 - 最低优先级。
    let IdentifierRules =
        let CanBeIdentifierChar (c: char) : bool =
            if c = '_' then true
            else 
            // Gets the Unicode category of the character. | 获取字符的 Unicode 类别。
            let category = CharUnicodeInfo.GetUnicodeCategory(c)
            match category with
            | UnicodeCategory.UppercaseLetter      // Lu
            | UnicodeCategory.LowercaseLetter      // Ll
            | UnicodeCategory.TitlecaseLetter      // Lt
            | UnicodeCategory.ModifierLetter       // Lm
            | UnicodeCategory.OtherLetter          // Lo (Includes CJK characters | 包含中文等)
            | UnicodeCategory.LetterNumber         // Nl
            | UnicodeCategory.ConnectorPunctuation // Pc (Mainly underscores | 主要是下划线)
            | UnicodeCategory.NonSpacingMark       // Mn (Combining marks | 组合标记)
            | UnicodeCategory.SpacingCombiningMark // Mc (Combining marks | 组合标记)
                -> true
            | _ -> false

        let CanBeIdentifierStartChar (c: char) : bool =
            not (System.Char.IsDigit c) && CanBeIdentifierChar c
    
        RuleSequence [ 
            SatisfyChar ("Identifier First", CanBeIdentifierStartChar)
            ZeroOrMore (SatisfyChar ("Identifier Rest", CanBeIdentifierChar))  
        ]
    
    ruleBuilder.Add Identifier IdentifierRules
    
    // Keyword rules.
    // 关键字规则。
    Keywords
    |> List.iter (fun (lexeme, keyword) ->
        ruleBuilder.Add (Keyword keyword) (CharSequence lexeme)
    )
    
    // Number literal rules.
    // 数字字面量规则。
    let NumberLiteralRules =
        let digits = OneOrMore (CharRange('0', '9'))
        let hexDigits = OneOrMore (OneOf [ CharRange('0', '9'); CharRange('a', 'f'); CharRange('A', 'F') ])
        let binaryDigits = OneOrMore (OneOf [ CharSequence "0"; CharSequence "1" ])
        let octalDigits = OneOrMore (CharRange('0', '7'))
    
        let decimalLiteral = digits
        let hexLiteral = RuleSequence [ OneOf[CharSequence "0X"; CharSequence "0x"]; hexDigits ]
        let binaryLiteral = RuleSequence [ OneOf[CharSequence "0B"; CharSequence "0b"]; binaryDigits ]
        let octalLiteral = RuleSequence [ OneOf[CharSequence "0O"; CharSequence "0o"]; octalDigits ]
    
        let integerLiteral = OneOf [ hexLiteral; binaryLiteral; octalLiteral; decimalLiteral ]
    
        let exponentPart = RuleSequence [
            OneOf [ CharSequence "e"; CharSequence "E" ]
            Optional (OneOf [ CharSequence "+"; CharSequence "-" ])
            digits
        ]
    
        let floatLiteral = OneOf [
            RuleSequence [ digits; CharSequence "."; digits; Optional exponentPart ]
            RuleSequence [ digits; CharSequence "."; Optional exponentPart ]
            RuleSequence [ CharSequence "."; digits; Optional exponentPart ]
            RuleSequence [ digits; exponentPart ]
        ]
        
        let typeSuffix = Optional (OneOf [
            CharSequence "b"  // byte
            CharSequence "sb" // sbyte
            CharSequence "s"  // short
            CharSequence "us" // ushort
            CharSequence "l"  // long
            CharSequence "ul" // ulong
            CharSequence "n"  // native int
            CharSequence "un" // unsigned native int
            CharSequence "m"  // decimal
            CharSequence "f"  // float
            CharSequence "d"  // double
        ])
    
        OneOf [
            RuleSequence [ floatLiteral; typeSuffix ]
            RuleSequence [ integerLiteral; typeSuffix ]
        ]
    ruleBuilder.Add NumberLiteral NumberLiteralRules
    
    // String literal rules.
    // 字符串字面量规则。
    let StringLiteralRules =
        let notQuote c = c <> '\"'
        let regularString =
            RuleSequence[
                CharSequence "\""
                ZeroOrMore (SatisfyChar ("Not Quote", notQuote)) 
                CharSequence "\""
            ]
    
        let verbatimString =
            RuleSequence[
                CharSequence "@\"" 
                ZeroOrMore (SatisfyChar ("Not Quote", notQuote)) 
                CharSequence "\""
            ]
    
        OneOf [
            regularString
            verbatimString
        ]
    ruleBuilder.Add StringLiteral StringLiteralRules
    
    // Character literal rules.
    // 字符字面量规则。
    let CharLiteralRules = 
        RuleSequence [
            CharSequence "'"
            OneOf [
                RuleSequence[
                    SatisfyChar ("Control Char \\", fun c -> c = '\\')
                    SatisfyChar ("Any Char", fun _ -> true)
                ]
                SatisfyChar ("Not Single Quote", fun c -> c <> '\'')
            ]
            CharSequence "'"
        ]
    ruleBuilder.Add CharLiteral CharLiteralRules

    // Symbol rules.
    // 符号规则。
    Symbols
    |> List.iter (fun (lexeme, symbol) ->
        ruleBuilder.Add (Symbol symbol) (CharSequence lexeme)
    )
    
    // Comment rules - Evaluated after strings/chars to prevent matching inside them.
    // 注释规则 - 次低优先级（在字符串之后，防止匹配字符串内部的斜杠）。
    let CommentRules = OneOf [
        // Multi-line comments | 多行注释
        RuleSequence [
            CharSequence "/*"  // Start | 开始符号
            ZeroOrMore (OneOf [        
                // Matches non-star characters | 匹配非星号字符
                let notStar c = c <> '*'
                SatisfyChar ("Not Star", notStar)
                // Matches a star followed by a non-slash to avoid premature termination
                // 匹配星号后紧跟非斜杠字符（防止误判结束符）
                RuleSequence [ 
                    CharSequence "*"
                    let notSlash c = c <> '/'
                    SatisfyChar ("Not Slash", notSlash)
                ]
            ])
            CharSequence "*/"  // End | 结束符号
        ]
        // Single-line comments | 单行注释
        let notNewline c = c <> '\n'
        RuleSequence [
            CharSequence "//" 
            ZeroOrMore (SatisfyChar ("Not Newline", notNewline))
        ]
    ]
    ruleBuilder.Add Comment CommentRules
    
    // Whitespace rules - Evaluated just before EndOfLine.
    // 空白规则 - 次最高优先级。
    let WhitespaceRules =
        OneOrMore (OneOf [
            CharSequence " "
            CharSequence "\t"
        ])
    ruleBuilder.Add Whitespace WhitespaceRules
    
    // EndOfLine rules - Highest priority.
    // 行结束规则 - 最高优先级。
    let EndOfLineRules = CharSequence "\n"
    
    ruleBuilder.Add EndOfLine EndOfLineRules

    ruleBuilder.Build()
    |> FromRuleList
    
open System

// Public entry point to tokenize a given string using the configured hybrid automaton.
// 公共入口点，使用配置好的混合自动机解析给定的字符串。
let FromString (str: string) = 
    let initialPosition = { AbsoluteOffset = 0; LineNumber = 0; ColumnNumber = 0 }
    let inputState = { Source = str; Position = initialPosition }
    TokenizerAll Instance inputState