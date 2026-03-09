module HyLex.Program
open HyLex.Core
type CalcToken =
    | Number      // 数字
    | Plus        // +
    | Minus       // -
    | Multiply    // *
    | Divide      // /
    | LeftParen   // (
    | RightParen  // )
    | Whitespace  // 空白符

// 1. 使用构造器定义规则
let ruleBuilder = LexicalRuleBuilder<CalcToken>()

// 数字：一个或多个数字
ruleBuilder.Add Number (OneOrMore (CharRange('0', '9')))

// 运算符
ruleBuilder.Add Plus (CharSequence "+")
ruleBuilder.Add Minus (CharSequence "-")
ruleBuilder.Add Multiply (CharSequence "*")
ruleBuilder.Add Divide (CharSequence "/")
ruleBuilder.Add LeftParen (CharSequence "(")
ruleBuilder.Add RightParen (CharSequence ")")

// 空白符
ruleBuilder.Add Whitespace (OneOrMore (OneOf [CharSequence " "; CharSequence "\t"]))

// 2. 编译规则为混合自动机
let rules = ruleBuilder.Build()
let lexer = FromRuleList rules

// 准备输入
let input = "123 + 45 * (67 - 89)"
let initialPos = { AbsoluteOffset = 0; LineNumber = 0; ColumnNumber = 0 }
let inputState = { Source = input; Position = initialPos }

// 执行 Tokenize
let tokens = TokenizerAll lexer inputState

// 打印结果
tokens
|> Seq.iter (fun token ->
    match token.Kind with
    | Some kind -> printfn $"[{kind}] -> %s{token.RawText.ToString()}"
    | None -> printfn $"[Error] -> %s{token.RawText.ToString()}"
)