module HyLex.Tests

open Xunit
open HyLex.Test

// Helper function: Simplifies the token list into (TokenKind, string) tuples for easier assertion.
// 辅助函数：将 Token 列表简化为 (TokenKind, 字符串内容) 的元组列表，方便断言。
let tokenize (code: string) =
    FromString code
    |> Seq.choose (fun t -> 
        match t.Kind with
        | Some k -> Some (k, t.RawText.ToString()) // Keep valid tokens | 只提取合法的 Token
        | None -> None                             // Ignore Error tokens | 忽略错误 Token
    )
    |> Seq.toList

// Filters out whitespace and newline tokens, focusing only on meaningful syntax elements.
// 过滤掉空白和换行，只关注有实质意义的 Token（大多数语法分析器都会执行此操作）。
let tokenizeNoTrivia (code: string) =
    tokenize code
    |> List.filter (fun (k, _) -> k <> TokenKind.Whitespace && k <> TokenKind.EndOfLine)

module NormalCases =

    [<Fact>]
    let ``Test 1: Basic Keywords and Identifiers`` () =
        let code = "let myVar = true"
        let expected = [
            (TokenKind.Keyword Keyword.Let, "let")
            (TokenKind.Identifier, "myVar")
            (TokenKind.Symbol Symbol.Assign, "=")
            (TokenKind.Keyword Keyword.True, "true")
        ]
        Assert.Equal<seq<TokenKind * string>>(expected, tokenizeNoTrivia code)

    [<Fact>]
    let ``Test 2: Numeric Literals (Int, Float, Hex, Bin, Oct, Suffixes)`` () =
        // Tests various numeric literal formats | 测试各种数字字面量格式
        let code = "123 0xFF 0b101 0o77 1.23 .5 1. 2e-3 123ul 1.5f"
        let tokens = tokenizeNoTrivia code
        
        Assert.All(tokens, fun (kind, _) -> Assert.Equal(TokenKind.NumberLiteral, kind))
        
        let texts = tokens |> List.map snd
        let expectedTexts = ["123"; "0xFF"; "0b101"; "0o77"; "1.23"; ".5"; "1."; "2e-3"; "123ul"; "1.5f"]
        Assert.Equal<seq<string>>(expectedTexts, texts)

    [<Fact>]
    let ``Test 3: Strings and Characters`` () =
        let code = "\"hello world\" @\"verbatim\" 'x'"
        let expected = [
            (TokenKind.StringLiteral, "\"hello world\"")
            (TokenKind.StringLiteral, "@\"verbatim\"")
            (TokenKind.CharLiteral, "'x'")
        ]
        Assert.Equal<seq<TokenKind * string>>(expected, tokenizeNoTrivia code)

    [<Fact>]
    let ``Test 4: Complex Real-world Snippet`` () =
        let code = """
        module Math
        let add(x: int, y: int) -> int = {
            return x + y;
        }
        """
        let tokens = tokenizeNoTrivia code
        let expected = [
            (TokenKind.Keyword Keyword.Module, "module")
            (TokenKind.Identifier, "Math")
            (TokenKind.Keyword Keyword.Let, "let")
            (TokenKind.Identifier, "add")
            (TokenKind.Symbol Symbol.LeftParen, "(")
            (TokenKind.Identifier, "x")
            (TokenKind.Symbol Symbol.Colon, ":")
            (TokenKind.Identifier, "int")
            (TokenKind.Symbol Symbol.Comma, ",")
            (TokenKind.Identifier, "y")
            (TokenKind.Symbol Symbol.Colon, ":")
            (TokenKind.Identifier, "int")
            (TokenKind.Symbol Symbol.RightParen, ")")
            (TokenKind.Symbol Symbol.Arrow, "->")
            (TokenKind.Identifier, "int")
            (TokenKind.Symbol Symbol.Assign, "=")
            (TokenKind.Symbol Symbol.LeftBrace, "{")
            (TokenKind.Keyword Keyword.Return, "return")
            (TokenKind.Identifier, "x")
            (TokenKind.Symbol Symbol.Plus, "+")
            (TokenKind.Identifier, "y")
            (TokenKind.Symbol Symbol.Semicolon, ";")
            (TokenKind.Symbol Symbol.RightBrace, "}")
        ]
        Assert.Equal<seq<TokenKind * string>>(expected, tokens)


module EdgeCases =

    [<Fact>]
    let ``Edge 1: Maximal Munch (Prefix Overlap Resolution)`` () =
        // Tests token overlap resolution (e.g., '==' vs '= =')
        // 测试等号与赋值、小于与小于等于的冲突，引擎应基于最长匹配原则解析
        let code = "== = <= < && & letting let"
        let expected = [
            (TokenKind.Symbol Symbol.Equal, "==")
            (TokenKind.Symbol Symbol.Assign, "=")
            (TokenKind.Symbol Symbol.LessEqual, "<=")
            (TokenKind.Symbol Symbol.LessThan, "<")
            (TokenKind.Symbol Symbol.LogicalAnd, "&&")
            (TokenKind.Symbol Symbol.BitwiseAnd, "&")
            (TokenKind.Identifier, "letting") // Must not be split into 'let' + 'ting' | 不能被错误切割
            (TokenKind.Keyword Keyword.Let, "let")
        ] 
        Assert.Equal<seq<TokenKind * string>>(expected, tokenizeNoTrivia code)

    [<Fact>]
    let ``Edge 2: Empty Input`` () =
        let code = ""
        let tokens = tokenize code
        // Based on the implementation, it may return empty or just EndOfLine
        // 根据引擎实现，可能返回空列表，或仅返回一个文件末尾标识
        Assert.True(List.isEmpty tokens || (tokens.Length = 1 && fst tokens.[0] = TokenKind.EndOfLine))

    [<Fact>]
    let ``Edge 3: Multi-line Comments and Cross-line Matching`` () =
        let code = "/* This is a\n multi-line\n comment */ let x"
        let tokens = tokenizeNoTrivia code
        let expected = [
            (TokenKind.Comment, "/* This is a\n multi-line\n comment */")
            (TokenKind.Keyword Keyword.Let, "let")
            (TokenKind.Identifier, "x")
        ]
        Assert.Equal<seq<TokenKind * string>>(expected, tokens)

    [<Fact>]
    let ``Edge 4: Single-line Comments End at Newline`` () =
        let code = "// this is a comment\nlet"
        let tokens = tokenizeNoTrivia code
        let expected = [
            (TokenKind.Comment, "// this is a comment") 
            (TokenKind.Keyword Keyword.Let, "let")
        ]
        Assert.Equal<seq<TokenKind * string>>(expected, tokens)

    [<Fact>]
    let ``Edge 5: Consecutive Symbols Without Spaces`` () =
        // Validates that distinct symbols are correctly split without spacing
        // 验证没有空格时的连续符号不会被误认为标识符
        let code = "[()]"
        let expected = [
            (TokenKind.Symbol Symbol.LeftBracket, "[")
            (TokenKind.Symbol Symbol.LeftParen, "(")
            (TokenKind.Symbol Symbol.RightParen, ")")
            (TokenKind.Symbol Symbol.RightBracket, "]")
        ]
        Assert.Equal<seq<TokenKind * string>>(expected, tokenizeNoTrivia code)

    [<Fact>]
    let ``Edge 6: Empty String`` () =
        let code = "\"\""
        let expected = [
            (TokenKind.StringLiteral, "\"\"")
        ]
        Assert.Equal<seq<TokenKind * string>>(expected, tokenizeNoTrivia code)


module ErrorAndComplexCases =

    // Helper function: Keeps Error tokens (None) and ignores trivia
    // 辅助函数：针对带有 Option 的 Token 结构，保留错误信息并过滤空白
    let tokenizeWithErrorNoTrivia (code: string) =
        FromString code
        |> Seq.choose (fun t -> 
            match t.Kind with
            | Some k when k = TokenKind.Whitespace || k = TokenKind.EndOfLine -> None
            | Some k -> Some (Some k, t.RawText.ToString())
            | None -> Some (None, t.RawText.ToString())
        )
        |> Seq.toList

    [<Fact>]
    let ``Error 1: Single Invalid Character Recovery`` () =
        // Simulates an invalid full-width character | 模拟用户输入了非法的全角符号
        let code = "let ＠ myVar = 1"
        let expected = [
            (Some (TokenKind.Keyword Keyword.Let), "let")
            (None, "＠") 
            (Some TokenKind.Identifier, "myVar")
            (Some (TokenKind.Symbol Symbol.Assign), "=")
            (Some TokenKind.NumberLiteral, "1")
        ]
        Assert.Equal<seq<TokenKind option * string>>(expected, tokenizeWithErrorNoTrivia code)

    [<Fact>]
    let ``Error 2: Continuous Invalid Characters Panic Mode`` () =
        // Invalid sequence halts at the valid modulo symbol '%'
        // 模拟连续的乱码，恐慌模式应该在遇到合法符号 '%' 时精确停止
        let code = "let ￥#@！% x"
        let expected = [
            (Some (TokenKind.Keyword Keyword.Let), "let")
            (None, "￥#@！") 
            (Some (TokenKind.Symbol Symbol.Modulo), "%") 
            (Some TokenKind.Identifier, "x")
        ]
        Assert.Equal<seq<TokenKind option * string>>(expected, tokenizeWithErrorNoTrivia code)

    [<Fact>]
    let ``Error 3: Invalid Characters at End of File`` () =
        // Checks if panic mode stops correctly at EOF | 测试探测循环能否在文件末尾正确终止
        let code = "return ???"
        let expected = [
            (Some (TokenKind.Keyword Keyword.Return), "return")
            (None, "???")
        ]
        Assert.Equal<seq<TokenKind option * string>>(expected, tokenizeWithErrorNoTrivia code)

    [<Fact>]
    let ``Error 4: Invalid Characters Inside Expressions`` () =
        // Recovers mid-expression | 表达式中间混入非法字符，测试能否正常恢复
        let code = "x + ￥% y"
        let expected = [
            (Some TokenKind.Identifier, "x")
            (Some (TokenKind.Symbol Symbol.Plus), "+")
            (None, "￥")
            (Some (TokenKind.Symbol Symbol.Modulo), "%") 
            (Some TokenKind.Identifier, "y")
        ]
        Assert.Equal<seq<TokenKind option * string>>(expected, tokenizeWithErrorNoTrivia code)

    [<Fact>]
    let ``Edge 7: Whitespace and Newlines Only`` () =
        // Should yield empty list after filtering | 全是空白符，过滤后应该为空
        let code = "   \n  \t  \r\n  \r "
        let tokens = tokenizeWithErrorNoTrivia code
        Assert.Empty(tokens)

    [<Fact>]
    let ``Edge 8: Extremely Long Identifier`` () =
        // Tests slicing logic with a massive identifier | 测试非常长的标识符，验证内存切片是否正常
        let longId = String.replicate 1000 "a"
        let code = $"let {longId} = 0"
        let expected = [
            (Some (TokenKind.Keyword Keyword.Let), "let")
            (Some TokenKind.Identifier, longId)
            (Some (TokenKind.Symbol Symbol.Assign), "=")
            (Some TokenKind.NumberLiteral, "0")
        ]
        Assert.Equal<seq<TokenKind option * string>>(expected, tokenizeWithErrorNoTrivia code)

    [<Fact>]
    let ``Edge 9: Numbers Starting With Zero (Not Octal/Hex)`` () =
        // Extraneous leading zeros | 测试多余的前导零
        let code = "000123 00"
        let expected = [
            (Some TokenKind.NumberLiteral, "000123")
            (Some TokenKind.NumberLiteral, "00")
        ]
        Assert.Equal<seq<TokenKind option * string>>(expected, tokenizeWithErrorNoTrivia code)

    [<Fact>]
    let ``Normal 5: Recursive Function (Fibonacci)`` () =
        // Parses a classic Fibonacci function | 经典的斐波那契数列函数解析
        let code = """
        let fib(n: int) -> int = {
            if (n <= 1) {
                return n;
            } else {
                return fib(n - 1) + fib(n - 2);
            }
        }
        """
        let tokens = tokenizeNoTrivia code
        
        let letCount = tokens |> List.filter (fun (k, _) -> k = TokenKind.Keyword Keyword.Let) |> List.length
        let returnCount = tokens |> List.filter (fun (k, _) -> k = TokenKind.Keyword Keyword.Return) |> List.length
        let idCount = tokens |> List.filter (fun (k, v) -> k = TokenKind.Identifier && v = "fib") |> List.length
        
        Assert.Equal(1, letCount)
        Assert.Equal(2, returnCount)
        Assert.Equal(3, idCount)

    [<Fact>]
    let ``Normal 6: JSON-like Data Structure`` () =
        // Simulates nested data structure parsing | 模拟嵌套的数据结构解析
        let code = """
        let config = {
            "host": "127.0.0.1",
            "port": 8080,
            "debug": true
        };
        """
        let expected = [
            (TokenKind.Keyword Keyword.Let, "let")
            (TokenKind.Identifier, "config")
            (TokenKind.Symbol Symbol.Assign, "=")
            (TokenKind.Symbol Symbol.LeftBrace, "{")
            (TokenKind.StringLiteral, "\"host\"")
            (TokenKind.Symbol Symbol.Colon, ":")
            (TokenKind.StringLiteral, "\"127.0.0.1\"")
            (TokenKind.Symbol Symbol.Comma, ",")
            (TokenKind.StringLiteral, "\"port\"")
            (TokenKind.Symbol Symbol.Colon, ":")
            (TokenKind.NumberLiteral, "8080")
            (TokenKind.Symbol Symbol.Comma, ",")
            (TokenKind.StringLiteral, "\"debug\"")
            (TokenKind.Symbol Symbol.Colon, ":")
            (TokenKind.Keyword Keyword.True, "true")
            (TokenKind.Symbol Symbol.RightBrace, "}")
            (TokenKind.Symbol Symbol.Semicolon, ";")
        ]
        Assert.Equal<seq<TokenKind * string>>(expected, tokenizeNoTrivia code)

    [<Fact>]
    let ``Normal 7: Complex Mathematical Expression`` () =
        // Complex mathematical precedence check | 复杂的算术符号组合测试
        let code = "a * (b + c) / d - 1e5 == 0b10"
        let tokens = tokenizeNoTrivia code |> List.map fst
        
        let expectedKinds = [
            TokenKind.Identifier
            TokenKind.Symbol Symbol.Multiply 
            TokenKind.Symbol Symbol.LeftParen
            TokenKind.Identifier
            TokenKind.Symbol Symbol.Plus
            TokenKind.Identifier
            TokenKind.Symbol Symbol.RightParen
            TokenKind.Symbol Symbol.Slash 
            TokenKind.Identifier
            TokenKind.Symbol Symbol.Minus 
            TokenKind.NumberLiteral
            TokenKind.Symbol Symbol.Equal
            TokenKind.NumberLiteral
        ] 
        Assert.Equal<seq<TokenKind>>(expectedKinds, tokens)

    [<Fact>]
    let ``Normal 8: Continuous Punctuation (Maximal Munch Check)`` () =
        // Ensures adjacent punctuation doesn't collapse incorrectly | 验证连续的符号不会被错误地贪婪组合
        let code = "{[()]};;;,,,->"
        let tokens = tokenizeNoTrivia code |> List.map snd
        
        let expectedTexts = [
            "{"; "["; "("; ")"; "]"; "}"; ";"; ";"; ";"; ","; ","; ","; "->"
        ]
        Assert.Equal<seq<string>>(expectedTexts, tokens)