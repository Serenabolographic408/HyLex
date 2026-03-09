module HyLex.Benchmarks
open System.Text
open System.Text.RegularExpressions
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

// 假设这些引用指向你的类库

// MemoryDiagnoser 特性极其关键！它会统计 GC 压力和 Allocated 内存量
[<MemoryDiagnoser>] 
type LexerBenchmark() =
    
    let mutable microCode = ""
    let mutable macroCode = ""

    // 预编译一个简单的正则表达式用于作为低效对比组
    // 真实情况下的正则 lexer 会更复杂且更慢
    let regexLexer = new Regex(@"let|return|[a-zA-Z_]\w*|\d+|[=+\-{};]", RegexOptions.Compiled)

    [<GlobalSetup>]
    member this.Setup() =
        // 1. 微型数据集
        microCode <- "let config = { host: \"127.0.0.1\", port: 8080 };"
        
        // 2. 宏型数据集 (生成约 10,000 行复杂代码)
        let sb = StringBuilder()
        for i in 1 .. 2000 do
            sb.AppendLine("let fib_test(n: int) -> int = {") |> ignore
            sb.AppendLine("    if (n <= 1) { return n; }") |> ignore
            sb.AppendLine("    else { return fib_test(n - 1) + fib_test(n - 2); }") |> ignore
            sb.AppendLine("}") |> ignore
            // 穿插多行注释以测试跳过效率
            sb.AppendLine("/* This is a generated comment block") |> ignore
            sb.AppendLine("   designed to test lexer skipping */") |> ignore
        macroCode <- sb.ToString()

    // ---------------------------------------------------------
    // 微型数据集测试
    // ---------------------------------------------------------
    [<Benchmark>]
    member this.HyLex_Micro() =
        HyLex.Test.FromString microCode

    [<Benchmark(Baseline = true)>] // 将正则设为基准线，报表中会显示百分比对比
    member this.Regex_Micro() =
        let matches = regexLexer.Matches(microCode)
        matches.Count

    // ---------------------------------------------------------
    // 宏型数据集测试 (压力测试)
    // ---------------------------------------------------------
    [<Benchmark>]
    member this.HyLex_Macro() =
        HyLex.Test.FromString macroCode

    [<Benchmark>]
    member this.Regex_Macro() =
        let matches = regexLexer.Matches(macroCode)
        matches.Count

module Program =
    [<EntryPoint>]
    let main argv =
        // 启动 Benchmark 引擎
        let summary = BenchmarkRunner.Run<LexerBenchmark>()
        0