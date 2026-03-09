# HyLex: 高性能混合自动机词法分析器生成库

一个使用 F# 编写的轻量级词法分析器生成库。它采用**混合自动机架构**，结合了 DFA（确定有限自动机）的查询速度与 NFA（非确定有限自动机）的表达灵活性，并针对 .NET 进行了深度的零堆内存分配优化。

---

## 目录

- [特性](#特性)
- [环境要求](#环境要求)
- [快速开始](#快速开始)
- [核心概念](#核心概念)
- [技术实现亮点](#技术实现亮点)
- [示例词法器](#示例词法器)
- [许可证](#许可证)

---

## 特性

- 🚄 **混合自动机**：将词法规则编译为“DFA 字典查找 + NFA 动态谓词”的混合结构，兼顾速度与灵活性。
- 🗑️ **零堆内存分配**：核心路径使用 `ReadOnlyMemory<char>`、预分配数组和栈式 DFS，最大限度减少 GC 压力。
- 🛠️ **声明式规则 DSL**：支持字符序列、范围、可选、重复（Kleene 星号/加号）、选择等常见正则构造。
- 🛡️ **健壮性**：内置最长匹配原则、规则优先级冲突解决、以及“恐慌模式”错误恢复机制。

---

## 环境要求

- **开发环境**：.NET 10 SDK 及以上（低版本未测试，建议使用 .NET 10）。
- **运行时**：兼容 .NET 10 及以上运行时。

---

## 快速开始

### 1. 引入代码

由于暂未发布 NuGet 包，请直接将 `HyLex.Core/HybridAutomation.fs` 文件复制到你的 F# 项目中。

### 2. 定义你的 Token 类型

```fsharp
// 定义一个简单的计算器 Token 类型
type CalcToken =
    | Number      // 数字
    | Plus        // +
    | Minus       // -
    | Multiply    // *
    | Divide      // /
    | LeftParen   // (
    | RightParen  // )
    | Whitespace  // 空白符
```

### 3. 构建词法规则并生成词法器

```fsharp
open HyLex.Core

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
```

### 4. 进行词法分析

```fsharp
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
```

---

## 核心概念

### `LexicalRuleInfo`：词法规则的抽象语法树

你通过以下组合子构建规则：

- **原子规则**：`SatisfyChar` (谓词)、`CharSequence` (字符串)、`CharRange` (字符范围)。
- **组合规则**：`RuleSequence` (顺序)、`OneOf` (选择)、`Optional` (可选)、`ZeroOrMore` (0次或多次)、`OneOrMore` (1次或多次)。

### `HybridState<'T>`：混合自动机状态

- **`DEdges`**：`Dictionary<char, HybridState<'T>>`，用于确定的单字符跳转（O(1) 查找）。
- **`NEdges`**：`((char -> bool) * HybridState<'T>) list`，用于需要动态判断的谓词跳转（O(N) 求值）。

---

## 技术实现亮点

### 1. 经典算法 pipeline

1. **Thompson 构造法**：将 `LexicalRuleInfo` 递归转换为 NFA。
2. **子集构造法**：通过 `Epsilon Closure` 计算，将 NFA 状态集合并为 `HybridState`。
3. **最长匹配 (Maximal Munch)**：使用栈式 DFS 探索所有路径，记录最后一个接受状态。

### 2. 深度性能优化

- **零堆分配文本切片**：全程使用 `ReadOnlyMemory<char>` 和 `Span<char>`，绝不进行 `Substring` 复制。
- **预分配与复用**：闭包计算使用 `sharedQueue` 和 `visited` 数组，避免在循环中 `new` 新对象。
- **引用相等性**：`NfaState` 标记 `[<ReferenceEquality>]`，防止 F# 对自动机状态进行深度结构比较。

---

## 示例词法器

仓库 `HyLex.Test` 项目中包含了一个覆盖关键字、标识符、数字、字符串、注释的完整示例语言词法器。你可以参考 `Tokenizer.fs` 来构建复杂的词法规则。

---

## 许可证

本项目采用 **MIT 许可证**。你可以自由使用、修改和分发代码。
