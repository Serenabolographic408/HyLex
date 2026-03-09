module HyLex.Core
open System.Collections.Generic
open System
open System.Text

// Represents the current position in the source text.
// 表示源码文本中的当前位置。
[<Struct>]
type Position = { 
    // Absolute global offset in the source. | 在源码中的全局绝对偏移量。
    AbsoluteOffset: int 
    // 0-based line number. | 行号（从 0 开始）。
    LineNumber: int 
    // 0-based column number. | 列号（从 0 开始）。
    ColumnNumber: int 
}

// Represents the state of the input stream being tokenized.
// 表示正在进行词法分析的输入流状态。
[<Struct>]
type InputState = {
    // The complete source text.
    // 完整的源文本。
    Source: string
    // The current parsing position. | 当前解析位置。
    Position: Position
}
 
// Represents a lexed token.
// 表示一个词法单元 (Token)。
type Token<'T> = {
    // Token kind. 'None' indicates an unrecognized or invalid sequence (Error Token).
    // Token 类型。None 表示未识别或非法的字符序列（错误 Token）。
    Kind: 'T option 
    // The raw text slice from the source. | 来自源文本的原始字符串切片。
    RawText: ReadOnlyMemory<char>
}

// AST nodes for defining lexical rules.
// 用于定义词法规则的抽象语法树 (AST) 节点。
type LexicalRuleInfo =
    // Matches a character satisfying the predicate. The string provides a description for debugging/errors.
    // 匹配满足断言的单个字符。字符串参数提供用于调试或错误报告的描述。
    | SatisfyChar of string * (char -> bool)
    // Matches an exact sequence of characters. | 匹配精确的字符串序列。
    | CharSequence of string
    // Matches any character within the specified inclusive range. | 匹配指定范围内的任意字符（闭区间）。
    | CharRange of char * char
    // Matches a sequence of sub-rules sequentially. | 顺序匹配一系列子规则。
    | RuleSequence of LexicalRuleInfo list
    // Optionally matches the sub-rule (0 or 1 time). | 可选匹配子规则（0 次或 1 次）。
    | Optional of LexicalRuleInfo
    // Matches the sub-rule zero or more times (Kleene Star). | 匹配子规则零次或多次（Kleene 星号）。
    | ZeroOrMore of LexicalRuleInfo
    // Matches the sub-rule one or more times (Kleene Plus). | 匹配子规则一次或多次（Kleene 加号）。
    | OneOrMore of LexicalRuleInfo
    // Matches exactly one of the provided sub-rules (Alternation). | 匹配所提供子规则中的任意一个（分支选择）。
    | OneOf of LexicalRuleInfo list

// Binds a specific lexical rule to a token kind.
// 将特定的词法规则绑定到对应的 Token 类型。
type LexicalRule<'T> = {
    TokenKind: 'T
    Rule: LexicalRuleInfo
}

// A builder to construct and collect lexical rules.
// 用于构建和收集词法规则的构造器。
type LexicalRuleBuilder<'T> () =
    let mutable rules: LexicalRule<'T> list = []
    member _.Add token rule =
        rules <- { TokenKind = token; Rule = rule } :: rules
    member _.Build() = List.rev rules

// Internal representation of an NFA (Nondeterministic Finite Automaton) state.
// 非确定性有限自动机 (NFA) 状态的内部表示。
// Enforces reference equality to prevent expensive deep structural comparisons in F# HashSets.
// 强制使用引用相等性，防止 F# 在 HashSet 中进行极其耗时的深度结构比较。
[<ReferenceEquality>]
type private NfaState<'T> = {
    // Unique identifier for O(1) visited array lookup during closure computation.
    // 唯一标识符，用于闭包计算时进行 O(1) 复杂度的访问数组查找。
    Id: int
    // State description used for debugging and visualization.
    // 用于调试和可视化的状态描述。
    StateInfo: string 
    // Outgoing edges from this state. | 从该状态出发的转移边。
    mutable Edges: NfaEdge<'T>
    // The accepting token kind and its priority level, if this is an accept state.
    // 如果是接受状态，则存储对应的 Token 类型及其优先级。
    mutable AcceptTokenKind: Option<'T * int>
}
and private NfaEdge<'T> = 
    | Char of char * NfaState<'T>
    | Predicate of (char -> bool) * NfaState<'T>
    | EpsilonList of NfaState<'T> list

// Constructs an NFA from the lexical rules using Thompson's construction algorithm.
// 使用 Thompson 构造法将词法规则转换为 NFA。
type private NfaBuilder<'T>() = 
    let mutable nextId = 0
    
    let CreateState stateInfo edges tkOps =
        let id = nextId
        nextId <- nextId + 1
        {
            Id = id
            StateInfo = stateInfo
            Edges = edges
            AcceptTokenKind = tkOps
        }

    // Recursively generates NFA states for a given rule info.
    // 递归地为给定的规则信息生成 NFA 状态。
    let rec FromRuleInfo ruleInfo : NfaState<'T> * NfaState<'T> = 
        match ruleInfo with
        | SatisfyChar (info, func) -> 
            let accept = CreateState $"({info})" (EpsilonList []) None
            let start = CreateState $"Accept({info})" (Predicate (func, accept)) None
            (start, accept)

        | CharSequence str ->
            if str.Length = 0 then failwith "CharSequence Cannot Be Empty"
            let accept = CreateState $"Accept`{str}`" (EpsilonList []) None
            let mutable start = accept
            for i in (str.Length - 1) .. -1 .. 0 do
                start <- CreateState $"'{str[i]}'" (Char (str[i], start)) None
            start <- CreateState $"`{str}`" (EpsilonList [start]) None
            (start, accept)

        | CharRange (cl, cr) ->
            if cl > cr then failwith "Invalid CharRange"
            else if cl = cr then 
                // Optimization for a single character range. | 针对单字符范围的优化。
                let accept = CreateState $"Accept'{cl}'" (EpsilonList []) None
                let start = CreateState $"'{cl}'" (Char (cl, accept)) None
                (start, accept)
            else 
                let aux c = c >= cl && c <= cr
                let accept = CreateState $"Accept('{cl}'-'{cr}')" (EpsilonList []) None
                let start = CreateState $"('{cl}'-'{cr}')" (Predicate (aux, accept)) None
                (start, accept)

        | Optional subRuleInfo -> 
            let (subStart, subAccept) = FromRuleInfo subRuleInfo
            let start = CreateState $"Optional({subStart.StateInfo})" (EpsilonList [subStart; subAccept]) None
            (start, subAccept)

        | RuleSequence subRuleInfoList ->
            if subRuleInfoList.IsEmpty then 
                failwith "RuleSequence Cannot Be Empty"
            let mutable (start, accept) = FromRuleInfo subRuleInfoList.Head
            let sb = System.Text.StringBuilder()
            sb.Append start.StateInfo |> ignore
            
            subRuleInfoList.Tail |> List.iter (fun remainRuleInfo ->
                sb.Append ", " |> ignore
                let (nextStart, nextAccept) = FromRuleInfo remainRuleInfo
                // Connect the previous accept state to the start of the next state using Epsilon.
                // 使用 Epsilon 边将上一个接受状态连接到下一个状态的起点。
                match accept.Edges with
                | EpsilonList edges -> 
                    accept.Edges <- EpsilonList (nextStart::edges)
                | _ -> failwith "Accept State Must Have Epsilon Edges"
                accept <- nextAccept
                sb.Append nextStart.StateInfo |> ignore
            )
            let stateInfo = sb.ToString()
            start <- CreateState $"Sequence({stateInfo})" (EpsilonList [start]) None
            (start, accept)

        | ZeroOrMore subRuleInfo ->   
            let (subStart, subAccept) = FromRuleInfo subRuleInfo
            let start = CreateState $"ZeroOrMore({subStart.StateInfo})" (EpsilonList [subStart; subAccept]) None
            // Create a loop back to the start state. | 创建一个回到起点的回环。
            match subAccept.Edges with
            | EpsilonList edges -> 
                subAccept.Edges <- EpsilonList (subStart::edges)
            | _ -> failwith "Accept State Must Have Epsilon Edges"
            (start, subAccept)

        | OneOrMore subRuleInfo -> 
            let (subStart, subAccept) = FromRuleInfo subRuleInfo
            // Create a loop back to the start state. | 创建一个回到起点的回环。
            match subAccept.Edges with
            | EpsilonList edges -> 
                subAccept.Edges <- EpsilonList (subStart::edges)
            | _ -> failwith "Accept state must have Epsilon edges"
            (subStart, subAccept)

        | OneOf subRuleInfoList -> 
            let subNfas = subRuleInfoList |> List.map FromRuleInfo 
            let subStateInfos = subNfas |> List.map(fun (s, _) -> s.StateInfo)
            let stateInfo = System.String.Join(", ", subStateInfos)
            let accept = CreateState $"Accept OneOf({stateInfo})" (EpsilonList []) None
            
            let edge = EpsilonList (subNfas |> List.map (fun (subStart, subAccept) -> 
                // Connect all sub-rules' accept states to a unified accept state.
                // 将所有子规则的接受状态连接到一个统一的接受状态。
                match subAccept.Edges with
                | EpsilonList edges -> 
                    subAccept.Edges <- EpsilonList (accept::edges)
                | _ -> failwith "Accept State Must Have Epsilon Edges"
                subStart
            ))
            let start = CreateState $"OneOf({stateInfo})" edge None
            (start, accept)
    
    let rec FromRule rule level : NfaState<'T> * NfaState<'T> =
        let (start, accept) = FromRuleInfo rule.Rule
        accept.AcceptTokenKind <- Some (rule.TokenKind, level)
        (start, accept)

    // Builds the root NFA starting point by combining all rules via Epsilon transitions.
    // 通过 Epsilon 转移连接所有规则，构建根 NFA 起点。
    member _.FromRuleList ruleList = 
        let nfaList = ruleList |> List.mapi (fun i r -> FromRule r i)
        let start = CreateState "NFA Start" (EpsilonList (nfaList |> List.map (fun (s, _) -> s))) None 
        start
        
    // Exposes the total number of allocated states for array optimizations.
    // 暴露已分配的状态总数，供后续数组优化使用。
    member _.MaxId = nextId

// Reads the current character and advances the input state.
// 读取当前字符并推进输入状态。
let private CurrentChar (input: InputState): InputState * char option = 
    if input.Position.AbsoluteOffset >= input.Source.Length then 
        (input, None)
    else
        let offset = input.Position.AbsoluteOffset
        let span = input.Source.AsSpan()
        let ch = span[offset]

        // Unified Line-Ending Normalization: Dynamically normalizes line endings while advancing the cursor.
        // 统一换行符处理逻辑：在游标移动时进行动态换行符标准化。
        if ch = '\r' then
            if offset + 1 < span.Length && span[offset + 1] = '\n' then
                // Matches \r\n (Windows). Advance offset by 2, but feed a single \n to the automaton.
                // 遇到 \r\n (Windows)。偏移量 +2，行列号更新，但向自动机喂入单个 \n。
                let nextPos = { 
                    AbsoluteOffset = offset + 2
                    LineNumber = input.Position.LineNumber + 1
                    ColumnNumber = 0 
                }
                ({ input with Position = nextPos }, Some '\n')
            else
                // Matches \r (Classic Mac).
                // 遇到单个 \r (老式 Mac)。
                let nextPos = { 
                    AbsoluteOffset = offset + 1
                    LineNumber = input.Position.LineNumber + 1
                    ColumnNumber = 0 
                }
                ({ input with Position = nextPos }, Some '\n')
        else if ch = '\n' then
            // Matches \n (Linux/Unix).
            // 遇到单个 \n (Linux/Unix)。
            let nextPos = { 
                AbsoluteOffset = offset + 1
                LineNumber = input.Position.LineNumber + 1
                ColumnNumber = 0 
            }
            ({ input with Position = nextPos }, Some '\n')
        else
            // Standard character handling.
            // 常规字符处理。
            let nextPos = { 
                AbsoluteOffset = offset + 1
                LineNumber = input.Position.LineNumber
                ColumnNumber = input.Position.ColumnNumber + 1 
            }
            ({ input with Position = nextPos }, Some ch)

// Represents a state in the Hybrid Automaton (combines DFA tables and Predicate evaluations).
// 混合自动机的状态表示（结合了 DFA 状态表和动态断言求值）。
type HybridState<'T> = {
    // Deterministic edge transitions (O(1) lookup). | 确定的字符转移边（O(1) 查找）。
    mutable DEdges: Dictionary<char, HybridState<'T>>
    // Nondeterministic predicate edge transitions (O(N) evaluation). | 非确定的断言转移边（O(N) 求值）。
    mutable NEdges: ((char -> bool) * HybridState<'T>) list
    // Token kind and priority level for accepting states. | 接受状态对应的 Token 类型与优先级。
    mutable AcceptTokenKind: ('T * int) option
}

// Converts the NFA to a Hybrid State Machine using Subset Construction.
// 使用子集构造法 (Subset Construction) 将 NFA 转换为混合状态机。
let private FromNfa (nfaStart: NfaState<'T>) (maxNfaId: int) = 
    // Memoization dictionary to avoid redundant state creation. | 用于避免重复创建状态的记忆化字典。
    let stateDic = Dictionary(HashSet.CreateSetComparer())
    let mutable curHybirdStateId = -1
    let createState set tokenAndLevel = 
        curHybirdStateId <- curHybirdStateId + 1
        let res = {
            DEdges = Dictionary<char, HybridState<'T>>()
            NEdges = []
            AcceptTokenKind = tokenAndLevel
        }
        stateDic.[set] <- res
        res

    // Pre-allocated reusable structures for closure computation (Zero-Allocation Loop).
    // 预分配的可复用结构，用于闭包计算（消除循环内的堆内存分配）。
    let sharedQueue = Queue<NfaState<'T>>()
    let visited = Array.zeroCreate<bool> maxNfaId

    // Calculates the Epsilon Closure for a given set of NFA states.
    // 计算给定 NFA 状态集合的 Epsilon 闭包。
    let getClosure nfas = 
        let resultSets = HashSet()
        let mutable tk = None
        sharedQueue.Clear()
        
        nfas |> Seq.iter (fun nfa -> sharedQueue.Enqueue nfa)
        
        while sharedQueue.Count <> 0 do
            let state = sharedQueue.Dequeue()
            
            // O(1) array lookup replaces expensive HashSet.Contains operations.
            // O(1) 的数组索引查找取代了昂贵的 HashSet.Contains 操作。
            if not visited.[state.Id] then
                visited.[state.Id] <- true
                resultSets.Add state |> ignore
                
                match state.AcceptTokenKind with
                | Some (tokenKind, level) -> 
                    // Conflict Resolution: Prioritize the token with the highest rule level.
                    // 解决冲突：只保留优先级更高的 Token。
                    match tk with
                    | None -> tk <- Some (tokenKind, level)
                    | Some (_, existingLevel) ->
                        if level > existingLevel then
                            tk <- Some (tokenKind, level) 
                | None -> ()
                        
                match state.Edges with
                | EpsilonList edges -> 
                    edges |> List.iter (fun s -> 
                        if not visited.[s.Id] then sharedQueue.Enqueue s
                    )
                | _ -> ()
                
        // Reset the visited array to clean state for the next call.
        // 重置访问数组，为下一次调用保持干净状态。
        for state in resultSets do
            visited.[state.Id] <- false
            
        (tk, resultSets)

    let waitQueue = Queue()
    let (firstTokenAndLevelOps, firstSets) = getClosure [nfaStart]
    let head = createState firstSets firstTokenAndLevelOps
    waitQueue.Enqueue (head, firstSets)
    
    // Process the state transition queue until all reachable states are constructed.
    // 处理状态转移队列，直到构建出所有可达的混合状态。
    while waitQueue.Count <> 0 do
        let (prev, set) = waitQueue.Dequeue()
        
        // Use imperative collections instead of Seq abstractions to save allocations.
        // 使用命令式集合替代 Seq 抽象，以节省大量中间对象的分配。
        let charGroups = Dictionary<char, List<NfaState<'T>>>()
        let predGroups = List<(char -> bool) * NfaState<'T>>()
        
        for nfa in set do
            match nfa.Edges with
            | Char (ch, nextNfa) -> 
                match charGroups.TryGetValue(ch) with
                | true, list -> list.Add(nextNfa)
                | false, _ -> 
                    let list = List<NfaState<'T>>()
                    list.Add(nextNfa)
                    charGroups.[ch] <- list
            | Predicate (pred, nextNfa) -> 
                predGroups.Add((pred, nextNfa))
            | _ -> ()

        // 1. Process deterministic character transitions. | 1. 处理确定的单字符转移。
        for kvp in charGroups do
            let ch = kvp.Key
            let nfaSeq = kvp.Value
            let (nextToken, nextSet) = getClosure nfaSeq
            match stateDic.TryGetValue nextSet with
            | true, hNode ->
                prev.DEdges.[ch] <- hNode
            | false, _ ->
                let newState = createState nextSet nextToken
                prev.DEdges.[ch] <- newState
                waitQueue.Enqueue (newState, nextSet)
        
        // 2. Process nondeterministic predicate transitions. | 2. 处理非确定的断言转移。
        for (pred, nextNfa) in predGroups do
            let (nextToken, nextSet) = getClosure [nextNfa]
            match stateDic.TryGetValue nextSet with
            | true, hNode -> 
                prev.NEdges <- (pred, hNode)::prev.NEdges
            | false, _ ->
                let newState = createState nextSet nextToken
                prev.NEdges <- (pred, newState)::prev.NEdges
                waitQueue.Enqueue (newState, nextSet)
    head
    
let FromRuleList (rules: LexicalRule<'T> list) =
    let nfaBuilder = NfaBuilder()
    let n = nfaBuilder.FromRuleList rules
    let maxId = nfaBuilder.MaxId
    FromNfa n maxId

// Tokenizes a single unit applying the Maximal Munch principle.
// 应用最长匹配原则 (Maximal Munch) 解析单个词法单元。
let private TokenizerOne (node: HybridState<'T>) (input: InputState) : ('T option * InputState) = 
    let mutable finalTokenAndLevel : ('T * int) option = None 
    let mutable finalInputState = input

    // Updates the final accepting token if a longer match or a higher priority match is found.
    // 发现更长的匹配，或在相同长度下发现更高优先级的匹配时，更新最终接受的 Token。
    let updateFinal curPos curTokenKindAndLevel = 
        if curPos.AbsoluteOffset > finalInputState.Position.AbsoluteOffset then
           finalTokenAndLevel <- Some curTokenKindAndLevel
           finalInputState <- { finalInputState with Position = curPos }
        else if curPos.AbsoluteOffset = finalInputState.Position.AbsoluteOffset then
            match finalTokenAndLevel with
            | Some (_, oldLevel) when snd curTokenKindAndLevel > oldLevel ->
                finalTokenAndLevel <- Some curTokenKindAndLevel
            | None -> 
                finalTokenAndLevel <- Some curTokenKindAndLevel
            | _ -> ()
        else ()

    // Explores all concurrent paths using Depth-First Search via Call Stack (Zero Heap Allocation).
    // 利用调用栈通过深度优先搜索 (DFS) 探索所有并行匹配路径（实现零堆内存分配）。
    let rec dfs (curState: HybridState<'T>) (curInput: InputState) =
        let (nextInput, charOps) = CurrentChar curInput
        match charOps with
        | None -> 
            // End of input stream reached. | 到达输入流末尾。
            match curState.AcceptTokenKind with
            | Some tk -> updateFinal nextInput.Position tk
            | _ -> ()
        | Some curChar ->
            // Check if the current state is an accepting state before consuming the next character.
            // 在消耗下一个字符前，检查当前状态是否已经是接受状态。
            match curState.AcceptTokenKind with
            | Some tuple -> updateFinal curInput.Position tuple
            | _ -> ()
            
            // Evaluate dynamic predicates. | 计算动态断言。
            curState.NEdges |> List.iter (fun (pred, next) ->
                if pred curChar then dfs next nextInput
            )
            
            // Perform fast dictionary lookup for exact character matches. | 通过字典快速查找精确的字符匹配。
            let ok, next = curState.DEdges.TryGetValue curChar
            if ok then 
                dfs next nextInput

    dfs node input
    let finalKind = finalTokenAndLevel |> Option.map fst
    (finalKind, finalInputState)

// Extracts the raw text slice without heap allocation.
// 在不分配堆内存的情况下截取原始文本切片。
let private LexFrom startInput endInput = 
    let startOffset = startInput.Position.AbsoluteOffset
    let length = endInput.Position.AbsoluteOffset - startOffset 
    startInput.Source.AsMemory().Slice(startOffset, length)
    
// Tokenizes the entire input stream, invoking error recovery (Panic Mode) when necessary.
// 解析整个输入流，在必要时调用错误恢复机制（恐慌模式）。
let TokenizerAll hybridStart input = 
    // Use ResizeArray (List<T>) to eliminate linked list allocation overhead.
    // 使用 ResizeArray 消除单向链表累加时的堆内存分配开销。
    let tokens = ResizeArray<Token<'T>>()
    
    let rec aux currentInput = 
        if currentInput.Position.AbsoluteOffset >= currentInput.Source.Length then
            ()
        else
            match TokenizerOne hybridStart currentInput with
            // Normal matching path: Ensures at least one character is consumed to prevent infinite loops.
            // 正常匹配路径：确保至少消耗了一个字符，防止陷入零长度匹配的死循环。
            | (Some tk, nextInput) when nextInput.Position.AbsoluteOffset > currentInput.Position.AbsoluteOffset ->   
                tokens.Add({ 
                    Kind = Some tk;         
                    RawText = LexFrom currentInput nextInput
                })
                aux nextInput
            
            // Unrecognized sequence -> Trigger Panic Mode error recovery.
            // 无法识别序列 -> 触发恐慌模式 (Panic Mode) 错误恢复。
            | _ -> 
                // 1. Force consume the invalid character to advance the cursor.
                // 1. 强行吃掉当前引起错误的字符，保证游标必然前进。
                let (advancedInput, _) = CurrentChar currentInput
                
                // 2. Probe loop: Greedily consume invalid characters until EOF or a valid sync point is found.
                // 2. 探测循环：不断吃掉非法字符，直到遇到文件末尾，或者下一个能够被成功解析的同步点。
                let rec skipInvalid skipInput =
                    if skipInput.Position.AbsoluteOffset >= skipInput.Source.Length then
                        skipInput
                    else
                        // Probe the next position to see if it produces a valid token.
                        // 试探下一个位置：能否解析出有效的规则？
                        let (tkOpt, probeInput) = TokenizerOne hybridStart skipInput
                        
                        // Sync point found: Stop skipping and keep the current cursor as the error boundary.
                        // 找到同步点：停止丢弃，保留当前游标作为错误片段的终点。
                        if tkOpt.IsSome && probeInput.Position.AbsoluteOffset > skipInput.Position.AbsoluteOffset then
                            skipInput 
                        else
                            // Still invalid: Keep consuming.
                            // 依然无法解析，继续吃掉当前字符。
                            let (nextSkip, _) = CurrentChar skipInput
                            skipInvalid nextSkip
                            
                let syncInput = skipInvalid advancedInput
                
                // 3. Package the contiguous block of invalid characters into an Error Token.
                // 3. 将跨越的所有连续非法字符打包成一个错误 Token (Kind = None)。
                tokens.Add({
                    Kind = None; 
                    RawText = LexFrom currentInput syncInput
                })
                aux syncInput

    aux input
    tokens