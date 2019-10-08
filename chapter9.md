# Branches & Loops – Part II

## Introduction

In the previous chapter, we finally added branch and loop constructs to our slowly evolving language. The constructs we chose are enough to tackle most if not all situations in programming. However, a good language should have some more constructs, to make it easier to read and comprehend if nothing else. We add some such constructs in this chapter.

## Rinse, Lather, Repeat, Until

Loop constructs, I've noticed, are very closely related to specific programming languages. The construct that a programmer uses most often tends to be influenced by the first language she learned. People who started with the Pascal programming language tend to be comfortable around the `REPEAT/UNTIL` loop.

That bit of trivia aside, the REPEAT/UNTIL loop is somewhat different from our While loop. Here's the syntax:

```pseudocode
Repeat
    <block>
Until <boolean expression>
```

At first glance, this does not seem all that different, except that it seems that this loop will keep looping (excuse me) while the boolean expression evaluates to false. Just an inverted While loop, right?

But take a closer look. The condition appears at the end of the loop. This means that the code in the block will be executed at least once unconditionally. There are situations which demand this behavior, and the `Repeat/Until` loop will be our weapon of choice for such situations.

## In the middle of the loop

So, we have a loop which checks a condition at the beginning(`While`), and one which checks a condition at the end(`Repeat`). How about one which checks a condition in the middle?

I'm not joking. There are situations where some code has to execute unconditionally, then a check needs to be made, and if the check is unsuccessful, some more code needs to be executed, and then the whole thing repeats.

The Ada programming language has a `loop/exit when/end loop` construct. Here's a simplified syntax:

```pseudocode
loop
    <code>
    exit when <boolean expression>
    <code>
end loop
```

The `exit when` statement can appear anywhere inside a `loop` block, and may appear any number of times.

Let's appropriate this statement for SIC.

## The Endless

There are situations where endless looping is necessary. Give me a minute (or eternity), and I'll think of one.

Seriously, when most people say that they need an endless loop, what they really want is a loop where the exit condition is not fixed at the beginning or end, and there may be more than one exit conditions. Which is exactly why we decided to implement the `Loop` loop.

In any case, if we really want an endless loop, all we have to do is not put any `Exit When` statements in a `Loop` block.

## Oh, For...

Next, we introduce the `For` loop. There's an in-joke in that last sentence. The `For` loop exists in almost all imperative languages, and can work in many different ways. The most common use of the `For` loop is to repeat a block of statements a predetermined number of times. This was the very first loop that I learned; the language was BASIC. Here's the syntax:

```pseudocode
FOR <variable> = <start> TO <end> [STEP <step>]
    <block>
NEXT
```

Here, `<start>`, `<end>` and `<step>` are numbers. Basically, the `<variable>` counts from the `<start>` value to the `<end>` value in steps of the `<step>` value, and iterates the `<block>` each time.

Sounds simple, but there are a number of tricky issues. Let's examine the loop's behaviour more closely.

At the beginning of the loop, the `<variable>` is set to the `<start>` value, and then compared with the `<end>` value. If the value of the `<variable>` is _greater_ (first issue! Explanation in a moment.), the loop terminates immediately. Otherwise, the block is executed, and then the value of the `<variable>` is _incremented_ (same issue) by the `<step>` value provided in the optional `Step` clause. If the `Step` clause is omitted, the variable's value is incremented by one. Then, the value of the `<variable>` is checked against the `<end>` value again, and so on.

The issue is this: the `Step` value may be negative! In which case, the variable (in this situation, referred to as the _loop counter_ or _iterator_) counts _down_. This obviously means that when we check the variable against the `<end>` value, we must check to see if it is _less_, rather than greater.

Here's another issue: we are assuming that the `<start>`, `<end>` and `<step>` values are constant numbers. In fact, they can be numeric expressions of any complexity. When should these numeric expressions be evaluated? It can't be on each iteration of the loop, because code in the loop can change the value of these expressions, and thus affect the number of times the loop will iterate.

Despite such issues, the `For` loop is quite popular, and so we will implement it. We will see how to tackle the issues shortly.

The C language, and its ilk, use a very different form of the For loop. Although that loop is also meant to be used for counted looping, it can (and often is) used for other kinds of looping situations. Although it has its good points, I always found this loop a little...I dunno...schizophrenic. We will go with the BASIC form of `For` for now, and look at adding the C form in a later chapter.

## A Very Select Statement

Here's a very common programming situation: If the value of a variable is 1, do something, if 2, do something else, if 3, do a third thing, and if none of the above, do a fourth thing. Essentially, the branching happens based on different values of a single variable or expression. We can do this using our `If...ElseIf...Else...EndIf` construct, but many languages provide a dedicated statement for such a situation: the _switch_ or _select_ statement.

Here is an example of the `switch` statement used by C, C++, Java, C# and most other languages of that family:

```pseudocode
switch(<variable>) {
    case 1:
        <block 1>
        break;
    case 2:
        <block 2>
    case 3:
        <block 3>
        break;
    default:
        <block 4>
}
```

The whole `switch` block's beginning and end is shown using braces ({}), as used by those languages.
If the value of the `<variable>` is 1, block 1 is executed, and execution continues after the
end of the `switch` block. If the value is 2, block 2 is executed, then block 3 is executed, and then execution continues after the end of the `switch` block. If the value is 3, block 3 is executed, and if it evaluates to anything else, block 4 is executed.

Note the `break` statement at the end of the first and third `case` blocks. Because this statement is omitted in the second `case`, if the `<variable>` evaluates to 2, blocks 2 and 3 are both executed. This feature is
called _falling through_, and is thought highly of in the C language. I find it weird. In my experience, you would not want to use fallthrough in most cases. Still, if you don't want fallthrough in a `case` block, you have to put a `break` in. It ends up being practically unnecessary but syntactically mandatory.

So, here's what we will do. For SIC, we will adopt the `switch` block , but will add a `FallThrough` statement, which will be the exact opposite of the `break` statement; i.e., it will cause execution to fall through to the next case. As I mentioned, I find fallthrough to be the exception rather than the rule. Thus, I'd rather use a `FallThrough` statement in the rare cases rather than a `break` statement in every case. Among modern languages, Go also has a `fallthrough` keyword.

Here's the proposed syntax:

```pseudocode
Switch <variable>
    Case 1
        <block 1>
    Case 2
        <block 2>
        FallThrough
    Case 3
        <block 3>
    Default
        <block 4>
End Switch
```

I would also like to use the word `Select` as a synonym for `Switch`.

## Goal

The goal of this chapter, therefore, is to parse and compile the `Repeat`, `Loop`, `For` and
`Switch` statements. Here are the syntax definitions:

```pseudocode
Repeat
    <block>
    [Continue Repeat]
    [Exit Repeat [When <boolean expression>]]
Until <boolean expression>

Loop
    <block>
    [Continue Loop]
    [Exit Loop [When <boolean expression>]]
End Loop

For <variable> = <start> To <end> [Step <step>]
    <block>
    [Continue For]
    [Exit For [When <boolean expression>]]
Next

Switch <expression>
    Case <expression>
        <block>
    Default
        <block>
End Switch
```

We'll look at BNF as we implement each statement.

Notice that we added the `When` clause to `Exit` in the case of `Repeat` and `For`, in addition to `Loop` for which it was originally intended. In fact, we will add it for `Exit While` as well.

## The Approach
The approach remains the same as the last chapter. We mostly need to add command parsers.

## First Step

Let's begin with the `Repeat/Until` loop. Here's the BNF:

```bnf
<repeatcommand> ::= "repeat"
                        <block>
                    <untilcommand>
<untilcommand>  ::= "until" <booleanexpression>
```

The `Repeat` command should create and emit a startpoint label, then parse a block. If the block is successfully parsed, it should emit its endpoint label.

The `Until` command should check that it was invoked inside a "repeat" block. If so, it should parse a boolean condition, and emit a jump back to the block's startpoint if the condition is false. Finally, it should end that block. 

We already have all the building blocks needed to write these. Add the following to **Commands.vb**.

```vbnet
Private Function ParseRepeatCommand() As ParseStatus
    Dim result As ParseStatus

    SkipWhiteSpace()
    
    ' There should be nothing after Repeat
    If Not EndOfLine Then
        result = CreateError(1, "end of statement")
    Else
        Dim startpoint As Integer
        Dim endpoint As Integer

        ' Generate and emit startpoint
        startpoint = m_Gen.DeclareLabel()
        m_Gen.EmitLabel(startpoint)

        ' Generate endpoint
        endpoint = m_Gen.DeclareLabel()

        ' Parse the Repeat block
        Dim repeatblock As New Block( _ 
                                "repeat", _
                                startpoint, _
                                endpoint _ 
        )


        result = ParseBlock(repeatblock)

        If result.Code = 0 Then
            ' Emit endpoint
            m_Gen.EmitLabel(repeatblock.EndPoint)
        End If
    End If

    Return result
End Function

Private Function ParseUntilCommand() As ParseStatus
    Dim result As ParseStatus

    ' Until is only valid if we are currently in a repeat block
    Dim repeatblock As Block = m_BlockStack.CurrentBlock

    If repeatblock.BlockType <> "repeat" Then
        result = CreateError(6, "Until")
    Else
        SkipWhiteSpace()

        ' There should be a boolean expression after Until
        If EndOfLine Then
            result = CreateError(1, "a boolean expression")
        Else
            result = ParseBooleanExpression()

            If result.Code = 0 Then
                ' If the boolean condition evaluates to FALSE
                ' we must jump to the start point.
                m_Gen.EmitBranchIfFalse(repeatblock.StartPoint)

                ' End the block
                result = BlockEnd()
            End If
        End If
    End If

    Return result
End Function
```

Pretty much what we said. Like all command parsers, the `Repeat` and `Until` parsers expect that the command itself has already been scanned and parsed by **ParseCommand**.

## Say when

To enable our `Continue` and `Exit` commands to work with the `Repeat` loop, all we have to do is to add "repeat" to our loop table. But before we do so, let's add a new, optional `When` clause for both of them. 

Currently, both commands are handled by a common parser, **ParseLoopControlCommand**, which emits an unconditional jump to either the startpoint (`Continue`) or the endpoint(`Exit`) of the block. We should check if a `When` clause exists, and if it does, parse and emit the boolean expression following it, and then emit a _conditional_ jump instead.

Here's the BNF for the `When` clause and the revised loop control commands:

```bnf
<loopcontrolconmmand>   ::= <exitcommand>|<continuecommand> [<whenclause>]
<exitcommand>           ::= "exit" <looptype>
<continuecommand>       ::= "continue" <looptype>
<whenclause>            ::= "when" <booleanexpression>
```

Change **ParseLoopControlCommand** in **Commands.vb** as follows:

```vbnet
Private Function ParseLoopControlCommand() As ParseStatus
    Dim result As ParseStatus

    ' The current token is either Exit or Continue
    Dim cmdName As String = CurrentToken.ToLowerInvariant()

    ' It should be followed by a valid loop type, and we 
    ' should be inside that type of loop

    ' Read the Exit loop type
    SkipWhiteSpace()
    ScanName()

    Dim loopkind As String = CurrentToken.ToLowerInvariant()

    If Not IsValidLoop(loopkind) Then
        result = CreateError(1, "a valid loop type")
    Else
        ' Try to get the block from the stack
        Dim loopBlock As Block = _
                m_BlockStack.GetClosestOuterBlock(loopkind)

        If loopBlock Is Nothing Then
            result = CreateError(6, cmdName & " " & loopkind)
        Else
            SkipWhiteSpace()

            If Not EndOfLine Then
                ' There should be a When clause here    
                result = ParseWhenClause()

                If result.Code=0 Then
                    If cmdName = "exit" Then
                        ' Emit conditional jump to EndPoint
                        m_Gen.EmitBranchIfTrue(loopBlock.EndPoint)
                    ElseIf cmdName = "continue" Then
                        ' Emit conditional jump to StartPoint
                        m_Gen.EmitBranchIfTrue(loopBlock.StartPoint)
                    Else
                        result = CreateError(6, cmdName)
                    End if
                End If
            Else
                result = Ok()
                
                If cmdName = "exit" Then
                    ' Emit jump to EndPoint
                    m_Gen.EmitBranch(loopBlock.EndPoint)
                ElseIf cmdName = "continue" Then
                    ' Emit jump to StartPoint
                    m_Gen.EmitBranch(loopBlock.StartPoint)
                Else
                    result = CreateError(6, cmdName)
                End if
            End If
        End If
    End If

    Return result
End Function
```

Next, add a new region called Clauses in **Commands.vb**, and add a **ParseWhenClause** method in it, as follows:

```vbnet
#Region "Clauses"
    Private Function ParseWhenClause() As ParseStatus
        Dim result As ParseStatus

        ScanName()

        If CurrentToken.ToLowerInvariant()<>"when" Then
            result = CreateError(1, "when")
        Else
            SkipWhiteSpace()

            result = ParseBooleanExpression()
        End If

        Return result
    End Function
#End Region
```

Clause parsers will have to scan their own first token, unlike command parsers where **ParseCommand** has already done that.

Now, all that remains is to add "repeat" to the list of valid loops, and to add `Repeat` and `Until` to the list of valid commands. You already know how to do the latter. Here's how you do the former.

Change the **InitLoops** method in **Commands.vb** as follows:

```vbnet
Private Sub InitLoops()
    m_loopTable = New List(Of String)

    ' Add loops here
    AddLoop("while")
    AddLoop("repeat")
End Sub
```

From now on, whenever we add a new type of loop, we will just need to add a line to this method. This won't be shown in the text any more; I'll just ask you to do it.

Speaking of which, add `Repeat` and `Until` to the list of valid commands, will you?

## End Repeat already

If at this point our compiler meets a line which contains `End Repeat`, it will happily finish the `Repeat` block, without any exit condition check! We don't want this.

The token `End` gets parsed by the **ParseEndCommand** method, which just checks the next token to see if it matches the current block. We need to add a check in that, for the `End Repeat` special case.

What should we do? Let's treat `End Repeat` as a synonym for `Until`, meaning it should be
followed by a boolean expression, and a jump to the start point if that evaluates to false. In other words, all we need to do is call **ParseUntilCommand**.

Modify **ParseEndCommand** in **Commands.vb**, like so:

```vbnet
Private Function ParseEndCommand() As ParseStatus
    Dim result As ParseStatus

    Dim block As Block = m_BlockStack.CurrentBlock

    ' We should be in a block when the End command
    ' is encountered.
    If Not block Is Nothing Then
        SkipWhiteSpace()
        ScanName()

        ' The token after the End command should 
        ' match the current block type
        If block.IsOfType(CurrentToken) Then
            ' If this is 'End Repeat'
            If CurrentToken.ToLowerInvariant() = "repeat" Then
                result = ParseUntilCommand()
            Else
                result = BlockEnd()
            End If
        Else
            ' Unless we are inside a comment block
            If m_inCommentBlock Then
                result = Ok()
            Else
                result = CreateError(1, block.BlockType)
            End If
        End If
    Else
        result = CreateError(2, "Not inside a block")
    End If

    Return result
End Function
```

That should do it. We can test the `Repeat` loop at this point.

## Testing the Repeat Loop

Compile and run. Test with the following:

```sic
Dim i As Integer:=1

Repeat
    print i

    if [i=3]
        i:=i+2
        continue repeat
    end if

    exit repeat when [i=8]

    i=i+1
Until [i>10]

print "Done"
```

As usual, try varying the tests. Use `End Repeat` instead of `Until`. Try the `When` clause for `Continue`. Introduce errors. Everything should work as expected.

