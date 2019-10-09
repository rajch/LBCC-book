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
    If m_BlockStack.IsEmpty _
            OrElse _
        m_BlockStack.CurrentBlock.BlockType <> "repeat" Then

        result = CreateError(6, "Until")
    Else
        Dim repeatblock As Block = m_BlockStack.CurrentBlock

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

## Loop, the Loop

With the plumbing we already have in place, the `Loop` loop can almost write itself. Here's the BNF:

```bnf
<loopcommand>   ::= "loop"
                        <block>
                    <endcommand>
```

Looks like we just need to start the block. Ending it, `Continue`, `Exit` have all been taken care of.

Add the new **ParseLoopCommand** to the Commands region **Commands.vb**, as follows:

```vbnet
Private Function ParseLoopCommand() As ParseStatus
    Dim result As ParseStatus

    SkipWhiteSpace()

    ' There should be nothing after Loop
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

        ' Create the Loop block
        Dim loopBlock As New Block( _ 
                            "loop", _
                            startpoint, _
                            endpoint _ 
        )

        result = ParseBlock(loopBlock)

        If result.Code = 0 Then
            ' Emit jump to startpoint
            m_Gen.EmitBranch(loopBlock.StartPoint)

            ' Emit endpoint
            m_Gen.EmitLabel(loopBlock.EndPoint)
        End If
    End If

    Return result
End Function
```

Once we add `Loop` to the list of commands and the list of loops, we are ready to test. Do that now.

## Testing the Loop Loop

Compile and run. Test with this:

```sic
int i:=1

loop
    print i
    exit loop when [i=4]
    i = i + 1
end loop
```

## For My Next Trick

The `For` loop is significantly trickier than the ones before. Let us examine the reasons. Here's the syntax again:

```pseudocode
For <variable> = <start> To <end> [Step <step>]
    <block>
    [Continue For]
    [Exit For [When <boolean expression>]]
Next
```
The loop counts from `<start>` to `<end>` in steps of `<step>`, setting the `<variable>` to the current value of the count in each iteration.

Here's the first tricky part: `<start>`, `<end>` and `<step>` can be numeric _expressions_. These expressions may involve variables. If code in the `<block>` changes any of these variables, the number of iterations should not change. We need to evaluate them _once_ before the loop itself begins, and not evaluate them again.

Here's the second: on every iteration, we have to compare the current value of `<variable>` with `<end>`. This is a less than or equal to comparison if the `<step>` value is positive (or not present, in which case it defaults to 1), and a greater than or equal to otherwise. At compile time, there's no certain way to tell if the `<step>` value is positive or negative, except in the default case[^1]. 

So here's what we will do to parse the `For` command. To begin with, we will parse the `<variable>`, then parse the `<start>` numeric expression and assign it to the variable. This is basically an assignment, for which we already have a parser. 

Then, we will parse the `<end>` and `<step>` numeric expressions, and _store them in two variables we generate ourselves_. These variables will be used internally only, and not be visible to the SIC program. This way, the those expressions involve variables and the variable values change, the loop won't be affected. We will use only the internally generated variable for the comparison.

Then, we will emit a start point label and parse a block. We know how to do this.

Once the block is successfully parsed, we will emit code the increment the variable by the `<step>` value, or by 1 if the `<step>` value has not been specified. We will then emit the comparison with the `<end>` value, and jump to the start point if the comparison is true. This is where we have to apply intelligence regarding the type of comparison.

Finally we will emit the end point label. This will ensure that `Exit For` works properly, using code that we have already written.  

We need a utility method which can generate unique variable names for us. Add it to the Helper Functions region in **Parser.vb**, as follows:

```vbnet
Private Function MakeUniqueVariableName() As String
    Dim result As New StringBuilder("_clrcompiler_t_i4_")
    Do
        result.Append(CInt(Rnd() * 10))
    Loop Until Not m_SymbolTable.Exists(result.ToString)
    
    Return result.ToString
End Function
```

Armed with that, let us write **ParseForCommand**. Add it to the Commands region in **Commands.vb**, as follows:

```vbnet
Private Function ParseForCommand() As ParseStatus
    Dim result As ParseStatus

    ' Try to scan a variable name
    ResetToken()
    SkipWhiteSpace()
    ScanName()

    If TokenLength = 0 Then
        Return CreateError(1, "a variable")
    End If

    ' Check if variable exists, and is numeric
    Dim varname As String = CurrentToken.ToLowerInvariant()
    If Not m_SymbolTable.Exists(varname) Then
        Return CreateError(4, CurrentToken)
    End If

    Dim variable As Symbol = m_SymbolTable.Fetch(varname)
    If Not variable.Type.Equals(GetType(Integer)) Then
        Return CreateError(1, "a numeric variable")
    End If

    ' Parse the next two tokens as an assignment statement
    result = ParseAssignmentStatement()

    If result.Code<>0 Then
        Return result
    End If

    ' Parse the To Clause
    SkipWhiteSpace()
    ScanName()

    If TokenLength=0 OrElse _
        CurrentToken.ToLowerInvariant()<>"to" Then

        Return CreateError(1, "To")
    End If

    ' Parse the <end> numberic expression
    SkipWhiteSpace()
    result = ParseNumericExpression()

    If result.Code<>0 Then
        Return result
    End If

    ' Declare a uniquely named variable and emit a store
    ' to it, thus storing the <end> value.
    Dim endVariableName As String = MakeUniqueVariableName()
    Dim endVariable As Integer =  _
            m_Gen.DeclareVariable( _
                    endVariableName, _
                    GetType(System.Int32) _
            )

    m_Gen.EmitStoreInLocal(endVariable)

    ' Process the option <step> value
    Dim defaultStep As Boolean = True
    Dim stepVariableName As String
    Dim stepVariable As Integer

    SkipWhiteSpace()
    If Not EndOfLine Then
        ScanName()

        If TokenLength=0 OrElse _
            CurrentToken.ToLowerInvariant()<>"step" Then
            Return CreateError(1, "step")
        End If

        SkipWhiteSpace()
        result = ParseNumericExpression()

        If result.Code<>0 Then
            Return result
        End If

        stepVariableName = MakeUniqueVariableName()
        stepVariable = m_Gen.DeclareVariable( _
                            stepVariableName, _
                            GetType(System.Int32) _
                    )

        m_Gen.EmitStoreInLocal(stepVariable)
        defaultStep = False
    End If

    ' At this point, initialization of the loop is complete
    ' We need to jump to the comparison. The jump is required
    ' because at this point, we will emit the code that
    ' increments the loop counter, and this incrementing should
    ' not happen on the first iteration of the loop. So, we
    ' declare a label that marks the start of the comparison,
    ' and emit a jump to it.
    Dim comparisonStart As Integer = m_Gen.DeclareLabel()
    m_Gen.EmitBranch(comparisonStart)

    ' Declare startpoint and endpoint, and emit the startpoint.
    ' At this point, we will emit code that increments the loop
    ' counter, and then code that checks the counter against the
    ' <end> value. This is the real starting point of the loop.
    Dim startpoint As Integer = m_Gen.DeclareLabel()
    Dim endpoint As Integer = m_Gen.DeclareLabel()

    m_Gen.EmitLabel(startpoint)

    ' Increment the counter variable by the step value
    m_Gen.EmitLoadLocal(variable.Handle)
    
    If defaultStep Then
        m_Gen.EmitNumber(1)
    Else
        m_Gen.EmitLoadLocal(stepVariable)
    End If
    
    m_Gen.EmitAdd()
    m_Gen.EmitStoreInLocal(variable.Handle)

    ' The comparison starts here. So, we emit the comparisonStart
    ' label.
    m_Gen.EmitLabel(comparisonStart)

    ' If the default step value has not been used
    ' Emit a check to see whether the step value is negative
    ' If step is negative, emit <end> and <variable> in that order,
    ' and jump to the comparison. In all other cases, emit 
    ' <variable> and <end> in that order.
    ' Then do the comparison
    Dim normalOrderLabel As Integer
    Dim actualCompareLabel As Integer

    If Not defaultStep Then
        With m_Gen
            normalOrderLabel = .DeclareLabel()
            actualCompareLabel = .DeclareLabel()
            ' Check if step is negative
            .EmitLoadLocal(stepVariable)
            .EmitNumber(0)
            .EmitGreaterThanComparison()
            ' It is. Normal comparison
            .EmitBranchIfTrue(normalOrderLabel)
            ' It isn't. Emit end and counter variables,
            ' in that order
            .EmitLoadLocal(endVariable)
            .EmitLoadLocal(variable.Handle)
            ' Jump to actual comparison
            .EmitBranch(actualCompareLabel)
            ' The normal order of comparison
            ' will happen now
            .EmitLabel(normalOrderLabel)
        End With
    End If

    ' Emit counter and end variables, in that order 
    m_Gen.EmitLoadLocal(variable.Handle)
    m_Gen.EmitLoadLocal(endVariable)
    
    ' The negative step case jumps to this point
    If Not defaultStep Then
        m_Gen.EmitLabel(actualCompareLabel)
    End If

    m_Gen.EmitGreaterThanComparison()
    ' If the comparison succeeds, the loop is over
    m_Gen.EmitBranchIfTrue(endPoint)

    ' Now, Parse the block
    Dim forBlock As Block = New Block( _ 
                                "for", _
                                startpoint, _
                                endpoint _
    )

    result = ParseBlock(forBlock)
    If result.Code = 0 Then
        ' Jump to the startpoint, where the <variable> 
        ' will be incremented and the comparison will be done
        m_Gen.EmitBranch(forBlock.StartPoint)

        ' Emit the endpoint
        m_Gen.EmitLabel(forBlock.EndPoint)
    End If

    Return result
End Function
```

This is probably the longest method we have written so far. Examine it carefully. We have implemented it exactly how we said we would, but some points bear highlighting.

Notice how we differentiate between the default step value and a non-default one. In the case of the default step value (no `Step` clause in the `For` statement), a lot of code ends up _not getting generated_. This is the way things should be. In fact, this is the first case of _optimization_ in this compiler. We will study optimization in detail in future chapters, but for now, remember this: not generating unnecessary code is a very good example of optimization.

In fact, if there were some way of knowing whether the final result produced by _ParseNumericExpression_ was positive or negative, we could have optimized this code further. As mentioned before, this is one case where keeping scanning and parsing so tightly coupled, and generating code immediately on parsing, is a disadvantage.

Moving on, we want the `For` block to end when either the `End For` or the `Next` command is encountered. Let's write the parser for the `Next` command.

Add the following to **Commands.vb**:

```vbnet
Private Function ParseNextCommand() As ParseStatus
    Dim result As ParseStatus
    
    If (Not m_BlockStack.IsEmpty) _
            AndAlso _
        m_BlockStack.CurrentBlock.BlockType = "for" Then

        result = BlockEnd()
    Else
        result = CreateError(6, "Next")
    End If

    Return result
End Function
```

And that's that. `End For` will automatically be taken care of, as will `Exit For` and `Continue For`, as soon as we add `For` and `Next` to the list of valid commands, and "for" to the list of valid loops. Do that
now.

## Testing the For Loop

Compile and run. Test with the following:

```sic
Dim i As Integer

Print "Countdown..."

For i:=10 To 1 Step -1
    Print i
Next

Print "Blast off"

int j

For i=1 to 5 Step 2
    Print "--------------"
    For j:=i To i+1
        Exit For When j==2

        Print "j"
        Print j
    End For

    If i==3 Then
        
        Continue For
    Else
        Print "i"
        Print i
    End If
Next

Print "Done"
```

## Flip The Switch

And now for every parser writer's nightmare...the `Switch` statement.

Technically, it shouldn't be all that difficult. The `Switch` statement can be considered syntactic sugar, sprinkled on plain old `If/ElseIf/Else/EndIf`. Practically, this sugar has a lot of calories. Let's take a closer look at the syntax.

```pseudocode
Switch <expression>
    Case <value 1>
        <block 1>
    Case <value 2>
        <block 2>
        FallThrough
    Case <value 3>
        <block 3>
    Default
        <block 4>
End Switch
```

The `Switch` statement allows us to specify an expression, which becomes the basis of all comparisons in the block. Each time we meet a `Case` statement, we need to compare the expression following it against the `Switch` expression (for equality), and process the block after the `Case` statement if the two expressions match. 

Here's the first catch: where does each `Case` block end? Could be another `Case` statement, could be a `Default` statement, and could be the end of the `Switch` block.

Here's another issue: there cannot be any code after the `Switch` statement, and before the first `Case`. Whereas `Switch` is the block proper, code actually appears inside `Case` blocks.

Finally, there can be one and only one `Default` in a single `Switch` block. This one's no problem; we did this with the `Else` statement earlier.

Only the second issue, the one about a `Case` statement having to immediately follow a `Switch` statement, will cause any major changes to what we have so far. The other issues can be taken care of, in much the same way that we took care of the `If` statement.

One big difference is that we somehow need to keep track of the initial `Switch` expression, and compare it against every `Case`. We'll use the technique we used in the `For` statement; we'll generate an internal variable and store the value of the `Switch` expression in that.

First, add the following to **Utilities.vb**:

```vbnet
Public Class SwitchState
    Public ExpressionVariable As Integer
    Public ExpressionType As Type
    Public CaseFlag As Boolean
    Public DefaultFlag As Boolean
    Public FallthroughFlag As Boolean
End Class
```

Add the following to the Commands region of **Commands.vb**:

```vbnet
Private Function ParseSwitchCommand() As ParseStatus
    Dim result As ParseStatus

    SkipWhiteSpace()

    ' Parse an Expression
    SkipWhiteSpace()
    result = ParseExpression()

    If result.Code<>0 Then
        Return result
    End If

    ' Store old Switch state
    ' This behavior is necessary because Switch
    ' statements can be nested.
    Dim oldSwitchState As SwitchState = m_SwitchState

    ' Create new state, with an internal variable to
    ' store the Switch expression just parsed
    m_SwitchState = New SwitchState()
    With m_SwitchState
        .CaseFlag = True
        .DefaultFlag = False
        .FallThroughFlag = False
        .ExpressionVariable = _ 
                m_Gen.DeclareVariable( _
                    MakeUniqueVariableName(), _
                    m_LastTypeProcessed _
                )
        .ExpressionType = m_LastTypeProcessed
    End With
    
    ' Emit a store to the internal variable. ParseExpression
    ' has left the value of the expression on the stack
    m_Gen.EmitStoreInLocal(m_SwitchState.ExpressionVariable)

    ' There should be nothing else on the line
    SkipWhiteSpace()
    If Not EndOfLine Then
        Return CreateError(1, "end of statement")
    End If

    ' Pre-set the endpoint. This will not change
    Dim endpoint As Integer = m_Gen.DeclareLabel()

    ' Parse the block
    Dim switchBlock As New Block( _
                            "switch", _
                            -1, _
                            endpoint _
    )

    result = ParseBlock(switchBlock)

    If result.Code = 0 Then
        ' If there is a dangling startpoint
        ' emit it
        If switchBlock.StartPoint <> -1 Then
            m_Gen.EmitLabel(switchBlock.StartPoint)
        End If

        ' Emit the endpoint
        m_Gen.EmitLabel(switchBlock.EndPoint)

        ' Restore the old Switch State
        m_SwitchState = oldSwitchState
    End If

    Return result
End Function

Private Function ParseCaseCommand() As ParseStatus
    Dim result As ParseStatus

    If m_BlockStack.IsEmpty _
            OrElse _
        m_BlockStack.CurrentBlock.BlockType <> "switch" Then

        Return CreateError(6, "Case")
    End If

    Dim switchblock As Block = m_BlockStack.CurrentBlock
    
    ' If default statement has already been processed
    ' get out
    If m_SwitchState.DefaultFlag Then
        Return CreateError(6, "Case")
    End If

    ' This could be the first case statement, or the end
    ' of the previous case statement. In the latter case,
    ' we need to emit a jump to the current block's
    ' endingpoint before we start processing this case
    ' statement
    If Not m_SwitchState.CaseFlag Then
        ' This is the end of the previous case statement
        m_Gen.EmitBranch(switchblock.EndPoint)
    End If

    ' If the fallthrough flag is set, we need to jump over
    ' the condition. So we generate a label and a jump
    ' Fallthrough is weird 
    Dim fallThroughLabel As Integer

    If m_SwitchState.FallThroughFlag Then
        fallThroughLabel = m_Gen.DeclareLabel()
        m_Gen.EmitBranch(fallThroughLabel)
    End If

    ' Emit the current startpoint if there is one
    If switchblock.StartPoint <> -1 Then
        m_Gen.EmitLabel(switchblock.StartPoint)
    End If

    ' Create new startpoint for the next case
    switchblock.StartPoint = m_Gen.DeclareLabel()

    SkipWhiteSpace()
    ' Depending on the type of the switch expression
    ' do the appropriate type of expression
    With m_SwitchState
        If .ExpressionType.Equals( _
                GetType(System.Int32) _
            ) Then

            result = ParseNumericExpression()
        ElseIf .ExpressionType.Equals( _
                    GetType(System.String) _
                ) Then

            result = ParseStringExpression()
        ElseIf .ExpressionType.Equals( _
                    GetType(System.Boolean) _
                ) Then

            result = ParseBooleanExpression()
        Else
            result = CreateError(1, "a valid expression")
        End If

        If result.Code = 0 Then
            ' Emit the Switch Expression
            m_Gen.EmitLoadLocal(.ExpressionVariable)

            ' Emit comparison
            If .ExpressionType.Equals( _
                    GetType(System.String) _
                ) Then

                m_Gen.EmitStringEquality()
            Else
                m_Gen.EmitEqualityComparison()
            End If

            ' If NOT equal, jump to the next Case
            m_Gen.EmitBranchIfFalse(switchBlock.StartPoint)

            ' Emit the Fallthrough label if the Fallthrough
            ' flag is set, and reset the flag
            If m_SwitchState.FallThroughFlag Then
                m_Gen.EmitLabel(fallThroughLabel)
                m_SwitchState.FallThroughFlag = False
            End If

            ' Reset the case flag
            m_SwitchState.CaseFlag = False
        End If
    End With

    Return result
End Function

Private Function ParseDefaultCommand() As ParseStatus
    Dim result As ParseStatus

    If m_BlockStack.IsEmpty _
            OrElse _
        m_BlockStack.CurrentBlock.BlockType <> "switch" Then

        Return CreateError(6, "Default")
    End If

    Dim switchblock As Block = m_BlockStack.CurrentBlock
    
    ' If default statement has already been processed
    ' get out
    If m_SwitchState.DefaultFlag Then
        Return CreateError(6, "Default")
    End If

    ' This default could be the ONLY case in the
    ' switch block, or the last case

    ' In the first situation, we need to emit a jump to
    ' the current block's endpoint before we start
    ' processing this
    If Not m_SwitchState.CaseFlag Then
        ' This is the end of a previous case statement
        m_Gen.EmitBranch(switchblock.EndPoint)
    End If

    ' Emit the current startpoint if there is one
    If switchblock.StartPoint <> -1 Then
        m_Gen.EmitLabel(switchblock.StartPoint)
    End If

    ' Reset the Case flag
    m_SwitchState.CaseFlag = False

    ' Set the default flag
    m_SwitchState.DefaultFlag = True

    ' No more startpoints needed, as default is the
    ' last case
    switchblock.StartPoint = -1

    Return result
End Function
```

Our approach here is very similar to the way we processed the `If` command. We define an end point for the block when we parse the `Switch` statement. Each `Case` statement defines a new start point (of the next `Case` or `Default`), performs a comparison, and jumps to that start point if the comparison fails. If a `Case` or `Default` statement follows a prior `Case` statement (m_CaseFlag is false. Remember, we set this to True when we parse the `Switch` statement.), an unconditional jump to the end point is emitted before
the `Case` statement is processed, thus ensuring that a previous `Case` does not fall through to the current one. At the end, we emit a dangling start point if one exists, emit the end point, and that's that.

Notice how we process a flag called the fallthrough flag in the `Case` command. If fallthrough is indicated in a `Case` block, the code in the next `Case` block (or `Default` block) should also be executed, without checking the switch expression. We have taken care of that.

If you look carefully, you will notice that unlike `ElseIf` or `Else`, each `Case` statement does not begin a new block. There's only one block: `Switch`. It's state is modified as we encounter `Case` or `Default` statements. We created the SwitchState class in **Utilities.vb** to represent that state.

Two things remain. First, the only statement allowed on the line following a `Switch` should be a `Case`. Actually, no. A `Switch` statement may be followed immediately by an `End Switch` (rather useless, but legal), or a `Comment` statement, or a `Rem` statement, or a `Case` statement, or a `Default` statement. For the first time since it was introduced, we need to modify the **ParseCommand** method to take care of this.

Change it in **Parser.vb**, as follows:

```vbnet
Private Function ParseCommand() As ParseStatus
    Dim result As ParseStatus
    
    If TokenLength = 0 Then
        result = CreateError(1, "a valid command")
    Else
        Dim commandname As String = _
                CurrentToken.ToLowerInvariant()

        If commandname = "comment" Then
            result = ParseCommentCommand()
        ElseIf commandname = "end"
            result = ParseEndCommand()
        ElseIf commandname = "rem" Then
            result = ParseRemCommand()
        ElseIf m_inCommentBlock Then
            ' Ignore rest of line
            SkipRestOfLine()
            ' All is good in a comment block
            result = Ok()
        ElseIf m_SwitchState IsNot Nothing _
                    AndAlso
                m_SwitchState.CaseFlag Then

            If commandname = "case" Then
                result = ParseCaseCommand()
            ElseIf commandname = "default" Then
                result = ParseDefaultCommand()
            Else
                result = CreateError(1, "Case or Default")
            End If
        Else
            If IsValidCommand(commandname) Then
                Dim parser as CommandParser = _
                        m_commandTable(commandname)

                result = parser()
            Else
                result = CreateError(1, "a valid command")
            End If
        End If
    End If

    Return result
End Function
```

Finally, the `FallThrough` command needs to be processed. We have already done the hard work in the `Case` command. All we need to do is ensure that a `FallThrough` command appears inside a switch block, and that it needs to immediately be followed by a `Case` or `Default` statement.

Add the following to the Commands region in **Commands.vb**:

```vbnet
Private Function ParseFallThroughCommand() As ParseStatus
    Dim result As ParseStatus

    If m_BlockStack.IsEmpty _
            OrElse _
        m_BlockStack.CurrentBlock.BlockType <> "switch" Then

        result = CreateError(6, "Fallthrough")
    Else
        m_SwitchState.CaseFlag = True
        m_SwitchState.FallThroughFlag = True
        result = Ok()
    End If
    
    Return result
End Function
```

All that remains is to add `Switch`, `Case`, `Default` and `FallThrough` to the list of valid command. Do that now.

## Testing the Switch Statement

Compile and run. Test with the following:

```sic
Int ItemPrice := 800
Dim CardType As String = "Platinum"
Dim DiscountPercent As Integer

Switch ItemPrice/100
    Case 10
        Print "Because 10"
        DiscountPercent:= 10
    Case 9
        Print "Because 9"
        FallThrough
    Case 8
        Print "Because 8"
        FallThrough
    Case 7
        Print "Because 7"
        Switch CardType
            Case "Platinum"
                Print "Because Platinum"
                DiscountPercent:=10
            Default
                Print "Because Not Platinum"
                DiscountPercent := 8
        End Switch
    Default
        Print "Because no other choice"
        DiscountPercent := 0
End Switch

Print "Discount percent is:"
Print DiscountPercent
```

Try different combinations of ItemPrice and CardType. Also try introducing errors. Everything should work as expected.

## Conclusion

At this point, our language has a rich set of branch and loop constructs. The next logical step would be procedures and functions. However, as I'm sure you noticed, our implementation is getting somewhat unwieldy now. As I stated in the first chapter, we took a rather "procedural" approach to building our compiler. Thus far, this approach has allowed me to explain things easily. But we are fast reaching a point where the approach will outlive its convenience. So, very soon, we will indulge in our first major refactoring
exercise, and come up with a more object-oriented compiler.

Another thing that will change, shortly, is our approach to scanning, parsing, and code generation. In our current implementation, the three work very closely together. By separating them into distinct parts, we will be able to do a lot of things much more efficiently. Now that we have understood the basics, it will be a lot easier for me to explain each topic in a more isolated manner.

But, I'm not finished with this implementation just yet. There are a few more things we can do to this compiler before we start working on version 2.0. And so, in the next chapter, we will add debugging capabilities to our language, such that programs written in it can be interactively debugged using any CLR debugging tool.

---
[^1]: This is where our approach of generating code the moment we parse a token becomes a problem: if the entire expression could be parsed and further examined before generating any code, we could have made more intelligent decisions at compile time.

