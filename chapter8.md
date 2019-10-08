# Branches and Loops Part I

## Introduction

In the last chapter, we made our compiler (and language) capable of declaring and initializing variables, and using variables in expressions. It's time now to tackle the next important programming concept: _control flow_, specifically _branches_ and _loops_.

## Control Flow

Like most other programming languages, our SIC language expects instructions to be executed in _sequence_. Our compiler reads the source, and emits CIL in the same sequence as the source statements. In real life, there will be many situations where the resulting CIL will need to execute in a different sequence. The three main examples of such situations are:

* Execution of an arbitrary set of statements under certain conditions. This is called _selection_. Every language allows some sort of selection construct, typically the ubiquitous `if` statement.
* Repeated execution of an arbitrary set of statements. This is called _iteration_ or _looping_. Again, every language has some form of a loop instruction. The most common example is the `while` statement.
* Execution of a known sequence or group of instructions as a unit, in lieu of a single instruction. Such groups are known as sub-programs, subroutines or functions.

Functions and subroutines are a topic unto themselves, and as such will be covered a few chapters down the line. In this chapter, we concern ourselves with selection and loops.

## Branches

The essential concept of selection can be expressed via this statement: "Carry out the following instructions if this condition is true". 

How do we ensure that the instructions following the condition are not executed if the condition is false? In most machine/assembly languages including CIL, there are instructions which cause execution to jump or _branch_ to another statement. Such an instruction can be used to jump over instructions that need to be carried out if the condition is true.

What about high-level languages? In the early days, most languages had some form of the infamous `goto` statement, which permitted so-called unconditional branching. This statement usually took the form

```pseudocode
Goto <label>
```

where `<label>` was some way of marking a location in the program; perhaps a line number or a name. The `goto` statement has been discouraged for years, because it supposedly encourages sloppy, unreadable programming. It can be quite difficult (for humans) to read and understand a program littered with `goto` statements. Nevertheless, in many early programming languages, the `goto` statement was the only way to both conditionally execute (or not execute) statements, and execute statements repeatedly.

As programming languages developed, several control flow constructs were added to improve the clarity and quality of programs. Such constructs could be used instead of `goto` statements. The use of such constructs collectively came to known as _structured programming_.

The most common use of the `goto` statement was to handle conditions. Not coincidentally, the most
common constructs in structured programming were those which dealt with _conditional branching_, the most common of these being the aforementioned `if` statement.

Back when I first learned about structured programming, the term "branch" was used as a loose synonym for conditional branching constructs. I will use that meaning in this and the next chapter, as we arm our
compiler with a few conditional branching constructs, starting with the `if` statement.

## Loops

There are many situations in programming where a sequence or block of statements have to be repeated, either a known number of times, or depending on some condition. A language construct that supports this functionality is called a _loop_. Loops can be very useful for writing most kinds of computer programs.

In this and the next chapter, we will add some loop constructs to our language and compiler, beginning with
the `while` construct.

## Goal

In this chapter, we will enhance our compiler and our language with one branch and one loop construct.

The branch construct we will implement is the `If` statement, as described below:

```pseudocode
If <boolean expression> [Then]
    <block>
[ElseIf < boolean expression> [Then]
    <block>]*
[Else
    <block>]
End If
```

The `If` statement is followed by a boolean expression, which in turn may optionally (that's what the square brackets in the pseudocode mean) be followed by the keyword `Then`. The lines that follow will be a block of instructions, which will end, in the simplest case, with the `End If` statement. These instructions will be carried out only if the boolean expression evaluates to true.

There may optionally be one and only one `Else` statement, followed by another block. The instructions in the `Else` block are to be carried out only if the boolean expression following the `If` statement evaluates to false.

Finally, there may be any number (that's what the star in the pseudocode means) of `ElseIf` statements, each followed by a boolean expression and block. All of them must occur after the initial `If` statement, and before an `Else` statement if one exists. `ElseIf` statements allow for checking multiple conditions. If the boolean expression following the first `If` evaluates to false, the boolean expression following the first `ElseIf` is checked. If that one evaluates to false, the next one is checked, and so on.

We will also introduce a common loop construct in this chapter: the `While` statement. The syntax is simple:

```pseudocode
While <boolean expression>
    <block>
    [Continue While]
    [Exit While]
End While
```

The block of statements between the `While` and `End While` statements will be repeated, as long as the boolean expression following the `While` statement evaluates to true. The optional `Exit While` statement will cause an immediate exit; i.e., execution will continue after the `End While` statement. The optional `Continue While` statement will cause execution to continue at the start of the `While` statement, which means that the condition will be re-evaluated.

## The Approach

In this chapter, we return to the approach we took in chapter 6. The elements of the `If` and `While` statements are essentially commands, so we need to write parser methods as appropriate, and add them to the list of commands.

## First Step

To begin with, let us parse the `If` statement. This is the point where we ask the usual question: how do we do branching in CIL?

Thus far, we have been emitting CIL instructions as soon as we parse anything. In the resulting executable, these instructions are executed in the order in which they were emitted. How do we cause instructions to be executed out of order?

Like any other "machine" language, CIL provides jump or branch instructions, which cause execution to, well, jump to another instruction. In CIL, the destination of such jump instructions is a number, which is a signed offset from the current instruction. This means that if we emit a branch instruction with the offset 4, the next instruction executed will be the one four places after the current instruction; if we use the offset -3, it will be the one three places before[^1].

I'm sure you can see a problem immediately. In a parser like ours, at the point when a jump instruction must be emitted, we don't yet know the exact offset to jump to. To understand this, let us examine how our `If` statement might translate down to CIL.

The boolean expression following the `If` statement can be parsed using our usual **ParseBooleanExpression**
method. Now, if this condition evaluates to false, then we need to jump _past_ the block of instructions that follow the If statement, because that block will execute if the boolean expression is True. Where do we need to jump to? In the simplest case, to the statement after the matching `End If` (we'll discuss the other cases later). Now, we cannot know the offset of that location. We have not parsed the block yet, and so don't know how many instructions we have to jump over!

Luckily, we are using the Reflection Emit library to generate CIL. The library provides a nice feature called a Label, which allows us to both mark a point to jump to in the CIL instruction stream, and emit a jump instruction that targets that point. We can actually emit a jump instruction to a label before using it to mark the destination point. The Label object keeps track of all CIL between the jump and the mark, and updates the correct offset in the jump instruction. This solves our problem.

So, at this point, we will enhance our CodeGen class with the ability to define and emit Labels, as well as emit some jump instructions. There are many jump instructions in CIL.  We will add only the following instructions for now:

|OpCode|What it does|
|---|---|
|Brfalse _target_|Expects a boolean or integer value (It can work with some other values, but let's stick to those two for now) on the stack. If the value is False or zero, jumps to the target location specified.|
|Brtrue _target_|Expects a boolean or integer value on the stack. If the value is True or non-zero, jumps to the target location specified.|
|Br _target_|Unconditionally jumps to the target location specified.|

The _target_ is a Label in all cases.

## Modifying CodeGen

Add the following to the CodeGen class in **CodeGen.vb**:

```vbnet
Private m_Labels As New List(Of Label)

Public Function DeclareLabel() As Integer
	Dim lbl As Label = m_ILGen.DefineLabel()
	m_Labels.Add(lbl)
    Return m_Labels.Count - 1
End Function

Public Sub EmitLabel(ByVal labelNumber As Integer)
	Dim lbl As Label = m_Labels(labelNumber)
	m_ILGen.MarkLabel(lbl)
End Sub

Public Sub EmitBranchIfFalse(ByVal labelNumber As Integer)
	Dim lbl As Label = m_Labels(labelNumber)
	m_ILGen.Emit(OpCodes.Brfalse, lbl)
End Sub

Public Sub EmitBranchIfTrue(ByVal labelNumber As Integer)
	Dim lbl As Label = m_Labels(labelNumber)
	m_ILGen.Emit(OpCodes.Brtrue, lbl)
End Sub

Public Sub EmitBranch(ByVal labelNumber As Integer)
	Dim lbl As Label = m_Labels(labelNumber)
	m_ILGen.Emit(OpCodes.Br, lbl)
End Sub
```

The CodeGen class now maintains a list of labels. The **DeclareLabel** method uses the **DefineLabel**
method of the ILGenerator object (that we use to emit all our code) to actually define a label, stores it in the list, and returns its position in the list as a handle. The handle can be used by the parser while calling **EmitLabel**, which uses the **MarkLabel** method of the ILGenerator object to actually mark an instruction point. It’s important to note that our **EmitLabel** method should be called _before_ emitting the instructions that will be executed after the jump. The label must come before the jump target.

The **EmitBranchIfTrue**, **EmitBranchIfFalse** and **EmitBranch** methods emit the respective CIL branch instructions. Note that **EmitLabel** need not be called before calling any of these. As long as the label that these instructions jump to has been declared (using **DeclareLabel**), these will function properly. Of course, at some point, the **EmitLabel** method would have to be called; otherwise we will end up with jumps to labels that do not occur in the instruction stream, resulting in an invalid application! However, we will not check for this. The nature of our parser will prevent that particular error.

## Parsing the If Command

Let's start by parsing the simplest form of the `If` statement, as follows:

```pseudocode
If <boolean expression> [Then]
    <block>
End If
```

In BNF, that would be:

```bnf
<ifcommand>  ::= "if" <booleanexpression> ["then"]
                    <block>
                 <endcommand> 
```

We already know how to parse boolean expressions, blocks of statements  and the `End` command, from chapters 4 through 6. Thus, parsing the `If` command itself is straightforward.

Add the following to the "Commands" region in **Commands.vb**:

```vbnet
Public Function ParseIfCommand() As ParseStatus
    Dim result As ParseStatus

    SkipWhiteSpace()
    result = ParseBooleanExpression()

    If result.Code=0 Then
        SkipWhiteSpace()

        If Not EndOfLine Then
            ' Try to read "then"
            ScanName()
            If CurrentToken.ToLowerInvariant<>"then" Then
                result = CreateError(1, "then")
            Else
                ' There shouldn't be anything after "then"
                SkipWhiteSpace()
                If Not EndOfLine Then
                    result = CreateError(1, "end of statement")
                End If
            End If
        End If

        If result.Code=0 Then
            Dim endpoint As Integer = m_Gen.DeclareLabel()

            ' If the condition just emitted is false, emit jump
            ' to endpoint
            m_Gen.EmitBranchIfFalse(endpoint)

            ' Parse the "If" block
            Dim ifblock As Block = New Block( _
                                "if", _
                                endpoint, _
                                endpoint
            )

            result = ParseBlock(ifblock)

            ' If the block was successfully parsed, emit
            ' the endpoint label
            If result.Code = 0  Then
                m_Gen.EmitLabel(ifblock.EndPoint)
            End If
        End If
    End If

    Return result
End Function
```

Like any other command, the **ParseIfCommand** method assumes that the calling **ParseCommand** method has already scanned the command itself. Thus, we parse a boolean expression immediately. If this succeeds, we check to see if the next token is a `Then`, and just "eat" it. We then check if there is anything else on the line (there shouldn't be). If not, we declare a label (which will mark the end of the `If` statement) and emit a jump to that label if the boolean expression evaluated to false (if the condition is false, execution continues after the `End If`). Then, we call **ParseBlock**. If a block is parsed successfully, we emit the label that we had generated earlier.

Note how we declare a label, and then use it to construct a Block object. Let's take a short aside to discuss how we handle blocks.

When we introduced blocks in chapter six, we had very briefly mentioned their start and end points. We had not used these in the `Comment` block, covered in that chapter. We will use them now. 

As can be seen from the code above, these properties of a Block object store "handles" of labels. In most cases, the start and end points of a block will store the locations where the code for the block starts and ends respectively.

In this particular case, the start and end point properties are both set to the label that marks the end of the `If` statement. This is because there is never a need to jump to the start of an `If` statement.

Note that when calling **EmitLabel**, we do not use the variable called endpoint. Instead, we use the EndPoint property of the Block object. They should be the same value, right? For now, they are. This will change shortly.

Finally, to make **ParseCommand** recognize and process `If`, we have to add it to the list of commands. Modify the **InitCommands** method in **Commands.vb** as follows:

```vbnet
Private Sub InitCommands()
    m_commandTable = New Dictionary(Of String, CommandParser)

    ' Add commands here
    AddCommand("print", AddressOf ParsePrintCommand)
    AddCommand("rem", AddressOf ParseRemCommand)
    AddCommand("comment", AddressOf ParseCommentCommand)
    AddCommand("end", AddressOf ParseEndCommand)
    AddCommand("dim", AddressOf ParseDimCommand)
    AddCommand("var", AddressOf ParseDimCommand)
    AddCommand("if", AddressOf ParseIfCommand)
End Sub
```

Adding a command simply means a call to **AddCommand** from the **InitCommands** method. From now on, this particular change will not be shown in the text again. We will mention that the command has been added.

## Testing the If Block

Compile and run. Test with the following:

```sic
Dim myName As String = "Raj"

If myName = "Raj" Then
    print "Hello, " & myName
    print "Nice name."
End If

If myName<>"Raj"
    print "Not so nice."
End If

REM Nested if statements also work
int i:=30
if i>15
    print "More than 15"
    if i<50 then
        print "but less than 50"
    end if
end if
```

The block infrastructure that we have already written can take care of arbitrary levels of nesting. Also, note that we did not have to write any code to process the `End If` command. The **ParseEndCommand** method takes care of it.

## Parsing the Else command

The `Else` command is slightly tricky. Let's examine how it changes things.

If the `If` expression evaluates to false, execution should continue at the first instruction after the Else statement. Which simply means that the jump after the boolean expression should now target the instruction after the `Else` statement.

What about the code that was executed if the If expression evaluated to true? For that code, execution should continue after the `End If` as soon as an `Else` statement is parsed. This means another jump, to a new end point label.

So to summarize, when we meet an `Else` statement, we need to generate a new label, emit a jump to that, then emit the old end point label, and then continue as usual.

Finally, an `Else` statement can appear only once in an `If` block. We will add a field, the Else flag, to keep track of this.

Add the following in **Commands.vb**, in the appropriate regions:

```vbnet
Private Function ParseElseCommand() As ParseStatus
    Dim result As ParseStatus

    SkipWhiteSpace()

    ' There should be nothing after Else on a line
    If Not EndOfLine Then
        result = CreateError(1, "end of statement")
    Else
        Dim currBlock As Block = m_BlockStack.CurrentBlock

        ' We should be in an If block, and the Else flag should
        ' not be set
        If currBlock Is Nothing _
                OrElse _
            currBlock.BlockType <> "if" _
                OrElse _
            m_ElseFlag Then

            result = CreateError(6, "Else")
        Else
            ' The end point is the same as the start point
            ' This means that only an If statement has been
            ' parsed before. Generate a new end point
            If currBlock.EndPoint = currBlock.StartPoint Then
                currBlock.EndPoint = m_Gen.DeclareLabel()
            End If

            ' Emit a jump to the end point
            ' Because a True If condition should never cause
            ' code in the Else block to be executed
            m_Gen.EmitBranch(currBlock.EndPoint)

            ' Emit the "start" point (of the Else block now)
            ' because a False If condition should cause
            ' execution to continue after the Else command.
            m_Gen.EmitLabel(currBlock.StartPoint)

            ' Set the Else flag
            m_ElseFlag = True

            result = CreateError(0, "Ok")
        End If
    End If
    
    Return result
End Function
```

Exactly how we described it earlier. Just one tricky bit here. Notice where we check whether the block EndPoint is the same as the StartPoint? The way we have handled the `If` command, they will always be the same, right? So why this check? We'll answer this question in the section immediately following this one.

Also, note the new error value of 6.

Now, we need to modify **ParseIfCommand** such that the Else flag is properly updated. Each time an `If` block begins, we need to save the old value of the Else flag, and set it to False. Notice that a successful **ParseElseCommand** sets it to True. When the `If` block ends, we need to restore the old value. This approach is required because `If` statements can be nested.

Modify **ParseIfCommand** in **Commands.vb** as follows:

```vbnet
Public Function ParseIfCommand() As ParseStatus
    Dim result As ParseStatus

    SkipWhiteSpace()
    result = ParseBooleanExpression()

    If result.Code=0 Then
        SkipWhiteSpace()

        If Not EndOfLine Then
            ' Try to read "then"
            ScanName()
            If CurrentToken.ToLowerInvariant<>"then" Then
                result = CreateError(1, "then")
            Else
                ' There shouldn't be anything after "then"
                SkipWhiteSpace()
                If Not EndOfLine Then
                    result = CreateError(1, "end of statement")
                End If
            End If
        End If

        If result.Code=0 Then
            ' Store old value of Else flag for nesting
            Dim oldelseflag As Boolean = m_ElseFlag
            ' ElseFlag should be false 
            ' at start of a new If block
            m_ElseFlag = False


            Dim endpoint As Integer = m_Gen.DeclareLabel()

            ' If the condition just emitted is false, emit jump
            ' to endpoint
            m_Gen.EmitBranchIfFalse(endpoint)

            ' Parse the "If" block
            Dim ifblock As Block = New Block( _
                                "if", _
                                endpoint, _
                                endpoint
            )

            result = ParseBlock(ifblock)

            ' If the block was successfully parsed, emit
            ' the endpoint label, and restore saved else
            ' flag for nesting
            If result.Code = 0  Then
                m_Gen.EmitLabel(ifblock.EndPoint)
                m_ElseFlag = oldelseflag
            End If
        End If
    End If

    Return result
End Function
```

Next, **CreateError** needs to be modified to handle the new error code, 6. Make the change im **Parser.vb**, as follows:

```vbnet
Private Function CreateError( _
    ByVal errorcode As Integer, _
    ByVal errorDescription As String _
    ) As ParseStatus

    Dim result As ParseStatus
    Dim message As String

    Dim errorpos As Integer
    ' Most errors happen
    ' at the scan position
    errorpos = m_CharPos + 1

    Select Case errorcode
        Case -1 ' Block finished
            message = ""
        Case 0  ' All good
            message = "Ok."
        Case 1  ' Expected something
            message = String.Format( _
                        "Expected {0}", _
                        errorDescription _
            )

        Case 2  ' Not in block
            message = errorDescription

            ' Not in block error happens
            ' after End command has been
            ' scanned
            errorpos = errorpos - TokenLength
        Case 3  ' Variable already declared
            message = String.Format( _
                        "Cannot redeclare variable '{0}'.", _
                        errorDescription
            )

            ' Error happens after scanning
            ' variable name
            errorpos = errorpos - TokenLength
        Case 4  ' Variable not declared
            message = String.Format( _
                        "Variable '{0}' not declared.", _
                        errorDescription
            )
            ' Error happens after scanning
            ' variable name
            errorpos = errorpos - TokenLength
        Case 5  ' Variable type mismatch
            message = String.Format( _
                        "Type mismatch for Variable '{0}'.", _
                        errorDescription
            )
            ' Error happens after scanning
            ' variable name
            errorpos = errorpos - TokenLength
        Case 6  ' Unexpected token
            message = String.Format( _
                        "'{0}' was unexpected at this time.", _
                        errorDescription
            )
            ' Error happens after scanning
            ' unexpected token
            errorpos = errorpos - TokenLength
        Case Else
            message = "Unknown error."
    End Select

    result = New ParseStatus(errorcode, _
                message, _
                errorpos, _
                m_linePos)


    Return result
End Function
```

**CreateError** is getting unwieldy. We should refactor it first chance we get. We will. For now, leave it as it is and add `Else` to the list of commands. You know how.

Compile and run. Test with the following:

```sic
Int i:=30

If i<30 Then
    Print "Less than 30"
Else
    If i>30
        Print "More than thirty"
    Else
        Print "Thirty!"
    End If
End If
```

## Parsing the ElseIf Command

Take a look at the test code above. If the first condition is false, we perform another test. This is a common enough situation in programming to merit its own statement. The `ElseIf` statement allows us to check multiple conditions in a single logical `If` block.

How does `ElseIf` work? If the boolean expression after an `If` statement evaluates to false, a jump is emitted. In this case, that jump  must target the boolean expression after the `ElseIf`. Also, the moment we hit an `ElseIf`, we must emit a jump which skips the entire `ElseIf` block. Very similar to what we did in the case of the `Else` statement.

The difference is this: there may be any number of `ElseIf` statements in a single `If` block. This will affect our jump logic as follows:

When an `If` block starts, the block's end point targets the end of the statements following the `If` - same as the start point. If we hit an `End If` without encountering an `ElseIf` or an `Else`, the targeting remains accurate. The first time we hit an `ElseIf`, we have to change the block's end point, because the end of the logical `If` block has just moved. For subsequent ElseIf statements, _we cannot change the end point again_. In fact, if there is as `Else` statement following the `ElseIf` statements, that cannot change the ending point either. 

Finally, an `ElseIf` statement cannot follow an `Else` statement.

Fortunately, we can handle all of these cases with the code we have already written. Add the following to **Commands.vb**, in the appropriate region:

```vbnet
Private Function ParseElseIfCommand As ParseStatus
    Dim result As ParseStatus

    Dim currBlock As Block = m_BlockStack.CurrentBlock
    
    ' The ElseIf command can only be in an If block, and the
    ' Else flag should not be set
    If currBlock Is Nothing _
            OrElse _
        currBlock.BlockType <> "if" _
            OrElse _
        m_ElseFlag Then

        result = CreateError(6, "ElseIf")
    Else
    
        ' If the endpoint is the same as the startpoint, this
        ' is the first ElseIf. Generate new endpoint. This 
        ' will mark the end of the If block.
        If currBlock.EndPoint = currBlock.StartPoint Then
            currBlock.EndPoint = m_Gen.DeclareLabel()
        End If

        ' Emit jump to the endpoint, because the ElseIf condition 
        ' and block should not be processed if the If condition 
        ' was true.
        m_Gen.EmitBranch(currBlock.EndPoint)

        ' Emit the "start" point. This marks the start of the
        ' ElseIf block
        m_Gen.EmitLabel(currBlock.StartPoint)

        SkipWhiteSpace()
        ' Parse the ElseIf condition
        result = ParseBooleanExpression()

        ' If successful
        If result.Code = 0 Then

            ' "Eat" the optional "Then"
            SkipWhiteSpace()

            If Not EndOfLine Then
                ' Try to read "then"
                ScanName()
                If CurrentToken.ToLowerInvariant<>"then" Then
                    result = CreateError(1, "then")
                Else
                    ' There shouldn't be anything after "then"
                    SkipWhiteSpace()
                    If Not EndOfLine Then
                        result = CreateError(1, "end of statement")
                    End If
                End If
            End If

            If result.Code = 0 Then        
                ' Generate new "start" point. This will mark
                ' the next elseif statement, or an else statement
                currBlock.StartPoint = m_Gen.DeclareLabel()

                ' If the condition is FALSE, jump to the 
                ' "start" point
                m_Gen.EmitBranchIfFalse(currBlock.StartPoint)
            End If
        End If
    End If

    Return result
End Function
```

When we meet an `ElseIf` command for the first time, the StartPoint and EndPoint of the current block are the same (we set them that way in **ParseIfCommand**). In this case, we set a new EndPoint and emit a jump to it. This new EndPoint will, from now on, represent the point after the `End If` statement. We then emit the old StartPoint, which is the target of the **BrFalse** opcode generated after the `If` condition. Then, we  proceed to parse and emit code for the `ElseIf` condition. The effect is that if the `If` condition evaluates to false, the `ElseIf` condition is checked immediately. 

Then, we generate a new label, store it in the block's StartPoint property, and emit a **BrFalse** which targets this new StartPoint. This will be the location that will be targeted if the `ElseIf` condition evaluates to false. This may point to the next `ElseIf` condition, an `Else` block, or the instruction
after the `End If`. We don't know yet.

What happens if we meet another `ElseIf`? At this point, the block's StartPoint and EndPoint are not the same, so we do not generate a new EndPoint. Instead, we emit a jump to the EndPoint that has already been set. Then, we emit the old StartPoint, and so on. This means that if the condition after the first `ElseIf` evaluates to false, the second `ElseIf` condition will be checked immediately. This may repeat any
number of times.

What about when we finally meet an `Else`? Our **ParseElseCommand** method already takes care of the fact that if the block's StartPoint and EndPoint are different, a new EndPoint is not generated. It instead emits a jump to the pre-set EndPoint, then emits the StartPoint, then proceeds to parse and emit code for the `Else` block. As a result, if the last `ElseIf` condition evaluates to false, the statement immediately after the `Else` statement will be processed, thus completing our picture.

The code that "eats" the optional `Then` has been repeated from **ParseIfCommand**. We really need to do some refactoring, soon.

There is one remaining problem, though. Suppose we have an `If`, followed by an `ElseIf`, and then another `ElseIf`, and then an `End If`, but no `Else`? What will happen?

The first `If` will generate a StartPoint which will be the same as the EndPoint. The first `ElseIf`
will generate a new EndPoint, use the old StartPoint, and generate a new StartPoint. The second `ElseIf` will use the old EndPoint, use the old StartPoint, and generate a new StartPoint. Then the `If` block will end, and while doing so, use the EndPoint. What happens to the StartPoint generated by the last `ElseIf`? If there was an `Else`, it would have been taken care of. Since the `Else` is optional, we need to take care of it.

To summarize, when an `If` block ends, if an `ElseIf` statement has been processed (StartPoint is different from EndPoint) and an `Else` statement has not been processed (m\_ElseFlag is false), we need to emit the dangling StartPoint, _before_ emitting the EndPoint. Let's do that now. Modify **ParseIfCommand** in **Commands.vb**, as follows:

```vbnet
Public Function ParseIfCommand() As ParseStatus
    Dim result As ParseStatus

    SkipWhiteSpace()
    result = ParseBooleanExpression()

    If result.Code=0 Then
        SkipWhiteSpace()

        If Not EndOfLine Then
            ' Try to read "then"
            ScanName()
            If CurrentToken.ToLowerInvariant<>"then" Then
                result = CreateError(1, "then")
            Else
                ' There shouldn't be anything after "then"
                SkipWhiteSpace()
                If Not EndOfLine Then
                    result = CreateError(1, "end of statement")
                End If
            End If
        End If

        If result.Code=0 Then
            ' Store old value of Else flag for nesting
            Dim oldelseflag As Boolean = m_ElseFlag
            ' ElseFlag should be false 
            ' at start of a new If block
            m_ElseFlag = False


            Dim endpoint As Integer = m_Gen.DeclareLabel()

            ' If the condition just emitted is false, emit jump
            ' to endpoint
            m_Gen.EmitBranchIfFalse(endpoint)

            ' Parse the "If" block
            Dim ifblock As Block = New Block( _
                                "if", _
                                endpoint, _
                                endpoint
            )

            result = ParseBlock(ifblock)

            ' If the block was successfully parsed, emit
            ' the endpoint label, and restore saved else
            ' flag for nesting
            If result.Code = 0  Then

                ' If there is a dangling StartPoint, emit
                ' it first
                If ifblock.StartPoint<>ifblock.EndPoint _
                        AndAlso _
                    Not m_ElseFlag Then

                    m_Gen.EmitLabel(ifblock.StartPoint)
                End If

                m_Gen.EmitLabel(ifblock.EndPoint)
                m_ElseFlag = oldelseflag
            End If
        End If
    End If

    Return result
End Function
```
Why do this at all? Why not just ignore the dangling label that we generated? Well, the Reflection Emit library, in specific, the TypeBuilder class does not permit this. Every label defined using **ILGenerator.DefineLabel** must have a corresponding **ILGenerator.MarkLabel**. I did not find this fact in the official documentation of the Reflection.Emit namespace.

Finally, add the `ElseIf` command to the list of valid commands. You know how.

Compile and run. Test with the following:

```sic
int i:=22

if i>30
    print "more than 30"
elseif i>15 then
    print "more than 15 but less than 31"
elseif i>10
    print "more than 10 but less than 16"
elseif i<5
    print "less than 5"
else
    print "between 6 and 10 inclusive"
end if

print "Done"
```

Try with any combination of `If`, `ElseIf` and `Else`. Valid ones will work as expected. Invalid combinations should produce an accurate error message.

## Parsing the While Command

All right, let's proceed to the `While` loop. Here's the simplest form:

```pseudocode
While <boolean expression>
    <block>
End While
```

Or in BNF:

```bnf
<whilecommand>  ::= "while" <booleanexpression>
                        <block>
                    <endcommand>
```

All we have to do is generate and emit a label (the startpoint) _before_ parsing the boolean expression, and emit a jump to that when the block ends. We also need to generate a label for the end of the loop (the endpoint). We will emit a jump to that if the boolean expression evaluates to false. We will emit the endpoint label itself when the block ends, after the jump to the startpoint.

Add the following to **Commands.vb**:

```vbnet
Private Function ParseWhileCommand() As ParseStatus
    Dim result As ParseStatus
            
    SkipWhiteSpace()
    If EndOfLine Then
        result = CreateError(1, "a boolean expression")
    Else
        ' Generate and emit the startpoint
        Dim startpoint As Integer = m_Gen.DeclareLabel()
        m_Gen.EmitLabel(startpoint)

        ' Parse the Boolean expression
        result = ParseBooleanExpression()

        If result.Code = 0 Then
            ' There should be nothing else on the line
            If Not EndOfLine Then
                result = CreateError(1, "end of statement")
            Else
                ' Generate endpoint, and emit a conditional
                ' jump to it
                Dim endpoint As Integer = m_Gen.DeclareLabel()
                m_Gen.EmitBranchIfFalse(endpoint)

                ' Parse the block
                Dim whileblock As New Block( _
                        "while", _
                        startpoint, _
                        endpoint _
                )
                result = ParseBlock(whileblock)

                If result.Code = 0 Then
                    ' Emit jump back to startpoint
                    m_Gen.EmitBranch(whileblock.StartPoint)

                    ' Emit endpoint
                    m_Gen.EmitLabel(whileblock.EndPoint)
                End If
            End If
        End If
    End If

    Return result
End Function
```

Easy, wasn't it? To test this, add `While` to the list of commands, compile and run. Test it with this famous program:

```sic
Int i:=99

While i>0
    Print i
    Print "bottles of beer on the wall,"
    Print i
    Print "bottles of beer."
    Print "Take one down and pass it around,"

    If i>1 Then
        Print i-1
        Print "bottles of beer on the wall."
    Else
        Print "No more bottles of beer on the wall."
    End If

    i = i - 1

    Print ""
End While

Print "No more bottles of beer on the wall."
Print "No more bottles of beer..."
Print "Go to the store and buy some more..."
Print "99 bottles of beer."
```

The SIC language could do with a way of combining strings and numbers in a single `Print` statement, but other than that, things should work fine.

Next, try this:

```sic
var i int

while i<5
    if i<2 then
        i = i + 2
    else
        i = i + 1
    end while
end if
```

You should get an accurate error message. Our block infrastructure takes care of the fact that blocks need to be nested properly.

## Parsing the Exit While and Continue While Commands

As described in the Goal section of this chapter, the `Exit While` statement will cause an immediate exit from the `While` block, and the `Continue While` will cause a jump to the start of the `While` block, which means that the condition will be re-evaluated.

At first glance, they seem simple to implement. For `Exit While`, just emit a jump to the endpoint of the current block. For `Continue While`, emit a jump to the startpoint. Assuming that these statements are only valid inside a `While` block, that's quite simple to implement, right?

Well, there is one complication. In practical use, an `Exit While` or a `Continue While` will always be conditional (think about this for a minute). And conditional, in the SIC language thus far, means an `If` block. See the problem? When we meet an `Exit While` or a `Continue While`, the current block will be an `If block`, and thus, we do not have access to the `While` block's startpoint or endpoint.

This is why our BlockStack class has a method called **GetClosestOuterBlock**. Given a block type, it will walk the stack searching for a matching block, starting from the current block and going down. If one is found, it will be returned, and then we can use that block's start and end points.

Let's think ahead a little bit. If we create more loop constructs, we will need `Exit` and `Continue` structures for them too. And the logic would be the same. So let's build these commands in a generic fashion.

First, a little plumbing. Add the following to **Commands.vb**, in the "Fields" and "Helper Functions" regions as appropriate:

```vbnet
Private m_loopTable As List(Of String)

Private Sub AddLoop(loopName As String)
    m_loopTable.Add(loopName)
End Sub

Private Function IsValidLoop(loopName As String) As Boolean
    Return m_loopTable.Contains(loopName)
End Function

Private Sub InitLoops()
    m_loopTable = New List(Of String)

    ' Add loops here
    AddLoop("while")
End Sub
```

This sets up a lookup table for loops, same as commands and types. We need to call the **InitLoops** method from the constructor of the Parser class. Modify it in **Parser.vb** as follows:

```vbnet
Public Sub New( _
    ByVal newStream As TextReader, _
    ByVal newGen As CodeGen _
    )

    m_InputStream = newStream
    m_Gen = newGen

    InitTypes()
    InitLoops()
    InitCommands()
End Sub
```

With that support system in place, we can now write the commands. Add the following to **Commands.vb**:

```vbnet
Private Function ParseLoopControlCommand() As ParseStatus
    Dim result As ParseStatus

    ' The current token is either Exit or Continue
    Dim cmdName As String = CurrentToken.ToLowerInvariant()

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
            result = CreateError(0, "Ok")
            
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

    Return result
End Function
```

This time, we have written a single parser that will cause slightly different results depending on the command being parsed. We do this because the only difference between `Exit` and `Continue` is the point to emit the jump to. The logic, though, is pretty straightforward. The checks made here will also prevent us from compiling a statement like `Exit If`!

We will hook up this common parser to _both_ `Exit` and `Continue` commands by modifying **InitCommands**. Make the change in **Commands.vb** as follows:

```vbnet
Private Sub InitCommands()
    m_commandTable = New Dictionary(Of String, CommandParser)

    ' Add commands here
    AddCommand("print",     AddressOf ParsePrintCommand)
    AddCommand("rem",       AddressOf ParseRemCommand)
    AddCommand("comment",   AddressOf ParseCommentCommand)
    AddCommand("end",       AddressOf ParseEndCommand)
    AddCommand("dim",       AddressOf ParseDimCommand)
    AddCommand("var",       AddressOf ParseDimCommand)
    AddCommand("if",        AddressOf ParseIfCommand)
    AddCommand("else",      AddressOf ParseElseCommand)
    AddCommand("elseif",    AddressOf ParseElseIfCommand)
    AddCommand("while",     AddressOf ParseWhileCommand)
    AddCommand("exit",      AddressOf ParseLoopControlCommand)
    AddCommand("continue",  AddressOf ParseLoopControlCommand)
End Sub
```

## Testing the While Loop

Compile and run. Test with this:

```sic
int i:=1

while i<=10

    if [i==3] then
        i:=i+2
        continue while
    elseif [i=8]
        exit while
    end if

    print i
    i := i+1
end while

print "Done"
```

and then with this:

```sic
int i:= 30

while i>1
    int j:=10

    print i
    print "-------"

    while j>0
        if j == 5
            j = j - 5
            continue while
        end if

        print j
        j = j - 1
    end while

    print "======="

    i = i / 15
end while
```

Try introducing errors, such as putting an `Exit While` or a `Continue While` outside a `While` loop. As usual, an accurate error message will be produced by the compiler.

## Conclusion

To quote Dr. Jack Crenshaw, "We could stop right here, and have a language that works." Our two constructs, `If` and `While`, are enough to take care of any iteration and selection cases that are required in a language like ours. However, most languages provide some more constructs, and so will we. In the next chapter, we will look at some more loop and branch constructs. See you then.

---
[^1] This is a simplification. The offset is actually calculated in bytes, starting from the start of the instruction following the jump instruction itself. To properly calculate the offset, we need to know how many bytes are taken up by each instruction and its parameters. Fortunately, because we use the Reflection Emit library to generate CIL, we don't have to calculate the offset ourselves.