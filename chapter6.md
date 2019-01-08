# Statements

## Introduction

So far, we have created a "language" which recognizes three types of expressions:
integer, string and Boolean. Our source consists of one or more lines, where each line contains exactly one of these expressions. The application we compile simply shows the results of these expressions on the screen. It is now time to expand our "language" to do more.

## Kinds of languages

Computer programming languages can be classified by _paradigms_. The programming paradigm that most of us are familiar with is called _imperative_. Imperative programming languages consist of instructions (also called _commands_ or _statements_) which are given in order, and are expected to be carried out in the same order. Each statement affects the state of the underlying machine, such that each command can depend on the results of previous commands. For example, if a statement sets the value of a variable, subsequent statements can depend on the value remaining set.

Within the imperative paradigm, there is a more focussed paradigm called _structured programming_. Structured programming languages aim to make improve program clarity and quality by organizing of _structuring_ groups of instructions using _blocks_, and controlling their order of execution using _control flow constructs_ such as _selection (if/then/else) and _iteration_ (while, repeat, for).

 Languages can be further classified into even more focused paradigms such _procedural_ and _object oriented_. Also, there is an entire other tree of non-imperative paradigms, starting with _declarative programming_, and specializing further into _functional programming_, _logic programming_ and so on. But let's not get into that discussion now.

Most people are used to the imperative paradigm, and more specifically structured programmaing. Thus, our language (called SIC, remember?) will also be a structured, imperative one.

## Making a statement

Any imperative language consists of _statements_, which are instructions understood by
that language. These instructions cause some kind of action to occur. There are usually
three kinds of statements defined by common languages:

* **Expression statements.** These simply cause an expression to be evaluated. Nothing happens with the final value per se, unless the language defines some action as part of expression evaluation. Many languages use this kind of statement to perform assignment of values to variables, and to handle operators such as ++ and --, which change the value of the variable they are applied to. Currently, this is the only kind of statement our compiler understands.
* **Simple statements.** These usually consist of a single instruction combined with data in the form of expressions. The instruction causes some kind of action to be performed on or with the data. Examples are the `PRINT` statement of BASIC, which causes the expression after it to be displayed on screen, or the `LET` statement, which expects a variable, followed by an assignment operator,followed by an expression, and assigns the value of the expression to the variable. The last example shows that a language can consider expression statements to be a special case of simple statement.
* **Compound statements.** These are a group of simple statements, or a combination of simple and compound statements. A common term for compound statements is _block_. A block is useful when a set of statements have to be executed as a sequential whole, perhaps when some condition is met. In a way, a complete program is nothing but a block.

All three (or two, if you consider expression statements as a special case of simple statement) kinds of statements have a defined beginning and end. The beginning is  straightforward; a statement begins when the one before it ends. What about the end?

Different languages take different approaches. The two most common approaches are:

* The end of the line is the end of the statement. When the scanner meets a carriage return/line feed, the line and the statement are supposed to have ended.

* A special end-of-statement symbol, usually the semicolon (;), is used. Thus, a statement can span multiple physical lines.

Modern compilers are clever enough to go a step further: a statement itself decides when it ends, which could well be several physical lines after it began.

What about compound statements? Once again, there are two common approaches:

* Some kind of block begin and block end symbols, such as the (hated – by me and others) "curly brackets" `{` and `}`, or the words `BEGIN` and `END`.

* "Block instructions", which are instructions which can cause a block of statements to be executed. These instructions usually have a matching "end" instruction to indicate the end of the block. Examples are the `If-End If` and `While-End While` pairs of the Basic language.

These days, some languages (notably Python) use indentation for block structuring. A block begins with block instruction, but all other instructions in the block are indented further than the starting one.

To keep matters Basically simple (yes, that was a lame pun), I hereby declare that:

* SIC statements will terminate at the end of each line.
* Sic will use matched instruction-end instruction pairs for blocks.

## Goal

Our goal in this chapter is to extend our compiler to understand simple and compound
statements. We will no longer simply evaluate expressions, but only do so if the
statement requires it. Thus, we are not going to use pure expression statements (as of
now).

To begin with, we will compile two examples of simple statements, and one example of a
compound statement. The statements we will compile are:

|Statement|What it does|
|---|---|
|`Print`|This statement will do what we have been doing all along; it will print expressions on screen. It will expect an expression after itself, and print the value of that expression.|
|`REM`|This statement will ignore the rest of the line after itself.|
|`Comment`...`End Comment`|This pair of instructions will demonstrate compound statements. All lines between a `Comment` statement and its matching `End Comment` will be ignored by the compiler.|

To keep things Basically simple (yes, yes), these statements will be case-insensitve. For example, `Print` can be written as `print` or `PRINT` or even `pRiNt`.

## The Approach

As always, we will approach the problem one step at a time. We will begin by introducing the concept of simple statements to our parser. Once done, we will move on to compound statements.

How does a statement compile down into CIL? Well, most statements will not have any direct CIL equivalents: they will compile down to a series of CIL instructions. The `Print` statement, for example, will compile to a series of CIL instructions which evaluate its expression, and then call the `WriteLine` appropriate to the expression's type.

Sounds like we got all the CodeGen-level plumbing ready, right? So, on to scanning and parsing.

## First Step

What does a statement look like? Usually it is a word, in English or an approximation thereof, that is not decorated by any delimiters. We don't have a scanner method which can read that yet, so let's create one. Our scanner method (and associated recognizer method) will read a new kind of token called a _name_. We will use names as statements, and also for some more purposes. So, let us define some rules for names.

* Names may consist of letters, digits and the underscore (_) character.
* Names must begin with a letter or an underscore, and NOT with a digit. Can anyone guess why most languages do not allow names starting with digits?
* Names are NOT case-sensitive.

Here's the BNF:

```bnf
<name>           ::= <letter><namecharacter>+
<namecharacter>  ::= <letter>|<digit>|"_"
<letter>         ::= ? a letter in any language ?
<digit>          ::= "0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"
```

Given these rules, we can create a scanner method for names pretty easily. Add the
following to the appropriate parts of **Parser.vb**

```vbnet
Private Function IsNameCharacter(ByVal c As Char) As Boolean
    Dim result As Boolean = False
    If Char.IsDigit(c) AndAlso _
                Tokenlength > 0 Then
        ' Digits allowed after start of name
        result = True
    ElseIf c.Equals("_"c) Then
        result = True
    ElseIf Char.IsLetter(c) Then
        result = True
    End If
    Return result
End Function

Public Sub ScanName()
    m_CurrentTokenBldr = New StringBuilder
    Do While IsNameCharacter(LookAhead)
        m_CurrentTokenBldr.Append(LookAhead)
        m_CharPos += 1
        If EndOfLine Then
            Exit Do
        End If
    Loop
End Sub
```

What about a parser method? Names will be used for different purposes in different contexts. So, we won't write a `ParseName` method. Instead, we will call `ScanName` as appropriate from various parse methods.

But before that, we need to reorganize a bit. Our **Parser.vb** file is getting rather large (900+ lines). And as we start adding more functionality, it will get harder to keep track of what is what. So let's see what we can do about it.

## Rocking the cradle

Unfortunately, we decided early on to keep scanning and parsing tightly coupled in our single Parser class. This certainly makes it easy to explain concepts, but makes it very hard to split functionality cleanly. We won't change that decision now. Instead, we will use the _partial class_ feature of Visual Basic to split the code for our Parser class across multiple files.

To start with, change the declaration of the Parser class, in **Parser.vb**, as follows:

```vbnet
Public Partial Class Parser
```

Next, create a new file called **Commands.vb**. Put the following code in it:

```vbnet
Option Strict On
Option Explicit On

Imports System.Collections.Generic

Public Partial Class Parser

#Region "Fields"
    Private Delegate Function CommandParser() As ParseStatus
    Private m_commandTable As Dictionary(Of String, CommandParser)
#End Region

#Region "Helper Functions"
    Private Sub AddCommand( _
                commandName As String, _
                commandParser As CommandParser _
            )
        m_commandTable.Add( _
            commandName.ToLowerInvariant(), _
            commandParser _
        )
    End Sub

    Private Function IsValidCommand(commandName As String) As Boolean
        Return m_commandTable.ContainsKey(commandName.ToLowerInvariant())
    End Function

    Private Sub InitCommands()
        m_commandTable = New Dictionary(Of String, CommandParser)

        ' Add commands here

    End Sub
#End Region

#Region "Commands"

#End Region

End Class
```

We will put the parser functions for statements in this file. We use the word 'command' as a synonym for 'statement' from this point forward.

Finally, rename **ParseStatus.vb** to **Utilities.vb**. We will need some more "utility" classes shortly - let's keep them all in one code file.

## Parsing Commands

The introduction of statements, or _commands_ as we will call them from now on, changes how our parser works at the line level itself. So far, a line meant an expression. Now, a line means a command.

Any command is basically a name. Once a name has been scanned, we can look it up against a list of valid commands. If there is a match, we can call a dedicated parser method for that command. If there is no match, we return an error.

Let's talk about implementing the `Print` command discussed above, which expects an expression and prints it. We already know how to parse an expression and print it, so writing the parser method should be simple.

The `REM` command is even simpler. It ignores everything after it on a line, so the parser method would have to simply skip the rest of the line, and return a successful parse status.

In both cases, we would have to add the command name to the list of valid commands.

Here's the BNF:

```bnf
<line>          ::= <command>
<command>  ::= <remcommand>|<printcommand>
<remcommand>    ::= "rem" <restofline>
<restofline>    ::= ? Anything. ?
<printcommand>  ::= "print" <exression>
```

We have put some boilerplate code in **Commands.vb** that will take care of maintaining a list of valid commands, and validating against it. Using it, we can translate the BNF easily. Let's go bottom up. First, the parser method for the `Print` command. Add it to the "Commands" region in **Commands.vb**.

```vbnet
Private Function ParsePrintCommand() As ParseStatus
    Dim result As ParseStatus

    SkipWhiteSpace()

    result = ParseExpression()
    If result.code = 0 Then
        If m_LastTypeProcessed.Equals( _
                                Type.GetType("System.Int32") _
                            ) Then
            m_Gen.EmitWriteLine()
        ElseIf m_LastTypeProcessed.Equals( _
                                    Type.GetType("System.String") _
                            ) Then
            m_Gen.EmitWriteLineString()
        ElseIf m_LastTypeProcessed.Equals( _
                                    Type.GetType("System.Boolean") _
                            ) Then
            m_Gen.EmitWriteLineBoolean()
        End If

        If Not EndOfLine Then
            result = CreateError(1, "end of statement.")
        End If
    End If

    Return result
End Function
```

As you can see, this is essentially the old `ParseLine` method. Now, the `REM` command. Put it in the same place.

```vbnet
Private Function ParseRemCommand() As ParseStatus
    ' Ignore the rest of the line
    m_CharPos = m_LineLength
    Return CreateError(0, "Ok")
End Function
```

This simply moves the scanner to the end of the line.

Next, we need to add these to the list of valid commands. In **Commands.vb**, we have put a method called `InitCommands`. At the end of that method, in the space marked by a comment, add the following two lines:

```vbnet
AddCommand("print", AddressOf ParsePrintCommand)
AddCommand("rem", AddressOf ParseRemCommand)
```

Now, who calls these? According to the BNF, we need a parser method for \<command\>. Add the following to **Parser.vb**. This, the top level of command parsing, we will retain there.

```vbnet
Private Function ParseCommand() As ParseStatus
    Dim result As ParseStatus

    If TokenLength = 0 Then
        result = CreateError(1, "a valid command")
    Else
        Dim commandname As String = CurrentToken.ToLower()

        If Not IsValidCommand(commandname) Then
            result = CreateError(1, "a valid command")
        Else
            Dim parser as CommandParser = _
                    m_commandTable(commandname)

            result = parser()
        End If
    End If

    Return result
End Function
```

Notice how `ParseCommand` expects a command in `CurrentToken`. This means someone should have called the appropriate scanner method before calling `ParseCommand`. As per the BNF above, this should be the Line parser.

## A New Line

As in just about every chapter, we once again have to modify `ParseLine`. Here's the new definition. Replace it in **Parser.vb**.

```vbnet
Private Function ParseLine() As ParseStatus
    Dim result As ParseStatus

    SkipWhiteSpace()

    m_LastTypeProcessed = Nothing

    If Not EndOfLine() Then
        ScanName()
        result = ParseCommand()
    Else
        result = CreateError(0, "Ok")
    End If

    Return result
End Function
```

We also take this opportunity to make a small addition: an empty line is syntactically
valid. And that is that.

Finally, we need to call the `InitCommands` method, which sets up the valid command table, from somewhere. We will call it from the constructor of the Parser class. Make the change in **Parser.vb**.

```vbnet
Public Sub New( _
    ByVal newStream As TextReader, _
    ByVal newGen As CodeGen _
    )

    m_InputStream = newStream
    m_Gen = newGen

    InitCommands()
End Sub
```

## Check it out

Time to compile. We have made changes to the cradle, so the process is a little different this time. You may want to review the instructions in the [Development Environment](/the-development-environment.md) chapter.

Compile with:

```bash
vbc /out:sicc.exe Compiler.vb Parser.vb Commands.vb CodeGen.vb Utilities.vb
```

Run using:

```bash
sicc.exe
```

Note that we are no longer calling our compiler executable **Compiler.exe**. Since we have finally started to define the SIC language proper, we will call it **sicc.exe** (for **SIC** **C**ompiler) from now on.

Test it with the following code:

```sic
REM This is a simple program
Print "Hello, World"

Print 2+4/2-1
Print 2=2 And 4=1
```

It should work as expected. If we make a mistake, our compiler will point it out.

## Building Blocks

Now that the basic framework for simple statements is in place, we can start thinking of
compound statements.

As discussed earlier, compound statements are simply a set of simple statements, which need to be considered together as a whole. For example, we want to execute one set of statements if a condition is true, and another set if it isn't. Compound statements are also called _blocks_, and that is the term we will use hereafter.

As we decided earlier, a block will begin when a "block instruction" (which is a
command which requires a block following it) is encountered, and will end when a
matching "end instruction" is encountered. Blocks can be nested, that is, one block can
contain another block.

To begin with, we will write some utility code for working with blocks. Add the
following new classes to **Utilities.vb**.

```vbnet
Public Class Block
    Private m_Blocktype As String
    Private m_StartPoint As Integer
    Private m_EndPoint As Integer

    Public ReadOnly Property BlockType() As String
        Get
            Return m_Blocktype
        End Get
    End Property

    Public Property StartPoint() As Integer
        Get
            Return m_StartPoint
        End Get
        Set(ByVal Value As Integer)
            m_StartPoint = Value
        End Set
    End Property

    Public Property EndPoint() As Integer
        Get
            Return m_EndPoint
        End Get
        Set(ByVal Value As Integer)
            m_EndPoint = Value
        End Set
    End Property

    Public Function IsOfType( _
                        blocktype as String _
                    ) As Boolean

        Return m_Blocktype = _
                    blocktype.ToLowerInvariant()

    End Function


    Public Sub New(ByVal blocktype As String, _
                    ByVal startpoint As Integer, _
                    ByVal endpoint As Integer)
        m_Blocktype = blocktype.ToLowerInvariant()
        m_StartPoint = startpoint
        m_EndPoint = endpoint
    End Sub
End Class

Public Class BlockStack
    Private m_stack As New Stack(Of Block)

    Public Sub Push(block as Block)
        m_stack.Push(block)
    End Sub

    Public Function Pop() As Block
        Return m_stack.Pop()
    End Function

    Public ReadOnly Property IsEmpty() As Boolean
        Get
            Return m_stack.Count = 0
        End Get
    End Property

    Public ReadOnly Property CurrentBlock() As Block
        Get
            Return If(IsEmpty, Nothing, m_stack.Peek())
        End Get
    End Property

    Public Function GetClosestOuterBlock( _
                        blocktype As String _
                    ) As Block

        Dim block As Block
        Dim result As Block = Nothing

        For Each block In m_stack
            If block.IsOfType(blocktype) Then
                result = block
                Exit For
            End If
        Next

        Return result
    End Function
End Class
```

The class called `Block` will represent a single block, specifying its type (such as IF, FOR
or COMMENT), and its start and end points. We will cover start and end points two chapters down the line.

The class called `BlockStack` is simply a collection of blocks. As the "stack" in the name suggests, this collection is "last in, first out". As we add a block to the stack, it becomes the topmost or "current" block. We also provide a method to find the "closest outer" block of a given type. For example, while inside a FOR block, we can find the last IF block. The search starts from the most recently added block, and goes backwards. This method will be used in later chapters.

Now, how do we actually parse a block - any block? A block is just a sequence of zero or more lines. In BNF, we can write than as:

```bnf
<block>      ::= <line>*
```

Since we already know how to parse lines, parsing a block should be easy. But beyond just the line parsing, we need to keep track of which block we are parsing, and whether it is inside another block. Let's do all that now. Type the following in **Parser.vb**, in the appropriate sections.

```vbnet
' Block stack
Private m_BlockStack As New BlockStack

Private Function ParseBlock(ByVal newblock As Block) _
                        As ParseStatus

    Dim result As ParseStatus
    m_BlockStack.Push(newblock)

    Do While ScanLine()
        result = ParseLine()
        If result.Code <> 0 Then
            Exit Do
        End If
    Loop

    ' Block will end when the result code returned
    ' is -1
    If result.Code = -1 Then
        result = CreateError(0, "Ok")
        m_BlockStack.Pop()
    End If

    Return result
End Function
```

The `ParseBlock` method scans one line at a time from the input stream, and calls `ParseLine` to process it. This is exactly what our top-level `Parse` method currently does. But we do two extra things here.

One, right at the start of parsing a block, we push the newly started Block object onto the block stack.

Two: notice the check for a `ParseStatus` code of -1? We haven't seen that before. We are hereby mandating that all "end-block" statements will return a special ParseStatus code value of –1, which indicates the ending of a block. The return code of –1 is not an error, so we "swallow" it, and remove the current block from the block stack.

We are now ready for dealing with blocks of all kinds. Whenever we want to start a block, we will create a `Block` object and call `ParseBlock`. Whenever we want to end one, we will return a `ParseStatus` with code -1.

## Comment...End Comment

The very first block instruction that we will implement is rather special. The `Comment` command will start the block, and the `End Comment` command will finish it. In between, any number of lines may be read, _but they should not be parsed unless they start with the End command_.

This means that the implementation will affect how we parse _all_ commands; in other words, we need to modify the `ParseCommand` method.

Add the following to **Commands.vb**, in the appropriate regions.

```vbnet
Private m_inCommentBlock As Boolean = False

Private Function ParseCommentCommand() As ParseStatus
    Dim result As ParseStatus

    SkipWhiteSpace()

    If Not EndOfLine Then
        result = CreateError(1, "end of statement")
    Else
        Dim newblock As Block
        Dim oldcommentstate As Boolean

        newblock = New Block("comment", -1, -1)

        oldcommentstate = m_inCommentBlock

        m_inCommentBlock = True
        result = ParseBlock(newblock)

        m_inCommentBlock = oldcommentstate
    End If

    Return result
End Function
```

The parser for the `Comment` command checks that line contains nothing other than the \<name\> "Comment". If so, it starts a new block of type "comment", sets a parser-level flag called `m_inCommentBlock` to `True`, and and calls `ParseBlock` to process it.

Notice how we are storing the current value of the flag before calling `ParseBlock`, and restoring it after `ParseBlock` returns. Can you guess why we are doing this?

Next, add the following to **Commands.vb**.

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
            result = CreateError(-1, "")
        Else
            ' Unless we are inside a comment block
            If m_inCommentBlock Then
                result = CreateError(0, "Ok")
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

Look carefully. This is not the parser for the `End Comment` command. Instead, this is a parser for the `End` command, which can end all kinds of blocks.

The `End` command is only valid if a block has already begun; in other works, the block stack has a current block. Furthermore, the token following it (such as `Comment`), should match the current block type. If it does, the block is legitimately finished, so we return a `ParseStatus` with a value of -1. If it doesn't, the end command is invalid, _unless we are inside a comment block_.

Notice that the `End` command parser calls `CreateError` with two new values: -1 for block end and 2 for the new "Not in block" error. Let's modify `CreateError` to take care of those. Make the change in **Parser.vb**.

```vbnet
Private Function CreateError( _
    ByVal errorcode As Integer, _
    ByVal errorDescription As String _
    ) As ParseStatus

    Dim result As ParseStatus
    Dim message As String

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
    End Select

    result = New ParseStatus(errorcode, _
                message, _
                m_CharPos + 1, _
                m_linePos)


    Return result
End Function
```

I don't like using magic numbers like -1, 1 and 2. We really should replace those with constants or an Enum. We will re-visit `CreateError` shortly, and clean up. For now, let's proceed.

## Hooking up the commands

All that remains is to add our `Comment` and `End` parsers to the list of valid commands, and ensure that they get called by the top-level `ParseCommand` method. The first part is easy. In **Commands.vb**, at the end of the `InitCommands` method, in the space marked by a comment, add the following two lines:

```vbnet
AddCommand("comment", AddressOf ParseCommentCommand)
AddCommand("end", AddressOf ParseEndCommand)
```

For any commands other than these two, this would have been enough. However, these two deserve special treatment, as they start and end a comment block. Since every line is supposed to start with a command, ParseCommand will get called for every line, and check if a command is valid. If we are inside a comment block, then the only command that ParseCommand should be looking for is the "End" command. Everything else should just be parsed as valid. This will save us the trouble of checking if we are inside a comment block in every command separately.

So, let's re-write ParseCommand to take care of this. Make the change in **Parser.vb**.

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
        ElseIf m_inCommentBlock Then
            ' Ignore rest of line
            m_CharPos = m_LineLength
            ' All is good in a comment block
            result = CreateError(0, "Ok")
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

## Are We There Yet?

One last check and we are done. Our parsing process is started off by the `Parse` method. When parsing is over, there should not be any dangling blocks left. In other words, we should not begin a block and not end it. In yet other words, when the `Parse` method finishes its work, the block stack should be empty. So, let us modify the `Parse` method accordingly. Make the change in **Parser.vb**.

```vbnet
Public Function Parse() As ParseStatus
    Dim result As ParseStatus

    Do While ScanLine()
        result = ParseLine()
        If result.Code <> 0 Then
            Exit Do
        End If
    Loop

    If result.Code = 0 AndAlso _
                (Not m_BlockStack.IsEmpty()) Then

        result = CreateError(1, _
                        "end " & _
                        m_BlockStack.CurrentBlock.BlockType _
        )
    End If

    Return result
End Function
```

## Testing

Let's test all this. Compile with:

```bash
vbc /out:sicc.exe Compiler.vb Parser.vb Commands.vb CodeGen.vb Utilities.vb
```

Run using:

```bash
sicc.exe
```

Test our compiler, first with this:

```sic
REM This is a simple program
Comment
  Print "Hello, World"
End Comment
Print 2+4/2-1
Print 2=2 And 4=1
```

and then with this:

```sic
REM This is a simple program

Comment
  Print "Hello, World"
  Comment
    Print 2+4/2-1
    Print 2=2 And 4=1
  End Comment
End Comment
```

Both should work. The second example should answer the question I asked when we wrote the `ParseCommentCommand` method.

Test with invalid programs as well, such as leaving out the `End Comment` or the `Comment`. Our compiler should accurately flag any errors that you make.

## Conclusion

In this chapter, we took the decision to make our compiler compile an imperative language, that is, one comprising of statements. We learned how to incorporate simple statements and compound statements into our parser, and have ended up with a compiler which understands four statements or commands: `Rem`, `Print`, and `Comment` and `End`. The `Print` command uses the expression parsers that we created previously. The real SIC language has now started taking shape.

In the next chapter, we will start working on that staple of all imperative programming languages: the variable.
