# Compiling Arithmetic Expressions

## Introduction

In the last chapter, we saw how we can create an executable file via the CLR's Reflection Emit library. That, as we have discussed, is the final task that a compiler does. There are some steps that come before it. In this chapter, we discuss these steps, and begin to implement them.

## The Goal

The goal for this chapter is to write a program that understands simple arithmetic expressions, and produces an executable. The produced executable should calculate and display the values of the expressions.

A typical arithmetic expression looks something like this:

```pseudocode
2+3*(4+2)
```

The value of the above expression should, of course, be 20. We know this, because BDMAS \(Bracket before Division and Multiplication before Addition and Subtraction\) has become a part of our thinking. A compiler does not know about the BDMAS rules. It's up to us to make it know.

## Why?

Why does just about every compiler-construction text begin with this task?

I’m not qualified to give a definitive answer, but here’s what I think: the process of translating arithmetic expressions effectively demonstrates most of the tasks necessary for building the core of a compiler. Consider:

* An expression consists of data \(2,3\) as well as instructions \(\*, +\), and these are interspersed. The compiler has to figure out which is the data and which is an instruction. Sometimes, it's not as easy as it looks.

* Some things have to be done before others, and we can't tell ahead of time what comes before what. The compiler has to figure this out, and ensure things happen in the correct order. The BDMAS rule is an example.

* There may be errors in the expression, which have to be dealt with. The compiler decides what is an error, and what to do about it.

## The Approach

The approach we will take here is very simple. We will read an expression from left to right, one character at a time. This one character we read, which we will refer to as the lookahead character, will give us a clue as to what to do next. As soon as we recognize something, we will translate it, that is, generate code for it. If something unexpected happens, we will stop then and there.

There is a formal name and definition for this approach. We will discuss that at the end of the chapter.

## The "Cradle"

This book, as mentioned in the first chapter, is inspired by Dr. Jack Crenshaw's "Let's Build A Compiler" series of articles. The greatest influence of that excellent work is in this chapter. Just as Dr. Crenshaw did, I will be going through the lessons to be learned in very small steps.

Dr. Crenshaw started his series by creating a mini-program he called the Cradle. This contained some boilerplate code, to do things like input/output, error reporting and so on. All exercises in the series were built on top of that cradle.

We will take a similar approach in this chapter. We already have a CodeGen class, which generates IL code in an executable file for us. We will use that, and build a class called Parser, which will "understand" mathematical expressions, and call CodeGen where necessary to generate executable code.

Our "cradle" will consist of a our CodeGen class, a new class called ParseStatus, a module called Compiler, and a starter version of the Parser class. The code in the first three units will not see much change after this chapter. We will, in this and subsequent chapters, mostly add more code to the Parser class.

Type in the following, and save as **ParseStatus.vb**.

```vbnet
Option Strict On
Option Explicit On

Public Class ParseStatus
    Private m_Code As Integer
    Private m_Description As String
    Private m_Row As Integer
    Private m_Column As Integer

    Public Sub New(ByVal newCode As Integer, _
            ByVal newDescription As String, _
            ByVal newColumn As Integer, _
            ByVal newRow As Integer _
            )
        m_Code = newCode
        m_Description = newDescription
        m_Column = newColumn
        m_Row = newRow
    End Sub

    Public ReadOnly Property Code() As Integer
        Get
            Return m_Code
        End Get
    End Property

    Public ReadOnly Property Description() As String
        Get
            Return m_Description
        End Get
    End Property

    Public ReadOnly Property Column() As Integer
        Get
            Return m_Column
        End Get
    End Property

    Public ReadOnly Property Row() As Integer
        Get
            Return m_Row
        End Get
    End Property
End Class
```

Next, type in the following, and save as **Parser.vb**.

```vbnet
Imports System
Imports System.IO
Imports System.Text

Public Class Parser

#Region "Fields"
    Private m_InputStream As TextReader
    Private m_Gen As CodeGen

    ' The current line being translated
    Private m_ThisLine As String
    Private m_LineLength As Integer = 0

    ' The position of the line being translated
    Private m_linePos As Integer = 0
    ' The character position currently being looked at
    Private m_CharPos As Integer = 0

    ' The current program element
    Private m_CurrentTokenBldr As StringBuilder
#End Region

#Region "Helper Functions"
    Private Function CreateError( _
        ByVal errorcode As Integer, _
        ByVal errorDescription As String _
        ) As ParseStatus

        Dim result As ParseStatus
        Dim message As String

        Select Case errorcode
            Case 0
                message = "Ok."
            Case 1
                message = String.Format( _
                            "Expected {0}", _
                            errorDescription _
                )
        End Select

        result = New ParseStatus(errorcode, _
                    message, _
                    m_CharPos + 1, _
                    m_linePos)


        Return result
    End Function
#End Region

#Region "Recognizers"

#End Region

#Region "Scanner"
    Private ReadOnly Property LookAhead() As Char
        Get
            Dim result As Char
            If m_CharPos < m_LineLength Then
                result = m_ThisLine.Chars(m_CharPos)
            Else
                result = " "c
            End If
            Return result
        End Get
    End Property

    Private ReadOnly Property CurrentToken() As String
        Get
            Return m_CurrentTokenBldr.ToString()
        End Get
    End Property

    Private ReadOnly Property TokenLength() As Integer
        Get
            Return m_CurrentTokenBldr.Length()
        End Get
    End Property

    Private ReadOnly Property EndOfLine() As Boolean
        Get
            Return m_CharPos >= m_LineLength
        End Get
    End Property

    Private Function ScanLine() As Boolean
        Dim result As Boolean
        Dim line As String

        line = m_InputStream.ReadLine()
        If line Is Nothing Then
            result = False
        Else
            ' set up line, line length, 
            ' increment line counter, 
            ' and set character position back to 0
            m_ThisLine = line
            m_LineLength = m_ThisLine.Length
            m_linePos += 1
            m_CharPos = 0
            result = True
        End If

        Return result
    End Function
#End Region

#Region "Parser"
    Private Function ParseLine() As ParseStatus
        Dim result As ParseStatus

        Return result
    End Function
#End Region

    Public Function Parse() As ParseStatus
        Dim result As ParseStatus
        If ScanLine() Then
            result = ParseLine()
        End If
        Return result
    End Function

    Public Sub New( _
        ByVal newStream As TextReader, _
        ByVal newGen As CodeGen _
        )

        m_InputStream = newStream
        m_Gen = newGen
    End Sub
End Class
```

## A brief discussion of terms

Some of the words used, including the name of the class, Parser, are familiar terms in compiler construction. We'll discuss these briefly here, with a more complete description at the end of the chapter.

In translating source code to object code, a part of the compiler is responsible for reading \(or scanning\) the source code, and another part for understanding \(or parsing\) it to ensure grammatical correctness. The first part is called a _lexical analyzer_, a _lexical scanner_ or just _scanner_. The second part is called a _parser_. These work hand-in-hand.

As the scanner reads the source code, it encounters strings of characters, which have special meaning to the parser. For example, a string of characters might be a variable, or a number, or an operator such as + or -. Such a string of characters, which has a collective meaning, is called a _token_. The scanner's job is to read the source and produce tokens. The parser checks the tokens themselves, as well as the sequence in which they appear, for grammatical correctness.

## A brief discussion of the "Cradle"

Traditionally, the scanner and the parser are built separately. Here, we will combine the two operations in our single Parser class. As a convention, we will use method names staring with the word 'Scan' for scanning, and names starting with 'Parse' for parsing. For reading convenience, the Parser class has been provided with \#Region sections, which clearly demarcate the scanner and parser parts.

The class ParseStatus, as the name suggests, will be used to indicate the result of trying to understand the input. If its code field contains 0, the input is understood and valid, otherwise not.

The Parser class will be instantiated by an external class, which is expected to provide it with a CLR TextReader object \(that will provide the scanner part with access to source code\), and also with a CodeGen object \(which is an instance of the CodeGen class we created in the last chapter\). The caller will then call the only Public method, which is Parse.

The Parse method already contains code that will call the ScanLine method from the scanner part. The ScanLine method will read one line of source code from the TextReader. The line will be held in the variable m\_ThisLine, and the position of the current character which needs to be looked at will be held in the variable m\_CharPos.

Provided the ScanLine method has actually read a line, the Parse method will call the ParseLine method, which is where all the code that we will write in this chapter starts out.

The ParseLine method will be responsible for understanding the input scanned by the ScanLine method. It will access the CurrentToken property \(in the scanner part of our class\) to see the actual input, and call methods in the CodeGen class if it understands the input. It will return an instance of the ParseStatus class, which indicates a successful parse or an error.

As we go along, we will add more methods to the Parser class for parsing \(method names staring with 'Parse'\) and scanning \(method names starting with 'Scan'\). We will also add some “recognizer” methods for validating the input, which will be used by the scanning and parsing methods. These will start with the word 'Is'.

When entering the code we will encounter in the rest of the chapter, it's a good idea to keep all methods of a given kind together. Put them in the appropriate \#Region section.

## First step

As of right now, we begin with the assumption that parsing a line of source code means parsing a number, and nothing else. With that in mind, let us proceed to write a method to parse a number, which will depend on a method to scan a number, which will depend on a method which can tell whether the current character being read is valid for a number or not. Add the following methods to the Parser class in **Parser.vb**. As mentioned, it's a good idea to add them to relevant sections of the source file.

```vbnet
Private Function IsNumeric(ByVal c As Char) As Boolean
    Dim result As Boolean

    If Char.IsDigit(c) Then
        ' Use the CLR's built-in digit recognition
        result = True
    Else
        result = False
    End If

    Return result
End Function

Private Sub ScanNumber()
    m_CurrentTokenBldr = New StringBuilder

    Do While m_CharPos < m_LineLength
        If Not IsNumeric(LookAhead) Then
            Exit Do
        End If
        m_CurrentTokenBldr.Append(LookAhead)
        m_CharPos += 1
    Loop
End Sub

Private Function ParseNumber() As ParseStatus
    Dim result As ParseStatus

    ' Ask the scanner to read a number
    ScanNumber()

    If TokenLength = 0 Then
        result = CreateError(1, " a number")
    Else
        ' Get the current token, and
        ' Emit it
        m_Gen.EmitNumber(CInt(CurrentToken))
        result = CreateError(0, "Ok")
    End If

    Return result
End Function
```

Now, these methods are typical of everything we are going to do going forward, so an explanation is in order here. The parsing method, ParseNumber, calls the scanning method ScanNumber. ScanNumber checks if the current character from input is valid for a number or not, and adds it the current token if it is. If not, ScanNumber just exits.

When ScanNumber returns, ParseNumber simply checks to see if there is anything in the current token. If not, this is an error, and ParseNumber returns a ParseStatus with the code 1. Otherwise, things are fine, and ParseNumber calls CodeGen to emit the number that is stored as the current token.

Note the lack of error checking, exception handling etc. Especially note that the ScanNumber method does not return any kind of status information. Not needed, as we shall see shortly.

This is the way it is going to be. We will create a Parse method for anything we want to understand, which will call a corresponding Scan method, which may use one or more Is methods to decide whether to proceed or to return. 

The Scan methods will ensure that the property CurrentToken contains a valid token, or is empty if the input is unexpected or invalid.

The Parse methods will always return a ParseStatus, which will contain an error code wherever needed.

Finally, change the method ParseLine as follows:

```vbnet
Private Function ParseLine() As ParseStatus
    Dim result As ParseStatus

    result = ParseNumber()

    If result.code = 0 Then
        m_Gen.EmitWriteLine()
    End If

    Return result
End Function
```

Right now, understanding a line means understanding a number. If ParseNumber succeeds, we call EmitWriteLine on the CodeGen instance, such that the number we so painstakingly parsed (and generated IL for) can be seen on screen when the resulting executable is run. Bonus points to anyone who remembers why we must call that EmitWriteLine.

## Does it work?

To find out, we will have to create a calling class that uses the parser class. Save the following as **Compiler.vb**.

```vbnet
Option Strict On
Option Explicit On

Imports System
Imports System.IO

Module Compiler
    Public Sub Main()
        Dim reader As TextReader
        Dim gen As CodeGen

        Dim status As ParseStatus
        Dim parser As Parser

        Console.WriteLine("Compiling...")

        reader = Console.In
        gen = New CodeGen("Test.exe")

        parser = New Parser(reader, gen)
        status = parser.Parse()

        With status
            If .Code <> 0 Then
                Console.WriteLine( _
                      "Error at line {0}, column {1} : {2}", _
                      .Row, _
                      .Column, _
                      .Description)
            Else
                gen.Save()
                Console.WriteLine("Done.")
            End If
        End With
    End Sub
End Module
```

Nothing fancy here. Just creating an instance of CodeGen and an instance of Parser, calling Parser.Parse, and if it succeeds, asking the CodeGen instance to save the results in a file called Test.exe.

Actually, there is something just a little fancy here. We are passing Console.In to the Parser instance. Console.In is a TextReader. When this compiler runs, we will be able to type in our "source code" right in the console, and get immediate results.

Okay, let us compile the lot. You may want to review the instructions in the [Development Environment](/the-development-environment.md) chapter.

Compile with:

```bash
vbc /out:Compiler.exe Compiler.vb Parser.vb CodeGen.vb ParseStatus.vb
```

Run using:

```bash
Compiler.exe
```

The "compiler" will wait for input. If we type in any integer number, followed by Enter, our compiler will write an executable file called **Test.exe**, and terminate saying "Done." At that point, we can run

```bash
Test.exe
```

to run the generated executable, which will faithfully echo the number we just typed.

If we type anything other than a number, our "compiler" will terminate, with an accurate error message.

## Error: Missing error

Try this: run our "compiler", and type

```bash
12c
```

Basically, some digits followed by any non-digit character, followed by enter. What happens?

Our "compiler" cheerfully accepts the digit part, and seems to just ignore anything that comes after it. This is because our scanner reads a number by starting with a digit, and then reading subsequent characters until it hits the first non-digit or the end of the line. Our parser then generates code from the current token, which is a string of digits. At this point, our parser does not specify that the current character, which was the first non-digit, is invalid. 

For now, we will ignore this problem; as we enhance the parser to understand more about mathematical expressions, this will be taken care of.

## White (?) Space

Try this: run our "compiler", and type a single space (or multiple spaces) followed by a valid number. What happens?

Our parser knows only about numbers. Our scanner stops as soon as it hits a non-digit-character. Therefore, we get an error.

There are some programming languages, such as COBOL, where white space is significant. How many spaces (or tabs) you put conveys some useful information about the program. However, most modern languages (Python being a notable exception) do not treat white space as significant, which means any amount of space can be left before and after anything. Our "language" should also ignore white space.

Let us adapt our compiler to deal with white space (does anyone know why it’s called "white"?) by adding a scanning method. White space does not have to be "understood" (i.e., no code needs to be generated for it), therefore we don't need a parsing method.

Add the following to the Parser class in **Parser.vb**. The first, as the name suggests, should be put in the Recognizers section. The second, although not following our naming convention, belongs in the Scanner section. We’ll use the different name to remind ourselves that white space is scanned, but not parsed.

```vbnet
Private Function IsWhiteSpace(ByVal c As Char) As Boolean
    Return Char.IsWhiteSpace(c)
End Function

Private Sub SkipWhiteSpace()
    Do While IsWhiteSpace(LookAhead)
        If EndOfLine() Then
            Exit Do
        Else
            m_CharPos += 1
        End If
    Loop
End Sub
```

Finally, in the method ParseLine, just before the call to ParseNumber(), put in a call to SkipWhiteSpace. Here is the rewritten ParseLine method for your convenience.

```vbnet
Private Function ParseLine() As ParseStatus
    Dim result As ParseStatus

    SkipWhiteSpace()

    result = ParseNumber()

    If result.code = 0 Then
        m_Gen.EmitWriteLine()
    End If

    Return result
End Function
```

Compile and run. This time, we can put as many spaces before or after a number, it will still successfully compile.

## Don't be negative

Now, try typing any negative number for our compiler. What happens?

There are many ways to deal with negativity. We will choose to treat a number with a leading minus sign (and no space between the minus sign and the number itself) as a negative number. While we are at it, we will also allow a positive number to be represented with a (optional) plus sign before it. 

What this requires is a change to the recognizer method, IsNumeric. Right now, if the current character being considered is a digit, IsNumeric returns True, and ScanNumber proceeds to add it to the current token. What we have to do is this: _if we are at the start of a token_, then IsNumeric should allow the "+" or "-" symbols as valid. ScanNumber will take care of the rest.

So, here is the rewritten IsNumeric.

```vbnet
Private Function IsNumeric(ByVal c As Char) As Boolean
    Dim result As Boolean

    If Char.IsDigit(c) Then
        ' Use the CLR's built-in digit recognition
        result = True
    ElseIf "+-".IndexOf(c) <> -1 And _
        TokenLength = 0 Then
        ' If the symbol being cheked is + or -
        ' AND we are at the START of the current
        ' token
        result = True
    Else
        result = False
    End If

    Return result
End Function
```

Also, we need to add an additional check in ParseNumber. After our modification of IsNumber, a single – or a +, by itself, would appear to be a correct number, and we would have trouble at the time of code generation. We need to check for this special case in ParseNumber, because as we discussed, ensuring correctness is the parser's job. Here is the rewritten ParseNumber.

```vbnet
Private Function ParseNumber() As ParseStatus
    Dim result As ParseStatus

    ' Ask the scanner to read a number
    ScanNumber()

    If TokenLength = 0 Then
        result = CreateError(1, "a number")
    ElseIf TokenLength = 1 AndAlso _
            Not Char.IsDigit(CurrentToken.Chars(0)) _
            Then 
        result = CreateError(1, "a number")
    Else
        ' Get the current token, and
        ' Emit it
        m_Gen.EmitNumber(CInt(CurrentToken))
        result = CreateError(0, "Ok")
    End If

    Return result
End Function
```

And that's that. When you compile and run again, negative numbers and numbers with + signs before them will now be correctly recognized and translated.

## Factoring in some operators

Okay, time to do a little more math. Right now, our Parser parses only one thing: a number. Let us make it parse three new things: _factor_, _mulordivoperator_, and _term_.

What is a factor? As of right now, a factor is what we already know how to parse: a number. Later, we will increase the scope of this definition.

What is a mulordivoperator? It is either a * or a / symbol, followed compulsorily by a factor.

A term is much more interesting. It represents either a number, or one or more multiplication or division operations. In other words, a term can be either one of the following:

* a factor
* a factor, followed by one or more mulordivoperators

Let’s understand this with some examples:

|Input|Scanned as|
|-----|----------|
|1|This is a valid term, because it is a single number, or factor.|
|1*2|The "\* 2" part is a valid mulordiv operator, as it is a * sign followed by a factor. The "1" part is a valid factor. Hence, the whole input is a valid term.|
|1*6/3|This is also a valid term, because it is one factor ("1") followed by two mulordivoperators (“* 6” and “/ 3”)|

What should happen when each of these get parsed? 

1. A factor, being a number, should cause the number to be emitted on to the stack.
2. A mulordivoperator is a symbol, followed compulsorily by a factor. It is not valid until the factor after the symbol has been parsed. If the factor got parsed successfully, then the number would already be emitted to the stack, as per the rule above. So, a successful parse for a mulordivoperator simply has to emit the **Mul** or **Div** instruction to the stack.
3. A term is a combination of the above.

If we follow the rules written above, the term “1*6/3” should translate to CIL as follows:

```msil
Ldc_i4 1
Ldc_i4 6
Mul
Ldc_i4 3
Div
```

Which, if you remember the last chapter, is exactly what we need.

Let's write up the rules in code. Add the following code to the Parser class in **Parser.vb**. The names should tell you where you should ideally add them.

```vbnet
Private Function IsMulOrDivOperator(ByVal c As Char) As Boolean
    Return "*/".IndexOf(c) > -1
End Function

Private Sub ScanMulOrDivOperator()
    m_CurrentTokenBldr = New StringBuilder

    If IsMulOrDivOperator(LookAhead) Then
        m_CurrentTokenBldr.Append(LookAhead)
        m_CharPos += 1
    End If
End Sub

Private Function ParseFactor() As ParseStatus
    Dim result As ParseStatus

    result = ParseNumber()

    SkipWhiteSpace()

    Return result
End Function

Private Function ParseMulOrDivOperator() As ParseStatus
    Dim result As ParseStatus
    Dim currentoperator As String = CurrentToken

    SkipWhiteSpace()

    result = ParseFactor()

    If result.Code = 0 Then
        If currentoperator = "*" Then
            m_Gen.EmitMultiply()
        Else
            m_Gen.EmitDivide()
        End If
    End If

    Return result
End Function

Private Function ParseTerm() As ParseStatus
    Dim result As ParseStatus

    result = ParseFactor()

    Do While result.Code = 0 _
        AndAlso _
        IsMulOrDivOperator(LookAhead)

        ScanMulOrDivOperator()

        If TokenLength = 0 Then
            result = CreateError(1, "* or /")
        Else
            result = ParseMulOrDivOperator()
            SkipWhiteSpace()
        End If
    Loop

    Return result
End Function
```

Finally, we need to tell ParseLine, the root of our parser, to process Terms instead of Numbers. Here is the rewritten ParseLine.

```vbnet
Private Function ParseLine() As ParseStatus
    Dim result As ParseStatus

    SkipWhiteSpace()

    result = ParseTerm()

    If result.code = 0 Then
        m_Gen.EmitWriteLine()
    End If

    Return result
End Function
```

Compile and run. Now, you can type a single number, or two numbers separated by a * or a /, or any number of numbers separated by * or /. The resulting executable will calculate the whole term from left to right, and show the result.

Why did we do multiplication and division first, instead of addition and subtraction?

## Express Yourself

We are almost at the point where we can parse complete arithmetic expressions. Addition and subtraction are left. Let's put them in.

So far, our compiler parses terms, mulordivoperators, factors and numbers. Let's finish off by adding two more things that it can parse; _addorsuboperators_ and _numericexpressions_.

What is a addorsuboperator? It is either a + or a - symbol, followed compulsorily by a term.

What is a numericexpression? It can be either one of the following:

* a term
* a term, followed by one or more addorsuboperators

Just we defined term using factors, so we define numericexpression using terms. And considering term correctly parses multiplication and division, this makes matters interesting.

Add the following to the Parser class. Again, the names indicate where they should be placed.

```vbnet
Private Function IsAddOrSubOperator(ByVal c As Char) As Boolean
    Return "+-".IndexOf(c) > -1
End Function

Private Sub ScanAddOrSubOperator()
    m_CurrentTokenBldr = New StringBuilder

    If IsAddOrSubOperator(LookAhead) Then
        m_CurrentTokenBldr.Append(LookAhead)
        m_CharPos += 1
    End If
End Sub

Private Function ParseAddOrSubOperator() As ParseStatus
    Dim result As ParseStatus
    Dim currentoperator As String = CurrentToken

    SkipWhiteSpace()

    result = ParseTerm()

    If result.Code = 0 Then
        If currentoperator = "+" Then
            m_Gen.EmitAdd()
        Else
            m_Gen.EmitSubtract()
        End If
    End If

    Return result
End Function

Private Function ParseNumericExpression() As ParseStatus
    Dim result As ParseStatus

    result = ParseTerm()

    Do While result.Code = 0 _
        AndAlso _
        IsAddOrSubOperator(LookAhead)

        ScanAddOrSubOperator()

        If TokenLength = 0 Then
            result = CreateError(1, "+ or -")
        Else
            result = ParseAddOrSubOperator()
            SkipWhiteSpace()
        End If
    Loop

    Return result
End Function
```

Finally, as usual, we need to change ParseLine so that it parses NumericExpressions instead of Terms. Here's the revised ParseLine.

```vbnet
Private Function ParseLine() As ParseStatus
    Dim result As ParseStatus

    SkipWhiteSpace()

    result = ParseNumericExpression()

    If result.code = 0 Then
        m_Gen.EmitWriteLine()
    End If

    Return result
End Function
```

Compile and run. Test it with the following input:

```bash
2+3+4*5
```

The answer, of course, should be 25. And it is.

Try it with any numeric expression. Our compiler will correctly do all multiplications and divisions before additions and subtractions. How? Well, ParseNumericExpression calls ParseTerm (which takes care of multiplications and divisions) before it deals with additions and subtractions. As simple as that.

## Brace Yourself

One more thing, and we can just about close the chapter on arithmetic expressions. To complete our parser, we need the ability to make parts of the expression be calculated before others, through the use of brackets.

The priority part is simple: we gave multiplication and division priority by handling them inside ParseTerm, which gets called from ParseNumericExpression before ParseNumericExpression handles addition and subtraction. All we have to do is handle brackets in a method that gets called from ParseTerm before ParseTerm handles multiplication and division. Yes; we will handle brackets in ParseFactor, which so far was just a call to ParseNumber.

But what do we do with brackets? What can appear inside brackets? Complete numeric expressions, right?

That's it.

Change ParseFactor as follows:

```vbnet
Private Function ParseFactor() As ParseStatus
    Dim result As ParseStatus

    If LookAhead.Equals("("c) Then
        SkipCharacter()

        result = ParseNumericExpression()

        If result.Code = 0 Then
            If Not LookAhead.Equals(")"c) Then
                result = CreateError(1, ")")
            Else
                SkipCharacter()
            End If
        End If
    Else
        result = ParseNumber()
    End If

    SkipWhiteSpace()

    Return result
End Function
```

Notice that ParseFactor is calling a new method, SkipCharacter. Whereas brackets are significant to the syntax, they do not need any special code generated (like white space). Therefore, we just advance the look-ahead character. Add the code for SkipCharacter to **Parser.vb**, in the Scanner section.

```vbnet
Private Sub SkipCharacter()
    m_CharPos += 1
End Sub
```

## Except…

There is always one error that is overlooked. In this case, we have met it before.

Test our compiler with the following:

```bash
12+12 23+23
13+42csuaydf632
4*2(3+2)
```

In all these cases, we have a perfectly valid expression up to a point. Our compiler cheerfully ignores everything from the first invalid character it encounters. The results you would have got from the examples given are 24, 55 and 8 respectively.

We need an additional error check. Our parser knows that a line of source code means a valid expression, but we haven't told it that _it should contain nothing else_. We introduce this additional check in ParseLine. Rewrite ParseLine as follows:

```vbnet
Private Function ParseLine() As ParseStatus
    Dim result As ParseStatus

    SkipWhiteSpace()

    result = ParseNumericExpression()

    If result.code = 0 Then
        If Not EndOfLine() Then
            result = CreateError(1, "end of statement")
        Else
            m_Gen.EmitWriteLine()
        End If
    End If

    Return result
End Function
```

This time, that pesky error should also be accurately caught and displayed.

## LOC

Our compiler, so far, takes one line of input and either generates code or shows an error. A real compiler is capable of doing this for any number of lines. Let's add that capability. Modify the Parse method as follows:

```vbnet
Public Function Parse() As ParseStatus
    Dim result As ParseStatus
    Do While ScanLine()
        result = ParseLine()
        If result.Code <> 0 Then
            Exit Do
        End If
    Loop
    Return result
End Function
```

Compile and run. This time when we run our compiler, we will be able to enter any number of expressions, ending each one with an Enter. To complete our input, press Ctrl and Z together if you are on Windows, or Ctrl and D together if you are on Linux (or OS X) followed by Enter. Test.exe will be generated, and when we run this, the results of our calculations will be displayed in the correct order.

Of course, if there is a compilation error, the compiler will stop immediately, not process any further lines, and not generate any code. This is not how professional compilers work, but it's good enough for us for now. We will re-visit this topic later.

## Finally

Believe it or not, we have a complete compiler of sorts now. Like any major language compiler, it processes source code line by line, and generates an executable. There remains only one problem; "real" compilers read source code from files, rather than get the user to type it in on invocation. Let's take care of that detail.

Change **Compiler.vb** as shown below:

```vbnet
Option Strict On
Option Explicit On

Imports System
Imports System.IO

Module Compiler
    Public Function Main(Byval CmdArgs() As String) As Integer
        Dim reader As TextReader
        Dim gen As CodeGen

        Dim status As ParseStatus
        Dim parser As Parser

        Console.WriteLine("Compiler for CLR")

        If CmdArgs.Length=0 Then
            reader = Console.In
            gen = New CodeGen("Test.exe")
        Else
            If File.Exists(CmdArgs(0)) Then
                Dim finfo As New FileInfo(CmdArgs(0))

                reader = New StreamReader( _
                            finfo.FullName)

                gen = new CodeGen( _
                        finfo.Name.Replace( _
                            finfo.extension, _
                            ".exe" _
                        ) _
                    )
            Else
                Console.Write( _
                    "Error: Could not find the file {0}.", _
                    CmdArgs(0))
                Return 1
            End If
        End If

        parser = New Parser(reader, gen)
        status = parser.Parse()

        With status
            If .Code <> 0 Then
                Console.WriteLine( _
                      "Error at line {0}, column {1} : {2}", _
                      .Row, _
                      .Column, _
                      .Description)
            Else
                gen.Save()
                Console.WriteLine("Done.")
            End If
        End With
    End Function
End Module
```

Remember, our Parser class takes a TextReader, which it uses to read the source. In this case, we just give it a StreamReader, which is a TextReader that can read a file.

Compile as usual. When the resulting Compiler.exe is run, it will check to see if any parameters have been passed on the command line. If no parameters are passed, the behavior is exactly as before. On the other hand, if a file name is passed as the first argument, that file will be parsed, and if everything is correct, be compiled into an executable file with the same name, but the extension .exe.

Test this by creating a file called **calc.sic**, which should contain the following:

```bash
1
2*(2+1)+4
10/5
5*5
```

Run our compiler with:

```bash
Compiler.exe calc.sic
```

This should create a **calc.exe** file, which when run should produce the following output.

```bash
1
10
2
25
```

## Lexing, Parsing and All That Jazz

Before we conclude the chapter, I had promised a slightly more detailed discussion of what we are doing here. Here it is.

Although this section is slightly more detailed in describing compiler theory than the rest of the chapter, do not expect very in-depth coverage. Consider this a layman-termed introduction, by a layman for laymen.

That said, let us proceed.

In classical compiler books, there are large discussions about the process of translating source code into machine language. These discussions fall largely under two heads: lexical analysis (or lexical scanning, or just scanning) and parsing. What exactly do these things mean?

Consider the following source code, which is in the BASIC language:

```basic
somevar = 3 + 1
```

This source code has to be read by the compiler, and understood piece by piece. Intuitively, we can tell that two numbers are being added, and being assigned to a variable. A compiler does this by reading this line, character by character, and separating it into _tokens_.

What is a token? A token is a concept that the next part of the compiler understands. It is a generalization of the things that need to be understood. In the source code shown above, there are four kinds of tokens:

* Variable
* Assignment Operator
* Number
* Addition Operator

The part of the compiler that reads the source, and decides what kind of token the current element is, is called a _lexical analyzer_, a _lexical scanner_ or a _lexer_. A lexer just reads source and identifies tokens; it does not decide if the order in which the tokens appear is correct or not. That is the job of the _parser_.

A parser decides whether the order in which the tokens appear makes sense, or not. The rules that are checked and enforced by the parser are called the _grammar_ or the _syntax_ of the language being parsed. It is here, for example, that we can specify that in a given line of Basic source code, a Variable token appearing at the start of a line can only be followed by an Assignment Operator token, and anything else is an error.

The Lexer passes at least two things to the Parser: a token and a _lexeme_. A token is a general type, such as 'Variable' or 'Assignment Operator', and is usually represented as an enumerated value such as 1 for variable, 2 for assignment operator and so on. A lexeme is the actual value read from source, like 'somevar' or '='. Sometimes the Parser needs the lexeme; sometimes it just needs the token. In the scanner part of our Parser class, the property CurrentToken actually returns the lexeme. In a small compiler like ours, we don't really need to differentiate.

What the Parser does, after checking for syntactic correctness, differs from implementation to implementation. Usually, it produces a representation of the code in a form called _abstract syntax tree_ or AST. The tree is then given to the code generator to generate code. In our case, the parser directly calls the code generator to generate code.

Both lexical scanning and parsing are subjects of intense study, and several texts and tools are devoted to each purpose. In fact, parsing has been studied to the point where there are some well-known methods of doing it efficiently. The terms LL(1), LL(k), LR and LALR are associated with these well-known methods.

The approach we have taken here can have several names applied to it. Consider: we begin by attempting to parse an Expression, which attempts to parse a Term, which attempts to parse a Factor, which either attempts to parse a Number, or (in the case of brackets) recursively, an Expression. This kind of parsing is called Top-down Recursive Descent Parsing. Some say this is the only technique that is suitable for writing a compiler by hand, as we are doing.

Also, we have a single look-ahead character, which allows us to decide whether what comes next is valid or not, and also what to do. As an example, consider ParseFactor: if the look-ahead character is a “(“,  we do a ParseNumericExpression, or else we go to ParseNumber. At any point, the single look-ahead character gives us enough information to decide what we do next. At no point do we have to go backward in the input to figure out what to do. The name given to this kind of parsing, where we can tell what comes next or what to do by looking at look-ahead characters and not backtracking, is called Predictive Parsing.

Finally, we talked about Top-Down parsing, visualizing NumericExpression at the top of the things that we had to translate, and Number at the bottom. Now, visualize the same thing on its side: NumericExpression calls Term calls Factor calls Number. The idea is to get to the value of the left-most thing. When we have finished NumericExpression, we have finished parsing. We do this by examining the source one character at a time, scanning the source from left to right. Any language syntax rule set (or grammar) which meets these three criteria is said to be LL(1). The first L is for scanning from Left-to-right, the second L for producing the value of the Left-most derivation (NumericExpression in our example), and the 1 for 1 look-ahead token.

## Conclusion

In this chapter, we have built a top-down, recursive-descent, predictive LL(1) parser (feel good?). We have also built a compiler, using said parser and the code generator we built in the last chapter. Our compiler compiles files full of calculations into executables that show the results of those calculations.

We have also created “cradle” for all the work we will do so far, consisting of:

* A utility class for reporting parse status (**ParseStatus.vb**)
* A code generator class (**CodeGen.vb**)
* A combiner scanned and parser class (**Parser.vb**)
* A “driver” which ties the parser and code generator together (**Compiler.vb**)

This structure will remain unchanged in the next several chapters. We will add or modify code, mostly in the parser.

By now, we have done everything that most computer languages do with integer numbers. We could now take up floating-point numbers, but let’s hold that for a while. In the next chapter, we look beyond numbers to that staple of modern languages: strings.
