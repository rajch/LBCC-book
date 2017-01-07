# Compiling Arithmetic Expressions

## Introduction

In the last chapter, we saw how we can create an executable file via the CLR's Reflection Emit library. That, as we have discussed, is the final task that a compiler does. There are some steps that come before it. In this chapter, we discuss these steps, and begin to implement them.

## The Goal

The goal for this chapter is to write a program that understands simple arithmetic expressions, and produces an executable. The produced executable should calculate and display the values of the expressions.

A typical arithmetic expression looks something like this:

```
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

