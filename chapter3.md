# Strings

## Introduction
In the last chapter, we built a parser that correctly recognizes and translates mathematical expressions. In this chapter, we will also make it recognize and translate strings.

## The Goal
Thus far, if our compiler meets a line consisting of a mathematical expression, it emits CIL instructions to perform the calculation, and the emitted executable shows the result. After we are done with this chapter, if a line contains a string instead, it should show the string.

## Representing strings
How is a string represented in source code? The almost universally accepted standard nowadays is to enclose a string in double quotes ("). So:

```
"This is a string"
```

This brings in an interesting question; what if a string had to contain a double quote? This is usually taken care of by specifying an 'escape' character, which, if it appears inside a string, causes the next character to be treated specially. So, one could represent a quote inside a string like this:

```
\"
```

This assumes that ```\``` is the escape character. The escape character itself can be literally represented like this:

```
\\
```

A variety of special characters can be represented by the escape character followed by another; for instance, ```\n``` for new line, ```\t``` for tab etc. These are called _escape sequences_.

For now, we will keep strings simple, and not use escape characters or sequences. For the single special case of representing a double quote within a string, we will borrow from the Basic language. That language specifies that to put a " sign inside a string, we have to use two double quotes without any space between them, like this:

```
"Raj says, ""We will use this for now."""
```

Finally, we need a way to represent an 'empty' string. Borrowing from most string-aware languages, we will represent this as two double quotes without any space between them, like this:

```
""
```
Is there such a thing as a string expression, involving strings and _string operators_? Well, most modern languages define an operator for string concatenation, or joining two strings together. We will once again borrow from the Basic language, which uses either the & symbol, or the + symbol for this purpose. So, the following are also valid strings:

```
"Hello " & "World"
"How do I" + " say goodbye?"
```

## The Approach
The approach we take remains the same as in the last chapter; we will create a scan method for reading a string, and a parse method for generating code for it. Since any character is valid inside a string, we do not need a recognizer method. Generating code will also not be a problem initially, since at the end of chapter 2, we had added the capability to load strings to our CodeGen class.

## First step
Just like in the last chapter, we will deal with strings one step at a time. To begin with, let us read and translate just a string, not the concatenation operator.

Add the following to our Parser class. As shown in the last chapter, the method names give a clue as to where they should ideally be added. The field at the beginning should be added near the other fields of the Parser class.

```vbnet
' Whether the last string scanned was empty
Private m_EmptyStringFlag As Boolean = False

Private Sub ScanString()
    m_EmptyStringFlag = False
    m_CurrentTokenBldr = New StringBuilder

    If Not LookAhead.Equals(""""c) Then
        Exit Sub
    End If

    Do While LookAhead.Equals(""""c)
        SkipCharacter()
        Do While Not LookAhead.Equals(""""c)

            If EndOfLine Then
                m_CurrentTokenBldr = New StringBuilder
                Exit Sub
            End If

            m_CurrentTokenBldr.Append(LookAhead)
            m_CharPos += 1
        Loop

        SkipCharacter()

        If LookAhead.Equals(""""c) Then
            m_CurrentTokenBldr.Append(LookAhead)
        End If
    Loop

    If TokenLength = 0 Then
        m_EmptyStringFlag = True
    End If
End Sub

Private Function ParseString() As ParseStatus
    Dim result As ParseStatus
    
    ScanString()
    
    If TokenLength = 0 And Not m_EmptyStringFlag Then
        result = CreateError(1, "a valid string.")
    Else
        m_Gen.EmitString(CurrentToken)
        result = CreateError(0,"Ok")
    End If
    
    SkipWhiteSpace()
    
    Return result
End Function
```

This is very similar to what we did in the last chapter. A Scan method reads the input, and a Parse method parses it. There is only one thing different about this Scan method; if we reach the end of the line before the ending quote has been encountered, it clears out the current token. In this way, it indicates to the Parse method that a valid string was not encountered. The unwritten agreement between the scanner and the parser is that the scanner will only pass on a valid token.

The ParseString method checks if the token length is zero, and that the emptystring flag is not set. The token length may be zero for an invalid string as well as for an empty string.

To test this, we will need to modify our ParseLine method, which currently triggers the numeric expression parser, to trigger our string parser instead. Here is the modified ParseLine, with the old line commented out.

```vbnet
Private Function ParseLine() As ParseStatus
    Dim result As ParseStatus

    SkipWhiteSpace()

    ' result = ParseNumericExpression()
    result = ParseString()

    If result.code = 0 Then
        If Not EndOfLine() Then
            result = CreateError(1, "end of statement")
        Else
            ' m_Gen.EmitWriteLine()
            m_Gen.EmitWriteLineString()
        End If
    End If

    Return result
End Function
```

Note that the ```m_Gen.EmitWriteLine``` call has been replaced by  ```m_Gen.EmitWriteLineString```. The numeric expression parser left a number on top of the stack; the string parser will leave a string, and we need to call the appropriate WriteLine. I hope everyone still remembers why a WriteLine is required, as things stand.

Time to compile. You may want to review the instructions in the [Development Environment](/the-development-environment.md) chapter.

Compile with:

```
vbc /out:Compiler.exe Compiler.vb Parser.vb CodeGen.vb ParseStatus.vb
```

Run using:

```
Compiler.exe
```

Remember, this is the multi-line version of the compiler, so you can enter any number of strings, one on each line. Press Ctrl-Z or F6, followed by Enter (or Ctrl-D if you are on Linux or OS X) to finish. Try the following:

```
"Testing strings"
"Our ""compiler"" should be able to process this."
"This should cause an error
So should this"
1234
```

As before, if there is no error, **Test.exe** would be generated. Run that to display the strings that have been successfully parsed.

## Expressions
Now, it's time to work on string expressions. We can follow the same technique as for numeric expressions. Thus, a string expression is one of the following:

* a string
* a string, followed by one or more _concatoperators_

What is a concatoperator? It is a concatenation operator symbol(& or +), followed compulsorily by a string.

As we saw in the last chapter, this is pretty easy to scan and parse. The result of the parsing should be as follows:
1. Parsing a string should cause the string to be emitted on to the stack.
2. A concatoperator is a symbol, followed compulsorily by a string. It is not valid until the string after the symbol has been parsed. If the string got parsed successfully, then it would already be emitted to the stack, as per the rule above. So, a successful parse for a concatoperator simply has to emit the CIL instruction for concatenating two strings to the stack.

Which is where we run into a problem. Our code generator currently does not know how to concatenate strings.

## Modifying CodeGen
We have not made any changes to CodeGen since Chapter 1. We have to do so now, to accomodate string concatenation.

Unlike everything else we have parsed so far, the operation of string concatenation _does not have a corresponding CIL instruction_. Instead, we have to rely on a class provided by the CLR base class library. The class is ```System.String```, which has a shared (static) method called ```Concat``` that does what we want.

This is not the first time we have used a shared (static) method from a class in the CLR base class library. Our CodeGen class already has two methods, ```EmitWriteLine``` and ```EmitWriteLineString```, where we have used the ```WriteLine``` method of a class called ```System.Console```. This time, let's take a closer look at how it's done.

Add the following to the ```CodeGen``` class in **CodeGen.vb**.

```vbnet
Public Sub EmitConcat()
    Dim stringtype As Type = Type.GetType("System.String")
    Dim paramtypes() As Type = { stringtype, stringtype }

    Dim concatmethod as MethodInfo = stringtype.GetMethod( _
            "Concat", paramtypes _
    )

    m_ILGen.Emit( _
        Opcodes.Call, _
        concatmethod   
    )
End Sub
```

This is the way to emit calls to class methods. We obtain a class as a ```Type``` object, which can be done using ```Type.GetType```. The ```GetMethod``` method of a Type object can be used to obtain a representation of any method of that type. Since methods can be overloaded, i.e., multiple methods may have the same name, but different parameters, we need to tell ```GetMethod``` the types of the parameters of the method we want. We do this by passing an array of Type objects as the second parameter to ```GetMethod```. If the named method (with the specified types of parameters) exists on the type, a ```MethodInfo``` object is returned. The ```MethodInfo``` object can be emitted as CIL, usually as a parameter to the opcode **Call**.

In the code above, we obtain ```System.String``` as a ```Type``` object. Then, we create an array of types, which contains two instances of ```System.String``` represented as ```Type``` objects. Then, we call the ```GetMethod``` method (bear with me) of the String type we obtained, passing it the name of the method we want, which is "Concat", and the array we created earlier. This returns a ```MethodInfo``` object representing the Concat method with two string parameters. We proceed to emit the CIL instruction **Call**, passing our ```MethodInfo``` object as a parameter.

At run time, this will cause the CLR to invoke a method called ```Concat``` (this was specified in the GetMethod call), which has two string parameters (this was specified via the Type array we created called paramtypes), on the String class. 

Where will the two string parameters come from? The stack, of course. This use case of the **Call** instruction will expect two strings on the stack, just like the **Add** instruction expected two numbers on the stack. We have to emit the strings before emitting this call. Needless to say, the two strings will be popped, and the result of the concatenation, a new string, will be pushed back on the stack.

## Parsing the String Expression
Okay, now that our code generator is capable of handling concatenation, let's take care of the parsing bit. Add the following to our ```Parser``` class. The names indicate where.

```vbnet
Private Function IsConcatOperator(c As Char) As Boolean
    Return "&+".IndexOf(c) > -1
End Function

Private Sub ScanConcatOperator
    m_CurrentTokenBldr = New StringBuilder
    
    If IsConcatOperator(LookAhead) Then
        m_CurrentTokenBldr.Append(LookAhead)
        m_CharPos += 1
    End If	
End Sub

Private Function ParseConcatOperator() As ParseStatus
    Dim result As ParseStatus
    Dim currentoperator As String = CurrentToken
    
    SkipWhiteSpace()
    
    result = ParseString()
    
    If result.code = 0 Then			
        m_Gen.EmitConcat
    End If
    
    Return result
End Function	

Private Function ParseStringExpression() As ParseStatus
            
    Dim result As ParseStatus
    
    result = ParseString()
    
    Do While result.code=0 _
        AndAlso _
        IsConcatOperator(LookAhead) 
        
        ScanConcatOperator()
        
        If TokenLength=0 Then
            result = CreateError(1, "& or +")
        Else
            result = ParseConcatOperator()
            SkipWhiteSpace()				
        End If
    Loop
                
    Return result	
End Function
```

As you can see, parsing a string expression is just about exactly the same as parsing a numeric expression.

To test this, we once again have to modify ParseLine, to call ParseStringExpression instead of ParseString. Here is the modified ParseLine.

```vbnet
Private Function ParseLine() As ParseStatus
    Dim result As ParseStatus

    SkipWhiteSpace()

    ' result = ParseNumericExpression()
    ' result = ParseString()
    result = ParseStringExpression()

    If result.code = 0 Then
        If Not EndOfLine() Then
            result = CreateError(1, "end of statement")
        Else
            ' m_Gen.EmitWriteLine()
            m_Gen.EmitWriteLineString()
        End If
    End If

    Return result
End Function
```

Compile and run. Test with the following strings:

```
"This " & "works"
"So " & "does " + "this"
"And """ & "even" & """ this."
```