# Booleans Part I

## Introduction
In the last chapter, we extended our parser to recognize and translate string expressions as well as mathematical expressions. In this chapter, we add one more kind of expression to the mix: the Boolean.

The Boolean data type, named after George Boole, a British mathematician, refers to data that evaluates to either `true` or `false`. At the very basic level, this involves comparison of two values, as in, _is 3 greater than 2_ (`true`). This is called a _condition_. More complex Boolean expressions involve combinations of one, two or more conditions with _logical operators_ such as AND, OR and NOT.

Many simpler languages, including older versions of Basic, do not treat Boolean as a distinct data type. Instead, they use Boolean expressions as necessary in language constructs such as branches. We will treat Boolean as a data type, just like we have done for numbers and strings.

## Goals
In this chapter, we will deal with the basic unit of Boolean data: the _condition_. More complex boolean expressions will be dealt with in the next chapter.

When we are done with this chapter, our compiler will recognize a Boolean expression, and show the result - either `true` or `false`.

## Representing a condition
A Boolean condition is expressed as an expression, followed by a _relational operator_, followed by another expression _of the same type_. A relational operator specifies a comparison to be performed between the expression on its left and the expression on its right. Common relational operators are:

|||
|---|---|
|= or ==|Equal to. The condition evaluates to `true` if the expressions on the left and right produce the same value, `false` otherwise.|
|<> or !=|Not equal to. The opposite of the the above.|
|>| Greater than. The condition evaluates to `true` if the expression on the left produces a value that is greater than the one on the right, `false` otherwise.|
|<= or =<|Less than or equal to. The opposite of the above.|
|<|Less than. The condition evaluates to `true` if the expression on the left produces a value that is less than the one on the right, `false` otherwise.|
|`>=` or `=>`|Greater than or equal to. The opposite of the above|
|||


Did you notice the difference from the last two chapters? For numbers and strings, the basic unit of parsing was a single piece of data: a number or a string. The condition has three pieces of data, and two of them are expressions - which have to be of the same type. 

We already know how to parse expressions, and detect their type. So what remains to be done? We need to recognize relational operators immediately following any expression, and if one is present, try and parse an expression of the same type immediately after it. The whole thing then becomes a condition, which is an expression of type Boolean.

## The Approach
So, a condition is

* An _expression_, followed by a _reloperator_

while a _reloperator_ is

* a relational operator, followed by an expression

We already have parsers for expressions. We will need recognizer and scanner methods for _reloperator_ - that's easy enough. The parser, though, will have to check that the two expressions, before and after, are of the same type.

Here's another challenge: so far, we could guess the type of expression by looking at the first character of the expression. If the lookahead character is +,-, or a digit, the type of the expression is numeric. If it is " (a double quote), the expression's type is string. The Boolean-type condition gives us no such clues. Indeed, it starts with an expression of either type; we only know that we are parsing a Boolean expression if we hit a _reloperator_. For once, the single-character lookahead seems inadequate. Can we still make things work? Let us see.

Lastly, how do we generate code for comparison? Numbers and strings require separate treatment; we will modify CodeGen to handle both cases. 

## First Step
As usual, we will do things one step at a time. Let us begin by recognizing and translating Boolean conditions that involve only numeric expressions on both sides of relational operators.

First and foremost, we have to enable our code generator to handle comparison operations.

## Modifying CodeGen
We have seen that the CLR understands numbers and strings, and provides instructions to load, store, and perform operations on them. What about Boolean data?

The CLR does have a formal Boolean data type. This type occupies one byte in memory. A zero value indicates `false`; any other value indicates `true`. 

However, there are no specific opcodes to load and store booleans - you can use integer instructions instead. Also, the opcodes that represent boolean operations push an integer to the stack: either 0 or 1 for `false` or `true`.

There are three such opcodes:

|OpCode|What it does|
|---|---|
|Ceq|Pops the last two values on the stack, and if they are equal, pushes an integer 1 on the stack, otherwise pushes an integer 0. As of now, we will consider the popped values to be integer numbers.|
|Cgt|Pops the last two values on the stack, and compares them. If the _first_ value is strictly greater than the _second_ value, an integer 1 is pushed onto the stack, otherwise 0 is pushed. Note that _first_ and _second_ here refer to the order in which the values were pushed on to the stack. So, the instruction sequence:<br />**Ldc.i4** 2<br />**Ldc.i4** 3<br />**Cgt**<br />would result in 0 being pushed back to the stack.|
|Clt|Pops the last two values on the stack, and compares them. If the _first_ value is strictly less than the _second_ value, an integer 1 is pushed onto the stack, otherwise 0 is pushed.|
|||

Generating code for these operations is straightforward. Add the following to **CodeGen.vb**

```vbnet
Public Sub EmitEqualityComparison()
	m_ILGen.Emit(Opcodes.Ceq)
End Sub

Public Sub EmitGreaterThanComparison()
	m_ILGen.Emit(OpCodes.Cgt)
End Sub

Public Sub EmitLessThanComparison()
	m_ILGen.Emit(OpCodes.Clt)
End Sub
```

What about the the three other kinds of inequality?

There are no direct CIL instructions. However, the three remaining relational operations are simply the opposite of the three operations that we have just handled. Exact inequality (<> or !=) is the opposite of equality,  less than or equal to (<=) is the opposite of greater than, and greater than or equal to (>=) is the opposite of less than. 

What we therefore do is to perform the opposite of the inequality that we want, and _compare the result for equality with the integer 0_. For example, to test for greater than or equal to, we actually test for a less than, and see if the _result_ is equal to false.

Add the following to **CodeGen.vb**:

```vbnet
Private Sub NegateComparison
	EmitNumber(0)
	EmitEqualityComparison
End Sub

Public Sub EmitInEqualityComparison
	EmitEqualityComparison
	NegateComparison
End Sub

Public Sub EmitGreaterThanOrEqualToComparison
	EmitLessThanComparison
	NegateComparison
End Sub

Public Sub EmitLessThanOrEqualToComparison
	EmitGreaterThanComparison
	NegateComparison
End Sub
```

Before we go on to the parsing, we will add another case of WriteLine, which will allow the emitted executable to print Booleans. Add the following to **CodeGen.vb**:

```vbnet
Public Sub EmitWriteLineBoolean()
	Dim booltype As Type = _
		Type.GetType("System.Boolean")
		
	Dim consoletype As Type = _
		Type.GetType("System.Console")
	
	Dim paramtypes() As Type = _
		{ booltype}
	
	m_ILGen.Emit(Opcodes.Call, _
		consoletype.GetMethod( _
		"WriteLine", paramtypes))
End Sub
```

## Parsing Relational Operators
First, let's write a recognizer and scanner for relational operators. Since there are muliple relational operators, the scanner has to check the token for validity before letting the parser have it.

We will allow the following relational operators:

|Operator|What it does|
|---|---|
|=,==,===|Equality. Allowing all these forms should make people who like most popular programming languages happy.|
|>, <, >=, <=, =>, =<|Just what they look like. We even support the seldom-used =< and =>, which have the same meaning as >= and <= respectively. The only other language I can think of that uses these operators is Ada.|
|<>,!=,!==|Inequality. Once again, we support both popular forms. We can _try_ to please everybody. Except the FORTRAN, TCL and shell people.|
|||

Where an operator uses two or more symbols (like <=), we do not allow white space between them. 

Add the following code to **Parser.vb**. As usual, the procedure names indicate the region where they should be added.

```vbnet
Private Function IsRelOperator(c As Char) As Boolean
	Return "=><!".IndexOf(c)>-1
End Function

Private Sub ScanRelOperator
    m_CurrentTokenBldr = New StringBuilder
    
    Do While IsRelOperator(LookAhead)
        m_CurrentTokenBldr.Append(LookAhead)
        m_CharPos += 1
    Loop

    Select Case CurrentToken
        Case "=","==", "===", "<>", "!=", "!==", ">", "<", ">=", "=>","<=","=<"
            ' Valid relational operator
        Case Else
            m_CurrentTokenBldr = New StringBuilder
    End Select
End Sub 
```

Next, let's write the parser. Add this to the Parser region in **Parser.vb**.

```vbnet
Private Sub GenerateRelOperation( _
		reloperator As String, _
        conditionType As Type)
		
    If conditionType.Equals( _
            Type.GetType("System.Int32")
        ) Then
        Select Case reloperator
            Case "=", "==", "==="
                m_Gen.EmitEqualityComparison()
            Case ">"
                m_Gen.EmitGreaterThanComparison()
            Case "<"
                m_Gen.EmitLessThanComparison()
            Case ">=", "=>"
                m_Gen.EmitGreaterThanOrEqualToComparison()
            Case "<=", "=<"
                m_Gen.EmitLessThanOrEqualToComparison()
            Case "<>", "!=", "!=="
                m_Gen.EmitInEqualityComparison()
        End Select
    End If
End Sub

Private Function ParseRelOperator() _
		As ParseStatus
		
	Dim result As ParseStatus
	Dim reloperator As String = CurrentToken
    Dim conditiontype As Type = m_LastTypeProcessed
	
	SkipWhiteSpace()
	
    ' The expression after a relational operator
    ' should match the type of the expression
    ' before it
    If conditiontype.Equals( _
            Type.GetType("System.Int32") _
        ) Then

        result = ParseNumericExpression()
    ElseIf conditiontype.Equals( _
            Type.GetType("System.String") _
        ) Then

        result = ParseStringExpression()
    Else
        result = CreateError(1, "an expression of type " & _
                        conditiontype.ToString())
    End If

    If result.Code = 0 Then
        GenerateRelOperation(reloperator, conditiontype)
    End If

    Return result
End Function
```

Note that for the first time, we have a method whose name begins with Generate. This is just a convenience, to handle the large number of relational operators. It should be placed in the parser section.

Also note that as discussed in First Steps, we are only processing conditions involving numeric expressions for now. Hence, the GenerateRelOperation method calls CodeGen only if the condition type is numeric.

Now, we need a parser for the condition itself. And here, we face something different from before.

## Parsing Conditions
Unlike string and numeric expressions, conditions cannot be recognized by their first character. So, it is possible that we find out that we are parsing a condition only after we have already parsed an expression, and the next token is a relational operator. It is also possible that we know from the outset that we are parsing a condition (we are not ready for that scenario yet, but will be soon). Our condition parser needs to take care of both cases.

Add the following to **Parser.vb**:

```vbnet
Private Function ParseCondition( _
                    Optional lastexpressiontype As Type = Nothing _
    ) As ParseStatus

    Dim result As ParseStatus

    If lastexpressiontype Is Nothing Then
        result = ParseExpression()
    Else
        m_LastTypeProcessed = lastexpressiontype
        result = CreateError(0,"")
    End If

    If result.Code = 0 Then

        ScanRelOperator()

        If TokenLength = 0 Then
            result = CreateError(1, "a relational operator.")
        Else
            result = ParseRelOperator()
            SkipWhiteSpace()
        End If
    End If

    Return result
End Function
```

The ParseCondition method will take an optional parameter indicating the type of the last expression parsed. If an expression has not already been parsed, the parser will parse one explicitly. Thereafter, with the type of the last expression set, it will try to scan and parse a relational operator, which in turn will type-check and parse the next expression.

## Parsing a Boolean Expression
At this point, a Boolean expression is just a condition. The parser is simple. Add it to **Parser.vb**.

```vbnet
Private Function ParseBooleanExpression( _
                    Optional lastexpressiontype As Type = Nothing _
    ) As ParseStatus

	Dim result As ParseStatus

    result = ParseCondition(lastexpressiontype)
    
    If result.Code = 0 Then
        m_LastTypeProcessed = _
            Type.GetType( _
            "System.Boolean")
    End If
	
	Return result
End Function
```

Note that in this parser too, we account for the fact that a Boolean expression can be detected after an expression has already been processed. Finally, as in all our root expression-parsing methods, we set the last type processed at successful completion. In this case, it is Boolean.

## Mixing it up
Now that we have a Boolean expression parser, we have to call it where appropriate. Which, at this point in our compiler construction story, is the ParseExpression method. Remember, the lookahead character is not helpful in this case.

Modify the ParseExpression method in **Parser.vb** as follows:

```vbnet
Private Function ParseExpression( _
                    Optional ByVal expressiontype _
                        As Type = Nothing) _
        As ParseStatus

    Dim result As ParseStatus

    ' Since we are doing the work of the scanner by using the
    ' lookahead character, we need to initialize the token
    ' builder
    m_CurrentTokenBldr = New StringBuilder

    If LookAhead.Equals(""""c) Then
        result = ParseStringExpression()
    ElseIf IsNumeric(LookAhead) Then
        result = ParseNumericExpression()
    ElseIf LookAhead.Equals("("c) Then
        ' For now, assuming only numeric expressions can use ()
        result = ParseNumericExpression()
    Else
        result = CreateError(1, "a boolean, numeric or string expression")
    End If

    If result.Code = 0 Then
        ' If a relational operator follows the expression just parsed
        ' we are in the middle of a Boolean expression. Our work is
        ' not yet done.
        If IsRelOperator(LookAhead) Then
            ' Parse a boolean expression, letting it know that the
            ' first part has already been parsed.
            result = ParseBooleanExpression(m_LastTypeProcessed)
        End If
    End If

    Return result
End Function
```

See how we make the check for a relational operator after the lookahead-predicted initial parse?

Finally, our ParseLine method has to be modified to generate the Boolean edition of WriteLine when required. Modify it in **Parser.vb** as follows:

```vbnet
Private Function ParseLine() As ParseStatus
    Dim result As ParseStatus
    
    SkipWhiteSpace()
    
    m_LastTypeProcessed = Nothing
    
    result = ParseExpression()
    
    If result.code = 0 Then
        If Not EndOfLine() Then
            result = CreateError(1, "end of statement")
        Else
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
        End If
    End If
    
    Return result
End Function
```

Time to compile. You may want to review the instructions in the [Development Environment](/the-development-environment.md) chapter.

Compile with:

```
vbc /out:Compiler.exe Compiler.vb Parser.vb CodeGen.vb ParseStatus.vb
```

Run using:

```
Compiler.exe
```

As in the previous chapter, this is the multi-line version of the compiler, so you can enter any number of expressions of any type, one on each line. Press Ctrl-Z or F6, followed by Enter (or Ctrl-D if you are on Linux or OS X) to finish. Try the following:

```
"Testing Boolean data"
"10>3"
10>3
"10<3"
10<3
"5>=5"
5>=5
"5>=2"
5>=2
"5>=10"
5>=10
"3<=3"
3<=3
"3<=5"
3<=5
"3<=2"
3<=2
"3<>5"
3<>5
"3!=5"
3!=5
```

We take advantage of our compiler's ability to handle lines consisting of different data types, and display each condition before actually evaluating it and showing the result. When you run the resulting test.exe, your output should look like this:

```
Testing Boolean data
10>3
True
10<3
False
5>=5
True
5>=2
True
5>=10
False
3<=3
True
3<=5
True
3<=2
False
3<>5
True
3!=5
True
```

Needless to say, our parser will correctly catch and display any errors in syntax in the Boolean expression. Try that too.

## Comparing Strings
We can now correctly compare any two numeric values. What about strings? Equality and inequality are fairly obvious, but how do you determine whether a string is "greater than" or "less than" another?

Comparison of strings is quite a can of worms. Most languages (including a prominent CLR language) simply avoid the topic by providing only equality and strict inequality comparisons for strings. But some languages, notably Basic, have always provided the ability to compare strings. So shall we. Let's look at how.

In common, day-to-day use of language, we usually use the so-called alphabetical order. This order loosely specifies how we compare strings; for example,. The string "A" is "less than" the string "B", and so on.  For representation on computers, a more strict specification is required.

As most people know, this specification was initially achieved by assigning a code number to each commonly used character, and the order of these codes were used to determine the order of strings as well. Over time, several systems of such codes evolved, culminating in the what is today known as Unicode.

We can still use this system to order (and therefore compare) our strings. Under this scheme, the order of commonly used characters is as follows:

    digits (0-9) < uppercase Roman letters (A-Z) < lowercase Roman letters (a-z) < accented letters (À etc.) < characters in non-roman languages

So, in this scheme:

    "013" < "JAR" < "Jar" < "jar" < "jÀr" < "jàr"

What about the case where the strings being compared are of different lengths? The comparison terminates the moment two corresponding characters are different. For example, if a character in the first string is greater than the corresponding character in the second string, the first string is greater, no matter what the length of either string is. If the two strings are identical up to a point, and only differ in length, then and only then, the longer string is considered greater. So, for example:

    "JAR" < "j"
    "ja">"JAR"
    "ja" < "jar"

The last special case is the empty string. This is considered less than any other string. Thus:

    "ja" > ""
    "" < "JA"

This method of comparing strings is formally called "ordinal" comparing, or sometimes "binary" comparing because the code(binary) value of each character in one string is compared against the corresponding character in the other. This is good enough in a lot of circumstances.

However, the moment you start dealing with non-English languages, this scheme may not be enough. There may be languages which use the roman character set, but have different ordering rules. And complex scripts, which involve things like combinations of characters, may not be easily amenable to this kind of ordering. Luckily, the CLR is aware of such globalization issues, and provides methods for "culture-aware" comparisons. 

The language from which I have generally been plagiarizing my treatment of strings, Microsoft Visual Basic, allows for comparison using either the ordinal or the culture-aware method. You can specify which option to use at the beginning of each source code file by the means of an instruction (`Option Compare`) to the compiler (Such an instruction, which does not cause code to be generated directly, but affects the behavior of  the compiler, is called a _pragma_).

For now, we will cause our string comparisons to exclusively use the ordinal method, where digits are less than uppercase letters are less than lowercase letters and so on.

Note that I am omitting a great deal of detail here. As has been the practice so far, we deal with just as much as we need. ~~If you would like to dig deeper, the .NET Framework SDK documentation contains huge amounts of information on the topic of cultures, string comparisons etc., replete with excellent examples. Look under globalization.~~ I no longer vouch for either the availability or the quality of any documentation provided by Microsoft; especially the Developer Division.

## Strings are not numbers
We cannot use the IL comparison opcodes (`Ceq`, `Cgt` and `Clt`) to compare strings. Instead, just like we did for string concatenation, we will borrow a shared (static) method of the string class, which is provided by the CLR itself.

The method we want is called `CompareOrdinal`. It expects two strings on the stack (let's call them string A and string B, in that order of pushing), and compares them using the ordinal comparison method described above. Depending on the result of the comparison, it pushes a value on the stack. The value pushed is a negative number if string A is less than string B, a positive number if string A is greater than string B, or zero if they are equal.

## Modifying CodeGen to compare strings
We want to create a set of CodeGen methods for strings, which mimic the behavior of our existing comparison methods. Let's do that now.

Add the following to **CodeGen.vb**.

```vbnet
Private Sub EmitStringCompare()
    Dim stringtype As Type = _
        Type.GetType("System.String")

    Dim paramtypes() As Type = _
        {stringtype, stringtype}

    m_ILGen.Emit( _
            OpCodes.Call, _
            stringtype.GetMethod( _
                    "CompareOrdinal", _
                    paramtypes _
            ) _
    )
End Sub

Public Sub EmitStringEquality()
    EmitStringCompare()
    EmitNumber(0)
    EmitEqualityComparison()
End Sub

Public Sub EmitStringInequality()
    EmitStringEquality()
    NegateComparison()
End Sub

Public Sub EmitStringGreaterThan()
    EmitStringCompare()
    EmitNumber(0)
    EmitGreaterThanComparison()
End Sub

Public Sub EmitStringLessThan()
    EmitStringCompare()
    EmitNumber(0)
    EmitLessThanComparison()
End Sub

Public Sub EmitStringGreaterThanOrEqualTo()
    EmitStringCompare()
    EmitNumber(-1)
    EmitGreaterThanComparison()
End Sub

Public Sub EmitStringLessThanOrEqualTo()
    EmitStringCompare()
    EmitNumber(1)
    EmitLessThanComparison()
End Sub
```

In the method `EmitStringCompare`, we are emitting a call to the shared (static) method `CompareOrdinal` of the class `System.String`. This is the basis of all the string comparisons, which function in exactly the same way as their numeric counterparts. With two exceptions.

In the case of 'greater than or equal to' and 'less than or equal to', we do not negate less than and greater than respectively. Instead, we take advantage of the nature of the return value of `CompareOrdinal`. Since `CompareOrdinal` returns a _negative number_ for less than and a zero for equal to, all we have to do is check to see if the returned value is less than one to perform the  'less than or equal to' comparison. The exact opposite approach is used for 'greater than or equal to'.

Now, we have to modify the `GenerateRelOperation` method of our parser to ensure that string comparisons generate the correct code. Replace the method in **Parser.vb** with the revised one shown below.

```vbnet
Private Sub GenerateRelOperation( _
        reloperator As String, _
        conditionType As Type)
        
    If conditionType.Equals( _
            Type.GetType("System.Int32")
        ) Then

        Select Case reloperator
            Case "=", "==", "==="
                m_Gen.EmitEqualityComparison()
            Case ">"
                m_Gen.EmitGreaterThanComparison()
            Case "<"
                m_Gen.EmitLessThanComparison()
            Case ">=", "=>"
                m_Gen.EmitGreaterThanOrEqualToComparison()
            Case "<=", "=<"
                m_Gen.EmitLessThanOrEqualToComparison()
            Case "<>", "!=", "!=="
                m_Gen.EmitInEqualityComparison()
        End Select

    ElseIf conditionType.Equals( _
            Type.GetType("System.String")
        ) Then

        Select Case reloperator
            Case "=", "==", "==="
                m_Gen.EmitStringEquality()
            Case ">"
                m_Gen.EmitStringGreaterThan()
            Case "<"
                m_Gen.EmitStringLessThan()
            Case ">=", "=>"
                m_Gen.EmitStringGreaterThanOrEqualTo()
            Case "<=", "=<"
                m_Gen.EmitStringLessThanOrEqualTo()
            Case "<>", "!=", "!=="
                m_Gen.EmitStringInequality()
        End Select

    End If
End Sub
```

And that's that. We are ready to perform string comparisons accurately. Compile our compiler, and let's test it.

## Testing String Comparisons
Let’s take a slightly different approach to testing this time. There are a large number of test cases. Let us put all of them into a file, and compile that file. Our compiler already this capability. So, type the following into a file called **teststrcomp.txt**. Make sure there are no blank lines in the file - not even at the end. An empty line is not a valid expression - our compiler will reject it, and thus the whole file.

```
"Testing string comparisons"
"--------------------------"
"Testing ="
"jar=JAR"
"jar"="JAR"
"jar=jar"
"jar"="jar"
"--------------------------"
"Testing >"
"jar>jar"
"jar">"jar"
"jar>JAR"
"jar">"JAR"
"JAR>jar"
"JAR">"jar"
"--------------------------"
"Testing <"
"JAR<JAR"
"JAR"<"JAR"
"JAR<jar"
"JAR"<"jar"
"jar<JAR"
"jar"<"JAR"
"--------------------------"
"Testing >="
"jar>=jar"
"jar">="jar"
"jar>=JAR"
"jar">="JAR"
"JAR>=jar"
"JAR">="jar"
"--------------------------"
"Testing <="
"JAR<=JAR"
"JAR"<="JAR"
"JAR<=jar"
"JAR"<="jar"
"jar<=JAR"
"jar"<="JAR"
"--------------------------"
"Testing <>"
"jar<>jar"
"jar"<>"jar"
"jar<>b"
"jar"<>"b"
"--------------------------"
"Testing different-sized strings"
"JAR< j"
"JAR"<"j"
"ja>JAR"
"ja">"JAR"
"ja < jar"
"ja" < "jar"
"--------------------------"
"Testing string expressions"
"raj = r & aj"
"raj" = "r" & "aj"
"ra & j = r & a & j"
"ra" & "j" = "r" & "a" & "j"
"--------------------------"
"Testing empty string"
"empty string > something"
"" > "something"
"empty string < something"
"" < "something"
```

Run:

```
Compiler.exe teststrcmp.txt
```

If all goes well, our compiler will produce **teststrcmp.exe**. Run that.

```
teststrcmp.exe
```

The output should look like this:

```
Testing string comparisons
--------------------------
Testing =
jar=JAR
False
jar=jar
True
--------------------------
Testing >
jar>jar
False
jar>JAR
True
JAR>jar
False
--------------------------
Testing <
JAR<JAR
False
JAR<jar
True
jar<JAR
False
--------------------------
Testing >=
jar>=jar
True
jar>=JAR
True
JAR>=jar
False
--------------------------
Testing <=
JAR<=JAR
True
JAR<=jar
True
jar<=JAR
False
--------------------------
Testing <>
jar<>jar
False
jar<>b
True
--------------------------
Testing different-sized strings
JAR< j
True
ja>JAR
True
ja < jar
True
--------------------------
Testing string expressions
raj = r & aj
True
ra & j = r & a & j
True
--------------------------
Testing empty string
empty string > something
False
empty string < something
True
```

## Conclusion
In this chapter, we have begun to process Boolean expressions involving both numbers and strings. Unlike a lot of languages, our parser can correctly generate code for all kinds of comparisons between strings, albeit using the ordinal method of string comparison. In the next chapter, we will extend our parser to understand complex Boolean expressions, involving logical operators such as AND, OR and NOT.