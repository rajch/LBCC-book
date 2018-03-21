# Booleans Part II

## Introduction

In the previous chapter, we gave our parser the ability to parse and translate Boolean conditions. In this chapter, we will extend it to recognize _complex conditions_ involving _logical operators_.

## Goal

Complex conditions are one or more conditions, combined with logical operators which affect the final value of these combinations. Common logical operators are:

|Operator|What it does|
|---|---|
|AND|Combines two conditions together, usually using the form `<condition> AND <condition>`. The combined expression evaluates to `true` if both the conditions are `true`, and `false` in every other case.|
|OR|Combines two conditions together, usually using the form `<condition> OR <condition>`. The combined expression evaluates to `true` if either one or both the conditions are `true`, and `false` otherwise.|
|NOT|Negates a single Boolean condition, using the form `NOT <condition>`. The combined expression is `false` if the condition is `true`, and vice versa.|

The condition, in each case, can be a simple condition or a complex one.

An expression can contain any permutation and combination of conditions and operator, such as `<condition> AND <condition> OR <condition> AND NOT <condition>`.

In processing such an expression, `NOT` takes precedence over `AND`, which takes precedence over `OR`. So, in the expression :

```psedocode
<condition 2B> OR NOT <condition 2> AND <condition B>
```

,first, `<condition 2>` will be evaluated and negated, then the result would be `AND`-ed (pardon the expression) with condition B, and finally the result of that will be `OR`-ed with `<condition 2B>`.

We will also allow for brackets to increase priority, as we did in the case of numeric expressions. So, in the expression:

```pseudocode
(<condition 2B> OR NOT <condition 2>) AND <condition B>
```

, first `<condition 2>` will be negated, then the result `OR`-ed with `<condition 2B>`, and then the result of that `AND`-ed with `<condition B>`.

## The Approach

The approach is similar to the one we took for mathematical expressions; we will deal with operator precedence by splitting the complex condition into smaller parts, like the terms and factors in Chapter 3. So, let us define these parts:

* _BooleanFactor_: As of right now, a _BooleanFactor_ is a _Condition_. We will expand this below.
* _NotOperation_: this can be two things; a _BooleanFactor_, or a _NotOperator_.
* _NotOperator_ this is the word `NOT` (uppercase or lowercase), or the symbol `!`, followed by a followed by a _BooleanFactor_.
* _AndOperation_: this is a _NotOperation_, followed by zero or more _AndOperator_s.
* _AndOperator_:  this is either the word `AND` (uppercase or lowercase), or the symbol `&`, followed by a _NotOperation_.
* _OrOperation_: this is an _AndOperation_, followed by zero or more _OROperator_s.
* _OrOperator_: this is either the word `OR` (uppercase or lowercase), or the symbol `|`, followed by an _AndOperation_.

As you can see, this is exactly the same as the way we process mathematical expressions. The only thing missing in the definition so far are the brackets, for precedence. We will handle these in the definition of _BooleanFactor_, by recursively calling the topmost level, _BooleanExpression_, just like we did in the case of factors in mathematical expressions. _BooleanExpression_ itself, currently equivalent to _Codndition_, needs to be modified to call the top of the complex condition recursive-descent hierarchy: _OrOperation_.

Here's the complete BNF for boolean expressions.

```bnf
<booleanexpression>          ::= <oroperation>

<oroperation>                ::= <andoperation><oroperator>*
<oroperator>                 ::= <orsymbol><andoperation>
<orsymbol>                   ::= "OR"|"or"|"|"

<andoperation>               ::= <notoperation><andoperator>*
<andoperator>                ::= <andsymbol><notoperation>
<andsymbol>                  ::= "AND"|"and"|"&"

<notoperation>               ::= <booleanfactor>|<notoperator>
<notoperator>                ::= <notsymbol><booleanfactor>
<notsymbol>                  ::= "NOT"|"not"|"not"

<booleanfactor>              ::= <condition>|<bracketedbooleanexpression>
<bracketedbooleanexpression> ::= "("<booleanexpression>")"

<condition>                  ::= <expression><reloperator><expression>
<reloperator>                ::= "="|"=="|"==="|"<>"|"!="|"!=="|
                             "<"|"<="|"=<"|">"|">=","=>"
```

## First Step

Before we parse any of the parts defined above, we have to enable CodeGen to emit logical operators. Let's start with that.

## Modifying CodeGen

There are two IL instructions which we need to use. These are:

|Opcode|What it does|
|---|---|
|And|Pops the last two values on the stack, performs an `AND` operation on them, and pushes the value back on the stack. An `AND` operation is a _bitwise_ operation, in which each corresponding binary digit of the two values are compared against the other, and the corresponding bit of the result is 1 if they both are 1, and 0 otherwise. This operation could be applied to any integer number. For example, 4 `AND` 6 evaluates to 4 (`100 AND 110` in binary evaluates to `100`), and 4 `AND` 2 evaluates to 0 (`100 AND 010` = `000`). Before using this opcode, we will ensure that the last two values pushed are 'Boolean' values; i.e.,  either 1 or 0. Thus the result will be 1 (`true`) if both values pushed are 1, and 0 otherwise, which is exactly what we want.|
|Or|Pops the last two values on the stack, performs an OR operation on them, and pushes the value back on the stack. `OR` is also a bitwise operation. Since we ensure that the opcode will be emitted only after two 'boolean' integer values, we will get the effect we want.|

For `NOT`, we use the technique we already have; we negate the last value on the stack by comparing it with 0; in other words, we call the `NegateComparison` method we have already written.

Add the following to **CodeGen.vb**:

```vbnet
Public Sub EmitBitwiseAnd()
    m_ILGen.Emit(OpCodes.And)
End Sub

Public Sub EmitBitwiseOr()
    m_ILGen.Emit(OpCodes.Or)
End Sub

Public Sub EmitLogicalNot()
    NegateComparison()
End Sub
```

## Parsing Complex Conditions

As we saw in the interlude chapter, BNF translates very nicely to parser code. The parts concerned with terminal symbols get handled by Is and Scan methods. Let's do those first.

Add the following to the appropriate parts of **Parser.vb**.

```vbnet
Private Function IsNotOperator(ByVal c As Char) As Boolean
    Return "nNoOtT!".IndexOf(c) > -1
End Function

Private Function IsAndOperator(ByVal c As Char) As Boolean
    Return "aAnNdD&".IndexOf(c) > -1
End Function

Private Function IsOrOperator(ByVal c As Char) As Boolean
    Return "oOrR|".IndexOf(c) > -1
End Function

Private Sub ScanNotOperator()
    m_CurrentTokenBldr = New StringBuilder

    Do While IsNotOperator(LookAhead)
        m_CurrentTokenBldr.Append(LookAhead)
        m_CharPos += 1
    Loop

    Select Case CurrentToken.ToLowerInvariant()
        Case "not", "!"
            ' Valid NOT operator
        Case Else
            m_CurrentTokenBldr = New StringBuilder
    End Select
End Sub

Private Sub ScanAndOperator()
    m_CurrentTokenBldr = New StringBuilder

    Do While IsAndOperator(LookAhead)
        m_CurrentTokenBldr.Append(LookAhead)
        m_CharPos += 1
    Loop

    Select Case CurrentToken.ToLowerInvariant()
        Case "and", "&"
            ' Valid AND operator
        Case Else
            m_CurrentTokenBldr = New StringBuilder
    End Select
End Sub

Private Sub ScanOrOperator()
    m_CurrentTokenBldr = New StringBuilder

    Do While IsOrOperator(LookAhead)
        m_CurrentTokenBldr.Append(LookAhead)
        m_CharPos += 1
    Loop

    Select Case CurrentToken.ToLowerInvariant()
        Case "or", "|"
            ' Valid OR operator
        Case Else
            m_CurrentTokenBldr = New StringBuilder
    End Select
End Sub
```

And now for the parsers. As you have probably guessed, CodeGen gets called after successfully parsing the Operators.

Add the following to the Parser region of **Parser.vb**.

```vbnet
Private Function ParseBooleanFactor( _
                    Optional lastexpressiontype As Type = Nothing _
    ) As ParseStatus

    Dim result As ParseStatus

    If LookAhead.Equals("("c) Then
        SkipCharacter()

        result = ParseBooleanExpression()

        If result.Code = 0 Then
            If Not LookAhead.Equals(")"c) Then
                result = CreateError(1, ")")
            Else
                SkipCharacter()
            End If
        End If
    Else
        result = ParseCondition(lastexpressiontype)
    End If

    SkipWhiteSpace()

    Return result
End Function

Private Function ParseNotOperator() _
    As ParseStatus

    Dim result As ParseStatus

    SkipWhiteSpace()

    result = ParseBooleanFactor()

    If result.Code = 0 Then
        m_Gen.EmitLogicalNot()
    End If

    Return result
End Function

Private Function ParseNotOperation( _
                    Optional lastexpressiontype As Type = Nothing _
    ) As ParseStatus

    Dim result As ParseStatus

    ' If lastexpressiontype has a value, it means we are halfway
    ' through parsing a boolean condition, and have just met a 
    ' relational operator. A NOT operator cannot appear at this
    ' point, so we just continue to parse a boolean factor
    If lastexpressiontype IsNot Nothing Then
        result = ParseBooleanFactor(lastexpressiontype)
    Else
        If IsNotOperator(LookAhead) Then
            ScanNotOperator()

            If TokenLength = 0 Then
                result = CreateError(1, _
                    "NOT")
            Else
                result = ParseNotOperator()
            End If
        Else
            result = ParseBooleanFactor()
        End If
    End If

    Return result
End Function

Private Function ParseAndOperator() _
        As ParseStatus

    Dim result As ParseStatus

    SkipWhiteSpace()

    result = ParseNotOperation()

    If result.Code = 0 Then
        m_Gen.EmitBitwiseAnd()
    End If

    Return result
End Function

Private Function ParseAndOperation( _
                    Optional lastexpressiontype As Type = Nothing _
    ) As ParseStatus

    Dim result As ParseStatus

    result = ParseNotOperation(lastexpressiontype)

    Do While result.Code = 0 And _
        IsAndOperator(LookAhead)

        ScanAndOperator()

        If TokenLength = 0 Then
            result = CreateError(1, _
                "AND")
        Else
            result = ParseAndOperator()
            SkipWhiteSpace()
        End If
    Loop

    Return result
End Function


Private Function ParseOrOperator() _
        As ParseStatus

    Dim result As ParseStatus

    SkipWhiteSpace()

    result = ParseAndOperation()

    If result.Code = 0 Then
        m_Gen.EmitBitwiseOr()
    End If

    Return result
End Function

Private Function ParseOrOperation( _
                    Optional lastexpressiontype As Type = Nothing _
    ) As ParseStatus

    Dim result As ParseStatus

    result = ParseAndOperation(lastexpressiontype)

    Do While result.Code = 0 And _
        IsOrOperator(LookAhead)

        ScanOrOperator()

        If TokenLength = 0 Then
            result = CreateError(1, _
                "OR")
        Else
            result = ParseOrOperator()
            SkipWhiteSpace()
        End If
    Loop

    Return result
End Function
```

Notice the optional parameter indicating the type of the last expression parsed, which gets passed right down the hierarchy to `ParseCondition`. As we saw in the last chapter, this is because we don't know that we are in a boolean expression until we hit the first relational or logical operator.

Next, we must change our definition of `ParseBooleanExpression`. Currently, it directly calls `ParseCondition`. We need to modify it to call the top level of the complex condition recursive-descent hierarchy, which is `ParseOrOperation`. Just like the BNF shows.

Make the change in **Parser.vb**.

```vbnet
Private Function ParseBooleanExpression( _
                    Optional lastexpressiontype As Type = Nothing _
    ) As ParseStatus

    Dim result As ParseStatus

    result = ParseOrOperation(lastexpressiontype)

    If result.Code = 0 Then
        m_LastTypeProcessed = _
            Type.GetType( _
            "System.Boolean")
    End If

    Return result
End Function
```

Finally, we need to change the very top and bottom of the boolean recursive-descent hierarchy, `ParseExpression` and `ParseCondition` respectively, to ensure that boolean expressions can be used at any level of nesting. We need to modify `ParseExpression` to ensure the following two things:

1. So far, we have defined that expressions can begin with either a numeric character (a digit or a + or - sign), or a string character (double quote). We will now also specify that an expression can begin with a NOT operator, in which case it is a boolean expression.
2. If `ParseExpression` identifies (via the lookahead character) the expression to be numeric or string, it calls the relevant parse function, and then checks if the lookahead character is a relational operator. This check should not be performed at all if `ParseExpression` has been called from `ParseCondition`, because in that case, `ParseCondition` itself will scan and parse the relational operator. For this change, we will also need to change `ParseCondition`.

Here are the changed `ParseCondition` and `ParseExpression` functions. Replace them in **Parser.vb**.

```vbnet
Private Function ParseCondition( _
                    Optional lastexpressiontype As Type = Nothing _
    ) As ParseStatus

    Dim result As ParseStatus

    If lastexpressiontype Is Nothing Then
        result = ParseExpression(Type.GetType("System.Boolean"))
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
    ElseIf IsNotOperator(LookAhead) Then
        result = ParseBooleanExpression()
    ElseIf LookAhead.Equals("("c) Then
        ' For now, assuming only numeric expressions can use ()
        result = ParseNumericExpression()
    Else
        result = CreateError(1, "a boolean, numeric or string expression")
    End If

    If result.Code = 0 Then
        If expressiontype Is Nothing Then
            ' If a relational operator follows the expression just parsed
            ' we are in the middle of a Boolean expression. Our work is
            ' not yet done.
            If IsRelOperator(LookAhead) Then
                ' Parse a boolean expression, letting it know that the
                ' first part has already been parsed.
                result = ParseBooleanExpression(m_LastTypeProcessed)
            End If
        End If
    End If

    Return result
End Function
```

## Testing

To test the large number of possibilities we have now enabled, we will once again take advantage of our compiler's ability to process whole files. Type the following into a file called **testcomplex.txt**. Make sure there are no blank lines in the file - not even at the end. A blank line is not a valid expression, and as such will be rejected by our compiler, which at this point parses expressions only.

```pseudocode
"Testing complex boolean conditions"
"----------------------------------"
"1=1"
1=1
"1=1 and 1=1"
1=1 and 1=1
"1=1 and 1=2"
1=1 and 1=2
"1=2 and 1=1"
1=2 and 1=1
"1=2 and 1=2"
1=2 and 1=2
"1=1 or 1=1"
1=1 or 1=1
"1=2 or 1=1"
1=2 or 1=1
"1=1 or 1=2"
1=1 or 1=2
"1=2 or 1=2"
1=2 or 1=2
"not 1=2"
not 1=2
"not 1=1"
not 1=1
"1=1 or 1=2 and 1=1"
1=1 or 1=2 and 1=1
"1=2 or 1=2 and 1=1"
1=2 or 1=2 and 1=1
"1=1 and not 1=2"
1=1 and not 1=2
"1=2 or not 1=1"
1=2 or not 1=1
"1=1 and not (1=2 or not 1=1)"
1=1 and not (1=2 or not 1=1)
```

Now, compile our compiler.  You may want to review the instructions in the [Development Environment](/the-development-environment.md) chapter.

Compile with:

```bash
vbc /out:Compiler.exe Compiler.vb Parser.vb CodeGen.vb ParseStatus.vb
```

Run using:

```bash
Compiler.exe testcomplex.txt
```

This will produce **testcomplex.exe**. When you run that, your output should look like this:

```pseudocode
Testing complex boolean conditions
----------------------------------
1=1
True
1=1 and 1=1
True
1=1 and 1=2
False
1=2 and 1=1
False
1=2 and 1=2
False
1=1 or 1=1
True
1=2 or 1=1
True
1=1 or 1=2
True
1=2 or 1=2
False
not 1=2
True
not 1=1
False
1=1 or 1=2 and 1=1
True
1=2 or 1=2 and 1=1
False
1=1 and not 1=2
True
1=2 or not 1=1
False
1=1 and not (1=2 or not 1=1)
True
```

## Caveat - Brackets

There is one subtle bug in our expression parser. Boolean expressions may have as many brackets as required, but an expression currently cannot start with a bracket. At the moment, `ParseExpression` assumes that brackets can begin only a numeric expression.

We will leave this bug in, for the moment, and formally correct it later. The reader is welcome to solve it as an exercise in the meantime.

## Note - Short Circuiting

As our complex conditions stand right now, simple conditions on both sides of a logical operator are evaluated in every case. However, most modern languages do not treat logical operators this way. Instead, they use a process called _short-circuiting_, which involves evaluating only as much of a complex condition as is necessary. For instance, if the condition on the left side of an `AND` operation is `false`, there is no need to check the one on the right; the complex condition is guaranteed to be `false`.

This can turn out to be quite an advantage in many situations. For example, the expression:

```pseudocode
1=2 AND 2=1/0
```

will cause a divide by zero run-time error in our current implementation. In an implementation which uses short-circuiting, the run-time error would never happen.

In fact, if you go by pure definitions, short-circuiting behavior is what is really called _logical_. The kind of behavior we have implemented is called _bitwise_. The only operator which currently behaves is a pure logical manner is `NOT`.

However, our current behavior works for our purposes. So, at the moment, purity be damned, we will use the current implementation. In a later chapter, we will add two more operators which will emit true logical `AND` and `OR` behavior.

## Conclusion

At this point, our compiler correctly processes boolean conditions of arbitrary complexity. However, comparing constants against one another is not very exciting. We already know the results. Booleans are really useful when they involve _variables_.

Variables will be introduced in Chapter 7. But just before that, we will take an important decision about the nature of the "language" that our compiler will compile, and introduce a subject which will implement that decision: statements.