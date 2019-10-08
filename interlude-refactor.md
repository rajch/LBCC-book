# Interlude - A Little Refactoring

Over eight chapters, our code has become a little unwieldy. I want to make a few (mostly cosmetic) changes, to bring things back under some sort of control.

## Goal

In this chapter, we want to:

* Put a more formal interface around the current token. No more manipulating the `m_CurrentTokenBldr` and `m_CharPos` variables directly.
* Create a formal method for parsing and discarding "noise" words like `As` and `Then`. Currently, we have repeated code in multiple methods, and that irks me.
* Implement better error management. We use tables for commands, types and loops: why manage errors in a huge `Select` statement?

## The Approach

We don't want to introduce any new functionality in this chapter. But since we are refactoring anyway, we will introduce a few more sanity checks in places that need them.

I will not show all changes "in-camera" in this chapter. When there is a change that spans multiple methods or files, I'll just tell you what has been done.

## Get Yer Token Here

Replace the entire Scanner region in **Parser.vb** with the following:

```vbnet
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

Private ReadOnly Property CurrentLine() As Integer
    Get
        Return m_linePos
    End Get
End Property

Private ReadOnly Property CurrentPosition() As Integer
    Get
        Return m_CharPos
    End Get
End Property

Private Function PeekAhead(ByVal count As Integer) As String
    Dim result As String
    Dim charcount As Integer = m_LineLength - m_CharPos

    If charcount <= 0 Then
        result = ""                    
    Else
        If charcount > count Then charcount = count
        result = m_ThisLine.Substring(m_CharPos,charcount)
    End if

    Return result
End Function

Private Sub SkipWhiteSpace()
    Do While IsWhiteSpace(LookAhead)
        If EndOfLine() Then
            Exit Do
        Else
            SkipCharacter()
        End If
    Loop
End Sub

Private Sub SkipRestOfLine()
    m_CharPos = m_LineLength
End Sub

Private Sub Backtrack()
    If TokenLength > 0 Then
        m_CharPos -= TokenLength
        ResetToken()
    End If
End Sub

Private Sub AppendToToken()
    m_CurrentTokenBldr.Append(LookAhead)
    m_CharPos += 1
End Sub

Private Sub ResetToken()
    m_CurrentTokenBldr = New StringBuilder()
End Sub

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

Private Sub ScanNumber()
    ResetToken()

    Do While m_CharPos < m_LineLength
        If Not IsNumeric(LookAhead) Then
            Exit Do
        End If
        AppendToToken()
    Loop
End Sub

Private Sub ScanMulOrDivOperator()
    ResetToken()

    If IsMulOrDivOperator(LookAhead) Then
        AppendToToken()
    End If
End Sub

Private Sub ScanAddOrSubOperator()
    ResetToken()

    If IsAddOrSubOperator(LookAhead) Then
        AppendToToken()
    End If
End Sub

Private Sub ScanString()
    m_EmptyStringFlag = False
    ResetToken()

    If Not LookAhead.Equals(""""c) Then
        Exit Sub
    End If

    Do While LookAhead.Equals(""""c)
        SkipCharacter()
        Do While Not LookAhead.Equals(""""c)

            If EndOfLine Then
                ResetToken()
                Exit Sub
            End If

            AppendToToken()
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

Private Sub ScanConcatOperator
    ResetToken()
    
    If IsConcatOperator(LookAhead) Then
        AppendToToken()
    End If	
End Sub

Private Sub ScanRelOperator
    ResetToken()
    
    Do While IsRelOperator(LookAhead)
        AppendToToken()
    Loop

    Select Case CurrentToken
        Case "=","==", "===", "<>", "!=", "!==", ">", "<", ">=", "=>","<=","=<"
            ' Valid relational operator
        Case Else
            ResetToken()
    End Select
End Sub

Private Sub ScanNotOperator()
    ResetToken()

    Do While IsNotOperatorSymbol(LookAhead)
        AppendToToken()
    Loop

    Select Case CurrentToken.ToLowerInvariant()
        Case "not", "!"
            ' Valid NOT operator
        Case Else
            ResetToken()
    End Select
End Sub

Private Sub ScanAndOperator()
    ResetToken()

    Do While IsAndOperator(LookAhead)
        AppendToToken()
    Loop

    Select Case CurrentToken.ToLowerInvariant()
        Case "and", "&"
            ' Valid AND operator
        Case Else
            ResetToken()
    End Select
End Sub

Private Sub ScanOrOperator()
    ResetToken()

    Do While IsOrOperator(LookAhead)
        AppendToToken()
    Loop

    Select Case CurrentToken.ToLowerInvariant()
        Case "or", "|"
            ' Valid OR operator
        Case Else
            ResetToken()
    End Select
End Sub

Private Sub ScanName()
    ResetToken()
    Do While IsNameCharacter(LookAhead)
        AppendToToken()
        If EndOfLine Then
            Exit Do
        End If
    Loop
End Sub

Private Sub ScanAssignmentOperator()
    ResetToken()
    Do While IsAssignmentCharacter(LookAhead)
        AppendToToken()
    Loop

    Select Case CurrentToken
        Case "=", ":="
            ' Valid Assignment operator
        Case Else
            ResetToken()
    End Select
End Sub
```

This is mostly a rearrangement, ensuring that there is no repeated code. There are a few new methods, notably **SkipRestOfLine** and **PeekAhead**. These will allow methods outside the Scanner section to play with the current character position without using the m_CharPos field.

To use the new Scanner functions, change **IsNotOperator** and **ParseCommand** methods in **Parser.vb**, as follows:

```vbnet
Private Function IsNotOperator(ByVal c As Char) As Boolean
    Dim result As Boolean = False

    If c = "!"c Then
        result = True
    ElseIf Char.ToLowerInvariant(c) = "n"c Then
        If PeekAhead(3).ToLowerInvariant() = "not" Then
            result = True
        End If
    End If

    Return result
End Function

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
            SkipRestOfLine()
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

Also change **ParseRemCommand** in Commands.vb:

```vbnet
Private Function ParseRemCommand() As ParseStatus
    ' Ignore the rest of the line
    SkipRestOfLine()
    Return CreateError(0, "Ok")
End Function
```

With these changes, the Scanner region is almost its own class. I'm tempted to just make it so, but will resist for now.

## Cut Out The Noise

Next, a formal method for parsing and ignoring noise words. This will replace the ad hoc code in `Dim` command parser (noise word: `As`) and the `If` and `ElseIf` parsers (noise word: `Then`).

Add the following to **Parser.vb**:

```vbnet
Private Function ParseNoiseWord( _
                    ByVal word As String,
                    Optional ByVal silent As Boolean = True
                ) As ParseStatus

    Dim result As ParseStatus = CreateError(0, "Ok")

    If Not EndOfLine Then 
        ' If the next token is the noise word
        ' followed by a whitespace, skip it silently.
        ' If it isn't, raise and error if required, 
        ' but don't move the current position.

        Dim peekby As Integer = word.Length + 1
        Dim peektoken As String = PeekAhead(peekby)

        word = word.ToLowerInvariant()
        peektoken = peektoken.ToLowerInvariant()

        If peektoken.StartsWith(word) _
                AndAlso _
            (
                word.Length = peektoken.Length _
                    OrElse _
                IsWhiteSpace( _
                    peektoken.Chars(peektoken.Length-1) _ 
                ) _
            ) Then

            ' The token matches, and the next character
            ' is empty or whitespace. Scan and ignore
            '  the word
            ScanName()
            SkipWhiteSpace()

        Else
            If Not silent Then
                result = CreateError(1, word)
            End If
        End If
    End if
    
    Return result
End Function

Private Function ParseLastNoiseWord( _
                    ByVal word As StringBuilder _ 
                ) As ParseStatus

    Dim result As ParseStatus

    ' The noise word should be the last word in
    ' a line
    result = ParseNoiseWord(word, False)
    If result.Code = 0 Then
        If Not EndOfLine Then
            result = CreateError(1, "end of statement")
        End If
    End If

    Return result
End Function 
```

Next, change the **ParseDimCommand**, **ParseIfCommand** and **ParseElseIfCommand** methods in **Commands.vb**:

```vbnet
Public Function ParseDimCommand() As ParseStatus
    Dim result As ParseStatus

    ' Read a variable name
    SkipWhiteSpace()
    ScanName()

    If TokenLength = 0 Then
        result = CreateError(1, "a variable name.")
    Else
        Dim varname As String
        varname = CurrentToken

        If m_SymbolTable.Exists(varname) Then
            ' Variable name already declared
            result = CreateError(3, CurrentToken)
        Else
            SkipWhiteSpace()

            ' Check and ignore "As"
            ParseNoiseWord("As")

            ' Read type
            ScanName()

            Dim typename As String
            typename = CurrentToken

            If TokenLength = 0 OrElse _
                    Not IsValidType(typename) Then
                result = CreateError(1, "a valid type.")
            Else
                Dim symbol As New Symbol( _
                                    varname,
                                    GetTypeForName(typename)
                )

                symbol.Handle = m_Gen.DeclareVariable( _
                                            symbol.Name, _
                                            symbol.Type
                                )

                m_SymbolTable.Add(symbol)

                SkipWhiteSpace()

                result = ParseDeclarationAssignment(symbol)

                If result.Code = 0 Then
                    If Not EndOfLine Then
                        result = CreateError(1, "end of statement.")
                    End If
                End If
            End If
        End If
    End If

    Return result
End Function

Public Function ParseIfCommand() As ParseStatus
    Dim result As ParseStatus

    SkipWhiteSpace()
    result = ParseBooleanExpression()

    If result.Code=0 Then
        SkipWhiteSpace()

        result = ParseLastNoiseWord("Then")

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

            result = ParseLastNoiseWord("Then")

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

That should do it.

## Put Your Errors On The Table

Since all our table-driven stuff is in **Commands.vb**, let's move error handling there, too.

Delete the **CreateErrors** method from **Parser.vb**. Since that was the only method in the Helper Functions region, you might as well delete the region itself.

Next, add to following to the Fields and Helper Functions regions of **Commands.vb**, as appropriate:

```vbnet
Private m_ErrorTable As Dictionary(Of Integer, String)

Private Function CreateError( _
                    ByVal errorcode As Integer, _
                    ByVal errorDetail As String,  _
                    Optional ByVal beforeToken As Boolean = False
                ) As ParseStatus

    Dim result As ParseStatus
    Dim message As String

    Dim errorpos As Integer

    If beforeToken Then
        ' Some errors happen
        ' at the scan position
        errorpos = m_CharPos
    Else
        ' Others happen after a token
        ' has been scanned
        errorpos = m_CharPos - TokenLength
    End If

    ' Columns should be 1-based for reporting
    errorpos = errorpos + 1
    
    If m_ErrorTable.ContainsKey(errorcode) Then
        message = String.Format( _
                    m_ErrorTable(errorcode), _
                    errorDetail _
        )
    Else
        message = String.Format(
                    "Unknown error code {0}",
                    errorcode
        )
    End if


    result = New ParseStatus(errorcode, _
                message, _
                errorpos, _
                m_LinePos)


    Return result
End Function

Public Function BlockEnd() As ParseStatus
    Return CreateError(-1, "", True)
End Function

Private Function Ok() As ParseStatus
    Return CreateError(0, "", True)
End Function

Private Sub InitErrors()
    m_ErrorTable = New Dictionary(Of Integer, String)

    ' Add Error Numbers here
    m_ErrorTable.Add(-1,    "{0}")
    m_ErrorTable.Add(0,     "{0}")
    m_ErrorTable.Add(1,     "Expected {0}")
    m_ErrorTable.Add(2,     "{0}")
    m_ErrorTable.Add(3,     "Cannot redeclare variable '{0}'")
    m_ErrorTable.Add(4,     "Variable '{0}' not declared")
    m_ErrorTable.Add(5,     "Type mismatch for Variable '{0}'")
    m_ErrorTable.Add(6,     "'{0}' was unexpected at this time")
End Sub
```

The new **CreateError** method pulls the error description formatting from a table, which is initialized in the **InitErrors** method. In the future, adding a new error will mean adding one line to **InitErrors**.

The new **Ok** and **BlockEnd** methods provide a convenient way of writing error values (status codes) that are not really errors.

**InitErrors** needs to be called somewhere. We'll call it in the Parser constructor as usual. Modify it as follows in **Parser.vb**:

```vbnet
Public Sub New( _
    ByVal newStream As TextReader, _
    ByVal newGen As CodeGen _
    )

    m_InputStream = newStream
    m_Gen = newGen

    InitErrors()
    InitTypes()
    InitLoops()
    InitCommands()
End Sub
```

Finally, some hard work. Find all calls to the **CreateError** method where the first parameter is 0, and replace the call with **Ok()**. Do this in both **Commands.vb** and **Parser.vb**. After this, in the **ParseEndCommand** method in **Commands.vb**, find the call to the **CreateError** method where the first parameter is -1, and replace it with a call to **BlockEnd()**.

Compile, and test with all the exercises of the previous chapters. Everything should work exactly the same as before.

## Conclusion

We have refactored our code to be slightly more readable, and slightly easier to maintain. Only slightly, but it will keep us going for a chapter or two more at least. And now, on to new things.
