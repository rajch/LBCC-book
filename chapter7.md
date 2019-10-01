# Variables

## Introduction

Over the last few chapters, we have created a compiler which is capable of parsing (and generating executable code for) various kinds of expressions. In the last chapter, we made our compiler capable of understanding statements, which are instructions that expect and act on such expressions. However, one important element is missing from our concept of expressions: the _variable_.

## Variables

Simply put, a variable is a name that represents a value. Wherever a value can be used, a variable can be substituted. Here are some examples using our own expression syntax:

```sic
2 + x
"Hello " & Myname
```
The assumption in the above examples is that a variable called `x` represents a numeric value, and a variable called `Myname` represents a string. 

How are variables associated with values? The most common way is to perform an operation called _assignment_, although there may be other methods. Most programming languages support a construct similar to the following:

```pseudocode
  <variable> = <value>
```
Once a value has been assigned to a variable, the variable can be used in any expression where the value could have been used. The advantage is that the same expression can be re-used with varying values. Hence the name: variable.

## Types

In practice, variables get a lot more complex. To begin with, variables have _types_. In our example, the variable `x` is of the numeric type, and `Myname` is of string type.

When is the type of a variable determined? There are two common approaches.  In the first approach, we can determine the type of a variable at run time, when it is actually used. This is called _dynamic typing_, and is commonly used in interpreters and so-called "scripting" languages. Examples of languages that use dynamic types are Perl, Python, Ruby and Visual Basic.

In the second approach, the type of a variable is declared as part of writing a program, and is checked by the compiler as part of the compiling process. This is called +static typing_, and is the more commonly used approach in compiled languages. Examples of languages that use static typing are C, Pascal, ML, and Visual Basic.

A variable of a given type can be used in any expression of that type. But what if we try to mix types in expressions, for instance by using a string variable in a numeric expression? Consider the following example, which uses our own `Print` command and expression syntax:

```sic
 Myname = "Raj"
 Myage = 32
 Print "My name is " & Myname & " and my age is " & MyAge 
```

Will this work?

Some languages do not allow expressions which combine different types. Such languages require an explicit conversion from one type to another if mixing of types is required. This kind of language is said to be _strongly typed_. Examples of strongly typed languages are Haskell, ML, Pascal, C++ (this is disputed by some), and Visual Basic. 

Other languages are not so strict. They convert variables of different types implicitly at run time. Such languages are said to be _weakly typed_. Examples of weakly typed languages are JavaScript ~~(I guess one should say ECMAScript instead)~~, Perl and Visual Basic.

Finally (and this will be the topic of a future chapter), apart from the kind of data stored by a variable, its type also refers to how such data is stored. For example, a variable can store a single value (scalar), multiple values (array or vector), or just a reference to a value stored elsewhere (pointer or reference).

SIC, the language that we are designing, will be statically and strongly typed.  

It is a common feature (though not a strict requirement) of statically typed languages that variables are declared before they are used. SIC will have this feature.

## Scope

Variables also have scope, which means an enclosing context. Think of scope as being a part of a program, such as a function or a block within a function. The whole program is also a valid scope.

The easiest way to understand the use of scope is this: within the same scope, no two variables can have the same name. But the same name can be used in different scopes, such as in different functions.

Scopes have important connotations for variables, and as such will be the topic of a future chapter. For now, we will assume that all variables that we compile will have a single scope: the whole program. This means that we cannot repeat or re-declare variables.

## Goal

Our goal in this chapter is threefold.

1. Recognize and translate a statement which _declares_ variables, that is, specifies a variable's name and data type. This statement will be in one of the following two forms: `Dim <variable> As <type>` or `<type> <variable>`. `Dim` is a command. Possible types are `Integer`, `String` and `Boolean`.
2. Recognize and translate an _assignment statement_, which will associate values with variables. This statement will be in one of the following two forms: `<variable> = <value>` or `<variable> := <value>`.
3. Allow the use of variables in any expressions, in lieu of constants.

## The Approach

In terms of scanning and parsing, there doesn't seem to be anything new in these goals. In fact, we already have most of the necessary scanners and recognizers in place. Types, variables, and the `Dim` command are _names_, for which we already have a scanner.

We do need some new parsers, though. And our ever-changing line parser needs to change again, because now a line can be any one of the following:

* a declaration statement
* an assignment statement
* a command

But before we get to that, there's that all-important question: how do variables translate down to CIL?

## First Step

Once again, the fact that we are compiling to the CLR helps us. The CLR natively understands the concept of variables. 

The CLR actually understands three kinds of variables: _locals_, _fields_, and _arguments_. As of right now, we need to look only at local variables, which are variables that are declared and used within a single scope. 

To work with variables, we will need to use a class called `LocalBuilder` in the Reflection Emit namespace, and two new Opcodes: `ldloc` and `stloc`.

## Modifying CodeGen

Add the following code to **CodeGen.vb**.

```vbnet
Private m_LocalVariables As _
      New Dictionary(Of Integer, LocalBuilder)

Public Function DeclareVariable( _
					Name As String, _
					VariableType As System.Type _
				) As Integer 
 
  Dim lb As LocalBuilder 

  lb = m_ILGen.DeclareLocal(VariableType)
	m_LocalVariables.Add(lb.LocalIndex, lb)

  Return lb.LocalIndex
End Function
```

So far, all our code generation has consisted of calling the `Emit` method of the field `m_ILGen`, which is an object of type `System.Reflection.Emit.ILGenerator`. This time, we are doing something different - we're _declaring_ a variable. This does not generate any code immediately. Instead, the `DeclareLocal` method of `m_ILGen` returns a `LocalBuilder` object. The `LocalBuilder` keeps track of a local variable's type, and its _index_. The index of a variable is its position in the scope where it is declared. The first variable declared in a scope will have an index or 0, the second, 1 and so on.

So, we have created a function called `DeclareVariable`, which takes a name and a type. It uses the type to create a `LocalBuilder`, and stores it in a dictionary using the index as the key. It then returns the index. Notice that we are not doing anything with the name, yet.

The CLR, in fact, does not care about names of variables at all. It just needs index numbers and types, which the `LocalBuilder` objects keep track of for us.

Now, we need to actually emit code that uses these local variables. As mentioned earlier, this means using two new OpCodes.

|OpCode|What it does|
|---|---|
|StLoc|Takes a variable index as a parameter. Pops the last value off the stack, and stores it in that variable. If the types do not match, there is a run-time error.|
|LdLoc|Takes a variable index as a parameter. Retrives the value stored in that variable, and pushes it on to the stack.|

Add the following to **CodeGen.vb**.

```vbnet
Public Sub EmitStoreInLocal( _
             Index As Integer _
		   )
	
	Dim lb As LocalBuilder 
 
	lb = m_LocalVariables(Index)
	m_ILGen.Emit(OpCodes.Stloc, lb)

End Sub

Public Sub EmitLoadLocal( _
             Index As Integer _
		   )
	
	Dim lb As LocalBuilder 
 
	lb = m_LocalVariables(Index)
	m_ILGen.Emit(OpCodes.Ldloc, lb)

End Sub
```
Pretty straightforward. Either method receives an Index, retrieves the corresponding `LocalBuilder` from the dictionary, and uses that with the relevant Opcode. 

That's all CodeGen requires to deal with variables. Now, we move on to the parsing part, which is significantly more difficult.

## The Symbol Table

To begin with, while variable names are not important to code generation, they are certainly important during parsing. We will encounter variable names at various places during the parsing stage. It is up to us to remember what the name represents, and generate appropriate code. 

This is different from anything else we have parsed so far. Up until now, our approach has been to generate code for whatever we parse immediately. And this has not been a problem, because the meaning of anything we parse has been static. For instance, as long as we are parsing a numeric expression, if we meet the number 11 anywhere, all we need to do is call `EmitNumber` in CodeGen.

We can't do that with variable names. If we find the name in a declaration statement, we have to call `DeclareVariable`. If it appears in an assignment statement, we have to call `EmitStoreInLocal`. If we find it in an expression, `EmitLoadLocal` is to be called. The concept is not called _variable_ for nothing.

Here's another twist. Variable names can appear within expressions, right? The question is: what type of expression? Or more correctly, what type of variable can appear in what type of expression? The answer, of course, is that the types of the variable and the expression should be the same.  Who ensures this? Not CodeGen. Look at the implementation of `EmitLoadLocal`. Does it do any kind of type checking? It's up to the parser to ensure that the correct variable gets used in the correct type of expression.

If we do generate code that uses the wrong type of variable in a given expression, the results can be unpredictable. By and large, the fact that the CLR can verify code generated by us will protect us from crashing the whole machine. But, recall what we said in Chapter 1. The preferred way of dealing with errors is _not producing them_. We have to ensure type correctness while compiling. Fortunately, we made the decision to make SIC a strongly and statically typed language. This will make ensuring type correctness a bit easier.

As per our first goal, when we declare a variable, we also declare what type it is. We will need to keep track of the variable's name and type (among other things). We will do this by maintaining a table of variables. When we parse a declaration statement, we will add the variable's name and type to this table. When a variable appears in an expression, we will look it up in the table, and check whether its type matches the type of the expression. If not, we will produce a compile-time error. We will also produce an error if a variable name used in an expression is not found in the table, thus enforcing the rule "A variable has to be declared before it can be used".

Variables are one kind of names that can be used in lieu of constants; examples of other kinds are arrays and functions. Names that represent values are collectively referred to as _symbols_ in compiler construction arcana. So, we need to build a _symbol table_ into our parser. 

Add the following to **Utilities.vb**.

```vbnet
Public Class Symbol
    Private m_Name As String = ""
    Private m_Type As Type = Nothing
    Private m_CodeGenHandle As Integer = -1 
 
    Public Sub New(newname as String, newtype As Type)
        m_Name = newname
        m_Type = newtype
    End Sub

    Public ReadOnly Property Name() As String
        Get
            Return m_Name
        End Get        
    End Property 
 
    Public ReadOnly Property Type() As Type
        Get
            Return m_Type
        End Get
    End Property 
 
    Public Property Handle() As Integer
        Get
            Return m_CodeGenHandle
        End Get
        
        Set(ByVal Value As Integer)
            m_CodeGenHandle = Value
        End Set
    End Property
End Class

Public Class SymbolTable
    Private m_symbolTable As New Dictionary(Of String, Symbol)

    Private Sub Add(symbol As Symbol)
        m_SymbolTable.Add( _
                        symbol.Name.ToLowerInvariant(), _
                        symbol
        )
    End Sub

    Public Function Fetch(name as String) As Symbol
        Return m_SymbolTable(name.ToLowerInvariant())
    End Function

    Public Function Exists(name as string) As Boolean
        Return m_SymbolTable.ContainsKey(name.ToLowerInvariant())
    End Function
End Class
```

The class called `Symbol` represents a single symbol, storing its name, type and _handle_. The handle is the CodeGen representation of a symbol, which is the index of the local variable.

The class called `SymbolTable` simply maintains a dictionary of symbols.

Next, add the following to **Parser.vb**, in the Fields section:

```vbnet
Private m_SymbolTable As New SymbolTable
```

## Types

Symbols are the second kind of _names_ that we have to work with (Commands were the first). In our goals above, we have defined a third kind: _types_.

Type names will appear in source code, specifically in declaration statements. We will need to validate them, and convert them to the compiler-internal representation, which is `System.Type`. Let's write some boilerplate code for doing this. 

The approach we will take is similar to commands: have a table of valid type names and their `System.Type` equivalients.

Add the following to **Commands.vb**.

```vbnet
Private m_typeTable As Dictionary(Of String, Type)

Private Sub AddType(typeName As String, type As Type)
    m_typeTable.Add(typeName.ToLowerInvariant(), type)
End Sub

Private Function IsValidType(typeName As String) As Boolean
    Return m_typeTable.ContainsKey(typeName.ToLowerInvariant())
End Function

Private Function GetTypeForName(typeName as String) As Type
    Return m_typeTable(typeName.ToLowerInvariant())
End Function

Private Sub InitTypes()
    m_typeTable = new Dictionary(Of String, Type)

    ' Add types here
    AddType("integer", GetType(System.Int32))
    AddType("string", GetType(System.String))
    AddType("boolean", GetType(System.Boolean))
End Sub
```

Put the fields in the Fields region, and the methods in the Helper Functions region.

We need to call the `InitTypes` method that we just created before doing any parsing. Like in the case of commands, we will call it from the constructor of the Parser class. Make the change in **Parser.vb**.

```vbnet
Public Sub New( _
    ByVal newStream As TextReader, _
    ByVal newGen As CodeGen _
    )

    m_InputStream = newStream
    m_Gen = newGen

    InitTypes()
    InitCommands()
End Sub
```

With all that infrastructure in place, we are finally ready to do some parsing.

## Parsing Declaration

Goal 1 above gives us two syntaxes for a declaration statement. Here's the BNF:

```bnf
<declarationstatement>     ::= <dimcommand>|<typefirstdeclaration>
<dimcommand>               ::= "Dim" <name> ["As"] <type>
<type>                     ::= "integer"|"string"|"boolean"
<typefirstdeclaration>     ::= <type> <name>
```

I have made one small enhancement; the word "As" in optional in the \<dimcommand> production.

`Dim` is a command, like others we have already parsed. So let's write a parser for it.

Add the following to **Commands.vb**, in the Commands region.

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
            ' Read either "as" or type
            SkipWhiteSpace()
            ScanName()

            ' Check and ignore "As"
            If CurrentToken.ToLowerInvariant() = "as" Then
                ' Read type
                SkipWhiteSpace()
                ScanName()
            End If

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

                If Not EndOfLine Then
                    result = CreateError(1, "end of statement.")
                Else
                    result = CreateError(0, "Ok")
                End If
            End If
        End If
    End If

    Return result
End Function
```

It reads pretty much like the BNF. Scan a variable name, then "As", and then a type name, with appropriate validations. If all is well, create a symbol, call `DeclareLocal` in CodeGen, and add the symbol to the symbol table.

Note the new error value of 3. If a variable name has already been declared, we do not allow re-declaration. We will have to modify `CreateError` to accomodate the new error. We will do that in a bit. For now, let us add the `Dim` command to the list of valid commands. In fact, let me make an enhancement here: we can use `Var` as a synonym for `Dim`.

Add the following to the `InitCommands` method in **Commands.vb**.

```vbnet
AddCommand("dim", AddressOf ParseDimCommand)
AddCommand("var", AddressOf ParseDimCommand)
```

And that's how easy it is to add a synonym to one of our commands; just use the same command parser.

Now, what about the second syntax, shown as \<typefirstdeclaration\> in the BNF above?

A parser is easy: just follow the BNF. Assume that a type name has been read, and proceed from there. Add the following to **Parser.vb**.

```vbnet
Private Function ParseTypeFirstDeclaration() _
                    As ParseStatus
    Dim result As ParseStatus

    Dim typename As String
    typename = CurrentToken

    If IsValidType(typename) Then
        ' Read a variable name
        SkipWhiteSpace()
        ScanName()

        If TokenLength = 0 Then
            result = CreateError(1, "a variable name.")
        Else
            Dim varname As String
            varname = CurrentToken

            If m_SymbolTable.Exists(varname) Then
                result = CreateError(3, varname)
            Else
                Dim symbol As New Symbol( _
                                    varname, _
                                    GetTypeOfName(typename)
                )

                symbol.Handle = m_Gen.DeclareVariable(
                                    symbol.Name,
                                    symbol.Type
                                )

                m_SymbolTable.Add(symbol)

                SkipWhiteSpace()

                If Not EndOfLine Then
                    result = CreateError(1, "end of statement.")
                Else
                    result = CreateError(0, "Ok")
                End If
            End If
        End If
    Else
        result = CreateError(1, "a valid type.")
    End If

    Return result
End Function
```

As things stand, this parser is not called from anywhere. We will deal with that, and the error value 3, shortly. For now, let us work on Goal 2.

## Parsing Assignment

Goal 2 gives us the syntax for an assignment statement. Here's the BNF:

```bnf
<assignmentstatement>         ::= <name> <assignmentoperator> <expression>
<assignmentoperator>          ::= "="|":="
```

We have to validate that the variable \<name> exists, and that the type of <\expression> matches the type of the variable. We already have most of the plumbing required, except a scanner for the assignment operator. Let's write that first.

Add the following to the appropriate regions of **Parser.vb**:

```vbnet
Private Function IsAssignmentCharacter(Byval c As Char) As Boolean
    Return ":=".IndexOf(c) > -1
End Function

Private Sub ScanAssignmentOperator()
    m_CurrentTokenBldr = New StringBuilder
    Do While IsAssignmentCharacter(LookAhead)
        m_CurrentTokenBldr.Append(LookAhead)
        m_CharPos += 1
    Loop

    Select Case CurrentToken
        Case "=", ":="
            ' Valid Assignment operator
        Case Else
            m_CurrentTokenBldr = New StringBuilder
    End Select
End Sub
```

With that in hand, let's write the parsers.

Add the following to **Parser.vb**:

```vbnet
Private Function ParseAssignment(variable As Symbol) _
                    As ParseStatus

    Dim result As ParseStatus
    
    SkipWhiteSpace()
    
    Select Case variable.Type.ToString()
        Case "System.Int32"
            result = ParseNumericExpression()
        Case "System.String"
            result = ParseStringExpression()
        Case "System.Boolean"
            result = ParseBooleanExpression()
        Case Else
            result = CreateError(1, " a valid type.")
    End Select 

    If result.Code = 0 Then
        ' Generate assignment code
        m_Gen.EmitStoreInLocal(variable.Handle)
    End If

    Return result
End Function

Private Function ParseAssignmentStatement() _
                    As ParseStatus

    Dim result As ParseStatus
    
    Dim varname As String
    varname = CurrentToken

    If m_SymbolTable.Exists(varname) Then
        ' Read assignment operator
        SkipWhiteSpace()
        ScanAssignmentOperator()

        If TokenLength = 0 Then
            result = CreateError(1, "= or :=")
        Else
            Dim variable As Symbol
            variable = m_SymbolTable.Get(varname)

            result = ParseAssignment(variable)
        End If
    Else
        result = CreateError(4, varname)
    End If

    Return result
End Function
```

We have split the job of assignment into two. `ParseAssignmentStatement` checks the validity of the statement itself, starting from the first token read, which should be a name that exists in the symbol table. This should be followed by an assignment operator. If this much is valid, `ParseAssignment` is invoked, passing it the symbol. Note the new error code, 4, which signals that a variable was not declared. 

`ParseAssignment` checks the type of the symbol, and invokes the matching expression parser. Any type errors would be caught by code we have already written. If there are no errors, the last bit of code generated would have pushed the result of the expression on the stack. So all we need to do is emit the StLoc OpCode (via `EmitStoreInLocal` of CodeGen), which will pop the value and store it in the relevant variable.


## A New Line (Again)

Now that we have parsers for declaration and assignment, we need someone to call them. That someone, inevitably, is `ParseLine`.

In the last chapter, we had defined a line to be one of our commands. Now, a line can either be a command (that includes the `Dim` command, which declares a variable), a type-first declaration, or an assignment statement. Here's the BNF:

```bnf
<line>               ::= <command>|<typefirstdeclaration>|<assignmentstatement>
```

So here's what `ParseLine` will do. We will scan a name. If the name is a valid command, we will invoke `ParseCommand`. If it is a valid type name, we will invoke `ParseTypeFirstDeclaration`. In all other cases, we will invoke `ParseAssignmentStatement`.

Make the change in **Parser.vb**.

```vbnet
Private Function ParseLine() As ParseStatus
    Dim result As ParseStatus
    
    SkipWhiteSpace()
    
    m_LastTypeProcessed = Nothing
    
    If EndOfLine() Then
        ' An empty line is valid
        result = CreateError(0, "Ok")
    Else
        ScanName()

        If TokenLength = 0 Then
            result = CreateError(1, "a statement.")
        Else
            Dim name As String
            name = CurrentToken

            If IsValidCommand(name) Then
                result = ParseCommand()
            ElseIf IsValidType(name) Then
                result = ParseTypeFirstDeclaration()
            Else
                result = ParseAssignmentStatement()
            End If
        End If
    End If

    Return result
End Function
```

## Error, Error On The Wall

We have introduced two new error values: 3 for variable already declared and 4 for variable not declared. In fact, we will need one more shortly, for variable type mismatch. We have to modify `CreateError` accordingly. Make the change in **Parser.vb**.

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

I took this opportunity to add an enhancement to `CreateError`. Note the use of the variable `errorpos` above. When we return an error via `ParseStatus`, we also return the column and line of source code where the error occurs. Most of our errors are detected _before_ we scan a token. In these cases, the error column position is the current scan position (`m_CharPos`), plus 1 because `m_CharPos` counts from 0. But some, like "Not in block" and the variable related errors, can only be detected after a token has been read. In these cases, the error column position has to be moved back by the length of the token. That's what gets done in the code above.

## Test It

Let's test what we have so far. You may want to review the instructions in the [Development Environment](/the-development-environment.md) chapter.

Compile with:

```bash
vbc /out:sicc.exe Compiler.vb Parser.vb Commands.vb CodeGen.vb Utilities.vb
```

Run using:

```bash
sicc.exe
```

Test it with the following code:

```sic
Dim x As Integer
x = 2*(3+56) /4
String y
y := "Hello world"
Var z As Boolean
Z := 1=2
```

 Also try re-declaring a variable, or not declaring a variable before using it, or assigning a wrong type of expression. Our compiler will report the error accurately.
 
 ## Using variables

Now that we are done with variable declaration and assignment, it's time for Goal 3: using variables in lieu of constants.

So far, SIC understands two types of constants: number and string. We parse them at the lowest levels of the relevant expression parsers. That is where we have to add the ability to use variables as well.

But first, we need a method to parse a variable, of any type. Add the following to **Parser.vb**:

```vbnet
Private Function ParseVariable(type As Type) _
                    As ParseStatus

    Dim result As ParseStatus

    ' Try to read variable name
    ScanName()
    
    If TokenLength = 0 Then
        result = CreateError(1, "a variable.")
    Else
        Dim varname As String
        varname = CurrentToken

        If Not m_SymbolTable.Exists(varname) Then
            result = CreateError(4, varname)
        Else
            Dim variable As Symbol
            variable = m_SymbolTable.Fetch(varname)

            If Not variable.Type.Equals(type) Then
                result = CreateError(5, variable.Name)
            Else
                ' Emit the variable
                m_Gen.EmitLoadLocal(variable.Handle)
                SkipWhiteSpace()
                    
                result = CreateError(0, "Ok.")
            End If
        End If
    End If 

    Return result
End Function
```

This parser should be invoked at a point where we definitely expect a variable. One such point can be found at the 'lowest' level of numeric expression parsing: `ParseFactor`.

## Using variables in numeric expressions

In `ParseFactor`, we currently check if the lookahead character is a "(", in which case we call `ParseNumericExpression`. Otherwise, we call `ParseNumber`. We need to add a third possibility: if the lookahead character is a _name start_ character (a letter or an underscore), we need to process the next token as a number-type variable. Let's do this now.

Make the change in **Parser.vb**, and add the new recognizer in the appropriate region.

```vbnet
Private Function IsNameStartCharacter(Byval c As Char) As Boolean
    Return c.Equals("_"c) OrElse Char.IsLetter(c)
End Function

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
    ElseIf IsNameStartCharacter(LookAhead) Then
        result = ParseVariable(GetType(System.Int32))
    Else
        result = ParseNumber()
    End If

    SkipWhiteSpace()

    Return result
End Function
```

## Test Numeric Variables
Let's test this. Compile and run. Test with the following:

```sic
Dim i As Integer
Integer j
i = 39
j := i + 2
Print 3+j 
```

You can try arbitrarily complex numeric expressions, in the assignment as well as the print statements. The assignment statement will work in all cases. The `Print` command will work in all cases, _except when the expression starts with a variable_. More on that later.

## Using variables in string expressions

We could deal with strings in a similar fashion, if we had a string "factor". Unfortunately, our string expression parser directly calls `ParseString`, with no possibility of a string being anything other than a literal, quoted string. Let's introduce a more flexible parsing structure, similar to numbers. This will also allow us to introduce brackets in a string expression. Here is the modified BNF for string expressions.

```bnf
<stringexpression>        ::= <stringfactor><concatoperation>*
<concatoperation>         ::= <concatoperator><stringfactor>
<concatoperator>          ::= "+"|"&"
<stringfactor>            ::= <string>|<name>|<bracketstringexpression>
<bracketstringexpression> ::= "(" <stringexpression> ")"
<string>                  ::= <quote-symbol><stringcharacter>*<quote-symbol>
<quote-symbol>            ::= '"'
<stringcharacter>         ::= <character>|<quote-in-string>
<quote-in-string>         ::= <quote-symbol><quote-symbol>
<character>               ::= ? every character other than double-quote or newline ?
```

Make the changes in **Parser.vb**.

```vbnet
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

Private Function ParseStringFactor() As ParseStatus
    Dim result As ParseStatus

    If LookAhead.Equals(""""c) Then
        result = ParseString()
    ElseIf IsNameStartCharacter(LookAhead) Then
        result = ParseVariable(GetType(System.String))
    ElseIf LookAhead.Equals("("c) Then
        SkipCharacter()

        result = ParseStringExpression()

        If result.Code = 0 Then
            If Not LookAhead.Equals(")"c) Then
                result = CreateError(1, ")")
            Else
                SkipCharacter()
            End If
        End If
    Else
        result = CreateError(1, "a string.")
    End If

    Return result
End Function

Private Function ParseConcatOperator() As ParseStatus
    Dim result As ParseStatus
    Dim currentoperator As String = CurrentToken
    
    SkipWhiteSpace()
    
    result = ParseStringFactor()
    
    If result.code = 0 Then
        m_Gen.EmitConcat()
    End If
    
    Return result
End Function	

Private Function ParseStringExpression() As ParseStatus
            
    Dim result As ParseStatus
    
    result = ParseStringFactor()
    
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

    m_LastTypeProcessed = Type.GetType("System.String")

    Return result	
End Function
```

Reads just like the BNF. Note the call to `ParseVariable` in the appropriate place, passing the string type.

## Test String Variables

Compile and run. Test with the following:

```sic
Dim myname As String 
myname = "Paula"
String greet
greet := myname & ", you brillant person."
print "Hello, " + greet
```

Again, you can try arbitrarily complex expressions. The assignment will work in all cases. The Print command will work in all cases, _except when the expression starts with a variable_. 

If you try to mix types, such as using a string variable in a numeric expression, or trying to assign a number to a string variable, you will get a very accurate error message.

## Using variables in Boolean expressions - not yet
Variables in boolean expressions are quite a challenge. Unlike numbers and strings, we cannot easily deal with them at the "factor" level. The Boolean "factor" is a condition, which in itself starts with an expression, which may start with a variable. We cannot, with any certainty, assume that the variable at that point _must_ be a boolean.

So, boolean variables, like boolean expressions, require special treatment in **ParseExpression** itself. The good news is that this treatment may solve the pesky error that we encoutered whenever an expression _starts with a variable_.

## In the beginning, there was a name
Our **ParseExpression** method detects the type of the expression by the lookahead character. A digit or sign indicates a numeric expression, and a quote(") indicates a string expression. If the lookahead character is an underscore(_) or a letter, we can assume that the next token is a name. But is it the name of a variable? 

There is a valid token which can appear at the beginning of an expression and looks like a name: the `not` operator. If it is, we are in a boolean expression. Fortunately, we know what happens at this point of a boolean. We can simply call **ParseNotOperator**.

If the expression genuinely begins with a variable, we have a problem. The type of variable tells us which expression parser to call. We can't figure out the type until we scan the full token. However, our string and numeric parsers insist on scanning the first token of the expression themselves. There's no convenient way to let them know that we have already scanned the first token, which happens to be a variable. 

How do our assignment statements work in every case, then? Well, in the case of the assignment statement, by the time we get to the expression, we already know what type it is likely to be, from the variable on the left of the assignment operator. So, we call the appropriate expression parser, and everything works.

## Backtracking
We have hit a roadblock. So far, we could reliably predict what was coming by looking at a single Lookahead character. We can't do that any longer. How do we solve this problem?

The correct way would be to revisit our expression parsers, and add a way for them to allow for a pre-parsed first token. It would involve passing the pre-parsed tokem, or some kind of flag, to the top of the expession parser hierarchy, and passing it down all the way to the "factor" level.  Our boolean parser already has something like this. 

There is a less correct, but shorter way. Once we have scanned a variable which starts an expression and determined its type, we can move the lookahead character back to the start of the variable, and call
the relevant expression parser. This does cause the variable to be scanned twice, but ensures correct output without massive changes in our parsers. Moving the position of the lookahead back in this manner is known as _backtracking_.

Backtracking is frowned upon, and rightly so. Let's try and understand why, in simple terms, using
our work so far as an example.

Why is backtracking necessary at all? Because of ambiguity. Consider our implementation of **ParseExpression**. An expression can be numeric, string or boolean, and we can usually tell which kind a particular one is by looking at the first character. Things are unambiguous as long as variables are not involved. The moment an expression starts with a name character, we can't tell what kind it is until we have read the variable and looked it up in the symbol table. Thus, we don't know how the rest of the expression has to be processed. The expression type is ambiguous. Only after resolving the ambiguity can we
proceed, but by then it is too late. So we backtrack.

Luckily, in our compiler, there are very few situations where there can be ambiguity regarding what comes next, and therefore where backtracking must occur. The most obvious one is when we call **ParseExpression**, and the Lookahead character is a name character. We call ParseExpression in two places so far: in the `Print` command, and in **ParseCondition**. Consider, each time we use the print statement and start the expression with a variable, the variable will be scanned twice. Furthermore, each time we use a boolean condition which starts with a variable (as most boolean expressions will), the variable will be scanned twice. As the programs that our compiler compiles get larger, this can cause performance problems for the compiler (Note: for the compiler only. Compiled code will run just fine.)!

Also, in our situation, we are lucky. No code has been generated at the point where we need to backtrack. There might be situations where code was generated, and then backtracking became necessary. We would have to somehow un-generate the code!

Backtracking should be avoided, and can be avoided with a little extra work. However, for the moment, we will
backtrack. 

One little aside: one of the most common places where backtracking is required is for reporting compile-time errors. We have been sort-of backtracking, in our **CreateError** method. Check out the way we handle all error cases other than case 1. Because we "sort-of" backtrack, and more importantly because we only ever report one compile-time error,this is not likely to affect the performance of our compiler.

## Implementing the backtrack
Add the following to **Parser.vb**, in the Scanner region:

```vbnet
Private Sub Backtrack()
    If TokenLength > 0 Then
        m_CharPos -= TokenLength
        m_CurrentTokenBldr = New StringBuilder()
    End If
End Sub
```
This should be self-explanatory.

## ParseInitialName
Next, we will create a new parser to handle the case of a name appearing at the beginning of an expression. Add the following to **Parser.vb**:

```vbnet
Private Function ParseInitialName() As ParseStatus
    Dim result As ParseStatus

    ' Scan the name
    ScanName()

    Dim varname As String
    varname = CurrentToken 

    If CurrentToken.ToLowerInvariant() == "not" Then
        ' This is a boolean. Backtrack and call
        ' boolean parser
        Backtrack()
        result = ParseBooleanExpression()
    Else
        If Not m_SymbolTable.Exists(varname) Then
            result = CreateError(4, varname)
        Else
            Dim variable As Symbol
            variable = m_SymbolTable.Fetch(varname)

            Select Case variable.Type.ToString()
                Case "System.Int32"
                    ' Backtrack, and call numeric parser
                    Backtrack()
                    result = ParseNumericExpression()
                Case "System.String"
                    ' Backtrack, and call string parser
                    Backtrack()
                    result = ParseStringExpression()
                Case Else
                    result = CreateError(1, " an Integer or String variable.")
            End Select

        End If
    End If

    Return result
End Function
```

This scans a name, and checks to see if the token is `not`. If so, it could have called **NotOperator** as discussed. But since we have decided to use backtracking anyway, it simply backtracks and calls the boolean parser. If the scanned name is not `not` (Sorry), it checks to see if is a variable, and backtracks and calls the appropriate parser. Note that we have not yet handled boolean variables.

## One more way to express yourself
Finally, we need to change **ParseExpression** to call **ParseInitialName** where appropriate. Modify it in **Parser.vb** as follows:

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
    ElseIf IsNameStartCharacter(LookAhead) Then
        result = ParseInitialName()
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

Notice how the **IsNameStartCharacter** check happens before **IsNotOperator**. This has to be so. Think about it for a minute.

## Test Variable-first Expressions

Compile and run. Test with the following:

```sic
integer x
x = 12
print x+2
integer y
y = x * 2
print y
print y<x
print y>x
string z
z := "Raj"
print z
print z & " Chaudhuri"
print z=="Raj"
print z>="Raj"
Dim myname As String 
myname = "Paula"
String greet
greet := myname & ", you brillant person."
print greet + " Hello."
```

All of these should work, including variables in boolean expressions. Which leaves us with one last problem: _boolean variables_. 

## Using variables in Boolean expressions - finally
For integers and strings, we parse variable names in the "factor" level of the relevant expression parser. As discussed earlier, the boolean "factor" is basically the condition. The BNF goes like this:

```bnf
<booleanfactor>              ::= <condition>|<bracketedbooleanexpression>
<bracketedbooleanexpression> ::= "("<booleanexpression>")"

<condition>                  ::= <expression><reloperator><expression>
<reloperator>                ::= "="|"=="|"==="|"<>"|"!="|"!=="|
                                 "<"|"<="|"=<"|">"|">=","=>"
```

As you can see, the condition itself is made up of expressions, which may well start with a variable. This variable could be integer or string, or boolean. This is the only place where we can detect a boolean variable with certainty.

So, the only place where we can parse a boolean variable (and decide what to do next) is in **ParseInitialName** itself. The most convenient place would be . Modify **ParseInitialName** as follows, in **Parser.vb**:

```vbnet
Private Function ParseInitialName() As ParseStatus
    Dim result As ParseStatus

    ScanName()

    Dim varname As String
    varname = CurrentToken 

    If CurrentToken.ToLowerInvariant() = "not" Then
        ' This is a boolean. Backtrack and call
        ' boolean parser
        Backtrack()
        result = ParseBooleanExpression()
    Else
        If Not m_SymbolTable.Exists(varname) Then
            result = CreateError(4, varname)
        Else
            Dim variable As Symbol
            variable = m_SymbolTable.Fetch(varname)

            Select Case variable.Type.ToString()
                Case "System.Int32"
                    ' Backtrack, and call numeric parser
                    Backtrack()
                    result = ParseNumericExpression()
                Case "System.String"
                    ' Backtrack, and call string parser
                    Backtrack()
                    result = ParseStringExpression()
                Case "System.Boolean"
                    ' Backtrack, parse variable because boolean factor
                    ' can't, then continue as a boolean expression
                    Backtrack()
                    result = ParseVariable(GetType(Boolean))
                    If result.Code = 0 Then
                        result = ParseBooleanExpression(GetType(Boolean))
                    End If
                Case Else
                    result = CreateError(1, " an Integer or String variable.")
            End Select

        End If
    End If

    Return result
End Function
```

Note the added case for a boolean type variable. If the name just scanned is a boolean, we backtrack and call **ParseVariable** with the boolean type parameter, which will take care of emitting it on the stack. And then we call **ParseBooleanExpression** with a parameter, which lets it know that the first token in that expression has already been parsed. 

Now, we need to add some more checks in the boolean parse tree. The first is that if a _condition starts with a boolean variable, the relational operator is optional_. A boolean variable by itself is a valid condition.

Change **ParseCondition** as follows, in **Parser.vb**:

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
            ' If the last type processed was a Boolean,
            ' a relational operator is not required
            If Not lastexpressiontype Is Nothing Then 
                If Not lastexpressiontype.Equals( _
                                            GetType(Boolean) _ 
                                        ) Then
                    result = CreateError(1, "a relational operator.")
                End If
            End If
        Else
            result = ParseRelOperator()
            SkipWhiteSpace()
        End If
    End If

    Return result
End Function
```

Next, the case where a relational operator does follow a boolean variable. The only relational operators allowed for booleans are equality and inequality. We can check for this in **ParseRelOperator**. Modify it in **Parser.vb** as follows:

```vbnet
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
    ElseIf conditiontype.Equals( _
            Type.GetType("System.Boolean") _
        ) Then
        ' Boolean conditions support only equality or
        ' inequality comparisons
        Select Case reloperator
            Case "=","==","===", "<>", "!=", "!=="
                result = ParseBooleanExpression()
            Case Else
                result = CreateError(1, "and = or <> operator")
        End Select
        
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

Finally, if a relation operator has been used with a boolean variable, we need to emit the relevant CIL. Luckily, the opcodes are the same as Integers. Change **GenerateRelOperation** in **Parser.vb** as follows:

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
    ElseIf conditionType.Equals( _
            Type.GetType("System.Boolean")
        ) Then

        Select Case reloperator
            Case "=", "==", "==="
                m_Gen.EmitEqualityComparison()
            Case "<>", "!=", "!=="
                m_Gen.EmitInEqualityComparison()
        End Select
    End If
End Sub
```

And now boolean variables are ready to be tested.

## Test Boolean Variables

Compile and run. Test with the following:

```sic
var xx as boolean
var yy as boolean
xx = 1=1
yy = 1=2
print "xx"
print xx
print "yy"
print yy
print "xx = yy"
print xx = yy
print "xx = xx"
print xx = xx
print "xx and yy"
print xx and yy
print "xx or yy"
print xx or yy
print "not xx"
print not xx
```

As usual, you can try arbitrarily complex expressions.

## More ambiguity
As if things were not complicated enough already. There is one more place where there can be ambiguity based on the fact that we are parsing variables now: the _NotOperation_.

Recall the definition from Chapter 5. Here's the BNF for referemce:

```bnf
<notoperation>               ::= <booleanfactor>|<notoperator>
<notoperator>                ::= <notsymbol><booleanfactor>
<notsymbol>                  ::= "NOT"|"not"|"!"
```

As you can see, a not-operation is either a boolean "factor", or a NOT operator followed by a boolean factor. Now, a NOT operator is either the symbol ! (no problems here) or the word NOT, uppercase or lowercase(oops!).

See the problem? At the start of any boolean factor, any variable starting with N, O, or T can cause the parser to try to scan a NOT operator.

Try it. Try to compile the following with our compiler:

```sic
Integer n
n:=42
Boolean m
m = n=42
```

You should get an "Expected NOT" error.

The problem comes from the fact that we use a single lookahead character to predict what comes next. This can cause ambiguity in situations such as this one. The exact cause of our current problem is this:

* A boolean expression may begin with a variable or a NOT operator
* Both of which may begin with the letter n

This problem could have been avoided if we did predictive parsing using a lookahead _token_ instead of a lookahead character. But we are too far in to bring in such a big change. So, we will cheat a bit. For the first time, we will look more than one character ahead in a recognizer method; specifically **IsNotOperator**.

Rename the existing **ReadNotOperator** method to **ReadNotOperatorSymbol**, and enter the new one shown below. Make the change in the Recognizers region in **Parser.vb**.

```vbnet
Private Function IsNotOperatorSymbol(ByVal c As Char) As Boolean
    Return "nNoOtT!".IndexOf(c) > -1
End Function

Private Function IsNotOperator(Byval c As Char) As Boolean
    Dim result As Boolean = False

    If c = "!"c Then
        result = True
    ElseIf Char.ToLowerInvariant(c) = "n"c AndAlso _
        m_LineLength >= m_CharPos+2 Then

        Dim peektoken As String
        peektoken = m_ThisLine.Substring(m_CharPos,3).ToLowerInvariant()
        If peektoken = "not" Then
            result = True
        End If
    End If

    Return result
End Function
```

Finally, modify **ScanNotOperator** to use **ReadNotOperatorSymbol** instead of **ReadNotOperator**. Make the changes in the Scanner region in **Parser.vb**.

```vbnet
Private Sub ScanNotOperator()
    m_CurrentTokenBldr = New StringBuilder

    Do While IsNotOperatorSymbol(LookAhead)
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
```

Compile and run. This time, the code which failed earlier should compile properly.

## Declare and assign
At this point, we have successfully implemented variables in our compiler. We can stop now, but I want to add a few extras.

Most modern languages allow us to declare a variable and assign it an initial value in the same statement. Thus, our language should be able to correctly process lines like the following:

```sic
Dim x As Integer = 42
String str := "Superstring"
```

Let's add this capability.

## Options, options
We have a situation where one of our constructs can be parsed in two ways. A declaration statement can be just a declaration, or an assignment as well.

This is not as difficult as it seems. Once we finish our regular processing of declaration statements, we just need to check if another token exists, and is an assignment operator. In our case, this simply means checking to see if the lookahead character is an assignment operator. If it is, we need to process the assignment. Let's first process the assignment by adding the following new method, **ParseDeclarationAssignment**, to the Parser class. Add it in **Parser.vb**.

```vbnet
Private Function ParseDeclarationAssignment(ByVal variable As Symbol) _
                                                        As ParseStatus
    Dim result As ParseStatus
    
    If IsAssignmentCharacter(LookAhead) Then
        ScanAssignmentOperator()

        If TokenLength = 0 Then
            result = CreateError(1, "= or :=")
        Else
            result = ParseAssignment(variable)
        End If
    Else
        result = CreateError(0, "Ok")
    End If

    Return result
End Function
```

This scans an assignment operator if available. On a successful scan, it calls the **ParseAssignment** method to parse the right side of the operator.

Note that if an assignment operator is not found, **ParseDeclarationAssignment** returns a successful result. This is how we deal with the fact that the assignment part is optional.

Now all that remains is to add a call to this in our two declaration parsers: **ParseTypeFirstDeclaration** and **ParseDimCommand**. Make the changes in **Parser.vb** and **Commands.vb** respectively, as shown below:

```vbnet
Private Function ParseTypeFirstDeclaration() _
                    As ParseStatus
    Dim result As ParseStatus

    Dim typename As String
    typename = CurrentToken

    If IsValidType(typename) Then
        ' Read a variable name
        SkipWhiteSpace()
        ScanName()

        If TokenLength = 0 Then
            result = CreateError(1, "a variable name.")
        Else
            Dim varname As String
            varname = CurrentToken

            If m_SymbolTable.Exists(varname) Then
                result = CreateError(3, varname)
            Else
                Dim symbol As New Symbol( _
                                    varname, _
                                    GetTypeForName(typename)
                )

                symbol.Handle = m_Gen.DeclareVariable(
                                    symbol.Name,
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
    Else
        result = CreateError(1, "a valid type.")
    End If

    Return result
End Function

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
            ' Read either "as" or type
            SkipWhiteSpace()
            ScanName()

            ' Check and ignore "As"
            If CurrentToken.ToLowerInvariant() = "as" Then
                ' Read type
                SkipWhiteSpace()
                ScanName()
            End If

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
```

Compile and run. Test with the following:

```sic
integer y:=2
dim x as string = "Hello"
dim a as integer
a = y+2
String b = "World"
Dim greetfirst As String := x & " "
String fullgreeting = greetfirst + b
Print fullgreeting
```