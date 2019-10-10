# Debug Builds

## Introduction

Debugging has around as long as programming itself, and I suspect will be around for a
while after programming becomes obsolete. _Debuggers_, though, came later.

To state the obvious, a debugger is a program that can be used to debug other programs. At the very least, when a program crashes, a debugger should be able tell us something about the state of the machine when it crashed. For so called _low-level_ debuggers, this usually means the machine language instruction that was last executed, the current values in the machine’s registers (if the machine has registers), and the contents of memory. If the debugger is a _source-level_ or _symbolic_ debugger, like most modern ones, it should be
able to show the original line of source code in whatever language the program was written in, and also the names and current values of the variables used (as opposed to just a memory dump).

Modern debuggers provide many more facilities, such as running a program one instruction at a time (this is called _single stepping_), running a program until a marked point in source code (called a _breakpoint_) is reached and monitoring values of variables(called _watching_). A good debugger can be a very useful tool, not just for removing bugs, but also for understanding how a program works.

I’m sure you have realized that compiling for the CLR has many advantages. In this chapter, we see yet another advantage: the CLR has good built-in facilities for debuggers. In fact, there are several good debuggers already available for the CLR, and programs in any language (that has a properly
implemented compiler) can be debugged using any of them.

## Goal

The goal of this chapter is to enhance our compiler such that SIC programs can be debugged using a CLR debugger. ~~We will test with the two debuggers available in the .NET Framework SDK: `cordbg`, the command-line debugger, and DbgCLR, the GUI debugger.~~ It's 2019. Those debuggers don't exist any more. You can test with any debugger of your choice.

## The Approach

The approach in this chapter is quite different from recent ones. We will have to add all sorts of new capabilities to our CodeGen class to enable debugging. How is code generation relevant to debugging? Let’s find out.

Recall what we know about the target of our compiler. SIC compiles down to CIL instructions. Imagine for a moment that ultimately, what get executed are the CIL instructions.[^1] So, if we were to “single-
step” through our program, would each step be a CIL instruction? Or would the debugger somehow have to know that each “step” should correspond to one line of SIC, which may translate to multiple CIL instructions?

Some debuggers might choose to take the former approach. These are the so-called “low-level” debuggers. Usually, they work at the level of machine language instructions rather than CIL. Most modern debuggers are “source-level”, which means they take the second approach.

Here’s another related fact. Recall what we learned about variables in chapter 7. Variable names are important in parsing the source code, but not important at the CIL stage. In CIL, variables are numbered in the order of declaration. CIL instructions involving variables use these numbers. Now, when we use a debugger, we want to see the original names of the variables. How would the debugger correlate the variable
numbers that are available in CIL to the names used in the source language?

Our compiler needs to emit some extra information while emitting CIL. Specifically, it needs to somehow
record the information that a particular set of CIL instructions correspond to a particular line of SIC source code. Also, it needs to record the symbols (in our case, the variable names) from the source code, such that these names can be used by the debugger.

Luckily for us, the Reflection Emit library that we have been using to generate CIL has these facilities built-in. By setting some parameters and calling a few extra methods, we will be able to produce an executable file which contains some extra debug information along with the actual CIL. In the process, we will also produce a file with the extension .pdb or .mdb, which will contain additional information that cannot readily be built into the EXE itself, such as the symbols. An executable file with debug information and an accompanying .pdb or .mdb file is known as a debug build.

## First Step

Producing debug builds using Reflection Emit is fairly easy. In chapter 1, we saw that to generate an executable, we had to create an Assembly via the AssemblyBuilder class, create a module inside that using a ModuleBuilder object, create a class (type) inside the module using an instance of the TypeBuilder class, create a method in that class using a MethodBuilder object, and finally generate code inside the method by using an ILGenerator object. For generating a debug build, the following steps are required:

1. When a module is being created, we need to specify that all code generated for the module needs to emit the relevant symbols as well.
2. We have to specify a _symbol writer_ for the module. This is an additional repository of information that gets written into the .pdb (or .mdb) file mentioned above. It specifies the location of the source code that the module was written in, the actual source language, and the vendor of said language.
3. When we emit the declaration of a variable, we need to emit the symbol information as well. The name of the variable as recognized in the source code gets written to the .pdb (or .mdb) file.
4. As we parse each line of source code, we need to mark a _sequence point_ in the resulting CIL. A sequence point is something like a label. It indicates that all CIL instructions after itself until the next sequence point correspond to a single “line” of source code. When a debugger is being used to single-step through the code, it will perform all CIL instructions after a sequence point till the next sequence point as a single
step.

The first three steps are all modifications to CodeGen. Let’s do that first.

## Modifying CodeGen

Add the following to **CodeGen.vb**. Place the `Imports` statement with the other imports, and the rest inside the CodeGen class.

The constructor that is shown here replaces the existing constructor. We need to pass in some additional information that is useful for debug builds.

```vbnet
Imports System.Diagnostics.SymbolStore

Private m_debugBuild As Boolean
Private m_symbolStore As ISymbolDocumentWriter

Public ReadOnly Property DebugBuild() As Boolean
	Get
		Return m_debugBuild
	End Get
End Property

Public Sub New( _
		ByVal sourceFileName As String, _
		ByVal outputFileName As String, _
		ByVal assemblyName As String, _
		ByVal debugBuild As Boolean _
	)

	m_SaveToFile = outputFileName
	m_debugBuild = debugBuild

	' Compiling a CLR language produces an assembly.
	' An assembly has one or more modules.
	' Each module has one or more types:
	' 				(structures or classes)
	' Each type has one or more methods.
	' Methods are where actual code resides.

	' Create a new assembly. An assembly must have a name.
	' An assembly's name consists of up to four parts:
	' a simple name, a four-part version number, the default
	' Culture of the assembly, and optionally a public key token.
	' At the moment, we will only set the simple name. The
	' version number will default to 0.0.0.0, and the culture
	' will default to neutral.
	Dim an As New assemblyName()
	an.Name = assemblyName

	m_producedAssembly = _
		AppDomain.CurrentDomain.DefineDynamicAssembly( _
							an, AssemblyBuilderAccess.Save _
		)
		
	' In the newly created assembly, create a
	' module with the same name.
	' The last parameter of DefineDynamicModule
	' indicates whether debug information will
	' be emitted for this assembly
	m_producedmodule = m_producedAssembly.DefineDynamicModule( _
										assemblyName, _
										outputFileName, _
										m_debugBuild _
	)

	' If this is a debug build, create a symbol
	' store for the module.
	If m_debugBuild Then
		m_symbolStore = m_producedmodule.DefineDocument( _
										sourceFileName, _
										Nothing, _
										Nothing, _
										SymDocumentType.Text _
		)
	End If

	' In the newly created module, create a class called
	' "MainClass".

	m_producedtype = m_producedmodule.DefineType("MainClass")

	' In MainClass, create a Shared (static) method
	' with Public scope, called "MainMethod".
	m_producedmethod = 	m_producedtype.DefineMethod(
									"MainMethod", _
									MethodAttributes.Public _
										Or _
									MethodAttributes.Static, _
									Nothing, _
									Nothing _
	)

	' All IL code that we produce will be contained
	' in MainMethod.
	m_ILGen = m_producedmethod.GetILGenerator()
End Sub
```
There are three important changes here. To begin with, we pass the source file name, the target file name, the assembly name, and a flag indicating whether a debug build should be generated. So far, we had only been passing the target file name. Why are we passing the extra parameters?

When a source-level debugger is used to debug the applications that we compile, it will need to display the source code. There are two choices available: we can write the complete source code into the symbol store (as mentioned above, this is a file with the .pdb or .mdb extension. The debugger needs access to the executable and the symbol store file to work properly), or we can simply tell the symbol store that the source code can be found in a particular source file. The Reflection Emit library encourages the second option. Thus, we need to pass in the name of the source file.

Every CLR assembly has a name that is used to identify it. As the comments in the code above state, this name can consist of up to four parts. So far, we had not given much importance to this, and every assembly we compiled was called MainAssembly. We cannot be so laissez-faire about this forever. So we’ll make a start towards formalizing assembly names in this chapter, and set the simple name part of the assembly to a non-hard-coded value. Expect to hear much more about this in a future chapter.

We start the creation of an assembly by calling **DefineDynamicAssembly**, and create a module inside it by calling **DefineDynamicModule**. The third parameter to **DefineDynamicModule** tells the Reflection Emit library whether debug information is to be emitted. If it is set to `True`, appropriate information will be written to the symbol store.

The symbol store itself needs to be set up, and we do that by calling the **DefineDocument** method on the newly created ModuleBuilder object. The parameters for **DefineDocument** are, in order, are:

* A URL pointing to a file which contains the source code for this module. We will pass the name of the source file, without any path specification, to this parameter. At the time of debugging, the debugger will look for a file with that name in the same location as the executable file.
* A value specifying the programming language of the source code. Certain languages are recognized by the symbol store, including Basic, C# and COBOL. In our case, we pass `Nothing`.
* A value specifying the vendor of the programming language. The symbol store system recognizes some vendors of programming languages. Okay, it recognizes one vendor by default: Microsoft. In our case, we pass `Nothing`.
* A value specifying the "kind" of the file passed in the first parameter. The constant **SymDocumentType.Text** identifies it as a text document.

A corresponding change needs to be made to the **Save** method in the same class. This gets called after all code in a source file has been parsed successfully. The **Save** method actually saves the assembly, after marking the single method that we create as the starting point or _entry point_ of the assembly. One additional thing needs to be done for debug builds.

Make the change in **Commands.vb**, as follows:

```vbnet
Public Sub Save()
	' Emit a RETurn opcode, which is the last
	' opcode for any method
	m_ILGen.Emit(OpCodes.Ret)

	' Actually create the type in the module
	m_producedtype.CreateType()

	' Specify that when the produced assembly
	' is run, execution will start from
	' the produced method (MainMethod). Also, the
	' produced assembly will be a console
	' application.
	m_producedAssembly.SetEntryPoint( _
		m_producedmethod, _
		PEFileKinds.ConsoleApplication
	)

	' If this is a debug build, set the User
	' Entry Point, which is the point where a
	' debugger will start stepping
	If m_debugBuild Then
		Dim isMono As Boolean = _
			Type.GetType("Mono.Runtime") IsNot Nothing

		If Not isMono Then
			m_producedmodule.SetUserEntryPoint( _
									m_producedmethod _
			)
		End If
	End If

	m_producedAssembly.Save(m_SaveToFile)
End Sub
```

As stated in the comment, we specify a user entry point (the point where a debugger starts stepping into the code) if this is a debug build. If we miss this step, some debuggers will not be able to properly single-step through our compiled programs.

Notice the check using the variable called isMono? That is required because the Mono implementation of the Reflection Emit library does not implement **ModuleBuilder.SetUserEntryPoint**. This is unfortunate, as it will mean that we will have to manually invoke the debugger before our SIC programs can be debugged. But apart from that, everything else remains the same as other CLR implementations such as the .NET Framework.[^2]

Next, make the following change to the **DeclareVariable** method in **Commands.vb**.

```vbnet
Public Function DeclareVariable( _
					Name As String, _
					VariableType As System.Type _
				) As Integer 

	Dim lb As LocalBuilder 

	lb = m_ILGen.DeclareLocal(VariableType)
	m_LocalVariables.Add(lb.LocalIndex, lb)

	If m_debugBuild Then
		lb.SetLocalSymInfo(Name)
	End If

	Return lb.LocalIndex
End Function
```

You may recall that when we first created the **DeclareVariable** method in Chapter 7, I had pointed out
that we were not doing anything with the variable name. At that point, we had no use for the name, because CIL deals with the variable's index. Now, for debug builds only, we provide the variable name to the symbol store by calling the **SetLocalSymInfo** method on the newly create LocalBuilder object.

Finally, add the following method to the CodeGen class in **Codegen.vb**:

```vbnet
Public Sub EmitSequencePoint( _
				ByVal startLine As Integer, _
				ByVal startColumn As Integer, _
				ByVal endLine As Integer, _
				ByVal endColumn As Integer _
			)

	m_ILGen.MarkSequencePoint( _
					m_symbolStore, _
					startLine, _
					startColumn, _
					endLine, _
					endColumn _
			)

End Sub
```
This will allow us to place a sequence point in the CIL that we generate. A sequence point, as mentioned above, is somewhat like a label, but useful for debugging only. 

A sequence point appears just before a set of CIL instructions. The corresponding symbol information states that the instructions from this sequence point up to the next one represent one statement in source code. This source code statement starts at a particular line and column and ends at a particular line and column in the source code file pointed at by our symbol store. 

We will call this method from our parsers where relevant.

## Changing the Compiler

Now that we have changed the constructor for our CodeGen class, we need to change the class that invokes it as well. Make the following changes to the **Main** method of the Compiler class, in **Compiler.vb**.

```vbnet
Public Function Main(Byval CmdArgs() As String) As Integer
    Dim reader As TextReader
    Dim gen As CodeGen

    Dim status As ParseStatus
    Dim parser As Parser

    Console.WriteLine("Compiler for CLR")
    
    If CmdArgs.Length=0 Then
        reader = Console.In
        gen = New CodeGen( _
                    "",
                    "Test.exe", _
                    "Test", _
                    False
        )
    Else
        If File.Exists(CmdArgs(0)) Then
            Dim finfo As New FileInfo(CmdArgs(0))
            
            reader = New StreamReader( _
                        finfo.FullName)
            
            gen = new CodeGen( _
                    finfo.FullName, _
                    finfo.Name.Replace( _
                        finfo.extension, _
                        ".exe" _
                    ), _
                    finfo.Name.Replace( _
                        finfo.Extension, _
                        "" _
                    ), _
                    True _
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
    
    Return status.Code
End Function
```

If there is no command-line argument to our compiler, we create a CodeGen object passing an empty source file name, “Test.exe” as the target file name, “Test” as the assembly name, and no debug information. If there is an argument, and it is the name of a valid file, we create a CodeGen object with the full source file name, the same name but with the extension replaced with .exe as the target file name, the same name with the extension removed as the assembly name, and debug information. We borrow a trick from Visual Studio here, and set the assembly name to the name of the target file, minus the .EXE extension.

So, at the moment, if we compile a file containing source code, a debug build will be generated.

## Changing the Parser

Now, all that remains is to emit sequence points at appropriate locations. The place to do that is the Parser class.

Sequence points are use to mark the start an end of statements. In the SIC language as defined so far, a statement is a line, and a line can be one of the following:

* a command
* a type-first declaration
* an assignment statement

We need to emit sequence points in the relevant parsers.

Now, there are certain lines where we should not emit sequence points. These are:

* Variable declarations, unless they also have an assignment.
* Lines containing the REM command
* Entire COMMENT blocks
* Lines containing ELSE or DEFAULT statements
* Lines containing REPEAT or LOOP statements
* END statements, except in the case of END REPEAT
* Lines containing CASE or ELSEIF statements

The reason is that none of these, except the last, actually generate any CIL code. The statements in the last case, too, do not generate CIL that exactly corresponds to that line. An explanation of the previous sentence can be found below.

But before all that, how do we actually emit the sequence point?

Add to following to the **Helper Functions** section in **Parser.vb**:

```vbnet
Private Sub GenerateSequencePoint()
    ' If this is a debug build
    If m_Gen.DebugBuild Then
        ' Generate a sequence point that corresponds
        ' to the current source line. Line numbers in
        ' the symbol store are 1-based, and column
        ' numbers are 0-based. We have to pass the
        ' starting and ending lines and columns
        ' in the source for the statement. We always
        ' pass the current line, and the first and
        ' last column of the current line.
        m_Gen.EmitSequencePoint( _
                        m_LinePos, _
                        0, _
                        m_LinePos, _
                        m_LineLength + 1 _
        )
    End If
End Sub
```

The comments say it all. The nice thing about having a line-oriented language is that we can make blanket assumptions about the start and end positions of the statement.

Now we need to call this at appropriate places. The easiest one is **ParseAssignment**. Modify the **ParseAssignment** method in **Parser.vb** as follows:

```vbnet
Private Function ParseAssignment(variable As Symbol) _
                    As ParseStatus

    Dim result As ParseStatus
    
    SkipWhiteSpace()
    
    GenerateSequencePoint()

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
```

This will take care of every statement where an assignment occurs, including type-first declarations.

Next, we should generate a sequence point at the start of every command that immediately generates CIL. Currently, that means `Print`, `If`, `While`, `Exit`, `Continue`, `Until`, `For`, and `Select`. This being a long list, I won't show all the changes in-camera. You can check them out in the companion code. Here are a couple, for reference:

```vbnet
Private Function ParsePrintCommand() As ParseStatus
    Dim result As ParseStatus

    SkipWhiteSpace()

    GenerateSequencePoint()

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
            GenerateSequencePoint()

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

Notice how **GenerateSequencePoint** is called _just before_ the first CIL-emitting sub-parser. We have to make sure that the sequence point is generated before the CIL it targets.

## NOP And Roll

Some important commands where sequence points have not been generated are `Repeat`, `Loop`, `Next`, and `Else`. The `Repeat`, `Loop` and `Next` commands, if you remember the previous chapter, do not generate any CIL per se, and as such should not have sequence points. `Else` has similar characteristics.

We cannot just emit a sequence point when we parse these statements. A sequence point, as discussed, is just a marker for CIL instructions corresponding to a line of source. We will have to generate some CIL to correspond to these statements.

This is a cue to introduce my favourite CIL instruction: **nop**. This instruction, much like me, does
nothing.

For debug builds, every time we encounter:

* a `Repeat` or a `Loop` statement
* an `Else` or a `Default` statement
* an `End` statement (except `End Repeat`)

we should emit mark a sequence point and emit a **nop**.

Does this seem wasteful? Are we taking too much trouble and generating “bloated” executable files just to give what I think is a good debugging experience? Well, debugging needs to be as explicit and comfortable a process as possible. Our language syntax has some amount of noise-words, but that is a part of its design. Tweaking the debugger experience around the syntax will make debugging seem more natural.

Besides, we are in very good company. The Visual Basic and C# compilers do exactly this, for their own noise words. Anyway, **mop** instructions are not generated for non-debug builds.

Let’s add the **nop** instruction to our CodeGen class. While we are at it, we will add another instruction: **break**. This simply triggers the debugger. It’s the same instruction that is patched into any code by debuggers themselves to implement breakpoints.

Add the following to the CodeGen class, in **CodeGen.vb**:

```vbnet
Public Sub EmitNOP()
	m_ILGen.Emit(OpCodes.Nop)
End Sub

Public Sub EmitBreak()
	m_ILGen.Emit(OpCodes.Break)
End Sub
```

Next, add the following method to the Helper Functions region in **Parser.vb**:

```vbnet
Public Sub GenerateNOP()
    ' We need to generate a NOP instruction for
    ' some commands, only for debug builds
    If m_Gen.DebugBuild Then
        m_Gen.EmitNOP()
    End If
End Sub
```

Finally, we have to modify the parser methods themselves. As before, I won't show all the changes in-camera. Three of them are shown for reference below.

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

        ' Note that the sequence point and the
        ' NOP are emitted AFTER the startpoint
        ' label. This will ensure that when the
        ' debugger jumps back, it will highlight
        ' the Repeat statement
        GenerateSequencePoint()
        GenerateNOP()

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

            ' Note that the sequence point and the
            ' NOP are emitted AFTER the startpoint
            ' label. This will ensure that when the
            ' debugger jumps to the Else part of an
            ' If block, it will highlight the Else
            ' statement
            GenerateSequencePoint()
            GenerateNOP()

            ' Set the Else flag
            m_ElseFlag = True

            result = Ok()
        End If
    End If
    
    Return result
End Function

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
                ' For any End command other than
                ' End Repeat, we need to generate
                ' a NOP.
                GenerateSequencePoint()
                GenerateNOP()
                
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

As the comments in the code point out, the sequence point and accompanying **nop** get emitted _after_ a startpoint label, if one exists. This is to ensure that after a jump, the debugger correctly highlights the target statement. Whereas sequence points and **nop**s do not affect execution logic in any way, they have to be placed at the correct spots in the CIL for the debugging experience to be accurate. The comments show where the correct spot is located.

At this time, there remain only two statements that do not get sequence points generated: the `Case` and `ElseIf` statements. Unlike the ones that we just covered, these statements do result in CIL being generated, and therefore do not require a **nop**. However, there are several jumps and labels involved. To get it right, the sequence point has to be generated at the right spot. 

Which is the right spot? The clue is there in the commands above: if a statement can be the target of a jump, the sequence point should be generated after the label marking the jump target.

Modify **ParseElseIfCommand** and **ParseCaseCommand** in **Commands.vb** as follows:

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

        ' Note that the sequence point is emitted AFTER the
        ' startpoint label. This will ensure that when the
        ' debugger jumps to an ElseIf in an If block, it will
        ' highlight the ElseIf statement
        GenerateSequencePoint()

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

    ' Note that the sequence point is emitted AFTER
    ' the startpoint label is emitted. This will ensure
    ' that when the debugger jumps to a Case in a Select
    ' block, it will highlight the Case statement
    GenerateSequencePoint()
    
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
```
## Stop

Some languages have a command which immediately invokes the debugger. Javascript, for example, has the `debugger` command, and Visual Basic has `Stop`. We might want to do this when debugging a very complex `If` or `Select` block: if the execution ever gets to this point, I want to know about it, so pop up the debugger.

Such a command becomes even more useful when paired with a condition. Usually, if the condition evaluates to false, then the debugger should be invoked. This technique is called assertion. An "assert" is an assumption or a condition that is intended to be true. If the condition evaluates to false (there is an assertion failure), there is something wrong. In such a case, ideally, a debugger should be invoked.

Of course, this statement should only work for debug builds. For non-debug builds, it should be silently ignored.

We will borrow from Visual Basic, and call our statement `Stop`. But we will extend it to add an optional When clause, which can specify a condition as mentioned above, except that the debugger will be invoked if the condition is true.

The required CIL instruction, **break**, is already in CodeGen. So all we have to do is add a `Stop` command parser. Add it to **Commands.vb**, as follows:

```vbnet
Private Function ParseStopCommand() As ParseStatus
    Dim result As ParseStatus

    ' If not a debug build, ignore the Stop
    ' instruction
    If Not m_Gen.DebugBuild Then
        Return Ok()
    End If

    SkipWhiteSpace()
    
    ' If there is no When clause
    If EndOfLine Then
        ' Emit a break
        m_Gen.EmitBreak()
        GenerateSequencePoint()
        GenerateNOP()
        result = Ok()
    Else
        result = ParseWhenClause()
    
        If result.Code = 0 Then

            Dim afterBreakPoint As Integer = _
                            m_Gen.DeclareLabel()

            ' If the condition evaluates to
            ' false, jump past the break
            m_Gen.EmitBranchIfFalse(afterBreakPoint)

            ' Emit the break
            m_Gen.EmitBreak()
            GenerateSequencePoint()
            GenerateNOP()

            ' Emit the label
            m_Gen.EmitLabel(afterBreakPoint)
        End If
    End If
    
    Return result
End Function
```

There are two things to note here. One, we generate code only if this is a debug build. Otherwise, the Stop statement is silently ignored as far as code generation is concerned.

Secondly, notice that we call **GenerateSequencePoint** and **GenerateNOP**. The problem is that the debugger stops at the first sequence point after a **break** instruction. We want this to represent the `Stop` statement itself. Therefore, rather than put the sequence point before the **break** instruction, we put it after it, followed by a **nop**.

Finally, add `Stop` to the list of valid commands.

And we’re done. Let us compile our compiler, and then test all this. So, compile.

## The Test program

Create a new file called TestDebug.ssc, and add the following into it:

```sic
Stop

Comment
    This program uses just about every feature of
    our language. It should be fun to debug.
End Comment

REM Test Repeat
Dim i As Integer:=1

Repeat
    print i
    if i=3
        i:=i+2
        continue repeat
    end if

    exit repeat when i=8

    i=i+1
end repeat i>10

print "Done testing Repeat"

REM stop

REM Test If
I := 22
if i>30 Then
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

print "Done testing If"

REM Test While
i = 1
while i<5
    if i<2 then
        i = i + 2
    else
        i = i + 1
    end if
    REM stop when i=4
end while

print "Done Testing While"

REM Test Loop

int j
j := 1
loop
    print j
    exit loop when j==4
    j = j + 1
end loop

print "Done testing Loop"

REM Test For

Dim k As Integer
Print "Countdown..."
For k:=10 To 1 Step -1
    Print k
Next
Print "Blast off"

For i=1 to 5 Step 2
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

Print "Done testing For"

REM Test Switch

Int ItemPrice := 800
Dim CardType As String = "Platinum"
Dim DiscountPercent As Integer

Switch ItemPrice/100
    Case 10
        DiscountPercent:= 10
    Case 9
        FallThrough
    Case 8
        FallThrough
    Case 7
        Switch CardType
            Case "Platinum"
                DiscountPercent:=10
            Default
                DiscountPercent := 8
        End Switch
    Default
        DiscountPercent := 0
End Switch

Print DiscountPercent

Print "Done testing Switch"
```

Note the initial `Stop`. If you have been using Mono, you have to do this to use a debugger. This is because the Mono implementation of the Reflection Emit library does not implement  the **SetUserEntryPoint** method, and therefore does not provide us with the location where a debugger can start. The `Stop` command at the very beginning will provide that location for the debugger. Thereafter, all debugger features can be used.

If you have been using the .NET Framework instead, the initial `Stop` is not needed, but will not cause any problems if present.

Compile this with our compiler. We are now ready to debug.

## Debugging

The first edition of this book had instructions for using some debuggers, which used to be included with the .NET Framework SDK. I don't think these debuggers exist any more. So, I won't provide any instructions here. There are a lot of choices available these days: Visual Studio, mdbg, sdb, MonoDevelop, Visual Studio Code and so on. Use any one of them. They should all allow single stepping through SIC code, and monitoring of all declared variables.

## Compiler switches

One more modification and we can close out this chapter. Debug builds are very helpful during the development process. However, as we have seen, a lot of additional information is generated in a debug build. Once we have the program working exactly the way we want it, we don’t need to generate all that any more. In fact, they may (they will, believe me) slow down the program.

So, we will modify the Compiler class such that we can pass a command-line switch that indicates if a debug build is desired. If nothing is indicated, we will compile a non-debug build. While we are at it, we will add one more command-line switch as well, for specifying the name of the output file.

Add to following to **Utilities.vb**:

```vbnet
Public Class CommandArgs
    Public SourceFile As String
    Public TargetFile As String
    Public AssemblyName As String
    Public DebugBuild As Boolean
    Public Valid As Boolean
    Public Message As String
End Class
```

Next, add the following functions to **Compiler.vb**. The **Main** function provided here replaces the old one:

```vbnet
Private Function ParseCommandLine( _
                            ByVal CmdArgs() As String _
                        ) As CommandArgs

    Dim result As New CommandArgs
    result.Message = "Please specify a source file."

    Dim switch As String
    Dim param As String
    
    For Each cmd As String In CmdArgs
        ' If the command-line paramater starts
        ' with a / or a -, it's a switch. If
        ' it contains a : , then the part after
        ' the : is the paramter to the switch
        If cmd.StartsWith("/") _
                        OrElse _
                cmd.StartsWith("-") Then

            cmd = Mid(cmd, 2)

            Dim paramStart As Integer = _
                        InStr(cmd, ":")
    
            If paramStart <> 0 Then
                switch = Left( _
                            cmd, _
                            paramStart - 1 _
                )

                param = Mid(cmd, paramStart + 1)
            Else
                switch = cmd
                param = ""
            End If

            Select Case switch.ToLowerInvariant()
                Case "debug", "d"
                    result.DebugBuild = True
                Case "out"
                    result.TargetFile = param
                Case Else
                    result.Message = String.Format( _
                            "Error: Invalid switch - /{0}", _
                            switch _
                    )
                    Exit For
            End Select
        Else
            ' If the command-line parameter is not
            ' a switch, it is considered to be the
            ' name of the source file.
            result.SourceFile = cmd
            result.Valid = True
        End If
    Next

    Return result
End Function

Public Function Main(Byval CmdArgs() As String) As Integer
    Dim reader As TextReader
    Dim gen As CodeGen

    Dim status As ParseStatus
    Dim parser As Parser

    Console.WriteLine("Compiler for CLR")
    
    If CmdArgs.Length=0 Then
        reader = Console.In
        gen = New CodeGen( _
                    "",
                    "Test.exe", _
                    "Test", _
                    False
        )
    Else
        ' Parse the command line. If a source
        ' file has not been specified, show
        ' an error message and quit 
        Dim args As CommandArgs = ParseCommandLine(CmdArgs)
        If Not args.Valid Then
            Console.WriteLine(args.Message)
            Return 1
        End If

        If File.Exists(args.SourceFile) Then
            Dim finfo As New FileInfo(args.SourceFile)
            
            reader = New StreamReader( _
                        finfo.FullName)
            
            ' If no target file has been specified, use the same
            ' name as the source file with the extension changed
            ' to .exe
            If args.TargetFile = "" Then
                args.TargetFile = finfo.Name.Replace( _
                                        finfo.Extension, _
                                        ".exe" _
                )
            Else
                ' Validate the target filename
                Dim finfoValid As FileInfo
                Try
                    finfoValid = New FileInfo(args.TargetFile)
                Catch ex As Exception
                    Console.WriteLine("Error: {0}", ex.Message)
                    Return 1
                End Try

                ' Target file name must have
                ' an extension
                If finfoValid.Extension = "" Then
                    args.TargetFile = finfoValid.Name & ".exe"
                End If
            End If

            args.AssemblyName = finfo.Name.Replace( _
                        finfo.Extension, _
                        "" _
            )
            
            gen = new CodeGen( _
                    finfo.FullName, _
                    args.TargetFile, _
                    args.AssemblyName, _
                    args.DebugBuild _
            )
        Else
            Console.Write( _
                "Error: Could not find the file {0}.", _
                args.SourceFile _
            )
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
    
    Return status.Code
End Function
```

As you can see, the command line of the compiler requires a mini-parser in itself.

We parse two possible command-line parameters here. If the parameter **/debug** or **/d** (or **–debug** or **–d**) is passed on the command line, our compiler will perform a debug build. The parameter **/out:`<file>`** allows us to specify the name of the output file.

Compile our compiler, and then use it to compile a debug and a release build from
TestDebug.txt using the following command lines:

```bash
sicc /debug TestDebug.txt
sicc TestDebug.txt /out:TestRelease.exe
```

Only **TestDebug.exe** will be accompanied by a .pdb (or .mdb) file. 

## Conclusion
The CLR has many more features for debugging and tracing applications, but we won’t cover them here. The purpose of this chapter was to show how to emit "debuggable" code from our compiler; I think that
purpose has been achieved.

In the next chapter, we are going to take a break, and re-design our compiler as I promised in the last chapter. See you then.

---
[^1]: This is NOT true. CIL instructions never actually get executed; they get compiled into native machine code. However, for now, just imagine.

[^2]: Mono has a library called Cecil, which is more powerful than Reflection Emit, and can be used for the same purpose. That library does implement **SetUserEntryPoint**. If Cecil had existed when I had started writing this book, maybe I would have implemented the CodeGen class using it rather than Reflection Emit.
