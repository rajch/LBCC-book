# Code Generation

## Introduction

As I promised, every chapter in this book, starting from this one, will produce a "compiler" which in turn produces code that can be run. Therefore, this chapter covers Code Generation, which is the process of creating object code that can be run directly on your computer.

## Goal

The goal of this chapter is simple; we will write a program that will generate an executable that can be run.

## The Approach

This is not a common approach in most compiler construction books, where the task of generating object or machine code is briefly covered at the end, or left to the reader to figure out from other sources. There is a good reason for that; there are many machines \(read microprocessors\) out there. The way each machine understands and carries out instructions, or deals with data, is vastly different from every other machine. If a lot of time was devoted to the characteristics of each machine, many other important facets of compiler construction would be ignored.

Thankfully, compiling for the CLR protects us from all this. Let us understand why, with an “accurate enough” explanation.

## “Virtual Machines”

Any real microprocessor allows us to manipulate data using instructions. The data and the instructions need to be stored in a location that is accessible by the microprocessor. This is typically done using registers[^1], which are quickly accessible locations available to the central processing unit.

Instructions operate on one or more units of data, and can result in more data being produced. For instance, a hypothetical instruction called ADD may require two numbers, and produce a result that is their sum. The two input numbers, and the result, would all be temporarily stored in registers as the CPU processes the instructions. Instructions such as ADD need to specify which registers they will access their input from, and where they will store their result. The speed of processing depends heavily on the number of registers available, and how they are used for different operations.

The number of registers available, the actual instructions that operate on then, and the optimal way that they can be used differs from microprocessor to microprocessor. This makes life difficult for compiler writers, as they would have to deeply understand processor architectures to generate optimal code.

The difficulty can be mitigated somewhat by splitting the process of compilation into two parts:_front-end_ and _back-end_. Instead of generating code for an actual microprocessor, the front-end compiler would generate code for an intermediate, hypothetical “virtual microprocessor”, which would define a neutral set of registers and instructions. The back-end compiler would be be responsible for converting the “intermediate language” to the real instructions for a target microprocessor, in a way that optimally uses the registers available to the target.

This makes life simpler by having a single well-defined “virtual machine”, whose “intermediate language” \(meaning registers and instruction set\) is the only one that front-end compiler writers have to generate. Also, multiple back-ends may be written, so that a single program compiled to target the “virtual machine” can finally run on multiple real microprocessors.

Some “virtual machines” eschew the idea of registers altogether. Instead, they use a last-in, first-out stack data structure to hold both data and instructions.Instructions in such a “virtual machine” assume that any data they require will come from the stack, and any results that they produce will be put back on the stack. For example, a hypothetical ADD instruction would take out the last two values from the stack, add them, and put the result back on the stack. This kind of “virtual machine” is called a _stack-based_ virtual machine, or _stack machine_. The previous kind is called _register-based_. Front-end compilers for stack machines are simpler and quicker to build than compilers for other machines[^2].

## The CLR “Virtual Machine”

You’ve probably guessed it; the CLR provides us with a stack-based “virtual machine”, whose instruction set is called  the Common Intermediate Language\(CIL\). All CLR compilers are, therefore, front-end compilers that target CIL. The CLR itself provides the back-end functionality of converting CIL to actual machine code.

The CLR implements a stack, and provides a set of instructions. All data that has to be acted upon is loaded on the stack. Any instruction operates on values on the stack, by removing values from the stack, or adding a new value on to the stack. The process of removal of values is called popping, and the process of putting values in is called pushing or loading. As the name "stack" suggests, values are popped in reverse of the order they are loaded; if you load 2, and then load 3, then when you pop, you will get 3 first, then 2.

For example, to add the values two and three, the following sequence is required.

1. Load the Value **2** onto the stack
2. Load the Value **3** onto the stack
3. Apply the CIL Instruction **Add**

The instruction Add will pop the last two values in the stack; add them together, and the load the result back on the stack. At the successful completion of this sequence, the stack will contain one value: the result. If there are less than two values on the stack when the ADD instruction is encountered, the CLR will indicate that this is an error.

Here are some of the instructions that are available in CIL. Instructions are also called OpCodes \(for Operation Codes\).

<table>
<thead>
<tr>
<th>
OpCode
</th>
<th>
What it does
</th>
</tr>
</thead>
<tbody>
<tr>
<td>
LdC_i4
</td>
<td>
Stands for LoaD Constant Integer of size 4-bytes. As the name suggests, it is used for loading a 32-bit integer number on
the stack. There are equivalent instructions for loading other kinds of data.
</td>
</tr>
<tr>
<td>
Add
</td>
<td>
Pops the last two values on the stack, adds them together, and loads the result on the stack.
</td>
</tr>
<tr>
<td>
Sub
</td>
<td>
Pops the last two values on the stack. Then, the first value popped is subtracted from the second value popped. So, the instruction
sequence:
<ol>
<li>Load 2</li>
<li>Load 3</li>
<li>Sub</li>
</ol>
would cause 2 to be loaded, then 3 to be loaded, then 3 to be popped, then 2 to be popped, then 3 (the first value popped)
to be subtracted from 2 (the second value popped) and finally, the result, -1, would be pushed back on
to the stack. The stack would only have the value –1 at the end of the sequence.
</td>
</tr>
<tr>
<td>
Mul
</td>
<td>
Pops the last two values on the stack, multiplies them, and loads the result on the stack.
</td>
</tr>
<tr>
<td>
Div
</td>
<td>
Pops the last two values on the stack. Then, the second value popped is divided by the first value popped. The result is
pushed back on the stack.
</td>
</tr>
</tbody>
</table>

If we were creating a compiler for a language which only performed arithmetic using integer numbers, these would be all the instructions we need. In fact, most classical compiler texts begin by creating just such a compiler. Let us do the same. We will learn more instructions, as we need them.

##Reflection Emit

It is one thing to know IL instructions, another thing to produce them. By produce, I mean emit them in a form that can be executed; in short, an executable file(.exe). Luckily, the CLR provides a way for us to directly produce an executable file from our own applications; this mechanism is called Reflection Emit.

We need not delve into the details of Reflection Emit just yet. All we need to know, at this point, is that Reflection Emit provides us with a class called **ILGenerator**, which enables us to produce IL instructions directly to an EXE file. This class has a method called **Emit**, which is what we use.

Here is some code that demonstrates how ILGenerator is used, and also sets up a framework for the code generation part of our compiler.

```vb
Imports System
Imports System.Reflection
Imports System.Reflection.Emit
 
Public Class CodeGen
      Private m_ILGen As ILGenerator
 
      Public Sub EmitNumber(num As Integer)
           m_ILGen.Emit(Opcodes.Ldc_i4, num)
      End Sub
End Class
```
We create a class called CodeGen, which has a private variable called m\_ILGen, which is of type ILGenerator. In the subroutine EmitNumber, we use the Emit method of m\_ILGen, passing it the Opcode **Ldc\_i4**, which we discussed earlier, and the number to be loaded. We can write all our emitting code like this. So, let us add the code for emitting the four mathematical operations to the CodeGen class.

```vb
Public Sub EmitAdd()
     m_ILGen.Emit(Opcodes.Add)
End Sub
 
Public Sub EmitSubtract()
     m_ILGen.Emit(Opcodes.Sub)
End Sub
 
Public Sub EmitMultiply()
     m_ILGen.Emit(Opcodes.Mul)
End Sub
 
Public Sub EmitDivide()
     m_ILGen.Emit(Opcodes.Div)
End Sub
```
##Code(Gen) Complete

By now, you would have noticed that we have not initialized the variable m_ILGen. Also, the question arises, where do opcodes and the data emitted by the ILGenerator go? The full explanation for these will be handled in a later chapter, but for now, here is the complete listing for the CodeGen class. Bear with me, and save this as **CodeGen.vb**. We will keep coming back to this class, both for a complete explanation of what is going on, as well as for adding new features to it.

```vb
Option Strict On
Option Explicit On


Imports System
Imports System.Reflection
Imports System.Reflection.Emit


Public Class CodeGen
	Private m_ILGen As ILGenerator
	Private m_producedAssembly As AssemblyBuilder
	Private m_producedmodule As ModuleBuilder
	Private m_producedtype As TypeBuilder
	Private m_producedmethod As MethodBuilder
	Private m_SaveToFile As String

	Public Sub EmitNumber(ByVal num As Integer)
		m_ILGen.Emit(OpCodes.Ldc_I4, num)
	End Sub

	Public Sub EmitAdd()
		m_ILGen.Emit(OpCodes.Add)
	End Sub

	Public Sub EmitSubtract()
		m_ILGen.Emit(OpCodes.Sub)
	End Sub

	Public Sub EmitMultiply()
		m_ILGen.Emit(OpCodes.Mul)
	End Sub

	Public Sub EmitDivide()
		m_ILGen.Emit(OpCodes.Div)
	End Sub

	Public Sub EmitWriteLine()
		Dim inttype As Type = Type.GetType("System.Int32")
		Dim consoletype As Type = Type.GetType("System.Console")
		Dim paramtypes() As Type = {inttype}

		m_ILGen.Emit( _
			OpCodes.Call, _
			consoletype.GetMethod( _
				"WriteLine", paramtypes _
			) _
		)
	End Sub

	Public Sub New(ByVal FileName As String)
		m_SaveToFile = FileName

		' Compiling a CLR language produces an assembly.
		' An assembly has one or more modules.
		' Each module has one or more types:
		' (structures or classes)
		' Each type has one or more methods.
		' Methods are where actual code resides.
		' Create an assembly called "MainAssembly".

		Dim an As New AssemblyName
		an.Name = "MainAssembly"

		m_producedAssembly = AppDomain.CurrentDomain.DefineDynamicAssembly( _
							an, AssemblyBuilderAccess.Save _
		)

		' In MainAssembly, create a module called
		' "MainModule".
		m_producedmodule = m_producedAssembly.DefineDynamicModule( _
							"MainModule", FileName, False
		)

		' In MainModule, create a class called
		' "MainClass".
		m_producedtype = m_producedmodule.DefineType("MainClass")

		' In MainClass, create a Shared (static) method
		' with Public scope, called "MainMethod".
		m_producedmethod = m_producedtype.DefineMethod( _
						"MainMethod", _
						MethodAttributes.Public Or MethodAttributes.Static, _
						Nothing, _
						Nothing _
		)

		' All IL code that we produce will be contained
		' in MainMethod.
		m_ILGen = m_producedmethod.GetILGenerator

	End Sub

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

		m_producedAssembly.Save(m_SaveToFile)
	End Sub
End Class

```
Here is a quick and dirty explanation of what is going on. Detailed explanations will be given in later chapters.

1. Each CLR application is contained in a package called an _assembly_. An assembly usually corresponds to an EXE (or a DLL) file.

2. An assembly can contain one or more _modules_. Modules are usually present inside the Assembly's EXE file, although they can exist outside.

3. A module consists of one or more _types_, which are classes or structures.

4. A type has members, called fields, properties, events and methods.

5. In the class constructor of our CodeGen class (`Sub New`) method, we create an assembly whose name is "MainAssembly". In this, we create a module called "MainModule", inside which we create a class type called "MainClass". Inside the class, we create a Shared (static) method called "MainMethod", for which we then obtain an ILGenerator. What this means is that all the IL instructions that our compiler will generate by calling the various Emit methods will be contained in the MainMethod in the EXE file that is finally produced.

6. When the Save method is called, the first thing it does is emit an Opcode called Ret. Every method in a CLR executable must end with the Ret instruction. Thereafter, we specify that MainMethod is the entry point of the assembly, which means that when the assembly EXE file is run, the code in MainMethod should be executed. We also specify that the assembly is a "console application". Finally, we save the assembly to an EXE file, the name of which had been passed to Init and stored in a field called m_SaveToFile.

The method EmitWriteLine emits IL instructions to cause the generated EXE to print the last number on the stack to the screen. The technique used in this method, as well as the actual Opcode emitted, **Call**, will be discussed in detail in a future chapter. As of now, we need to remember only this: just like EmitAdd expects two numbers on the stack, and pops them, EmitWriteline expects a single number on top of the stack, and pops it. The number is displayed on the screen.

##Testing CodeGen
Let's test this. Save the following code as **Tester1.vb**.

```
Option Strict On
Option Explicit On

Module Tester1
	Sub Main()
		Dim cg As New CodeGen("hello.exe")

		cg.EmitNumber(2)
		cg.EmitNumber(2)
		cg.EmitAdd()
		cg.EmitWriteLine()

		cg.Save()
	End Sub
End Module
```

Compile with the following command:
```bash
vbc /out:Tester1.exe Tester1.vb CodeGen.vb
```
Then, run it with:
```
Tester1.exe
```
which should produce the file hello.exe. Now, run:
```bash
Hello
```

Voila. Tester1.exe produced Hello.exe. Hello.exe is our first “compiled” executable, which correctly adds 2 and 2, and shows the result.

##What's happening here

First, we load the number two on to the stack. Then, we load the number two (again) on to the stack. Then, we emit the instruction **Add**, which pops the numbers from the stack, adds them, and loads the result (4) onto the stack. Finally, EmitWriteLine pops a number (the result) from the stack, and shows it. At the end of it all, the stack is empty.

##Error: Error not found

I like to keep my examples as real-life as possible, and this last one was not real-life at all. _It worked the first time_. Every developer knows that you should expect some errors the first time.

So, let us introduce some errors. Modify the code in **Tester1.vb** to look like this, and save as **Tester2.vb**.

```vb
Option Strict On
Option Explicit On

Module Tester1
	Sub Main()
		Dim cg As New CodeGen("hello.exe")

		cg.EmitNumber(2)
		cg.EmitWriteLine()
		cg.EmitNumber(2)
		cg.EmitAdd()

		cg.Save()
	End Sub
End Module
```
Again, compile:
```bash
vbc /out:Tester2.exe Tester2.vb CodeGen.vb
```
Run:
```bash 
Tester2.exe
```
which should produce the file hello.exe. Now run:
```bash
Hello.exe
```
Ouch!!! What happened?

##What's going on?

First, we load the number 2 on to the stack. Then we call EmitWriteLine, which pops that number 2 to write it to the screen. _At this point, the stack is empty_. Then, we load the number 2 on to the stack. Then we emit the instruction **Add**, which pops two numbers…OOPS! There is just one number on the stack.

##Invalid Applications(or, You CAN'T run with scissors)

Look carefully at the output of the last run of hello.exe. By rights, the error occurred after the call to WriteLine. So, we should see a 2 on screen, and then the error message. Is that what happened?

Nope. That is because, as things are, the EXE is invalid. It is impossible to run this EXE and not get an error. The CLR can detect this right at the time of loading the EXE, and choose not to run it. That is exactly what happened. Not even the first load was executed, because the CLR _verified_ the EXE and found it to be invalid. This process of verification happens for any code executed under the CLR, so unlike traditional systems, you can't shoot yourself in the foot. Neat, isn't it?

##It’s trickier than apparent (or, for each bug you see, there's one you don't)

You can trigger verification without running the EXE, using a tool called `peverify`. We can test this now by executing the command:
```bash
peverify hello.exe
```

This reports **two** errors. What gives?

There are actually two errors. The first one is a "Stack Underflow", as reported by peverify, which is the one we discussed earlier; there are not enough values on the stack for the **Add** Opcode to work. The other one, which is shown as "Unspecified Error" by peverify, stems from the fact that the stack is not empty when the method finishes. The last valid thing we did was load the number 2.

The stack has to be empty at the end of our method. If we had omitted the call to EmitAdd, we would not have got the "Stack Underflow" error, but we would have got the other error, which peverify would have been able to more correctly identify.

The complete rule is "The stack must be empty at the end of a VOID method", which is a method that does not return any value. In the produced hello.exe, the only method is MainMethod, which does not return any value. So, the stack must be empty when MainMethod finishes.


[^1]: Wikipedia article about registers \([https://en.wikipedia.org/wiki/Processor\_register](https://en.wikipedia.org/wiki/Processor_register)\)

[^2]: Wikipedia article about stack machines \([https://en.wikipedia.org/wiki/Stack\_machine](https://en.wikipedia.org/wiki/Stack_machine)\)

