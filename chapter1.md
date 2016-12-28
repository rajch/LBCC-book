# Code Generation

## Introduction

As I promised, every chapter in this book, starting from this one, will produce a "compiler" which in turn produces code that can be run. Therefore, this chapter covers Code Generation, which is the process of creating object code that can be run directly on your computer.

## Goal

The goal of this chapter is simple; we will write a program that will generate an executable that can be run.

## The Approach

This is not a common approach in most compiler construction books, where the task of generating object or machine code is briefly covered at the end, or left to the reader to figure out from other sources. There is a good reason for that; there are many machines \(read microprocessors\) out there. The way each machine understands and carries out instructions, or deals with data, is vastly different from every other machine. If a lot of time was devoted to the characteristics of each machine, many other important facets of compiler construction would be ignored.

Thankfully, compiling for the CLR protects us from all this. Let us understand why, with an “accurate enough” explanation.

## “Virtual Machines”

Any real microprocessor allows us to manipulate data using instructions. The data and the instructions need to be stored in a location that is accessible by the microprocessor. This is typically done using registers, which arequickly accessible locations available to the central processing unit.

Instructions operate on one or more units of data, and can result in more data being produced. For instance, a hypothetical instruction called ADD may require two numbers, and produce a result that is their sum. The two input numbers, and the result, would all be temporarily stored in registers as the CPU processes the instructions. Instructions such as ADD need to specify which registers they will access their input from, and where they will store their result. The speed of processing depends heavily on the number of registers available, and how they are used for different operations.

The number of registers available, the actual instructions that operate on then, and the optimal way that they can be used differs from microprocessor to microprocessor. This makes life difficult for compiler writers, as they would have to deeply understand processor architectures to generate optimal code.

The difficulty can be mitigated somewhat by splitting the process of compilation into two parts:front-endandback-end. Instead of generating code for an actual microprocessor, the front-end compiler would generate code for an intermediate, hypothetical “virtual processor”, which would define a neutral set of registers and instructions. The back-end compiler would be be responsible for converting the “intermediatelanguage” to the real instructions for a target microprocessor, in a way that optimally uses the registers available to the target.

This makes life simpler by having a single well-defined “virtual machine”, whose “intermediate language” \(meaning registers and instruction set\) is the only one that front-end compiler writers have to generate. Also, multiple back-ends may be written, so that a single program compiled to target the “virtual machine” can finally run on multiple real microprocessors.

Some “virtual machines” eschew the idea of registers altogether. Instead, they use a last-in, first-out stack data structure to hold both data and instructions.Instructions in such a “virtual machine” assume that any data they require will come from the stack, and any results that they produce will be put back on the stack. For example, a hypothetical ADD instruction would take out the last two values from the stack, add them, and put the result back on the stack. This kind of “virtual machine” is called astack-basedvirtual machine, orstack machine. The previous kind is called aregister-based. Front-end compilers for stack machines are simpler and quicker to build than compilers for other machines.

