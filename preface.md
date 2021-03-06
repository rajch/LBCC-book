# Preface

## Introduction

This book demonstrates the process of creating a language compiler for the CLR. It contains a mixture of generic compiler construction topics and topics specific to compiling for the CLR.

## How it came to be

For many years, I wanted to write a compiler. I read, or tried to read, a lot of books on the subject. Two things about these books consistently turned me off: one, they were, one and all, written using language familiar to mathematics and computer science students, but Greek \(often literally\) to the rest of us. Two, almost all of them were full of information about how to read source code and understand it \(_lexing_ and _parsing_\), but contained precious little information about how to generate machine code, and specifically machine code for the only kind of machine that I had handy; x86.

Then, I hit the jackpot; I read a brilliant article series called "Let's Build a Compiler" by Dr. Jack Crenshaw. I love Dr. Crenshaw's approach of learning by doing, showing ONE way of doing things while mentioning that there were others, avoiding jargon until unavoidable and explaining it lucidly when not, and above all, keeping it simple. This article series taught me more than all the books I had read so far, and even the books started making more sense after I finished the series. "Let's Build a Compiler", © Jack Crenshaw[^1], is definitely recommended reading for anyone who wants to learn about how compilers are built.

When the CLR appeared, I was delighted with the features provided for compiler writers, and quickly wrote my first CLR compiler. As happens very often with quick-and-very-dirty projects, a month later, I could not read what I had written. So I started re-writing it, this time being careful to document what I wrote. That documentation is what became chapters one and two of this book.

## Goal

The goal of this book is to teach you to create a language compiler for the CLR. That's it. This book does not aim to teach you everything about compiler construction theory. In fact, we will not even discuss several "pure theoretical" topics. Instead, we will discuss some topics which are important in creating compilers for the CLR, but not applicable elsewhere.

This is not to say we will not learn anything about generic compiler construction. Whenever we actually implement a compiler construction technique, we will discuss the theory behind it, and also discuss alternative implementations. And we will implement basic as well as advanced principles; for instance, we will implement \(and discuss\) scanning and parsing, code generation, strong data types, and optimization.

## The Approach

I have been heavily influenced by Dr. Crenshaw's approach, and this book tries to follow it closely.

To begin with, this is a "learn by doing" book. Each chapter begins with a goal, and then builds code in steps, trying to achieve that goal. You need to start reading each chapter from the beginning, and run the code examples when you come to them. Whereas we will discuss a lot of the things we do, much of the material to be learned is going to be in the code itself. So, running and understanding the code is essential.

Also, each chapter builds on the work of the previous chapter, so the whole book needs to be read, and the code run, in sequence. A couple of times in the book, we will stop and change our approach, and possibly re-factor our code to match. Consider this as creating multiple versions of our compiler.

Secondly, my aim is to create a compiler for the CLR in such a way that the process of creating it can be examined and understood. We are not trying to produce a template for creating all kinds of compilers. Nor are we trying to write the best, fastest, most reusable, or the most elegant compiler possible. Therefore, preference will be given to readability and understandability, over reusability, performance and above all, elegance. This applies both to compiler construction principles and the coding technique.

Thirdly, I believe nothing provides better comprehension than running \(or debugging\) code that produces tangible output. Therefore, at the end of every chapter, starting with chapter 1, we will have a functioning compiler, which will produce an executable file which can also be run. We will not mess with intermediate representations or "assembler" code. The CLR is a brilliant environment for compiler writers in this respect.

Finally, I reiterate that the aim is to examine and understand the process of building a working compiler. To that end, I use one approach, although many alternatives are possible. There may be differences of opinion over how we implement a certain part of the compiler. This is good, and I would appreciate any feedback or corrections. Unfortunately, unlike a couple of my idols, Dr. Donald Knuth and Mr. Bruce McKinney, I cannot offer a reward for these.

## The development language

The code in this book has been written using Microsoft Visual Basic.NET, only because that is my language of preference. I have a chronic dislike of semicolons, "curly brackets" and case sensitivity. It could well be written using C\#, or any other CLR language. In fact, if someone volunteers to bring out a version of the book using any other language, I will be happy to collaborate. Just let me know.

In many ways, the approach itself is slightly biased towards a Basic programmer…oh, sorry, "programmer", in that first I make something work before thinking of how to make it work well. I am not sorry.

## The language being developed

A compiler compiles some source language into executable code. Which one should we compile?

I was tempted to create a C\# compiler, just to spite the people from that camp who say "your language compiler is written using our language". But what we are going to do is create a new language, called **SIC**, which stands for **S**imple **I**nstruction **C**ode. I have not decided the complete feature set for SIC. We will define the language as we go along.

## The compiler being developed

As mentioned earlier, at the end of every chapter, we will have a working "compiler", one which produces ready-to-run executable files. In fact, starting from Chapter 2, the compiler we create will behave just like the big boys: it will be able to read source code from files and generate an executable. Our compiler will be different in just one way; it will stop compiling at the very first error it finds and reports. Later, we will enhance it to find and report multiple errors.

## The development environment

Since we are developing a compiler for the CLR, you obviously need to have the CLR itself installed. ~~I have tested all the code in this book with the Microsoft .NET Framework versions 1.0 and 1.1. The generated code should run on Mono, too, although I have not tested for this.~~

This topic now merits its own chapter. That's the [next one](/the-development-environment.md).

## Conclusion

This is, first and foremost, a "fun" book. The primary aim is to create a running compiler from scratch, and secondary aim is to learn a little bit about the arcane art of compiler construction in the process. I hope you enjoy reading and working with it as much as I enjoyed writing it.
