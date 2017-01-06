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

