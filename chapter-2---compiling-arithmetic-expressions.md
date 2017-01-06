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

*  An expression consists of data \(2,3\) as well as instructions \(\*, +\), and these are interspersed. The compiler has to figure out which is the data and which is an instruction. Sometimes, it's not as easy as it looks.

*  Some things have to be done before others, and we can't tell ahead of time what comes before what. The compiler has to figure this out, and ensure things happen in the correct order. The BDMAS rule is an example.

*  There may be errors in the expression, which have to be dealt with. The compiler decides what is an error, and what to do about it.



