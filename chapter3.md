# Strings

## Introduction
In the last chapter, we built a parser that correctly recognizes and translates mathematical expressions. In this chapter, we will also make it recognize and translate strings.

## The Goal
Thus far, if our compiler meets a line consisting of a mathematical expression, it emits CIL instructions to perform the calculation, and the emitted executable shows the result. After we are done with this chapter, if a line contains a string instead, it should show the string.

## Representing strings
How is a string represented in source code? The almost universally accepted standard nowadays is to enclose a string in double quotes ("). So:

```
"This is a string"
```

This brings in an interesting question; what if a string had to contain a double quote? This is usually taken care of by specifying an 'escape' character, which, if it appears inside a string, causes the next character to be treated specially. So, one could represent a quote inside a string like this:

```
\"
```

This assumes that ```\``` is the escape character. The escape character itself can be literally represented like this:

```
\\
```

A variety of special characters can be represented by the escape character followed by another; for instance, ```\n``` for new line, ```\t``` for tab etc. These are called _escape sequences_.

For now, we will keep strings simple, and not use escape characters or sequences. For the single special case of representing a double quote within a string, we will borrow from the Basic language. That language specifies that to put a " sign inside a string, we have to use two double quotes without any space between them, like this:

```
"Raj says, ""We will use this for now."""
```

Finally, we need a way to represent an 'empty' string. Borrowing from most string-aware languages, we will represent this as two double quotes without any space between them, like this:

```
""
```
Is there such a thing as a string expression, involving strings and _string operators_? Well, most modern languages define an operator for string concatenation, or joining two strings together. We will once again borrow from the Basic language, which uses either the & symbol, or the + symbol for this purpose. So, the following are also valid strings:

```
"Hello " & "World"
"How do I" + " say goodbye?"
```

## The Approach
The approach we take remains the same as in the last chapter; we will create a scan method for reading a string, and a parse method for generating code for it. Since any character is valid inside a string, we do not need a recognizer method. Generating code will also not be a problem initially, since at the end of chapter 2, we had added the capability to load strings to our CodeGen class.
