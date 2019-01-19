# Interlude - Grammars and BNF

We have written a lot of code already, to compile a fairly large number of things - including numbers, terms, factors, strings, conditions, and three different kind of expression. To help us keep track of all the things we compile, let's take a short break from code, and learn a useful theorical subject: _grammars_.

## What is _a_ Grammar?

A grammar, also called a _formal grammar_, is a set of rules which describe string sequences that are _valid_ for a given _formal language_. A formal language is whatever that we are trying to parse - it could consist of just numbers on lines, like when we started, or it could be a full programming language. The rules that define it are collectively called it's grammar.

The subject of formal language theory, and especially of formal grammar, makes for fascinating reading. But following this book's philosophy of "accurate enough", here is what we need to know.

A grammar consists of a set of rules. These rules, called _production rules_ or _productions_, are built from _terminal symbols_ and _nonterninal symbols_. A _non-terminal symbol_ is a valid concept in the grammar's language - but which does not actually appear in the source code. For example, **relationaloperator** is a nonterminal symbol - it represents an inportant concept to be parsed, but does not appear directly in the source code. The source code contains symbols such as "<" or "=" instead - these are called _terminal symbols_. A production defines nonterminal symbols in terms of combinations of terminal symbols and other nonterminal symbols. The production specifies what kind of combination is valid.

Let's undestand this with an example. Let us say that the nonterminal symbol **digit** can be any one of the following terminal symbols: "0", "1", "2", "3", "4", "5", "6", "7", "8" and "9", and nothing else. This is a production, which can be written as follows:

```bnf
<digit> ::= "0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"
```

If this were the only production in the grammar, than all that the language would recognize as valid would be a single digit.

A language's grammar is complete when all possible combinations of terminal symbols in source code can be converted into nonterminals.

## Backus-Naur Form, or BNF

Did you see how we wrote down the production above? That is a formal notation technique called the Backus-Naur Form - named after [John Warner Backus](https://en.wikipedia.org/wiki/John_Backus) and [Peter Naur](https://en.wikipedia.org/wiki/Peter_Naur). Most programming language specifications are today written in BNF, or various extended dialects of BNF.

In BNF, we write production rules in terms of a single nonterminal symbol, followed by the definition symbol (::=), followed by a sequence of terminal and non-terminal symbols. Nonterminal symbols are written using angle brackets \(<>), and terminal symbols using either double or single quotes. The pipe symbol (|) denotes alternatives - so in the example above, the nonterminal **digit** can mean _any one_ of the terminal symbols. Finally, square brackets ([]) denote anything that is _optional_.

In writing our grammars, we will use two notations that are not available in pure, unextended BNF. A terminal or a nonterminal symbol immediately followed by an asterisk (*) will mean that the symbol can occur zero or more times in that production, while a symbol immediately followed by a plus (+) will mean that the symbol can occur one or more times. For example:

```bnf
<number> ::= <digit>+
```

means that a number can have one or more digits.

## The grammar for our NumericExpression

To understand BNF more clearly, let us define the grammar for numeric expressions as our compiler understands them today. To begin with:

```bnf
<digit>  ::= "0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"
<sign>   ::= "+"|"-"
<number> ::= [<sign>]<digit>+
```

This means that a **number** can optionally have a **sign** (which is either a + or a -) followed by one or more **digit**s.

Moving along, we have:

```bnf
<numericexpression>          ::= <term><addorsuboperation>*
<addorsuboperation>          ::= <addorsuboperator><term>
<addorsuboperator>           ::= "+"|"-"

<term>                       ::= <factor><mulordivoperation>*
<mulordivoperation>          ::= <mulordivoperator><factor>
<mulordivoperator>           ::= "*"|"/"

<factor>                     ::= <number>|<bracketednumericexpression>
<bracketednumericexpression> ::= "("<numericexpression>")"
```

[Chapter 2](chapter2.md) describes this hiearchy in detail.

Now. Notice how the productions with terminal symbols are taken care of by our `Scan` methods with the help of our `Is` methods? And from the level we have productions involving only terminals, we have a corresponding `Parse` method? Go ahead, check.

This is why we are taking the time out to learn BNF. It will help us specify new feautures using a formal notation, which is less cumbersome than explaining it in English every time. Also, the BNF will show us what recognizer, scanner and parser methods to write.

## Start Symbol

When we define a grammar in BNF, we have to specify a _start symbol_. This is a nonterminal that is the top of the grammar hierarchy. When the start nonterminal symbol has been produced, the grammar's work is complete.

If, for instance, we take our numeric expression as an independent language, the start symbol would be `<numericexpression>`.

## The grammar for StringExpression

Here is the grammer for string expressions:

```bnf
<stringexpression> ::= <string><concatoperation>*
<concatoperation>  ::= <concatoperator><string>
<concatoperator>   ::= "+"|"&"
<string>           ::= <quote-symbol><stringcharacter>*<quote-symbol>
<quote-symbol>     ::= '"'
<stringcharacter>  ::= <character>|<quote-in-string>
<quote-in-string>  ::= <quote-symbol><quote-symbol>
<character>        ::= ? every character other than double-quote or newline ?
```

Note the `? every character other than double-quote or newline ?` in the production of `<character>`? This, again, is not a part of pure BNF, but a conevenience that we are adapting from one of the extended dialects. This saves me from writing every possible character in double quotes, separated by pipes. So, ? means don't treat the enclosed as pure BNF; instead, read and understand it as English.

Again, if you check our code in [Chapter 3](chapter3.md), you will see that the BNF maps very well.

If this were an independent grammar, the start symbol would have been `<stringexpression>`.

## The full grammar so far

So, let's write the complete grammar for our parser so far, including all kinds of expressions. We will go from the top down. The start symbol is `<program>`.

```bnf
<program>                ::= <line>*
<line>                   ::= <expression>
<expression>             ::= <booleanexpression>|
                             <stringexpression>|
                             <numericexpression>

<booleanexpression>      ::= <condition>
<condition>              ::= <expression><reloperator><expression>
<reloperator>            ::= "="|"=="|"==="|"<>"|"!="|"!=="|
                             "<"|"<="|"=<"|">"|">=","=>"

<stringexpression> ::= <string><concatoperation>*
<concatoperation>  ::= <concatoperator><string>
<concatoperator>   ::= "+"|"&"
<string>           ::= <quote-symbol><stringcharacter>*<quote-symbol>
<quote-symbol>     ::= '"'
<stringcharacter>  ::= <character>|<quote-in-string>
<quote-in-string>  ::= <quote-symbol><quote-symbol>
<character>        ::= ? every character other than double-quote or newline ?

<numericexpression>          ::= <term><addorsuboperation>*
<addorsuboperation>          ::= <addorsuboperator><term>
<addorsuboperator>           ::= "+"|"-"
<term>                       ::= <factor><mulordivoperation>*
<mulordivoperation>          ::= <mulordivoperator><factor>
<mulordivoperator>           ::= "*"|"/"
<factor>                     ::= <number>|<bracketednumericexpression>
<bracketednumericexpression> ::= "("<numericexpression>")"
<number> ::= [<sign>]<digit>+
<sign>   ::= "+"|"-"
<digit>  ::= "0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"
```

A "**program**", as understood by our compiler, is zero or more **line**s. Each line has an **expression**, which can be a **booleanexpression**, **stringexpression** or **numericexpression**. The breakdown of the individual expression types have already been discussed above. Except **booleanexpression**, which should be easy enough to understand.

## Conclusion

As our compiler gets bigger, keeping track of exactly what it has to parse, and the rules thereof, becomes harder. BNF will help us to define the whole grammar in a more compact manner, and will give us a great starting point for structuring our code.