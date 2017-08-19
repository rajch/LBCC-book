# The Development Environment

## Introduction

The code that is presented in this book requires a Visual Basic compiler that targets the CLR. In 2016, that gives us two good options: the Microsoft .NET Framework and Mono. The hot new .NET Core is not yet viable.

## Getting the code

If you like, you can actually type/copy-and-paste the code from the book as you read \(ha ha\). Or, you can download the code from github, at:

[https://github.com/rajch/LBCC-code](https://github.com/rajch/LBCC-code)

## Developing with the .NET Framework - command line

1. Download and install any version of Visual Studio.
2. Open a "Visual Studio Command Prompt"
3. Compile the code with the `vbc` command, as shown in each chapter.

## Developing with Mono - command line

1. Download and install Mono. On Linux, be sure to get the`mono-allpackage` or ensure that you have the `mono`, `mono-devel`, `mono-utils` and `mono-vbnc` packages.
2. Compile using the `vbnc` command as shown in each chapter. Whereever the text says `vbc`, substitute `vbnc`.
3. Where the text asks you to run a .exe file, use `mono <whatever>.exe`.

## A bit of history

When I first started writing this book in mid-2004, there were already multiple versions of the CLR. Microsoft had the .NET Framework 1.0, the .NET Framework 1.1, the .NET Compact Framework, and the mostly-ignored Shared Source CLI \(codename Rotor\). Mono 1.0 had just been released. I was freshly unemployed, and no longer had access to the best tool at the time, Visual Studio .NET 2003. So I built the code in this book using whatever I could get for free.

The .NET Framework itself, since the beginning, included a Visual Basic compiler called `vbc`. That would have been enough, but a few more tools, such as `peverify`\(you will meet it in Chapter 1\) and a debugger were helpful. In those days, Microsoft used to offer a package called the .NET Framework SDK for free download, which included these tools. So I built using that, on top of .NET Framework 1.1. The resulting code ran on all the variations of the CLR mentioned above

In 2016, we have the Visual Studio Express editions, and even the full-fledged Visual Studio Community Edition, which includes the modern .NET Platform SDK. Mono, meanwhile, has gained a very respectable Visual Basic compiler called `vbnc`.

I have moved away from Windows. All the code in this book is now compiled on Ubuntu Linux 16.04, using mono 4.2.1 and vbnc 0.0.0.5943. The results should run unmodified on any CLR.

