---
Title: From Python to Haskell
Subtitle: or my experience picking up Haskell as a Python Programmer
Date: 2015-4-15 12:20
Category: Coding
Tags: programming, haskell, 
Slug: from-python-to-haskell-intro
URL: /posts/2015-04-15-python-to-haskell-intro/
Author: Matthew Griffith
Summary: My experience picking up Haskell as a Python Programmer
---

# General Background

# Data First
In python, in many cases,  the structure of the data you're working with is implied by the code.  In Haskell, it is stated explicitly.  I noticed a subtle shift in how I was coding in haskell in that my first thought is more aligned with "how is the data structured", while in python my first thought was "what does the code say."

Once you have this shift, and you start with looking at data first, it becomes easier to read haskell, because there's a lot you don't have to read.  To understand a function, you look at its type signature.  You only really look at the guts of the function if you're trying to come to a greater understanding of that specific function.

There is a whole (small) language that is meant to define data in haskell.  In python, the structure of the data generally flows from the code.

In fact, I found a reaction in myself when I started programming in haskell...I felt like I wasn't programming.  I wanted to get 'into the code' asap...and my brain didn't really consider writing data structures to be writing code.

One of the lessons, which I've seen echoed across a few is that when writing in haskell, you think about the code more.



# The Great Synonym Table


Map a function over one argument that is in a container. (i.e. Functors)

function | notes | examples
---------|---------------------|---------
map      | specific to lists   
fmap     | More generic, works on anything that has implemented it
<$>      | Exactly the same as fmap, but its infix.  f <$> d
liftM    | Same as fmap, but in the Monad module.
liftA    | Same as fmap, but in the Applicative module.


Map a function over multiple arguments, each in containers (i.e. Applicative)

function | notes | examples
---------|---------------------|---------
<*>      |  In the Applicative |
`ap`     |  In Monad


Put a value in a container.  How does it know which container?  By the type signature, or it figures it out based on code around it.

function | notes | examples
---------|---------------------|---------
pure     |  In the Applicative 
return   |  In Monad



Bind multiple functions
function | notes | examples
---------|---------------------|---------
'>=='    |  In the Monad module. 
'<-'     |  This is 'do' notation





# A note for people from python.  
https://www.python.org/dev/peps/pep-0020/
When I started and found this list of synonyms, I started to think that this haskell thing was bogus.  I tracked it back to the python principle of 'There should be one-- and preferably only one --obvious way to do it.'  So many synonyms said that there were a number of ways to do things in haskell.  While I still generally agree with the 'only one' principle, Haskell provides an interesting entry.  Because of Haskell's type system, where a function is guaranteed to take a specific input and always return a specific output....you have a huge guarantee of safety.  This level of safety allows you to be more expressive in your code.


