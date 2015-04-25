---
title: Becoming Productive in Haskell
subtitle: coming from Python
date: 2015-4-19 12:20
category: Coding
author: Matthew Griffith
description: How I became productive in Haskell
---
<div>By <a href="/about">Matthew Griffith</a></div>

<p class="drop-first"><span class="run-in">Sometime recently</span> I became proficient enough in Haskell to be productive, and I wanted to capture some of my thoughts on the learning experience before it got too far away.  I do most of my web prototyping in Haskell now, though I still regularly use and enjoy Python.
</p>

# Data First
This is more of a thought on moving from a dynamic language to a static language, but in Haskell the structure of your data is mostly stated in data declarations and type signatures. In Python it's mostly implied by the code.  

My first thought with a Haskell function is "What does the data look like?  This function takes a \____ and returns a \_____ ?", while in Python my first thought is "What does the code say?" 

Thinking 'data first' improved my coding, even when coming back to Python.  I more often recognize when the structure of my data changes for no real reason other than it was easy and I was very 'zoomed in' on the problem at the time.  

Limiting changes in data structure also makes the code less complex and easier to understand.

# The Readability
One of my main motivations for using Python is readability of code.  Haskell originally looked ugly outside of what seemed to be carefully crafted examples.  Pieces of it looked very clear, but were surrounded by flotsam and jetsam of nonsense.  But it was also obviously powerful.

I definitely wanted to avoid 'clever' code that was powerful but confusing.  

However, my ability to assess readability was in assessing other imperative languages.  It was a bit like criticizing the readability of Mandarin as an English reader.

I found that Haskell is not 'clever but deceptive.'  Of course you can write 'clever' code in Haskell, just like any language, but it's not the common case.

Actually, in Haskell that 'clever code' can only do so many clever things, as it's constrained by the type system.  If it says it returns an Int, it will return an Int or fail to compile.

The more powerful and precise abstraction mechanisms that Haskell supplies just sometimes smell like the magic that I try to avoid in Python.

# No, Really, the Readability

In the beginning, though, you kinda have to have faith that, yes, people do read it without any trouble and on a regular basis.  Once over the hump, Haskell became very readable for me.

1. Type signatures.  They're like getting a little summary at the top of a chapter of a book. With the added bonus that it's guaranteed to be true.  Wouldn't that be great to have next time you try to learn another language?

<aside id="book-summary" class="hilarious">
<blockquote>This is the chapter where Tommy goes to the market and buys a duck.</blockquote>
```haskell
    chapter :: Tommy -> Market -> Duck
```
</aside>

2. Composing functions out of other, smaller functions offers a big reduction in complexity.  If they're named well, this allows you to write functions that are easy to read.

3. It's concise.  You don't need a ton of code to express a powerful idea. 


# Infix Symbols and Noise
I also wanted to mention something about the infix symbols that are common in Haskell code too (\$,<\$>,<-,->,etc.), as they can create a sort of symbol induced despair/anger in newcomers.  

Don't despair!  I know they reek of deceptive cleverness, but there are only a limited number of common ones. Once you know them you'll see they're useful and simple.  I think there are maybe 5 infix symbols that I use on a regular basis.

That being said,  I would say ignore the lens library in the beginning, as it has a ton of infix symbols.  It's a very cool library, but you can get by just fine without it.  Wait until you're comfortable creating medium-sized things in Haskell, and then approach it at your leisure.

# A Whole New Vocabulary.
There are a lot of completely new words to learn when you learn Haskell.  Things like *Functor* and *Monad*.

These words are going to feel _heavier_ to learn for a few reasons.  When starting to learn imperative programming, a lot of the new vocabulary has at least _some_ familiarity.  A loop brings to mind...well, loops.  Race tracks, roller coasters, uhh....cereal. 

We store memories by attaching them to previously made memories, so there is going to be a tendency for your brain to just shut off if too many of these new, heavy words show up in a sentence or paragraph.  I had no associations with the word _Functor_, so it was hard to store.

My strategy in learning these words was to come up with my own name that made sense to me and mentally substitute it every time that heavy word came up.  After a while, these made-up synonyms anchored me and I had no problem with the 'heavy word.'

For example: Functor.

In Haskell, this is something that can be mapped over.  For example, a list is a Functor.  This means there is a mapping function that takes another function and applies it to everything in the list and creates a list with the results.  

```haskell
map (+1) [1,2,3,4]
-- results in [2,3,4,5]
```

So, I started calling it _Mappable_.  Mappable was easy for me to remember and was descriptive of what it did.  A list is a Functor.  A list is Mappable.  

# My Trusty Print Statement

In Python, my main development tool is the print statement/function.

In Haskell, my main development tool is the type system.  It checks what I'd normally use print statements to check: what data a function is actually receiving or returning.

But!  You can use [Debug.Trace](https://hackage.haskell.org/package/base-4.6.0.1/docs/Debug-Trace.html) as a Python style print function without having to bother with Haskell's IO type.  This can be very useful to get started.  Though, once you get moving in Haskell, you probably won't use it as much as you think you would.

If you leave trace statements in your code after you're finished debugging...well, you will feel dirtier when you do that in Haskell than when you do it in Python.

# The Best Monad Tutorial
was a Parsec tutorial.

When you hear about someone becoming productive in Haskell, it mostly involves a description of how they finally understood Monads.  Well, damn, here it goes.

I needed to write a parser.  I had something in Python, but due to my inexperience in writing parsers, the growing complexity of my code was slowing me down considerably. 

So, I had some extra time. I thought maybe I should give it a go in Haskell.

I found the Youtube video, [Parsing Stuff in Haskell](https://www.youtube.com/watch?v=r_Enynu_TV0), which explains how to create a JSON parser in Haskell using the Parsec library.

But it also inadvertently showed me how to use Monads and Applicatives as tools to create something I needed. It showed me how they function (har, har) and how they are related to each other.

After writing a parser with them, I began to understand other code that used them.  I then started to understand their abstract nature...but that abstractness was a lesson for another day, not for starting out.

Also, Parsec provided enough structure that my inexperience in writing parsers did not really matter.  In fact, as someone just learning Haskell, I was able to write a parser that was better in every measure (lower complexity, faster speed, better readability, easier extensibility), compared to what I could do as a programmer who has worked with Python for years but with no expertise in parsers.


# The learning process was incredibly rewarding
Haskell is my main web prototyping language now for several reasons.

Well, reason 0 is I have the opportunity to choose what technology I use.  I know that's a luxury.  

1. I'm able to write a prototype faster, and that prototype is usually my production version.
2. I don't have to waste my time on trivial bugs.
3. The bugs I do encounter are generally more meaningful and lead me to understanding the problem more.  Note: meaningful doesn't always mean harder.
3. Python taught me not to worry about speed that much.  Haskell agreed with that but let me have it anyway.
4. Refactoring is a *breeze*.  In Python, I always had a nagging feeling that I forgot to change some small part of your code that will be important later.  
5. Excellent libraries.  I feel that the basic guarantees of the Haskell language make the standard quality of libraries exceptionally high.  Then there are libraries that were game-changers for me (Parsec and QuickCheck immediately come to mind, but there are others).
6. [A helpful community](https://gist.github.com/quchen/5280339).
7. Easy to scale up code to using many cores.
8. Haskell infrastructure is improving all the time.  Last year, when GHC (which is the Haskell compiler) 7.8 came out, it [doubled the performance of Warp](http://www.yesodweb.com/blog/2014/02/new-warp), one of the prominent web servers that was already pretty fast.

And finally, I have to say that writing Haskell code comes with a deep level of satisfaction.  It's more rewarding than most any coding experience I've had.


# Where to start?

It can be tough to find a good starting point.

Here's how I would do it if I had to learn Haskell again.

First, reading at least Chapters 1 through 8 in [Learn you a Haskell for Great Good](http://learnyouahaskell.com/chapters).

Then!

1. __Write a small module that doesn't worry about IO.__  Something like a Sudoku module that generates sudoku puzzles.  Don't worry about using a random number as a seed.  Use Debug.Trace as your print statement to see what's going on.  Generate a puzzle and Debug.Trace it to the screen.  Create your own data types, and just use functions (i.e. no custom typeclasses).
2. __Turn that into a Website__ using either [Scotty](http://adit.io/posts/2013-04-15-making-a-website-with-haskell.html) or [Spock](http://www.spock.li/tutorial/).  Keep it simple,  a URL that shows a sudoku puzzle.  Then, a URL that [produces JSON](https://www.fpcomplete.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json) of a sudoku puzzle.
3. __Mess around with real IO.__  Try printing the puzzle to the terminal without Debug.Trace.
4. __Find incremental ways to add to it.__  Design a file format for sudoku puzzles and write a Parsec parser for it!  Don't have the file format be JSON—make something up.


Good luck!













