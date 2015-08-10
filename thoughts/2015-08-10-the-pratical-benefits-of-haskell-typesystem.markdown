---
title: The Basic, Practical Benefits
subtitle: of the Haskell Type System
date: 2015-8-10 12:20
category: Coding
author: Matthew Griffith
description: How I became productive in Haskell
---

<span class="run-in"><span class="drop">O</span>n September 23rd, 1999</span>, the <a href="https://en.wikipedia.org/wiki/Mars_Climate_Orbiter#Cause_of_failure">Mars Climate Orbiter disintegrated</a> as it descended into the Martian Atmosphere 53 kilometers lower than intended.

It was later found that a software module written by scientists at Lockheed Martin provided numbers in pound-seconds, while another software module expected the numbers to be in metric.  

Five days ago, some dude’s website kicked the bucket because an obscure function that’s only called on Thursdays received a string that was capitalized when it was expected to be only lowercase.

(Okay, that last one didn’t actually happen… or did it? It probably did… somewhere.)

These seem like they should be simple, catchable errors.

And yet these pieces of software were all written by smart people. In some cases, really smart people.

# Can we do any better?

Of course, if we were there, we could have...

 * Been more vigilant as programmers.  
 * Written more unit tests.
 * Performed more code reviews.  

But, do we expect ourselves to be more vigilant than the rocket scientists at Lockheed Martin? Or always have the time to write unit tests?  Or even beyond that, that they will be comprehensive? Or the agony luxury of having someone else available to review your code?

Sometimes the above options are the only options.

But I’d like to talk about another one.

We could use a type system like the one in Haskell.

<!--more-->

# Haskell’s Other Language: Types

Let’s take addition as a function.  

```haskell
add x y = x + y
```

It’s pretty benign as far as functions go.  It takes two numbers and returns a number.

Except this function crashed the Mars Climate Orbiter.  (Well, okay, not exactly this function, but stick with me.)  If we give this function a distance in miles and a distance in kilometers, it will return an absurd number.

We need to make it more specific.  We need to be able to say, “This function takes a number in meters and another number in meters, and returns a number in meters.”

In Haskell, we can do this with a type declaration and some annotations to the code.

```haskell
-- First we declare that we have a new type
-- that represents meters as a Float.
newtype MetricDistance = Meters Float
-- You can say this in english as
--    ‘Create a data type named MetricDistance,
--     which can have value called ‘Meters’
--     that is a Floating Point Number


-- addMetric takes a MetricDistance,
--       and another MetricDistance
--       and returns a MetricDistance
addMetric :: MetricDistance -> MetricDistance -> MetricDistance
addMetric (Meters x) (Meters y) = Meters (x + y)
-- The 'real code', first unpacks the type on the left
-- Does the addition, and repacks the result into the 'Meters' type.

```

This function doesn’t crash the Mars Climate Orbiter. When another function tries to call it with Miles, it gives a type error when it compiles, far before the orbiter is launched.

```
Type-error in function, addMetric
Expected Type: MetricDistance
Actual Type:   Miles
```

This is called __type safety__.

Types remove the reliance on a programmer’s discipline. They allow you to relax after writing code, because all the functions know exactly the form of what they’re getting as input and producing as output. Your code won’t compile if there is a mismatch.

This idea reverberates through third party libraries, too. Not only do you know that the functions that a library exposes follow the type signatures, but also that the library itself has internal consistency.


# Making Illegal Values Impossible to Represent

In Haskell, all the values that a type can be are explicitly written in a type declaration. In our above example, the MetricDistance type can only be Miles with a Floating point number. There’s no option for it to be ‘null’, or a String, or anything else.

You can use the Haskell type system to represent only the states that make sense.

For example, the character in your game can be alive with some HP, unconscious, or dead with a high score.

```haskell

-- Declare HP and Score as new types
newtype HP = HP Int
newtype Score = Score Int

-- Use 'data' so a type can have multiple options for a value
data CharacterStatus = Alive HP
                     | Unconscious
                     | Dead Score

showStatus :: CharacterStatus -> String
showStatus (Alive (HP hp))       = "Alive with " ++ show hp ++ "hp left"
showStatus (Unconscious)         = "Uhh, are you alive?"
showStatus (Dead (Score points)) = "Oh, no, they’re definitely dead.  " ++
                                   "But they had a high score of " ++
                                    show points ++ ". " ++
                                   "I mean, that's something, right?"


```

It’s specific.  

There’s no guessing here. We have a character status and we know exactly what that means. It’s one of three values. There are no other secret values it can be. There’s no ‘you forgot to make that string lowercase’ state. There’s no, ‘a value is null and you forgot to check it but what does null even mean in this case? ’ state.

Not only that, but in the future if you add another possible value to characterStatus, Haskell will tell you that the showStatus function is no longer exhaustive and you need to add another case.


# Fire the Missiles!

There’s another interesting idea in Haskell, and it’s about communicating with the outside world: printing a statement to the console, interacting with your database, or firing the missiles.  

It’s about IO.

Haskell uses the idea of restricting which pieces of code can do IO.  

If you wanted, you could make every function be able to do IO in Haskell, just like code you’re familiar with, but there are places you know you don’t want interacting with the outer world. Do your html templates need to access the database directly?  Should the coffee machine be able to launch the missiles?

This idea of enforcing which piece of code can speak with the outside world is closely related to a concept you might have heard about called purity.

A function is pure if it doesn’t talk to the outside world.  

Of course, you can write pure functions in Python, or Javascript, or C#, or Java.

But in Haskell you have tools to guarantee that it doesn’t talk to the outside world.  You can enforce purity, or else the code wouldn’t compile.

Purity turns out to be a pretty cool property. If a function doesn’t talk to the outside world, when you give it the same arguments, it will always return the same value. This makes understanding your own code much easier.


# How Haskell restricts which Functions can do IO

In Haskell, IO is a container. You run a function that talks with the outside, and it will return the result to you encased in a container.

This is just the same as if you were to run a function and it returned a value in a one element list. A list is a container as well.

Let’s look at reading a file in Haskell.

Here’s the type signature of the function ‘readFile’.  

```haskell
readFile :: FilePath -> IO String
```

First, it takes a FilePath. That may throw you if you’re not used to types, but it’s just a string. When you see FilePath, think ‘a string that has been explicitly labeled as a filepath’.

Now, the readFile function, goes out, does its IO, and returns a String that is inside an IO container.

It’s similar in form to a function that takes a string, and returns that string in a list.

```haskell
toList :: String -> [String]
toList str = [str]
```

Except in Haskell, when a value is in an IO container, you know it came from the outside.


# Oh God, Monads
You may have heard of Monads. You may have even heard they have something to do with IO. Or that they are the mathematical voodoo that prevents you from trying to learn Haskell.

But it’s much simpler than that.

They’re just a few functions that deal with putting things in containers and taking them out.  

That’s it.

IO is a container, so it would make sense be able to get the value out.

Because moving values into and out of containers is so common, it’s given some syntactical sugar called do notation.  

Let’s write a small function that concatenates the contents of two files together and returns them as one string.

```haskell
catFiles :: FilePath -> FilePath -> IO String
catFiles path1 path2 = do
    -- We read a file, which returns a String in a container
    -- We take the string out of a container
    -- and put it in the fileContents1 variable
    fileContents1 <- readFile path1

    -- We do the same thing, take
    -- the second string out of its container
    -- and put it in the fileContents2 variable
    fileContents2 <- readFile path2

    -- We then concatenate the two strings with ++
    -- We also need to wrap it in an IO container
    -- because we did IO.
    --  We do this with a function called ‘return’
    return (fileContents1 ++ fileContents2)
    -- The ‘return’ function is not the return statement you’re used to.  
    -- In this case, it is a function that wraps a value in a container.
    -- In Haskell, see return, think ‘wrap’
```

This function interacts with the outside, so it has to return its value in an IO container. The compiler will complain otherwise.

So, in the above example, the back arrow, <- means take a value out of a container and put it in the variable to the left.

And the unfortunately named ‘return’ is a function that puts a value in a container. (It has nothing to do with the return statement in other languages, it’s just a coincidence).

How does Haskell know which container ‘return’ puts the value into?  I keep saying a list is also a container, like IO.  So, why doesn’t ‘return’ put a value into a list?

It knows by the type signature which says ‘this function will give a string that is in an IO container’.


# What if there is no type signature?

If you leave out a type signature, Haskell will figure out the types based on the functions you used.

In this case, we used ‘readFile’ in our function body, and  ‘readFile’ returns an IO container. Therefore, it knows that this function is about the IO container. It knows that the back arrow <- will take a value out of an IO container, and it knows that ‘return’ should wrap the result in an IO container as opposed to, say, a list.

This calculating of types is called ‘type inference’.

Actually, this is one of the defining features that Haskell’s type system has over other type systems like the one in Java. This creates succinct code, as every intermediate variable does not have to be declared as a specific type.

Type inference can also help you out directly.

Don’t know a type? Just ask the Haskell compiler. Leave an underscore in the type signature and the Haskell compiler will tell you what it thinks should be in the blank.

```haskell
catFiles :: FilePath -> FilePath -> _
catFiles path1 path2 = do
		fileContents1 <- readFile path1
		fileContents2 <- readFile path2
		return (fileContents1 ++ fileContents2)
```

The compiler will tell you:
```bash
Found hole '_' with type IO String
```

Following the Ripple of Changes

But you get something else with the type system.

Let’s say the day has come. You’ve written the code. You’ve tested it and shipped.

Now you’re on to version 2.0. You want to refactor or restructure, or whatever we’re calling it now.

In Haskell, you change a type or a function and the compiler will just give you a list of the code that is affected by your change. “Here’s the list of code you have to look at now.” It’s so straightforward, it’s almost boring.  

Does an ‘if’ statement not catch all cases after your change?  It’ll tell you.

A function that is called only on third Wednesdays of the month was depending on a number in a field you just deleted? It’ll tell you.

In Python or Javascript you just execute the new code and hope for a swift death. You hope that any errors that are introduced are encountered immediately on execution. Or that it’s caught in your unit tests that you definitely had time to write when your deadline was looming.


# Where does that leave us?

Using Haskell isn’t about looking smarter, or using ‘obscure, academic, few decade old, but too bleeding edge technology’, or even about learning a bunch of math.  

For me, it’s about being productive. I’ve adopted Haskell for the same reason I adopted Python a handful of years back…

It’s about quickly writing code.  

Except now there’s an additional step with my Haskell code: relaxing confidently.

It’s about writing code that stays written and doesn’t need to be revisited some number of times over again.  

Because for code that you ship and then actually have to maintain and develop, Haskell is much more productive. Once it’s written in Haskell, it’s much harder to break and much easier to add functionality.

Does it prevent you from all errors? No.

Does it prevent you from the most common, least interesting errors? Yup.

Will you still need to write unit tests? Yeah, but fewer and they’ll be more meaningful than checking basic arguments.

Is there a learning curve? Yes, just like anything new. You’re going to have to think differently. You may even have to suspend your intuition a bit, because it’s going to change. But I believe the benefits are enormous and it doesn’t take very long if you’re actually serious about it.

What about all those freaky words like Monoid, Monad, and Applicative? I will tell you this: those words are making a false promise to you. They promise that they will be hard. They promise that they will be harder than what you’ve already dealt with in other languages.  

They are not. In fact, when you learn them you will most likely be underwhelmed at how straightforward they are. I might even go so far as to say the hardest part about them is getting over the word.


# So, give Haskell a try!
  * Use <a href="http://www.haskellforall.com/2015/01/use-haskell-for-shell-scripting.html">turtle</a> for type-safe shell scripting in Haskell.  If you have huge shell scripts, the type-safe part turns out to be very useful in maintaining them.
  * Do some front-end programming in <a href="http://elm-lang.org/">Elm</a>, which is like a mini-Haskell that compiles into javascript.  It’s <a href="http://elm-lang.org/blog/blazing-fast-html">even faster than ReactJs</a>.  Of course, that’s with all the caveats  you should have when you read benchmarks.
  * Write a website in <a href="https://www.spock.li/">spock</a>.


And who knows, maybe we can stop having all these dumb errors.
