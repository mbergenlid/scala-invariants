Scala-invariants
=================

An experimental plugin for the scala compiler that lets you add constraints to expressions
which has to hold at compile time. This plugin was created as an experiment because I thought
it was a cool idea and I wanted to learn about scala plugin development. The state of the plugin is 
far from stable at this time but maybe it will turn into something useful at some stage.

Anyway, below is a brief explanation of what the plugin is capable of at the moment.

Basic Usage
-----------

Start REPL with the `-Xplugin` option. Note that you also have to add it to the classpath.

    $ scala -Xplugin:scala-invariants.jar -cp scala-invariants.jar

Now we can add a method with some basic constraints. This method takes an integer which has to be
greater than `0`.

    scala> import mbergenlid.scalainvariants.annotations._
    scala> def myMethod(@GreaterThan(0) x: Int) = x + 1

Let's try it out.

    scala> myMethod(1)
    res0: Int = 2
    
    scala> myMethod(0)
    <console>:12: error: 
    Could not assign Literal to x.
    Unable to prove that:
      Literal constrained by (Literal == 0)
    is a subtype of
      x constrained by (x > 0 && x <= 2147483647)
          
                  myMethod(0)
                           ^
                           
You can see that it is ok to pass the integer `1` to `myMethod` but we get an error from the compiler
when we try to pass value `0` (because `myMethod` requires an `Int > 0`)

We are not limited to just passing literals though.

    scala> {
         | val x = 5
         | myMethod(x)
         | }
    res3: Int = 6

Here, we know at compile time that `x` is 5 which is greater than `0`. 
However, if the value of `x` is not known, then

    scala> {
         | val y = new scala.util.Random().nextInt()
         | myMethod(y)
         | }
    <console>:14: error: 
    Could not assign y to x.
    Unable to prove that:
      y constrained by (y == y && y >= -2147483648 && y <= 2147483647)
    is a subtype of
      x constrained by (x > 0 && x <= 2147483647)
          
                  myMethod(y)
                           ^
We get an error because it's impossible to prove that `y` is within the valid range.

We can always check that `y` is within range.

    scala> {
        | val y = new scala.util.Random().nextInt()
        | if(y > 0)
        |   myMethod(y)
        | else 
        |   0
        | }
    res6: Int = 488657411

In the positive branch of the if statement `y` is correctly considered to be grater than `0`.
This is of course also true for the other branch of the if expression, we could rewrite it like this.

    scala> {
         | val y = new scala.util.Random().nextInt()
         | if(y <= 0)
         |   0
         | else
         |   myMethod(y)
         | }
    res7: Int = 0

Return Values
--------------

Methods can also be annotated with constraints, for example, imagine the function `abs` which takes an integer
and returns the absolute value of that integer. This could be defined as follows, (at least from a mathematical
perspective).


    scala> @GreaterThanOrEqual(0)
         | def abs(x: Int) = if(x < 0) -x else x

However, that fails with the following error:

    <console>:11: error: 
    Could not assign If to abs.
    Unable to prove that:
      If constrained by (If <= 2147483648 && If > 0 && If == 0 + -x || If <= 2147483647 && If >= 0 && If == x)
    is a subtype of
      abs constrained by (abs >= 0 && abs <= 2147483647)
          
           def abs(x: Int) = if(x < 0) -x else x
                             ^

Which is correct, since (for technical reasons) `(-Int.MinValue) == Int.MinValue`. Hmm.. 
That's not good.

We could fix that by, for example, not allowing `Int.MinValue` to be passed in, like so.
   
    scala> @GreaterThanOrEqual(0)
         | def abs1(@GreaterThan(Int.MinValue)x: Int) = if(x < 0) -x else x
    abs1: (x: Int)Int


Constraints in terms of input
------------------------------

Often, a method is constrained in terms of its input and it is possible to express that in
scala-invariants as well. For example, 

    scala> @GreaterThan("x")
         | def increment(@LessThan(Int.MaxValue)x: Int) = x+1
    increment: (x: Int)Int

The parameter to the `@GreaterThan`-annotation is here passed as a String, this means that it should be
resolved as a method parameter.

The method call will then be resolved in terms of the input. 

    scala> {
         | val x = 5
         | @GreaterThan(5)
         | val y = increment(x)
         | }


A parameter can also be constrained by another parameter in the same method.

    scala> @GreaterThan(0)
         | def minus(@GreaterThanOrEqual(0)a: Int, @GreaterThan(a) b: Int) = b - a
    minus: (a: Int, b: Int)Int

Here, `b` has to be greater than `a` which has to be greater than or equal to `0`
A parameter can not be constrained by a parameter that appears after the parameter itself.

Advanced Example
---------------------

Here is a more advanced example of how `scala-invariants` can be used. We are going to implement the method
`find(array: Array[Int], element: Int, start: Int)` which finds the first occurrence of `element` in `array`
starting from index `start`.

    scala> :paste
    // Entering paste mode (ctrl-D to finish)
    
    @GreaterThanOrEqual(-1)
    @LessThan("array.length")
    def find(
      array: Array[Int],
      element: Int,
      @GreaterThanOrEqual(0) @LessThanOrEqual("array.length") start: Int): Int = {

      if(start == array.length) -1
      else if(array(start) == element) start
      else find(array, element, start+1)
    }

All invariants are validated at compile time, if we for example forgot to add the base case test, 
`start == array.length`, the code wouldn't compile.


    scala> :paste
    // Entering paste mode (ctrl-D to finish)
    
    @GreaterThanOrEqual(-1)
        @LessThan("array.length")
        def find(
          array: Array[Int],
          element: Int,
          @GreaterThanOrEqual(0) @LessThanOrEqual("array.length") start: Int): Int = {
     
          if(array(start) == element) start
          else find(array, element, start+1)
    }
    
    // Exiting paste mode, now interpreting.
    
    <console>:17: error: 
    Could not assign start to index.
    Unable to prove that:
      start constrained by (start < start + array.length && start >= 0 && start <= array.length && start <= 2147483647 && start == start)
    is a subtype of
      index constrained by (index >= 0 && index < array.length)
          
                 if(array(start) == element) start
                          ^
Here, start could be equal to `array.length` which is not a valid argument for `array.apply`.

    <console>:18: error: 
    Could not assign $plus to start.
    Unable to prove that:
      $plus constrained by ($plus >= 1 && $plus == start + 1 && $plus <= 2147483648 && $plus <= array.length + 1)
    is a subtype of
      start constrained by (start >= 0 && start <= array.length)
          
                 else find(array, element, start+1)
                                                ^
Here, since `start <= array.length` then `start+1 <= array.length + 1` which can be greater than `array.length`  

    <console>:17: error: 
    Could not assign If to find.
    Unable to prove that:
      If constrained by (If < start + array.length && If <= 2147483647 && If >= 0 && If <= array.length && If == start || If < start + array.length && If < 2147483647 && If >= -1 && If < array.length)
    is a subtype of
      find constrained by (find >= -1 && find < array.length)
          
                 if(array(start) == element) start

Here, the final return is not within the method's declared bounds, since `start` could be equal to 
`array.length` but the method is declared as returning something that is strictly less than `array.length`.

Smart If Expressions
---------------------

This section lists a few examples of how if expressions can be used.

    scala> @GreaterThan(0)
         | def ifExpr1() = {
         |   val x = scala.util.Random.nextInt
         |   val y = scala.util.Random.nextInt
         | 
         |   if(x > 0 || y > 0) { 
         |     if(y > 0) { //y > 0 in this branch
         |       y
         |     } else { //y <= 0 ==> x > 0
         |       x
         |     }
         |   } else {
         |     5
         |   }
         | }


The snippet above compiles since all branches can be proven to be greater than zero. 
In the first branch, either `y` or `x` is greater than zero. Next if `y > 0` then we return `y` or else,
since `y` is not greater than 0 then `x` must be greater than zero, we return `x`.

