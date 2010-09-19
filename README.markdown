# FDL

FDL (pronounced "Fuddle") stands for Functional Drawing Language. It is a language for describing animated pictures. It is implemented in Haskell as an embedded DSL and renders using OpenGL. It is an example of Functional Temporal Programming (sometime called Functional Reactive Programming or FRP)

## History

I wanted a way to teach programming to my daughters. I needed a problem domain that would motivate them and a language that would allow them to:

- do something immediately;

- do simple things quickly;

- compose simple ideas to produce impressive results.

I also wanted something that would stimulate ideas from the basic building blocks.

I felt that a language for drawing pictures would work well as it could produce nice visual results and adding the ability to animate would really bring programs to life. I considered Logo, but as a functional programmer, I was somewhat disturbed by the core stateful nature of Logo programming. FRP frameworks like Fran and Reactive seemed to have the right idea but I found them a little to sophisticated for young learners.

## Concept

FDL allows you to describe pictures by composing simple pieces. Attributes such as color, size and position are set in contexts and only affect contained elements - side effects can never escape. Any attribute can be defined as a time-varying value giving rise to animation. Eventually I hope to add audio input too, so pictures can respond to music.

Currently this is an embedded DSL in Haskell which means you get the parser and type-checker for free, but at the cost of some possibly frightening error messages. Eventually, I may write a standalone compiler.

## Examples

The simplest FDL program would be something like:

    main = draw $ circle

which... draws a circle. It uses the default color and size, so it will be white with a radius of 1. 1 what? Well a circle of radius of 1 will fill the window. Want it red?

    main = draw $ color red circle

What this means is, with the drawing color set to red, draw a circle. We can compose drawings with the +> operator, so:

    main = draw $ color red circle +> star

draws a white star on a red circle. Note that the color only affects the contained drawing, the circle. If we wanted them both red (which would be stupid), we would use brackets:

    main = draw $ color red (circle +> star)

We can name drawings so that we can reuse them:

    logo = scale (1/2) (color red circle +> star)

    main = draw $ move (-1/2,0) logo +> move (1/2,0) logo

By setting an attribute as a time varying value, we get pictures that change over time.

    main = draw $ scale pulse (color red circle +> star)

"pulse" gives a value that oscillates between 0 and 1. It uses a sin wave with a period of 1 second, so this gives a rather nasty pulsating effect. If we slow it down it is not so horrid:

    main = draw $ speed (1/5) (scale pulse (color red circle +> star))

or

    main = draw $ scale (speed (1/5) pulse) (color red circle +> star)

For more examples see the examples folder.

## Running the examples

To install the package, switch to the root directory of the package and run

    cabal install

To run an example use the ghc command:

    ghc -fglasgow-exts -O2 -o draw --make examples/xxx.hs

where xxx.hs is whichever example you want to run. To run it just type:

    draw

## Status

So the vision is all very nice, but it isn't there yet. So far, it can draw circles and stars in any color and do some basic transformations. It can also set a numeric attribute as the current run time. I know how I'm going to do all the stuff above, so hopefully it'll come quite quickly.

The mechanism to do repetitions is a bit of a hack as it was done before I added lambdas and there is currently no optimization.

## Learning

Apart from this note, there's no documentation at the moment, but the examples should be pretty self explanatory. I've been implementing the language one feature at a time, so you might want to run through the commit history to see how it developed.

## Implementation

The implementation really became interesting when I added support for lambdas. Prior to that, I had an abstract syntax tree for your program (this is a deep embedding) and did a fairly simple transformation into a chain of IO actions that get executed for each frame. There was some complexity in the transformation in order to handle the time varying values - which I handled using a reader monad to build up the operations in the IO monad. This code was perhaps a little confusing, but not particularly complicated.

Adding lambdas to the language representation was pretty simple, but handling variables in the transform to IO operations was where the fun began. I firstly altered my language representation to applications on primitives rather than nested expressions. Extending this with lambdas and variables formed a lambda calculus. I then implemented a transform to combinatory logic (SKI) which eliminates variables. This transform was actually pretty easy except for the replacement of a variable with the I combinator. The compiler was unable to verify that the types were correct. The solution was to introduce a type equivalence witness. Following this the transformation to the IO chain was trivial.

To see this development take a look at the commit history.


Have fun.
