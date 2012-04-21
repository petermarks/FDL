# FDL

FDL (pronounced "Fiddle") stands for Functional Drawing Language. It is a language for describing animated pictures. It was originally implemented as an embedded DSL in Haskell, but is now a full language with a standalone evaluator which renders using OpenGL. I'm also considering compiling to Processing.js. It is an example of Functional Temporal Programming (sometime called Functional Reactive Programming or FRP)

## History

I wanted a way to teach programming to my daughters. I needed a problem domain that would motivate them and a language that would allow them to:

- do something immediately;

- do simple things quickly;

- compose simple ideas to produce impressive results.

I also wanted something that would stimulate ideas from the basic building blocks.

I felt that a language for drawing pictures would work well as it could produce nice visual results and adding the ability to animate would really bring programs to life. I considered Logo, but as a functional programmer, I was somewhat disturbed by the core stateful nature of Logo programming. FRP frameworks like Fran and Reactive seemed to have the right idea but I found them a little too sophisticated for young learners.

## Concept

FDL allows you to describe pictures by composing simple pieces. Attributes such as color, size and position are set in contexts and only affect contained elements - side effects can never escape. Any attribute can be defined as a time-varying value giving rise to animation. Eventually I hope to add audio input too, so pictures can respond to music.

## Examples

The simplest FDL program would be something like:

    circle

which... draws a circle. It uses the default color and size, so it will be white with a radius of 1. 1 what? Well a circle of radius of 1 will fill the window. Want it red?

    color red circle

What this means is, with the drawing color set to red, draw a circle. We can compose drawings by following one by another (note that line breaks and indentation are significant), so:

    color red circle
    star

draws a white star on a red circle. Note that the color only affects the contained drawing, the circle. If we wanted them both red (which would be stupid), we would use an indented block:

    color red
      circle
      star

We can name drawings so that we can reuse them:

    move (-1/2,0) logo
    move (1/2,0) logo

    logo =
      size (1/2)
        color red circle
        star

We can add parameters to named drawings to create functions:

    move (-1/2,0) (logo red)
    move (1/2,0) (logo blue)

    logo c =
      size (1/2)
        color c circle
        star

By setting an attribute as a time varying value, we get animations.

    size pulse
      color red circle
      star

"pulse" gives a value that oscillates between 0 and 1. It uses a sine wave with a period of 1 second, so this gives a rather nasty pulsating effect. If we slow it down it is not so horrid:

    speed (1/5)
      size pulse
        color red circle
        star

For more examples see the examples folder.

## Running the examples

To install the package, switch to the root directory of the package and run

    cabal install

To run an example use the draw command:

    draw examples/xxx.hs

where xxx.hs is whichever example you want to run.

## Status

All the examples above should work now. The language supports the following primitives:

### Shapes

- circle - A circle of unit radius (fills the window)

- star - A 5 pointed star of unit radius

- square - A 2 x 2 square (fills the window)

### Color

- color Color Picture - draws the given picture in the given color

- red, green, blue, yellow, cyan, magenta, white, black, pink, purple - colors

- rgba Number Number Number Number - produces a color from the given proportions of red green and blue as numbers from 0 to 1 and an alpha value from 0 (transparent) to 1 (solid)

### Transformation

- size Number Picture - draws the given picture scaled by the given size - a positive number where 1 is original size

- scale (Number, Number) Picture - draws the given picture scaled by the given sizes - a pair of numbers for the width and height

- move (Number, Number) Picture - draws the given picture at the given position - a pair of coordinates where (-1,-1) is the bottom left and (1,1) is the top right

- rotate Number Picture - draws the given picture rotated by the given amount - 1 is a full 360 degree rotation

### Numbers

- Number - numbers can be positive or negative (negative numbers may need to be put in brackets)

- Number + Number - the sum of the two numbers

- Number - Number - the second number subtracted from the first

- Number * Number - the product of the two numbers

- Number / Number - the first number divided by the second

### Time

- time - the number of seconds since the program started

- pulse - a value that moves smoothly between 0 and 1 - a sine wave of time with period of 1 second

- speed Number Picture - draws the given picture with time multiplied be the given number

- delay Number Picture - draws the given picture with time delayed by the given number

See the examples folder for use of all the above.

The language is still likely to change a lot. I've done no optimization of either the expression tree or the GL actions produced.

## Learning

Apart from this note, there's no documentation at the moment, but the examples should be pretty self explanatory. I've been implementing the language one feature at a time, so you might want to run through the commit history to see how it developed.

## Implementation

I started off by creating an embedded DSL in Haskell.

The implementation really became interesting when I added support for lambdas. Prior to that, I had an abstract syntax tree for your program (this is a deep embedding) and did a fairly simple transformation into a chain of IO actions that get executed for each frame. There was some complexity in the transformation in order to support the time varying values - which I handled using a reader monad to build up the operations in the IO monad. This code was perhaps a little confusing, but not particularly complicated.

Adding lambdas to the language representation was pretty simple, but handling variables in the transform to IO operations was where the fun began. I firstly altered my language representation to applications on primitives rather than nested expressions. Extending this with lambdas and variables formed a lambda calculus. I then implemented a transform to combinatory logic (SKI) which eliminates variables. This transform was actually pretty easy except for the replacement of a variable with the I combinator. The compiler was unable to verify that the types were correct. The solution was to introduce a type equivalence witness. Following this the transformation to the IO chain was trivial.

After that I decided it was time to go for an standalone language. This was mostly driven by wanting simpler syntax and easier to understand error messages. The parser was easy with a combinator library, but the fun bit was type checking and transforming from the untyped parse tree to the original GADT representation. I got pretty stuck on handling functions. The only way I could get it to work was by removing polymorphism from the language.

To see this development take a look at the commit history.


Have fun.
