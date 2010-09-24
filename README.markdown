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

For more examples see the examples folder.

## Running the examples

To install the package, switch to the root directory of the package and run

    cabal install

To run an example use the ghc command:

    ghc -fglasgow-exts -O2 -o xxx --make examples/xxx.hs

where xxx.hs is whichever example you want to run. To run it just type:

    xxx

## Status

All the examples above should work now. The language supports the following primitives:

### Shapes

- circle - A circle of unit radius (fills the window)

- star - A 5 pointed star of unit radius

- square - A 2 x 2 square (fills the window)

### Color

- color Color Picture - draws the given picture in the given color

- red, green, blue, yellow, cyan, magenta, white, black, pink, purple - colors

- rgb Red Green Blue - produces a color from the given proportions of red green and blue as numbers from 0 to 1

- rgba Red Green Blue Alpha - as rgb, but also takes an alpha value from 0 (transparent) to 1 (solid)

### Transformation

- scale Size Picture - draws the given picture scaled by the given size - a positive number where 1 is original size or a pair of numbers

- move Position Picture - draws the given picture at the given position - a pair of coordinates where (-1,-1) is the bottom left and (1,1) is the top right

- rotate Amount Picture - draws the given picture rotated by the given amount - 1 is a full rotation

### Composition

- Picture +> Picture - draws the first picture then the second

### Numbers

- Literal - numbers can be positive or negative (negative numbers may need to be put in brackets)

- Number + Number - the sum of the two numbers

- Number - Number - the second number subtracted from the first

- Number * Number - the product of the two numbers

- Number / Number - the first number divided by the second

### Time

- time - the number of seconds since the program started

- pulse - a value that moves smoothly between 0 and 1 - a sine wave of time with period of 1 second

- speed Number Picture - draws the given picture with time multiplied be the given number

- delay Number Picture - draws the given picture with time delayed by the given number

### Repetition

- rotations Number Picture - draws the given picture the given number of times, rotated at even amounts

- grid Size Picture - repeats the given picture in a grid of the given size (width, height), scaled to fit in the cells

- with Value Lambda - calls the given lambda with the given value - of no current use, but needed internally

- withSteps Steps Range Lambda - calls the given lambda with each step in the given range and draws the pictures produced

See the examples folder for use of all the above.

The language is likely to change a lot and will probably be reimplemented as an external DSL. I've done no optimization of either the expression tree or the GL actions produced.

## Learning

Apart from this note, there's no documentation at the moment, but the examples should be pretty self explanatory. I've been implementing the language one feature at a time, so you might want to run through the commit history to see how it developed.

## Implementation

The implementation really became interesting when I added support for lambdas. Prior to that, I had an abstract syntax tree for your program (this is a deep embedding) and did a fairly simple transformation into a chain of IO actions that get executed for each frame. There was some complexity in the transformation in order to handle the time varying values - which I handled using a reader monad to build up the operations in the IO monad. This code was perhaps a little confusing, but not particularly complicated.

Adding lambdas to the language representation was pretty simple, but handling variables in the transform to IO operations was where the fun began. I firstly altered my language representation to applications on primitives rather than nested expressions. Extending this with lambdas and variables formed a lambda calculus. I then implemented a transform to combinatory logic (SKI) which eliminates variables. This transform was actually pretty easy except for the replacement of a variable with the I combinator. The compiler was unable to verify that the types were correct. The solution was to introduce a type equivalence witness. Following this the transformation to the IO chain was trivial.

To see this development take a look at the commit history.


Have fun.
