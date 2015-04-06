#Structure and Interpretation of Computer Programs

##Problem set descriptions

###Introductory assignment
`00-intro/`

Relevant section in text: 1.1

The goal of this initial assignment is to get students typing at the computer
and running Scheme. It deals with evaluating simple expressions, both on paper
and using the interpreter, and it introduces some of the basic mechanics of
using the Edwin editor.

This assignment also introduces the Scheme debugger.  In our teaching, we've
found it very valuable to get students using the debugger right at the outset.
This helps instill the habit of thinking about the errors they encounter, which
saves a lot of grief in later assignments. The assignment also includes some
playing around with the ways in which our computers at MIT are connected to the
rest of the network.

In order to make much use of this assignment (beyond the simple evaluation
exercises) you will need to be running the Edwin editor and integrated
debugger, which is included with the MIT distribution of Scheme. If you are not
using this version of Scheme, we suggest that you create an analogous initial
assignment to give students experience with the simple mechanics of using the
system, before they have to confront the more difficult conceptual material to
come.

###The Game of Twenty-One
`01-twenty-one/`

Relevant section in text: 1.3

This assignment presents a simple game-playing program, in which strategies are
represented as higher-order procedures. The prisoner's dilemma assignment takes
a similar approach.

###Graphing with higher-order procedures
`02-graphing-hof/`

Relevant section in text: 1.3

The assignment develops a representation of points in the plane as procedures
that take 0 or 1 as an argument and returns, correspondingly, an x or y
coordinate. (This is precisely the procedural representation of pairs described
in section 2.1.3.) Starting with this, students build a functional language of
curves and transformations on curves, including a recursive transformation that
draws fractal curves.

This assignment can be a bit mindblowing, since it gives
such a fast immersion into higher-order procedures.

###Continued fractions
`03-cont-frac/`

Relevant section in text: 1.3

The assignment describes the use of higher-order procedures to implement
operations on continued fractions. The material is a more elaborate (and
earlier form) of book exercises 1.37, 1.38, and 1.39. There is no source code
required for this assignment.

###RSA encryption
`04-rsa/`

Relevant sections in text: 1.2.6, 2.2.1

The assignment begins with the fast exponentiation and primality testing in
section 1.2 and expands it to produce an implementation of the RSA system for
public-key encryption. Students will also need to use lists to represent
sequences and write simple car-cdr manipulations.

Before assigning this, you should consider updating "the story" to something
more contemporary and less MIT-specific.

Disclaimer for all the cryptographers who are waiting to pounce on us: The
system described here is a "pure RSA" system, which is not secure in practice.

###Prisoner's dilemma
`05-prisoners-dilemma/`

Relevant sections in text: 1.3, 2.2.1

This assignment presents a simple game-playing program, in which strategies are
represented as higher-order procedures. The Game of Twenty-One assignment takes
a similar approach. The prisoner's dilemma game also uses some simple
list-manipulation commands. There is an opportunity here to run prisoner's
dilemma tournament for the class.'

We are grateful to Franklyn Turbak, Mike Eisenberg, and Mitch Resnick for
developing this assignment.

###Picture language
`06-pict-lang/`

Relevant section in text: 2.2.4

This is the original material on the square-limit language, which was revised
to form section 2.2.4 of the book. For explanations of how the language works,
you should use the book rather than the assignment text given here. We have not
included a postscript file here, because the explanation (and the figures) in
the book are better.

The code below includes an implementation of primitive
painters for line-drawings and two-dimensional images. This code is specific to
MIT Scheme. It includes a constructor that creates primitive painters from pgm
files.

###Term-rewriting evaluator
`07-term-rewrite-evaluator/`

Relevant sections in text: 1.1, 2.3

This is an extensive assignment, which develops an alternative view of
evaluation to the one presented in chapters 3 and 4 of the text. This view
extends the informal substitution model of section 1.1 to a full term-rewriting
model for evaluating Scheme expressions. Working through this assignment
provides experience with the formal view of language syntax and semantics, as
well as considerable exercise with symbolic manipulation.

In order to do the assignment, students will need to read the supplementary
handout (provided below), which describes term rewriting and the evaluation
model.

We are grateful to Prof. Albert Meyer of MIT for developing this assignment and
the underlying model.

###Freshman advisor
`08-freshman-adv/`

Relevant sections in text: 2.3

This assignment, which provides experience with symbol manipulation, explores
the possibility of replacing the college advising system with a rather stupid
conversational program, based loosely on Joseph Weizenbaum's famous "Doctor"
program of the 1960's. For this assignment, we provide a rule-based pattern
matcher, so that the assignment serves as an introduction to rule-based
programming as well as to symbol manipulation.

The pattern matcher here is quite sophisticated. It uses success and failure
continuations to implement backtracking for segment variables (much in the
style of the AMB evaluator of section 4.3). At this point in the course, we
typically do not ask students to study the implementation of the pattern
matcher, but only to use it as a black box, although we sometimes include a
lecture on this material. We do not have explanatory notes for the matcher
available, but you can find an explanation of a similar matcher in Abelson and
Sussman, "Lisp: a Language for Stratified Design," Byte Magazine, February
1988.

###Generic arithmetic
`09-generic-arith/`

Relevant sections in text: 2.4, 2.5

This assignment is based on sections 2.4 and 2.5 of the text, which discuss a
generic arithmetic system that is capable of dealing with rational functions
(quotients of polynomials).

###Object-oriented adventure game
`10-oo-game/`

Relevant sections in text: 3.1, 3.2, 3.3

The assignment introduces object-oriented programming and a simple model of
inheritance. The object system presented here is based on the one presented by
J. Rees and N. Adams, "Object-oriented programming in Scheme," in the 1988 ACM
Conference on Lisp and Functional Programming. One of the underlying messages
here is that in a language like Scheme, which includes first-class procedures,
it is easy to implement a variety of object-oriented programming models. If you
plan to use this assignment, we recommend that you present a lecture on the
material covered in the supplementary notes included below.

The actual assignment has students extend and modify an adventure-style game,
where the objects are people, places, and various magical things. The
particular setup presented here includes a lot of MIT-specific places and
jokes, and you should modify it accordingly. The inclusion of a troll who eats
the course lecturers, or consigns them to some other horrible fate, is highly
recommended.

###Concurrency
`11-concurrency/`

Relevant sections in text: 3.4

This assignment explores issues of synchronization and concurrency in the
context of simulating a currency-exchange market.

The code for this assignment relies on the parallel-execute facility (see the
text, section 3.4). If you are not running MIT Scheme, this may be difficult to
arrange. The file parallel.scm below gives an implementation of
parallel-execute in terms of primitives included in MIT Scheme, but which are
not part of the Scheme standard.

###Streams and series
`12-streams/`

Relevant sections in text: 3.5

This assignment explores the use of infinite streams to represent power series.

###Evaluators
`13-evaluators/`

Relevant sections in text: 4.1, 4.2

This assignment covers the evaluators presented at the beginning of chapter 4:
the metacircular evaluator of section 4.1, the analyze evaluator of section
4.1.7, and the normal-order evaluator of section 4.2.

###Languages for object-oriented programming
`14-oop-lang/`

Relevant sections in text: 4.1

In this assignment, students assume the role of language designer, and consider
issues in the design of languages for object-oriented programming by working
with a version of the metacircular evaluator that has been extended to include
classes, methods, and inheritance. The programming language used here is
modelled on the Dylan language, and the extensions to the evaluator follow the
approach described in The Art of the Metaobject Protocol, by Gregor Kiczales,
Jim des Rivieres, and Dan Bobrow (MIT Press, 1991).

This is a different object-oriented programming system than the one used in the
object-oriented adventure game assignment.

Note: The construction of this assignment predates our final revison of the
text for this second edition, so there are several minor differences between
the code here and the metacircular evaluator as given in section 4.1. You may
wish to update the code (notably some of the syntax procedures and data
structures) before assigning this.

###Register Machines and Compilation
`15-compiler/`

Relevant sections in text: 5.1, 5.2, 5.4., 5.5

This assignment covers the three major programs introduced in chapter 5 --
register-machine simulator, explicit-control evaluator and compiler. There is a
lot a lot of actual programming required here, and it can be done without
mastering the details of these three programs. Nevertheless, students will be
dealing wit a lot of code and they will need time to assimilate it.
