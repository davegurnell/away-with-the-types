# Away with the types!

&copy; 2017 Dave Gurnell.

Slides licensed [CC-BY-SA-3.0][].

Code samples licensed [Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0).

Slides and demos for a talk I gave
on programming with partial type information
at [Scala Exchange][] on 14th December 2017.

## Abstract

Static types are fantastic for ensuring code quality,
but rather annoyingly they have to be defined at compile time.
What happens when you need to determine
the structure of your data at run time?
For example, when you have to load schemas from a database?

Do you throw away static types and go dynamic? Of course not!
You have to change the level of abstraction in your code.
In this session we will explore this process,
working through a real-life example
of switching from domain-specific static types
to a composable data description language.

How do you keep your code boilerplate-free?
How do you maintain correctness?
And how do you write expressions that operate on data
for which you don't have simple static types?

Come along for answers to these questions and more.

[CC-BY-SA-3.0]: http://creativecommons.org/licenses/by/3.0/
[Apache 2.0]: https://www.apache.org/licenses/LICENSE-2.0
[Scala Exchange]: http://scala-exchange.com

[Dave Gurnell]: https://davegurnell.com
[Underscore]: https://underscore.io
