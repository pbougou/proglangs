# Programming Languages ΙΙ

| **Title** | **Description** |
| ----- | ----- |
|**[Algorithms in Haskell](algorithms_in_haskell/)**| Algorithmic implentations in Haskell. |
| **[Property-based testing in Haskell's QuickCheck](bird_tree/)** | <ul> <li> Tree data structure implementation and testing<il><li> [Bird's tree](http://www.cs.ox.ac.uk/ralf.hinze/publications/Bird.pdf) implementation and testing <il> </ul> |
| **[Virtual machines](interpreter/)** | Using GNU C extensions and [these techniques] to eliminate branch prediction overhead (https://courses.softlab.ntua.gr/pl2/2009b/slides/vm.pdf), there are implemented interpreters for: <ul><li>[Befunge 93](http://catseye.tc/view/befunge-93/doc/Befunge-93.markdown)<il><li>Bytecode language [defined here](https://courses.softlab.ntua.gr/pl2/2017b/exercises/vm.pdf) <il><ul> |
| **[Garbage collector](mark_and_sweep_gc/)** | It is integrated with the interpreter for bytecode language. Algorithms and techniques used for gc: <ul><li>Find all unreachable memory objects with mark and sweep.<il><li>GC is called every time a new heap allocation happens.<il><ul>  |
| **[Axiomatic semantics](frama-c/)** | For program verification in C programming language, [Frama-C](http://frama-c.com/) is used. There are verified two algorithms' implementations:<ul><li>Find the maximum number of same consecutive elements in an array.<il><li>Check if an array contains the same element twice.<il><ul> |
| **[Type inference in Haskell](type_inference/)** | Type inference a la Hidney-Milner for the [simply-typed lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus). |
| **[Denotational semantics](denotational_semantics/)**  | Implementation of denotational_semantics for two small languages in Haskell: <ul><li><il>Simple lambda calculus with IO(see [continuation passing IO par.7.6](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/fpio.pdf)), which is [defined](https://courses.softlab.ntua.gr/pl2/2016b/exercises/densem.pdf) in course's exercise. <li>[While++](https://courses.softlab.ntua.gr/pl2/2017b/exercises/densem.pdf), a simple imperative language with side effects<il><ul> |
| **[Scripting Languages](scripting/)**  | Implementation of websites, which host a game, with PHP and client-side scripting with Python for playing and winning the game.   |
