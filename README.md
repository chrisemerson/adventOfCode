Advent Of Code
==============

This repository contains my code solutions for the Advent Of Code challenges [http://www.adventofcode.com].

I don't claim these are the best or most efficient solutions, but I do like some of them :)

2015
----

If you download the repository to give it a go yourself, you will need to have composer installed
[http://getcomposer.org] and run `composer install` within the `2015-PHP-TDD` directory to download and install PHPSpec, which I
use for the tests.

Tests can be found in `2015-PHP-TDD/spec/`, solutions in `2015-PHP-TDD/src/`, my puzzle inputs in `2015-PHP-TDD/res/` and run files to get each
solution in `2015-PHP-TDD/run/`.

To run all the tests (some are pretty slow because of inefficient code), run `bin/phpspec run` on the command line in
the `2015-PHP-TDD` directory. You also just run the tests for a certain day with, eg, `bin/phpspec run spec/Day7`

2016
----

I'm going to attempt the 2016 challenges in Clojure, a language I know a very small amount of but really like and would
like to get better at. You will need to have Leiningen installed [http://leiningen.org/] if you want to run them yourself.

To get started, change to the `2016-Clojure` directory and run

    $ lein deps
    
This will download the project dependencies. To run the code for a particular day, run

    $ lein run d ...args

...substituting the day number for d. For example, day 1 takes the input as a file name with the instructions:

    $ lein run 1 "resources/day1"

2017
----

Right, no more excuses - it's about time I learned Python. This ended up being the first year I completed all 50 stars
 - hooray!

2018
----

Decided to use Go this year, but wasn't a fan. I still have a few to finish off so will come back to it.

2019
----

Kotlin this year - really loved using this language. Loads of functional features, lots of stuff made really elegant for
the developer, thumbs up!

2020
----

Decided to try Rust this year. I can see how it's good for the applications for which it was designed, but I think it's
a pretty niche case, and there's a lot of boilerplate / admin / babysitting you have to do for 99% of applications which
don't need it. Glad I have given it a go though.

2021
----

I ended up using TypeScript this year - it was inevitable one year that I'd use one of JS or TS, thought I should
improve my knowledge in this area and would much rather use TS than JS.

2022
----

After doing some .NET at work in the past I thought I should give C# a go and see how I get on with it, given I enjoyed
working with it on a project.

2023
----

Haskell... How bad can it be? It has a reputation as a bit of a language for academics, though I thought it was actually
pretty pleasant to use and quite easy to pick up given my Clojure knowledge. Let's see if i get further than in 2016
which was the last time I tried to use a functional programming language... and hopefully leading to use F# in the
future!

2024
----

Using Ruby this year mainly so I can upskill and help out with a website that is built on Ruby on Rails more easily.