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

Right, no more excuses - it's about time I learned Python.
