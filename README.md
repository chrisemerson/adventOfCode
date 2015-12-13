Advent Of Code
==============

This repository contains my code solutions for the Advent Of Code challenges [http://www.adventofcode.com].

I don't claim these are the best or most efficient solutions, but I do like some of them :)

If you download the repository to give it a go yourself, you will need to have composer installed
[http://getcomposer.org] and run `composer install` to download and install PHPSpec, which I use for the tests.

Tests can be found in `spec/`, solutions in `src/`, my puzzle inputs in `res/` and run files to get each solution in
`run/`.

To run all the tests (some are pretty slow because of inefficient code), run `bin/phpspec run` on the command line. You
also just run the tests for a certain day with, eg, `bin/phpspec run spec/Day7`
