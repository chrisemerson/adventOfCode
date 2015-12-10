<?php
namespace AdventOfCode\Day10;

class LookAndSay
{
    public function lookAndSay($input, $iterations = 1)
    {
        for ($i = 0; $i < $iterations; $i++) {
            $input = $this->performLookAndSayOnSingleString($input);
        }

        return $input;
    }

    private function performLookAndSayOnSingleString($input)
    {
        $output = '';

        while (preg_match('/^((.)\2*)/', $input, $matches)) {
            $output .= strlen($matches[0]) . $matches[2];
            $input = substr($input, strlen($matches[0]));
        }

        return $output;
    }
}
