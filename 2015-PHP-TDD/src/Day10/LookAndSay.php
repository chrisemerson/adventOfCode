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

        if (preg_match_all('/(.)\1*/', $input, $groups)) {
            foreach ($groups[0] as $group) {
                $output .= strlen($group) . $group[0];
            }
        }

        return $output;
    }
}
