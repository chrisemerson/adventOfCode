<?php
namespace AdventOfCode\Day23;

class TuringLock
{
    private $a = 0;
    private $b = 0;

    private $instructions = [];

    public function getRegisterValue($registerId)
    {
        return $this->$registerId;
    }

    public function addInstruction($instruction)
    {
        $this->instructions[] = $instruction;
    }

    public function runProgram()
    {
        for ($lineNumber = 0; $this->isValidInstruction($lineNumber); $lineNumber++) {
            $instruction = $this->parseInstruction($lineNumber);
            switch ($instruction[0]) {
                case 'inc':
                    $this->{$instruction[1]}++;
                    break;

                case 'hlf':
                    $this->{$instruction[1]} /= 2;
                    break;

                case 'tpl':
                    $this->{$instruction[1]} *= 3;
                    break;

                case 'jmp':
                    $lineNumber += intval($instruction[1]) - 1;
                    break;

                case 'jie':
                    if ($this->registerIsEven($instruction[1])) {
                        $lineNumber += intval($instruction[2]) - 1;
                    }
                    break;

                case 'jio':
                    if ($this->registerIsOne($instruction[1])) {
                        $lineNumber += intval($instruction[2]) - 1;
                    }
                    break;
            }
        }
    }

    private function isValidInstruction($lineNumber)
    {
        return isset($this->instructions[$lineNumber]);
    }

    private function parseInstruction($lineNumber)
    {
        return array_map(
            function ($item) {
                return trim($item, ',');
            },
            explode(" ", $this->instructions[$lineNumber])
        );
    }

    private function registerIsEven($register)
    {
        return $this->$register % 2 == 0;
    }

    private function registerIsOne($register)
    {
        return $this->$register == 1;
    }
}
