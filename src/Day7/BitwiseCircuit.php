<?php
namespace AdventOfCode\Day7;

class BitwiseCircuit
{
    private $wiring = [];
    private $allowedInstructions;

    const MAX_SIGNAL_VALUE = 65535;

    public function __construct()
    {
        $this->allowedInstructions = [
            'AND' => function ($a, $b) {
                return $this->getValue($a) & $this->getValue($b);
            },
            'OR' => function ($a, $b) {
                return $this->getValue($a) | $this->getValue($b);
            },
            'LSHIFT' => function ($a, $b) {
                return $this->getValue($a) << (int)$b;
            },
            'RSHIFT' => function ($a, $b) {
                return $this->getValue($a) >> (int)$b;
            },
            'NOT' => function ($notused, $a) {
                //Implement bitwise NOT as this, because we need to use unsigned ints, PHP uses signed
                return self::MAX_SIGNAL_VALUE - $this->getValue($a);
            }
        ];
    }

    public function addWiring($wiringInstruction)
    {
        list($instruction, $wire) = array_map('trim', explode(' -> ', $wiringInstruction));
        $this->wiring[$wire] = $instruction;
    }

    public function getWireValue($wire)
    {
        $instruction = $this->wiring[$wire];

        foreach ($this->allowedInstructions as $allowedInstruction => $function) {
            if (strpos($instruction, $allowedInstruction) !== false) {
                list($a, $b) = array_map('trim', explode($allowedInstruction, $instruction));
                $answer = call_user_func($function, $a, $b);
                $this->wiring[$wire] = $answer;
                return $answer;
            }
        }

        return $this->getValue($instruction);
    }

    private function getValue($operand)
    {
        if (array_key_exists($operand, $this->wiring)) {
            return $this->getWireValue($operand);
        }

        return (int) $operand;
    }
}
