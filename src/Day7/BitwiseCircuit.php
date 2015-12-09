<?php
namespace AdventOfCode\Day7;

class BitwiseCircuit
{
    private $wiring = [];

    public function addWiring($wiringInstruction)
    {
        list($instruction, $wire) = array_map('trim', explode(' -> ', $wiringInstruction));
        $this->wiring[$wire] = $instruction;
    }

    public function getWireValue($wire)
    {
        $allowedInstructions = [
            'AND' => function ($a, $b) {
                return $this->getWireValue($a) & $this->getWireValue($b);
            },
            'OR' => function ($a, $b) {
                return $this->getWireValue($a) | $this->getWireValue($b);
            },
            'LSHIFT' => function ($a, $b) {
                return $this->getWireValue($a) << (int)$b;
            },
            'RSHIFT' => function ($a, $b) {
                return $this->getWireValue($a) >> (int)$b;
            },
            'NOT' => function ($notused, $a) {
                return 65535 - $this->getWireValue($a); // PHP uses unsigned bits, use this to get around the fact
            }
        ];

        if (array_key_exists($wire, $this->wiring)) {
            $instruction = $this->wiring[$wire];

            foreach ($allowedInstructions as $allowedInstruction => $function) {
                if (strpos($instruction, $allowedInstruction) !== false) {
                    return $this->wiring[$wire] =
                        call_user_func_array($function, array_map('trim', explode($allowedInstruction, $instruction)));
                }
            }

            return $this->getWireValue($instruction);
        }

        return (int) $wire;
    }
}
