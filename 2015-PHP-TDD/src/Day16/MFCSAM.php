<?php
namespace AdventOfCode\Day16;

class MFCSAM
{
    private $auntSues = [];
    private $discoveries = [];

    public function addSue($argument1)
    {
        if (preg_match('/^Sue (\d+): (.*)$/', $argument1, $matches)) {
            $this->auntSues[$matches[1]] = $this->parseProperties($matches[2]);
        }
    }

    private function parseProperties($propertiesString)
    {
        $return = [];

        foreach (explode(",", $propertiesString) as $property) {
            list($name, $value) = array_map('trim', explode(':', $property));
            $return[$name] = $value;
        }

        return $return;
    }

    public function addDiscovery($argument1)
    {
        list($name, $value) = array_map('trim', explode(':', $argument1));
        $this->discoveries[$name] = $value;
    }

    public function findSue($testName = 'test')
    {
        return array_keys(array_filter($this->auntSues, [$this, $testName]))[0];
    }

    private function test($sueProperties)
    {
        foreach ($this->discoveries as $discoveryName => $discoveryValue) {
            if (isset($sueProperties[$discoveryName]) && $sueProperties[$discoveryName] != $discoveryValue) {
                return false;
            }
        }

        return true;
    }

    private function test2($sueProperties)
    {
        foreach ($this->discoveries as $discoveryName => $discoveryValue) {
            switch ($discoveryName) {
                case 'cats':
                case 'trees':
                    if (isset($sueProperties[$discoveryName]) && $sueProperties[$discoveryName] <= $discoveryValue) {
                        return false;
                    }
                    break;

                case 'pomeranians':
                case 'glodfish':
                    if (isset($sueProperties[$discoveryName]) && $sueProperties[$discoveryName] >= $discoveryValue) {
                        return false;
                    }
                    break;

                default:
                    if (isset($sueProperties[$discoveryName]) && $sueProperties[$discoveryName] != $discoveryValue) {
                        return false;
                    }
                    break;
            }
        }

        return true;
    }
}
