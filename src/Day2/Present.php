<?php
namespace AdventOfCode\Day2;

class Present
{
    private $length = 0;
    private $width = 0;
    private $height = 0;

    private $LWSide = 0;
    private $WHSide = 0;
    private $HLSide = 0;

    const DIMENSION_SEPARATOR = 'x';

    public function __construct($dimensions)
    {
        list($this->length, $this->width, $this->height) = explode(self::DIMENSION_SEPARATOR, $dimensions);

        $this->HLSide = $this->height * $this->length;
        $this->WHSide = $this->width * $this->height;
        $this->LWSide = $this->length * $this->width;
    }

    public function getSurfaceArea()
    {
        return 2 * ($this->LWSide + $this->WHSide + $this->HLSide);
    }

    public function getAreaOfSmallestSize()
    {
        return min($this->LWSide, $this->WHSide, $this->HLSide);
    }

    public function getSmallestPerimeter()
    {
        $lwPerim = 2 * ($this->length + $this->width);
        $whPerim = 2 * ($this->width + $this->height);
        $hlPerim = 2 * ($this->height + $this->length);

        return min($lwPerim, $whPerim, $hlPerim);
    }

    public function getVolume()
    {
        return $this->length * $this->width * $this->height;
    }
}
