#pragma once
#include   <array>
#include "CPU6502.h"
class Bus
{

//devices
public:
    std::array<uint8_t, 64 * 1024>  memory;
    CPU6502 cpu;
public:
    
    Bus();
    ~Bus();
    
    uint8_t read(uint16_t a, bool mode = false);
    
    void write(uint16_t a, uint8_t d);
};

