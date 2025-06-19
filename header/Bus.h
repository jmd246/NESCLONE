#pragma once
class Bus
{
public:
    
    Bus();
    ~Bus();
    uint8_t read(uint16_t a, bool mode = false);
    
    void write(uint16_t a, uint8_t d);
};

