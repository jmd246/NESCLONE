#include "../header/Bus.h"

Bus::Bus() {
	memory.fill(0);
}

//write	a	byte	to	memory
void	Bus::write(uint16_t	addr, uint8_t	data) {
	if (addr >= 0x0000 && addr <= 0xffff) {
		memory[addr] = data;
	}
}

uint8_t	Bus::read(uint16_t	addr,bool mode	= false) {
	if (addr >= 0x0000 && addr <= 0xffff) {
		return	memory[addr];
	}
	return	0x00;
}