#include "../header/Bus.h"
#include  "../header/CPU6502.h"

CPU6502::CPU6502() {
	using a = CPU6502;
	//resembles the table from nes wiki
	lookup =
	{
		{ "BRK", &a::BRK, &a::IMM, 7 },{ "ORA", &a::ORA, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 3 },{ "ORA", &a::ORA, &a::ZP0, 3 },{ "ASL", &a::ASL, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PHP", &a::PHP, &a::IMP, 3 },{ "ORA", &a::ORA, &a::IMM, 2 },{ "ASL", &a::ASL, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::NOP, &a::IMP, 4 },{ "ORA", &a::ORA, &a::ABS, 4 },{ "ASL", &a::ASL, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BPL", &a::BPL, &a::REL, 2 },{ "ORA", &a::ORA, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "ORA", &a::ORA, &a::ZPX, 4 },{ "ASL", &a::ASL, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "CLC", &a::CLC, &a::IMP, 2 },{ "ORA", &a::ORA, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "ORA", &a::ORA, &a::ABX, 4 },{ "ASL", &a::ASL, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
		{ "JSR", &a::JSR, &a::ABS, 6 },{ "AND", &a::AND, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "BIT", &a::BIT, &a::ZP0, 3 },{ "AND", &a::AND, &a::ZP0, 3 },{ "ROL", &a::ROL, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PLP", &a::PLP, &a::IMP, 4 },{ "AND", &a::AND, &a::IMM, 2 },{ "ROL", &a::ROL, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "BIT", &a::BIT, &a::ABS, 4 },{ "AND", &a::AND, &a::ABS, 4 },{ "ROL", &a::ROL, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BMI", &a::BMI, &a::REL, 2 },{ "AND", &a::AND, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "AND", &a::AND, &a::ZPX, 4 },{ "ROL", &a::ROL, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "SEC", &a::SEC, &a::IMP, 2 },{ "AND", &a::AND, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "AND", &a::AND, &a::ABX, 4 },{ "ROL", &a::ROL, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
		{ "RTI", &a::RTI, &a::IMP, 6 },{ "EOR", &a::EOR, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 3 },{ "EOR", &a::EOR, &a::ZP0, 3 },{ "LSR", &a::LSR, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PHA", &a::PHA, &a::IMP, 3 },{ "EOR", &a::EOR, &a::IMM, 2 },{ "LSR", &a::LSR, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "JMP", &a::JMP, &a::ABS, 3 },{ "EOR", &a::EOR, &a::ABS, 4 },{ "???", &a::XXX, &a::IMP, 6 },{ "LSR", &a::LSR, &a::ACC, 2 },
		{ "BVC", &a::BVC, &a::REL, 2 },{ "EOR", &a::EOR, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "EOR", &a::EOR, &a::ZPX, 4 },{ "LSR", &a::LSR, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "CLI", &a::CLI, &a::IMP, 2 },{ "EOR", &a::EOR, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "EOR", &a::EOR, &a::ABX, 4 },{ "???", &a::XXX, &a::IMP, 7 },{ "LSR", &a::LSR, &a::ZP0, 5 },
		{ "RTS", &a::RTS, &a::IMP, 6 },{ "ADC", &a::ADC, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 3 },{ "ADC", &a::ADC, &a::ZP0, 3 },{ "ROR", &a::ROR, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PLA", &a::PLA, &a::IMP, 4 },{ "ADC", &a::ADC, &a::IMM, 2 },{ "ROR", &a::ROR, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "JMP", &a::JMP, &a::IND, 5 },{ "ADC", &a::ADC, &a::ABS, 4 },{ "???", &a::XXX, &a::IMP, 6 },{ "LSR", &a::LSR, &a::ZPX, 6 },
		{ "BVS", &a::BVS, &a::REL, 2 },{ "ADC", &a::ADC, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "ADC", &a::ADC, &a::ZPX, 4 },{ "ROR", &a::ROR, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "SEI", &a::SEI, &a::IMP, 2 },{ "ADC", &a::ADC, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "ADC", &a::ADC, &a::ABX, 4 },{ "???", &a::XXX, &a::IMP, 7 },{ "LSR", &a::LSR, &a::ABS, 6 },
		{ "???", &a::NOP, &a::IMP, 2 },{ "STA", &a::STA, &a::IZX, 6 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 6 },{ "STY", &a::STY, &a::ZP0, 3 },{ "STA", &a::STA, &a::ZP0, 3 },{ "STX", &a::STX, &a::ZP0, 3 },{ "???", &a::XXX, &a::IMP, 3 },{ "DEY", &a::DEY, &a::IMP, 2 },{ "???", &a::NOP, &a::IMP, 2 },{ "TXA", &a::TXA, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "STY", &a::STY, &a::ABS, 4 },{ "STA", &a::STA, &a::ABS, 4 },{ "STX", &a::STX, &a::ABS, 4 },{ "???", &a::XXX, &a::IMP, 4 },
		{ "BCC", &a::BCC, &a::REL, 2 },{ "STA", &a::STA, &a::IZY, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 6 },{ "STY", &a::STY, &a::ZPX, 4 },{ "STA", &a::STA, &a::ZPX, 4 },{ "STX", &a::STX, &a::ZPY, 4 },{ "???", &a::XXX, &a::IMP, 4 },{ "TYA", &a::TYA, &a::IMP, 2 },{ "STA", &a::STA, &a::ABY, 5 },{ "TXS", &a::TXS, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 5 },{ "???", &a::NOP, &a::IMP, 5 },{ "STA", &a::STA, &a::ABX, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "???", &a::XXX, &a::IMP, 5 },
		{ "LDY", &a::LDY, &a::IMM, 2 },{ "LDA", &a::LDA, &a::IZX, 6 },{ "LDX", &a::LDX, &a::IMM, 2 },{ "???", &a::XXX, &a::IMP, 6 },{ "LDY", &a::LDY, &a::ZP0, 3 },{ "LDA", &a::LDA, &a::ZP0, 3 },{ "LDX", &a::LDX, &a::ZP0, 3 },{ "???", &a::XXX, &a::IMP, 3 },{ "TAY", &a::TAY, &a::IMP, 2 },{ "LDA", &a::LDA, &a::IMM, 2 },{ "TAX", &a::TAX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "LDY", &a::LDY, &a::ABS, 4 },{ "LDA", &a::LDA, &a::ABS, 4 },{ "LDX", &a::LDX, &a::ABS, 4 },{ "???", &a::XXX, &a::IMP, 4 },
		{ "BCS", &a::BCS, &a::REL, 2 },{ "LDA", &a::LDA, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 5 },{ "LDY", &a::LDY, &a::ZPX, 4 },{ "LDA", &a::LDA, &a::ZPX, 4 },{ "LDX", &a::LDX, &a::ZPY, 4 },{ "???", &a::XXX, &a::IMP, 4 },{ "CLV", &a::CLV, &a::IMP, 2 },{ "LDA", &a::LDA, &a::ABY, 4 },{ "TSX", &a::TSX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 4 },{ "LDY", &a::LDY, &a::ABX, 4 },{ "LDA", &a::LDA, &a::ABX, 4 },{ "LDX", &a::LDX, &a::ABY, 4 },{ "???", &a::XXX, &a::IMP, 4 },
		{ "CPY", &a::CPY, &a::IMM, 2 },{ "CMP", &a::CMP, &a::IZX, 6 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "CPY", &a::CPY, &a::ZP0, 3 },{ "CMP", &a::CMP, &a::ZP0, 3 },{ "DEC", &a::DEC, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "INY", &a::INY, &a::IMP, 2 },{ "CMP", &a::CMP, &a::IMM, 2 },{ "DEX", &a::DEX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "CPY", &a::CPY, &a::ABS, 4 },{ "CMP", &a::CMP, &a::ABS, 4 },{ "DEC", &a::DEC, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BNE", &a::BNE, &a::REL, 2 },{ "CMP", &a::CMP, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "CMP", &a::CMP, &a::ZPX, 4 },{ "DEC", &a::DEC, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "CLD", &a::CLD, &a::IMP, 2 },{ "CMP", &a::CMP, &a::ABY, 4 },{ "NOP", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "CMP", &a::CMP, &a::ABX, 4 },{ "DEC", &a::DEC, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
		{ "CPX", &a::CPX, &a::IMM, 2 },{ "SBC", &a::SBC, &a::IZX, 6 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "CPX", &a::CPX, &a::ZP0, 3 },{ "SBC", &a::SBC, &a::ZP0, 3 },{ "INC", &a::INC, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "INX", &a::INX, &a::IMP, 2 },{ "SBC", &a::SBC, &a::IMM, 2 },{ "NOP", &a::NOP, &a::IMP, 2 },{ "???", &a::SBC, &a::IMP, 2 },{ "CPX", &a::CPX, &a::ABS, 4 },{ "SBC", &a::SBC, &a::ABS, 4 },{ "INC", &a::INC, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BEQ", &a::BEQ, &a::REL, 2 },{ "SBC", &a::SBC, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "SBC", &a::SBC, &a::ZPX, 4 },{ "INC", &a::INC, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "SED", &a::SED, &a::IMP, 2 },{ "SBC", &a::SBC, &a::ABY, 4 },{ "NOP", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "SBC", &a::SBC, &a::ABX, 4 },{ "INC", &a::INC, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
		{ "LSR", &a::LSR, &a::ABX, 7 },{"ROR",&a::ROR,&a::ACC,2},{"ROR",&a::ROR,&a::ZP0,5} ,{"ROR",&a::ROR,&a::ZPX,6},{"ROR",&a::ROR,&a::ABS,6},{"ROR",&a::ROR,&a::ABX,7}, {"ROL",&a::ROL,&a::ACC,2},{"ROL",&a::ROL,&a::ZP0,5}, {"ROL",& a::ROL,& a::ZPX,6}
	};
}

CPU6502::~CPU6502()
{
	// Destructor - has nothing to do
}

uint8_t	CPU6502::read(uint16_t	a) {
	return	bus->read(a,false);
}
void CPU6502::write(uint16_t addr, uint8_t data) {
	bus->write(addr, data);
}

void CPU6502::clock() {
	if (cycles == 0) {
		//fetch
		opcode = read(pc);
		//implement pc
		pc++;
		//get number of cycles for instruction


		uint8_t	additionalCycles1 = (this->*lookup[opcode].addrmode)();
		uint8_t	additionalCycles2 = (this->*lookup[opcode].operate)();

		cycles += (additionalCycles1 & additionalCycles2);

		//execute

	}
	cycles--;
}

void CPU6502::reset() {
	//reset the 3 registers
	a = 0;
	x = 0;
	y = 0;
	//reset stack pointer it grows downward from 0xFF to 0x00 leaves room for special stuff so we want to start at 0x
	sp = 0xFD;
	status = 0x00 | U;
	//reset abs addr to a particular value
	addr_abs = 0xFFFC;
	// get the pc from memory
	uint16_t lo = read(addr_abs),hi = read(addr_abs + 1);
	pc = (hi << 8) | lo;
	// reset the fetch value and the stored addresses
	fetched = 0x00;
	addr_abs = 0x0000;
	addr_rel = 0x0000;
	//simulate the delay needed for reset
	cycles = 8;
}



//12 addressing modes
//immediate address operand is next byte 
uint8_t CPU6502::IMM() {
	addr_abs = pc++;
	return 0;
}
//implied no data is returned but may be operating on accumalator
uint8_t CPU6502::IMP() {
	fetched = a;
	return 0;
}
// zero page 
uint8_t CPU6502::ZP0() {
   //get operand
	addr_abs = read(pc);
	//increment program counter
	pc++;
	//ensure its the zero page
	addr_abs &= 0x0FF;
	return	0;
}
//zero page page with x off set 
uint8_t CPU6502::ZPX() {
	//get	operand	add	x	register	to	it	and	wrap	back	to	zero	page
	addr_abs = (read(pc) + x);
	addr_abs	&= 0x0FF;
	pc++;
	return	0;
}
//zero page page with y off set 
uint8_t CPU6502::ZPY() {
	//get	operand	add	y register	to	it	and	wrap	back	to	zero	page
	addr_abs = (read(pc) + y);
	addr_abs &= 0x0FF;
	pc++;
	return	0;
}
//absolute address needs a 16 bit number gets lo and high byte then combinds them for complete address
uint8_t CPU6502::ABS() {
	uint16_t	lo = read(pc++),
		hi=read(pc++);
	addr_abs = (hi << 8) | lo;
		
	return	0;
}
//absolute address needs a 16 bit number gets lo and high byte then combinds them for complete address adds a x offset
uint8_t CPU6502::ABX() {
	uint16_t	lo = read(pc++),
		hi = read(pc++);
	addr_abs = ((hi << 8) | lo)	+	x;
	//check if page boundary was crossed if so add extra clock cycle  wrap around needs additional clock cyclr
	if ((addr_abs & 0xFF00) != (hi << 8)) {
		return 1;  
	}

	return	0;
}
//absolute address needs a 16 bit number gets lo and high byte then combinds them for complete address adds a x offset

uint8_t CPU6502::ABY() {
	uint16_t	lo = read(pc++),
		hi = read(pc++);
	addr_abs = ((hi << 8) | lo) + y;
	//check if page boundary was crossed if so add extra clock cycle
	if ((addr_abs & 0xFF00) != (hi << 8)) {
		return 1;
	}

	return	0;
}
uint8_t CPU6502::IND() {
	uint16_t	ptr_lo = read(pc++),
		ptr_hi = read(pc++);
	uint16_t ptr = ((ptr_hi << 8) | ptr_lo);
	//simulate page boundary  bug
	if (ptr == 0xFF) {
		uint16_t lo = read(ptr), hi = read(ptr & 0xFF00); //high byte wraps within page
		addr_abs = (hi << 8) | lo;
	}
	else {
		uint16_t lo = read(ptr), hi = read(ptr + 1);
		addr_abs = (hi << 8) | lo ;
	}
	return 0;
}
uint8_t CPU6502::IZX() {
   // get index pointer to the zero page
	uint16_t t = (read(pc++) + x) & 0x00FF;
	//get lo bytefrom zero page
	uint16_t lo = read(t & 0x00FF),
		hi = read((t + 1) & 0x00FF);
	addr_abs = (hi << 8) | lo;
}
uint8_t CPU6502::IZY() {
	uint16_t t = read(pc++); //get ptr from  instruction
	//get lo and hi byte
	uint16_t lo = read(t & 0x00FF),
		hi = read((t + 1) & 0x00FF);
	uint16_t base  = (hi << 8) | lo;
	addr_abs = base + y;
	//if	page	boundary	breached	increment	cycles
	if ((addr_abs & 0xFF00) != (hi<<8)) {
		return 1;
	}
	return 0;
}
uint8_t	CPU6502::REL() {
	addr_rel = read(pc++);
	//check	the	value	of	7th	bit	if	its	1	its	negative
	if (addr_rel & 0x80) {
		addr_rel |= 0xFF00;
	}
	return	0;
}
uint8_t CPU6502::ACC() {
	return 0;
}
//instructions
uint8_t CPU6502::fetch() {
	if (lookup[opcode].addrmode != IMM && lookup[opcode].addrmode != ACC) {
		//get value instruction will use from the address
		fetched = read(addr_abs);
	}
	else if (lookup[opcode].addrmode == ACC) {
		fetched = a;
	}
	return fetched;
}
uint8_t CPU6502::CLC() {
	//sets carry flag to 0
	setFlag(C, false);
	return 0;
}
uint8_t CPU6502::CLD() {
	//sets carry flag to 0
	setFlag(D, false);
	return 0;
}uint8_t CPU6502::CLI() {
	//sets carry flag to 0
	setFlag(I, false);
	return 0;
}uint8_t CPU6502::CLV() {
	//sets carry flag to 0
	setFlag(V, false);
	return 0;
}
uint8_t CPU6502::AND() {
	//fetch
	fetch();
	//and with val in accumulator
	a &= fetched;
	//and affects the zero and negative flag so reset them
	//if result of and is all zeros set zero flag
	setFlag(Z, a == 0x00); 
	//if bit 7 is 1 set N flag
	setFlag(N, a & 0x80);
	return 1; // may add a cycle depending on addr mode
}

//   branching
//will directly impact clock cycles

//branch  if cary is set
uint8_t CPU6502::BCS() {
	if (getFlag(C) == 1) {
		//add a cycle for branch
		cycles++;
		//set abs address to pc + jump offset 
		addr_abs = pc + addr_rel;
		// if page boundary cross add a cycle
		if ((addr_abs & 0xFF00) != (pc & 0xFF00)) {
			cycles++;
		}
		pc = addr_abs;
	}
	return 0;
}
//branch if cary clear
uint8_t CPU6502::BCC() {
	//branch  if cary is set
	if (getFlag(C) == 0) {
		//add a cycle for branch
		cycles++;
		//set abs address to pc + jump offset 
		addr_abs = pc + addr_rel;
		// if page boundary cross add a cycle
		if ((addr_abs & 0xFF00) != (pc & 0xFF00)) {
			cycles++;
		}
		pc = addr_abs;
	}
	return 0;
}

//branch if equal
uint8_t CPU6502::BEQ() {
	if (getFlag(Z) == 1) {
		//add a cycle for branch
		cycles++;
		//set abs address to pc + jump offset 
		addr_abs = pc + addr_rel;
		// if page boundary cross add a cycle
		if ((addr_abs & 0xFF00) != (pc & 0xFF00)) {
			cycles++;
		}
		pc = addr_abs;
	}
	return 0;
}
//branch if not equal
uint8_t CPU6502::BNE() {
	if (getFlag(Z) == 0) {
		//add a cycle for branch
		cycles++;
		//set abs address to pc + jump offset 
		addr_abs = pc + addr_rel;
		// if page boundary cross add a cycle
		if ((addr_abs & 0xFF00) != (pc & 0xFF00)) {
			cycles++;
		}
		pc = addr_abs;
	}
	return 0;
}
//branch if positive
uint8_t CPU6502::BPL() {
	if (getFlag(N) == 0) {
		//add a cycle for branch
		cycles++;
		//set abs address to pc + jump offset 
		addr_abs = pc + addr_rel;
		// if page boundary cross add a cycle
		if ((addr_abs & 0xFF00) != (pc & 0xFF00)) {
			cycles++;
		}
		pc = addr_abs;
	}
	return 0;
}
//branch if negative
uint8_t CPU6502::BMI() {
	if (getFlag(N) == 1) {
		//add a cycle for branch
		cycles++;
		//set abs address to pc + jump offset 
		addr_abs = pc + addr_rel;
		// if page boundary cross add a cycle
		if ((addr_abs & 0xFF00) != (pc & 0xFF00)) {
			cycles++;
		}
		pc = addr_abs;
	}
	return 0;
}
//branch if overflow
uint8_t CPU6502::BVC() {
	if (getFlag(V) == 0) {
		//add a cycle for branch
		cycles++;
		//set abs address to pc + jump offset 
		addr_abs = pc + addr_rel;
		// if page boundary cross add a cycle
		if ((addr_abs & 0xFF00) != (pc & 0xFF00)) {
			cycles++;
		}
		pc = addr_abs;
	}
	return 0;
}
//branch if no overflow
uint8_t CPU6502::BVS() {
	if (getFlag(V) == 1) {
		//add a cycle for branch
		cycles++;
		//set abs address to pc + jump offset 
		addr_abs = pc + addr_rel;
		// if page boundary cross add a cycle
		if ((addr_abs & 0xFF00) != (pc & 0xFF00)) {
			cycles++;
		}
		pc = addr_abs;
	}
	return 0;
}

// end of branching instructions

//problematic instructions addition and subtraction
// idea is fetch data and cary bit then add or subtract it from accumulator
//including cary bit alllows a chain of 8 bit additions that can from a larger digit
/*
   A = 250
   M = 10
   will cause over flow so just add low bytes will spit out carry then just add it to the high byte

   // the caveat is desingners thought that programmers may want to use signed numbers which will reduce the size of values that can be represented
   // truth table evaluation we get overflow v = ((A^R) & (~ (A^M)))
      these have the capability to add an additional cycle
   */
uint8_t CPU6502::ADC() {
	fetch();
	//including carry bit allows adding pass range of 8 bits
	uint16_t temp = (uint16_t) a + (uint16_t) getFlag(C) + (uint16_t) fetched;
	//if temp is more than 255 set cary bit
	setFlag(C, temp > 255);
	//if low byte is zero set zero flag
	setFlag(Z, (temp & 0x00FF) == 0);
	setFlag(N, temp & 0x80);
	//overflow v = ((A^R) & (~ (A^M)))
	setFlag(V,  ( ~( (uint16_t)a ^ (uint16_t) fetched) & ((uint16_t)a ^ temp)) & 0x0080);
	//set the low 8 bits
	a = temp & 0x00FF;
	return 1;
}
//A = A - M - (1-c) -> A = A - (M - (1-c)) -> A = A - M + c + 1
uint8_t CPU6502::SBC() {
	//occupy fetch with value at current address
	fetch();
	//invert bottom 8 bits of M to -M
	uint16_t val = ((uint16_t)fetched) ^ 0x00FF;
	//including carry bit allows subtracting pass range of 8 bits
	uint16_t temp = (uint16_t)a + (uint16_t)getFlag(C) + val;
	setFlag(C, temp &  0xFF00);
	//if low byte is zero set zero flag
	setFlag(Z, (temp & 0x00FF) == 0);
	setFlag(N, temp & 0x80);
	//overflow v = ((A^R) & ((A^M)))
	setFlag(V,   ((temp ^ (uint16_t) a )  & (temp ^ val)) & 0x0080 );
	//set low 8 bits
	a = temp & 0x00FF;
	return 1;
}
//stack instrucutions
//push
uint8_t CPU6502::PHA() {
   //write to the stack     (0x0100) is a hardcoded value where the memory for stack starts
	write(0x0100 + sp, a);  // push accumulator onto stack
	//decrement stack
	sp--;
	return 0;
}
uint8_t CPU6502::PHP() {
	//write to the stack     (0x0100) is a hardcoded value where the memory for stack starts
	write(0x0100 + sp, status | 0x30);  // push NV11BDIZC onto stack
	//decrement stack
	sp--;
	return 0;
}
//pop
uint8_t CPU6502::PLA() {
	sp++;
	//write to the stack     (0x0100) is a hardcoded value where the memory for stack starts
	a = read(0x0100 + sp);  // read stack at sp
	//set zero and negative flags
	setFlag(N, a & 0x80);
	setFlag(Z, a == 0x00);
	return 0;
}
//pull  processor  status
uint8_t CPU6502::PLP() {
	//1. increment stack pointer to get most recent byte
	sp++;
	//read byte from stack and load in into status
	status = read(0x0100 + sp);
	//clear bit B position 4 ;
	status &= 0xEF;
	//set bit U
	status |= 0x20;
	return 0;
}
// interuots
//maskable
void CPU6502::irq() {
	//only trigger if interupts are disabled
	if (getFlag(I) == 0) {
		//run code to servie interrupt 
		// 1. write lo and hi to stkptr 
		//low end
		write(0x0100 + sp, (pc >> 8) & 0x00FF);
		sp--;
		//high end
		write(0x0100 + sp, pc & 0x00FF);
		sp--;
		// 2. set flags
		setFlag(U, 1);
		setFlag(B, 0);
		// 3. write to status
		write(0x0100 + sp, status);
		sp--;
		//disable interupts
		setFlag(I, 1);
		//read new pc from irq vector
		addr_abs = 0xFFFE;
		uint16_t lo = read(addr_abs), hi = read(addr_abs + 1);		
		pc = (hi << 8) | lo;
		//takes 7 cycles
		cycles = 7;
	}
}
// non maskable
void CPU6502::nmi() {
	//push pc to stack
	write(0x0100 + sp, (pc >> 8) & 0x00FF);
	sp--;
	write(0x0100 + sp, pc & 0x00FF);
	sp--;
	//clear break and unused bit flags set status
	setFlag(U, 1);
	setFlag(B, 0);
	write(0x0100 + sp, status);
	sp--;
	//set interupt disable 
	setFlag(I, 1);
	//read new pc from nmi vector
	addr_abs = 0xFFFA;

	uint16_t lo = read(addr_abs), hi = read(addr_abs + 1);
	pc = ( hi << 8) | lo;
	//takes 8 cycles
	cycles = 8;
}
//return interrupt
uint8_t CPU6502::RTI() {
	//get status
	sp++;
	status = read(0x0100 + sp);
	//reset flags
	status &= ~B;
	status &= ~U;
	sp++;
	//get pc 
	pc = (uint16_t)read(0x0100 + sp);
	sp++;
	pc |= (uint16_t)read(0x0100 + sp) << 8;
	return 0;
}

//asl brk bit dec inc inx


uint8_t CPU6502::INC() {
	fetch();
	
	fetched += 1;
	
	setFlag(Z, fetched == 0x00);
	setFlag(N, fetched & 0x80);

	write(addr_abs, fetched);

	return 0;

}
uint8_t CPU6502::INX() {
	x += 1;
	setFlag(Z, x == 0x00);
	setFlag(N, x & 0x80);
	return 0;
}
uint8_t CPU6502::INY() {
	y += 1;
	setFlag(Z, y == 0x00);
	setFlag(N, y & 0x80);
	return 0;
}
uint8_t CPU6502::DEC() {
	fetch();

	fetched -= 1;

	setFlag(Z, fetched == 0x00);
	setFlag(N, fetched & 0x80);

	write(addr_abs, fetched);

	return 0;
}

uint8_t CPU6502::ROR() {
	//carry bit is value of bit 0
	if (lookup[opcode].addrmode == ACC) {
		uint8_t carryIn = getFlag(C) << 7;
		//save bit 0 into carry
		setFlag(C, a & 0x01);
		//carry in goes into bit 7
		a = (a >> 1) | carryIn;
		setFlag(Z, a == 0);
		setFlag(N, a & 0x80);
	}
	else {
		fetch();
		uint8_t carryIn = getFlag(C) << 7;
		setFlag(C, fetched & 0x01);
		uint8_t result = (fetched >> 1) | carryIn;
		setFlag(Z, result == 0);
		setFlag(N, result & 0x80);
		//write to memory
		write(addr_abs, result);
	}
	return 0;
}
uint8_t CPU6502::ROL() {
	if (lookup[opcode].addrmode == ACC) {
		uint8_t carryIn = getFlag(C);
		//save bit 7 into carry
		setFlag(C, a & 0x80);
		a = (a << 1) | carryIn;
		setFlag(Z, a == 0);
		setFlag(N, a & 0x80);
	}
	else {
		//fetch from memory
		fetch();
		uint8_t carryIn = getFlag(C);
		//save bit 7 into carry
		setFlag(C, fetched & 0x80);
		uint8_t result = (fetched << 1) | carryIn;
		setFlag(Z, result == 0);
		setFlag(N, result & 0x80);
		write(addr_abs, result);	
	}
	return 0;
}

uint8_t CPU6502::LSR() {
	//operate on either memory or accumulator
	if (lookup[opcode].addrmode == &CPU6502::ACC) {
		//carry bit is value of bit 0
		setFlag(C, a & 0x01);
		//shift right
		a >>= 1;
		//negative bit is set to 0
		setFlag(N, 0);
		setFlag(Z, a == 0x00);
	}
	else {
		fetch();
		setFlag(C, fetched & 0x01);
		uint8_t result = fetched >> 1;
		setFlag(Z, result == 0x00);
		setFlag(N, 0);
		write(addr_abs, result);
	}
	return 0;
}
uint8_t CPU6502::JMP() {

}
uint8_t CPU6502::DEX() {
	x -= 1;
	setFlag(Z, x == 0x00);
	setFlag(N, x & 0x80);
	return 0;
}
uint8_t CPU6502::DEY() {
	y -= 1;
	setFlag(Z, y == 0x00);
	setFlag(N, y & 0x80);
	return 0;

}
uint8_t CPU6502::EOR() {
   // -- a = a ^ memory
      //occupy memory
	fetch();
	a = a ^ fetched;
	   //set flags 
	setFlag(Z, a == 0x00);
	setFlag(N, a & 0x80);
	return 0;
}
uint8_t CPU6502::CPX() {

}
uint8_t CPU6502::CPY() {

}
uint8_t CPU6502::LDX() {

}
uint8_t CPU6502::LDY() {

}
uint8_t CPU6502::LDA() {

}
uint8_t CPU6502::BIT() {

}
uint8_t CPU6502::BRK() {

}
uint8_t CPU6502::ASL() {

}
uint8_t CPU6502::TXA() {

}
uint8_t CPU6502::TYA() {

}
uint8_t CPU6502::CMP() {

}
uint8_t CPU6502::JSR() {

}