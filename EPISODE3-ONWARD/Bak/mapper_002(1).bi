#Include Once "windows.bi"
#Include once "crt.bi"
#Include Once "crt/stdint.bi"

	'virtual bool cpuMapRead(uint16_t addr, uint32_t &mapped_addr)	 = 0;
	'virtual bool cpuMapWrite(uint16_t addr, uint32_t &mapped_addr, uint8_t data = 0)	 = 0;
	''// Transform PPU bus address into CHR ROM offset
	'virtual bool ppuMapRead(uint16_t addr, uint32_t &mapped_addr)	 = 0;
	'virtual bool ppuMapWrite(uint16_t addr, uint32_t &mapped_addr)	 = 0;

	'virtual void reset() = 0;
	
Declare Function ppuMapRead OverLoad (addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As bool
Declare Function cpuMapWrite(addr As uint16_t ,ByRef apped_addr As  uint32_t,  data1 As uint8_t = 0) As bool
Declare Function ppuMapRead(addr1 as uint16_t, byref mapped_addr as uint32_t) as bool 
Declare Function ppuMapWrite OverLoad (addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As bool 
Declare Sub reset_mapper()

 	
