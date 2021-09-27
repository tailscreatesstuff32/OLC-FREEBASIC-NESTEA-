#Include Once "windows.bi"
#Include once "crt.bi"
#Include Once "crt/stdint.bi"
#Include Once "containers/vector.bi"
#Include Once "mapper.bi"
	'virtual bool cpuMapRead(uint16_t addr, uint32_t &mapped_addr)	 = 0;
	'virtual bool cpuMapWrite(uint16_t addr, uint32_t &mapped_addr, uint8_t data = 0)	 = 0;
	''// Transform PPU bus address into CHR ROM offset
	'virtual bool ppuMapRead(uint16_t addr, uint32_t &mapped_addr)	 = 0;
	'virtual bool ppuMapWrite(uint16_t addr, uint32_t &mapped_addr)	 = 0;

	'virtual void reset() = 0;
	Declare function reset_mapper OverLoad() As MIRROR
'Declare Function ppuMapRead OverLoad (addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As bool
'Declare Function cpuMapWrite OverLoad (addr As uint16_t ,ByRef apped_addr As  uint32_t,  data1 As uint8_t = 0) As bool
'Declare Function cpuMapRead OverLoad (addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As bool
'Declare Function ppuMapWrite OverLoad (addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As bool 
'Declare Sub reset_mapper()
Declare Sub get_scanline()

dim shared bIRQActive As bool = false 
dim Shared bIRQEnable As bool =  false 
dim Shared bIRQUpdate As bool =  false 

Dim Shared nTargetRegister As uint8_t
Dim Shared bPRGBankMode As bool = false 
Dim Shared bCHRInversion As bool  = false 

Dim Shared mirrormode As MIRROR = HORIZONTAL

MVectorTemplate(uint8_t)
Dim Shared vRAMStatic As TVECTORUINT8_T

Dim shared pRegister(8) As uint32_t
Dim shared pCHRBank(8)  As uint32_t
Dim shared pPRGBank(4)  As uint32_t

Dim Shared nIRQCounter As uint16_t 
Dim Shared nIRQReload  As uint16_t


