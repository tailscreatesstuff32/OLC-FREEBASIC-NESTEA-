#Include Once "windows.bi"
#Include Once "containers/vector.bi"
'#include "nes/Cartridge.bi"
MVectorTemplate(uint8_t)

	Declare functon puRead( addr1 As uint16_t, uint8_t &data1) As bool 
	Declare functon cpuWrite(addr1 As uint16_t,  data1 As uint8_t)As bool

	'// Communication with PPU Bus
	Declare functon ppuRead(addr1 As uint16_t, uint8_t &data1)As bool
	Declare functon ppuWrite(addr1 As uint16_t,  data1 As uint8_t) As bool
   Declare Sub cart_reset()
   Declare sub cartridge( sFileName As String)

	Dim As uint8_t nMapperID = 0 
	Dim As uint8_t nPRGBanks = 0 
	Dim As uint8_t nCHRBanks = 0 

	Enum MIRROR
	
		HORIZONTAL 
		VERTICAL 
		ONESCREEN_LO 
		ONESCREEN_HI 
	End Enum
	
	Dim mirror As MIRROR =  HORIZONTAL 
	
	Type sHeader
	 
		name As String * 4 
		prg_rom_chunks As uint8_t 
		chr_rom_chunks As uint8_t  
	   mapper1 as uint8_t
		mapper2 As uint8_t 
		prg_ram_size As uint8_t 
		tv_system1 As uint8_t 
		tv_system2 As uint8_t  
		unused As String * 5 
		
	End Type
	
	Dim Shared header As sHeader
	
	Dim bImageValid As bool 
	
	bImageValid = FALSE
	
	T'std::vector<uint8_t> vPRGMemory;
	'std::vector<uint8_t> vCHRMemory;
	
	
	Sub cartridge( sFileName As String)
		
		
		
		
		
	End Sub
	
	
	
	
	
	Sub cart_reset()
		
		
		
		
		
	End Sub
	
	
	
	
	
