#Include once "windows.bi"
#Include Once"crt.bi"
#Include Once "mapper_NEW.bas"



  Type Mapper_00 extends Mapper
  	
  	Public:
  Declare Constructor(prgbanks As uint8_t,chrbanks As uint8_t)
  declare Destructor()
  	
  	Public:
  	Declare function cpuMapRead(addr As uint16_t,ByRef mapped_addr As uint32_t , byref byte_data As uint8_t) As BOOLEAN
  	Declare function cpuMapWrite(addr As uint16_t,ByRef mapped_addr As uint32_t ,byte_data As uint8_t = 0) As BOOLEAN
  	Declare function ppuMapRead(addr As uint16_t,ByRef mapped_addr As uint32_t) As BOOLEAN
  	Declare function ppuMapWrite(addr As uint16_t,ByRef mapped_addr As uint32_t) As BOOLEAN
  	  	
  	Declare function SetBattery()  as boolean	
  	declare function GetBattery() as boolean'TVECTORUINT8_T ptr'TVECTORUINT8_T
  	
  	
  	Declare Sub resetmappper(hard as boolean)
  	
  	
  	private:
  	 vRAMStatic As TVECTORUINT8_T 

  End Type

Constructor Mapper_00(prgbanks As uint8_t,chrbanks As uint8_t) ': base(prgbanks As uint8_t,chrbanks As uint8_t) 
							 base(prgbanks ,chrbanks)
							 
							 
		this.vRAMStatic.clear
   	this.vRAMStatic.resize(32*1024)
							 

End Constructor
Destructor Mapper_00()


End Destructor

 
 
 
 '  Constructor Mapper_00()
 ' 
 ' 

 ' 
 'End Constructor

  Sub Mapper_00.resetmappper(hard as boolean)   
  	
  
'hard reset////////////////////////////////////
    if bat_saves = false then
   '	if this.vRAMStatic.size() <> 0 then
   	
   	this.vRAMStatic.clear
   	this.vRAMStatic.resize(32*1024)
   	
   	''
    EndIf
   	
   	'erase SRAM
   	
 
   
'//////////////////////////////////////////////
  	
  End Sub
  
  Function Mapper_00.cpuMapWrite( addr1 As uint16_t, ByRef mapped_addr As uint32_t ,data1 As  uint8_t ) As boolean
 
	'// if PRGROM is 16KB
	'//     CPU Address Bus          PRG ROM
	'//     0x8000 -> 0xBFFF: Map    0x0000 -> 0x3FFF
	'//     0xC000 -> 0xFFFF: Mirror 0x0000 -> 0x3FFF
	'// if PRGROM is 32KB
	'//     CPU Address Bus          PRG ROM
	'//     0x8000 -> 0xFFFF: Map    0x0000 -> 0x7FFF	
	if (addr1 >= &H8000 And addr1 <= &HFFFF) Then
	 
		mapped_addr = addr1 And iif(this.nPRGBanks > 1,&H7FFF , &H3FFF) 
		return true 
	End If

	return false

End function 

  Function Mapper_00.cpuMapRead(addr1 As uint16_t , ByRef mapped_addr As uint32_t, ByRef data1 As uint8_t ) As boolean 
 
	'// if PRGROM is 16KB
	'//     CPU Address Bus          PRG ROM
	'//     0x8000 -> 0xBFFF: Map    0x0000 -> 0x3FFF
	'//     0xC000 -> 0xFFFF: Mirror 0x0000 -> 0x3FFF
	'// if PRGROM is 32KB
	'//     CPU Address Bus          PRG ROM
	'//     0x8000 -> 0xFFFF: Map    0x0000 -> 0x7FFF	
	if (addr1 >= &H8000 And addr1 <= &HFFFF) Then
	 
		mapped_addr = addr1 And iif(this.nPRGBanks > 1,&H7FFF , &H3FFF) 
		return true 
	End If

	return false

End function 
  
Function Mapper_00.ppuMapRead(addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As boolean 
 
	'// if PRGROM is 16KB
	'//     CPU Address Bus          PRG ROM
	'//     0x8000 -> 0xBFFF: Map    0x0000 -> 0x3FFF
	'//     0xC000 -> 0xFFFF: Mirror 0x0000 -> 0x3FFF
	'// if PRGROM is 32KB
	'//     CPU Address Bus          PRG ROM
	'//     0x8000 -> 0xFFFF: Map    0x0000 -> 0x7FFF	
	if (addr1 >= &H0000 And addr1 <= &H1FFF) Then
	  
		mapped_addr = addr1 
		return true 
	End If

	return false

End function 

Function Mapper_00.ppuMapWrite(addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As boolean 
 
	'// if PRGROM is 16KB
	'//     CPU Address Bus          PRG ROM
	'//     0x8000 -> 0xBFFF: Map    0x0000 -> 0x3FFF
	'//     0xC000 -> 0xFFFF: Mirror 0x0000 -> 0x3FFF
	'// if PRGROM is 32KB
	'//     CPU Address Bus          PRG ROM
	'//     0x8000 -> 0xFFFF: Map    0x0000 -> 0x7FFF	
	if (addr1 >= &H0000 And addr1 <= &H1FFF) Then
	 
	
	 If (this.nCHRBanks = 0) Then
	 	mapped_addr = addr1 
		return TRUE 
	 EndIf
	
		
		
		
	End If

	return FALSE

End Function



 function Mapper_00.GetBattery() as boolean 'TVECTORUINT8_T ptr 'TVECTORUINT8_T
			
			'					dim sram_save as integer = FreeFile
			'
			'open "kirbyadventure.batt" for binary as sram_save
			'' 
			' put # sram_save,,*cast(uint8_t ptr,@this.vRAMStatic[0]),32*1024
			''
			'close sram_save
		
	return true '@this.vRAMStatic
 End Function
 
 function Mapper_00.SetBattery()  as boolean'TVECTORUINT8_T
	'dim sram_save as integer = freefile
	'open "C:\Users\Gamer\Desktop\Kirby's Adventure (USA)\kirbyadventure.batt" for binary as sram_save
	'		 
			'   this.vRAMStatic.clear						
  ''this.vRAMStatic.resize(32*1024)
  ''
			 
			 
			 
	'		 get # sram_save,,this.vRAMStatic[0],32*1024
	'		
	'		close sram_save
	'		
	'return true 
	
 End Function




