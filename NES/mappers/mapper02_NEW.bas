#Include once "windows.bi"
#Include Once"crt.bi"
#Include Once "mapper_NEW.bas"



  Type Mapper_02 extends Mapper
  	
  	Public:
  Declare Constructor(prgbanks As uint8_t,chrbanks As uint8_t)
  declare Destructor()
  	
  	Public:
  	Declare function cpuMapRead(addr As uint16_t,ByRef mapped_addr As uint32_t , byref byte_data As uint8_t) As BOOLEAN
  	Declare function cpuMapWrite(addr As uint16_t,ByRef mapped_addr As uint32_t ,byte_data As uint8_t = 0) As BOOLEAN
  	Declare function ppuMapRead(addr As uint16_t,ByRef mapped_addr As uint32_t) As BOOLEAN
  	Declare function ppuMapWrite(addr As uint16_t,ByRef mapped_addr As uint32_t) As BOOLEAN
  	Declare Sub resetmappper(hard as boolean)
  	
   Declare function SetBattery()  as boolean	
  	declare function GetBattery() as boolean'TVECTORUINT8_T ptr'TVECTORUINT8_T
  	
  	
  	private:
  	nPRGBankSelectLo As uint8_t = 0 
	nPRGBankSelectHi As uint8_t = 0


  End Type

Constructor Mapper_02(prgbanks As uint8_t,chrbanks As uint8_t) 
							 base(prgbanks ,chrbanks)


End Constructor
Destructor Mapper_02()


End Destructor


  Sub Mapper_02.resetmappper(hard as boolean)   
  	this.nPRGBankSelectLo = 0
	this.nPRGBankSelectHi = this.nPRGBanks - 1
  End Sub
  
  Function Mapper_02.cpuMapWrite( addr1 As uint16_t, ByRef mapped_addr As uint32_t ,data1 As  uint8_t ) As boolean
' print this.nPRGBanks
'print this.nCHRBanks
	'// if PRGROM is 16KB
	'//     CPU Address Bus          PRG ROM
	'//     0x8000 -> 0xBFFF: Map    0x0000 -> 0x3FFF
	'//     0xC000 -> 0xFFFF: Mirror 0x0000 -> 0x3FFF
	'// if PRGROM is 32KB
	'//     CPU Address Bus          PRG ROM
	'//     0x8000 -> 0xFFFF: Map    0x0000 -> 0x7FFF	
	if (addr1 >= &H8000 and addr1 <= &HFFFF) then
	 		
		nPRGBankSelectLo = data1 And &H0F 
	End If

	'// Mapper has handled write, but do not update ROMs
	return false 

End function 

Function Mapper_02.cpuMapRead(addr1 As uint16_t , ByRef mapped_addr As uint32_t, ByRef data1 As uint8_t ) As boolean 
 
	If (addr1 >= &H8000 And addr1 <= &HBFFF) Then
	 
		mapped_addr = this.nPRGBankSelectLo * &H4000 + (addr1 And &H3FFF)
		return true
   End If
	if (addr1 >= &HC000 and addr1 <= &HFFFF) then
 
		mapped_addr = this.nPRGBankSelectHi * &H4000 + (addr1 And &H3FFF)
		return true 
	EndIf
	
	return false 
 

End function 
  
Function Mapper_02.ppuMapRead(addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As boolean 
 
	if (addr1 < &H2000) then
	 
		mapped_addr = addr1 
		return true 
	 
	else
		return false 
	End if

End function 

Function Mapper_02.ppuMapWrite(addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As boolean 
 
	if (addr1 < &H2000) Then
	 
		If (this.nCHRBanks = 0) then'// Treating as RAM
	 
			mapped_addr = addr1 
			return true 
	End If
End If 
	return false 

End Function

 function Mapper_02.GetBattery() as boolean 'TVECTORUINT8_T ptr 'TVECTORUINT8_T
			
			'					dim sram_save as integer = FreeFile
			'
			'open "kirbyadventure.batt" for binary as sram_save
			'' 
			' put # sram_save,,*cast(uint8_t ptr,@this.vRAMStatic[0]),32*1024
			''
			'close sram_save
		
	return true '@this.vRAMStatic
 End Function
 
 function Mapper_02.SetBattery()  as boolean'TVECTORUINT8_T
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
	return true 
	
 End Function








