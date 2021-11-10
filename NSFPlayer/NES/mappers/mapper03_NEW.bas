#Include once "windows.bi"
#Include Once"crt.bi"
#Include Once "mapper_NEW.bas"



  Type Mapper_03 extends Mapper
  	
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
  nCHRBankSelect as 	uint8_t = &H00

  End Type

Constructor Mapper_03(prgbanks As uint8_t,chrbanks As uint8_t) 
							 base(prgbanks ,chrbanks)


End Constructor
Destructor Mapper_03()


End Destructor


  Sub Mapper_03.resetmappper(hard as boolean)   
   this.nCHRBankSelect = 0
  End Sub
  
  Function Mapper_03.cpuMapWrite( addr1 As uint16_t, ByRef mapped_addr As uint32_t ,data1 As  uint8_t ) As boolean
	if (addr1 >= &H8000 and addr1 <= &HFFFF) then
	 
		this.nCHRBankSelect = data1 and &H03 
		mapped_addr = addr1		
	end if

	'// Mapper has handled write, but do not update ROMs
	'return false;s handled write, but do not update ROMs
	return false 

End function 

Function Mapper_03.cpuMapRead(addr1 As uint16_t , ByRef mapped_addr As uint32_t, ByRef data1 As uint8_t ) As boolean 
 
	if (addr1 >= &H8000 and addr1 <= &HFFFF) then
  
		if (this.nPRGBanks  = 1) then'// 16K ROM 
			mapped_addr = addr1 and &H3FFF
		end if 
		if (this.nPRGBanks =  2) then '// 32K ROM
			mapped_addr = addr1 and &H7FFF 
			
		end if
		return true 
	 
	else
		return false 
	end if
		
		
End function 
  
Function Mapper_03.ppuMapRead(addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As boolean 
 
	if (addr1 < &H2000) then
	 
		mapped_addr = this.nCHRBankSelect * &H2000 + addr1
		return true 
	 
	else
		return false 
	End if

End function 

Function Mapper_03.ppuMapWrite(addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As boolean 
 
return false
End Function

 function Mapper_03.GetBattery() as boolean 'TVECTORUINT8_T ptr 'TVECTORUINT8_T
			
			'					dim sram_save as integer = FreeFile
			'
			'open "kirbyadventure.batt" for binary as sram_save
			'' 
			' put # sram_save,,*cast(uint8_t ptr,@this.vRAMStatic[0]),32*1024
			''
			'close sram_save
		
	return true '@this.vRAMStatic
 End Function
 
 function Mapper_03.SetBattery()  as boolean'TVECTORUINT8_T
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
