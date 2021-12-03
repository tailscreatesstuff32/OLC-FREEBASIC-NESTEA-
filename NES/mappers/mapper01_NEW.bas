#Include once "windows.bi"
#Include Once"crt.bi"
#Include Once "mapper_NEW.bas"
#include once "containers/vector.bi"

MVectorTemplate(uint8_t)
  Type Mapper_01 extends Mapper
  	
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
  	
  	declare function _mirror() as MIRROR
  	 
  	
  	private:
  	
	nCHRBankSelect4Lo as uint8_t = &H00 
   nCHRBankSelect4Hi as uint8_t = &H00 
	nCHRBankSelect8 as uint8_t = &H00 

	nPRGBankSelect16Lo as uint8_t  = &H00 
	nPRGBankSelect16Hi as uint8_t = &H00 
	nPRGBankSelect32 as uint8_t = &H00 

	nLoadRegister as uint8_t  = &H00 
	nLoadRegisterCount as uint8_t  = &H00 
	nControlRegister as uint8_t = &H00 

   mirrormode as MIRROR= MIRROR.HORIZONTAL

   vRAMStatic As TVECTORUINT8_T
 
  End Type

Constructor Mapper_01(prgbanks As uint8_t,chrbanks As uint8_t) 
							 base(prgbanks,chrbanks)
  
  this.vRAMStatic.resize(32*1024)



End Constructor
Destructor Mapper_01()


End Destructor


  Sub Mapper_01.resetmappper(hard as boolean)   

if hard then
      if bat_saves = false then
   '	if this.vRAMStatic.size() <> 0 then
   	
   	this.vRAMStatic.clear
   	this.vRAMStatic.resize(32*1024)
   	
   	''
      EndIf
end if









   this.nControlRegister = &H1C 
	this.nLoadRegister = &H00 
	nLoadRegisterCount = &H00 
	
	this.nCHRBankSelect4Lo = 0 
	this.nCHRBankSelect4Hi = 0 
	this.nCHRBankSelect8 = 0 

	this.nPRGBankSelect32 = 0 
	this.nPRGBankSelect16Lo = 0 
	this.nPRGBankSelect16Hi = nPRGBanks - 1 





  End Sub
  
  Function Mapper_01.cpuMapWrite( addr1 As uint16_t, ByRef mapped_addr As uint32_t ,data1 As  uint8_t ) As boolean
if (addr1 >= &H6000 and addr1 <= &H7FFF) then
	 
		'// Write is to static ram on cartridge
		mapped_addr = &HFFFFFFFF 

		'// Write data to RAM
		vRAMStatic[addr1 and &H1FFF] = data1 

		'// Signal mapper has handled request
		return true 
end if

	if (addr1 >= &H8000) then
	' 
		if (data1 and &H80) then
	'	 
			'// MSB is set, so reset serial loading
			nLoadRegister = &H00 
			nLoadRegisterCount = 0 
			nControlRegister = nControlRegister or &H0C 
	'	
		else
	'	
	'		'// Load data in serially into load register
	'		'// It arrives LSB first, so implant this at
	'		'// bit 5. After 5 writes, the register is ready
			nLoadRegister shr= 1 
			nLoadRegister or= (data1 and &H01) shl 4 
			nLoadRegisterCount+=1

			if (nLoadRegisterCount = 5) then
	'		 
	'			'// Get Mapper Target Register, by examining
	'			'// bits 13 & 14 of the address
				dim nTargetRegister as uint8_t = (addr1 shr 13) and &H03 

				if (nTargetRegister =  0) then '// 0x8000 - 0x9FFF
	'			
	'				'// Set Control Register
					nControlRegister = nLoadRegister and &H1F 

					select case(nControlRegister and &H03)
					 
					case 0:	mirrormode = ONESCREEN_LO 
					case 1: mirrormode = ONESCREEN_HI 
					case 2: mirrormode = VERTICAL 
					case 3:	mirrormode = HORIZONTAL 
					end select
				 
				elseif (nTargetRegister  = 1) then'// 0xA000 - 0xBFFF
	'			 
	'				'// Set CHR Bank Lo
					if (nControlRegister and &b10000) then 
				 
						'// 4K CHR Bank at PPU 0x0000
						nCHRBankSelect4Lo = nLoadRegister and &H1F 
					 
					else
					 
						'// 8K CHR Bank at PPU 0x0000
						nCHRBankSelect8 = nLoadRegister and &H1E 
					 
				 end if
				elseif (nTargetRegister  = 2)  then'// 0xC000 - 0xDFFF
	'			 
	'				'// Set CHR Bank Hi
					if (nControlRegister and &b10000) then
					 
					'	// 4K CHR Bank at PPU 0x1000
						nCHRBankSelect4Hi = nLoadRegister and &H1F 
					end if
	'		
				elseif (nTargetRegister = 3) then'// 0xE000 - 0xFFFF
	'			
	'			'// Configure PRG Banks
					dim nPRGMode as uint8_t = (nControlRegister shr 2) and &H03 

					if (nPRGMode = 0 or nPRGMode = 1) then
					 
					'// Set 32K PRG Bank at CPU 0x8000
						nPRGBankSelect32 = (nLoadRegister and &H0E)shr 1 
					 
					elseif (nPRGMode = 2) then
				 
						'// Fix 16KB PRG Bank at CPU 0x8000 to First Bank
						nPRGBankSelect16Lo = 0 
						'// Set 16KB PRG Bank at CPU 0xC000
						nPRGBankSelect16Hi = nLoadRegister and &H0F 
					 
					elseif (nPRGMode = 3) then
				 
						'// Set 16KB PRG Bank at CPU 0x8000
						nPRGBankSelect16Lo = nLoadRegister and &H0F 
						'// Fix 16KB PRG Bank at CPU 0xC000 to Last Bank
						nPRGBankSelect16Hi = nPRGBanks - 1 
					end if
				end if

	'			'// 5 bits were written, and decoded, so
	'			'// reset load register
				nLoadRegister = &H00 
				nLoadRegisterCount = 0 
			end if
		end if

	end if

	'// Mapper has handled write, but do not update ROMs
	return false



End function 

Function Mapper_01.cpuMapRead(addr1 As uint16_t , ByRef mapped_addr As uint32_t, ByRef data1 As uint8_t ) As boolean 

	if (addr1 >= &H6000 and addr1<= &H7FFF) then
	 
		'// Read is from static ram on cartridge
		mapped_addr = &HFFFFFFFF 

		'// Read data from RAM
		data1 = vRAMStatic[addr1 and &H1FFF] 

		'// Signal mapper has handled request
		return true 
	end if

	if (addr1 >= &H8000) then
	 
		if (nControlRegister and &B01000) then
	 
			'// 16K Mode
			if (addr1 >= &H8000 and addr1 <= &HBFFF) then
			 
				mapped_addr = nPRGBankSelect16Lo * &H4000 + (addr1 and &H3FFF) 
				return true 
			end if

			if (addr1 >= &HC000 and addr1 <= &HFFFF) then
			
				mapped_addr = nPRGBankSelect16Hi * &H4000 + (addr1 and &H3FFF)
				return true
			
		 
		else
		 
			'// 32K Mode
			mapped_addr = nPRGBankSelect32 * &H8000 + (addr1 and &H7FFF) 
			return true 
			end if
		end if
end if

	

	return false 
End function 
  
Function Mapper_01.ppuMapRead(addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As boolean 
	
 	if (addr1 < &H2000) then
	
		if (nCHRBanks = 0) then
		 
			mapped_addr = addr1 
			return true 
		 
		else
		 
			if (nControlRegister and &B10000) then
		 
			'// 4K CHR Bank Mode
				if (addr1 >= &H0000 and addr1 <= &H0FFF) then
				 
					mapped_addr = nCHRBankSelect4Lo * &H1000 + (addr1 and &H0FFF) 
					return true 
			   end if
				if (addr1 >= &H1000 and addr1 <= &H1FFF) then
			 
					mapped_addr = nCHRBankSelect4Hi * &H1000 + (addr1 and &H0FFF) 
					return true 
				end if
			 
			else
		 
			'	// 8K CHR Bank Mode
				mapped_addr = nCHRBankSelect8 * &H2000 + (addr1 and &H1FFF) 
				return true 
			end if
		end if
	end if	

	return false 
 
End Function


Function Mapper_01.ppuMapWrite(addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As boolean 
 
	if (addr1 < &H2000) then
	 
		if (nCHRBanks = 0) then
		 
			mapped_addr = addr1 
			return true 
		end if

		return true 
	
	else
		return false 
	end if
End Function



function  Mapper_01._mirror() as MIRROR
	Return this.mirrormode
End Function

 function Mapper_01.GetBattery() as boolean 'TVECTORUINT8_T ptr 'TVECTORUINT8_T
			
			'					dim sram_save as integer = FreeFile
			'
			'open "kirbyadventure.batt" for binary as sram_save
			'' 
			' put # sram_save,,*cast(uint8_t ptr,@this.vRAMStatic[0]),32*1024
			''
			'close sram_save
		
	return true '@this.vRAMStatic
 End Function
 
 function Mapper_01.SetBattery()  as boolean'TVECTORUINT8_T
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
