#Include Once "windows.bi"
#Include once "crt.bi"
#Include Once "crt/stdint.bi"

#Include Once "mapper.bi"



 Sub mapper_000(prgBanks As uint8_t ,chrBanks As uint8_t)

 	nPRGBanks = prgBanks
	nCHRBanks = chrBanks
 	
 End Sub
 
 
 
 Sub resetmapper overload ()
 	
 	
 	
 End Sub



Function cpuMapRead OverLoad (addr1 As uint16_t , ByRef mapped_addr As uint32_t, ByRef data1 As uint8_t ) As bool
 
	'// if PRGROM is 16KB
	'//     CPU Address Bus          PRG ROM
	'//     0x8000 -> 0xBFFF: Map    0x0000 -> 0x3FFF
	'//     0xC000 -> 0xFFFF: Mirror 0x0000 -> 0x3FFF
	'// if PRGROM is 32KB
	'//     CPU Address Bus          PRG ROM
	'//     0x8000 -> 0xFFFF: Map    0x0000 -> 0x7FFF	
	if (addr1 >= &H8000 And addr1 <= &HFFFF) Then
	 
		mapped_addr = addr1 And iif(nPRGBanks > 1,&H7FFF , &H3FFF) 
		return true 
	End If

	return false

End function 



Function cpuMapWrite( addr1 As uint16_t, ByRef mapped_addr As uint32_t ,data1 As  uint8_t ) As bool
 
	'// if PRGROM is 16KB
	'//     CPU Address Bus          PRG ROM
	'//     0x8000 -> 0xBFFF: Map    0x0000 -> 0x3FFF
	'//     0xC000 -> 0xFFFF: Mirror 0x0000 -> 0x3FFF
	'// if PRGROM is 32KB
	'//     CPU Address Bus          PRG ROM
	'//     0x8000 -> 0xFFFF: Map    0x0000 -> 0x7FFF	
	if (addr1 >= &H8000 And addr1 <= &HFFFF) Then
	 
		mapped_addr = addr1 And iif(nPRGBanks > 1,&H7FFF , &H3FFF) 
		return true 
	End If

	return false

End function 





Function ppuMapRead OverLoad (addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As bool
 
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

Function ppuMapWrite OverLoad (addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As bool
 
	'// if PRGROM is 16KB
	'//     CPU Address Bus          PRG ROM
	'//     0x8000 -> 0xBFFF: Map    0x0000 -> 0x3FFF
	'//     0xC000 -> 0xFFFF: Mirror 0x0000 -> 0x3FFF
	'// if PRGROM is 32KB
	'//     CPU Address Bus          PRG ROM
	'//     0x8000 -> 0xFFFF: Map    0x0000 -> 0x7FFF	
	if (addr1 >= &H0000 And addr1 <= &H1FFF) Then
	 
	
	 If (nCHRBanks = 0) Then
	 	mapped_addr = addr1 
		return TRUE 
	 EndIf
	
		
		
		
	End If

	return FALSE

End Function



'
'Sleep
'
'
