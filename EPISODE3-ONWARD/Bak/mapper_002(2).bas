#Include Once "windows.bi"
#Include once "crt.bi"
#Include Once "crt/stdint.bi"

#Include Once "mapper.bi"



	Dim Shared nPRGBankSelectLo As uint8_t = 0 
	Dim Shared nPRGBankSelectHi As uint8_t = 0

 Declare Sub mapper_002(prgBanks As uint8_t ,chrBanks As uint8_t)

 Sub mapper_002(prgBanks As uint8_t ,chrBanks As uint8_t)

 	nPRGBanks = prgBanks
	nCHRBanks = chrBanks
 	
 End Sub
 
 
 
 Sub resetmapper overload ()
 	
 	nPRGBankSelectLo = 0
	nPRGBankSelectHi = nPRGBanks - 1
 	
 End Sub



Function cpuMapRead OverLoad (addr1 As uint16_t , ByRef mapped_addr As uint32_t,ByRef data1 As uint8_t = 0 ) As bool
 
	if (addr1 >= &H8000 And addr1 <= &HBFFF) Then
	 
		mapped_addr = nPRGBankSelectLo * &H4000 + (addr1 And &H3FFF)
		return true
   End If
	if (addr1 >= &HC000 and addr1 <= &HFFFF) then
 
		mapped_addr = nPRGBankSelectHi * &H4000 + (addr1 And &H3FFF)
		return true 
	EndIf
	
	return false 
 
End function 



Function cpuMapWrite OverLoad (addr1 As uint16_t , ByRef mapped_addr As uint32_t,data1 As uint8_t ) As bool
 
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





Function ppuMapRead OverLoad (addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As bool
 
	if (addr1 < &H2000) then
	 
		mapped_addr = addr1 
		return true 
	 
	else
		return false 
End if
End function 

Function ppuMapWrite OverLoad (addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As bool
 
	if (addr1 < &H2000) Then
	 
		if (nCHRBanks = 0) then'// Treating as RAM
	 
			mapped_addr = addr1 
			return true 
	End If
End If 
	return false 

End Function



'
'Sleep
'
'
