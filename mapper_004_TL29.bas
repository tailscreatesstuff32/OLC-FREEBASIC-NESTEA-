#Include "mapper_004.bi"
Dim Shared lastread As uint16_t
'Declare function reset_mapper OverLoad() As MIRROR
Sub mapper_004(prgBanks As uint8_t ,chrBanks As uint8_t)
 	
	vRAMStatic.resize(32*1024)

 	nPRGBanks = prgBanks
	nCHRBanks = chrBanks
 	resetmapper
End Sub


Function cpuMapWrite( addr1 As uint16_t, ByRef mapped_addr As uint32_t ,data1 As  uint8_t ) As bool
Dim tmp1 As bool


	if (addr1 >= &H6000 and addr1 <= &H7FFF) Then
	 
		'// Write is to static ram on cartridge
		mapped_addr = &HFFFFFFFF 

	'	// Write data to RAM
		vRAMStatic[addr1 and &H1FFF] = data1 

		'// Signal mapper has handled request
		return true 
	End If
'///////////////////////////////////////////


	If (addr1 >= &H8000 and addr1 <= &H9FFF) Then
	  tmp1 = IIf(addr1 And &H0001,0,1)
		'// Bank Select
		if  tmp1 Then
		 
			nTargetRegister = data1 And &H07 
			bPRGBankMode = (data1 And &H40) 
			bCHRInversion = (data1 And &H80) 
		 
		else
		 
			'// Update target register
			pRegister(nTargetRegister) = data1 
			
'///////////////////////////////////////////////////////////////////
			'// Update Pointer Table
			if (bCHRInversion) Then
			 
		          pCHRBank(0) = pRegister(2) * &H0400
                pCHRBank(1) = pRegister(3) * &H0400
                pCHRBank(2) = pRegister(4) * &H0400
                pCHRBank(3) = pRegister(5) * &H0400
                pCHRBank(4) = (pRegister(0) AND &HFE) * &H0400
                pCHRBank(5) = pRegister(0) * &H0400 + &H0400
                pCHRBank(6) = (pRegister(1) AND &HFE) * &H0400
                pCHRBank(7) = pRegister(1) * &H0400 + &H0400
		 
			else
		 
			       pCHRBank(0) = (pRegister(0) AND &HFE) * &H0400
                pCHRBank(1) = pRegister(0) * &H0400 + &H0400
                pCHRBank(2) = (pRegister(1) AND &HFE) * &H0400
                pCHRBank(3) = pRegister(1) * &H0400 + &H0400
                pCHRBank(4) = pRegister(2) * &H0400
                pCHRBank(5) = pRegister(3) * &H0400
                pCHRBank(6) = pRegister(4) * &H0400
                pCHRBank(7) = pRegister(5) * &H0400
			End If
      '////////////////////////////////////////////////////////
      
			if (bPRGBankMode) Then
			 
				pPRGBank(2) = (pRegister(6) and &H3F) * &H2000 
				pPRGBank(0) = (nPRGBanks * 2 - 2) * &H2000 
			 
			else
			 
			 	pPRGBank(0) = (pRegister(6) and &H3F) * &H2000 
				pPRGBank(2) = (nPRGBanks * 2 - 2) * &H2000 
			End If
			'
			pPRGBank(1) = (pRegister(7) and &H3F) * &H2000 
			pPRGBank(3) = (nPRGBanks * 2 - 1) * &H2000 

				
      End If
			
			

		return false 
	End If

	If (addr1 >= &HA000 and addr1 <= &HBFFF) Then
	 
	  tmp1 = IIf(addr1 And &H0001,0,1)
		If tmp1 Then
		 
		'// Mirroring
			if (data1 and &H01) Then
				mirrormode = HORIZONTAL
			else
				mirrormode = VERTICAL
			End If
		else
		 
			'// PRG Ram Protect
			'// TODO:
		End If
		return false 
	End If

	If (addr1 >= &HC000 And addr1 <= &HDFFF) then
	 	  tmp1 = IIf(addr1 And &H0001,0,1)
		If tmp1  Then
		 
			nIRQReload = data1 
		 
		else
		 
			nIRQCounter = &H0000 
	 End If
		return false 
		
	End If


	If (addr1 >= &HE000 and addr1 <= &HFFFF) Then
	  tmp1 = IIf(addr1 And &H0001,0,1)
		if  tmp1   Then
 
			bIRQEnable = false 
			bIRQActive = false 
 
		else
	 
			bIRQEnable = true 
		End If
		return false 
	End If



	return false 
End Function 






Function ppuMapWrite(addr1 As uint16_t ,ByRef mapped_addr As uint32_t  ) As bool
 

	return false
End Function
Sub resetmapper overload ()
 	
 	nTargetRegister = &H00
	bPRGBankMode = false
	bCHRInversion = false
	mirrormode = HORIZONTAL

	bIRQActive = false 
	bIRQEnable = false 
	bIRQUpdate = false 
	nIRQCounter = &H0
	nIRQReload = &H0

   Erase pPRGBank
   Erase pCHRBank
   Erase pRegister 

	pPRGBank(0) = 0 * &H2000 
	pPRGBank(1) = 1 * &H2000 
	pPRGBank(2) = (nPRGBanks * 2 - 2) * &H2000 
	pPRGBank(3) = (nPRGBanks * 2 - 1) * &H2000 
	
 	
 End Sub


'Declare function reset_mapper OverLoad() As MIR
'Function _mirror overload() As MIRROR 
'	Return mirrormode
'End Function 

Sub get_scanline()
		if (nIRQCounter =  0) Then
   	nIRQCounter = nIRQReload 
	else
		nIRQCounter-=1
		'nIRQCounter And= &Hff
	End If
	if (nIRQCounter =  0 and bIRQEnable) Then
	 
		bIRQActive = true
	End If
End Sub

Function cpuMapRead OverLoad (addr1 As uint16_t , ByRef mapped_addr As uint32_t, ByRef data1 As uint8_t ) As bool

if (addr1 >= &H6000 and addr1 <= &H7FFF) then
	 
	'	// Write is to static ram on cartridge
		mapped_addr = &HFFFFFFFF 

	'	// Write data to RAM
		data1 = vRAMStatic[addr1 and &H1FFF] 

	'	// Signal mapper has handled request
		return true 
End If

	if (addr1 >= &H8000 and addr1 <= &H9FFF) Then
	 
		mapped_addr = pPRGBank(0) + (addr1 and &H1FFF) 
		return true 
	End If

	If (addr1 >= &HA000 and addr1 <= &HBFFF) Then
 
		mapped_addr = pPRGBank(1) + (addr1 And &H1FFF) 
		return true 
	End If

	If (addr1 >= &HC000 and addr1 <= &HDFFF) then
 
		mapped_addr = pPRGBank(2) + (addr1 and &H1FFF) 
		return true 
	End If

	If (addr1 >= &HE000 and addr1 <= &HFFFF) Then
 
		mapped_addr = pPRGBank(3) + (addr1 and &H1FFF) 
		return true 
	End If

	return false 
 
End Function

Function ppuMapRead( addr1 As uint16_t,ByRef mapped_addr As uint32_t ) As bool
  
 
lastread = 	addr1

    
If addr1 < &H2000 Then
	
	'If (lastread And &H1000) = 0 And (addr1 And &H1000) > 0 Then
    'get_scanline()
'End if
	

End If



    ' If((lastread And &H0800) = 0) And ((addr1 And &H0800) > 0) Then
    '	
    '	get_scanline()
    'EndIf 
      '  If(((addr1 And &H0800) > 0)) Then
    	'
    	'get_scanline()
      '  EndIf 
   

 'javidx9's//////////////////////////////////////////////////////////////////
	if (addr1 >= &H0000 And addr1 <= &H03FF) Then
	
		mapped_addr = pCHRBank(0)    + (addr1 And &H03FF) 
		return true 
	End If

	If (addr1 >= &H0400 and addr1 <= &H07FF) Then
 
		mapped_addr = pCHRBank(1)+ (addr1 and &H03FF) 
		return true 
	End If

	If (addr1 > &H0800 and addr1 <= &H0BFF) Then
	 
		mapped_addr = pCHRBank(2) + (addr1 And &H03FF) 
		return true 
	End If

	If (addr1 >= &H0C00 And addr1 <= &H0FFF) Then
 
		mapped_addr = pCHRBank(3) + (addr1 And &H03FF) 
		return true 
	End If

	If (addr1 >= &H1000  And addr1 <= &H13FF) Then
	
		mapped_addr = pCHRBank(4) + (addr1 And &H03FF) 
		return true
	End if

	If (addr1 >= &H1400 And addr1 <= &H17FF) Then
 
			mapped_addr = pCHRBank(5) + (addr1 And &H03FF)
		return true 
	End If

	If (addr1 >= &H1800 And addr1 <= &H1BFF) then
	 
	mapped_addr = pCHRBank(6) + (addr1 And &H03FF)
		return true 
	End If

	If (addr1 >= &H1C00 and addr1 <= &H1FFF) Then
	 
		mapped_addr = pCHRBank(7) + (addr1 And &H03FF) 
		return true
	End If
'/////////////////////////////////////////////////////////////////////////
















	return false
End Function