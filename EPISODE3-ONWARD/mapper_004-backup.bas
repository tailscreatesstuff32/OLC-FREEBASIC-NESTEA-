#Include "mapper_004.bi"

'Declare function reset_mapper OverLoad() As MIRROR
Sub mapper_004(prgBanks As uint8_t ,chrBanks As uint8_t)
 	
	vRAMStatic.resize(32*1024)

 	nPRGBanks = prgBanks
	nCHRBanks = chrBanks
 	
End Sub


Function cpuMapWrite( addr1 As uint16_t, ByRef mapped_addr As uint32_t ,data1 As  uint8_t ) As bool
 
	if (addr1 >= &H6000 and addr1 <= &H7FFF) Then
	 
		'// Write is to static ram on cartridge
		mapped_addr = &HFFFFFFFF 

	'	// Write data to RAM
		vRAMStatic[addr1 and &H1FFF] = data1 

		'// Signal mapper has handled request
		return true 
	End If
'///////////////////////////////////////////


	if (addr1 >= &H8000 and addr1 <= &H9FFF) Then
	 
		'// Bank Select
		if ((addr1 And  &H0001))=0 Then
		 
			nTargetRegister = data1 And &H07 
			bPRGBankMode = (data1 And &H40) 
			bCHRInversion = (data1 And &H80) 
		 
		else
		 
			'// Update target register
			pRegister(nTargetRegister) = data1 

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

			if (bPRGBankMode) Then
			 
				pPRGBank(2) = (pRegister(6) and &H3F) * &H2000 
				pPRGBank(0) = (nPRGBanks * 2 - 2) * &H2000 
			 
			else
			 
				pPRGBank(0) = (pRegister(6) and &H3F) * &H2000 
				pPRGBank(2) = (nPRGBanks * 2 - 2) * &H2000 
			End If
			pPRGBank(1) = (pRegister(7) and &H3F) * &H2000 
			pPRGBank(3) = (nPRGBanks * 2 - 1) * &H2000 

				
       End If
			
			

		return false 
	End If

	if (addr1 >= &HA000 and addr1 <= &HBFFF) Then
	 
		If ((addr1  And &H0001))=0 Then
		 
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

	if (addr1 >= &HC000 And addr1 <= &HDFFF) then
	 
		If ((addr1 And &H0001))=0 Then
		 
			nIRQReload = data1 
		 
		else
		 
			nIRQCounter = &H0000 
	 End If
		return false 
		
	End If


	if (addr1 >= &HE000 and addr1 <= &HFFFF) Then
	 
		if ((addr1 and &H0001))=0 Then
 
			bIRQEnable = false 
			bIRQActive = false 
 
		else
	 
			bIRQEnable = true 
		End If
		return false 
	End If



	return false 
End Function 


'FUNCTION cpuMap04Write (addr1 AS _UNSIGNED INTEGER, mapped_addr AS _UNSIGNED LONG, byte_data AS _UNSIGNED _BYTE)
'     '   dim temp_addr as   _UNSIGNED INTEGER
'  '  mapped_addr = 0
'
'    IF addr1 >= &H6000 AND addr <= &H7FFF~% THEN
'
'        mapped_addr = &HFFFFFFFF
'
'        vRAMStatic(addr1 AND &H1FFF ) = byte_data
'
'
'        cpuMap04Write = 1
'     EXIT FUNCTION
'    END IF
'
'
'    IF (addr >= &H8000~% AND addr <= &H9FFF~%) THEN
'
'
'   '    temp_addr = addr and &H0001
'
'        ' Bank Select
'      IF (addr AND &H0001) = 0 THEN
'
'            nTargetRegister = (byte_data AND &H07)
'            bPRGBankMode = (byte_data AND &H40)
'            bCHRInversion = (byte_data AND &H80)
'
'              ' _title hex$(addr)
'        ELSE
'           pRegister(nTargetRegister) = byte_data
'            '// Update Pointer Table
'            IF (bCHRInversion) THEN
'
'                pCHRBank(0) = pRegister(2) * &H0400~%
'                pCHRBank(1) = pRegister(3) * &H0400~%
'                pCHRBank(2) = pRegister(4) * &H0400~%
'                pCHRBank(3) = pRegister(5) * &H0400~%
'                pCHRBank(4) = (pRegister(0) AND &HFE~%) * &H0400~%
'                pCHRBank(5) = pRegister(0) * &H0400~% + &H0400~%
'                pCHRBank(6) = (pRegister(1) AND &HFE~%) * &H0400~%
'                pCHRBank(7) = pRegister(1) * &H0400~% + &H0400~%
'
'            ELSE
'
'                pCHRBank(0) = (pRegister(0) AND &HFE~%) * &H0400~%
'                pCHRBank(1) = pRegister(0) * &H0400~% + &H0400~%
'                pCHRBank(2) = (pRegister(1) AND &HFE~%) * &H0400~%
'                pCHRBank(3) = pRegister(1) * &H0400~% + &H0400~%
'                pCHRBank(4) = pRegister(2) * &H0400~%
'                pCHRBank(5) = pRegister(3) * &H0400~%
'                pCHRBank(6) = pRegister(4) * &H0400~%
'                pCHRBank(7) = pRegister(5) * &H0400~%
'            END IF
'
'            IF (bPRGBankMode) THEN
'
'                    pPRGBank(2) = (pRegister(6) AND &H3F~%) * &H2000~%
'                    pPRGBank(0) = (n_PRGbanks * 2 - 2) * &H2000~%
'
'            ELSE
'
'                   pPRGBank(0) = (pRegister(6) AND &H3F~%) * &H2000~%
'                   pPRGBank(2) = (n_PRGbanks * 2 - 2) * &H2000~%
'            END IF
'
'              pPRGBank(1) = (pRegister(7) AND &H3F~%) * &H2000~%
'              pPRGBank(3) = (n_PRGbanks * 2 - 1) * &H2000~%
'
'
'        END IF
'
'            cpuMap04Write = 0
'           EXIT FUNCTION
'
'    END IF
'    IF (addr >= &HA000~% AND addr <= &HBFFF~%) THEN
'
'        IF ((addr AND &H0001)) = 0 THEN
'
'            '// Mirroring
'            IF (byte_data AND &H01) THEN
'                mirrormode = 0
'            ELSE
'                mirrormode = 1
'
'            END IF
'        else
'
'            ' PRG Ram Protect
'            ' TODO:
'
'        END IF
'            cpuMap04Write = 0
'          EXIT FUNCTION
'
'
'    END IF
'
'    IF (addr >= &HC000~% AND addr <= &HDFFF~%) THEN
'
'        IF ((addr AND &H0001))= 0 THEN
'
'            nIRQReload = byte_data
'
'        ELSE
'
'            nIRQCounter = &H0000
'
'        END IF
'            cpuMap04Write = 0
'         EXIT FUNCTION
'
'    END IF
'
'    IF (addr >= &HE000~% AND addr <= &HFFFF~%) THEN
'
'        IF ((addr AND &H0001))= 0 THEN
'
'            bIRQEnable = 0
'            bIRQActive = 0
'
'        ELSE
'
'            bIRQEnable = 1
'
'        END IF
'        cpuMap04Write = 0
'       EXIT FUNCTION
'
'
'    END IF
'
'END FUNCTION



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
Function irqstate() As bool
	
	return bIRQActive
	
End function
Sub irqclear() 
	
	bIRQActive = FALSE
	
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

	if (addr1 >= &HA000 and addr1 <= &HBFFF) Then
 
		mapped_addr = pPRGBank(1) + (addr1 And &H1FFF) 
		return true 
	End If

	if (addr1 >= &HC000 and addr1 <= &HDFFF) then
 
		mapped_addr = pPRGBank(2) + (addr1 and &H1FFF) 
		return true 
	End If

	if (addr1 >= &HE000 and addr1 <= &HFFFF) Then
 
		mapped_addr = pPRGBank(3) + (addr1 and &H1FFF) 
		return true 
	End If

	return false 
 
End Function

Function ppuMapRead( addr1 As uint16_t,ByRef mapped_addr As uint32_t ) As bool
 
	if (addr1 >= &H0000 And addr1 <= &H03FF) Then
	
		mapped_addr = pCHRBank(0)  + (addr1 And &H03FF) 
		return true 
	End If

	if (addr1 >= &H0400 and addr1 <= &H07FF) Then
 
		mapped_addr = pCHRBank(1)+ (addr1 and &H03FF) 
		return true 
	End If

	if (addr1 > &H0800 and addr1 <= &H0BFF) Then
	 
		mapped_addr = pCHRBank(2) + (addr1 And &H03FF) 
		return true 
	End If

	if (addr1 >= &H0C00 And addr1 <= &H0FFF) Then
 
		mapped_addr = pCHRBank(3) + (addr1 And &H03FF) 
		return true 
	End If

	if (addr1 >= &H1000  And addr1 <= &H13FF) Then
	
		mapped_addr = pCHRBank(4) + (addr1 And &H03FF) 
		return true
	End if



	if (addr1 >= &H1400 And addr1 <= &H17FF) Then
 
			mapped_addr = pCHRBank(5) + (addr1 And &H03FF)
		return true 
	End If

	if (addr1 >= &H1800 And addr1 <= &H1BFF) then
	 
	mapped_addr = pCHRBank(6) + (addr1 And &H03FF)
		return true 
	End If

	if (addr1 >= &H1C00 and addr1 <= &H1FFF) Then
	 
		mapped_addr = pCHRBank(7) + (addr1 And &H03FF) 
		return true
	End If

	return false
End Function