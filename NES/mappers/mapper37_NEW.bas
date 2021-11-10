#Include once "windows.bi"
#Include Once"crt.bi"
#Include Once "mapper_NEW.bas"
#include once "containers/vector.bi"

MVectorTemplate(uint8_t)
  Type Mapper_37 extends Mapper
  	
  	Public:
   Declare Constructor(prgbanks As uint8_t,chrbanks As uint8_t)
   declare Destructor()
  	
  	Public:
  	Declare function cpuMapRead(addr As uint16_t,ByRef mapped_addr As uint32_t , byref byte_data As uint8_t) As BOOLEAN
  	Declare function cpuMapWrite(addr As uint16_t,ByRef mapped_addr As uint32_t ,byte_data As uint8_t = 0) As BOOLEAN
  	Declare function ppuMapRead(addr As uint16_t,ByRef mapped_addr As uint32_t) As BOOLEAN
  	Declare function ppuMapWrite(addr As uint16_t,ByRef mapped_addr As uint32_t) As BOOLEAN
  	Declare Sub resetmappper()
  	
  	declare sub get_scanline() 
  	
  	declare function GetIrqReloadVal() as uint16_t
  	
  	declare function irqstate() as bool
  	declare sub irqclear()
  	declare function _mirror() as MIRROR


  	private:
  	m37_outer_bank as uint8_t
  	  	
  	declare function m37_chr(p as uint32) as uint32_t
  	declare function m37_prg(p as uint32) as uint32_t
  		
    nTargetRegister As uint8_t
	 bPRGBankMode As bool = false 
	 bCHRInversion As bool  = false 

    mirrormode as MIRROR= MIRROR.HORIZONTAL
    
    reloadirq as boolean

	 nIRQCounter As uint16_t 
	 nIRQReload  As uint16_t
  	
	 lastread as uint16_t

	 pRegister(8) As uint32_t
	 pCHRBank(8)  As uint32_t
	 pPRGBank(4)  As uint32_t



    vRAMStatic As TVECTORUINT8_T
    bIRQActive As bool = false 
    bIRQEnable As bool =  false 
    bIRQUpdate As bool =  false 
  End Type

Constructor Mapper_37(prgbanks As uint8_t,chrbanks As uint8_t) 
							base(prgbanks ,chrbanks)
							
							
							
							
  this.vRAMStatic.resize(32*1024)
  



End Constructor
Destructor Mapper_37()


End Destructor


  Sub Mapper_37.resetmappper()   

 	this.nTargetRegister = &H00
	this.bPRGBankMode = false
	this.bCHRInversion = false
	this.mirrormode = MIRROR.HORIZONTAL

	this.bIRQActive = false 
	this.bIRQEnable = false 
	this.bIRQUpdate = false 
	this.nIRQCounter = &H0
	this.nIRQReload = &H0

   Erase pPRGBank
   Erase pCHRBank
   Erase pRegister 

	this.pPRGBank(0) = 0 * &H2000 
	this.pPRGBank(1) = 1 * &H2000 
	this.pPRGBank(2) = (this.nPRGBanks * 2 - 2) * &H2000 
	this.pPRGBank(3) = (this.nPRGBanks * 2 - 1) * &H2000 


  End Sub
  
  Function Mapper_37.cpuMapWrite( addr1 As uint16_t, ByRef mapped_addr As uint32_t ,data1 As  uint8_t ) As boolean
 dim tmp1 as bool
 

			
	if (addr1 >= &H6000 and addr1 <= &H7FFF) Then '8 KB PRG RAM bank (optional)
	 
		'// Write is to static ram on cartridge
		mapped_addr = &HFFFFFFFF 

	'	// Write data to RAM
		this.vRAMStatic[addr1 and &H1FFF] = data1 
		
     'm37_outer_bank = (addr1 and &b111)
     
		'// Signal mapper has handled request
		return true 
	End If
'///////////////////////////////////////////


	If (addr1 >= &H8000 and addr1 <= &H9FFF) Then
	  tmp1 = IIf(addr1 And &H0001,0,1)
		'// Bank Select
		if  tmp1 Then
		 
			this.nTargetRegister = data1 And &H07 
			this.bPRGBankMode = (data1 And &H40) 
			this.bCHRInversion = (data1 And &H80) 
		 
		else
		 
			'// Update target register
			this.pRegister(nTargetRegister) = data1 
			
'///////////////////////////////////////////////////////////////////
			'// Update Pointer Table
			if (this.bCHRInversion) Then
			 
		          this.pCHRBank(0) = this.pRegister(2) * &H0400
                this.pCHRBank(1) = this.pRegister(3) * &H0400
                this.pCHRBank(2) = this.pRegister(4) * &H0400
                this.pCHRBank(3) = this.pRegister(5) * &H0400
                this.pCHRBank(4) = (this.pRegister(0) AND &HFE) * &H0400
                this.pCHRBank(5) = this.pRegister(0) * &H0400 + &H0400
                this.pCHRBank(6) = (this.pRegister(1) AND &HFE) * &H0400
                this.pCHRBank(7) = this.pRegister(1) * &H0400 + &H0400
		 
			else
		 
			       this.pCHRBank(0) = (this.pRegister(0) AND &HFE) * &H0400
                this.pCHRBank(1) = this.pRegister(0) * &H0400 + &H0400
                this.pCHRBank(2) = (this.pRegister(1) AND &HFE) * &H0400
                this.pCHRBank(3) = this.pRegister(1) * &H0400 + &H0400
                this.pCHRBank(4) = this.pRegister(2) * &H0400
                this.pCHRBank(5) = this.pRegister(3) * &H0400
                this.pCHRBank(6) = this.pRegister(4) * &H0400
                this.pCHRBank(7) = this.pRegister(5) * &H0400
			End If
      '////////////////////////////////////////////////////////
      
			if (bPRGBankMode) Then
			 
				this.pPRGBank(2) = (this.pRegister(6) and &H3F) '* &H2000 
				this.pPRGBank(0) = (this.nPRGBanks * 2 - 2) '* &H2000 
			 
			else
			 
			 	this.pPRGBank(0) = (this.pRegister(6) and &H3F) '* &H2000 
				this.pPRGBank(2) = (this.nPRGBanks * 2 - 2) '* &H2000 
			End If
			'
			this.pPRGBank(1) = (this.pRegister(7) and &H3F) '* &H2000 
			this.pPRGBank(3) = (this.nPRGBanks * 2 - 1) '* &H2000 

				
      End If
			
			
			
			
			
			this.pPRGBank(0) *= &H2000
			this.pPRGBank(1) *= &H2000
			this.pPRGBank(2) *= &H2000
			this.pPRGBank(3) *= &H2000

		return false 
	End If

	If (addr1 >= &HA000 and addr1 <= &HBFFF) Then
	 
	  tmp1 = IIf(addr1 And &H0001,0,1)
		If tmp1 Then
		 
		'// Mirroring
			if (data1 and &H01) Then
				this.mirrormode = HORIZONTAL
			else
				this.mirrormode = VERTICAL
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
		 
			this.nIRQReload = data1
		 
		else
		' this.reloadIrq = true
			this.nIRQCounter = &H0000 
			
	 End If
		return false 
		
	End If


	If (addr1 >= &HE000 and addr1 <= &HFFFF) Then
	  tmp1 = IIf(addr1 And &H0001,0,1)
		if  tmp1   Then
 
			this.bIRQEnable = false 
			this.bIRQActive = false 
 
		else
	 
			this.bIRQEnable = true 
		End If
		return false 
	End If











End function 

Function Mapper_37.cpuMapRead(addr1 As uint16_t , ByRef mapped_addr As uint32_t, ByRef data1 As uint8_t ) As boolean 
if (addr1 >= &H6000 and addr1 <= &H7FFF) then
	 
	'	// Write is to static ram on cartridge
		mapped_addr = &HFFFFFFFF 

	'	// Write data to RAM
		data1 = this.vRAMStatic[addr1 and &H1FFF] 

	'	// Signal mapper has handled request
		return true 
End If

	if (addr1 >= &H8000 and addr1 <= &H9FFF) Then
	 
		mapped_addr = this.pPRGBank(0) + (addr1 and &H1FFF) 
		return true 
	End If

	If (addr1 >= &HA000 and addr1 <= &HBFFF) Then
 
		mapped_addr = this.pPRGBank(1) + (addr1 And &H1FFF) 
		return true 
	End If

	If (addr1 >= &HC000 and addr1 <= &HDFFF) then
 
		mapped_addr = this.pPRGBank(2) + (addr1 and &H1FFF) 
		return true 
	End If

	If (addr1 >= &HE000 and addr1 <= &HFFFF) Then
 
		mapped_addr = this.pPRGBank(3) + (addr1 and &H1FFF) 
		return true 
	End If

	return false 

End function 
  
Function Mapper_37.ppuMapRead(addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As boolean 
	   
	   'if addr1 <&H2000 then
	   ' if((this.lastRead and &H1000)  = 0 and (addr1 and &H1000) > 0) then 
      ' ' // A12 went high, clock irq
      '  this.get_scanline() 

      '
      'this.lastread = addr1
	   'end if 
	   'end if 
 	if (addr1 >= &H0000 And addr1 <= &H03FF) Then
	
		mapped_addr = this.pCHRBank(0)    + (addr1 And &H03FF) 
		return true 
	End If

	If (addr1 >= &H0400 and addr1 <= &H07FF) Then
 
		mapped_addr = this.pCHRBank(1)+ (addr1 and &H03FF) 
		return true 
	End If

	If (addr1 > &H0800 and addr1 <= &H0BFF) Then
	 
		mapped_addr = this.pCHRBank(2) + (addr1 And &H03FF) 
		return true 
	End If

	If (addr1 >= &H0C00 And addr1 <= &H0FFF) Then
 
		mapped_addr = this.pCHRBank(3) + (addr1 And &H03FF) 
		return true 
	End If

	If (addr1 >= &H1000  And addr1 <= &H13FF) Then
	
		mapped_addr =this.pCHRBank(4) + (addr1 And &H03FF) 
		return true
	End if

	If (addr1 >= &H1400 And addr1 <= &H17FF) Then
 
			mapped_addr = this.pCHRBank(5) + (addr1 And &H03FF)
		return true 
	End If

	If (addr1 >= &H1800 And addr1 <= &H1BFF) then
	 
	mapped_addr = this.pCHRBank(6) + (addr1 And &H03FF)
		return true 
	End If

	If (addr1 >= &H1C00 and addr1 <= &H1FFF) Then
	 
		mapped_addr = this.pCHRBank(7) + (addr1 And &H03FF) 
		return true
	End If

 
End Function


Function Mapper_37.ppuMapWrite(addr1 As uint16_t , ByRef mapped_addr As uint32_t ) As boolean 
 
return false

End Function



sub Mapper_37.get_scanline()
	if (this.nIRQCounter =  0  ) Then
   	this.nIRQCounter = this.nIRQReload 
   	'this.reloadIrq = false
	else
		this.nIRQCounter-=1
		
	'	this.nIRQCounter And= &Hff
	End If
	if (this.nIRQCounter =  0 and this.bIRQEnable) Then
	 
		this.bIRQActive = true
	End If
	
End sub

Function Mapper_37.irqstate() as bool
	
	return this.bIRQActive
End Function

sub Mapper_37.irqclear()
	this.bIRQActive = FALSE
	
End sub

function  Mapper_37._mirror() as MIRROR
	Return this.mirrormode
End Function

 function Mapper_37.GetIrqReloadVal()  as uint16_t
	
	return this.nIRQReload 
 End Function

function Mapper_37.m37_chr(p as uint32_t) as uint32_t
		'	if (m37_outer_bank >= 4) then
		'	p or= &H80 
		'return p 
	
End Function


function Mapper_37.m37_prg(p as uint32_t)as uint32_t
		'if (m37_outer_bank <= 2) then
		'	p and= &H07 
		'else if (m37_outer_bank = 3)  then
		'	p and= &H07 
		'	p or= &H08 
		'else if (m37_outer_bank = 7) then
		'	p and= &H07 
		'	p or= &H20 
		'else if (m37_outer_bank >= 4) then
		'	p and= &H0f 
		'	p or= &H10 
		'end if
		'return p 
End Function
