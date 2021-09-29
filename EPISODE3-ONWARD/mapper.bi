




Declare Function cpuMapRead overload(addr1 As uint16_t, ByRef mapped_addr As uint32_t, ByRef data1 As uint8_t )As bool
Declare Function cpuMapWrite OverLoad(addr1 As uint16_t ,ByRef mapped_addr As  uint32_t, data1 As uint8_t) As bool
Declare Function ppuMapRead OverLoad(addr1 As uint16_t, ByRef mapped_addr As uint32_t) As bool 
Declare Function ppuMapWrite OverLoad(addr1 As uint16_t, ByRef mapped_addr As uint32_t) As bool 
Declare sub resetmapper OverLoad()

Declare Sub mapper OverLoad(prgBanks As uint8_t ,chrBanks As uint8_t)

Declare function irqState() As bool
Declare sub  irqClear() 


dim shared bIRQActive As bool = false 
dim Shared bIRQEnable As bool =  false 
dim Shared bIRQUpdate As bool =  false 

'Declare sub scanline()

	Enum MIRROR
	   HARDWARE
		HORIZONTAL 
		VERTICAL 
		ONESCREEN_LO 
		ONESCREEN_HI 
	End Enum

Dim Shared mirrormode As MIRROR = HARDWARE
Dim Shared As  uint8_t nPRGBanks = 0 
Dim  Shared As uint8_t nCHRBanks = 0 

Sub mapper OverLoad(prgBanks As uint8_t ,chrBanks As uint8_t)
	

	
	nPRGBanks = prgBanks
	nCHRBanks = chrBanks
	
End Sub

'
Function _mirror overload() As MIRROR 
	Return mirrormode
End Function

Function irqstate() As bool
	
	return bIRQActive
	
End function
Sub irqclear() 
	
	bIRQActive = FALSE
	
End Sub
