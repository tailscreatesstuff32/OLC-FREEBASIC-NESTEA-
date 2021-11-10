#Include "windows.bi"
#Include Once"crt.bi"

#include once "containers/vector.bi"

MVectorTemplate(uint8_t)

Enum MIRROR
HARDWARE
HORIZONTAL
VERTICAL
ONESCREEN_LO
ONESCREEN_HI
End Enum


  Type Mapper extends object
  	
  	Public:
  	Declare Constructor(prgbanks As uint8_t = 0,chrbanks As uint8_t = 0)
  	declare Destructor()
  	
  	Public:
  	Declare abstract function cpuMapRead(addr As uint16_t,ByRef mapped_addr As uint32_t , byref byte_data As uint8_t) As BOOLEAN
  	Declare abstract function cpuMapWrite(addr As uint16_t,ByRef mapped_addr As uint32_t ,byte_data As uint8_t = 0) As BOOLEAN
  	Declare abstract function ppuMapRead(addr As uint16_t,ByRef mapped_addr As uint32_t) As BOOLEAN
  	Declare abstract function ppuMapWrite(addr As uint16_t,ByRef mapped_addr As uint32_t) As BOOLEAN
   
   'nIRQCounter As uint16_t 
	' nIRQReload  As uint16_t
	
  	declare virtual function GetBattery() as boolean' TVECTORUINT8_T ptr  'TVECTORUINT8_T
  	declare virtual function SetBattery() as boolean 'TVECTORUINT8_T
  	
   declare virtual function GetIrqReloadVal()  as uint16_t 
  	
  	Declare virtual function irqstate() as bool
  	Declare virtual Sub irqclear()
  	
  	
  	Declare virtual Sub get_scanline()
  	Declare virtual Sub resetmappper(hard as boolean)
  	
  	vRAMStatic As TVECTORUINT8_T
  

  	Declare virtual Function _mirror() As MIRROR
  	
  	Protected:
  	nPRGBanks As uint8_t = 0
   nCHRBanks As uint8_t = 0
   
   SRAM(32*1024) as uint8_t
   
 
  	
  	
  End Type


Destructor Mapper()

End Destructor





  Constructor Mapper(prgbanks As uint8_t,chrbanks As uint8_t)
  
  
   	this.nPRGBanks = prgbanks 
   	this.nCHRBanks = chrbanks
		this.resetmappper(true)
  
  End Constructor
  
 


  function Mapper.irqstate() as bool
  	
  	
  	
  	
  End function 
    Sub Mapper.irqclear()
  	
  	
  	
  	
    End Sub
    Sub Mapper.get_scanline()
  	
  	
  	
  	
    End Sub
  Sub Mapper.resetmappper(hard as boolean)   
  	
  	
  	
  	
  End Sub
  Function Mapper._mirror() As MIRROR
  	
  	Return MIRROR.HARDWARE	
  End Function


 function Mapper.GetIrqReloadVal()  as uint16_t
	
	
 End Function

 function Mapper.GetBattery()  as boolean'TVECTORUINT8_T ptr'TVECTORUINT8_T
	
	
 End Function
 function Mapper.SetBattery()  as boolean'TVECTORUINT8_T
	
	
	
	
 End Function