#Include Once "windows.bi"
#Include once "crt.bi"
#Include Once "crt/stdint.bi"
#Include Once "win/windef.bi"



#Include "containers/map.bi"
#Include "containers/stack.bi"
#Include "containers/list.bi"

'fake ram
'Dim Shared  ram(64 * 1024) As uint8_t 



Dim Shared  cpuram(2048) As uint8_t 
Dim Shared controller(2) As uint8_t


Dim  Shared controller_state(2) As uint8_t


Declare sub bus_write(addr1 As uint16_t, data1 As uint8_t)
Declare Function bus_read(addr1 As uint16_t, bReadOnly As bool = false) As uint8_t 
	
'#Include Once "nes/olc6502.bi"
'#Include Once "olc6502.bas"
'
'#Include Once "nes/olc2C02.bi"
'#Include Once "olc2c02.bas"
'
'#Include Once "nes/cartridge.bi"
'#Include Once "cartridge.bas"




Dim Shared As uint32_t nSystemClockCounter = 0



'TODO connect cpu to bus?

'



'TODO devices On bus?



	

'Function Bus_read( addr As uint16_t, bReadOnly As bool = FALSE) As uint8_t 
' 
'	if (addr >= &H0000 And addr <= &HFFFF) Then 
'		return ram(addr)
'	End If
'	return &H00 
' 
'End function
'
'
'Sub Bus_write( addr as uint16_t,data1 as uint8_t  )
' 
'	if (addr >= &H0000 And addr <= &HFFFF) Then 
'		 ram(addr) = data1
'
'	End If
'	
'End Sub
'
'
'Sub bus_clock
'	
'	
'	
'	
'	
'	
'	
'	
'End Sub
'




