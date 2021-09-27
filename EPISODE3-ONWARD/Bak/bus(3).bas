
#Include Once "nes/bus.bi"


'
'#Include Once "nes/olc6502.bi"
'#Include Once "nes/olc2C02.bi"

'Function Bus_read( addr1 As uint16_t, bReadOnly As bool = FALSE) As uint8_t 
' 
'	if (addr1 >= &H0000 And addr1 <= &HFFFF) Then 
'		return ram(addr1)
'	End If
'	return &H00 
' 
'End function
'
'
'Sub Bus_write( addr1 as uint16_t,data1 as uint8_t  )
' 
'	if (addr1 >= &H0000 And addr1 <= &HFFFF) Then 
'		 ram(addr1) = data1
'
'	End If
'	
'End Sub


Function Bus_read( addr1 As uint16_t, bReadOnly As bool = FALSE) As uint8_t 
 	Dim As uint8_t data1 = &H00 
 	
	If (cart_cpuRead(addr1, data1)) Then
	 
	'	// Cartridge Address Range

	ElseIf (addr1 >= &H0000 and addr1 <= &H1FFF) Then
	 
		'// System RAM Address Range, mirrored every 2048
		data1 = cpuRam(addr1 and &H07FF) 
	 
	ElseIf (addr1 >= &H2000 And addr1 <= &H3FFF) then
 
		'// PPU Address range, mirrored every 8
		data1 = ppu_cpuRead(addr1 And &H0007, bReadOnly) 
 
	ElseIf (addr1 >= &H4016 and addr1 <= &H4017) then
	
		data1 = (controller_state(addr1 and &H0001) and &H80) > 0 
		controller_state(addr1 and &H0001) Shl= 1 
	End If

	return data1

End Function


Sub Bus_write( addr1 as uint16_t,data1 as uint8_t  )
 
	if (cart_cpuWrite(addr1, data1)) Then
	 
		'// The cartridge "sees all" and has the facility to veto
		'// the propagation of the bus transaction if it requires.
		'// This allows the cartridge to map any address to some
		'// other data, including the facility to divert transactions
		'// with other physical devices. The NES does not do this
		'// but I figured it might be quite a flexible way of adding
		'// "custom" hardware to the NES in the future!
	
	elseif (addr1 >= &H0000 And addr1 <= &H1FFF) Then
	
		'// System RAM Address Range. The range covers 8KB, though
		'// there is only 2KB available. That 2KB is "mirrored"
		'// through this address range. Using bitwise AND to mask
		'// the bottom 11 bits is the same as addr % 2048.
		cpuRam(addr1 And &H07FF) =  data1 

	
	ElseIf (addr1 >= &H2000 And addr1 <= &H3FFF) Then

		'// PPU Address range. The PPU only has 8 primary registers
		'// and these are repeated throughout this range. We can
		'// use bitwise AND operation to mask the bottom 3 bits,
		'// which is the equivalent of addr % 8.
	
		ppu_cpuWrite(addr1 And &H0007, data1) 

	elseif (addr1 >= &H4016 And addr1 <= &H4017) Then

		controller_state(addr1 and &H0001) = controller(addr1 And &H0001)
	End If

	
End Sub


Sub bus_clock
	
	'
	'	// Clocking. The heart and soul of an emulator. The running
	'// frequency is controlled by whatever calls this function.
	'// So here we "divide" the clock as necessary and call
	'// the peripheral devices clock() function at the correct
	'// times.

	'// The fastest clock frequency the digital system cares
	'// about is equivalent to the PPU clock. So the PPU is clocked
	'// each time this function is called.
	ppu_clock() 

	'// The CPU runs 3 times slower than the PPU so we only call its
	'// clock() function every 3 times this function is called. We
	'// have a global counter to keep track of this.
	if (nSystemClockCounter mod 3 =  0) Then
 
		clock_cpu() 
	End if 

	'// The PPU is capable of emitting an interrupt to indicate the
	'// vertical blanking period has been entered. If it has, we need
	'// to send that irq to the CPU.
	If (irqState()) Then

		irqClear()
		irq()		
	End If
	'
	if (ppu_nmi) Then
	 
		ppu_nmi = false 
		cpu_nmi() 
	End If

	nSystemClockCounter+=1
	
	
	
	
	
End Sub
Sub bus_reset()
	
	cart_reset()
   cpu_reset()
	ppu_reset() 
	nSystemClockCounter = 0
	
	
	
	
	
End Sub	
	
	
	
	

	
	
	
