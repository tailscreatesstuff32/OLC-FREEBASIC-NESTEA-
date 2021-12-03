









type as Bus _Bus

#pragma once

#include "crt.bi"

#include Once "NES\olc6502.bi"
#Include Once "NES\cartridge.bas"
#Include Once "NES\olc2c02.bas"
#include Once "NES\APU.bi" 'NesJS port




type Bus
	
	public:
	
	declare constructor()
	
  'devices on bus
  
  	ram(&H800) As uint8_t
	cpu as olc6502
	apu as APU 'NesJS port
	'ppu as olc2c02
	controller(2) As uint8_t
	'////////////
	
	frameirqwanted As BOOLEAN =  FALSE

 'declare sub _clock ()
 declare function _clock() as bool
 
 
   declare sub getsamples(bytes_data() As double, count As uint16_t) 
	Declare Function _read(adr As uint16_t,rdonly as boolean= false) As uint8_t
	Declare Function _write(adr As uint16_t,value As uint8_t) As boolean
   declare Sub _reset(hard as boolean)

  

	private:
	
    controller_state(2) As uint8_t
	As uint32_t nSystemClockCounter = 0
	
	
dma_page As uint8_t
dma_addr As uint8_t
dma_data As uint8_t

dma_transfer As bool = FALSE
dma_dummy As bool = TRUE


public:
declare sub SetSampleFrequency( sample_rate as uint32_t)
dAudioSample as Double= 0.0


private:
dAudioTime as double = 0.0
dAudioGlobalTime as Double = 0.0
dAudioTimePerNESClock as Double = 0.0
dAudioTimePerSystemSample  as Double = 0.0

 	 
End Type



