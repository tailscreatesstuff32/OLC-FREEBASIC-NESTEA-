'#Include Once "crt/mem.bi"

'#Include "nes/bus.bi"


'need to figure this out..............
'#Include "string.bi"
   Type UINT16T As uint16_t
'#Include "containers/vector.bi"
MMapTemplate(UINT16T ,String)
MListTemplate(String)





'.....................................


'TODO optional see what needs to be declareor private////////////////////////////////
Dim Shared a As uint8_t 
Dim Shared x As uint8_t
Dim Shared y As uint8_t
Dim Shared stkp As uint8_t
Dim Shared pc As uint16_t
'works correctly///////////////////////////////////
Dim Shared cpu cpustatus  As uint8_t
Enum FLAGS6502
	C = (1 Shl 0)
	Z = (1 Shl 1)
	I = (1 Shl 2)
	D = (1 Shl 3)
	B = (1 Shl 4)
	U = (1 Shl 5)
	V = (1 Shl 6)
	N = (1 Shl 7)

End Enum
Declare  Function Getflag(f As FLAGS6502) As uint8_t
Declare  Sub SetFlag(f As FLAGS6502,v As BOOL)
'//////////////////////////////////////////////////

Declare  Sub cpu_reset()
Declare  Sub irq()
Declare  Sub nmi()
Declare  Sub clock_cpu()


Declare  Function complete As bool

'TODO add disassemble with type MAP

'TODO can bus connect?

Dim Shared fetched As uint8_t
Dim Shared  temp As uint16_t
Dim Shared  addr_abs As uint16_t
Dim Shared addr_rel As uint16_t
Dim Shared opcode As uint8_t
Dim Shared cycles As uint8_t
Dim Shared clock_count As uint32_t

Declare  Function cpu_read( a As uint16_t) As uint8_t
Declare sub cpu_write( a As uint16_t,d As uint8_t ) 

declare Function fetch() As uint8_t

Type INSTRUCTION
	Name As String * 3
   operate As  Function() As uint8_t '= NULL
	addrmode  As  Function() As uint8_t' = NULL
	cycles As uint8_t '= 0
	
End Type

'NES opcodes
declare  Function op_ADC() As uint8_t: Declare  Function op_AND() As uint8_t
declare  Function op_ASL() As uint8_t: Declare  Function op_BCC() As uint8_t
declare  Function op_BCS() As uint8_t: Declare  Function op_BEQ() As uint8_t
declare  Function op_BIT() As uint8_t: Declare  Function op_BMI() As uint8_t
declare  Function op_BNE() As uint8_t: Declare  Function op_BPL() As uint8_t
declare  Function op_BRK() As uint8_t: Declare  Function op_BVC() As uint8_t
declare  Function op_BVS() As uint8_t: Declare  Function op_CLC() As uint8_t
declare  Function op_CLD() As uint8_t: Declare  Function op_CLI() As uint8_t
declare  Function op_CLV() As uint8_t: Declare  Function op_CMP() As uint8_t
declare  Function op_CPX() As uint8_t: Declare  Function op_CPY() As uint8_t
declare  Function op_DEC() As uint8_t: Declare  Function op_DEX() As uint8_t
declare  Function op_DEY() As uint8_t: Declare  Function op_EOR() As uint8_t
declare  Function op_INC() As uint8_t: Declare  Function op_INX() As uint8_t
declare  Function op_INY() As uint8_t: Declare  Function op_JMP() As uint8_t
declare  Function op_JSR() As uint8_t: Declare  Function op_LDA() As uint8_t
declare  Function op_LDX() As uint8_t: Declare  Function op_LDY() As uint8_t
declare  Function op_LSR() As uint8_t: Declare  Function op_NOP() As uint8_t
declare  Function op_ORA() As uint8_t: Declare  Function op_PHA() As uint8_t
declare  Function op_PHP() As uint8_t: Declare  Function op_PLA() As uint8_t
declare  Function op_PLP() As uint8_t: Declare  Function op_ROL() As uint8_t
declare  Function op_ROR() As uint8_t: Declare  Function op_RTI() As uint8_t
declare  Function op_RTS() As uint8_t: Declare  Function op_SBC() As uint8_t
declare  Function op_SEC() As uint8_t: Declare  Function op_SED() As uint8_t
declare  Function op_SEI() As uint8_t: Declare  Function op_STA() As uint8_t
declare  Function op_STX() As uint8_t: Declare  Function op_STY() As uint8_t
declare  Function op_TAX() As uint8_t: Declare  Function op_TAY() As uint8_t
declare  Function op_TSX() As uint8_t: Declare  Function op_TXA() As uint8_t
declare  Function op_TXS() As uint8_t: Declare  Function op_TYA() As uint8_t
Declare  Function op_XXX() As uint8_t

'NES address modes
declare  Function ad_IMP() As uint8_t:Declare  Function ad_IMM() As uint8_t
declare  Function ad_ZPO() As uint8_t:Declare  Function ad_ZPX() As uint8_t
declare  Function ad_ZPY() As uint8_t:Declare  Function ad_REL() As uint8_t
declare  Function ad_ABS() As uint8_t:Declare  Function ad_ABX() As uint8_t
declare  Function ad_ABY() As uint8_t:Declare  Function ad_IND() As uint8_t
declare  Function ad_IZX() As uint8_t:Declare  Function ad_IZY() As uint8_t
'/////////////////////////////////////////////////////


'TODO maybe turn into a VECTOR?
Dim Shared As INSTRUCTION lookup(0 To 256-1) => _ ' recheck
{ _ 
("BRK",@op_BRK,@ad_IMM,7),("ORA",@op_ORA,@ad_IZX,6),("???",@op_XXX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,8),("???",@op_NOP,@ad_IMP,3),("ORA",@op_ORA,@ad_ZPO,3),("ASL",@op_ASL,@ad_ZPO,5),("???",@op_XXX,@ad_IMP,5),("PHP",@op_PHP,@ad_IMP,3),("ORA",@op_ORA,@ad_IMM,2),("ASL",@op_ASL,@ad_IMP,2),("???",@op_XXX,@ad_IMP,2),("???",@op_NOP,@ad_IMP,4),("ORA",@op_ORA,@ad_ABS,4),("ASL",@op_ASL,@ad_ABS,6),("???",@op_XXX,@ad_IMP,6), _                    
("BPL",@op_BPL,@ad_REL,2),("ORA",@op_ORA,@ad_IZY,5),("???",@op_XXX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,8),("???",@op_NOP,@ad_IMP,4),("ORA",@op_ORA,@ad_ZPX,4),("ASL",@op_ASL,@ad_ZPX,6),("???",@op_XXX,@ad_IMP,6),("CLC",@op_CLC,@ad_IMP,2),("ORA",@op_ORA,@ad_ABY,4),("???",@op_NOP,@ad_IMP,2),("???",@op_XXX,@ad_IMP,7),("???",@op_NOP,@ad_IMP,4),("ORA",@op_ORA,@ad_ABX,4),("ASL",@op_ASL,@ad_ABX,7),("???",@op_XXX,@ad_IMP,7), _
("JSR",@op_JSR,@ad_ABS,6),("AND",@op_AND,@ad_IZX,6),("???",@op_XXX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,8),("BIT",@op_BIT,@ad_ZPO,3),("AND",@op_AND,@ad_ZPO,3),("ROL",@op_ROL,@ad_ZPO,5),("???",@op_XXX,@ad_IMP,5),("PLP",@op_PLP,@ad_IMP,4),("AND",@op_AND,@ad_IMM,2),("ROL",@op_ROL,@ad_IMP,2),("???",@op_XXX,@ad_IMP,2),("BIT",@op_BIT,@ad_ABS,4),("AND",@op_AND,@ad_ABS,4),("ROL",@op_ROL,@ad_ABS,6),("???",@op_XXX,@ad_IMP,6), _
("BMI",@op_BMI,@ad_REL,2),("AND",@op_AND,@ad_IZY,5),("???",@op_XXX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,8),("???",@op_NOP,@ad_IMP,4),("AND",@op_AND,@ad_ZPX,4),("ROL",@op_ROL,@ad_ZPX,6),("???",@op_XXX,@ad_IMP,6),("SEC",@op_SEC,@ad_IMP,2),("AND",@op_AND,@ad_ABY,4),("???",@op_NOP,@ad_IMP,2),("???",@op_XXX,@ad_IMP,7),("???",@op_NOP,@ad_IMP,4),("AND",@op_AND,@ad_ABX,4),("ROL",@op_ROL,@ad_ABX,7),("???",@op_XXX,@ad_IMP,7), _
("RTI",@op_RTI,@ad_IMP,6),("EOR",@op_EOR,@ad_IZX,6),("???",@op_XXX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,8),("???",@op_NOP,@ad_IMP,3),("EOR",@op_EOR,@ad_ZPO,3),("LSR",@op_LSR,@ad_ZPO,5),("???",@op_XXX,@ad_IMP,5),("PHA",@op_PHA,@ad_IMP,3),("EOR",@op_EOR,@ad_IMM,2),("LSR",@op_LSR,@ad_IMP,2),("???",@op_XXX,@ad_IMP,2),("JMP",@op_JMP,@ad_ABS,3),("EOR",@op_EOR,@ad_ABS,4),("LSR",@op_LSR,@ad_ABS,6),("???",@op_XXX,@ad_IMP,6), _
("BVC",@op_BVC,@ad_REL,2),("EOR",@op_EOR,@ad_IZY,5),("???",@op_XXX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,8),("???",@op_NOP,@ad_IMP,4),("EOR",@op_EOR,@ad_ZPX,4),("LSR",@op_LSR,@ad_ZPX,6),("???",@op_XXX,@ad_IMP,6),("CLI",@op_CLI,@ad_IMP,2),("EOR",@op_EOR,@ad_ABY,4),("???",@op_NOP,@ad_IMP,2),("???",@op_XXX,@ad_IMP,7),("???",@op_NOP,@ad_IMP,4),("EOR",@op_EOR,@ad_ABX,4),("LSR",@op_LSR,@ad_ABX,7),("???",@op_XXX,@ad_IMP,7), _
("RTS",@op_RTS,@ad_IMP,6),("ADC",@op_ADC,@ad_IZX,6),("???",@op_XXX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,8),("???",@op_NOP,@ad_IMP,3),("ADC",@op_ADC,@ad_ZPO,3),("ROR",@op_ROR,@ad_ZPO,5),("???",@op_XXX,@ad_IMP,5),("PLA",@op_PLA,@ad_IMP,4),("ADC",@op_ADC,@ad_IMM,2),("ROR",@op_ROR,@ad_IMP,2),("???",@op_XXX,@ad_IMP,2),("JMP",@op_JMP,@ad_IND,5),("ADC",@op_ADC,@ad_ABS,4),("ROR",@op_ROR,@ad_ABS,6),("???",@op_XXX,@ad_IMP,6), _
("BVS",@op_BVS,@ad_REL,2),("ADC",@op_ADC,@ad_IZY,5),("???",@op_XXX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,8),("???",@op_NOP,@ad_IMP,4),("ADC",@op_ADC,@ad_ZPX,4),("ROR",@op_ROR,@ad_ZPX,6),("???",@op_XXX,@ad_IMP,6),("SEI",@op_SEI,@ad_IMP,2),("ADC",@op_ADC,@ad_ABY,4),("???",@op_NOP,@ad_IMP,2),("???",@op_XXX,@ad_IMP,7),("???",@op_NOP,@ad_IMP,4),("ADC",@op_ADC,@ad_ABX,4),("ROR",@op_ROR,@ad_ABX,7),("???",@op_XXX,@ad_IMP,7), _
("???",@op_NOP,@ad_IMP,2),("STA",@op_STA,@ad_IZX,6),("???",@op_NOP,@ad_IMP,2),("???",@op_XXX,@ad_IMP,6),("STY",@op_STY,@ad_ZPO,3),("STA",@op_STA,@ad_ZPO,3),("STX",@op_STX,@ad_ZPO,3),("???",@op_XXX,@ad_IMP,3),("DEY",@op_DEY,@ad_IMP,2),("???",@op_NOP,@ad_IMP,2),("TXA",@op_TXA,@ad_IMP,2),("???",@op_XXX,@ad_IMP,2),("STY",@op_STY,@ad_ABS,4),("STA",@op_STA,@ad_ABS,4),("STX",@op_STX,@ad_ABS,4),("???",@op_XXX,@ad_IMP,4), _
("BCC",@op_BCC,@ad_REL,2),("STA",@op_STA,@ad_IZY,6),("???",@op_XXX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,6),("STY",@op_STY,@ad_ZPX,4),("STA",@op_STA,@ad_ZPX,4),("STX",@op_STX,@ad_ZPY,4),("???",@op_XXX,@ad_IMP,4),("TYA",@op_TYA,@ad_IMP,2),("STA",@op_STA,@ad_ABY,5),("TXS",@op_TXS,@ad_IMP,2),("???",@op_XXX,@ad_IMP,5),("???",@op_NOP,@ad_IMP,5),("STA",@op_STA,@ad_ABX,5),("???",@op_XXX,@ad_IMP,5),("???",@op_XXX,@ad_IMP,5), _
("LDY",@op_LDY,@ad_IMM,2),("LDA",@op_LDA,@ad_IZX,6),("LDX",@op_LDX,@ad_IMM,2),("???",@op_XXX,@ad_IMP,6),("LDY",@op_LDY,@ad_ZPO,3),("LDA",@op_LDA,@ad_ZPO,3),("LDX",@op_LDX,@ad_ZPO,3),("???",@op_XXX,@ad_IMP,3),("TAY",@op_TAY,@ad_IMP,2),("LDA",@op_LDA,@ad_IMM,2),("TAX",@op_TAX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,2),("LDY",@op_LDY,@ad_ABS,4),("LDA",@op_LDA,@ad_ABS,4),("LDX",@op_LDX,@ad_ABS,4),("???",@op_XXX,@ad_IMP,4), _
("BCS",@op_BCS,@ad_REL,2),("LDA",@op_LDA,@ad_IZY,5),("???",@op_XXX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,5),("LDY",@op_LDY,@ad_ZPX,4),("LDA",@op_LDA,@ad_ZPX,4),("LDX",@op_LDX,@ad_ZPY,4),("???",@op_XXX,@ad_IMP,4),("CLV",@op_CLV,@ad_IMP,2),("LDA",@op_LDA,@ad_ABY,4),("TSX",@op_TSX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,4),("LDY",@op_LDY,@ad_ABX,4),("LDA",@op_LDA,@ad_ABX,4),("LDX",@op_LDX,@ad_ABY,4),("???",@op_XXX,@ad_IMP,4), _
("CPY",@op_CPY,@ad_IMM,2),("CMP",@op_CMP,@ad_IZX,6),("???",@op_NOP,@ad_IMP,2),("???",@op_XXX,@ad_IMP,8),("CPY",@op_CPY,@ad_ZPO,3),("CMP",@op_CMP,@ad_ZPO,3),("DEC",@op_DEC,@ad_ZPO,5),("???",@op_XXX,@ad_IMP,5),("INY",@op_INY,@ad_IMP,2),("CMP",@op_CMP,@ad_IMM,2),("DEX",@op_DEX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,2),("CPY",@op_CPY,@ad_ABS,4),("CMP",@op_CMP,@ad_ABS,4),("DEC",@op_DEC,@ad_ABS,6),("???",@op_XXX,@ad_IMP,6), _
("BNE",@op_BNE,@ad_REL,2),("CMP",@op_CMP,@ad_IZY,5),("???",@op_XXX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,8),("???",@op_NOP,@ad_IMP,4),("CMP",@op_CMP,@ad_ZPX,4),("DEC",@op_DEC,@ad_ZPX,6),("???",@op_XXX,@ad_IMP,6),("CLD",@op_CLD,@ad_IMP,2),("CMP",@op_CMP,@ad_ABY,4),("NOP",@op_NOP,@ad_IMP,2),("???",@op_XXX,@ad_IMP,7),("???",@op_NOP,@ad_IMP,4),("CMP",@op_CMP,@ad_ABX,4),("DEC",@op_DEC,@ad_ABX,7),("???",@op_XXX,@ad_IMP,7), _
("CPX",@op_CPX,@ad_IMM,2),("SBC",@op_SBC,@ad_IZX,6),("???",@op_NOP,@ad_IMP,2),("???",@op_XXX,@ad_IMP,8),("CPX",@op_CPX,@ad_ZPO,3),("SBC",@op_SBC,@ad_ZPO,3),("INC",@op_INC,@ad_ZPO,5),("???",@op_XXX,@ad_IMP,5),("INX",@op_INX,@ad_IMP,2),("SBC",@op_SBC,@ad_IMM,2),("NOP",@op_NOP,@ad_IMP,2),("???",@op_SBC,@ad_IMP,2),("CPX",@op_CPX,@ad_ABS,4),("SBC",@op_SBC,@ad_ABS,4),("INC",@op_INC,@ad_ABS,6),("???",@op_XXX,@ad_IMP,6), _
("BEQ",@op_BEQ,@ad_REL,2),("SBC",@op_SBC,@ad_IZY,5),("???",@op_XXX,@ad_IMP,2),("???",@op_XXX,@ad_IMP,8),("???",@op_NOP,@ad_IMP,4),("SBC",@op_SBC,@ad_ZPX,4),("INC",@op_INC,@ad_ZPX,6),("???",@op_XXX,@ad_IMP,6),("SED",@op_SED,@ad_IMP,2),("SBC",@op_SBC,@ad_ABY,4),("NOP",@op_NOP,@ad_IMP,2),("???",@op_XXX,@ad_IMP,7),("???",@op_NOP,@ad_IMP,4),("SBC",@op_SBC,@ad_ABX,4),("INC",@op_INC,@ad_ABX,7),("???",@op_XXX,@ad_IMP,7)  _
}
Declare Sub map_InOrder (pRoot As MAPNODEUINT16TSTRING Ptr)

'Function cpu_read(a As uint16_t) As uint8_t
'	
'	Return bus_Read(a,false)
'
'	
'End Function
'
'Sub cpu_write(a As uint16_t,d As uint8_t )
'	
'	 bus_Write(a,d)
'		
'	
'End Sub
'
'
'Sub irq()
'	If (getflag(I)) = 0 Then
'		
'		'	// Push the program counter to the stack. It's 16-bits dont
'	'	// forget so that takes two pushes
'		bus_write(&H0100 + stkp, (pc shr 8) And &H00FF) 
'		stkp-=1 
'		bus_write(&H0100 + stkp, pc And &H00FF) 
'		stkp-=1
'
'		'// Then Push the  cpustatus  register to the stack
'		SetFlag(B, 0)
'		SetFlag(U, 1)
'		SetFlag(I, 1)
'		bus_write(&H0100 + stkp,  cpustatus )
'		stkp-=1 
'
'		'// Read new program counter location from fixed address
'		addr_abs = &HFFFE
'		dim lo As uint16_t = bus_read(addr_abs + 0) 
'		Dim hi As uint16_t = bus_read(addr_abs + 1) 
'		pc = (hi Shl 8) Or lo 
'
'		'// IRQs take time
'		cycles = 7 
'		
'		
'	EndIf
'	
'	
'End Sub
'
'Sub nmi()
'	bus_write(&H0100 + stkp, (pc Shr 8) and &H00FF) 
'	stkp-=1 
'	bus_write(&H0100 + stkp, pc And &H00FF) 
'	stkp-=1 
'
'	SetFlag(B, 0) 
'	SetFlag(U, 1) 
'	SetFlag(I, 1) 
'	bus_write(&H0100 + stkp,  cpustatus ) 
'	stkp-=1 
'
'	addr_abs = &HFFFA 
'	dim lo As uint16_t = bus_read(addr_abs + 0) 
'	Dim hi As uint16_t = bus_read(addr_abs + 1) 
'	pc = (hi shl 8) or lo 
'
'	cycles = 8 
'	
'End Sub
'Sub SetFlag(f As FLAGS6502,v As BOOL) ' finished
'	
'	If (v) Then
'		
'		 cpustatus  Or= f
'	Else
'		 cpustatus  And= Not f
'		
'	EndIf
'	
'End Sub
'Function Getflag(f As FLAGS6502) As uint8_t ' finished
'	
'	Return IIf((( cpustatus  And f) > 0),1,0)
'	
'	
'	
'End Function
'
'
''added all//////////////////////////////////
'Function ad_IMP() As uint8_t ' finished
'	
'	
'	fetched = a
'	return 0
'
'End Function
'Function ad_IMM() As uint8_t ' finished
'	
'	
'	addr_abs = pc:pc+=1
'	
'	
'	
'	
'	Return 	0
'	
'End Function
'Function ad_ZPO() As uint8_t ' finished
'	
'	addr_abs = cpu_read(pc)
'	pc+=1
'   addr_abs And= &H00FF
'	Return 	0
'	
'End Function
'Function ad_ZPX() As uint8_t ' finished
'	
'
' 	addr_abs = (cpu_read(pc) + x)
'	pc+=1
'   addr_abs And= &H00FF
'	Return 	0
'	
'	
'End Function
'Function ad_ZPY() As uint8_t ' finished
'	
'	addr_abs = (cpu_read(pc) + y)
'	pc+=1
'   addr_abs And= &H00FF
'	Return 	0
'	
'End Function
'Function ad_REL() As uint8_t ' finished
'	
'	addr_rel = cpu_read(pc)
'	pc+=1
'	If (addr_rel And &H80) Then
'		
'		addr_rel Or= &HFF00
'		
'	EndIf
'Return 0
'End Function
'Function ad_IND() As uint8_t ' finished
'
'
'	dim ptr_lo  As uint16 = cpu_read(pc)
'	pc+=1
'	Dim  ptr_hi As uint16 = cpu_read(pc)
'	pc+=1
'
'	dim ptr1 As uint16_t = (ptr_hi shl 8) or ptr_lo
'
'	if (ptr_lo = &H00FF) Then' Simulate page boundary hardware bug
'	
'		addr_abs = (cpu_read(ptr1 and &HFF00) Shl 8) Or cpu_read(ptr1 + 0)
'	
'	else ' Behave normally
'	
'		addr_abs = (cpu_read(ptr1 + 1) Shl 8) or cpu_read(ptr1 + 0)
'	
'	End If
'	
'	return 0
'
'
'End Function
'Function ad_IZX() As uint8_t ' finished
'	
'	Dim t As uint16_t = cpu_read(pc)
'	pc+=1
'
'	Dim lo As uint16_t = cpu_read(cast(uint16_t,t + Cast(uint16_t,x)) And &H00FF)
'	dim hi  As  uint16_t = cpu_read(cast(uint16_t,t + Cast(uint16_t,x+ 1)) And &H00FF)
'
'	addr_abs = (hi shl 8) or lo
'
'	return 0
'	
'
'End Function
'Function ad_IZY() As uint8_t ' finished
'	
'	dim t As uint16_t = cpu_read(pc)
'	pc+=1
'
'	dim lo As uint16_t = cpu_read(t And &H00FF)
'	dim hi As uint16_t = cpu_read((t + 1) and &H00FF)
'
'	addr_abs = (hi shl 8) Or lo
'	addr_abs += y
'
'	if ((addr_abs and &HFF00) <> (hi Shl 8)) Then
'		return 1
'	else
'		return 0
'	End If
'	
'End Function
'Function ad_ABS() As uint8_t ' finished
'	Dim lo As uint16_t = cpu_read(pc)
'	pc+=1
'	Dim hi As uint16_t  = cpu_read(pc)
'	pc+=1
'
'	addr_abs = (hi Shl 8) or lo
'
'	return 0
'
'	
'End Function
'Function ad_ABX() As uint8_t ' finished
'	Dim lo As uint16_t = cpu_read(pc)
'	pc+=1
'	Dim hi As uint16_t  = cpu_read(pc)
'	pc+=1
'
'	addr_abs = (hi shl 8) Or lo
'	addr_abs += x
'
'	if ((addr_abs and &HFF00) <> (hi Shl 8)) Then
'		return 1
'	else
'		return 0
'   End if	
'
'End Function
'Function ad_ABY() As uint8_t ' finished
'	Dim lo As uint16_t = cpu_read(pc)
'	pc+=1
'	Dim hi As uint16_t  = cpu_read(pc)
'	pc+=1
'
'	addr_abs = (hi shl 8) Or lo
'	addr_abs += y
'
'	if ((addr_abs and &HFF00) <> (hi Shl 8)) Then
'		return 1
'	else
'		return 0
'	End if	
'
'
'End Function
''////////////////////////////////////////////
'
'Function complete() As BOOL ' finished
'	
'	Return cycles = 0
'	 
'	
'End Function
'Function fetch() As uint8_t' finished
'	
'	If not(lookup(opcode).addrmode = @ad_IMP) Then
'		fetched = cpu_read(addr_abs)
'		
'	End If
'	Return fetched 
'End Function
'
'
'Sub clock_cpu()' finished
'	
'	If (cycles = 0) Then
'		
'		opcode = cpu_read(pc)
'		
'		SetFlag(U,true)
'		
'		pc += 1
'		
'		cycles = lookup(opcode).cycles
'		
'		Dim additional_cycle1 As uint8_t = lookup(opcode).addrmode() 
'		Dim additional_cycle2 As uint8_t = lookup(opcode).operate() 
'		
'		SetFlag(U, true)
'		
'		cycles += (additional_cycle1 And additional_cycle2)
'		
'	EndIf
'	
'	clock_count +=1
'	cycles -= 1 
'	
'	
'End Sub
'
'Sub cpu_reset() ' finished
'	addr_abs = &HFFFC
'	dim lo As uint16_t = cpu_read(addr_abs + 0)
'	Dim hi As uint16_t = cpu_read(addr_abs + 1)
'
'
'	pc = (hi shl 8) Or lo
'
'
'	a = 0
'	x = 0
'	y = 0
'	stkp = &HFD
'	 cpustatus  = &H00 Or U
'
'	
'	addr_rel = &H0000
'	addr_abs = &H0000
'	fetched = &H00
'
'
'	cycles = 8
'	
'	
'	
'End Sub
'
'Function op_ADC() As uint8_t' finished
'	
'	    fetch()
'	    temp = Cast(uint16_t,a) + Cast(uint16_t,fetched) + Cast(uint16_t,getflag(c))
'	    
'	    setflag(c,temp > 255)
'	    SetFlag(Z, (temp and &H00FF) = 0)
'	    SetFlag(V, (not(Cast(uint16_t,a) Xor Cast(uint16_t,fetched)) and (cast(uint16_t,a) xor Cast(uint16_t,temp))) and &H0080)
'
'	    'The negative flag is set to the most significant bit of the result
'	    SetFlag(N, temp And &H80)
'
'	    'Load the result into the accumulator (it's 8-bit dont forget!)
'	    a = temp and &H00FF
'
'	    'This instruction has the potential to require an additional clock cycle
'	    return 1
'End Function
'Function op_SBC() As uint8_t' finished
'	
'	fetch()
'	
'	Dim value As uint16_t = Cast(uint16,fetched) Xor &H00FF
'	temp = Cast(uint16_t,a) + value + Cast(uint16_t,getflag(c))
'	SetFlag(C, temp and &HFF00)
'	SetFlag(Z, ((temp and &H00FF) = 0))
'	SetFlag(V, (temp xor cast(uint16_t,a)) and (temp xor value) and &H0080) 
'	SetFlag(N, temp And &H0080)
'	a = temp And &H00FF 
'	
'	Return 1
'	
'End Function
'Function op_AND() As uint8_t' finished
'	
'	fetch()
'	a = a And fetched
'	SetFlag(Z, a = &H00)
'	SetFlag(N, a and &H80)
'	Return 1
'		
'End Function
'Function op_ASL() As uint8_t' finished
'	
'	fetch()
'	temp = cast(uint16_t,fetched shl 1)
'	SetFlag(C, (temp and &HFF00) > 0)
'	SetFlag(Z, (temp And &H00FF) = &H00)
'	SetFlag(N, temp And &H80)
'	if (lookup(opcode).addrmode = @ad_IMP) Then
'		a = temp And &H00FF
'	else
'		cpu_write(addr_abs, temp And &H00FF)
'	End if
'	return 0
'
'		
'End Function
'Function op_BCC() As uint8_t' finished
'
'	if (GetFlag(C) = 0) Then
'	 
'		cycles+=1
'		addr_abs = pc + addr_rel
'
'		if((addr_abs and &HFF00) <> (pc And &HFF00)) Then
'			cycles+=1
'		End If
'
'		pc = addr_abs
'	End If
'	return 0
'	
'End Function
'Function op_BCS() As uint8_t' finished
'		if (GetFlag(C) = 1) Then
'	
'		cycles+= 1
'		addr_abs = pc + addr_rel
'
'		if ((addr_abs And &HFF00) <> (pc and &HFF00)) Then
'			cycles+=1
'		End if
'		pc = addr_abs
'		End If
'
'	Return 0
'	
'End Function
'Function op_BEQ() As uint8_t' finished
'	
'	If (GetFlag(Z) = 1) Then
'	
'		cycles+=1 
'		addr_abs = pc + addr_rel 
'
'		if ((addr_abs and &HFF00) <> (pc and &HFF00)) Then
'			cycles+=1  
'		End If
'
'		pc = addr_abs 
'	End if
'	return 0
'	
'End Function
'Function op_BIT() As uint8_t' finished
'	
'	fetch()
'	temp = a And fetched
'	SetFlag(Z, (temp and &H00FF) =  &H00)
'	SetFlag(N, fetched And (1 Shl 7))
'	SetFlag(V, fetched And (1 Shl 6))
'	return 0
'	
'	
'End Function
'Function op_BMI() As uint8_t' finished
'	
'	
'		if (GetFlag(N) =  1) Then
'	 
'		cycles+=1 
'		addr_abs = pc + addr_rel 
'
'		if ((addr_abs And &HFF00) <> (pc and &HFF00)) Then
'			cycles+=1
'		End If
'
'		pc = addr_abs
'	End If
'	return 0
'End Function
'Function op_BNE() As uint8_t' finished
'	
'		if (GetFlag(Z) = 0) Then
'	 
'			cycles+=1 
'			addr_abs = pc + addr_rel 
'
'			
'	   if ((addr_abs And &HFF00) <> (pc and &HFF00)) Then
'			cycles+=1
'	   End If
'	
'		pc = addr_abs
'		End If
'		
'	return 0
'	
'End Function
'Function op_BPL() As uint8_t' finished
'		if (GetFlag(N) = 0) Then
'	 
'		cycles+=1
'		addr_abs = pc + addr_rel 
'
'		if ((addr_abs and &HFF00) <> (pc and &HFF00)) Then
'			cycles+=1 
'	   End If
'		pc = addr_abs 
'		End If
'	return 0 
'		
'End Function
'Function op_BRK() As uint8_t' finished
'
'		pc+=1
'
'	SetFlag(I, 1)
'	cpu_write(&H0100 + stkp, (pc shr 8) and &H0FF)
'	stkp-=1
'	cpu_write(&H0100 + stkp, pc and &H00FF)
'	stkp-=1
'
'	SetFlag(B, 1)
'	cpu_write(&H0100 + stkp,  cpustatus ) 
'	stkp-=1
'	SetFlag(B, 0) 
'
'	pc = Cast(uint16,cpu_Read(&HFFFE)) Or Cast(uint16_t,cpu_read(&HFFFF) Shl 8)
'	return 0
'
'	
'End Function
'Function op_CLC() As uint8_t' finished
'
'	
'	SetFlag(C,FALSE)
'	Return 	0
'	
'End Function
'Function op_CLD() As uint8_t' finished
'
'	SetFlag(D,FALSE)
'	Return 	0
'	
'End Function
'Function op_CLI() As uint8_t' finished
'	SetFlag(I,FALSE)
'	Return 	0
'	
'End Function
'Function op_CLV() As uint8_t' finished
'	SetFlag(V,FALSE)
'	
'	Return 	0
'	
'End Function
'Function op_SEC() As uint8_t' finished
'	
'	SetFlag(C,TRUE)
'	Return 	0
'	
'End Function
'Function op_SED() As uint8_t' finished
'	
'	
'	SetFlag(D,TRUE)
'	Return 	0
'	
'End Function
'Function op_SEI() As uint8_t' finished
'	
'	SetFlag(I,TRUE)
'	Return 0
'	
'End Function
'Function op_XXX() As uint8_t' finished
'
'	Return 	0
'	
'End Function
'Function op_TXS() As uint8_t' finished
'	
'	stkp = x
'	Return 	0
'	
'End Function
'Function op_TYA() As uint8_t' finished
'	
'	SetFlag(Z,a = &H00)
'	SetFlag(N,a = &H80)
'	Return 	0
'	
'End Function
'Function op_NOP() As uint8_t' finished
'
'	Select Case  (opcode)  
'		case &H1C
'		case &H3C
'		case &H5C
'		case &H7C
'		case &HDC
'		case &HFC
'		return 1 
'	End Select
'	return 0
'End Function
'Function op_TXA() As uint8_t' finished
'	a = x
'	SetFlag(Z, a = &H00)
'	SetFlag(N, a And &H80)
'	return 0
'	
'End Function
'Function op_TSX() As uint8_t' finished
'	x = stkp
'	SetFlag(Z, x = &H00)
'	SetFlag(N, x And &H80)
'	return 0
'End Function
'Function op_TAY() As uint8_t' finished
'	
'	y = a
'	SetFlag(Z, y = &H00)
'	SetFlag(N, y And &H80)
'	return 0
'	
'End Function
'Function op_TAX() As uint8_t' finished
'   x = a 
'	SetFlag(Z, x =  &H00) 
'	SetFlag(N, x And &H80) 
'	Return 0 
'End Function
'Function op_STX() As uint8_t' finished
'
'	
'	cpu_write(addr_abs, x) 
'	return 0
'	
'End Function
'Function op_STY() As uint8_t' finished
'
'	cpu_write(addr_abs, y) 
'	return 0 
'	
'End Function
'Function op_STA() As uint8_t' finished
' 
'	cpu_write(addr_abs, a) 
'	return 0
'	
'End Function
'Function op_PLP() As uint8_t' finished
'	
'	stkp+=1 
'	 cpustatus  = cpu_read(&H0100 + stkp) 
'	SetFlag(U, 1) 
'	return 0 
'	
'End Function
'Function op_RTI() As uint8_t' finished
'	 
'	stkp+=1
'	 cpustatus  = cpu_read(&H0100 + stkp) 
'	 cpustatus  And= Not(B)
'	 cpustatus  And= Not(U)
'
'	stkp+=1
'	pc = cast(uint16_t,cpu_read(&H0100 + stkp))
'	stkp+=1
'	pc Or= cast(uint16_t,cpu_read(&H0100 + stkp))  Shl 8 
'	return 0
'	
'End Function
'Function op_RTS() As uint8_t' finished
'	 
'	stkp+=1
'	pc = cast(uint16_t,cpu_read(&H0100 + stkp))
'	stkp+=1
'	pc Or= cast(uint16_t,cpu_read(&H0100 + stkp))  Shl 8
'
'	pc+=1
'	return 0 
'	
'End Function
'Function op_ROL() As uint8_t' finished
'
'	fetch()
'	temp = Cast(uint16_t,fetched  Shl 1) Or GetFlag(C)
'	SetFlag(C, temp And &HFF00)
'	SetFlag(Z, (temp and &H00FF)  = &H0000)
'	SetFlag(N, temp And &H0080)
'	if (lookup(opcode).addrmode = @ad_IMP) then
'		a = temp And &H00FF
'	else
'		cpu_write(addr_abs, temp And &H00FF)
'	End If
'	return 0
'End Function
'Function op_ROR() As uint8_t' finished
'	
'	fetch()
'	temp = cast(uint16_t,GetFlag(C) shl 7) or (fetched Shl 1)
'	SetFlag(C, fetched and &H01)
'	SetFlag(Z, (temp And &H00FF) = &H00)
'	SetFlag(N, temp And &H0080)
'	
'	if (lookup(opcode).addrmode = @ad_IMP) Then
'		a = temp And &H00FF
'	else
'		cpu_write(addr_abs, temp and &H00FF)
'	End if
'	return 0
'	
'End Function
'Function op_EOR() As uint8_t' finished
'	
'	fetch()
'	a = a xor fetched 
'	SetFlag(Z, a =  &H00) 
'	SetFlag(N, a And &H80) 
'	return 1  
'End Function
'Function op_INX() As uint8_t' finished
'	
'	x+=1
'	SetFlag(Z, x =  &H00)
'	SetFlag(N, x and &H80)
'	return 0 
'	
'End Function
'Function op_INY() As uint8_t' finished
'	
'	
'	y+=1
'	SetFlag(Z, y = &H00)
'	SetFlag(N, y and &H80)
'	return 0
'	
'End Function
'Function op_DEX() As uint8_t' finished
'	
'	x-=1
'	SetFlag(Z, x = &H00)
'	SetFlag(N, x And &H80)
'	return 0
'	
'End Function
'Function op_DEY() As uint8_t' finished
'
'	y-=1
'	SetFlag(Z, y = &H00)
'	SetFlag(N, y And &H80)
'	return 0
'	
'End Function
'Function op_DEC() As uint8_t' finished
'	
'	fetch() 
'	temp = fetched - 1 
'	cpu_write(addr_abs, temp and &H00FF) 
'	SetFlag(Z, (temp and &H00FF) =  &H0000) 
'	SetFlag(N, temp and &H0080) 
'	return 0 
'
'End Function
'Function op_INC() As uint8_t' finished
'
'	fetch()
'	temp = fetched + 1
'	cpu_write(addr_abs, temp and &H00FF)
'	SetFlag(Z, (temp And &H00FF) = &H0000)
'	SetFlag(N, temp And &H0080)
'	return 0
'
'End Function
'Function op_JMP() As uint8_t' finished
'	
'	
'	pc = addr_abs
'	return 0
'	
'End Function
'Function op_JSR() As uint8_t' finished
'	pc-=1
'
'	cpu_write(&H0100 + stkp, (pc Shr 8) And &H00FF)
'	stkp-=1
'	cpu_write(&H100 + stkp, pc And &H00FF)
'	stkp-=1
'
'	pc = addr_abs
'	return 0
'End Function
'Function op_BVC() As uint8_t' finished
'	 
'		if (GetFlag(V) = 0) Then
'	 
'		cycles+=1 
'		addr_abs = pc + addr_rel 
'
'		if ((addr_abs and &HFF00) <> (pc and &HFF00)) Then
'			cycles+=1 
'		End If
'
'		pc = addr_abs 
'	End If
'	return 0 
'	
'End Function
'Function op_BVS() As uint8_t' finished
'	
'	If (GetFlag(V)  = 1) Then
'	 
'		cycles+=1
'		addr_abs = pc + addr_rel 
'
'		if ((addr_abs And &HFF00) <> (pc and &HFF00)) then
'			cycles+=1
'	End If
'
'		pc = addr_abs 
'	End If
'	return 0 
'End Function
'Function op_CMP() As uint8_t' finished
'	 
'	fetch()
'	temp = cast(uint16_t,a) - Cast(uint16_t,fetched)
'	SetFlag(C, a >= fetched)
'	SetFlag(Z, (temp and &H00FF) = &H0000) 
'	SetFlag(N, temp And &H0080)
'	return 1
'	
'End Function
'Function op_PHA() As uint8_t' finished
'	
'	cpu_write(&H0100 + stkp, a) 
'	stkp-=1
'	return 0 
'	
'End Function
'Function op_PHP() As uint8_t' finished
'	
'	cpu_write(&H0100 + stkp,  cpustatus  or B or U) 
'	SetFlag(B, 0) 
'	SetFlag(U, 0) 
'	stkp-=1 
'	return 0 
'	
'End Function
'Function op_LDA() As uint8_t' finished
'	
'	
'	fetch()
'	a = fetched
'	SetFlag(Z, a =  &H00)
'	SetFlag(N, a And &H80)
'	return 1
'	
'End Function
'Function op_PLA() As uint8_t' finished
'	
'	stkp+=1 
'	a = cpu_read(&H0100 + stkp) 
'	SetFlag(Z, a =  &H00) 
'	SetFlag(N, a and &H80) 
'	return 0 
'
'	
'End Function
'Function op_ORA() As uint8_t' finished
'	fetch() 
'	a = a or fetched 
'	SetFlag(Z, a =  &H00) 
'	SetFlag(N, a And &H80) 
'	return 1 
'End Function
'Function op_LSR() As uint8_t' finished
'	
'	fetch() 
'	SetFlag(C, fetched And  &H0001) 
'	temp = fetched Shr  1
'	SetFlag(Z, (temp And &H00FF) =  &H0000) 
'	SetFlag(N, temp And &H0080) 
'	if (lookup(opcode).addrmode =  @ad_IMP) Then
'		a = temp And &H00FF 
'	else
'		cpu_write(addr_abs, temp and &H00FF)
'	End If 
'	return 0 
'End Function
'Function op_LDX() As uint8_t' finished
'	
'	fetch()  
'	x = fetched 
'	SetFlag(Z, x =  &H00) 
'	SetFlag(N, x And &H80) 
'	return 1 
'
'	
'End Function
'Function op_LDY() As uint8_t' finished
'	
'	fetch() 
'	y = fetched 
'	SetFlag(Z, y =  &H00) 
'	SetFlag(N, y and &H80) 
'	
'return 1 
'	
'End Function
'Function op_CPX() As uint8_t' finished
'	
'	fetch()
'	temp = Cast(uint16_t,x) - cast(uint16_t,fetched)
'	SetFlag(C, x >= fetched)
'	SetFlag(Z, (temp and &H00FF)  = &H0000)
'	SetFlag(N, temp And &H0080)
'	return 0
'	
'End Function
'Function op_CPY() As uint8_t' finished
'	
'	fetch()
'	temp = cast(uint16_t,y )- cast(uint16_t,fetched)
'	SetFlag(C, y >= fetched)
'	SetFlag(Z, (temp And &H00FF) =  &H0000)
'	SetFlag(N, temp And &H0080)
'	return 0
'	
'End Function
'
'
'
'
'	Function hex1 (n As uint32_t,  d As uint8_t) As  string
'	
'		 Dim s As String = String(d, "0")
'		 Dim i As Integer 
'		
'		'for (int i = d - 1; i >= 0; i--, n >>= 4)
'		i = d-1
'		While i >= 0
'			s[i] = Asc("0123456789ABCDEF", (n And &Hf)+1)  '[n And &HF]
'				n shr= 4
'			   i-=1
'		Wend
'		'	s[i] = "0123456789ABCDEF"[n & &HF];
'		'Next
'		
'		return s
'	End Function
'
'Function disassemble(nStart As uint16_t,nStop As uint16_t)   As TMAPUINT16TSTRING 'done until i figure out MAPS in freebasic
'	Dim addr1 As uint32_t = nStart
'  Dim i As Integer
'	
'	Dim maplines As  TMAPUINT16TSTRING
'	
'	
'	Dim As uint8_t value,lo,hi
'	Dim As uint16_t line_addr 
'	Dim sInst As String
'	Dim opcode As uint8_t
'	
'	
'	 While (addr1  <= Cast(uint32_t,nStop))
'	 	
'	 	line_addr = addr1
'	 	
'	 	i +=0
'	 	sInst = "$" + hex1(addr1,4) + ": "
'
'	 	opcode = bus_read(addr1,TRUE):addr1+=1
'	
'	 	sInst += lookup(opcode).name + " "
'	 	
'	 	
'	 	If lookup(opcode).addrmode = @ad_IMP Then
'	 		
'	 		sInst += " {IMP}"
'	 		
'	 	ElseIf lookup(opcode).addrmode = @ad_IMM Then
'	 		
'	 		value = bus_read(addr1,TRUE):addr1+=1
'	 		sInst += "#$" + hex1(value, 2) + " {IMM}"
'	 		
'	 	ElseIf lookup(opcode).addrmode = @ad_ZPO Then
'	 		
'	 		lo = bus_read(addr1,TRUE):addr1+=1
'	 		hi = 0
'	 		sInst += "$" + hex1(lo, 2) + " {ZP0}"
'	 		
'	 	ElseIf lookup(opcode).addrmode = @ad_ZPX Then
'	 		
'	 		lo = bus_read(addr1,TRUE):addr1+=1
'	 		hi = 0
'	 		sInst += "$" + hex1(lo, 2) + ", X {ZPX}"
'	 		
'	 	ElseIf lookup(opcode).addrmode = @ad_ZPY Then
'	 		
'	 		lo = bus_read(addr1,TRUE):addr1+=1
'	 		hi = 0
'	 		sInst += "$" + hex1(lo, 2) + ", Y {ZPY}"
'	 		
'	 	ElseIf lookup(opcode).addrmode = @ad_IZX Then
'	 		lo = bus_read(addr1,TRUE):addr1+=1
'	 		hi = 0
'	 		sInst += "($" + hex1(lo, 2) + ", X) {IZX}"
'	 		
'	 	ElseIf lookup(opcode).addrmode = @ad_IZY Then
'	 		
'	 		 lo = bus_read(addr1,TRUE):addr1+=1
'	 	    hi = 0
'	 		 sInst += "($" + hex1(lo, 2) + "), Y {IZY}"
'	 		 	
'	 	ElseIf lookup(opcode).addrmode = @ad_ABS Then 
'	 		
'	 		 lo = bus_read(addr1,TRUE):addr1+=1
'	 		 hi = bus_read(addr1,TRUE):addr1+=1
'	 		 sInst += "$" + hex1(Cast(uint16_t,hi Shl 8) or lo, 4) + " {ABS}"
'	 		
'	 	ElseIf lookup(opcode).addrmode = @ad_ABX Then
'	 		
'	 	     lo = bus_read(addr1,TRUE):addr1+=1
'	 		  hi = bus_read(addr1,TRUE):addr1+=1
'	 		  sInst += "$" + hex1(Cast(uint16_t,hi Shl 8) or lo, 4) + ", X {ABX}"
'	 				
'	 	ElseIf lookup(opcode).addrmode = @ad_ABY Then
'	 		
'	 	      lo = bus_read(addr1,TRUE):addr1+=1
'	 	      hi = bus_read(addr1,TRUE):addr1+=1
'	 	      sInst += "$" + hex1(Cast(uint16_t,hi Shl 8) or lo, 4) + ", Y {ABY}"
'	 		
'	 	ElseIf lookup(opcode).addrmode = @ad_IND Then 
'	 		
'	 		    lo = bus_read(addr1,TRUE):addr1+=1
'	 		    hi = bus_read(addr1,TRUE):addr1+=1
'	 		    sInst += "($" + hex1(Cast(uint16_t,hi Shl 8) Or lo, 4) + ") {IND}"
'	 		
'	 	ElseIf lookup(opcode).addrmode = @ad_REL Then	
'	 		
'	 	       value = bus_read(addr1,TRUE):addr1+=1
'	 		    sInst += "$" + hex1(value, 2) + " [$" + hex1(addr1 + value, 4) + "] {REL}" 'cast(int8_t,value)
'	 		
'	 	EndIf
'	 	
'	 		 
'	maplines.insert(line_addr,sInst)
'	 Wend
'	 
'	 
'	 
'
'	Return maplines
'	
'End Function
'
'
'
'
'
'
'
'
'
'
'







