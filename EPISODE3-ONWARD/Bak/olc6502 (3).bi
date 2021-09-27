'#Include Once "crt/mem.bi"
#Include once "crt.bi"
#Include Once "crt/stdint.bi"

'#Include "containers/vector.bi"


'#Include "containers/map.bi
'#Include "string.bi"


#Include Once "win/windef.bi"

'TODO optional see what needs to be public or private////////////////////////////////
Dim Shared a As uint8_t
Dim Shared x As uint8_t
Dim Shared y As uint8_t
Dim Shared stkp As uint8_t
Dim Shared pc As uint16_t
'works correctly///////////////////////////////////
Dim Shared status As uint8_t
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
Declare Function Getflag(f As FLAGS6502) As uint8_t
Declare Sub SetFlag(f As FLAGS6502,v As BOOL)
'//////////////////////////////////////////////////





Declare Sub Reset_cpu()
Declare Sub irq()
Declare Sub nmi()
Declare Sub clock()


Declare Function complete As bool

'TODO add disassemble with type MAP


'TODO can bus connect?


Dim fetched As uint8_t
Dim temp As uint16_t
Dim addr_abs As uint16_t
Dim addr_rel As uint16_t
Dim opcode As uint8_t
Dim cycles As uint8_t
Dim clock_count As uint32_t

Declare Function cpu_read( a As uint16_t) As uint8_t
Declare sub cpu_write( a As uint16_t,d As uint8_t ) 


Declare Function fetch() As uint8_t








Type INSTRUCTION
	Name As String * 3
   operate As  Function() As uint8_t '= NULL
	addrmode  As  Function() As uint8_t' = NULL
	cycles As uint8_t '= 0
	
End Type









'NES opcodes
Declare Function op_ADC() As uint8_t: Declare Function op_AND() As uint8_t
Declare Function op_ASL() As uint8_t: Declare Function op_BCC() As uint8_t
Declare Function op_BCS() As uint8_t: Declare Function op_BEQ() As uint8_t
Declare Function op_BIT() As uint8_t: Declare Function op_BMI() As uint8_t
Declare Function op_BNE() As uint8_t: Declare Function op_BPL() As uint8_t
Declare Function op_BRK() As uint8_t: Declare Function op_BVC() As uint8_t
Declare Function op_BVS() As uint8_t: Declare Function op_CLC() As uint8_t
Declare Function op_CLD() As uint8_t: Declare Function op_CLI() As uint8_t
Declare Function op_CLV() As uint8_t: Declare Function op_CMP() As uint8_t
Declare Function op_CPX() As uint8_t: Declare Function op_CPY() As uint8_t
Declare Function op_DEC() As uint8_t: Declare Function op_DEX() As uint8_t
Declare Function op_DEY() As uint8_t: Declare Function op_EOR() As uint8_t
Declare Function op_INC() As uint8_t: Declare Function op_INX() As uint8_t
Declare Function op_INY() As uint8_t: Declare Function op_JMP() As uint8_t
Declare Function op_JSR() As uint8_t: Declare Function op_LDA() As uint8_t
Declare Function op_LDX() As uint8_t: Declare Function op_LDY() As uint8_t
Declare Function op_LSR() As uint8_t: Declare Function op_NOP() As uint8_t
Declare Function op_ORA() As uint8_t: Declare Function op_PHA() As uint8_t
Declare Function op_PHP() As uint8_t: Declare Function op_PLA() As uint8_t
Declare Function op_PLP() As uint8_t: Declare Function op_ROL() As uint8_t
Declare Function op_ROR() As uint8_t: Declare Function op_RTI() As uint8_t
Declare Function op_RTS() As uint8_t: Declare Function op_SBC() As uint8_t
Declare Function op_SEC() As uint8_t: Declare Function op_SED() As uint8_t
Declare Function op_SEI() As uint8_t: Declare Function op_STA() As uint8_t
Declare Function op_STX() As uint8_t: Declare Function op_STY() As uint8_t
Declare Function op_TAX() As uint8_t: Declare Function op_TAY() As uint8_t
Declare Function op_TSX() As uint8_t: Declare Function op_TXA() As uint8_t
Declare Function op_TXS() As uint8_t: Declare Function op_TYA() As uint8_t
Declare Function op_XXX() As uint8_t

'NES address modes
Declare Function ad_IMP() As uint8_t:Declare Function ad_IMM() As uint8_t
Declare Function ad_ZPO() As uint8_t:Declare Function ad_ZPX() As uint8_t
Declare Function ad_ZPY() As uint8_t:Declare Function ad_REL() As uint8_t
Declare Function ad_ABS() As uint8_t:Declare Function ad_ABX() As uint8_t
Declare Function ad_ABY() As uint8_t:Declare Function ad_IND() As uint8_t
Declare Function ad_IZX() As uint8_t:Declare Function ad_IZY() As uint8_t
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


Function cpu_read(a As uint16_t) As uint8_t
	
	'Return bus_cpuRead(a,false)
	
	
End Function

Sub cpu_write(a As uint16_t,d As uint8_t )
	
	 'bus_cpuWrite(a,d)
	
End Sub
Sub SetFlag(f As FLAGS6502,v As BOOL) ' finished
	
	If (v) Then
		
		status Or= f
	Else
		status And= Not f
		
	EndIf
	
End Sub
Function Getflag(f As FLAGS6502) As uint8_t ' finished
	
	Return IIf(((status And f) > 0),1,0)
	
	
	
End Function



Function ad_IMP() As uint8_t
	
	
	 Print "IMP";
	'print "NES IMP ADDR FUNC"
	Return 	0
	
End Function
Function ad_IMM() As uint8_t
	
	
	
    Print "IMM";
	'print "NES IMM ADDR FUNC"
	Return 	0
	
End Function
Function ad_ZPO() As uint8_t
	
	
	Print "ZPO";
	'print "NES ZPO ADDR FUNC"
	Return 	0
	
End Function
Function ad_ZPX() As uint8_t
	
	
	
   Print "ZPX";
	'print "NES ZPX ADDR FUNC"
	Return 	0
	
End Function
Function ad_ZPY() As uint8_t
	
	
	 Print "ZPY";
	'print "NES ZPY ADDR FUNC"
	Return 	0
	
End Function
Function ad_REL() As uint8_t
	
	
	
   Print "REL";
	'print "NES REL ADDR FUNC"
	Return 	0
	
End Function
Function ad_IND() As uint8_t
	
	
	
    Print "IND";
	'print "NES IND ADDR FUNC"
	Return 	0
	
End Function
Function ad_IZX() As uint8_t
	
	Print "IZX";
	'print "NES IZX ADDR FUNC"
	Return 	0
	
End Function
Function ad_IZY() As uint8_t
	
	Print "IZY";
	'print "NES IZY ADDR FUNC"
	Return 	0
	
End Function
Function ad_ABS() As uint8_t
	
	
	
   Print "ABS";
	'print "NES ABS ADDR FUNC"
	Return 	0
	
End Function
Function ad_ABX() As uint8_t	
	
    Print "ABX";
	'print "NES ABX ADDR FUNC"
	Return 	0

End Function
Function ad_ABY() As uint8_t
	Print "ABY";
	'print "NES ABY ADDR FUNC"
	Return 	0
End Function


Function complete() As BOOL ' finished
	
	Return complete = 0
	 
	
End Function


Function fetch() ' finished
	
	If not(lookup(opcode).addrmode = @ad_IMP) Then
		fetched = cpu_read(addr_abs)
		
	EndIf
	Return fetched 
End Function

Sub clock() ' finished
	If (cycles = 0) Then
		
		opcode = cpu_read(pc)
		
		SetFlag(U,true)
		
		pc += 1
		
		cycles = lookup(opcode).cycles
		
		Dim additional_cycle1 As uint8_t = lookup(opcode).addrmode() 
		Dim additional_cycle2 As uint8_t = lookup(opcode).operate() 
		
		SetFlag(U, true)
		
		cycles += (additional_cycle1 & additional_cycle2)
		
	EndIf
	
	clock_count +=1
	cycles -= 1 
	
	
End Sub

Sub Reset_CPU() ' finished
	addr_abs = &HFFFC
	uint16_t lo = cpu_read(addr_abs + 0)
	uint16_t hi = cpu_read(addr_abs + 1)


	pc = (hi shl 8) Or lo


	a = 0
	x = 0
	y = 0
	stkp = &HFD
	status = &H00 Or U

	
	addr_rel = &H0000
	addr_abs = &H0000
	fetched = &H00


	cycles = 8
	
	
	
End Sub




Function op_ADC() As uint8_t
	
	    
		'print "NES ADC OP FUNC"
		Return 0
	
End Function
Function op_ASL() As uint8_t
	
	
		'print "NES ASL OP FUNC"
		Return 0
End Function
Function op_AND() As uint8_t
	
	
		'print "NES AND OP FUNC"
		Return 0
		
End Function
Function op_BCC() As uint8_t

	
	
	'print "NES BCC OP FUNC"
	Return 	0
	
End Function

Function op_BCS() As uint8_t
	


	'print "NES BCS OP FUNC"
	Return 	0
	
End Function
Function op_BEQ() As uint8_t
	
	
	'print "NES BEQ OP FUNC"
	Return 	0
	
End Function
Function op_BIT() As uint8_t
	
	
	'print "NES BIT OP FUNC"
	
Return 0
End Function
Function op_BMI() As uint8_t
	
	
	'print "NES BMI OP FUNC"
		Return 0
End Function

Function op_BNE() As uint8_t
	
	
	'print "NES BNE OP FUNC"
	Return 	0
	
End Function
Function op_BPL() As uint8_t
	
		'print "NES BPL OP FUNC"
		Return 0
End Function
Function op_BRK() As uint8_t

	Print"BRK:";
	'print "NES BRK OP FUNC"
Return 0
	
End Function
Function op_BVC() As uint8_t
	
		'print "NES BVC OP FUNC"
		Return 0
	
End Function

Function op_BVS() As uint8_t
	
		'print "NES BVS OP FUNC"
		Return 0
	
End Function

Function op_CLC() As uint8_t' finished
	
	
	SetFlag(C,FALSE)
	Return 	0
	
End Function
Function op_CLD() As uint8_t' finished
	
	SetFlag(D,FALSE)
	Return 	0
	
End Function
Function op_CLI() As uint8_t' finished
	
	SetFlag(I,FALSE)
	Return 	0
	
End Function
Function op_CLV() As uint8_t' finished
	
	SetFlag(V,FALSE)
	
	Return 	0
	
End Function

Function op_CMP() As uint8_t
	
	
	'print "NES CMP OP FUNC"
	Return 	0
	
End Function
Function op_CPX() As uint8_t
	
	
	'print "NES CPX OP FUNC"
	Return 	0
	
End Function
Function op_CPY() As uint8_t
	
	
	'print "NES CPY OP FUNC"
	Return 	0
	
End Function

Function op_DEC() As uint8_t
	
	
	'print "NES DEC OP FUNC"
	Return 	0
	
End Function
Function op_DEX() As uint8_t
	
	
	'print "NES DEX OP FUNC"
	Return 	0
	
End Function
Function op_DEY() As uint8_t

	'print "NES DEY OP FUNC"
	Return 	0
	
End Function
Function op_EOR() As uint8_t
	
	'print "NES RTI OP FUNC"
		Return 0
End Function

Function op_INC() As uint8_t
	

	'print "NES INC OP FUNC"
	Return 	0
	
End Function
Function op_INX() As uint8_t
	
	'print "NES SEI OP FUNC"
	Return 	0
	
End Function
Function op_INY() As uint8_t
	
	'print "NES SEI OP FUNC"
	Return 	0
	
End Function
Function op_JMP() As uint8_t
	
		'print "NES JMP OP FUNC"
		Return 0
	
End Function

Function op_JSR() As uint8_t
	
		'print "NES JSR OP FUNC"
		Return 0
End Function
Function op_LDA() As uint8_t
	
	
	'print "NES LDA OP FUNC"
	Return 	0
	
End Function
Function op_LDX() As uint8_t
	
	
	'print "NES LDX OP FUNC"
	Return 	0
	
End Function
Function op_LDY() As uint8_t
	
	
	'print "NES LDY OP FUNC"
	Return 	0
	
End Function

Function op_LSR() As uint8_t
	
	'print "NES LSR OP FUNC"
		Return 0
End Function
Function op_NOP() As uint8_t
	
		'print "NES NOP OP FUNC"
		Return 0
End Function
Function op_ORA() As uint8_t
	Print"ORA:";
		'print "NES ORA OP FUNC"
		Return 0
End Function
Function op_PHA() As uint8_t
	
	
	'print "NES PHA OP FUNC"
	Return 	0
	
End Function

Function op_PHP() As uint8_t
	
	
	'print "NES PHP OP FUNC"
	Return 	0
	
End Function
Function op_PLA() As uint8_t
	
	
	'print "NES PLA OP FUNC"
	Return 	0
	
End Function
Function op_PLP() As uint8_t
	
	
	
	
	'print "NES PLP OP FUNC"
	Return 	0
	
End Function
Function op_ROL() As uint8_t
	'print "NES ROL OP FUNC"
		Return 0
End Function

Function op_SEC() As uint8_t ' finished
	
	SetFlag(C,TRUE)
	Return 	0
	
End Function
Function op_SED() As uint8_t ' finished
	
	
	SetFlag(D,TRUE)
	Return 	0
	
End Function
Function op_SEI() As uint8_t ' finished
	
	SetFlag(I,TRUE)
	Return 0
	
End Function

Function op_STA() As uint8_t
	
	'print "NES STA OP FUNC"
	Return 	0
	
End Function

Function op_ROR() As uint8_t
	
	
		'print "NES ROR OP FUNC"
		Return 0
	
End Function
Function op_RTI() As uint8_t
	
	'print "NES RTI OP FUNC"
		Return 0
End Function
Function op_RTS() As uint8_t
	
		'print "NES RTS OP FUNC"
		Return 0
	
End Function
Function op_SBC() As uint8_t
	
	
	'print "NES SBC OP FUNC"
	Return 	0
	
End Function

Function op_STX() As uint8_t
	
	
	'print "NES STX OP FUNC"
	Return 	0
	
End Function
Function op_STY() As uint8_t
	
	Print "STY"
	'print "NES STY OP FUNC"
	Return 	0
	
End Function
Function op_TAX() As uint8_t
	
	
	Print "TAX"
	'print "NES TAX OP FUNC"
	Return 	0
	
End Function
Function op_TAY() As uint8_t
	
	
	Print "TAY"
	'print "NES TAY OP FUNC"
	Return 	0
	
End Function

Function op_TSX() As uint8_t
	Print "TSX"
	'print "NES TSX OP FUNC"
	Return 	0
	
End Function
Function op_TXA() As uint8_t
	
	Print "TXA"
	'print "NES TXA OP FUNC"
	Return 	0
	
End Function

Function op_TXS() As uint8_t ' finished
	
	stkp = x
	Return 	0
	
End Function
Function op_TYA() As uint8_t ' finished
	
	SetFlag(Z,a = &H00)
	SetFlag(N,a = &H80)
	Return 	0
	
End Function
Function op_XXX() As uint8_t 'finished

	Return 	0
	
End Function






