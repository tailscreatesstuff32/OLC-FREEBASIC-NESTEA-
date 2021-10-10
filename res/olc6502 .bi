'#Include Once "crt/mem.bi"
#Include once "crt.bi"
#Include Once "crt/stdint.bi"
#Include "containers/vector.bi"

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




Dim As INSTRUCTION lookup(0 To 256-1) => _ ' recheck
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


Sleep

Function ad_IMP() As uint8_t
	
	
	
	Print "NES IMP ADDR FUNC"
	Return 	0
	
End Function
Function ad_IMM() As uint8_t
	
	
	

	Print "NES IMM ADDR FUNC"
	Return 	0
	
End Function
Function ad_ZPO() As uint8_t
	
	
	
	Print "NES ZPO ADDR FUNC"
	Return 	0
	
End Function
Function ad_ZPX() As uint8_t
	
	
	

	Print "NES ZPX ADDR FUNC"
	Return 	0
	
End Function
Function ad_ZPY() As uint8_t
	
	
	
	Print "NES ZPY ADDR FUNC"
	Return 	0
	
End Function
Function ad_REL() As uint8_t
	
	
	

	Print "NES REL ADDR FUNC"
	Return 	0
	
End Function
Function ad_IND() As uint8_t
	
	
	

	Print "NES IND ADDR FUNC"
	Return 	0
	
End Function
Function ad_IZX() As uint8_t
	
	
	Print "NES IZX ADDR FUNC"
	Return 	0
	
End Function
Function ad_IZY() As uint8_t
	
	
	Print "NES IZY ADDR FUNC"
	Return 	0
	
End Function
Function ad_ABS() As uint8_t
	
	
	

	Print "NES ABS ADDR FUNC"
	Return 	0
	
End Function
Function ad_ABX() As uint8_t	
	

	Print "NES ABX ADDR FUNC"
	Return 	0

End Function
Function ad_ABY() As uint8_t
	
	Print "NES ABY ADDR FUNC"
	Return 	0
End Function


Function op_ADC() As uint8_t
		Print "NES ADC OP FUNC"
		Return 0
	
End Function
Function op_ASL() As uint8_t
		Print "NES ASL OP FUNC"
		Return 0
End Function
Function op_AND() As uint8_t
	
	
		Print "NES AND OP FUNC"
		Return 0
		
End Function
Function op_BCC() As uint8_t

	
	
	Print "NES BCC OP FUNC"
	Return 	0
	
End Function

Function op_BCS() As uint8_t
	
	

	Print "NES BCS OP FUNC"
	Return 	0
	
End Function
Function op_BEQ() As uint8_t
	
	
	Print "NES BEQ OP FUNC"
	Return 	0
	
End Function
Function op_BIT() As uint8_t
	
	Print "NES BIT OP FUNC"
	
Return 0
End Function
Function op_BMI() As uint8_t
	
	Print "NES BMI OP FUNC"
		Return 0
End Function

Function op_BNE() As uint8_t
	
	Print "NES BNE OP FUNC"
	Return 	0
	
End Function
Function op_BPL() As uint8_t
	
		Print "NES BPL OP FUNC"
		Return 0
End Function
Function op_BRK() As uint8_t

	
	Print "NES BRK OP FUNC"
Return 0
	
End Function
Function op_BVC() As uint8_t
	
		Print "NES BVC OP FUNC"
		Return 0
	
End Function

Function op_BVS() As uint8_t
	
		Print "NES BVS OP FUNC"
		Return 0
	
End Function
Function op_CLC() As uint8_t
	
	
	Print "NES CLC OP FUNC"
	Return 	0
	
End Function
Function op_CLD() As uint8_t
	
	Print "NES CLD OP FUNC"
	Return 	0
	
End Function
Function op_CLI() As uint8_t
	
	Print "NES CLI OP FUNC"
	Return 	0
	
End Function

Function op_CLV() As uint8_t
	
	Print "NES CLV OP FUNC"
	Return 	0
	
End Function
Function op_CMP() As uint8_t
	
	
	Print "NES CMP OP FUNC"
	Return 	0
	
End Function
Function op_CPX() As uint8_t
	
	
	Print "NES CPX OP FUNC"
	Return 	0
	
End Function
Function op_CPY() As uint8_t
	
	
	Print "NES CPY OP FUNC"
	Return 	0
	
End Function

Function op_DEC() As uint8_t
	
	
	Print "NES DEC OP FUNC"
	Return 	0
	
End Function
Function op_DEX() As uint8_t
	
	
	Print "NES DEX OP FUNC"
	Return 	0
	
End Function
Function op_DEY() As uint8_t

	Print "NES DEY OP FUNC"
	Return 	0
	
End Function
Function op_EOR() As uint8_t
	
	Print "NES RTI OP FUNC"
		Return 0
End Function

Function op_INC() As uint8_t
	

	Print "NES INC OP FUNC"
	Return 	0
	
End Function
Function op_INX() As uint8_t
	
	Print "NES SEI OP FUNC"
	Return 	0
	
End Function
Function op_INY() As uint8_t
	
	Print "NES SEI OP FUNC"
	Return 	0
	
End Function
Function op_JMP() As uint8_t
	
		Print "NES JMP OP FUNC"
		Return 0
	
End Function

Function op_JSR() As uint8_t
	
		Print "NES JSR OP FUNC"
		Return 0
End Function
Function op_LDA() As uint8_t
	
	
	Print "NES LDA OP FUNC"
	Return 	0
	
End Function
Function op_LDX() As uint8_t
	
	
	Print "NES LDX OP FUNC"
	Return 	0
	
End Function
Function op_LDY() As uint8_t
	
	
	Print "NES LDY OP FUNC"
	Return 	0
	
End Function

Function op_LSR() As uint8_t
	
	Print "NES LSR OP FUNC"
		Return 0
End Function
Function op_NOP() As uint8_t
	
		Print "NES NOP OP FUNC"
		Return 0
End Function
Function op_ORA() As uint8_t
		Print "NES ORA OP FUNC"
		Return 0
End Function
Function op_PHA() As uint8_t
	
	
	Print "NES PHA OP FUNC"
	Return 	0
	
End Function

Function op_PHP() As uint8_t
	
	
	Print "NES PHP OP FUNC"
	Return 	0
	
End Function
Function op_PLA() As uint8_t
	
	
	Print "NES PLA OP FUNC"
	Return 	0
	
End Function
Function op_PLP() As uint8_t
	
	
	
	
	Print "NES PLP OP FUNC"
	Return 	0
	
End Function
Function op_ROL() As uint8_t
	Print "NES ROL OP FUNC"
		Return 0
End Function

Function op_SEC() As uint8_t
	
	Print "NES SEC OP FUNC"
	Return 	0
	
End Function
Function op_SED() As uint8_t
	
	Print "NES SED OP FUNC"
	Return 	0
	
End Function
Function op_SEI() As uint8_t
	
	Print "NES SEI OP FUNC"
	Return 	0
	
End Function
Function op_STA() As uint8_t
	
	Print "NES STA OP FUNC"
	Return 	0
	
End Function

Function op_ROR() As uint8_t
	
	
		Print "NES ROR OP FUNC"
		Return 0
	
End Function
Function op_RTI() As uint8_t
	
	Print "NES RTI OP FUNC"
		Return 0
End Function
Function op_RTS() As uint8_t
	
		Print "NES RTS OP FUNC"
		Return 0
	
End Function
Function op_SBC() As uint8_t
	
	
	Print "NES SBC OP FUNC"
	Return 	0
	
End Function

Function op_STX() As uint8_t
	
	
	Print "NES STX OP FUNC"
	Return 	0
	
End Function
Function op_STY() As uint8_t
	
	
	Print "NES STY OP FUNC"
	Return 	0
	
End Function
Function op_TAX() As uint8_t
	
	
	
	Print "NES TAX OP FUNC"
	Return 	0
	
End Function
Function op_TAY() As uint8_t
	
	
	
	Print "NES TAY OP FUNC"
	Return 	0
	
End Function

Function op_TSX() As uint8_t
	
	Print "NES TSX OP FUNC"
	Return 	0
	
End Function
Function op_TXA() As uint8_t
	
	
	Print "NES TXA OP FUNC"
	Return 	0
	
End Function
Function op_TXS() As uint8_t
	
	
	Print "NES TXS OP FUNC"
	Return 	0
	
End Function
Function op_TYA() As uint8_t
	
	Print "NES TYA OP FUNC"
	Return 	0
	
End Function

Function op_XXX() As uint8_t
	
	Print "NES XXX OP FUNC"
	Return 	0
	
End Function






