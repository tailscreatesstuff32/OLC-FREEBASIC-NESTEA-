
#Include Once  "nes/NES.bi"


'#Include "crt.bi"

Dim Shared keys(255) As Boolean 

Declare Sub drawString(x1 As Integer,y1 As Integer  ,str1 As String,col1 As Integer )


Declare Sub main()

Declare Sub DrawCode_real2(x1 As Integer, y1 As Integer, nlines As Integer)

Declare Sub InOrder2 (pRoot As MAPNODEUINT16TSTRING Ptr) 


'	MMapTemplate(UInteger , String)
Type STCK As Any ptr

	MStackTemplate(Long)
	Dim Shared sp As TSTACKLONG
' Dim Shared list1 As TLISTSTRING


SetConsoleTitle("OLC-NES-NESTEA-FB")

WIDTH 90, 40 
setflag(U,TRUE)
Dim shared pressed_R As boolean = FALSE
Dim shared pressed_space As boolean = FALSE
Dim shared pressed_N As boolean = FALSE
Dim shared pressed_I As boolean = FALSE
Dim shared pressed_C As boolean = FALSE
Dim shared pressed_F As boolean = FALSE


Dim Shared mapAsm As TMAPUINT16TSTRING

	'MMapTemplate(UINT16T , String)
	
	
		
FUNCTION keypress_space (k AS long) As boolean


'pressed = (GetAsyncKeyState(k) = 0)
 	
 	'could of used multikey() too...
If (GetAsyncKeyState(vk_space) < 0 and pressed_space = false ) Then
    
        pressed_space  = true 
        Return true
EndIf
    if (GetAsyncKeyState(vk_space) = 0 And pressed_space = TRUE)   then
     
        pressed_space  = false
            Return FALSE 
    EndIf


END Function

FUNCTION keypress_R (k AS long) As boolean


'pressed = (GetAsyncKeyState(k) = 0)
 	
If (GetAsyncKeyState(vk_R) < 0 and pressed_R = false ) Then
    
        pressed_R  = true 
        Return true
EndIf
    if (GetAsyncKeyState(vk_R) = 0 And pressed_R = TRUE)   then
     
       pressed_R   = false
            Return FALSE 
    EndIf


END Function

		
		
		
'' set the color to light grey text on a blue background
Color , 1

'' clear the screen to the background color
Cls


Sub InOrder2 (pRoot As MAPNODEUINT16TSTRING Ptr) 
	Dim nlines As Integer 
	'Dim mapvec1ptr As  any Ptr
	' mapvec1ptr = @mapvec1[0]->ndata
 
		'Locate 8, 58
		'	If pRoot <> 0 Then
			'	inOrder2(pRoot->pRight)
			'	
			'	
			'
			'			Locate , 58
			'			Print *pRoot->nData
			'			
			'	
			'	
		   '   InOrder2(pRoot->pleft)
			'	
			'EndIf   
			'
	'	Dim str1 As  String = mapasm.find(0)
			
	'proot = mapasm.proot
				'Locate 21, 58
			 	'	Print *proot->ndata
			 	
			 	
		'	 Dim nLineY As Integer = (nLines Shr 1) * 1  + y1
			
			 'While (proot <> 0 Or stck1.size() <> 0 ) And nlines <> 27
			 	
			 	
			 
			 	
			 	'If proot <> 0 Then
			 	'	'sp.push(Cast(Long,pRoot))	
			 	'stck1.insert(0,Cast(Long,pRoot))
			 	'pRoot = pRoot->pleft
		      
			 	'Else
			 		'proot = stck1.front()
			 		
			 		
			 		
			 		'stck1.front 
			 		'stck1.erase(0)
			 		'sp.pop()
			 		
			 		While nlines <> 27
		
				
			'21
				'If nlines = 0 Then Color 11 Else: Color 255
				
				'If mapvec1 <> NULL Then
					'mapvec1[0]+=1
				'	mapvec1ptr+=1 
				
						'drawString(58, 8+nlines,mapvec1[1],255)
			'	Else
				'	drawString(58, 8+nlines,Str(mapvec1.at(nlines)),255)
				'End If
				
			     nlines+=1
			 		
			 	'	pRoot = pRoot->pright
			 		
			 	'EndIf
			Wend	
			 
			' Delete(mapvec1)
			' proot = mapasm.proot
'drawString(58, 21,mapasm.find(0),11)
End sub









SUB Drawram (x1 AS INTEGER, y1 AS INTEGER, addr AS uint16_t, nRows AS INTEGER, nColumns AS INTEGER)
	
	
	Color 255
	
    Dim ncpuramX As Integer = x1
     Dim ncpuramY As Integer = y1
     Dim sOffset As String
     Dim row As Integer
     Dim col As Integer 
    ' Dim addr As uint16_t

    FOR row = 0 TO nRows - 1
        sOffset = "$" + hex1(addr, 4) + ":"
        FOR col = 0 TO nColumns - 1


            sOffset = sOffset  + " " + hex1(bus_read(addr, TRUE),2)

            addr+=1

        NEXT col

        LOCATE ncpuramY, ncpuramX


        PRINT sOffset
        ncpuramY = ncpuramY + 1
    NEXT row

END SUB
Sub printorder (pRoot As MAPNODEUINT16TSTRING Ptr)  
	'Locate 7, 58
			If pRoot <> 0 Then
				
				'printorder(pRoot->pLeft)
				
			'	#if ctkey = String
					
					'#if ctdata = String
						
						'Print *(pRoot->nKey) ; "     " ; *(pRoot->nData)
						
					'#else
						
						'Print *(pRoot->nKey) ; "     " ; pRoot->nData
						
					'#endif
					
				'#else
					
					'#if ctdata = String
				   
				     ' proot = proot->pLeft
				     
				      '  proot = proot->pLeft
				     ' proot = proot->pLeft
				      proot = proot->pleft
				      proot = proot->pRight
				        proot = proot->pleft
				         proot = proot->pRight
						Locate , 58
						Print  *(pRoot->nData)
						   '  proot = proot->pRight
						      '   proot = proot->pleft
						    ' Print  *(pRoot->nData)
					'#else   
						
					'	Print pRoot->nKey ; "     " ; pRoot->nData
						
					'#endif      
					
				'#endif
				
				'printorder(pRoot->pRight)      
				
			End if   
			
			
			
			
			
			
			
			
			
			
			
			
			
			
	'		Dim  it_a As MAPNODEUINT16TSTRING  Ptr 
	''temporary to see if fits
	'
	''
	''For i As integer = 0 To  nlines-1
	''	
	''	Locate y1+i,x1
	''
	''	If i = 14-1 Then
	''		Color 11,1
	''	Else
	''		Color 255,1
	''	EndIf
	''	
	''		Print "$0000: BRK #$00 {IMM}"
	''Next
	'
	'
	''Color,2
	'
   ' it_a = mapasm.proot

	'	
	'
	' dim nLineY As Integer = (nLines Shr 1) * 1  + y1
	' 	
	' 	If it_a <> 0 Then
	' 	Color 11,1
	' 	Locate nLineY ,x1
	' 'it_a = it_a->pleft
	' 	Print *(it_a->nData)
	' 	'it_a = it_a->pleft
	' 	'it_a = it_a->pleft
	' 	While (nliney < (nlines *1) +y1)
	' 		
	' 		nLineY += 1
	' 		
	' 		 	
	' 		'it_a = it_a->pRight
	' 		
	' 		If it_a <>0 Then
	' 		'	 it_a = it_a->pLeft
	' 		'it_a = it_a->pRight
	' 		 
	' 		 
	' 		Color 255,1
	' 		Locate nLineY,x1
	' 		'inorder(mapasm.proot)
	' 	  Print *(it_a->nData)
	''it_a = it_a->pRight
	' 		End If
	' 	  
	' 	Wend
	' 	End If
	'
	
	 'mapasm.find(pc)
	 
 'nLineY = (nLines Shr 1) * 1  + y1
	 '
	 'while (nLineY > y1)
			
			'nLineY -= 1
			
			
	 '		Color 255,1
	 '		Locate nLineY,x1  
	 '	   Print *(it_a->nData)
	 '	   
	 '	   
	 'Wend
			
End Sub

			

SUB DrawCpu (x1 AS INTEGER, y1 AS INTEGER)


    LOCATE y1, x1

Color 255
'11
    Print " status: ";
    Color(IIf(cpu_cpustatus And N,10,12))
    PRINT " N ";
      Color(IIf(cpu_cpustatus And V,10,12))
    PRINT " V ";
       Color(IIf(cpu_cpustatus And U,10,12))
    PRINT " - ";
      Color(IIf(cpu_cpustatus And B,10,12))
    PRINT " B ";
      Color(IIf(cpu_cpustatus And D,10,12))
    PRINT " D ";
    Color(IIf(cpu_cpustatus And I,10,12))
    PRINT " I ";
      Color(IIf(cpu_cpustatus And Z,10,12))
    PRINT " Z ";
        Color(IIf(cpu_cpustatus And C,10,12))
    PRINT " C "
    Color 255


    'Locate , x

   ' PRINT " ";getflag(N);" ";getflag(V);" ";getflag(U);" "; getflag(B);" ";getflag(D);" ";getflag(I);" "; getflag(Z);" ";getflag(C)
  
 '   Locate , x
   
    Locate y1+1, x1
    PRINT " PC: $"; LTRIM$(hex1(pc, 4))
    Locate , x1
    PRINT " A: $"; LTRIM$(hex1(a, 2)) + " [" + LTRIM$(STR$(a)) + "]"
    Locate , x1
    PRINT " X: $"; LTRIM$(hex1(x, 2)) + " [" + LTRIM$(STR$(x)) + "]"
    Locate , x1
    PRINT " Y: $"; LTRIM$(hex1(y, 2)) + " [" + LTRIM$(STR$(y)) + "]"
    LOCATE , x1
    PRINT " Stack P: $"; LTRIM$(hex1(stkp, 4))




END Sub
Sub DrawCode(x1 As Integer, y1 As Integer, nlines As Integer)
'
'	
'	Dim  it_a As MAPNODEUINT16TSTRING Ptr 
'	'temporary to see if fits
'	
'	'
'	'For i As integer = 0 To  nlines-1
'	'	
'	'	Locate y1+i,x1
'	'
'	'	If i = 14-1 Then
'	'		Color 11,1
'	'	Else
'	'		Color 255,1
'	'	EndIf
'	'	
'	'		Print "$0000: BRK #$00 {IMM}"
'	'Next
'	
'	
'	'Color,2
'	
'    it_a = mapasm.proot
'
'		
'	
'	 dim nLineY As Integer = (nLines Shr 1) * 1  + y1
'	 	
'	 '	If it_a <> 0 Then
'	 '	Color 11,1
'	 '	Locate nLineY ,x1
'	 'it_a = it_a->pleft
'	'Print *(it_a->nData)
'	 	'it_a = it_a->pleft
'	 	'it_a = it_a->pleft
'	' 	While (nliney < (nlines *1) +y1)
'	' 		
'	' 		nLineY += 1
'	' 		
'	' 		 	
'	' 		'it_a = it_a->pRight
'	' 		
'	' 		If it_a <>0 Then
'	' 		'	 it_a = it_a->pLeft
'	' 		'it_a = it_a->pRight
'	' 		 
'	' 		 
'	' 		Color 255,1
'	' 		Locate nLineY,x1
'	' 		
'	' 	  'Print *(it_a->nData)
'	''it_a = it_a->pRight
'	' 		End If
'	' 	  
'	' 	Wend
'	' 	End If
'	'
'	
'	 'mapasm.find(pc)
'	 
' 'nLineY = (nLines Shr 1) * 1  + y1
'	 '
'	 'while (nLineY > y1)
'			
'			'nLineY -= 1
'			
'			
'	 '		Color 255,1
'	 '		Locate nLineY,x1  
'	 '	   Print *(it_a->nData)
'	 '	   
'	 '	   
'	 'Wend
'			
'	 
'	
'	
End Sub

Sub DrawCode_real(x1 As Integer, y1 As Integer, nlines As Integer) 'work in progress figuring out MAPS

	'it_a = mapAsm.find(pc) ' find key position
	'dim nLineY As Integer = (nLines shr 1) * 10 + y 
	'	if (it_a <> mapAsm.end())Then
	'	
	'		DrawString(x, nLineY, (*it_a).second, olc::CYAN) 'gets first iteration for cursor
	'		while (nLineY < (nLines * 10) + y)
	'		 
	'			nLineY += 10
	'			it_a+=1  'increment iterator
	'			if (it_a <> mapAsm.end()) Then
	'		 
	'				DrawString(x, nLineY, (*it_a).second) 'draws bottom half of code
	'			End If
	'		Wend
	'	End If

	'	it_a = mapAsm.find(nes.cpu.pc) ' find key position
	'	nLineY = (nLines Shr 1) * 10 + y 
	'	if (it_a <> mapAsm.end()) Then 
	'	 
	'		while (nLineY > y)
	'		 
	'			nLineY -= 10
	'				it_a-=1			''decrement iterator
	'			if (it_a <> mapAsm.end()) Then
	'			
	'				DrawString(x, nLineY, (*it_a).second)'draws top half of code
	'			End If
	'		Wend
	'	End if
	
	
End Sub










Sub DrawCode_real2(x1 As Integer, y1 As Integer, nlines As Integer) 'work in progress figuring out MAPS
	

	'it_a = mapAsm.find(pc) ' find key position
	dim nLineY As Integer = (nLines shr 1) * 1 + y1  
	'	'if (it_a <> mapAsm.end())Then
	'	
	 		DrawString(x1, nLineY, "GOODBYE",  11) 'gets first iteration for cursor
	 		while (nLineY < (nLines * 1) + y1)
	 		 
	 			nLineY += 1
	'			'it_a+=1  'increment iterator
	'		'	if (it_a <> mapAsm.end()) Then
	'		 
	 				DrawString(x1, nLineY, "HELLO",255) 'draws bottom half of code
	'			'End If
	 		Wend
	'	End If

		'it_a = mapAsm.find(nes.cpu.pc) ' find key position
		nLineY = (nLines Shr 1) * 1 + y1 
		'if (it_a <> mapAsm.end()) Then 
		 
			while (nLineY > y1)
			 
				nLineY -= 1
				'	it_a-=1			''decrement iterator
			'	if (it_a <> mapAsm.end()) Then
				
					DrawString(x1, nLineY, "HELLO",255)'draws top half of code
				'End If
			Wend
		'End if
	
	
End Sub






main


Locate ,,0
Dim keys1 As String
'Locate 8, 58
'InOrder2(mapasm.proot) ' work in progress need to figure out MAPS in freebasic...

Do

'pressed = FALSE
If keypress_space(vk_space) = TRUE   Then
      

	  '// If the low-order bit is 1, the key is toggled
     ' if ((GetasyncKeyState(VK_SPACE)and &H0001) = 0)Then
        	Do
	
		
		clock_cpu()
		
	Loop While Not(complete())
    '  End If

	End If

	 If keypress_R(vk_R) = TRUE Then


	cpu_reset()
	  End If

	

	
	
	
	
	
	  DrawCpu 57, 1  
     DrawCode_real2(58,8,26)
     drawString ( 3,37,"SPACE = Step Instruction    R = RESET    I = IRQ    N = NMI",255) 
       
	  Drawram 2, 1, &H0000, 16, 16
     Drawram 2, 19, &H8000, 16, 16
       
   ' Locate 37, 3
 ' print  "SPACE = Step Instruction    R = RESET    I = IRQ    N = NMI" 
  
'Locate 8, 58
	
	
	
	
Loop Until InKey() = "q"

Sleep
Sub main()
	
	

	 
	' drawcode(58,7,26)
	

'   
   	insert_cartridge("donkey.nes")
	If ImageValid = TRUE Then
	Print "CRC: " & "???? WORK IN PROGRESS"
	Print "loaded rom!"
	Print "MAGIC NUM: "; header.Name1
	Print "ROM BANKS: " & Str(header.prg_rom_chunks)
	Print "PRG ROM SIZE: " & Str(vPRGMemory.size())
	Print "CHRROM BANKS: " & str(header.chr_rom_chunks)
	Print "CHRROM SIZE:" & Str(vCHRMemory.size())
	Print "MAPPER: " & Str(nmapperid)
	Print "PRGRAM SIZE: " & Str(header.prg_ram_size)
	Print header.tv_system1
	Print header.tv_system2
	Print header.unused
	Print 
	Print  "press Any key To continue" 
	
	Sleep
	cls
	else
	Print "rom not found or isnt valid"
	Print 
	Print  "press Any key To Quit"
		Sleep
		End
	EndIf
	'
	'



'Dim noffset As uint16_t = &H0
'Dim ss As String = "A20A8E0000A2038E0100AC0000A900186D010088D0FA8D0200EAEAEA"
'
'Dim code2 As String = "A2 0A 8E 00 00 A2 03 8E 01 00 AC 00 00 A9 00 18 6D 01 00 88 D0 FA 8D 02 00 EA EA EA"
'Dim i As Integer =1

'While noffset < UBound(cpuram)-1'  And noffset <=&HFFFF
'
'cpuram(noffset ) =  Val("&H" + mid(code1, i,2))
'i+=1
'noffset+=1
'Wend
    'FOR x1 As Integer = 0 TO LEN(ss) - 1
    '  cpuram(nOffset) = Val("&H" + MID$(ss, (2 * x1 + 1), 2))
    '    nOffset = nOffset + 1
    'Next x1
    '

	   

cpuram(&HFFFC) = &H00
cpuram(&HFFFD) = &H80
mapAsm = disassemble(&H0000, &HFFFF) '&H35
bus_reset()



End Sub

Sub drawString(x1 As Integer,y1 As Integer  ,str1 As String,col1 As Integer )
	
	
	Color col1
	Locate y1,x1
	Print str1
	'Color 255
	
	
End Sub


