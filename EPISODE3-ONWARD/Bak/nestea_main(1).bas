

#Include Once  "nes/NES.bi"
#include "fbgfx.bi"

Dim Shared hIcon As HICON 
Dim Shared hInstance As HMODULE


 'tempscrn2= ImageCreate(128*2,128*2,RGB(0,0,0),32)

Using fb

ScreenRes 780,480,32,2
'#Include "crt.bi"

'Dim Shared nesscrn As Any Ptr

Dim shared id As uint8_t  
nesscrn = ImageCreate(512,480,RGB(0,0,0),32)
pattables(0)=ImageCreate(128,128,RGB(0,0,0),32)
pattables(1)=ImageCreate(128,128,RGB(0,0,0),32)

Dim Shared  tempscrn2 As Any Ptr
'tempscrn2= ImageCreate(128*2,128*2,RGB(0,0,0),32)
 

Dim Shared keys(255) As Boolean 

Declare Sub drawString(x1 As Integer,y1 As Integer  ,str1 As String,col1 As Integer )
Dim Shared bEmulationRun As bool = FALSE

Declare Sub main()

Declare Sub DrawCode_real2(x1 As Integer, y1 As Integer, nlines As Integer)

Declare Sub InOrder2 (pRoot As MAPNODEUINT16TSTRING Ptr) 
dim shared p as uint8_t
Dim Shared nSelectedPalette As uint8_t
'	MMapTemplate(UInteger , String)
Type STCK As Any ptr

	MStackTemplate(Long)
	Dim Shared sp As TSTACKLONG
' Dim Shared list1 As TLISTSTRING


SetConsoleTitle("OLC-NES-NESTEA-FB")
WindowTitle("OLC-NES-NESTEA-FB")






WIDTH 90, 40 
setflag(U,TRUE)
Dim shared pressed_R As boolean = FALSE
Dim shared pressed_space As boolean = FALSE
Dim shared pressed_N As boolean = FALSE
Dim shared pressed_I As boolean = FALSE
Dim shared pressed_C As boolean = FALSE
Dim shared pressed_F As boolean = FALSE

Dim shared pressed_P As boolean = FALSE

Dim Shared mapAsm As TMAPUINT16TSTRING

	'MMapTemplate(UINT16T , String)
	
	
	


FUNCTION keypress_P (k AS long) As boolean


'pressed = (GetAsyncKeyState(k) = 0)
 	
If (GetAsyncKeyState(vk_P) < 0 and pressed_P = false ) Then
    
        pressed_P  = true 
        Return true
EndIf
    if (GetAsyncKeyState(vk_P) = 0 And pressed_P = TRUE)   then
     
       pressed_P   = false
            Return FALSE 
    EndIf


END Function	
	
		
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

Sub Drawram (x1 AS INTEGER, y1 AS INTEGER, addr AS uint16_t, nRows AS INTEGER, nColumns AS INTEGER)
	
	
	'Color 255
	Color RGB(255,255,255)
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

END Sub
DIM SHARED ff As Integer
DIM SHARED fps As Integer
DIM SHARED start As Single

SUB frames_per_sec
    ff  = ff + 1


    IF TIMER - start  >= 1 THEN fps = ff : ff  = 0: start  = TIMER
   WindowTitle "OLC-NES-NESTEA-FB:"+" FPS:" + STR (fps )


END Sub
Sub drawRect(x1 As uint32_t,y1 As uint32_t,w As uint32_t,h As uint32_t)
	
	'Dim As HBRUSH whitecol = CreatePen(PS_SOLID,1,BGRA(255,255,255,0))
   'Dim As HGDIOBJ oldobj = SelectObject(hdc,whitecol)
	Line(x1,y1)-(x1 + w,y1)
	Line(x1 + w,y1)-( x1 + w, y1 + h)
	line(x1 + w,y1 + h)-(x1, y1 + h)
	line(x1, y1 + h)-(x1, y1)
	

	
	
	'MoveToEx(hdc,x,y,NULL):LineTo(hdc,x + w,y)
	'
	'MoveToEx(hdc,x + w,y, NULL):LineTo(hdc, x + w, y + h)
	'
	'MoveToEx(hdc,x + w,y + h,NULL):LineTo(hdc,x, y + h)
	'
	'MoveToEx(hdc,x, y + h,NULL):LineTo(hdc, x, y)
	'
	''SelectObject(hdc,oldobj)
	'DeleteObject(whitecol)
	'
	
End Sub
Sub DrawCpu (x1 AS INTEGER, y1 AS INTEGER)

Draw String (x1, y1), "STATUS:" 
Draw String (x1, y1+ 10), "PC: $"+hex1(pc, 4)
Draw String (x1, y1+ 20), "A: $"+hex1(a, 2) + "  [" + Str(a) + "]"
Draw String (x1, y1+ 30), "X: $"+hex1(x, 2) + "  [" + Str(x) + "]"
Draw String (x1, y1+ 40), "Y: $"+hex1(y, 2) + "  [" + Str(y) + "]"
Draw String (x1, y1+ 50), "Stack P: $"+ hex(stkp, 4)

Draw String (x1+64, y1),"N",IIf(cpu_cpustatus And N,RGB(0,255,0),RGB(255,0,0))
Draw String (x1+80, y1),"V",IIf(cpu_cpustatus And V,RGB(0,255,0),RGB(255,0,0))
Draw String (x1+96, y1),"-",IIf(cpu_cpustatus And U,RGB(0,255,0),RGB(255,0,0))
Draw String (x1+112,y1),"B",IIf(cpu_cpustatus And B,RGB(0,255,0),RGB(255,0,0))
Draw String (x1+128,y1),"D",IIf(cpu_cpustatus And D,RGB(0,255,0),RGB(255,0,0))
Draw String (x1+144,y1),"I",IIf(cpu_cpustatus And I,RGB(0,255,0),RGB(255,0,0))
Draw String (x1+160,y1),"Z",IIf(cpu_cpustatus And Z,RGB(0,255,0),RGB(255,0,0))
Draw String (x1+178,y1),"C",IIf(cpu_cpustatus And C,RGB(0,255,0),RGB(255,0,0))
    'LOCATE y1, x1



'Color RGB(255,255,255)
'11
    'Print " status: ";
    'Color(IIf(cpu_cpustatus And N,10,12))
    'PRINT " N ";
    '  Color(IIf(cpu_cpustatus And V,10,12))
    'PRINT " V ";
    '   Color(IIf(cpu_cpustatus And U,10,12))
    'PRINT " - ";
    '  Color(IIf(cpu_cpustatus And B,10,12))
    'PRINT " B ";
    '  Color(IIf(cpu_cpustatus And D,10,12))
    'PRINT " D ";
    'Color(IIf(cpu_cpustatus And I,10,12))
    'PRINT " I ";
    '  Color(IIf(cpu_cpustatus And Z,10,12))
    'PRINT " Z ";
    '    Color(IIf(cpu_cpustatus And C,10,12))
    'PRINT " C "
    
   '     Print " status: ";
   ' Color(IIf(cpu_cpustatus And N,RGB(0,255,0),RGB(255,0,0)))
   ''PRINT " N ";
   '  Color(IIf(cpu_cpustatus And V,RGB(0,255,0),RGB(255,0,0)))
   ' 'PRINT " V ";
   '  Color(IIf(cpu_cpustatus And U,RGB(0,255,0),RGB(255,0,0)))
   ' 'PRINT " - ";
   ' Color(IIf(cpu_cpustatus And B,RGB(0,255,0),RGB(255,0,0)))
   ' 'PRINT " B ";
   ' Color(IIf(cpu_cpustatus And D,RGB(0,255,0),RGB(255,0,0)))
   ' 'PRINT " D ";
   ' Color(IIf(cpu_cpustatus And I,RGB(0,255,0),RGB(255,0,0)))
   ' 'PRINT " I ";
   '  Color(IIf(cpu_cpustatus And Z,RGB(0,255,0),RGB(255,0,0)))
   ' 'PRINT " Z ";
   ' Color(IIf(cpu_cpustatus And C,RGB(0,255,0),RGB(255,0,0)))
   '' PRINT " C "
    
    
    
   ' Color 255
'Color RGB(255,255,255)

    'Locate , x

   ' PRINT " ";getflag(N);" ";getflag(V);" ";getflag(U);" "; getflag(B);" ";getflag(D);" ";getflag(I);" "; getflag(Z);" ";getflag(C)
  
 '   Locate , x
   
    'Locate y1+1, x1
    'PRINT " PC: $"; LTRIM$(hex1(pc, 4))
    'Locate , x1
    'PRINT " A: $"; LTRIM$(hex1(a, 2)) + " [" + LTRIM$(STR$(a)) + "]"
    'Locate , x1
    'PRINT " X: $"; LTRIM$(hex1(x, 2)) + " [" + LTRIM$(STR$(x)) + "]"
    'Locate , x1
    'PRINT " Y: $"; LTRIM$(hex1(y, 2)) + " [" + LTRIM$(STR$(y)) + "]"
    'LOCATE , x1
    'PRINT " Stack P: $"; LTRIM$(hex1(stkp, 4))




END Sub


Sub DrawCode_real2(x1 As Integer, y1 As Integer, nlines As Integer) 'work in progress figuring out MAPS
	
'
'	'it_a = mapAsm.find(pc) ' find key position
'	dim nLineY As Integer = (nLines shr 1) * 1 + y1  
'	'	'if (it_a <> mapAsm.end())Then
'	'	
'	 		'DrawString(x1, nLineY, "GOODBYE",  11) 'gets first iteration for cursor
'	 		DrawString(x1, nLineY, "GOODBYE",RGB(0,255,255))
'	 		while (nLineY < (nLines * 1) + y1)
'	 		 
'	 			nLineY += 1
'	'			'it_a+=1  'increment iterator
'	'		'	if (it_a <> mapAsm.end()) Then
'	'		 
'	 				'DrawString(x1, nLineY, "HELLO",255) 'draws bottom half of code
'	 					DrawString(x1, nLineY, "HELLO",RGB(255,255,255))
'	'			'End If
'	 		Wend
'	'	End If
'
'		'it_a = mapAsm.find(nes.cpu.pc) ' find key position
'		nLineY = (nLines Shr 1) * 1 + y1 
'		'if (it_a <> mapAsm.end()) Then 
'		 
'			while (nLineY > y1)
'			 
'				nLineY -= 1
'				'	it_a-=1			''decrement iterator
'			'	if (it_a <> mapAsm.end()) Then
'				
'				'	DrawString(x1, nLineY, "HELLO",255)'draws top half of code
'				DrawString(x1, nLineY, "HELLO",RGB(255,255,255))
'				
'				'End If
'			Wend
'		'End if
'	
'	


	'it_a = mapAsm.find(pc) ' find key position
	dim nLineY As Integer = (nLines shr 1) * 10 + y1  
	'	'if (it_a <> mapAsm.end())Then
	'	
	 		'DrawString(x1, nLineY, "GOODBYE",  11) 'gets first iteration for cursor
	 		Draw String (x1, nLineY), "GOODBYE" ,RGB(0,255,255) 
	 		while (nLineY < (nLines * 10) + y1)
	 		 
	 			nLineY += 10
	'			'it_a+=1  'increment iterator
	'		'	if (it_a <> mapAsm.end()) Then
	'		 
	 				'DrawString(x1, nLineY, "HELLO",255) 'draws bottom half of code
	 				Draw String (x1, nLineY), "HELLO" 
	'			'End If
	 		Wend
	'	End If

		'it_a = mapAsm.find(nes.cpu.pc) ' find key position
		nLineY = (nLines Shr 1) * 10 + y1 
		'if (it_a <> mapAsm.end()) Then 
		 
			while (nLineY > y1)
			 
				nLineY -= 10
				'	it_a-=1			''decrement iterator
			'	if (it_a <> mapAsm.end()) Then
				
				'	DrawString(x1, nLineY, "HELLO",255)'draws top half of code
				Draw String (x1, nLineY), "HELLO" 
				
				'End If
			Wend
		'End if
	
	
End Sub



Sub renderer2
	
	'ImageDestroy tempscrn1
'ImageDestroy tempscrn2

 'DrawCode_real2(58,8,26)

		'line(516 + nSelectedPalette * (nSwatchSize * 5) - 1, 339)- step(nSwatchSize * 4 , nSwatchSize),,B		
				'	drawRect(


	 'tempscrn2 = GetPatternTable2(1,nSelectedPalette,2)
	 
	 
	 'For y1 As  uint8_t = 0 To 30-1
	 '	 For x1 As uint8_t = 0 To 32-1
	 	 	
	 	 	
	 'Draw String(x1 * 16,y1 *16),hex1(Cast(uint32_t,tblname(0,y1*32+x1)),2) 
	 
	' id = tblname(0,y1*32+x1)
	 '	Put (x1*16, y1*16), tempscrn2,(((id And &H0F) Shl 3)*2,((id shr 4) Shl 3)*2)-Step(16-1,16-1), PSet
	 	 		 	 	
	 
	 	
	 	' Next
	 	
	 	
	 	
	 'Next
  	'Put (0, 0), tempscrn2, PSet

    
    ' drawString ( 3,37,"SPACE = Step Instruction    R = RESET    I = IRQ    N = NMI",255) 
        '  drawString ( 3,37,"SPACE = Step Instruction    R = RESET    I = IRQ    N = NMI",RGB(255,255,255)) 
	 ' Drawram 2, 1, &H0000, 16, 16
     'Drawram 2, 19, &H8000, 16, 16
     
     '   Drawram 2, 1+20+18, &H0000, 16, 10
     'Drawram 40, 19+20, &H8000, 16, 10
     '
     'GetPatternTable(0,0,450,100)
     ' GetPatternTable(1,0,588,100)
     ' 
           
    ' GetPatternTable(0,0,450+55,305)
     ' GetPatternTable(1,0,588+55,305)
     
     'GetPatternTable(0,nSelectedPalette,516,348)
    ' GetPatternTable(1,nSelectedPalette,648,348)
     
End Sub




main




Do
frames_per_sec
		controller(0) = &H00 
		controller(0) Or= IIf(multikey(SC_X), &H80, &H00)    ' // A Button
		controller(0) Or= IIf(multikey(SC_Z), &H40, &H00)     ' // B Button
		controller(0) Or= IIf(multikey(SC_A), &H20, &H00)     ' // Select
		controller(0) Or= IIf(multikey(SC_S), &H10, &H00)     ' // Start
		controller(0) Or= Iif(multikey(SC_UP), &H08, &H00) 
		controller(0) Or= Iif(multikey(SC_DOWN), &H04, &H00) 
		controller(0) Or= Iif(multikey(SC_LEFT),&H02,&H00) 
		controller(0) Or= Iif(multikey(SC_RIGHT),&H01, &H00) 
		
     If keypress_space(vk_space) = TRUE   Then
         bEmulationRun = Not(bEmulationRun)

    '
	  End If



	 If keypress_R(vk_R) = TRUE Then
	 bus_reset()
	 End If

		 
     	 'Cls
        	 	 If bEmulationRun Then
 		 		Do:  bus_clock():  Loop while not(frame_complete) 
				frame_complete = false 
 		 	
 		 	
 		 	
 		 Else
 		 	
 		 	
        	 	 EndIf
        	 	 
        	 	 if keypress_P(0) Then 
        	 	 	nSelectedPalette+=1 
        	 	 	nSelectedPalette And= &H07
        	 	 EndIf


	
	'ScreenLock
		Cls
		 DrawCpu 516, 2  
   
    
    DrawCode_real2(516,72,26)
   
		const  nSwatchSize As Integer = 6
		for p As Integer = 0 to 8 - 1' // For each palette
			For s As Integer=  0 To 4-1  '// For each index
				Line(516 + p * (nSwatchSize * 5) + s * nSwatchSize, 340)- Step(nSwatchSize-1, nSwatchSize-1), GetColourFromPaletteRam(p, s),bf 
					Next
					 next
					
	
					drawRect(516 + nSelectedPalette * (nSwatchSize * 5) - 1, 339, (nSwatchSize * 4), nSwatchSize)
	   
	 ' DrawCpu 57, 1
	 
	Put (516, 348), GetPatternTable(0,nSelectedPalette), PSet
	 Put (648, 348), GetPatternTable(1,nSelectedPalette), PSet

    Put (0, 0), nesscrn, PSet
     
     	 ScreenCopy( 0, 1)
     
     	 
      'ScreenUnLock
'setconsoletitle(Str(vram_addr.reg))
Loop Until InKey() = "q"




Sleep







ImageDestroy nesscrn
ImageDestroy pattables(0)
ImageDestroy pattables(1)





Sub main()
	
	'' set the color to light grey text on a blue background
'Color , 1
Color , RGB(0,0,128)

ScreenSet( 0,1)
'' clear the screen to the background color
Cls

'Dim Hwnd As HWND
'Screencontrol(GET_WINDOW_HANDLE , Cast(Integer, Hwnd))
'
'hInstance=GetModuleHandle(NULL)	 
'hIcon= LoadIcon(hInstance,MAKEINTRESOURCE(100)) ' our first Win API loading icon to our created window
'SendMessage(Handle,WM_SETICON,NULL,Cast(LPARAM,hIcon)) ' our second WIN API sets the loaded icon to our dialog 

'SetWindowLongA(Handle, GWL_EXSTYLE, WS_EX_APPWINDOW OR WS_EX_WINDOWEDGE)
   'insert_cartridge("California Games (U).nes")
  ' insert_cartridge("donkey.nes")
   ' insert_cartridge("Burger Time (U) [!].nes")
insert_cartridge("Ren & Stimpy Show, The (U).nes")
   'insert_cartridge("roms/soccer.nes")
    'insert_cartridge("roms/tennis.nes")
     'insert_cartridge("roms/kung fu.nes")
     ' insert_cartridge("pac-man.nes")
    'insert_cartridge("digdug.nes")
     '  insert_cartridge("roms/mario bros.nes")
      ' insert_cartridge("ice climber.nes")
	If ImageValid = TRUE Then
	Print "CRC: " & "???? WORK IN PROGRESS"
	Print "loaded rom!"
	Print "MAGIC NUM: "; header.Name1
	Print "MIRROR MODE: "; IIf(cart_mirror = horizontal,"HORIZONTAL","VERTICAL")
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
	 ScreenCopy( 0, 1)
	Sleep
	cls
	else
	Print "rom not found or isnt valid"
	Print 
	Print  "press Any key To Quit"
	ScreenCopy( 0, 1)
		Sleep
		End
	EndIf

'turn cursor off
Locate ,,0

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

