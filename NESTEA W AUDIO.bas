#Include Once"windows.bi"


Dim shared As HWND hwnd 
''using fbsound for audio for now WIP
'#include "../inc/fbsound.bi"

'the nes system starts here//////////
'#Include Once  "nes/old/NES.bi" ' WITHOUT AUDIO

'WITH AUDIO///////////////////////
#Include Once  "nes/NES.bi" 
#include "NES\bus.bas"
#include "NES\Audio.bas"
'/////////////////////////////////


'///////////////////////////////////

dim shared nes as bus

dim shared audio_hndler as AudioHandler ptr 
	

dim shared elapsedMS as float
dim shared as uint64_t end1 
dim shared as uint64_t start1

'
'pattables(0)=ImageCreate(128,128,RGB(0,0,0),32)
'pattables(1)=ImageCreate(128,128,RGB(0,0,0),32)
#Define GET_X_LPARAM(LPARAM) LoWord(LPARAM)
#Define GET_Y_LPARAM(LPARAM) HiWord(LPARAM)

   
#Include Once"crt.bi"
#Include Once "win/commctrl.bi"
#Include Once  "fbgfx.bi"
#Include "file.bi"
#Include "filesaveopen.bas"

#include "resources.bi"


#define SCREEN_WIDTH  640								'	// We want a 800 pixel width resolution
#define SCREEN_HEIGHT 480								'	// We want a 600 pixel height resolution

#define KEYDOWN(vk_code) (IIf(GetAsyncKeyState(vk_code) And &H8000), 1, 0)
#define KEYUP(vk_code)   (IIf(GetAsyncKeyState(vk_code) And &H8000),0, 1)

const DEF_CX = 256 
const  DEF_CY = 240

Using fb
Dim Shared fResidualTime As float
Dim Shared fElapsedTime As float
Dim Shared fElapsedstart As float

DIM SHARED ff As Integer
DIM SHARED fps As Integer
DIM SHARED start As Single
DIM SHARED bEmulationRun As bool = false
DIM SHARED bSingleFrame As bool = FALSE


Dim Shared isFpressed As bool = FALSE

Dim Shared ispressed As bool = FALSE

Dim Shared cart As String
Dim Shared pic1 As String
Dim Shared zoom_scrn As Integer = 2




Dim Shared nesrect As rect => (0,0,256*2 ,240*2) '*1.45

Dim Shared bFullScreen As bool = FALSE 								'	// Create a boolean and set it to false.  If we choose full screen, set it to TRUE

	 Dim Shared hmenu As HMENU  
	  Dim Shared hViewsub As HMENU
	  
	  Dim Shared hFilesub As HMENU
	  Dim Shared hFile_Recent_sub As HMENU

	  Dim Shared hView_Zoom_sub As HMENU
	  
	  Dim Shared my_pattables As uint32_t Ptr'(128*128) 
	  
	  
fElapsedstart = Timer

Declare Function ChangeToFullScreen(width1 As Integer ,height1 As  Integer ) As BOOL


declare sub audioloop()


audio_hndler = new AudioHandler(48000,AUDIO_S16SYS,2048,16)
audio_hndler->start_aud()

 dim shared thread1 as any ptr 
 
'thread1 =  threadcreate(@audioloop)

dim shared quit as boolean = false


'declare Sub Process2 Cdecl (ByVal userdata As Any Ptr, ByVal audbuffer As Ubyte Ptr, ByVal audlen As Integer)





'Sub Process2 Cdecl (ByVal userdata As Any Ptr, ByVal audbuffer As Ubyte Ptr, ByVal audlen As Integer)
''
''while not(nes._clock) 
''	
''Wend
'
'
'	'
'	'	    if(that->inputReadPos + 2048 > that->inputBufferPos) then
'   '   'we overran the buffer
'   '  'log("Audio buffer overran");
'   '   that->inputReadPos = that->inputBufferPos - 2048 
'	'    end if
'	'    
'   ' if(that->inputReadPos + 4096 < that->inputBufferPos) then
'   '   '// we underran the buffer
'   '   '//log("Audio buffer underran");
'   '  that->inputReadPos += 2048
'   ' end if
'   ' 
'	'For n as integer = 0 to (audlen/2)-1  
'   ''that->x+=.010
'	'
'	'	dim sample as Sint16 ptr = cast(Sint16 ptr,audbuffer) 
'	'	
'	'	
'	'	'that->sample = sin((that->x*4))*0.20'(rnd*2-1)*0.15
'	'	
'	'	that->intSample = cast(Sint32, that->inputbuffer(that->inputReadPos and &HFFF) * that->maxAmplitude)
'	'	
'	'	sample[n]  = cast(Sint16,that->intSample)
'	'	
'	'	
'	'
'	'	that->inputReadPos+=1
'
'	'Next n
'	
'End Sub


Sub fill_pal_rect(hdc As hdc,x1 As Integer,y1 As Integer,pal1 As uint8_t ,pix As uint8_t ) 
	
		'fillrect(hdc,@Type<rect>(x1,y1,x1+6,y1+6),createsolidbrush(Cast(COLORREF,GetColourFromPaletteRamBGR(pal1,pix))))
	Dim br As hbrush = createsolidbrush(GetColourFromPaletteRamBGR(pal1,pix))
	fillrect(hdc,@Type<rect>(x1,y1,x1+12,y1+12),br)
	
 DeleteObject(br)
	
	
End Sub

Function caculateLineBytes(width1 As uint32_t)  As uint32_t
    '//********* Four-byte Alignment**********
    return Int((24 * width1 + 31)/32) *4
    '//********* Four-byte Alignment**********
End Function



Sub SaveNESScrn(path As String,width1 as integer,height1 as integer,scrndata As uint32_t Ptr = NULL)
	
Dim bmpheader As BITMAPFILEHEADER
Dim bmpinfoheader As BITMAPINFOHEADER

bmpheader.bftype = &H4D42
bmpheader.bfReserved1 = 0
bmpheader.bfReserved1 = 0
bmpheader.bfOffBits = &H36

bmpinfoheader.bisize = SizeOf(bmpinfoheader) 
bmpinfoheader.biwidth = width1
bmpinfoheader.biheight = -height1
bmpinfoheader.biXPelsPerMeter = 5000 
bmpinfoheader.biYPelsPerMeter = 5000 
bmpinfoheader.biClrUsed = 0 
bmpinfoheader.biClrImportant = 0 
bmpinfoheader.biPlanes = 1
bmpinfoheader.biBitCount = 24

Dim linebytes As uint32_t = caculateLineBytes(bmpinfoheader.biwidth)
bmpheader.bfsize = SizeOf(BitmapFileHeader) + sizeof(BitmapInfoHeader) + lineBytes * Abs(bmpinfoheader.biheight)



bmpinfoheader.biSizeImage =Int((bmpinfoheader.biwidth  * bmpinfoheader.biBitCount/8)) * Abs(bmpinfoheader.biheight)

Dim savebmpfile As Integer = FreeFile
	Open path For Binary As savebmpfile
	
			
		Put # savebmpfile, ,*Cast(UByte Ptr,@bmpheader),SizeOf(BITMAPFILEHEADER) 
		Put # savebmpfile, ,*Cast(UByte Ptr,@bmpinfoheader),SizeOf(BITMAPINFOHEADER)
		
		
		
Dim pixels As uint8_t Ptr
pixels = New uint8_t[bmpinfoheader.biSizeImage]
'
'Print scrndata[0]
'Print scrndata[1]
'Print scrndata[2]
'Print scrndata[3]
Dim pos1 As uint32_t = 0
For i As uint32_t = 0 To bmpinfoheader.biSizeImage-1 Step 3 
	

	pixels[i+2] = RGBA_R(scrndata[pos1]) ' red

	pixels[i+1] = RGBA_G(scrndata[pos1])'green

	pixels[i+0] = RGBA_B(scrndata[pos1])'blue
pos1+=1
Next








Put # savebmpfile,,pixels[0],bmpinfoheader.biSizeImage







Close savebmpfile


Delete pixels
pixels = NULL


Dim bmpinfo As BITMAPINFOHEADER 
	
	
End Sub


sub init_rom
Dim bmp As String

Dim s As String




Print "choose a rom"

While cart = ""
	
	
	cart = file_opensave(NULL)
	If cart = "" Then 
	Print "invalid rom file"
	print " "

	input "select another? y/n: ",s
	If s  = "Y" Or s  = "y" Then
   Print "choose a rom"
	ElseIf s  = "N" Or s  = "n" Then	
	print " "
	
	Print  "press Any key To Quit"
	
	if hwnd <> null then

	SendMessage(hwnd, WM_CLOSE, 0, 0)
	else
	Sleep
	End
	End If
	end if
	end if
Wend

'Print bmp
'	bmp = file_opensave(NULL)
	'Print bmp
	
	

setconsoletitle("OLC-NES-NESTEA-FB-V0.1")
'Print cart
'Input   "enter rom path or drag file here: ", cart
insert_cartridge(cart,hwnd)
setconsoletitle("OLC-NES-NESTEA-FB-V0.1")
Input   "fullscreen Y/N?: ", s 
Cls



If s  = "Y" Or s  = "y" Then
	
	bfullscreen = TRUE
	'FreeConsole()

ElseIf s  = "N" Or s  = "n" Then

bfullscreen = FALSE
EndIf

bEmulationRun = true

		If ImageValid = TRUE Then
	Print "CRC: " &  crcfromfile(cart)
	Print "loaded rom!"
	Print "MAGIC NUM: "; header.Name1
	Print "MIRROR MODE: "; IIf(cart_mirror = horizontal,"HORIZONTAL","VERTICAL")
	Print "BATTERY BACKED: "; IIf(bat_saves,"YES","NO")
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
	
	if bat_saves then
		
	print "battery loaded"
	print	
		
	EndIf
	
 	else
	Print "rom not found or isnt valid"
	Print 
	Print  "press Any key To Quit"
	
		Sleep
		End
		EndIf

'insert_cartridge("roms/kung fu.nes")
'insert_cartridge("donkey.nes")
'insert_cartridge("Burger Time (U) [!].nes")
'insert_cartridge("Felix the Cat (U).nes")
'insert_cartridge("Felix the Cat (U).nes")
'insert_cartridge("roms/Kirby's Adventure (USA).nes")
'insert_cartridge("roms/Volleyball.nes")
'insert_cartridge("roms/SMB3.nes")
'insert_cartridge("roms/SMB2.nes")
'insert_cartridge("Ren & Stimpy Show, The (U).nes")
'insert_cartridge("California Games (U).nes")
'insert_cartridge("roms/SMB.nes")
'insert_cartridge("ice climber.nes")
'insert_cartridge("roms/Megaman1.nes")

'init_rom

'mapAsm = disassemble(&H0000, &HFFFF) '&H35






nes._reset(true)







'nes.SetSampleFrequency(44100)

'audio_hndler = new AudioHandler(44100,AUDIO_S16SYS,4096,16,@Process2)
	
	
audio_hndler->start_aud()


End Sub


Function KEYPRESSED(vk_code As Integer) As bool
	static Iskeyup(&HFF) As bool
			
	If  IIf(GetAsyncKeyState(vk_code) And &H8000, 1, 0) And iskeyup(vk_code) = TRUE Then
		iskeyup(vk_code) = IIf(GetAsyncKeyState(vk_code) And &H8000, FALSE, TRUE)
	Return TRUE
	 
	ElseIf IIf(GetAsyncKeyState(vk_code) And &H8000, 0, 1) And iskeyup(vk_code)= FALSE Then
		iskeyup(vk_code) = IIf(GetAsyncKeyState(vk_code) And &H8000, FALSE, true)		
		Return FALSE
	End If
	
	Return FALSE			
End Function

SUB frames_per_sec(hwnd As hwnd)
    ff  = ff + 1


    IF TIMER - start  >= 1 THEN fps = ff : ff  = 0: start  = TIMER
 SetConsoleTitle "OLC-NES-NESTEA-FB:"+" FPS:" + STR (fps )
 Dim titles1 As String
 titles1="OLC-NES-NESTEA-FB-V0.1:"&" FPS:" & STR (fps )
setwindowtext(hwnd,titles1 )

END Sub

Sub DrawNesScrn(dc As hDC,ImageData() As uint32_t,x1 As int16_t,y1 As int16_t, destwdth As int16_t, desthght As  int16_t)

 'Dim BI As BITMAP
 Dim BI As BITMAPINFO
 
 With BI.bmiHeader
        .biWidth = 256
        .biHeight = -240
        .biSize  = 40
        .biBitCount  = 32
        .biPlanes = 1
 End With
 

'StretchDIBits dc, 0, 0, destwdth, desthght, 0, 0, 341, 261 ,@ImageData(0, 0, 0), @bi,0,SRCCOPY
StretchDIBits dc, x1, y1, destwdth, desthght, 0, 0, 256, 240 ,@ImageData(0), @bi,0,SRCCOPY


End Sub

Sub DrawPatTable(dc As hDC,ImageData As uint32_t Ptr,x1 As int16_t,y1 As int16_t, destwdth As int16_t, desthght As  int16_t)

 'Dim BI As BITMAP
 Dim BI As BITMAPINFO
 
 With BI.bmiHeader
        .biWidth = 128
        .biHeight = -128
        .biSize  = 40
        .biBitCount  = 32
        .biPlanes = 1
 End With
 

'StretchDIBits dc, 0, 0, destwdth, desthght, 0, 0, 341, 261 ,@ImageData(0, 0, 0), @bi,0,SRCCOPY
StretchDIBits dc, x1, y1, destwdth, desthght, 0, 0, 128, 128 ,ImageData[0], @bi,0,SRCCOPY


End Sub


Sub win_menu(hwnd As HWND)
	

	  
			hmenu = CreateMenu()
			hFilesub = CreatePopupMenu()
			hViewsub = CreatePopupMenu()
			
			hFile_Recent_sub = CreatePopupMenu()
			hView_Zoom_sub = CreatePopupMenu()
			
			AppendMenu(hmenu,MF_POPUP,hFilesub,"&file")
				AppendMenu(hFilesub,MF_ENABLED,IDM_FILE_LOADROM,"&load rom")
				
					AppendMenu(hFilesub,MF_ENABLED,IDM_FILE_RESET,"&Reset NES")
					AppendMenu(hFilesub,MF_ENABLED,IDM_FILE_HARDRESET,"&Hard Reset NES")
					  AppendMenu(hFilesub,MF_UNCHECKED,IDM_FILE_PAUSE,"&Pause NES")
					  	AppendMenu(hFilesub,MF_ENABLED,IDM_FILE_SINGLEFRAME,"&Single Frame")
							AppendMenu(hFilesub,MF_ENABLED,IDM_FILE_CYCLE,"&Cycle")
				AppendMenu(hFilesub,MF_SEPARATOR,0,"-")
				AppendMenu(hFilesub,MF_POPUP Or MF_GRAYED,hFile_Recent_sub,"&Recent roms") ' WIP
							AppendMenu(hFile_Recent_sub,MF_GRAYED,IDM_FILE_RECENT_EMPTY,"Empty")
									AppendMenu(hFile_Recent_sub,MF_GRAYED,IDM_FILE_RECENT_EMPTY,"Empty")
											AppendMenu(hFile_Recent_sub,MF_GRAYED,IDM_FILE_RECENT_EMPTY,"Empty")
													AppendMenu(hFile_Recent_sub,MF_GRAYED,IDM_FILE_RECENT_EMPTY,"Empty")
															AppendMenu(hFile_Recent_sub,MF_GRAYED,IDM_FILE_RECENT_EMPTY,"Empty")
															
				AppendMenu(hFilesub,MF_SEPARATOR,IDM_SEPARATOR,"-")			
							
				AppendMenu(hFilesub,MF_ENABLED,IDM_FILE_SAVEPIC ,"&Save NES Screen")
				AppendMenu(hFilesub,MF_ENABLED,IDM_FILE_PATTABLE01 ,"&Save Pat table left")
				AppendMenu(hFilesub,MF_ENABLED,IDM_FILE_PATTABLE02 ,"&Save Pat table right")
				  'AppendMenu(hFilesub,MF_ENABLED,IDM_FILE_SAVEPIC ,"&Save Frames")
				
													
			AppendMenu(hFilesub,MF_SEPARATOR,IDM_SEPARATOR,"-")
			AppendMenu(hFilesub,MF_POPUP,IDM_FILE_EXIT,"&Exit")
							
							
			AppendMenu(hmenu,MF_POPUP,hViewsub,"&View")				
							AppendMenu(hViewsub,MF_POPUP,hView_Zoom_sub,"&Zoom")
									AppendMenu(hView_Zoom_sub,MF_UNCHECKED,103,"1x")
									AppendMenu(hView_Zoom_sub,MF_CHECKED,104,"2x")
									AppendMenu(hView_Zoom_sub,MF_UNCHECKED,105,"3x")
									AppendMenu(hView_Zoom_sub,MF_UNCHECKED,106,"4x")				
									AppendMenu(hView_Zoom_sub,MF_UNCHECKED Or MF_GRAYED,107,"&Fullscreen")
				SetMenu(hwnd,hmenu)			
							
End Sub

Sub nesmain(hwnd As HWND,rct As rect)
	
'fElapsedTime = Timer-fElapsedstart
' fElapsedstart = timer
'
'
'		controller(0) = &H00 
'		controller(0) Or= IIf((GetAsyncKeyState(vk_X) And &H8000), &H80, &H00)    ' // A Button
'		controller(0) Or= IIf((GetAsyncKeyState(vk_Z) And &H8000), &H40, &H00)     ' // B Button
'		controller(0) Or= IIf((GetAsyncKeyState(vk_A) And &H8000), &H20, &H00)     ' // Select
'		controller(0) Or= IIf((GetAsyncKeyState(vk_S) And &H8000), &H10, &H00)     ' // Start
'		controller(0) Or= Iif((GetAsyncKeyState(vk_UP) And &H8000), &H08, &H00) 
'		controller(0) Or= Iif((GetAsyncKeyState(vk_DOWN) And &H8000), &H04, &H00) 
'		controller(0) Or= Iif((GetAsyncKeyState(vk_LEFT) And &H8000),&H02,&H00) 
'		controller(0) Or= Iif((GetAsyncKeyState(vk_RIGHT) And &H8000),&H01, &H00)
'		
'
'          'print pmapper->GetIrqReloadVal()
'				if (bEmulationRun) Then
'					
'					
'					
'				  
'					if (fResidualTime > 0.0f) Then
'						fResidualTime -= fElapsedTime
'					Else
'						fResidualTime += (1.0f / 60.0f) - fElapsedTime
'
'					Do:  bus_clock2:  Loop while Not(frame_complete) 
'					frame_complete = FALSE
'					frames_per_sec(hwnd)		
'					End If
'					
'					Else
'						
'					If bsingleframe = TRUE Then
'
'				Do:  bus_clock2:  Loop while iif(frame_complete,0,1)
'				Do:  bus_clock2:  Loop while iif(complete(),0,1)
'				frame_complete = FALSE
'				bSingleFrame = FALSE
'				
'					end If
'
'			If  KEYPRESSED(vk_f)  Then
'				Do:  bus_clock2:  Loop while iif(frame_complete,0,1)
'				Do:  bus_clock2:  Loop while iif(complete(),0,1)
'				frame_complete = FALSE
'			End if	
'				
'		If  KEYPRESSED(vk_space)  Then
'				Do:  bus_clock2:  Loop while IIf(complete(),0,1)
'				Do:  bus_clock2:  Loop while iif(complete(),1,0)
'		End if	
'				EndIf
'				
'		If  KEYPRESSED(vk_r)  Then
'			
'			'if pmapper <> null then
'			''pmapper->setbattery ' load batter data
'			'end if
'			'
'
'			
'			 bus_reset(false)
'		End If
'				If  KEYPRESSED(vk_h)  Then
'			
'			'if pmapper <> null then
'			''pmapper->setbattery ' load batter data
'			'end if
'			'
'
'			
'			 bus_reset(true)
'				End If
'				
'				
'				
'		
'			'	If  KEYPRESSED(vk_b)  Then ' save battery data
'			'
'			'
'			'
'			''if pmapper <> null then
'			''		dim sram_save as integer = FreeFile
'			''
'			''open "kirbyadventure.batt" for binary as sram_save
'			''' 
'			'' put # sram_save,,*cast(uint8_t ptr,pmapper->getbattery()),32*1024
'			'''
'			''close sram_save
'			''end if
'			''
'
'			'
'			' pmapper->getbattery
'			' 
'			'	End If
'		'
'		'If  KEYPRESSED(vk_n)  Then ' load battery data
'		'	
'		'	
'		'	
'	
'		'	pmapper->setbattery
'		'	end if
'		'	
'
'		'			
'						
'						
'      If  KEYPRESSED(vk_e)  Then
'			 bEmulationRun = Not(bEmulationRun)
'				checkmenuitem(hFilesub,IDM_FILE_PAUSE ,IIf(bEmulationRun,MF_UNCHECKED,MF_CHECKED))
'      End If
'		
'	
'		
'		
'	
'
'	
'	'fillrect(dc,@Type<rect>(640-64,0,640,480),Cast(HBRUSH,GetStockObject(WHITE_BRUSH)))
'		
'	'fillrect(dc,@Type<rect>(640-128,0,640,480),Cast(HBRUSH,GetStockObject(WHITE_BRUSH)))
'
'
'	'fill_the_rect(dc,384,0,0,1)
'	'fill_the_rect(dc,384+128,0,0,2)
'	'
'	'my_pattables = GetPatternTable(0,0) 
'	'DrawPatTable(dc,@my_pattables,384,480-128,128,128)
'	'my_pattables = GetPatternTable(1,0) 
'	'DrawPatTable(dc,@my_pattables,384+128,480-128,128,128)
'	
'	
'		Dim As hdc dc
'	dc = GetDC(hwnd)
''	
''	
''		Dim posx As integer
''	Dim posy As integer
''	Dim pal1 As uint8_t
''	pal1 = 0
''		For p As Integer = 0 To 4-1
''		
''		
''		For s As Integer = 0 To 4-1
''			
''		
''			
''			'fill_pal_rect(dc,(256+10)+p*(14*5),(480-128)+0*14,s,p)
''			fill_pal_rect(dc,266 + p * (12 * 4) + s * 12,(480-128)+0*14,p,s)
''fill_pal_rect(dc,266 + p * (12 * 4) + s * 12,(480-128)+1*14,p+4,s)
''
''
''		Next
''		Next
''	
''	my_pattables = GetPatternTable(0,0) 
''	DrawPatTable(dc,@my_pattables,0,480-128,128,128)
''	my_pattables = GetPatternTable(1,0) 
''	DrawPatTable(dc,@my_pattables,132,480-128,128,128)
''	
'	
'	
'	
'	
'	
'	'fill_the_rect(dc,512+16,0,0,1)
'	'fill_the_rect(dc,512+32,0,0,2)
'	'fill_the_rect(dc,512+48,0,0,3)
'	'
'	'fill_the_rect(dc,512+24,0,1,0)
'	'fill_the_rect(dc,512+30,0,1,1)
'	'fill_the_rect(dc,512+36,0,1,2)
'	'fill_the_rect(dc,512+42,0,1,3)
'	'
'	'fill_the_rect(dc,512+48,0,2,0)
'	'fill_the_rect(dc,512+54,0,2,1)
'	'fill_the_rect(dc,512+60,0,2,2)
'	'fill_the_rect(dc,512+66,0,2,3)
'	'
'	'
'	'fill_the_rect(dc,512+72,0,3,0)
'	'fill_the_rect(dc,512+78,0,3,1)
'	'fill_the_rect(dc,512+84,0,3,2)
'	'fill_the_rect(dc,512+90,0,3,3)
'	'
'	'
'	'
'	'fill_the_rect(dc,512,16,4,0)
'	'fill_the_rect(dc,512+16,16,4,1)
'	'fill_the_rect(dc,512+32,16,4,2)
'	'fill_the_rect(dc,512+48,16,4,3)
'	'
'	'fill_the_rect(dc,512+64,16,5,0)
'	'fill_the_rect(dc,512+70,16,5,1)
'	'fill_the_rect(dc,512+86,16,5,2)
'	'fill_the_rect(dc,512+102,16,5,3)
'	'
'	'fill_the_rect(dc,512+118,16,6,0)
'	'fill_the_rect(dc,512+134,16,6,1)
'	'fill_the_rect(dc,512+150,16,6,2)
'	'fill_the_rect(dc,512+166,16,6,3)
'	'
'	'
'	'fill_the_rect(dc,512+182,16,7,0)
'	'fill_the_rect(dc,512+198,16,7,1)
'	'fill_the_rect(dc,512+204,16,7,2)
'	'fill_the_rect(dc,512+220,16,7,3)
'	'
'	
'	'
'	'fillrect(dc,@Type<rect>((640-128)+posx,0+posy,((640-128)+6)+posx,6+posy),createsolidbrush(GetColourFromPaletteRamBGR(0,2)))
'		'fillrect(dc,@Type<rect>((640-128)+posx+6,0+posy,((640-128)+6)+posx+6,6+posy),createsolidbrush(GetColourFromPaletteRamBGR(0,1)))
'
'	'
'	'
'	'fillrect(dc,@Type<rect>(640-128,0,(640-128)+12,12),GetColourFromPaletteRam(0,0))
'	'
'	'fillrect(dc,@Type<rect>((640-128)+12,0,((640-128)+12)+12,12),GetColourFromPaletteRam(0,1))
'	'
'	'
'	'fillrect(dc,@Type<rect>(640-128,0,(640-128)+12,12),GetColourFromPaletteRam(0,0))
'	'fillrect(dc,@Type<rect>((640-128)+12,0,((640-128)+12)+12,12),GetColourFromPaletteRam(0,1))
'	'fillrect(dc,@Type<rect>(640-128,0,(640-128)+12,12),GetColourFromPaletteRam(0,0))
'	'fillrect(dc,@Type<rect>((640-128)+12,0,((640-128)+12)+12,12),GetColourFromPaletteRam(0,1))
'	'
'	'
'	'fillrect(dc,@Type<rect>(640-128,0,(640-128)+12,12),GetColourFromPaletteRam(0,0))
'	'fillrect(dc,@Type<rect>((640-128)+12,0,((640-128)+12)+12,12),GetColourFromPaletteRam(0,1))
'	'fillrect(dc,@Type<rect>(640-128,0,(640-128)+12,12),GetColourFromPaletteRam(0,0))
'	'fillrect(dc,@Type<rect>((640-128)+12,0,((640-128)+12)+12,12),GetColourFromPaletteRam(0,1))
'	'
'	'fillrect(dc,@Type<rect>(640-128,0,(640-128)+12,12),GetColourFromPaletteRam(0,0))
'	'fillrect(dc,@Type<rect>((640-128)+12,0,((640-128)+12)+12,12),GetColourFromPaletteRam(0,1))
'	'fillrect(dc,@Type<rect>(640-128,0,(640-128)+12,12),GetColourFromPaletteRam(0,0))
'	'fillrect(dc,@Type<rect>((640-128)+12,0,((640-128)+12)+12,12),GetColourFromPaletteRam(0,1))
'	'
'	'
'	'fillrect(dc,@Type<rect>(640-128,0,(640-128)+12,12),GetColourFromPaletteRam(0,0))
'	'fillrect(dc,@Type<rect>((640-128)+12,0,((640-128)+12)+12,12),GetColourFromPaletteRam(0,1))
'	'fillrect(dc,@Type<rect>(640-128,0,(640-128)+12,12),GetColourFromPaletteRam(0,0))
'	'fillrect(dc,@Type<rect>((640-128)+12,0,((640-128)+12)+12,12),GetColourFromPaletteRam(0,1))
'	'
'	'
'	'fillrect(dc,@Type<rect>(640-128,0,(640-128)+12,12),GetColourFromPaletteRam(0,0))
'	'fillrect(dc,@Type<rect>((640-128)+12,0,((640-128)+12)+12,12),GetColourFromPaletteRam(0,1))
'	'fillrect(dc,@Type<rect>(640-128,0,(640-128)+12,12),GetColourFromPaletteRam(0,0))
'	'fillrect(dc,@Type<rect>((640-128)+12,0,((640-128)+12)+12,12),GetColourFromPaletteRam(0,1))
'	'
'	'
'	'fillrect(dc,@Type<rect>(640-128,0,(640-128)+12,12),GetColourFromPaletteRam(0,0))
'	'fillrect(dc,@Type<rect>((640-128)+12,0,((640-128)+12)+12,12),GetColourFromPaletteRam(0,1))
'	'fillrect(dc,@Type<rect>(640-128,0,(640-128)+12,12),GetColourFromPaletteRam(0,0))
'	'fillrect(dc,@Type<rect>((640-128)+12,0,((640-128)+12)+12,12),GetColourFromPaletteRam(0,1))
'	'
'	'fillrect(dc,@Type<rect>(640-128,0,(640-128)+12,12),GetColourFromPaletteRam(0,0))
'	'fillrect(dc,@Type<rect>((640-128)+12,0,((640-128)+12)+12,12),GetColourFromPaletteRam(0,1))
'	'fillrect(dc,@Type<rect>(640-128,0,(640-128)+12,12),GetColourFromPaletteRam(0,0))
'	'fillrect(dc,@Type<rect>((640-128)+12,0,((640-128)+12)+12,12),GetColourFromPaletteRam(0,1))
'	
'	
'	if bfullscreen then
'DrawNesScrn(dc,mydata(),((0+128)-64),0,rct.right,rct.bottom)
'else
'DrawNesScrn(dc,mydata(),0,0,rct.right,rct.bottom)
'	end if
'
'
'
'
'
'
'
'	ReleaseDC(hwnd,dc)
'	
'
'	
	
	
End Sub

Sub nesmain_with_audio(hwnd As HWND,rct As rect)
fElapsedTime = Timer-fElapsedstart
 fElapsedstart = timer


     '	 start1 =  SDL_GetPerformanceCounter() 
	


		nes.controller(0) = &H00 
		nes.controller(0) Or= IIf((GetAsyncKeyState(vk_X) And &H8000), &H80, &H00)    ' // A Button
		nes.controller(0) Or= IIf((GetAsyncKeyState(vk_Z) And &H8000), &H40, &H00)     ' // B Button
		nes.controller(0) Or= IIf((GetAsyncKeyState(vk_A) And &H8000), &H20, &H00)     ' // Select
		nes.controller(0) Or= IIf((GetAsyncKeyState(vk_S) And &H8000), &H10, &H00)     ' // Start
		nes.controller(0) Or= Iif((GetAsyncKeyState(vk_UP) And &H8000), &H08, &H00) 
		nes.controller(0) Or= Iif((GetAsyncKeyState(vk_DOWN) And &H8000), &H04, &H00) 
		nes.controller(0) Or= Iif((GetAsyncKeyState(vk_LEFT) And &H8000),&H02,&H00) 
		nes.controller(0) Or= Iif((GetAsyncKeyState(vk_RIGHT) And &H8000),&H01, &H00)
		


	
	frame_complete = false
	
	if  bEmulationRun then
			if (fResidualTime > 0.0f) Then
						fResidualTime -= fElapsedTime
					Else
						fResidualTime += (1.0f / 60.0f) - fElapsedTime
						
               
               
					Do:   nes._clock: Loop while Not(frame_complete)
					'nes.apu.apu_cycle
				
					audio_hndler->start_aud
					 
					 
						nes.getsamples(audio_hndler->sampleBuffer(), audio_hndler->samplesPerFrame)
					audio_hndler->nextBuffer()
				
					 
					frame_complete = FALSE
					frames_per_sec(hwnd)		
					End If
					
	end if	
	
		
			If  KEYPRESSED(vk_space)  Then
	bEmulationRun = Not(bEmulationRun)
							checkmenuitem(hFilesub,IDM_FILE_PAUSE ,IIf(bEmulationRun,MF_UNCHECKED,MF_CHECKED))
					if bEmulationRun = false then
						
						audio_hndler->stop_aud
					else
						audio_hndler->start_aud
					EndIf
			EndIf
			
			
	
	
	
		Dim As hdc dc
	dc = GetDC(hwnd)

	
	if bfullscreen then
DrawNesScrn(dc,mydata(),((0+128)-64),0,rct.right,rct.bottom)
else
DrawNesScrn(dc,mydata(),0,0,rct.right,rct.bottom)
	end if

	ReleaseDC(hwnd,dc)
	
'SwitchToThread()
	
' end1 	= SDL_GetPerformanceCounter()

    ' dim elapsed  as float = (end1 - start1) / cast(float,SDL_GetPerformanceFrequency())
    '   elapsedMS    = (end1 - start1) / cast(float,SDL_GetPerformanceFrequency()) * 1000.0f

 '     SDL_Delay(int(16.666f - elapsedMS))
	
	
End Sub








Function myloadbmp(dc As hdc, path As String,mydata1 As ULong ptr = 0) As BOOLEAN
	
Dim databuf(1) As UByte Ptr 
Dim pixels As UByte Ptr
	
Dim bmpheader As BITMAPFILEHEADER Ptr
Dim bmpinfo As BITMAPINFOHEADER Ptr
Dim pal_4bit(0 To 15) As RGBQUAD 
Dim pal_8bit(0 To 255) As RGBQUAD
	
Dim As Integer bmpfile1 = FreeFile 
 
Open path For Binary As bmpfile1
  

databuf(0) = New UByte[SizeOf(BITMAPFILEHEADER)]
databuf(1) = New UByte[SizeOf(BITMAPINFOHEADER)]
 
 
Get #bmpfile1,,databuf(0)[0],SizeOf(BITMAPFILEHEADER) 
Get #bmpfile1,,databuf(1)[0],SizeOf(BITMAPINFOHEADER) 

bmpheader = Cast(BITMAPFILEHEADER Ptr,databuf(0))
bmpinfo  = Cast(BITMAPINFOHEADER Ptr,databuf(1))

If bmpheader->bfType <> &H4D42 Then
Print "invalid bitmap"
EndIf

pixels = New UByte[bmpInfo->biSizeImage]


'Uint8 tmpRGB = 0; // Swap buffer
'	for (unsigned long i = 0; i < bmpInfo->biSizeImage; i += 3)
'	{
'		tmpRGB        = pixels[i];
'		pixels[i]     = pixels[i + 2];
'		pixels[i + 2] = tmpRGB;
'	}

'If bmpinfo->biBitCount = 4 Then
'	Get #bmpfile1,,pal1(0),16
'Else
'	Seek #bmpfile1,bmpheader->bfOffBits
'End If

'Print Seek(bmpheader)
'Seek #bmpfile1,bmpheader->bfOffBits
'Sleep


Get #bmpfile1,,*pixels,bmpInfo->biSizeImage 

	Close bmpfile1


StretchDIBits dc,0,0,bmpinfo->biWidth,bmpinfo->biHeight,0,0,bmpinfo->biWidth,bmpinfo->biHeight,@pixels[0],bmpinfo,DIB_RGB_COLORS,SRCCOPY



	Delete[] databuf(0)
	Delete[] databuf(1)
	Delete[] pixels 
	
	databuf(0) = NULL
	databuf(1) = NULL

Return TRUE
			
end Function

Function mysavebmp(path As String,mydata1 As ULong ptr = 0) As BOOLEAN
	
   
Dim databuf(1) As UByte Ptr 
Dim pixels As UByte Ptr
	
Dim bmpheader As BITMAPFILEHEADER Ptr
Dim bmpinfo As BITMAPINFOHEADER Ptr
Dim pal_4bit(0 To 15) As RGBQUAD 
Dim pal_8bit(0 To 255) As RGBQUAD
	
Dim As Integer bmpfile1 = FreeFile 
 
Open path For Binary As bmpfile1
  

databuf(0) = New UByte[SizeOf(BITMAPFILEHEADER)]
databuf(1) = New UByte[SizeOf(BITMAPINFOHEADER)]
 

Get #bmpfile1,,databuf(0)[0],SizeOf(BITMAPFILEHEADER) 
Get #bmpfile1,,databuf(1)[0],SizeOf(BITMAPINFOHEADER) 

bmpheader = Cast(BITMAPFILEHEADER Ptr,databuf(0))
bmpinfo  = Cast(BITMAPINFOHEADER Ptr,databuf(1))

If bmpheader->bfType <> &H4D42 Then
Print "invalid bitmap"
EndIf

pixels = New UByte[bmpInfo->biSizeImage]


'Uint8 tmpRGB = 0; // Swap buffer
'	for (unsigned long i = 0; i < bmpInfo->biSizeImage; i += 3)
'	{
'		tmpRGB        = pixels[i];
'		pixels[i]     = pixels[i + 2];
'		pixels[i + 2] = tmpRGB;
'	}

'If bmpinfo->biBitCount = 4 Then
'	Get #bmpfile1,,pal1(0),16
'Else
'	Seek #bmpfile1,bmpheader->bfOffBits
'End If

'Print Seek(bmpheader)
'Seek #bmpfile1,bmpheader->bfOffBits
'Sleep


Get #bmpfile1,,*pixels,bmpInfo->biSizeImage 

	Close bmpfile1
			
			
	
	Dim savebmpfile As Integer = FreeFile
	
	
		   If FileExists("C:\Users\Gamer\Desktop\Kirby's Adventure (USA)\testrewrite.bmp") Then
   	Kill("C:\Users\Gamer\Desktop\Kirby's Adventure (USA)\testrewrite.bmp")
		   End If
			
	Open "testrewrite.bmp" For Binary As savebmpfile
	
			
		Put # savebmpfile, ,databuf(0)[0],SizeOf(BITMAPFILEHEADER) 
		
		
		Put 	# savebmpfile,,databuf(1)[0],SizeOf(BITMAPINFOHEADER)
		
		Put # savebmpfile,,pixels[0],bmpInfo->biSizeImage
			
			
			
			
			
		
	Delete[] databuf(0)
	Delete[] databuf(1)
	Delete[] pixels 
	
	databuf(0) = NULL
	databuf(1) = NULL	
	'pixels  = NULL		
end Function

sub free_rom()

	erase(mydata)

	bat_saves = 0

	
	if pmapper <> null then
	delete pmapper
	pmapper = null
	end if
	
	cls
	
	'bus_reset
	
	
	
End Sub

Function WndProc(hWnd As HWND, msg As  UINT, wParam As WPARAM, lParam As LPARAM) As Integer


	   
	  Dim hdc As HDC
	  Dim ps As PAINTSTRUCT
	  Dim rct As RECT
	
	Select Case ( Msg )
	
					case WM_KEYDOWN':								'	// If we pressed a key
			Select Case (wParam) 							'	// Check what the wParam holds.  The wParam holds which key we pressed.
				 
				case VK_ESCAPE':							'	// If we pressed the ESCAPE key.
					if(bFullScreen) Then						'	// If we went to full screen mode
					 										'// Calling the same function with NULL and 0 reset the screen settings and resolution
						ChangeDisplaySettings(NULL, 0)';
						showcursor 1
					End If
					SendMessage(hwnd, WM_CLOSE, 0, 0)';		'// Close the program
					'break;
			End Select
			
			
		case WM_CREATE

		Case WM_SIZE
	
		
			Return 0		
					
		
		   		
		case WM_QUIT	
			
		Case WM_CLOSE
					If(bFullScreen) Then						'	// If we went to full screen mode
					 										'// Calling the same function with NULL and 0 reset the screen settings and resolution
						ChangeDisplaySettings(NULL, 0)';
					End If
					
				
				
					
			'	dim sram_save as integer = FreeFile
			'
			'open "kirbyadventure.batt" for binary as sram_save
			' 
			' put # sram_save,,*cast(uint8_t ptr,pmapper->getbattery()),32*1024
			'
			'close sram_save
				pmapper->getbattery()
		
				'	delete pmapper
					'pmapper = null
					free_rom	
					
					
					'	threadwait(thread1)
					 quit = true
			DestroyWindow(hWnd)
			
				Case WM_SIZE
			
			'
		Case WM_DESTROY
			'ImageDestroy nesscrn
         'ImageDestroy pattables(0)
         'ImageDestroy pattables(1)
       
			
			PostQuitMessage(NULL)
		   ExitProcess(0)
         End	
         
         

		Case WM_MOUSEMOVE

		Case WM_SIZING

		Case WM_ERASEBKGND
		
		hdc = getdc(hwnd)
		fillrect(hdc,@Type<rect>(0,0,640,480),Cast(HBRUSH,GetStockObject(BLACK_BRUSH)))
		releasedc(hwnd,hdc)
			Return  1 
			
			case WM_INITMENU
			audio_hndler->stop_aud
			
			
		Case WM_COMMAND
		
		

			Select Case HiWord(wParam)
				Case BN_CLICKED,1
					
					Select Case LoWord(wParam)
						
						
						
						
						
						
						Case IDM_FILE_LOADROM
							'cart = ComDlgFileName(hwnd, "Open Source File", "C:\", "*.NES|*.NES", OFN_FORCESHOWHIDDEN)
			dim sram_save as integer = FreeFile
			
			'open "kirbyadventure.batt" for binary as sram_save
			' 
			' put # sram_save,,*cast(uint8_t ptr,pmapper->getbattery()),32*1024
			'
			'close sram_save
								cart = ""
	                    if vCHRMemory.clear then
	                    '	print"erased"
	                    End If
	                     
	                     if vPRGMemory.clear then
	                    ' print"erased"
	                     end if
	                   '  print pmapper->vRAMStatic.size()
	                     
	                     vCHRMemory.resize(0)
	                     vPRGMemory.resize(0)
	                    'sleep
							free_rom
							
							init_rom
					
							pmapper->getbattery()
							
							
							
							
						Case IDM_FILE_SAVEPIC
							
							pic1 = file_opensave(hwnd,FALSE)
								
		   If FileExists(pic1) Then
   	Kill(pic1)
		   End If
							'Print pic1
							SaveNESScrn(pic1,256,240,@mydata(0))
						
						Case IDM_FILE_PATTABLE01
											pic1 = file_opensave(hwnd,FALSE)
								
		   If FileExists(pic1) Then
   	Kill(pic1)
		   End If
							'Print pic1
							SaveNESScrn(pic1,128,128,@mydata_pattables(0).mydata(0))
						Case IDM_FILE_PATTABLE02
								pic1 = file_opensave(hwnd,FALSE)
		   					If FileExists(pic1) Then
		   					Kill(pic1)
		  						 End If
							'Print pic1
							SaveNESScrn(pic1,128,128,@mydata_pattables(1).mydata(0))
							
							
						Case IDM_FILE_EXIT
							
							SendMessage(hWnd,WM_CLOSE,0,0)
						Case IDM_FILE_RESET
							'bus_reset(false)
							nes._reset(false)
						Case IDM_FILE_HARDRESET
							nes._reset(true)
							
							
						Case IDM_FILE_SINGLEFRAME
							bSingleFrame = TRUE
						Case IDM_FILE_CYCLE
				'			
				'Do:  bus_clock2:  Loop while IIf(complete(),0,1)
				'Do:  bus_clock2:  Loop while iif(complete(),1,0)
				
				
		
						Case IDM_ZOOM_1X
							
					
								zoom_scrn = 1
									nesrect.right = 256*zoom_scrn
							nesrect.bottom = 240*zoom_scrn
Dim winRect2 As RECT							
getwindowrect(hwnd,@winrect2)								
Dim winRect As RECT => (0,0,(256*zoom_scrn),(240*zoom_scrn))
AdjustWindowRectEx(@winrect,WS_OVERLAPPED Or WS_SYSMENU  Or WS_CAPTION or WS_MINIMIZEBOX   or WS_CLIPCHILDREN or WS_CLIPSIBLINGS,TRUE,NULL)
setwindowpos(hwnd,NULL,winrect2.left,winrect2.top,winrect.right-winrect.left,winrect.bottom-winrect.top,SWP_SHOWWINDOW)
		
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_1X,MF_CHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_2X,MF_UNCHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_3X,MF_UNCHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_4X,MF_UNCHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_FULLSCREEN,MF_UNCHECKED)
						
						Case IDM_ZOOM_2X
							
							zoom_scrn = 2
							nesrect.right = 256*zoom_scrn
							nesrect.bottom = 240*zoom_scrn
							
Dim winRect2 As RECT							
getwindowrect(hwnd,@winrect2)								
Dim winRect As RECT => (0,0,(256*zoom_scrn),(240*zoom_scrn))
AdjustWindowRectEx(@winrect,WS_OVERLAPPED Or WS_SYSMENU  Or WS_CAPTION or WS_MINIMIZEBOX   or WS_CLIPCHILDREN or WS_CLIPSIBLINGS,TRUE,NULL)
setwindowpos(hwnd,NULL,winrect2.left,winrect2.top,winrect.right-winrect.left,winrect.bottom-winrect.top,SWP_SHOWWINDOW)
		
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_1X,MF_UNCHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_2X,MF_CHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_3X,MF_UNCHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_4X,MF_UNCHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_FULLSCREEN,MF_UNCHECKED)
			
						Case IDM_ZOOM_3X
							
							zoom_scrn = 3
								nesrect.right = 256*zoom_scrn
							nesrect.bottom = 240*zoom_scrn
Dim winRect2 As RECT							
getwindowrect(hwnd,@winrect2)								
Dim winRect As RECT => (0,0,(256*zoom_scrn),(240*zoom_scrn))
AdjustWindowRectEx(@winrect,WS_OVERLAPPED Or WS_SYSMENU  Or WS_CAPTION or WS_MINIMIZEBOX   or WS_CLIPCHILDREN or WS_CLIPSIBLINGS,TRUE,NULL)
setwindowpos(hwnd,NULL,winrect2.left,winrect2.top,winrect.right-winrect.left,winrect.bottom-winrect.top,SWP_SHOWWINDOW)
		
	   checkmenuitem(hView_Zoom_sub,IDM_ZOOM_1X,MF_UNCHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_2X,MF_UNCHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_3X,MF_CHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_4X,MF_UNCHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_FULLSCREEN,MF_UNCHECKED)
						
						Case IDM_ZOOM_4X
							zoom_scrn = 4
							nesrect.right = 256*zoom_scrn
							nesrect.bottom = 240*zoom_scrn
Dim winRect2 As RECT							
getwindowrect(hwnd,@winrect2)								
Dim winRect As RECT => (0,0,(256*zoom_scrn),(240*zoom_scrn))
AdjustWindowRectEx(@winrect,WS_OVERLAPPED Or WS_SYSMENU  Or WS_CAPTION or WS_MINIMIZEBOX   or WS_CLIPCHILDREN or WS_CLIPSIBLINGS,TRUE,NULL)
setwindowpos(hwnd,NULL,winrect2.left,winrect2.top,winrect.right-winrect.left,winrect.bottom-winrect.top,SWP_SHOWWINDOW)
		
		   checkmenuitem(hView_Zoom_sub,IDM_ZOOM_1X,MF_UNCHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_2X,MF_UNCHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_3X,MF_UNCHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_4X,MF_CHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_FULLSCREEN,MF_UNCHECKED)
		
					   Case IDM_ZOOM_FULLSCREEN
								
		   checkmenuitem(hView_Zoom_sub,IDM_ZOOM_1X,MF_UNCHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_2X,MF_UNCHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_3X,MF_UNCHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_4X,MF_UNCHECKED)
			checkmenuitem(hView_Zoom_sub,IDM_ZOOM_FULLSCREEN,MF_CHECKED)
			
			ChangeToFullScreen(SCREEN_WIDTH, SCREEN_HEIGHT)
			
			ShowCursor 0
			
			'ShowWindow(hWnd, SW_HIDE)
			'UpdateWindow(hWnd)
			
			

							
						Case IDM_FILE_PAUSE
				
							bEmulationRun = Not(bEmulationRun)
							checkmenuitem(hFilesub,IDM_FILE_PAUSE ,IIf(bEmulationRun,MF_UNCHECKED,MF_CHECKED))
					if bEmulationRun = false then
						
						audio_hndler->stop_aud
					else
						audio_hndler->start_aud
						
					EndIf
				
							
							
							
					End Select
         End Select
		Case Else 
		return DefWindowProc(hWnd, Msg, wParam, lParam) 
 
	End Select
	
	Return 0
	
End Function

Function WinMain(hInstance As HINSTANCE, hPrevInstance As HINSTANCE, lpCmdLine As LPSTR, nShowCmd As Integer) As Integer
  Dim As WNDCLASSEX wcex
  Dim As MSG msg
  Dim As string classname="OLC-NES-NESTEA-FB"
  Dim As HANDLE hIconLib
  
  Dim bdone As bool = FALSE
  Dim screenSize  As SIZE
  Dim As LONG winX, winY 

	ZeroMemory(@wcex, sizeof(WNDCLASSEX)) 
	ZeroMemory(@msg, sizeof(MSG)) 

	wcex.cbSize = sizeof(WNDCLASSEX) 
	wcex.hbrBackground = Cast(HBRUSH,GetStockObject(WHITE_BRUSH)) 
	wcex.hCursor = LoadCursor(hInstance, IDC_ARROW) 
	wcex.hIcon   = LoadIcon(hInstance, MAKEINTRESOURCE(IDI_ICON1)) 
	wcex.hInstance = hInstance 
	wcex.lpfnWndProc = @WndProc 
	wcex.lpszClassName =  StrPtr(classname) 
	wcex.style = CS_HREDRAW Or CS_VREDRAW 
	wcex.cbWndExtra = 0 
	wcex.cbClsExtra = 0 
	wcex.lpszMenuName = NULL 

	if ((RegisterClassEx(@wcex))) = 0 then  
		return -1 
	End If
	
	
 Dim winRect As RECT => (0,0,256*zoom_scrn,240*zoom_scrn)


If bfullscreen = FALSE Then
 AdjustWindowRectEx(@winrect,WS_OVERLAPPED Or WS_SYSMENU  Or WS_CAPTION or WS_MINIMIZEBOX   or WS_CLIPCHILDREN or WS_CLIPSIBLINGS,TRUE,NULL)

hwnd = CreateWindowEx( _
		NULL, _
		 wcex.lpszClassName, _
		"OLC-NES-NESTEA-FB-V0.1", _
			WS_OVERLAPPED Or WS_SYSMENU Or WS_CAPTION or WS_MINIMIZEBOX  or WS_CLIPCHILDREN or WS_CLIPSIBLINGS, _
		300,300, _
		winrect.right-winrect.left, _
		winrect.bottom-winrect.top, _
		0, _
		0, _
		hInstance, _
		0) 


win_menu(hwnd)
Else
		
		ChangeToFullScreen(SCREEN_WIDTH, SCREEN_HEIGHT)
			
			ShowCursor 0
	'dwStyle = WS_POPUP or WS_CLIPSIBLINGS Or WS_CLIPCHILDREN
	    hwnd = CreateWindow (StrPtr(classname), _		'			// window class name 
						 "Full Screen App", _		  		'	// window's Title    
						 WS_POPUP or WS_CLIPSIBLINGS Or WS_CLIPCHILDREN, _						'	// window style		 
						 0, _								'	// initial x position
						 0, _									'// initial y position
						 SCREEN_WIDTH, _						'// initial x size - Our resolution width
						 SCREEN_HEIGHT, _					'    // initial y size - Our resolution height	 
						 NULL, _								'// Pass NULL for the parent window
						 NULL, _								'// Pass NULL for a menu
						 hInstance, _						 '   // Pass in our hInstance
						 NULL) 							'	// Pass NULL to the wndProc, we don't want to pass it any variables address's.


'enableWindow(hwnd,true)		
'SetActiveWindow(hwnd)

End If


ShowWindow(hWnd, SW_SHOW)
UpdateWindow(hWnd)
SetActiveWindow(hwnd)

'Dim As hdc dc = getdc(hwnd)
'
'	myloadbmp(dc,"redtest.bmp")
'releasedc(hwnd,dc)

mysavebmp("C:\Users\Gamer\Desktop\OLC-FREEBASIC-NESTEA--main\imagetest1.bmp")


	while (bDone = FALSE)  

		if (PeekMessage(@msg, NULL, 0, 0, PM_REMOVE)) then 
			TranslateMessage(@msg) 
			DispatchMessage(@msg) 

			if (msg.message = WM_QUIT)  Then
				bDone = TRUE 
			End If
		 
		Else
'Dim As hdc dc = getdc(hwnd)
	'myloadbmp(dc,"C:\Users\Gamer\Desktop\OLC-FREEBASIC-NESTEA--main\index.bmp")
'releasedc(hwnd,dc)
  'cls
  

  	'nesmain(hWnd,NesRect)
	nesmain_with_audio(hWnd,NesRect)
	
	
		End If
  
	Wend
	
	ShowCursor 1
	
	Return msg.wParam 

	'DestroyWindow( hWnd )
	'UnregisterClass(wcex.lpszClassName, hInstance)
'	Return 0
	
End Function

Sub init() 
	
'Dim bmp As String
'
'Dim s As String
'
'Print "choose a rom"
'
'While cart = ""
'	
'	
'	cart = file_opensave(NULL)
'	If cart = "" Then 
'	Print "invalid rom file"
'		
'		
'	EndIf
'Wend
'
''Print bmp
''	bmp = file_opensave(NULL)
'	'Print bmp
'	
'	
'
'setconsoletitle("OLC-NES-NESTEA-FB-V0.1")
''Print cart
''Input   "enter rom path or drag file here: ", cart
'insert_cartridge(cart)
'setconsoletitle("OLC-NES-NESTEA-FB-V0.1")
'Input   "fullscreen Y/N?: ", s 
'Cls
'
'
'
'If s  = "Y" Or s  = "y" Then
'	
'	bfullscreen = TRUE
'	'FreeConsole()
'
'ElseIf s  = "N" Or s  = "n" Then
'
'bfullscreen = FALSE
'EndIf
'		If ImageValid = TRUE Then
'	Print "CRC: " & "???? WORK IN PROGRESS"
'	Print "loaded rom!"
'	Print "MAGIC NUM: "; header.Name1
'	Print "MIRROR MODE: "; IIf(cart_mirror = horizontal,"HORIZONTAL","VERTICAL")
'	Print "ROM BANKS: " & Str(header.prg_rom_chunks)
'	Print "PRG ROM SIZE: " & Str(vPRGMemory.size())
'	Print "CHRROM BANKS: " & str(header.chr_rom_chunks)
'	Print "CHRROM SIZE:" & Str(vCHRMemory.size())
'	Print "MAPPER: " & Str(nmapperid)
'	Print "PRGRAM SIZE: " & Str(header.prg_ram_size)
'	Print header.tv_system1
'	Print header.tv_system2
'	Print header.unused
'	Print 
'	else
'	Print "rom not found or isnt valid"
'	Print 
'	Print  "press Any key To Quit"
'	
'		Sleep
'		End
'		EndIf
'
''insert_cartridge("roms/kung fu.nes")
''insert_cartridge("donkey.nes")
''insert_cartridge("Burger Time (U) [!].nes")
''insert_cartridge("Felix the Cat (U).nes")
''insert_cartridge("Felix the Cat (U).nes")
''insert_cartridge("roms/Kirby's Adventure (USA).nes")
''insert_cartridge("roms/Volleyball.nes")
''insert_cartridge("roms/SMB3.nes")
''insert_cartridge("roms/SMB2.nes")
''insert_cartridge("Ren & Stimpy Show, The (U).nes")
''insert_cartridge("California Games (U).nes")
''insert_cartridge("roms/SMB.nes")
''insert_cartridge("ice climber.nes")
''insert_cartridge("roms/Megaman1.nes")
'
''init_rom
'
''mapAsm = disassemble(&H0000, &HFFFF) '&H35
'
'bus_reset()
color 255
print "			 WELCOME TO ";
color 60
print "NESTEA V0.1!"
color 255
print " "
print "please note this emulator is for EDUCATIONAL and FUN purposes"
print "and it's a working in progress, such audio bugs.It can only play 1 player for now with mapper 0,1,2,3,66 and 4 games"
print "mapper 4 is incomplete i got the irq working but might need some fixing still. 
print "should be able to run some mapper 4 games with it."

print " "
print "the controls are:"
print "X - A"
print "Z - B"
print "A - SELECT"
print "S - START"
print" "
print "press any key to continue"


sleep
cls
init_rom


End Sub

Function ChangeToFullScreen(width1 As Integer ,height1 As  Integer ) As BOOL
 
	dim dmSettings As DEVMODE								'	// Device Mode variable - Needed to change modes

	memset(@dmSettings,0,sizeof(dmSettings)) 			'// Makes Sure Memory's Cleared

	'// Get the current display settings.  This function fills our the settings.
	if(EnumDisplaySettings(NULL,ENUM_CURRENT_SETTINGS,@dmSettings) = 0) Then
	 
		'// Display error message if we couldn't get display settings
		MessageBox(NULL, "Could Not Enum Display Settings", "Error", MB_OK) 
		Return FALSE
	End If

	dmSettings.dmPelsWidth	= Width1';					// Set the desired Screen Width
	dmSettings.dmPelsHeight	= height1';					// Set the desired Screen Height
	dmSettings.dmFields = DM_PELSWIDTH Or DM_PELSHEIGHT';	// Set the flags saying we're changing the Screen Width and Height
	
	'// This function actually changes the screen to full screen
'	// CDS_FULLSCREEN Gets Rid Of Start Bar.
'	// We always want to get a result from this function to check if we failed
	dim result As Integer = ChangeDisplaySettings(@dmSettings,CDS_FULLSCREEN)';	

	'// Check if we didn't receive a good return message From the function
	if(result <> DISP_CHANGE_SUCCESSFUL) Then
 
	'	// Display the error message and quit the program
		MessageBox(NULL, "Display Mode Not Compatible", "Error", MB_OK) 
		PostQuitMessage(0) 

	End If
End Function

Sub palrect_pos(x1 As Integer,y1 As Integer)
	
	
	'For y As Integer = 0 To 1
	'	
	'	
	'	For x As Integer
	'		
	'		fill_pal_rect(dc,256*1.5,y1,0,0)
	'		
	'		
	'	Next
	'Next
	
	
	'
	'fill_pal_rect(dc,(256*1.5)+12,y1,0,1)
	'fill_pal_rect(dc,(256*1.5)+24,y1,0,2)
	

	
	
End Sub


init
WinMain(GetModuleHandle(NULL), NULL, COMMAND(), SW_NORMAL)
				


sub audioloop()
'audio_hndler = new AudioHandler(48000,AUDIO_S16SYS,2048,16)
'audio_hndler->start_aud()
'audio_hndler->audio_spec.callback = NULL

	'do
	'
	'
	'
	'if frame_complete <> true then
	''
	'end if	
	'
	'
	'
	'loop	
	
	do 
	sdl_delay(1000)
	Loop until quit = true
	
	
End Sub
