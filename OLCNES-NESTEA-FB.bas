'
Dim Shared pattables(1) As Any ptr '=> {ImageCreate(128,128,RGB(0,0,0),32),ImageCreate(128,128,RGB(0,0,0),32)}
'
'pattables(0)=ImageCreate(128,128,RGB(0,0,0),32)
'pattables(1)=ImageCreate(128,128,RGB(0,0,0),32)
#Define GET_X_LPARAM(LPARAM) LoWord(LPARAM)
#Define GET_Y_LPARAM(LPARAM) HiWord(LPARAM)

#Include Once  "nes/NES.bi"

#Include Once"windows.bi"   
#Include Once"crt.bi"
#Include Once "win/commctrl.bi"
#Include Once  "fbgfx.bi"

#include "tests1.bi"



#define KEYDOWN(vk_code) (IIf(GetAsyncKeyState(vk_code) And &H8000), 1, 0)
#define KEYUP(vk_code)   (IIf(GetAsyncKeyState(vk_code) And &H8000),0, 1)

const DEF_CX = 256 
const  DEF_CY = 240

Using fb
Dim Shared fResidualTime As float
Dim Shared fElapsedTime As float
Dim Shared fElapsedstart As float

'Dim Shared nesscrn As Any Ptr
'
'Dim shared id As uint8_t  
'nesscrn = ImageCreate(512,480,RGB(0,0,0),32)

'Dim Shared mydata(341 * 261) As uint32_t

'Dim Shared mydata(256 * 240) As uint32_t

'Sub paintbackgrnd(hwnd As hwnd)
DIM SHARED ff As Integer
DIM SHARED fps As Integer
DIM SHARED start As Single
DIM SHARED bEmulationRun As bool = FALSE
DIM SHARED bSingleFrame As bool = FALSE





Dim Shared isFpressed As bool = FALSE

Dim Shared ispressed As bool = FALSE


Dim Shared zoom_scrn As Integer = 2
Dim Shared nesrect As rect => (0,0,256*2 ,240*2)

	 Dim Shared hmenu As HMENU  
	  Dim Shared hViewsub As HMENU
	  
	  Dim Shared hFilesub As HMENU
	  Dim Shared hFile_Recent_sub As HMENU

	  Dim Shared hView_Zoom_sub As HMENU



'/1000'gettickcount/1000.0f
fElapsedstart = Timer

Function KEYPRESSED(vk_code As Integer) As bool
	static Iskeyup(&HFF) As bool
	
	  
	' Iskeyup = IIf(GetAsyncKeyState(vk_code) And &H8000, true, FALSE)
			
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
 titles1="OLC-NES-NESTEA-FB:"&" FPS:" & STR (fps )
setwindowtext(hwnd,titles1 )

END Sub

Sub DrawNesScrn(dc As hDC,ImageData() As uint32_t, destwdth As int16_t, desthght As  int16_t)

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
StretchDIBits dc, 0, 0, destwdth, desthght, 0, 0, 256, 240 ,@ImageData(0), @bi,0,SRCCOPY


End Sub

Sub win_menu(hwnd As HWND)
	

	  
			hmenu = CreateMenu()
			hFilesub = CreatePopupMenu()
			hViewsub = CreatePopupMenu()
			
			hFile_Recent_sub = CreatePopupMenu()
			hView_Zoom_sub = CreatePopupMenu()
			
			AppendMenu(hmenu,MF_POPUP,hFilesub,"&file")
				AppendMenu(hFilesub,MF_GRAYED,IDM_FILE_LOADROM,"&load rom")
					AppendMenu(hFilesub,MF_ENABLED,IDM_FILE_RESET,"&Reset NES")
					  AppendMenu(hFilesub,MF_CHECKED,IDM_FILE_PAUSE,"&Pause NES")
					  	AppendMenu(hFilesub,MF_ENABLED,IDM_FILE_SINGLEFRAME,"&Single Frame")
							AppendMenu(hFilesub,MF_ENABLED,IDM_FILE_CYCLE,"&Cycle")
				AppendMenu(hFilesub,MF_SEPARATOR,0,"-")
				AppendMenu(hFilesub,MF_POPUP Or MF_GRAYED,hFile_Recent_sub,"&Recent roms")
							AppendMenu(hFile_Recent_sub,MF_GRAYED,IDM_FILE_RECENT_EMPTY,"Empty")
				AppendMenu(hFilesub,MF_SEPARATOR,IDM_SEPARATOR,"-")			
							
				AppendMenu(hFilesub,MF_GRAYED,IDM_FILE_SAVEPIC ,"&Save Pic")
													
			AppendMenu(hFilesub,MF_SEPARATOR,IDM_SEPARATOR,"-")
			AppendMenu(hFilesub,MF_POPUP,IDM_FILE_EXIT,"&Exit")
							
							
			AppendMenu(hmenu,MF_POPUP,hViewsub,"&View")				
							AppendMenu(hViewsub,MF_POPUP,hView_Zoom_sub,"&Zoom")
									AppendMenu(hView_Zoom_sub,MF_UNCHECKED,103,"1x")
									AppendMenu(hView_Zoom_sub,MF_CHECKED,104,"2x")
									AppendMenu(hView_Zoom_sub,MF_UNCHECKED,105,"3x")
									AppendMenu(hView_Zoom_sub,MF_UNCHECKED,106,"4x")				
									AppendMenu(hView_Zoom_sub,MF_GRAYED,107,"&Fullscreen")
				SetMenu(hwnd,hmenu)			
							
							
							
							
					'For i As Integer = 0 To 10-1
					'AppendMenu(hFile_Recent_sub,MF_GRAYED,101,"Empty")
					'
					'Next i
End Sub


Sub nesmain(hwnd As HWND,rct As rect)
			'	Dim fElapsednewstart As float = Timer-fElapsedstart 'gettickcount/1000.0f 'gettickcount/1000.0f
	fElapsedTime = Timer-fElapsedstart  '/1000.0f   '	(gettickcount/1000.0f) - fElapsedstart	
 fElapsedstart = timer

	'GetClientRect(hwnd,@rct) 
	'	For i As uint32_t = 0 To (341 *261)-1
	'mydata(i) = RGB(Rnd*255,Rnd*255,Rnd*255)
	'
	'Next

		controller(0) = &H00 
		controller(0) Or= IIf((GetAsyncKeyState(vk_X) And &H8000), &H80, &H00)    ' // A Button
		controller(0) Or= IIf((GetAsyncKeyState(vk_Z) And &H8000), &H40, &H00)     ' // B Button
		controller(0) Or= IIf((GetAsyncKeyState(vk_A) And &H8000), &H20, &H00)     ' // Select
		controller(0) Or= IIf((GetAsyncKeyState(vk_S) And &H8000), &H10, &H00)     ' // Start
		controller(0) Or= Iif((GetAsyncKeyState(vk_UP) And &H8000), &H08, &H00) 
		controller(0) Or= Iif((GetAsyncKeyState(vk_DOWN) And &H8000), &H04, &H00) 
		controller(0) Or= Iif((GetAsyncKeyState(vk_LEFT) And &H8000),&H02,&H00) 
		controller(0) Or= Iif((GetAsyncKeyState(vk_RIGHT) And &H8000),&H01, &H00)
		






			
				if (bEmulationRun) Then
					
					'if (fResidualTime > 0.0!) Then
				'		fResidualTime -= fElapsedTime
				''ff  = ff + 1
				'	else
			
				'		fResidualTime += (1.0! / 1.0!) - fElapsedTime
				'		Print "works"
				'	'Do:  bus_clock2:  Loop while Not(frame_complete) 
				'	
				'	' Print ff
				'	'ff = 0
				'	
				''Do:  bus_clock2:  Loop while iif(frame_complete,0,1) 
				'	frame_complete = FALSE
				'	End If
					
					if (fResidualTime > 0.0f) Then


						fResidualTime -= fElapsedTime
				'ff  = ff + 1
					Else
						
						fResidualTime += (1.0f / 60.0f) - fElapsedTime
					 '	Print fElapsedstart
					Do:  bus_clock2:  Loop while Not(frame_complete) 
					
					' Print ff
					'ff = 0
				
				'Do:  bus_clock2:  Loop while iif(frame_complete,0,1) 
					frame_complete = FALSE
							frames_per_sec(hwnd)	
							
					End If
					
					Else
						
						
					If bsingleframe = TRUE Then
					
					
					
				Do:  bus_clock2:  Loop while iif(frame_complete,0,1)
				Do:  bus_clock2:  Loop while iif(complete(),0,1)
				frame_complete = FALSE
				bSingleFrame = FALSE
				
					end If
						
		'If  IIf(GetAsyncKeyState(vk_F) And &H8000, 1, 0) And isFpressed = FALSE  Then
	
		'		Do:  bus_clock2:  Loop while iif(frame_complete,0,1)
		'		Do:  bus_clock2:  Loop while iif(complete(),0,1)
		'		frame_complete = FALSE
		'		
		'	isFpressed = TRUE
		'elseIf  IIf(GetAsyncKeyState(vk_F) And &H8000, 0, 1) Then	
	   '   isFpressed = FALSE
		'	
		'EndIf
		'
		
			If  KEYPRESSED(vk_f)  Then
	
				Do:  bus_clock2:  Loop while iif(frame_complete,0,1)
				Do:  bus_clock2:  Loop while iif(complete(),0,1)
				frame_complete = FALSE
			End if	
				
		If  KEYPRESSED(vk_space)  Then
	
				
				Do:  bus_clock2:  Loop while IIf(complete(),0,1)
				Do:  bus_clock2:  Loop while iif(complete(),1,0)
			
		End if	
				EndIf
				
		If  KEYPRESSED(vk_r)  Then
	
			 bus_reset
				

		End If
		
      If  KEYPRESSED(vk_e)  Then
			 bEmulationRun = Not(bEmulationRun)
				checkmenuitem(hFilesub,IDM_FILE_PAUSE ,IIf(bEmulationRun,MF_UNCHECKED,MF_CHECKED))
						
			
				

						End If
		'End If
	
		
		
	
	Dim As hdc dc
	dc = GetDC(hwnd)
	
	
	'DrawNesScrn(dc,mydata(),rct.right*zoom_scrn,rct.bottom*zoom_scrn)
	

DrawNesScrn(dc,mydata(),rct.right,rct.bottom)
	ReleaseDC(hwnd,dc)
	

	
	
	
End Sub


Function WndProc(hWnd As HWND, msg As  UINT, wParam As WPARAM, lParam As LPARAM) As Integer
	 ' Dim hmenu As HMENU
	 ' Dim hFilesub As HMENU
	 ' Dim hRecent_sub As HMENU
	   Dim hStatus As hwnd
	   
	   
	  Dim hdc As HDC
	  Dim ps As PAINTSTRUCT
	  Dim rct As RECT
	
	Select Case ( Msg )
	
			
		case WM_CREATE
	 			'InitCommonControls()
	
	     win_menu(hwnd)
			
			
		  'hStatus = CreateWindowEx(0, STATUSCLASSNAME, NULL,
        'WS_CHILD | WS_VISIBLE | SBARS_SIZEGRIP, 0, 0, 0, 0,
        'hwnd, (HMENU)IDC_MAIN_STATUS, GetModuleHandle(NULL), NULL)
			
			
			
			'hstatus = CreateWindowEx(0,STATUSCLASSNAME,"", _
        'WS_CHILD Or WS_VISIBLE, 0, 0, 0, 0, _
        'hwnd, Cast(HMENU,1009), GetModuleHandle(NULL), NULL)
        '
        'Dim client_rect As rect
        'GetClientRect(hwnd,@client_rect)
        '        Dim status_parts(1) as  Integer 
        '        
        '        
      
        '         status_parts(0) = client_rect.right-client_rect.left-170                         
        '       status_parts(1) = client_rect.right-client_rect.left
        '            SendMessage(hstatus , SB_SETPARTS, 2, Cast(LPARAM, @status_parts(0))) 
        '            SendMessage(hstatus , SB_SETTEXT, 0, Cast (LPARAM,@"Ready")) 

        'ShowWindow(hstatus,SW_SHOW)
		'no rom loaded...
		Case WM_SIZE
	
			 'nesrect.right = LoWord(lparam )+172
			 'nesrect.bottom =   HiWord(lparam)+42
			 '
			 '
				
							
			Return 0				
		case WM_QUIT	
			
		Case WM_CLOSE
			DestroyWindow(hWnd)
				Case WM_SIZE
			
			'
		Case WM_DESTROY
			'ImageDestroy nesscrn
         'ImageDestroy pattables(0)
         'ImageDestroy pattables(1)
			PostQuitMessage(NULL)
		   ExitProcess(0)
         'End	
         
         

		Case WM_MOUSEMOVE
		'Dim xpos As Integer =	 GET_X_LPARAM(lparam)
		'Dim ypos As Integer =	 GET_Y_LPARAM(lparam)
		'Cls 
		'Print xpos	
		'Print ypos	
							
		Case WM_SIZING
			
		
		'Dim temprect As rect
		'	
		'	getclientrect(hwnd,@temprect)
		'	
		'	nesrect.bottom = (temprect.bottom+42)
		'	nesrect.right = (temprect.right)
	
	
		
		Case WM_ERASEBKGND
			'GetClientRect(hwnd,@rct)
			'hdc = BeginPaint(hwnd,@ps)
			'FillRect(GetDC(hwnd),@rct,GetStockObject(GRAY_BRUSH))
			'
			'
			'
			'
			'EndPaint(hwnd,@ps)
		'	getclientrect(hwnd,@NesRect)
		
		'getclientrect(hwnd,@NesRect)
			'Dim temprect As rect
			'
			'getclientrect(hwnd,@temprect)
			'
			'nesrect.bottom = (temprect.bottom + 42)
			'nesrect.right = (temprect.right + (85*2))
			Return  1 'paintbackgrnd(hwnd)
		Case WM_COMMAND
			
	
			
				
			Select Case HiWord(wParam)
				Case BN_CLICKED,1
					Select Case LoWord(wParam)
						Case IDM_FILE_EXIT
							SendMessage(hWnd,WM_CLOSE,0,0)
						Case IDM_FILE_RESET
							bus_reset
						Case IDM_FILE_SINGLEFRAME
							bSingleFrame = TRUE
						Case IDM_FILE_CYCLE
							
				Do:  bus_clock2:  Loop while IIf(complete(),0,1)
				Do:  bus_clock2:  Loop while iif(complete(),1,0)
		
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
			
							
						Case IDM_FILE_PAUSE
				
							bEmulationRun = Not(bEmulationRun)
							checkmenuitem(hFilesub,IDM_FILE_PAUSE ,IIf(bEmulationRun,MF_UNCHECKED,MF_CHECKED))

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
  Dim As string classname="OLC-NESTEA-FB"
  Dim As HANDLE hIconLib
  Dim hwnd As HWND
  Dim bdone As bool = FALSE
  Dim screenSize  As SIZE
  Dim As LONG winX, winY 


'fElapsedstart = Timer
   


	ZeroMemory(@wcex, sizeof(WNDCLASSEX)) 
	ZeroMemory(@msg, sizeof(MSG)) 

	wcex.cbSize = sizeof(WNDCLASSEX) 
	'wcex.hbrBackground = Cast(HBRUSH,GetStockObject(WHITE_BRUSH)) 
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
	
 'screenSize.cx = GetSystemMetrics(SM_CXSCREEN)
 'screenSize.cy = GetSystemMetrics(SM_CYSCREEN)
 'winX = ( screenSize.cx - (DEF_CX + GetSystemMetrics(SM_CXBORDER)*2) ) / 2 
 'winY = ( screenSize.cy - (DEF_CY + GetSystemMetrics(SM_CYBORDER) + GetSystemMetrics(SM_CYCAPTION)) ) / 2 
 '
 '
 '
 
 Dim winRect As RECT => (0,0,256*zoom_scrn,240*zoom_scrn)
 'WS_OVERLAPPEDWINDOW Or 
 AdjustWindowRectEx(@winrect,WS_OVERLAPPED Or WS_SYSMENU  Or WS_CAPTION or WS_MINIMIZEBOX   or WS_CLIPCHILDREN or WS_CLIPSIBLINGS,TRUE,NULL)

hwnd = CreateWindowEx( _
		NULL, _
		 wcex.lpszClassName, _
		"OLC-NES-NESTEA-FB", _
			WS_OVERLAPPED Or WS_SYSMENU Or WS_CAPTION or WS_MINIMIZEBOX  or WS_CLIPCHILDREN or WS_CLIPSIBLINGS, _
		300,300, _
		winrect.right-winrect.left, _
		winrect.bottom-winrect.top, _
		0, _
		0, _
		hInstance, _
		0) 

ShowWindow(hWnd, SW_SHOW)
	UpdateWindow(hWnd)
	
	'Dim T As Double
	'Dim LT As Double
	'Dim DLY As Double
	'Dim FPS As Double	
	'Dim F As Double
	
'T  = timer 'Set the time, this will be used for the FPS lock..
'
Dim LOCFPS As Integer
'LOCFPS = 60 'Make a contant to store the disired FPS, this will also
	while (bDone = FALSE)  



		
		if (PeekMessage(@msg, NULL, 0, 0, PM_REMOVE)) then 
			TranslateMessage(@msg) 
			DispatchMessage(@msg) 

			if (msg.message = WM_QUIT)  Then
				bDone = TRUE 
			End If
		 
		Else
	

'getclientrect(hwnd,@NesRect)

  	nesmain(hWnd,NesRect)
	

		
		
		
		
		End If
		
		
  


  

				
		
  
	Wend
	Return msg.wParam 

	'DestroyWindow( hWnd )
	'UnregisterClass(wcex.lpszClassName, hInstance)
'	Return 0
	
End Function
insert_cartridge("roms/kung fu.nes")
'insert_cartridge("donkey.nes")
'insert_cartridge("Burger Time (U) [!].nes")
'insert_cartridge("Felix the Cat (U).nes")
'insert_cartridge("Felix the Cat (U).nes")
'Dim active as bool   = false 
'dim wasKeyDown As bool  = false 
'
'
'

'Do
'
'Dim key1 As bool 
'key1 = GetAsyncKeyState(VK_F) and &H8000 
'
''// Check if key is currently down
'if (key1 <> wasKeyDown And key1) then
' 
'  wasKeyDown = true 
'  active =  Not(active )
' if active Then Print "enabled" Else Print "disabled"
' 
' 
'End If
'
''// Toggle key state
'if (active<> wasKeyDown and Not(active)) Then
' 
'  wasKeyDown = false 
'End If
'		Loop Until InKey = " "
'Sleep
'
' End



'insert_cartridge("roms/Kirby's Adventure (USA).nes")
'insert_cartridge("roms/Volleyball.nes")

'insert_cartridge("roms/SMB3.nes")
'insert_cartridge("roms/SMB2.nes")
'insert_cartridge("Ren & Stimpy Show, The (U).nes")
'insert_cartridge("California Games (U).nes")
'insert_cartridge("roms/SMB.nes")
'insert_cartridge("ice climber.nes")
'insert_cartridge("roms/Megaman1.nes")
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
	'Print  "press Any key To continue" 
	 'ScreenCopy( 0, 1)
	'Sleep
	'cls
	else
	Print "rom not found or isnt valid"
	Print 
	Print  "press Any key To Quit"
	'ScreenCopy( 0, 1)
		Sleep
		End
	EndIf
'cpuram(&HFFFC) = &H00
'cpuram(&HFFFD) = &H80

'mapAsm = disassemble(&H0000, &HFFFF) '&H35


bus_reset()

WinMain(GetModuleHandle(NULL), NULL, COMMAND(), SW_NORMAL)

  ' LT  = gettickcount' Set Loop start time..
  
	'	Cls
	'	Print FPS
	'   FPS  = F / (gettickcount- T )' Find FPS
   'IF LOCFPS <> INT(FPS) THEN 'If the actual FPS is not the FPS wanted
   '                             'then modify the delay...
   '                          
   '     IF LOCFPS < FPS  THEN DLY  += .001 'Increace Delay if too fast
   '     IF LOCFPS > FPS  THEN DLY  -= .001 'Decreace Delay if too slow
   '    
	'	END IF 'Stop checking
	'	


 't = gettickcount

' All game handling routines go here
' ...
' ...

' If game handling took less than a 24th of a second, then hold on a moment
'DO: LOOP UNTIL gettickcount > t  +  (1.0!/60.0!)




  '	Sleep 1000/60
'  	

'    DO: LOOP UNTIL (gettickcount - LT) >= DLY  'Loop delay, with the help of
''                                         'above, this will speed up or
''                                         'slow down the main loop speed.
''                                         
'    F += 1 'Count the frames, this will divide to loop time for FPS
'  
		 
	
	
	
		'fElapsedTime = gettickcount()
 		 	
	'if (fResidualTime > 0.0!) then
				'fResidualTime -= fElapsedTime
				
		'	else
		 
				'fResidualTime += (1.0!  / 1.0! ) - fElapsedTime 
				
				

