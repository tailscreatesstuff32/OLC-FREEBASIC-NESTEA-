/'
	Dialog Example, by fsw

	compile with:	fbc -s gui dialog.rc dialog.bas

'/

#Include Once "windows.bi"
#Include Once "win/shellapi.bi"
'#Include Once "win/winutils.bi"



#Include "fbgfx.bi"
#Include "Window.bi"

Dim Shared img As HBITMAP
Dim Shared buffer() As Long
Dim Shared testimg As FB.image
Dim Shared destimg As FB.image

Const ClassName="DLGCLASS"
Const AppName="NESTEA-FB"
Const AboutMsg=!"NESTEA-FB\13\10Copyright © tailsluver29 2021"
Const Warning="You didnt type any text"
Const Copyright="This is my software"


Sub drawRect(hdc As HDC,x As Integer,y As Integer,w As Integer,h As Integer)
	
	Dim As HBRUSH whitecol = CreatePen(PS_SOLID,1,BGRA(255,255,255,0))
   Dim As HGDIOBJ oldobj = SelectObject(hdc,whitecol)
	
	
	
	MoveToEx(hdc,x,y,NULL):LineTo(hdc,x + w,y)
	MoveToEx(hdc,x + w,y, NULL):LineTo(hdc, x + w, y + h)
	MoveToEx(hdc,x + w,y + h,NULL):LineTo(hdc,x, y + h)
	MoveToEx(hdc,x, y + h,NULL):LineTo(hdc, x, y)
	
	SelectObject(hdc,oldobj)
	DeleteObject(whitecol)
	
	
End Sub

'Sub shapes
Sub DrawBuffer(SrcIMG As FB.Image Ptr, DestHWND As hWnd)
'Credits to FreeBASIC forum, VANYA, super_castle, led-bloon
 
    If SrcIMG = 0 Then Exit Sub

    Dim As BITMAPV4HEADER bmi
    Dim As Rect DestRECT
    Dim As Integer DestW, DestH, SrcW, SrcH
   
    With bmi
        .bV4Size = Len(BITMAPV4HEADER)
        .bv4width = SrcIMG->Pitch / 4
        .bv4height = -(SrcIMG->Height)
        .bv4planes = 1
        .bv4bitcount = 32
        .bv4v4compression = 0
        .bv4sizeimage = ((SrcIMG->Pitch / 4) * SrcIMG->Height) * 4
        .bV4RedMask = &h0f00
        .bV4GreenMask = &h00f0
        .bV4BlueMask = &h000f
        .bV4AlphaMask = &hf000
    End With
 
    Var hdc = GetDC(DestHWND)
    GetClientRect(DestHWND , @DestRECT )
   
    If DestRECT.Bottom > SrcIMG->Height Then
        SrcH = SrcIMG->Height       
    Else
        SrcH = DestRECT.Bottom
    End If
    DestH = SrcH
   
    If DestRECT.Right > SrcIMG->Width Then
        SrcW = SrcIMG->Width       
    Else
        SrcW = DestRECT.Right
    End If
    DestW = SrcW
   
    StretchDIBits(hDC, 0, 0, DestW, DestH, 0, 0, SrcW, SrcH, CPtr(Any Ptr, SrcIMG) + SizeOf(FB.Image), CPtr( BITMAPINFO Ptr, @bmi), DIB_RGB_COLORS, SRCCOPY )
    DeleteDC(hdc)

End Sub
Sub drawscrn(hdc As HDC)
	Dim bm As BITMAP
	Dim bmi As BITMAPINFOHEADER
	
	bmi.biSize = 40
	bmi.biPlanes = 1
	bmi.biBitCount= 240
	bmi.biCompression = 0
	
	Dim bmlen As Long
	
	bmlen = Len(bm)
	bmi.biWidth = 256
	bmi.biHeight =  240
	'GetObject(img,bmlen,bm)
	
	ReDim imagedata(256 * 240 - 1) As Long
	
	'TextOut(hdc,0,0,"test",Len("test"))
	
	
	
	   'StretchDIBits(hDC, 0, 0, 512, 480, 0, 0,bmi.biWidth ,bmi.biHeight , Buffer(0), @bmi,DIB_RGB_COLORS,SRCCOPY)
    StretchDIBits(hDC, 0, 0, 512, 480, 0, 0, 256, 240, buffer(0), CPtr( BITMAPINFO Ptr, @bmi), DIB_RGB_COLORS, SRCCOPY )
	
	
End Sub


Declare Function DlgProc(ByVal hWin As HWND, ByVal uMsg As UINT, ByVal wParam As WPARAM, ByVal lParam As LPARAM) As Integer

'''
''' Program start
'''

	''
	'' Create the Dialog
	''
	hInstance=GetModuleHandle(NULL)
	DialogBoxParam(hInstance, Cast(ZString Ptr,IDD_DLG1), NULL, @DlgProc, NULL)
	''
	'' Program has ended
	''

	ExitProcess(0)
	End

'''
''' Program end
'''
Function DlgProc(ByVal hWin As HWND,ByVal uMsg As UINT,ByVal wParam As WPARAM,ByVal lParam As LPARAM) As Integer
	Dim As Long id, Event, x, y
	'Dim hBtn As HWND
	Dim rect As RECT
	Dim dc As HDC
 


	Select Case uMsg
		
	

		Case WM_INITDIALOG
      '8
      '28
      			hIcon= LoadIcon(hInstance,MAKEINTRESOURCE(IDC_ICON1)) ' our first Win API loading icon to our created window
         SendMessage(hWin,WM_SETICON,NULL,Cast(LPARAM,hIcon)) ' our second WIN API sets the loaded icon to our dialog 
	
      
		'SetClassLongPtr( hWin, GCLP_HBRBACKGROUND, CAST(LONG_PTR, CreateSolidBrush(BGRA(255, 255, 255,0))))

					
         '8
			'32
			
		'dialog
		
		
		MoveWindow(hWin ,200,100,780+8,480+32+24,TRUE)
		
		
		Case WM_CLOSE
			EndDialog(hWin, 0)
			'
		Case WM_COMMAND
			'id=LoWord(wParam)
			'Event=HiWord(wParam)
			'S	elect Case id
			'	Case IDC_BTN1
			'		EndDialog(hWin, 0)
			'		'
			'End Select
			Select Case LoWord(wParam) 'id
				 Case IDM_EXIT
					EndDialog(hWin,0)
					
				Case IDM_LOADROM
				
				Case IDM_ABOUT
					ShellAbout(hWin,@AppName,@AboutMsg,hIcon)

				'Case BN_CLICKED,1
              
			Select Case HiWord(wParam)'event
				
				Case IDM_EXIT ' not here
					
			'		EndDialog(hWin,0)
										
		End Select
			
			End Select
			
		Case WM_SIZE
			'GetClientRect(hWin,@rect)
		'	hBtn=GetDlgItem(hWin,IDC_BTN1)
		'	x=rect.right-100
		''	y=rect.bottom-35
		'	MoveWindow(hBtn,0,0,97,31,TRUE)
		
		Case WM_PAINT
		Dim nSelectedPalette As UByte
		
	  'SetClassLongPtr( hWin, GCLP_HBRBACKGROUND, CAST(LONG_PTR, CreateSolidBrush(BGRA(255,0, 0,0))))


		Dim ps As PAINTSTRUCT
			dc = GetDC(hWin)
			
			BeginPaint(hwin,@ps)
		
	Const nSwatchSize As Integer = 6
		'FillRect(dc,@ps.rcPaint,Cast(HBRUSH,CreateSolidBrush(BGRA(128,0,0,0))))
		
	For p As Integer = 0 To 8-1
		For s As Integer = 0 To 4-1
			FillRect(dc, @Type<RECT>(516 + p * (nSwatchSize * 5) + s * nSwatchSize, 340,(516 + p * (nSwatchSize * 5) + s * nSwatchSize)+nSwatchSize, nSwatchSize+340),Cast(HBRUSH,CreateSolidBrush(BGRA(0,0,0,0))))
			
		Next
		
		
	Next
	
	
	'Rectangle(dc,516 + nSelectedPalette * (nSwatchSize * 5) - 1,339,(nSwatchSize * 4)+516 + (nSelectedPalette * (nSwatchSize * 5) - 1),nSwatchSize+339)
		'FrameRect(dc, @Type<RECT>(516 + nSelectedPalette * (nSwatchSize * 5) - 1,339,nSwatchSize, nSwatchSize),Cast(HBRUSH,CreateSolidBrush(BGRA(255,255,255,0))))
      
 '	FillRect(dc, @Type<RECT>(516 + p * (nSwatchSize * 5) + s * nSwatchSize,339,512,480),Cast(HBRUSH,CreateSolidBrush(BGRA(0,0,0,0))))



		drawRect(dc,516 + nSelectedPalette * (nSwatchSize * 5) - 1,339,(nSwatchSize*4),nSwatchSize)
	
		
			
		FillRect(dc, @Type<RECT>(0,0,512,480),Cast(HBRUSH,CreateSolidBrush(BGRA(0,0,0,0))))
		
		'	drawscrn(ps.hdc)
		
		FillRect(dc, @Type<RECT>(516,348,128+516,348+128),Cast(HBRUSH,CreateSolidBrush(BGRA(0,0,0,0))))
		FillRect(dc, @Type<RECT>(648,348,128+648,348+128),Cast(HBRUSH,CreateSolidBrush(BGRA(0,0,0,0))))
		
		TextOut(dc,
		
	
	
			EndPaint(hwin,@ps)
			  	
		Case WM_CTLCOLORDLG
			Return Cast(Long,CreateSolidBrush(BGRA(0,0,128,0)))
			
			
	'	Case WM_ERASEBKGND
			
	    'SetClassLongPtr( hWin, GCLP_HBRBACKGROUND, CAST(LONG_PTR, CreateSolidBrush(BGRA(255,0,0,0))))

	'Return TRUE
			'
		Case Else
			Return FALSE
			'
	End Select
	Return TRUE

End Function

Sub shape1(hWin As HWND)
	
	Dim hShape As HWND
Dim hctrlId As HWND 
	
	 hctrlId =  GetDlgItem(hWin,IDC_BACKGRND)
		MoveWindow(hctrlId ,0,0,780,480,TRUE)
		
		


	
	 hShape =  GetDlgItem(hWin,IDC_SHP1)
		MoveWindow(hShape,0,0,512,480,TRUE)
		
			
	 hShape =  GetDlgItem(hWin,IDC_SHP2)
		MoveWindow(hShape,516,0,264,340,TRUE)
		
		
		
		
		
		
		
		
		 hShape =  GetDlgItem(hWin,IDC_SHP13)
		MoveWindow(hShape,516,340,6,6,TRUE)
		
		hShape =  GetDlgItem(hWin,IDC_SHP9)
		MoveWindow(hShape,516+6,340,6,6,TRUE)
		
				hShape =  GetDlgItem(hWin,IDC_SHP5)
		MoveWindow(hShape,516+12,340,6,6,TRUE)
		
		
		 				hShape =  GetDlgItem(hWin, IDC_SHP6)
		MoveWindow(hShape,516+18,340,6,6,TRUE)
		

			 				hShape =  GetDlgItem(hWin, IDC_SHP15)
		MoveWindow(hShape,516+30 ,340,6,6,TRUE)
		
		
					 				hShape =  GetDlgItem(hWin, IDC_SHP14)
		MoveWindow(hShape,516+36 ,340,6,6,TRUE)
		
		
						 				hShape =  GetDlgItem(hWin, IDC_SHP20)
		MoveWindow(hShape,516+42 ,340,6,6,TRUE)
		
							 				hShape =  GetDlgItem(hWin, IDC_SHP7)
		MoveWindow(hShape,516+48 ,340,6,6,TRUE)
		
									 				hShape =  GetDlgItem(hWin, IDC_SHP19)
		MoveWindow(hShape,516+60 ,340,6,6,TRUE)
		
		
										 				hShape =  GetDlgItem(hWin, IDC_SHP8 )
		MoveWindow(hShape,516+66 ,340,6,6,TRUE)
		
												 				hShape =  GetDlgItem(hWin, IDC_SHP16 )
		MoveWindow(hShape,516+72,340,6,6,TRUE)
		
		
												 				hShape =  GetDlgItem(hWin, IDC_SHP18 )
		MoveWindow(hShape,516+78,340,6,6,TRUE)
		
		
													 				hShape =  GetDlgItem(hWin, IDC_SHP11 )
		MoveWindow(hShape,516+90,340,6,6,TRUE)

			
													 				hShape =  GetDlgItem(hWin, IDC_SHP17 )
		MoveWindow(hShape,516+96,340,6,6,TRUE)
		
				
													 				hShape =  GetDlgItem(hWin, IDC_SHP10)
		MoveWindow(hShape,516+102,340,6,6,TRUE)
		
						
													 				hShape =  GetDlgItem(hWin, IDC_SHP12)
		MoveWindow(hShape,516+108,340,6,6,TRUE)
		
		
		
		
		
				
		 hShape =  GetDlgItem(hWin,IDC_SHP25)
		MoveWindow(hShape,636,340,6,6,TRUE)
		
		hShape =  GetDlgItem(hWin,IDC_SHP26)
		MoveWindow(hShape,636+6,340,6,6,TRUE)
		
				hShape =  GetDlgItem(hWin,IDC_SHP27)
		MoveWindow(hShape,636+12,340,6,6,TRUE)
		
		
		 				hShape =  GetDlgItem(hWin, IDC_SHP28)
		MoveWindow(hShape,636+18,340,6,6,TRUE)
		



			 				hShape =  GetDlgItem(hWin, IDC_SHP29)
		MoveWindow(hShape,636+30 ,340,6,6,TRUE)
		
		'
					 				hShape =  GetDlgItem(hWin, IDC_SHP30)
		MoveWindow(hShape,636+36 ,340,6,6,TRUE)
		'
		'
					 				hShape =  GetDlgItem(hWin, IDC_SHP31)
		MoveWindow(hShape,636+42 ,340,6,6,TRUE)
		'
					 				hShape =  GetDlgItem(hWin, IDC_SHP32)
		MoveWindow(hShape,636+48 ,340,6,6,TRUE)
		
		
		
		
		'
									 				hShape =  GetDlgItem(hWin, IDC_SHP21)
		MoveWindow(hShape,636+60 ,340,6,6,TRUE)
		'
		'
										 				hShape =  GetDlgItem(hWin, IDC_SHP22 )
		MoveWindow(hShape,636+66 ,340,6,6,TRUE)
		
		
		
												 				hShape =  GetDlgItem(hWin, IDC_SHP23 )
		MoveWindow(hShape,636+72,340,6,6,TRUE)
		
		
			
											 				hShape =  GetDlgItem(hWin, IDC_SHP24 )
		MoveWindow(hShape,636+78,340,6,6,TRUE)
		
		
		
													 				hShape =  GetDlgItem(hWin, IDC_SHP33 )
		MoveWindow(hShape,636+90,340,6,6,TRUE)

			
													 				hShape =  GetDlgItem(hWin, IDC_SHP34 )
		MoveWindow(hShape,636+96,340,6,6,TRUE)
		'
		'		
												 				hShape =  GetDlgItem(hWin, IDC_SHP35)
		MoveWindow(hShape,636+102,340,6,6,TRUE)
		'
		'				
													 				hShape =  GetDlgItem(hWin, IDC_SHP36)
		MoveWindow(hShape,636+108,340,6,6,TRUE)
		
		
	 hShape =  GetDlgItem(hWin,IDC_SHP37)
		MoveWindow(hShape,516-1,340-1,(6*4)+1,6+1,TRUE)
		
		
			
		 	 hShape =  GetDlgItem(hWin,IDC_SHP3)
		
	MoveWindow(hShape,516,376-28,128,128,TRUE)
		
		 	 hShape =  GetDlgItem(hWin,IDC_SHP4)
		
	MoveWindow(hShape,516+132,376-28,128,128,TRUE)
			
	
	
	
	
	
	
	
		
End Sub





