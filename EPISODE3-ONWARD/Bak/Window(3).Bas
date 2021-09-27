/'
	Dialog Example, by fsw

	compile with:	fbc -s gui dialog.rc dialog.bas

'/

#Include Once "windows.bi"

#Include Once "win/shellapi.bi"

#Include Once "crt/mem.bi"
#Include Once "crt/stdlib.bi"



#Include  "containers/MAP.bi"

#Include "fbgfx.bi"
#Include "Window.bi"


Dim Shared img As HBITMAP
Dim Shared buffer() As Long
Dim Shared testimg As FB.image
Dim Shared destimg As FB.image

 Dim Shared hfont As HFONT

Const ClassName="DLGCLASS"
Const AppName="NESTEA-FB"
Const AboutMsg=!"NESTEA-FB\13\10Copyright © tailsluver29 2021"
Const Warning="You didnt type any text"
Const Copyright="This is my software"
  


MMapTemplate(UInteger,string)
'dim Shared mapasm as TMAPUINTEGERSTRING
Sub DrawString(hdc As hdc,txt As String,x As Integer,y As Integer,col As COLORREF)
		SetBkMode(hdc,TRANSPARENT)
		SetBkColor(hdc,BGRA(0,0,0,0))
			
			SetTextColor(hdc,col )
	TextOut(hdc,x,y,txt,Len(txt))
End Sub

Sub drawcode(hdc As hdc,x As Integer,y As Integer,nlines As Integer)
	
		'Dim mapasm = 

	'Var it_a = mapasm.find(pc)
	Dim As Integer nliney = (nlines Shr 1)*10+y
	
	'If it_a <> 0 Then
		'DrawString(hdc,x,nliney,ita->nData,BGRA(255,0,255))
		DrawString(hdc,"$0000: BRK #$00 {IMM}",x,nliney,BGRA(0,255,255,0))
		While (nliney < (nlines *10) + y)
			
	   nliney += 10
	'If it_a <> 0 Then
		
		DrawString(hdc,"$0000: BRK #$00 {IMM}",x,nliney,BGRA(255,255,255,0))
		
'	EndIf
		Wend
		
	'EndIf
	nliney = (nlines Shr 1)*10+y
		While (nliney > y)
			
	   nliney -= 10
	'If it_a <> 0 Then
		
		DrawString(hdc,"$0000: BRK #$00 {IMM}",x,nliney,BGRA(255,255,255,0))
		
'	EndIf
		Wend
		
	
End Sub
Enum FontOptionsEnum
    Bold          = &b1
    Italic        = &b10
    Underline     = &b100
    StrikeThrough = &b1000
    AntiAliased   = &b10000
End Enum

Function MakeFont( ByVal FontName As String = "Arial", _
                   ByVal FontSize As uInteger = 12, _
                   ByVal FontOptions As uInteger = 0 _
                   ) As HFONT
    Dim nHeight As uInteger
    Dim nWidth As uInteger
    Dim nEscapement As uInteger
    Dim nOrientation As uInteger
    Dim fnWeight As uInteger
    Dim fdwItalic As uInteger
    Dim fdwUnderline As uInteger
    Dim fdwStrikeOut As uInteger
    Dim fdwCharSet As uInteger
    Dim fdwOutputPrecision As uInteger
    Dim fdwClipPrecision As uInteger
    Dim fdwQuality As uInteger
    Dim fdwPitchAndFamily As uInteger
   
    Dim hFont As HFONT
    Dim hDC As HDC
   
    hDC = GetDC(HWND_DESKTOP)
    nHeight = FontSize '-MulDiv(FontSize, GetDeviceCaps(hDC, LOGPIXELSY), 72)' "pt" sized
    nWidth = 0
    ReleaseDC HWND_DESKTOP, hDC
   
    nEscapement = NULL
    nOrientation = NULL
    If FontOptions And Bold          Then fnWeight     = 700  Else fnWeight = 100
    If FontOptions And Italic        Then fdwItalic    = TRUE
    If FontOptions And Underline     Then fdwUnderline = TRUE
    If FontOptions And StrikeThrough Then fdwStrikeOut = TRUE
   
    fdwQuality = NONANTIALIASED_QUALITY 
    fdwCharSet = NULL
    fdwOutputPrecision = NULL
    fdwClipPrecision = NULL
   ' fdwQuality = NULL
    fdwPitchAndFamily = NULL
   
    hFont = CreateFont(             _
        nHeight,                    _
        nWidth,                     _
        nEscapement,                _
        nOrientation,               _
        fnWeight,                   _
        fdwItalic,                  _
        fdwUnderline,               _
        fdwStrikeOut,               _
        fdwCharSet,                 _
        fdwOutputPrecision,         _
        fdwClipPrecision,           _
        fdwQuality,                 _
        fdwPitchAndFamily,          _
        StrPtr(FontName)            _
    )
   
    Return hFont
End Function


Sub drawCPU (hdc As hdc,x As Integer,y As Integer)
	
	
	'TODO add the status regs
	
	DrawString(hdc,"STATUS: ",x,y,BGRA(255,255,255,0))
	
	DrawString(hdc,"N",x+64,y,IIf(0,BGRA(0,0,255,0),BGRA(255,0,0,0)))
	DrawString(hdc,"V",x+80,y,IIf(0,BGRA(0,0,255,0),BGRA(255,0,0,0)))
	DrawString(hdc,"-",x+96,y,IIf(1,BGRA(0,255,0,0),BGRA(255,0,0,0)))
	DrawString(hdc,"B",x+112,y,IIf(0,BGRA(0,255,0,0),BGRA(255,0,0,0)))
	DrawString(hdc,"D",x+128,y,IIf(0,BGRA(0,255,0,0),BGRA(255,0,0,0)))
	DrawString(hdc,"I",x+144,y,IIf(0,BGRA(0,255,0,0),BGRA(255,0,0,0)))
	DrawString(hdc,"Z",x+160,y,IIf(0,BGRA(0,255,0,0),BGRA(255,0,0,0)))
	DrawString(hdc,"C",x+176,y,IIf(0,BGRA(0,255,0,0),BGRA(255,0,0,0)))
	
	DrawString(hdc,"PC: $"+"0000",x,y+10,BGRA(255,255,255,0))
	DrawString(hdc,"A: $"+"00"+"  "+"[0]",x,y+20,BGRA(255,255,255,0))
	DrawString(hdc,"X: $"+"00"+"  "+"[0]",x,y+30,BGRA(255,255,255,0))
	DrawString(hdc,"Y: $"+"00"+"  "+"[0]",x,y+40,BGRA(255,255,255,0))
	DrawString(hdc,"Stack P: $"+"0000",x,y+50,BGRA(255,255,255,0))
	'
End Sub



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


 'TODO do i need this duplicate?
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
		
       hfont = MakeFont("Modern DOS 8x8",8)
									
     
		'dialog
		MoveWindow(hWin ,200,100,780+8,480+32+24,TRUE)
		
		Case WM_CLOSE
			EndDialog(hWin, 0)
			'
		Case WM_COMMAND
		
			Select Case LoWord(wParam) 'id
				Case IDM_EXIT
					
					EndDialog(hWin,0)
					
				Case IDM_LOADROM
				
				Case IDM_ABOUT
					ShellAbout(hWin,@AppName,@AboutMsg,hIcon)

	
			Select Case HiWord(wParam)'event
				
				Case IDM_EXIT ' not here

										
		End Select
			
			End Select
			
		Case WM_CREATE
	
			
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
				FillRect(dc,@ps.rcPaint,Cast(HBRUSH,CreateSolidBrush(BGRA(0,0,128,0))))
		SelectObject(dc ,hfont)
		'	drawscrn(ps.hdc)
		drawCPU(dc,516,2)
	drawcode(dc,516, 72, 26)
	Const nSwatchSize As Integer = 6
	
	
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
	

		FillRect(dc, @Type<RECT>(516,348,128+516,348+128),Cast(HBRUSH,CreateSolidBrush(BGRA(0,0,0,0))))
		FillRect(dc, @Type<RECT>(648,348,128+648,348+128),Cast(HBRUSH,CreateSolidBrush(BGRA(0,0,0,0))))
			
		
	
		
	
	

			EndPaint(hwin,@ps)
			  	
		Case WM_CTLCOLORDLG
			Return Cast(Long,CreateSolidBrush(BGRA(0,0,128,0)))
			
		Case WM_DESTROY
			
	'	Case WM_ERASEBKGND

				DeleteObject(hfont)
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





