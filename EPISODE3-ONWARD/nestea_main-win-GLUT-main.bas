 #include once "GL/glut.bi"
  #Include once  "crt.bi"
  #Include Once "nes/nes.bi"
  
  
  #Define FPS 60
  
  Dim Shared time1 As Integer  
  time1=glutGet(GLUT_ELAPSED_TIME)
   Dim Shared timebase As Integer
  '  Dim Shared initial_time  As Integer 
  'initial_time = Time_(NULL) 
  Dim Shared frame_count As Integer
  
  Dim Shared bEmulationRun As bool '= TRUE
  'Dim Shared cycle As Integer
  'Dim Shared scanline As Integer
  'Dim Shared frame_complete As BOOLEAN = false
  'Dim Shared final_time As  integer
  

Sub KeyboardHandler Cdecl(Key As Ubyte, x As Integer, y As Integer)
  Select Case Key
     Case 27 ' [ESCAPE]
      End
     Case Asc(" ") 
       bEmulationRun = Not(bEmulationRun)
     Case Asc("f") 
    		'	// Clock enough times to draw a single frame
				Do:  bus_clock():  Loop while not(complete())
				'// Use residual clock cycles to complete current instruction
				Do:   bus_clock():    Loop while not(complete())
				'// Reset frame completion flag
				'frame_complete = false 
  	  Case Asc("c") 
       	Do:  bus_clock():  Loop while Not(complete()) 
				'// CPU clock runs slower than system clock, so it may be
				'// complete for additional system clock cycles. Drain
				'// those out
			Do:  bus_clock():  Loop while (complete()) 
     Case Asc("r") 
    bus_reset()
        
    Case Else
      beep                  '
  End Select
  glutPostRedisplay()
End Sub

   Sub GL_fillrect(x1 As Integer,y1 As Integer,x2 As Integer,y2 As Integer)
 	
 	
	 glBegin(GL_QUADS) 
	 
glVertex2i(x1,y1) ' top left
glVertex2i(x1+x2,y1)' top right
glVertex2i(x1+x2,y1+y2) ' bottom left
glVertex2i(x1,y1+y2) ' bottom right
	 glEnd()          
 	
 End Sub
Sub display Cdecl
	
	
	 
	
	 
     

'GL_fillrect(125,100,250,250)

      
      glPointSize(2)
 '     
 '    For y1 As Integer = 0 To 240/2 
 '     For x1 As integer = 0 To 256/2
 '          glBegin(GL_POINTS):glColor3f(1,1,0): glVertex2i(x1*2,y1*2): glEnd()
 '     Next

 'Next   
 
 
  glClear(GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT)
 
  
 	 If bEmulationRun Then
 		 		Do:  bus_clock():  Loop while not(frame_complete) 
				frame_complete = false 
 		 	
 		 	
 		 	
 		 Else
 		 	
 		 	
 	 EndIf
 	'Do
 	'glBegin(GL_POINTS):  	glColor3f(Rnd*1,Rnd*1,Rnd*1):  glVertex2i(cycle-1,scanline): glEnd()
 	'cycle+=1
 	'
	'If (cycle >= 341) then
 
	'	cycle = 0 
	'	scanline+=1
	'	if (scanline >= 261) Then
	'	 
	'		scanline = -1
	'		frame_complete = TRUE
	'		 
	'	End If
	'End If
 	'Loop While Not(frame_complete)
 	'frame_complete = false
 	
	
	 '    For y1 As Integer = 0 To 240
     ' For x1 As integer = 0 To 256
    
       ' GL_fillrect(x1,y1,2,2)
   
      'Next

' Next

	  
	  'glutSwapBuffers()


	 
	 'final_time = Time_(NULL) 
	 'If final_time - initial_time > 0 Then
	 '	cls
	 'Print "FPS: "; frame_count/(final_time - initial_time)
	 'frame_count = 0
	 'initial_time = final_time 
	 'End If
	 
End Sub 
 
 Sub resizewin Cdecl (x1 As integer,y1 As Integer)
 	' glViewport(0,0,256,240)
 	
 	
 	glutReshapeWindow( 512,480)
 End Sub

  Sub idle1 Cdecl()
 	 
 	glutpostredisplay()
 		
 	glutTimerFunc(1000/FPS,@idle1,0)

 	
 		 frame_count+=1
 	 glutSetWindowTitle(hex1(pc,4) )	 
 		' glutSetWindowTitle(hex1(pc,8) )
'glutSetWindowTitle(hex1(pc,4) )
 		 
	 time1 = glutGet(GLUT_ELAPSED_TIME)
	 if (time1 - timebase > 1000)  Then
	 	cls
		Print "FPS: "; Cast(Integer,frame_count*1000.0/(time1-timebase))
		'Print RGB(255,0,0)
		timebase = time1 
		frame_count = 0 
	End If
	  	 glFlush()
 End Sub

 
  Sub init()
  	
  	  insert_cartridge("nestest.nes")
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
  	
 glClearColor(0.3,0.3,0.3,0) 
 gluOrtho2D(0,256,240,0) 
 glClear(GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT)
 
 bus_reset
 End Sub

  Sub main()
  	
  	
  	
 glutInit 1, strptr( " " ) 
 glutInitDisplayMode(GLUT_SINGLE Or GLUT_RGB)

 glutInitWindowSize(512,480) 
 glutCreateWindow("test")

 init() 
 glutSetWindowTitle(hex1(pc,4) )
 glutDisplayFunc(@display) 
 glutReshapeFunc(@resizewin)
 'glutIdleFunc(@idle1) 
 glutTimerFunc(1000/FPS,@idle1,0)
 glutKeyboardFunc(@KeyboardHandler)

 glutMainLoop() 
 	
 	
 	
 	
 	
 	
 End Sub
 

 main
 
 
 
 
 

 
 
 
 
 

 

 
 