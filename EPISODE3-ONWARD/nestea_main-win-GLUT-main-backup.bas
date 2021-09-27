 #include once "GL/glut.bi"
  #Include once  "crt.bi"
  
  #Define FPS 60
  
  Dim Shared time1 As Integer  
  time1=glutGet(GLUT_ELAPSED_TIME)
   Dim Shared timebase As Integer
  '  Dim Shared initial_time  As Integer 
  'initial_time = Time_(NULL) 
  Dim Shared frame_count As Integer
  Dim Shared cycle As Integer
  Dim Shared scanline As Integer
  Dim Shared frame_complete As BOOLEAN = false
  'Dim Shared final_time As  integer
  
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
 	
 	Do
 	glBegin(GL_POINTS):  	glColor3f(Rnd*1,Rnd*1,Rnd*1):  glVertex2i(cycle-1,scanline): glEnd()
 	cycle+=1
 	
	If (cycle >= 341) then
 
		cycle = 0 
		scanline+=1
		if (scanline >= 261) Then
		 
			scanline = -1
			frame_complete = TRUE
			 
		End If
	End If
 	Loop While Not(frame_complete)
 	frame_complete = false
 	
	
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
 	 glFlush()
 	
 		 frame_count+=1
	 time1 = glutGet(GLUT_ELAPSED_TIME)
	 if (time1 - timebase > 1000)  Then
	 	cls
		Print "FPS: "; Cast(Integer,frame_count*1000.0/(time1-timebase))
		timebase = time1 
		frame_count = 0 
	End If
	 
 End Sub

 
  Sub init()
 glClearColor(0.3,0.3,0.3,0) 
 gluOrtho2D(0,256,240,0) 
 
 End Sub

  Sub main()
 glutInit 1, strptr( " " ) 
 glutInitDisplayMode(GLUT_SINGLE Or GLUT_RGB)

 glutInitWindowSize(512,480) 
 glutCreateWindow("test")
 init() 
 glutDisplayFunc(@display) 
 glutReshapeFunc(@resizewin)
 'glutIdleFunc(@idle1) 
 glutTimerFunc(1000/FPS,@idle1,0)
 'glutKeyboardFunc(Buttons)

 glutMainLoop() 
 	
 	
 	
 	
 	
 	
 End Sub
 

 main
 
 
 
 
 

 
 
 
 
 

 

 
 