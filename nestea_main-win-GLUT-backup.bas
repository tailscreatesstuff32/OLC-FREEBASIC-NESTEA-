' #include once "GL/gl.bi"
 '#include once "GL/glu.bi"
 #include once "GL/glut.bi"
 
 Sub resizefunc Cdecl (w As GLsizei,h As GLsizei)
 	glViewport(0, 0, Cast(GLsizei,w),  Cast(GLsizei,h))
 	
 	glmatrixmode(GL_PROJECTION)
 	glLoadIdentity()
 	
 	'glutPostRedisplay()
 End Sub
 
 sub doRender Cdecl
 
 	   
 		glClear( GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT)
 	
 	glBegin(GL_TRIANGLES)
   glColor3f(1,0,0)
   glVertex2f(-0.5,-0.5)
   glColor3f(0,1,0)
   glVertex2f(0.5,-0.5)
   glColor3f(0,0,1)
    glVertex2f(0.0,0.5)
   glEnd
   ']
   '	glBegin(GL_POINTS)
   '	
   'glColor3f(1,0,0)
   'glVertex2f(-0.5,-0.5)

   glEnd
   
   
   
   
 	glutSwapBuffers()
 	
 	
 End Sub
 
 Sub main()
   glutInit 1, strptr( " " ) 
   glutInitDisplayMode(GLUT_DEPTH Or GLUT_DOUBLE Or GLUT_RGBA)
   glutInitWindowPosition(100,100)
   glutInitWindowSize(640,480)
   glutCreateWindow("Simple GLUT Application")
   glClearColor(0,0,0,1)
   
   
   glutDisplayFunc(@doRender)
   glutReshapeFunc(@resizefunc)
 '  glutIdleFunc(@doRender) 
   glutMainLoop()
   
   
 End Sub
  
  Sub keyboard()
  	
  	
  	
  	
  End Sub

 
 main()
 