#include "GL/gl.bi"
#include "GL/glu.bi"
#Include "GL/glut.bi"
'#include <stdlib.h>
'#include <stdio.h>

#define checkImageWidth 64
#define checkImageHeight 64
Dim Shared checkImage(checkImageHeight,checkImageWidth,3) As GLubyte 

static Shared zoomFactor  As GLdouble = 1.0
Static Shared  height As GLint 

Sub makeCheckImage()
 
   Dim As integer i, j, c 
    
  For i = 0 To checkImageHeight-1
      for  j =  0 To checkImageWidth-1  
         c = ((((i and &H8)=0)xor((j And &H8))= 0))*255 
         checkImage(i,j,0) = 255 'cast(GLubyte, c )
         checkImage(i,j,1) = 255'cast(GLubyte, c) 
         checkImage(i,j,2) = 255'cast(GLubyte, c )
     Next
   Next
 End sub

Sub init( )
    
   glClearColor (0.0, 0.0, 0.0, 0.0)
   glShadeModel(GL_FLAT)
   makeCheckImage()
   glPixelStorei(GL_UNPACK_ALIGNMENT, 1)
End Sub

sub display cdecl( )
 
   glClear(GL_COLOR_BUFFER_BIT)
   glRasterPos2i(0, 0)
   glDrawPixels(checkImageWidth, checkImageHeight, GL_RGB, _
                GL_UNSIGNED_BYTE, @checkImage(0,0,0))
   glFlush()
End Sub

sub reshape Cdecl(w As integer, h As integer)
 
   glViewport(0, 0, cast(GLsizei,w), Cast(GLsizei,h)) 
   height = Cast(GLint,h) 
   glMatrixMode(GL_PROJECTION) 
   glLoadIdentity() 
   gluOrtho2D(0.0, cast(GLdouble,w), 0.0, Cast(GLdouble, h)) 
   glMatrixMode(GL_MODELVIEW) 
   glLoadIdentity() 
End sub

Sub motion Cdecl(x As Integer, y As Integer)
 
   static screeny As GLint 
   
   screeny = height - cast(GLint, y)
   glRasterPos2i (x, screeny) 
   glPixelZoom (zoomFactor, zoomFactor) 
   glCopyPixels (0, 0, checkImageWidth, checkImageHeight, _
                 GL_COLOR)  
   glPixelZoom (1.0, 1.0) 
   glFlush () 
End Sub

Sub keyboard Cdecl( key As String,  x As integer,   y As Integer)
 
   Select Case (key)  
      case "r"
      case "R"
         zoomFactor = 1.0 
         glutPostRedisplay() 
         'printf ("zoomFactor reset to 1.0\n") 
       '
      case "z" 
         zoomFactor += 0.5 
         if (zoomFactor >= 3.0)  Then
            zoomFactor = 3.0 
      '  printf ("zoomFactor is now %4.1f\n", zoomFactor);
         End if
      case "Z"
         zoomFactor -= 0.5 
         if (zoomFactor <= 0.5) Then
            zoomFactor = 0.5 
       '  printf ("zoomFactor is now %4.1f\n", zoomFactor);
          End If
   	'case Else 
        ' exit(0) 
         
    
 End Select
End Sub

Sub main()
 
   glutInit(1, StrPtr( " " )) 
   glutInitDisplayMode(GLUT_SINGLE Or GLUT_RGB)
   glutInitWindowSize(250, 250) 
   glutInitWindowPosition(100, 100) 
   glutCreateWindow("pixelz") 
   init() 
   glutDisplayFunc(@display) 
   glutReshapeFunc(@reshape) 
   glutKeyboardFunc(@keyboard) 
   glutMotionFunc(@motion) 
   glutMainLoop() 
    
End Sub

main()