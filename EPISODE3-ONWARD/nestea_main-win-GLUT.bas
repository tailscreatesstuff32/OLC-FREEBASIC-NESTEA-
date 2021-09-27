#include once "GL/gl.bi"
    9 #include once "GL/glu.bi"
   10 #include once "GL/glut.bi"
   11 
   12 ''
   13 declare sub         doMain           ( )
   14 declare sub         doShutdown       ( )
   15 
   16 
   17 
   18     ''
   19     '' Entry point
   20     ''
   21     doMain
   22     
   23 
   24     
   25 
   26 '' ::::::::::::
   27 '' name: doRender
   28 '' desc: Is called by glut to render scene
   29 ''
   30 '' ::::::::::::
   31 sub doRender cdecl
   32     static rtri as single
   33     static rqud as single
   34     
   35     glClear GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT
   36     glPushMatrix
   37     
   38     glLoadIdentity
   39     glTranslatef -1.5, 0.0, -6.0
   40     glRotatef rtri, 0, 1, 0
   41          
   42     glBegin GL_TRIANGLES
   43         glColor3f   1.0, 0.0, 0.0           '' Red
   44         glVertex3f  0.0, 1.0, 0.0           '' Top Of Triangle  Front)
   45         glColor3f   0.0, 1.0, 0.0           '' Green
   46         glVertex3f -1.0,-1.0, 1.0           '' Left Of Triangle  Front)
   47         glColor3f   0.0, 0.0, 1.0           '' Blue
   48         glVertex3f  1.0,-1.0, 1.0           '' Right Of Triangle  Front)
   49         glColor3f   1.0, 0.0, 0.0           '' Red
   50         glVertex3f  0.0, 1.0, 0.0           '' Top Of Triangle  Right)
   51         glColor3f   0.0, 0.0, 1.0           '' Blue
   52         glVertex3f  1.0,-1.0, 1.0           '' Left Of Triangle  Right)
   53         glColor3f   0.0, 1.0, 0.0           '' Green
   54         glVertex3f  1.0,-1.0,-1.0           '' Right Of Triangle  Right)
   55         glColor3f   1.0, 0.0, 0.0           '' Red
   56         glVertex3f  0.0, 1.0, 0.0           '' Top Of Triangle  Back)
   57         glColor3f   0.0, 1.0, 0.0           '' Green
   58         glVertex3f  1.0,-1.0,-1.0           '' Left Of Triangle  Back)
   59         glColor3f   0.0, 0.0, 1.0           '' Blue
   60         glVertex3f -1.0,-1.0,-1.0           '' Right Of Triangle  Back)
   61         glColor3f   1.0, 0.0, 0.0           '' Red
   62         glVertex3f  0.0, 1.0, 0.0           '' Top Of Triangle  Left)
   63         glColor3f   0.0, 0.0, 1.0           '' Blue
   64         glVertex3f -1.0,-1.0,-1.0           '' Left Of Triangle  Left)
   65         glColor3f   0.0, 1.0, 0.0           '' Green
   66         glVertex3f -1.0,-1.0, 1.0           '' Right Of Triangle  Left)
   67     glEnd
   68     
   69     glColor3f 0.5, 0.5, 1.0
   70     glLoadIdentity    
   71     glTranslatef -1.5, 0.0, -6.0
   72     glTranslatef 3.0,0.0,0.0    
   73     glRotatef rqud, 1.0, 0.0, 0.0
   74     
   75     glBegin GL_QUADS
   76         glVertex3f -1.0, 1.0, 0.0
   77         glVertex3f  1.0, 1.0, 0.0
   78         glVertex3f  1.0,-1.0, 0.0
   79         glVertex3f -1.0,-1.0, 0.0
   80     glEnd    
   81 
   82     glPopMatrix            
   83     glutSwapBuffers
   84     
   85     rtri = rtri + 2.0
   86     rqud = rqud + 1.5
   87     
   88 end sub
   89 
   90 
   91 
   92 '' ::::::::::::
   93 '' name: doInput
   94 '' desc: Handles input
   95 ''
   96 '' ::::::::::::
   97 sub doInput CDECL ( byval kbcode as unsigned byte, _
   98               byval mousex as integer, _
   99               byval mousey as integer )
  100               
  101     if ( kbcode = 27 ) then
  102         doShutdown
  103         end 0
  104     end if
  105 
  106 end sub
  107 
  108 
  109 
  110 '' ::::::::::::
  111 '' name: doInitGL
  112 '' desc: Inits OpenGL
  113 ''
  114 '' ::::::::::::
  115 sub doInitGL
  116     dim i as integer
  117     dim lightAmb(3) as single
  118     dim lightDif(3) as single
  119     dim lightPos(3) as single
  120     
  121     ''
  122     '' Rendering stuff
  123     ''
  124     glShadeModel GL_SMOOTH
  125     glClearColor 0.0, 0.0, 0.0, 0.5
  126     glClearDepth 1.0
  127     glEnable GL_DEPTH_TEST
  128     glDepthFunc GL_LEQUAL
  129     glEnable GL_COLOR_MATERIAL
  130     glHint GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST
  131     
  132     ''
  133     '' Light setup ( not used at the moment )
  134     ''
  135     for i = 0 to 3
  136         lightAmb(i) = 0.5
  137         lightDif(i) = 1.0
  138         lightPos(i) = 0.0
  139     next i
  140 
  141     lightAmb(3) = 1.0
  142     lightPos(2) = 2.0
  143     lightPos(3) = 1.0    
  144     
  145     glLightfv GL_LIGHT1, GL_AMBIENT, @lightAmb(0)
  146     glLightfv GL_LIGHT1, GL_DIFFUSE, @lightDif(0)
  147     glLightfv GL_LIGHT1, GL_POSITION,@lightPos(0)
  148     glEnable GL_LIGHT1
  149 
  150     ''
  151     '' Blending ( not used at the moment )
  152     ''
  153     glColor4f 1.0, 1.0, 1.0, 0.5
  154     glBlendFunc GL_SRC_ALPHA, GL_ONE
  155 
  156 end sub
  157 
  158 
  159 
  160 '' ::::::::::::
  161 '' name: doReshapeGL
  162 '' desc: Reshapes GL window
  163 ''
  164 '' ::::::::::::
  165 sub doReshapeGL CDECL ( byval w as integer, _
  166                         byval h as integer )
  167     
  168     glViewport 0, 0, w, h 
  169     glMatrixMode GL_PROJECTION
  170     glLoadIdentity
  171     
  172     if ( h = 0 ) then
  173         gluPerspective  80/2, w, 1.0, 5000.0 
  174     else
  175         gluPerspective  80/2, w / h, 1.0, 5000.0
  176     end if
  177     
  178     glMatrixMode GL_MODELVIEW
  179     glLoadIdentity
  180 
  181 end sub
  182 
  183 '':::::
  184 sub initGLUT
  185     ''
  186     '' Setup glut
  187     ''
  188     glutInit 1, strptr( " " )    
  189     
  190     glutInitWindowPosition 0, 0
  191     glutInitWindowSize 640, 480
  192     glutInitDisplayMode GLUT_RGBA or GLUT_DOUBLE or GLUT_DEPTH
  193     glutCreateWindow "FreeBASIC OpenGL example"
  194     
  195     doInitGL
  196     
  197     glutDisplayFunc  @doRender
  198     glutIdleFunc     @doRender
  199     glutReshapeFunc  @doReshapeGL
  200     glutKeyboardFunc @doInput
  201 
  202 end sub
  203 
  204 '':::::
  205 sub doInit
  206     
  207     ''
  208     '' Init GLUT
  209     ''
  210     initGLUT    
  211     
  212 end sub
  213 
  214 '':::::
  215 sub shutdownGLUT
  216 
  217     '' GLUT shutdown will be done automatically by atexit()
  218 
  219 end sub
  220 
  221 '':::::
  222 sub doShutdown
  223     
  224     ''
  225     '' GLUT
  226     ''
  227     shutdownGLUT
  228     
  229 end sub
  230 
  231 '' ::::::::::::
  232 '' name: doMain
  233 '' desc: Main routine
  234 ''
  235 '' ::::::::::::
  236 sub doMain
  237     
  238     ''
  239     '' 
  240     ''
  241     doInit
  242     
  243     ''
  244     ''
  245     ''
  246     glutMainLoop
  247     
  248 end Sub