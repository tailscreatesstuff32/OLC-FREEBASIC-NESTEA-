#include "fbgfx.bi"

Type Font
    '' Our font buffer.
  Buf     As FB.Image Ptr
    '' Font header.
  Hdr     As UByte Ptr
  
    '' Current font color.
  Col     As UInteger
  
    '' Make our font buffer.
  Declare Sub Make( ByVal _Col_ As UInteger = RGB(255, 255, 255) )
    '' Change the font color and edit the font buffer.
    '' Return the new font.
  Declare Function myFont( ByVal _Col_ As UInteger = RGB(255, 255, 255) ) As FB.Image Ptr
  
    '' Create/Destroy our font.
      '' Set a default color to it if you like.
  Declare Constructor( ByVal _Col_ As UInteger = RGB(255, 255, 255) )
  Declare Destructor()
End Type

  '' Create our font's buffer.
Constructor Font( ByVal _Col_ As UInteger = RGB(255, 255, 255) )
  This.Make( _Col_ )
End Constructor

  '' Destroy font buffer.
Destructor Font()
  ImageDestroy( Buf )
End Destructor

  '' Assign the FBgfx font into our font buffer.
Sub Font.Make( ByVal _Col_ As UInteger = RGB(255, 255, 255) )
    '' No image buffer data.  Create it.
  If This.Buf = 0 Then
  
      '' No screen created yet.
    If ScreenPtr = 0 Then Exit Sub
    
      '' Support 256 characters, 8 in width.
      '' Add the extra row for the font header.
    This.Buf = ImageCreate( 256 * 8, 9 )
    
      '' Get the address of the font header,
      '' which is the same as getting our pixel address
      '' Except that we always will use a ubyte.
    This.Hdr = Cast(UByte Ptr, This.Buf) + SizeOf(FB.Image)
    
      '' Assign header information.
    This.Hdr[0] = 0
      '' First supported character
    This.Hdr[1] = 0
      '' Last supported character
    This.Hdr[2] = 255
  Else
    If This.Col = _Col_ Then Exit Sub
    
  End If
  
    '' Draw our font.
  For DoVar As Integer = 0 To 255
      '' Set font width information.
    This.Hdr[3 + DoVar] = 8
    
    Draw String This.Buf, (DoVar * 8, 1), Chr(DoVar), _Col_
  Next
  
    '' Remember our font color.
  This.Col = _Col_
End Sub

  '' Get the buffer for our font.
  '' Remake the font if the color's different.
Function Font.myFont( ByVal _Col_ As UInteger = RGB(255, 255, 255) ) As FB.Image Ptr
    '' If our colors match, just return the current buffer.
  If _Col_ = Col Then
    Return Buf
  End If
  
    '' Make the font with a new color.
  This.Make( _Col_ )
    '' Return out buffer.
  Return This.Buf
End Function


  '' MAIN CODE HERE!
ScreenRes 640, 480, 32

  '' Create our font.
Dim As Font myFont = RGB(255, 255, 255)

  '' Draw a string using our custom font.
Draw String (0,0), "Hello.  I am the custom font.",, myFont.myFont()
  '' Gasp.  A new color!
Draw String (0,8), "Hello.  I am the custom font.",, myFont.myFont(RGB(255, 0, 0))
Sleep

  '' Speed test.  Turns out it's quite slow.
Scope
  Randomize Timer
    '' Our timer.
  Dim As Double T = Timer
  
    '' Time how long it takes to make a new font this way.
  For DoVar As Integer = 0 To 499
    myFont.Make( RGB(Rnd * 255, Rnd * 255, Rnd * 255) )
  Next
  
    '' And we're all done.  Print important data.
  Locate 3, 1
  Print "Time to Re-Draw font 499 times: " & ( Timer - T )
  Print "Time per Re-Draw: " & ( Timer - T ) / 500
  Sleep
End Scope