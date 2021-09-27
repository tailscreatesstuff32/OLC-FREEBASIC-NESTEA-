

''Set up a graphics screen
Const W = 640, H = 480
ScreenRes W, H


Width W/8,h/16
Dim As Integer twid, tw, th

'' Fetch and print current text width/height:
twid = Width()
tw = LoWord(twid): th = HiWord(twid)
Print "Default for current screen (8*8)"
Print "Width:  " & tw
Print "Height: " & th
Sleep