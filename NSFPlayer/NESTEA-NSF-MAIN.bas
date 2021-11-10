'WIP nes music player based on NesJs javascript emulator

#Include Once "windows.bi"
'#Include Once "nes/nes.bi"
#Include Once "containers/vector.bi"
#include Once "file.bi"

#Include "NSFplayer.bas"

	Dim Shared NSFplayer1 As NSFplayer = NSFplayer()
	Dim Shared currentsong As uint8_t= 1	
	Dim Shared loaded As boolean = false
	
Sub drawvisuals()
		
	'	cls
	Print "Title: ";NSFplayer1.nsffile.namesong
	Print "Artist: ";NSFplayer1.nsffile.artist
	Print "Copyright: ";NSFplayer1.nsffile.copyright
	Print "Song ";currentsong;" of ";NSFplayer1.NSFfile.totalsongs
	
		'TODO WIP maybe?
		
		
End Sub
	
Sub loadNSFRom(filenme As String)
		
		
		If NSFplayer1.LoadNSFile(filenme) Then
			
			
			If loaded = 0 Then
				'TODO WIP
				
				
				NSFplayer1.runframe()
				
				drawvisuals()
				
		   End if
			
			
			loaded = TRUE
			currentsong = NSFplayer1.NSFfile.startsong 
		EndIf
		
		
		
		
		
		
		
		
		
		
End Sub

sub  init_nsf()

	'NSFplayer = NSFplayer()
	 
	
	loadNSFRom("smb.nsf")
	
	

'	If  Then
		
'	Else
		
		
		
	'EndIf
	
	
	
	
	
End Sub
	
'WIP the rest of the functions and subs
	

	init_nsf()
	Sleep
	
	
	
	
	
	
	
	
	
	
	
	
	
