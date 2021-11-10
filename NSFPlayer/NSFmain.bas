
#include once "olc2A02.bas"
#Include once "NSFmapper.bas"
#include once "NSF.bas"

'#include once "NSFGui.bas"



dim player as NSFplayer ptr = new NSFplayer()

dim shared paused as boolean = FALSE
dim shared loaded as boolean = FALSE 
dim shared pausedInBg as boolean = FALSE

dim loopid as uint8_t
const scrnwdth = 256
const scrnhght = 240

'dim currentsong as uint8_t


	Dim Shared NSFplayer1 As NSFplayer = NSFplayer()
	Dim Shared currentsong As uint8_t= 1	
	'Dim Shared loaded As boolean = false
	
Sub drawvisuals()
		
	cls
	Print "Title: ";NSFplayer1.nsffile.namesong
	Print "Artist: ";NSFplayer1.nsffile.artist
	Print "Copyright: ";NSFplayer1.nsffile.copyright
	Print "Song ";currentsong;" of ";NSFplayer1.NSFfile.totalsongs
	
		'TODO WIP maybe?
		
		
End Sub
sub runframe()
		NSFplayer1.runframe()
		NSFplayer1.getsamples(null,1)
		
	   drawvisuals()
	
End Sub

sub update()
runframe()
End Sub
Sub loadNSFRom(filenme As String)
		
		
		If NSFplayer1.LoadNSFile(filenme) Then
			
			
			If loaded = false and paused = false Then
				'TODO WIP
				
				update()
	
				
		   End if
			
			
			loaded = TRUE
			currentsong = NSFplayer1.NSFfile.startsong 
		EndIf
		
		
		
		
		
		
		
		
		
		
End Sub




sub  init_nsf()

	'NSFplayer = NSFplayer()
	 
	
	loadNSFRom("Journey to Silius [RAF World] (1990-08-10)(Sunsoft).nsf")
	
	

'	If  Then
		
'	Else
		
		
		
	'EndIf
	
	
	
	
	
End Sub
	
'WIP the rest of the functions and subs
	

	init_nsf()
	Sleep
	




sub logit(txt as string)
	print txt
End Sub

sub reset_NSF
	if loaded then
		NSFplayer1.playsong(currentsong)
		drawvisuals()
	EndIf
	
	
	
End Sub


delete player 


