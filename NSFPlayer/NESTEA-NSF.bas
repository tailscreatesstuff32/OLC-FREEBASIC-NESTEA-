#Include Once "windows.bi"
'#Include Once "nes/nes.bi"
#Include Once "containers/vector.bi"
#include Once "file.bi"


#Include "NSFmapper.bas"
#Include "NSFplayer.bas"

Type NSFheader
	
	format As  String * 5 
	version As uint8_t 
	totalsongs As uint8_t
	startsong As uint8_t
	loadaddr As uint16_t
	initaddr As uint16_t
	playaddr As uint16_t
	Namesong As String '*32
	artist As String '*32
	copyright As String '*32
	initbanks(7) As uint8_t
	total As uint8_t  
	banking As boolean
	con_progdata As uint8_t ptr = New uint8_t(3)
	'romdata As uint8_t = New uint8_t(Any)
	rom_data As TVECTORUINT8T 
	
	
	
	
	
	
End Type






Function LoadNSFile(NSF As String) As BOOLEAN
	Dim NSFfile As NSFheader
	
	Dim nsf1 As Integer = FreeFile
		
	Open NSF For Binary As nsf1
	'
	'Get #nsf1,,nsffile.format
	'Print nsffile.format
	'
	'Get #nsf1,,nsffile.version
	'	Print nsffile.version
	'
	'	Get #nsf1,,nsffile.totalsongs
	'	Print nsffile.totalsongs
	'	
	'	Get #nsf1,,nsffile.startsong
	'	Print nsffile.startsong
	'	
	'	
	'		
	'Get #nsf1,,nsffile.loadaddr
	'Print Hex(nsffile.loadaddr)
	'Get #nsf1,,nsffile.initaddr
	'Print Hex(nsffile.initaddr)
	'Get #nsf1,,nsffile.playaddr
	'Print Hex(nsffile.playaddr)	
	'	
	'Get #nsf1,,nsffile.Namesong
	'Print nsffile.Namesong	
   '
   'Get #nsf1,,nsffile.artist
	'Print nsffile.artist
   '
   'Get #nsf1,,nsffile.copyright
	'Print nsffile.copyright
   '
   'Seek #nsf1,2
   '
   'Get #nsf1,,nsffile.initbanks()
	'Print nsffile.initbanks(0)
   'Print nsffile.initbanks(1)
   'Print nsffile.initbanks(2)
   'Print nsffile.initbanks(3)
   'Print nsffile.initbanks(4)
   'Print nsffile.initbanks(5)
   'Print nsffile.initbanks(6)
   'Print nsffile.initbanks(7)
   'Seek #nsf1,2
   'Seek #nsf1,1
   'Seek #nsf1,1
   'Seek #nsf1,1
   '
   '
   'Get  #nsf1,,*nsffile.con_progdata,3 
   'Print " " 
   '
   '	'Print nsffile.con_progdata
   ''	nsffile.con_progdata+=1
   '	'Print nsffile.con_progdata[0]
   '	'Print nsffile.con_progdata[1]
   '	'Print nsffile.con_progdata[2]
   ''	Print nsffile.con_progdata
   	

	'nsffile.banking = IIf(nsffile.total > 0,TRUE,FALSE)
 
 
 
	'Print Iif(nsffile.banking,TRUE,FALSE)
	
	'Seek #nsf1,1 
	
	nsffile.rom_data.resize(Lof(nsf1))
	Get  #nsf1,,nsffile.rom_data[0],Lof(nsf1)
	Close nsf1
	
	if(nsffile.rom_data.size() < &H80) Then
		
	   Print "Invalid NSF loaded"      
	    return FALSE
		
		
	EndIf
	
	    if  nsffile.rom_data[0] <> &H4E Or  nsffile.rom_data[1] <> &H45 _
	    Or nsffile.rom_data[2] <> &H53 Or nsffile.rom_data[3] <> &H4D Or nsffile.rom_data[4] <> &H1a then
	    
	   Print "Invalid NSF loaded"
	
	    End If
	
	    if nsffile.rom_data[5] <> 1    Then
	    
	         Print "Unknown NSF version: " & nsffile.rom_data[5]    
	           return FALSE
	           
	    End If
	    
	    
	    Dim totalsongs As uint8_t = nsffile.rom_data[6]
	    Dim startsong  As uint8_t = nsffile.rom_data[7]
	    
	   dim loadAdr As uint16_t = nsffile.rom_data[8] Or (nsffile.rom_data[9] shl 8)
	     if(loadAdr < &H8000) Then
	     	Print "Load address less than 0x8000 is not supported"
	     	 return FALSE
	     EndIf
	
	Dim initAdr As uint16_t  = nsffile.rom_data[&HA] or (nsffile.rom_data[&HB] shl 8)
	Dim playAdr As uint16_t  = nsffile.rom_data[&HC] or (nsffile.rom_data[&HD] shl 8)
	
	
	Dim name1 As String
	
	
	For i As uint8_t = 0 To 32-1
		If nsffile.rom_data[&H0E + i] = 0 Then
			
			Exit for
		Else
			
			nsffile.namesong +=  Chr(nsffile.rom_data[&H0E + i])
		
		EndIf
	Next
	
	Print nsffile.namesong
	
		For i As uint8_t = 0 To 32-1
		If nsffile.rom_data[&H2E + i] = 0 Then
			
			Exit for
		Else
			
			nsffile.Artist +=  Chr(nsffile.rom_data[&H2E + i])
		
		EndIf
	Next
	Print nsffile.Artist
	
	
			For i As uint8_t = 0 To 32-1
		If nsffile.rom_data[&H4E + i] = 0 Then
			
			Exit for
		Else
			
			nsffile.copyright +=  Chr(nsffile.rom_data[&H4E + i])
		
		EndIf
	Next
	Print nsffile.copyright
	
	
	Erase nsffile.initbanks
	
	For i As uint8_t = 0 To 8 - 1
		
		nsffile.initbanks(i) = nsffile.rom_data[&H70 + i]
		nsffile.total = nsffile.rom_data[&H70 + i]
		
	Next
	
	nsffile.banking = IIf(nsffile.total > 0,TRUE,FALSE)
	
	
	
	Dim nsfmapper1 As NSFMapper = nsfmapper(nsffile.rom_data,NSFfile.loadaddr,NSFfile.banking,@NSFfile.initbanks(0))
	
	
	'what is a callarea?
	
	
	
	'todo playsong
	
	
	Return TRUE
	
	
End function

sub  init_nsf
	LoadNSFile("SMB.nsf")
'	If  Then
		
'	Else
		
		
		
	'EndIf
	
	
	
	
	
End Sub
	
	
	init_nsf
	sleep

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
