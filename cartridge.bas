
#Include Once "nes/cartridge.bi"


#Include Once "windows.bi"
#Include Once "containers/vector.bi"
#include "nes/Cartridge.bi"
#include "file.bi" 

#Include Once "mapper_000.bas"
'#Include Once "mapper_002.bas"
'#Include Once "mapper_004.bas"
'#Include Once "mapper_004_TL29.bas"


	Declare Function cart_cpuRead( addr1 As uint16_t, Byref data1 As uint8_t) As bool 
	Declare function cart_cpuWrite(addr1 As uint16_t,  data1 As uint8_t)As bool

	'// Communication with PPU Bus
	Declare Function cart_ppuRead(addr1 As uint16_t, Byref data1 As uint8_t)As bool
	Declare Function cart_ppuWrite(addr1 As uint16_t,  data1 As uint8_t) As bool
   Declare Sub cart_reset()
   Declare sub insert_cartridge( sFileName As String)


   Dim shared As bool bImageValid = false
   
   Declare Function ImageValid() As bool
   Dim shared As uint8_t nMapperID  = 0 
	Dim Shared As uint8_t cart_nPRGBanks  = 0 
	Dim Shared As uint8_t cart_nCHRBanks  = 0 

	
	Dim Shared cart_mirror As MIRROR =  HORIZONTAL 
	
	Type sHeader
	 
		Name1 As String* 4-1
		prg_rom_chunks As uint8_t 
		chr_rom_chunks As uint8_t  
	   mapper1 as uint8_t
		mapper2 As uint8_t 
		prg_ram_size As uint8_t 
		tv_system1 As uint8_t 
		tv_system2 As uint8_t  
		unused As String * 5-1
		
	End Type
	
	Dim Shared header As sHeader
	
	
	
	bImageValid = FALSE
	
	MVectorTemplate(uint8_t)
	Dim Shared vPRGMemory As TVECTORUINT8_T
	Dim Shared vCHRMemory As TVECTORUINT8_T
	'std::vector<uint8_t> vPRGMemory;
	'std::vector<uint8_t> vCHRMemory;
	Dim Shared hw_mirror As mirror
	
	'C:\Program Files (x86)\FreeBASIC107\my projects\OLCNES-FB'

	
	
	Sub insert_cartridge( sFileName As String)
		Dim f As Integer

	
		Open  sFileName For Binary As #f
		
		  If FILEEXISTS(sFileName) THEN
        'PRINT "found rom"
      



       Open sFileName FOR BINARY AS #1


        GET #1, , header
        
        	
        IF header.mapper1 AND &H04 THEN
           ' PRINT "has trainer-skipping"
            SEEK #1, 512

     '   ELSE
         '   PRINT "no trainer"
        END IF

		'// Determine Mapper ID
		nMapperID = ((header.mapper2 shr 4) shl 4) or(header.mapper1 shr 4) 
		hw_mirror = IIf((header.mapper1 And &H01), VERTICAL, HORIZONTAL )

		'// "Discover" File Format
		Dim As uint8_t nFileType = 1 
        
      If (nFileType = 0) Then
 
      End If
        
        
        
        
        if (nFileType  = 1) Then
		 
			cart_nPRGBanks = header.prg_rom_chunks 
			vPRGMemory.resize(cart_nPRGBanks * 16384) 
			
			 GET #1, ,vPRGMemory[0],vPRGMemory.size()
			
			'ifs.read((char*)vPRGMemory.data(), vPRGMemory.size());
			
			cart_nCHRBanks = header.chr_rom_chunks
			'nCHRbanks = header.chr_rom_chunks
			
       if (cart_nCHRBanks = 0) Then
     	vCHRMemory.resize(8192)
       Else
        	vCHRMemory.resize(cart_nCHRBanks * 8192)
       EndIf
        GET #1, ,vCHRMemory[0],vCHRMemory.size()
        End If
        
        
			'nCHRBanks = header.chr_rom_chunks;
			'if (nCHRBanks == 0)
			'{
		'		// Create CHR RAM
			'	vCHRMemory.resize(8192);
			'}
			'else
			'{
		'		// Allocate for ROM
			'	vCHRMemory.resize(nCHRBanks * 8192);
			'}
			'ifs.read((char*)vCHRMemory.data(), vCHRMemory.size());
       
        
        
        	if (nFileType = 2) Then
 
        	End If
        
        
        
        
		'TODO add mappers again and see if it can use pointers
        
       ' // Load appropriate mapper
		Select Case (nMapperID)
		 
		case   0  'pMapper = std::make_shared<Mapper_000>(nPRGBanks, nCHRBanks); break;
		
		'works fine
 	mapper_000(cart_nPRGBanks,cart_nCHRBanks)     
		
		case   2 ' is actually working so far
	'mapper_002(cart_nPRGBanks,cart_nCHRBanks)  
		
		'//case   2: pMapper = std::make_shared<Mapper_002>(nPRGBanks, nCHRBanks); break;
		'//case   3: pMapper = std::make_shared<Mapper_003>(nPRGBanks, nCHRBanks); break;
		case   4:
	' mapper_004(cart_nPRGBanks,cart_nCHRBanks)  
		'//case  66: pMapper = std::make_shared<Mapper_066>(nPRGBanks, nCHRBanks); break;
		End select 
        
        
        
        
		Close #f
		bImageValid = true
	
		End If
		
	End Sub
	
	
	Sub cartqb64 ' for reference
	'	
	'	    DIM orig_dest AS _UNSIGNED LONG
   ' DIM nPRGBanks AS _UNSIGNED _BYTE
   ' DIM nCHRBanks AS _UNSIGNED _BYTE
   ' $CONSOLE

   ' _CONSOLETITLE "NEStea Debuggger"


   ' orig_dest = _DEST
   ' 'mapper ????
   ' IF cart_info_disp = 1 THEN
   '     _CONSOLE ON


   '     _DEST _CONSOLE
   ' ELSE
   '     _CONSOLE OFF


   ' END IF

   ' nMapperID = 0
   ' nPRGBanks = 0
   ' nCHRBanks = 0

   ' bImageValid = -1


   ' file$ = cart

   ' IF _FILEEXISTS(file$) THEN
   '     PRINT "found rom"



   '     OPEN file$ FOR BINARY AS #1


   '     GET #1, , header

   '     PRINT "magic number: ";
   '     PRINT header.name_nes



   '     IF header.mapper1 AND &H04 THEN
   '         PRINT "has trainer-skipping"
   '         SEEK #1, 512

   '     ELSE
   '         PRINT "no trainer"
   '     END IF



   '     IF header.mapper1 AND &H01 THEN

   '         cart_mirror = VERTICAL


   '         mirror$ = "VERTICAL"
   '     ELSE

   '         cart_mirror = HORIZONTAL



   '         mirror$ = "HORIZONTAL"
   '     END IF
   '     'ONESCREEN_LO
   '     'ONESCREEN_HI


   '     PRINT "mirror mode: "; mirror$


   '     nMapperID = _SHL(_SHR(header.mapper2, 4), 4) OR _SHR(header.mapper1, 4)



   '     nFileType = 1

   '     IF header.mapper2 AND &H0C = &H08 THEN
   '         nFileType = 2
   '     END IF



   '     IF nFileType = 0 THEN

   '     END IF

   '     IF nFileType = 1 THEN
   '         PRINT "file type:" + STR$(nFileType)
   '         nPRGBanks = header.prg_rom_chunks
   '         REDIM vPRGMemory((nPRGBanks * 16384) - 1)
   '         GET #1, , vPRGMemory()
   '         PRINT "NUM PRG BANKS:" + STR$(nPRGBanks)
   '         PRINT "PRG ROM SIZE:" + STR$(LEN(vPRGMemory()))

   '         nCHRBanks = header.chr_rom_chunks

   '         PRINT "NUM CHR BANKS:" + STR$(nCHRBanks)
   '         IF nCHRBanks = 0 THEN
   '             REDIM vCHRMemory(8192 - 1)
   '             PRINT "CHR RAM: " + STR$(LEN(vCHRMemory()))
   '         ELSE
   '             '   PRINT "CHR ROM"
   '             REDIM vCHRMemory((nCHRBanks * 8192) - 1)
   '             PRINT "CHR ROM SIZE:" + STR$(LEN(vCHRMemory()))

   '         END IF
   '         GET #1, , vCHRMemory()


   '     END IF


   '     IF nFileType = 2 THEN
   '         PRINT "file type WORK IN PROGRESS:" + STR$(nFileType)
   '     END IF

   '     SELECT CASE nMapperID

   '         CASE 0:

   '             PRINT "valid mapper-found Mapper:" + STR$(nMapperID)


   '             mapper_000 nPRGBanks, nCHRBanks

   '             'CASE 1:

   '             '    PRINT "valid mapper-found Mapper WORK IN PROGRESS: " + STR$(nMapperID)

   '             '    ' mapper_001 nPRGBanks, nCHRBanks

   '         CASE 2:

   '             PRINT "valid mapper-found Mapper WORKING:" + STR$(nMapperID)
   '             mapper_002 nPRGBanks, nCHRBanks

   '         CASE 3:

   '             PRINT "valid mapper-found Mapper WORKING:" + STR$(nMapperID)
   '             mapper_003 nPRGBanks, nCHRBanks


   '         CASE 4:

   '             PRINT "valid mapper-found Mapper WORK IN PROGRESS: " + STR$(nMapperID)

   '             mapper_004 nPRGBanks, nCHRBanks

   '             '    'CASE 65:

   '             '    '    PRINT "valid mapper-found Mapper WORK IN PROGRESS:" + STR$(nMapperID)

   '             '    ' mapper_065 nPRGBanks, nCHRBanks


   '         CASE 66:

   '             PRINT "valid mapper-found Mapper WORK IN PROGRESS:" + STR$(nMapperID)

   '             mapper_066 nPRGBanks, nCHRBanks

   '         CASE ELSE:
   '             PRINT "invalid mapper:" + STR$(nMapperID)
   '     END SELECT

   '     bImageValid = 1
   '     CLOSE #1

   ' ELSE
   '     bImageValid = 0
   '     PRINT "rom not found"
   ' END IF


   ' IF cart_info_disp = 1 THEN
   '     _DEST orig_dest
   ' END If
	End Sub
	
	
	Sub cart_reset()
		
		
	   'TODO add reset mapper again
		resetmapper
		
	End Sub
	
	Function ImageValid() As bool
		return bImageValid
	End Function
	Function cart_cpuRead( addr1 As uint16_t, Byref data1 As uint8_t) As bool 
		Dim As uint32_t mapped_addr = 0
	if (cpuMapRead(addr1, mapped_addr,data1)) Then

		if (mapped_addr = &HFFFFFFFF) then
	 
			'// Mapper has actually set the data value, for example cartridge based RAM
			return true 
		 
		else
	 
		'	// Mapper has produced an offset into cartridge bank memory
			Data1 = vPRGMemory[mapped_addr] 
		End If
		return true 
	 
	else
		return false 
		
End If
	End Function
	
	
	Function cart_cpuWrite( addr1 As uint16_t,  data1 As uint8_t) As bool 
    DIM mapped_addr AS  uint32_t


    mapped_addr = 0


    IF cpuMapWrite(addr1, mapped_addr, data1) THEN ' cpuMapWrite(addr, mapped_addr, byte_data) THEN

        IF mapped_addr = &HFFFFFFFF  THEN

           
              Return TRUE

        ELSE
            vPRGMemory[mapped_addr] = data1
        END IF
      
        Return TRUE


    ELSE

           Return FALSE

    END If
		
	End Function
	
	
	Function cart_ppuRead( addr1 As uint16_t, Byref data1 As uint8_t) As bool 
		 DIM mapped_addr AS uint32_t = 0
	if (ppuMapRead(addr1, mapped_addr)) then
	 
	data1 = vCHRMemory[mapped_addr] 
			 
		return true 
	 
	else
      return FALSE
	End If
	
	End Function
	
	Function cart_ppuWrite( addr1 As uint16_t,  data1 As uint8_t) As bool 
	    DIM mapped_addr AS uint32_t = 0

    mapped_addr = 0

    If ppuMapWrite(addr1, mapped_addr) THEN
        vCHRMemory[mapped_addr] = data1

        cart_ppuWrite = 1
    Else

        cart_ppuWrite = 0
    END If
	End Function
	

 Function cart_MirrorMode() As MIRROR
 
	Dim m As Mirror = _mirror()
	if (m = HARDWARE) Then
	
		'// Mirror configuration was defined
		'// in hardware via soldering
		return hw_mirror
	 
	else
 
	'	// Mirror configuration can be
	'	// dynamically set via mapper
		Return m
	End If
	Return 	hw_mirror 'hw_mirror
 End Function

'sleep