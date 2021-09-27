

Dim tblname(2,1024) As UByte 
Dim tblpattern(2,4096) As UByte 
Dim tblpalette(32) As UByte 

Dim palScreen(&H40)As Long
Dim sprScreen As Long
Dim sprNameTable(2) As Long 
Dim sprPatternTable(2) As Long 


Union PPUSTATUS

Type
	unused:5 As UByte
	sprite_overflow:1 As UByte 
	sprite_zero_hit:1 As UByte
	vertical_blank:1 As UByte
End Type

reg As UByte

End Union: Dim status As PPUSTATUS

Union PPUMASK

Type
	grayscale:1 As UByte
	render_background_left:1 As UByte 
	render_sprites_left:1 As UByte
	render_background:1 As UByte
	render_sprites:1 As UByte
	enhance_red:1 As UByte 
	enhance_green:1 As UByte
	enhance_blue:1 As UByte
End Type

reg As UByte

End Union: Dim mask As PPUMASK


Union PPUCTRL

Type 
	nametable_x:1 As UByte
	nametable_y:1 As UByte 
	increment_mode:1 As UByte
	pattern_sprite:1 As UByte
	pattern_background :1 As UByte
	sprite_size:1 As UByte 
	slave_mode:1 As UByte 'unused
	enable_nmi:1 As UByte
End Type

reg As UByte

End Union: Dim control As PPUCTRL

Union loopy_reg

Type 
	coarse_x:5 As UInteger
	coarse_y:5 As UInteger
	nametable_x:1 As UInteger
	nametable_y:1 As UInteger
	fine_y:3 As UInteger
	unused:1 As UInteger
End Type

reg As UInteger

End Union

Dim vram_addr As loopy_reg
Dim tram_addr As loopy_reg

Dim fine_x As UByte

Dim address_latch As UByte

Dim scanline As Integer
Dim cycle As Integer
'Dim odd_frame As Boolean = FALSE

Dim  bg_next_tile_id As UByte
Dim  bg_next_tile_attrib As UByte
Dim  bg_next_tile_lsb As UByte
Dim  bg_next_tile_msb As UByte
Dim  bg_shifter_pattern_lo As UInteger
Dim  bg_shifter_pattern_hi As UInteger
Dim  bg_shifter_attrib_lo As UInteger
Dim  bg_shifter_attrib_hi As UInteger

Dim nmi As boolean = FALSE
Dim frame_complete As boolean


'TODO cartridge shared pointer?



'Sleep