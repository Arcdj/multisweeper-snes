;== Include memorymap, header info, and SNES initialization routines
.INCLUDE "header.inc"
.INCLUDE "InitSNES.asm"

;========================
; Start
;========================



.DEFINE boardWidth  = 10
.DEFINE boardHeight = 10
.DEFINE minesAmount = 15

.DEFINE tp          = $00
.DEFINE tp2         = $01
.DEFINE tpp         = $02
.DEFINE thold       = $03
.DEFINE tport       = $04
.DEFINE tbx         = $05
.DEFINE tby         = $06
.DEFINE tsx         = $07
.DEFINE tsy         = $08
.DEFINE tflags      = $09

.DEFINE mines       = $0A
.DEFINE width       = $0C
.DEFINE height      = $0D
.DEFINE swidth      = $0E
.DEFINE sheight     = $0F
.DEFINE boardx      = $10
.DEFINE boardy      = $11

.DEFINE mousex      = $12
.DEFINE mousey      = $13

.DEFINE scopex      = $14
.DEFINE scopey      = $15
.DEFINE scopeofsx   = $16
.DEFINE scopeofsy   = $17

.DEFINE rng1        = $18
.DEFINE rng2        = $19
.DEFINE rng3        = $1A
.DEFINE rng4        = $1B

.DEFINE checkfor    = $1C

.DEFINE drawin      = $1D
.DEFINE drawout     = $1E
.DEFINE drawqueue   = $7EFF00

.DEFINE emptyin     = $1F
.DEFINE emptyout    = $20
.DEFINE emptyx      = $7EFD00
.DEFINE emptyy      = $7EFE00

.DEFINE board       = $7F0000

.DEFINE c_hide      = $21

.DEFINE minesRemaining = $22    ; 2 bytes
.DEFINE tilesRemaining = $24    ; 2 bytes

.DEFINE b2d_result  = $30       ; max 5 bytes

.DEFINE t0          = $E0
.DEFINE t1          = $E1
.DEFINE t2          = $E2
.DEFINE t3          = $E3
.DEFINE t4          = $E4

.DEFINE flags       = $FF

.DEFINE c1sx        = $0100
.DEFINE c1sy        = $0101
.DEFINE c1bx        = $0102
.DEFINE c1by        = $0103
.DEFINE p1p         = $0104
.DEFINE p1hold      = $0105
.DEFINE p1flags     = $0106

.DEFINE c2sx        = $0120
.DEFINE c2sy        = $0121
.DEFINE c2bx        = $0122
.DEFINE c2by        = $0123
.DEFINE p2p         = $0124
.DEFINE p2hold      = $0125
.DEFINE p2flags     = $0126


.BANK 0 SLOT 0
.ORG 0
.SECTION "MainCode"

numtiles:
.BYT $06, $08, $0A, $0C, $0E, $20, $22, $24, $26, $28


vblank:
    lda #$8F            ; = 10001111
    sta $2100           ; Turn off screen, full brightness

    lda $213C
    lda $213C
    sta scopex
    lda $213D
    lda $213D
    sta scopey

    sep #$10
    stz $2102 ; oam
    lda c1sx
    sta $2104
    lda c_hide
    bit #%00000001
    beq +
    eor #%00000001
    sta c_hide
    lda #$E1
    jmp ++
+
    lda c1sy
++
    sta $2104

    lda #$02
    sta $2102 ; oam
    lda c2sx
    sta $2104
    lda c_hide
    bit #%00000010
    beq +
    eor #%00000010
    sta c_hide
    lda #$E1
    jmp ++
+
    lda c2sy
++
    sta $2104

update_tile:
    ldx drawout
    cpx drawin
    beq exit

    lda drawqueue, x
    sta $2116
    inx
    lda drawqueue, x
    sta $2117
    inx
    lda drawqueue, x
    sta $2118
    inx

    stx drawout


    jmp update_tile
exit:
    rep #$10


    stz $2121
    lda $F00002
    sta $2122
    lda $F00003
    sta $2122

    lda #$0F
    jsl ezrainbow

    lda #$83
    jsl ezrainbow

    lda $4210           ; read nmi flag (???)
    rti

Start:
    InitSNES            ; Init Snes :)

                        ; populate VRAM with DMA
    LDA #$80
    STA $2115           ; Set VRAM port to word access
    LDX #$1801
    STX $4300           ; Set DMA mode to incremental source, WORD to $2118/9
    LDX #$4000
    STX $2116           ; Set VRAM port address to $0000
    ldx #$8000
    STX $4302           ; Set source address to $xx:8000
    LDA #$04
    STA $4304           ; Set source bank to $04
    LDX #$1000
    STX $4305           ; Set transfer size to 1000 bytes
    LDA #$01
    STA $420B           ; Initiate transfer

                        ; populate GCRAM

    stz $2121
    lda #%10001100      ; GGGRRRRR
    sta $F00002
    sta $2122
    lda #%00110001      ; 0BBBBBGG
    sta $2122           ; somewhat light grey
    sta $F00003

    lda #$02
    sta $2121

    lda #%00010000      ; GGGRRRRR
    sta $2122
    lda #%01000010      ; 0BBBBBGG
    sta $2122           ; dark grey

    lda #%00011000      ; GGGRRRRR
    sta $2122
    lda #%01100011      ; 0BBBBBGG
    sta $2122           ; light(er) grey

    lda #%11111111      ; GGGRRRRR
    sta $2122
    lda #%01111111      ; 0BBBBBGG
    sta $2122           ; white

    lda #%00000000      ; GGGRRRRR
    sta $2122
    lda #%01111100      ; 0BBBBBGG
    sta $2122           ; blue

    lda #%00000000      ; GGGRRRRR
    sta $2122
    lda #%00000010      ; 0BBBBBGG
    sta $2122           ; dark green

    lda #%00011111      ; GGGRRRRR
    sta $2122
    lda #%00000000      ; 0BBBBBGG
    sta $2122           ; red

    lda #%00000000      ; GGGRRRRR
    sta $2122
    lda #%01000000      ; 0BBBBBGG
    sta $2122           ; dark blue / purple

    lda #%00010000      ; GGGRRRRR
    sta $2122
    lda #%00000000      ; 0BBBBBGG
    sta $2122           ; dark red

    lda #%00000000      ; GGGRRRRR
    sta $2122
    lda #%01000010      ; 0BBBBBGG
    sta $2122           ; dark cyan

    lda #%00010000      ; GGGRRRRR
    sta $2122
    lda #%01000000      ; 0BBBBBGG
    sta $2122           ; 9 color (dakr pupler)


    lda #$0F
    sta $2121
    lda #%00011111      ; GGGRRRRR
    sta $2122
    stz $2122           ; red

    lda #$82
    sta $2121           ; sprite color

    lda #%11111111      ; GGGRRRRR
    sta $2122
    lda #%01111111      ; 0BBBBBGG
    sta $2122           ; white

    stz $2122
    lda #%00111110      ; 0BBBBBGG
    sta $2122           ; somewhere between blue and green


    lda #%00000100
    sta $210B          ; background 1 graphics to $4000

    lda #%00000010
    sta $2101          ; object graphics to $400

    lda #%00000000
    sta $2107           ; set background 1 tilemap VRAM address to $0000, and 32x32 tilemap

    lda #%00010001
    sta $212C           ; background 1 and objects to main screen
    sta $212D           ; and sub screen, for fun :)

    lda #%00000010      ; set scope callibartion flag and/or hi-res flag, for fun :)
    sta flags

    ; Joost was here =)                  <------ crime scene

    stz $2102
    stz $2103           ; oam
    stz $2104
    stz $2104
    lda #$40
    sta $2104
    lda #%00100000
    sta $2104

    lda #$02
    sta $2102
    stz $2103           ; oam
    stz $2104
    stz $2104
    lda #$40
    sta $2104
    lda #%00100000
    sta $2104

    lda #boardWidth
    dea
    sta width

    lda #boardHeight
    dea
    sta height

    ldx #minesAmount
    stx mines

    lda $F00000
    ina
    sta rng1
    lda $F00001
    ina
    sta rng2



restart:
    ; step 1; clear the board in WRAM and VRAM with DMA

    STZ $2181		;set WRAM address to $000000
    STZ $2182
    lda #$01
    STA $2183

    LDX #$8008
    STX $4310         ;Set DMA mode to fixed source, BYTE to $2180
    LDX #zero
    STX $4312         ;Set source offset
    LDA #:zero
    STA $4314         ;Set source bank
    LDX #$0000
    STX $4315         ;Set transfer size to 64k bytes


    STZ $2115           ; Set VRAM port to low byte access
    LDX #$1809
    STX $4300           ; Set DMA mode to fixed source, WORD to $2118/9
    LDX #$0200
    STX $4302           ; Set source address to $xx:0200
    LDA #$00
    STA $4304           ; Set source bank to $00
    LDX #$4000
    STX $4305           ; Set transfer size to 4000 bytes
    LDX #$0000
    STX $2116           ; Set VRAM port address to $0000


    LDA #$03
    STA $420B           ; Initiate transfer

    ; step 2; (re)calculate bg1 scroll so the board is centred
    ; and set bg mode and interlacing based on hi-res flag

    lda flags
    bit #%00000001
    beq +
    lda #%00000001
    sta $2133           ; Turn on interlacing for hi-res mode, for fun :)
    lda #%00010101      ; mode 5
    jmp ++
+
    stz $2133           ; NO interlacing
    lda #%00010001      ; mode 1
++
    sta $2105           ; set background mode to 1(or 5, for fun :), and 16x16 tiles on BG1


    lda flags           ; calculate board width(pixels)
    bit #%00000001
    beq +
    lda #$8
    jmp ++
+
    lda #$10
++
    sta $211B
    stz $211B           ; set up PPU multiplication(lmao)

    ldx #$01
-   lda width, x
    ina
    sta $211C
    lda $2134
    sta swidth, x

    dex
    bpl -


    lda flags           ; calculate board position, so it's centred
    bit #%00000001
    beq +
    lda #$E1
    sta t1
    lda #$4
    sta t2
    lda #$8
    jmp ++
+
    lda #$70
    sta t1
    lda #$8
    sta t2
++

    ldx #$01
-
    sta $211B
    stz $211B           ; set up PPU multiplication(lmao)

    lda width, x
    ina
    sta $211C
    lda $2134
    sta t0
    lda t1
    sec
    sbc t0
    sta boardx, x

    lda #$80
    sta t1
    lda t2
    dex
    bpl -

    lda #$00            ; calculate bg scroll from board position and set it
    sec
    sbc boardx
    sta $210D
    lda #$00
    sbc #$00
    sta $210D
    lda #$FF
    sec
    sbc boardy
    sta $210E
    lda #$FF
    sta $210E           ; set scroll

    ; step 3; generate a new board, and set amount of mines and safe tiles
    ldy mines
    sty minesRemaining

    lda width
    ina
    sta $211B
    stz $211B           ; set up PPU multiplication(lmao)

    lda height
    ina
    sta $211C

    rep #$20            ; 16 bit A!!!

    lda $2134
    sec
    sbc mines
    sta tilesRemaining

    sep #$20            ; 8 bit A

place:
    jsl rng
    ldx #$01
-
    lda rng3, x
    sta $4204
    lda width, x
    ina
    sta $4206
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    lda $4216
    sta rng3, x

    dex
    bpl -

    ldx rng3
    lda board, x
    bit #$80
    bne place
    lda #%10000000
    sta board, x
    
    dey
    bne place

    lda rng1
    sta $F00000
    lda rng2
    sta $F00001         ; store RNG seeds in save RAM



;    stz $2115           ; set vram auto increment to low byte

    sep #$10            ; 8 bit X and Y
    inc width
    inc height

    ; step 4; upload an empty board (tilemap) to VRAM

    ldy #$00

    lda #$20
    sta $211B
    stz $211B           ; set up PPU multiplication(lmao)

--
    ldx #$00
    sty $211C
    lda $2135
    sta $2117
    lda $2134
    sta $2116
    lda #$02
-
    sta $2118
    inx
    cpx width
    bne -

    iny
    cpy height
    bne --

    dec width
    dec height
    rep #$10            ; 16 bit X and Y

    ; step 5; set cursor positions and prepare for the first frame

    stz tbx
    stz c1bx
    stz tby
    stz c1by
    jsl derive_s_pos
    lda tsx
    sta c1sx
    lda tsy
    sta c1sy

    lda width
    sta c2bx
    sta tbx
    stz c2by
    jsl derive_s_pos
    lda tsx
    sta c2sx
    lda tsy
    sta c2sy

    lda #%10000000
    sta checkfor

    lda #$FF
    sta c_hide

    lda #%10000001
    sta $4200           ; enable auto joypad read and vblank NMI

forever:
    wai

-   lda #%11111110
    ora $4212
    cmp #$FF
    beq -	; wait for auto joypad read to finish

    lda $4219
    eor p1p
    and $4219
    bit #%01000000
    bne +
    jmp ++
+
    sta p1p
    stz $4200           ; disable auto joypad read and vblank NMI

    lda $4219
    bit #%00100000      ; select
    beq +
    lda mines
    sec
    sbc #1
    sta mines
    lda mines + 1
    sbc #0
    sta mines + 1
+
    lda $4219
    bit #%00010000      ; start
    beq +
    lda mines
    clc
    adc #1
    sta mines
    lda mines + 1
    adc #0
    sta mines + 1
+
    lda $4219
    bit #%00001000      ; up
    beq +
    inc height
+
    lda $4219
    bit #%00000100      ; down
    beq +
    dec height
+
    lda $4219
    bit #%00000010      ; left
    beq +
    inc width
+
    lda $4219
    bit #%00000001      ; right
    beq +
    dec width
+
    lda $4218
    bit #%01000000      ; x
    beq +
    lda flags
    eor #%00000001
    sta flags
+
    lda $4218
    bit #%00100000      ; l
    beq +
    lda mines
    sec
    sbc #10
    sta mines
    lda mines + 1
    sbc #0
    sta mines + 1
+
    lda $4218
    bit #%00010000      ; r
    beq +
    lda mines
    clc
    adc #10
    sta mines
    lda mines + 1
    adc #0
    sta mines + 1
+


    jmp restart
++

    lda #$0F            ; = 00001111
    sta $2100           ; Turn on screen, full brightness


    lda $4218
    sta tp
    lda $4219
    sta tp2
    lda p1p
    sta tpp
    lda p1hold
    sta thold
    lda c1bx
    sta tbx
    lda c1by
    sta tby
    lda c1sx
    sta tsx
    lda c1sy
    sta tsy
    stz tport
    lda p1flags
    sta tflags

    jsl controls

    lda tpp
    sta p1p
    lda thold
    sta p1hold
    lda tbx
    sta c1bx
    lda tby
    sta c1by
    lda tsx
    sta c1sx
    lda tsy
    sta c1sy
    lda tflags
    sta p1flags


    lda $421A
    sta tp
    lda $421B
    sta tp2
    lda p2p
    sta tpp
    lda p2hold
    sta thold
    lda c2bx
    sta tbx
    lda c2by
    sta tby
    lda c2sx
    sta tsx
    lda c2sy
    sta tsy
    lda #$01
    sta tport
    lda p2flags
    sta tflags

    jsl controls

    lda tpp
    sta p2p
    lda thold
    sta p2hold
    lda tbx
    sta c2bx
    lda tby
    sta c2by
    lda tsx
    sta c2sx
    lda tsy
    sta c2sy
    lda tflags
    sta p2flags


    sep #$10            ; 8 bit X and Y
    ldx emptyout
    cpx emptyin
    beq +
    lda emptyx, x
    sta tbx
    lda emptyy, x
    sta tby
    inx
    stx emptyout
    jsl check_around
+

    rep #$10            ; 16 bit X and Y

    ldx minesRemaining

    jsl binary2decimal

    sep #$10            ; 8 bit X and Y
    ldy #0
    ldx drawin

    lda width
    lsr
    sec
    sbc #2
    clc
    adc #$E0
    sta t0
-
    tya
    clc
    adc t0
    sta drawqueue, x
    inx
    lda #$03
    sta drawqueue, x
    inx
    lda b2d_result, y
    phy
    tay
    lda numtiles, y
    ply
    sta drawqueue, x
    inx

    iny
    cpy #5
    bne -

    stx drawin

    rep #$10            ; 16 bit X and Y

    lda #$8F            ; = 10001111
    sta $2100           ; Turn off screen, full brightness
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop                 ; this will make a black streak appear whenever the processing is done for the frame
    nop                 ; (at the time of writing still within vblank lmao)
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    lda #$0F            ; = 00001111
    sta $2100           ; Turn on screen, full brightness


    jmp forever


.ENDS

.INCLUDE "routines.asm"

.BANK 4 SLOT 0
.ORG 0
.SECTION "Graphics"
; $048000
.incbin "tiles"

.ENDS

; bonus, the old test board;

;    lda #$01
;    sta $7F0202
;    sta $7F0203
;    sta $7F0301         ; board will be at bank 7F, create a basic board
    
;    sta $7F0106
;    sta $7F0107
;    sta $7F0108
;    sta $7F0206
;    sta $7F0208
;    sta $7F0306
;    sta $7F0307
;    sta $7F0308
;    sta $7F0406
;    sta $7F0408
;    sta $7F0506
;    sta $7F0508
;    sta $7F0606
;    sta $7F0608
;    sta $7F0708
;    sta $7F0807
;    sta $7F0600
;    sta $7F0603
;    sta $7F0800
;    sta $7F0901
;    sta $7F0902
;    sta $7F0803