;== Include memorymap, header info, and SNES initialization routines
.INCLUDE "header.inc"
.INCLUDE "InitSNES.asm"

;========================
; Start
;========================

.BANK 0 SLOT 0
.ORG 0
.SECTION "MainCode"


.DEFINE boardWidth  = 10
.DEFINE boardHeight = 10
.DEFINE mines       = 10

.DEFINE tp           = $00
.DEFINE tp2          = $01
.DEFINE tpp          = $02
.DEFINE thold        = $03
.DEFINE tport        = $04
.DEFINE tbx          = $05
.DEFINE tby          = $06
.DEFINE tsx          = $07
.DEFINE tsy          = $08

.DEFINE width       = $09
.DEFINE height      = $0A
.DEFINE swidth      = $0B
.DEFINE sheight     = $0C
.DEFINE boardx      = $0D
.DEFINE boardy      = $0E

.DEFINE mousex      = $10
.DEFINE mousey      = $11
.DEFINE tsxp        = $12
.DEFINE tsyp        = $13

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
.DEFINE emptyx = $7EFD00
.DEFINE emptyy = $7EFE00

.DEFINE board       = $7F0000

.DEFINE c_hide      = $21

.DEFINE t0          = $E0
.DEFINE t1          = $E1
.DEFINE t2          = $E2


.DEFINE flags       = $FF

.DEFINE c1sx        = $0100
.DEFINE c1sy        = $0101
.DEFINE c1bx        = $0102
.DEFINE c1by        = $0103
.DEFINE p1p         = $0104
.DEFINE p1hold      = $0105

.DEFINE c2sx        = $0120
.DEFINE c2sy        = $0121
.DEFINE c2bx        = $0122
.DEFINE c2by        = $0123
.DEFINE p2p         = $0124
.DEFINE p2hold      = $0125


numtiles:
.BYT $06, $08, $0A, $0C, $0E, $20, $22, $24, $26


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
    LDX #$0800
    STX $4305           ; Set transfer size to 780 bytes
    LDA #$01
    STA $420B           ; Initiate transfer

                        ; populate GCRAM

    stz $2121
    lda #%11001110      ; GGGRRRRR
    sta $2122
    lda #%00111001      ; 0BBBBBGG
    sta $2122           ; somewhat light grey

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

    lda #$82
    sta $2121           ; sprite color

    lda #%11111111      ; GGGRRRRR
    sta $2122
    lda #%01111111      ; 0BBBBBGG
    sta $2122           ; white

    lda #%00000100
    sta $210B          ; background 1 graphics to $4000

    lda #%00000010
    sta $2101          ; object graphics to $400

    lda #%00000011
    sta $2107           ; set background 1 tilemap VRAM address to $0000, and 64x64 tilemap

    lda #%00010001
    sta $212C           ; background 1 and objects to main screen
    sta $212D           ; and sub screen, for fun :)

    lda #%00010001
    sta $2105           ; set background mode to 1(or 5, for fun :), and 16x16 tiles on BG1

;    lda #%00000001
;    sta $2133           ; Turn on interlacing for hi-res mode, for fun :)

    lda #%00000010      ; set scope callibartion flag and/or hi-res flag, for fun :)
    sta flags

    lda #$FF
    sta c_hide

    ; Joost was here =)                  <------ crime scene

    stz $2102
    stz $2103           ; oam
    stz $2104
    stz $2104
    lda #$2E
    sta $2104
    lda #%00100000
    sta $2104

    lda #$02
    sta $2102
    stz $2103           ; oam
    stz $2104
    stz $2104
    lda #$2E
    sta $2104
    lda #%00100000
    sta $2104

    lda #boardWidth
    dea
    sta width
    lda #boardHeight
    dea
    sta height

    lda flags
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


    lda flags
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

    lda #$00
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

    lda $F00000
    ina
    sta rng1
    lda $F00001
    ina
    sta rng2


    ldy #mines
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
    sta $F00001



    stz $2115           ; set vram auto increment to low byte

    sep #$10            ; 8 bit X and Y
    inc width
    inc height

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

    jsl derive_s_pos
    lda tsx
    sta c1sx
    lda tsy
    sta c1sy

    lda width
    sta c2bx
    sta tbx
    jsl derive_s_pos
    lda tsx
    sta c2sx
    lda tsy
    sta c2sy

    lda #%10000000
    sta checkfor


    lda #%10000001
    sta $4200           ; enable auto joypad read and vblank NMI



forever:
    wai

    lda #$0F            ; = 00001111
    sta $2100           ; Turn on screen, full brightness

-   lda #%11111110
    ora $4212
    cmp #$FF
    beq -	; wait for auto joypad read to finish


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

/*
    lda #$8F            ; = 10001111
    sta $2100           ; Turn off screen, full brightness
    nop
    nop
    nop                 ; this will make a black streak appear whenever the processing is done for the frame
    nop                 ; (at the time of writing still within vblank lmao)
    nop
    nop
    lda #$0F            ; = 00001111
    sta $2100           ; Turn on screen, full brightness
*/

    jmp forever


.ENDS

.BANK 1 SLOT 0
.ORG 0
.SECTION "Routines"

rng:                    ; SMW rng function, thanks RGME!
    phy
    ldy #$01
    jsr tickrng
    dey
    jsr tickrng
    ply
    rtl

tickrng:
    lda rng1
    asl a
    asl a
    sec
    adc rng1
    sta rng1
    asl rng2
    lda #$20
    bit rng2
    bcc +
    beq +++
    bne ++
+   bne +++
++  inc rng2
+++ lda rng2
    eor rng1
    sta rng3, y
    rts

controls:
    sep #$10            ; 8 bit X and Y
    lda tp
    bit #%00001111
    bne +
    ldy tport
    lda $4016, y
    bit #%00000001
    bne ++
    stz tpp
    jmp controls_hide_and_exit
+
    jmp ++++
++
    rep #$10            ; 16 bit X and Y
    lda tp2
    bit #%00001111
    beq ++
    lda thold
    dea
    bne +
    stz tpp
    lda #$06
+

    jmp +++
++
    lda #$14
+++
    sta thold

    lda tp2
    eor tpp
    and tp2
    bit #%00001000 ; up
    beq +
    lda tby
    beq +
    dea
    sta tby

+   lda tp2
    eor tpp
    and tp2
    bit #%00000100 ; down
    beq +
    lda tby
    cmp height
    beq +
    ina
    sta tby

+   lda tp2
    eor tpp
    and tp2
    bit #%00000010 ; left
    beq +
    lda tbx
    beq +
    dea
    sta tbx

+   lda tp2
    eor tpp
    and tp2
    bit #%00000001 ; right
    beq +
    lda tbx
    cmp width
    beq +
    ina
    sta tbx
+

    jsl derive_s_pos

    lda tp2
    eor tpp
    and tp2
    bit #%10000000 ; B
    beq +
    jsl flag
+

    lda tp
    bit #%10000000 ; A
    beq +
    jsl check
+

    lda tp2
    sta tpp

    rtl
++++
    and #%00001111
    cmp #%00001111      ; check for super scope
    beq +
    jmp +++
+
    lda tport
    cmp #$01            ; if it's in controller port 1,
    beq +
    jmp controls_hide_and_exit ; bail, as the super scope only works in port 2
+
    lda flags
    bit #%00000010      ; check callibration flag
    beq ++
    lda #$80
    sta tsx
    lda #$70
    sta tsy
    lda tpp
    bit #%01000000      ; cursor button
    beq +
    lda tp2
    bit #%00000011      ; null and noise flags
    bne +
    lda tsx
    sec
    sbc scopex
    sta scopeofsx

    lda tsy
    sec
    sbc scopey
    sta scopeofsy
    lda flags
    eor #%00000010      ; flip calibration flag
    sta flags
+
    lda tp2
    sta tpp
    rep #$10            ; 16 bit X and Y
    rtl
++
    lda tp2
    bit #%00010000      ; pause button
    beq +
    lda flags
    ora #%00000010
    sta flags
    stz tpp
    jmp controls_hide_and_exit
+

    lda tp2
    bit #%00000011      ; null and noise flags
    beq +
    lda tp2
    sta tpp
    jmp controls_hide_and_exit
+
    lda tpp
    bit #%11000000      ; fire and cursor buttons, we only want the cursor visible when one or both are pressed
    bne +
    lda tp2
    sta tpp
    jmp controls_hide_and_exit
+
    rep #$10            ; 16 bit X and Y

    lda scopex
    clc
    adc scopeofsx
    sta tsx

    lda scopey
    clc
    adc scopeofsy
    sta tsy

    jsl derive_b_pos

    lda tpp
    eor tp2
    and tpp
    bit #%01000000      ; cursor button(release)
    beq +
    jsl flag
+

    lda tp2
    bit #%10000000      ; fire button
    beq +
    jsl check
+
    lda tp2
    sta tpp
    rtl
+++
    rep #$10            ; 16 bit X and Y
    lda flags
    bit #%00000001
    beq +
    lsr boardy
+

    sep #$10            ; 8 bit X and Y
    ldx #$10
    ldy tport
-   lda $4016, y
    lsr a
    rol mousex
    rol mousey
    dex
    bne -

    rep #$10            ; 16 bit X and Y

    ldx #$01
-
    lda tsx, x
    sta tsxp, x
    lda mousex, x
    bit #%10000000
    bne +
    clc
    adc tsx, x
    sta tsx, x

    lda tsxp, x
    bpl ++


    lda boardx, x
    clc
    adc swidth, x
    dea
    cmp tsx, x
    bpl ++
    sta tsx, x
    jmp ++
+
    and #%01111111
    sta mousex, x
    lda tsx, x
    sec
    sbc mousex, x
    sta tsx, x

    cmp boardx, x
    bpl ++
    lda tsxp, x
    bmi ++
    lda boardx, x
    sta tsx, x
++
    dex
    bpl -

    lda flags
    bit #%00000001
    beq +
    asl boardy
+

    jsl derive_b_pos


    lda tp
    eor tpp
    and tp
    bit #%10000000 ; right click
    beq +
    jsl flag
+

    lda tp
    bit #%01000000 ; left click
    beq +
    jsl check
+

    lda tp
    eor tpp
    and tp
    and #%11000000
    cmp #%11000000
    bne +
    sep #$10            ; 8 bit X and Y
    ldy tport
    lda #$01
    sta $4016
    lda $4016, y
    stz $4016
    rep #$10            ; 16 bit X and Y
+
    lda tp
    sta tpp

    rtl
controls_hide_and_exit:
    ldy tport
    lda #%00000001
-
    dey
    bmi +
    asl a
    bra -
+
    ora c_hide
    sta c_hide
    rep #$10            ; 16 bit X and Y
    rtl

derive_s_pos:
    lda flags
    bit #%00000001
    beq +
    lda #$03
    sta t0
    lda boardy
    clc
    ror
    sta t1
    lda #$08
    jmp ++
+
    lda #$07
    sta t0
    lda boardy
    sta t1
    lda #$10
++
    sta $211B
    stz $211B           ; set up PPU multiplication(lmao)

    lda tbx
    sta $211C
    lda $2134
    clc
    adc t0
    adc boardx
    sta tsx

    lda tby
    sta $211C
    lda $2134
    clc
    adc t0
    adc t1
    sta tsy
    rtl

derive_b_pos:
    lda flags
    bit #%00000001
    beq +
    lsr boardy
+

    ldx #$01
-
    lda tsx, x
    sec
    sbc boardx, x
    sta $4204
    lda flags
    bit #%00000001
    beq +
    lda #$8
    jmp ++
+
    lda #$10
++
    sta $4206
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    lda $4214
;    clc
;    adc #$10            ; bsnes >:(
    sta tbx, x

    dex
    bpl -

    lda flags
    bit #%00000001
    beq +
    asl boardy
+

    rtl

settilepos:
    pha
    sep #$10            ; 8 bit X and Y

    lda #$20
    sta $211B
    stz $211B           ; set up PPU multiplication(lmao)

    ldx drawin
    lda tby
    sta $211C
    lda $2134
    clc
    adc tbx
    sta drawqueue, x
    lda $2135
    inx
    sta drawqueue, x
    inx

    pla
    rts

check:
    ldx tbx
    lda board, x
    bit #%01000000      ; is this tile flagged?
    bne +
    bit #%00100000      ; has it already been cleared?
    beq ++

    lda #%10000000
    sta checkfor
    jsr check_tile

    sty t0

    lda #%01000000
    sta checkfor

    jsr check_tile

    lda #%10000000
    sta checkfor

    tya

    cmp t0
    bmi +
    jsl check_around
+

;    rep #$10            ; 16 bit X and Y
+
    jmp +++
++
    ora #%00100000
    sta board, x

    jsr settilepos

    bit #%10000000      ; is it a mine?
    bne +

    phx
    rep #$10            ; 16 bit X and Y
    jsr check_tile
    sep #$10            ; 8 bit X and Y
    plx
    lda $811c, y

    jmp ++
+
    phx
    rep #$10            ; 16 bit X and Y
    ldx tbx
    lda board, x
    ora #%01000000
    sta board, x
    sep #$10            ; 8 bit X and Y
    plx
    lda #$2A
    ldy #$01
++
    sta drawqueue, x
    inx

    stx drawin

    cpy #$00
    bne +
    ldx emptyin
    lda tbx
    sta emptyx, x
    lda tby
    sta emptyy, x
    inx
    stx emptyin
    rep #$10            ; 16 bit X and Y
    jmp +++
+
    tya
    rep #$10            ; 16 bit X and Y
    ldx tbx
    ora board, x
    sta board, x
+++
    rtl

flag:
    ldx tbx
    lda board, x
    bit #%00100000      ; has this tile already been cleared?
    bne +++

    jsr settilepos

    bit #%01000000
    bne +
    lda #$04
    jmp ++
+
    lda #$02
++
    sta drawqueue, x
    inx

    stx drawin

    rep #$10            ; 16 bit X and Y
    ldx tbx
    lda board, x
    eor #%01000000
    sta board, x
+++
    rtl

check_around:
    rep #$10            ; 16 bit X and Y
    dec tbx
    jsr check_safe
    dec tby
    jsr check_safe
    inc tbx
    jsr check_safe
    inc tbx
    jsr check_safe
    inc tby
    jsr check_safe
    inc tby
    jsr check_safe
    dec tbx
    jsr check_safe
    dec tbx
    jsr check_safe
    inc tbx
    dec tby
    rtl

check_safe:
    lda tbx
    bmi +++
    dea
    cmp width
    bpl +++
    lda tby
    bmi +++
    dea
    cmp height
    bpl +++
    ldx tbx
    lda board, x
    bit #%01100000      ; is this tile flagged, or has it already been cleared?
    bne +++

    ora #%00100000
    sta board, x

    jsr settilepos

    bit #%10000000      ; is it a mine?
    bne +

    phx
    rep #$10            ; 16 bit X and Y
    jsr check_tile
    sep #$10            ; 8 bit X and Y
    plx
    lda $811c, y

    jmp ++
+
    phx
    rep #$10            ; 16 bit X and Y
    ldx tbx
    lda board, x
    ora #%01000000
    sta board, x
    sep #$10            ; 8 bit X and Y
    plx
    lda #$2A
    ldy #$01
++
    sta drawqueue, x
    inx

    stx drawin

    cmp #$06
    bne +
    ldx emptyin
    lda tbx
    sta emptyx, x
    lda tby
    sta emptyy, x
    inx
    stx emptyin
    rep #$10            ; 16 bit X and Y
    jmp +++
+
    tya
    rep #$10            ; 16 bit X and Y
    ldx tbx
    ora board, x
    sta board, x
+++
    rts

check_tile:

    ldy #$0000
    dec tby
    ldx tbx
    dex
    jsr check_step
    inx
    jsr check_step
    inx
    jsr check_step
    inc tby
    ldx tbx
    dex
    jsr check_step
    inc tby
    ldx tbx
    dex
    jsr check_step
    inx
    jsr check_step
    inx
    jsr check_step
    dec tby
    ldx tbx
    inx
    jsr check_step

    rts

check_step:
    lda board, x
    bit checkfor
    beq +
    iny
+
    rts

.ENDS

.BANK 4 SLOT 0
.ORG 0
.SECTION "Graphics"
; $048000
.incbin "tiles"

.ENDS