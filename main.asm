;== Include memorymap, header info, and SNES initialization routines
.INCLUDE "header.inc"
.INCLUDE "InitSNES.asm"

;========================
; Start
;========================

.BANK 0 SLOT 0
.ORG 0
.SECTION "MainCode"

.DEFINE mines       = 15

.DEFINE tx          = $00
.DEFINE ty          = $01

.DEFINE width       = $02
.DEFINE height      = $03
.DEFINE swidth      = $04
.DEFINE sheight     = $05
.DEFINE boardx      = $06
.DEFINE boardy      = $07
.DEFINE c1sx        = $08
.DEFINE c1sy        = $09
.DEFINE c1bx        = $0A
.DEFINE c1by        = $0B
.DEFINE p1p         = $0C
.DEFINE p1p2        = $0D
.DEFINE p1hold      = $0E

.DEFINE mousex      = $0F
.DEFINE mousey      = $10
.DEFINE c1sxp       = $11
.DEFINE c1syp       = $12

.DEFINE rng1        = $13
.DEFINE rng2        = $14
.DEFINE rng3        = $15
.DEFINE rng4        = $16

.DEFINE checkfor    = $17

.DEFINE drawin      = $18
.DEFINE drawout     = $19
.DEFINE drawqueue   = $7EFF00

.DEFINE emptyin     = $1A
.DEFINE emptyout    = $1B
.DEFINE emptyx = $7EFD00
.DEFINE emptyy = $7EFE00

.DEFINE board       = $7F0000


.DEFINE flags       = $FF

numtiles:
.BYT $06, $08, $0A, $0C, $0E, $20, $22, $24, $26


vblank:
    lda #$8F            ; = 00001111
    sta $2100           ; Turn off screen, full brightness

    sep #$10
    stz $2102 ; oam
    lda c1sx
    sta $2104
    lda c1sy
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
    lda #%00001000      ; GGGRRRRR
    sta $2122
    lda #%00100001      ; 0BBBBBGG
    sta $2122           ; very dark grey

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

    lda #%00010101
    sta $2105           ; set background mode to 1, and 16x16 tiles on BG1

    lda #%00000001
    sta $2133           ; Turn on interlacing for hi-res mode, for fun :)

    lda #%00000001      ; set hi-res flag, for fun :)
    sta flags

    stz $2102
    stz $2103           ; oam
    stz $2104
    stz $2104
    lda #$2E
    sta $2104
    lda #%00100000
    sta $2104

    lda #$09
    sta width
    sta height

    lda #$10
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
    lda #$C0
    sta $21
    lda #$4
    jmp ++
+
    lda #$70
    sta $21
    lda #$8
++
    sta $211B
    stz $211B           ; set up PPU multiplication(lmao)

    ldx #$01
-
    lda width, x
    ina
    sta $211C
    lda $2134
    sta $20
    lda $21
    sec
    sbc $20
    sta boardx, x

    lda #$80
    sta $21
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


    ldy #$00
    sty $2117
    stz $2116
    lda #$02
    jsr fill
    ldx #$1F
    stx $2116
    sty $2117
    jsr fill
    ldx #$3F
    stx $2116
    sty $2117
    jsr fill
    ldx #$5F
    stx $2116
    sty $2117
    jsr fill
    ldx #$7F
    stx $2116
    sty $2117
    jsr fill
    ldx #$9F
    stx $2116
    sty $2117
    jsr fill
    ldx #$BF
    stx $2116
    sty $2117
    jsr fill
    ldx #$DF
    stx $2116
    sty $2117
    jsr fill
    ldx #$FF
    stx $2116
    sty $2117
    jsr fill
    ldy #$01
    ldx #$1F
    stx $2116
    sty $2117
    jsr fill


    jmp +
fill:
    sta $2118
    sta $2118
    sta $2118
    sta $2118
    sta $2118
    sta $2118
    sta $2118
    sta $2118
    sta $2118
    sta $2118
    rts

+
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
    beq -	; wait

    lda $4218
    bit #%00000001      ; check for SNES mouse
    beq +
    jmp ++++
+
    lda $4219
    bit #%00001111
    beq ++
    lda p1hold
    dea
    bne +
    stz p1p
    lda #$0A
+

    jmp +++
++
    lda #$14
+++
    sta p1hold

    lda $4219
    eor p1p
    and $4219
    bit #%00001000 ; up
    beq +
    lda c1by
    beq +
    dea
    sta c1by

+   lda $4219
    eor p1p
    and $4219
    bit #%00000100 ; down
    beq +
    lda c1by
    cmp height
    beq +
    ina
    sta c1by

+   lda $4219
    eor p1p
    and $4219
    bit #%00000010 ; left
    beq +
    lda c1bx
    beq +
    dea
    sta c1bx

+   lda $4219
    eor p1p
    and $4219
    bit #%00000001 ; right
    beq +
    lda c1bx
    cmp width
    beq +
    ina
    sta c1bx

+   lda $4219
    eor p1p
    and $4219
    bit #%10000000 ; B
    beq +
    lda c1bx
    sta tx
    lda c1by
    sta ty
    jsl flag
+

    lda $4218
    bit #%10000000 ; A
    beq +
    lda c1bx
    sta tx
    lda c1by
    sta ty
    jsl check
+

    lda $4219
    sta p1p
    lda $4218
    sta p1p2

    lda flags
    bit #%00000001
    beq +
    lda #$03
    sta $20
    lda boardy
    clc
    ror
    sta $21
    lda #$08
    jmp ++
+
    lda #$07
    sta $20
    lda boardy
    sta $21
    lda #$10
++
    sta $211B
    stz $211B           ; set up PPU multiplication(lmao)

    lda c1bx
    sta $211C
    lda $2134
    clc
    adc $20
    adc boardx
    sta c1sx

    lda c1by
    sta $211C
    lda $2134
    clc
    adc $20
    adc $21
    sta c1sy
    jmp emptyclear
++++
    ldx #$10
-   lda $4016
    lsr a
    rol mousex
    rol mousey
    dex
    bne -


    ldx #$01
-
    lda c1sx, x
    sta c1sxp, x
    lda mousex, x
    bit #%10000000
    bne +
    clc
    adc c1sx, x
    sta c1sx, x

    lda c1sxp, x
    bpl ++


    lda boardx, x
    clc
    adc swidth, x
    dea
    cmp c1sx, x
    bpl ++
    sta c1sx, x
    jmp ++
+
    and #%01111111
    sta mousex, x
    lda c1sx, x
    sec
    sbc mousex, x
    sta c1sx, x

    cmp boardx, x
    bpl ++
    lda c1sxp, x
    bmi ++
    lda boardx, x
    sta c1sx, x
++
    lda c1sx, x
    sec
    sbc boardx, x
    sta $4204
    lda #$10
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
    sta c1bx, x
    dex
    bpl -

    lda $4218
    eor p1p
    and $4218
    bit #%10000000 ; right click
    beq +
    lda c1bx
    sta tx
    lda c1by
    sta ty
    jsl flag
+

    lda $4218
    bit #%01000000 ; left click
    beq +
    lda c1bx
    sta tx
    lda c1by
    sta ty
    jsl check
+

    lda $4218
    eor p1p
    and $4218
    and #%11000000
    cmp #%11000000
    bne +
    lda #$01
    sta $4016
    lda $4016
    stz $4016
+
    lda $4218
    sta p1p

emptyclear:
    sep #$10            ; 8 bit X and Y
    ldx emptyout
    cpx emptyin
    beq +
    lda emptyx, x
    sta tx
    lda emptyy, x
    sta ty
    inx
    stx emptyout
    jsl check_around
+

    rep #$10            ; 16 bit X and Y
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

settilepos:
    pha
    sep #$10            ; 8 bit X and Y

    lda #$20
    sta $211B
    stz $211B           ; set up PPU multiplication(lmao)

    ldx drawin
    lda ty
    sta $211C
    lda $2134
    clc
    adc tx
    sta drawqueue, x
    lda $2135
    inx
    sta drawqueue, x
    inx

    pla
    rts

check:
    ldx tx
    lda board, x
    bit #%01000000      ; is this tile flagged?
    bne +
    bit #%00100000      ; has it already been cleared?
    beq ++

    lda #%10000000
    sta checkfor
    jsr check_tile

    sty $20

    lda #%01000000
    sta checkfor

    jsr check_tile

    lda #%10000000
    sta checkfor

    tya

    cmp $20
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
    ldx tx
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
    lda tx
    sta emptyx, x
    lda ty
    sta emptyy, x
    inx
    stx emptyin
    rep #$10            ; 16 bit X and Y
    jmp +++
+
    tya
    rep #$10            ; 16 bit X and Y
    ldx tx
    ora board, x
    sta board, x
+++
    rtl

flag:
    ldx tx
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
    ldx tx
    lda board, x
    eor #%01000000
    sta board, x
+++
    rtl

check_around:
    rep #$10            ; 16 bit X and Y
    dec tx
    jsr check_safe
    dec ty
    jsr check_safe
    inc tx
    jsr check_safe
    inc tx
    jsr check_safe
    inc ty
    jsr check_safe
    inc ty
    jsr check_safe
    dec tx
    jsr check_safe
    dec tx
    jsr check_safe
    rtl

check_safe:
    lda tx
    bmi +++
    dea
    cmp width
    bpl +++
    lda ty
    bmi +++
    dea
    cmp height
    bpl +++
    ldx tx
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
    ldx tx
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
    lda tx
    sta emptyx, x
    lda ty
    sta emptyy, x
    inx
    stx emptyin
    rep #$10            ; 16 bit X and Y
    jmp +++
+
    tya
    rep #$10            ; 16 bit X and Y
    ldx tx
    ora board, x
    sta board, x
+++
    rts

check_tile:

    ldy #$0000
    dec ty
    ldx tx
    dex
    jsr check_step
    inx
    jsr check_step
    inx
    jsr check_step
    inc ty
    ldx tx
    dex
    jsr check_step
    inc ty
    ldx tx
    dex
    jsr check_step
    inx
    jsr check_step
    inx
    jsr check_step
    dec ty
    ldx tx
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