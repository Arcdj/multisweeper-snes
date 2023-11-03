
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
    bit #%00000001
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
    lda tpp
    and #$F0
    sta tpp
    lda #$06
+

    jmp +++
++
;    lda #$14
    lda thold
    cmp #$14
    beq +++
    ina
    cmp #$0B
    bne +++
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
;    eor tpp
;    and tp2
    bit #%10000000 ; B
    beq +
    jsl flag
    bra ++
+
    lda tflags
    and #%11111100
    sta tflags          ; reset flagging flags
++

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

    ldx #$01
-
    lda scopex, x       ; calibration offset
    clc
    adc scopeofsx, x    ; raw extlatch data
    sta tsx, x

    dex
    bpl -

    jsl derive_b_pos

    lda tpp
    eor tp2
    and tpp
    bit #%01000000      ; cursor button(release)
    beq +
    jsl flag

    lda tflags
    and #%11111100
    sta tflags          ; reset flagging flags
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
    lda mousex, x
    bit #%10000000
    bne ++              ; branch if negative, because it's sign and magnitude
    clc
    adc tsx, x          ; add mouse movement to screen position
    bcc +               ; if it's overflown
    lda #$FF            ; do this
+
    sta tsx, x          ; new position

    lda boardx, x
    clc
    adc swidth, x       ; board position + board width = right/bottom edge of board
    dea                 ; -1
    cmp tsx, x          ; if the mouse moved past it,
    bcs +++
    sta tsx, x          ; snap it back
    jmp +++
++
    and #%01111111      ; delete the sign 1
    sta mousex, x
    lda tsx, x
    sec
    sbc mousex, x       ; subtract the mouse movement from the screen position, because it's negative
    bcs +
    lda #$00
+
    sta tsx, x          ; new position

    cmp boardx, x       ; board position = left/top edge of board
    bcs +++
    lda boardx, x
    sta tsx, x
+++
    dex
    bpl -

    lda flags
    bit #%00000001
    beq +
    asl boardy
+

    jsl derive_b_pos


    lda tp
;    eor tpp
;    and tp
    bit #%10000000 ; right click
    beq +
    jsl flag
    bra ++
+
    lda tflags
    and #%11111100
    sta tflags          ; reset flagging flags
++
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


    and #%00001111
    sta t0

    lda #%01000000
    sta checkfor

    jsr check_tile

    lda #%10000000
    sta checkfor

    tya

    cmp t0
    bcc +
    jsl check_around
+

;    rep #$10            ; 16 bit X and Y
    jmp ++++
++
    ora #%00100000
    sta board, x

    jsr settilepos

    bit #%10000000      ; is it a mine?
    bne ++

    phx
    rep #$10            ; 16 bit X and Y
    jsr check_tile

    ldx tilesRemaining
    dex
    stx tilesRemaining
    bne +
    lda $7007FF
    ina
    sta $7007FF
+

    sep #$10            ; 8 bit X and Y
    plx
    lda numtiles, y

    jmp +++
++
    phx
    rep #$10            ; 16 bit X and Y
    ldx minesRemaining
    dex
    stx minesRemaining

    ldx tbx
    lda board, x
    ora #%01000000
    sta board, x
    sep #$10            ; 8 bit X and Y
    plx
    lda #$2C
    ldy #$01
+++
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
    jmp ++++
+
    tya
    rep #$10            ; 16 bit X and Y
    ldx tbx
    ora board, x
    sta board, x
++++
    rtl

flag:
    ldx tbx
    lda board, x
    bit #%00100000      ; has this tile already been cleared?
    bne ++++
    lda tflags
    bit #%00000001      ; have we figured out whether this hold will add or remove flags?
    bne +++
    ; no, we will do so now
    lda board, x
    bit #%01000000
    beq +
    ; current tile is a flag
    lda tflags
    ora #%00000001      ; so we will only remove them
    jmp ++
+
    ; current tile is not a flag
    lda tflags
    ora #%00000011      ; so we will only add them
++
    sta tflags
+++
    ; yes, we will obey our decision
    bit #%00000010      ; add or remove?
    beq +
    ; add
    lda board, x
    bit #%01000000      ; is the tile flagged?
    bne ++++            ; exit
;    lda $7007FF
;    ina
;    sta $7007FF
    lda #$04
    jmp ++
+
    ; remove
    lda board, x
    bit #%01000000      ; is the tile flagged?
    beq ++++            ; exit
    lda #$02
++
    jsr settilepos

    sta drawqueue, x
    inx

    stx drawin

    rep #$10            ; 16 bit X and Y
    ldy minesRemaining
    cmp #$02
    beq +
    dey
    jmp ++
+
    iny
++
    sty minesRemaining

    ldx tbx
    lda board, x
    eor #%01000000
    sta board, x
++++
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
    bpl +
    jmp ++++
+
    dea
    cmp width
    bmi +
    jmp ++++
+
    lda tby
    bpl +
    jmp ++++
+
    dea
    cmp height
    bpl ++++
    ldx tbx
    lda board, x
    bit #%01100000      ; is this tile flagged, or has it already been cleared?
    bne ++++

    ora #%00100000
    sta board, x

    jsr settilepos

    bit #%10000000      ; is it a mine?
    bne ++

    phx
    rep #$10            ; 16 bit X and Y
    jsr check_tile

    ldx tilesRemaining
    dex
    stx tilesRemaining
    bne +
    lda $7007FF
    ina
    sta $7007FF
+

    sep #$10            ; 8 bit X and Y
    plx
    lda numtiles, y

    jmp +++
++
    phx
    rep #$10            ; 16 bit X and Y
    ldx minesRemaining
    dex
    stx minesRemaining

    ldx tbx
    lda board, x
    ora #%01000000
    sta board, x
    sep #$10            ; 8 bit X and Y
    plx
    lda #$2C
    ldy #$01
+++
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
    bra ++++
+
    tya
    rep #$10            ; 16 bit X and Y
    ldx tbx
    ora board, x
    sta board, x
++++
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

ezrainbow:          ; takes cgram address in the accumalator and runs rainbow
    pha
    sta $2121
    lda $213B
    sta t0
    lda $213B
    sta t1

    jsl rainbow

    pla
    sta $2121
    lda t0
    sta $2122
    lda t1
    sta $2122

    rtl

rainbow:            ; Rainbow function, takes a GCRAM formatted color in t0 and t1
    ; Inspired by (aka blatantly copied from) a tutorial example in FUZE4 Nintendo Switch
    ; first, get the color out of CGRAM format and into 3 seperate R, G and B values

    lda t0      ; GGGRRRRR
    and #%00011111
    sta t2      ; R

    lda t0      ; GGGRRRRR
    lsr
    lsr
    lsr
    lsr
    lsr
    sta t3
    lda t1      ; 0BBBBBGG
    and #%00000011
    asl
    asl
    asl
    ora t3
    sta t3      ; G

    lda t1      ; 0BBBBBGG
    lsr
    lsr
    and #%00011111
    sta t4      ; B

    ; rainbow time
    lda t2      ; R
    beq +
    lda t4      ; B
    bne +
    ; if red > 0 and blue = 0
    dec t2      ; R
    inc t3      ; G
    jmp ++
+
    lda t3      ; G
    beq +
    ; if green > 0
    dec t3      ; G
    inc t4      ; B
    jmp ++
+
    ; if green = 0
    dec t4      ; B
    inc t2      ; R
++
    ; now we need to get everything back into CGRAM format
    lda t2      ; R
    and #%00011111
    sta t0      ; GGGRRRRR

    lda t3      ; G
    asl
    asl
    asl
    asl
    asl
    ora t0
    sta t0      ; GGGRRRRR

    lda t4      ; B
    asl
    asl
    sta t1      ; 0BBBBBGG

    lda t3      ; G
    lsr
    lsr
    lsr
    and #%00000011
    ora t1
    sta t1

    rtl

binary2decimal:
    ldy #4

-
    stx $4204
    lda #10
    sta $4206
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    lda $4216
    sta b2d_result, y

    ldx $4214
    beq +
    dey
    bpl -
    rtl
+
    lda #00
-
    dey
    bmi +
    sta b2d_result, y
    jmp -
+
    rtl

.ENDS