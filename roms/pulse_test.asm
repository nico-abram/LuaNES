;;;; Taken from https://safiire.github.io/blog/2015/03/29/creating-sound-on-the-nes/
;  Create an iNES header
.ines {"prog": 1, "char": 0, "mapper": 0, "mirror": 0}


;;;;
;  Include all the symbols in the nes library
.inc <nes.sym>


;;;;
;  Open the prog section bank 0
.segment prog 0


;;;;
;  Structure to keep track of input
.org $0000
.scope controller_state
  .space b 1
  .space a 1
.


;;;;
;  Setup the interrupt vectors
.org $FFFA
.dw vblank
.dw reset
.dw irq


;;;;
;  Here is our code entry point
.org $C000
.scope reset
  sei                   ; SEt Interrupt (Disables them)
  cld                   ; CLear Decimal Mode

  ldx #$ff              
  txs                   ; Set the stack pointer

  ldx #$00
  stx nes.ppu.control
  stx nes.ppu.mask      ; Disable Vblank & Rendering

  jsr zero_apu          ; Zero all APU registers

  ;  We need to wait for at least 2 Vblanks to happen
  ;  before we know the PPU has stabilized at startup
  ;  Here we wait for the first one.
  wait_vblank1:
    bit nes.ppu.status
    bpl wait_vblank1

  ;  Before we wait for the second vblank, lets
  ;  zero all of the working RAM $0 to $800
  ;  The $200s are sprite OAM, and should be set to $ff
  clear_ram:
    lda #$00
    sta $00, x
    sta $100, x
    sta $300, x
    sta $400, x
    sta $500, x
    sta $600, x
    sta $700, x
    lda #$ff
    sta $200, x
    inx
    bne clear_ram

  ;  Now wait for the second vblank
  wait_vblank2:
    bit nes.ppu.status
    bpl wait_vblank2

  jsr initialize

  forever:
    jmp forever
  rti
.


;;;;
;  Initialize everything
.scope initialize
  ;  Enable pulse1 and pulse2 in the APU
  lda #%00000011
  sta nes.apu.channel_enable

  ;  Initialize the controller states
  lda #$00
  sta controller_state.a zp
  sta controller_state.b zp

  ;  Reenable interrupts, Turn Vblank back on
  lda #%10000000
  sta nes.ppu.control
  cli
  rts
.


;;;;
;  VBlank is called 60 times per second
.scope vblank
  jsr read_input
  rti
.


;;;;
;  IRQ, we are not using
.scope irq
  rti
.


;;;;
;  Zero all the APU registers
.scope zero_apu
  lda #$00
  ldx #$00
  loop:
    sta $4000, x
    inx
    cpx $18
    bne loop
  rts
.


;;;;
;  Read input from controller 1
.scope read_input
  lda #$01                ; strobe joypad
  sta nes.controller1
  lda #$00
  sta nes.controller1

  ;  Handle Button A
  lda nes.controller1
  and #$01
  beq update_a_state

  ;  A is pressed, but did it just change to being pressed now?
  ldx controller_state.a zp
  bne update_a_state

  ;  do the thing A does
  jsr play_a440

  update_a_state:
    sta controller_state.a zp

  ;  Handle Button B
  lda nes.controller1
  and #$01
  beq update_b_state

  ;  B is pressed, but did it just change to being pressed now?
  ldx controller_state.b zp
  bne update_b_state

  ;  Do the thing B does
  jsr play_a220

  update_b_state:
    sta controller_state.b zp

  rts
.


;;;;
;;  This will play an A 220hz note
;;  On the pulse1 generator
.scope play_a220
  pha
  
;DDLC VVVV 	Duty (D), envelope loop / length counter halt (L), constant volume (C), volume/envelope (V) 
  lda #%10001111
  sta nes.apu.pulse1.control

;TTTT TTTT 	Timer low (T)
  lda #%11111011
  sta nes.apu.pulse1.ft

;LLLL LTTT 	Length counter load (L), timer high (T) 
  lda #%11111001
  sta nes.apu.pulse1.ct

  pla
  rts
.


;;;;
;;  This will play an A 220hz note
;;  On the pulse2 generator
.scope play_a440
  pha
;DDLC VVVV 	Duty (D), envelope loop / length counter halt (L), constant volume (C), volume/envelope (V) 
  lda #%10011111
  sta nes.apu.pulse2.control

;TTTT TTTT 	Timer low (T)
  lda #%11111101
  sta nes.apu.pulse2.ft

;LLLL LTTT 	Length counter load (L), timer high (T) 
  lda #%11111000
  sta nes.apu.pulse2.ct

  pla
  rts
.

