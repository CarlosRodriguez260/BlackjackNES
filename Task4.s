.segment "HEADER"
.byte $4e, $45, $53, $1a, $02, $01, $00, $00

.segment "ZEROPAGE"
inputs: .res 1 ; Current state of the controller buttons
counter_on: .res 1 ; Decides if the counter should continue or stop

card_pointer: .res 2 ; Tile number currently being rendered
card_pointer_dealer: .res 2 ; Dealer's tile number for rendering
card_pointer_player: .res 2 ; Player's tile number for rendering
card_pointer_reviser: .res 2 ; To check if card positions are out of bounds

result_text: .res 2 ; Pointer to the result text in nametable

card_offset: .res 2 ; Pointer used for rendering to nametable
card_offset_dealer: .res 2 ; Pointer to dealer's card position in  the nametable
card_offset_player: .res 2 ; Pointer to player's card position in the nametable

player_bid: .res 2 ; Counter for the bid amount
player_bid_temp: .res 2 ; Temporary pointer for bid rendering
player_bid_display: .res 2 ; Pointer to bid display the  position in nametable
player_cash: .res 3 ; Counter for the cash amount
player_cash_temp: .res 2 ; Temporary pointer for cash rendering
player_cash_display: .res 2 ; Pointer to cash display the position in nametable
OFV: .res 1 ; Used for dealing with overflow when adding or subtracting cash

dealer_wins_flag: .res 1 ; Flag for dealer winning
player_wins_flag: .res 1 ; Flag for player winning
natural_blackjack_dealer: .res 1 ; Verifies if theres a natural blackjack
natural_blackjack_player: .res 1 ; Verifies if theres a natural blackjack
tie_flag: .res 1 ; Flag for tie
ace_counter_dealer: .res 1 ; Flag for amount of aces with dealer
ace_counter_player: .res 1 ; Flag for amount of aces with player
 
A_pressed:          .res 1   ; Flag for A button pressed
up_pressed:         .res 1   ; Flag for Up button pressed
down_pressed:       .res 1   ; Flag for Down button pressed
B_pressed:          .res 1   ; Flag for B button pressed
start_pressed:      .res 1   ; Flag for Start button pressed

; Variables for the cards points
current_points: .res 1 ;Used for storing the current points
dealers_points: .res 1 ;Used for storing the current points of the dealer
players_points: .res 1  ;Used for storing the current points of the player

dummy_pointsD: .res 1 ;Used for state testing without point system implemented (Dealer)
dummy_pointsP: .res 1 ;Used for state testing without point system implemented (Player)

seed: .res 2 ;Used for getting random number between 1-13
seed_high: .res 1 ;Used for getting random number between 1-4
seed_low: .res 1 ;Used for getting random number between 1-4
card_deck: .res 1 ;Determines from which deck to draw
card_choice: .res 1 ;Determines what specific card to draw from the deck
counter: .res 1 ;Used for making better randomization
reviser_offset: .res 1 ;Used for making cards unique
dealer_flipped: .res 1 ;Used to checking if the first card of dealer is flipped
current_player: .res 1 ;Determines who is taking a card out
last_tile_displayed: .res 1;Last tile seen when generating a card

; States of the Game
; States will be used to determine what behaviors are allowed depending on the situation
; Here are the summed up behaviors per state:
;   + before_game = 1 -> The game has not started yet. All we can do here is press "A" to start the game (COMPLETED)
;     - BONUS: Make it so text displays that says "Press 'A' to start game" (use sprites if possible)
;     - Pressing "A" will
;       + before_game = 1 -> 0
;       + middle_game = 0 -> 1
;       + Generate two cards for dealer and player (first one for dealer has value but will be flipped)
;   + middle_game = 1 -> The game is in process. Here we can press "A", "B", "Up" and "Down"
;      - Initially, the player has $20 in their cash reserve, and can use the bid to bet a certain amount of their money
;         + The bid cannot be higher than the amount of cash the player has, and the bid can only go up to $100
;         + If the player loses all his money, they should be forced to reset the game
;      - Pressing :
;         + A -> Give a card to the player from the deck
;         + B -> Player stays with his cards and dealer must draw until his card points are >= 17


before_game: .res 1 ; Indicates us that we are still waiting for the game to start
middle_game: .res 1 ; Indicates us that we are in the middle of a game
round_win: .res 1 ; Indicates us that the player won a round
round_lost: .res 1 ; Indicates us that the player lost a round
round_tie: .res 1 ; Indicates us that the player tied the round
out_of_money: .res 1 ;Indicates a special state in which the player lost all money and can only reset the game
full_of_money: .res 1 ;Indicates a special state in which the player got $1000 and can only reset the game

; Segment is an assemble directive.
.segment "CODE"
PPU_CTRL = $2000
PPU_STATUS = $2002
PPU_ADDR = $2006
PPU_DATA = $2007
PPU_MASK = $2001
OAM_ADDR = $2003 ; OAM (Object Attribute Memory)
OAM_DMA = $4014 ; Used to set where in OAM we want to write to, usually $00

CONTROLLER = $4016
BTN_RIGHT   = %00000001
BTN_LEFT    = %00000010
BTN_DOWN    = %00000100
BTN_UP      = %00001000
BTN_START   = %00010000
BTN_SELECT  = %00100000
BTN_B       = %01000000
BTN_A       = %10000000

dealerNum = 0
; Interrupt Vectors : Certain events that interrupt the processor. The type of event tells the processor what to do
;   IRQ (Interrupt Request) -> Triggered by NES sound processor or cartridge hardware
;   NMI (Non-maskable Interrupt) -> Occurs when PPU starts preparing next frame of graphics, 60 times per second
;   Reset Handler or Reset Vector -> Occurs when the system is first turned on, or user presses reset button

; Proc is another assemble directive that lets you create nex lexical scopes in your code. Every proc makes it so a label can be unique even if the same name is used on different procs.
.proc irq_handler
  RTI
.endproc

.proc nmi_handler
  PHA
  TXA
  PHA
  TYA
  PHA

  LDA #$00
  STA OAM_ADDR
  LDA #$02
  STA OAM_DMA
  LDA #$00
  STA $2005
  STA $2005

  LDX counter
  INX
  STX counter

  skipcounter:
  ;Read Controller Inputs
  LDA #$01
  STA CONTROLLER
  LDA #$00
  STA CONTROLLER

  LDA #%00000001
  STA inputs

  readbuttons:
    LDA CONTROLLER
    LSR A 
    ROL inputs
    BCC readbuttons
  
  ; If before_game = 1, only accept "A" input
  before:
    LDA before_game
    CMP #1
    BNE middle
    LDA inputs
    AND #%10001100
    STA inputs
    jmp endofstates

  ; If middle_game = 1, we accept 'A', 'B' as inputs (basically no need to alter the inputs variable)
  middle:
    LDA middle_game
    CMP #1
    BNE win
    LDA inputs
    AND #%11110011
    STA inputs
    jmp endofstates
  
  win: ; If game_win = 1 or game_lost = 1, player won/lost a round and we only accept 'reset' button which will reset everything except the cash counter (Should have more/less money than before)
    LDA round_win
    CMP #1
    BNE lost
    LDA inputs
    AND #BTN_START
    STA inputs
    jmp endofstates
  lost:
    LDA round_lost
    CMP #1
    BNE tie
    LDA inputs
    AND #BTN_START
    STA inputs
    jmp endofstates
  tie:
    LDA round_tie
    CMP #1
    BNE out_money
    LDA inputs
    AND #BTN_START
    STA inputs
    jmp endofstates

  out_money: ; If out_of_money = 1, player lost the entire game and we only accept a reset
    LDA out_of_money
    CMP #1
    BNE full_money
    LDA inputs
    AND #BTN_START
    STA inputs
    jmp endofstates

  full_money: ; If full_of_money = 1, player won the entire game and we only accept a reset
    LDA full_of_money
    CMP #1
    BNE endofstates
    LDA inputs
    AND #BTN_START
    STA inputs

  endofstates:
  PLA
  TAY
  PLA
  TAX
  PLA
  RTI
.endproc

;-----------------------------------------------------------------------------

.proc reset_handler
  SEI
  CLD
  LDX #$40
  STX $4017
  LDX #$FF
  TXS
  INX
  STX PPU_CTRL
  STX PPU_MASK
  STX $4010
  BIT PPU_STATUS

vblankwait:
  BIT PPU_STATUS
  BPL vblankwait
clear_memory:
  lda #$00
  sta $0000, x             ; Clear RAM page 0
  sta $0100, x             ; Clear RAM page 1
  sta $0200, x             ; Clear RAM page 2
  sta $0300, x             ; Clear RAM page 3
  sta $0400, x             ; Clear RAM page 4
  sta $0500, x             ; Clear RAM page 5
  sta $0600, x             ; Clear RAM page 6
  sta $0700, x             ; Clear RAM page 7
  inx
  bne clear_memory         ; Loop until X overflows to 0
  vblankwait1:
  BIT PPU_STATUS
  BPL vblankwait1
  JMP main
.endproc

;-----------------------------------------------------------------------

; Address $3f00 is where the PPU palette memory begins
; Addresses $2000 - $5fff are for Memory Mapped I/O (MMIO)
; Memory addresses in the low $2000 are connections to PPU
; Sprite Buffer in CPU -> $0200-$02ff
.proc main
  LDX PPU_STATUS
  LDX #$3f
  STX PPU_ADDR
  LDX #$00
  STX PPU_ADDR

  ; Set low byte of seed and other states to 0
  LDX #$00
  STX seed_low
  STX middle_game
  STX round_win
  STX round_lost
  STX out_of_money
  STX full_of_money
  STX dealer_flipped
  STX OFV
  STX current_player
  ; Set high byte of seed, counter_on and initial state of game to 1
  LDX #$01
  STX seed_high
  STX before_game
  STX counter_on

  ldx #$00
  stx card_offset_dealer ;set initial tile position for dealer
  stx card_offset_player ;set initial tile position for player


   ; Reset button pressed flags
    ldx #$00
    stx A_pressed
    stx up_pressed
    stx B_pressed
    stx start_pressed
    stx down_pressed
    stx card_offset    ; Reset current tile number
    stx card_pointer_dealer   ; Reset nametable pointers
    stx card_pointer_player
    stx card_pointer
    stx card_pointer + 1

;--------------------------------------------------------------------
  ;Initialize card pointer
      LDA #$20
      STA card_pointer_dealer
      LDA #$82
      STA card_pointer_dealer + 1

      LDA #$22
      STA card_pointer_player
      LDA #$62
      STA card_pointer_player + 1 

      LDA #$22
      STA player_bid_display
      LDA #$1B
      STA player_bid_display + 1

      LDA #$22
      STA player_cash_display
      LDA #$11
      STA player_cash_display+1

    ; Initialize player bet 
    ldx #0
    stx player_bid
    ldx #5
    stx player_bid + 1

    ; Initialize player cash
    ldx #9
    stx player_cash
    ldx #9
    stx player_cash+1
    ldx #0
    stx player_cash+2

    ; Initialize points
    ldx #0
    stx current_points
    stx dealers_points
    stx players_points

    ; Initialize flags
    ldx #0
    stx natural_blackjack_dealer
    stx natural_blackjack_player
    stx tie_flag
    stx player_wins_flag
    stx dealer_wins_flag

    ; Initialize ace counters
    ldx #$00
    stx ace_counter_player
    stx ace_counter_dealer

    ;initialize Win/Lose Text
    LDA #$20
    STA result_text
    LDA #$2c
    STA result_text + 1
;---------------------------------------------------------------------
  ldx #0
  palette_loading:
    LDA palettes, X
    STA PPU_DATA
    INX
    CPX #$20
    BNE palette_loading
    jsr backgroundloading
    jmp continue_forever


  .proc backgroundloading
  PHA
  TXA
  PHA
  TYA
  PHA
    lda #0               ; Enable NMI and set PPU control
    sta PPU_CTRL
    lda #0               ; Enable background and sprites
    sta PPU_MASK
  vblankwait3:
  BIT PPU_STATUS
  BPL vblankwait3
    background:
      LDX #$00
      LDA #$20
      STA PPU_ADDR
      LDA #$00
      STA PPU_ADDR
      LDX #$00
    background_loop:
      LDA testBG, X
      STA PPU_DATA
      INX
      CPX #$ff
      BNE background_loop
    
    background1:
      LDX #$00
      LDA #$21
      STA PPU_ADDR
      LDA #$00
      STA PPU_ADDR
    
    background_loop1:
      LDA testBG+256, X
      STA PPU_DATA
      INX
      CPX #$ff
      BNE background_loop1
    
    background2:
      LDX #$00
      LDA #$22
      STA PPU_ADDR
      LDA #$00
      STA PPU_ADDR

    background_loop2:
      LDA testBG+512, X
      STA PPU_DATA
      INX
      CPX #$ff
      BNE background_loop2
    
    background3:
      LDX #$00
      LDA #$23
      STA PPU_ADDR
      LDA #$00
      STA PPU_ADDR

    background_loop3:
      LDA testBG+768, X
      STA PPU_DATA
      INX
      CPX #$bf
      BNE background_loop3
    lda #%10100000               ; Enable NMI and set PPU control
    sta PPU_CTRL
    lda #%00001110               ; Enable background and sprites
    sta PPU_MASK
  PLA
  TAY
  PLA
  TAX
  PLA
  rts
  .endproc
;--------------------------------------------------------------------
  continue_forever:
    ; Prepare bid rendering pointers
    lda player_bid_display
    sta player_bid_temp
    lda player_bid_display + 1
    sta player_bid_temp + 1

    jsr bid_counter         ; Draw the initial bid counter on screen

    lda player_cash_display
    sta player_cash_temp
    lda player_cash_display+1
    sta player_cash_temp+1

    jsr cash_counter

    lda player_cash_temp
    sta player_cash_display
    lda player_cash_temp + 1
    sta player_cash_display + 1
    
    lda player_bid_temp
    sta player_bid_display
    lda player_bid_temp + 1
    sta player_bid_display + 1

enable_rendering:
    lda #%10100000               ; Enable NMI and set PPU control
    sta PPU_CTRL
    lda #%00001110               ; Enable background and sprites
    sta PPU_MASK
    
    jmp forever

;---------------------------------------------------------------------
load_player:
    pha                          ; Keep accumulator
    txa
    pha                          ; Keep X register
    tya
    pha   
 ; Start button logic
    lda inputs
    and #BTN_START
    beq start_unpressed          ; Skip if Start not pressed
    jsr start_press          ; Handle Start button press
    lda start_pressed
    cmp #1
    beq initiate_reset

start_unpressed:
    lda start_pressed
    cmp #1
    bne continue_game
    jsr reset_game         ; Reset game state if Start was pressed
    ldx #$00
    stx start_pressed
    jmp continue_game

initiate_reset:
    jmp continue_game             ; Prevent immediate reset until button is released

;-------------------------------------------------------------------------------------

continue_game:

    ; A button logic
    lda inputs
    and #BTN_A
    beq A_unpressed               ; Skip if A  its not pressed
    jsr A_press             ; Handle A button press
    lda A_pressed
    cmp #1
    beq skipA

    skipA:
      jmp skip_A_button

A_unpressed: 
    lda A_pressed
    cmp #1
    bne skipA

    A_in_before: ; Used for determining what 'A' does in the 'before_game' state, which is the state before starting a game
      LDX before_game
      CPX #1
      BNE jump
      jmp continue

      jump:
        jmp A_in_middle
        
      continue:
      lda #0
      sta current_player

      lda card_pointer_dealer
      sta card_pointer
      lda card_pointer_dealer + 1
      sta card_pointer + 1
      JSR draw_card
      jsr check_ace

      ;For Stalling Card Generation
      LDA counter
      CLC
      ADC #$ff
      ADC #$2f
      STA counter

      lda card_pointer
      sta card_pointer_dealer
      lda card_pointer + 1
      sta card_pointer_dealer + 1
      JSR draw_card
      jsr check_ace

      ;For Stalling Card Generation
      LDA counter
      CLC
      ADC #$ff
      ADC #$2f
      STA counter

      lda card_pointer
      sta card_pointer_dealer
      lda card_pointer + 1
      sta card_pointer_dealer + 1

      lda #1
      sta current_player

      lda card_pointer_player
      sta card_pointer
      lda card_pointer_player+1
      sta card_pointer+1
      JSR draw_card
      jsr check_ace

      ;For Stalling Card Generation
      LDA counter
      CLC
      ADC #$ff
      ADC #$2f
      STA counter

      lda card_pointer
      sta card_pointer_player
      lda card_pointer + 1
      sta card_pointer_player + 1
      JSR draw_card
      jsr check_ace
      lda card_pointer
      sta card_pointer_player
      lda card_pointer + 1
      sta card_pointer_player + 1

      ;For Stalling Card Generation
      LDA counter
      CLC
      ADC #$ff
      ADC #$2f
      STA counter

      LDA #0
      STA before_game
      LDA #1
      STA middle_game ; Once we press 'A' to start the game, we move into the 'middle_game' state and make 'before_game' = 0
      ldx #$00
      stx A_pressed
      jsr natural_blackjack

      ; If dealer has natural blackjack and player does not, we end the round on a loss for player
      ; If dealer has no NB and player does, we end the round on a win for player
      ; If neither have NB, proceed with the normal game. If they both have NB, tie the round

      LDA dealer_wins_flag
      CMP #0
      BEQ skipToPlayer ; If dealer_wins_flag = 1, skip branch

      ; In here, dealer_wings_flag = 1
      LDA player_wins_flag
      CMP #0
      BEQ skipToDealer ; If player_wins_flag = 1, skip branch

      ; In here, both conditions are 1 so it's a tie
      jmp roundTie

      skipToPlayer:
        ; In here, dealer_wins_flag = 0
        LDA player_wins_flag
        CMP #0
        BEQ skip_A_button ; If player_wins_flag = 1, go to round win
        jmp roundWin
      
      skipToDealer:
        ; In here, player_wins_flag = 0
        LDA dealer_wins_flag
        CMP #0
        BEQ skip_A_button ; If dealer_wins_flag = 1, go to round loss
        jmp roundLoss

      jmp skip_A_button

    A_in_middle: ; Used for determining what 'A' does in the 'middle_game' state, which is the state of a game in progress
      lda #1
      sta current_player
      lda card_pointer_player
      sta card_pointer
      lda card_pointer_player+1
      sta card_pointer+1
      jsr draw_card
      lda card_pointer
      sta card_pointer_player
      lda card_pointer+1
      sta card_pointer_player+1        
      ldx #$00
      stx A_pressed

      lda players_points ; Check if player points step over 21 and make him lose the round if so
      cmp #22
      bcs roundLossx
      jmp skip_A_button

      roundLossx:
        jmp roundLoss
    
skip_A_button:
;---------------------------------------------------------------------
;Fix skip if A press
  lda A_pressed
  cmp #1
  beq skip_B_buttonx
  jmp continueA

  skip_B_buttonx:
    jmp skip_B_button
    ; B button logic

    continueA:
    lda inputs
    and #BTN_B
    beq b_unpressed               ; Skip if B not pressed
    jsr B_press              ; Handle B button press
    lda B_pressed
    cmp #1
    beq skip_B_buttonx
    jmp b_unpressed

b_unpressed: ; Since 'B' is only used in middle state, it does not need special definitions
    lda B_pressed
    cmp #1
    bne skip_B_buttonx

    ; Check if dealer points are below 17. 
    below17:
      LDA dealers_points
      CMP #17
      BEQ aboveequal17
      BCS aboveequal17

      lda #0
      sta current_player

      ;For Stalling Card Generation
      LDA counter
      CLC
      ADC #$ff
      ADC #$2f
      STA counter

      lda card_pointer_dealer
      sta card_pointer
      lda card_pointer_dealer + 1
      sta card_pointer + 1
      lda card_offset_dealer
      sta card_offset
      jsr draw_card            
      jsr check_ace
      lda card_pointer
      sta card_pointer_dealer
      lda card_pointer + 1
      sta card_pointer_dealer + 1
      lda card_offset
      sta card_offset_dealer

      lda dealers_points ; Check if dealer points step over 21 and make him lose the round if so
      cmp #22
      bcs roundWin
      jmp below17
    
    aboveequal17: ; If we get here, it means neither points went above 21 
      lda #0
      sta middle_game
      ; From here on, we decide what state to go into based on the points and then cash reserve
      ; The points determine if we lose or gain money
      ; If we get to 0 dollars, we lose the whole game. If we get to 1000 dollars, we win the whole game.
      ; If the cash reserve is not 0 or 1000+ after subtracting or adding, we will go to the next round.
      LDA players_points
      CMP dealers_points
      BEQ roundTie ; If points are tied, do nothing to cash reserve
      BCS roundWin ; If player has more points, add money to his reserve
      jmp roundLoss ; If player has less points, subtract money from his reserve

      roundTie:
        lda #0
        sta middle_game
        LDA dealers_points
        STA $02fe
        LDA players_points
        STA $02ff
        LDA natural_blackjack_dealer
        STA $02fc
        LDA natural_blackjack_player
        STA $02df

        LDX #1
        STX round_tie ; Used for generating display of tie

        LDA #$20
        STA card_pointer
        LDA #$82
        STA card_pointer+1
        jsr draw_card

        noTieBlackjack:
        ldx #$00
        stx B_pressed
        ldx #0
        stx middle_game

        jsr display
        jmp skip_B_button

      roundWin:
        lda #0
        sta middle_game
        LDA dealers_points
        STA $02fe
        LDA players_points
        STA $02ff
        LDA natural_blackjack_dealer
        STA $02fc
        LDA natural_blackjack_player
        STA $02fd

        LDX #1
        STX round_win

        LDA #$20
        STA card_pointer
        LDA #$82
        STA card_pointer+1
        jsr draw_card

        ; If player wins and he has a blackjack, double the amount of money won in bid
        LDA player_wins_flag
        CMP #1
        BNE noPlayerBlackjack
        jsr increase_cash
        noPlayerBlackjack:
        jsr increase_cash ; This will determine if the whole game is won or not
        jsr cash_counter
        jsr display

        ldx #$00
        stx B_pressed
        ldx #0
        stx middle_game
        jmp skip_B_button
      
      roundLoss:
        lda #0
        sta middle_game
        LDA dealers_points
        STA $02fe
        LDA players_points
        STA $02ff
        LDA natural_blackjack_dealer
        STA $02fc
        LDA natural_blackjack_player
        STA $02fd

        LDX #1
        STX round_lost

        LDA #$20
        STA card_pointer
        LDA #$82
        STA card_pointer+1
        jsr draw_card

        jsr decrease_cash ; This will determine if the whole game is lost or not
        jsr cash_counter
        jsr display

        ldx #$00
        stx B_pressed
        ldx #0
        jmp skip_B_button

skip_B_button:
;------------------------------------------------------------------------

; Up button logic
    lda inputs
    and #BTN_UP
    beq up_unpressed  ; Skip if button Up is not pressed
    jsr up_press      ; Handle how Up button is press
    lda up_pressed
    cmp #1
    beq exit_increment_bid
up_unpressed:
    lda up_pressed
    cmp #1
    bne exit_increment_bid
    jsr increment_bid   ; Increment bid counter
    lda player_bid_display
    sta player_bid_temp
    lda player_bid_display + 1
    sta player_bid_temp + 1
    jsr bid_counter    ; Redraw bid counter on the screen
    lda player_bid_temp
    sta player_bid_display
    lda player_bid_temp + 1
    sta player_bid_display + 1
    ldx #$00
    stx up_pressed
exit_increment_bid:
;-----------------------------------------------------------------------------

    ; Down button logic
    lda inputs
    and #BTN_DOWN
    beq down_unpressed  ; Skip if Down is not pressed
    jsr down_press           ; Handle Down button press
    lda down_pressed
    cmp #1
    beq skip_decrement_bid
down_unpressed:
    lda down_pressed
    cmp #1
    bne skip_decrement_bid
    jsr increment_bid       ; Update bid counter (decrement)
    lda player_bid_display
    sta player_bid_temp
    lda player_bid_display + 1
    sta player_bid_temp + 1
    jsr bid_counter      ; Redraw bid counter on screen
    lda player_bid_temp
    sta player_bid_display
    lda player_bid_temp + 1
    sta player_bid_display + 1
    ldx #$00
    stx down_pressed
skip_decrement_bid:

    pla                          ; Restore Y register
    tay
    pla                          ; Restore X register
    tax
    pla                          ; Restore accumulator
    rts                          ; Return from subroutine
;--------------------------------------------------------------------------------

;--------------------------------------------------------------------
  vblankwait5:
    BIT PPU_STATUS
    BPL vblankwait5

    LDA #%10010000  ; turn on NMIs, sprites use first pattern table
    STA PPU_CTRL
    LDA #%00011110  ; turn on screen
    STA PPU_MASK

  forever:
    jsr load_player
    JMP forever
;---------------------------------------------------------------------
  draw_card:
    pha                          ; Keep accumulator
    txa
    pha                          ; Keep X register
    tya
    pha                          ; Keep Y register

;---------------------------------------------- RNG ----------------------------------------------------
    LDA round_win
    CMP #1
    BEQ unflip
    LDA round_lost
    CMP #1
    BEQ unflip
    LDA round_tie
    CMP #1
    BEQ unflip
    jmp RandomDeck

    unflip:
      LDA $0300
      STA card_deck
      LDA $0301
      STA card_choice
      LDA card_choice
      jmp check1
    
    RandomDeck: ; Determines which deck the next card will be pulled out of (LSFR)
      LDA seed_high ; Load high byte of the seed
      EOR seed_low ; XOR with low byte
      AND #%00000001 ; Mask to get bit 0 (feedback bit)
      STA $0002 ; Store feedback bit temporarily

      LSR seed_high ; Logical shift right on high byte
      ROR seed_low  ; Rotate right through carry into low byte

      LDA seed_high ; Load high byte
      ORA $0002     ; Insert feedback bit
      STA seed_high ; Store updated high byte

      LDA seed_low  ; Return a pseudo-random number in low byte of the seed
      cmp #5 ; Makes sure we only get a number below 5
      bcs RandomDeck
      cmp #0 ; If we get 0, invalid output and restart RNG
      beq RandomDeck
      STA card_deck
      

    RandomCard: ; Determines which card to pull out from any of the decks (LSFR)
      ; This RNG Algorithm was from nesdev forum
      LDX counter ; Use frame counter for randomness
      STX seed+1 ; Store counter into high byte of seed

      LDY #8 ; Load Y with 8 to simulate 8 bits
      LDA seed+0 ; Load low byte of seed into accumulator
      :
      ASL ; Shif low byte of seed to the left
      ROL seed+1 ; Rotate over left high byte of seed
      BCC :+ ; If carry is clear, branch to ":" ahead
      EOR #$39 ; If not clear, XOR with hex 39
      :
      DEY ; Subtract 1 from Y, one less left shift to do
      BNE :-- ; If value in Y is not 0, go back to the first ":"
      CMP #14 ; Makes sure that the value we get is below 14
      BCS RandomCard
      cmp #0 ; If we get a 0, invalid output and restart RNG
      BEQ RandomCard
      STA seed 

      LDX #0
      ; Reviser and its sub-subroutines check if a card is already stored in memory, and stores them if they aren't
      ; Cards are supposed to be stored between $0300 - $0337, but pressing the buttons too fast can cause unnecesary stores at times
      reviser: 
        TXA
        CMP reviser_offset
        BEQ reviserDone
        LDA $0300,X 
        CMP card_deck
        BEQ reviserHalf
        INX
        INX
        jmp reviser
      
      reviserHalf:
        INX
        LDA $0300,X 
        CMP seed
        BEQ RandomDeck
        INX
        jmp reviser
      

      reviserDone:
        LDX reviser_offset
        LDA card_deck
        STA $0300,X 
        INX
        LDA seed
        STA $0300,X 
        INX
        STX reviser_offset

        ; Hardcoded cases that verify output of RNG and assigns the start index of a card
        LDA seed
        check1:
          CMP #$01
          BNE check2
          LDA #$00
          STA card_choice

        check2:
          CMP #2
          BNE check3
          LDA #$0c
          STA card_choice
          jmp vblankwait4

        check3:
          CMP #$03
          BNE check4
          LDA #$18
          STA card_choice

        check4:
          CMP #$04
          BNE check5
          LDA #$24
          STA card_choice

        check5:
          CMP #$05
          BNE check6
          LDA #$30
          STA card_choice

        check6:
          CMP #$06
          BNE check7
          LDA #$3c
          STA card_choice

        check7:
          CMP #$07
          BNE check8
          LDA #$48
          STA card_choice

        check8:
          CMP #$08
          BNE check9
          LDA #$54
          STA card_choice

        check9:
          CMP #$09
          BNE check10
          LDA #$60
          STA card_choice

        check10:
          CMP #$0a
          BNE check11
          LDA #$6c
          STA card_choice

        check11:
          CMP #$0b
          BNE check12
          LDA #$78
          STA card_choice

        check12:
          CMP #$0c
          BNE check13
          LDA #$84
          STA card_choice

        check13:
          CMP #$0d
          BNE vblankwait4
          LDA #$90
          STA card_choice
;---------------------------------------------- RNG ----------------------------------------------------

    vblankwait4:
      BIT PPU_STATUS
      BPL vblankwait4

      LDA dealer_flipped
      ; When 'A' is pressed in the 'before_game' state, the first card of the dealer must be flipped
      flippedgen: ;Data of card is stored in memory, so flipping it here does not affect its value
        CMP #1
        BEQ skipFlip
        LDX #1
        STX dealer_flipped
        jmp noskipFlip

        skipFlip:
          jmp noflip
        
        noskipFlip:
        LDA PPU_STATUS
        LDA card_pointer
        STA PPU_ADDR
        LDA card_pointer+1
        STA PPU_ADDR

        LDX #0
        LDY flippedCard, X        
        STY PPU_DATA
        INX                 ; X = X + 1
        LDY flippedCard, X        
        STY PPU_DATA
        INX                 ; X = X + 1
        LDY flippedCard, X        
        STY PPU_DATA
        INX                 ; X = X + 1

        LDA card_pointer+1
        CLC
        ADC #32             ; Add 32 to low byte
        STA card_pointer+1
        LDA card_pointer 
        STA PPU_ADDR
        LDA card_pointer+ 1
        STA PPU_ADDR

        LDY flippedCard, X        
        STY PPU_DATA
        INX                 ; X = X + 1
        LDY flippedCard, X        
        STY PPU_DATA
        INX                 ; X = X + 1
        LDY flippedCard, X        
        STY PPU_DATA
        INX                 ; X = X + 1

        LDA card_pointer+1
        CLC
        ADC #32             ; Add 32 to low byte
        STA card_pointer+1
        LDA card_pointer 
        STA PPU_ADDR
        LDA card_pointer+ 1
        STA PPU_ADDR

        LDY flippedCard, X        
        STY PPU_DATA
        INX                 ; X = X + 1
        LDY flippedCard, X        
        STY PPU_DATA
        INX                 ; X = X + 1
        LDY flippedCard, X        
        STY PPU_DATA
        INX                 ; X = X + 1

        LDA card_pointer+1
        CLC
        ADC #32             ; Add 32 to low byte
        STA card_pointer+1
        LDA card_pointer 
        STA PPU_ADDR
        LDA card_pointer+ 1
        STA PPU_ADDR

        LDY flippedCard, X        
        STY PPU_DATA
        INX                 ; X = X + 1
        LDY flippedCard, X        
        STY PPU_DATA
        INX                 ; X = X + 1
        LDY flippedCard, X        
        STY PPU_DATA
        INX                 ; X = X + 1

        lda card_pointer+1
        CLC
        adc #4
        sec 
        sbc #96
        sta card_pointer+1
        
        LDA card_deck ; Use numbers from RNG to determine what the last tile of the card would be
        checkH: ; Check if card would be in Hearts Deck
          CMP #1
          BNE checkD
          LDA card_choice ; Initial index of card in deck
          CLC
          ADC #11
          TAX
          LDY hearts,X ; Beginning index of card + 11 = Last Tile of Card
          STY last_tile_displayed
          jmp endCheck
        
        checkD: ; Check if card would be in Diamonds Deck
          CMP #3
          BNE checkC
          LDA card_choice ; Initial index of card in deck
          CLC
          ADC #11
          TAX
          LDY diamonds,X ; Beginning index of card + 11 = Last Tile of Card
          STY last_tile_displayed
          jmp endCheck
        
        checkC: ; Check if card would be in Clubs Deck
          CMP #2
          BNE checkS
          LDA card_choice ; Initial index of card in deck
          CLC
          ADC #11
          TAX
          LDY clubs,X ; Beginning index of card + 11 = Last Tile of Card
          STY last_tile_displayed
          jmp endCheck
        
        checkS: ; Check if card would be in Spades Deck
          CMP #4
          BNE endCheck
          LDA card_choice ; Initial index of card in deck
          CLC
          ADC #11
          TAX
          LDY spades,X ; Beginning index of card + 11 = Last Tile of Card
          STY last_tile_displayed
        
        endCheck:
        LDA #0
        LDX #0
        LDY #0
        jsr get_card_value
        jmp END
      
      noflip:
      ; Check if card_pointer has reached the end for dealer
      LDA #$3E
      STA card_pointer_reviser
      CMP card_pointer+1
      bne CHECK_END_PLAYER
      jmp END

      ; Check if card_pointer has reached the end for player
      CHECK_END_PLAYER:
      LDA #$1E
      STA card_pointer_reviser
      CMP card_pointer+1
      bne CONTINUE
      jmp END

;--------------------------------------------------------------------------
      CONTINUE:
      ; Check if card_pointer is going out of bounds ($209E for Dealer)
      LDA #$9E
      STA card_pointer_reviser
      CMP card_pointer+1
      beq CHANGEDEALER
      LDA #$7E
      STA card_pointer_reviser
      CMP card_pointer+1
      beq CHANGEPLAYER
      jmp KEEP_DRAWING

      CHANGEDEALER:
        LDA #$21
        STA card_pointer
        LDA #$22
        STA card_pointer+1
        jmp KEEP_DRAWING
      
      CHANGEPLAYER:
        LDA #$23
        STA card_pointer
        LDA #$02
        STA card_pointer+1

      KEEP_DRAWING:
      LDX card_choice            ; X will be our tile index (0 to 5)

;--------------------------------------------------------------------------
      ; Here we decide from which deck to draw cards from
      LDA card_deck

      deck1: ; Hearts Deck
        CMP #1
        BNE deck1skip
        jmp deck1continue

        deck1skip:
          jmp deck2
        deck1continue:
        LDA card_pointer
        STA PPU_ADDR
        LDA card_pointer+1
        STA PPU_ADDR

        ; Write first tile
        LDY hearts, X        
        STY PPU_DATA
        INX                 ; X = X + 1
        ; Write second tile
        LDY hearts, X
        STY PPU_DATA
        INX                 ; X = X + 1
        LDY hearts, X
        STY PPU_DATA
        INX  
        ; Move card_pointer to the next row
        ; Add 32 to low byte of card_pointer
        LDA card_pointer+1
        CLC
        ADC #32             ; Add 32 to low byte
        STA card_pointer+1
        LDA card_pointer 
        STA PPU_ADDR
        LDA card_pointer+ 1
        STA PPU_ADDR
        ; No carry handling

        ; Write fourth tile
        LDY hearts, X       
        STY PPU_DATA
        INX                 ; X = X + 1
        ; Write fifth tile
        LDY hearts, X
        STY PPU_DATA
        INX                ; X = X + 1
        LDY hearts, X
        STY PPU_DATA
        INX         
        ; Move card_pointer to the next row
        ; Add 32 to low byte of card_pointer
        LDA card_pointer+1
        CLC
        ADC #32             ; Add 32 to low byte
        STA card_pointer+1
        LDA card_pointer 
        STA PPU_ADDR
        LDA card_pointer+ 1
        STA PPU_ADDR
        ; No carry handling

        ; Write seventh tile
        LDY hearts, X        ; Load tile from cards array
        STY PPU_DATA
        INX                 ; X = X + 1
        ; Write eigth tile
        LDY hearts, X
        STY PPU_DATA
        INX                 ; X = X + 1
        LDY hearts, X
        STY PPU_DATA
        INX         
        ; Move card_pointer to the next row
        ; Add 32 to low byte of card_pointer
        LDA card_pointer+1
        CLC
        ADC #32             ; Add 32 to low byte
        STA card_pointer+1
        ; No carry handling
        LDA card_pointer 
        STA PPU_ADDR
        LDA card_pointer+ 1
        STA PPU_ADDR

        ; Write tenth tile
        LDY hearts, X        ; Load tile from cards array
        STY PPU_DATA
        INX                 ; X = X + 1
        ; Write eleventh tile
        LDY hearts, X
        STY PPU_DATA
        INX                 ; X = X + 1
        LDY hearts, X
        STY PPU_DATA
        INX         
        stx card_offset
        sty last_tile_displayed
        lda card_pointer+1
        CLC
        adc #4
        sec 
        sbc #96
        sta card_pointer+1
        jsr get_card_value
        jmp END
      
      deck2: ; Diamonds Deck
        CMP #3
        BNE deck2skip
        jmp deck2continue

        deck2skip:
          jmp deck3
        deck2continue:
        LDA card_pointer
        STA PPU_ADDR
        LDA card_pointer+1
        STA PPU_ADDR

        ; Write first tile
        LDY diamonds, X        
        STY PPU_DATA
        INX                 ; X = X + 1
        ; Write second tile
        LDY diamonds, X
        STY PPU_DATA
        INX                 ; X = X + 1
        LDY diamonds, X
        STY PPU_DATA
        INX  
        ; Move card_pointer to the next row
        ; Add 32 to low byte of card_pointer
        LDA card_pointer+1
        CLC
        ADC #32             ; Add 32 to low byte
        STA card_pointer+1
        LDA card_pointer 
        STA PPU_ADDR
        LDA card_pointer+ 1
        STA PPU_ADDR
        ; No carry handling

        ; Write fourth tile
        LDY diamonds, X       
        STY PPU_DATA
        INX                 ; X = X + 1
        ; Write fifth tile
        LDY diamonds, X
        STY PPU_DATA
        INX                ; X = X + 1
        LDY diamonds, X
        STY PPU_DATA
        INX         
        ; Move card_pointer to the next row
        ; Add 32 to low byte of card_pointer
        LDA card_pointer+1
        CLC
        ADC #32             ; Add 32 to low byte
        STA card_pointer+1
        LDA card_pointer 
        STA PPU_ADDR
        LDA card_pointer+ 1
        STA PPU_ADDR
        ; No carry handling

        ; Write seventh tile
        LDY diamonds, X        ; Load tile from cards array
        STY PPU_DATA
        INX                 ; X = X + 1
        ; Write eigth tile
        LDY diamonds, X
        STY PPU_DATA
        INX                 ; X = X + 1
        LDY diamonds, X
        STY PPU_DATA
        INX         
        ; Move card_pointer to the next row
        ; Add 32 to low byte of card_pointer
        LDA card_pointer+1
        CLC
        ADC #32             ; Add 32 to low byte
        STA card_pointer+1
        ; No carry handling
        LDA card_pointer 
        STA PPU_ADDR
        LDA card_pointer+ 1
        STA PPU_ADDR

        ; Write tenth tile
        LDY diamonds, X        ; Load tile from cards array
        STY PPU_DATA
        INX                 ; X = X + 1
        ; Write eleventh tile
        LDY diamonds, X
        STY PPU_DATA
        INX                 ; X = X + 1
        LDY diamonds, X
        STY PPU_DATA
        INX         
        stx card_offset
        sty last_tile_displayed
        lda card_pointer+1
        CLC
        adc #4
        sec 
        sbc #96
        sta card_pointer+1
        jsr get_card_value
        jmp END
      
      deck3:
        CMP #2
        BNE deck3skip
        jmp deck3continue

        deck3skip:
          jmp deck4
        deck3continue:
        LDA card_pointer
        STA PPU_ADDR
        LDA card_pointer+1
        STA PPU_ADDR

        ; Write first tile
        LDY clubs, X        
        STY PPU_DATA
        INX                 ; X = X + 1
        ; Write second tile
        LDY clubs, X
        STY PPU_DATA
        INX                 ; X = X + 1
        LDY clubs, X
        STY PPU_DATA
        INX  
        ; Move card_pointer to the next row
        ; Add 32 to low byte of card_pointer
        LDA card_pointer+1
        CLC
        ADC #32             ; Add 32 to low byte
        STA card_pointer+1
        LDA card_pointer 
        STA PPU_ADDR
        LDA card_pointer+ 1
        STA PPU_ADDR
        ; No carry handling

        ; Write fourth tile
        LDY clubs, X       
        STY PPU_DATA
        INX                 ; X = X + 1
        ; Write fifth tile
        LDY clubs, X
        STY PPU_DATA
        INX                ; X = X + 1
        LDY clubs, X
        STY PPU_DATA
        INX         
        ; Move card_pointer to the next row
        ; Add 32 to low byte of card_pointer
        LDA card_pointer+1
        CLC
        ADC #32             ; Add 32 to low byte
        STA card_pointer+1
        LDA card_pointer 
        STA PPU_ADDR
        LDA card_pointer+ 1
        STA PPU_ADDR
        ; No carry handling

        ; Write seventh tile
        LDY clubs, X        ; Load tile from cards array
        STY PPU_DATA
        INX                 ; X = X + 1
        ; Write eigth tile
        LDY clubs, X
        STY PPU_DATA
        INX                 ; X = X + 1
        LDY clubs, X
        STY PPU_DATA
        INX         
        ; Move card_pointer to the next row
        ; Add 32 to low byte of card_pointer
        LDA card_pointer+1
        CLC
        ADC #32             ; Add 32 to low byte
        STA card_pointer+1
        ; No carry handling
        LDA card_pointer 
        STA PPU_ADDR
        LDA card_pointer+ 1
        STA PPU_ADDR

        ; Write tenth tile
        LDY clubs, X        ; Load tile from cards array
        STY PPU_DATA
        INX                 ; X = X + 1
        ; Write eleventh tile
        LDY clubs, X
        STY PPU_DATA
        INX                 ; X = X + 1
        LDY clubs, X
        STY PPU_DATA
        INX         
        stx card_offset
        sty last_tile_displayed
        lda card_pointer+1
        CLC
        adc #4
        sec 
        sbc #96
        sta card_pointer+1
        jsr get_card_value
        jmp END

      deck4:
        CMP #4
        BNE deck4skip
        jmp deck4continue

        deck4skip:
          jmp END
        deck4continue:
        LDA card_pointer
        STA PPU_ADDR
        LDA card_pointer+1
        STA PPU_ADDR

        ; Write first tile
        LDY spades, X        
        STY PPU_DATA
        INX                 ; X = X + 1
        ; Write second tile
        LDY spades, X
        STY PPU_DATA
        INX                 ; X = X + 1
        LDY spades, X
        STY PPU_DATA
        INX  
        ; Move card_pointer to the next row
        ; Add 32 to low byte of card_pointer
        LDA card_pointer+1
        CLC
        ADC #32             ; Add 32 to low byte
        STA card_pointer+1
        LDA card_pointer 
        STA PPU_ADDR
        LDA card_pointer+ 1
        STA PPU_ADDR
        ; No carry handling

        ; Write fourth tile
        LDY spades, X       
        STY PPU_DATA
        INX                 ; X = X + 1
        ; Write fifth tile
        LDY spades, X
        STY PPU_DATA
        INX                ; X = X + 1
        LDY spades, X
        STY PPU_DATA
        INX         
        ; Move card_pointer to the next row
        ; Add 32 to low byte of card_pointer
        LDA card_pointer+1
        CLC
        ADC #32             ; Add 32 to low byte
        STA card_pointer+1
        LDA card_pointer 
        STA PPU_ADDR
        LDA card_pointer+ 1
        STA PPU_ADDR
        ; No carry handling

        ; Write seventh tile
        LDY spades, X        ; Load tile from cards array
        STY PPU_DATA
        INX                 ; X = X + 1
        ; Write eigth tile
        LDY spades, X
        STY PPU_DATA
        INX                 ; X = X + 1
        LDY spades, X
        STY PPU_DATA
        INX         
        ; Move card_pointer to the next row
        ; Add 32 to low byte of card_pointer
        LDA card_pointer+1
        CLC
        ADC #32             ; Add 32 to low byte
        STA card_pointer+1
        ; No carry handling
        LDA card_pointer 
        STA PPU_ADDR
        LDA card_pointer+ 1
        STA PPU_ADDR

        ; Write tenth tile
        LDY spades, X        ; Load tile from cards array
        STY PPU_DATA
        INX                 ; X = X + 1
        ; Write eleventh tile
        LDY spades, X
        STY PPU_DATA
        INX                 ; X = X + 1
        LDY spades, X
        STY PPU_DATA
        INX         
        stx card_offset
        sty last_tile_displayed
        lda card_pointer+1
        CLC
        adc #4
        sec 
        sbc #96
        sta card_pointer+1
        jsr get_card_value
        jmp END
    
    get_card_value:
      ; Load the last byte drawn (card value)
      LDA last_tile_displayed
      clc                          ; Clear carry flag

      ; Compare with red card values
      get1:
      CMP #$48
      BEQ card_is_A
      CMP #$56
      BEQ card_is_A
      jmp get2
      card_is_A:
        jmp card_is_Ax
      get2:
      CMP #$4A
      BEQ card_is_2
      CMP #$58
      BEQ card_is_2
      jmp get3
      card_is_2:
        jmp card_is_2x
      get3:
      CMP #$4B
      BEQ card_is_3
      CMP #$59
      BEQ card_is_3
      jmp get4
      card_is_3:
        jmp card_is_3x
      get4:
      CMP #$4C
      BEQ card_is_4
      CMP #$5A
      BEQ card_is_4
      jmp get5
      card_is_4:
        jmp card_is_4x
      get5:
      CMP #$4D
      BEQ card_is_5
      CMP #$5B
      BEQ card_is_5
      jmp get6
      card_is_5:
        jmp card_is_5x
      get6:
      CMP #$4E
      BEQ card_is_6
      CMP #$5C
      BEQ card_is_6
      jmp get7
      card_is_6:
        jmp card_is_6x
      get7:
      CMP #$4F
      BEQ card_is_7
      CMP #$5D
      BEQ card_is_7
      jmp get8
      card_is_7:
        jmp card_is_7x
      get8:
      CMP #$50
      BEQ card_is_8
      CMP #$5E
      BEQ card_is_8
      jmp get9
      card_is_8:
        jmp card_is_8x
      get9:
      CMP #$51
      BEQ card_is_9
      CMP #$5F
      BEQ card_is_9
      jmp get10
      card_is_9:
        jmp card_is_9x
      get10:
      CMP #$52
      BEQ card_is_10
      CMP #$60
      BEQ card_is_10
      jmp get11
      card_is_10:
        jmp card_is_10x
      get11:
      CMP #$53
      BEQ card_is_10
      CMP #$61
      BEQ card_is_10
      jmp get12
      get12:
      CMP #$54
      BEQ card_is_10
      CMP #$62
      BEQ card_is_10
      jmp get13
      get13:
      CMP #$55
      BEQ card_is_10
      CMP #$63
      BEQ card_is_10
      ; If no match, return
      RTS
    
    card_is_Ax: 
      LDA current_player
      CMP #1
      BNE add_to_dealer ; If not looking at player, look at dealer
      LDA players_points
      CLC
      ADC #11
      CMP #22
      BCS cannot11 ; If adding 11 to the points passes 21, make the ace worth one
      STA players_points
      LDA ace_counter_player
      CLC
      ADC #1
      STA ace_counter_player
      jmp end_get_card_value

      cannot11:
        LDA players_points
        CLC
        ADC #1
        STA players_points
        jmp end_get_card_value
      
      add_to_dealer:
        LDA dealers_points
        CLC
        ADC #11
        CMP #22
        BCS cannot11dealer
        STA dealers_points
        LDA ace_counter_dealer
        CLC
        ADC #1
        STA ace_counter_dealer
        jmp end_get_card_value

        cannot11dealer:
          LDA dealers_points
          CLC
          ADC #1
          STA dealers_points
          jmp end_get_card_value

    card_is_2x:
        LDA #2
        sta current_points
        Jsr add_to_points
        jmp end_get_card_value

    card_is_3x:
        LDA #3
        sta current_points
        Jsr add_to_points
        jmp end_get_card_value

    card_is_4x:
        LDA #4
        sta current_points
        Jsr add_to_points
        jmp end_get_card_value

    card_is_5x:
        LDA #5
        sta current_points
        Jsr add_to_points
        jmp end_get_card_value

    card_is_6x:
        LDA #6
        sta current_points
        Jsr add_to_points
        jmp end_get_card_value

    card_is_7x:
        LDA #7
        sta current_points
        Jsr add_to_points
        jmp end_get_card_value

    card_is_8x:
        LDA #8
        sta current_points
        Jsr add_to_points
        jmp end_get_card_value

    card_is_9x:
        LDA #9
        sta current_points 
        jsr add_to_points
        jmp end_get_card_value

    card_is_10x:
        LDA #$0A
        sta current_points
        jsr add_to_points
        jmp end_get_card_value

    end_get_card_value:
      rts
    
    add_to_points:
      lda current_player ; Check if the current player is the player or the dealer
      cmp #1
      beq add_to_player

      ; Dealer's points
      lda current_points
      clc
      adc dealers_points
      ; If it goes over 21, see if there are aces for player that can change to 1
      CMP #22
      BCS checkAcesP
      sta dealers_points
      lda #0
      sta current_points
      rts

      checkAcesP:
        lda ace_counter_dealer
        CMP #1
        BCS yesAcesP ; If 1 or more aces in player hand, reduce at least one of them to 1
        lda current_points
        clc 
        adc dealers_points
        sta dealers_points
        lda #0
        sta current_points
        rts

        yesAcesP:
          lda current_points
          clc
          adc dealers_points
          sec
          sbc #10
          sta dealers_points
          lda #0
          sta current_points

    add_to_player:
        ; Player's points
      lda current_points
      clc
      adc players_points
      ; If it goes over 21, see if there are aces for player that can change to 1
      CMP #22
      BCS checkAcesP1
      sta players_points
      lda #0
      sta current_points
      rts

      checkAcesP1:
        lda ace_counter_player
        CMP #1
        BCS yesAcesP1 ; If 1 or more aces in player hand, reduce at least one of them to 1
        lda current_points
        clc 
        adc players_points
        sta players_points
        lda #0
        sta current_points
        rts

        yesAcesP1:
          lda current_points
          clc
          adc players_points
          sec
          sbc #10
          sta players_points
          lda #0
          sta current_points
    
    ; subroutine to natural blackjack
    natural_blackjack:

      check_dealer:
      lda dealers_points
      cmp #21
      bne check_player
      lda #1
      sta natural_blackjack_dealer

      check_player:
      lda players_points
      cmp #21
      bne check_dealer_win
      lda #1
      sta natural_blackjack_player

      check_tie:
      lda natural_blackjack_dealer
      cmp natural_blackjack_player
      bne check_dealer_win
      lda #1
      sta tie_flag
      jmp end_natural_blackjack

      check_dealer_win:
      lda natural_blackjack_dealer
      cmp #1
      bne check_player_win
      lda #1
      sta dealer_wins_flag
      jmp end_natural_blackjack

      check_player_win:
      lda natural_blackjack_player
      cmp #1
      bne end_natural_blackjack
      lda #1
      sta player_wins_flag

      end_natural_blackjack:
        rts
    END:
      ; Re-enable NMI and rendering after drawing
      lda #%00001110
      sta PPU_MASK        ; Re-enable background and sprites              
      pla                           ; Restore Y register
      tay
      pla                           ; Restore X register
      tax
      pla                           ; Restore accumulator
      rts                           ; Return from subroutine
;------------------------------------------------------------------------
display:
  LDX #0

  LDA out_of_money
  CMP #1
  BEQ noMoneyX

  LDA full_of_money
  CMP #1
  BEQ fullMoneyX

  LDA round_tie
  CMP #1
  BEQ tieDisplay

  LDA round_win
  CMP #1
  BEQ winDisplayx

  LDA round_lost
  CMP #1
  BEQ loseDisplay
  rts

  winDisplayx:
    jmp winDisplay
  
  noMoneyX:
    jmp noMoneyDisplay

  fullMoneyX:
    jmp fullMoneyDisplay

  tieDisplay:
    LDA PPU_STATUS
    LDA result_text
    STA PPU_ADDR
    LDA result_text+1
    STA PPU_ADDR

    ;Write the letter T
    LDY Tie, X
    STY PPU_DATA
    INX

    ;Write the letter I
    LDY Tie, X
    STY PPU_DATA
    INX

    ;Write the letter E
    LDY Tie, X
    STY PPU_DATA
    INX
    rts
  
  loseDisplay:
    ; Display bytes containing the word "PlayerLost" in the top
    ; This is for testing purposes
    LDA result_text
    STA PPU_ADDR
    lda result_text+1
    STA PPU_ADDR

    ;Write the letter P
    LDY PlayerLost, X
    STY PPU_DATA
    INX

    ;Write the letter L
    LDY PlayerLost, X
    STY PPU_DATA
    INX

    ;Write the letter A
    LDY PlayerLost, X
    STY PPU_DATA
    INX

    ;Write the letter Y
    LDY PlayerLost, X
    STY PPU_DATA
    INX

    ;Write the letter E
    LDY PlayerLost, X
    STY PPU_DATA
    INX

    ;Write the letter R
    LDY PlayerLost, X
    STY PPU_DATA
    INX

    ;Write space
    LDY PlayerLost, X
    STY PPU_DATA
    INX

    ;Write the letter L
    LDY PlayerLost, X
    STY PPU_DATA
    INX

    ;Write the letter O
    LDY PlayerLost, X
    STY PPU_DATA
    INX

    ;Write the letter S
    LDY PlayerLost, X
    STY PPU_DATA
    INX

    ;Write the letter T
    LDY PlayerLost, X
    STY PPU_DATA
    inx
    rts

  winDisplay:
    LDA PPU_STATUS
    LDA result_text
    STA PPU_ADDR
    lda result_text+1
    STA PPU_ADDR

    ;Write the letter P
    LDY PlayerWins, X
    STY PPU_DATA
    INX

    ;Write the letter L
    LDY PlayerWins, X
    STY PPU_DATA
    INX

    ;Write the letter A
    LDY PlayerWins, X
    STY PPU_DATA
    INX

    ;Write the letter Y
    LDY PlayerWins, X
    STY PPU_DATA
    INX

    ;Write the letter E
    LDY PlayerWins, X
    STY PPU_DATA
    INX

    ;Write the letter R
    LDY PlayerWins, X
    STY PPU_DATA
    INX

    ;Write space
    LDY PlayerWins, X
    STY PPU_DATA
    INX

    ;Write the letter W
    LDY PlayerWins, X
    STY PPU_DATA
    INX

    ;Write the letter I
    LDY PlayerWins, X
    STY PPU_DATA
    INX

    ;Write the letter N
    LDY PlayerWins, X 
    STY PPU_DATA
    INX

    ;Write the letter S
    LDY PlayerWins, X
    STY PPU_DATA
    INX
    rts
  
  noMoneyDisplay:
    LDA PPU_STATUS
    LDA result_text
    STA PPU_ADDR
    lda result_text+1
    STA PPU_ADDR

    ;Write the letter P
    LDY LosesGame, X
    STY PPU_DATA
    INX

    ;Write the letter L
    LDY LosesGame, X
    STY PPU_DATA
    INX

    ;Write the letter A
    LDY LosesGame, X
    STY PPU_DATA
    INX

    ;Write the letter Y
    LDY LosesGame, X
    STY PPU_DATA
    INX

    ;Write the letter E
    LDY LosesGame, X
    STY PPU_DATA
    INX

    ;Write the letter R
    LDY LosesGame, X
    STY PPU_DATA
    INX

    ;Write space
    LDY LosesGame, X
    STY PPU_DATA
    INX

    ;Write the letter W
    LDY LosesGame, X
    STY PPU_DATA
    INX

    ;Write the letter I
    LDY LosesGame, X
    STY PPU_DATA
    INX
    rts
  
  fullMoneyDisplay:
    LDA PPU_STATUS
    LDA result_text
    STA PPU_ADDR
    lda result_text+1
    STA PPU_ADDR

    ;Write the letter P
    LDY WinsGame, X
    STY PPU_DATA
    INX

    ;Write the letter L
    LDY WinsGame, X
    STY PPU_DATA
    INX

    ;Write the letter A
    LDY WinsGame, X
    STY PPU_DATA
    INX

    ;Write the letter Y
    LDY WinsGame, X
    STY PPU_DATA
    INX

    ;Write the letter E
    LDY WinsGame, X
    STY PPU_DATA
    INX

    ;Write the letter R
    LDY WinsGame, X
    STY PPU_DATA
    INX

    ;Write space
    LDY WinsGame, X
    STY PPU_DATA
    INX

    ;Write the letter W
    LDY WinsGame, X
    STY PPU_DATA
    INX

    ;Write the letter I
    LDY WinsGame, X
    STY PPU_DATA
    INX

    ;Write the letter N
    LDY WinsGame, X 
    STY PPU_DATA
    INX

    ;Write the letter S
    LDY WinsGame, X
    STY PPU_DATA
    INX

    ;Write the letter S
    LDY WinsGame, X
    STY PPU_DATA
    INX

    ;Write the letter S
    LDY WinsGame, X
    STY PPU_DATA
    INX

    ;Write the letter S
    LDY WinsGame, X
    STY PPU_DATA
    INX


    LDA PPU_STATUS
    LDA #$22
    STA PPU_ADDR
    LDA #$0b
    STA PPU_ADDR

    LDX #0
    getRidofCashCounter:
      LDY $32
      STY PPU_DATA
      INX
      CPX #9
      BNE getRidofCashCounter

    rts

;------------------------------------------------------------------------------
check_ace:
    ; Save registers
    PHA
    TXA
    PHA
    TYA
    PHA
    ; Check if the player has an ace
    ;check if the dealer has an ace
    ; Restore registers and return
    PLA
    TAY
    PLA
    TAX
    PLA
    RTS

start_press:
  PHA
  TXA
  PHA
  TYA
  PHA
    lda start_pressed
    cmp #1
    beq pressed_start     ; Skip if its already pressed
    lda #$01
    sta start_pressed         ; Set start button pressed flag
    
pressed_start:
  PLA
  TAY
  PLA
  TAX
  PLA
    rts
;---------------------------------------------------------------------

A_press:
  PHA
  TXA
  PHA
  TYA
  PHA
    lda A_pressed
    cmp #1
    beq pressed_A         ; Skip if its already pressed
    lda #$01
    sta A_pressed             ; Set A button pressed flag
pressed_A:
  PLA
  TAY
  PLA
  TAX
  PLA
    rts
;-----------------------------------------------------------------------

B_press:
  PHA
  TXA
  PHA
  TYA
  PHA
    lda B_pressed
    cmp #1
    beq pressed_B         ; Skip if its already pressed
    lda #$01
    sta B_pressed             ; Set B button pressed flag
pressed_B:
  PLA
  TAY
  PLA
  TAX
  PLA
    rts
;--------------------------------------------------------------

down_press:
  PHA
  TXA
  PHA
  TYA
  PHA
    lda down_pressed
    cmp #1
    beq already_pressed_down      ; Skip if already pressed
    lda #$01
    sta down_pressed          ; Set Down button pressed flag
already_pressed_down:
  PLA
  TAY
  PLA
  TAX
  PLA
    rts
;----------------------------------------------------------------------    

up_press:
  PHA
  TXA
  PHA
  TYA
  PHA
    lda up_pressed
    cmp #1
    beq already_pressed_up        ; Skip if already pressed
    lda #$01
    sta up_pressed           ; Set Up button pressed flag
already_pressed_up:
  PLA
  TAY
  PLA
  TAX
  PLA
    rts
;------------------------------------------------------------------------

reset_game:
    PHA
    TXA
    PHA
    TYA
    PHA
  ; Clear any existing VBlank flag by reading $2002
    BIT PPU_STATUS

    ; Wait for VBlank to start
    @WaitForStart:
        BIT PPU_STATUS            ; Read PPU_STATUS
        BPL @WaitForStart    ; If bit 7 is clear (not in VBlank), keep looping
    jsr backgroundloading         ; Reload background graphics

    LDX #0
    cardmemoryclean: ; Clears the memory spots where cards are recorded
      LDA #0
      STA $0300,X 
      CPX #$50
      INX
      BNE cardmemoryclean
    LDX #0
    STX reviser_offset

    ldx #$00
    stx player_bid              ; Reset bid counter to 5
    ldx #$05
    stx player_bid + 1

    ;For Stalling Card Generation
    LDA counter
    CLC
    ADC #$ff
    ADC #$2f
    STA counter

    ;reset result text position
    lda #$20
    sta result_text
    lda #$2c
    sta result_text+1

    ; Reset card_pointer_dealer to initial values (#$20/$82)
    LDA #$20
    STA card_pointer_dealer
    LDA #$82
    STA card_pointer_dealer + 1

    ; Reset card_pointer_player to initial values (#$22/$62)
    LDA #$22
    STA card_pointer_player
    LDA #$62
    STA card_pointer_player + 1

    ldx #00
    stx card_offset_dealer
    stx card_offset_player

    ldx #$22
    stx player_bid_display        ; Reset bid display position
    ldx #$1b
    stx player_bid_display + 1

    ldx #$22
    stx player_cash_display
    ldx #$11
    stx player_cash_display+1

    lda #0
    sta card_offset
    sta current_player
    ; Reset Ace Counters
    sta ace_counter_dealer
    sta ace_counter_player

    ; Update temporary bid display pointers
    lda player_bid_display
    sta player_bid_temp
    lda player_bid_display+ 1
    sta player_bid_temp + 1

    jsr bid_counter        ; Redraw the bid counter

    ; Restore bid display pointers
    lda player_bid_temp
    sta player_bid_display
    lda player_bid_temp + 1
    sta player_bid_display + 1
  
    ; Update temporary cash display pointers
    LDA #0
    STA OFV

    LDA round_win
    CMP #1
    BEQ skipCashReset
    LDA round_lost
    CMP #1
    BEQ skipCashReset
    ldx #0
    stx player_cash
    ldx #2
    stx player_cash+1
    ldx #0
    stx player_cash+2

    skipCashReset:
    lda player_cash_display
    sta player_cash_temp
    lda player_cash_display+ 1
    sta player_cash_temp + 1

    jsr cash_counter        ; Redraw the cash counter

    ; Restore cash display pointers
    lda player_cash_temp
    sta player_cash_display
    lda player_cash_temp + 1
    sta player_cash_display + 1

    ; Clear all input flags
    ldx #$00
    stx A_pressed
    stx B_pressed
    stx up_pressed
    stx down_pressed
    stx start_pressed
  
    ;reset points and current_player flag
    lda #0
    sta players_points
    sta dealers_points
    sta current_points
    sta current_player

    ; Reset flags
    lda #0
    sta natural_blackjack_dealer
    sta natural_blackjack_player
    sta tie_flag
    sta player_wins_flag
    sta dealer_wins_flag

    LDX #1
    STX before_game
    STX counter_on
    LDX #0
    STX middle_game
    STX round_lost
    STX round_win
    STX round_tie
    STX out_of_money
    STX full_of_money
    STX dealer_flipped
  PLA
  TAY
  PLA
  TAX
  PLA
    rts
;-----------------------------------------------------------------------
  bid_counter:
    pha                          ; Keep accumulator
    txa
    pha                          ; Keep X register
    tya
    pha                          ; Keep Y register

  vblank_wait3:
    bit PPU_STATUS
    bpl vblank_wait3              ; Wait for VBlank

    ; Disable rendering during bid counter update
    lda PPU_MASK
    and #%11100000
    sta PPU_MASK

    lda player_bid_temp
    sta PPU_ADDR                  ; Set PPU address for bid display
    lda player_bid_temp + 1
    sta PPU_ADDR

    ldx player_bid
    ldy Int, x
    sty PPU_DATA                  ; Draw tens digit of bid

    ldx player_bid + 1
    ldy Int, x
    sty PPU_DATA                  ; Draw ones digit of bid

   ; Re-enable rendering after drawing
    lda #%00001110                 ; Enable background and sprites
    sta PPU_MASK
    pla                           ; Restore Y register
    tay
    pla                           ; Restore X register
    tax
    pla 
    rts
;---------------------------------------------------------------------------
increment_bid:
    pha                          ; Keep accumulator
    txa
    pha                          ; Keep X register
    tya
    pha                          ; Keep Y register

    lda up_pressed
    cmp #1
    bne decrement_bid            ; If Up is not pressed, proceed to decrement

    ; Do not allow bid to be higher than cash reserve (Do Later)
  
    LDA player_bid ; We do not let the bid go above $90 for now
    CMP #9
    BEQ end_bid

    LDA player_cash
    CMP #0
    BNE continue_bid
    LDA player_cash+1
    CMP player_bid
    BNE continue_bid
    LDA player_cash+2
    CMP player_bid+1
    BNE continue_bid
    jmp end_bid

    ; Increment bid counter
    continue_bid:
    lda player_bid + 1
    clc
    adc #5
    cmp #5
    beq no_overflow_increment
    lda #$00
    sta player_bid + 1
    lda player_bid
    clc
    adc #1
    sta player_bid
    jmp end_bid
no_overflow_increment:
    sta player_bid + 1
    jmp end_bid

;--------------------------------------------------------------------------
decrement_bid:
    ; Decrement bid counter
    LDA player_bid
    CMP #0
    BEQ both0
    jmp skip0

    both0:
      LDA player_bid+1
      CMP #0
      BEQ end_bid

    skip0:
    lda player_bid + 1
    sec
    sbc #5
    cmp #0
    beq no_overflow_decrement
    lda #5
    sta player_bid + 1
    lda player_bid
    sec
    sbc #1
    sta player_bid
    jmp end_bid
no_overflow_decrement:
    sta player_bid + 1
end_bid:
    pla                           ; Restore Y register
    tay
    pla                           ; Restore X register
    tax
    pla 
    rts
;-------------------------------------------------------------------------------
  cash_counter:
    pha                          ; Keep accumulator
    txa
    pha                          ; Keep X register
    tya
    pha                          ; Keep Y register

  vblank_wait4:
    bit PPU_STATUS
    bpl vblank_wait4              ; Wait for VBlank

    ; Disable rendering during cash counter update
    lda PPU_MASK
    and #%11100000
    sta PPU_MASK

    lda player_cash_temp
    sta PPU_ADDR                  ; Set PPU address for bid display
    lda player_cash_temp+1
    sta PPU_ADDR

    ldx player_cash
    ldy Int, x
    sty PPU_DATA                  ; Draw tens digit of bid

    ldx player_cash + 1
    ldy Int, x
    sty PPU_DATA                  ; Draw ones digit of bid

    LDX player_cash + 2
    LDY Int, x
    STY PPU_DATA 

   ; Re-enable rendering after drawing
    lda #%00001110                 ; Enable background and sprites
    sta PPU_MASK
    pla                           ; Restore Y register
    tay
    pla                           ; Restore X register
    tax
    pla 
    rts
;-------------------------------------------------------------------------------
  increase_cash:
    ; Revise first byte
    firstbyte:
      LDA player_cash+2
      CLC
      ADC player_bid+1
      CMP #9
      BEQ no_overflow_increase1
      BCC no_overflow_increase1
      BCS overflow_increase1

      no_overflow_increase1:
        STA player_cash+2
        LDA #0
        STA OFV
        jmp secondbyte

      overflow_increase1:
        LDA player_cash+2
        CMP #0
        BNE atleast1nonzero1
        LDA player_bid+1
        CMP #0
        BNE atleast1nonzero1
        LDA #0
        STA player_cash+2
        STA OFV
        jmp secondbyte

        atleast1nonzero1:
          LDA player_cash+2
          CLC
          ADC player_bid+1
          ADC OFV
          LDX #10
          STX OFV
          SEC
          SBC OFV
          STA player_cash+2

          LDY #1
          STY OFV
          jmp secondbyte
    
    ;Revise second byte
    secondbyte:
      LDA player_cash+1
      CLC
      ADC player_bid
      ADC OFV
      CMP #9
      BEQ no_overflow_increase2
      BCC no_overflow_increase2
      BCS overflow_increase2

      no_overflow_increase2:
        STA player_cash+1
        LDA #0
        STA OFV
        jmp thirdbyte

      overflow_increase2:
        LDA player_cash+1
        CMP #0
        BNE atleast1nonzero2
        LDA player_bid
        CMP #0
        BNE atleast1nonzero2
        LDA #0
        STA player_cash+1
        jmp thirdbyte

        atleast1nonzero2:
          LDA player_cash+1
          CLC
          ADC player_bid
          ADC OFV
          LDX #10
          STX OFV
          SEC
          SBC OFV
          STA player_cash+1

          LDY #1
          STY OFV
          jmp thirdbyte

    ; Revise Third Byte
    thirdbyte:
      LDA player_cash+0
      CLC
      ADC OFV
      CMP #$0a
      BEQ setGameWon
      STA player_cash+0
      LDA #1
      STA round_win
      jmp backtoS
    
    setGameWon:
      LDA #0
      STA round_lost
      LDA #0
      STA round_win
      LDA #1
      STA full_of_money
    backtoS:
      rts
;-------------------------------------------------------------------------------
  decrease_cash:
    ; Revise first byte
    onetwothree:
      LDA player_cash+2
      CMP player_bid+1
      BEQ equalZero
      jmp cashHigher

      equalZero:
        LDA #0
        STA player_cash+2
        jmp fourfivesix
      
      cashHigher:
        LDA player_cash+2
        SEC
        SBC player_bid+1
        CMP #$f6
        BCS overflow_decrease1
        STA player_cash+2
        jmp fourfivesix

        overflow_decrease1:
          LDA player_cash+2
          CLC
          ADC #10
          SEC
          SBC player_bid+1
          STA player_cash+2
          LDY #1
          STY OFV
          jmp fourfivesix

    ; Revise Second Byte
    fourfivesix:
      LDA player_cash+1
      CMP player_bid
      BEQ equalZero1
      jmp cashHigher1

      equalZero1:
        LDA OFV
        CMP #1
        BCS overflow_high
        LDA #0
        STA player_cash+1
        jmp seveneightnine

        overflow_high:
          LDA player_cash+1
          CLC
          ADC #10
          SEC
          SBC OFV
          STA player_cash+1
          jmp seveneightnine
      
      cashHigher1:
        LDA player_cash+1
        SEC
        SBC player_bid
        CMP #$f6
        BCS overflow_decrease2
        SEC
        SBC OFV
        STA player_cash+1
        LDA #0
        STA OFV
        jmp seveneightnine

        overflow_decrease2:
          LDA player_cash+1
          CLC
          ADC #10
          SEC
          SBC player_bid
          SBC OFV
          STA player_cash+1
          LDY #1
          STY OFV
          jmp seveneightnine

    seveneightnine:
      LDA player_cash+0
      CMP #0
      BEQ setGameLoss
      SEC
      SBC OFV
      STA player_cash

    setGameLoss:
      LDA player_cash
      CMP #0
      BNE backtoS2
      LDA player_cash+1
      CMP #0
      BNE backtoS2
      LDA player_cash+2
      CMP #0
      BNE backtoS2

      LDA #0
      STA round_lost
      LDA #0
      STA round_win
      LDA #1
      STA out_of_money

    backtoS2:
      rts


Int:
     .byte $1E, $1F, $20, $21, $22, $23, $24, $25, $26, $27  ; Tile numbers for digits 0-9

	testBG:
	.byte $00,$00,$00,$73,$74,$75,$76,$77,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f
	.byte $7f,$7f,$7c,$7d,$7e,$7f,$7d,$7e,$7f,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$07,$08,$04,$0f,$08,$15,$00,$00,$00,$00,$00,$00,$7d,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$7f,$7f,$7f,$00,$00
	.byte $70,$71,$00,$73,$74,$75,$76,$77,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f
	.byte $9f,$9f,$9c,$9d,$9e,$9f,$9d,$9e,$9f,$8d,$8e,$8f,$8f,$8f,$00,$00
	.byte $00,$29,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a
	.byte $2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2d,$00
	.byte $00,$2d,$92,$93,$94,$95,$92,$93,$94,$00,$92,$92,$93,$94,$92,$93
	.byte $94,$00,$92,$93,$94,$00,$92,$93,$94,$00,$92,$93,$94,$00,$2d,$00
	.byte $00,$2d,$a2,$a3,$a4,$a5,$a2,$a3,$a4,$78,$a2,$a2,$a3,$a4,$a2,$a3
	.byte $a4,$90,$a2,$a3,$a4,$00,$a2,$a3,$a4,$98,$a2,$a3,$a4,$9c,$2d,$00
	.byte $00,$2d,$b2,$b3,$b4,$b5,$b2,$b3,$b4,$78,$b2,$b2,$b3,$b4,$b2,$b3
	.byte $b4,$a0,$b2,$b3,$b4,$a4,$b2,$b3,$2f,$a8,$b2,$b3,$b4,$ac,$2d,$00
	.byte $00,$2d,$c2,$c3,$c4,$c5,$c2,$c3,$c4,$88,$c2,$c2,$c3,$c4,$c2,$c3
	.byte $c4,$b0,$c2,$c3,$c4,$b4,$c2,$c3,$c4,$b8,$c2,$c3,$c4,$00,$2d,$00
	.byte $00,$2d,$71,$72,$00,$00,$75,$76,$77,$78,$79,$7a,$7b,$7c,$7d,$7e
	.byte $7f,$c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$00,$2d,$00
	.byte $00,$2d,$92,$93,$94,$74,$92,$93,$94,$92,$93,$94,$93,$94,$92,$93
	.byte $94,$d0,$92,$93,$94,$d4,$92,$93,$94,$d8,$92,$93,$94,$00,$2d,$00
	.byte $00,$2d,$a2,$a3,$a4,$74,$a2,$a3,$a4,$a2,$a3,$a4,$a3,$a4,$a2,$a3
	.byte $a4,$00,$a2,$a3,$a4,$00,$a2,$a3,$a4,$e8,$a2,$a3,$a4,$00,$2d,$00
	.byte $00,$2d,$b2,$b3,$b4,$74,$b2,$b3,$b4,$b2,$b3,$b4,$b3,$b4,$b2,$b3
	.byte $b4,$f0,$b2,$b3,$b4,$00,$b2,$b3,$b4,$00,$b2,$b3,$b4,$00,$2d,$00
	.byte $f0,$2b,$c2,$c3,$c4,$74,$c2,$c3,$c4,$c2,$c3,$c4,$c3,$c4,$c2,$c3
	.byte $c4,$f0,$c2,$c3,$c4,$f4,$c2,$c3,$c4,$00,$c2,$c3,$c4,$00,$2d,$00
	.byte $00,$2e,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c
	.byte $2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2d,$00
	.byte $00,$70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7a,$7b,$7c,$7d,$7e
	.byte $a0,$f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$00,$00,$00
	.byte $00,$80,$70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7a,$7b,$7c,$7d
	.byte $b0,$f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe
	.byte $00,$13,$0f,$04,$1c,$08,$15,$00,$00,$00,$00,$06,$04,$16,$0b,$30
	.byte $31,$1e,$1e,$1e,$00,$05,$0c,$07,$30,$31,$1e,$1e,$23,$00,$fd,$fe
	.byte $00,$a0,$80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d
	.byte $d0,$e0,$00,$00,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$00,$00,$00
	.byte $00,$29,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a
	.byte $2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2d,$00
	.byte $00,$2d,$a2,$a2,$a3,$a4,$a2,$a2,$a2,$a2,$a2,$a2,$a2,$a2,$a2,$a2
	.byte $a2,$a2,$a2,$a2,$a2,$a2,$a2,$a2,$a2,$a2,$a2,$a3,$a4,$00,$2d,$00
	.byte $00,$2d,$b2,$a2,$a2,$a2,$b2,$b2,$b2,$b2,$b2,$b2,$b2,$b2,$b2,$b2
	.byte $b2,$b2,$b2,$b2,$b2,$b2,$b2,$b2,$b2,$b2,$b2,$b3,$b4,$00,$2d,$00
	.byte $00,$2d,$c2,$b2,$b2,$b2,$c2,$c2,$c2,$c2,$c2,$c2,$c2,$c2,$c2,$c2
	.byte $c2,$c2,$c2,$c2,$c2,$c2,$c2,$c2,$c2,$c2,$c2,$c3,$c4,$00,$2d,$00
	.byte $00,$2d,$d2,$c2,$c2,$c2,$d2,$d2,$d2,$d2,$d2,$d2,$d2,$d2,$d2,$d2
	.byte $d2,$d2,$d2,$d2,$d2,$d2,$d2,$d2,$d2,$d2,$d2,$d3,$d4,$00,$2d,$00
	.byte $00,$2d,$00,$d2,$d2,$d2,$d2,$d2,$d3,$d4,$d4,$d4,$78,$79,$7a,$7b
	.byte $e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$00,$00,$2d,$00
	.byte $00,$2d,$92,$92,$92,$92,$92,$92,$92,$92,$92,$92,$92,$92,$92,$92
	.byte $92,$92,$92,$92,$92,$92,$92,$92,$92,$92,$92,$93,$94,$00,$2d,$00
	.byte $00,$2d,$a2,$a2,$a2,$a2,$a2,$a2,$a2,$a2,$a2,$a2,$a2,$a2,$a2,$a2
	.byte $a2,$a2,$a2,$a2,$a2,$a2,$a2,$a2,$a2,$a2,$a2,$a3,$a4,$00,$2d,$00
	.byte $00,$2d,$b2,$b2,$b2,$b2,$b2,$b2,$b2,$b2,$b2,$b2,$b2,$b2,$b2,$b2
	.byte $b2,$b2,$b2,$b2,$b2,$b2,$b2,$b2,$b2,$b2,$b2,$b3,$b4,$00,$2d,$00
	.byte $00,$2d,$c2,$c2,$c2,$c2,$c2,$c2,$c2,$c2,$c2,$c2,$c2,$c2,$c2,$c2
	.byte $c2,$c2,$c2,$c2,$c2,$c2,$c2,$c2,$c2,$c2,$c2,$c3,$c4,$fd,$2d,$00
	.byte $00,$2e,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c
	.byte $2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2c,$2d,$00
	.byte $00,$f0,$f1,$d0,$d0,$c0,$c0,$b0,$b0,$b0,$b0,$b0,$b0,$b0,$b0,$c0
	.byte $f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$00
	.byte $42,$50,$50,$50,$50,$50,$50,$10,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

  flippedCard:
    .byte $40,$3f,$41,$3c,$33,$3d,$3c,$33,$3d,$42,$3e,$43
  
  PlayerWins:
    .byte $13, $0f, $04, $1c, $08, $15, $32, $1a, $0c, $11, $16
  PlayerLost:
    .byte $13, $0f, $04, $1c, $08, $15, $32, $0f, $12, $16, $17
  Tie:
    .byte $17, $0c, $08
  LosesGame: ; 9 bytes
    .byte $11, $12, $32, $10, $12, $11, $08, $1c, $28
  WinsGame: ; 14 bytes
    .byte $04, $0f, $0f, $32, $17, $0b, $08, $32, $10, $12, $11, $08, $1c, $28
.endproc

.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler

.segment "RODATA"
palettes:
;Background
.byte $0f, $05, $20, $0f
.byte $0f, $06, $09, $19
.byte $0f, $07, $09, $17
.byte $0f, $08, $09, $15

;Sprite
.byte $19, $0f, $0f, $0f
.byte $19, $05, $20, $30 
.byte $19, $19, $09, $2a
.byte $19, $16, $07, $1a
sprites: ; $YPosition, $Tilenum, $Attribute, $XPosition

;Hearts
hearts:
  .byte $BC, $BD, $BE, $CC, $44, $CE,$DC, $DD, $DE, $EC, $ED, $56
  .byte $BC, $BD, $BE, $CC, $44, $CE,$DC, $DD, $DE, $EC, $ED, $58
  .byte $BC, $BD, $BE, $CC, $44, $CE,$DC, $DD, $DE, $EC, $ED, $59
  .byte $BC, $BD, $BE, $CC, $44, $CE,$DC, $DD, $DE, $EC, $ED, $5A
  .byte $BC, $BD, $BE, $CC, $44, $CE,$DC, $DD, $DE, $EC, $ED, $5B
  .byte $BC, $BD, $BE, $CC, $44, $CE,$DC, $DD, $DE, $EC, $ED, $5C
  .byte $BC, $BD, $BE, $CC, $44, $CE,$DC, $DD, $DE, $EC, $ED, $5D
  .byte $BC, $BD, $BE, $CC, $44, $CE,$DC, $DD, $DE, $EC, $ED, $5E
  .byte $BC, $BD, $BE, $CC, $44, $CE,$DC, $DD, $DE, $EC, $ED, $5F
  .byte $BC, $BD, $BE, $CC, $44, $CE,$DC, $DD, $DE, $EC, $ED, $60
  .byte $BC, $BD, $BE, $CC, $44, $CE,$DC, $DD, $DE, $EC, $ED, $63
  .byte $BC, $BD, $BE, $CC, $44, $CE,$DC, $DD, $DE, $EC, $ED, $62
  .byte $BC, $BD, $BE, $CC, $44, $CE,$DC, $DD, $DE, $EC, $ED, $61
;Diamonds
diamonds:
  .byte $BC, $BD, $BE, $CC, $45, $CE,$DC, $DD, $DE, $EC, $ED, $56
  .byte $BC, $BD, $BE, $CC, $45, $CE,$DC, $DD, $DE, $EC, $ED, $58
  .byte $BC, $BD, $BE, $CC, $45, $CE,$DC, $DD, $DE, $EC, $ED, $59
  .byte $BC, $BD, $BE, $CC, $45, $CE,$DC, $DD, $DE, $EC, $ED, $5A
  .byte $BC, $BD, $BE, $CC, $45, $CE,$DC, $DD, $DE, $EC, $ED, $5B
  .byte $BC, $BD, $BE, $CC, $45, $CE,$DC, $DD, $DE, $EC, $ED, $5C
  .byte $BC, $BD, $BE, $CC, $45, $CE,$DC, $DD, $DE, $EC, $ED, $5D
  .byte $BC, $BD, $BE, $CC, $45, $CE,$DC, $DD, $DE, $EC, $ED, $5E
  .byte $BC, $BD, $BE, $CC, $45, $CE,$DC, $DD, $DE, $EC, $ED, $5F
  .byte $BC, $BD, $BE, $CC, $45, $CE,$DC, $DD, $DE, $EC, $ED, $60
  .byte $BC, $BD, $BE, $CC, $45, $CE,$DC, $DD, $DE, $EC, $ED, $63
  .byte $BC, $BD, $BE, $CC, $45, $CE,$DC, $DD, $DE, $EC, $ED, $62
  .byte $BC, $BD, $BE, $CC, $45, $CE,$DC, $DD, $DE, $EC, $ED, $61
;Clubs
clubs:
  .byte $BC, $BD, $BE, $CC, $46, $CE,$DC, $DD, $DE, $EC, $ED, $48
  .byte $BC, $BD, $BE, $CC, $46, $CE,$DC, $DD, $DE, $EC, $ED, $4A
  .byte $BC, $BD, $BE, $CC, $46, $CE,$DC, $DD, $DE, $EC, $ED, $4B
  .byte $BC, $BD, $BE, $CC, $46, $CE,$DC, $DD, $DE, $EC, $ED, $4C
  .byte $BC, $BD, $BE, $CC, $46, $CE,$DC, $DD, $DE, $EC, $ED, $4D
  .byte $BC, $BD, $BE, $CC, $46, $CE,$DC, $DD, $DE, $EC, $ED, $4E
  .byte $BC, $BD, $BE, $CC, $46, $CE,$DC, $DD, $DE, $EC, $ED, $4F
  .byte $BC, $BD, $BE, $CC, $46, $CE,$DC, $DD, $DE, $EC, $ED, $50
  .byte $BC, $BD, $BE, $CC, $46, $CE,$DC, $DD, $DE, $EC, $ED, $51
  .byte $BC, $BD, $BE, $CC, $46, $CE,$DC, $DD, $DE, $EC, $ED, $52
  .byte $BC, $BD, $BE, $CC, $46, $CE,$DC, $DD, $DE, $EC, $ED, $53
  .byte $BC, $BD, $BE, $CC, $46, $CE,$DC, $DD, $DE, $EC, $ED, $54
  .byte $BC, $BD, $BE, $CC, $46, $CE,$DC, $DD, $DE, $EC, $ED, $55
;Spades
spades:
  .byte $BC, $BD, $BE, $CC, $47, $CE,$DC, $DD, $DE, $EC, $ED, $48
  .byte $BC, $BD, $BE, $CC, $47, $CE,$DC, $DD, $DE, $EC, $ED, $4A
  .byte $BC, $BD, $BE, $CC, $47, $CE,$DC, $DD, $DE, $EC, $ED, $4B
  .byte $BC, $BD, $BE, $CC, $47, $CE,$DC, $DD, $DE, $EC, $ED, $4C
  .byte $BC, $BD, $BE, $CC, $47, $CE,$DC, $DD, $DE, $EC, $ED, $4D
  .byte $BC, $BD, $BE, $CC, $47, $CE,$DC, $DD, $DE, $EC, $ED, $4E
  .byte $BC, $BD, $BE, $CC, $47, $CE,$DC, $DD, $DE, $EC, $ED, $4F
  .byte $BC, $BD, $BE, $CC, $47, $CE,$DC, $DD, $DE, $EC, $ED, $50
  .byte $BC, $BD, $BE, $CC, $47, $CE,$DC, $DD, $DE, $EC, $ED, $51
  .byte $BC, $BD, $BE, $CC, $47, $CE,$DC, $DD, $DE, $EC, $ED, $52
  .byte $BC, $BD, $BE, $CC, $47, $CE,$DC, $DD, $DE, $EC, $ED, $53
  .byte $BC, $BD, $BE, $CC, $47, $CE,$DC, $DD, $DE, $EC, $ED, $54
  .byte $BC, $BD, $BE, $CC, $47, $CE,$DC, $DD, $DE, $EC, $ED, $55

.segment "CHARS"
.incbin "BlackJackMap.chr"