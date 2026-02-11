; Zoo Escape
; John Eblen
; February 2026

; Header 
  processor 6502
  include "vcs.h"
  include "macro.h"

; Constants
GAMEBOARD_BASE_BGCOLOR = $D0
GAMEBOARD_BGCOLOR_AM = $10
SCOREBOARD_BGCOLOR = $FF
SCOREBOARD_BGCOLOR_AM = $F0
DIGIT_COLOR = $00
NUM_ANIMALS = $05
SOUND_EFFECT_VOLUME = $0A
CHANGE_DIR_FREQ = $c0 ; How frequently do animals change direction?
                      ; in number of frames
SELECT_DEBOUNCE_TIME = $1e
MAX_NUM_BOARDS = $0A ; Capped at 10 for now, so it can be
                     ; displayed with a single digit

; Variables
  SEG.U variables
  org $80

; Which board is being played?
; This is changed by game select and displayed in the lower left
; It is used as the seed for the RNG whenever the game is reset
board_num ds 1

game_state ds 1 ; 0 - attract mode
                ; 1 - ready mode
                ; 2 - game mode
clock ds 1  ; A nice, regular clock incremented once per frame
game_select_debounce_time ds 1
x_array ds 6
y_array ds 6
x_last_array ds 6
y_last_array ds 6
room_x ds 6
room_y ds 6
dir_array ds 6

room_trans_type ds 1 ; Local variable for room_trans_* subroutines ONLY
change_dir_count ds 1 ; Clock to keep track of when animals change direction

bit_hash ds 1 ; Random byte

; Scoreboard digits
scoreboard_digits ds 2

; PF values for board number (left side of screen)
PF1_board_number_values ds 8
; PF values for 8 lines of timer (right side of screen)
PF1_timer_values ds 8
PF2_timer_values ds 8

; Offset of animal sprite to draw (recomputed each frame)

animal_to_draw ds 1
animal_sprite_offset ds 1 ; Should be animal_to_draw * 10

scoreboard_segment_to_update ds 1
clock_update_counter ds 1 ; Update every 6 frames (appx. every 1/10 second)

tmp0 ds 1 ; temporary storage bytes
tmp1 ds 1

in_same_room ds 1 ; 0 - player and animal in same room
animals_still_loose ds 1 ; bitmask of animals still in play

; Sound bytes
sound_loop_counter ds 1 ; loops remaining
sound_note_counter ds 1 ; "notes" remaining for current loop
sound_beat_counter ds 1 ; "beats" (frames) remaining for current note
sound_note_offset ds 1 ; Offset to current "note"
sound_start_note_offset ds 1 ; Offset to first "note"

; Sprite speed control bytes
move_animals ds 1 ; whether animal movement is on or off (1 or 0)
move_player ds 1 ; whether player movement is on or off (1 or 0)
move_animals_counter ds 1 ; counter for toggling "move_animals"
move_player_counter ds 1 ; counter for toggling "move_player"

; Start of ROM code
  SEG code
  org $F000

; This is the only variable not re-initialized on game reset
; (is stored at $FFF9)
  lda #0
  sta board_num

Reset
  sei
  cld
; Andrew Davie's 8-byte initialization code - now 9 bytes to set y=0 too.
; January 2026: modified to avoid clearing board_num and select debouncing
  ldx #0
  txa
  ldy board_num ; save board_num
Clear
  dex
  txs
  pha
  bne Clear
  sty board_num ;restore board_num
  ; Avoids incrementing board_num twice when select causes a game reset
  ldy #SELECT_DEBOUNCE_TIME
  sty game_select_debounce_time
  tay ; set y=0

; Variable initializations
  ldx #$FF
  txs ; Don't forget to position stack at FF!
  sta sound_loop_counter

  lda #1
  sta game_state

  ; Reset player and animal locations
  ldx #5 ; 6 characters (player + 5 animals)
  lda x_bounds
  ldy y_bounds
init_locations_loop
  sta x_array,x
  sta x_last_array,x
  sty y_array,x
  sty y_last_array,x
  dex
  bpl init_locations_loop

  ; Put animals in play
  lda #0
  ldx #NUM_ANIMALS
put_animals_in_play_loop
  rol
  eor #%00000001
  dex
  bne put_animals_in_play_loop
  sta animals_still_loose

  ; Place player in lower-left room
  lda #0
  sta room_x
  sta room_y

  ; Loop to place animals in a random room
  ; Also assigns a random direction
  ldx #NUM_ANIMALS
assign_animals_to_rooms

  ; Load random byte
  jsr hash_bits
  lda hash_bits

  ; Assign random row (1 or 2)
  and #$07
  ldy #1
  ror
  bcc put_animal_in_middle_row
  iny
put_animal_in_middle_row
  sty room_x,x

  ; Assign random column (1-4)
  clc
  adc #1
  sta room_y,x

  ; Load random byte
  jsr hash_bits
  lda bit_hash

  ; Assign random direction
  and #$07
  tay
  lda joystick_values,y
  sta dir_array,x

  dex
  bne assign_animals_to_rooms
  ; End loop to place animals in a random room

  ; Set info about sprite
  lda #0
  sta animal_to_draw

  ; Set the random seed to board_num
  lda board_num
  sta bit_hash

  lda #1
  sta move_animals
  sta move_player
  lda num_frames_move_animals
  sta move_animals_counter
  lda num_frames_move_player
  sta move_player_counter

;---------------------------- START MAIN LOOP ----------------------------------
Mainloop
; Vertical Blanking
  lda #2
  sta VSYNC
  sta WSYNC
  sta WSYNC
  sta WSYNC
  lda #0
  sta VSYNC
  lda #43
  sta TIM64T ; clock expires just before end of 37th vertical blank line
             ; At that point, do one more "sta WSYNC" and then start processing.
; Loop initialization
  sta HMCLR
  sta CXCLR
  inc clock

  ; Set screen colors
  lda room_y ; Set background color
  asl
  asl
  asl
  asl
  adc room_x
  adc room_x
  adc #GAMEBOARD_BASE_BGCOLOR
  sta COLUBK
  lda game_state
  bne do_not_set_gb_attract_bgcolor
  lda #GAMEBOARD_BGCOLOR_AM
  sta COLUBK
do_not_set_gb_attract_bgcolor
  sta COLUPF

  ; First, create bitmask (tmp0) of animals that are either no longer in
  ; play or in the same room. This byte is then shifted for easier
  ; computations and is used to find animals that just arrived in the same
  ; room as the player, which is used for triggering sound effects.
  ; tmp1 holds the index of the new arrival.
  lda #0
  sec
  sbc animals_still_loose
  sec
  sbc #1
  ora in_same_room
  ldx NUM_ANIMALS
previous_same_room_byte_shift
  cpx #8
  beq end_previous_same_room_byte_shift
  asl
  inx
  jmp previous_same_room_byte_shift
end_previous_same_room_byte_shift
  sta tmp0

  ; Compute in_same_room (animals in the same room as player
  ; and animals that have just arrived)
  lda #0
  sta tmp1 ; Animal first appearing in same room, or $00 if none
  ldx NUM_ANIMALS
same_room_loop
  asl
  sec
  ldy room_x,x
  cpy room_x
  bne same_room_loop_cont
  sec
  ldy room_y,x
  cpy room_y
  bne same_room_loop_cont
  ora #%00000001 ; Place animal in same room
  ldy tmp0 ; tmp0 is previous (and shifted) same-room byte
  bmi same_room_loop_cont
  ; Animal just appeared in same room - store in tmp1
  stx tmp1
same_room_loop_cont
  asl tmp0
  dex
  bne same_room_loop
  sta in_same_room

  ; SOUND EFFECT HANDLING CODE
  ; Each frame we either continue the sound currently
  ; playing (section 1) or see if we should start a
  ; new sound (section 2) - never both.
  
  ; Manage sound effect currently playing (section 1)
  lda sound_loop_counter
  beq no_sound_playing ; no sound playing - abort to section 2
  dec sound_beat_counter
  bne end_sound_handling ; note still playing - abort to end

  dec sound_note_counter
  bne load_new_note ; more notes to play - skip ahead

  dec sound_loop_counter
  bne rewind_sound ; play it again Sam

  ; Sound effect finished - turn it off
  lda #0
  sta AUDV0
  jmp end_sound_handling

  ; Rewind (reset note offset)
rewind_sound
  lda sound_start_note_offset
  sta sound_note_offset

  ; Load a new note
load_new_note
  ldy sound_note_offset
  iny
  lda sound_data,y
  sta AUDC0
  iny
  lda sound_data,y
  sta AUDF0
  iny
  lda sound_data,y
  sta sound_beat_counter
  sty sound_note_offset
  
  jmp end_sound_handling
  ; End section 1 of sound handling


  ; See if we should load a new sound (section 2)
  ; Load a new sound on the following events
  ; 1) Animal first appears in same room as player.
no_sound_playing
  ldx tmp1 ; tmp1 is id of animal that just arrived
  beq end_sound_handling
  lda #0
  ldy #0
  clc

  ; Iterate to the correct animal's sound data
sound_data_search_loop
  dex
  beq load_sound_data
  ; Data length is (number of notes)*3 + 2
  adc sound_data,y
  adc sound_data,y
  adc sound_data,y
  adc #2
  tay
  bne sound_data_search_loop ; should always branch

load_sound_data
  ; Load the sound
  lda sound_data,y
  sta sound_note_counter
  iny
  lda sound_data,y
  sta sound_loop_counter
  iny
  lda sound_data,y
  sta AUDC0
  iny
  lda sound_data,y
  sta AUDF0
  iny
  lda sound_data,y
  sta sound_beat_counter
  sty sound_note_offset
  sty sound_start_note_offset
  lda #SOUND_EFFECT_VOLUME
  sta AUDV0
end_sound_handling
  ; END SOUND EFFECT HANDLING CODE
  

; Compute animal sprite data for drawing code
  ; First loop - rotate in_same_room byte so that rightmost
  ; bit is for current animal
  lda #%00000001
  sta tmp0
  lda in_same_room
  ldx animal_to_draw
  beq end_in_same_room_rot_loop
in_same_room_rot_loop
  ; Mimic a byte rotation by setting carry to be the
  ; value of the rightmost bit
  bit tmp0
  beq bit_is_zero_isr
  sec
  bcs carry_okay
bit_is_zero_isr
  clc
carry_okay
  ror
  dex
  bne in_same_room_rot_loop
end_in_same_room_rot_loop

  ; Second loop - find next animal to draw
find_next_animal_to_draw_loop
  inc animal_to_draw
  lsr
  bcs found_an_animal
  bne find_next_animal_to_draw_loop
  ; Did not find an animal - set animal_to_draw == 0
  lda #0
  sta animal_to_draw
  beq end_find_next_animal
found_an_animal
  lda animal_to_draw
  and #%00000111 ; Mod by 8
  sta animal_to_draw
end_find_next_animal
  ; Multiply by ten and store as sprite offset
  asl
  asl
  asl
  adc animal_to_draw
  adc animal_to_draw
  sta animal_sprite_offset

  ; Update info on when to move animals
  ; (toggle move_animals and update counter if needed)
  dec move_animals_counter
  bne end_updating_info_on_moving_animals
  lda #1
  eor move_animals
  sta move_animals

  lda num_frames_stop_animals
  ldx move_animals
  beq animals_halted
  lda num_frames_move_animals
animals_halted
  sta move_animals_counter
end_updating_info_on_moving_animals

  ; Update info on when to move player (similar to above code for animals)
  ; (toggle move_player and update counter if needed)
  dec move_player_counter
  bne end_updating_info_on_moving_player
  lda #1
  eor move_player
  sta move_player

  lda num_frames_stop_player
  ldx move_player
  beq player_halted
  lda num_frames_move_player
player_halted
  sta move_player_counter
end_updating_info_on_moving_player

; Move logic - a case statement
; Check for room transitions when boundary hit.  Looks convoluted,
; but all we're doing is calling the room transition subroutine
; instead of storing the new x or y position.

  ; Skip movement while in attract mode
  lda game_state
  ror
  bcs start_hor_shift

  ; This pre-loop logic handles 3 out of 4 cases:
  ; move both player and animals, move neither, or move player only
  ; The fourth case, move animals only, is handled at the end of the loop
  ldy #NUM_ANIMALS
  lda move_animals
  bne move_loop
  lda move_player
  beq start_hor_shift ; Skip movement
  lda #0
  tay

move_loop
  ; Backup current position first
  lda x_array,y
  sta x_last_array,y
  lda y_array,y
  sta y_last_array,y
  
  jsr change_animal_dir

  ; February 2026: Do not move animals already captured
  ; Note: This MUST be done after calling change_animal_dir
  ; 1) So that RNG is called a prime number of times
  ; 2) To update change_dir_count
  tax
  lda animals_still_loose
  and animal_is_loose_mask,y
  beq end_js_read
  txa

  asl
  bcs case_left
  ldx x_array,y
  inx
  sec
  cpx x_bounds+1
  bcs right_bound_hit
  stx x_array,y
  jmp case_left
right_bound_hit
  jsr room_trans_right
case_left
  asl
  bcs case_down
  ldx x_array,y
  dex
  sec
  cpx x_bounds
  bcc left_bound_hit
  stx x_array,y
  jmp case_down
left_bound_hit
  jsr room_trans_left
case_down
  asl
  bcs case_up
  ldx y_array,y
  dex
  sec
  cpx y_bounds
  bcc bottom_bound_hit
  stx y_array,y
  jmp case_up
bottom_bound_hit
  jsr room_trans_bottom
case_up
  asl
  bcs end_js_read
  ldx y_array,y
  inx
  sec
  cpx y_bounds+1
  bcs top_bound_hit
  stx y_array,y
  jmp end_js_read
top_bound_hit
  jsr room_trans_top
end_js_read
  dey
  bmi start_hor_shift ; y < 0
  bne move_loop ; y > 0 indicates animal

  ; y = 0 indicates player. Check move_player
  lda move_player
  bne move_loop
end_move

start_hor_shift
; Do player horizontal shift
  ldx #0
  lda x_array
  jsr horpos

; Do animal horizontal shift
  ldx #1
  ldy animal_to_draw
  lda x_array,y
  jsr horpos
  sta WSYNC
  sta HMOVE
 
  ; Increment clock 
  lda game_state
  ror
  ror
  bcc skip_inc_clock ; only increment clock in game mode
  inc clock_update_counter
  lda clock_update_counter
  sec
  cmp #6
  bcc skip_inc_clock
  lda #0
  sta clock_update_counter

  ; Do actual clock increment
  sed
  lda scoreboard_digits+1
  clc
  adc #1
  sta scoreboard_digits+1
  lda scoreboard_digits
  adc #0
  sta scoreboard_digits
  cld
skip_inc_clock
  

; Compute scoreboard graphics
; (compute only one segment for first three digits on each frame)
; (compute more segments for fourth digit (see notes below))
  inc scoreboard_segment_to_update
  lda scoreboard_segment_to_update
  and #%00000111
  sta scoreboard_segment_to_update

  ; Multiply A by ten to get digit sprite offset
  ; Store result in tmp0 and x
  clc
  rol
  rol
  rol
  adc scoreboard_segment_to_update
  adc scoreboard_segment_to_update
  ; ldx #10
  ; jsr multiply
  tax
  sta tmp0

  ; January 2026: Draw board number instead of first digit of timer
  lda board_num
  clc
  adc tmp0
  ror
  tay
  lda digit_sprites,y
  bcc no_shift_digit_1
  asl
  asl
  asl
  asl
no_shift_digit_1
  and #%11110000 ; Only one digit, so only use the left side of PF1
  ldy scoreboard_segment_to_update
  sta PF1_board_number_values,y

  ; Second digit
  lda scoreboard_digits
  and #%00001111
  clc
  adc tmp0
  ror
  tay
  lda digit_sprites,y
  bcc no_shift_digit_2
  asl
  asl
  asl
  asl
no_shift_digit_2
  and #%11110000
  sta tmp1

  ; Third digit
  lda scoreboard_digits+1
  lsr
  lsr
  lsr
  lsr
  clc
  adc tmp0
  ror
  tay
  lda digit_sprites,y
  bcs no_shift_digit_3
  lsr
  lsr
  lsr
  lsr
no_shift_digit_3
  and #%00001111
  eor tmp1
  ldy scoreboard_segment_to_update
  sta PF1_timer_values,y

  ; Fourth digit
  ; This digit must be updated more frequently. So we update either
  ; all even segments or all odd segments each time (4 iterations)
  ldx #4
  lda scoreboard_digits+1
  and #%00001111
  sta tmp1 ; Store digit offset, so we don't have to recompute every
           ; iteration
  lda tmp0
draw_fourth_digit_segment
  clc
  adc tmp1
  ror
  tay
  lda digit_sprites_reversed,y
  bcs no_shift_digit_4
  lsr
  lsr
  lsr
  lsr
no_shift_digit_4
  rol ; Shift right to make room for decimal
  rol
  and #%00111100
  ldy scoreboard_segment_to_update
  dey
  sec
  cpy #3
  bcs do_not_draw_decimal_point
  eor #%00000010 ; Add decimal point
do_not_draw_decimal_point
  iny
  sta PF2_timer_values,y

  ; tmp0 is offset for digit segments. Increase by #20 or 2 segments
  ; We also need to update scoreboard_segment_to_update, which is fine
  ; because it cycles back to its original value.
  lda tmp0
  clc
  adc #10
  inc scoreboard_segment_to_update
  sec
  cmp #80
  bne do_not_reset_1
  lda #0
  sta scoreboard_segment_to_update
do_not_reset_1
  clc
  adc #10
  inc scoreboard_segment_to_update
  sec
  cmp #80
  bne do_not_reset_2
  lda #0
  sta scoreboard_segment_to_update
do_not_reset_2
  sta tmp0
  dex
  bne draw_fourth_digit_segment
; End compute scoreboard graphics

; Sync to end of vertical blank period
vbwait
  lda INTIM
  bne vbwait
  sta WSYNC
  sta VBLANK

; Begin drawing screen

; Main loop for drawing gameboard
  ldy #184
draw_gameboard_line
  dey
  beq end_draw_gameboard
  tya
  ror
  bcs draw_animal
  sec
  tya
  sbc y_array
  adc player_sprite_data
  sta WSYNC
  bcc draw_gameboard_line
  lsr
  tax
  lda player_sprite,x
  sta GRP0
  lda player_sprite_data,x
  sta COLUP0
  jmp draw_gameboard_line
draw_animal
  tya
  ldx animal_to_draw
  sbc y_array,x
  ldx animal_sprite_offset
  adc sprite_data,x
  sta WSYNC
  bcc draw_gameboard_line
  lsr
  clc
  adc animal_sprite_offset
  tax
  lda animal_to_draw
  beq draw_gameboard_line
  lda sprite,x
  sta GRP1
  lda sprite_data,x
  sta COLUP1
  jmp draw_gameboard_line
end_draw_gameboard
; End drawing gameboard

; Main loop for drawing scoreboard
  lda #SCOREBOARD_BGCOLOR
  sta tmp0
  ldx game_state
  bne do_not_set_sb_attract_bgcolor
  lda #SCOREBOARD_BGCOLOR_AM
  sta tmp0
do_not_set_sb_attract_bgcolor
  sta COLUBK
  sta COLUPF
  ldy #8
draw_scoreboard_line
  sta WSYNC ; One line between gameboard and scoreboard
  dey
  bmi end_draw_scoreboard

  ; Horizontal blank
  ; Set color for digits, set PF1 to display board number, and clear PF2
  lda #DIGIT_COLOR
  sta COLUPF
  lda PF1_board_number_values,y
  sta PF1
  lda #0
  sta PF2

  ; Wait until drawing of PF2(a) to set PF1 to new value (timer part)
  nop
  nop
  nop
  nop
  nop
  nop
  nop
  nop
  nop
  nop
  lda PF1_timer_values,y
  sta PF1

  ; Wait until drawing of PF0(b) PF1(b) to set PF2
  nop
  nop
  nop
  lda PF2_timer_values,y
  sta PF2
  jmp draw_scoreboard_line
end_draw_scoreboard

; Set timer for overscan period
  lda #2
  sta VBLANK
  lda #35
  sta TIM64T ; clock expires just before end of overscan.
             ; At that point, do one more "sta WSYNC" and then start new frame

  ; Handle game state transitions

  ; Criteria for game over (transition to attract mode)
  jsr handle_collisions
  bcc game_not_over
  lda animals_still_loose
  ldx animal_to_draw
  and animal_remove_mask,x
  sta animals_still_loose
  bne game_not_over
  sta game_state
game_not_over

  ; Check joystick button (transition to game mode)
  lda INPT4
  rol
  bcs do_not_start_game
  lda game_state
  ror
  bcc do_not_start_game ; only from ready mode, not attract mode
  lda #2
  sta game_state
do_not_start_game

  ; Check for game select using proper debouncing
  lda game_select_debounce_time
  bne game_select_debounce
  lda SWCHB
  ror
  ror
  bcs oswait

  inc board_num
  lda board_num
  sec
  cmp #MAX_NUM_BOARDS
  bne do_not_reset_board_num
  lda #0
  sta board_num
do_not_reset_board_num
  lda #SELECT_DEBOUNCE_TIME
  sta game_select_debounce_time
  beq oswait
game_select_debounce
  dec game_select_debounce_time

; Start new frame
oswait
  lda INTIM
  bne oswait
  sta WSYNC

  ; Check for game reset (transition to ready mode)
  lda game_state
  ror
  bcs do_not_reset ; skip reset if already in reset mode
                   ; (prevents ugly flicker while switch pressed)
  lda SWCHB
  ror
  bcc reset_pressed
  ror
  bcs do_not_reset ; neither switch pressed
reset_pressed
  lda #1
  sta game_state
  jmp Reset
do_not_reset
  jmp Mainloop
;---------------------------- END MAIN LOOP ------------------------------------

; Constants
num_frames_stop_animals ; number of frames to stop animals (when move_animals is 0)
  .byte $01
num_frames_move_animals ; number of frames to move animals (when move_animals is 1)
  .byte $07
num_frames_stop_player ; number of frames to stop player (when move_player is 0)
  .byte $01
num_frames_move_player ; number of frames to move player (when move_player is 1)
  .byte $FF

x_bounds ; Bounding box for sprite movements
  .byte $0F,$99
y_bounds
  .byte $15,$B9

num_rooms_x ; Dimensions of the zoo
  .byte $04
num_rooms_y
  .byte $02

joystick_values
  .byte #%01111111,#%10111111,#%11011111,#%11101111,#%01011111,#%01101111,#%10011111,#%10101111

; Subroutines

; Linear Congruential Random Number Generator
; Formula is x = (5x+13) mod 256
; Input - bit_hash variable
; Output - bit_hash variable, also in A
; Only A is altered
hash_bits
  lda bit_hash
  clc
  adc bit_hash
  clc
  adc bit_hash
  clc
  adc bit_hash
  clc
  adc bit_hash
  clc
  adc #13
  sta bit_hash
  rts

; Room transitioning subroutine
; "room_trans_type" varies based on entry point (variable is inc 0-3 times).
; Next, we shift bits and leap frog through a case statement to decide which
; case to handle.
; Input is Y - offset of animal to transition
; Output is required changes to animal position array
; No registers altered (A and X are saved on stack)
room_trans_right
  inc room_trans_type
room_trans_left
  inc room_trans_type
room_trans_bottom
  inc room_trans_type
room_trans_top
  pha ; Save A and X
  txa
  pha
  tya ; Transfer Y to X (need X for inc and dec)
  tax
  lda room_trans_type
  lsr
  bcs rt_right_or_bottom
rt_left_or_top
  lsr
  bcs rt_left
rt_top
  lda room_y,x
  sec
  cmp num_rooms_y
  bcs rt_exit
  inc room_y,x
  lda y_bounds
  sta y_array,x
  jmp rt_exit
rt_left
  lda room_x,x
  beq rt_exit
  dec room_x,x
  lda x_bounds+1
  sta x_array,x
  jmp rt_exit
rt_right_or_bottom
  lsr
  bcs rt_right
rt_bottom
  lda room_y,x
  beq rt_exit
  dec room_y,x
  lda y_bounds+1
  sta y_array,x
  jmp rt_exit
rt_right
  lda room_x,x
  sec
  cmp num_rooms_x
  bcs rt_exit
  inc room_x,x
  lda x_bounds
  sta x_array,x
rt_exit ; Important restoration work before returning
  lda #0
  sta room_trans_type ; assumed to be 0 when subroutine entered
  pla
  tax
  pla
  rts

; Subroutine to handle collisions in prior frame
; return carry set if collision between players
handle_collisions
  lda CXP0FB
  bpl no_pf_coll
  lda x_last_array
  sta x_array
  lda y_last_array
  sta y_array
no_pf_coll
  lda CXPPMM
  rol
  rts

; Subroutine to handle animal movement
; Input is Y - offset of positions array
; Output is A - SWCHA-like bitmap of animal direction (as if by joystick)
; Only the A register is altered!
change_animal_dir
  tya
  bne not_player ; player movement is just read from the joystick
  lda SWCHA
  rts
not_player
  ; Compute a new random direction if it is time to do so
  lda change_dir_count
  sec
  cmp #CHANGE_DIR_FREQ
  bne skip_change_dir
  jsr hash_bits
  lda bit_hash
  and #$07
  sty tmp0
  tay
  lda joystick_values,y
  ldy tmp0
  sta dir_array,y
skip_change_dir

  ; Update change_dir_count, only done by animal 1 (last one to be moved)
  tya
  cmp #1
  bne skip_update_count
  inc change_dir_count
  lda change_dir_count
  sec
  cmp #CHANGE_DIR_FREQ+1
  bne skip_update_count
  lda #0
  sta change_dir_count
skip_update_count

  ; Return the direction, whether changed or not
  lda dir_array,y
  rts

; Sprite Graphics
; Sprites are self-contained.  All data needed for the sprite is stored in a
; single data structure in ROM.  (No RAM variables needed.)

; Sprites are organized in two arrays as follows:
; Array 1 (sprite data)
; Height*2 (1 byte)
; Color of each line (height bytes)

; Array 2 (sprite graphics)
; $00 (Sprites are null-terminated so that final sta GRP0/1 turns off sprite)
; Sprite graphics (height bytes)

; Per usual Atari custom, the sprite, and by extension the color information,
; is reversed from how it appears on screen.

sprite_data
player_sprite_data
  .byte $14,$F2,$3F,$3F,$0F,$0F,$0F,$3F,$0F,$0F
  ; Elephant
  .byte $14,$06,$02,$02,$02,$02,$02,$02,$02,$04
  ; Zebra
  .byte $14,$0F,$00,$0F,$00,$0F,$00,$0F,$00,$0F
  ; Lion
  .byte $14,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$12
  ; Monkey
  .byte $14,$F0,$2A,$2A,$2A,$2A,$2E,$2E,$2A,$4E
  ; Crocodile
  .byte $0C,$DA,$DA,$DA,$DA,$DA
sprite
player_sprite
  .byte %00000000
  .byte %01100011
  .byte %00100010
  .byte %00010100
  .byte %00011100
  .byte %00001110
  .byte %00001101
  .byte %00001101
  .byte %00011110
  .byte %00001100
  ; Elephant
  .byte %00000000
  .byte %11001100
  .byte %11001100
  .byte %11001101
  .byte %11111101
  .byte %11111101
  .byte %11111101
  .byte %11111110
  .byte %01111100
  .byte %00111100
  ; Zebra
  .byte %00000000
  .byte %00011011
  .byte %00011011
  .byte %00011111
  .byte %00011111
  .byte %00100000
  .byte %11000000
  .byte %11000000
  .byte %01000000
  .byte %01000000
  ; Lion
  .byte %00000000
  .byte %01000101
  .byte %00100111
  .byte %00011110
  .byte %00011000
  .byte %01111000
  .byte %00011000
  .byte %11111000
  .byte %01111000
  .byte %00111000
  ; Monkey
  .byte %00000000
  .byte %00001010
  .byte %00011100
  .byte %00111101
  .byte %00111110
  .byte %01110000
  .byte %01110000
  .byte %00110000
  .byte %00110000
  .byte %00001000
  ; Crocodile
  .byte %00000000
  .byte %00010010
  .byte %11111111
  .byte %00011110
  .byte %00101000
  .byte %01000000

; Sound Data
; Each sound effect is a self-contained ROM data structure, described below:
; Byte 0: Length (number of notes)
; Byte 1: Number of loops
; Remaining bytes are the notes, 3 bytes each, as described:
; Byte 0: Channel
; Byte 1: Frequency
; Byte 2: Duration (frames)
sound_data
animal_sounds

  ; Elephant
  .byte $03,$01,$03,$01,$0A,$03,$00,$1E,$03,$01,$0A
  ; Zebra
  .byte $05,$01,$04,$1A,$0A,$04,$14,$0A,$04,$1A,$0A,$04,$14,$0A,$04,$1A,$0A
  ; Lion
  .byte $03,$01,$0A,$0B,$1E,$0A,$19,$14,$0A,$1B,$14
  ; Monkey
  .byte $03,$01,$03,$00,$1E,$01,$00,$1E,$03,$00,$1E
  ; Crocodile
  .byte $01,$01,$0E,$1F,$40
  

digit_sprites ; 8x4 bits for each digit. First column is all 0's for spacing)
              ; First row is all 0's for spacing and convenience (8 rows make
              ; for easier binary calculations and the blank row turns off
              ; playfield bits)
  .byte %00000000,%00000000,%00000000,%00000000,%00000000
  .byte %01110001,%01110111,%00010111,%01110001,%01110001
  .byte %01010001,%01000001,%00010001,%01010001,%01010001
  .byte %01010001,%01000001,%00010001,%01010001,%01010001
  .byte %01010001,%01110111,%01110111,%01110001,%01110111
  .byte %01010001,%00010001,%01010100,%01000001,%01010101
  .byte %01010001,%00010001,%01010100,%01000001,%01010101
  .byte %01110001,%01110111,%01010111,%01000111,%01110111
digit_sprites_reversed
  .byte %00000000,%00000000,%00000000,%00000000,%00000000
  .byte %11101000,%11101110,%10001110,%11101000,%11101000
  .byte %10101000,%00101000,%10001000,%10101000,%10101000
  .byte %10101000,%00101000,%10001000,%10101000,%10101000
  .byte %10101000,%11101110,%11101110,%11101000,%11101110
  .byte %10101000,%10001000,%10100010,%00101000,%10101010
  .byte %10101000,%10001000,%10100010,%00101000,%10101010
  .byte %11101000,%11101110,%10101110,%00101110,%11101110

animal_remove_mask ; Bitmask used to mark capture of animal
  .byte %11111111  ; Wasted byte, since animal indices start at 1
  .byte %11111110
  .byte %11111101
  .byte %11111011
  .byte %11110111
  .byte %11101111
  .byte %11011111
  .byte %10111111
  .byte %01111111
animal_is_loose_mask ; Bitmask used to test if animal has been captured
  .byte %11111111    ; Player is never "captured" if animals are loose
  .byte %00000001
  .byte %00000010
  .byte %00000100
  .byte %00001000
  .byte %00010000
  .byte %00100000
  .byte %01000000
  .byte %10000000
;-------------------------------------------------------------------------------
; R. Mundschau's horizontal positioning code.
; Positions an object horizontally
; Inputs: A = Desired position.
; X = Desired object to be positioned (0-5).
; scanlines: If control comes on or before cycle 73 then 1 scanline is consumed.
; If control comes after cycle 73 then 2 scanlines are consumed.
; Outputs: X = unchanged
; A = Fine Adjustment value.
; Y = the "remainder" of the division by 15 minus an additional 15.
; control is returned on cycle 6 of the next scanline.
;-------------------------------------------------------------------------------
horpos
  sta WSYNC
  sec
.divideby15
  sbc #15
  bcs .divideby15
  tay
  lda fineAdjustTable,y
  sta HMP0,x
  sta RESP0,x
  rts

; This table converts the "remainder" of the division by 15 (-1 to -15) to the
; correct fine adjustment value. This table is on a page boundary to guarantee
; the processor will cross a page boundary and waste a cycle in order to be at
; the precise position for a RESP0,x write

; JE:  Must be placed on a page boundary, so I've placed it at the start of the
; very last page.

  org $FF00

fineAdjustBegin
  DC.B %01110000; Left 7
  DC.B %01100000; Left 6
  DC.B %01010000; Left 5
  DC.B %01000000; Left 4
  DC.B %00110000; Left 3
  DC.B %00100000; Left 2
  DC.B %00010000; Left 1
  DC.B %00000000; No movement.
  DC.B %11110000; Right 1
  DC.B %11100000; Right 2
  DC.B %11010000; Right 3
  DC.B %11000000; Right 4
  DC.B %10110000; Right 5
  DC.B %10100000; Right 6
  DC.B %10010000; Right 7
fineAdjustTable EQU fineAdjustBegin - %11110001; NOTE: %11110001 = -15

  org $FFFA
  .word Reset
  .word Reset
  .word Reset
