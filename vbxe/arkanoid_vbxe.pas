
{ ----------------------------------------------------------------------- }
{                                                                         }
{                                ARKANOID                                 }
{                        Written by Claudio Bazzan                        }
{                  Fundamentals of Computer Science I Exam                }
{                        Professor Eduardo Calabrese                      }
{                   Revision v1.02 for the public domain                  }
{                                                                         }
{ Note: This program is a faithful reproduction (as far as possible)      }
{ of the famous barroom Coin-Op Arkanoid. The rights to the original game }
{ are held by Taito Corp, this version is intended only for demonstration }
{ purposes. It is therefore strictly forbidden to be sold or any other    }
{ profit-making purpose.                                                  }
{                                                                         }
{ The program runs on all machines with 80286 processor or higher.        }
{ It is recommended that you use an 80386 to apreciate a full             }
{ its qualities.                                                          }
{                                                                         }
{ And compilation with RANGE CHECKING enabled by is not recommended       }
{ since execution times are critical and a slight delay in the execution  }
{ of some points can cause a significant and annoying global slowdown.    }
{                                                                         }
{ Of course, the program runs smoothly even with the interval             }
{ check active.                                                           }
{                                                                         }
{ ----------------------------------------------------------------------- }

(*

 !!! kod programu nie może przekroczyć $A000 !!!

---------------------------------------
 Arkanoid VBXE v2.3 by Tebe/Madteam
---------------------------------------

 2026-05-25
 - enemies, 3 VBXE blits

 2026-05-20
 - enemies

 2026-05-17
 - ultra-fast-sqrt (sqrt16)

 2026-04-22
 - MAXSPEED = 1440, skalowanie SQRT (najpierw SHR 3, na koncu wynik SQRT16 SHL 3)
 - tablica SQRTABLE dzięki skalowaniu tylko 256 elementów

 2026-03-14
 - const [striped]
 - mul16

 2026-02-15
 - optymalizacje
 - laser_blast
 - TextOut

 2026-02-06
 - optymalizacje smallint -> byte absolute zpage (split_line)
 - mouseclick -> (trig0 = 0)

 2026-02-02
 - ball.speedx, ball.speedy always <> 0 (procedure set_ball_speed)
 - 'ball.speedy > 0' -> 'ball.speedy >= 0' shorter code

 2026-01-26
 - ball_hit_block optimizations ( nx < ox ; ny < oy)
 - moving variables to zero page

 2026-01-23
 - letter (typ, nexttype, last) smallint -> byte

 2025-10-15
 - mousecoords : joy_left_up, joy_left_down, joy_right_up, joy_right_down

 2025-09-21
 - ball_hit_block optimizations

 2025-09-16
 - adjw [0..3, 0..3] (ball_hit_block) compiler bugfix
 - more accurate collision detection

 2025-09-15
 - ball.brwhit additional reset
 - additional test deflect (ball_hit_block)

 2025-09-11
 - 'write_score' optimization
 - 'create_vaus' optimization

 2025-09-10
 - 'remove_block', 'plot_lives' fully accelerated by blitter
 - 'place_block' accelerated by blitter

*)


// 207240

// TO DO:
// C - z prawej strony przyklejona pilka leci w prawo, z lewej w lewo
// wieksze szanse na L, R


// service.pas   3102 (*16, *12)



program arkanoid;

uses crt, atari, vbxe, joystick, xsfx;

{$r arkanoid.rc}

//{$f $90}


{ ------------------------------------------------------------------------- }


type

   TSoundFX = (sfx_ball_bounce = 6, sfx_ball_brick = 7, sfx_letter_p = 2, sfx_vaus_destroyed = 11, sfx_check_letter = 4, sfx_solid_brick = 9,
               sfx_hard_brick = 8, sfx_shot_enemy = 4, sfx_fire = 5, sfx_vaus_enlarged = 3, sfx_vaus_teleport = 12);


   ENEMYTYPE= RECORD of $cb
              typ: cardinal;		{ address }
              x, y: byte;
	      adx, ady: byte;
	      adf: byte;		{ add frame }
	      hit,
	      ping,			{ ping_pong on/off }
	      fade: Boolean;		{ enter to playfield }
              tic: byte;		{ tick counter }
              frm: byte;		{ frame counter }
              mfrm: byte;		{ maximum frame number }
	      width,
	      fade_tmp,
	      fadein: word;		{ 16*width..0 }
	      END;

   BTMTYPE  = RECORD                   { for a drawing in BTM format }
              width   : word;          { drawing width }
              height  : byte;          { height }
              ofs : cardinal;
              END;

   VAUSTYPE = RECORD                   { for vaus data }
              x,y: byte;               { current x,y coordinates }

              oldx,                    { old vaus coordinates }
              oldy   : byte;
              oldlen : byte;           { and old length }
              width,                   { width }
              height : byte;           { thickness (or height) }
              flash  : byte;           { indicates the current color of the edges }
              iflash : byte;           { delay counter for}
                                       { the blinking of edges }
              letter : byte;
              END;


   BALLTYPE = RECORD of $00            { contains the data of the ball }
              x,y: byte;               { current x,y coordinates }
              finex,finey: byte;       { submultiples of the coordinates }
              oldx,oldy: byte;         { old coordinates }
              speed  : word;           { speed 256 = 70 pixels per sec. }
              finespeed : word;        { speed (submultiple) }
              speedx,                  { x-axis velocity }
              speedy : smallint;       { y-axis velocity }
              sbd    : word;           { to avoid ball loops }
              brwhit : byte;           { number of brown blocks affected in succession }
              inplay : boolean;        { flag, TRUE if the ball is in play }
              launch : boolean;        { flag, TRUE if the ball must be }
                                       { thrown again }
              onvaus : byte;           { width in pixels of the vaus }
              stm    : byte;           { magnet counter }
              END;

   WALLTYPE = array [0..16*16-1] of byte;//array[0..12,-1..15] of byte; { for the wall (13x15 bricks) }

   WHOLEWALLS = array[0..32] of WALLTYPE;  { for all 33 walls }

   SCORETYPE  = RECORD                            { keeps score }
                player : array[0..2] of cardinal; { player 1 and 2 }
                wall_n : array[0..2] of byte;     { current wall }
                lives  : array[0..2] of byte;     { remaining lives }
                hiscore: cardinal;                { record }
                pl_numb: byte;                    { current player }
                roundsel : array[0..2] of boolean;
                abortplay : boolean;
                END;

   SHREC      = RECORD                 { for the sparkle of the bricks }
                xb, yb      : byte;
                frame       : byte;
                block       : byte;
                active      : boolean;
                END;

   LETTERREC  = RECORD                 { data related to the letter }
                x,y      : byte;       { coord. }
                typ      : byte;       { Type, B,C,E,L,P,D,S }
                frame    : byte;       { frame number }
                subframe : byte;       { number of cycles per frame }
                active   : boolean;    { the letter can be active }
                incoming : word;       { holds the sum, >1000 the letter falls out }
                nextx,                 { Coordination of where it should fall if activated }
                nexty,
                nexttype,	       { type of letter that will have to fall }
                last     : byte;       { last letter dropped }
                END;

   FIRETYPE   = RECORD                 { for lasers }
                x,y  : byte;           { coord. }
                shot : boolean;        { if the shot went off }
                avl  : boolean;        { if it's available (thanks to L) }
                nw   : boolean;        { if he just left VAUS }
		blastx,
		blasty,
		blastfrm: byte;
		blast: Boolean;
                END;


{ ------------------------------------------------------------------------- }


const
	charset = $b000;

	VBXE_DIGIT = $5000;

        VBXE_DATA = $6000;

	playscreen_ofs  = $030000;

	vram = $040000;

	pattern_temp = $050000;


	explosion_ofs   = VBXE_DATA;
	shinewall_ofs   = VBXE_DATA + $0000319D;
	letters_ofs     = VBXE_DATA + $00003ECC;
	minivaus_ofs	= VBXE_DATA + $0000369D;

	shoots_ofs      = VBXE_DATA + $00005E4C;
	balldata_ofs    = VBXE_DATA + $00005EB4;

	presents_ofs = VBXE_DATA + 90*320;

	flux2_ofs = presents_ofs + 320*200;

	laserblast_ofs = flux2_ofs + 24*21;

	opengate_ofs = laserblast_ofs + 16*12;

	enemies_ofs0 = opengate_ofs + 32*48;
	enemies_ofs1 = enemies_ofs0 + 128*32;
	enemies_ofs2 = enemies_ofs1 + 384*32;
	enemies_ofs3 = enemies_ofs2 + 176*32;

	explossion0 = enemies_ofs3 + 160*32;
	explossion1 = explossion0 + 112*16;

	minivaus_width = 20;
	minivaus_height = 5;

	explosion_width = 42;

	shoots_width = 13;
	shoots_height = 8;

	laserblast_width = 16;
	laserblast_height = 6;

	opengate_width = 32;
	opengate_height = 8;


   CL_BLACK = 32;
   CL_WHITE = 84;

   err1 = 1; // 'Ball speed exceed program capability'
   err2 = 2; // 'Ball seems to be still'
   err3 = 3; // 'Ball hit a block not on its surface'
   err4 = 4; // 'No collisions detected'


   SCRMIN     = 10;  { X coordinate of the left edge of the playing area  }
   SCRMAX     = 216; { X coordinate of the right edge of the playing area }
   SCRTOP     = 12;  { Y coordinate of the upper edge of the playing area }
   SCRBOT     = 200; { Y coordinate of the lower edge of the playing area }

   VAUS_W     = 34;  { Width of the VAUS in pixels                        }
   VAUS_H     = 4;   { Height of the VAUS in pixels                       }
   VAUS_LINE  = 184; { Y coordinate on which the VAUS moves horizontally  }
   EMP        = 255; { Use EMP (empty) instead of 255                     }
   BALLDIM    = 5;   { Diameter of the ball (in pixels)                   }
   BALLSPOT   = 3;   { Radius of the ball (in pixels) = diameter/2 +1     }

(*
   BALLARRAY  : packed array[0..4,0..4] of byte =
                                           ((0,1,1,1,0),
                                            (1,1,2,1,1),
                                            (1,2,1,1,1),
                                            (1,1,1,1,1),
                                            (0,1,1,1,0));
                                            { Ball design }

*)

   BALLDEV    = 30; { Angle of deviation when hitting }
                    { the red edges of the VAUS       }

   SPEEDFLASH = 10; { Number of 50ths of a second to wait before changing }
                    { the color of the VAUS borders                       }

   FLASH      : array[0..10] of byte = ( 255,212,211,210,209, 208,207,206,205,204,203);
		    { Colors that the extremes of the VAUS take on during flashing }

   [striped] SCORE_WALL : array[0..10] of word = (0, 10,20,30,40,50,100,200,250,500,1000 );


   EMERG_DEV  : array[0..8] of byte = (0, $02,$13,$24,$35,$12,$23,$34,$45 );


   COLORBLOCK : array[0..9] of byte = ( 212,211,210,209,208, 207,206,205,204,203 );
                                         { Color of the bricks }

   MAXENEMYY  = 250;

   GRAYDOWN   = 1;   { Number of strokes-1 to knock down a gray brick }
   STARTWALL  = 1;   { Starting level }
   BALLSPEED  = 587; { Ball speed (256 = 70 pixels per second }
   MAXSPEED   = 1440;{ Maximum permitted ball speed }
   MAXBRWHIT  = 100; { Maximum number of indistr. blocks it can hit }
                     { before splashing off changing speed          }

   PATNUMBER  = 4;   { Number of available backdrops }

   POS_DIGIT  : array[0..3] of byte = (0, 60,93,128);
   { Y coordinate of the three scores (player 1, player 2, hiscore) }


//   DIGITS     : array[0..10] of byte = ( 125,96,55,103,106,79, 95,97,127,111,0 );
   { Data for displaying digital digits in scores }

   [striped] LEVEL      : array[0..5] of word = (0, 1000,300,100,60,35);

   SBDIR      = 600; { Cycles it must do before the ball you have to (dev. adjust) }
   DEFLEVEL   = 3;   { Default game level }

   LETTER_PROB= 300; { range in which the random number of the letter is drawn }
   LETTER_DROP= 1000;{ Number that must reach the sum to drop the letter }
//   LETTER_NUMB= 8;   { number of letters+1 }
   LETTER_FRM = 8;   { Number of frames that constitute the animation of the letter }
   LETTER_SBF = 5;   { Number of cycles it must complete before moving to the next frame }

   { Probability of letter drop in % }   {  L   E   B   D   S   C  P }
   LETTER_DIS : array[0..7] of byte = (  0, 16, 20, 3, 18, 20, 20, 5 );

   [striped] MUL16: array of word = [ {$eval 48,":1*16"} ] ;

   FLUXLEVEL  = 177;

	sfx1: array of byte =
	[
	$01,$00,$00,			// ball bounces at the brick two hits
	$01,$13,$AF,
	$01,$12,$AD,
	$01,$12,$A7,
	$01,$13,$A5,
	$01,$12,$A5,
	$01,$12,$A4,
	$01,$12,$A3,
	$01,$12,$A1,
	$01,$12,$A0,
	$01,$12,$A1,
	$00,$FF
	];


	sfx2: array of byte =		// additional life (bonus P)
	[
	$01,$00,$00,
	$02,$2F,$AC,
	$02,$33,$AC,
	$03,$2F,$AE,
	$03,$33,$AE,
	$00,$FF
	];


	sfx3: array of byte =		// vaus expand width
	[
	$01,$00,$00,
	$03,$FF,$AC,
	$03,$F5,$AD,
	$02,$EB,$AE,
	$02,$E1,$AF,
	$00,$FF
	];


	sfx4: array of byte =		// enemy explode
	[
	$01,$00,$00,
	$04,$1E,$8A,
	$01,$20,$88,
	$01,$24,$85,
	$01,$2C,$84,
	$00,$FF
	];


	sfx5: array of byte =		// shot the fire
	[
	$01,$00,$00,
	$01,$01,$27,
	$01,$02,$29,
	$01,$04,$2A,
	$01,$05,$28,
	$01,$04,$25,
	$00,$FF
	];


	sfx6: array [0..34] of byte =	// Arkanoid ball bounces
	(
	$01,$00,$00,			// 1 frame AUDF, AUDC
	$01,$3C,$AF,
	$01,$3B,$AD,
	$01,$3B,$A7,
	$01,$3C,$A5,
	$01,$3B,$A5,
	$01,$3C,$A4,
	$01,$3C,$A3,
	$01,$3C,$A1,
	$01,$3C,$A0,
	$01,$3C,$A1,
	$00,$FF				// $00,$FF end
	);


	sfx7: array [0..34] of byte =	// Arkanoid ball bounces at the brick
	(
	$01,$00,$00,			// 1 frame AUDF, AUDC
	$01,$32,$AF,
	$01,$31,$AD,
	$01,$31,$A7,
	$01,$32,$A5,
	$01,$31,$A5,
	$01,$32,$A4,
	$01,$32,$A3,
	$01,$32,$A1,
	$01,$32,$A0,
	$01,$32,$A1,
	$00,$FF
	);


	sfx8: array [0..34] of byte =	// Arkanoid ball bounces at the hard brick
	(
	$01,$00,$00,			// 1 frame AUDF, AUDC
	$01,$19,$AF,
	$01,$18,$AD,
	$01,$18,$A7,
	$01,$19,$A5,
	$01,$18,$A5,
	$01,$19,$A4,
	$01,$19,$A3,
	$01,$19,$A1,
	$01,$19,$A0,
	$01,$19,$A1,
	$00,$FF
	);

	sfx9: array [0..34] of byte =	// Arkanoid ball bounces at the hard brick
	(
	$01,$00,$00,
	$01,$13,$AF,
	$01,$12,$AD,
	$01,$12,$A7,
	$01,$13,$A5,
	$01,$12,$A5,
	$01,$12,$A4,
	$01,$12,$A3,
	$01,$12,$A1,
	$01,$12,$A0,
	$01,$12,$A1,
	$00,$FF
	);


	sfx10: array [0..160] of byte =	// Arkanoid DOH destroyed
	(
	$01,$00,$00,
	$03,$03,$22,
	$03,$05,$22,
	$03,$04,$22,
	$03,$03,$22,
	$03,$03,$24,
	$03,$05,$24,
	$03,$04,$24,
	$03,$03,$24,
	$03,$03,$26,
	$03,$05,$26,
	$03,$04,$26,
	$03,$03,$26,
	$03,$03,$28,
	$03,$05,$28,
	$03,$04,$28,
	$03,$03,$28,
	$03,$03,$2A,
	$03,$05,$2A,
	$03,$04,$2A,
	$03,$03,$2A,
	$03,$03,$2C,
	$03,$05,$2C,
	$03,$04,$2C,
	$03,$03,$2C,
	$03,$03,$2E,
	$03,$05,$2E,
	$03,$04,$2E,
	$03,$03,$2E,
	$03,$03,$2C,
	$03,$05,$2C,
	$03,$04,$2C,
	$03,$03,$2C,
	$03,$03,$2A,
	$03,$05,$2A,
	$03,$04,$2A,
	$03,$03,$2A,
	$03,$03,$28,
	$03,$05,$28,
	$03,$04,$28,
	$03,$03,$28,
	$03,$03,$26,
	$03,$05,$26,
	$03,$04,$26,
	$03,$03,$26,
	$03,$03,$24,
	$03,$05,$24,
	$03,$04,$24,
	$03,$03,$24,
	$03,$03,$22,
	$03,$05,$22,
	$03,$04,$22,
	$03,$03,$22,
	$00,$FF
	);

	sfx11: array [0..46] of byte =	// Arkanoid VAUS destroyed
	(
	$03,$14,$29,			// 3 frame AUDF, AUDC
	$04,$10,$2C,			// 4 frame AUDF, AUDC
	$02,$12,$2B,
	$03,$14,$27,
	$04,$10,$2A,
	$02,$12,$29,
	$03,$14,$25,
	$04,$10,$28,
	$02,$12,$27,
	$03,$14,$23,
	$04,$10,$26,
	$02,$12,$25,
	$03,$14,$22,
	$04,$10,$25,
	$02,$12,$24,
	$00,$FF
	);

	sfx12: array [0..25] of byte =	// Arkanoid VAUS walks through the door to next round
	(
	$01,$00,$00,			// 1 frame AUDF, AUDC
	$07,$1F,$2F,			// 7 frame AUDF, AUDC
	$07,$21,$2D,
	$07,$23,$2B,
	$07,$25,$29,
	$06,$27,$27,
	$06,$29,$25,
	$06,$2B,$23,
	$00,$FF
	);


{ ------------------------------------------------------------------------- }


var

    blt        : TBCB absolute VBXE_BCBADR+VBXE_WINDOW;
    blt_letter : TBCB absolute VBXE_BCBADR+VBXE_WINDOW+21;
    blt_box    : TBCB absolute VBXE_BCBADR+VBXE_WINDOW+21*2;
    blt_zero   : TBCB absolute VBXE_BCBADR+VBXE_WINDOW+21*3;

    enemy0     : TBCB absolute VBXE_BCBADR+VBXE_WINDOW+21*4;
    enemy1     : TBCB absolute VBXE_BCBADR+VBXE_WINDOW+21*5;
    enemy2     : TBCB absolute VBXE_BCBADR+VBXE_WINDOW+21*6;

    vbxe_ram: TVBXEMemoryStream;

    sfx: TSFX;

    balldata   : BTMTYPE;
    playscreen : BTMTYPE;  { area di gioco (320x200) }
    playvaus   : BTMTYPE;  { vaus }
    normal     : BTMTYPE;  { vaus normale }
    enlarged   : BTMTYPE;  { allargato }
    lasers     : BTMTYPE;  { traformato per sparare }
    explosion  : BTMTYPE;  { esplosione vaus }
    newvaus    : BTMTYPE;  { sequenza di animazione di partenza }
    presents   : BTMTYPE;  { scritta ARKANOID }
//    soundfx    : BTMTYPE;  { l'icona con la nota e la nota sbarrata }
    shinewall  : BTMTYPE;  { luccichio dei mattoncini grigi e beige }
    minivaus   : BTMTYPE;  { vaus piccolino che indica le vite }
    levelsel   : BTMTYPE;  { 5 frames dei numeri per scegliere il livello }
    letters    : BTMTYPE;  { le animazioni delle 7 lettere }
    shoots     : BTMTYPE;  { e il disegno dei laser }
//    flux       : BTMTYPE;
    vaus       : VAUSTYPE; { data relating to the VAUS (see above) }
    pattern    : BTMTYPE;  { background }

    pattern0   : BTMTYPE;  { background }
    pattern1   : BTMTYPE;  { background }
    pattern2   : BTMTYPE;  { background }
    pattern3   : BTMTYPE;  { background }
    pattern4   : BTMTYPE;  { background }

    status     : byte;

//    success    : boolean;                 { status flag for BTM loading }

    remain_blk : byte;                    { bricks still to be knocked down }
    totalwall  : byte;                    { bricks throughout }
    score      : SCORETYPE;               { current score }
    cur_player : byte;                    { current player }

    shinerec   : shrec;                   { holds the data of the block }
                                          { that is currently flashing }

    lv         : smallint;                { level of play }
    trainer    : byte;

    lett       : LETTERREC;               { the parameters of the letters }
    fire       : FIRETYPE;                { and laser beams }
    balls_in_play : byte;                 { number of balls in play }
    scrflux    : boolean;
    scrfluxcnt : byte;

    sound_on   : Boolean;

    old_scores : cardinal;
    enemies_adr: cardinal;

    hlp: word;

    scr: array [0..255] of byte absolute VBXE_WINDOW+$0200;
    pom: array [0..127] of byte absolute VBXE_WINDOW+$0280;
    //pat: array [0..2047] of byte absolute VBXE_WINDOW+$0300;

    [striped] sqrtable : array [0..255] of word absolute $a000;


    { !!! the $B000..$BFFF area is occupied by the VBXE window !!! }


    [striped] row : array[0..255] of WORD absolute $c000; { array (see initRowArray) }

    Mod10Table: array [0..255] of byte absolute $c000+$200;
    Mod90Table: array [0..255] of byte absolute $c000+$300;

    [striped] Mod360Table: array [0..255] of WORD absolute $c000+$400;

    atan_tab : array [0..255] of byte absolute $c000+$600;
    log2_tab : array [0..255] of byte absolute $c000+$700;

    [striped] scale360 : array [0..255] of WORD absolute $c000+$800;

    [striped] sintable: array [0..90-1] of smallint absolute $c000+$a00;

    enm0: ENEMYTYPE absolute $cb00;
    enm1: ENEMYTYPE absolute $cb00 + sizeof(ENEMYTYPE);
    enm2: ENEMYTYPE absolute $cb00 + sizeof(ENEMYTYPE)*2;


    wall_p : array[0..2] of WALLTYPE absolute $d800;   { memorization of the wall itself }
    wall       : WALLTYPE absolute $d800+$300;         { wall }

    [striped] mul90_16: array [0..15] of WORD absolute $d800+$400;

    all_walls  : WHOLEWALLS absolute $d800+$500;       { all the walls }


{ ------------------------------------------------------------------------- }


procedure nmi; interrupt; assembler;
asm

 sta nmist

 sta regA
 stx regX
 sty regY

	lda SFX
	ldy SFX+1
	jsr XSFX.TSFX.PLAY

 lda regA: #$00
 ldx regX: #$00
 ldy regY: #$00

end;


{ ------------------------------------------------------------------------- }

procedure start_level;
begin


end;


function rand(range: smallint): smallint; assembler;	// range 0..1023
asm
	ldy range+1
	bpl @+

	lda #0
	sub range
	sta range
	lda #0
	sbc range+1
	sta range+1
@
	lda random
	adc seed_l: #$00
	sta Result
	sta seed_l
	lda random
	adc seed_h: #$00
	sta seed_h
	and #%00111000
	lsr @
	lsr @
	lsr @
	sta Result+1

loop	cpw Result range
	bcc @+

	lsr Result+1
	ror Result

	jmp loop
@
	tya
	bpl @exit

	lda #0
	sub Result
	sta Result
	lda #0
	sbc Result+1
	sta Result+1
end;


function mod90(a: smallint): smallint; assembler;
asm
    ldy a+1
    bpl @+

    lda #0
    sub a
    sta a
    lda #0
    sbc a+1
    tay
@
    LDA adr.Mod90Table,Y	; tablica: (X*256) mod 90
    CLC
    ADC a			; dodaj młodszy bajt
    sta Result
    lda #0
    adc #0
    sta Result+1

    cpw Result #90
    BCC done
    SBW Result #90

    cpw Result #90
    BCC done
    SBW Result #90

    cpw Result #90
    BCC done
    SBW Result #90

done

    lda a+1
    bpl @exit

    lda #0
    sub Result
    sta Result
    lda #0
    sbc Result+1
    sta Result+1
end;


function mod360(a: smallint): smallint; assembler;
asm
    ldy a+1
    bpl @+

    lda #0
    sub a
    sta a
    lda #0
    sbc a+1
    tay
@
    LDA adr.Mod360Table,Y	; tablica: (X*256) mod 360
    CLC
    ADC a			; dodaj młodszy bajt
    sta Result
    lda adr.Mod360Table+256,Y
    adc #0
    sta Result+1

    cpw Result #360
    BCC done
    SBW Result #360

    cpw Result #360
    BCC done
    SBW Result #360

    cpw Result #360
    BCC done
    SBW Result #360

done

    lda a+1
    bpl @exit

    lda #0
    sub Result
    sta Result
    lda #0
    sbc Result+1
    sta Result+1
end;


function SinDeg(a: smallint): SmallInt; register;
begin

  a := mod360(a);

  if a < 0 then
    a := a + 360;

  if a <= 90 then
    Result := sintable[a]
  else if a <= 180 then
    Result := sintable[180 - a]
  else if a <= 270 then
    Result := -sintable[a - 180]
  else
    Result := -sintable[360 - a];

end;


function sqrt16(a: word): word; assembler; register;
asm
; http://www.txbobsc.com/aal/1986/aal8611.html#a1

; SAVE S.PUTNEY-RBSC FISQR
; --------------------------------
;       ULTRA FAST INTEGER SQUARE ROOTS
;
;   BY: CHARLES H. PUTNEY
;       18 QUINNS ROAD
;       SHANKILL, CO. DUBLIN, IRELAND
;
;  INPUT: X = ARG HIGH BYTE
;         A = ARG LOW BYTE
;
;  OUTPUT: Y=INTEGER SQUARE ROOT OF X,A
;          X AND A DESTROYED
;
; --------------------------------

;BAS_ARG    =  $02           ; ,01
;NUMBER     =  $04           ; ,03

ARGSAV     =  :EAX           ; ,05
ARGLO      =  :EAX+2


	stx @sp+1

	ldx a+1
	lda a
	jsr SQRT

	sty Result
	lda #0

	asl Result		; *8 (scale factor)
	rol @
	asl Result
	rol @
	asl Result
	rol @

	sta Result+1

@sp	ldx #$00
	jmp @exit


SQRT
       cpx #$40             ; VALUE ALREADY NORMALIZED?
       bcc l2               ; ...NO

; ---ARG = $4000...FFFF-----------49152 CASES
       cpx #$FF             ; CHECK FOR ARG-HI = $FF
       beq l9               ; ...YES, SPECIAL CASE
       ldy ROOT,X           ; GET ROOT, USE AS INDEX
       cmp TABLE2,Y
       bcc l1               ; ...SPEEDS UP AVERAGE BY 0.8 CYCLE
       txa                  ; ARG-HI
       sbc TABLE3,Y
       bcc l1
       iny
l1     rts

; ---ARG = $0000...3FFF-----------
l2     stx ARGSAV+1         ; SAVE ARG-HI
       cpx #0               ; IS ARG-HI ZERO?
       beq l7               ; ...YES

; ---ARG = $01FF...3FFF-----------16128 CASES
       sta ARGSAV           ; SAVE ARG-LO FOR SHIFTING
       sta ARGLO            ; SAVE ARG-LO FOR LATER COMPARE
       txa                  ; ARG-HI TO A-REG
       ldy #0               ; START SHIFT COUNT = 0
l3     asl ARGLO
       rol
       asl ARGLO
       rol
       iny
       cmp #$40
       bcc l3

; ---A=NORM-ARG, Y=SHIFT-CNT------
l4     tax                  ; USE NORM-ARG FOR INDEX
       lda ROOT,X           ; GET ROOT FROM TABLE
l5     lsr                  ; HALF ROOT SHIFT-CNT TIMES
       dey
       bne l5
       tay                  ; USE SHIFTED ROOT FOR INDEX NOW
       lda ARGSAV           ; GET ARG-LO
       cmp TABLE2,Y
       bcc l6               ; ...SPEEDS UP AVERAGE BY 0.7 CYCLE
       lda ARGSAV+1
       sbc TABLE3,Y
       bcc l6
       iny
l6     rts

; ---ARG = $0000...00FF-----------
l7     tay                  ; IS ARG-LO ALSO ZERO?
       beq l1               ; ...YES, SQRT=0

; ---ARG = $0001...00FF-----------255 CASES
       sta ARGSAV           ; SAVE ARG-LO FOR LATER COMPARE
       ldy #4               ; START SHIFT COUNT = 4
l8     cmp #$40             ; NORMALIZED YET?
       bcs l4               ; ...YES, GET ROOT NOW
       asl
       asl
       iny                  ; COUNT THE SHIFT
       bne l8               ; ...ALWAYS

; ---ARG = $FFXX------------------
l9     ldy #$FF
       rts

;       SQUARE ROOT TABLE OF N
;       FROM $4000 (16384)
;       TO $FF00   (65280)
;       BY $100    (256)
TABLE1 .byte $80 ,$80 ,$81 ,$82 ,$83 ,$84 ,$85 ,$86
       .byte $87 ,$88 ,$89 ,$8A ,$8B ,$8C ,$8D ,$8E
       .byte $8F ,$90 ,$90 ,$91 ,$92 ,$93 ,$94 ,$95
       .byte $96 ,$96 ,$97 ,$98 ,$99 ,$9A ,$9B ,$9B
       .byte $9C ,$9D ,$9E ,$9F ,$A0 ,$A0 ,$A1 ,$A2
       .byte $A3 ,$A3 ,$A4 ,$A5 ,$A6 ,$A7 ,$A7 ,$A8
       .byte $A9 ,$AA ,$AA ,$AB ,$AC ,$AD ,$AD ,$AE
       .byte $AF ,$B0 ,$B0 ,$B1 ,$B2 ,$B2 ,$B3 ,$B4
       .byte $B5 ,$B5 ,$B6 ,$B7 ,$B7 ,$B8 ,$B9 ,$B9
       .byte $BA ,$BB ,$BB ,$BC ,$BD ,$BD ,$BE ,$BF
       .byte $C0 ,$C0 ,$C1 ,$C1 ,$C2 ,$C3 ,$C3 ,$C4
       .byte $C5 ,$C5 ,$C6 ,$C7 ,$C7 ,$C8 ,$C9 ,$C9
       .byte $CA ,$CB ,$CB ,$CC ,$CC ,$CD ,$CE ,$CE
       .byte $CF ,$D0 ,$D0 ,$D1 ,$D1 ,$D2 ,$D3 ,$D3
       .byte $D4 ,$D4 ,$D5 ,$D6 ,$D6 ,$D7 ,$D7 ,$D8
       .byte $D9 ,$D9 ,$DA ,$DA ,$DB ,$DB ,$DC ,$DD
       .byte $DD ,$DE ,$DE ,$DF ,$E0 ,$E0 ,$E1 ,$E1
       .byte $E2 ,$E2 ,$E3 ,$E3 ,$E4 ,$E5 ,$E5 ,$E6
       .byte $E6 ,$E7 ,$E7 ,$E8 ,$E8 ,$E9 ,$EA ,$EA
       .byte $EB ,$EB ,$EC ,$EC ,$ED ,$ED ,$EE ,$EE
       .byte $EF ,$F0 ,$F0 ,$F1 ,$F1 ,$F2 ,$F2 ,$F3
       .byte $F3 ,$F4 ,$F4 ,$F5 ,$F5 ,$F6 ,$F6 ,$F7
       .byte $F7 ,$F8 ,$F8 ,$F9 ,$F9 ,$FA ,$FA ,$FB
       .byte $FB ,$FC ,$FC ,$FD ,$FD ,$FE ,$FE ,$FF

;       SQUARE TABLE CONTAINING LOW BYTE OF (N+1)
TABLE2 .byte $01 ,$04 ,$09 ,$10 ,$19 ,$24 ,$31 ,$40
       .byte $51 ,$64 ,$79 ,$90 ,$A9 ,$C4 ,$E1 ,$00
       .byte $21 ,$44 ,$69 ,$90 ,$B9 ,$E4 ,$11 ,$40
       .byte $71 ,$A4 ,$D9 ,$10 ,$49 ,$84 ,$C1 ,$00
       .byte $41 ,$84 ,$C9 ,$10 ,$59 ,$A4 ,$F1 ,$40
       .byte $91 ,$E4 ,$39 ,$90 ,$E9 ,$44 ,$A1 ,$00
       .byte $61 ,$C4 ,$29 ,$90 ,$F9 ,$64 ,$D1 ,$40
       .byte $B1 ,$24 ,$99 ,$10 ,$89 ,$04 ,$81 ,$00
       .byte $81 ,$04 ,$89 ,$10 ,$99 ,$24 ,$B1 ,$40
       .byte $D1 ,$64 ,$F9 ,$90 ,$29 ,$C4 ,$61 ,$00
       .byte $A1 ,$44 ,$E9 ,$90 ,$39 ,$E4 ,$91 ,$40
       .byte $F1 ,$A4 ,$59 ,$10 ,$C9 ,$84 ,$41 ,$00
       .byte $C1 ,$84 ,$49 ,$10 ,$D9 ,$A4 ,$71 ,$40
       .byte $11 ,$E4 ,$B9 ,$90 ,$69 ,$44 ,$21 ,$00
       .byte $E1 ,$C4 ,$A9 ,$90 ,$79 ,$64 ,$51 ,$40
       .byte $31 ,$24 ,$19 ,$10 ,$09 ,$04 ,$01 ,$00
       .byte $01 ,$04 ,$09 ,$10 ,$19 ,$24 ,$31 ,$40
       .byte $51 ,$64 ,$79 ,$90 ,$A9 ,$C4 ,$E1 ,$00
       .byte $21 ,$44 ,$69 ,$90 ,$B9 ,$E4 ,$11 ,$40
       .byte $71 ,$A4 ,$D9 ,$10 ,$49 ,$84 ,$C1 ,$00
       .byte $41 ,$84 ,$C9 ,$10 ,$59 ,$A4 ,$F1 ,$40
       .byte $91 ,$E4 ,$39 ,$90 ,$E9 ,$44 ,$A1 ,$00
       .byte $61 ,$C4 ,$29 ,$90 ,$F9 ,$64 ,$D1 ,$40
       .byte $B1 ,$24 ,$99 ,$10 ,$89 ,$04 ,$81 ,$00
       .byte $81 ,$04 ,$89 ,$10 ,$99 ,$24 ,$B1 ,$40
       .byte $D1 ,$64 ,$F9 ,$90 ,$29 ,$C4 ,$61 ,$00
       .byte $A1 ,$44 ,$E9 ,$90 ,$39 ,$E4 ,$91 ,$40
       .byte $F1 ,$A4 ,$59 ,$10 ,$C9 ,$84 ,$41 ,$00
       .byte $C1 ,$84 ,$49 ,$10 ,$D9 ,$A4 ,$71 ,$40
       .byte $11 ,$E4 ,$B9 ,$90 ,$69 ,$44 ,$21 ,$00
       .byte $E1 ,$C4 ,$A9 ,$90 ,$79 ,$64 ,$51 ,$40
       .byte $31 ,$24 ,$19 ,$10 ,$09 ,$04 ,$01 ,$00

;       SQUARE TABLE CONTAINING HIGH BYTE OF (N+1)
TABLE3 .byte $00 ,$00 ,$00 ,$00 ,$00 ,$00 ,$00 ,$00
       .byte $00 ,$00 ,$00 ,$00 ,$00 ,$00 ,$00 ,$01
       .byte $01 ,$01 ,$01 ,$01 ,$01 ,$01 ,$02 ,$02
       .byte $02 ,$02 ,$02 ,$03 ,$03 ,$03 ,$03 ,$04
       .byte $04 ,$04 ,$04 ,$05 ,$05 ,$05 ,$05 ,$06
       .byte $06 ,$06 ,$07 ,$07 ,$07 ,$08 ,$08 ,$09
       .byte $09 ,$09 ,$0A ,$0A ,$0A ,$0B ,$0B ,$0C
       .byte $0C ,$0D ,$0D ,$0E ,$0E ,$0F ,$0F ,$10
       .byte $10 ,$11 ,$11 ,$12 ,$12 ,$13 ,$13 ,$14
       .byte $14 ,$15 ,$15 ,$16 ,$17 ,$17 ,$18 ,$19
       .byte $19 ,$1A ,$1A ,$1B ,$1C ,$1C ,$1D ,$1E
       .byte $1E ,$1F ,$20 ,$21 ,$21 ,$22 ,$23 ,$24
       .byte $24 ,$25 ,$26 ,$27 ,$27 ,$28 ,$29 ,$2A
       .byte $2B ,$2B ,$2C ,$2D ,$2E ,$2F ,$30 ,$31
       .byte $31 ,$32 ,$33 ,$34 ,$35 ,$36 ,$37 ,$38
       .byte $39 ,$3A ,$3B ,$3C ,$3D ,$3E ,$3F ,$40
       .byte $41 ,$42 ,$43 ,$44 ,$45 ,$46 ,$47 ,$48
       .byte $49 ,$4A ,$4B ,$4C ,$4D ,$4E ,$4F ,$51
       .byte $52 ,$53 ,$54 ,$55 ,$56 ,$57 ,$59 ,$5A
       .byte $5B ,$5C ,$5D ,$5F ,$60 ,$61 ,$62 ,$64
       .byte $65 ,$66 ,$67 ,$69 ,$6A ,$6B ,$6C ,$6E
       .byte $6F ,$70 ,$72 ,$73 ,$74 ,$76 ,$77 ,$79
       .byte $7A ,$7B ,$7D ,$7E ,$7F ,$81 ,$82 ,$84
       .byte $85 ,$87 ,$88 ,$8A ,$8B ,$8D ,$8E ,$90
       .byte $91 ,$93 ,$94 ,$96 ,$97 ,$99 ,$9A ,$9C
       .byte $9D ,$9F ,$A0 ,$A2 ,$A4 ,$A5 ,$A7 ,$A9
       .byte $AA ,$AC ,$AD ,$AF ,$B1 ,$B2 ,$B4 ,$B6
       .byte $B7 ,$B9 ,$BB ,$BD ,$BE ,$C0 ,$C2 ,$C4
       .byte $C5 ,$C7 ,$C9 ,$CB ,$CC ,$CE ,$D0 ,$D2
       .byte $D4 ,$D5 ,$D7 ,$D9 ,$DB ,$DD ,$DF ,$E1
       .byte $E2 ,$E4 ,$E6 ,$E8 ,$EA ,$EC ,$EE ,$F0
       .byte $F2 ,$F4 ,$F6 ,$F8 ,$FA ,$FC ,$FE ,$00

ROOT   =  TABLE1-$40    ; SET UP SO $80 IS FIRST SQUARE ROOT
EXACTL =  TABLE2        ; SET UP SO 0 INDEX        (OF $4000)
EXACTH =  TABLE3        ; GIVES EXACT SQUARE OF 1

end;


(*
function Q0(x, y: byte): byte;
var
  lx, ly: byte;

begin

  lx := log2_tab[x];   // 32*log2(x)
  ly := log2_tab[y];   // 32*log2(y)

  if lx >= ly then
   Result := (-atan_tab[byte(lx - ly)]) and $3f
  else
   Result := atan_tab[byte(ly - lx)];

end;


function Atan2(y, x: smallint): byte;
{
; https://www.msx.org/forum/msx-talk/development/8-bit-atan2?page=0
; 8-bit atan2

; Calculate the angle, in a 256-degree circle.
; The trick is to use logarithmic division to get the y/x ratio and
; integrate the power function into the atan table.

;	input
; 	B = x, C = y	in -128,127
;
;	output
;	A = angle	in 0-255

;      |
;  q1  |  q0
;------+-------
;  q3  |  q2
;      |
}
var
  a, e: byte;

  sx: word register;
  sy: word register;
begin

  if x < 0 then
   sx := -x
  else
   sx := x;

  if y < 0 then
   sy := -y
  else
   sy := y;


  while (sx > 127) or (sy > 127) do
  begin
    sx := sx shr 1;
    sy := sy shr 1;
  end;

  if x < 0 then
   x := -sx
  else
   x := sx;


  if y < 0 then
   y := -sy
  else
   y := sy;


  e:=0;

  // test znaków
  if (y < 0) then e:=e or 2;
  if (x < 0) then e:=e or 1;


  if e = 1 then
  begin
    // Q1
    //x := -x;
    a := Q0(-x, y); // wywołanie podstawowego bloku
    a := -a;
    a := a and $7F;
    Exit(a);
  end;

  if e = 2 then
  begin
    // Q2
    //y := -y;
    a := Q0(x, -y);
    a := -a;
    Exit(a);
  end;

  if e = 3 then
  begin
    // Q3
    //x := -x;
    //y := -y;
    a := Q0(-x, -y);
    a := a + 128;
    Exit(a);
  end;

  // Q0 – główna część algorytmu
  Result := Q0(x, y);
end;
*)



function Atan2(y,x: smallint): byte; register;
(*
@description:
* Calculate the angle, in a 256-degree circle, between two points.
* The trick is to use logarithmic division to get the y/x ratio and
* integrate the power function into the atan table. Some branching is
* avoided by using a table to adjust for the octants.
* In otherwords nothing new or particularily clever but nevertheless
* quite useful.
*
* by Johan Forslöf (doynax)


@return: Result - byte
*)
const

octant_adjust : array [0..7] of byte = (
	%00111111,		// x+,y+,|x|>|y|
	%00000000,		// x+,y+,|x|<|y|
	%11000000,		// x+,y-,|x|>|y|
	%11111111,		// x+,y-,|x|<|y|
	%01000000,		// x-,y+,|x|>|y|
	%01111111,		// x-,y+,|x|<|y|
	%10111111,		// x-,y-,|x|>|y|
	%10000000		// x-,y-,|x|<|y|
	);

var
  sx: word register;
  sy: word register;

begin

  if x < 0 then
   sx := -x
  else
   sx := x;

  if y < 0 then
   sy := -y
  else
   sy := y;


  while (sx > 127) or (sy > 127) do
  begin
    sx := sx shr 1;
    sy := sy shr 1;
  end;

  if x < 0 then
   x := -sx
  else
   x := sx;


  if y < 0 then
   y := -sy
  else
   y := sy;


asm
	txa:pha

octant	= :TMP			// temporary zeropage variable

	lda #$00
	sta octant

atan2		clc
		lda x
		bpl @+

		eor #$ff
		sec
@
		tax
		rol octant

		clc
		lda y
		bpl @+

		sec
		eor #$ff
@
		tay
		rol octant

		lda adr.log2_tab,x
		sbc adr.log2_tab,y
		scc
		eor #$ff
		tax

		lda octant
		rol @
		and #%111
		tay

		lda adr.atan_tab,x
		eor adr.octant_adjust,y

		sta Result

	pla:tax
end;

end;


{$i ..\service.pas}



procedure init_game;
begin

   NoSound;

   randomize;

   sfx.clear;

   sfx.add(@sfx1);
   sfx.add(@sfx2);
   sfx.add(@sfx3);
   sfx.add(@sfx4);
   sfx.add(@sfx5);
   sfx.add(@sfx6);
   sfx.add(@sfx7);
   sfx.add(@sfx8);
   sfx.add(@sfx9);
   sfx.add(@sfx10);
   sfx.add(@sfx11);
   sfx.add(@sfx12);

   sfx.play;

   mem[756] := hi(charset);

   vbxe.VideoRAM := vram;	// VBXE video ram address

   initVGA;       { Activates 320x200x256 col. graphics mode. }
   initRowArray;   { Initializes a useful array to avoid multiplications }
                   { by 320. }


   {$i btm.inc}


   totalwall:=32;

   score.hiscore:=50000;
   { the record score is initially set to 50000 by default }

   sound_on:=TRUE;      { by default at the beginning the sound is ON }
   lv:=DEFLEVEL;        { and the level is set to DEFLEVEL            }

   repeat

      { mainscreen returns 1,2 (play number ) or -1 = quit }
      score.pl_numb:=mainscreen;

      if score.pl_numb>0 then start_game(score.pl_numb);

   until score.pl_numb<1; { cycle until it's worth -1 = quit }

end;



begin

 init_game;

end.