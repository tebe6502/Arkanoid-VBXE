
{ ----------------------------------------------------------------------- }
{                                                                         }
{                                 ARKANOID                                }
{                         Written by Claudio Bazzan                       }
{                     Esame di Fondamenti di Informatica I                }
{                         Professor Eduardo Calabrese                     }
{                    Revisione v1.02 per il publico dominio               }
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
 
 Arkanoid VBXE v1.8 by Tebe/Madteam
 

 2025-09-10 
 - 'remove_block', 'plot_lives' fully accelerated by blitter
 - 'place_block' accelerated by blitter

 2025-09-11
 - 'write_score' optimization
 - 'create_vaus' optimization

*)


program arkanoid;

uses crt, atari, vbxe, joystick;

{$r arkanoid.rc}


{ ------------------------------------------------------------------------- }


type

   BTMTYPE  = RECORD                   { per un disegno in fomrato BTM }
              width   : word;          { larghezza disegno       }
              height  : byte;          { altezza                 }
              ofs : cardinal;
              end;

   VAUSTYPE = RECORD                   { per i dati del vaus }
              x,y: byte;               { attuali coordinate x,y }

              oldx,                    { vecchie coordinate del vaus }
              oldy   : smallint;
              oldlen : smallint;       { e vecchia lunghezza }
              width,                   { larghezza }
              height : byte;           { spessore (o altezza) }
              flash  : byte;           { indica il colore attuale dei bordi }
              iflash : byte;           { contatore di ritardo per il }
                                       { il lampeggio dei bordi }
              letter : byte;
              end;


   BALLTYPE = RECORD                   { contiene i dati della pallina }
              x,y: smallint;           { coordinate x,y attuali }
              finex,finey: byte;       { submultiples of the coordinates }
              oldx,oldy: smallint;     { vecchie coordinate }
              speed  : word;           { velocita' (256 = 70 pixel al sec. }
              finespeed : word;        { speed (submultiple) }
              speedx,                  { velocita' sull'asse x }
              speedy : smallint;       { velocita' sull'asse y }
              sbd    : word;           { to avoid ball loops }
              brwhit : byte;           { number of brown blocks affected in succession }
              inplay : boolean;        { flag, TRUE if the ball is in play }
              launch : boolean;        { flag, TRUE if the ball must be }
                                       { thrown again }
              onvaus : smallint;       { width in pixels of the vaus }
              stm    : byte;           { magnet counter }
              end;

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
                end;

   SHREC      = RECORD                 { for the sparkle of the bricks }
                xb, yb      : byte;
                frame       : byte;
                block       : byte;
                active      : boolean;
                end;

   LETTERREC  = RECORD                 { data related to the letter }
                x,y      : byte;       { coord. }
                typ      : word;       { Type, B,C,E,L,P,D,S }
                frame    : byte;       { frame number }
                subframe : byte;       { number of cycles per frame }
                active   : boolean;    { the letter can be active }
                incoming : smallint;   { holds the sum, >1000 the letter falls out }
                nextx,                 { Coordination of where it should fall if activated }
                nexty    : byte;
                nexttype : smallint;   { type of letter that will have to fall }
                last     : smallint;   { last letter dropped }
                end;

   FIRETYPE   = RECORD                 { for lasers }
                x,y  : byte;           { coord. }
                shot : boolean;        { if the shot went off }
                avl  : boolean;        { if it's available (thanks to L) }
                nw   : boolean;        { if he just left VAUS }
                end;


{ ------------------------------------------------------------------------- }


const
	VBXE_DIGIT = $5000;

        VBXE_DATA = $6000;

	playscreen_ofs  = $020000;

	vram = $030000;
	
	pattern_temp = $040000;


	explosion_ofs   = VBXE_DATA;
	shinewall_ofs   = VBXE_DATA + $0000319D;
	letters_ofs     = VBXE_DATA + $00003ECC;
	minivaus_ofs	= VBXE_DATA + $0000369D;

	shoots_ofs      = VBXE_DATA + $00005E4C;
	flux_ofs	= VBXE_DATA + $00005EB4;
	balldata_ofs    = VBXE_DATA + $00005FFC;

	presents_ofs = VBXE_DATA + 90*320;

	
	minivaus_width = 20;
	minivaus_height = 5;

	explosion_width = 42;

	shoots_width = 13;
	shoots_height = 8;


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

   SCORE_WALL : array[0..10] of word = (0, 10,20,30,40,50,100,200,250,500,1000 );


   EMERG_DEV  : array[0..8] of byte = (0, $02,$13,$24,$35,$12,$23,$34,$45 );


   COLORBLOCK : array[0..9] of byte = ( 212,211,210,209,208, 207,206,205,204,203 );
                                         { Color of the bricks }


   GRAYDOWN   = 1;   { Number of strokes-1 to knock down a gray brick }
   STARTWALL  = 1;   { Starting level }
   BALLSPEED  = 550; { Ball speed (256 = 70 pixels per second }
   MAXSPEED   = 1023;{ Maximum speed attainable by the ball }
   MAXBRWHIT  = 100; { Maximum number of indistr. blocks it can hit }
                     { before splashing off changing speed          }

   PATNUMBER  = 4;   { Number of available backdrops }

   POS_DIGIT  : array[0..3] of byte = (0, 60,93,128);
   { Y coordinate of the three scores (player 1, player 2, hiscore) }


   DIGITS     : array[0..10] of byte = ( 125,96,55,103,106,79, 95,97,127,111,0 );
   { Data for displaying digital digits in scores }

   LEVEL      : array[0..5] of word = (0, 1000,300,100,60,35);

   SBDIR      = 600; { Cycles it must do before the ball you have to (dev. adjust) }
   DEFLEVEL   = 3;   { Default game level }

   LETTER_PROB= 300; { range in which the random number of the letter is drawn }
   LETTER_DROP= 1000;{ Number that must reach the sum to drop the letter }
   LETTER_NUMB= 8;   { number of letters+1 }
   LETTER_FRM = 8;   { Number of frames that constitute the animation of the letter }
   LETTER_SBF = 5;   { Number of cycles it must complete before moving to the next frame }

   { Probability of letter drop in % }  {  L   E  B   D   S   C  P }
   LETTER_DIS : array[0..7] of byte = ( 0, 16, 20, 3, 18, 20, 20, 3 );

   FLUXLEVEL  = 176;


{ ------------------------------------------------------------------------- }


var
	
	blt        : TBCB absolute VBXE_BCBADR+VBXE_WINDOW;
	blt_letter : TBCB absolute VBXE_BCBADR+VBXE_WINDOW+21;
	blt_box    : TBCB absolute VBXE_BCBADR+VBXE_WINDOW+21*2;
	blt_zero   : TBCB absolute VBXE_BCBADR+VBXE_WINDOW+21*3;

	vbxe_ram: TVBXEMemoryStream;


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
    flux       : BTMTYPE;
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
    
    hlp: word;
    f_hlp: single;


    scr: array [0..255] of byte absolute VBXE_WINDOW+$0200;
    pom: array [0..127] of byte absolute VBXE_WINDOW+$0280;
    pat: array [0..2047] of byte absolute VBXE_WINDOW+$0300;

    mody       : array[0..255] of byte absolute $0500;
    modx       : array[0..255] of byte absolute $0600;
    
    sqrtable : array [0..1023] of cardinal absolute $a000;


    [striped] row : array[0..255] of WORD absolute $c000; { array (see initRowArray) }

    Mod10Table: array [0..255] of byte absolute $c000+$200;
    Mod90Table: array [0..255] of byte absolute $c000+$300;

    [striped] Mod360Table: array [0..255] of WORD absolute $c000+$400;
    
    atan_tab : array [0..255] of byte absolute $c000+$600;
    log2_tab : array [0..255] of byte absolute $c000+$700;

    [striped] scale360 : array [0..255] of WORD absolute $c000+$800;
 
    sintable: array [0..450-1] of smallint absolute $c000+$a00;


    wall_p : array[0..2] of WALLTYPE absolute $d800;   { memorization of the wall itself }
    wall       : WALLTYPE absolute $d800+$300;         { wall }

    [striped] mul90_16: array [0..15] of WORD absolute $d800+$400;

    all_walls  : WHOLEWALLS absolute $d800+$500;       { all the walls }


{ ------------------------------------------------------------------------- }


procedure start_level;
begin


end;

procedure death_sound(a: word);
begin

end;


procedure ball_block_sound(a,b: word);
begin

end;


function rand(range: word): word; assembler;
asm

seed = MAIN.SYSTEM.RndSeed

Random16:
	lda seed+1
	tay 		; store copy of high byte
	; compute seed+1 ($39>>1 = %11100)
	lsr @		; shift to consume zeroes on left...
	lsr @
	lsr @
	sta seed+1	; now recreate the remaining bits in reverse order... %111
	lsr @
	eor seed+1
	lsr @
	eor seed+1
	eor seed+0	; recombine with original low byte
	sta seed+1
	; compute seed+0 ($39 = %111001)
	tya		; original high byte
	sta seed+0
	asl @
	eor seed+0
	asl @
	eor seed+0
	asl @
	asl @
	asl @
	eor seed+0
	sta seed+0

	lda seed+1
	and #$03
	sta Result+1
	lda seed
	sta Result

loop	cpw Result range
	bcc @exit

	lsr Result+1
	ror Result

	jmp loop
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


function FastSqrt(x: Single): Single;	// much faster with little less precision
var
  i: cardinal absolute x;
begin
  i := (i shr 1) + $1fc00000;

  Result := PSingle(@i)^;
end;


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
;	A = angle		in 0-255

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



{$i ..\service.pas}



procedure init_game;
begin

   randomize;

   initSVGA;       { Activates 320x200x256 col. graphics mode. }
   initRowArray;   { Initializes a useful array to avoid multiplications }
                   { by 320. }


   {$i btm.inc}


   totalwall:=32;

   score.hiscore:=50000;
   { the record score is initially set to 50000 by default }

   sound_on:=TRUE;      { by default at the beginning the sound is ON }
   lv:=DEFLEVEL;        { and the level is set to DEFLEVEL            }

   repeat

//      mousereset;

      { mainscreen returns 1,2 (play number ) or -1 = quit }
      score.pl_numb:=mainscreen;

      if score.pl_numb>0 then start_game(1);//score.pl_numb);


   until score.pl_numb<1; { cycle until it's worth -1 = quit }

end;



begin

 init_game;

end.