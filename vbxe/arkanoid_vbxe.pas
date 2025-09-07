
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
 
 Arkanoid VBXE v1.6 by Tebe/Madteam

 2025-09-07

*)


program arkanoid;

uses crt, atari, vbxe, joystick;

{$r arkanoid.rc}


//{$f $c8}


const
	VBXE_DIGIT = $5000;

        VBXE_DATA = $6000;

	playscreen_ofs  = $020000;

	vram = $030000;

	explosion_ofs   = VBXE_DATA;
	shinewall_ofs   = VBXE_DATA + $0000319D;
	letters_ofs     = VBXE_DATA + $00003ECC;
	shoots_ofs      = VBXE_DATA + $00005E4C;
	flux_ofs	= VBXE_DATA + $00005EB4;
	balldata_ofs    = VBXE_DATA + $00005FFC;

	presents_ofs = VBXE_DATA + 90*320;

	
	minivaus_width = 20;
	minivaus_height = 5;

	explosion_width = 42;

	shoots_width = 13;
	shoots_height = 8;
	
var
	blt        : TBCB absolute VBXE_BCBADR+VBXE_WINDOW;
	blt_letter : TBCB absolute VBXE_BCBADR+VBXE_WINDOW+21;
	blt_box    : TBCB absolute VBXE_BCBADR+VBXE_WINDOW+21*2;
	blt_zero   : TBCB absolute VBXE_BCBADR+VBXE_WINDOW+21*3;

	vbxe_ram: TVBXEMemoryStream;


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
    lda a+1
    sta sign
  
    bpl @+

    lda #0
    sub a
    sta a
    lda #0
    sbc a+1
    sta a+1
@
    tay
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

    lda sign: #$00
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
    lda a+1
    sta sign
  
    bpl @+

    lda #0
    sub a
    sta a
    lda #0
    sbc a+1
    sta a+1
@
    tay
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

    lda sign: #$00
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
  i := (i shr 1) + (127 shl 22);
  Result := PSingle(@i)^;
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