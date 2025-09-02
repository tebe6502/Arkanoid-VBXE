
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
 
 VBXE v1.3 by Tebe/Madteam

 2025-09-01
 2025-09-03
*)


program arkanoid;

uses crt, atari, vbxe, joystick;

{$r arkanoid.rc}

{$define romoff}


const
        VBXE_DATA = VBXE_OVRADR + 320*216;

var

	blt: TBCB absolute VBXE_BCBADR+VBXE_WINDOW;
	blt_letter: TBCB absolute VBXE_BCBADR+VBXE_WINDOW+21;

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



function sqrt32(r: cardinal): word;
var b,q,t: cardinal;
begin

 q:=0;
 b:=1 shl 30;
 
 while b>r do b:=b shr 2;
 
 while b>0 do begin
  t:=q+b;
  
  q:=q shr 1;
  
  if r>=t then begin
   r:=r-t;
   q:=q+b;  
  end;
  
  b:=b shr 2;
 
 end;
 
 result:=q;

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