
{ ----------------------------------------------------------------------- }
{                                                                         }
{                                 ARKANOID                                }
{                         Written by Claudio Bazzan                       }
{                     Esame di Fondamenti di Informatica I                }
{                         Professor Eduardo Calabrese                     }
{                    Revisione v1.02 per il publico dominio               }
{                                                                         }
{ Nota: Questo programma e' la riproduzione fedele (per quanto possibile) }
{       del famoso Coin-Op da bar Arkanoid. I diritti del gioco originale }
{       sono della Taito Corp, questa versione e' da intendersi solamente }
{       a titolo dimostrativo. E' quindi assolutamente vietata la vendita }
{       o qualsiasi altro scopo di lucro.                                 }
{                                                                         }
{       Il programma gira su tutte le macchine con processore 80286 o su- }
{       periore. E' consigliato l'utilizzo di un 80386 per aprezzarne a   }
{       pieno le qualita'.                                                }
{                                                                         }
{       E sconsigliata la compilazione con RANGE CHECKING attivato dal    }
{       momento che i tempi di esecuzione sono critici e un minimo ritar- }
{       do nell'esecuzione di alcuni punti puo' provocarne un significa-  }
{       tivo e fastidioso rallentamento globale.                          }
{       Naturalmente il programma funziona tranquillamente anche con      }
{       la verifica degli intervalli attiva.                              }
{                                                                         }
{ ----------------------------------------------------------------------- }

{ NOTA PER CHI CERCASSE SPECIFICHE RIGUARDO ALLA GRAFICA                  }
{                                                                         }
{ La grafica di questo programma e' stata progettata con POWERDESIGN 386, }
{ un programma shareware che permette di manipolare immagini in modo      }
{ avanzato. Chiunque fosse interessato puo' prelevarlo gratuitamente da   }
{ qualunque nodo ISN oppure a Banana's BBS al 2:332/508 (Tel.0521/904046).}
{ Il programma viene continuamente aggiornato, l'ultima versione disponi- }
{ bile al momento della stesura di questo testo e' la v1.06 beta.         }
{ Il nome con cui e' possibile prelevare tale programma e' PD386Bxx.ARJ o }
{ PD386Bxx.ZIP ove al posto delle xx viene messo il numero della versione }
{ corrente. Pertanto un eventuale DOWNLOAD puo' essere fatto col nome:    }
{ PD386B*.*                                                               }
{ Il programma richiede un processore 386 (o migliore), scheda VGA o SVGA }
{ ed e' consigliabile la presenza di memoria estesa/espansa e relativo    }
{ driver di memoria (es. EMM386, QEMM, ecc.)                              }


program arkanoid;

uses crt, atari, vbxe, joystick;//, fastmath;

{$r arkanoid.rc}

{$define romoff}

//{$f $c8}

const
        VBXE_DATA = VBXE_OVRADR + 320*216;

var

	blt: TBCB absolute VBXE_BCBADR+VBXE_WINDOW;

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



function sqrt32(v: cardinal): word;
var b,q,r,t: cardinal;
begin

 r:=v;
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