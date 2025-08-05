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

program Arkanoid;

uses crt,graph,mouse,snd,service;//,FM;

begin

   { Questa porzione contiene solo il caricamento in memoria dei files dati }

   DetectFM;
   success:=TRUE;  { Success viene messo a FALSE se si verifica un errore da disco }

   load_all_walls; { Carica i 32 muri di gioco }

   initSVGA;       { Attiva la modalita grafica 320x200x256 col. }
   initRowArray;   { Inizializza un array utile per evitare le moltiplicazioni }
                   { per 320. }


   { Carica uno dopo l'altro tutti i disegni grafici di cui necessita }

   loadBTM('PLAYGR.BTM'  ,playscreen,TRUE);  { lo schermo virtuale }
   loadBTM('PRESENT.BTM' ,presents,false);   { la scritta ARKANOID }
   loadBTM('EXPLODE.BTM' ,explosion,FALSE);  { l'esplosione del vaus }
   loadBTM('NEWVAUS.BTM' ,newvaus,FALSE);    { la creazione del vaus }
   loadBTM('SOUNDFX.BTM' ,soundfx,FALSE);    { l'icona sound on/off  }
   loadBTM('SHINEWAL.BTM',shinewall,FALSE);  { il lucchichio dei mattoni }
   loadBTM('MINIVAUS.BTM',minivaus,FALSE);   { i vaus che indicano le vite }
   loadBTM('LEVELSEL.BTM',levelsel,FALSE);   { l'icona del livello di gioco }
   loadBTM('DROPS.BTM'   ,letters,FALSE);    { le 7 lettere }
   loadBTM('VAUS.BTM'    ,normal,FALSE);     { il vaus }
   loadBTM('LASERS.BTM'  ,lasers,FALSE);     { i raggi laser sparati dal vaus }
   loadBTM('ENLARGED.BTM',enlarged,FALSE);   { il vaus allargato }
   loadBTM('FIRE.BTM'    ,shoots,FALSE);     { il vaus coi laser montati }
   loadBTM('SCRFLUX.BTM' ,flux,FALSE);       { l'onda di flusso (per la lett. B) }

   if not success then fatal_error('Program can''t find some BTM files');

   score.hiscore:=50000;
   { il punteggio record viene settato inizialmente a 50000 per default }

   sound_on:=TRUE;      { per default all'inizio il suono e' attivo }
   lv:=DEFLEVEL;        { e il livello viene settato a DEFLEVEL     }

   repeat

      mousereset;

      { mainscreen restituisce 1,2 (numero gioc. ) o -1 = quit }
      score.pl_numb:=mainscreen;
      if score.pl_numb>0 then start_game(score.pl_numb);

   until score.pl_numb<1; { cicla finche' non vale -1 = quit }

//   closegraph; { Chiude la grafica }
//   closeprogram;

end.
