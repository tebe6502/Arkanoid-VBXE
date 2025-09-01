//unit service;

{ ------------------------------------------------------------------------- }
//                                 interface
{ ----------------------------------------------------------------------- }

const

   vram = VBXE_OVRADR; // screen[0]

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
   EMP        = -1;  { Use EMP (empty) instead of -1                      }
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
   BALLSPEED  = 500; { Ball speed (256 = 70 pixels per second }
   MAXSPEED   = 2000;{ Maximum speed attainable by the ball }
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

   { Probability of letter drop in % }       {  L   E  B   D   S   C  P }
   LETTER_DIS : array[0..7] of byte = ( 0, 16, 20, 3, 18, 20, 20, 3 );

   FLUXLEVEL  = 176;

type

   TText = string[31];

   arr768   = array[0..767] of byte;       { For the 256 colors in RGB (x3) }
   //arr64k   = array[0..320*250-1] of byte; { for the 320x200 screen }

   BTMTYPE  = RECORD                   { per un disegno in fomrato BTM }
              width   : word;          { larghezza disegno       }
              height  : word;          { altezza                 }
              ofs : cardinal;

//              trasp   : byte;        { trasparenza (non usato) }
//              palette : arr768;      { puntatore alla palette di colori }
//              palused : boolean;     { flag TRUE = la palette esiste }
//              map     : arr64k;        { dati contenenti il disegno }
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
              letter : shortint;
              end;


   BALLTYPE = RECORD                   { contiene i dati della pallina }
              x,y,                     { coordinate x,y attuali }
              finex,finey,             { sottomultipli delle coordinate }
              oldx,oldy,               { vecchie coordinate }
              speed  : smallint;       { velocita' (256 = 70 pixel al sec. }
              finespeed : smallint;    { velocita' (sottomultiplo) }
              speedx,                  { velocita' sull'asse x }
              speedy : smallint;       { velocita' sull'asse y }
              sbd    : word;           { per evitare i loop della pallina }
              brwhit : byte;           { n. di blocchi marroni colpiti di seguito }
              inplay : boolean;        { flag, TRUE if the ball is in play }
              launch : boolean;        { flag, TRUE if the ball must be }
                                       { thrown again }
              onvaus : smallint;       { larghezza in pixel del vaus }
              stm    : byte;           { contatore di calamita }
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
                nexty,
                nexttype : smallint;   { type of letter that will have to fall }
                last     : smallint;   { last letter dropped }
                end;

   FIRETYPE   = RECORD                 { for lasers }
                x,y  : byte;           { coord. }
                shot : boolean;        { if the shot went off }
                avl  : boolean;        { if it's available (thanks to L) }
                nw   : boolean;        { if he just left VAUS }
                end;

var
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
    pattern    : BTMTYPE;  { sfondo }

    pattern0   : BTMTYPE;  { sfondo }
    pattern1   : BTMTYPE;  { sfondo }
    pattern2   : BTMTYPE;  { sfondo }
    pattern3   : BTMTYPE;  { sfondo }
    pattern4   : BTMTYPE;  { sfondo }

    status     : byte;

    success    : boolean;               { status flag for BTM loading }

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

//    def_pal: arr768;

{$IFDEF ATARI}
    [striped] row        : array[0..255] of word absolute $c000; { array (see initRowArray) }

    tmp        : array [0..255] of byte absolute $c000+$200;

    mody       : array[0..255] of byte absolute $c000+$300;
    modx       : array[0..255] of byte absolute $c000+$400;
    
    scanline   : array[0..255] of byte absolute $c000+$500;
    scanline2  : array[0..127] of byte absolute $c000+$580;
    
    wall_p : array[0..2] of WALLTYPE absolute $d800;   { memorization of the wall itself }
    wall       : WALLTYPE absolute $d800+$300;         { wall }
    all_walls  : WHOLEWALLS absolute $d800+$400;       { all the walls }
{$ELSE}
    tmp        : array [0..255] of byte;

    row        : array[0..255] of word; { array (see initRowArray) }

    modx       : array[0..319] of byte;
    mody       : array[0..199] of byte;
    
    wall_p : array[0..2] of WALLTYPE; { memorization of the wall itself }
    wall       : WALLTYPE;            { wall }
    all_walls  : WHOLEWALLS;          { all the walls }
{$ENDIF}



{$IFDEF ATARI}
//    screen     : array [0..0] of byte;
{$ELSE}
    screen     : array [0..255*1024] of byte;
{$ENDIF}
               { forcing the screen map to the VGA address }
               { a000:0000 inherent to the 320x200x256 col. graphics mode }

{ ------------------------------------------------------------------------- }

{ These are the functions that must be seen by the main program.            }
   {
function  mainscreen : smallint;
procedure fatal_error(err_type : string);
procedure loadBTM(name : string; var BTMREC : BTMTYPE; pal : boolean);
procedure load_all_walls;
procedure InitSVGA;
procedure initRowArray;
function  random_letter_drop : smallint;
procedure start_game(players : smallint);
procedure closeprogram;

{ ------------------------------------------------------------------------- }
//                              implementation
{ ------------------------------------------------------------------------- }


{$IFDEF ATARI}


procedure mousecoords(var x: smallint);
var a: byte;
begin

 a:=porta and $0f;
 
 case a of
  joy_left: if x > SCRMIN then dec(x, 4);
  joy_right: if x < SCRMAX then inc(x, 4);
 end;

 end;
 
 
 
function mouseclick: byte;
begin
    
    result:=trig0 xor 1;
    
end;

 

procedure blitBOX(src, dst: cardinal; w: word; h: byte);
begin

	asm
	  fxs FX_MEMS #$80
	end;

 blt.src_adr.byte2:=src shr 16;
 blt.src_adr.byte1:=src shr 8;
 blt.src_adr.byte0:=src;

 blt.dst_adr.byte2:=dst shr 16;
 blt.dst_adr.byte1:=dst shr 8;
 blt.dst_adr.byte0:=dst;

 blt.src_step_x:=1;
 blt.dst_step_x:=1;

 blt.dst_step_y:=320;
 blt.src_step_y:=320;

 blt.blt_width:=w-1;
 blt.blt_height:=h-1;

 blt.blt_and_mask:=$ff;

 blt.blt_zoom:=0;	// x1

 blt.blt_control:=0;

	asm
	  fxs FX_MEMS #$00
	end;
	
 RunBCB(blt);
	
end;


procedure blitZERO(src, dst: cardinal; w : word; h: byte);
//var i: word;
begin

// for i := 0 to size-1 do
//  if screen[src + i] <> 0 then screen[dst + i] := screen[src + i];

	asm
	  fxs FX_MEMS #$80
	end;

 blt.src_adr.byte2:=src shr 16;
 blt.src_adr.byte1:=src shr 8;
 blt.src_adr.byte0:=src;

 blt.dst_adr.byte2:=dst shr 16;
 blt.dst_adr.byte1:=dst shr 8;
 blt.dst_adr.byte0:=dst;

 blt.src_step_x:=1;
 blt.dst_step_x:=1;

 blt.blt_control := 1;

 blt.dst_step_y:=320;
 blt.src_step_y:=w;
 
 blt.blt_height:=h-1;

 blt.blt_width:=w-1;

 blt.blt_and_mask := $ff;

	asm
	  fxs FX_MEMS #$00
	end;


 RunBCB(blt);

end;


procedure blitTMP(dst: cardinal; size: byte);
//var x: byte;
begin

// for x := 0 to size-1 do
//  screen[dst+x] := tmp[x];

vbxe_ram.position:=dst;

vbxe_ram.WriteBuffer(TMP, size);

end;


procedure blitROW(src, dst: cardinal; size: word);
//var x: word;
begin

// for x := 0 to size-1 do
//  screen[dst+x] := screen[src+x];

	asm
	  fxs FX_MEMS #$80
	end;

 blt.src_adr.byte2:=src shr 16;
 blt.src_adr.byte1:=src shr 8;
 blt.src_adr.byte0:=src;

 blt.dst_adr.byte2:=dst shr 16;
 blt.dst_adr.byte1:=dst shr 8;
 blt.dst_adr.byte0:=dst;

 blt.src_step_x:=1;
 blt.dst_step_x:=1;

 blt.src_step_y:=0;
 blt.dst_step_y:=0;
 
 blt.blt_height:=0;
 blt.blt_control := 0;

 blt.blt_width:=size-1;

 blt.blt_and_mask := $ff;

	asm
	  fxs FX_MEMS #$00
	end;

 RunBCB(blt);

end;


{
procedure blitBYTE(src, dst: cardinal);
var a: byte;
begin

//  screen[dst] := screen[src];

 vbxe_ram.position := src;
 a := vbxe_ram.ReadByte;

 vbxe_ram.position := dst;
 vbxe_ram.WriteByte(a);

end;
}


procedure putBYTE(dst: cardinal; v: byte);
begin

//  screen[dst] := v;

 vbxe_ram.position := dst;
 vbxe_ram.WriteByte(v);

end;


function getBYTE(src: cardinal): byte;
begin

//  result := screen[src];

 vbxe_ram.position := src;
 Result := vbxe_ram.ReadByte;

end;


{$ELSE}

procedure blitZERO(src, dst: cardinal; size : word);
var i: word;
begin

 for i := 0 to size-1 do
  if screen[src + i] <> 0 then screen[dst + i] := screen[src + i];

end;


procedure blitTMP(dst: cardinal; size: byte);
var x: byte;
begin

 for x := 0 to size-1 do
  screen[dst+x] := tmp[x];

end;


procedure blitROW(src, dst: cardinal; size: word);
var x: word;
begin

 for x := 0 to size-1 do
  screen[dst+x] := screen[src+x];

end;


procedure blitBYTE(src, dst: cardinal);
begin

  screen[dst] := screen[src];

end;


procedure putBYTE(dst: cardinal; v: byte);
begin

  screen[dst] := v;

end;


function getBYTE(src: cardinal): byte;
begin

  result := screen[src];

end;

{$ENDIF}


{ ------------------------------------------------------------------------- }
{ ------------------------------------------------------------------------- }


procedure setcolor(c: byte);
begin

end;

function textwidth(a: string): word;
begin

 Result := length(a) * 8;

end;


procedure outtextxy(x,y: smallint; s: TText);
begin


end;

procedure nosound;
begin


end;


procedure mydelay(a: word);
begin



end;


procedure closeprogram;
  begin
{
  clrscr;
  textcolor(7);
  writeln('Important!');
  writeln;
  writeln('This game is only a demostration of how is possible to build');
  writeln('sophisticated programs, even in Turbo Pascal, using a powerful');
  writeln('shareware utility whose name is Power Design 386 (or PD386 for short).');
  writeln('PD386 offers all the features you need to create multicolored sprites');
  writeln('and pictoresque background screens for your games, your aplications');
  writeln('and your programs in general. It also supports the .GIF graphic format!');
  writeln('If you are interested in programming graphics you cannot miss PD386.');
  writeln('Please take a look.');
  writeln;
  writeln('End of message.');
}
  halt;
  end;


{ return the maximum between a and b }
function max(a,b : smallint) : smallint;
  begin
  if(a>b) then max:=a
  else max:=b;
  end;

{ same for the minimum }
function min(a,b : smallint) : smallint;
  begin
  if(a<b) then min:=a
  else min:=b;
  end;


{ clear, isn't it? }
procedure fatal_error(err_type: byte);

   begin

    status := err_type;
    
    asm
      sta $100
      
      kk: lda $d20a
      sta $d01a
      jmp kk
    
    end;

{
   nosound;
   //closegraph;
   //write;
   //writeln('Arkanoid can run no long');
   //writeln('Fatal Error: ',err_type);
     Application.MessageBox(PWideChar(err_type),'Error' ,MB_ICONEXCLAMATION);

   halt;
}
   end;


function inkey : word;   { restituisce il codice del tasto premuto }
//var ch,ch2: char;        { 0 = nessun tasto premuto }
    begin
{
    ch:=#0;
    ch2:=#0;

    if keypressed then
       begin
       ch:=readkey;
       if ch=#0 then ch2:=readkey;
       end;

    inkey:=(ord(ch2)*256)+ord(ch);
}

 result:=ord('r');
    end;


procedure initRowArray;  { inizializza l'array ROW; row[n]:=320*n }
var y : byte;
begin
  
  hlp:=0;
  
  for y:=0 to 255 do begin
  
    if y>= 200 then
     row[y] := 320*200
    else
     row[y]:=hlp;
     
    inc(hlp, 320);

  end;
  
end;


procedure InitSVGA; { Inizializza il driver della SuperVGA come da esempio }
//var
//   AutoDetect : pointer;
//   GraphMode, GraphDriver, ErrCode: smallint;

   begin
   
{$IFDEF ATARI}   

 if VBXE.GraphResult <> VBXE.grOK then begin
  writeln('VBXE not detected');
  halt;
 end;

 SetHorizontalRes(VBXE.VGAMed);
 ColorMapOff;

 VBXEControl(vc_xdl+vc_xcolor+vc_no_trans);

 SetTopBorder(20);
 SetXDLHeight(200);

 vbxe_ram.position:=VBXE_OVRADR;
 vbxe_ram.size:=320*200;
 vbxe_ram.clear;

 dmactl:=0;

	asm
	  fxs FX_MEMS #$80
	end;

 fillByte(blt, sizeof(TBCB), 0)	;

	asm
	  fxs FX_MEMS #$00
	end;

{$ENDIF}


 {
	GraphDriver := InstallUserDriver('SVGA256',@DetectVGA);
   if RegisterBGIDriver(@svgadrv)<0 then
      fatal_error('Unable to register driver');

	ErrCode:=GraphResult;
	if ErrCode <> grOk then
		begin
		WriteLn('Error installing TestDriver:');
		Writeln(GraphErrorMsg(ErrCode));
		Halt(1);
		end;
	GraphDriver:=Detect;
	InitGraph(GraphDriver,GraphMode,'');
	ErrCode := GraphResult;
	if ErrCode <> grOk then
		begin
		WriteLn('Error during Init: ', GraphErrorMsg(ErrCode));
		Halt(1);
		end;
}
   end; { Fine della procedura InitSVGA }

{ ------------------------------------------------------------------------ }

procedure shine_block;    { esegue lo scintillio di un blocco }
var
    xb,yb,                { i parametri del blocco sono contenuti nella }
    frame : word;         { variabile globale SHINEREC }
    xf,yf,
    fr,og : word;
    
    y, i: byte;

    begin
    xb   :=shinerec.xb;   { mette in xb,yb le coordinate del blocco }
    yb   :=shinerec.yb;
    
    i := xb+yb*16;

    if wall[i]>8 then                  { se il blocco e grigio o marrone }
       begin
       frame:=(shinerec.frame shr 1);  { calcola il n. del frame }
       if wall[i]<>10 then inc(frame,5);

       xf:= 9+(xb shl 4);  { trova le coordinate sullo shermo del blocco }
       yf:=22+(yb shl 3);  { da far scintillare }
       fr:=frame  shl 7;   { si calcola la posizione del n-esimo frame }

       for y:=0 to 7 do    { e copia il frame n-esimo sullo schermo }
           begin
           og:=y shl 4;      { equivale ad y*16, ma piu' veloce }
//           memcpy(shinewall.map[fr+og], screen[xf+row[yf+y]], 16);

           blitROW(shinewall.ofs+fr+og, vram + xf+row[yf+y], 16);
           end;
       end;

    inc(shinerec.frame);  { incrementa il frame counter }
    if shinerec.frame=10 then shinerec.active:=FALSE;
    { e quando il frame e' l'ultimo allora lo scintillio e' finito }

    end;

procedure unshine_block; { interrompe lo scintillio di un blocco se la }
                         { palla urtandone un altro causa lo scintillio }
                         { di un altro blocco }
    begin
    shinerec.frame:=9;   { cioe' setta il frame come ultimo }
    shine_block;         { ed esegue lo scintillio del blocco con l'ultimo }
                         { frame, cioe' il blocco torna normale }
    end;

procedure shine(xb,yb : byte);   { questa procedura imposta lo }
                                 { scintillio di un blocco }
    begin
    if shinerec.active then unshine_block;

    shinerec.xb    :=xb;  { coordinate del blocco }
    shinerec.yb    :=yb;  { x,y }
    shinerec.frame :=0;   { frame di partenza }
    shinerec.active:=TRUE;                 { scintillio attivo }
    shinerec.block :=wall[byte(xb+yb*16)]; { tipo di blocco (marrone o grigio) }
    end;

procedure checkshine; { se lo scintillio e' attivato allora lo esegue }
                      { passando al frame successivo }
    begin
    if shinerec.active=TRUE then shine_block;
    end;


function random_letter_drop : smallint;
var rn,sum,letter : word;
   begin
   repeat
      rn:=random(100);  { Tira a caso un numero fra 0 e 99 }
      sum:=0;           { pone la somma a zero             }
      letter:=0;        { e la lettera corrente a 0        }

      repeat


         inc(letter);                   { Incrementa la lettera corrente }
         inc(sum,LETTER_DIS[letter]);   { Incrementa somma della percentuale di }
                                        { probabilita' della lettera corrente }

      until sum>rn; { Se la somma oltrepassa il numero casuale scelto }
                    { il programma fa cadere la lettera corrente      }
                    { altrimenti passa alla lettera dopo.             }

   until smallint(letter-1) <> lett.last;

   random_letter_drop:=(letter-1);
   end;


procedure put_letter;
var fl,fw : word;
    yl: byte;



procedure blitLETTER(src, dst: cardinal; w : word; h: byte);
begin

	asm
	  fxs FX_MEMS #$80
	end;

 blt.src_adr.byte2:=src shr 16;
 blt.src_adr.byte1:=src shr 8;
 blt.src_adr.byte0:=src;

 blt.dst_adr.byte2:=dst shr 16;
 blt.dst_adr.byte1:=dst shr 8;
 blt.dst_adr.byte0:=dst;

 blt.src_step_x:=1;
 blt.dst_step_x:=1;

 blt.blt_control := 1;

 blt.dst_step_y:=320;
 blt.src_step_y:=128;
 
 blt.blt_height:=h-1;

 blt.blt_width:=w-1;

 blt.blt_and_mask := $ff;

	asm
	  fxs FX_MEMS #$00
	end;


 RunBCB(blt);

end;



begin
    fl:=(lett.typ shl 10)+(lett.frame shl 4);

    blitLETTER(letters.ofs + fl, vram + lett.x+row[lett.y], 16, 8);

{
    for yl:=0 to 7 do
        begin
        fw:=yl shl 7;
        //memzerocpy(letters.map[fw+fl], screen[lett.x+row[lett.y+yl]], 16);
        blitZERO(letters.ofs + fw+fl, vram + lett.x+row[lett.y+yl], 16, 1);
        end;
}
end;


procedure remove_letter;
var yl: byte;

begin
//    if (lett.x>=0) and (lett.x<320) and (lett.y>0) and (lett.y<240) then
//       begin

    hlp:=lett.x+row[lett.y];

    if lett.y < 200 then
      blitBOX(playscreen.ofs + hlp, vram + hlp, 16, 8);

{
       for yl:=0 to 7 do
           begin
           hlp:=lett.x+row[lett.y+yl];
           if hlp<64000 then
              //memcpy(playscreen.map[ad], screen[ad], 16);

              blitROW(playscreen.ofs+ hlp, vram+ hlp, 16);
           end;
//       end;
}
end;


procedure disable_letter;
   begin
   remove_letter;
   lett.active:=FALSE;
   end;


procedure start_letter(xl,yl,letter : word);
   begin
   if lett.active then disable_letter;

   lett.x       :=xl;
   lett.y       :=yl;
   lett.typ     :=letter;
   lett.frame   :=0;
   lett.subframe:=0;
   lett.active  :=TRUE;
   end;


procedure check_letter;
   begin
   if lett.active then
      begin
      remove_letter;
      inc(lett.y);
      if lett.y>=200 then disable_letter
      else
        begin
        put_letter;
        inc(lett.subframe);
        if lett.subframe=LETTER_SBF then
           begin
           lett.subframe:=0;
           inc(lett.frame);
           end;

        if lett.frame=LETTER_FRM then lett.frame:=0;

        if (vaus.x < (lett.x+16)) and ((vaus.x+vaus.width)>lett.x) and
           (vaus.y < (lett.y+8))  and ((vaus.y+vaus.height)>lett.y) then
           begin
           ball_block_sound(100,10);
           vaus.letter:=lett.typ+1;
           inc(score.player[cur_player],SCORE_WALL[10]);
           disable_letter;
           end;
        end;

      lett.incoming:=0;
      end
   else if (lett.incoming>LETTER_DROP) then
          start_letter(lett.nextx, lett.nexty, lett.nexttype);
   end;


{ Copy the specified drawing to the screen starting from coordinate 0,0 }
procedure showBTMpicture(BTM : BTMTYPE);
//var x,y,ofst : word;

  begin
    
    blitBOX(BTM.ofs, vram, BTM.width, BTM.height);

(*
  for y:=0 to BTM.height-1 do   { la y varia da 0 all'altezza-1 del disegno }
     begin
     ofst:=y*BTM.width;         { calcola l'indirizzo nella matrice del dis.}
     for x:=0 to BTM.width-1 do
        screen[x+row[y]]:=BTM.map[x+ofst]; { mette il disegno sullo schermo }
     end;
*)

  end;


{ Draw the ball on the screen, the coordinates are specified in }
{ BALL.x and BALL.y, BALLSPOT.x = BALLSPOT.y is the radius of the ball }
{ in pixels }
procedure place_ball(var ball : BALLTYPE);
//var yp, x : byte;
begin

  hlp:=ball.x-BALLSPOT+row[ball.y - BALLSPOT];
  
  blitZERO(balldata.ofs, vram + hlp, BALLDIM, BALLDIM); 

{
  x:=0;

  for yp:=0 to BALLDIM-1 do
     begin
     hlp:=ball.x-BALLSPOT+row[ball.y+yp-BALLSPOT];
  
     //mzerocpy(BALLARRAY[yp,0], screen[adr], BALLDIM);
     blitZERO(balldata.ofs + x, vram + hlp, BALLDIM, 1);
     
     inc(x, BALLDIM);

     end;
}
end;


{ Delete the ball from the screen, called one moment before }
{ place_ball }
procedure remove_ball(var ball: BALLTYPE);
//var
//  yp   : byte;

begin

  hlp := ball.oldx-BALLSPOT+row[ball.oldy-BALLSPOT];
  
  blitBOX(playscreen.ofs + hlp, vram + hlp, BALLDIM, BALLDIM);

{
  for yp:=0 to BALLDIM-1 do
      begin
      hlp:=ball.oldx-BALLSPOT+row[ball.oldy+yp-BALLSPOT];

      //if (temp>0) and (temp<64000) then
         //memcpy(playscreen.map[temp], screen[temp], BALLDIM);

         blitROW(playscreen.ofs + hlp, vram + hlp, BALLDIM);
      end;
}      
end;


procedure Wait_VBL;
//label ALTO,BASSO;
   begin
  (*
      { questa porzione di codice assembler attende che il pennello }
      { elettronico del monitor sia nella posizione di vertical blank }
      { in modo da evitare lo sfarfallamento dell'immagine. }

      { provvede anche a temporizzare la CPU regolando la velocita' di }
      { esecuzione del programma indipendentemente dal tipo di processore }

      { ogni scheda VGA in modalita' 320x200x256 col. lavora ad una }
      { frequenza di 70Hz. }

*)

{$IFDEF ATARI}

	pause;

{$ENDIF}

//  form1.show_play;
end;


procedure set_ball(var ball : BALLTYPE);
var b0, b1: Boolean;
  begin
  
  b0:=(ball.oldx<>EMP) and (ball.oldy<>EMP);
  b1:=(ball.oldx<>ball.x) or (ball.oldy<>ball.y);
  
  if (b0 and b1) then
      remove_ball(ball); { as soon as VB starts the ball is moved to the }

  place_ball(ball);  { new coordinates }

  ball.oldx:=ball.x; { si settano le vecchie coordinate uguali a quelle }
  ball.oldy:=ball.y; { correnti, le correnti verrano modificate poi }
  end;


procedure set_ball_speed(var ball : BALLTYPE; speed : smallint);
var
  sx,sy : smallint;   { Sets the speed of the ball based on the speed }
  vm : single;     { vector module passed in SPEED: smallint.      }
  i: cardinal;
  
  a,b: word;

  begin
  sx:=ball.speedx;  { stores the x and y components of velocity }
  sy:=ball.speedy;  { in sx and sy, respectively                }
  
  a:=abs(sx);
  b:=abs(sy);
  
  i:=(a*a) + (b*b);
  
  hlp:=sqrt32(i);
  
  vm:=speed / hlp ;//sqrt(sx*sx+sy*sy); { calculate the coefficient of proportionality  }
                                 { between the old and new speeds                }
                                 { (the direction does not change, only          }
                                 { the modulus changes).                         }

  ball.speedx:=trunc(sx * vm);   { e quindi moltiplica per tale coef. }
  ball.speedy:=trunc(sy * vm);   { le due proiezioni della velocita'. }

  end;

procedure set_ball_direction(var ball : BALLTYPE; angle : smallint);
var w : single;
  begin                  { imposta l'angolo di traiettoria della palla }
 
  w:=angle*pi/180.0;     { w viene espresso in gradi }

  ball.speedx:=trunc(256*cos(w));  { la velocita' si suppone unitaria }
  ball.speedy:=-trunc(256*sin(w)); { v=256 equivale a 70 pixel al sec. }
  end;


function get_ball_direction(var ball : BALLTYPE): smallint;
var w : smallint; { restituisce la direzione in cui si muove la palla }
  begin

  if ball.speedx=0 then w:=-90*(ball.speedy div abs(ball.speedy))
  else
    begin
    { calcola l'arcotangente e aggiunge multipli di 90 gradi a seconda dei }
    { segni di ball.speedx e ballspeed.y }
      
    f_hlp:= -ball.speedy / ball.speedx;
    
    f_hlp := arctan(f_hlp)*180.0/pi;

    w:=trunc(f_hlp);

    if(ball.speedx<0) then inc(w,180);

    inc(w,360);
    w:=w mod 360;
    end;

  get_ball_direction:=w;
  end;


procedure start_ball(var ball : BALLTYPE);
  begin
  { initialize the ball parameters when it is to be }
  { thrown by the vaus. }

  ball.x:=vaus.x+ball.onvaus;

                     { the starting coordinate is that of vaus+16 }
                     { i.e., since vaus is 34 pixels, the ball must }
                     { start at the center of vaus, i.e., vaus.x+16 }

  ball.y:=vaus.y-BALLSPOT;
                           { clearly, the center of the ball will be on }
                           { the line where the vaus flows, minus the radius }
                           { of the ball }

  ball.finex:=0;     { and the two submultiples of the position are 0 }
  ball.finey:=0;

  ball.inplay:=TRUE; { set the flag that stores whether the ball is in play }

  ball.sbd:=0;
  ball.brwhit:=0;

  end;


function ball_speed(ball : BALLTYPE): smallint;
var i: cardinal;
    a, b: word;
  begin
  { returns the ball velocity formula, uses the Pythagorean theorem }
  { (v=sqrt(x^2+y^2)) }
  
  a:=abs(ball.speedx);
  b:=abs(ball.speedy);
  
  i:=a*a + b*b;
  
  ball_speed:=sqrt32(i);

  //ball_speed:=trunc(f_hlp);
    
  end;


procedure move_ball(var ball : BALLTYPE);
var b0,b1,b2: Boolean;
var
  x,y : word;
  angle : smallint;

  begin
  { moves the ball by adding the two velocity vectors to the x,y coordinates }
  { before performing this addition, everything is multiplied by 256 in order to }
  { have a higher number of positions. }

  x:=(ball.x shl 8)+ball.finex+ball.speedx;
  y:=(ball.y shl 8)+ball.finey+ball.speedy;

  ball.x:=x shr 8;
  ball.y:=y shr 8;

  ball.finex:=x and $ff;
  ball.finey:=y and $ff;

  { check if the ball hits the right wall }
  { if it hits, reverse the sign }

  if(ball.x>SCRMAX) then
     begin
     ball.speedx:=-ball.speedx;  { inverte il vettore x della velocita' }
     ball.x:=2*SCRMAX-ball.x;    { riflette sull'asse x=SCRMAX la palla }
     ball.finex:=255-ball.finex; { aggiusta i sottomultipli della velocita' }
     ball_block_sound(240,5);    { emette il suono dell'urto sulla la parete }
     end;

  { idem per la parete di sinistra }

  if(ball.x<SCRMIN) then
     begin
     ball.speedx:=-ball.speedx;
     ball.x:=2*SCRMIN-ball.x;
     ball.finex:=255-ball.finex;
     ball_block_sound(240,5);
     end;

  { ... e per quella superiore }

  if(ball.y<SCRTOP) then
     begin
     ball.speedy:=-ball.speedy;
     ball.y:=2*SCRTOP-ball.y;
     ball.finey:=255-ball.finey;
     ball_block_sound(240,5);
     end;


  { if the ball is on the y-axis of the vaus, if the velocity vy is
    greater than 0 (i.e., the ball is moving downward), and if the
    ball was previously above the vaus, then ...}

  b0:=(ball.y + BALLSPOT > VAUS_LINE);
  b1:=(ball.speedy > 0);
  b2:=(ball.oldy <= VAUS_LINE);

//  if(ball.y+BALLSPOT>VAUS_LINE) and (ball.speedy>0) and (ball.oldy<=VAUS_LINE) then
  if b0 and b1 and b2 then
     begin
     { se un qualsiasi punto della palla si trova sul vaus ... }

     if(ball.x > smallint(vaus.x-BALLSPOT)) and (ball.x < smallint(vaus.x+vaus.width+BALLSPOT)) then
        begin
        { inverte il vettore vy della velocita' della palla }
        ball.speedy:=-ball.speedy;

        if (vaus.letter=6) and (not ball.launch) then
           begin
           ball.stm:=0;
           ball.launch:=TRUE;
           ball.onvaus:=ball.x-vaus.x;
           end;

        ball_block_sound(300,6);
        { emette il suono d'urto palla-vaus }

        { se la palla urta il cilindretto rosso di sinistra del vaus }
        if (ball.x < smallint(vaus.x+10)) then
           begin
           { inverte il vettore vx, velocita' x della palla }
           ball.speedx:=-ball.speedx;

           { mette nella variabile angle l'angolo di movimento della palla
             piu' un valore casuale di deviasione compreso fra 0 e BALLDEV }
           angle:=get_ball_direction(ball)+random(BALLDEV);

           { re-imposta secondo questo nuovo angolo la direzione di movimento
             della pallina. Comunque sia' l'angolo e' compreso fra 120 e 160
             gradi. Valori superiori o inferiori a questo range vengono auto
             maticamente riportati ai valori estremi del range stesso.
             Ad esempio 175 gradi viene riportato a 160 gradi. }

           set_ball_direction(ball,max(120,min(160,angle)));

           { re-imposta la velocita' della palla, perche' cambiando l'angolo
             di movimento la velocita' viene perduta. }

           set_ball_speed(ball,ball.speed);
           end;

        { del tutto simile al precedente con la differenza che il discorso
          viene fatto per il cilindretto rosso di destra. }

        if (ball.x > smallint(vaus.x + vaus.width - 10)) then
           begin
           ball.speedx:=-ball.speedx;
           angle:=get_ball_direction(ball)-random(BALLDEV);
           set_ball_direction(ball,min(60,max(20,angle)));
           set_ball_speed(ball,ball.speed);
           end;

        end;
     end;

  { se la palla supera il vaus, senza che avvenga una collisione, vale
    a dire: se entrambe, la vecchia e la nuova coordinata y sono maggiori
    della dell'ordinata su cui scorre il vaus e la velocita' y della palla
    e maggiore di 0, cioe' la palla si muove verso il basso, allora la palla
    e' persa e il vaus viene fatto esplodere.}

  if (ball.oldy>VAUS_LINE) and (ball.y>SCRBOT) and (ball.speedy>0) then
     begin
     ball.inplay:=FALSE; { per adesso viene solo settato il flag, palla non
                           piu' in gioco. }
     remove_ball(ball);  { e si cancella la palla dallo schermo }
     end;

  end;

procedure modify_vaus;
  begin
  vaus.oldlen:=vaus.width;
  vaus.width :=playvaus.width;   { larghezza del vaus }
  vaus.height:=playvaus.height;  { altezza del vaus   }
  end;

procedure set_vaus; { setta i parametri iniziali (di partenza) del vaus }
  begin
  vaus.x:=((SCRMAX-SCRMIN) shr 1)-8;
  vaus.y:=VAUS_LINE;

  vaus.oldx:=EMP;                { le vecchie coordinate vengono poste a EMP }
  vaus.oldy:=EMP;                { poiche' il vaus non si e' spostato }
  vaus.iflash:=0;                { Questo viene incrementato ogni 1/70 sec. }
                                 { e quando arriva ad un certo valore viene }
                                 { azzerato e viene incrementato vaus.flash }

  vaus.flash:=0;                 { Da questa var. dipende il colore dei }
                                 { dei bordini del vaus. }
                                 { (che cambia in continuazione) }

  vaus.width :=playvaus.width;   { larghezza del vaus }
  vaus.height:=playvaus.height;  { altezza del vaus   }
  vaus.oldlen:=vaus.width;
  vaus.letter:=EMP;
                                 { entrambi sono contenuti nel file .BTM }

  end;

procedure start_vaus;
  begin
//  mouse_x_limit(SCRMIN,(SCRMAX-vaus.width-1) shl 1);
//  mousemove((SCRMAX-SCRMIN)-16,VAUS_LINE);
  vaus.x:=((SCRMAX-SCRMIN) shr 1)-8;
  vaus.y:=VAUS_LINE;
  
  { set the cursor in the center of the playing area }
  { x=(x1+x2)/2 average between the maximum and minimum }
  { even the mouse pointer (which is not visible) }
  { is moved to the center }

  end;

procedure remove_vaus;
//var y : byte;
begin
  { Remove the vaus and draw the background in its place. }
  
  hlp:=vaus.oldx+row[vaus.oldy];
  
  blitBOX(playscreen.ofs + hlp, vram + hlp, vaus.oldlen, vaus.height);
{
  for y:=vaus.height downto 0 do begin
     blitROW(playscreen.ofs + hlp, vram + hlp, vaus.oldlen);
     
     inc(hlp, 320);
  end;
}
  vaus.oldlen:=vaus.width;
end;


procedure place_vaus;
var
  y: byte;

  begin
  inc(vaus.iflash);               { viene incrementato ogni ciclo (1/70 sec.) }

  if vaus.iflash>SPEEDFLASH then  { se raggiunge il valore SPEEDFLASH... }
     begin
     inc(vaus.flash);             { viene incrementato vaus.flash  }
     vaus.iflash:=0;              { e viene riazzerato vaus.iflash }
     end;

  if vaus.flash>10 then vaus.flash:=0;
  { There are 10 different colors that the edges of the vaus can take on }
  { after which the cycle repeats from the beginning (i.e., from 0) }

  { Draw the vaus, making sure that the borders (which have color 255 }
  { in the .BTM drawing) are replaced by the color specified by }
  { flash[vaus.flash], where obviously flash[] is a function dependent on }
  { vaus.flash mentioned above. For example, flash[2]:=211 (see at the beginning in the }
  { declaration of constants. }

  hlp:=0;
 
  for y:=0 to vaus.height-1 do
     begin
     { this multiplication is done here so as not to repeat it }
     { vaus.width times }
     //hlp:=y * vaus.width;
     //memzerocpy(playvaus.map[cnt], screen[vaus.x+row[y+vaus.y]], vaus.width);
     blitZERO(playvaus.ofs + hlp, vram + vaus.x+row[y+vaus.y], vaus.width, 1);

     if (y>=2) and (y<(vaus.height-2)) then
        begin
        //screen[vaus.x+row[y+vaus.y]]:=FLASH[vaus.flash];
        putBYTE(vram + vaus.x+row[y+vaus.y], FLASH[vaus.flash]);

        //screen[vaus.x+vaus.width-1+row[y+vaus.y]]:=FLASH[vaus.flash];
        putBYTE(vram + vaus.x+vaus.width-1+row[y+vaus.y], FLASH[vaus.flash]);
        end;
	
      inc(hlp, vaus.width);
     end;
  end;


{ moves the vaus to coordinates x,y }
procedure move_vaus(x,y : smallint);
  begin

  { se le coordinate oldx,oldy sono valide allora bisogna cancellarlo }
  { da quella posizione }
  if(vaus.oldx <> EMP) and (vaus.oldx <> vaus.x) or (vaus.width <> vaus.oldlen) then
     remove_vaus;

  vaus.oldx:=vaus.x; { le nuove coordinate diventano le vecchie }
  vaus.oldy:=vaus.y;

  { le coordinate x,y diventano le nuove }
  { viene eseguito un clipping delle coordinate, cioe' se i valori ad }
  { esempio sono troppo alti, vengono settate al massimo valore accettabile }
  { analogamente per il minimo }

  vaus.x:=max(SCRMIN,min(x,(SCRMAX-vaus.width)));
  vaus.y:=max(SCRTOP,min(y,(SCRBOT-vaus.height)));

  place_vaus;  { chiama la funzione placevaus di cui sopra }
  end;


{ remove a brick from the screen }
procedure remove_block(xa,ya : byte);
var
    x,y, i: byte;

    xs,ys : byte;
    yh : word;
    cl, shadow: byte;

begin

    xs:=(xa shl 4)+9;      { si calcola le coordinate sullo schermo }
    ys:=(ya shl 3)+22;     { del mattoncino es. 0,0 ---schermo---> 9,22 }

    for y:=0 to 7 do
        begin
        yh:=pattern.width*mody[ys+y]; { calcola la coord. y relativa alla }
                                      { mattonella di sfondo che deve rim-  }
                                      { piazzare il mattoncino che non c'e' }
                                      { piu' }
        { il mattoncino viene rimpiazzato col fondale, tuttavia il fondale }
        { potrebbe essere inscurito da un ombra proiettata da un altro }
        { mattoncino }

	hlp := row[y+ys] + xs;
	
	i:=0;
	
	vbxe_ram.Position:=playscreen.ofs + hlp;
	vbxe_Ram.ReadBuffer(scanline, 16);

	vbxe_ram.Position:=pattern.ofs + yh;
	vbxe_Ram.ReadBuffer(scanline2, 40);

        for x:=0 to 15 do
            if (x+xs) < SCRMAX then
               begin
               { calocla l'eventuale ombra proiettata da un altro mattoncino }
               { shadow:=128 nessuna ombra, shadow:=0 c'e' l'ombra }

               //shadow:=playscreen.map[x+xs+row[y+ys]] and 128;
               //shadow:=getBYTE(playscreen.ofs + hlp + x) and $80;
               shadow:=scanline[x] and $80;

               { prende il pixel di sfondo e ci aggiunge l'ombra se necessario }
               //cl:=(pattern.map[modx[x+xs]+yh] and 127) or shadow;
               //cl:=(getBYTE(pattern.ofs + modx[x+xs]+yh) and $7f) or shadow;
               cl:=(scanline2[modx[x+xs]] and $7f) or shadow;

	       tmp[i] := cl;
	       inc(i);

               { dopodiche' mette il colore sia sullo schermo della VGA sia }
               //screen[x+xs+row[y+ys]]:=cl;
               //putBYTE(vram + hlp + x, cl);

               { sullo schermo ausiliario dove sono presenti solo gli oggetti }
               { statici e non quelli in movimento tipo pallina o vaus.}
               //playscreen.map[x+xs+row[y+ys]]:=cl;
               //putBYTE(playscreen.ofs + hlp + x, cl);
               end;

	blitTMP(vram + hlp, i);
	//blitTMP(playscreen.ofs + hlp, i);
	blitROW(vram + hlp, playscreen.ofs + hlp, i);

        end;

    { In any case, when the brick disappears, its shadow must also disappear }
    { The shadow is nothing more than a small rectangle of the same size as the }
    { brick but shifted 8 pixels on the x-axis and 4 }
    { pixels on the y-axis. In other words, the edge of the shadow coincides with }
    { the center of the brick }

    for y:=ys+4 to ys+12 do begin

	hlp := row[y] + xs;

	vbxe_ram.Position:=playscreen.ofs + hlp;
	vbxe_Ram.ReadBuffer(TMP, 32);

        for x:=xs+8 to xs+24 do

            { It is necessary to check that the coordinates are not greater than }
            { those of the playing field because in that case there is no }
            { shadow to remove since the last brick casts a }
            { partial shadow that is not reflected on the side wall }
            { Therefore, there is no shadow to remove on the side wall }

            { Lo stesso discorso non viene fatto per il minimo, poiche' }
            { l'ombra e' sempre piu' a destra e piu' in basso del mattone }
            { che la proietta, quindi nessun mattoncino puo' proiettare un }
            { ombra sulla parete di sinistra. Nondimeno nessun mattoncino }
            { e' talmente basso da proiettare un ombra sul vaus. }

            { Dunque il caso da tenere in considerazione e' solo x<SCRMAX }

            if x < SCRMAX then
               begin
               { prende il colore di sfondo e toglie l'ombra }
               //cl:=playscreen.map[x+row[y]] or 128;
               //cl:=getBYTE(playscreen.ofs + hlp + x) or $80;
               TMP[x - xs]:=TMP[x - xs] or $80;

	       //tmp[i] := cl;
	       //inc(i);

               { e lo memorizza sia sullo schermo fisico ...}
               //screen[x+row[y]]:=cl;
               //putBYTE(vram + x+row[y], cl);

               { che su quello virtuale (cioe' quello che tiene solo }
               { gli oggetti fissi }
               //playscreen.map[x+row[y]]:=cl;
               //putBYTE(playscreen.ofs + x+row[y], cl);
               end;

	//inc(hlp, xs+8);

	blitTMP(vram + hlp, 32);
	//blitTMP(playscreen.ofs + hlp, 32);
	blitROW(vram + hlp, playscreen.ofs + hlp, 32);

    end;

end;


procedure place_block(xa,ya,block : byte);
var
    x,y, i: byte;
    
    xs,ys : word;
    cl,cl2: byte;
    shadow: byte;

begin

    xs:=(xa shl 4)+9;   { calcola le coordinate sullo schermo relativa }
    ys:=(ya shl 3)+22;  { al mattoncino xa,ya }

    for y:=0 to 7 do begin

        hlp := xs+row[ys+y];
	
	vbxe_ram.Position:=playscreen.ofs + hlp;
	vbxe_ram.ReadBuffer(scanline, 16);

        for x:=0 to 15 do
            begin
            { check if any bricks are at the specified coordinates }
            { cast a shadow }
            //shadow:=playscreen.map[xs+x+row[ys+y]] and 128;
            //shadow:=getBYTE(playscreen.ofs + hlp + x) and $80;
            shadow:=scanline[x] and $80;

            if (y<7) and (x<15) then
                begin
                { if it is the inside of the brick, draw it in the }
                { color specified in block }

                cl:=(COLORBLOCK[(block-1) and 15] and $7f) or shadow;

		tmp[x] := cl;

                //screen[xs+x+row[ys+y]]:=cl;
                //putBYTE(vram + hlp, cl);

                //playscreen.map[xs+x+row[ys+y]]:=cl;
                //putBYTE(playscreen.ofs + hlp, cl);
                end
            else
               begin
               { if the coordinates are on the right or bottom edge, }
               { draw the pixels in black }
	       
	       tmp[x] := shadow;

               //screen[xs+x+row[ys+y]]:=shadow; { sarebbe shadow or 0 }
               //putBYTE(vram + hlp, shadow);

               //playscreen.map[xs+x+row[ys+y]]:=shadow; {...quindi shadow }
               //putBYTE(playscreen.ofs + hlp, shadow);
               end;
	       
            end;

	blitTMP(vram + hlp, 16);    
	//blitTMP(playscreen.ofs + hlp, 16);
	blitROW(vram + hlp, playscreen.ofs + hlp, 16);

    end;


    { now draw the shadow of the brick }
    for y:=ys+4 to ys+12 do begin
    
  //      i:=0;
  
        hlp := row[y] + xs;

	vbxe_ram.Position:=playscreen.ofs + hlp;
	vbxe_ram.ReadBuffer(TMP, 32);

        for x:=xs+8 to xs+24 do
            if x<SCRMAX then  { check as in remove_block that the coordinates }
                              { are not beyond the right wall, because }
                              { the shadow is not cast on that wall }
               begin

               { preleva il pixel x,y dallo schermo e ci proietta sopra }
               { l'ombra. }
               //cl:=playscreen.map[x+row[y]] and 127;
               //cl:=getBYTE(playscreen.ofs + hlp + x) and $7f;
               TMP[x - xs]:=TMP[x - xs] and $7f;
	       
//	       tmp[i] := cl;
//	       inc(i);

               { dopo di che lo rimette sullo schermo fisico... }
               //screen[x+row[y]]:=cl;
               //putBYTE(vram + hlp, cl);

               { e su quello virtuale }
               //playscreen.map[x+row[y]]:=cl;
               //putBYTE(playscreen.ofs + hlp, cl);
               end;

//	inc(hlp, xs+8);

	blitTMP(vram + hlp, 32);
	//blitTMP(playscreen.ofs + hlp, 32);
	blitROW(vram + hlp, playscreen.ofs + hlp, 32);

    end;


    if block>8 then { but if the block is gray (=9) or brown (=10) ... }
       begin
       cl2:=0;
       if (block and 15)=9 then
          begin
          cl2:=202; { The color of the brick is gray. }
          wall[byte(xa+ya*16)]:=9+(GRAYDOWN shl 4); { and the number of bricks is 9+16*n }
          { where n+1 is the number of hits needed to knock it down }
          { e.g. wall[1,2]=9+(1*16)=25 means that the brick at }
          { coordinates 1,2 falls if hit twice }
          end

       else if block=10 then cl2:=201;
       { if the block is brown, the color is no. 201 }

       { draw the top edge of the brick }
       for y:=0 to 6 do
           begin
	   
	   hlp := xs+row[y+ys];

           { takes the pixel xs,y+ys from the screen, adds a shadow to it }
           { i.e., makes the color darker }
           //cl:=playscreen.map[xs+row[y+ys]] and 128;
           cl:=getBYTE(playscreen.ofs + hlp) and $80;

           cl2:=(cl2 and 127) or cl;

           { ... e lo rimette sullo schermo fisico }
           //screen[xs+row[ys+y]]:=cl2;
           putBYTE(vram + hlp, cl2);

           { ... e su quello virtuale }
           //playscreen.map[xs+row[ys+y]]:=cl2;
           putBYTE(playscreen.ofs + hlp, cl2);
           end;

       hlp := xs+row[ys];

       vbxe_ram.Position:=playscreen.ofs + hlp;
       vbxe_ram.ReadBuffer(scanline, 15);

       { draw the right edge of the brick }
       for x:=0 to 14 do
           begin

           { comments similar to above }
           //cl:=playscreen.map[xs+x+row[ys]] and 128;
           //cl:=getBYTE(playscreen.ofs + hlp + x) and $80;
           cl:=scanline[x] and $80;

           tmp[x]:=(cl2 and $7f) or cl;

           //screen[xs+x+row[ys]]:=cl2;
           //putBYTE(vram + hlp, cl2);

           //playscreen.map[xs+x+row[ys]]:=cl2;
           //putBYTE(playscreen.ofs + hlp, cl2);
           end;
	   
	blitTMP(vram + hlp, 15);
	//blitTMP(playscreen.ofs + hlp, 15);
	blitROW(vram + hlp, playscreen.ofs + hlp, 15);

       end;
       
end;


procedure put_wall;  { displays the wall contained in wall[x,y] on the screen }
var
    x,y : byte;

    begin
    for y:=0 to 14 do
        for x:=0 to 12 do
            if wall[byte(x+y*16)] <> 0 then place_block(x,y,wall[byte(x+y*16)]);
    end;


procedure set_wall;             { imposta il muro }
var x,y,wl  : byte;
//    name    : string;

    begin
    remain_blk:=0;                { sono i blocchi da distruggere }
    wl:=score.wall_n[cur_player]; { questo e' il muro a cui e' fermo il }
                                  { giocatore cur_player }

    for y:=0 to 14 do             { conta i blocchi distruttibili }
        for x:=0 to 12 do         { cioe' il blocco deve essere <>0 e <>10 }
                                  { poiche' 0 = nessun blocco, 10 = marrone }

            if (wall[byte(x+y*16)]<>0) and (wall[byte(x+y*16)]<>10) then inc(remain_blk);

    wl:=(wl-1) mod PATNUMBER;

    case wl of
     0: pattern := pattern0;
     1: pattern := pattern1;
     2: pattern := pattern2;
     3: pattern := pattern3;
     4: pattern := pattern4;
    end;

(*
    name:=chr(48+((wl-1) mod PATNUMBER));
    name:=concat('PATTERN', name);
    name:=concat(name, '.BTM');

//    name:='PATTERN'+chr(48+((wl-1) mod PATNUMBER))+'.BTM';
    { name e' una stringa che contiene il nome del file di sfondo da caricare }

    loadBTM(name,pattern,FALSE);
    if not success then
       fatal_error('Some Background files seems to be missing');
*)
    { e quindi carica il file in questione }
    end;


{ takes the coordinates of two points as input and calculates }
{ the points where the brick grid intersects the segment      }
{ connecting the two points.                                  }

{ the points of intersection can be 1 or 2 }

function split_line(var x1,y1,x2,y2 : smallint) : byte;
var
    x,y,
    xk,yk,
    xj,yj,
    xh,yh,
    xn,yn,
    xp1,yp1,
    xp2,yp2,
    xp,yp : smallint;

    collision: byte;

    begin
    inc(x1,16);         { incrementa le coordinate di tutti i punti }
    inc(y1,24);         { per evitare che nel corso delle operazioni }
    inc(x2,16);         { qualche coordinata diventi negativa }
    inc(y2,24);         { prima di terminare la proc. li rimette a posto }

    collision:=0;       { number of intersections between segment and grid }

    xp1:=x1 shr 4;      { si calcola all'interno di quale mattoncino stanno }
    yp1:=y1 shr 3;      { i due punti in questione }
    xp2:=x2 shr 4;
    yp2:=y2 shr 3;

    xk:=x1;             { copia temporaneamente le coord. dei due punti }
    yk:=y1;             { in due vettori in modo da poter operare liberamente }
    xj:=x2;             { le coord. iniziali vengono passate per indirizzo }
    yj:=y2;             { e quindi non devono perdersi i valori }

    xh:=x1;
    yh:=y1;
    xn:=x2;
    yn:=y2;


    { If this “if” is true, it means that there is a bug in the program   }
    { and therefore the game quits immediately, reporting the error.      }
    { This error occurs easily if MAXSPEED is set to >> 2000              }

    if (abs(x1-x2)>16) or (abs(y2-y1)>8) then
       fatal_error(err1);


    if (xp1<>xp2) or (yp1<>yp2) then   { se i due punti non coincidono... }
       begin
       if (yp1<>yp2) then    { se i due punti hanno diversa y }
          begin
          collision:=collision or 1; { il bit piu' basso viene messo a 1 }

          while ((yn and 7)<>0) and ((yn and 7)<>7) do
            begin
            x:=(xh+xn) shr 1; { dopo di che continua a dividere il segmento }
            y:=(yh+yn) shr 1; { (x1,y1)-(x2,y2) finche non trova un inter- }
                              { sezione con un reticolo }
            yp:=y shr 3;

            if yp=yp1 then    { dei tre punti (due sono gli estremi del }
               begin          { segmento) ne scarta uno con lo stesso   }
               xh:=x;         { principio del teorema di Weierstrass.   }
               yh:=y;
               end;

            if yp=yp2 then    { Il punto di mezzo sostituisce cioe' uno }
               begin          { dei due estremi in modo che il segmento }
               xn:=x;         { sia ancora a cavallo del reticolo.      }
               yn:=y;
               end;
            end;

          end;

       if (xp1<>xp2) then     { if the two points have different x-coordinates ...}
          begin
          collision:=collision or 2;  { in this case, set the second bit }

          while ((xj and 15)<>0) and ((xj and 15)<>15) do
            begin
            x:=(xk+xj) shr 1;         { and the steps are similar for x }
            y:=(yk+yj) shr 1;

            xp:=x shr 4;

            if xp=xp1 then
               begin
               xk:=x;
               yk:=y;
               end;

            if xp=xp2 then
               begin
               xj:=x;
               yj:=y;
               end;

            end;

          end;


       { The values are reassigned to the extremes depending on which }
       { portions of the program have been executed above             }

       if collision=1 then       { i.e., the two x's are equal, the two y's are different }
          begin
          x2:=xn;
          y2:=yn;
          end
       else if collision=2 then  { the two x are different, the two y are the same}
          begin
          x2:=xj;
          y2:=yj;
          end
       else if collision=3 then  { both the two x's and the two y's are different}
          begin
          x1:=xj;         { in this case, there are 2 intersections   }
          y1:=yj;         { and the procedure returns them in (x1,y1) }
          x2:=xn;         { (x2,y2).                                  }
          y2:=yn;
          end;

       end

    else fatal_error(err2);
    { otherwise something went wrong! }

    dec(x1,16);   { restore the old coordinates }
    dec(y1,24);
    dec(x2,16);
    dec(y2,24);


    x1:=min(207,max(0,x1));
    x2:=min(207,max(0,x2));

    { For y, coordinates <0 and >120 are not cut because the matrix   }
    { containing them is virtually longer for safety reasons, and for }
    { invalid coordinates, no brick to collide with is found.         }
    { Clipping in this case is simpler.                               }

    split_line:=collision;
    end;

{ Consider hit the xb,yb block: if it is a normal block it takes it down,  }
{ if it is a gray that withstands multiple hits it decreases its strength. }
{ If the block is not knocked down then it makes it shimmer.               }

procedure shoot_block(xb,yb : byte; var ball : BALLTYPE);
var i: byte;
    begin
    { Controlla che le coordinate del blocco siano numeri validi... }
    if (xb>=0) and (xb<=12) and (yb>=0) and (yb<=14) then
       begin
       
       i:=xb+yb*16;

       if wall[i]<>0 then { ... che ci sia un blocco da colpire... }
          begin
          if wall[i]<10 then { se il blocco puo' essere abbattuto... }
             begin
             remove_block(xb,yb); { ..lo toglie dallo schermo }
             dec(remain_blk);     { ..decrementa il numero di blocchi che restano }

             { Incrementa lo SCORE del giocatore attuale a seconda }
             { del blocco colpito (i punti sono nell'array in SCORE_WALL) }
             inc(score.player[cur_player],SCORE_WALL[wall[i]]);

             inc(lett.incoming,random(LETTER_PROB));

             lett.nextx:=(xb shl 4)+9;
             lett.nexty:=((yb+1) shl 3)+22;
             lett.nexttype:=random_letter_drop;

             wall[i]:=0;              { il blocco viene cancellato        }
             ball_block_sound(440,3); { emette un LA (nota musicale)      }
             ball.sbd:=0;             { azzera il contatore di deviazione }
             ball.brwhit:=0;          { e il cont. di dev. di emergenza   }
             end

          else    { se il blocco e marrone, o un grigio che non cade subito }
             begin
             if (wall[i] and 15)=9 then { ...se e' grigio... }
                begin
                ball.brwhit:=0;         { azzera il cont. di dev. di emergenza }
                dec(wall[i],16);        { decrementa la resistenza del blocco  }

                ball_block_sound(370,4);{ Emette un Fa# (nota musicale)    }
                shine(xb,yb);           { e imposta il luccichio del blocco }
                end
             else
                begin
                inc(ball.brwhit); { incrementa il cont. di dev. di emergenza }
                shine(xb,yb);     { imposta il luccichio }

                ball_block_sound(200,7); { ed emette una nota piuttosto bassa }
                end;
             end;
          end;
       end;
    end;

{ Simile a quella prima ma per la collisione fire_blocco }
procedure shoot_block_with_fire(xb,yb : byte);
var i: byte;
    begin
    if (xb>=0) and (xb<=12) and (yb>=0) and (yb<=14) then
       begin
       
       i:=xb+yb*16;
       
       if wall[i]<>0 then    { ... che ci sia un blocco da colpire... }
          begin
          if wall[i]<10 then { se il blocco puo' essere abbattuto... }
             begin
             remove_block(xb,yb); { ..lo toglie dallo schermo }
             dec(remain_blk);     { ..decrementa il numero di blocchi che restano }
             inc(score.player[cur_player],SCORE_WALL[wall[i]]);
             wall[i]:=0;              { il blocco viene cancellato        }
             ball_block_sound(440,3); { emette un LA (nota musicale)      }
             end

          else    { se il blocco e marrone, o un grigio che non cade subito }
             begin
             if (wall[i] and 15)=9 then { ...se e' grigio... }
                begin
                dec(wall[i],16); { decrementa la resistenza del blocco  }
                ball_block_sound(370,4);{ Emette un Fa# (nota musicale)    }
                shine(xb,yb);           { e imposta il luccichio del blocco }
                end
             else
                begin
                shine(xb,yb);     { imposta il luccichio }
                ball_block_sound(200,7); { ed emette una nota piuttosto bassa }
                end;
             end;
          end;
       end;
    end;


procedure ball_hit_block(var ball : BALLTYPE);
var 

    x,y: byte;

    lx,ly,
    xb,yb    : shortint;

    ox,oy,
    nx,ny,
    mx,my,
    angle,
    myx,myy  : smallint;

    f1,f2    : word;
    a,b      : byte;

    emergency,
    mimax,
    deflect,
    around,
    collision,
    touch    : byte;

    adjw     : array[0..3,0..3] of byte;

    begin
    emergency:=EMP;    { the emergency rebound indicator }

    nx:=ball.x-9;      { nx,ny have the coordinates of the ball with respect to }
    ny:=ball.y-22;     { the origin fixed in the Northwest corner of }
                       { field of play (within which the ball moves). }

    ox:=ball.oldx-9;   { idem per le vecchie coordinate, l'origine e'   }
    oy:=ball.oldy-22;  { quindi il punto dello schermo (9,22).          }

    xb:=nx shr 4;     { xb,yb sono le coordinate del blocco (eventualmente  }
    yb:=ny shr 3;     { ipotetico) su cui si trova ora la pallina.          }
                      { Ricordarsi che (0,0) e' il blocco in altro a destra }


    if wall[byte(xb)+byte(yb)*16]<>0 then  { ...if the block is not hypothetical but exists }
       begin
       collision:=split_line(ox,oy,nx,ny);
       { calculates the intersection of the segment connecting the old and }
       { new coordinates. “Collision” contains a value that depends on the }
       { type of intersections found between the segment and the grid of   }
       { blocks.                                                           }

       if collision=3 then     { if two collisions have occurred... }
          begin
          lx:=ball.oldx-ox-9;  { si calcola la distanza della vecchia }
          ly:=ball.oldy-oy-22; { coordinata dal punto di intersezione 1 }

          mx:=ball.oldx-nx-9;  { e dal punto di intersezione 2 }
          my:=ball.oldy-ny-22;

	  a:=abs(lx); b:=abs(ly);

          f1:=a*a+b*b;         { indi sceglie fra i due il punto di }

	  a:=abs(mx); b:=abs(my);

          f2:=a*a+b*b;         { intersezione piu' vicino alle vecchie coord. }

          if (f1 < f2) then    { f1 e f2 sono il quadrato del modulo del }
                               { vettore distanza (vedi sopra) }

             { Consider the case where the closest intersection is number 1. }

             begin
             xb:=min(12,max(word(ox) shr 4,0));  { Vengono assegnate le coord. }
             yb:=(byte(oy+24) shr 3)-3;        { del blocco relative a tale  }
                                           { intersezione.               }

             if wall[byte(xb)+byte(yb)*16]=0 then  { Se non vi e' alcun blocco   }
                begin
                xb:=min(12,max(0,word(nx) shr 4)); { Allora l'urto avviene sull' }
                yb:=(byte(ny+24) shr 3)-3;       { altra intersezione. La n.2  }
                end
             else
                begin                        { Se invece il blocco esiste  }
                nx:=ox;                      { allora alle nuove coord. si }
                ny:=oy;                      { assegna il punto di inter-  }
                                             { sezione contenuto nelle vec-}
                                             { chie.                       }
                end;
             end
          else
             begin
             { If it is the second intersection closest to the }
             { old coordinates, proceed in the same way.       }

             xb:=min(12,max(0,word(nx) shr 4)); { Si calcolano le coord. del blocco }
             yb:=(byte(ny+24) shr 3)-3;       { sull'intersezione nx,ny (la seconda) }

             if wall[byte(xb)+byte(yb)*16]=0 then     { Se il blocco non c'e'... }
                begin
                nx:=ox;                   { allora l'intersezione valida e' }
                ny:=oy;                   { l'altra, e si procede... }

                xb:=min(12,max(0,word(nx) shr 4)); { ...riassegnando alle nuove }
                yb:=(byte(ny+24) shr 3)-3;       { coord. l'intersezione n.1  }
                end;
             end;

          end;

       ball.x:=nx+9;    { Le nuove coordinate della palla sono quelle      }
       ball.y:=ny+22;   { contentenute nelle variabili nx,ny, ritraslando  }
                        { gli assi. nx,ny avevano i relativi assi centrati }
                        { in (9,22).                                       }

       shoot_block(xb,yb,ball);  { breaks down the block in question }

       x:=(nx and 15) shr 1;     { si calcola il punto d'urto della palla }
       y:=(ny and 7);            { rispetto al mattoncino.                }

       { Dividendo per 2 la coord. x dell'urto si ottiene una sezione d'urto }
       { su di un mattone quadrato invece che rettangolare che semplifica in }
       { seguito i calcoli. Il mattone e' infatti di 16x8 pixel dividendo    }
       { per 2 diventa di 8x8 pixel, e i calcoli sulle diagonali sono piu'   }
       { semplici. }


       { Se l'urto non avviene su uno dei bordi del mattone allora vuole }
       { dire che qualcosa e' andato storto. In teoria non dovrebbe mai  }
       { verificarsi.                                                    }
       if (x<>0) and (x<>7) and (y<>0) and (y<>7) then
          fatal_error(err3);


       { These are the values assumed by EMERGENCY depending on the point of impact }

       {                     5     1     8                               }
       {                      -----------                                }
       {                    2 | mattone | 4                              }
       {                      -----------                                }
       {                     6     3     7                               }



       { Se la palla urta il bordo superiore del mattoncino... }

       if (y<x) and (x<byte(7-y)) then
          begin
          ball.speedy:=-ball.speedy;   { Si inverte la coord. y della vel. }
          emergency:=1;                { e segna l'eventiale punto di contatto }
          end;

       { ...il bordo inferiore... }
       if (byte(7-y) < x) and (x<y) then
          begin
          ball.speedy:=-ball.speedy;   { inverte la y della vel. }
          emergency:=3;
          end;

       { ...il bordo sinistro... }
       if (x<y) and (y<byte(7-x)) then
          begin
          ball.speedx:=-ball.speedx;   { inverte la x della vel. }
          emergency:=2;
          end;

       { ... e quello destro ... }
       if (byte(7-x)<y) and (y<x) then
          begin
          ball.speedx:=-ball.speedx;   { inverte la x della vel. }
          emergency:=4;
          end;

       { ... se invece avviene su uno dei quattro spigoli ... }
       if (x=y) or (x=byte(7-y)) then
          begin
          deflect:=$00;
          touch:=0;

          { touch assume valori diversi a seconda dello spigolo         }
          { Segue la tabella (per es. 0 = angolo in alto a sinistra)    }

          { 0 1 }
          { 2 3 }

          if x>4 then touch:=touch or 1;
          if y>4 then touch:=touch or 2;


          { Here, fill a 3x3 matrix with 1s or 0s depending on whether }
          { there are other bricks around the blocked block or not     }

          { The left and right edges of the playing field are considered }
          { as indestructible bricks in this case.                       }

          for lx:=-1 to 1 do
              for ly:=-1 to 1 do
                  begin
                  mx:=max(min(byte(xb)+byte(lx),12),0); { When referring to x,      }
                  my:=yb+ly;                { the coordinate            }
                                            { must be between 0 and 12. }
  

                  if (shortint(xb+lx)<0 ) or
                     (shortint(xb+lx)>12) or
                     (wall[byte(mx)+byte(my)*16]<>0) then
                        adjw[byte(lx+1),byte(ly+1)]:=1   { There are bricks }
                  else
                     adjw[byte(lx+1),byte(ly+1)]:=0;     { There are no bricks }

                  end;

          { Around contains a value that represents the state of         }
          { the bricks surrounding the brick that was hit.               }

          {        -------------                      }
          {        | 1 | 2 | 4 |                      }
          {        -------------                      }
          {        |128| U | 8 |   U = bumped brick   }
          {        -------------                      }
          {        | 64| 32| 16|                      }
          {        -------------                      }

          { Example:                                                      }
          { if bricks 1, 2, and 128 are located around U, the valu e'     }
          { of around is 1+2+128=131.                                     }
	  
          around:=adjw[0,0] or (adjw[1,0] shl 1) or
                               (adjw[2,0] shl 2) or (adjw[2,1] shl 3) or
                               (adjw[2,2] shl 4) or (adjw[1,2] shl 5) or
                               (adjw[0,2] shl 6) or (adjw[0,1] shl 7);

          { Deflect contains a value that represents in hexadecimal       }
          { the changes to be made to vx (first hexadecimal digit)        }
          { and y (second hexadecimal digit).                             }
	  { According to the following table.                             }

          { 0 = coordinate unchanged }
          { 1 =     ‘’     negative  }
          { 2 =     ‘’     positive  }
          { 3 =     ‘’     inverted  }

          { Example: 
	  {     Deflect:=$13 means set vx negative and invert vy          }
          {     Deflect:=$20 means set vx positive and leave vy unchanged }
	  
          { ------------------------------------------------------------- }

          { The combinations of the edge hit, the bricks around it,       }
	  { and the resulting direction of the ball are calculated        }
	  { on a case-by-case basis.                                      }

          { The logical AND means that only bricks whose sum              }
          { equals the number that follows are considered.                }

          { For example, “and 131” means consider only the bricks 1+2+128 }
          { the others, if there are any, do not matter.                  }

          if touch=0 then       { upper left corner }
             begin
             if (around and 131)=0   then deflect:=$11;
             if (around and 131)=1   then deflect:=$33;
             if (around and 131)=2   then deflect:=$10;
             if (around and 131)=3   then deflect:=$12;
             if (around and 131)=128 then deflect:=$01;
             if (around and 131)=129 then deflect:=$21;
             if (around and 131)=130 then deflect:=$11;

             emergency:=5;
             shoot_block(xb-1,yb-1,ball);
             end;

          { "and 14" sono i mattoni 2+4+8, gli altri non importa }

          if touch=1 then       { upper right corner }
             begin
             if (around and 14)=0    then deflect:=$21;
             if (around and 14)=2    then deflect:=$20;
             if (around and 14)=4    then deflect:=$33;
             if (around and 14)=6    then deflect:=$22;
             if (around and 14)=8    then deflect:=$01;
             if (around and 14)=10   then deflect:=$21;
             if (around and 14)=12   then deflect:=$11;

             emergency:=8;
             shoot_block(xb+1,yb-1,ball);
             end;

          if touch=2 then       { Spigolo in basso a sinistra }
             begin
             if (around and 224)=0    then deflect:=$12;
             if (around and 224)=32   then deflect:=$10;
             if (around and 224)=64   then deflect:=$33;
             if (around and 224)=96   then deflect:=$11;
             if (around and 224)=128  then deflect:=$02;
             if (around and 224)=160  then deflect:=$12;
             if (around and 224)=192  then deflect:=$22;

             emergency:=6;
             shoot_block(xb-1,yb+1,ball);
             end;

          if touch=3 then       { Spigolo in basso a destra   }
             begin
             if (around and 56)=0    then deflect:=$22;
             if (around and 56)=8    then deflect:=$02;
             if (around and 56)=16   then deflect:=$33;
             if (around and 56)=24   then deflect:=$12;
             if (around and 56)=32   then deflect:=$20;
             if (around and 56)=40   then deflect:=$22;
             if (around and 56)=48   then deflect:=$21;

             emergency:=7;
             shoot_block(xb+1,yb+1,ball);
             end;

          { La prima cifra hex (esadecimale) viene messa in myx }
          { e la seconda in myy. }

          myx:=deflect shr 4;
          myy:=deflect and 15;

          if myx=1 then ball.speedx:=-abs(ball.speedx);
          if myx=2 then ball.speedx:= abs(ball.speedx);
          if myx=3 then ball.speedx:=-    ball.speedx ;

          if myy=1 then ball.speedy:=-abs(ball.speedy);
          if myy=2 then ball.speedy:= abs(ball.speedy);
          if myy=3 then ball.speedy:=-    ball.speedy ;

          end;

       end;


    { In case the number of indestructible bricks bumped consecutively   }
    { before bumping a brick of another type exceeds a certain threshold }

    if ball.brwhit>MAXBRWHIT then
       begin
       { Se emergency e' rimasto a EMP significa che qualcosa e' andato storto }
       if emergency=EMP then fatal_error(err4);

       mimax:=EMERG_DEV[emergency]; { Altrimenti si calcola la deviazione }
                                    { massima e minima del mattoncino.    }

       { e a seconda di quale spigolo viene urtato e di come sono i mattoni }
       { attorno a tale spigolo, la deviazione viene modificata. }

       { Per quanto il rimbalzo finale possa essere strano, questo controllo }
       { viene fatto per evitare che la palla si incastri in un loop infinito }

       { ovviamente il caso vale per i mattoni indistruttibili perche' gli }
       { altri prima o poi cadono e quindi non possono bloccare la palla   }
       { per un tempo infinito.                                            }

       { Ogni cifra hex di mimax esprime un angolo a multipli di 90 gradi  }
       { la prima cifra e' l'angolo minimo, la seconda quello massimo.     }

       { Es. MIMAX:=$03; singifica angolo minimo 0*90 = 0 gradi, angolo max }
       { 3*90 = 270 gradi, e cosi' via...                                   }

       { una scrittura del tipo "mimax:=mimax and $0f or 10" significa }
       { metti a 1 la prima cifra di mimax indipendentemente da quanto }
       { vale adesso, lasciando inalterata la seconda.                 }

       { Analogo il ragionamento per "... and $f0 or $03" che agisce   }
       { sulla seconda cifra invece che sulla prima...                 }

       case emergency of

         5: begin
            if adjw[1,0]=0 then mimax:=(mimax and $0f) or $00;
            if adjw[0,1]=0 then mimax:=(mimax and $f0) or $03;
            end;

         6: begin
            if adjw[0,1]=0 then mimax:=(mimax and $0f) or $10;
            if adjw[1,2]=0 then mimax:=(mimax and $f0) or $04;
            end;

         7: begin
            if adjw[1,2]=0 then mimax:=(mimax and $0f) or $20;
            if adjw[2,1]=0 then mimax:=(mimax and $f0) or $05;
            end;

         8: begin
            if adjw[2,1]=0 then mimax:=(mimax and $0f) or $30;
            if adjw[1,0]=0 then mimax:=(mimax and $f0) or $06;
            end;

         end;


       repeat

          lx:=90*(mimax shr 4);    { la prima cifra di mimax viene posta in }
          mx:=90*(mimax and 15);   { lx e la seconda in mx.                 }

          angle:=random(smallint(mx-lx))+lx; { L'angolo e' una variabile casuale fra  }
                                   { lx e mx }

       until ((angle mod 90)>30) and ((angle mod 90)<60);
       { e questo ciclo si ripete finche' la palla ha un inclinazione }
       { compresa fra i 30 e i 60 gradi piu' multipli di 90 gradi.    }

       set_ball_direction(ball,angle mod 360);
       set_ball_speed(ball,ball.speed);

       ball.brwhit:=0; { Reset emergency counter }
       end;

    end;


{ Draw the backdrop on the playing field }
procedure fill_picture_with_pattern(var patt : BTMTYPE);
var yb: word;
    x, y, cl, shadow, k: byte;

    begin
    { It computes a priori all values of x mod patt.width            }
    { patt.width = width of the small square defining the background }

    for y:=0 to 255 do begin
        modx[y]:=y mod patt.width;

    { analogamente per y mod patt.height }

//    for y:=0 to 255 do
        mody[y]:=y mod patt.height;
    end; 

    { Runs the main loop and the secondary loop filling the }
    { screen with as many background squares as needed      }

    for y:=SCRTOP-2 to SCRBOT-2 do
        begin

        yb:=mody[y]*patt.width;
	
	vbxe_ram.Position:=patt.ofs + yb;
	vbxe_ram.ReadBuffer(scanline, 40);

        k:=0;
        for x:=SCRMIN-1 to SCRMAX-1 do
            begin
            //cl:=patt.map[modx[x]+yb]; { Takes the pixel from the background }
            //cl:=getBYTE(patt.ofs + modx[x]+yb);
	    
	    cl:=scanline[ modx[x] ];

            shadow:=128;              { Shadow = 128 -> shadow not present }

            { Fa l'ombra sul fianco sinistro e superiore dello schermo  }
            { E' l'ombra proiettata dal bordo metallico sullo sfondo di }
            { gioco.                                                    }
            if (y<16) or (x<18) then shadow:=0; { Shadow=0 -> ombra presente }

            { Disegna il pixel sullo schermo con l'eventuale ombra }
            //playscreen.map[x+row[y]]:=(cl and 127) or shadow;

            tmp[k]:=(cl and $7f) or shadow;

            inc(k);
            end;

        blitTMP(playscreen.ofs+row[y]+SCRMIN-1, k);

        end;

    end;


(*
{ Loads all the walls contained in the WHWALLS.BTM file. }
procedure load_all_walls;
var
    s    : string[255];
    f1   : text;
    x,
    y,
    z,
    w    : smallint;
    loop : boolean;

    begin

    loop:=TRUE;
    assign(f1,'whwalls.dta');    { Apre il file }
    reset(f1);                   { si porta all'inizio dello stesso }

    x:=0;  { coordinata x del mattoncino attuale-3 }
    y:=0;  { coordinata y del mattoncino attuale }
    z:=0;  { numero del muro attuale-1 }

    while(loop) do
       begin
       s:='                                      ';
       readln(f1,s);
       if s[1]='*' then { If the first character in the line is an asterisk }
          begin
          if (y>14) then fatal_error('Too many blocks ('+inttostr(z)+')');

          for x:=3 to 15 do    { Allora dal terzo al 15 vi sono i 13 blocchi }
              begin            { che costituiscono la fila }
              w:=ord(s[x])-96; { Se e' un "a" (codice 97) allora w:=1 }
              if w<0 then w:=0;
              all_walls[z][x-3+y*16]:=w;  { Quindi all_walls contiene tutti }
              end;                        { i muri. z=numerod del muro.     }

          inc(y); { Passa alla fila successiva }

          for x:=0 to 12 do           { The two outer rows are always empty, }
              begin                   { i.e., [x,-1] and [x,15]          }
// !!!!!!!!!!!!!!!
              //all_walls[z][x,-1]:=0;
               all_walls[z][x+15*16]:=0;
              end;
          end

       else if s[1]=';' then   { Il ";" indica che si intende passare al }
          begin                { prossimo muro. }
          x:=0;
          y:=0;

          if (z>32) then fatal_error('Too many walls');
          inc(z);
          end
       else if s[1]='/' then loop:=false;
       { lo slash indica la fine logica del file, tutto cio' che segue }
       { viene ignorato.                                               }

       { Any line beginning with a character other than ";" "*" "/" }
       { is considered a comment line and is therefore ignored      }

       end;

     close(f1);
     totalwall:=z;
     end;
*)

procedure write_round_level;
var x,y : smallint;            { Stampa la scritta ROUND xx, READY.        }
//    s,r,                      { eventualmente anche il nome del giocatore }
//    sc  : string[20];         { cioe' PLAYER ONE o PLAYER TWO.            }

    begin
(*

//    settextstyle(DefaultFont,HorizDir,1);
    str(score.wall_n[cur_player]:2,s);
    r:=concat('Round ',s);
    sc:='';
    if score.pl_numb=2 then   { Nel caso di 2 giocatori, occorre anche }
       begin                                       { dire a chi tocca. }
       if cur_player=1 then sc:='Player One'
       else sc:='Player Two';
       end;

    setcolor(0);                            { Stampa le scritte in nero }
    for x:=0 to 2 do                        { come contorno spostandole }
        for y:=0 to 2 do                    { di una coordinata in tutte }
            begin                           { le direzioni.              }
            outtextxy(72+x,129+y,sc);
            outtextxy(80+x,139+y,r);
            outtextxy(90+x,149+y,'Ready');
            end;

    setcolor(1);                  { E poi centrata, in bianco stampa }
    outtextxy(73,130,sc);         { la scritta vera e propria.       }
    outtextxy(81,140,r);
    outtextxy(91,150,'Ready');
*)

    end;


procedure remove_round_level;  { Togli la scritta ROUND xx, READY }
//var y : byte;                  { copiandoci sopra il fondale.     }
begin

    hlp:=row[129] + 72;

    blitBOX(playscreen.ofs + hlp, vram + hlp, 88, 160-129);

{
    for y:=129 to 160 do begin
        //memcpy(playscreen.map[72+row[y]], screen[72+row[y]], 88);

        hlp:=row[y] + 72;

        blitROW(playscreen.ofs + hlp, vram + hlp, 88);
    end;
}
end;


{ Print the GAME OVER script }
procedure Game_over;
var x,y : smallint;
    sc  : string[20];

    begin
//    settextstyle(DefaultFont,HorizDir,1);  { Setta la font di default }
(*
    sc:='';
    if score.pl_numb=2 then                  { Se vi sono 2 giocatori  }
       begin                                 { deve dire quale dei due }
       if cur_player=1 then sc:='Player One' { ha finito.              }
       else sc:='Player Two';                { E mette in sc "PLAYER ... " }
       end;                                  { altrimenti sc rimane vuoto  }

    setcolor(0);
    for x:=0 to 2 do
        for y:=0 to 2 do
            begin                                { Disegna la scritta in    }
            outtextxy(72+x,129+y,sc);            { nero spostata di una     }
            outtextxy(76+x,139+y,'Game Over');   { coord. in tutte le direz.}
            end;

    setcolor(1);                    { E poi al centro delle scritte in nero }
    outtextxy(73,130,sc);           { stampa quella in bianco.              }
    outtextxy(77,140,'Game Over');

    mydelay(500);
    remove_round_level; { in questo caso rimuove la scritta GAME OVER }
                        { invece della scritta ROUND xx, READY.       }
*)
    end;


{ It shows in sequence the frames of the vaus being destroyed. }
procedure destroy_vaus;
var z,a,b  : word;
    w,x,y  : byte;

begin
    playvaus:=normal;
    modify_vaus;

    move_vaus(vaus.x,vaus.y);

    a:=vaus.x-4;     { Si calcola uno spostamento dovuto al brush  }
    b:=vaus.y-5;     { dell'animazione che e' leggermente spostato }
                     { dall'origine degli assi.                    }

    for w:=0 to 6 do  { w = frame to display, cycles through all frames from 0 to 6 }
        begin
        for y:=0 to 15 do
            begin
            
	    z:=y*explosion.width+w*(explosion.width shl 4);
	    
	    vbxe_ram.Position:=explosion.ofs + z;
	    vbxe_ram.ReadBuffer(scanline, explosion.width);   
	    
	    hlp := a+row[y+b];

	    vbxe_ram.Position:=playscreen.ofs + hlp;
	    vbxe_ram.ReadBuffer(scanline2, explosion.width);   
	    
            for x:=0 to explosion.width-1 do
                begin
                { Se il colore e' trasparente o il fotogramma e' il 6 }
                { allora viene usato il colore del fondale.           }
                if (w=6) or (scanline[x] = 0) {(getBYTE(explosion.ofs + x+z) = 0)} then
                   //screen[x+a+row[y+b]]:=playscreen.map[x+a+row[y+b]]
                   //blitBYTE(playscreen.ofs + hlp + x, vram + hlp + x)
		   
		   //tmp[x] := GetBYTE(playscreen.ofs + hlp + x)
		   tmp[x] := scanline2[x]
                else
                   //screen[x+a+row[y+b]]:=explosion.map[x+z];
                   //blitBYTE(explosion.ofs + x+z, vram + hlp + x)
		   
		   //tmp[x] := GetBYTE(explosion.ofs + x+z)
		   tmp[x] := scanline[x];
                end;
		
	    blitTMP(vram + hlp, explosion.width);
            end;

        death_sound(w);   { Il cicalino di quando il vaus viene distrutto }
                          { per ogni valore di w c'e' una nota diversa    }
        end;

    death_sound(7);
    mydelay(150);         { attende qualche istante. }
    disable_letter;       { se nel frattempo stava scendendo una lettera, }
                          { la toglie.                                    }
end;


{ It's exactly like the one before, only it shows the animation }
{ of the vaus being built.                                      }
procedure create_vaus;
var x,y,w  : byte;
    z,a,b  : word;

begin
    nosound;
    a:=((SCRMAX-SCRMIN) div 2)-12;
    b:=vaus_line-5;

    for w:=11 downto 0 do
        begin
        for y:=0 to 15 do
            begin
	               
	    z:=y*newvaus.width+w*(newvaus.width*16);

	    hlp := a+row[y+b];
	        
	    vbxe_ram.Position:=newvaus.ofs + z;
	    vbxe_ram.ReadBuffer(scanline, newvaus.width);

	    vbxe_ram.Position:=playscreen.ofs + hlp;
	    vbxe_ram.ReadBuffer(scanline2, newvaus.width);

	    for x:=0 to newvaus.width-1 do
                begin
//                if (getBYTE(newvaus.ofs + x+z) = 0) then
                if scanline[x] = 0 then

                   //screen[x+a+row[y+b]]:=playscreen.map[x+a+row[y+b]]
                   //blitBYTE(playscreen.ofs + x+a+row[y+b], vram + x+a+row[y+b])
		   
//		   tmp[x] := getBYTE(playscreen.ofs + hlp + x)
		   tmp[x] := scanline2[x]
		   
                else
                   //screen[x+a+row[y+b]]:=newvaus.map[x+z];
                   //blitBYTE(newvaus.ofs + x+z, vram + x+a+row[y+b])

//		   tmp[x] := getBYTE(newvaus.ofs + x+z)
		   tmp[x] := scanline[x]
                end;
		
	    blitTMP(vram + hlp, newvaus.width);
            end;

        mydelay(1);
        end;
end;


procedure put_digit(px: word; py, num: byte);  { Stampa la cifra digitale num }
                                               { alle coord. px,py.           }
var x,y,a : byte;

    begin
    a:=222; { Color 222 is dark red, which is used if the LED in question should appear off.
            { Color 223 is bright red, which is used when the LED is on. }

    { ----------------- }
    { |       0       | }
    { ----------------- }
    { |   |       |   | }
    { | 3 |       | 5 | }
    { |   |       |   | }
    { |   |       |   | }
    { ----------------- }
    { |       1       | }
    { ----------------- }
    { |   |       |   | }
    { | 4 |       | 6 | }
    { |   |       |   | }
    { |   |       |   | }
    { ----------------- }
    { |       2       | }
    { ----------------- }

    { Bit 0 }
    if (DIGITS[num] and 1)=1 then a:=223;   { Se il bit 0 e' 1 allora    }
    for x:=1 to 4 do                        { il colore del led in altro }
        //screen[px+x+row[py]]:=a;          { e' rosso vivo, altrimenti  }
	putBYTE(vram + px+x+row[py], a);
                                            { rosso scuro.               }

    { Bit 1 }
    a:=222;                               { "a" viene assunto come rosso scuro }
    if (DIGITS[num] and 2)=2 then a:=223; { eventualmente viene poi cambiato }
    for x:=1 to 4 do
        //screen[px+x+row[py+5]]:=a;    { py+5 perche' il trattino in mezzo }
	putBYTE(vram + px+x+row[py+5], a);
                                      { e' 5 pixel piu' sotto di quello   }
                                      { in altro.                         }
    { Bit 2 }
    a:=222;
    if (DIGITS[num] and 4)=4 then a:=223;
    for x:=1 to 4 do
        //screen[px+x+row[py+10]]:=a;
	putBYTE(vram + px+x+row[py+10], a);

    { Bit 3 }
    a:=222;
    if (DIGITS[num] and 8)=8 then a:=223;
    for y:=1 to 4 do
        //screen[px+row[py+y]]:=a;
	putBYTE(vram + px+row[py+y], a);

    { Bit 4 }
    a:=222;
    if (DIGITS[num] and 16)=16 then a:=223;
    for y:=1 to 4 do
        //screen[px+row[py+y+5]]:=a;
	putBYTE(vram + px+row[py+y+5], a);

    { Bit 5 }
    a:=222;
    if (DIGITS[num] and 32)=32 then a:=223;
    for y:=1 to 4 do
        //screen[px+5+row[py+y]]:=a;
	putBYTE(vram + px+5+row[py+y], a);

    { Bit 6 }
    a:=222;
    if (DIGITS[num] and 64)=64 then a:=223;
    for y:=1 to 4 do
        //screen[px+5+row[py+y+5]]:=a;
	putBYTE(vram + px+5+row[py+y+5], a);

    end;


{ Print the 5 digits of the score at coordinates px,py }
procedure write_score(px: word; py : byte; sc : cardinal);
var n1 : byte;
    f  : boolean;

   begin
   f:=false; { As long as this remains false, the 0s are not printed. }
             { This is to ensure that the score starts at 0 and not   }
             { 000000, which looks bad.                               }
	     
   { first digital digit }
   n1:=(sc div 100000) mod 10;
   if n1>0 then f:=true;          { Se la prima cifra e' >0 allora }
   if f then put_digit(px,py,n1)  { occorre stamparla }
   else put_digit(px,py,10);      { altrimenti stampa un numero spento }

   { second digital digit }
   n1:=(sc div 10000) mod 10;     { Ditto for the remaining blocks }
   if n1>0 then f:=true;
   if f then put_digit(px+7,py,n1)
   else put_digit(px+7,py,10);

   { third digital digit }
   n1:=(sc div 1000) mod 10;
   if n1>0 then f:=true;
   if f then put_digit(px+14,py,n1)
   else put_digit(px+14,py,10);

   { fourth digital digit }
   n1:=(sc div 100) mod 10;
   if n1>0 then f:=true;
   if f then put_digit(px+21,py,n1)
   else put_digit(px+21,py,10);

   { fifth digital digit }
   n1:=(sc div 10) mod 10;
   put_digit(px+28,py,n1);

   { sixth and last digital digit (which of course is always 0 because }
   { the score travels in multiples of 10 points.                      }
   put_digit(px+35,py,0);
   end;


{ When the pause is invoked, the control switches to this procedure }
procedure pause_game;
var x,y,z : byte;

    begin
    nosound;                    { disattiva qualunque suono del cicalino }
    setcolor(0);                { Stampa la scritta in nero spostandola  }
    for x:=0 to 2 do            { in tutte le direzioni.                 }
        for y:=0 to 2 do
            outtextxy(66+x,129+y,'Game Paused');

    setcolor(1);                      { Indi stampa quella in bianco }
    outtextxy(67,130,'Game Paused');

    repeat
    z:=inkey;                        { e aspetta finche' non viene premuto }
    until (z=ord('p')) or (z=32);    { o la "p" o lo spazio (z=32)         }


    { Erase the writing by copying the background over it }
    for y:=129 to 140 do
        //memcpy(playscreen.map[66+row[y]], screen[66+row[y]], textwidth('Game Paused')+1);
        blitROW(playscreen.ofs + 66+row[y], vram + 66+row[y], textwidth('Game Paused')+1);

    { textwidth('Game Paused') is a function that returns the length }
    { in pixels of the text 'Game Paused'.                           }
    end;


{ Print the small vaus in the lower left corner indicating the number of }
{ lives remaining available (not counting the one in play).              }
procedure plot_lives(lives : smallint);

const XLIVES = 11;
      YLIVES = 192;

var x,y,cn: byte;

    xp,yp,
    xl,yl   : word;
    shadow, cl  : byte;

    begin
    dec(lives); { The number of lives must be decreased by one   }
                { because the one in play should not be counted. }

    for cn:=0 to 7 do                       { at most he draws 8 }
        for y:=0 to minivaus.height-1 do begin

  	    vbxe_ram.Position:=minivaus.ofs + y*minivaus.width;
	    vbxe_ram.ReadBuffer(scanline, minivaus.width);

            yl:=y+YLIVES;
	    
	    hlp := XLIVES+cn*minivaus.width;
	    

  	    vbxe_ram.Position:=playscreen.ofs + hlp + row[yl];
	    vbxe_ram.ReadBuffer(scanline2, minivaus.width);
	    

            for x:=0 to minivaus.width-1 do
                begin
                xl:=x + hlp;

                xp:=modx[xl];
                yp:=mody[yl]*pattern.width;
		
                { if the number of lives is greater than the counter }
                { then draw a vaus.                                  }
                if (lives>cn) and (scanline[x] <> 0) {(getBYTE(minivaus.ofs + x+y*minivaus.width) <> 0)} then
                   begin
                   //cl:=minivaus.map[x+y*minivaus.width];
                   //cl:=getBYTE(minivaus.ofs + x+y*minivaus.width);
		   cl:=scanline[x];

                   //screen[xl+row[yl]]:=minivaus.map[x+y*minivaus.width];
                   //putBYTE(vram + xl+row[yl], cl);

                   //playscreen.map[xl+row[yl]]:=minivaus.map[x+y*minivaus.width];
                   //putBYTE(playscreen.ofs + xl+row[yl], cl);
		   
		   tmp[x] := cl;

                   end

                { otherwise it copies the background of the screen so that }
                { if vaus was present it is now deleted.                   }
                else
                  begin
                  //shadow:=playscreen.map[xl+row[yl]] and 128;
                  //shadow:=getBYTE(playscreen.ofs + xl+row[yl]) and 128;
                  shadow:=scanline2[x] and 128;

                  //cl:=(pattern.map[xp+yp] and 127) or shadow;
                  cl:=(getBYTE(pattern.ofs + xp+yp) and 127) or shadow;

                  //screen[xl+row[yl]]:=cl;
                  //putBYTE(vram + xl+row[yl], cl);

                  //playscreen.map[xl+row[yl]]:=cl;
                  //putBYTE(playscreen.ofs + xl+row[yl], cl);
		  
		  tmp[x] := cl;
                  end;
                end;
		
	hlp := hlp + row[yl];
	blitTMP(vram + hlp, minivaus.width);
	//blitTMP(playscreen.ofs + hlp, minivaus.width);
	blitROW(vram + hlp, playscreen.ofs + hlp, minivaus.width);
		
	end;

    end;


procedure place_fire;
//var fw : word;
//    y: byte;
begin

    hlp := fire.x + row[fire.y];

    blitZERO(shoots.ofs, vram + hlp, shoots.width, shoots.height);

{
    for y:=0 to shoots.height-1 do
        begin
        fw:=shoots.width*y;
        //memzerocpy(shoots.map[fw], screen[fire.x+row[y+fire.y]], shoots.width);
        blitZERO(shoots.ofs + fw, vram + fire.x+row[y+fire.y], shoots.width, 1);
        end;
}
end;


procedure remove_fire;
//var fw : word;
//    y: byte;
begin

   hlp := fire.x+row[fire.y];
   
   blitBOX(playscreen.ofs + hlp, vram + hlp, shoots.width, shoots.height);

{
    for y:=0 to shoots.height-1 do
        begin
        //fw:=shoots.width*y;
        //memcpy(playscreen.map[fire.x+row[y+fire.y]], screen[fire.x+row[y+fire.y]], shoots.width);
        blitROW(playscreen.ofs + fire.x+row[y+fire.y], vram + fire.x+row[y+fire.y], shoots.width);
        end;
}
end;


procedure check_fire;
var x1,x2,y1,y2 : word;
    begin
    if (fire.avl) then
       begin
       if (mouseclick=1) and (fire.avl) and (not fire.shot) then
          begin
          fire.x:=vaus.x+byte(vaus.width-shoots.width) shr 1;
          fire.y:=vaus.y-shoots.height;
          fire.shot:=TRUE;
          fire.nw  :=FALSE;
          ball_block_sound(700,5);
          end;

       if fire.shot then
          begin
          if fire.nw then remove_fire;
          fire.nw:=TRUE;

          dec(fire.y,4);
          if fire.y<22 then fire.shot:=FALSE
          else
              begin
              place_fire;

              if ((fire.y-22)>=0) and ((fire.y-22)<120) then
                 begin
                 x1:=byte(fire.x-9 ) shr 4;
                 y1:=byte(fire.y-22) shr 3;

                 x2:=byte(fire.x+shoots.width-9) shr 4;
                 y2:=y1;

                 if (wall[byte(x1+y1*16)]<>0) or (wall[byte(x2+y2*16)]<>0) then
                    begin
                    remove_fire;
                    fire.shot:=FALSE;

                    shoot_block_with_fire(x1,y1);
                    shoot_block_with_fire(x2,y2);
                    end;
                 end;
              end;
          end;
       end;
    end;


procedure remove_flux;
//var y : byte;
begin
    hlp := row[FLUXLEVEL] + 217;

    blitBOX(playscreen.ofs + hlp, vram + hlp, 8, 20);
    
{    
    for y:=0 to 19 do
        //memcpy(playscreen.map[217+row[y+FLUXLEVEL]], screen[217+row[y+FLUXLEVEL]], 8);
        blitROW(playscreen.ofs + 217+row[y+FLUXLEVEL], vram + 217+row[y+FLUXLEVEL], 8);
}
end;


procedure check_flux;
var y,fx  : byte;

   begin
   fx:=scrfluxcnt;

   if scrflux then
      begin
      
      for y:=0 to 19 do
          //memcpy(flux.map[(y+fx) shl 3], screen[217+row[y+FLUXLEVEL]], 8);
          blitROW(flux.ofs + (y+fx) shl 3, vram + 217+row[y+FLUXLEVEL], 8);

      inc(scrfluxcnt);
      if scrfluxcnt>20 then scrfluxcnt:=0;
      end;
   end;


procedure vaus_out;
var x,y,z : word;
    begin
    nosound;

    inc(score.player[cur_player],10000);
    remain_blk:=0;

    z:=vaus.x;

    wait_vbl;
    remove_vaus;
    place_vaus;

    for x:=z to z+44 do
        begin
        Wait_VBL;

        vaus.oldx:=vaus.x;
        vaus.x:=x;
        remove_vaus;
        check_flux;
        place_vaus;
	
	hlp := row[vaus.y] + 225;
	
	blitBOX(playscreen.ofs + hlp, vram + hlp, 40, vaus.height);
{
        for y:=vaus.y to vaus.y+vaus.height do
            //memcpy(playscreen.map[225+row[y]], screen[225+row[y]], 40);
            blitROW(playscreen.ofs + 225+row[y], vram + 225+row[y], 40);
}
        end;

    end;


procedure check_bonus_type(var b1,b2,b3 : BALLTYPE);
var x : smallint;
    begin
    
      if vaus.letter>0 then
         begin
         lett.last:=vaus.letter-1;
         if b2.inplay then remove_ball(b2);
         if b3.inplay then remove_ball(b3);
         b2.inplay:=FALSE;
         b3.inplay:=FALSE;
         scrflux:=FALSE;
         remove_flux;

         if vaus.letter<>6 then
            begin
            b1.launch:=FALSE;
            b2.launch:=FALSE;
            b3.launch:=FALSE;
            end;
         end;

      case vaus.letter of

        1: begin                              { Letter L }
           if fire.shot then remove_fire;
           playvaus:=lasers;
           modify_vaus;
           vaus.letter:=0;
           fire.avl:=TRUE;
           fire.shot:=FALSE;
           end;

        2: begin                              { Letter E }
           if fire.shot then remove_fire;
           playvaus:=enlarged;
           modify_vaus;
           vaus.letter:=0;
           fire.avl:=FALSE;
           end;

        3: begin                              { Letter B }
           if fire.shot then remove_fire;
           playvaus:=normal;
           modify_vaus;
           vaus.letter:=0;
           fire.avl:=FALSE;
           scrflux:=TRUE;
           end;

        4: begin                              { Letter D }
           if fire.shot then remove_fire;
           playvaus:=normal;
           modify_vaus;
           fire.avl:=FALSE;
           end;

        5: begin                              { Letter S }
           if fire.shot then remove_fire;
           playvaus:=normal;
           modify_vaus;
           vaus.letter:=0;
           x:=max(b1.speed-500,BALLSPEED);
           set_ball_speed(b1,x);
           fire.avl:=FALSE;
           end;

        6: begin                              { Letter C }
           if fire.shot then remove_fire;
           playvaus:=normal;
           modify_vaus;
           fire.avl:=FALSE;
           end;

        7: begin                              { Letter P }
           if fire.shot then remove_fire;
           playvaus:=normal;
           modify_vaus;
           vaus.letter:=0;
           inc(score.lives[cur_player]);
           plot_lives(score.lives[cur_player]);
           ball_block_sound(2000,10);
           fire.avl:=FALSE;
           end;

        end;
     end;


procedure deviate_ball(var ball : BALLTYPE);
var temp : smallint;

   begin

   repeat
    temp:=get_ball_direction(ball)+random(BALLDEV)-(BALLDEV shr 1);
   until ((temp mod 90)>30) and ((temp mod 90)<60);

   set_ball_direction(ball,temp);
   set_ball_speed(ball,ball.speed);
   
   ball.sbd:=0;
   end;

{ ------------------------------------------------------------------------ }

{ This is the main procedure that during the game calls all          }
{ others. As long as the ball is not lost or the frame is terminated }
{ (or possibly the game is aborted) the procedure retains            }
{ control. It terminates in only one of the above cases or in case   }
{ a fatal_error occurs due to which the program is forced            }
{ to quit automatically and inevitably.                              }

function Bounceball : boolean;
var
  x,y  : smallint;
  key  : smallint;
//  ball : array[0..2] of BALLTYPE;
  t1,t2: smallint;
  hlp: smallint;

//  cn: byte;

  ball0: BALLTYPE;
  ball1: BALLTYPE;
  ball2: BALLTYPE;
  
  scores: cardinal;
  

  procedure check_ball(var ball: BALLTYPE);
  begin

         if (ball.inplay) then  { all considerations apply if the ball }
                                    { is in play.                          }
            begin
            if (ball.y>=22) and (ball.y<142) then
               ball_hit_block(ball);

            set_ball(ball);
            ball.speed:=ball_speed(ball);
            end;
  end;


  procedure test_ball(var ball: BALLTYPE);
  begin

         if ball.inplay then
            begin
            inc(ball.finespeed);

            if ball.finespeed > LEVEL[lv] then
               begin
               ball.finespeed:=0;

               { If the speed is less than the maximum speed }
               if ball.speed < MAXSPEED then
                  begin
                  inc(ball.speed,10);  { you increase it }
                  set_ball_speed(ball,ball.speed); { and you update it }
                  end;
               end;

            inc(ball.sbd); { this is the regular deviation counter }

            { if it exceeds a certain threshold (SBDIR) a deviation is set }
            { random by an angle between -BALLDEV/2 and +BALLDEV/2 }
            if (ball.sbd >= SBDIR) and (ball.speedy < 0) then
               deviate_ball(ball);
            end;

  end;


  begin

  scrfluxcnt:=0;
  scrflux:=FALSE;

  balls_in_play:=1;

  fire.avl:=FALSE;
  playvaus:=normal;

  lett.last:=EMP;
  lett.active:=FALSE;

  { Mette lo sfondo giusto a seconda del muro }
  fill_picture_with_pattern(pattern);

  { Disegna il quadro di gioco con lo sfondo pocanzi settato}
  showBTMpicture(playscreen);

  { Print the number of lives of the current player        }
  { cur_player=1 or 2 depending on which player is to play }
  plot_lives(score.lives[cur_player]);

  { adjust the colors, in theory they should already be okay. }
//  setpalette(playscreen);

  { Stampa il punteggio dei 2 giocatori e l'hi-score. }
  write_score(253,POS_DIGIT[1],score.player[1]);
  write_score(253,POS_DIGIT[2],score.player[2]);
  write_score(253,POS_DIGIT[3],score.hiscore);

  { Draw the bricks }
  put_wall;

  { Performs a reset in case the mouse has some problem sometimes }
  { happens. }
  //mousereset;

  { The ball is in play and must be thrown }
  ball0.inplay:=TRUE;
  ball0.launch:=TRUE;

  { Set the initial coordinates of the ball }
  ball0.x:=((SCRMAX+SCRMIN) shr 1)-2;
  ball0.y:=VAUS_LINE-BALLSPOT;

  { cancel the old ones }
  ball0.oldx:=EMP;
  ball0.oldy:=EMP;

  { Deviation occurs when this value exceeds a certain threshold }
  ball0.sbd:=0;

  { The initial distance of the ball when it is on the VAUS      }
  { from the edge left of the VAUS itself.                       }
  ball0.onvaus:=16;

  { Tiene il numero di vertical-blank che passano da quando appare   }
  { il vaus con la pallina a quando essa viene lanciata.              }
  { Se il valore supera le 2000 unita' la palla parte automaticamente }
  ball0.stm:=0;

  { Alla partenza la variabile lett.incoming assume un valore casuale }
  { fra 0 e LETTER_DROP (costante definita all'inizio della unit.     }
  lett.incoming:=random(LETTER_DROP);

  { Shows the animation of the VAUS materializing out of thin air }
  create_vaus;

  { and prints the words ROUND xx, READY. }
  write_round_level;

  set_vaus;                       { Adjusts the initial VAUS parameters. }
  start_vaus;
  move_vaus(vaus.x,VAUS_LINE);    { brings him to the center of the playing area }
  start_level;                    { If the sound is on, it plays the tune }
  start_vaus;
  remove_round_level;             { Removes the words ROUND xx, READY}
  set_ball(ball0);
  
  { This is the main cycle, it can only get out of it if : }
  { - the ball is lost, }
  { - the picture is ended (i.e., no more bricks are left to be destroyed. }
  { - the game is somehow aborted.                 }

  set_ball_direction(ball0,random(15)+60)  ; { random starting angle }
                                             { 60 and 75 degrees }
  set_ball_speed(ball0,BALLSPEED);

  { initial speed = constant BALLSPEED }
  ball0.finespeed:=0; { submultiples of the speed are 0  }

  ball1.inplay:=FALSE;
  ball2.inplay:=FALSE;
  
  x:=vaus.x;

  while(ball0.inplay) and (remain_blk>0) and (not score.abortplay) do
     begin
     Wait_VBL; { Waits for the vertical blank }

{$IFDEF ATARI}


{$ELSE}
     form1.show_play;
{$ENDIF}

     mousecoords(x);  { reads mouse coordinates }

     {  if trainer (VAUS in automatic mode) is not active }
     { moves VAUS to mouse coord.x }

     if trainer=0 then move_vaus(x,VAUS_LINE)

     { if it is active, however, it dictates that the x of the VAUS is equal }
     { to the x of the ball, with an appropriate translation coefficient    }
     { so that the ball hits the center of the vaus and not the left edge.  }

     else if trainer=1 then
          move_vaus(min(SCRMAX-32,max(ball0.x-ball0.onvaus,SCRMIN)),VAUS_LINE);

     { ball[0].launch is worth TRUE if the ball is attached to the VAUS and }
     { is to be thrown. Otherwise, when it is in play it is worth false.    }

     if ball0.launch=TRUE then
        begin
        inc(ball0.stm);    { if the ball is attached the shot counter }
                           { is continuously incremented.             }


        { When it reaches 250 the ball automatically starts }
        if ball0.stm=250 then ball0.launch:=FALSE;

        { Makes the ball follow the vaus if it is moved }
        start_ball(ball0);

        { If you press the mouse button then the ball starts }
        if mouseclick=1 then ball0.launch:=FALSE;
        end

     else begin
        { Otherwise if the ball is not attached one simply needs to move it. }
        { Clearly if there are 3 balls you need to move all 3.               }

        //for cn:=0 to 2 do
            if ball0.inplay then move_ball(ball0);
            if ball1.inplay then move_ball(ball1);
            if ball2.inplay then move_ball(ball2);

     end;

     { If the coordinates of the ball cn are between 22 and 142 (respectively}
     { maximum and minimum coordinates at which a brick can be bumped) then  }
     { you need to check whether the ball actually bumped a brick or not.    }

     check_ball(ball0);
     check_ball(ball1);
     check_ball(ball2);
  (*
     for cn:=0 to 2 do
         begin
         if (ball[cn].inplay) then  { all considerations apply if the ball }
                                    { is in play.                          }
            begin
            if (ball[cn].y>=22) and (ball[cn].y<142) then
               ball_hit_block(ball[cn]);

            set_ball(ball[cn]);
            ball[cn].speed:=ball_speed(ball[cn]);
            end;
         end;
*)
     checkshine;     { check to see if there is a brick to be sparked }
     check_letter;   { If a letter is coming down }
     check_bonus_type(ball0,ball1,ball2); { If a letter is collected }
     check_fire;     { whether a laser shot was fired }
     check_flux;

     if ((vaus.x+vaus.width) = (SCRMAX-1)) and (scrflux) then vaus_out;

     if vaus.letter=4 then   { In case a D has been collected the balls }
        begin                { become 3.                                }
        balls_in_play:=3;

        ball1:=ball0;    { ball 2 and 3 are placed equal to 1 }
        ball2:=ball0;

        t1:=get_ball_direction(ball0) div 90;
        { you the quadrant in which the velocity vector is located }
        t2:=ball0.speed;  { as well as the modulus of the vector itself }

        { you impose a 30-degree tilt to the quadrant at }
        { first ball, 45 at the second, and 60 at the third.              }

        { At this point the three balls are forced to split up. }
	
        hlp:=t1*90;

        set_ball_direction(ball0,(hlp+30));
        set_ball_direction(ball1,(hlp+45));
        set_ball_direction(ball2,(hlp+60));


        { Instead, the three velocities remain that of the first ball }
        set_ball_speed(ball0,t2);
        set_ball_speed(ball1,t2);
        set_ball_speed(ball2,t2);

        vaus.letter:=0;
        end;

     { As long as there is more than one ball in play, no letter should come }
     if balls_in_play>1 then lett.incoming:=0;


     { Update player's score }
     scores := score.player[cur_player];
     
     if old_scores <> scores then begin
       
       write_score(253,POS_DIGIT[cur_player], scores);

       old_scores := scores;

     end;
     

     { If the player's score is greater than the hi-score }
     if score.player[cur_player] > score.hiscore then
        begin
        { places the hi-score equal to the player's score }
        score.hiscore:=score.player[cur_player];
        { And prints the hi-score on the screen }
        write_score(253,POS_DIGIT[3],score.hiscore);
        end;

     { This cycle increases the speed of all balls in play the value        }
     { of LEVEL[lv] obviously depends on the lv, that is, the level         }
     { selected before starting the game.                                   }     

     test_ball(ball0);
     test_ball(ball1);
     test_ball(ball2);
(*
     for cn:=0 to 2 do
         begin
         if ball[cn].inplay then
            begin
            inc(ball[cn].finespeed);
            if ball[cn].finespeed>LEVEL[lv] then
               begin
               ball[cn].finespeed:=0;

               { If the speed is less than the maximum speed }
               if ball[cn].speed < MAXSPEED then
                  begin
                  inc(ball[cn].speed,10);  { you increase it }
                  set_ball_speed(ball[cn],ball[cn].speed); { and you update it }
                  end;
               end;

            inc(ball[cn].sbd); { this is the regular deviation counter }

            { if it exceeds a certain threshold (SBDIR) a deviation is set }
            { random by an angle between -BALLDEV/2 and +BALLDEV/2 }
            if (ball[cn].sbd>=SBDIR) and (ball[cn].speedy<0) then
               deviate_ball(ball[cn]);
            end;
         end;
  *)
     { This cycle ensures that ball no. 1 is always in play          }
     { (unless all three are lost)                                   }

     { If no. 1 is lost, no. 2 takes the place of no. 1, no. 3 takes }
     { the place of no. 2, and finally no. 3 is deactivated          }

     { since the three balls are identical, the process takes place  }
     { without the player physically noticing the replacement.       }

     { in this way, if ball no. 1 is not in play at the end          }
     { of the cycle, it means that all three have fallen.            }

//     for cn:=0 to 2 do
	
	if ball0.launch = FALSE then

         if not ball0.inplay then
            begin
            ball0:=ball1;
            ball1:=ball2;
            ball2.inplay:=FALSE;
            end;


     balls_in_play:=0;  { The number of balls in play is recalculated each time. }
//     for cn:=0 to 2 do
         if ball0.inplay then inc(balls_in_play);
         if ball1.inplay then inc(balls_in_play);
         if ball2.inplay then inc(balls_in_play);


     if (not ball0.inplay) then   { If the No. 1 ball is no longer in play. }
        begin
        ball0.launch:=TRUE;
        remove_ball(ball0);             { takes it off the screen }
        destroy_vaus;                   { shows the destruction sequence }
        dec(score.lives[cur_player]);   { decreases the number of lives by 1 }
        wall_p[cur_player]:=wall;       { stores the an assigned variable }
                                        { to the player the current wall }

        { This happens because if there are two players, one must pass }
        { to the other player who will probably not be in the same }
        { position. The bricks must then be returned as such }
        { when the turn passes back to the player who has now lost one }
        { life. }
        nosound; { and disable the sound }
        end;


     { If the current note lasts until snd_delay becomes 0 }
//     if snd_delay>0 then dec(snd_delay)
//     else nosound;

     { ---------------------- Trainer Options -------------------- }
  (*
     key:=inkey;  { checks whether a key is pressed }

     if (key=ord('p')) or (key=32) then pause_game; { The P key pauses the game. }

     if key=7680 then score.abortplay:=TRUE;  { ALT+A, the game is over }

     if (key=ord('T')) then { Pressing T (uppercase) generates }
        begin               { a row of bricks.                 }
        for cn:=0 to 12 do
            begin
            { Clearly, if the indestructible brick is to replace }
            { another destructible brick, the total number       }
            { of bricks to be knocked down to complete           }
	          { the picture must decrease by 1                     }
            if (wall[cn+14*16]>0) and (wall[cn+14*16]<>10) then
               dec(remain_blk);

            wall[cn+14*16]:=10;  { and the brick in question becomes indestructible }
            end;

        put_wall; { and the update is performed on the screen }
        end;


     { La 'R' maiuscola abilita la modalita' automatica del vaus }
     if key=ord('R') then trainer:=1;

     { La 'r' minuscola la disabilita }
     if key=ord('r') then trainer:=0;

     { Se il tasto e' una a,b,c,d,e,f,g: viene fatta cadere una lettera }
     if (key>=97) and (key<(97+LETTER_NUMB-1)) then { Fa cadere la lettera }
        start_letter(104,30,key-97);

     if key=11520 then                        { ALT+X, quit-ta }
        begin
        //closegraph;
        nosound;
        closeprogram;
        end;
*)
     end;

  { BounceBall esce con false se la palla e' stata persa, con true se }
  { il quadro e' stato finito. }

  Result:=FALSE;
  if remain_blk=0 then Result:=TRUE;
  end;

{ ------------------------------------------------------------- }

function choose_start_wall : smallint;
const px = 70;
      py = 100;
      dx = 34;
      dy = 35;
      ddx= 19;
      ddy= 14;

var x,y : smallint;
    st    : smallint;
    oldx,
    oldy,
    newx,
    newy  : smallint;
    //sc    : string[20];

    begin

    st:=1;  { Si comincia a scegliere partendo dal muro n.1 }
(*
    //settextstyle(DefaultFont,HorizDir,1);
    setcolor(0);

    { Stampa PLAYER xxx se il numero di giocatori e' 2 }
    if cur_player=1 then sc:='Player One'
    else sc:='Player Two';

    { stampa le scritte CHOOSE YOER WALL, ecc... }
    for x:=-1 to 1 do
        for y:=-1 to 1 do
            begin
            outtextxy(px+5+x,py+y,sc);
            outtextxy(px+x,py+y+10,'Choose your');
            outtextxy(px+6+x,py+y+20,'start wall');

            outtextxy(px-39+x,py+58+y,'Move mouse to select;');
            outtextxy(px-45+x,py+68+y,'left button to confirm');
            end;

    setcolor(1);
    outtextxy(px+5,py,sc);
    outtextxy(px,py+10,'Choose your');
    outtextxy(px+6,py+20,'start wall');

    outtextxy(px-39,py+58,'Move mouse to select;');
    outtextxy(px-45,py+68,'left button to confirm');

    { Disegna il quadrato nero in cui devono apparire i numeri del muro }
    { da scegliere.                                                     }
    for y:=py+dy to py+dy+ddy do
        for x:=px+dx to px+dx+ddx do
            screen[x+row[y]]:=0;

    mousecoords(oldx,oldy);  { rileva le coordinate del mouse }

//    while(mouseclick=1) do;
    while(mouseclick<>1) do
       begin
       put_digit(px+dx+3,py+dy+2,st div 10); { Scrive il numero del quadro }
       put_digit(px+dx+11,py+dy+2,st mod 10);

       mousecoords(newx,newy);       { Se le coord. sono diverse: }
       if newx>oldx then inc(st);    { se la x e' maggiore il quadro aumenta }
       if newx<oldx then dec(st);    { se e' munore diminuisce.              }

       { Se supera il massimo selezionabile torna al n.1 }
       if st>totalwall then st:=st-totalwall;

       { se supera il minimo selezionabile torna al massimo (n.32) }
       if st<1 then st:=st+totalwall;

       oldx:=newx; { le nuove coord. diventano le vecchie. }
       oldy:=newy;

       end;
  *)
    choose_start_wall:=st;  { e ritorna il numero selezionato }
    end;


procedure set_start_parameters;
var x : byte;
   begin
   { Imposta i parametri del giocatore 1 e 2 }
   
   old_scores := 1;

   for x:=1 to 2 do
       begin
       score.player[x]:=0;
       score.lives[x] :=5;
       score.wall_n[x]:=STARTWALL;
       wall_p[x]:=all_walls[STARTWALL-1];

       score.roundsel[x]:=FALSE;
       end;

   cur_player:=1;
   end;


procedure soundicon;

    begin
(*
    { Altezza dell'icona (l'icona e' alta il doppio perche' il brush }
    { e' composto dall'icona con la nota e l'icona con la X una sopra l'altra }
    h:=soundfx.height div 2;

    fl:=0; { se sound_on non e' false, cioe' e' TRUE allora fl:=0 }
           { punto in cui inizia il disegno dell'icona con la nota }


    { altrimenti fl viene spostato al punto in cui c'e' l'icona con la X }
    if sound_on=FALSE then
       fl:=soundfx.width*h;

    { e quindi copia uno dei due disegni sullo schermo }
    for y:=0 to h-1 do
        begin
        fw:=y*soundfx.width;
        for x:=0 to soundfx.width-1 do
            screen[320-soundfx.width+x+row[y+200-h]]:=soundfx.map[x+fw+fl];
        end;
*)
    end;


(*
procedure level_selection;
var x,y,fl,fw,h : word;

    begin
    { Disegna sullo schermo uno dei 5 numeri a seconda del valore di lv }
    h:=levelsel.height div 5;    { I frames sono 5 quindi l'altezza di un }
                                 { frame e' l'altezza totale del disegno/5 }

    fl:=(lv-1)*h*levelsel.width; { fl contiene l'indirizzo del frame da }
                                 { copiare sullo schermo.               }

    for y:=0 to h-1 do
        begin
        fw:=y*levelsel.width;
        for x:=0 to levelsel.width-1 do
            screen[x+row[y+200-h]]:=levelsel.map[x+fw+fl];
        end;
    end;
*)


function mainscreen : smallint;
var x,y,z : word;
    //ps    : smallint;
//    srow  : array[0..100] of word;
    k,ik  : smallint;

    begin
    nosound;                { turns off the buzzer }
    score.abortplay:=FALSE; { is set the aborted match flag to FALSE }

//    for x:=0 to 63999 do    { Clear the screen }
//        screen[x]:=0;

//    setpalette(playscreen); { Set colors }

    { And copies the presentation page with ARKANOID written on the screen }
    { via the procedure written in assembler. }
    //memcpy(presents.map, screen, 64000);

    blitBOX(presents.ofs, vram, 320, 200);
  

//    soundicon;         { draw the sound icon }
//    level_selection;   { and that of the level }
//    mousereset;        { reset the mouse as a precaution }

//    repeat             { It cycles until something is done }
       { K holds the status of the mouse, IK any keys pressed }
(*
       k:=mouseclick;
       ik:=inkey;

       if ik=11520 then k:=-1;   { ALT+X = quit }  { k<>0 breaks the cycle }


       if ik=32 then             { SPACE BAR = sound on/off switch }
          begin
          sound_on:=sound_on XOR TRUE;    { x xor 1 inverte il valore di x }
          soundicon; { e disegna l'icona} { 0 xor 1 = 1; 1 xor 1 = 0 }
          end;

       if ik=15104 then    { se si preme F1 il livello aumenta }
          begin
          inc(lv);
          if lv>5 then lv:=1;   { se e' superiore a 5 torna ad 1 }
          level_selection;      { e stampa il numero sullo schermo }
          end;

       if ik=15360 then    { se si preme F2 il livello diminuisce }
          begin
          dec(lv);
          if lv<1 then lv:=5;  { se e' inferiore a 1 torna a 5 }
          level_selection;     { e stampa il numero sullo schermo }
          end;
   *)
    //until k<>0;    { the cycle breaks if k is different from 0 }

    mainscreen:=1;//k; { thus returns: -1=quit, 1=one player, 2=two players }
    end;


procedure start_game(players : smallint);
var nwall : boolean;

    begin
    set_start_parameters;                 { imposta i parametri di partenza }
    if players=1 then score.lives[2]:=0;  { se c'e' un solo giocatore il }
                                          { seconda non ha neanche una vita }

    trainer:=0;                           { il trainer viene disattivato }
    wall:=wall_p[cur_player];             { si copia nel muro corrente }


                                          { quello del giocatore corrente }
    set_wall;                             { e lo si disegna }

    fill_picture_with_pattern(pattern);   { si imposta lo sfondo }
    showBTMpicture(playscreen);           { e si disegna tutto quanto sullo }
                                          { schermo }

//    setpalette(playscreen);               { si impostano i colori. }
    

    { you print the three scores, player 1, 2 and hi-score }
    write_score(253,POS_DIGIT[1],score.player[1]);
    write_score(253,POS_DIGIT[2],score.player[2]);
    write_score(253,POS_DIGIT[3],score.hiscore);

    { And you draw the bricks of the wall }
    //put_wall;
   
    repeat

          repeat
             { if the wall to start from has not yet been chosen }
             { the player (cur_player) is made to choose now }
             if not score.roundsel[cur_player] then
                begin
                score.wall_n[cur_player]:=choose_start_wall; 

                { the chosen wall is assigned to the player }
                wall_p[cur_player]:=
                      all_walls[score.wall_n[cur_player]-1];

                { at this point the wall was chosen }
                score.roundsel[cur_player]:=TRUE;
                end;

             { the player's wall is put in the current wall }
             wall:=wall_p[cur_player];
             set_wall;

             { It starts, this assignment calls bounceball, which does not }
             { end until I vieen lost the vaus or is terminated }
             { the picture. }
             nwall:=BounceBall;

             { If the NWALL framework is terminated, it is TRUE }
             if nwall then
                begin
                { so you increment the number of the wall it is at the cur_player }
                inc(score.wall_n[cur_player]); 

                { And if the maximum number is exceeded, it starts from No.1 again }
                if score.wall_n[cur_player] > totalwall then
                   score.wall_n[cur_player]:=1;

                { and is taken in new wall from the general matrix }
                wall_p[cur_player]:=
                      all_walls[score.wall_n[cur_player]-1];
                end
             else
                 { if the wall has not been completed you look to see if the number }
                 { of lives has dropped to zero in which case you print GAME OVER }
                 if score.lives[cur_player]=0 then Game_Over;

          { the loop repeats until bounceball says it was not }
          { completed the wall (nwall=FALSE) which means it was }
          { lost a life }
          until nwall=FALSE;

          { then control passes to the other player }
          inc(cur_player);
          if cur_player>players then cur_player:=1;

          { unless one player has run out of lives available, in which }
          { case control remains with the player who has lost lives. }
          { Note that this also works just fine if there is only one player, }
          { since the other player has lives to 0. }
          if score.lives[cur_player]=0 then cur_player:=3-cur_player;


    { the cycle repeats until both players run out of lives }
    { or the game is aborted with ALT+A }
    until ((score.lives[1]=0) and (score.lives[2]=0)) or (score.abortplay);
    end;

//end.
