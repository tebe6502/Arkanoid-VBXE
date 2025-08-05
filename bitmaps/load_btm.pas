uses crt;

const
    vbxe_data = $5000 + 320*200;

type

   arr768   = array[0..767] of byte;       { For the 256 colors in RGB (x3) }
   arr64k   = array[0..320*250-1] of byte; { for the 320x200 screen }

   BTMTYPE  = RECORD                   { per un disegno in fomrato BTM }
              width   : word;          { larghezza disegno       }
              height  : word;          { altezza                 }
//              trasp   : byte;        { trasparenza (non usato) }
//              palette : arr768;      { puntatore alla palette di colori }
//              palused : boolean;     { flag TRUE = la palette esiste }
              ofs: cardinal;
              map     : arr64k;        { dati contenenti il disegno }
              end;


var  
    playscreen : BTMTYPE;  { area di gioco (320x200) }
    playvaus   : BTMTYPE;  { vaus }
    normal     : BTMTYPE;  { vaus normale }
    enlarged   : BTMTYPE;  { allargato }
    lasers     : BTMTYPE;  { traformato per sparare }
    pattern    : BTMTYPE;  { sfondo }
    explosion  : BTMTYPE;  { esplosione vaus }
    newvaus    : BTMTYPE;  { sequenza di animazione di partenza }
    presents   : BTMTYPE;  { scritta ARKANOID }
    soundfx    : BTMTYPE;  { l'icona con la nota e la nota sbarrata }
    shinewall  : BTMTYPE;  { luccichio dei mattoncini grigi e beige }
    minivaus   : BTMTYPE;  { vaus piccolino che indica le vite }
    levelsel   : BTMTYPE;  { 5 frames dei numeri per scegliere il livello }
    letters    : BTMTYPE;  { le animazioni delle 7 lettere }
    shoots     : BTMTYPE;  { e il disegno dei laser }
    flux       : BTMTYPE;

    def_pal: arr768;
  
    success: Boolean;
    
    ofset: cardinal;
    
    dum: array [0..320*256] of byte;
    
    f: file;


procedure loadBTM(name : string; var BTMREC : BTMTYPE; pal : boolean);
var
  h1   : file;
  s    : array[0..10] of byte;   { carica un file in formato BTM }
  cnt  : integer;
  size : integer;

  pl: arr768;

  begin

  {$I-}
  assign(h1,name);               { apre il file }
  reset(h1,1);                   { e si pone all'inizio }

  blockread(h1,s,11,cnt);        { legge 11 bytes di intestazione }


  if pal = TRUE then begin
    blockread(h1,DEF_PAL,768,cnt);      { legge la palette dei colori }

//    BTMREC.palused:=TRUE;
  end else begin
    blockread(h1,pl,768,cnt);      { legge la palette dei colori }

//    BTMREC.palused:=FALSE;
  end;

  BTMREC.width :=s[6]+s[7]*256;  { legge la larghezza, byte 6 e 7 }
  BTMREC.height:=s[8]+s[9]*256;  { l'altezza, byte 8 e 9 }
//  BTMREC.trasp :=s[10];          { e il colore di trasparenza }

  size:=(BTMREC.width)*(BTMREC.height); { calcola dimensione immagine }

//  getmem(BTMREC.map,size);              { alloca la memoria per l'immagine }
  blockread(h1,BTMREC.map,size,cnt);   { e si legge l'immagine da disco }


  BTMREC.ofs:=ofset - vbxe_data;
  blockwrite(f,BTMREC.map,size);   { e si legge l'immagine da disco }
  
  inc(ofset, size);


  close(h1);  { e chiude il file letto da disco }
  {$I+}

  if IOResult<>0 then success:=FALSE;
  end;


procedure arkanoid;
var t: text;
begin

   success:=TRUE;  { Success viene messo a FALSE se si verifica un errore da disco }

//   load_all_walls; { Carica i 32 muri di gioco }


   { Carica uno dopo l'altro tutti i disegni grafici di cui necessita }

   loadBTM('..\PLAYGR.BTM'  ,playscreen,TRUE);  { lo schermo virtuale }
   loadBTM('..\PRESENT.BTM' ,presents,false);   { la scritta ARKANOID }
   loadBTM('..\EXPLODE.BTM' ,explosion,FALSE);  { l'esplosione del vaus }
   loadBTM('..\NEWVAUS.BTM' ,newvaus,FALSE);    { la creazione del vaus }
   loadBTM('..\SOUNDFX.BTM' ,soundfx,FALSE);    { l'icona sound on/off  }
   loadBTM('..\SHINEWAL.BTM',shinewall,FALSE);  { il lucchichio dei mattoni }
   loadBTM('..\MINIVAUS.BTM',minivaus,FALSE);   { i vaus che indicano le vite }
   loadBTM('..\LEVELSEL.BTM',levelsel,FALSE);   { l'icona del livello di gioco }
   loadBTM('..\DROPS.BTM'   ,letters,FALSE);    { le 7 lettere }
   loadBTM('..\VAUS.BTM'    ,normal,FALSE);     { il vaus }
   loadBTM('..\LASERS.BTM'  ,lasers,FALSE);     { i raggi laser sparati dal vaus }
   loadBTM('..\ENLARGED.BTM',enlarged,FALSE);   { il vaus allargato }
   loadBTM('..\FIRE.BTM'    ,shoots,FALSE);     { il vaus coi laser montati }
   loadBTM('..\SCRFLUX.BTM' ,flux,FALSE);       { l'onda di flusso (per la lett. B) }

   if not success then writeln('Program can''t find some BTM files');



{
  VBXE_OVRADR = $5000;

   playscreen.ofs := 0;
   presents.ofs := 64000;

   explosion := 128000;
   newvaus :=
   soundfx :=
   shinewall :=
   minivaus :=
   levelsel :=
   letters :=
   normal :=
   lasers :=
   enlarged :=
   shoots :=
   flux :=
}   

 assign(t, 'btm.inc'); rewrite(t);

 writeln(t, #9'playscreen.ofs := VBXE_DATA + ', playscreen.ofs , ';');
 writeln(t, #9'presents.ofs := VBXE_DATA + ', presents.ofs , ';');
 writeln(t, #9'explosion.ofs := VBXE_DATA + ', explosion.ofs , ';');
 writeln(t, #9'newvaus.ofs := VBXE_DATA + ', newvaus.ofs , ';');
 writeln(t, #9'soundfx.ofs := VBXE_DATA + ', soundfx.ofs , ';');
 writeln(t, #9'shinewall.ofs := VBXE_DATA + ', shinewall.ofs , ';');
 writeln(t, #9'minivaus.ofs := VBXE_DATA + ', minivaus.ofs , ';');
 writeln(t, #9'levelsel.ofs := VBXE_DATA + ', levelsel.ofs , ';'); 
 writeln(t, #9'letters.ofs := VBXE_DATA + ', letters.ofs , ';');
 writeln(t, #9'normal.ofs := VBXE_DATA + ', normal.ofs , ';');
 writeln(t, #9'lasers.ofs := VBXE_DATA + ', lasers.ofs , ';');
 writeln(t, #9'enlarged.ofs := VBXE_DATA + ', enlarged.ofs , ';');
 writeln(t, #9'shoots.ofs := VBXE_DATA + ', shoots.ofs , ';');
 writeln(t, #9'flux.ofs := VBXE_DATA + ', flux.ofs , ';');
 
 flush(t);
 close(t);
   
end;



begin

 ofset := vbxe_data;


 assign(f, 'btm.dat'); rewrite(f, 1);
 
 blockwrite(f, dum, ofset);
 
 Arkanoid;
  
 close(f);

 repeat until keypressed;

end.