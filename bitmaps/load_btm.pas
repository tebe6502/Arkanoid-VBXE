uses crt, sysutils;

const

   BALLARRAY  : packed array[0..4,0..4] of byte =
                                           ((0,1,1,1,0),
                                            (1,1,2,1,1),
                                            (1,2,1,1,1),
                                            (1,1,1,1,1),
                                            (0,1,1,1,0));
                                            { Ball design }

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
    balldata   : BTMTYPE;
    
    pattern0   : BTMTYPE;
    pattern1   : BTMTYPE;
    pattern2   : BTMTYPE;
    pattern3   : BTMTYPE;
    pattern4   : BTMTYPE;

    def_pal: arr768;
  
    success: Boolean;
    
    ofset: cardinal;
    
    j: integer;
    
    dum: array [0..320*1024] of byte;
    
    f: file;


{$i exportbmp.pas}

procedure loadBTM(name : string; var BTMREC : BTMTYPE; pal : boolean);
var
  h1   : file;
  s    : array[0..10] of byte;   { carica un file in formato BTM }
  cnt  : integer;
  size : integer;

  pl: arr768;

  begin
  
  
  if name = 'ball' then begin

   BTMREC.width:=5;
   BTMREC.height:=5;
     
   BTMREC.ofs:=ofset;
   blockwrite(f,BALLARRAY,sizeof(BALLARRAY));   { e si legge l'immagine da disco }
  
   inc(ofset, sizeof(BALLARRAY));
   
   exit;
  
  end;


  {$I-}
  assign(h1,name);               { apre il file }
  reset(h1,1);                   { e si pone all'inizio }

  blockread(h1,s,11,cnt);        { legge 11 bytes di intestazione }


  if pal = TRUE then begin
    blockread(h1,DEF_PAL,768,cnt);      { legge la palette dei colori }

    close(h1);
    exit;

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


  BTMREC.ofs:=ofset;
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

//   loadBTM('..\PRESENT.BTM' ,presents,false);   { la scritta ARKANOID }
   loadBTM('..\EXPLODE.BTM' ,explosion,FALSE);  { l'esplosione del vaus }
   loadBTM('..\NEWVAUS.BTM' ,newvaus,FALSE);    { la creazione del vaus }
//   loadBTM('..\SOUNDFX.BTM' ,soundfx,FALSE);    { l'icona sound on/off  }
   loadBTM('..\SHINEWAL.BTM',shinewall,FALSE);  { il lucchichio dei mattoni }
   loadBTM('..\MINIVAUS.BTM',minivaus,FALSE);   { i vaus che indicano le vite }
   loadBTM('..\LEVELSEL.BTM',levelsel,FALSE);   { l'icona del livello di gioco }
   loadBTM('..\DROPS.BTM'   ,letters,FALSE);    { le 7 lettere }
   loadBTM('..\VAUS.BTM'    ,normal,FALSE);     { il vaus }
   loadBTM('..\LASERS.BTM'  ,lasers,FALSE);     { i raggi laser sparati dal vaus }
   loadBTM('..\ENLARGED.BTM',enlarged,FALSE);   { il vaus allargato }
   loadBTM('..\FIRE.BTM'    ,shoots,FALSE);     { il vaus coi laser montati }
   loadBTM('..\SCRFLUX.BTM' ,flux,FALSE);       { l'onda di flusso (per la lett. B) }

   loadBTM('ball' ,balldata,FALSE);   

//   loadBTM('..\pattern0.BTM' ,pattern,FALSE);   
   
   loadBTM('..\pattern0.BTM' ,pattern0,FALSE);       
   loadBTM('..\pattern1.BTM' ,pattern1,FALSE);       
   loadBTM('..\pattern2.BTM' ,pattern2,FALSE);       
   loadBTM('..\pattern3.BTM' ,pattern3,FALSE);       
   loadBTM('..\pattern4.BTM' ,pattern4,FALSE);       

   loadBTM('..\PLAYGR.BTM'  ,playscreen,TRUE);  { lo schermo virtuale }
   
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

{
 writeln(t, #9'presents.ofs := VBXE_DATA + $', intToHex(presents.ofs,8) , ';');
 writeln(t, #9'presents.width := ', presents.width , ';');
 writeln(t, #9'presents.height := ', presents.height , ';');
 writeln(t); 
}

 writeln(t, #9'explosion.ofs := VBXE_DATA + $', intToHex(explosion.ofs,8) , ';');
 writeln(t, #9'explosion.width := ', explosion.width , ';');
 writeln(t, #9'explosion.height := ', explosion.height , ';');
 writeln(t);
 writeln(t, #9'newvaus.ofs := VBXE_DATA + $', intToHex(newvaus.ofs,8) , ';');
 writeln(t, #9'newvaus.width := ', newvaus.width , ';');
 writeln(t, #9'newvaus.height := ', newvaus.height , ';'); 
 writeln(t);
// writeln(t, #9'soundfx.ofs := VBXE_DATA + ', soundfx.ofs , ';');
// writeln(t, #9'soundfx.width := ', soundfx.width , ';');
// writeln(t, #9'soundfx.height := ', soundfx.height , ';');
// writeln(t); 
 writeln(t, #9'shinewall.ofs := VBXE_DATA + $', intToHex(shinewall.ofs,8) , ';');
 writeln(t, #9'shinewall.width := ', shinewall.width , ';');
 writeln(t, #9'shinewall.height := ', shinewall.height , ';'); 
 writeln(t);
 writeln(t, #9'minivaus.ofs := VBXE_DATA + $', intToHex(minivaus.ofs,8) , ';');
 writeln(t, #9'minivaus.width := ', minivaus.width , ';');
 writeln(t, #9'minivaus.height := ', minivaus.height , ';');
 writeln(t);
 writeln(t, #9'levelsel.ofs := VBXE_DATA + $', intToHex(levelsel.ofs,8) , ';'); 
 writeln(t, #9'levelsel.width := ', levelsel.width , ';'); 
 writeln(t, #9'levelsel.height := ', levelsel.height , ';'); 
 writeln(t);
 writeln(t, #9'letters.ofs := VBXE_DATA + $', intToHex(letters.ofs,8) , ';');
 writeln(t, #9'letters.width := ', letters.width , ';');
 writeln(t, #9'letters.height := ', letters.height , ';');
 writeln(t);
 writeln(t, #9'normal.ofs := VBXE_DATA + $', intToHex(normal.ofs,8) , ';');
 writeln(t, #9'normal.width := ', normal.width , ';');
 writeln(t, #9'normal.height := ', normal.height , ';');
 writeln(t);
 writeln(t, #9'lasers.ofs := VBXE_DATA + $', intToHex(lasers.ofs,8) , ';');
 writeln(t, #9'lasers.width := ', lasers.width , ';');
 writeln(t, #9'lasers.height := ', lasers.height , ';');
 writeln(t);
 writeln(t, #9'enlarged.ofs := VBXE_DATA + $', intToHex(enlarged.ofs,8) , ';');
 writeln(t, #9'enlarged.width := ', enlarged.width , ';');
 writeln(t, #9'enlarged.height := ', enlarged.height , ';');
 writeln(t);
 writeln(t, #9'shoots.ofs := VBXE_DATA + $', intToHex(shoots.ofs,8) , ';');
 writeln(t, #9'shoots.width := ', shoots.width , ';');
 writeln(t, #9'shoots.height := ', shoots.height , ';');
 writeln(t);
 writeln(t, #9'flux.ofs := VBXE_DATA + $', intToHex(flux.ofs,8) , ';');
 writeln(t, #9'flux.width := ', flux.width , ';');
 writeln(t, #9'flux.height := ', flux.height , ';');
 writeln(t);
 
 writeln(t, #9'balldata.ofs := VBXE_DATA + $', intToHex(balldata.ofs,8) , ';');
 writeln(t, #9'balldata.width := ', balldata.width , ';');
 writeln(t, #9'balldata.height := ', balldata.height , ';');
 writeln(t);

// writeln(t, #9'pattern.ofs := VBXE_DATA + ', pattern.ofs , ';');
// writeln(t, #9'pattern.width := ', pattern.width , ';');
// writeln(t, #9'pattern.height := ', pattern.height , ';');
// writeln(t);
 writeln(t, #9'pattern0.ofs := VBXE_DATA + $', intToHex(pattern0.ofs,8) , ';');
 writeln(t, #9'pattern0.width := ', pattern0.width , ';');
 writeln(t, #9'pattern0.height := ', pattern0.height , ';');
 writeln(t);
 writeln(t, #9'pattern1.ofs := VBXE_DATA + $', intToHex(pattern1.ofs,8) , ';');
 writeln(t, #9'pattern1.width := ', pattern1.width , ';');
 writeln(t, #9'pattern1.height := ', pattern1.height , ';');
 writeln(t);
 writeln(t, #9'pattern2.ofs := VBXE_DATA + $', intToHex(pattern2.ofs,8) , ';');
 writeln(t, #9'pattern2.width := ', pattern2.width , ';');
 writeln(t, #9'pattern2.height := ', pattern2.height , ';');
 writeln(t);
 writeln(t, #9'pattern3.ofs := VBXE_DATA + $', intToHex(pattern3.ofs,8) , ';');
 writeln(t, #9'pattern3.width := ', pattern3.width , ';');
 writeln(t, #9'pattern3.height := ', pattern3.height , ';');
 writeln(t);
 writeln(t, #9'pattern4.ofs := VBXE_DATA + $', intToHex(pattern4.ofs,8) , ';');
 writeln(t, #9'pattern4.width := ', pattern4.width , ';');
 writeln(t, #9'pattern4.height := ', pattern4.height , ';');
 writeln(t);

{ 
 writeln(t, #9'playscreen.ofs := VBXE_DATA + $', intToHex(playscreen.ofs,8) , ';');
 writeln(t, #9'playscreen.width := ', playscreen.width , ';');
 writeln(t, #9'playscreen.height := ', playscreen.height , ';'); 
 writeln(t);
}

 writeln(t, #9'// ofset = VBXE_DATA + $', intToHex(ofset,8));
 
 flush(t);
 close(t);
   
end;



begin

 ofset := 0;

 assign(f, 'btm.dat'); rewrite(f, 1);
  
 Arkanoid;
 
 fillByte(dum, sizeof(dum), 0); 
 blockwrite(f, dum, 320);
  
 close(f);

 
 for j:=0 to 767 do def_pal[j]:=def_pal[j] shl 2;

 assign(f, 'btm.pal'); rewrite(f, 1);
 blockwrite(f, def_pal, 768);
 close(f);


 assign(f, 'btm.dat'); reset(f, 1);
 blockread(f, dum, sizeof(dum), j);
 close(f);

 inc(j, 320);

 ExportBMP('btm.bmp', 320, j div 320);

 repeat until keypressed;

end.