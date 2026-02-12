
{

---------------------------------------------------------------
procedure set_ball_speed(var ball : BALLTYPE; speed : word);
---------------------------------------------------------------
$00..$11

---------------------------------------------------------------
function ball_speed(ball : BALLTYPE): word;
---------------------------------------------------------------
$00..$07

---------------------------------------------------------------
function split_line(var _x1,_y1,_x2,_y2 : smallint) : byte;
---------------------------------------------------------------
$00..$0f

---------------------------------------------------------------
procedure ball_hit_block(var ball : BALLTYPE);
---------------------------------------------------------------  
$00..$2f

---------------------------------------------------------------  
procedure deviate_ball(var ball : BALLTYPE);
---------------------------------------------------------------  
$00.$03

---------------------------------------------------------------
function Bounceball : boolean;
---------------------------------------------------------------
$30..$3a
$40..$5f
$60..$7f
$e0..$ff

}


procedure mousecoords(var x: byte);
var a: byte;
begin

 a := porta and $0f;
 
 case a of
  joy_left, joy_left_up, joy_left_down:
  
            if x > SCRMIN then begin
             dec(x, 4);
	     
	     if x < SCRMIN then x:=SCRMIN;	     
	    end; 

  joy_right, joy_right_up, joy_right_down:

            if x < byte(SCRMAX - vaus.width) then begin
             inc(x, 4);
	      
	     if byte(x + vaus.width) > SCRMAX then x:=SCRMAX - vaus.width;
	    end; 
 end;
  

end;


procedure blitZERO(src: cardinal; w : word; h: byte); register;
{
  dst = vram
}
begin

 asm
   fxs FX_MEMS #$80
 end;

 blt_zero.src_adr.byte2:=src shr 16;
 blt_zero.src_adr.byte1:=src shr 8;
 blt_zero.src_adr.byte0:=src;

 blt_zero.dst_adr.byte1:=hlp shr 8;
 blt_zero.dst_adr.byte0:=hlp;

 blt_zero.src_step_y:=w;
 
 blt_zero.blt_height:=h-1;

 blt_zero.blt_width:=w-1;

 asm
   fxs FX_MEMS #$00
 end;

 RunBCB(blt_zero);
 while BlitterBusy do;

end;


procedure blitTEMP(swidth, dwidth: word); overload; register;
begin

 blt.dst_step_y:=dwidth;
 blt.src_step_y:=swidth;
 
end;


procedure blitTEMP(src, dst: cardinal; w : word; h: byte); overload; register;
begin

 blt.src_adr.byte2:=src shr 16;
 blt.src_adr.byte1:=src shr 8;
 blt.src_adr.byte0:=src;

 blt.dst_adr.byte2:=dst shr 16;
 blt.dst_adr.byte1:=dst shr 8;
 blt.dst_adr.byte0:=dst;

 blt.blt_height:=h-1;

 blt.blt_width:=w-1;

 RunBCB(blt);
 while BlitterBusy do;

end;


procedure blitTEMP2TEMP(w, h: byte); register;
begin
  
 blt.src_adr.byte2:=playscreen_ofs shr 16;
 blt.dst_adr.byte2:=playscreen_ofs shr 16;

 blt.src_adr.byte1:=hlp shr 8;
 blt.dst_adr.byte1:=hlp shr 8;
 blt.src_adr.byte0:=hlp;
 blt.dst_adr.byte0:=hlp;

 blt.blt_height:=h-1;

 blt.blt_width:=w-1;

 RunBCB(blt);
 while BlitterBusy do;

 blt.src_adr.byte2:=vram shr 16;
 blt.dst_adr.byte2:=vram shr 16;

 RunBCB(blt);
 while BlitterBusy do;

end;


procedure blitCLR(w : word; h: byte); register;
begin

 blt.dst_step_y:=320;
 blt.src_step_y:=320;
  
 blt.src_adr.byte2:=pattern_temp shr 16;

 blt.src_adr.byte1:=hlp shr 8;
 blt.dst_adr.byte1:=hlp shr 8;
 blt.src_adr.byte0:=hlp;
 blt.dst_adr.byte0:=hlp;

 blt.dst_adr.byte2:=playscreen_ofs shr 16;

 blt.blt_height:=h-1;

 blt.blt_width:=w-1;

 RunBCB(blt);
 while BlitterBusy do;

 blt.dst_adr.byte2:=vram shr 16;

 RunBCB(blt);
 while BlitterBusy do;

end;


procedure blitSCR(swidth: byte; dwidth: word; w : word; h: byte); register;
begin

 blt.dst_step_y:=dwidth;
 blt.src_step_y:=swidth;
  
 blt.src_adr.byte1:=$02;	// scr = $0200
 
 blt.src_adr.byte2:=$00;
 blt.src_adr.byte0:=$00;

 blt.dst_adr.byte2:=playscreen_ofs shr 16;
 blt.dst_adr.byte1:=hlp shr 8;
 blt.dst_adr.byte0:=hlp;

 blt.blt_height:=h-1;

 blt.blt_width:=w-1;

 RunBCB(blt);
 while BlitterBusy do;

 blt.dst_adr.byte2:=vram shr 16;

 RunBCB(blt);
 while BlitterBusy do;

end;


procedure blitBOX(w: word; h: byte); register;
{
  src = playscreen_ofs
  dst = vram
}
begin

 asm
   fxs FX_MEMS #$80
 end;

 blt_box.src_adr.byte1:=hlp shr 8;
 blt_box.dst_adr.byte1:=hlp shr 8;

 blt_box.src_adr.byte0:=hlp;
 blt_box.dst_adr.byte0:=hlp;

 blt_box.blt_width:=w-1;
 blt_box.blt_height:=h-1;

 asm
   fxs FX_MEMS #$00
 end;
	
 RunBCB(blt_box);
 while BlitterBusy do;

end;


{ ------------------------------------------------------------------------- }
{ ------------------------------------------------------------------------- }


procedure setcolor(c: byte);
begin

end;

function textwidth(a: string): word;
begin

 Result := length(a) * 8;

end;


procedure outtextxy(x,y: smallint; s: TString);
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

end;


function inkey : word;   { returns the code of the key pressed }
//var ch,ch2: char;        { 0 = no key pressed }
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


procedure initRowArray;  { initialize the ROW array; row[n]:=320*n }
var y : byte;
begin

  hlp:=0;

  for y:=0 to 15 do begin

   mul90_16[y] := hlp;
   
   inc(hlp, 90);
  
  end; 


  hlp:=0;

  for y:=0 to 255 do begin

    Mod10Table[y] := y mod 10;
    Mod90Table[y] := word(y*256) mod 90;
    Mod360Table[y] := word(y*256) mod 360;

    if y >= 200 then
     row[y] := 320*200
    else begin
     row[y]:=hlp;

     inc(hlp, 320);
    end;

  end;

end;


procedure InitSVGA; { Initialize the SuperVGA driver as shown in the example. }
//var
//   AutoDetect : pointer;
//   GraphMode, GraphDriver, ErrCode: smallint;

   begin
   

 if VBXE.GraphResult <> VBXE.grOK then begin
  writeln('VBXE not detected');
  halt;
 end;

 SetHorizontalRes(VBXE.VGAMed);
 ColorMapOff;

 VBXEControl(vc_xdl+vc_xcolor+vc_no_trans);

 SetTopBorder(20);
 SetXDLHeight(200);

 SetOverlayAddress(vram);

 vbxe_ram.position:=vram;
 vbxe_ram.size:=320*200+vram;
 vbxe_ram.clear;

 dmactl:=0;

	asm
	  fxs FX_MEMS #$80
	end;

 fillByte(blt, sizeof(TBCB), 0); 
 fillByte(blt_letter, sizeof(TBCB), 0);
 fillByte(blt_box, sizeof(TBCB), 0);
 fillByte(blt_zero, sizeof(TBCB), 0);
 
 
 blt.src_step_x:=1;
 blt.dst_step_x:=1;

// blt.blt_control := 0;

 blt.blt_and_mask:=$ff;



 blt_letter.dst_adr.byte2:=vram shr 16;

 blt_letter.src_step_x:=1;
 blt_letter.dst_step_x:=1;

 blt_letter.blt_control := 1;

 blt_letter.dst_step_y:=320;
 blt_letter.src_step_y:=128;
 
 blt_letter.blt_height:=8-1;

 blt_letter.blt_width:=16-1;

 blt_letter.blt_and_mask := $ff;



 blt_box.src_adr.byte2:=playscreen_ofs shr 16;

 blt_box.dst_adr.byte2:=vram shr 16;

 blt_box.dst_step_y:=320;
 blt_box.src_step_y:=320;

 blt_box.src_step_x:=1;
 blt_box.dst_step_x:=1;

// blt_box.blt_control:=0;

 blt_box.blt_and_mask:=$ff;



 blt_zero.dst_adr.byte2:=vram shr 16;

 blt_zero.dst_step_y:=320;

 blt_zero.src_step_x:=1;
 blt_zero.dst_step_x:=1;

 blt_zero.blt_control := 1;

 blt_zero.blt_and_mask:=$ff;


	asm
	  fxs FX_MEMS #$00
	end;

 pause;
 
 asm
  sei
  lda #0
  sta nmien
  sta irqen
  
  lda #$fe
  sta portb
  
  mwa #NMI $fffa
 
  mva #$40 nmien
 end;


   end; { End of the InitSVGA procedure }

{ ------------------------------------------------------------------------ }

procedure shine_block;    { performs block scintillation }
var
    xb,yb: byte;          { The block parameters are contained }
    frame : byte;         { in the global variable SHINEREC }
    xf,yf: byte;

    fr : word;
    
    i: byte;

begin
    xb:= shinerec.xb;     { puts the coordinates of the block in xb,yb }
    yb:= shinerec.yb;
    
    i := xb+yb*16;

    asm
	fxs FX_MEMS #$80
    end;


    if wall[i]>8 then                  { if the block is gray or brown }
       begin
       frame:=(shinerec.frame shr 1);  { calculate the frame number }
       if wall[i]<>10 then inc(frame,5);

       xf:= 9+(xb shl 4);  { find the coordinates on the screen of the block }
       yf:=22+(yb shl 3);  { to be made to flash }
       fr:=frame shl 7;    { calculate the position of the nth frame. }


       blitTEMP(16,320);

       blitTEMP(shinewall_ofs + fr, vram + xf + row[yf], 16, 8);

(*
       for y:=0 to 7 do    { e copia il frame n-esimo sullo schermo }
           begin
           og:=y shl 4;    { equivale ad y*16, ma piu' veloce }
//           memcpy(shinewall.map[fr+og], screen[xf+row[yf+y]], 16);

           blitROW(shinewall.ofs+fr+og, vram + xf+row[yf+y], 16);
           end;
*)

       end;


       asm
	  fxs FX_MEMS #$00
       end;

    inc(shinerec.frame);  { increase the frame counter }
    if shinerec.frame=10 then shinerec.active:=FALSE;
    { and when the frame is the last one, then the sparkle is over }

end;


procedure unshine_block; { interrupts the sparkle of a block if the }
                         { ball hitting another causes the sparkle  }
                         { of another block }
begin
    shinerec.frame:=9;   { i.e., set the frame as the last one }
    shine_block;         { and perform the block flicker with the last }
                         { frame, i.e., the block returns to normal }
end;


procedure shine(xb,yb : byte);   { this procedure sets the }
                                 { sparkle of a block }
begin
    if shinerec.active then unshine_block;

    shinerec.xb     := xb;  { block coordinates }
    shinerec.yb     := yb;  { x,y }
    shinerec.frame  := 0;   { starting frame }
    shinerec.active := TRUE;                 { active sparkle }
    shinerec.block  := wall[byte(xb+yb*16)]; { block type (brown or gray) }
end;


procedure checkshine; { if sparkle is enabled, then execute it }
                      { moving on to the next frame }
begin
    if shinerec.active=TRUE then shine_block;
end;


function random_letter_drop : byte;
var rn,sum,letter : byte;
begin

   repeat
      rn:=rand(100);    { Randomly pick a number between 0 and 99 }
      sum:=0;           { set the sum to zero                     }
      letter:=0;        { and the current letter to 0             }

      repeat


         inc(letter);                   { Increase the current letter           }
         inc(sum,LETTER_DIS[letter]);   { Increase the sum of the percentage of }
                                        { probability of the current letter     }

      until sum > rn; { If the sum exceeds the chosen random number }
                      { the program drops the current letter        }
                      { otherwise it moves on to the next letter.   }

   until byte(letter-1) <> lett.last;

   random_letter_drop:=(letter-1);
end;


procedure put_letter;
const
	mul1024: array [0..8] of word = (0, 1024, 1024*2, 1024*3, 1024*4, 1024*5, 1024*6, 1024*7, 1024*8);
	mul16: array [0..8] of word = (0, 16, 16*2, 16*3, 16*4, 16*5, 16*6, 16*7, 16*8);

var src : cardinal;
begin

//     src := letters_ofs + (lett.typ shl 10) + (lett.frame shl 4);

     src := letters_ofs + mul1024[lett.typ] + mul16[lett.frame];

     asm
       fxs FX_MEMS #$80
     end;

     blt_letter.src_adr.byte2:=src shr 16;
     blt_letter.src_adr.byte1:=src shr 8;
     blt_letter.src_adr.byte0:=src;

     hlp := lett.x + row[lett.y];

     blt_letter.dst_adr.byte1:=hlp shr 8;
     blt_letter.dst_adr.byte0:=hlp;

     asm
       fxs FX_MEMS #$00
     end;

     RunBCB(blt_letter);

end;


procedure remove_letter;
begin

    if lett.y < 200 then begin

      hlp := lett.x + row[lett.y];

      blitBOX(16, 8);

    end;  

end;


procedure disable_letter;
   begin
   remove_letter;
   lett.active:=FALSE;
   end;


procedure start_letter(xl,yl: byte; letter : byte);
   begin
   if lett.active then disable_letter;

   lett.x        := xl;
   lett.y        := yl;
   lett.typ      := letter;
   lett.frame    := 0;
   lett.subframe := 0;
   lett.active   := TRUE;
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

        if (vaus.x < byte(lett.x+16)) and (byte(vaus.x+vaus.width) > lett.x) and
           (vaus.y < byte(lett.y+8))  and (byte(vaus.y+vaus.height) > lett.y) then
           begin

//	   ball_block_sound(100,10);
//	   sfx.init(sfx_check_letter);

           vaus.letter:=lett.typ+1;
           inc(score.player[cur_player],SCORE_WALL[10]);
           disable_letter;
           end;
        end;

      lett.incoming:=0;
      end
   else if (lett.incoming > LETTER_DROP) then
          start_letter(lett.nextx, lett.nexty, lett.nexttype);
   end;


{ Draw the ball on the screen, the coordinates are specified in }
{ BALL.x and BALL.y, BALLSPOT.x = BALLSPOT.y is the radius of the ball }
{ in pixels }
procedure place_ball(var ball : BALLTYPE);
begin

 hlp:=ball.x-BALLSPOT+row[ball.y - BALLSPOT];
  
 blitZERO(balldata_ofs, BALLDIM, BALLDIM); 

{
 asm
   fxs FX_MEMS #$80
 end;

 blt_zero.src_adr.byte2:=balldata_ofs shr 16;
 blt_zero.src_adr.byte1:=balldata_ofs shr 8;
 blt_zero.src_adr.byte0:=balldata_ofs;

 blt_zero.dst_adr.byte1:=hlp shr 8;
 blt_zero.dst_adr.byte0:=hlp;

 blt_zero.src_step_y:=BALLDIM;
 
 blt_zero.blt_height:=BALLDIM-1;

 blt_zero.blt_width:=BALLDIM-1;

 asm
   fxs FX_MEMS #$00
 end;

 RunBCB(blt_zero);
 while BlitterBusy do;
}
end;


{ Delete the ball from the screen, called one moment before }
{ place_ball }
procedure remove_ball(var ball: BALLTYPE);
begin

 hlp := ball.oldx-BALLSPOT+row[ball.oldy-BALLSPOT];
  
 blitBOX(BALLDIM, BALLDIM);

{
 asm
   fxs FX_MEMS #$80
 end;

 blt_box.src_adr.byte1:=hlp shr 8;
 blt_box.dst_adr.byte1:=hlp shr 8;

 blt_box.src_adr.byte0:=hlp;
 blt_box.dst_adr.byte0:=hlp;

 blt_box.blt_width:=BALLDIM-1;
 blt_box.blt_height:=BALLDIM-1;

 asm
   fxs FX_MEMS #$00
 end;
	
 RunBCB(blt_box);
 while BlitterBusy do; 
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

	pause;


//  form1.show_play;
end;


procedure set_ball(var ball : BALLTYPE);
//var b0, b1: Boolean;
begin
  
//  b0 := (byte(ball.oldx) <> EMP) and (byte(ball.oldy) <> EMP);
//  b1 := (byte(ball.oldx) <> byte(ball.x)) or (byte(ball.oldy) <> byte(ball.y));
  
//  if (b0 and b1) then

//      remove_ball(ball); { as soon as VB starts the ball is moved to the }
  hlp := ball.oldx-BALLSPOT + row[ball.oldy-BALLSPOT];
  blitBOX(BALLDIM, BALLDIM);

//  place_ball(ball);  { new coordinates }
  hlp:=ball.x-BALLSPOT + row[ball.y - BALLSPOT];  
  blitZERO(balldata_ofs, BALLDIM, BALLDIM); 
  

  ball.oldx := ball.x; { set the old coordinates equal to the current ones, }
  ball.oldy := ball.y; { the current ones will then be modified}
end;


procedure set_ball_speed(var ball : BALLTYPE; speed : word);
var
//  sx, sy: smallInt;
//  a, b, x, y, len: word;
//  i: cardinal;

  sx: smallint absolute $00;
  sy: smallint absolute $02;

  a: word absolute $04;
  b: word absolute $06;
  x: word absolute $08;
  y: word absolute $0a;
  len: word absolute $0c;

  i: cardinal absolute $0e;
  
begin

  sx := ball.speedx;
  sy := ball.speedy;

  if sx < 0 then
   a := -sx
  else
   a := sx;

  if sy < 0 then
   b := -sy
  else
   b := sy;

  if a > MAXSPEED then a := MAXSPEED;
  if b > MAXSPEED then b := MAXSPEED;

  i:=sqrtable[a] + sqrtable[b];
  
  len := trunc( FastSqrt(i) );
  
  if len = 0 then
    Exit;
    
  if len > MAXSPEED then len := MAXSPEED;  

  // skalowanie w integerach (zaokrąglamy najbliżej)
  x := (a * speed) div len;
  
  if (x = 0) then exit;  
  
  y := (b * speed) div len;

  if (y = 0) then exit;
    
  if x > MAXSPEED then x := MAXSPEED;
  if y > MAXSPEED then y := MAXSPEED;
  
  if sx < 0 then 
   sx := -x
  else
   sx := x;
 
  if sy < 0 then 
   sy := -y
  else
   sy := y;  
  
  ball.speedx := sx;
  ball.speedy := sy;

end;



(*
procedure set_ball_speed(var ball : BALLTYPE; speed : word);
var
  sx,sy : smallint;   { Sets the speed of the ball based on the speed }
  vm : single;        { vector module passed in SPEED: smallint.      }
  i: cardinal;
  
  a,b: word;
begin

  sx:=ball.speedx;  { stores the x and y components of velocity }
  sy:=ball.speedy;  { in sx and sy, respectively                }
  
  a:=abs(sx) and 1023;
  b:=abs(sy) and 1023;
  
  i:=sqrtable[a] + sqrtable[b];
  
  f_hlp := FastSqrt(i);
  
  vm:=speed / f_hlp ;//sqrt(sx*sx+sy*sy); { calculate the coefficient of proportionality  }
                                 { between the old and new speeds                }
                                 { (the direction does not change, only          }
                                 { the modulus changes).                         }

  ball.speedx:=trunc(sx * vm);   { and then multiply by that coefficient. }
  ball.speedy:=trunc(sy * vm);   { the two projections of the velocity.   }
    

  if ball.speedx = 0 then ball.speedx := $a0;
  if ball.speedy = 0 then ball.speedy := $a0;

end;
*)


procedure set_ball_direction(var ball : BALLTYPE; angle : smallint);
//var w : single;
begin                  { sets the trajectory angle of the ball }

  ball.speedx := SinDeg(angle+90);  { the velocity is assumed to be unitary }
  ball.speedy := -SinDeg(angle);    { v=256 equals 70 pixels per sec. }

(*
  w:=angle*pi/180.0;     { w viene espresso in gradi }

  ball.speedx:=trunc(256*cos(w));  { la velocita' si suppone unitaria }
  ball.speedy:=-trunc(256*sin(w)); { v=256 equivale a 70 pixel al sec. }
*)
end;


function get_ball_direction(var ball : BALLTYPE): smallint;
//var w : smallint; { Returns the direction in which the ball is moving }
begin


(*
  if ball.speedx=0 then begin
   
   if ball.speedy >= 0 then
    w:=-90
   else
    w:=90;
   
  end 
  else
    begin
    { calculates the arcotangent and adds multiples of 90 degrees depending on }
    { signs of ball.speedx and ball.speedy }
      
    f_hlp := -ball.speedy / ball.speedx;
    
    f_hlp := arctan(f_hlp)*180.0/pi;

    w:=trunc(f_hlp);

    if(ball.speedx<0) then inc(w,180);
    
    inc(w,360);
    w:=mod360(w);
    end;
*)

  get_ball_direction := scale360[ Atan2(-ball.speedy, ball.speedx) ];

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


procedure move_ball(var ball : BALLTYPE);
var b0,b1,b2: Boolean;
var
  x,y : word;
  angle : smallint;

  begin
  { moves the ball by adding the two velocity vectors to the x,y coordinates }
  { before performing this addition, everything is multiplied by 256 in order to }
  { have a higher number of positions. }

  x := (byte(ball.x) shl 8) + ball.finex + ball.speedx;
  y := (byte(ball.y) shl 8) + ball.finey + ball.speedy;

  ball.x:=x shr 8;
  ball.y:=y shr 8;

  ball.finex:=x and $ff;
  ball.finey:=y and $ff;

  { check if the ball hits the right wall }
  { if it hits, reverse the sign }

  if(byte(ball.x) > SCRMAX) then
     begin
     ball.speedx:=-ball.speedx;  { reverses the velocity vector x }
     ball.x:=2*SCRMAX-ball.x;    { reflects the ball on the axis x=SCRMAX }
     ball.finex:=255-ball.finex; { adjust the speed submultiples }
//     ball_block_sound(240,5);    { emits the sound of impact on the wall }
     end;

  { the same for the left wall }

  if(byte(ball.x) < SCRMIN) then
     begin
     ball.speedx:=-ball.speedx;
     ball.x:=2*SCRMIN-ball.x;
     ball.finex:=255-ball.finex;
//     ball_block_sound(240,5);
     end;

  { ... and for the upper one }

  if(byte(ball.y) < SCRTOP) then
     begin
     ball.speedy:=-ball.speedy;
     ball.y:=2*SCRTOP-ball.y;
     ball.finey:=255-ball.finey;
//     ball_block_sound(240,5);
     end;


  { if the ball is on the y-axis of the vaus, if the velocity vy is }
  { greater than 0 (i.e., the ball is moving downward), and if the  }
  { ball was previously above the vaus, then ... }

  b0 := byte(ball.y + BALLSPOT) > VAUS_LINE;
  b1 := ball.speedy >= 0;
  b2 := byte(ball.oldy) <= VAUS_LINE;

//  if(ball.y+BALLSPOT>VAUS_LINE) and (ball.speedy > 0) and (ball.oldy<=VAUS_LINE) then
  if b0 and b1 and b2 then
     begin
     { if any point on the ball is on the vaus ... }

     if(byte(ball.x) > byte(vaus.x-BALLSPOT)) and (byte(ball.x) < byte(vaus.x+vaus.width+BALLSPOT)) then
        begin
        { reverses the vy vector of the ball's velocity }
        ball.speedy:=-ball.speedy;
	
        if (vaus.letter=6) and (not ball.launch) then
           begin
           ball.stm:=0;
           ball.launch:=TRUE;
           ball.onvaus:=ball.x-vaus.x;
           end;

        //ball_block_sound(300,6);
	sfx.init(sfx_ball_bounce);

        { emits the sound of a ball hitting a bat }
	
	ball.brwhit:=0;
	ball.sbd := 0;

        { if the ball hits the left red cylinder of the vaus }
        if (ball.x < byte(vaus.x+10)) then
           begin
           { reverses the vector vx, velocity x of the ball }
           ball.speedx:=-ball.speedx;

           { sets the angle variable to the ball's angle of movement }
           { plus a random deviation value between 0 and BALLDEV     }
           angle:=get_ball_direction(ball) + rand(BALLDEV);

           { Reset the direction of movement of the ball according to this new }
           { angle. However, the angle must be between 120 and 160 degrees.    }
           { Values above or below this range are automatically adjusted to    } 
	   { the extreme values of the range itself. For example, 175 degrees  }
           { is adjusted to 160 degrees.                                       }

           set_ball_direction(ball,max(120,min(160,angle)));

           { Reset the speed of the ball, because changing the angle of }
           { movement causes speed to be lost.                          }

           set_ball_speed(ball, ball.speed);
           end;

        { Completely similar to the previous one, with the difference that }      
	{ the description refers to the small red cylinder on the right.   }

        if (byte(ball.x) > byte(vaus.x + vaus.width - 10)) then
           begin
           ball.speedx:=-ball.speedx;
           angle:=get_ball_direction(ball)-rand(BALLDEV);
           set_ball_direction(ball,min(60,max(20,angle)));
           set_ball_speed(ball, ball.speed);
           end;

        end;
     end;

  { if the ball passes the vaus without colliding with it, that is to say:
    if both the old and new y coordinates are greater than the ordinate on which
    the vaus runs and the speed y of the ball is greater than 0, i.e.
    the ball is moving downwards, then the ball is lost and the vaus is detonated.  }

  if (byte(ball.oldy) > VAUS_LINE) and (byte(ball.y) > SCRBOT) and (ball.speedy >= 0) then	// '>=0' is shorter then '>0'
     begin
     ball.inplay:=FALSE; { For now, only the flag is set, the ball is no longer in play. }
     remove_ball(ball);  { and the ball is removed from the screen }
     end;

end;


procedure modify_vaus;
begin
  vaus.oldlen:=vaus.width;
  vaus.width :=playvaus.width;   { width of the vaus }
  vaus.height:=playvaus.height;  { height of the vaus   }
end;


procedure set_vaus; { set the initial (starting) parameters of the vaus }
begin
  vaus.x:=((SCRMAX-SCRMIN) shr 1)-8;
  vaus.y:=VAUS_LINE;

  { set the cursor in the center of the playing area }
  { x=(x1+x2)/2 average between the maximum and minimum }
  { even the mouse pointer (which is not visible) }
  { is moved to the center }

  vaus.oldx:=EMP;                { the old coordinates are set to EMP }
  vaus.oldy:=EMP;                { since vaus has not moved }
  vaus.iflash:=0;                { This is incremented every 1/70 sec. }
                                 { and when it reaches a certain value it is }
                                 { reset to zero and vaus.flash is incremented }

  vaus.flash:=0;                 { This variable determines the color of }
                                 { the edges of the vaus. }
                                 { (which changes continuously) }

  vaus.width :=playvaus.width;   { width of the vaus }
  vaus.height:=playvaus.height;  { height of the vaus  }
  vaus.oldlen:=vaus.width;
  vaus.letter:=EMP;
                                 { both are contained in the .BTM file }

end;


(*
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
*)


procedure remove_vaus;
begin
  { Remove the vaus and draw the background in its place. }
  
  hlp:=vaus.oldx+row[vaus.oldy];
  
  blitBOX(vaus.oldlen, vaus.height);

  vaus.oldlen:=vaus.width;

end;


procedure place_vaus;
var
  i, y: byte;
  video: cardinal;

begin

  inc(vaus.iflash);               { is increased every cycle (1/70 sec.) }

  if vaus.iflash>SPEEDFLASH then  { if it reaches the SPEEDFLASH value... }
     begin
     inc(vaus.flash);             { vaus.flash is increased }
     vaus.iflash:=0;              { and vaus.iflash is reset }
     end;

  if vaus.flash>10 then vaus.flash:=0;
  { There are 10 different colors that the edges of the vaus can take on }
  { after which the cycle repeats from the beginning (i.e., from 0) }

  { Draw the vaus, making sure that the borders (which have color 255 }
  { in the .BTM drawing) are replaced by the color specified by }
  { flash[vaus.flash], where obviously flash[] is a function dependent on }
  { vaus.flash mentioned above. For example, flash[2]:=211 (see at the beginning in the }
  { declaration of constants. }

  hlp := vaus.x + row[vaus.y];

  blitZERO(playvaus.ofs, vaus.width, vaus.height);

  asm
   fxs FX_MEMS #$80
  end;

  hlp := vaus.x + row[vaus.y + 2];
  
  video := vram + hlp;
  
  y := vaus.height - 4;
    
  blitTEMP(320, 320);
  
  blt.blt_and_mask := $00;
  blt.blt_xor_mask := FLASH[vaus.flash];
    
  blitTEMP(video, video, 1, y);
  
  i:=vaus.width - 1;
  
  blitTEMP(video+i, video+i, 1, y);

  blt.blt_and_mask := $ff;
  blt.blt_xor_mask := $00;

(* 
  for y:=0 to vaus.height-1 do
     begin
     { this multiplication is done here so as not to repeat it }
     { vaus.width times }
     //hlp:=y * vaus.width;
     //memzerocpy(playvaus.map[cnt], screen[vaus.x+row[y+vaus.y]], vaus.width);
//     blitZERO(playvaus.ofs + hlp, vram + vaus.x+row[y+vaus.y], vaus.width, 1);

     if (y>=2) and (y<(vaus.height-2)) then
        begin
        //screen[vaus.x+row[y+vaus.y]]:=FLASH[vaus.flash];
	
	hlp := vaus.x + row[y+vaus.y];

        putBYTE(vram + hlp, FLASH[vaus.flash]);

        //screen[vaus.x+vaus.width-1+row[y+vaus.y]]:=FLASH[vaus.flash];
        putBYTE(vram + hlp + vaus.width-1, FLASH[vaus.flash]);
        end;
	
     end;
*)

  asm
   fxs FX_MEMS #$00
  end;

end;


{ moves the vaus to coordinates x,y }
procedure move_vaus(x,y : byte);
begin

  { if oldx,oldy coordinates are valid then you have to delete it }
  { from that location }
  if (vaus.oldx <> EMP) and (vaus.oldx <> vaus.x) or (vaus.width <> vaus.oldlen) then remove_vaus;

  vaus.oldx:=vaus.x; { the new coordinates become the old ones }
  vaus.oldy:=vaus.y;

  { x,y coordinates become the new ones          }
  { a coordinate clipping is performed, that is, }
  { if the values at example are too high,       }
  { they are set to the maximum acceptable value }
  { similarly for the minimum                    }

  vaus.x:=x;//max(SCRMIN,min(x,(SCRMAX-vaus.width)));
  vaus.y:=y;//max(SCRTOP,min(y,(SCRBOT-vaus.height)));

  place_vaus;  { call the above place_vaus function }

end;


{ remove a brick from the screen }
procedure remove_block(xa,ya : byte);
var
//    x,y, i: byte;
//    yh : word;
//    cl, shadow: byte;

    i, xs,ys : byte;

begin
  
    xs:=(xa shl 4)+9;      { calculate the coordinates on the screen }
    ys:=(ya shl 3)+22;     { of the brick, e.g., 0.0 ---screen---> 9.22 }

    hlp := row[ys] + xs;

    asm
	fxs FX_MEMS #$80
    end;


    blt.blt_and_mask:=$80;
    blt.blt_xor_mask:=$00;

    blitTEMP(320, 16);
    blitTEMP(playscreen_ofs + hlp, $0200, 16, 8);			// scr
  

(*
    for y:=7 downto 0 do
        begin
        yh:=byte(pattern.width)*mody[ys+y]; { calcola la coord. y relativa alla }
                                      { mattonella di sfondo che deve rim-  }
                                      { piazzare il mattoncino che non c'e' }
                                      { piu' }
        { the brick is replaced with the backdrop, however the backdrop }
        { could be darkened by a shadow cast by another }
        { brick }

	//hlp := row[y+ys] + xs;
	
	i:=y*16;

        for x:=15 downto 0 do
            if (x+xs) < SCRMAX then
               begin
               { calculates any shadow cast by another brick }
               { shadow:=128 no shadow, shadow:=0 there is a shadow }

               //shadow:=playscreen.map[x+xs+row[y+ys]] and 128;
	       shadow:=scr[x+i] and $80;

               { takes the background pixel and adds a shadow if necessary }
               //cl:=(pattern.map[modx[x+xs]+yh] and 127) or shadow;
	       cl:=(pat[modx[x+xs]+yh] and $7f) or shadow;

	       scr[x+i]:=cl;

               { then it puts the color on both the VGA screen and }
               //screen[x+xs+row[y+ys]]:=cl;

               { on the auxiliary screen where only static objects are present }
               { and not moving objects such as balls or vaus. }
               //playscreen.map[x+xs+row[y+ys]]:=cl;
               end;

        end;
*)

    blt.blt_and_mask:=$7f;
    blt.blt_xor_mask:=$00;

    blt.blt_control := 3;	// OR
    
    blitTEMP(pattern_temp + hlp, $200, 16,8);



    blt.blt_and_mask:=$ff;
    blt.blt_xor_mask:=$00;

    blt.blt_control := 0;

    //blitTEMP(16, 320);
    //blitTEMP($200, playscreen_ofs + hlp, 16,8);
    //blitTEMP($200, vram + hlp, 16,8);

    blitSCR(16,320,16,8);



    { In any case, when the brick disappears, its shadow must also disappear }
    { The shadow is nothing more than a small rectangle of the same size as the }
    { brick but shifted 8 pixels on the x-axis and 4 }
    { pixels on the y-axis. In other words, the edge of the shadow coincides with }
    { the center of the brick }

    inc(xs, 8);
    hlp := row[ys+4] + xs;// + 8;


    blt.blt_and_mask:=$7f;
    blt.blt_xor_mask:=$80;

    blitTEMP(320, 320);
    
    if byte(xs+17) > SCRMAX then 
     i:=8//xs+17 - SCRMAX
    else
     i:=17;
    

//    blitTEMP(playscreen_ofs + hlp, playscreen_ofs + hlp, i, 9);
//    blitTEMP(vram + hlp, vram + hlp, i, 9);
     blitTEMP2TEMP(i,9);


(*
    i:=0;

//    for y:=ys+4 to ys+12 do begin
    for y:=8 downto 0 do begin

        //for x:=xs+8 to xs+24 do
        for x:=16 downto 0 do

            { It is necessary to check that the coordinates are not greater than }
            { those of the playing field because in that case there is no }
            { shadow to remove since the last brick casts a }
            { partial shadow that is not reflected on the side wall }
            { Therefore, there is no shadow to remove on the side wall }

            { The same argument does not apply to the minimum, since }
            { the shadow is always further to the right and lower than the brick }
            { that casts it, so no brick can cast a }
            { shadow on the left wall. Nevertheless, no brick }
            { is low enough to cast a shadow on the wall. }

            { Therefore, the only case to consider is x<SCRMAX. }

            if x+xs{+8} < SCRMAX then
               begin
               { prende il colore di sfondo e toglie l'ombra }
               //cl:=playscreen.map[x+row[y]] or 128;

	       pat[x+i] := pat[x+i] or $80;

               { e lo memorizza sia sullo schermo fisico ...}
               //screen[x+row[y]]:=cl;

               { che su quello virtuale (cioe' quello che tiene solo }
               { gli oggetti fissi }
               //playscreen.map[x+row[y]]:=cl;
               end;

	inc(i, 17);

    end;

  
    blitPAT;
*)


    blt.blt_and_mask:=$ff;
    blt.blt_xor_mask:=0;

    blt.blt_control := 0;


    asm
	fxs FX_MEMS #$00
    end;

end;


procedure place_block(xa,ya,block : byte);
var
    xs,ys, x,y, i: byte;
    
//    yh: word;

//    cl,
    cl2: byte;

//    shadow: byte;

begin

    xs:=(xa shl 4)+9;   { calculate the coordinates on the screen relative }  // xa = [0..12]
    ys:=(ya shl 3)+22;  { to the brick xa,ya }				      // ya = [0..14]

    hlp := row[ys] + xs;

    asm
	  fxs FX_MEMS #$80
    end;

    
    blitTEMP(320, 320);
    blt.blt_and_mask := $80;
    blt.blt_xor_mask := $00;

//    blitTEMP(playscreen_ofs + hlp, playscreen_ofs + hlp, 16, 8);
//    blitTEMP(vram + hlp, vram + hlp, 16, 8);
    blitTEMP2TEMP(16,8);


    blt.blt_and_mask := $80;
    blt.blt_xor_mask := (COLORBLOCK[byte(block-1) and 15] and $7f);

//    blitTEMP(playscreen_ofs + hlp, playscreen_ofs + hlp, 15, 7);
//    blitTEMP(vram + hlp, vram + hlp, 15, 7);
    blitTEMP2TEMP(15,7);   

    blt.blt_and_mask := $ff;
    blt.blt_xor_mask := $00;


(*
    for y:=7 downto 0 do begin
    
        i:=y*16;

        for x:=15 downto 0 do
            begin
            { check if any bricks are at the specified coordinates }
            { cast a shadow }
            //shadow:=playscreen.map[xs+x+row[ys+y]] and 128;
	    shadow := scr[x + i] and $80;

            if (y<7) and (x<15) then
                begin
                { if it is the inside of the brick, draw it in the }
                { color specified in block }

                cl:=(COLORBLOCK[byte(block-1) and 15] and $7f) or shadow;

		scr[x + i] := cl;

                //screen[xs+x+row[ys+y]]:=cl;

                //playscreen.map[xs+x+row[ys+y]]:=cl;
                end
            else
               begin
               { if the coordinates are on the right or bottom edge, }
               { draw the pixels in black }
	       
	       scr[x + i] := shadow;

               //screen[xs+x+row[ys+y]]:=shadow; { sarebbe shadow or 0 }

               //playscreen.map[xs+x+row[ys+y]]:=shadow; {...quindi shadow }
               end;
	       
            end;

    end;

    blitSCR(16, 320, 16,8);
*)


    hlp := row[ys+4] + xs + 8;
{
    blitTEMP(320, 17);
    blitTEMP(playscreen_ofs + hlp, $0300, 17, 9);
    
    i:=0;
}

    blitTEMP(320, 320);
  
    blt.blt_and_mask:=$7f;
    blt.blt_xor_mask:=$00;

    if byte(xs+8+17) > SCRMAX then 
     i:=8//xs+8+17 - SCRMAX
    else
     i:=17;
    

//    blitTEMP(playscreen_ofs + hlp, playscreen_ofs + hlp, i, 9);
//    blitTEMP(vram + hlp, vram + hlp, i, 9);
    blitTEMP2TEMP(i,9);

    blt.blt_and_mask:=$ff;
    blt.blt_xor_mask:=$00;



(*
    { now draw the shadow of the brick }
    //for y:=ys+4 to ys+12 do begin
    for y:=8 downto 0 do begin
    
        //for x:=xs+8 to xs+24 do
        for x:=16 downto 0 do	
            if x+xs+8<SCRMAX then  { check as in remove_block that the coordinates }
                              { are not beyond the right wall, because }
                              { the shadow is not cast on that wall }
               begin

               { preleva il pixel x,y dallo schermo e ci proietta sopra }
               { l'ombra. }
               //cl:=playscreen.map[x+row[y]] and 127;

	       pat[x + i] := pat[x + i] and $7f;

               { dopo di che lo rimette sullo schermo fisico... }
               //screen[x+row[y]]:=cl;

               { e su quello virtuale }
               //playscreen.map[x+row[y]]:=cl;
               end;

	inc(i, 17);
    end;

    
    blitPAT;
*)

    hlp := row[ys] + xs;

    blitTEMP(320, 16);
    blitTEMP(playscreen_ofs + hlp, $0200, 16, 7);


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


       i:=0;

       { draw the top edge of the brick }
       for y:=6 downto 0 do
           begin

	   //i:=y*16;

           { takes the pixel xs,y+ys from the screen, adds a shadow to it }
           { i.e., makes the color darker }
           //cl:=playscreen.map[xs+row[y+ys]] and 128;
	   //cl := scr[i] and $80;

           cl2:=(cl2 and $7f) or (scr[i] and $80);

           { ... and puts it back on the physical screen }
           //screen[xs+row[ys+y]]:=cl2;

           { ... and on the virtual one }
           //playscreen.map[xs+row[ys+y]]:=cl2;
	   scr[i] := cl2;
	   
	   inc(i, 16);
	   
           end;


       { draw the right edge of the brick }
       for x:=14 downto 0 do
           begin

           { comments similar to above }
           //cl:=playscreen.map[xs+x+row[ys]] and 128;
	   //cl:=scr[x] and $80;

	   scr[x] := (cl2 and $7f) or (scr[x] and $80);

           //screen[xs+x+row[ys]]:=cl2;

           //playscreen.map[xs+x+row[ys]]:=cl2;

           end;

       end;


    blitSCR(16, 320, 15,7);


    asm
	fxs FX_MEMS #$00
    end;

end;


procedure put_wall;  { displays the wall contained in wall[x,y] on the screen }
var
    x,y, i: byte;

begin

    for y:=0 to 14 do begin
   
	i:=y*16;
    
        for x:=0 to 12 do
            if wall[x + i] <> 0 then place_block(x,y, wall[x + i]);
    end;

(*

a = white   { normal falls with one hit }
b = orange                ''
c = cyan                  ''
d = green                 ''
e = red                   ''
f = blue                  ''
g = purple                ''
h = yelow                 ''
i = gray    { requires multiple hits to knock down }
j = brown   { indestructible                       }

*)

end;


procedure set_wall;               { set the wall }
var x,y,wl  : byte;
//    name    : string;

begin
    remain_blk:=0;                { these are the blocks to be destroyed }
    wl:=score.wall_n[cur_player]; { this is the wall where the  }
                                  { player cur_player }

    for y:=0 to 14 do             { counts the destructible blocks  }
        for x:=0 to 12 do         { i.e., the block must be <>0 and <>10}
                                  { since 0 = no block, 10 = brown}

            if (wall[byte(x+y*16)] <> 0) and (wall[byte(x+y*16)] <> 10) then inc(remain_blk);

    wl:=byte(wl-1) mod PATNUMBER;

    case wl of
     0: pattern := pattern0;
     1: pattern := pattern1;
     2: pattern := pattern2;
     3: pattern := pattern3;
//     4: pattern := pattern4;
    end;

end;


{ takes the coordinates of two points as input and calculates }
{ the points where the brick grid intersects the segment      }
{ connecting the two points.                                  }

{ the points of intersection can be 1 or 2 }

function split_line(var _x1,_y1,_x2,_y2 : byte) : byte;
var

//    x1,y1,x2,y2: smallint;

    x1: byte absolute $00;
    y1: byte absolute $01;
    x2: byte absolute $02;
    y2: byte absolute $03;

    xp1: byte absolute $04;
    yp1: byte absolute $05;
    xp2: byte absolute $06;
    yp2: byte absolute $07;

    x: byte absolute $08;
    y: byte absolute $09;
    xj: byte absolute $0a;
    yj: byte absolute $0b;
    xh: byte absolute $0c;
    yh: byte absolute $0d;
    xn: byte absolute $0e;
    yn: byte absolute $0f;

    xk,yk,
    xp,yp : byte;

    collision: byte;

    tx, ty: Boolean;

begin

    x1 := _x1;  { access to local variables is the fastest }
    y1 := _y1;
    x2 := _x2;
    y2 := _y2;

    inc(x1,16);          { increases the coordinates of all points  }
    inc(y1,24);          { to prevent any coordinates from becoming }
    inc(x2,16);          { negative during operations  }
    inc(y2,24);          { before ending the process, it resets them  }

    collision:=0;        { number of intersections between segment and grid }

    xp1:=byte(x1) shr 4; { calculate which brick contains the two points in question}
    yp1:=byte(y1) shr 3;
    xp2:=byte(x2) shr 4;
    yp2:=byte(y2) shr 3;

    xk:=x1;              { temporarily copy the coordinates of the two points }
    yk:=y1;              { into two vectors so that you can operate freely }
    xj:=x2;              { the initial coordinates are passed by address }
    yj:=y2;              { and therefore the values must not be lost }

    xh:=x1;
    yh:=y1;
    xn:=x2;
    yn:=y2;


    { If this “if” is true, it means that there is a bug in the program   }
    { and therefore the game quits immediately, reporting the error.      }
    { This error occurs easily if MAXSPEED is set to >> 2000              }

//    if (abs(x1-x2)>16) or (abs(y2-y1)>8) then
//       fatal_error(err1);

    tx := (xp1 <> xp2);   
    ty := (yp1 <> yp2);


    if {(xp1<>xp2) or (yp1<>yp2)} tx or ty then   { if the two points do not coincide... }
       begin
       
       if {(yp1<>yp2)} ty then     { if the two points have different y-values }
          begin
          collision:=collision or 1; { the lowest bit is set to 1 }

          while ((yn and 7) <> 0) and ((yn and 7)<>7) do
            begin
            x:=(xh+xn) shr 1; { after which it continues to divide the segment }
            y:=(yh+yn) shr 1; { (x1,y1)-(x2,y2) until it finds an intersection }
                              { with a lattice                                 }
            yp:=y shr 3;

            if yp=yp1 then    { of the three points (two are the endpoints }
               begin          { of the segment), it discards one using the }
               xh:=x;         { same principle as Weierstrass' theorem.    }
               yh:=y;
               end;

            if yp=yp2 then    { the midpoint replaces one of the }
               begin          { two extremes so that the segment }
               xn:=x;         { still straddles the lattice.     }
               yn:=y;
               end;
            end;

          end;

       if {(xp1<>xp2)} tx then     { if the two points have different x-coordinates ...}
          begin
          collision:=collision or 2;  { in this case, set the second bit }

          while ((xj and 15) <> 0) and ((xj and 15) <> 15) do
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

    else exit(0);// begin {dpoke($600, x1); dpoke($602, y1); poke($604, x2); dpoke($606, y2);}   fatal_error(collision or $80); end;
    { otherwise something went wrong! }


    //x1:=min(207,max(0,x1));
    if x1 < 0+16 then x1 := 0+16;
    if x1 > 207+16 then x1 := 207+16;
    
    //x2:=min(207,max(0,x2));
    if x2 < 0+16 then x2 := 0+16;
    if x2 > 207+16 then x2 := 207+16;    

//    dec(x1,16);   { restore the old coordinates }
//    dec(y1,24);
//    dec(x2,16);
//    dec(y2,24);

    _x1 := x1-16;
    _y1 := y1-24;
    _x2 := x2-16;
    _y2 := y2-24;

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
    { Check that the block coordinates are valid numbers... }
    if {(xb>=0) and} (xb<=12) and {(yb>=0) and} (yb<=14) then
       begin
       
       i:=xb+yb*16;

       if wall[i] <> 0 then    { ... that there is a block to hit ... }
          begin
          if wall[i] < 10 then { if the blockade can be broken ... }
             begin
             remove_block(xb,yb); { ..removes it from the screen }
             dec(remain_blk);     { ..decreases the number of blocks remaining }

             { Increase the current player's SCORE depending on }
             { the block hit (the points are in the array in SCORE_WALL). }
             inc(score.player[cur_player],SCORE_WALL[wall[i]]);

             inc(lett.incoming,rand(LETTER_PROB));

             lett.nextx:=(xb shl 4)+9;
             lett.nexty:=((yb+1) shl 3)+22;
             lett.nexttype:=random_letter_drop;

             wall[i]:=0;              { the block is canceled               }
             //ball_block_sound(440,3); { emits an A (musical note)           }
	     sfx.init(sfx_ball_brick);

             ball.sbd:=0;             { resets the deviation counter        }
             ball.brwhit:=0;          { and the emergency deviation counter }
             end

 
          else    { if the block is brown, or a gray that does not fall off immediately }
             begin
             if (wall[i] and 15)=9 then { ...if it's gray... }
                begin
                ball.brwhit:=0;         { reset the emergency deviation counter }
                dec(wall[i],16);        { decreases the resistance of the block }

                //ball_block_sound(370,4);{ It emits an F# (musical note). }
		sfx.init(sfx_hard_brick);

                shine(xb,yb);           { and set the block's sparkle }
                end
             else
                begin
                inc(ball.brwhit); { increase the emergency development fund }
                shine(xb,yb);     { set the sparkle }

                //ball_block_sound(200,7); { and emits a rather low note }
		sfx.init(sfx_solid_brick);
		
                end;
             end;
          end;
       end;
end;


{ Similar to the first one, but for the collision fire_block }
procedure shoot_block_with_fire(xb,yb : byte);
var i: byte;
begin
    if {(xb>=0) and} (xb<=12) and {(yb>=0) and} (yb<=14) then
       begin
       
       i:=xb+yb*16;
       
       if wall[i] <> 0 then    { ... that there is a block to hit... }
          begin
          if wall[i] < 10 then { if the blockade can be broken... }
             begin
             remove_block(xb,yb); { ..removes it from the screen }
             dec(remain_blk);     { ..decreases the number of blocks remaining }
             inc(score.player[cur_player],SCORE_WALL[wall[i]]);
             wall[i]:=0;              { the block is deleted }
             //ball_block_sound(440,3); { emits an A (musical note) }
	     
	     //sfx.init(sfx_ball_brick);
             end

          else    { if the block is brown, or a gray that does not fall off immediately }
             begin
             if (wall[i] and 15)=9 then { ...if it's gray... }
                begin
                dec(wall[i],16); { decreases the resistance of the block }
                //ball_block_sound(370,4);{ It emits an F# (musical note). }
		
		sfx.init(sfx_hard_brick);
	
                shine(xb,yb);           { and set the block's sparkle }
                end
             else
                begin
                shine(xb,yb);     { set the sparkle }
                //ball_block_sound(200,7); { and emits a rather low note }
		
		sfx.init(sfx_solid_brick);
                end;
             end;
          end;
       end;
end;


procedure ball_hit_block(var ball : BALLTYPE);
var 

//    x,y, i: byte;
//    xb,yb    : byte;
    
    x: byte absolute $10;
    y: byte absolute $11;
    i: byte absolute $12;
    xb: byte absolute $13;
    yb: byte absolute $14;  

//    lx,ly,
//    mx,my    : shortint;

    lx: shortint absolute $15;
    ly: shortint absolute $16;
    mx: shortint absolute $17;
    my: shortint absolute $18;

//    emergency,
//    mimax,
//    deflect,
//    around,
//    collision,
//    touch    : byte;

    emergency: byte absolute $19;
    mimax: byte absolute $1a;
    deflect: byte absolute $1b;
    around: byte absolute $1c;
    collision: byte absolute $1d;
    touch: byte absolute $1e;

//    yes      : Boolean;

    yes: Boolean absolute $1f;

//    sp, angle,
//    ox,oy,
//    nx,ny    : smallint;

    sp: smallint absolute $20;
    angle: smallint absolute $22;

    ox: byte absolute $24;
    oy: byte absolute $25;
    nx: byte absolute $26;
    ny: byte absolute $27;

//    myx,myy  : byte;
//    a,b      : byte;

    myx: byte absolute $28;
    myy: byte absolute $29;
    a: byte absolute $2a;
    b: byte absolute $2b;

//    f1,f2    : word;

    f1: word absolute $2c;
    f2: word absolute $2e;

    adjw     : array[0..3,0..3] of byte absolute $0000;

begin
    emergency:=EMP;   { the emergency rebound indicator }

    nx:=ball.x-9;     { nx,ny have the coordinates of the ball with respect  }
    ny:=ball.y-22;    { to the origin fixed in the Northwest corner of field }
                      { of play (within which the ball moves).               }

    ox:=ball.oldx-9;  { ditto for the old coordinates, the origin }
    oy:=ball.oldy-22; { is so the screen point (9,22).            }

    xb:=nx shr 4;     { xb,yb are the coordinates of the (possibly hypothetical) }
    yb:=ny shr 3;     { block on which the ball is now located. Remember that    }
                      { (0,0) is the block in the upper right-hand corner        }

    i:=xb+yb*16;


{ collision test with blocks touching only at the corners }

    if (i > 16) and (xb > 0) and (xb < 12) then
    if (wall[i] = 0) then begin

      x:=byte(nx and 15) shr 1;     { you calculate the impact point of   }
      y:=(ny and 7);                { the ball with respect to the brick. }


      if (x=y) or (x=byte(7-y)) then begin
  
 
          around:=0;
          deflect:=0;
          touch:=0;
 
          { 0 1 }
          { 2 3 }
	  

          if (x=7) then touch:=touch or 1;
          if (y=7) then touch:=touch or 2;

          {        -------------                      }
          {        | 1 | 2 | 4 |                      }
          {        -------------                      }
          {        |128| U | 8 |   U = bumped brick   }
          {        -------------                      }
          {        | 64| 32| 16|                      }
          {        -------------                      }


          if (wall[byte(i-1)] <> 0) then around:=around or $80;
          if (wall[byte(i+1)] <> 0) then around:=around or 8;
          if (wall[byte(i-16)] <> 0) then around:=around or 2;
          if (wall[byte(i+16)] <> 0) then around:=around or $20;
      
         if around <> 0 then begin

          if (wall[byte(i-1-16)] <> 0) then around:=around or 1;
          if (wall[byte(i+1-16)] <> 0) then around:=around or 4;
          if (wall[byte(i-1+16)] <> 0) then around:=around or $40;
          if (wall[byte(i+1+16)] <> 0) then around:=around or $10;


	  case touch of
	  
	   0: if (around and 131 = 130) then       { upper left corner }
	      if (nx < ox) and (ny < oy) then
              begin
	       deflect:=$11;

	       shoot_block(xb,yb-1,ball);
	       shoot_block(xb-1,yb,ball);

              end;

           1: if (around and 14 = 10) then       { upper right corner }
              if (nx > ox) and (ny < oy) then
	      begin
               deflect:=$21;

	       shoot_block(xb,yb-1,ball);
	       shoot_block(xb+1,yb,ball);

              end;

           2: if (around and 224 = 160) then       { Bottom left corner }
	      if (nx < ox) and (ny > oy) then
              begin
               deflect:=$12;

 	       shoot_block(xb-1,yb,ball);
	       shoot_block(xb,yb+1,ball);

              end;

           3: if (around and 56 = 40) then       { Bottom right corner }
	      if (nx > ox) and (ny > oy) then
              begin
               deflect:=$22;

	       shoot_block(xb+1,yb,ball);
	       shoot_block(xb,yb+1,ball);

              end;

	  end;  

	     
	  if deflect <> 0 then begin
	    ball.speedx:=-ball.speedx;
	    ball.speedy:=-ball.speedy;

	    exit;
          end;


         end; // if around <> 0


	 end;
 
      end;



    if (wall[i] <> 0) then  { ...if the block is not hypothetical but exists }
       begin
       
       collision:=split_line(ox,oy,nx,ny);

       { calculates the intersection of the segment connecting the old and }
       { new coordinates. “Collision” contains a value that depends on the }
       { type of intersections found between the segment and the grid of   }
       { blocks.                                                           }
       
       if collision = 0 then fatal_error($FF);


       if collision=3 then     { if two collisions have occurred... }
          begin
          lx:=ball.oldx-ox-9;  { the distance of the old coordinate }
          ly:=ball.oldy-oy-22; { from intersection point 1 and      }

          mx:=ball.oldx-nx-9;  { intersection point 2 is calculated }
          my:=ball.oldy-ny-22;

          if lx < 0 then
	   a := -lx
	  else
	   a := lx;

          if ly < 0 then
	   b := -ly
	  else
	   b := ly;

          f1:=sqrtable[a] + sqrtable[b];        { indi chooses between the two the  }

          if mx < 0 then
	   a := -mx
	  else
	   a := mx;

          if my < 0 then
	   b := -my
	  else
	   b := my;

          f2:=sqrtable[a] + sqrtable[b];        { intersection point closest to the old coord. }

          if f1 < f2 then                       { f1 and f2 are the square of the modulus }
                                                { of distance vector (see above)          }

             { Consider the case where the closest intersection is number 1. }

             begin
	     xb:=ox shr 4;
             //xb:=min(12,max(i,0));           { Coordinates are assigned }
	     if xb > 12 then xb := 12;
             yb:=(byte(oy+24) shr 3)-3;      { of the block related to such }
                                             { intersection.                }

             if wall[byte(xb+yb*16)]=0 then  { If there is no blockage }
                begin
		xb:=nx shr 4;
                //xb:=min(12,max(0,i));        { Then the collision occurs at the }
		if xb > 12 then xb := 12;
                yb:=(byte(ny+24) shr 3)-3;     { other intersection. No. 2  }
                end
             else
                begin                        { If, on the other hand, the block exists }
                nx:=ox;                      { then the new coordinates are assigned   }
                ny:=oy;                      { the intersection point contained        }
                                             { in the old ones. }
                end;
             end
          else
             begin
             { If it is the second intersection closest to the }
             { old coordinates, proceed in the same way.       }

	     xb:=nx shr 4;
             //xb:=min(12,max(0,i));        { We calculate the coordinates of the block  }
	     if xb > 12 then xb := 12;
             yb:=(byte(ny+24) shr 3)-3;   { on the intersection nx,ny (the second one) }

             if wall[byte(xb+yb*16)]=0 then    { If the blockade is not there... }
                begin
                nx:=ox;                   { then the valid intersection is }
                ny:=oy;                   { the other, and it goes on...   }

		xb:=nx shr 4;
                //xb:=min(12,max(0,i));         { ...reassigning to the new  }
		if xb > 12 then xb := 12;
                yb:=(byte(ny+24) shr 3)-3;    { coord. the intersection #1 }
                end;
             end;

          end;

       ball.x:=nx + 9;    { The new ball coordinates are those contained    }
       ball.y:=ny + 22;   { in the variables nx,ny, retranslating the axes. }
                          { nx,ny had their axes centered in (9,22).        }

	//shoot_block(xb,yb,ball);  { breaks down the block in question }

       x:=byte(nx and 15) shr 1;     { you calculate the impact point of   }
       y:=(ny and 7);                { the ball with respect to the brick. }

       { Dividing the coord. x of the impact by 2 gives a cross section   }
       { on a square brick instead of a rectangular one, which simplifies }
       { in following the calculations. The brick is in fact 16x8 pixels  }
       { by dividing by 2 becomes 8x8 pixels, and the calculations on the }
       { diagonals are more simple. }


       { If the bump does not occur on one of the edges of the brick then }
       { it wants to say that something went wrong. In theory it should   }
       { never occur.                                                     }

//       if (x<>0) and (x<>7) and (y<>0) and (y<>7) then
//          fatal_error(err3);


       { These are the values assumed by EMERGENCY depending on the point of impact }

       {                     5     1     8                               }
       {                      -----------                                }
       {                    2 |  brick  | 4                              }
       {                      -----------                                }
       {                     6     3     7                               }

       { If the ball hits the top edge of the brick... }

       if (y<x) and (x<byte(7-y)) then
          begin
          ball.speedy:=-ball.speedy;   { It reverses the y-coordinate of vel.    }
          emergency:=1;                { and marks the eventual point of contact }
          end;

       { ...the bottom edge... }
       if (byte(7-y) < x) and (x<y) then
          begin
          ball.speedy:=-ball.speedy;   { Reverses the y of the vel. }
          emergency:=3;  
          end;

       { ...the left edge... }
       if (x<y) and (y<byte(7-x)) then
          begin
          ball.speedx:=-ball.speedx;   { Reverses the x of the vel.}
          emergency:=2;
          end;

       { ...and the right one... }
       if (byte(7-x)<y) and (y<x) then
          begin
          ball.speedx:=-ball.speedx;   { Reverses the x of the vel. }
          emergency:=4;
          end;


        yes := (x=y) or (x=byte(7-y));

	if (yes = false) and (emergency < 5) then shoot_block(xb,yb,ball);  { breaks down the block in question }


       { ... if it occurs on one of the four edges instead ... }
//       if (x=y) or (x=byte(7-y)) then
       if yes then
          begin

          deflect:=$00;
          touch:=0;
	  around:=0;

          { touch takes different values depending on the corner }
          { Follows the table (e.g., 0 = upper left corner)      }

          { 0 1 }
          { 2 3 }

          if x>4 then touch:=touch or 1;
          if y>4 then touch:=touch or 2;


          { Here, fill a 3x3 matrix with 1s or 0s depending on whether }
          { there are other bricks around the blocked block or not     }

          { The left and right edges of the playing field are  }
          { considered as indestructible bricks in this case.  }
	  
          for lx:=-1 to 1 do
              for ly:=-1 to 1 do
                  begin

                  //mx:=max(min(xb+lx,12),0); { When referring to x, the coordinate }
		  mx:=xb+lx;                  { must be between 0 and 12.           }
		  //if mx < 0 then mx := 0;
		  //if mx > 12 then mx := 12;

                  my:=yb+ly;

                  if (mx < 0 ) or
                     (mx > 12) or
                     (wall[byte(mx)+byte(my)*16] <> 0) then
                        adjw[byte(lx+1),byte(ly+1)] := 10  { There are bricks }
                  else
                     adjw[byte(lx+1),byte(ly+1)] := 20     { There are no bricks }

                  end;

          { Around contains a value that represents the state }
          { of the bricks surrounding the brick that was hit. }

          {        -------------                      }
          {        | 1 | 2 | 4 |                      }
          {        -------------                      }
          {        |128| U | 8 |   U = bumped brick   }
          {        -------------                      }
          {        | 64| 32| 16|                      }
          {        -------------                      }

          { Example:                                                }
          { if bricks 1, 2, and 128 are located around U, the value }
          { of around is 1+2+128=131.                               }

{	  
          around:=(adjw[0,0] and $01) or 
	          (adjw[1,0] and $02) or
                  (adjw[2,0] and $04) or 
		  (adjw[2,1] and $08) or
                  (adjw[2,2] and $10) or 
		  (adjw[1,2] and $20) or
                  (adjw[0,2] and $40) or 
		  (adjw[0,1] and $80);
}

        asm
	lda #10

	cmp adr.ADJW
	ror around
	cmp adr.ADJW+$04
	ror around
	cmp adr.ADJW+$08
	ror around	
	cmp adr.ADJW+$09
	ror around
	cmp adr.ADJW+$0A
	ror around
	cmp adr.ADJW+$06
	ror around
	cmp adr.ADJW+$02
	ror around
	cmp adr.ADJW+$01
	ror around
	end;
	

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
	     a := around and 131;

	     case a of

                0: deflect:=$11;
                1: begin deflect:=$33; shoot_block(xb-1,yb-1,ball); end;
                2: begin deflect:=$10; {shoot_block(xb,yb-1,ball);} end;
                3: begin deflect:=$12; shoot_block(xb-1,yb-1,ball); end;
              128: begin deflect:=$01; {shoot_block(xb-1,yb,ball);} end;
              129: begin deflect:=$21; shoot_block(xb-1,yb-1,ball); end;
              130: begin deflect:=$11; shoot_block(xb-1,yb,ball); shoot_block(xb,yb-1,ball); end;

	     end; 

	     if a <> 130 then shoot_block(xb,yb,ball);
 
             emergency:=5;
             end;


          { “and 14” are the bricks 2+4+8, the others don't matter }

          if touch=1 then       { upper right corner }
             begin
	     a := around and 14;

	     case a of

                0: deflect:=$21;
                2: begin deflect:=$20; {shoot_block(xb,yb-1,ball);} end;
                4: begin deflect:=$33; shoot_block(xb+1,yb-1,ball); end;
                6: begin deflect:=$22; shoot_block(xb+1,yb-1,ball); end;
                8: begin deflect:=$01; {shoot_block(xb+1,yb,ball);} end;
               10: begin deflect:=$21; shoot_block(xb,yb-1,ball); shoot_block(xb+1,yb,ball); end;
               12: begin deflect:=$11; shoot_block(xb+1,yb-1,ball); end;

	     end;

	     if a <> 10 then shoot_block(xb,yb,ball);

             emergency:=8;
             end;


          if touch=2 then       { Bottom left corner }
             begin
	     a := around and 224;

	     case a of

                0: deflect:=$12;
               32: begin deflect:=$10; {shoot_block(xb,yb+1,ball);} end;
               64: begin deflect:=$33; shoot_block(xb-1,yb+1,ball); end;
               96: begin deflect:=$11; shoot_block(xb-1,yb+1,ball); end;
              128: begin deflect:=$02; {shoot_block(xb-1,yb,ball);} end;
              160: begin deflect:=$12; shoot_block(xb-1,yb,ball); shoot_block(xb,yb+1,ball); end;
              192: begin deflect:=$22; shoot_block(xb-1,yb+1,ball) end;
	      
	     end; 

	     if a <> 160 then shoot_block(xb,yb,ball);

             emergency:=6;
             end;


          if touch=3 then       { Bottom right corner }
             begin
	     a := around and 56;

	     case a of
	     
                0: deflect:=$22;
                8: begin deflect:=$02; {shoot_block(xb+1,yb,ball);} end;
               16: begin deflect:=$33; shoot_block(xb+1,yb+1,ball); end;
               24: begin deflect:=$12; shoot_block(xb+1,yb+1,ball); end;
               32: begin deflect:=$20; {shoot_block(xb,yb+1,ball);} end;
               40: begin deflect:=$22; shoot_block(xb+1,yb,ball); shoot_block(xb,yb+1,ball); end;
               48: begin deflect:=$21; shoot_block(xb+1,yb+1,ball); end;

	     end;

	     if a <> 40 then shoot_block(xb,yb,ball);

             emergency:=7;
             end;

          { The first hex (hexadecimal) digit is put in myx }
          { and the second in myy. }

          myx := deflect shr 4;
          myy := deflect and 15;
	  
	  if ball.speedx < 0 then
	   sp := -ball.speedx
	  else
	   sp := ball.speedx;

          if myx=1 then ball.speedx:= -sp;
          if myx=2 then ball.speedx:= sp;
          if myx=3 then ball.speedx:= -ball.speedx ;

	  if ball.speedy < 0 then
	   sp := -ball.speedy
	  else
	   sp := ball.speedy;

          if myy=1 then ball.speedy:= -sp;
          if myy=2 then ball.speedy:= sp;
          if myy=3 then ball.speedy:= -ball.speedy ;

          end;

       end;


    { In case the number of indestructible bricks bumped consecutively   }
    { before bumping a brick of another type exceeds a certain threshold }

    if ball.brwhit > MAXBRWHIT then
       begin
       { If emergency stayed in EMP, it means something went wrong. }
       if emergency=EMP then fatal_error(err4);

       mimax:=EMERG_DEV[emergency]; { Otherwise the deviation is calculated }
				    { maximum and minimum of the brick.     }

      
        { and depending on which edge is bumped and how the bricks are }
	{ around that edge, the deflection is changed.                 }

	{ as strange as the final bounce may be, this control is done  }
	{ to prevent the ball from getting stuck in an infinite loop   }

	{ obviously the case applies to indestructible bricks because  }
	{ the others sooner or later fall and therefore cannot block   }
	{ the ball for an infinite time.                               }

	{ Each hex digit of mimax expresses an angle at multiples of 90 degrees }
	{ the first digit is the minimum angle, the second the maximum angle.   }

	{ E.g., MIMAX:=$03; singifies minimum angle 0*90 = 0 degrees, max angle }
	{ 3*90 = 270 degrees, and so on...                                      }

	{ a writing such as "mimax:=mimax and $0f or 10" means     }
	{ put the first digit of mimax to 1 regardless of how much }
	{ applies now, leaving the second unchanged.               }

	{ Similar reasoning for "... and $f0 or $03" acting }
	{ on the second digit instead of the first...       }


{	  
          around:=(adjw[0,0] and $01) or 
	          (adjw[1,0] and $02) or
                  (adjw[2,0] and $04) or 
		  (adjw[2,1] and $08) or
                  (adjw[2,2] and $10) or 
		  (adjw[1,2] and $20) or
                  (adjw[0,2] and $40) or 
		  (adjw[0,1] and $80);
}

       case emergency of

         5: begin
            if {adjw[1,0]=0} around and $02 = 0 then mimax:=(mimax and $0f) or $00;
            if {adjw[0,1]=0} around and $80 = 0 then mimax:=(mimax and $f0) or $03;
            end;

         6: begin
            if {adjw[0,1]=0} around and $80 = 0 then mimax:=(mimax and $0f) or $10;
            if {adjw[1,2]=0} around and $20 = 0 then mimax:=(mimax and $f0) or $04;
            end;

         7: begin
            if {adjw[1,2]=0} around and $20 = 0 then mimax:=(mimax and $0f) or $20;
            if {adjw[2,1]=0} around and $08 = 0 then mimax:=(mimax and $f0) or $05;
            end;

         8: begin
            if {adjw[2,1]=0} around and $08 = 0 then mimax:=(mimax and $0f) or $30;
            if {adjw[1,0]=0} around and $02 = 0 then mimax:=(mimax and $f0) or $06;
            end;

         end;


       repeat

          ox:=mul90_16[mimax shr 4];    { the first digit of mimax is placed in }
          oy:=mul90_16[mimax and 15];   { ox and the second in oy.              }

          angle := rand(oy-ox) + ox;    { Angle is a random variable between ox and oy }

       until (mod90(angle) > 30) and (mod90(angle) < 60);
       { and this cycle repeats until the ball has an inclination }
       { between 30 and 60 degrees plus multiples of 90 degrees.  }
       
       set_ball_direction(ball, mod360(angle));
       set_ball_speed(ball, ball.speed);

       ball.brwhit:=0; { Reset emergency counter }
       end;

end;


{ Draw the backdrop on the playing field }
procedure fill_picture_with_pattern;
var yb: word;
    x, y, i: byte;
//    cl, shadow: byte;
begin

    { Runs the main loop and the secondary loop filling the }
    { screen with as many background squares as needed      }


    asm
	fxs FX_MEMS #$80
    end;


    { we copy the pattern using VBXE pattern_feature, which replaces the need to use the MODX array }

    blitTEMP(pattern.width, 320);

    blt.blt_and_mask := $7f;
    blt.blt_xor_mask := $80;

    blt.pattern_feature:=(pattern.width-1) or $80;

    hlp := row[SCRTOP-2] + SCRMIN-1;

 
    yb:=0;
    i:=0;
    
    for y:=SCRTOP-2 to SCRBOT-2 do begin

        blitTEMP(pattern.ofs + yb, playscreen_ofs + hlp, SCRMAX-SCRMIN+1, 1);
     
	inc(hlp, 320);
	
	inc(yb, pattern.width);
	
	inc(i);
	if i = pattern.height then begin
	  yb:=0;
          i:=0;
	end;
	
    end;

    blt.pattern_feature:=0;


// kopiujemy ekran bez cieni do pattern_temp

    blitTEMP(320, 320);

    blt.blt_and_mask:=$ff;
    blt.blt_xor_mask:=$00;   
   
    blitTEMP(playscreen_ofs, pattern_temp, 320, 200);	// playscreen_ofs -> pattern_temp
 


// dodajemy cienie z lewej strony i od góry

    i:=SCRMAX-1;

    for y:=SCRTOP-2 to SCRBOT-2 do
        begin

        //yb:=mody[y]*patt.width;

	blitTEMP(playscreen_ofs + row[y], $0200, SCRMAX, 1);
	
	
	if y>=16 then i:=17;
	

        for x:=SCRMIN-1 to i do
            begin
            //cl:=patt.map[modx[x]+yb]; { Takes the pixel from the background }
	    
	    //cl := pat[ modx[x] + yb ];

            //shadow:=128;              { Shadow = 128 -> shadow not present }

            { It makes the shadow on the left and upper side of the screen }
            { It is the shadow cast by the metal edge on the background of }
            { play. }
            if (x<18) or (y<16) then //shadow:=0; { Shadow=0 -> shadow present }
	     scr[x] := scr[x] and $7f;
	    //else
	     //scr[x] := cl or $80;

            { Draw the pixel on the screen with any shadow }
            //playscreen.map[x+row[y]]:=(cl and 127) or shadow;

	    //scr[x] := (cl {and $7f}) or shadow;
	    
            end;

	blitTEMP($200, playscreen_ofs + row[y], SCRMAX, 1);

        end;


   
    asm
	fxs FX_MEMS #$00
    end;

end;


procedure write_round_level;
var x,y : smallint;            { Print the text ROUND xx, READY. }
//    s,r,                      { possibly also the player's name }
//    sc  : string[20];         { cioe' PLAYER ONE o PLAYER TWO.  }

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


procedure remove_round_level;  { Remove the ROUND xx, READY label. }
//var y : byte;                { by copying the background onto it. }
begin

    hlp:=row[129] + 72;

    blitBOX(88, 160-129);

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
var a,b,w,x,y  : byte;
    
    z, vm: word;

begin
    playvaus:=normal;
    modify_vaus;

    move_vaus(vaus.x, vaus.y);

    a:=vaus.x-4;     { Si calcola uno spostamento dovuto al brush  }
    b:=vaus.y-5;     { dell'animazione che e' leggermente spostato }
                     { dall'origine degli assi.                    }

    sfx.init( sfx_vaus_destroyed );

    asm
	fxs FX_MEMS #$80
    end;

    blitTEMP(explosion_width, explosion_width);

    vm:=0;

    for w:=0 to 6 do  { w = frame to display, cycles through all frames from 0 to 6 }
        begin
	
	z:=vm;

        for y:=0 to 15 do
            begin

	    //z:=y*explosion_width+w*(explosion_width shl 4);

	    blitTEMP(explosion_ofs + z, $280, explosion_width, 1);

	    hlp := a + row[y+b];

	    blitTEMP(playscreen_ofs + hlp, $200, explosion_width, 1);

            for x:=0 to explosion_width-1 do
                begin
                { If the color is transparent or the frame is 6 }
                { then the background color is used. }
                if (w=6) or (pom[x] = 0) then
                   //screen[x+a+row[y+b]]:=playscreen.map[x+a+row[y+b]]
                   //blitBYTE(playscreen_ofs + hlp + x, vram + hlp + x)
		   
		   //scr[x] := scr[x]
                else
                   //screen[x+a+row[y+b]]:=explosion.map[x+z];
                   //blitBYTE(explosion.ofs + x+z, vram + hlp + x)
		   
		   scr[x] := pom[x];
                end;

	    blitTEMP($200, vram + hlp, explosion_width, 1);

	    inc(z, explosion_width);
            end;

	inc(vm, explosion_width shl 4);

	pause;
	pause;
        //death_sound(w);   { Il cicalino di quando il vaus viene distrutto }
                          { per ogni valore di w c'e' una nota diversa    }
        end;

    asm
	fxs FX_MEMS #$00
    end;

    mydelay(150);         { attende qualche istante. }
    disable_letter;       { se nel frattempo stava scendendo una lettera, }
                          { la toglie.                                    }
end;


{ It's exactly like the one before, only it shows the animation }
{ of the vaus being built.                                      }
procedure create_vaus;
var x,y,w  : byte;
    z, j,mw  : word;
    
    a, b: byte;

begin
    nosound;

    a:=((SCRMAX-SCRMIN) shr 1)-13;
    b:=vaus_line-5;

    asm
	fxs FX_MEMS #$80
    end;

    blitTEMP(newvaus.width, newvaus.width);

    mw:=(byte(newvaus.width)*16);

    j:=0;

    for w:=11 downto 0 do
        begin
	
	z:=j;
        for y:=0 to 15 do
            begin
	               
	    //z:=y*newvaus.width+w*(newvaus.width*16);

	    hlp := a + row[y+b];

	    blitTEMP(newvaus.ofs + z, $280, newvaus.width, 1);		// pom

	    blitTEMP(playscreen_ofs + hlp, $200, newvaus.width, 1);	// scr

	    for x:=0 to newvaus.width-1 do
                begin
                if pom[x] = 0 then

                   //screen[x+a+row[y+b]]:=playscreen.map[x+a+row[y+b]]
                   //blitBYTE(playscreen_ofs + x+a+row[y+b], vram + x+a+row[y+b])

		   //scr[x] := scr[x]

                else
                   //screen[x+a+row[y+b]]:=newvaus.map[x+z];
                   //blitBYTE(newvaus.ofs + x+z, vram + x+a+row[y+b])

		   scr[x] := pom[x]
                end;

	    blitTEMP($200, vram + hlp, newvaus.width, 1);

	    inc(z, newvaus.width);
            end;

        pause;

	inc(j, mw);
        end;
	
    asm
	fxs FX_MEMS #$00
    end;
	
end;


(*
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
*)


{ Print the 5 digits of the score at coordinates px,py }
procedure write_score(py : byte; sc_ : cardinal);
const
   mul_6 : array [0..10] of byte = ( {$eval 11,":1*6"} );

var n1 : byte;
    f  : boolean;

    sc: cardinal register;					// register TMP


procedure put_digit(num: byte);        { Print the digital number num }
                                       { at coordinates px,py.        }
begin

 blitTEMP(VBXE_DIGIT + mul_6[num], vram + hlp, 6, 11 );		// register EDX, ECX, EAX

 inc(hlp, 7);

end;



begin
   f:=false; { As long as this remains false, the 0s are not printed. }
             { This is to ensure that the score starts at 0 and not   }
             { 000000, which looks bad.                               }

    asm
	fxs FX_MEMS #$80
    end;

   blitTEMP(128, 320);
   
   hlp := row[py] + 253;

   { first digital digit }


   sc:=sc_;

   n1:=0;
   while sc >= 100000 do begin
    dec(sc, 100000);
    inc(n1);
   end;
   n1:=mod10table[n1];
   if n1 > 0 then f:=true;    { If the first digit is >0 then }
   if f then put_digit(n1)    { it must be printed }
   else put_digit(10);        { otherwise print a zero }

   { second digital digit }
   n1:=0;
   while sc >= 10000 do begin
    dec(sc, 10000);
    inc(n1);
   end;
   n1:=mod10table[n1];        { Ditto for the remaining blocks }
   if n1 > 0 then f:=true;
   if f then put_digit(n1)
   else put_digit(10);

   { third digital digit }
   n1:=0;
   while word(sc) >= 1000 do begin
    dec(sc, 1000);
    inc(n1);
   end;
   n1:=mod10table[n1];
   if n1 > 0 then f:=true;
   if f then put_digit(n1)
   else put_digit(10);

   { fourth digital digit }
   n1:=0;
   while word(sc) >= 100 do begin
    dec(sc, 100);
    inc(n1);
   end;
   n1:=mod10table[n1];
   if n1 > 0 then f:=true;
   if f then put_digit(n1)
   else put_digit(10);

   { fifth digital digit }
   n1:=0;
   while byte(sc) >= 10 do begin
    dec(sc, 10);
    inc(n1);
   end;
//   n1:=mod10table[n1];
   put_digit(mod10table[n1]);

   { sixth and last digital digit (which of course is always 0 because }
   { the score travels in multiples of 10 points.                      }
   put_digit(0);

    asm
	fxs FX_MEMS #$00
    end;

end;


{ When the pause is invoked, the control switches to this procedure }
procedure pause_game;
var x,y,z : byte;

begin

    nosound;                    { Turn off any buzzer sounds.  }
    setcolor(0);                { Print the text in black,     }
    for x:=0 to 2 do            { moving it in all directions. }
        for y:=0 to 2 do
            outtextxy(66+x,129+y,'Game Paused');

    setcolor(1);                      { Then print the blank one }
    outtextxy(67,130,'Game Paused');

    repeat
    z:=inkey;                        { and wait until either }
    until (z=ord('p')) or (z=32);    { “p” or space (z=32) is pressed. }


    { Erase the writing by copying the background over it }
    for y:=129 to 140 do
        //memcpy(playscreen.map[66+row[y]], screen[66+row[y]], textwidth('Game Paused')+1);
        //blitROW(playscreen_ofs + 66+row[y], vram + 66+row[y], textwidth('Game Paused')+1);

    { textwidth('Game Paused') is a function that returns the length }
    { in pixels of the text 'Game Paused'.                           }
end;


{ Print the small vaus in the lower left corner indicating the number of }
{ lives remaining available (not counting the one in play).              }
procedure plot_lives(lives : byte);

const XLIVES = 11;
      YLIVES = 192;

var x,y,xl,yl,cn : byte;

    //xp,yp   : word;

    //shadow,
    cl,i : byte;

begin
    dec(lives); { The number of lives must be decreased by one   }
                { because the one in play should not be counted. }

    asm
	fxs FX_MEMS #$80
    end;

    hlp := row[YLIVES] + XLIVES;
	
    //blitTEMP(pattern_temp + hlp, playscreen_ofs + hlp, 8*minivaus_width, minivaus_height);    
    //blitTEMP(pattern_temp + hlp, vram + hlp, 8*minivaus_width, minivaus_height);  
    blitCLR(8*minivaus_width, minivaus_height);


    if lives > 0 then begin


    if lives > 7 then
     i:=7
    else
     i:=lives-1;

    blitTEMP(minivaus_width, 320);

    blt.blt_control := 1;
     

	for cn:=0 to i do begin                      { at most he draws 8 }

		//hlp := row[YLIVES] + XLIVES;

		blitTEMP(minivaus_ofs, playscreen_ofs + hlp, minivaus_width, minivaus_height);
		blitTEMP(minivaus_ofs, vram + hlp, minivaus_width, minivaus_height);

		inc(hlp, minivaus_width);
	end;

    blt.blt_control := 0;


    end;

(*
    blitTEMP(pattern.width, pattern.width);
    blitTEMP(pattern.ofs, $300, pattern.width, pattern.height);			// pat

    blitTEMP(minivaus_width, minivaus_width);
    blitTEMP(minivaus.ofs, $280, minivaus_width, minivaus_height);		// pom


    for cn:=0 to 7 do begin                      { at most he draws 8 }

        xl := XLIVES + byte(minivaus_width) * cn;

        hlp := row[YLIVES] + xl;

        blitTEMP(320, minivaus_width);
        blitTEMP(playscreen_ofs + hlp, $200, minivaus_width, minivaus_height);	// scr

	i:=0;

        for y:=0 to minivaus_height-1 do begin

//	    blitTEMP(minivaus.ofs + y*minivaus_width, $280, minivaus_width, 1);	// pom

            yl:=y+YLIVES;

            yp := byte(pattern.width) * mody[yl];

//	    blitTEMP(playscreen_ofs + hlp, $200, minivaus_width, 1);		// scr

            for x:=minivaus_width-1 downto 0 do
                begin

                xp:=modx[xl + x];

                { if the number of lives is greater than the counter }
                { then draw a vaus.                                  }
                if (lives>cn) and (pom[i+x] <> 0) then
                   begin
                   //cl:=minivaus.map[x+y*minivaus.width];

                   //screen[xl+row[yl]]:=minivaus.map[x+y*minivaus.width];

                   //playscreen.map[xl+row[yl]]:=minivaus.map[x+y*minivaus.width];
		   
		   scr[i+x] := pom[i+x];

                   end

                { otherwise it copies the background of the screen so that }
                { if vaus was present it is now deleted.                   }
                else
                  begin
                  //shadow:=playscreen.map[xl+row[yl]] and 128;
                  shadow:=scr[i+x] and $80;

                  //cl:=(pattern.map[xp+yp] and 127) or shadow;
		  cl:=(pat[xp+yp] and $7f) or shadow;

                  //screen[xl+row[yl]]:=cl;

                  //playscreen.map[xl+row[yl]]:=cl;

		  scr[i+x] := cl;
                  end;
                end;
		
	inc(i, minivaus_width);
				
	end;

	blitSCR(minivaus_width, 320, minivaus_width, minivaus_height);
	
     end;
*)

    asm
	fxs FX_MEMS #$00
    end;

end;


procedure place_fire;
begin

    hlp := fire.x + row[fire.y];

    blitZERO(shoots_ofs, shoots_width, shoots_height);

end;


procedure remove_fire;
begin

   hlp := fire.x + row[fire.y];
   
   blitBOX(shoots_width, shoots_height);

end;


procedure check_fire;
var x1,x2,y1,y2 : byte;
begin

    if (fire.avl) then
       begin
       if (trig0 = 0) and (fire.avl) and (not fire.shot) then
          begin
          fire.x:=vaus.x+byte(vaus.width-shoots_width) shr 1;
          fire.y:=vaus.y-shoots_height;
          fire.shot:=TRUE;
          fire.nw  :=FALSE;
          //ball_block_sound(700,5);
	  sfx.init(sfx_fire);
          end;

       if fire.shot then
          begin
          if fire.nw then remove_fire;
          fire.nw:=TRUE;

          dec(fire.y,4);
          if fire.y < 22 then fire.shot:=FALSE
          else
              begin
              place_fire;

              if (byte(fire.y-22) >= 0) and (byte(fire.y-22) < 120) then
                 begin
                 x1:=byte(fire.x-9 ) shr 4;
                 y1:=byte(fire.y-22) shr 3;

                 x2:=byte(fire.x+shoots_width-9) shr 4;
                 y2:=y1;

                 if (wall[byte(x1+y1*16)] <> 0) or (wall[byte(x2+y2*16)] <> 0) then
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
begin

    hlp := row[FLUXLEVEL] + 217;

    blitBOX(8, 20);
    
end;


procedure check_flux;
var y,fx  : byte;
begin

//   scrflux:=true;

   if scrflux then
      begin

      fx:=(scrfluxcnt shr 2) shl 3;


      asm
	fxs FX_MEMS #$80
      end;


      blitTEMP(24, 320);
      //blitTEMP(flux_ofs + fx, vram + 217+row[FLUXLEVEL], 8, 20);
      
      blitTEMP(flux2_ofs + fx, vram + 217 + row[FLUXLEVEL], 8, 21);


      asm
	fxs FX_MEMS #$00
      end;


      inc(scrfluxcnt);
      if scrfluxcnt>11 then scrfluxcnt:=0;
      end;
end;


procedure vaus_out;
var x,z : byte;
begin

//    nosound;

    sfx.init(sfx_vaus_teleport);

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
	
	blitBOX(40, vaus.height);

        end;

end;


procedure check_bonus_type(var b1,b2,b3 : BALLTYPE);
var x : smallint;
begin
    
     if vaus.letter <> EMP then    
      if (vaus.letter > 0) then
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
           fire.shot:=FALSE;
           fire.avl:=TRUE;
           end;

        2: begin                              { Letter E }
           if fire.shot then remove_fire;
           playvaus:=enlarged;
           modify_vaus;
           vaus.letter:=0;

	   sfx.init(sfx_vaus_enlarged);

           fire.avl:=FALSE;
           end;

        3: begin                              { Letter B }
           if fire.shot then remove_fire;
           playvaus:=normal;
           modify_vaus;
           vaus.letter:=0;
           scrflux:=TRUE;
           fire.avl:=FALSE;
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
           //ball_block_sound(2000,10);

	   sfx.init(sfx_letter_p);

           fire.avl:=FALSE;
           end;

        end;
end;


procedure deviate_ball(var ball : BALLTYPE);
var 
//  temp, dir : smallint;

  temp: smallint absolute $00;
  dir: smallint absolute $02;

begin

   dir := get_ball_direction(ball);

   repeat
    temp := rand(BALLDEV) + dir - (BALLDEV shr 1);    
   until (mod90(temp)>30) and (mod90(temp)<60);

   set_ball_direction(ball, temp);
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
  scores: cardinal absolute $30;

//  tmp  : smallint;
//  t1,t2: smallint;
 
  tmp: smallint absolute $34;
  t1: smallint absolute $36;
  t2: smallint absolute $38;

  x  : byte absolute $3a;

  ball0: BALLTYPE absolute $40;
  ball1: BALLTYPE absolute $60;

  ball2: BALLTYPE absolute $e0;


  ball_speed_result: word absolute $34;
  

  procedure ball_speed(var ball: BALLTYPE);
  var 
  //    i: cardinal;
  //    a, b: word;

    i: cardinal absolute $00;

    a: word absolute $04;
    b: word absolute $06;

  begin
  { returns the ball velocity formula, uses the Pythagorean theorem }
  { (v=sqrt(x^2+y^2)) }

    if ball.speedx < 0 then
      a := -ball.speedx
    else
      a := ball.speedx;

    if ball.speedy < 0 then
      b := -ball.speedy
    else
      b := ball.speedy;
   
    if a > MAXSPEED then a := MAXSPEED;
    if b > MAXSPEED then b := MAXSPEED;

    i:=sqrtable[a] + sqrtable[b];

    ball_speed_result := trunc( FastSqrt(i) );
  
    if ball_speed_result > MAXSPEED then ball_speed_result := MAXSPEED;

  end;



  procedure check_ball(var ball: BALLTYPE);
  begin
            if (byte(ball.y) >= 22) and (byte(ball.y) < 142) then ball_hit_block(ball);

            set_ball(ball);
	    ball_speed(ball);
            ball.speed := ball_speed_result;
  end;

{
  procedure check_ball0;
  begin
            if (byte(ball0.y)>=22) and (byte(ball0.y)<142) then ball_hit_block(ball0);

            set_ball(ball0);
            ball_speed(ball0.speedx, ball0.speedy);
	    ball0.speed := ball_speed_result;
  end;

  procedure check_ball1;
  begin
            if (byte(ball1.y)>=22) and (byte(ball1.y)<142) then ball_hit_block(ball1);

            set_ball(ball1);
            ball_speed(ball1.speedx, ball1.speedy);
	    ball1.speed := ball_speed_result;
  end;

  procedure check_ball2;
  begin
            if (byte(ball2.y)>=22) and (byte(ball2.y)<142) then ball_hit_block(ball2);

            set_ball(ball2);
            ball_speed(ball2.speedx, ball2.speedy);
	    ball2.speed := ball_speed_result;
  end;
}


  procedure test_ball(var ball: BALLTYPE);
  begin
            inc(ball.finespeed);

            if ball.finespeed > LEVEL[lv] then
               begin
               ball.finespeed:=0;

               { If the speed is less than the maximum speed }
               if ball.speed < MAXSPEED then
                  begin
                  inc(ball.speed, 17);             { you increase it }
                  set_ball_speed(ball,ball.speed); { and you update it }
                  end;
               end;

            inc(ball.sbd); { this is the regular deviation counter }

            { if it exceeds a certain threshold (SBDIR) a deviation is set }
            { random by an angle between -BALLDEV/2 and +BALLDEV/2 }
            if (ball.sbd >= SBDIR) and (ball.speedy < 0) then
               deviate_ball(ball);
  end;



  begin

  scrfluxcnt:=0;
  scrflux:=FALSE;

  balls_in_play:=1;

  fire.avl:=FALSE;
  playvaus:=normal;

  lett.last:=EMP;
  lett.active:=FALSE;

  { Sets the right background depending on the wall }
  fill_picture_with_pattern;

  { Draw the game board with the background you just set }
  hlp:=0;
  blitBOX(320, 200);

  { Print the number of lives of the current player        }
  { cur_player=1 or 2 depending on which player is to play }
  plot_lives(score.lives[cur_player]);

  { adjust the colors, in theory they should already be okay. }
//  setpalette(playscreen);

  { Print the scores of the two players and the high score. }
  write_score(POS_DIGIT[1],score.player[1]);
  write_score(POS_DIGIT[2],score.player[2]);
  write_score(POS_DIGIT[3],score.hiscore);

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

  { Keeps track of the number of vertical blanks that pass from }
  { when the ball appears to when it is launched. If the value  }
  { exceeds 2000 units, the ball is launched automatically      }
  ball0.stm:=0;

  { At the start, the variable lett.incoming takes on a random value between }
  { 0 and LETTER_DROP (constant defined at the beginning of the unit).       }
  lett.incoming:=rand(LETTER_DROP);

  { Shows the animation of the VAUS materializing out of thin air }
  create_vaus;

  { and prints the words ROUND xx, READY. }
  write_round_level;

  set_vaus;                       { Adjusts the initial VAUS parameters. }
  move_vaus(vaus.x,VAUS_LINE);    { brings him to the center of the playing area }
  start_level;                    { If the sound is on, it plays the tune }
  remove_round_level;             { Removes the words ROUND xx, READY}
  set_ball(ball0);
  
  { This is the main cycle, it can only get out of it if : }
  { - the ball is lost, }
  { - the picture is ended (i.e., no more bricks are left to be destroyed. }
  { - the game is somehow aborted.                 }

  set_ball_direction(ball0,rand(15)+60);   { random starting angle }
                                           { 60 and 75 degrees }
  set_ball_speed(ball0, BALLSPEED);

  { initial speed = constant BALLSPEED }
  ball0.finespeed:=0; { submultiples of the speed are 0  }

  ball1.inplay:=FALSE;
  ball2.inplay:=FALSE;
  
//  inc(vaus.x, 45);
  
  x:=vaus.x;


//  set_ball_direction(ball0,20);
//  set_ball_speed(ball0, 700);

  
  
  while(ball0.inplay) and (remain_blk > 0) and (not score.abortplay) do
     begin
     //Wait_VBL; { Waits for the vertical blank }
     
     
     asm
      ;mva #$00 $d01a
      @: lda vcount
      cmp #110
      bne @-
      ;mva #$0f $d01a       
     end;
     

     mousecoords(x);  { reads mouse coordinates }

     move_vaus(x,VAUS_LINE);


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
        if (trig0 = 0) then ball0.launch:=FALSE;
	
	if ball0.launch=FALSE then sfx.init(sfx_ball_bounce);
        end

     else begin
        { Otherwise if the ball is not attached one simply needs to move it. }
        { Clearly if there are 3 balls you need to move all 3.               }

        //for cn:=0 to 2 do
            if ball0.inplay then move_ball(ball0);
            if ball1.inplay then move_ball(ball1);
            if ball2.inplay then move_ball(ball2);

     end;

     { If the coordinates of the ball cn are between 22 and 142 (respectively }
     { maximum and minimum coordinates at which a brick can be bumped) then   }
     { you need to check whether the ball actually bumped a brick or not.     }

     if ball0.inplay then check_ball(ball0);
     if ball1.inplay then check_ball(ball1);
     if ball2.inplay then check_ball(ball2);


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

     if (byte(vaus.x+vaus.width) = byte(SCRMAX-1)) and (scrflux = true) then vaus_out;

     if vaus.letter=4 then   { In case a D has been collected the balls }
        begin                { become 3.                                }
        balls_in_play:=3;
	
	ball0.brwhit:=0;

        ball1:=ball0;    { ball 2 and 3 are placed equal to 1 }
        ball2:=ball0;

	tmp:=get_ball_direction(ball0) ;
	
	t1:=0;
	while tmp >= 90 do begin
	 dec(tmp, 90);
	 inc(t1);
	end;
//        t1:=get_ball_direction(ball0) div 90;
	
	
        { you the quadrant in which the velocity vector is located }
        t2:=ball0.speed;  { as well as the modulus of the vector itself }

        { you impose a 30-degree tilt to the quadrant at }
        { first ball, 45 at the second, and 60 at the third.              }

        { At this point the three balls are forced to split up. }
	
	t1:=mul90_16[byte(t1)];

        set_ball_direction(ball0,(t1+30));
        set_ball_direction(ball1,(t1+45));
        set_ball_direction(ball2,(t1+60));


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
       
       write_score(POS_DIGIT[cur_player], scores);

       old_scores := scores;

     end;
     

     { If the player's score is greater than the hi-score }
     if score.player[cur_player] > score.hiscore then
        begin
        { places the hi-score equal to the player's score }
        score.hiscore:=score.player[cur_player];
        { And prints the hi-score on the screen }
        write_score(POS_DIGIT[3],score.hiscore);
        end;

     { This cycle increases the speed of all balls in play the value        }
     { of LEVEL[lv] obviously depends on the lv, that is, the level         }
     { selected before starting the game.                                   }     

     if (ball0.inplay) and (ball0.launch = false) then test_ball(ball0);
     if ball1.inplay then test_ball(ball1);
     if ball2.inplay then test_ball(ball2);
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

{
//     for cn:=0 to 2 do
         if not ball0.inplay then
            begin
            ball0:=ball1;
            ball1:=ball2;
            ball2.inplay:=FALSE;
            end;
}

         if not ball0.inplay then	    
	    if ball1.inplay then begin
             ball0:=ball1;
             ball1.inplay:=FALSE;
	    end else
	     if ball2.inplay then begin
              ball0:=ball2;
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
//     if snd_delay > 0 then dec(snd_delay)
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
            if (wall[cn+14*16] > 0) and (wall[cn+14*16]<>10) then
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

  { BounceBall exits with false if the ball was lost, with true if }
  { the picture was finished. }

  Result:=FALSE;
  if remain_blk=0 then Result:=TRUE;
  end;

{ ------------------------------------------------------------- }

function choose_start_wall : byte;
{
const px = 70;
      py = 100;
      dx = 34;
      dy = 35;
      ddx= 19;
      ddy= 14;
}
var 
//    x,y : smallint;

    st    : byte;

//    oldx,
//    oldy,
//    newx,
//    newy  : smallint;
    //sc    : string[20];

    begin

    st:=1;  { Start choosing from wall no. 1. }
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
    choose_start_wall:=st;  { and returns the selected number }
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
var 
    //x,y,z : word;
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



    asm
      fxs FX_MEMS #$80
    end;

    blitTEMP(320, 320);
    blitTEMP(presents_ofs, vram, 320, 200);

    asm
      fxs FX_MEMS #$00
    end;
  
  

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

    fill_picture_with_pattern;            { si imposta lo sfondo }
    
    hlp:=0;
    blitBOX(320, 200);                    { e si disegna tutto quanto sullo }
                                          { schermo }

//    setpalette(playscreen);               { si impostano i colori. }
    

    { you print the three scores, player 1, 2 and hi-score }
    write_score(POS_DIGIT[1],score.player[1]);
    write_score(POS_DIGIT[2],score.player[2]);
    write_score(POS_DIGIT[3],score.hiscore);

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
                wall_p[cur_player] := //all_walls[4];
                      all_walls[byte(score.wall_n[cur_player]-1)];

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
                      all_walls[byte(score.wall_n[cur_player]-1)];
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
