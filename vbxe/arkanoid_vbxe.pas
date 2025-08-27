program arkanoid: $1000;

{
; optimize OK (service.pas), line = 1616

	lda #$00
	sta :STACKORIGIN+STACKWIDTH+10
	lda YB
	asl @
	rol :STACKORIGIN+STACKWIDTH+10
	asl @
	rol :STACKORIGIN+STACKWIDTH+10
	asl @
	asl @
	add XB
	sta I
}



uses crt, atari, vbxe, joystick;

{$r arkanoid.rc}

{$define romoff}

const
        VBXE_OVRADR = $5000;
        VBXE_DATA = VBXE_OVRADR + 320*200;

	levels_wall = $d800+$400;

var

  mous: record
          x,y: smallint;
	  left, right: smallint;
	  top, bottom: smallint;
          fire: Boolean;
        end;


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



(* Follow the common procedures based on interrupt $33 *)

procedure mousereset;
//var regs : REGISTERS;
   begin
{
   regs.ax:=0;
   intr($33,regs);
}
   end;


function mouseclick: byte;
begin
     result:=1;//ord(mous.fire);

     mous.fire:=true;//not mous.fire;
end;


procedure mousecoords(var x,y : smallint);
var a: byte;
begin

 a:=porta and $0f;
 
 case a of
  joy_left: if mous.x > mous.left then dec(mous.x, 2);
  joy_right: if mous.x < mous.right then inc(mous.x);
 end;
 
 x:=mous.x;

 end;

procedure mouse_x_limit(mn,mx : smallint);
begin
 
  mous.left:=mn;
  mous.right:=mx;
 
end;

procedure mouse_y_limit(mn,mx : smallint);
begin

 mous.top := mn;
 mous.bottom := mx;

end;

procedure mousemove(x,y : smallint);
begin

 mous.x:=x;
 mous.y:=y;

end;



{$i ..\service.pas}



procedure init_game;
begin

   initSVGA;       { Activates 320x200x256 col. graphics mode. }
   initRowArray;   { Initializes a useful array to avoid multiplications }
                   { by 320. }


   {$i btm.inc}


   mous.fire:=true;

   totalwall:=32;

   score.hiscore:=50000;
   { the record score is initially set to 50000 by default }

   sound_on:=TRUE;      { by default at the beginning the sound is ON }
   lv:=DEFLEVEL;        { and the level is set to DEFLEVEL            }

   repeat

      mousereset;

      { mainscreen returns 1,2 (play number ) or -1 = quit }
      score.pl_numb:=mainscreen;

      if score.pl_numb>0 then start_game(1);//score.pl_numb);

   until score.pl_numb<1; { cycle until it's worth -1 = quit }

end;



begin

 init_game;

end.