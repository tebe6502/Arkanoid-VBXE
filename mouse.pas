{ Questa unit e' stata importata esternamente da altre fonti }
{ e non fa parte del sorgente pascal scritto dall'autore.    }

unit mouse; { and other interesting stuff }

interface

procedure hidemouse;
procedure memcpy(source,destination : cardinal; size : word);
procedure memzerocpy(source,destination : cardinal; size : word); overload;
procedure memzerocpy(source: pointer; destination : cardinal; size : word); overload;
procedure mousecoords(var x,y : smallint);
procedure mousemove(x,y : smallint);
procedure mousereset;
procedure mouse_x_limit(mn,mx : smallint);
procedure mouse_y_limit(mn,mx : smallint);
procedure showmouse;

procedure mydelay(time : integer);

function mouseclick:smallint;
function longtime : integer;

implementation


(* Copies "size" bytes from "source" to "destination".       *)
(* The operator addr may be used to pass the correct adress. *)

(* Example: memcpy(addr(source),addr(destination),200)       *)
(* where:   source,destination : array[...] of bytes;        *)
(*          200 is the number of bytes to be copied          *)

procedure memcpy(source,destination : cardinal; size : word);
//label LP;
   begin
{  
  asm
     push ds
     push si

     mov cx,size
     lds si,source
     les di,destination

     LP:
     lodsb
     stosb
     loop LP

     pop si
     pop ds
     end;
} 
 end;

(* Does exactly the same as memcpy except that it does not copies *)
(* those bytes whose value is 0. It's useful to put on the screen *)
(* drawings using color 0 as trasparent.                          *)

procedure memzerocpy(source: pointer; destination : cardinal; size : word); overload;
//label LP,L1,L2,L3;
   begin
{ 
 asm
     push ds
     push si

     mov cx,size
     mov dx,0
     lds si,source
     les di,destination

     LP:
     lodsb
     cmp al,dl
     je  L2

     L1:
     stosb
     jmp L3

     L2:
     inc di

     L3:
     loop LP

     pop si
     pop ds
     end;
}  
  end;

procedure memzerocpy(source,destination : cardinal; size : word); overload;
//label LP,L1,L2,L3;
   begin
{ 
 asm
     push ds
     push si

     mov cx,size
     mov dx,0
     lds si,source
     les di,destination

     LP:
     lodsb
     cmp al,dl
     je  L2

     L1:
     stosb
     jmp L3

     L2:
     inc di

     L3:
     loop LP

     pop si
     pop ds
     end;
}  
  end;


(* Return a value between 0 and 8640000 depending on current time *)
function longtime : integer;
{
var h,m,s,c : word;
    h1,m1,
    s1,c1   : integer;
}
    begin
{ 
 gettime(h,m,s,c);

    h1:=h;
    m1:=m;
    s1:=s;
    c1:=c;

    longtime:=c1+s1*100+m1*6000+h1*360000;
} 
 end;

(* It's the equivalent of DELAY except that it's based on hardware time     *)
(* and not on clock ticks. If some one alters the CPU speed using the TURBO *)
(* button MYDELAY is not affected. This is not the same for DELAY.          *)

procedure mydelay(time : integer);
{
var h,m,s,c : word;
    a,b,
    h1,m1,
    s1,c1   : integer;
}
    begin
{ 
 gettime(h,m,s,c);

    h1:=h;
    m1:=m;
    s1:=s;
    c1:=c;

    a:=c1+s1*100+m1*6000+h1*360000;

    repeat
        gettime(h,m,s,c);
        h1:=h;
        m1:=m;
        s1:=s;
        c1:=c;
        b:=((c1+s1*100+m1*6000+h1*360000)+8640000-a) mod 8640000;
    until (b>time);
}
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

procedure showmouse;
//var regs : REGISTERS;
   begin
{
   regs.ax:=1;
   intr($33,regs);
}
   end;

procedure hidemouse;
//var regs : REGISTERS;
   begin
{ 
   regs.ax:=2;
   intr($33,regs);
}  
  end;

function mouseclick:smallint;
//var regs : REGISTERS;
   begin
{   
   regs.ax:=3;
   regs.bx:=0;
   intr($33,regs);
   mouseclick:=regs.bx;
}   
   end;

procedure mousecoords(var x,y : smallint);
//var regs : REGISTERS;
   begin
{
	regs.ax:=3;
	regs.bx:=0;
   intr($33,regs);
   x:=regs.cx shr 1;
   y:=regs.dx;
} 
 end;

procedure mouse_x_limit(mn,mx : smallint);
//var regs : REGISTERS;
   begin
{
	regs.ax:=7;
	regs.cx:=mn;
	regs.dx:=mx;
   intr($33,regs);
} 
 end;

procedure mouse_y_limit(mn,mx : smallint);
//var regs : REGISTERS;
   begin
{
	regs.ax:=8;
	regs.cx:=mn;
	regs.dx:=mx;
   intr($33,regs);
} 
 end;

procedure mousemove(x,y : smallint);
//var regs : REGISTERS;
   begin
{
	regs.ax:=4;
	regs.cx:=x;
	regs.dx:=y;
   intr($33,regs);
}
   end;

end.
