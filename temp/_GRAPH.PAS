unit _graph;
interface
{$G+,s-}
Type rgb_type = record r,g,b:byte; end;
Type pal_type = array[0..255]of rgb_type;

CONST vidptr:pointer=ptr($a000,0);

var DESTENATION:pointer;
    sprite:array[0..255]of pointer;


procedure setrgb(c,r,g,b:byte);
procedure setpal(pal:pal_type);

procedure flip(video,double:pointer;col:word);
Procedure cls(p:pointer;col:word);inline( $59/$5f/$07/
                                          $d1/$e9/
                                          $33/$c0/
                                          $f3/$ab);
procedure loadimg(b:byte;s:string);
procedure loadpal(s:string);
procedure linetex(x,y1,y2:integer;col:word;POINT:POINTER);
procedure line_tex(x,y1,y2,col:word;p:pointer);

procedure _putpix(x,y,color:word);inline( $c4/$3e/DESTENATION/$58/
                                          $5b/$59/$8b/$fb/$c1/$e7/$08/
                                          $c1/$e3/$06/$03/$fb/$03/$f9/$aa
                                          );

implementation

procedure setrgb(c,r,g,b:byte);assembler;
asm
mov dx,03c8h;mov al,c;out dx,al;mov dx,3C9h;cli;mov al,R;out dx,al
mov al,G;out dx,al;mov al,B;out dx,al;sti
end;
procedure setpal(pal:pal_type);
var i:byte;
begin
for i:=0 to 255 do with pal[i] do
                   begin
                   setrgb(i,r,g,b);
                   end;
end;

procedure flip(video,double:pointer;col:word);assembler;
asm
push ds;lds si,video;les di,double;mov cx,col;shr cx,1;rep movsw;pop ds;
end;

{procedure copytex(b:byte;s:string);
var pal:pal_type;
x,y:byte;
begin getmem(sprite[b],64*64);pcx_load(s,vidptr,pal);
for y:=0 to 63 do for x:=0 to 63 do mem[seg(sprite[b]^):ofs(sprite[b]^)+64*y+x]:=mem[$a000:320*y+x];
setpal(pal);end;}

procedure loadimg(b:byte;s:string);
var f:file;
begin
{$i-}
assign(f,s);reset(f,1);getmem(sprite[b],64*64);
blockread(f,sprite[b]^,64*64);
close(f);
{$i+}
end;

procedure loadpal(s:string);
var f:file;
    pal:pal_Type;
begin
{$i-}
assign(f,s);reset(f,1);
blockread(f,pal,256*3);
close(f);
setpal(pal);
{$i+}
end;

procedure linetex(x,y1,y2:Integer;col:word;POINT:POINTER);
var yd,y,coof,yo:word;
s,off:word;color:byte;
begin
s:=seg(POINT^);off:=ofs(POINT^)+col;
yd:=(y2-y1);
coof:=(63*256) div yd;
yo:=0;
for y:=0 to yd do
begin
color:=mem[s:off+(yo shr 8)shl 6];
inc(yo,coof);
_putPix(x,y1+y,color);
end;
end;

procedure line_tex(x,y1,y2,col:word;p:pointer);assembler;
asm
push ds
les di,destenation;mov ax,320;mul y1;add ax,x;mov di,ax
{es:di+(320*y1)+x = scr}
lds si,p;add si,col
{ds:si+col=texture}
mov cx,y2;sub cx,y1;
{DELTA y = CX}
mov ax,256*63;div cx;mov dx,ax
shl cx,1
{COOF = DX}
xor bx,bx
{bx = Yo to TEXTUTE}
@cxLoop:
        add bx,dx {yo:=yo+coof}
        mov ax,bx
        shr ax,8
        shl ax,6
        push si
        add si,ax
        mov al,[ds:si]
        mov [es:di],al

        pop si
        dec cx
        add di,320
        Loop @cxLoop
pop ds
end;

begin
asm mov ax,13h;int 10h end;
DEstenation:=VidPtr;
{copyimg(2,'Fire.img');
copytex(1,'e:\andrew\lu-96\work\game\chaos\alg\4.tex');}
cls(VIDPTR,320*200);
end.
