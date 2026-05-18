uses ray,_graph,lokeyb,fix;
var virscr:pointer;

begin
Loadpal('andrew.pal');
Loadimg(1,'andrew.img');
getmem(virscr,320*200);
destenation:=virscr;
x_pos:= 10 shl 8+10;y_pos:= 6 shl 8+10;
pos_angle:=180*4;
initkeyboard;
repeat
if checkkey(kbRight) then inc(pos_angle,10);
if checkkey(kbLeft) then  dec(pos_angle,10);
if checkkey(kbUp) then  begin
                        x_pos:=(x_pos shl 3+fix_cos(pos_angle)) shr 3;
                        y_pos:=(y_pos shl 3+fix_sin(pos_angle)) shr 3;
                        end;
if checkkey(kbDown) then  begin
                        x_pos:=(x_pos shl 3-fix_cos(pos_angle)) shr 3;
                        y_pos:=(y_pos shl 3-fix_sin(pos_angle)) shr 3;
                        end;

cls(virscr,320*200);
cutrays;
flip(virscr,vidptr,320*200);
until checkkey(kbEsc);
donekeyboard;
asm mov ax,3h;int 10h end;
freemem(virscr,320*200);
end.