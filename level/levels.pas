uses crt, sysutils;

type

   WALLTYPE = array [0..16*16-1] of byte;//array[0..12,-1..15] of byte; { for the wall (13x15 bricks) }

   WHOLEWALLS = array[0..32] of WALLTYPE;  { for all 33 walls }

var

  totalwall: byte;
    
  all_walls  : WHOLEWALLS; 

  f: file;


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
          if (y>14) then begin writeln('Too many blocks ('+inttostr(z)+')'); end;

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

          if (z>32) then begin writeln('Too many walls'); halt; end;
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
     

begin

 load_all_walls;
 
 assign(f, 'walls.dat'); rewrite(f, 1);
 blockwrite(f, all_walls, sizeof(all_walls));
 close(f);
 
 
 writeln(totalwall);
 
 repeat until keypressed;

end.