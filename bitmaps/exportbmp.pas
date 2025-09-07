
procedure ExportBMP(fnam: string; width, height: word);
(*----------------------------------------------------------------------------*)
(* E X P O R T  A S -> indexed B M P                                          *)
(*----------------------------------------------------------------------------*)
var i,j, FileHandle: integer;
    v: byte;
    temp: array [0..3] of byte;

  BMPHeader : packed record
                    bftype:word;
                    bfsize:longint;
                    bfreserv1:word;
                    bfreserv2:word;
                    bfoffbits:longint;

                    bisize:longint;
                    biwidth:longint;
                    biheight:longint;
                    biplanes:word;
                    bibitcount:word;
                    bicompress:longint;
                    bisizeimage:longint;
                    biXPelsPerMeter:longint;
                    biYPelsPerMeter:longint;
                    biClrUsed:longint;
                    biClrImportant:longint;
                 end;
    
begin

 with bmpheader do begin
  bftype:=256*ord('M')+ord('B');
  bisize:=40;
  biwidth:=width;		    { szerokosc w pixlach }
  biheight:=height;                 { wysokosc }
  bfoffbits:=14+bisize+1024;
  bisizeimage:=biwidth*biheight;
  bfsize:=bfoffbits+bisizeimage;

  biClrUsed:=$100;         { liczba kolorow = max 256 }
  biClrImportant:=0;
  biXPelsPerMeter:=0;
  biYPelsPerMeter:=0;
  bfreserv1:=0;
  bfreserv2:=0;
  bicompress:=0;
  biplanes:=1;
  bibitcount:=8;           { liczba bitow na pixel }
 end;

 fnam:=ChangeFileExt(fnam,'.bmp');

 FileHandle := FileCreate(fnam);
 try


 FileWrite(FileHandle, bmpheader, sizeof(bmpheader));


 for i:=0 to 255 do begin
  temp[0]:=def_pal[i*3+2];
  temp[1]:=def_pal[i*3+1];
  temp[2]:=def_pal[i*3];
  temp[3]:=0;

  FileWrite(FileHandle, temp,4);
 end;

 for j:=height-1 downto 0 do begin

  FileWrite(FileHandle, dum[j*width], width);

 end;


 finally
  FileClose(FileHandle);
 end;

end;
