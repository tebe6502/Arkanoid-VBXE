// speed range 0..1440
// a:=a shr 3; (1440 shr 3 = 180)
// b:=b shr 3;
// i := a*a + b*b;
// x := sqrt(i) shl 3;

uses crt;

var
  i: word;

  tsqr: array [0..255] of word;

  fn: file;

  v: byte;


begin

 for i:=0 to 255 do begin

  tsqr[i] := i*i;

 end;

 assign(fn, 'sqrtable.dat'); rewrite(fn, 1);

 for i:=0 to 255 do begin
  v:=lo(tsqr[i]);
  blockwrite(fn, v, 1);
 end;

 for i:=0 to 255 do begin
  v:=hi(tsqr[i]);
  blockwrite(fn, v, 1);
 end;

 close(fn);

 writeln('Done.');

 repeat until keypressed;

end.
