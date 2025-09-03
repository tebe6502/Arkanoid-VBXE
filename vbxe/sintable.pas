uses crt, math;

var
  f: double;
  
  a, angle: word;
  
  tsin: array [0..360+90-1] of byte;
  
  fn: file;


begin 

 for angle:=0 to 360+90-1 do begin

  a:=angle mod 360;
 
  f:=a*pi/180.0;     { w viene espresso in gradi }

  tsin[angle] := floor(256*sin(f));

 end;
 
 assign(fn, 'sintable.dat'); rewrite(fn, 1);
 blockwrite(fn, tsin, 450);
 close(fn);
 
end.
