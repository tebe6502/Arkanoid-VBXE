uses crt, math;

var
  f: double;
  
  a, angle: word;
  
  tsin: array [0..360+90-1] of smallint;
  
  fn: file;


begin 

 for angle:=0 to 360+90-1 do begin

  a:=angle mod 360;
 
  f:=a*pi/180.0;     { w viene espresso in gradi }

  tsin[angle] := floor(256*sin(f));
  
  if tsin[angle] >= 0 then 
   inc(tsin[angle], $40)
  else  
   dec(tsin[angle], $40);
  
  
  write(tsin[angle],',');

 end;
 
 assign(fn, 'sintable.dat'); rewrite(fn, 1);
 blockwrite(fn, tsin, sizeof(tsin));
 close(fn);
 
 repeat until keypressed;

end.
