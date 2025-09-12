uses crt, math;

var
  f: double;
  
  a, angle: word;
  
  d: smallint;
  
  hsin, lsin: array [0..90-1] of byte;
  
  fn: file;


begin 

 for angle:=0 to 90-1 do begin

  a:=angle mod 360;
 
  f:=a*pi/180.0;     { w viene espresso in gradi }

  d := floor(256*sin(f));
  
  if d >= 0 then 
   inc(d, $40)
  else  
   dec(d, $40);
   
  lsin[angle] := lo(d);
  hsin[angle] := hi(d); 
  
  write(d,',');

 end;
 
 assign(fn, 'sintable.dat'); rewrite(fn, 1); 
 blockwrite(fn, lsin, sizeof(lsin));
 blockwrite(fn, hsin, sizeof(hsin));
 close(fn);
 
 repeat until keypressed;

end.
