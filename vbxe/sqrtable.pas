uses crt;

var
  i: word;
  
  tsqr: array [0..1023] of cardinal;
  
  fn: file;


begin 

 for i:=0 to 1023 do begin

  tsqr[i] := i*i;
  
 end;
 
 assign(fn, 'sqrtable.dat'); rewrite(fn, 1);
 blockwrite(fn, tsqr, sizeof(tsqr));
 close(fn);
 
 repeat until keypressed;

end.
