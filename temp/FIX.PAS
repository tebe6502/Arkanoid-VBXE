unit fix;
interface
type TFix=longint;
     Shortfix=Integer;
const Fix_Bit=8;
      Fix_Num=256;
      Fix_Bit_Mask=$FFffFF00;
      Fix_Not_Bit_Mask=$000000ff;

Function Real_to_fix(r:real):TFix;
Function Long_to_fix(l:longint):TFix;
Function Fix_to_Real(f:TFix):real;
Function Fix_to_Long(f:TFix):longint;

Function Fix_Mul(f1,f2:TFix):TFix;
Function Fix_Div(f1,f2:TFix):TFix;{inline( $66/$5b/
                                          $66/$58/
                                          $66/$c1/$e0/08/
                                          $66/$f7/$f3);}

procedure Fix_Inc(var f:TFix);
procedure Fix_Dec(var f:TFix);

function Fix_Cos(angle:word):TFix;
function Fix_Sin(angle:word):TFix;
function Fix_Tg(angle:word):TFix;

function FIx_Round(f:Tfix):ShortFix;
function FIx_Drob(f:Tfix):ShortFix;
Function _Fix(f:Tfix):TFix;

implementation
{$i fixtreg.inc}
{rem $l _fix.obj}
procedure int0;interrupt;
begin end;

Function Real_to_fix(r:real):TFix;
begin
Real_to_Fix:=round(r * Fix_Num);
end;

Function Long_to_fix(l:longint):TFix;
begin
Long_to_FIx:=l shl Fix_Bit;
end;

Function Fix_to_Real(f:TFix):real;
begin
Fix_to_Real:=f / Fix_Num;
end;

Function Fix_to_Long(f:TFix):longint;
begin
Fix_to_Long:=f shr Fix_Bit;
end;

Function Fix_Mul(f1,f2:TFix):TFix;
begin
Fix_Mul:=(f1 * f2) shr Fix_Bit;
end;

Function Fix_Div(f1,f2:TFix):TFix;{external;}
begin
Fix_Div:=(f1 shl Fix_Bit) div f2;
end;

procedure Fix_Inc(var f:TFix);
begin
f:=f+Fix_Num;
end;

procedure Fix_Dec(var f:TFix);
begin
f:=f-Fix_Num;
end;

function Fix_Cos(angle:word):TFix;
begin
Fix_Cos:=Fix_Cos_Table[angle mod (360*4)];
end;

function Fix_Sin(angle:word):TFix;
begin
Fix_Sin:=Fix_Cos(270*4+angle);
end;

function Fix_Tg(angle:word):TFix;
begin
Fix_tg:=Fix_Div(Fix_Sin(angle),Fix_Cos(angle));
end;

function FIx_Round(f:Tfix):ShortFix;
begin
Fix_Round:=f and Fix_Bit_Mask;
end;

function FIx_Drob(f:Tfix):ShortFix;
begin
Fix_Drob:=f and Fix_Not_Bit_Mask;
end;

Function _Fix(f:Tfix):TFix;
begin
_Fix:=(Fix_Num shl Fix_bit) div f;
end;


begin
POINTER(meml[0:0]):=@int0;
end.