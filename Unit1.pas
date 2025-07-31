unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, GR32, GR32_Image, GR32_Layers;

type
  TForm1 = class(TForm)
    Image321: TImage32;
    Button1: TButton;

    procedure Image321MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure Image321MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    procedure show_play;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image321MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

  private
    { Private declarations }

  public
    { Public declarations }

    procedure idle(sender: TObject; var done: boolean);

  end;

var
  Form1: TForm1;

  mous: record
          x,y: smallint;
          fire: Boolean;
        end;

  ShiftCtrl: Boolean;
  KeyMove: word;


implementation

{$R *.dfm}

//uses snd;//,FM;


procedure memcpy(const source; var destination; size : word);
var i: word;
    src, dst: PByte;
begin

 src:=@source;
 dst:=@destination;

 for i := 0 to size-1 do
    dst[i] := src[i];

end;


(* Does exactly the same as memcpy except that it does not copies *)
(* those bytes whose value is 0. It's useful to put on the screen *)
(* drawings using color 0 as trasparent.                          *)
procedure memzerocpy(const source; var destination; size : word);
var i: word;
    src, dst: PByte;
begin

 src:=@source;
 dst:=@destination;

 for i := 0 to size-1 do
  if src[i] <> 0 then dst[i] := src[i];

end;


procedure start_level;
begin


end;

procedure death_sound(a: word);
begin

end;


procedure ball_block_sound(a,b: word);
begin

end;



(* Follow the common procedures based on interrupt $33 *)

procedure mousereset;
//var regs : REGISTERS;
   begin
{
   regs.ax:=0;
   intr($33,regs);
}
   end;

procedure showmouse;
//var regs : REGISTERS;
   begin
{
   regs.ax:=1;
   intr($33,regs);
}
   end;

procedure hidemouse;
//var regs : REGISTERS;
   begin
{
   regs.ax:=2;
   intr($33,regs);
}
  end;

function mouseclick:smallint;
//var regs : REGISTERS;
   begin
{
   regs.ax:=3;
   regs.bx:=0;
   intr($33,regs);
   mouseclick:=regs.bx;
}
     result:=ord(mous.fire);

     mous.fire:=not mous.fire;
   end;

procedure mousecoords(var x,y : smallint);
//var regs : REGISTERS;
   begin
{
	regs.ax:=3;
	regs.bx:=0;
   intr($33,regs);
   x:=regs.cx shr 1;
   y:=regs.dx;
}
  x:=mous.x;
  y:=190;//mous.y;
 end;

procedure mouse_x_limit(mn,mx : smallint);
//var regs : REGISTERS;
   begin
{
	regs.ax:=7;
	regs.cx:=mn;
	regs.dx:=mx;
   intr($33,regs);
}
 end;

procedure mouse_y_limit(mn,mx : smallint);
//var regs : REGISTERS;
   begin
{
	regs.ax:=8;
	regs.cx:=mn;
	regs.dx:=mx;
   intr($33,regs);
}
 end;

procedure mousemove(x,y : smallint);
//var regs : REGISTERS;
   begin
{
	regs.ax:=4;
	regs.cx:=x;
	regs.dx:=y;
   intr($33,regs);
}
   end;


{$i service.pas}


procedure TForm1.idle(sender: TObject; var done: boolean);
begin

 done:=false;
end;


procedure TForm1.Image321MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin

 mous.fire:=false;

end;


procedure TForm1.FormCreate(Sender: TObject);
begin

 Application.OnIdle := idle;

end;


procedure TForm1.Image321MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
 mous.fire:=true;
end;

procedure TForm1.Image321MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer; Layer: TCustomLayer);
begin

 mous.x:=x;
 mous.y:=y;

end;

procedure TForm1.show_play;
var i, j: word;
    bmp: TBitmap32;
    line: PColor32Array;
    v: byte;
begin

 sleep(5);

 bmp:=TBitmap32.Create;
 bmp.SetSize(320,200);

 bmp.BeginUpdate;

 bmp.Clear;

 for j := 0 to 199 do begin
  line:=bmp.ScanLine[j];

  for i:=0 to 319 do begin

        v:=screen[i+j*320];

        TColor32Entry(line^).R := def_pal[v*3] shl 2;
        TColor32Entry(line^).G := def_pal[v*3+1] shl 2;
        TColor32Entry(line^).B := def_pal[v*3+2] shl 2;


        inc(line);

  end;

 end;

 bmp.EndUpdate;

 form1.Image321.Bitmap:=bmp;

 bmp.Free;

 form1.Update;

end;


procedure Arkanoid;
begin

   { This portion contains only the in-memory loading of data files }

   //DetectFM;
   success:=TRUE;  { Success is set to FALSE if an error occurs from disk }

   load_all_walls; { Upload the 32 game walls }

   initSVGA;       { Activates 320x200x256 col. graphics mode. }
   initRowArray;   { Initializes a useful array to avoid multiplications }
                   { by 320. }

   { Upload one after another all the graphic designs it needs }

   loadBTM('PLAYGR.BTM'  ,playscreen,TRUE);  { lo schermo virtuale }
   loadBTM('PRESENT.BTM' ,presents,false);   { la scritta ARKANOID }
   loadBTM('EXPLODE.BTM' ,explosion,FALSE);  { l'esplosione del vaus }
   loadBTM('NEWVAUS.BTM' ,newvaus,FALSE);    { la creazione del vaus }
   loadBTM('SOUNDFX.BTM' ,soundfx,FALSE);    { l'icona sound on/off  }
   loadBTM('SHINEWAL.BTM',shinewall,FALSE);  { il lucchichio dei mattoni }
   loadBTM('MINIVAUS.BTM',minivaus,FALSE);   { i vaus che indicano le vite }
   loadBTM('LEVELSEL.BTM',levelsel,FALSE);   { l'icona del livello di gioco }
   loadBTM('DROPS.BTM'   ,letters,FALSE);    { le 7 lettere }
   loadBTM('VAUS.BTM'    ,normal,FALSE);     { il vaus }
   loadBTM('LASERS.BTM'  ,lasers,FALSE);     { i raggi laser sparati dal vaus }
   loadBTM('ENLARGED.BTM',enlarged,FALSE);   { il vaus allargato }
   loadBTM('FIRE.BTM'    ,shoots,FALSE);     { il vaus coi laser montati }
   loadBTM('SCRFLUX.BTM' ,flux,FALSE);       { l'onda di flusso (per la lett. B) }

   if not success then// fatal_error('Program can''t find some BTM files');
     Application.MessageBox('Program cant find some BTM files','Error' ,MB_ICONEXCLAMATION);

   score.hiscore:=50000;
   { the record score is initially set to 50000 by default }

   sound_on:=TRUE;      { by default at the beginning the sound is ON }
   lv:=DEFLEVEL;        { and the level is set to DEFLEVEL            }

   repeat

      mousereset;

      { mainscreen returns 1,2 (play number ) or -1 = quit }
      score.pl_numb:=mainscreen;
      if score.pl_numb>0 then start_game(1);//score.pl_numb);

   until score.pl_numb<1; { cycle until it's worth -1 = quit }

//   closegraph; { Closing graphics }
//   closeprogram;

end;


procedure TForm1.Button1Click(Sender: TObject);
begin

 Arkanoid;

end;

end.
