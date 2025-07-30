unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, GR32, GR32_Image, GR32_Layers;

type
  TForm1 = class(TForm)
    Image321: TImage32;
    Button1: TButton;

    procedure Image321MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure Image321MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    procedure show_play;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image321MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

  private
    { Private declarations }

      procedure TestKeyDown(Var Msg:TMsg; var Handled:Boolean);

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

uses snd;//,FM;


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

   { Questa porzione contiene solo il caricamento in memoria dei files dati }

   DetectFM;
   success:=TRUE;  { Success viene messo a FALSE se si verifica un errore da disco }

   load_all_walls; { Carica i 32 muri di gioco }

   initSVGA;       { Attiva la modalita grafica 320x200x256 col. }
   initRowArray;   { Inizializza un array utile per evitare le moltiplicazioni }
                   { per 320. }

   { Carica uno dopo l'altro tutti i disegni grafici di cui necessita }

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
   { il punteggio record viene settato inizialmente a 50000 per default }

   sound_on:=TRUE;      { per default all'inizio il suono e' attivo }
   lv:=DEFLEVEL;        { e il livello viene settato a DEFLEVEL     }

   repeat

      mousereset;

      { mainscreen restituisce 1,2 (numero gioc. ) o -1 = quit }
      score.pl_numb:=mainscreen;
      if score.pl_numb>0 then start_game(1);//score.pl_numb);

   until score.pl_numb<1; { cicla finche' non vale -1 = quit }

//   closegraph; { Chiude la grafica }
//   closeprogram;

end;



procedure TForm1.TestKeyDown(Var Msg:TMsg; var Handled:Boolean);
begin


 if Msg.message=WM_KEYUP then
  if (Msg.wParam in [VK_SHIFT, VK_CONTROL]) then begin

   ShiftCtrl:=false

  end;


 if Msg.message=WM_KEYDOWN then

 if (Msg.wParam in [VK_SHIFT, VK_CONTROL]) then begin

  ShiftCtrl:=true;

 end else

 if (Msg.wParam in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, vk_NUMPAD2, vk_NUMPAD4, vk_NUMPAD6, vk_NUMPAD8{, vk_SPACE}]) then begin


    KeyMove:=Msg.wParam;

  end;

end;


procedure TForm1.Button1Click(Sender: TObject);
begin

 Arkanoid;

end;

end.
