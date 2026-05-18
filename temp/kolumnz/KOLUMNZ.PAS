{$A+,B-,F-,I+,R-,S-}
Program Kolumz;
Uses Crt, Graph, Pcx_tp;

type
  Map     = Array [0..30,0..7] of Byte;
  Line    = Array [0..2] of Byte;
  OfsMap  = Array [0..1,0..3] of ShortInt;
  cub_buf = Array [0..299] of byte;

const
  LibName   : String[12] = 'KOLUMZ.PIC';
  MaxY      : Byte = 18;
  MaxCol    : Byte = 6;
  OfsMapX   : Ofsmap = ((1,1,0,1),(2,2,0,2));
  OfsMapY   : Ofsmap = ((0,1,1,-1),(0,2,2,-2));
  ZLn       : Line = (0,0,0);
  File_Name : Array[0..6] of string[12] = ('BLACKCUB.PCX','REDCUB.PCX',
                                       'GREENCUB.PCX','CYANCUB.PCX',
                                       'MAGENCUB.PCX','YELLCUB.PCX',
                                       'BLUECUB.PCX');
  Dels=100;
  TopX=365;
  TopY=87;
var
  SysTime                 : LongInt absolute $0:$046C;
  OldTime,NewTime         : LongInt;
  Count,GameCount,GX,GY,OldX,OldY,I,J,K : Byte;
  Snd,RealSpeed,Speed,GameScore,WorkTime : Word;
  BeepFlag,FoundLn,Flag   : Boolean;
  GameMap,Map1            : Map;
  GmLn                    : Line;
  GmStr, NameT            : String;
  Key                     : Char;
  Cubes                   : Array [0..6] of cub_buf;
  SizeCub                 : Array [0..6] of integer;
  ScrenBuf                : Array [0..27000] of byte;
  SizeScren,RetCode,GD,GM : Integer;

Procedure Beep;
begin
  Sound(300);
  Delay(25);
  NoSound;
end;

Procedure VerifyName;
Type
    RecF = record
           NameWin  : String[20];
           ScoreWin : Word;
           end;
Var
OuSt     : String;
TopTen   : File of RecF;
Top      : Array[1..10] of RecF;

begin
     Beep;
     Assign(TopTen,'KOLUMZ.TOP');
     Reset(TopTen);
     SetTextJustify(LeftText,CenterText);
     SetTextStyle(DefaultFont, HorizDir, 0);
     for i:=1 to 10 do
     begin
          Read(TopTen,Top[i]);
          for j:=Length(Top[i].NameWin)+1 to 20 do
              Top[i].NameWin:=Top[i].NameWin+'.';
          Str(Top[i].ScoreWin:5,NameT);
          NameT:=Top[i].NameWin+'  '+NameT;
          OutTextXY(TopX,i*10+TopY,NameT);
     end;
     Close(TopTen);
     while GameScore>Top[i].ScoreWin do dec(i);
     if i<10 then
     begin
          NameT:='';
          SetColor(0);
          OutTextXY(TopX,i*10+TopY,'ллллллллллллллллллллллллллл');
          SetColor(15);
          Str(GameScore,OuSt);
          OuSt:='....................   '+OuSt;
          OutTextXY(TopX,i*10+TopY,OuSt);
          MoveTo(TopX,i*10+TopY);
          Count:=0;
          repeat
                repeat
                      Delay(dels);
                      SetColor(15);
                      OutTextXY(GetX,GetY,'л');
                      SetColor(0);
                      Delay(dels);
                      OutTextXY(GetX,GetY,'л');
                      SetColor(15);
                      OutTextXY(Getx,GetY,'.');
                until keypressed;
                Key:=ReadKey;
                if Key=#0 then Key:=ReadKey;
                case Key of
                     #32..#127 : if Count<20 then
                                 begin
                                      Key:=UpCase(Key);
                                      SetColor(0);
                                      OutTextXY(GetX,GetY,'л');
                                      SetColor(15);
                                      OutText(Key);
                                      NameT:=NameT+Key;
                                      Inc(Count);
                                 end;
                     #8 : if Count>0 then
                          begin
                               MoveTo(GetX-8,GetY);
                               SetColor(0);
                               OutTextXY(GetX,GetY,'л');
                               SetColor(15);
                               OutTextXY(GetX,GetY,'.');
                               Delete(NameT,Count,1);
                               Dec(Count);
                          end;
                end;
          until Key=#13;
          for j:=10 downto i+1 do Top[j]:=Top[j-1];
          Top[j].NameWin:=NameT;
          Top[j].ScoreWin:=GameScore;
          Rewrite(TopTen);
          for j:=1 to 10 do Write(TopTen,Top[j]);
          Close(TopTen);
     end;
     repeat until keypressed;
     Key:=ReadKey;
     if Key=#0 then Key:=ReadKey;
end;

Procedure MyExit;
begin
     retcode := pcxSetMode(pcxText);
     Halt(2);
end;

Procedure FoundLines(x,y:byte;l:map);
var
  mask,a,e,f,g,h	:	shortint;

begin
  mask:=L[y,x];
  if mask=0 then exit;
  for a:=0 to 3 do
  begin
       e:=y+OfsMapY[0,a];
       f:=x+OfsMapX[0,a];
       g:=y+OfsMapY[1,a];
       h:=x+OfsMapX[1,a];
       if ((L[e,f]=mask) and (L[g,h]=mask)) then
       begin
            Map1[y,x]:=1;
            Map1[e,f]:=1;
            Map1[g,h]:=1;
            inc(GameScore);
            inc(GameCount);
            BeepFlag:=True;
       end;
    end;
end;

Procedure PutMap;
begin
     for I:=1 to MaxY do
          for J:=0 to 5 do
               retcode := pcxBufferDisplay(@cubes[GameMap[I,J]],
                          sizecub[GameMap[I,J]],(J shl 4)+119,(I+1)*13+4,0);
end;

Procedure PutLine;
var i:byte;
begin
     for I:=0 to 2 do
     retcode := pcxBufferDisplay(@cubes[ZLn[i]],sizecub[ZLn[i]],
                                 (OldX shl 4)+119,(OldY-I)*13+4,0);
     for I:=0 to 2 do
     retcode := pcxBufferDisplay(@cubes[GmLn[i]],sizecub[GmLn[i]],
                                 (GX shl 4)+119,(GY-I)*13+4,0);
end;

Procedure MainGame;
  begin
     SetTextJustify(RightText,CenterText);
     SetTextStyle(DefaultFont, HorizDir, 2);
     retcode := pcxBufferDisplay(@ScrenBuf,SizeScren,0,0,0);
     GameScore:=0;
     FillChar(GameMap,200,#0);
     PutMap;
     Speed:=10;
     GameCount:=0;
     while GameMap[3,3]=0 do
     begin
          GX:=3;GY:=4;
          MemW[$0000:$041A]:=MemW[$0000:$041C];
          OldX:=3;OldY:=4;
          for I:=0 to 2 do GmLn[I]:=Random(MaxCol)+1;          {RandLine}
          PutLine;
          RealSpeed:=Speed;
	  While ((GY<=MaxY) and (GameMap[GY,GX]=0)) do
	    begin
                OldTime:=SysTime;
                repeat
                      if keypressed then
                      begin
                         Key:=ReadKey;
                         case Key of
                              #0: begin
                                  Key:=ReadKey;
                                  case Key of
                                       #75: begin
                                            OldX:=GX;
                                            OldY:=GY;
                                            if GX>0 then Dec(GX);
                                            if GameMap[GY,GX]>0 then GX:=OldX;
                                            PutLine;
                                            end;
                                       #77: begin
                                            OldX:=GX;
                                            OldY:=GY;
                                            if Gx<5 then Inc(GX);
                                            if GameMap[GY,GX]>0 then GX:=OldX;
                                            PutLine;
                                            end;
                                       #72: begin
                                                 i:=GmLn[0];
                                                 GmLn[0]:=GmLn[1];
                                                 GmLn[1]:=GmLn[2];
                                                 GmLn[2]:=i;
                                                 PutLine;
                                            end;
                                       #80: RealSpeed:=1;
                                  end;
                              end;
                              #27: MyExit;
                              #32: begin
                                        repeat until keypressed;
                                        Key:=ReadKey;
                                        if Key=#0 then Key:=ReadKey;
                                   end;
                         end;
                      end;
                      NewTime:=SysTime;
                      WorkTime:=NewTime-OldTime;
                until worktime>=RealSpeed;
                OldY:=GY;
                OldX:=GX;
                Inc(GY);
                PutLine;
            end;
          for i:=0 to 2 do GameMap[GY-I-1,GX]:=GmLn[I];
{ ************** DelLines *************** };
          BeepFlag:=False;
          repeat
                FillChar(Map1,200,#0);
                FoundLn:=false;
                for i:=MaxY downto 1 do
                    for j:=0 to 5 do FoundLines(j,i,GameMap);
                for i:=MaxY downto 1 do
                    for j:=0 to 5 do
                        if Map1[i,j]=1 then
                        begin
                             GameMap[i,j]:=0;
                             FoundLn:=true;
                        end;
{ ************** LnDown *************** };
     flag:=true;
     while flag do
     begin
          Flag:=false;
          For J:=MaxY downto 1 do
          begin
               for I:=0 to 5 do
               if ((GameMap[J,I]=0) and (GameMap[J-1,I]>0)) then
               begin
                    Flag:=true;
                    for K:=J-1 downto 1 do GameMap[K+1,I]:=GameMap[K,I];
               end;
               if flag then J:=1;
          end;
          PutMap;
     end;

{ ************** LnDown *************** };
          until not FoundLn;
          if BeepFlag then Beep;
          str(GameScore:3,GmStr);          {PrintScore}
          SetColor(0);
          OutTextXY(275,42,'ллл');
          SetColor(15);
          OutTextXY(275,42,GmStr);
{ ************** DelLines *************** };
          if GameCount>=20 then
            begin
                 for Snd:=1000 to 5000 do Sound(Snd);
                 NoSound;
                 delay(150);
                 for Snd:=1000 to 9000 do Sound(Snd);
                 for Snd:=9000 downto 1000 do Sound(Snd);
                 NoSound;
                 GameCount:=0;
                 Dec(Speed);
            end;
     end;
     VerifyName;
  end;

begin
     Randomize;
     DetectGraph(GD, GM);
     case GD of
          VGA: begin
                    retcode := pcxSetDisplay(pcxVGA_12);
                    GM := VGAHi;
               end;
          EGA: begin
                    retcode := pcxSetDisplay(pcxEGA_10);
                    GM := EGAHi;
               end;
     end;
     InitGraph(GD,GM,'');
     retcode := pcxSetMode(pcxGraphics);
     for i:=0 to 6 do sizecub[i]:=pcxLibBuffer(LibName,file_name[i],
                                               @cubes[i],299);
     SizeScren := pcxLibBuffer(LibName,'MAINSCR.PCX',@ScrenBuf,27000);
     While MaxCol=6 do MainGame;
end.
