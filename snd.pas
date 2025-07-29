unit snd;

interface

var
   sound_on  : boolean;
   snd_delay : integer;
   use_sb    : boolean;

procedure DetectFM;
procedure MakeNote(nota,attack,del : smallint);
procedure Start_level;
procedure ball_block_sound(freq,duration : smallint);
procedure Death_sound(w : smallint);

implementation


procedure MakeNote(nota,attack,del : smallint);
   begin

   end;

procedure ball_block_sound(freq,duration : smallint);
   begin

   end;

procedure SB_note(voice,note,del : smallint);
   begin

   end;


procedure start_level;
begin

end;


procedure DetectFM;
begin {DetectFm}
  use_sb:=TRUE;

end; {DetectFM}


procedure Death_sound(w : smallint);
   begin

   end;

end.