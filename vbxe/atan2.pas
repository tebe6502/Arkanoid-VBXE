uses crt;

const

  atan_tab: array[0..255] of Byte = (
    $20,$20,$20,$21,$21,$22,$22,$23,$23,$23,$24,$24,$25,$25,$26,$26,
    $26,$27,$27,$28,$28,$28,$29,$29,$2A,$2A,$2A,$2B,$2B,$2C,$2C,$2C,
    $2D,$2D,$2D,$2E,$2E,$2E,$2F,$2F,$2F,$30,$30,$30,$31,$31,$31,$31,
    $32,$32,$32,$32,$33,$33,$33,$33,$34,$34,$34,$34,$35,$35,$35,$35,
    $36,$36,$36,$36,$36,$37,$37,$37,$37,$37,$37,$38,$38,$38,$38,$38,
    $38,$39,$39,$39,$39,$39,$39,$39,$39,$3A,$3A,$3A,$3A,$3A,$3A,$3A,
    $3A,$3B,$3B,$3B,$3B,$3B,$3B,$3B,$3B,$3B,$3B,$3B,$3C,$3C,$3C,$3C,
    $3C,$3C,$3C,$3C,$3C,$3C,$3C,$3C,$3C,$3D,$3D,$3D,$3D,$3D,$3D,$3D,
    $3D,$3D,$3D,$3D,$3D,$3D,$3D,$3D,$3D,$3D,$3D,$3D,$3D,$3E,$3E,$3E,
    $3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E,
    $3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E,$3F,$3F,
    $3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,
    $3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,
    $3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,
    $3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,
    $3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F
  );

  log2_tab: array[0..255] of Byte = (
    $00,$00,$20,$32,$40,$4A,$52,$59,$60,$65,$6A,$6E,$72,$76,$79,$7D,
    $80,$82,$85,$87,$8A,$8C,$8E,$90,$92,$94,$96,$98,$99,$9B,$9D,$9E,
    $A0,$A1,$A2,$A4,$A5,$A6,$A7,$A9,$AA,$AB,$AC,$AD,$AE,$AF,$B0,$B1,
    $B2,$B3,$B4,$B5,$B6,$B7,$B8,$B9,$B9,$BA,$BB,$BC,$BD,$BD,$BE,$BF,
    $C0,$C0,$C1,$C2,$C2,$C3,$C4,$C4,$C5,$C6,$C6,$C7,$C7,$C8,$C9,$C9,
    $CA,$CA,$CB,$CC,$CC,$CD,$CD,$CE,$CE,$CF,$CF,$D0,$D0,$D1,$D1,$D2,
    $D2,$D3,$D3,$D4,$D4,$D5,$D5,$D5,$D6,$D6,$D7,$D7,$D8,$D8,$D9,$D9,
    $D9,$DA,$DA,$DB,$DB,$DB,$DC,$DC,$DD,$DD,$DD,$DE,$DE,$DE,$DF,$DF,
    $DF,$E0,$E0,$E1,$E1,$E1,$E2,$E2,$E2,$E3,$E3,$E3,$E4,$E4,$E4,$E5,
    $E5,$E5,$E6,$E6,$E6,$E7,$E7,$E7,$E7,$E8,$E8,$E8,$E9,$E9,$E9,$EA,
    $EA,$EA,$EA,$EB,$EB,$EB,$EC,$EC,$EC,$EC,$ED,$ED,$ED,$ED,$EE,$EE,
    $EE,$EE,$EF,$EF,$EF,$EF,$F0,$F0,$F0,$F1,$F1,$F1,$F1,$F1,$F2,$F2,
    $F2,$F2,$F3,$F3,$F3,$F3,$F4,$F4,$F4,$F4,$F5,$F5,$F5,$F5,$F5,$F6,
    $F6,$F6,$F6,$F7,$F7,$F7,$F7,$F7,$F8,$F8,$F8,$F8,$F9,$F9,$F9,$F9,
    $F9,$FA,$FA,$FA,$FA,$FA,$FB,$FB,$FB,$FB,$FB,$FC,$FC,$FC,$FC,$FC,
    $FD,$FD,$FD,$FD,$FD,$FD,$FE,$FE,$FE,$FE,$FE,$FF,$FF,$FF,$FF,$FF
  );


var
 f: file;
 
 scale_l, scale_h: array [0..255] of byte;
 
 i: byte;
 
 w: word;
  
  
begin

 for i:=0 to 255 do begin
  w:=round(i*(360/256));
  
  scale_l[i] := lo(w);
  scale_h[i] := hi(w);
 end;  

 assign(f, 'atan_tab.dat'); rewrite(f, 1);
 
 blockwrite(f, atan_tab, 256);
 blockwrite(f, log2_tab, 256);
 blockwrite(f, scale_l, 256);
 blockwrite(f, scale_h, 256);
 
 close(f);

end.