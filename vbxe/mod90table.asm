
	opt h-
/*
lo = $80
hi = $81

	org $2000

	mwa #91 lo


; N = hi:lo (16-bit)
; chcemy N mod 90 w A

    LDA hi
    TAX
    LDA Mod90Table,X   ; tablica: (X*256) mod 90
    CLC
    ADC lo             ; dodaj młodszy bajt
    CMP #90
    BCC done
    SBC #90
    CMP #90
    BCC done
    SBC #90            ; max 2 odejmowania bo 256 < 3*90
done:
    ; A = reszta mod 90

	nop

	brk
*/
Mod90Table :256 dta b( [[#*256]%90] )
