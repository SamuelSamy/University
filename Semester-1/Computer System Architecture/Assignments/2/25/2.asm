; (c + d + d) - (a + a + b)
; a, b, c, d - byte

bits 32 

global start        

extern exit               
import exit msvcrt.dll  

segment data use32 class=data
    a   db  4
    b   db  8
    _c  db  16
    d   db  2


segment code use32 class=code
    start:
        
        mov AL, [_c] ; AL <- [_c]
        add AL, [d] ; AL <- AL + [d]
        add AL, [d] ; AL <- AL + [d]
        
        mov BL, [a] ; BL <- [a]
        add BL, [a] ; BL <- BL +[a]
        add BL, [b] ; BL <- BL +[a]
       
        sub AL, BL ; AL <- AL - BL
        
        push    dword 0      
        call    [exit]
