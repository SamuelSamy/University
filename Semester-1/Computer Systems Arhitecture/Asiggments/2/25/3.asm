; (a + b - c) - d
; a, b, c, d - word

bits 32 

global start        

extern exit               
import exit msvcrt.dll  


segment data use32 class=data
    a   dw  16
    b   dw  8
    _c  dw  4
    d   dw  2
    
segment code use32 class=code
    start:
       
        mov AX, [a]
        add AX, [b]
       
        sub AX, [_c]
        
        sub AX, [d]
       
        push    dword 0      
        call    [exit]
