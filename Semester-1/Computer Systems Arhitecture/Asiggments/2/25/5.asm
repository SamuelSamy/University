; (e + f + g) / (a + b)
; a, b, c, d - byte
; e, f, g, h - word

bits 32 

global start        

extern exit               
import exit msvcrt.dll  


segment data use32 class=data
    e   dw  16
    f   dw  16
    g   dw  16
    a   db  4
    b   db  4

segment code use32 class=code
    start:
       
        mov EAX, [e]
        add EAX, [f]
        add EAX, [g]
           
        mov DL, [a]
        add DL, [b]
           
        div DL
       
        push    dword 0      
        call    [exit]