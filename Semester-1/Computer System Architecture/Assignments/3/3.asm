; Fatu Samuel
; a - byte, b - word, c - double word, d - qword 
; (a + b + c) - (d + d) + (b + c)

bits 32 

global start        

extern exit               
import exit msvcrt.dll  


segment data use32 class=data
    a   db  5
    b   dw  10
    _c  dd  15
    d   dq  10

segment code use32 class=code
    start:
        
        mov AL, [a]
        xor AH, AH
        
        
        
        
        push    dword 0      
        call    [exit]
