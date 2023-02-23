; 64 * 4

bits 32 

global start        

extern exit               
import exit msvcrt.dll  


segment data use32 class=data
    a   db  4

segment code use32 class=code
    start:
        
        mov AX, 64
        mul byte [a]
        
        push    dword 0      
        call    [exit]
