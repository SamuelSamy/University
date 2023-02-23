; [100 - 10 * a + 4 * (b + c)] - d
; a, b, c - byte
; d - word

bits 32 

global start        

extern exit               
import exit msvcrt.dll  

segment data use32 class=data
    a   db  8
    b   db  4
    _c  db  2
    d   dw  1
    
   temp db  10

segment code use32 class=code
    start:
      
        mov AL, [a] ; AL <- [a]
        mul byte [temp] ; AX <- AL * [temp]
       
        mov BX, AX ; BX <- AX ; (10 * a)
       
        mov AL, [b]  ; AL <- [b]
        add AL, [_c] ; AL <- AL + [_c] 
       
        mov byte [temp], 4 ; [temp] <- 4
       
        mul byte [temp] ; AX <- AL * [temp]
       
        mov DX, AX ; 4 * (b + c)
       
       
        mov byte [temp], 100 ; [temp] <- 100
       
        mov AX, [temp] ; AX <- [temp]
        sub AX, BX ; AX <- AX - BX ; 100 - 10 * a
        add AX, DX ; AX <- AX - DX ; 100 - 10 * a + 4 * (b + c)
       
        sub AX, [d] ; AX <- AX - [d]
        
        push    dword 0      
        call    [exit]
