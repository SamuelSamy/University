; Fatu Samuel
; (a + b - c) + (a + b + d) - (a + b)
;      r1     +     r2      -    r3

; a - byte, b - word, c - double word, d - qword

bits 32 

global start        

extern exit               
import exit msvcrt.dll  


segment data use32 class=data
    a   db  5
    b   dw  10
    _c  dd  30
    d   dq  10
    
    r1 resw 2
    r2 resw 1

    
segment code use32 class=code
    start:
       
       ; r1 = (a + b - c)
       
        mov AL, [a]
        cbw ; AX = [a]
        
        add AX, [b]
        cwd ; DX:AX = [a] + [b]
        
        sub AX, word [_c] ; AX = AX - word [_c]
        sbb DX, word [_c + 2] ; DX = DX - word [_c + 2]
        
        push DX
        push AX
        pop dword [r1]
           
        
        ; EBX:ECX = (a + b + d)
        
        mov AL, [a]
        cbw
        
        add AX, [b]
        cwde
        cdq ; EDX:EAX
        
        add EAX, [d]
        adc EDX, [d + 4]
        
        mov EBX, EDX
        mov ECX, EAX
        
        ; r2 = (a + b)
        
        mov AL, [a]
        cbw
        
        add AX, [b]
        
        push AX
        pop word [r2]
        
        mov EAX, dword [r1]
        cdq
        
        add EAX, ECX
        adc EDX, EBX
        
        sub EAX, [r2]
        
        push    dword 0      
        call    [exit]

        
        