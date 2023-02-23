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
    
    r1 resw 4
    r2 resw 4
    r3 resw 4

    
segment code use32 class=code
    start:
       
       ; r1 = (a + b - c)
       
        mov AL, [a]
        cbw ; AX = [a]
        
        add AX, [b]
        cwde ; EAX = [a] + [b]
        
        sub EAX, [_c]
        cdq
        
        mov dword [r1], EAX
        mov dword [r1 + 4], EDX
        
        ; r2 = (a + b + d)
        
        mov AL, [a] ; AL = [a]
        cbw ; AX = [a]
        
        add AX, [b] ; AX = AX + [b] = [a] + [b]
        cwde ; EAX = [a] + [b]
        cdq ; EDX:EAX = [a] + [b]
        
        add EAX, dword [d]
        adc EDX, dword [d + 4]
        
        ; move EDX:EAX (a + b + d) to r2 
        mov dword [r2], EAX
        mov dword [r2 + 4], EDX
        
        ; r3 = (a + b)
        
        mov AL, [a]
        cbw ; AX = [a]
        
        add AX, [b] ; AX = AX + [b] = [a] + [b]
        cwde ; EAX = [a] + [b]
        cdq ; EDX:EAX = [a] + [b]
        
        mov dword [r3], EAX ; dword [r2] = EAX
        mov dword [r3 + 4], EDX ; dword [r2 + 2] = EDX
        
        mov EAX, dword [r1] ; EAX = dword [r1]
        mov EDX, dword [r1 + 4] ; EDX = dword [r1 + 4]
        
        
        add EAX, dword [r2] ; EAX = EAX + dword [r2]
        adc EDX, dword [r2 + 4] ; EDX = EDX + dword [r2 + 4] + cf
        
        sub EAX, dword [r3] ; EAX = EAX - dword [r2]
        sbb EDX, dword [r3 + 4] ; EDX = EDX - dword [r2 + 4] - cf

        push    dword 0      
        call    [exit]

        
        