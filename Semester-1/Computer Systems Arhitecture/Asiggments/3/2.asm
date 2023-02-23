; Fatu Samuel
; (a * a + b + x) / (b + b) + c * c
; a-word; b-byte; c-doubleword; x-qword

bits 32 

global start        

extern exit               
import exit msvcrt.dll  


segment data use32 class=data
    a   dw  10
    b   db  5
    _c  dd  3
    x   dq  5

    temp resw 2
    r resw 2
    
    
segment code use32 class=code
    start:
       
        mov AL, [b] ; AL = [b]
        cbw ; AX = AL = [b]
        cwde ; EAX = AX = [b]
        cdq ; EDX:EAX = EAX = [b]
       
        mov ECX, EAX ; ECX = EAX
        mov EBX, EDX ; EBX = EDX
       
        ; EBX:ECX = [b]
        
        mov AX, [a] ; AX = [a]
        imul AX, [a] ; DX:AX = AX * [a]
        
        push DX
        push AX
        
        pop EAX ; EAX = DX:AX
        
        cdq ; EDX:EAX = EAX
       
        add EAX, ECX ; EAX = EAX + ECX
        adc EDX, EBX ; EDX = EDX + EBX + cf
       
        add EAX, dword [x] ; EAX = EAX + dword [x]
        adc EDX, dword [x + 4] ; EDX = EDX + dword [x + 4] + cf
        ; EDX:EAX = (a * a + b + x)
       
        mov ECX, EAX ; ECX = EAX
        mov EBX, EDX ; EBX = EDX
        ; EBX:ECX = (a * a + b + x)
        
        mov AL, [b] ; AL = [b]
        add AL, [b] ; AL = AL + [b]
        cbw ; AX = AL = [b] + [b]
        cwde ; EAX = AX = [b] + [b]
        
        mov dword [temp], EAX ; [temp] = EAX
        
        mov EAX, ECX ; EAX = ECX
        mov EDX, EBX ; EDX = EBX
        
        ; quadword / dword = dword
        idiv dword [temp] ; EAX = EDX:EAX / [temp] = (a * a + b + x) /  (b + b)
        
        mov dword [r], EAX ; r = EAX = (a * a + b + x) / (b + b) 
        
        mov EAX, [_c] ; EAX = [_c]
        imul dword [_c] ; EDX:EAX = c * c
        
        mov ECX, EAX ; ECX = EAX
        mov EBX, EDX ; EBX = EDX
        ; EBX:ECX =  c * c
        
        mov EAX, dword [r] ; EAX = dword [r]
        cdq ; EDX:EAX = EAX = [r]
        
        add EAX, ECX ; EAX = EAX + ECX
        adc EDX, EBX ; EDX = EDX + EBX + cf
        
        ; EDX:EAX = (a * a + b + x) / (b + b) + c * c
        
        push    dword 0      
        call    [exit]

        
        