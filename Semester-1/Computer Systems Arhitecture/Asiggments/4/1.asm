; Fatu Samuel

; Given the doublewords M and N, compute the doubleword P as follows.
; the bits 0-6 of P are the same as the bits 10-16 of M
; the bits 7-20 of P are the same as the bits 7-20 of (M AND N).
; the bits 21-31 of P are the same as the bits 1-11 of N.

bits 32 

global start        

extern exit               
import exit msvcrt.dll  


segment data use32 class=data
    n   dd  00000000000000000000111111111110b
    m   dd  11111111111111111111111111111111b
        
    p   dd  0
    
    
segment code use32 class=code
    start:
       
        mov ebx, 0 ; we compute the result in ebx
       
        mov eax, [m] ; we isolate bits 10 - 16 of M
        and eax, 00000000000000011111110000000000b
        mov  cl, 10 
        ror eax, cl ; we rotate 10 positions to the right
        or  ebx, eax ; we put the the bits into the result
       
        mov eax, [n] ; we isolate bits 7 - 20 of (M AND N)
        and eax, [m]
        and eax, 00000000000111111111111110000000b
        or  ebx, eax ; we put the bits into the result
        
        mov eax, [n] ; we isolate the bits 1 - 11 of N
        and eax, 00000000000000000000111111111110b
        mov  cl, 20
        rol eax, cl ; we rotate 20 poositions to the left
        or  ebx, eax ; we put the bits into the result
        
        mov dword [p], ebx ; we move the result from the register to the variable
        
        push    dword 0      
        call    [exit]
