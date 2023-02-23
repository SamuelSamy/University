; Fatu Samuel


; A string of bytes is given. Obtain the mirror image of the binary representation of this string of bytes.
; s DB 01011100b, 10001001b, 11100101b 
; d DB 10100111b, 10010001b, 00111010b

bits 32 

global start        

extern exit               
import exit msvcrt.dll  


segment data use32 class=data
    s   db  01011100b, 10001001b, 11100101b
    len_s equ ($ - s)
    
    d   times len_s db  0
    

segment code use32 class=code
    start:
        
        mov esi, s
        mov edi, d + len_s - 1
        
        mov ecx, len_s
       
        jecxz end_loop
        
        start_loop:
            
            cld ; clears the direction flag
            lodsb ; the btye from the address <esi> is loaded in `al` ; esi += 1
            
            mov bl, 0  ; bl = 0
            
            mov dl, 8 ; we will mirror 8 bits
            
            mirror_bits:
            
                shr al, 1 ; shift to the right `al` with 1 bit ; cf will be the last disappearing bit from `al`
                rcl bl, 1 ; rotate `bl`with carry to the left ; cf will be added as the last b of 'bl'
                
                dec dl ; dl -= 1
                cmp dl, 0 ; compares `dl` with 0
            
            jne mirror_bits ; jumps if dl != 0
            
            mov al, bl ; al = bl
            
            std ; sets the direction flag
            stosb ; the byte from `al` is stored into the byte from the address <edi> ; edi -= 1 (df = 1)
           
        loop start_loop
        
        end_loop
        
        push    dword 0      
        call    [exit]
