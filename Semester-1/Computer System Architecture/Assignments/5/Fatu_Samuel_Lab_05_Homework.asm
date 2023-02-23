; Fatu Samuel

; Two character strings S1 and S2 are given. 
; Obtain the string D which contains all the elements of S1 that do not appear in S2.

; S1: '+', '4', '2', 'a', '8', '4', 'X', '5'
; S2: 'a', '4', '5'
;  D: '+', '2', '8', 'X'

bits 32 

global start        

extern exit               
import exit msvcrt.dll  


segment data use32 class=data
    s1 db  '+', '4', '2', 'a', '8', '4', 'X', '5'
    l_s1 equ $-s1 ; length of s1
    
    s2 db 'a', '4', '5'
    l_s2 equ $-s2 ; length of s2
    
    d times l_s1 db 0
    

segment code use32 class=code
    start:
        
        mov edi, 0
        
        mov ecx, l_s1 ; ecx = len(s1)
        jecxz end_outer_loop ; jumps to 'end_outer_loop' if len(s1) == 0
        
        mov ecx, 0 ; ecx = len(s1)
        
        outer_loop:
            
            mov ebx, l_s2 ; ebx = len(s2)
            mov esi, 0
            mov al, [s1 + ecx] ; al = (edx)th element of s1
            
            cmp ebx, 0 ; compares len(s2) with 0
            je end_inner_loop ; jumps to 'end_inner_loop' if len(s2) == 0
            
            inner_loop:
                
                mov dl, [s2 + esi] ; dl = [s2 + esi]
                
                cmp al, dl ; compares al with dl
                
                je end_inner_loop ; if al == dl jumps to 'end_inner_loop' (we do not store the charcter)
                
                inc esi ; esi += 1
                
                cmp esi, l_s2 ; compares esi with the length of the second string (zf == 1 <=> we looped over all the elements from s2)
                
                je store_character ; if zf == 1 (esi == len(s2)) -> jump to 'store_character' ([s2 + esi] is not in s1
                
                jmp inner_loop
                
                
            store_character:
                
                mov [d + edi], al ; [d + dsi] = al (stores the charcater if it's not in s1)
                
                inc edi ; edi += 1
            
            end_inner_loop:
                            
        
        inc ecx ; ecx += 1
        
        cmp ecx, l_s1 ; compares ecx with len(s1)
        jb outer_loop ; jumps to 'outer_loop' if ecx < len(s1)
        
        end_outer_loop:
       
        push    dword 0      
        call    [exit]
