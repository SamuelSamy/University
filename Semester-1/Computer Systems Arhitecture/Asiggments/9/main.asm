; Fatu Samuel

; Read a string of signed numbers in base 10 from keyboard. 
; Determine the minimum value of the string and write it in the file min.txt (it will be created) in 16 base.

bits 32 

global start        

import exit msvcrt.dll  
import scanf msvcrt.dll
import printf msvcrt.dll
import fopen msvcrt.dll
import fprintf msvcrt.dll
import fclose msvcrt.dll

extern exit, scanf, printf, fopen, fprintf, fclose
extern read_string, validate_input, get_next_number


segment data use32 class=data
    file_name           db  "min.txt", 0
    file_descriptior    dd  -1 
    access_mode         db  "w", 0
    
    b16_format          db  "%x ", 0
    
    invalid_input_text  db  "The specified input is not valid!", 0
    
    minim        dd  01000000000000000000000000000000b
    
    text        times 128 db 0
  
    temp        dd  0
    
    
segment code use32 class=code
    start:
        
        push dword text
        call read_string  ; reads the string from console
        
        push dword text
        call validate_input  ; validates the string

        cmp edx, 0  ; the input is not valid
        je Invalid_Input
        
        
        mov esi, text
        
        solve:
            
            push dword esi
            call get_next_number
            
            mov [temp], ecx
            
            cmp eax, [minim]
            jge skip
            mov [minim], eax
            
            mov ecx, [temp]
            
            skip:
        cmp ecx, -1  ; if ecx == -1 we computed the last number
        jne solve
        
        ; open the file
        
        push dword access_mode
        push dword file_name
        call [fopen]    
        add esp, 4 * 2
        
        mov [file_descriptior], eax 
        
        ; writes the number
        push dword [minim]
        push dword b16_format
        push dword [file_descriptior]
        call [fprintf]
        add esp, 4 * 2
        
        ; close the file
        push dword [file_descriptior]
        call [fclose]
        add esp, 4 * 1
        
        
        jmp _End
        
        Invalid_Input:
            push dword invalid_input_text
            call [printf]
            
            
        _End:
        
        push    dword 0      
        call    [exit]
