; Fatu Samuel

; A file name and a text (defined in data segment) are given. 
; The text contains lowercase letters, uppercase letters, digits and special characters. 
; Replace all spaces from the text with character 'S'. 
; Create a file with the given name and write the generated text to file.

bits 32 

global start        

extern exit, fopen, fprintf, fclose      
import exit msvcrt.dll  
import fopen msvcrt.dll
import fprintf msvcrt.dll
import fclose msvcrt.dll


segment data use32 class=data
    file_name           db  "output.txt", 0
    file_descriptior    dd  -1 
    access_mode         db  "w", 0
    
    text        db  "A very Long text that c0nta1ns    a lot of characters!? @#", 0
    text_len    equ ($ - text)
    
    new_text    times text_len  db  0
    
    
segment code use32 class=code
    start:
        
        ; replace all space characters from 'text' with 'S'
        ; the result is stored in 'new_text'
        mov ecx, text_len  ; text_len 
        
        mov esi, text
        mov edi, new_text
        
        Loop_Start:
                
            lodsb
            
            cmp al, ' '
            jne After_if
            
            mov al, 'S'
            
            After_if:
            stosb
            
        loop Loop_Start
        
        ; try to create the new file
        
        push dword access_mode
        push dword file_name
        call [fopen]    ; the file descriptor will be stored in eax
        add esp, 4 * 2
        
        mov [file_descriptior], eax ; store the file descriptor
        
        ; check if the file was succesfully created (eax != 0)
        cmp eax, 0 
        je Exit
        
        ; write the new text in the file
        push dword new_text
        push dword [file_descriptior]
        call [fprintf]
        add esp, 4 * 2
        
        ; close the file
        push dword [file_descriptior]
        call [fclose]
        add esp, 4 * 1
        
        
        Exit:
        push    dword 0      
        call    [exit]
