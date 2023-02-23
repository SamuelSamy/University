bits 32

import scanf msvcrt.dll
extern scanf

global read_string, validate_input, get_next_number

segment code use32 public code


character       db  0
read_format     db  "%c", 0
ten             dw  10

read_string:
    ; Reads text from the console untill the Enter button is pressed
   
    mov edi, [esp + 4]
    
    read_character:
       
        ; scanf(read_format, &character)
        push dword character
        push dword read_format
        call [scanf]
        add esp, 4 * 2  ; clear the stack
        
        cmp byte [character], 10  ; compare character with '\n'
        je read_end  ; we are done reading (the last character was '\n')
        
        ; [edi] = [character]
        mov al, [character]
        mov [edi], al
        inc edi
        
    jmp read_character ; read another character
    
    read_end:
    ret 1 * 4
    
    
    
    
validate_input:
    ; Checks if every character from the given string is either a digit or a space
    ; Returns 1 if the string has only digits and spaces, 0 otherwise
    ; The return result is stored in EDX
    
    mov esi, [esp + 4]
    mov edx, 0 
    mov ebx, 0 
    
    lodsb  ; al = [esi]
    
    cmp al, 0  ; check if the first character is 0
    je validate_end
    
    start_loop:
       
        cmp al, 20h  ; compare al with ' '
        je is_space
        
        cmp al, 2Dh  ; compare al with '-'
        je is_minus
        
        cmp al, 30h  ; compare al with '0'
        jb validate_end
        
        cmp al, 39h  ; compare al with '9'
        jg validate_end
        
        jmp valid_character
        is_minus:
            cmp ebx, 1  ; checks if we already found a minus sign
            je validate_end 
            
            mov ebx, 1  ; we found a minus sign
            jmp valid_character
            
        is_space:
            mov ebx, 0
            jmp valid_character
            
        valid_character:
        lodsb  ; al = [esi]
        
    cmp al, 0
    jne start_loop; read another character
    
    mov edx, 1
    validate_end:
    ret 1 * 4
    
    
get_next_number:
    ; Returns the next number
    ; The return result is stored in EAX
    ; ECX will be -1 if we found 0 (end of input)
    
    mov esi, [esp + 4]
    mov eax, 0 
    mov ecx, 0
    mov ebx, 0
    
    
    lodsb  ; al = [esi]
    xchg cl, al  ; cl = [esi]
    
    cmp cl, 20h  ; check if the first character is space
    je get_next_number_end
    
    cmp cl, 0  ; check if the first character is 0
    je end_of_input
    
    
    start_loop_gen:
       
        cmp cl, 2Dh  ; compare al with '-'
        je is_minus_gen
        
        imul word [ten] ; result = result * 10
        sub cl, 30h  ; get the actual value of the digits (not in ascii code)
        add eax, ecx  ; add the digit to the result
        
        jmp get_next_char
        
        is_minus_gen:
            mov ebx, 1  ;  we will negate the number later
            

        get_next_char:
        
        xchg ecx, eax  ; change them so we do not break EAX
        lodsb  ; al = [esi]
        xchg ecx, eax  ; cl <-> al
        
        cmp cl, 0
        je end_of_input
        
    cmp cl, 20h
    jne start_loop_gen ; read another character
    
    jmp skip_end_of_input
    
    end_of_input:
    mov ecx, -1  ; ecx will be one if there are no more characters after the last checked one
    
    skip_end_of_input:
    
    cmp ebx, 1  ; check if we have to negate the number
    jne get_next_number_end
    neg eax
    
    get_next_number_end:
    ret 1 * 4
    
    
    
    