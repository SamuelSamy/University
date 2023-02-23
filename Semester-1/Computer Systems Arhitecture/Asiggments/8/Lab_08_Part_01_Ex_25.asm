; Fatu Samuel


; Read two numbers a and b (in base 10) from the keyboard and determine the order relation between them (either a < b, or a = b, or a > b). 
; Display the result in the following format: "<a> < <b>, <a> = <b> or <a> > <b>".

bits 32 

global start        

extern exit, printf, scanf               
import exit msvcrt.dll  
import printf msvcrt.dll
import scanf msvcrt.dll


segment data use32 class=data
    a   dd  0
    b   dd  0
    
    print_format_a   db  "a = ", 0
    print_format_b   db  "b = ", 0
    print_format db "<%d> %s <%d>", 0
    
    decimal_scan_format  db "%d", 0
       
    greater db  ">", 0
    equal   db  "=", 0
    less    db  "<", 0
  
  
segment code use32 class=code
    start:
        
        ; read a
        push dword print_format_a ; push the print format on the stack
        call [printf]
        
        add esp, 4 * 1 ; increment the esp
        
        
        push dword a ; push a on the stack
        push dword decimal_scan_format ; push the scan format on the stack
        
        call [scanf]
        
        add esp, 4 * 2 ; increment the esp
        
        
        
        ; read b
        push dword print_format_b ; ; push the print format on the stack
        call [printf]
        
        add esp, 4 * 1 ; increment the esp
        
        push dword b ; push b on the stack
        push dword decimal_scan_format ; push the scan format on the stack
        
        call [scanf]
        
        add esp, 4 * 2 ; increment the esp
        
        
        ; print result
        
        push dword [b] ; push b on the stack

        mov eax, [a] ; eax = [a]
        mov ebx, [b] ; ebx = [b]
        cmp eax, ebx 
        
        jg Greater  ; a > b
        je Equal    ; a = b
        jl Less     ; a < b
                    
            
        Greater:
            push dword greater ; push the '>' sign on the stack
            jmp Print

           
        Equal:
            push dword equal ; puush the '=' sign on the stack
            jmp Print
                
                
        Less:
            push dword less ; push the '<' sign on the stack
            
            
        Print:

        push dword [a] ; push a on the stack
        push dword print_format ; push the print format on the stack
        
        call [printf]
        
        add esp, 4 * 4 ; increment the esp
        
        push    dword 0      
        call    [exit]
