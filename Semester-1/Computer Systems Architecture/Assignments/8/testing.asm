;26. A file name (defined in data segment) is given. Create a file with the given name, then read words from the keyboard until character '$' is read. Write only the words that contain at least one uppercase letter to file.
bits 32

global start

; declare external functions needed by our program
extern exit
import exit msvcrt.dll


; our data is declared here (the variables needed by our program)
segment data use32 class=data
    a dw 1a2bh
    b db 3ch, 4dh
    c dd 1a2b3c4dh
    
; our code starts here
segment code use32 class=code
    start:
        mov eax, [a + 1]
        push dword 0
        call [exit]