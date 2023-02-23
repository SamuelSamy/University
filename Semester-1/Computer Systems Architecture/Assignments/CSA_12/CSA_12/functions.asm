bits 32


global _compute_maxim


segment data public data use32
segment code public code use32

; int compute_maxim(int*, int);
_compute_maxim:
	
	push ebp
	mov ebp, esp
	; pushad  ; save the registers on the stack

	mov esi, [ebp + 8]   ; address of the first element
	mov ecx, [ebp + 12]  ; the len of the array
	dec ecx

	mov ebx, 10000000000000000000000000000000b  ; we will store the resuult in ebx (ebx is INT_MIN)

	_loop:
		lodsd  ; eax = [esi], esi += 4

		cmp eax, ebx   
		jle skip_move  ; eax <= ebx
		mov ebx, eax  ; ebx = eax

		skip_move:
	loop _loop

	; popad  ; get the registers from the stack
	mov eax, ebx
	mov esp, ebp
	pop ebp
	ret

