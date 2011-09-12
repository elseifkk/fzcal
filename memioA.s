section .data align=16
str_buf dd 0
;;; 
section .text align=16
global mcp_
global dw2str_
	
%assign arg1 08
%assign arg2 12
%assign arg3 16
	
mcp_:	
	push ebp
	mov ebp, esp
	push edi
	push esi
	;;
	mov edi, [ebp+arg1]
	mov esi, [ebp+arg2]
	mov ecx, [ebp+arg3]
	;;
	mov eax, ecx		; eax = n
	shr eax, 2		; eax = n/4
	mov edx, ecx		; edx = n
	mov ecx, eax		; ecx = n/4
	shl eax, 2		; eax = 4(n/4)
	sub edx, eax		; edx = n-4(n/4)
	;;
	cld
  	rep movsd
	;;
	mov ecx, edx
	rep movsb
	;;
	pop esi
	pop edi
	pop ebp
	ret

	;; 
%macro div10m 0
	push eax
;;-------------------; x*0.11b
	shr eax, 1   ; x*0.1b
	mov ebx, eax ;
	shr ebx, 1   ; x*0.01b
	jz %%.next
	add eax, ebx ; x*0.11b
;;-- -- -- -- --- ---;
	mov ebx, eax ; x*0.11b
	shr ebx, 4   ; x*0.000011b
	jz %%.next
	add eax, ebx ; x*0.110011b
;;- - - - - -  - - - ;
	mov ebx, eax ; x*0.110011b
	shr ebx, 8   ; x*0.00000000110011b
	jz %%.next
	add eax, ebx ; x*0.11001100110011b
;;- - - - - -  - - - ;
	mov ebx, eax ; x*0.11001100110011b
	shr ebx, 16  ; x*0.000000000000000011001100110011b
	add eax, ebx ; x*0.110011001100110011001100110011b
;;- - - - - -  - - - ;
%%.next:             ;
	shr eax, 3   ; x*0.000110011001100110011001100110011b (34 bit)
;------------------------------;
	pop edx	               ; this method evaluates [a/10] or [a/10]-1. we have to
	mov ecx, eax           ; examine which one is obtained by calculating remainder of them
	lea eax, [eax+eax*4]
	add eax, eax
	sub edx, eax            ; r=a-10*q
	mov eax, edx            ; eax=r
	sub eax, 0x0a           ; eax=r-10
	jb %%.exit              ; [a/10] obtained since remainder is less than 10
;;==============================;
	inc ecx			; ecx=q+1
	mov edx, eax		;
;;==============================;
%%.exit:          		;
	mov eax, ecx		;
%endm

dw2str_:
%assign D2STR_BUFFER_SIZE 16
	push ebp
	mov ebp, esp
	;; 
	push edi
	push esi
	push ebx
	;; 
	mov edi, str_buf                ; edi = ptr str_buf
	mov esi, edi                    ; esi = ptr str_buf
	mov eax, [ebp+arg1]             ;
	and eax, 0x7FFFFFFF		; eax = positive i
	;; 
	add edi, D2STR_BUFFER_SIZE-1
	mov [edi], byte 30h
	or eax, eax         
	jz .cpbuf
	inc edi             
	;; 
.L10:
       	div10m
	add edx, 0x30       
	dec edi             
	mov [edi], BYTE dl  
.L20:               
	or eax, eax         
	jnz .L10          
.cpbuf:
	mov ecx, esi                    ;
	sub ecx, edi                    ; get length of digits
	add ecx, D2STR_BUFFER_SIZE        ; set up for copying buffer
	mov esi, edi
	mov edi, [ebp+arg2]		; edi = ptr str
	mov edx, [ebp+arg1]		; edx = a
	mov eax, ecx			; return code = len str
	cmp dword edx, 0
	jl .putminus
.cont:	
	rep movsb
	;; 
.exit
	pop ebx
	pop esi
	pop edi
	pop ebp
	ret
	;; 
.putminus:
	mov byte [edi], "-"
	inc edi
	inc eax
	jmp .cont
