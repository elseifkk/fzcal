;/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; *   Copyright (C) 2011-2012 by Kazuaki Kumagai                            *
; *   elseifkk@users.sf.net                                                 *
; *                                                                         *
; *   This program is free software; you can redistribute it and/or modify  *
; *   it under the terms of the GNU General Public License as published by  *
; *   the Free Software Foundation; either version 2 of the License, or     *
; *   (at your option) any later version.                                   *
; *                                                                         *
; *   This program is distributed in the hope that it will be useful,       *
; *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
; *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
; *   GNU General Public License for more details.                          *
; *                                                                         *
; *   You should have received a copy of the GNU General Public License     *
; *   along with this program; if not, write to the                         *
; *   Free Software Foundation, Inc.,                                       *
; *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
%undef _REL_
%ifdef _USE32_
	BITS 32
%else
	BITS 64
	%ifdef _DYNAMIC_
		%define _REL_
		%define ADDR(x) rel (x) wrt ..gotpcrel
	%else
		%define ADDR(x) (x)
	%endif
%endif

section .bss align=16
str_buf resq 0

;;;
section .text align=16
global mcp_
global mcle_
%ifdef _USE32_
global dw2str_
%else
global qw2str_
%endif

%macro start_proc 0
%ifdef _USE32_
%assign arg1 08
%assign arg2 12
%assign arg3 16
	push ebp
	mov ebp, esp
	push edi
	push esi
	push ebx
%else
	push rbp
	mov rbp, rsp
%endif
%endm

%macro end_proc 0
%ifdef _USE32_
	pop ebx
	pop esi
	pop edi
	pop ebp
	ret
%else
	pop rbp
	ret
%endif
%endm

mcp_:
	start_proc
%ifdef _USE32_
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
%else
 	mov rax, rdx ; n
 	shr rax, 3   ; n/8
 	mov rcx, rax ; n/8
 	shl rcx, 3   ; (n/8)*8
 	sub rdx, rcx ; n-(n/8)*8
 	mov rcx, rax
 	cld
 	rep movsq
 	mov rcx, rdx
 	rep movsb
	;;
 	mov rax, rdi
%endif
	end_proc

mcle_:
	start_proc
%ifdef _USE32_
	mov edi, [ebp+arg1]
	mov ecx, [ebp+arg2]
	;;
	mov eax, ecx		; eax = n
	shr eax, 2		; eax = n/4
	mov edx, ecx		; edx = n
	mov ecx, eax		; ecx = n/4
	shl eax, 2		; eax = 4(n/4)
	sub edx, eax		; edx = n-4(n/4)
	;;
	cld
	xor eax, eax
  	rep stosd
	;;
	mov ecx, edx
	rep stosb
%else
 	mov rax, rsi ; n
 	shr rax, 3   ; n/8
 	mov rcx, rax ; n/8
 	shl rcx, 3   ; (n/8)*8
 	sub rdx, rcx ; n-(n/8)*8
 	mov rcx, rax
 	cld
	xor rax, rax
 	rep stosq
 	mov rcx, rdx
 	rep stosb
	;;
 	mov rax, rdi
%endif
	end_proc

;;;
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
;-------------------------------;
	pop edx	                ; this method evaluates [a/10] or [a/10]-1. we have to
	mov ecx, eax            ; examine which one is obtained by calculating remainder of them
	lea eax, [eax+eax*4]	;
	add eax, eax		;
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

%ifdef _USE32_
dw2str_:
;;      integer function dw2str(dw,pstr)
%assign D2STR_BUFFER_SIZE 11
 	start_proc
	;;
	lea edi, [str_buf]                    ; edi = ptr str_buf
	mov esi, edi                          ; esi = ptr str_buf
	mov eax, [ebp+arg1]
	and eax, 0x7FFFFFFF		      ; eax = positive i
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
	add ecx, D2STR_BUFFER_SIZE      ; set up for copying buffer
	mov esi, edi
	mov edi, [ebp+arg2]		; edi = ptr str
 	mov edx, [ebp+arg1]		; edx = dw
	mov eax, ecx			; return code = len str
	cmp edx, 0
	jl .putminus
.cont:
	rep movsb
	;;
.exit
 	end_proc
	;;
.putminus:
	mov byte [edi], "-"
	inc edi
	inc eax
	jmp .cont
%else
qw2str_:
%assign D2STR_BUFFER_SIZE 21
	start_proc
	;;
 	mov rax, rdi			; rax  = a
	and rax, 0x7FFFFFFFFFFFFFFF	; eax = positive i
	push rax			; save dw
	push rsi			; save pstr
	;;
	lea rdi, [ADDR(str_buf)] 	; rdi = ptr str_buf
	mov rsi, rdi                    ; esi = ptr str_buf
	;;
	add rdi, D2STR_BUFFER_SIZE-1
	mov [rdi], byte 30h
	or rax, rax
	jz .cpbuf
	inc rdi
        mov r9, 10
	;;
.L10:
	xor rdx, rdx
       	div r9
	add rdx, 0x30
	dec rdi
	mov [rdi], BYTE dl
.L20:
	or rax, rax
	jnz .L10
.cpbuf:
	mov rcx, rsi                    ;
	sub rcx, rdi                    ; get length of digits
	add rcx, D2STR_BUFFER_SIZE      ; set up for copying buffer
	mov rsi, rdi
	pop rdi			        ; restore pstr
	pop rdx			        ; restore dw
	mov rax, rcx			; return code = len str
	cmp rdx, 0
	jl .putminus
.cont:
	rep movsb
	;;
.exit
	end_proc
	;;
.putminus:
	mov byte [rdi], "-"
	inc rdi
	inc rax
	jmp .cont

%endif
