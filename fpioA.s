%ifdef _USE32_
BITS 32
%else
BITS 64
%endif

; parameters to set format for fptoa proc
FP_LESSTHAN equ 0x01
FP_EQUALTO  equ 0x40
FP2A_MAXIMUM_DIGIT               equ 18
FP2A_MINIMUM_DIGIT               equ 3
FP2A_TRIM_TRAILING_ZEROS         equ  0x000100
FP2A_ALLOW_INTEGER_EXPRESSION    equ  0x000200
FP2A_ALLOW_ORDINARY_EXPRESSION   equ  0x000400
FP2A_FORCE_NOT_SHOW_EXPSIGN      equ  0x000800
FP2A_FORCE_SHOW_SIGN             equ  0x001000
FP2A_KEEP_LEADING_ZEROS          equ  0x002000 ; for exponential digits
FP2A_ALLOW_ENGINEERING_NOTATION  equ  0x004000
FP2A_INPUT_REAL10                equ  0x008000
FP2A_SUPRESS_E0                  equ  0x010000
FP2A_NULLTERM                    equ  0x020000
FP2A_TRIM_ALL_TRAILING_ZEROS     equ  0x040000 ; 1.0 become 1.
FP2A_ADJUSTR                     equ  0x080000 ; LSB 4bit represents digit.
FP2A_INPUT_REAL4		 equ  0x100000
FP2A_DEFAULT equ \
8 |\
FP2A_ALLOW_ORDINARY_EXPRESSION | \
FP2A_FORCE_NOT_SHOW_EXPSIGN | \
FP2A_SUPRESS_E0 | \
FP2A_TRIM_TRAILING_ZEROS

section .data
; I'd rather this was local to the procedure, but masm doesn't do
; local arrays correctly.
%ifdef _USE32_
pstr dd 0
%else
pstr dq 0
%endif

szTemp  times 20  db 0
real8   times 8 db 0
format  dd 0
	
%ifdef _USE32_
iExp	dd 0
%else
iExp	dq 0
%endif
	
nDigit 	dd 0
prefix	dd 0
SI_PREFIX db 'y','z','a','f','p','n','u','m' ; 9
          db 0                               ; 1
          db 'k','M','G','T','P','E','Z','Y' ; 9
real10 dt 0.0
;;;;;; 
;;;ten1  dt 1.0e1 			; 0x 4002 a000 0000 0000 0000
;;;ten2  dt 1.0e2
;;;ten3  dt 1.0e3
;;;ten4  dt 1.0e4
;;;ten5  dt 1.0e5
;;;ten6  dt 1.0e6
;;;ten7  dt 1.0e7
;;;ten8  dt 1.0e8
;;;ten9 dt 1.0e9
;;;ten10 dt 1.0e10
;;;ten11 dt 1.0e11
;;;ten12 dt 1.0e12
;;;ten13 dt 1.0e13
;;;ten14 dt 1.0e14
;;;ten15 dt 1.0e15
;;;ten16 dt 1.0e16
;;;ten17 dt 1.0e17
;;;ten18 dt 1.0e18
;;;ten19 dt 1.0e19
;;;ten20 dt 1.0e20
;;;;;;
ten_1 dt 1.0e1
ten_2 dt 1.0e2
ten_3 dt 1.0e3
ten_4 dt 1.0e4
ten_5 dt 1.0e5
ten_6 dt 1.0e6
ten_7 dt 1.0e7
ten_8 dt 1.0e8
ten_9 dt 1.0e9
ten_10 dt 1.0e10
ten_11 dt 1.0e11
ten_12 dt 1.0e12
ten_13 dt 1.0e13
ten_14 dt 1.0e14
ten_15 dt 1.0e15
;;; 
ten_16 dt 1.0e16
ten_32 dt 1.0e32
ten_48 dt 1.0e48
ten_64 dt 1.0e64
ten_80 dt 1.0e80
ten_96 dt 1.0e96
ten_112 dt 1.0e112
ten_128 dt 1.0e128
ten_144 dt 1.0e144
ten_160 dt 1.0e160
ten_176 dt 1.0e176
ten_192 dt 1.0e192
ten_208 dt 1.0e208
ten_224 dt 1.0e224
ten_240 dt 1.0e240
;;; 
ten_256 dt 1.0e256
; The remaining exponents are only necessary if we decide to support
; 10-byte doubles.  FloatToStr and StrToFloat only support 8-byte,
; but PowerOf10 doesn't care, so we'll include them.
ten_512 dt 1.0e512
ten_768 dt 1.0e768
ten_1024 dt 1.0e1024
ten_1280 dt 1.0e1280
ten_1536 dt 1.0e1536
ten_1792 dt 1.0e1792
ten_2048 dt 1.0e2048
ten_2304 dt 1.0e2304
ten_2560 dt 1.0e2560
ten_2816 dt 1.0e2816
ten_3072 dt 1.0e3072
ten_3328 dt 1.0e3328
ten_3584 dt 1.0e3584
ten_4096 dt 1.0e4096
ten_4352 dt 1.0e4352
ten_4608 dt 1.0e4608
ten_4864 dt 1.0e4864
	
;;;
%ifndef _USE32_
section .init
	mov qword [rel pstr wrt ..gotpcrel], 0
	mov qword [rel szTemp wrt ..gotpcrel], 0
	mov qword [rel szTemp+8 wrt ..gotpcrel], 0
	mov dword [rel szTemp+16 wrt ..gotpcrel], 0
	mov qword [rel iExp wrt ..gotpcrel], 0
	mov dword [rel nDigit wrt ..gotpcrel], 0
	mov dword [rel prefix wrt ..gotpcrel], 0
	mov byte [rel SI_PREFIX wrt ..gotpcrel], 'y'
	mov byte [rel SI_PREFIX+1 wrt ..gotpcrel], 'z'
	mov byte [rel SI_PREFIX+2 wrt ..gotpcrel], 'a'
	mov byte [rel SI_PREFIX+3 wrt ..gotpcrel], 'f'
	mov byte [rel SI_PREFIX+4 wrt ..gotpcrel], 'p'
	mov byte [rel SI_PREFIX+5 wrt ..gotpcrel], 'n'
	mov byte [rel SI_PREFIX+6 wrt ..gotpcrel], 'u'
	mov byte [rel SI_PREFIX+7 wrt ..gotpcrel], 'm'
	mov byte [rel SI_PREFIX+8 wrt ..gotpcrel], 0
	mov byte [rel SI_PREFIX+9 wrt ..gotpcrel], 'k'
	mov byte [rel SI_PREFIX+10 wrt ..gotpcrel], 'M'
	mov byte [rel SI_PREFIX+11 wrt ..gotpcrel], 'G'
	mov byte [rel SI_PREFIX+12 wrt ..gotpcrel], 'T'
	mov byte [rel SI_PREFIX+13 wrt ..gotpcrel], 'P'
	mov byte [rel SI_PREFIX+14 wrt ..gotpcrel], 'E'
	mov byte [rel SI_PREFIX+15 wrt ..gotpcrel], 'Z'
	;;
%endif
;;; 
section .text align=16

;;; 
global f2str_
	
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
	;; 
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

%ifndef _USE32_
_init_tab:
	push rdx
%macro put_real10 4
	lea rdx, [rel %1 wrt ..gotpcrel]
	mov dword [rdx], %2
	mov dword [rdx+4], %3
	mov word [rdx+8], %4
%endm
	put_real10 ten_1,  0x00000000, 0xA0000000, 0x4002
	put_real10 ten_2,  0x00000000, 0xC8000000, 0x4005
	put_real10 ten_3,  0x00000000, 0xFA000000, 0x4008
	put_real10 ten_4,  0x00000000, 0x9C400000, 0x400C
	put_real10 ten_5,  0x00000000, 0xC3500000, 0x400F
	put_real10 ten_6,  0x00000000, 0xF4240000, 0x4012
	put_real10 ten_7,  0x00000000, 0x98968000, 0x4016
	put_real10 ten_8,  0x00000000, 0xBEBC2000, 0x4019
	put_real10 ten_9,  0x00000000, 0xEE6B2800, 0x401C
	put_real10 ten_10, 0x00000000, 0x9502F900, 0x4020
	put_real10 ten_11, 0x00000000, 0xBA43B740, 0x4023
	put_real10 ten_12, 0x00000000, 0xE8D4A510, 0x4026
	put_real10 ten_13, 0x00000000, 0x9184E72A, 0x402A
	put_real10 ten_14, 0x80000000, 0xB5E620F4, 0x402D
	put_real10 ten_15, 0xA0000000, 0xE35FA931, 0x4030
	put_real10 ten_16, 0x04000000, 0x8E1BC9BF, 0x4034
	;;
	put_real10 ten_32,  0x2B70B59E, 0x9DC5ADA8, 0x4069
	put_real10 ten_48,  0x0E4395D7, 0xAF298D05, 0x409E
	put_real10 ten_64,  0xFFCFA6D5, 0xC2781F49, 0x40D3
	put_real10 ten_80,  0x87DAF7FC, 0xD7E77A8F, 0x4108
	put_real10 ten_96,  0xC59B14A3, 0xEFB3AB16, 0x413D
	put_real10 ten_112, 0x9923329E, 0x850FADC0, 0x4173
	put_real10 ten_128, 0x80E98CE0, 0x93BA47C9, 0x41A8
	put_real10 ten_144, 0xA8D3A6E7, 0xA402B9C5, 0x41DD
	put_real10 ten_160, 0x7FE617AA, 0xB616A12B, 0x4212
	put_real10 ten_176, 0x859BBF93, 0xCA28A291, 0x4247
	put_real10 ten_192, 0x3927556B, 0xE070F78D, 0x427C
	put_real10 ten_208, 0x37826146, 0xF92E0C35, 0x42B1
	put_real10 ten_224, 0xE33CC930, 0x8A5296FF, 0x42E7
	put_real10 ten_240, 0xD6BF1766, 0x9991A6F3, 0x431C
	put_real10 ten_256, 0x9DF9DE8E, 0xAA7EEBFB, 0x4351
	;;
	put_real10 ten_768,  0xCD00A68C, 0x973F9CA8, 0x49F6
	put_real10 ten_1024, 0x81750C17, 0xC9767586, 0x4D48
	put_real10 ten_1280, 0xEB856ECB, 0x862C8C0E, 0x509B
	put_real10 ten_1536, 0x3993A7E4, 0xB2B8353B, 0x53ED
	put_real10 ten_1792, 0x924AB88C, 0xEE0DDD84, 0x573F
	put_real10 ten_2048, 0xC53D5DE5, 0x9E8B3B5D, 0x5A92
	put_real10 ten_2304, 0x41F4806F, 0xD32E2032, 0x5DE4
	put_real10 ten_2560, 0x20A1F0A6, 0x8CA554C0, 0x6137
	put_real10 ten_2816, 0x9BD977CC, 0xBB570A9A, 0x6489
	put_real10 ten_3072, 0xD88B5A8B, 0xF9895D25, 0x67DB
	put_real10 ten_3328, 0x5699FE45, 0xA630EF7D, 0x6B2E
	put_real10 ten_3584, 0xBF27F3F8, 0xDD5DC8A2, 0x6E80
	put_real10 ten_4096, 0x8A20979B, 0xC4605202, 0x7525
	put_real10 ten_4352, 0x7BE11CB4, 0x82C952E3, 0x7878
	put_real10 ten_4608, 0x6ED559F0, 0xAE351162, 0x7BCA
	put_real10 ten_4864, 0xB9146D6D, 0xE80B387F, 0x7F1C

	pop rdx
	ret
%endif
; Multiply a floating point value by an integral power of 10.
;
; Entry: EAX = power of 10, -4932..4932.
; ST(0) = value to be multiplied
;
; Exit: ST(0) = value x 10^eax
_PowerOf10:
	call _init_tab
	
	mov ecx, eax
	cmp eax, 0
	jg .L1
	neg eax
.L1
	fld1
	mov dl, al
	and edx, 0x0f
	jz .L2 
	lea edx, [edx+edx*4]
%ifdef _USE32_
	fld tword [ten_1+edx*2-10]
%else
	lea rbx, [rel ten_1 wrt ..gotpcrel]
	fld tword [rbx]
%endif
	fmulp st1, st0
.L2
	mov dl, al
	shr dl, 4
	and edx, 0x0f
	jz .L3
	lea edx, [edx+edx*4]
%ifdef _USE32_
	fld tword [ten_16+edx*2-10]
%else
	fld tword [rel ten_16+rdx*2-10 wrt ..gotpcrel]
%endif
	fmulp st1, st0
.L3
	mov dl, ah
	and edx, 0x1f
	jz .L4
	lea edx, [edx+edx*4]
%ifdef _USE32_
	fld tword [ten_256+edx*2-10]
%else
	fld tword [rel ten_256+rdx*2-10 wrt ..gotpcrel]
%endif
	fmulp st1, st0
.L4
	cmp ecx, 0
	jge .L5
	fdivp st1, st0
	ret
.L5	
	fmulp st1, st0
	ret

; Convert a floating point register to ASCII.  For internal use.
; The result always has exactly 18 digits, with zero padding on the
; left if required.
;
; Entry: ST(0) = a number to convert, 0 <= ST(0) < 1E19.
;  szTemp = an 18-character buffer.
;
; Exit:  szTemp = the converted result.
_FloatToBCD:	

  ; The fbstp instruction converts the top of the stack to a
  ; packed BCD form in ten bytes, with two digits per byte.  The top
  ; byte has the sign, which we ignore.

%ifdef _USE32_
	push edi
	sub esp, 10
	fbstp [esp]
%else
	push rdi
	sub rsp, 10
	fbstp [rsp]
%endif

  ; Now we need to unpack the BCD to ASCII.

%ifdef _USE32_
	lea esi, [esp+8]
	mov edi, szTemp
%else
	lea rsi, [rsp+8]
	lea rdi, [rel szTemp wrt ..gotpcrel]
%endif
	mov ecx, 9
.do
%ifdef _USE32_
	mov al, [esi]  ; xxxx xxxx AAAA BBBB
	dec esi
%else
	mov al, [rsi]  ; xxxx xxxx AAAA BBBB
	dec rsi	
%endif
	rol ax, 12  ; BBBB xxxx xxxx AAAA
	rol ah, 4  ; xxxx BBBB xxxx AAAA
	and ax, 0x0f0f  ; 0000 BBBB 0000 AAAA
	add ax, 0x3030  ; 3B3A
%ifdef _USE32_
	mov [edi], ax
	add edi, 2
%else
	mov [rdi], ax
	add rdi, 2	
%endif
	dec ecx
	jnz .do
%ifdef _USE32_
	add esp, 10
	pop edi
%else
	add rsp, 10
	pop rdi
%endif
	ret

; ; Convert a double precision number to a string.
; ;
; ; Entry: fpin = 8-byte double to convert
; ;  szDbl = character buffer
; ;
; ; Exit:  szDbl = converted value
; ;
; ; szDbl should be at least 19 bytes long.
; ;

; FloatToStr PROC stdcall public USES esi edi ebx,
;   fpin: dword, ; PTR REAL10,
;   szDbl: dword, ; PTR CHAR,
;   format: DWORD

;     LOCAL iExp: DWORD
;     LOCAL stat: WORD
;     LOCAL mystat: WORD
; local nDigit: DWORD ; number of significant digit
; local prefix: BYTE ; for engineering notation
f2str_:	
	start_proc
	;;
%ifdef _USE32_
	mov esi, [ebp+arg1] ; fpin	
	mov edi, [ebp+arg2] ; szDbl
	mov eax, [ebp+arg3] ; opt
	mov pstr, edi
%else
	;; rdi rsi rdx
	mov rax, rdx		; opt
	mov rdx, rdi		; fpin
	mov rdi, rsi		; pstr
	mov rsi, rdx		; fpin
%endif
	;; 
	or eax, eax		
	jnz .set_format
%ifdef _USE32_
	mov dword [format], FP2A_DEFAULT
%else
	mov dword [rel format wrt ..gotpcrel], FP2A_DEFAULT
%endif
	jmp .begin

.set_format:
%ifdef _USE32_
	mov [format], eax
%else
	mov [rel format wrt ..gotpcrel], eax
%endif
	test eax, FP2A_INPUT_REAL4
	jz .begin
%ifdef _USE32_
	fld dword [esi]
	mov esi, real8
	fstp qword [esi]
%else
	fld dword [rsi]
	lea rsi, [rel real8 wrt ..gotpcrel]
	fstp qword [rsi]
%endif
	
.begin:	
	xor eax, eax
%ifdef _USE32_
	cmp dword [esi], 0
%else
	cmp dword [rsi], 0
%endif
	jne .L1
%ifdef _USE32_
	test dword [format], FP2A_INPUT_REAL10
%else
	test dword [rel format wrt ..gotpcrel], FP2A_INPUT_REAL10
%endif
	jne .L2
%ifdef _USE32_
	test dword [esi+4], 0x7FFFFFFF
%else
	test dword [rsi+4], 0x7FFFFFFF
%endif
	jne .L3
	inc eax
.L3:
	jmp .L4
.L2:
%ifdef _USE32_
	cmp dword [esi+4], 0
%else
	cmp dword [rsi+4], 0
%endif
	jne .L5
%ifdef _USE32_
	cmp word [esi+8], 0
%else
	cmp word [rsi+8], 0
%endif
	jne .L5
	inc eax
.L1:	
.L4:	
.L5:
%ifdef _USE32_
	cmp dword [esi], 0
%else
	cmp dword [rsi], 0
%endif
	jne .L6
%ifdef _USE32_
	cmp dword [esi+4], 0
%else
	cmp dword [rsi+4], 0	
%endif
	jne .L6
%ifdef _USE32_
	test dword [format], FP2A_INPUT_REAL10
%else
	test dword [rel format wrt ..gotpcrel], FP2A_INPUT_REAL10	
%endif
	je .L7
%ifdef _USE32_
	cmp word [esi+8], 0
%else
	cmp word [rsi+8], 0
%endif
	je .L6
	inc eax
.L7:
	inc eax
	jmp .L6
.L6	
	or eax, eax
	jz near .L8
%ifdef _USE32_
	test dword [format], FP2A_FORCE_SHOW_SIGN
%else
	test dword [rel format wrt ..gotpcrel], FP2A_FORCE_SHOW_SIGN
%endif	
	jz .L9
%ifdef _USE32_
	mov byte [edi], "+"
	inc edi
%else
	mov byte [rdi], "+"
	inc rdi
%endif

.L9:
%ifdef _USE32_
	mov byte [edi], 0x30
	inc edi
	test dword [format], FP2A_ALLOW_ORDINARY_EXPRESSION
%else
	mov byte [rdi], 0x30
	inc rdi
	test dword [rel format wrt ..gotpcrel], FP2A_ALLOW_ORDINARY_EXPRESSION	
%endif
	jnz .L10
%ifdef _USE32_
	test dword [format], FP2A_TRIM_TRAILING_ZEROS
%else
	test dword [rel format wrt ..gotpcrel], FP2A_TRIM_TRAILING_ZEROS
%endif
	jz .L11
%ifdef _USE32_
        mov word [edi], 0x302e
        add edi, 2
%else
        mov word [rdi], 0x302e
        add rdi, 2	
%endif
        add eax, 2
	jmp .L12
.L11:	
; no check for format length
%ifdef _USE32_
        mov byte [edi], 0x2E
        inc edi
        mov ecx, dword [format]
%elseif
        mov byte [rdi], 0x2E
        inc rdi
        mov ecx, dword [rel format wrt ..gotpcrel]
%endif
        mov edx, ecx
        and ecx, 0x0FF
        dec ecx
        mov al, 0x30
        rep stosb
.L12
%ifdef _USE32_
	test dword [format], FP2A_ALLOW_ORDINARY_EXPRESSION | FP2A_SUPRESS_E0
%else
	test dword [rel format wrt ..gotpcrel], FP2A_ALLOW_ORDINARY_EXPRESSION | FP2A_SUPRESS_E0	
%endif
	jnz .L10
%ifdef _USE32_
        mov byte [edi], 0x65 ; e
        inc edi
	test dword [format], FP2A_FORCE_NOT_SHOW_EXPSIGN
%else
        mov byte [rdi], 0x65 ; e
        inc rdi
	test dword [rel format wrt ..gotpcrel], FP2A_FORCE_NOT_SHOW_EXPSIGN
%endif
	jnz .L11a
%ifdef _USE32_
        mov byte [edi], "+"
        inc edi
%else
        mov byte [rdi], "+"
        inc rdi
%endif
	
.L11a:
%ifdef _USE32_
        mov byte [edi], 0x30
        inc edi
%else
        mov byte [rdi], 0x30
        inc rdi	
%endif
.L10:
%ifdef _USE32_
	test dword [format], FP2A_NULLTERM
%else
	test dword [rel format wrt ..gotpcrel], FP2A_NULLTERM	
%endif
	jz .L11b
        xor al, al
	jmp .L12a

.L11b:	
        mov al, 0x20
.L12a
%ifdef _USE32_
	mov byte [edi], al
	mov eax, edi
	sub eax, dword [pstr]
%else
	mov byte [rdi], al
	mov rax, rdi
	sub rax, qword [rel pstr wrt ..gotpcrel]
%endif
.return:
	end_proc

.L8:	
; Check for a negative number.
	xor eax, eax
%ifdef _USE32_
	test dword [format], FP2A_INPUT_REAL10
%else
	test dword [rel format wrt ..gotpcrel], FP2A_INPUT_REAL10	
%endif
	jz .L13
%ifdef _USE32_
	cmp word [esi+8], 0
%else
	cmp word [rsi+8], 0	
%endif
	jge .L14
%ifdef _USE32_
        lea eax, [esi+9]
        and byte [eax], 0x07f ; change to positive
%else
        lea rax, [rsi+9]	
        and byte [rax], 0x07f
%endif

.L13:
%ifdef _USE32_
	cmp word [esi+6], 0
%else
	cmp word [rsi+6], 0	
%endif
	jge .L14
%ifdef _USE32_
        lea eax, [esi+7]
        and byte [eax], 0x07f ; change to positive
%else
        lea rax, [rsi+7]
        and byte [rax], 0x07f
%endif
.L14:
%ifdef _USE32_
	or eax, eax		;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<???
%else
	or rax, rax
%endif
	jz .L15
%ifdef _USE32_
	mov byte [edi], "-"  ; store a minus sign
	inc edi
%else
	mov byte [rdi], "-"  ; store a minus sign
	inc rdi	
%endif
	jmp .L16
.L15
%ifdef _USE32_
	test dword [format], FP2A_FORCE_SHOW_SIGN
%else
	test dword [rel format wrt ..gotpcrel], FP2A_FORCE_SHOW_SIGN
%endif
	jz .L16
%ifdef _USE32_
	mov byte [edi], "+"
	inc edi
%else
	mov byte [rdi], "+"
	inc rdi
%endif
.L16

; ; Initialize the floating point unit and load our value onto the stack.
; ; eax != 0 if src have been forced to be positive
;     fclex
;     fstcw [stat]
;     mov [mystat], 027fh
;     fldcw [mystat]

%ifdef _USE32_
	test dword [format], FP2A_INPUT_REAL10
%else
	test dword [rel format wrt ..gotpcrel], FP2A_INPUT_REAL10
%endif
	jz .L20
	;; Sep 12, 2011
	;; gfortran's real*10 does not have 63rd bit set.
	;; for non-zero value, it must be set.
%ifdef _USE32_
	fld tword [esi]
	mov esi, real10
	fstp tword [esi]
	or dword [esi+4],0x80000000
	fld tword [esi]
%else
	fld tword [rsi]
	lea rsi, [rel real10 wrt ..gotpcrel]
	fstp tword [rsi]
	or dword [rsi+4],0x80000000
	fld tword [rsi]	
%endif
	jmp .L21
	.L20
%ifdef _USE32_
	fld qword [esi]
%else
	fld qword [rsi]
%endif
	.L21
	fld st0
%ifdef _USE32_
	or eax, eax
%else
	or rax, rax
%endif 
	jz .L22
; recover minus sign; source never be changed!!!
%ifdef _USE32_
	or byte [eax], 0x80
%else
	or byte [rax], 0x80	
%endif
.L22:	
; ; Compute the closest power of 10 below the number.  We can't get an
; ; exact value because of rounding.  We could get close by adding in
; ; log10(mantissa), but it still wouldn't be exact.  Since we'll have to
; ; check the result anyway, it's silly to waste cycles worrying about
; ; the mantissa.
; ;
; ; The exponent is basically log2(fpin).  Those of you who remember
; ; algebra realize that log2(fpin) x log10(2) = log10(fpin), which is
; ; what we want.

	fxtract   ; ST=> mantissa, exponent, fpin
	fstp st0   ; drop the mantissa
	fldlg2   ; push log10(2)
	fmulp st1, st0  ; ST = log10(fpin), fpin
%ifdef _USE32_
	fistp dword [iExp]  ; ST = fpin
%else
	fistp dword [rel iExp wrt ..gotpcrel]
%endif
; ; An 8-byte double can carry almost 16 digits of precision.  Actually, it's
; ; 15.9 digits, so some numbers close to 1E17 will be wrong in the bottom
; ; digit.  If this is a concern, change the '16' to a '15'.
; ;
; ; A 10-byte double can carry almost 19 digits, but fbstp only stores the
; ; guaranteed 18.  If you're doing 10-byte doubles, change the '16' to '18'.

%ifdef _USE32_
	test dword [format], FP2A_ALLOW_INTEGER_EXPRESSION
%else
	test dword [rel format wrt ..gotpcrel], FP2A_ALLOW_INTEGER_EXPRESSION
%endif
	je .end_aie
%ifdef _USE32_
	cmp dword [iExp], 18
%else
	cmp dword [rel iExp wrt ..gotpcrel], 18
%endif
	jae .end_cmp_iexp_18
	fld st0   ; ST = fpin, fpin
	frndint   ; ST = int(fpin), fpin
	fcomp st1  ; ST = fpin, status set
	fstsw ax
	test ah, FP_EQUALTO
	jz .end_cmp_eq

; ; We have an integer!  Lucky day.  Go convert it into a temp buffer.

	call _FloatToBCD

	mov eax, 17
%ifdef _USE32_
	mov ecx, [iExp]
%else
	mov ecx, [rel iExp wrt ..gotpcrel]
%endif
	sub eax, ecx
	inc ecx
%ifdef _USE32_
	lea esi, [szTemp+eax]
%else
	lea rsi, [rel szTemp+rax wrt ..gotpcrel]	
%endif
; ; The off-by-one order of magnitude problem below can hit us here.
; ; We just trim off the possible leading zero.
%ifdef _USE32_
	cmp byte [esi], 0
%else
	cmp byte [rsi], 0
%endif
	jnz .L25
%ifdef _USE32_
	inc esi
%else
	inc rsi
%endif
	dec ecx
.L25
; ; Copy the rest of the converted BCD value to our buffer.

	rep movsb
	jmp .ftsExit

.end_cmp_eq:	

.end_cmp_iexp_18:	

.end_aie:
; ; format[0-7] = NUMBER_OF_DIGIT
; ; We use the format [-]d.ddddddE+ddd.  That means we only need a maximum
; ; of NUMBER_OF_DIGIT decimal places.  Let's have fbstp do our rounding for us.
%ifdef _USE32_
	mov eax, dword [format]      ; X07h
%else
	mov eax, dword [rel format wrt ..gotpcrel]
%endif
	and eax, 0x0FF
	cmp al, FP2A_MAXIMUM_DIGIT
	ja near .ftsExit
	cmp al, FP2A_MINIMUM_DIGIT
	jb near .ftsExit
	mov esi, eax
	sub esi, 2
%ifdef _USE32_
	mov dword [nDigit], eax
%else
	mov dword [rel nDigit wrt ..gotpcrel], eax
%endif
	add esi, esi
	jz .L30
	lea esi, [esi+esi*4]
%ifdef _USE32_
	mov ebx, ten_1
	add ebx, esi
%else
	lea rbx, [rel ten_1 wrt ..gotpcrel]
	add rbx, rsi	
%endif
.L30:	
	dec eax
%ifdef _USE32_
	sub eax, [iExp]       ; adjust exponent to NUMBER_OF_DIGIT
%else
	sub eax, [rel iExp wrt ..gotpcrel]
%endif
	call _PowerOf10

; ; Either we have exactly NUMBER_OF_DIGIT digits,
; ; or we have exactly NUMBER_OF_DIGIT - 1 digits.  We can
; ; detect that condition and adjust now.
%ifdef _USE32_
	fld tword [ebx]
%else
	fld tword [rbx]
%endif
	fcomp st1
;     ; x0xxxx00 means top of stack < tenX
;     ; x0xxxx01 means top of stack > tenX
;     ; x1xxxx00 means top of stack = tenX
	fstsw ax
	test ah, 0x43
	jnz .L40
%ifdef _USE32_
	fld tword [ten_1]
%else
	fld tword [rel ten_1 wrt ..gotpcrel]
%endif
	fmulp st1, st0
%ifdef _USE32_
	dec dword [iExp]	
%else
	dec dword [rel iExp wrt ..gotpcrel]
%endif

.L40:	
; ; Go convert to BCD.
	call _FloatToBCD
%ifdef _USE32_
	lea esi, [szTemp+18] ; point to converted buffer
	sub esi, [nDigit]    ;
	cmp dword [nDigit], FP2A_MAXIMUM_DIGIT
%else
	lea rsi, [rel szTemp+18 wrt ..gotpcrel] ; point to converted buffer
	sub rsi, [rel nDigit wrt ..gotpcrel]    ;
	cmp dword [rel nDigit wrt ..gotpcrel], FP2A_MAXIMUM_DIGIT	
%endif
	je .L41
%ifdef _USE32_
	cmp byte [esi-1], 0x30  ; check rounding of FloatToBCD 
%else
	cmp byte [rsi-1], 0x30
%endif
	je .L41
%ifdef _USE32_
	dec esi
	inc dword [iExp]
%else
	dec rsi
	inc dword [rel iExp wrt ..gotpcrel]
%endif

.L41:	

; ; If the exponent is between -1 and 6, we can express this as a number
; ; without scientific notation.
%ifdef _USE32_
	test dword [format], FP2A_ALLOW_ORDINARY_EXPRESSION
%else
	test dword [rel format wrt ..gotpcrel], FP2A_ALLOW_ORDINARY_EXPRESSION
%endif
	jz near .end_aoe
%ifdef _USE32_
	push dword [iExp]
	mov ecx, [iExp]
%else
	push qword [rel iExp wrt ..gotpcrel]
	mov ecx, dword [rel iExp wrt ..gotpcrel]
%endif

; ; if you allow use of SI prefixes, the exponent -24 to 24 can be as
; ; an ordinary number with the prefix, which we call engineering notation
%ifdef _USE32_
	mov byte [prefix], 0
	test dword [format], FP2A_ALLOW_ENGINEERING_NOTATION
%else
	mov byte [rel prefix wrt ..gotpcrel], 0
	test dword [rel format wrt ..gotpcrel], FP2A_ALLOW_ENGINEERING_NOTATION	
%endif
	jz .end_aen
	mov eax, ecx
	cdq
	mov ecx, 3
	idiv ecx
	cmp eax, 8
	jg .end_aen
	cmp eax, -7
	jl .end_aen
	cmp edx, 0
	jge .L26
        add edx, 3
        dec eax
.L26
%ifdef _USE32_
	mov [iExp], edx
%else
	mov [rel iExp wrt ..gotpcrel], edx
%endif
	add eax, 8
%ifdef _USE32_
	mov ebx, SI_PREFIX
%else
	lea rbx, [rel SI_PREFIX wrt ..gotpcrel]
%endif
	xlatb
%ifdef _USE32_
	mov byte [prefix], al
%else
	mov byte [rel prefix wrt ..gotpcrel], al
%endif
.end_aen
%ifdef _USE32_
	mov ecx, [iExp]
	mov edx, [format]
%else
	mov ecx, [rel iExp wrt ..gotpcrel]
	mov edx, [rel format wrt ..gotpcrel]
%endif
	and edx, 0x0FF
	dec dl
	cmp ecx, -1
	jl .end_aoe1
	cmp ecx, edx
	jg .end_aoe1
; ;
; ; We need to copy ecx+1 digits, then a decimal point (maybe), then
; ; the remaining 6-ecx digits.  If exponent is 0, add a leading 0.

	cmp ecx, -1
	jne .L27
%ifdef _USE32_
        mov byte [edi], 0x30	; "0"
        inc edi
%else
        mov byte [rdi], 0x30	; "0"
        inc rdi	
%endif
.L27
	inc ecx
	rep movsb
	mov ecx, edx
%ifdef _USE32_
	mov byte [edi], 0x2E	; "."
	inc edi
	sub ecx, dword [iExp]
%else
	mov byte [rdi], 0x2E	; "."
	inc rdi
	sub ecx, dword [rel iExp wrt ..gotpcrel]	
%endif
	rep movsb

	;; Trim off trailing zeros.
%ifdef _USE32_
	test dword [format], FP2A_TRIM_TRAILING_ZEROS
%else
	test dword [rel format wrt ..gotpcrel], FP2A_TRIM_TRAILING_ZEROS
%endif	
	jz .L28
.L29:
%ifdef _USE32_
	cmp byte [edi-1], 0x30
%else
	cmp byte [rdi-1], 0x30
%endif
	jne .L28
%ifdef _USE32_
        mov byte [edi-1], 20h
        dec edi
%else
        mov byte [rdi-1], 20h
        dec rdi
%endif
	jmp .L29

.L28
	;; if we cleared out all the decimal digits, kill the decimal point, too.
%ifdef _USE32_
	cmp byte [edi-1], 0x2E
%else
	cmp byte [rdi-1], 0x2E
%endif
	jne .L2A
%ifdef _USE32_
        mov byte [edi-1], 0x20
        dec edi
%else
        mov byte [rdi-1], 0x20
        dec rdi
%endif

.L2A
	;; put the prefix
%ifdef _USE32_
	cmp byte [prefix], 0
%else
	cmp byte [rel prefix wrt ..gotpcrel], 0
%endif
	je .L2B
%ifdef _USE32_
        mov al, byte [prefix]
        mov byte [edi], al
        inc edi
%else
        mov al, byte [rel prefix wrt ..gotpcrel]
        mov byte [rdi], al
        inc rdi	
%endif
.L2B
	;; That's it.
%ifdef _USE32_
        add esp, 4
%else
	add rsp, 8
%endif
	jmp .ftsExit

.end_aoe1
	
%ifdef _USE32_
	pop dword [iExp]
%else
	pop qword [rel iExp wrt ..gotpcrel]
%endif
	
.end_aoe

; ;
; ; Now convert this to a standard, usable format.  If needed, a minus
; ; sign is already present in the outgoing buffer, and edi already points
; ; past it.
; ;
	movsb                 ; copy the first digit
%ifdef _USE32_
	mov byte [edi], 0x2E  ; plop in a decimal point
	inc edi
	mov ecx, [nDigit]
%else
	mov byte [rdi], 0x2E  ; plop in a decimal point
	inc rdi
	mov ecx, [rel nDigit wrt ..gotpcrel]
%endif
	dec ecx
%ifdef _USE32_
	push ecx
%else
	push rcx
%endif
	shr ecx, 2
	rep movsd
%ifdef _USE32_
	pop ecx
%else
	pop rcx
%endif
	and ecx, 3
	rep movsb

; ; The printf %g specified trims off trailing zeros here.  I dislike
; ; this, so I've disabled it.  Comment out the if 0 and endif if you
; ; want this.
%ifdef _USE32_
	test dword [format], FP2A_TRIM_TRAILING_ZEROS
%else
	test dword [rel format wrt ..gotpcrel], FP2A_TRIM_TRAILING_ZEROS
%endif
	jz .L50
.L51
%ifdef _USE32_
	cmp byte [edi-1], 0x30
%else
	cmp byte [rdi-1], 0x30
%endif
	jne .L50a
%ifdef _USE32_
	mov byte [edi-1], 0x20
	dec edi
%else
	mov byte [rdi-1], 0x20
	dec rdi	
%endif
	jmp .L51
.L50a
%ifdef _USE32_
	test dword [format], FP2A_TRIM_ALL_TRAILING_ZEROS
%else
	test dword [rel format wrt ..gotpcrel], FP2A_TRIM_ALL_TRAILING_ZEROS
%endif
	jnz .L50
%ifdef _USE32_
	cmp byte [edi-1], 0x2E
%else
	cmp byte  [rdi-1], 0x2E
%endif
	jne .L50
%ifdef _USE32_
        mov byte [edi], 0x30
        inc edi
%else	
        mov byte [rdi], 0x30
        inc rdi
%endif
	
.L50
; ; Shove in the exponent.  If you support 10-byte reals, remember to
; ; allow 4 digits for the exponent.
%ifdef _USE32_
	mov eax, [iExp]
	test dword [format], FP2A_SUPRESS_E0
%else
	mov eax, [rel iExp wrt ..gotpcrel]
	test dword [rel format wrt ..gotpcrel], FP2A_SUPRESS_E0	
%endif
	jz .L52
	or eax, eax
	je .ftsExit
.L52
%ifdef _USE32_
	mov byte [edi], "e" ; start the exponent
	inc edi
%else
	mov byte [rdi], "e"
	inc rdi
%endif
	cmp eax, 0
	jge .L53
%ifdef _USE32_
	mov byte [edi], "-"
%else
	mov byte [rdi], "-"
%endif
	neg eax
	jmp .L55
.L53
%ifdef _USE32_
	test dword [format], FP2A_FORCE_NOT_SHOW_EXPSIGN
%else
	test dword [rel format wrt ..gotpcrel], FP2A_FORCE_NOT_SHOW_EXPSIGN
%endif
	jnz .L54
%ifdef _USE32_
	mov byte [edi], "+"
%else
	mov byte [rdi], "+"
%endif
	jmp .L55
.L54
%ifdef _USE32_
	dec edi ; <<<<<<<<<<<<<<<<<< ?
%else
	dec rdi
%endif
.L55
%ifdef _USE32_
	mov ebx, dword [format]
%else
	mov ebx, dword [rel format wrt ..gotpcrel]
%endif
	and ebx, FP2A_KEEP_LEADING_ZEROS
	shr ebx, 12 ; 0 if remove leading zeros
	or eax, eax
	jnz .L56
	or bl, bl
	jnz .L56
	;; you cant remove all zeros, put just one zero and exit
%ifdef _USE32_
	inc edi
	mov byte [edi], 0x30
%else
	inc rdi
	mov byte [rdi], 0x30	
%endif
	jmp .ExitDoPop
.L56
	mov ecx, 10

	xor edx, edx
	div ecx
	push dx ; the ones

	xor edx, edx
	div ecx
	push dx ; the tens

	xor edx, edx
	div ecx
	push dx ; the hundreds

	xor edx, edx
	div ecx
	push dx			; <<<<<<<<<<<<<<<<<<<<<<<<<<

	mov ecx, 4
.DoPop:
	dec ecx
	jl .ExitDoPop
	pop ax
	or bl, al ; if al /= 0 dl become /=0 and next time ... you see
	jz .DoPop
	add al, 0x30
%ifdef _USE32_
	inc edi
	mov byte [edi], al
%else
	inc rdi
	mov byte [rdi], al
%endif
	jmp .DoPop

.ExitDoPop:
%ifdef _USE32_
	inc edi ; point to terminator
%else
	inc rdi
%endif
	
	;; clean up and go home.
.ftsExit:
%ifdef _USE32_
	test dword [format], FP2A_NULLTERM
%else
	test dword [rel format wrt ..gotpcrel], FP2A_NULLTERM
%endif
	jz .L60
	xor al, al
	jmp .L61
.L60
	mov al, 20h
.L61
;     fldcw [stat]  ; restore control word
;     fwait
%ifdef _USE32_
	mov byte [edi], al
	mov eax, edi
	sub eax, dowrd [pstr]
	test dword [format], FP2A_ADJUSTR
%else
	mov byte [rdi], al
	mov rax, rdi
	sub rax, qword [rel pstr wrt ..gotpcrel]
	test dword [rel format wrt ..gotpcrel], FP2A_ADJUSTR
%endif
	jnz .adjustR

.adjustR:
	mov ebx, eax ; return code
	mov edx, eax
%ifdef _USE32_
	mov eax, [format]
%else
	mov eax, [rel format wrt ..gotpcrel]
%endif
	shr eax, 28
	cmp eax, edx
	jbe .doneAdjustR
	mov ebx, eax ; return code
%ifdef _USE32_
	mov esi, dword [pstr]
	mov edi, esi
	add edi, eax
	add esi, edx
	dec esi
	dec edi
%else
	mov rsi, qword [rel pstr wrt ..gotpcrel]
	mov rdi, rsi
	add rdi, rax
	add rsi, rdx
	dec rsi
	dec rdi
%endif
	mov ecx, edx
	std
	rep movsb
	cld
%ifdef _USE32_
	mov edi, dword [pstr]
%else
	mov rdi, qword [rel pstr wrt ..gotpcrel]
%endif
	mov ecx, eax
	mov al, 20h
	sub ecx, edx
	rep stosb
.doneAdjustR:
	mov eax, ebx
	jmp .return

