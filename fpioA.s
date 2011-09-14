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

;;; 
section .data align=16
; I'd rather this was local to the procedure, but masm doesn't do
; local arrays correctly.
szTemp  times 20  db 0
real8   times 8 db 0
format  dd 0
iExp	dd 0
nDigit 	dd 0
prefix	dd 0
SI_PREFIX db 'y','z','a','f','p','n','u','m' ; 9
          db 0                               ; 1
          db 'k','M','G','T','P','E','Z','Y' ; 9
real10 dt 0.0
ten1  dt 1.0e1
ten2  dt 1.0e2
ten3  dt 1.0e3
ten4  dt 1.0e4
ten5  dt 1.0e5
ten6  dt 1.0e6
ten7  dt 1.0e7
ten8  dt 1.0e8
ten9 dt 1.0e9
ten10 dt 1.0e10
ten11 dt 1.0e11
ten12 dt 1.0e12
ten13 dt 1.0e13
ten14 dt 1.0e14
ten15 dt 1.0e15
ten16 dt 1.0e16
ten17 dt 1.0e17
ten18 dt 1.0e18
ten19 dt 1.0e19
ten20 dt 1.0e20
;;; 
ten_1 dt 1.0e1
 dt 1.0e2
 dt 1.0e3
 dt 1.0e4
 dt 1.0e5
 dt 1.0e6
 dt 1.0e7
 dt 1.0e8
 dt 1.0e9
 dt 1.0e10
 dt 1.0e11
 dt 1.0e12
 dt 1.0e13
 dt 1.0e14
 dt 1.0e15
;;; 
ten_16 dt 1.0e16
 dt 1.0e32
 dt 1.0e48
 dt 1.0e64
 dt 1.0e80
 dt 1.0e96
 dt 1.0e112
 dt 1.0e128
 dt 1.0e144
 dt 1.0e160
 dt 1.0e176
 dt 1.0e192
 dt 1.0e208
 dt 1.0e224
 dt 1.0e240
;;; 
ten_256 dt 1.0e256
; The remaining exponents are only necessary if we decide to support
; 10-byte doubles.  FloatToStr and StrToFloat only support 8-byte,
; but PowerOf10 doesn't care, so we'll include them.
 dt 1.0e512
 dt 1.0e768
 dt 1.0e1024
 dt 1.0e1280
 dt 1.0e1536
 dt 1.0e1792
 dt 1.0e2048
 dt 1.0e2304
 dt 1.0e2560
 dt 1.0e2816
 dt 1.0e3072
 dt 1.0e3328
 dt 1.0e3584
 dt 1.0e4096
 dt 1.0e4352
 dt 1.0e4608
 dt 1.0e4864

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
	pop rbx
	ret
%endif
%endm
	
; Multiply a floating point value by an integral power of 10.
;
; Entry: EAX = power of 10, -4932..4932.
; ST(0) = value to be multiplied
;
; Exit: ST(0) = value x 10^eax
_PowerOf10:
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
	fld tword [ten_1+edx*2-10]
	fmulp st1, st0
.L2
	mov dl, al
	shr dl, 4
	and edx, 0x0f
	jz .L3
	lea edx, [edx+edx*4]
	fld tword [ten_16+edx*2-10]
	fmulp st1, st0
.L3
	mov dl, ah
	and edx, 0x1f
	jz .L4
	lea edx, [edx+edx*4]
	fld tword [ten_256+edx*2-10]
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
	push edi
	sub esp, 10

  ; The fbstp instruction converts the top of the stack to a
  ; packed BCD form in ten bytes, with two digits per byte.  The top
  ; byte has the sign, which we ignore.

	fbstp [esp]

  ; Now we need to unpack the BCD to ASCII.

	lea esi, [esp+8]
	mov edi, szTemp
	mov ecx, 9
.do
	mov al, [esi]  ; xxxx xxxx AAAA BBBB
	dec esi
	rol ax, 12  ; BBBB xxxx xxxx AAAA
	rol ah, 4  ; xxxx BBBB xxxx AAAA
	and ax, 0x0f0f  ; 0000 BBBB 0000 AAAA
	add ax, 0x3030  ; 3B3A
	mov [edi], ax
	add edi, 2
	dec ecx
	jnz .do
	add esp, 10
	pop edi
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
	mov edi, [ebp+arg2] ; szDbl
	mov esi, [ebp+arg1] ; fpin
	mov eax, [ebp+arg3] ; opt
	;; 
	or eax, eax
	jnz .set_format
	mov dword [format], FP2A_DEFAULT
	jmp .begin

.set_format:	
	mov [format], eax		;
	test eax, FP2A_INPUT_REAL4
	jz .begin
	fld dword [esi]
	mov esi, real8
	fstp qword [esi]
.begin:	
	xor eax, eax
	cmp dword [esi], 0
	jne .L1
	test dword [format], FP2A_INPUT_REAL10
	jne .L2
	test dword [esi+4], 0x7FFFFFFF
	jne .L3
	inc eax
.L3:
	jmp .L4
.L2:	
	cmp dword [esi+4], 0
	jne .L5
	cmp word [esi+8], 0
	jne .L5
	inc eax
.L1:	
.L4:	
.L5:	
	cmp dword [esi], 0
	jne .L6
	cmp dword [esi+4], 0
	jne .L6
	test dword [format], FP2A_INPUT_REAL10
	je .L7
	cmp word [esi+8], 0
	je .L6
	inc eax
.L7:
	inc eax
	jmp .L6
.L6	
	or eax, eax
	jz near .L8
	test dword [format], FP2A_FORCE_SHOW_SIGN
	jz .L9
	mov byte [edi], "+"
	inc edi
.L9:
	mov byte [edi], 0x30
	inc edi
	test dword [format], FP2A_ALLOW_ORDINARY_EXPRESSION
	jnz .L10
	test dword [format], FP2A_TRIM_TRAILING_ZEROS
	jz .L11
        mov word [edi], 0x302e
        add eax, 2
        add edi, 2
	jmp .L12
.L11:	
; no check for format length
        mov byte [edi], 0x2E
        inc edi
        mov ecx, dword [format]
        mov edx, ecx
        and ecx, 0x0FF
        dec ecx
        mov al, 0x30
        rep stosb
.L12
	test dword [format], FP2A_ALLOW_ORDINARY_EXPRESSION | FP2A_SUPRESS_E0
	jnz .L10
        mov byte [edi], 0x65 ; e
        inc edi
	test dword [format], FP2A_FORCE_NOT_SHOW_EXPSIGN
	jnz .L11a
        mov byte [edi], "+"
        inc edi
.L11a:	
        mov byte [edi], 0x30
        inc edi
.L10:
	test dword [format], FP2A_NULLTERM
	jz .L11b
        xor al, al
	jmp .L12a

.L11b:	
        mov al, 0x20
.L12a
	mov byte [edi], al
	mov eax, edi
	sub eax, [ebp+arg2] ; szDbl

.return:
	end_proc

.L8:	
; Check for a negative number.
	xor eax, eax
	test dword [format], FP2A_INPUT_REAL10
	jz .L13
	cmp word [esi+8], 0
	jge .L14
        lea eax, [esi+9]
        and byte [eax], 0x07f ; change to positive
.L13:	
	cmp word [esi+6], 0
	jge .L14
        lea eax, [esi+7]
        and byte [eax], 0x07f ; change to positive
.L14:	
	or eax, eax
	jz .L15
	mov byte [edi], "-"  ; store a minus sign
	inc edi
	jmp .L16
.L15
	test dword [format], FP2A_FORCE_SHOW_SIGN
	jz .L16
	mov byte [edi], "+"
	inc edi
.L16

; ; Initialize the floating point unit and load our value onto the stack.
; ; eax != 0 if src have been forced to be positive
;     fclex
;     fstcw [stat]
;     mov [mystat], 027fh
;     fldcw [mystat]

	test dword [format], FP2A_INPUT_REAL10
	jz .L20
	;; Sep 12, 2011
	;; gfortran's real*10 does not have 63rd bit set.
	;; for non-zero value, it must be set.
	fld tword [esi]
	mov esi, real10
	fstp tword [esi]
	or dword [esi+4],0x80000000

	fld tword [esi]
	jmp .L21
	.L20
	fld qword [esi]
	.L21
	fld st0
	or eax, eax
	jz .L22
; recover minus sign; source never be changed!!!
	or byte [eax], 0x80
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
	fistp dword [iExp]  ; ST = fpin

; ; An 8-byte double can carry almost 16 digits of precision.  Actually, it's
; ; 15.9 digits, so some numbers close to 1E17 will be wrong in the bottom
; ; digit.  If this is a concern, change the '16' to a '15'.
; ;
; ; A 10-byte double can carry almost 19 digits, but fbstp only stores the
; ; guaranteed 18.  If you're doing 10-byte doubles, change the '16' to '18'.

	test dword [format], FP2A_ALLOW_INTEGER_EXPRESSION
	je .end_aie
	cmp dword [iExp], 18
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
	mov ecx, [iExp]
	sub eax, ecx
	inc ecx
	lea esi, [szTemp+eax]

; ; The off-by-one order of magnitude problem below can hit us here.
; ; We just trim off the possible leading zero.
	cmp byte [esi], 0
	jnz .L25
	inc esi
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
	mov eax, dword [format]      ; X07h
	and eax, 0x0FF               ; 07h
	cmp al, FP2A_MAXIMUM_DIGIT
	ja near .ftsExit
	cmp al, FP2A_MINIMUM_DIGIT
	jb near .ftsExit
	mov esi, eax                 ; 07h
	sub esi, 2
	mov dword [nDigit], eax     ; 0 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
	add esi, esi
	jz .L30
	lea esi, [esi+esi*4]
	mov ebx, ten1
	add ebx, esi
.L30:	
	dec eax               ; 6
	sub eax, [iExp] ; adjust exponent to NUMBER_OF_DIGIT
	call _PowerOf10

; ; Either we have exactly NUMBER_OF_DIGIT digits,
; ; or we have exactly NUMBER_OF_DIGIT - 1 digits.  We can
; ; detect that condition and adjust now.
	fld tword [ebx]
	fcomp st1
;     ; x0xxxx00 means top of stack < tenX
;     ; x0xxxx01 means top of stack > tenX
;     ; x1xxxx00 means top of stack = tenX
	fstsw ax
	test ah, 0x43
	jnz .L40
	fld tword [ten1]
	fmulp st1, st0
	dec dword [iExp]

.L40:	
; ; Go convert to BCD.
	call _FloatToBCD

	lea esi, [szTemp+18] ; point to converted buffer
	sub esi, [nDigit]    ;
	cmp dword [nDigit], FP2A_MAXIMUM_DIGIT
	je .L41
	cmp byte [esi-1], 0x30  ; check rounding of FloatToBCD 
	je .L41
	dec esi
	inc dword [iExp]
.L41:	

; ; If the exponent is between -1 and 6, we can express this as a number
; ; without scientific notation.
	test dword [format], FP2A_ALLOW_ORDINARY_EXPRESSION
	jz near .end_aoe
	push dword [iExp]
	mov ecx, [iExp]
; ; if you allow use of SI prefixes, the exponent -24 to 24 can be as
; ; an ordinary number with the prefix, which we call engineering notation
	mov byte [prefix], 0
	test dword [format], FP2A_ALLOW_ENGINEERING_NOTATION
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
	mov [iExp], edx
	add eax, 8
	mov ebx, SI_PREFIX
	xlatb
	mov byte [prefix], al
.end_aen
	mov ecx, [iExp]
	mov edx, [format]
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
        mov byte [edi], 0x30	; "0"
        inc edi
.L27

	inc ecx
	rep movsb
	mov byte [edi], 0x2E	; "."
	inc edi
	mov ecx, edx
	sub ecx, dword [iExp]
	rep movsb

;       ; Trim off trailing zeros.
	test dword [format], FP2A_TRIM_TRAILING_ZEROS
	jz .L28
.L29:	
	cmp byte [edi-1], 0x30
	jne .L28
        mov byte [edi-1], 20h
        dec edi
	jmp .L29

.L28
;       ; If we cleared out all the decimal digits, kill the decimal point, too.
	cmp byte [edi-1], 0x2E
	jne .L2A
        mov byte [edi-1], 0x20
        dec edi
.L2A

; ; put the prefix
	cmp byte [prefix], 0
	je .L2B
        mov al, byte [prefix]
        mov byte [edi], al
        inc edi
.L2B

; ; That's it.
        add esp, 4 		; iExp
	jmp .ftsExit

.end_aoe1
	pop dword [iExp]
.end_aoe

; ;
; ; Now convert this to a standard, usable format.  If needed, a minus
; ; sign is already present in the outgoing buffer, and edi already points
; ; past it.
; ;
	movsb    ; copy the first digit
	mov byte [edi], 0x2E  ; plop in a decimal point
	inc edi
	mov ecx, [nDigit]
	dec ecx
	push ecx
	shr ecx, 2
	rep movsd
	pop ecx
	and ecx, 3
	rep movsb

; ; The printf %g specified trims off trailing zeros here.  I dislike
; ; this, so I've disabled it.  Comment out the if 0 and endif if you
; ; want this.
	test dword [format], FP2A_TRIM_TRAILING_ZEROS
	jz .L50
.L51
	cmp byte [edi-1], 0x30
	jne .L50a
	mov byte [edi-1], 0x20
	dec edi
	jmp .L51
.L50a
	test dword [format], FP2A_TRIM_ALL_TRAILING_ZEROS
	jnz .L50
	cmp byte [edi-1], 0x2E
	jne .L50
        mov byte [edi], 0x30
        inc edi
.L50

; ; Shove in the exponent.  If you support 10-byte reals, remember to
; ; allow 4 digits for the exponent.

	mov eax, [iExp]
	test dword [format], FP2A_SUPRESS_E0
	jz .L52
	or eax, eax
	je .ftsExit
.L52
	mov byte [edi], "e" ; start the exponent
	inc edi
	cmp eax, 0
	jge .L53
	mov byte [edi], "-"
	neg eax
	jmp .L55
.L53
	test dword [format], FP2A_FORCE_NOT_SHOW_EXPSIGN
	jnz .L54
	mov byte [edi], "+"
	jmp .L55
.L54
	dec edi ; <<<<<<<<<<<<<<<<<< CHECK OUT !!!!!!!
.L55
	mov ebx, dword [format]
	and ebx, FP2A_KEEP_LEADING_ZEROS
	shr ebx, 12 ; 0 if remove leading zeros
	or eax, eax
	jnz .L56
	or bl, bl
	jnz .L56
;       ; you cant remove all zeros, put just one zero and exit
	inc edi
	mov byte [edi], 0x30
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
	push dx

	mov ecx, 4
.DoPop:
	dec ecx
	jl .ExitDoPop
	pop ax
	or bl, al ; if al /= 0 dl become /=0 and next time ... you see
	jz .DoPop
	add al, 0x30
	inc edi
	mov byte [edi], al
	jmp .DoPop

.ExitDoPop:
	inc edi ; point to terminator

; ; Clean up and go home.

.ftsExit:
	test dword [format], FP2A_NULLTERM
	jz .L60
	xor al, al
	jmp .L61
.L60
	mov al, 20h
.L61
	mov byte [edi], al
;     fldcw [stat]  ; restore control word
;     fwait
	mov eax, edi
	sub eax, [ebp+arg2]; [szDbl]

	test dword [format], FP2A_ADJUSTR
	jnz .adjustR

.adjustR:
	mov ebx, eax ; return code
	mov edx, eax
	mov eax, [format]
	shr eax, 28
	cmp eax, edx
	jbe .doneAdjustR
	mov ebx, eax ; return code
	mov esi, [ebp+arg2] ; [szDbl]
	mov edi, esi
	add edi, eax
	add esi, edx
	std
	dec esi
	dec edi
	mov ecx, edx
	rep movsb
	cld
	mov edi, [ebp+arg2] ; szDbl
	mov ecx, eax
	mov al, 20h
	sub ecx, edx
	rep stosb
.doneAdjustR:
	mov eax, ebx
	jmp .return