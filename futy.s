BITS 64
section .data align=16
fpucw dw 0
	
section .text align=16
global poweroften_

 poweroftwo_:
 	fstcw word [fpucw]
 	or byte [fpucw+1], 1111b
 	fldcw word [fpucw]
 	fld st0			; x
 	fld st0			; x x
 	frndint                 ; int(x) x
 	fxch			; x int(x)
 	fsub st0, st1		; x-int(x) int(x)
 	f2xm1			; 2**(x-int(x))-1 int(x)
 	fld1			; 1 2**(x-int(x))-1 int(x)
 	faddp   		; 2**(x-int(x)) int(x)
 	fxch			; int(x) 2**(x-int(x)) 
 	fld1			; 1 int(x) 2**(x-int(x)) 
 	fscale			; 2**int(x) int(x) 2**(x-int(x))
 	fstp st1		; 2**int(x) 2**(x-int(x)) 
 	fmulp			; 2**int(x)*2**(x-int(x)) 2**(x-int(x))    
 	ret

poweroften_:
	push rbp
	mov rbp, rsp
	;;
 	fstcw word [fpucw]
 	or byte [fpucw+1], 1111b
 	fldcw word [fpucw]
	;; 
	fild qword [rdi]	; n
	fldl2t			; ln(10) n 
	fmulp			; n*ln(10)
	call poweroftwo_	; 2**(n*ln(10))
	fstp tword [rsi]
	;; 
	pop rbp
	ret