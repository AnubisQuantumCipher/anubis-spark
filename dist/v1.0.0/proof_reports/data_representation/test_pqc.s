	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
Ltext0:
	.file 1 "/Users/sicarii/Desktop/anubis-spark/src/test_pqc.adb"
	.const
	.align	3
lC31:
	.ascii "\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220"
	.align	3
lC32:
	.ascii " Testing ML-KEM-1024 (NIST Level 5)"
	.align	3
lC33:
	.ascii "1. Generating keypair... "
	.align	3
lC34:
	.ascii "\342\234\223 SUCCESS"
	.align	3
lC35:
	.ascii "   Public key:  1,568 bytes"
	.align	3
lC36:
	.ascii "   Secret key:  3,168 bytes (VALID)"
	.align	3
lC37:
	.ascii "\342\234\227 FAILED"
	.align	3
lC38:
	.ascii "2. Encapsulating shared secret... "
	.align	3
lC39:
	.ascii "   Ciphertext:    1,568 bytes"
	.align	3
lC40:
	.ascii "   Shared secret: 32 bytes (VALID)"
	.align	3
lC41:
	.ascii "3. Decapsulating shared secret... "
	.align	3
lC42:
	.ascii "4. Verifying shared secrets match... "
	.align	3
lC43:
	.ascii "   Both parties derived identical 32-byte shared secret!"
	.align	3
lC44:
	.ascii "\342\234\227 FAILED - Shared secrets DO NOT match!"
	.align	3
lC45:
	.ascii "5. Securely zeroizing keys... "
	.align	3
lC46:
	.ascii "   All secrets marked as INVALID"
	.align	3
lC47:
	.ascii "\342\234\227 WARNING - Zeroization may have failed"
	.text
	.align	2
_test_pqc__test_ml_kem.0:
LFB2:
	.loc 1 18 4
	sub	x10, sp, #16384
	str	xzr, [x10]
	sub	x10, x10, #4096
	str	xzr, [x10, 1328]
	stp	x29, x30, [sp, -96]!
LCFI0:
	mov	x29, sp
LCFI1:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
	stp	x25, x26, [sp, 64]
	str	x27, [sp, 80]
	mov	x13, 6768
	sub	sp, sp, x13
LCFI2:
	mov	x19, x16
	sub	x0, x29, #16384
	str	x16, [x0, 9960]
	.loc 1 18 4
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x1, [x0]
	str	x1, [x29, -8]
	mov	x1, 0
	.loc 1 21 7
	sub	x0, x29, #3184
	bl	_anubis_types__ml_kem_secret_keyIP
	.loc 1 25 7
	sub	x0, x29, #4096
	sub	x0, x0, #2304
	bl	_anubis_types__ml_kem_shared_secretIP
	.loc 1 28 7
	sub	x0, x29, #4096
	sub	x0, x0, #2264
	bl	_anubis_types__ml_kem_shared_secretIP
LBB2:
	.loc 1 30 7
	adrp	x0, lC31@PAGE
	add	x20, x0, lC31@PAGEOFF;
	adrp	x0, lC6@PAGE
	add	x21, x0, lC6@PAGEOFF;
	mov	x0, x20
	mov	x1, x21
	bl	_ada__text_io__put_line__2
LBE2:
LBB3:
	.loc 1 31 7
	adrp	x0, lC32@PAGE
	add	x22, x0, lC32@PAGEOFF;
	adrp	x0, lC15@PAGE
	add	x23, x0, lC15@PAGEOFF;
	mov	x0, x22
	mov	x1, x23
	bl	_ada__text_io__put_line__2
LBE3:
LBB4:
	.loc 1 32 7
	adrp	x0, lC31@PAGE
	add	x24, x0, lC31@PAGEOFF;
	adrp	x0, lC6@PAGE
	add	x25, x0, lC6@PAGEOFF;
	mov	x0, x24
	mov	x1, x25
	bl	_ada__text_io__put_line__2
LBE4:
	.loc 1 33 7
	mov	w0, 1
	bl	_ada__text_io__new_line__2
LBB5:
	.loc 1 36 7
	adrp	x0, lC33@PAGE
	add	x26, x0, lC33@PAGEOFF;
	adrp	x0, lC25@PAGE
	add	x27, x0, lC25@PAGEOFF;
	mov	x0, x26
	mov	x1, x27
	bl	_ada__text_io__put__4
LBE5:
	.loc 1 37 7
	sub	x1, x29, #3184
	sub	x0, x29, #4096
	sub	x0, x0, #2224
	bl	_anubis_types__pqc__ml_kem_generate_keypair
	.loc 1 37 7 is_stmt 0 discriminator 1
	strb	w0, [x19]
	.loc 1 39 7 is_stmt 1
	ldrb	w0, [x19]
	cmp	w0, 0
	beq	L2
	.loc 1 39 27 discriminator 1
	sub	x0, x29, #3184
	bl	_anubis_types__is_valid__6
	.loc 1 39 18 discriminator 2
	cmp	w0, 0
	beq	L2
LBB6:
	.loc 1 40 10
	adrp	x0, lC34@PAGE
	add	x0, x0, lC34@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9616]
	adrp	x0, lC13@PAGE
	add	x0, x0, lC13@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9624]
	sub	x0, x29, #4096
	sub	x0, x0, #2560
	ldp	x0, x1, [x0, -112]
	bl	_ada__text_io__put_line__2
LBE6:
LBB7:
	.loc 1 41 10
	adrp	x0, lC35@PAGE
	add	x0, x0, lC35@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9632]
	adrp	x0, lC14@PAGE
	add	x0, x0, lC14@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9640]
	sub	x0, x29, #4096
	sub	x0, x0, #2560
	ldp	x0, x1, [x0, -96]
	bl	_ada__text_io__put_line__2
LBE7:
LBB8:
	.loc 1 42 10
	adrp	x0, lC36@PAGE
	add	x0, x0, lC36@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9648]
	adrp	x0, lC15@PAGE
	add	x0, x0, lC15@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9656]
	sub	x0, x29, #4096
	sub	x0, x0, #2560
	ldp	x0, x1, [x0, -80]
	bl	_ada__text_io__put_line__2
LBE8:
	nop
	.loc 1 47 7
	mov	w0, 1
	bl	_ada__text_io__new_line__2
LBB9:
	.loc 1 50 7
	adrp	x0, lC38@PAGE
	add	x0, x0, lC38@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9680]
	adrp	x0, lC26@PAGE
	add	x0, x0, lC26@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9688]
	sub	x0, x29, #4096
	sub	x0, x0, #2560
	ldp	x0, x1, [x0, -48]
	bl	_ada__text_io__put__4
LBE9:
	.loc 1 51 7
	sub	x2, x29, #4096
	sub	x2, x2, #2304
	sub	x1, x29, #4096
	sub	x1, x1, #656
	sub	x0, x29, #4096
	sub	x0, x0, #2224
	bl	_anubis_types__pqc__ml_kem_encapsulate
	.loc 1 51 7 is_stmt 0 discriminator 1
	strb	w0, [x19]
	.loc 1 53 7 is_stmt 1
	ldrb	w0, [x19]
	cmp	w0, 0
	beq	L5
	b	L15
L2:
LBB10:
	.loc 1 44 10
	adrp	x0, lC37@PAGE
	add	x0, x0, lC37@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9664]
	adrp	x0, lC16@PAGE
	add	x0, x0, lC16@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9672]
	sub	x0, x29, #4096
	sub	x0, x0, #2560
	ldp	x0, x1, [x0, -64]
	bl	_ada__text_io__put_line__2
LBE10:
	.loc 1 45 10
	b	L1
L15:
	.loc 1 53 27 discriminator 1
	sub	x0, x29, #4096
	sub	x0, x0, #2304
	bl	_anubis_types__is_valid__7
	.loc 1 53 18 discriminator 2
	cmp	w0, 0
	beq	L5
LBB11:
	.loc 1 54 10
	adrp	x0, lC34@PAGE
	add	x0, x0, lC34@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9696]
	adrp	x0, lC13@PAGE
	add	x0, x0, lC13@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9704]
	sub	x0, x29, #4096
	sub	x0, x0, #2560
	ldp	x0, x1, [x0, -32]
	bl	_ada__text_io__put_line__2
LBE11:
LBB12:
	.loc 1 55 10
	adrp	x0, lC39@PAGE
	add	x0, x0, lC39@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9712]
	adrp	x0, lC27@PAGE
	add	x0, x0, lC27@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9720]
	sub	x0, x29, #4096
	sub	x0, x0, #2560
	ldp	x0, x1, [x0, -16]
	bl	_ada__text_io__put_line__2
LBE12:
LBB13:
	.loc 1 56 10
	adrp	x0, lC40@PAGE
	add	x0, x0, lC40@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9728]
	adrp	x0, lC26@PAGE
	add	x0, x0, lC26@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9736]
	sub	x0, x29, #4096
	sub	x0, x0, #2560
	ldp	x0, x1, [x0]
	bl	_ada__text_io__put_line__2
LBE13:
	nop
	.loc 1 62 7
	mov	w0, 1
	bl	_ada__text_io__new_line__2
LBB14:
	.loc 1 65 7
	adrp	x0, lC41@PAGE
	add	x0, x0, lC41@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9760]
	adrp	x0, lC26@PAGE
	add	x0, x0, lC26@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9768]
	sub	x0, x29, #4096
	sub	x0, x0, #2560
	ldp	x0, x1, [x0, 32]
	bl	_ada__text_io__put__4
LBE14:
	.loc 1 66 7
	sub	x2, x29, #4096
	sub	x2, x2, #2264
	sub	x1, x29, #3184
	sub	x0, x29, #4096
	sub	x0, x0, #656
	bl	_anubis_types__pqc__ml_kem_decapsulate
	.loc 1 66 7 is_stmt 0 discriminator 1
	strb	w0, [x19]
	.loc 1 68 7 is_stmt 1
	ldrb	w0, [x19]
	cmp	w0, 0
	beq	L7
	b	L16
L5:
LBB15:
	.loc 1 58 10
	adrp	x0, lC37@PAGE
	add	x0, x0, lC37@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9744]
	adrp	x0, lC16@PAGE
	add	x0, x0, lC16@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9752]
	sub	x0, x29, #4096
	sub	x0, x0, #2560
	ldp	x0, x1, [x0, 16]
	bl	_ada__text_io__put_line__2
LBE15:
	.loc 1 59 10
	sub	x0, x29, #3184
	bl	_anubis_types__pqc__zeroize_ml_kem_secret
	.loc 1 60 10
	b	L1
L16:
	.loc 1 68 27 discriminator 1
	sub	x0, x29, #4096
	sub	x0, x0, #2264
	bl	_anubis_types__is_valid__7
	.loc 1 68 18 discriminator 2
	cmp	w0, 0
	beq	L7
LBB16:
	.loc 1 69 10
	adrp	x0, lC34@PAGE
	add	x0, x0, lC34@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9776]
	adrp	x0, lC13@PAGE
	add	x0, x0, lC13@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9784]
	sub	x0, x29, #4096
	sub	x0, x0, #2560
	ldp	x0, x1, [x0, 48]
	bl	_ada__text_io__put_line__2
LBE16:
LBB17:
	.loc 1 70 10
	adrp	x0, lC40@PAGE
	add	x0, x0, lC40@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9792]
	adrp	x0, lC26@PAGE
	add	x0, x0, lC26@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9800]
	sub	x0, x29, #4096
	sub	x0, x0, #2560
	ldp	x0, x1, [x0, 64]
	bl	_ada__text_io__put_line__2
LBE17:
	nop
	.loc 1 77 7
	mov	w0, 1
	bl	_ada__text_io__new_line__2
LBB18:
	.loc 1 80 7
	adrp	x0, lC42@PAGE
	add	x0, x0, lC42@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9824]
	adrp	x0, lC23@PAGE
	add	x0, x0, lC23@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9832]
	sub	x0, x29, #4096
	sub	x0, x0, #2560
	ldp	x0, x1, [x0, 96]
	bl	_ada__text_io__put__4
LBE18:
LBB19:
	.loc 1 84 19
	sub	x1, x29, #4096
	sub	x1, x1, #2264
	sub	x0, x29, #4096
	sub	x0, x0, #2304
	bl	_anubis_types__pqc__secrets_match
	.loc 1 84 19 is_stmt 0 discriminator 1
	sub	x1, x29, #8192
	strb	w0, [x1, 1791]
	.loc 1 86 10 is_stmt 1
	sub	x0, x29, #8192
	ldrb	w0, [x0, 1791]
	cmp	w0, 0
	beq	L9
	b	L17
L7:
LBE19:
LBB23:
	.loc 1 72 10
	adrp	x0, lC37@PAGE
	add	x0, x0, lC37@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9808]
	adrp	x0, lC16@PAGE
	add	x0, x0, lC16@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9816]
	sub	x0, x29, #4096
	sub	x0, x0, #2560
	ldp	x0, x1, [x0, 80]
	bl	_ada__text_io__put_line__2
LBE23:
	.loc 1 73 10
	sub	x0, x29, #3184
	bl	_anubis_types__pqc__zeroize_ml_kem_secret
	.loc 1 74 10
	sub	x0, x29, #4096
	sub	x0, x0, #2304
	bl	_anubis_types__pqc__zeroize_shared_secret
	.loc 1 75 10
	b	L1
L17:
LBB24:
LBB20:
	.loc 1 87 13
	adrp	x0, lC34@PAGE
	add	x0, x0, lC34@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9840]
	adrp	x0, lC13@PAGE
	add	x0, x0, lC13@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9848]
	sub	x0, x29, #4096
	sub	x0, x0, #2560
	ldp	x0, x1, [x0, 112]
	bl	_ada__text_io__put_line__2
LBE20:
LBB21:
	.loc 1 88 13
	adrp	x0, lC43@PAGE
	add	x0, x0, lC43@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9856]
	adrp	x0, lC28@PAGE
	add	x0, x0, lC28@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9864]
	sub	x0, x29, #4096
	sub	x0, x0, #2560
	ldp	x0, x1, [x0, 128]
	bl	_ada__text_io__put_line__2
	b	L10
L9:
LBE21:
LBB22:
	.loc 1 90 13
	adrp	x0, lC44@PAGE
	add	x0, x0, lC44@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9872]
	adrp	x0, lC24@PAGE
	add	x0, x0, lC24@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9880]
	sub	x0, x29, #4096
	sub	x0, x0, #2560
	ldp	x0, x1, [x0, 144]
	bl	_ada__text_io__put_line__2
L10:
LBE22:
LBE24:
	.loc 1 93 7
	mov	w0, 1
	bl	_ada__text_io__new_line__2
LBB25:
	.loc 1 96 7
	adrp	x0, lC45@PAGE
	add	x0, x0, lC45@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9888]
	adrp	x0, lC29@PAGE
	add	x0, x0, lC29@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9896]
	sub	x0, x29, #4096
	sub	x0, x0, #2560
	ldp	x0, x1, [x0, 160]
	bl	_ada__text_io__put__4
LBE25:
	.loc 1 97 7
	sub	x0, x29, #3184
	bl	_anubis_types__pqc__zeroize_ml_kem_secret
	.loc 1 98 7
	sub	x0, x29, #4096
	sub	x0, x0, #2304
	bl	_anubis_types__pqc__zeroize_shared_secret
	.loc 1 99 7
	sub	x0, x29, #4096
	sub	x0, x0, #2264
	bl	_anubis_types__pqc__zeroize_shared_secret
	.loc 1 101 14
	sub	x0, x29, #3184
	bl	_anubis_types__is_valid__6
	.loc 1 101 14 is_stmt 0 discriminator 1
	eor	w0, w0, 1
	and	w19, w0, 255
	.loc 1 102 14 is_stmt 1
	sub	x0, x29, #4096
	sub	x0, x0, #2304
	bl	_anubis_types__is_valid__7
	.loc 1 102 14 is_stmt 0 discriminator 1
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 101 38 is_stmt 1
	cmp	w19, 0
	ccmp	w0, 0, 4, ne
	cset	w0, ne
	and	w19, w0, 255
	.loc 1 103 14
	sub	x0, x29, #4096
	sub	x0, x0, #2264
	bl	_anubis_types__is_valid__7
	.loc 1 103 14 is_stmt 0 discriminator 1
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 102 36 is_stmt 1
	cmp	w19, 0
	ccmp	w0, 0, 4, ne
	cset	w0, ne
	and	w0, w0, 255
	.loc 1 101 7
	cmp	w0, 0
	beq	L11
LBB26:
	.loc 1 105 10
	adrp	x0, lC34@PAGE
	add	x0, x0, lC34@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9904]
	adrp	x0, lC13@PAGE
	add	x0, x0, lC13@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9912]
	sub	x0, x29, #4096
	sub	x0, x0, #2560
	ldp	x0, x1, [x0, 176]
	bl	_ada__text_io__put_line__2
LBE26:
LBB27:
	.loc 1 106 10
	adrp	x0, lC46@PAGE
	add	x0, x0, lC46@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9920]
	adrp	x0, lC30@PAGE
	add	x0, x0, lC30@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9928]
	sub	x0, x29, #4096
	sub	x0, x0, #2560
	ldp	x0, x1, [x0, 192]
	bl	_ada__text_io__put_line__2
	b	L12
L11:
LBE27:
LBB28:
	.loc 1 108 10
	adrp	x0, lC47@PAGE
	add	x0, x0, lC47@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9936]
	adrp	x0, lC24@PAGE
	add	x0, x0, lC24@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 9944]
	sub	x0, x29, #4096
	sub	x0, x0, #2560
	ldp	x0, x1, [x0, 208]
	bl	_ada__text_io__put_line__2
L12:
LBE28:
	.loc 1 110 7
	mov	w0, 1
	bl	_ada__text_io__new_line__2
	.loc 1 112 8
	nop
L1:
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L14
	bl	___stack_chk_fail
L14:
	mov	sp, x29
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldr	x27, [sp, 80]
	ldp	x29, x30, [sp], 96
LCFI3:
	ret
LFE2:
	.const
	.align	2
lC6:
	.word	1
	.word	165
	.align	2
lC15:
	.word	1
	.word	35
	.align	2
lC25:
	.word	1
	.word	25
	.align	2
lC13:
	.word	1
	.word	11
	.align	2
lC14:
	.word	1
	.word	27
	.align	2
lC26:
	.word	1
	.word	34
	.align	2
lC16:
	.word	1
	.word	10
	.align	2
lC27:
	.word	1
	.word	29
	.align	2
lC23:
	.word	1
	.word	37
	.align	2
lC28:
	.word	1
	.word	56
	.align	2
lC24:
	.word	1
	.word	41
	.align	2
lC29:
	.word	1
	.word	30
	.align	2
lC30:
	.word	1
	.word	32
	.text
	.const
	.align	3
lC48:
	.space	1
	.align	3
lC49:
	.ascii "\342\225\224\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\227"
	.align	3
lC50:
	.ascii "\342\225\221  ANUBIS-SPARK: Post-Quantum Cryptography Test Suite     \342\225\221"
	.align	3
lC51:
	.ascii "\342\225\221                                                          \342\225\221"
	.align	3
lC52:
	.ascii "\342\225\221  Testing NIST Level 5 algorithms:                       \342\225\221"
	.align	3
lC53:
	.ascii "\342\225\221  \342\200\242 ML-KEM-1024 (Key Encapsulation)                      \342\225\221"
	.align	3
lC54:
	.ascii "\342\225\221  \342\200\242 ML-DSA-87 (Digital Signatures)                       \342\225\221"
	.align	3
lC55:
	.ascii "\342\225\232\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\235"
	.align	3
lC56:
	.ascii "Initializing liboqs..."
	.align	3
lC57:
	.ascii "\342\234\223 liboqs initialized"
	.align	3
lC58:
	.ascii " Test Suite Complete"
	.align	3
lC59:
	.ascii "\342\234\223 ML-KEM-1024 key encapsulation works correctly"
	.align	3
lC60:
	.ascii "\342\234\223 ML-DSA-87 digital signatures work correctly"
	.align	3
lC61:
	.ascii "\342\234\223 Secure zeroization verified"
	.align	3
lC62:
	.ascii "ANUBIS-SPARK post-quantum crypto bindings: OPERATIONAL"
	.text
	.align	2
	.globl __ada_test_pqc
__ada_test_pqc:
LFB1:
	.loc 1 11 1
	sub	x10, sp, #16384
	str	xzr, [x10, 3744]
	sub	sp, sp, #352
LCFI4:
	stp	x29, x30, [sp, 272]
LCFI5:
	add	x29, sp, 272
LCFI6:
	stp	x20, x21, [sp, 288]
	stp	x22, x23, [sp, 304]
	stp	x24, x25, [sp, 320]
	stp	x26, x27, [sp, 336]
LCFI7:
	.loc 1 11 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x1, [x0]
	str	x1, [x29, -8]
	mov	x1, 0
	add	x0, x29, 80
	.loc 1 11 1 is_stmt 0 discriminator 1
	str	x0, [x29, -16]
LBB29:
	.loc 1 217 4 is_stmt 1
	adrp	x0, lC48@PAGE
	add	x2, x0, lC48@PAGEOFF;
	adrp	x0, lC0@PAGE
	add	x3, x0, lC0@PAGEOFF;
	mov	x0, x2
	mov	x1, x3
	bl	_ada__text_io__put_line__2
LBE29:
LBB30:
	.loc 1 218 4
	adrp	x0, lC49@PAGE
	add	x20, x0, lC49@PAGEOFF;
	adrp	x0, lC1@PAGE
	add	x21, x0, lC1@PAGEOFF;
	mov	x0, x20
	mov	x1, x21
	bl	_ada__text_io__put_line__2
LBE30:
LBB31:
	.loc 1 219 4
	adrp	x0, lC50@PAGE
	add	x22, x0, lC50@PAGEOFF;
	adrp	x0, lC2@PAGE
	add	x23, x0, lC2@PAGEOFF;
	mov	x0, x22
	mov	x1, x23
	bl	_ada__text_io__put_line__2
LBE31:
LBB32:
	.loc 1 220 4
	adrp	x0, lC51@PAGE
	add	x24, x0, lC51@PAGEOFF;
	adrp	x0, lC3@PAGE
	add	x25, x0, lC3@PAGEOFF;
	mov	x0, x24
	mov	x1, x25
	bl	_ada__text_io__put_line__2
LBE32:
LBB33:
	.loc 1 221 4
	adrp	x0, lC52@PAGE
	add	x26, x0, lC52@PAGEOFF;
	adrp	x0, lC2@PAGE
	add	x27, x0, lC2@PAGEOFF;
	mov	x0, x26
	mov	x1, x27
	bl	_ada__text_io__put_line__2
LBE33:
LBB34:
	.loc 1 222 4
	adrp	x0, lC53@PAGE
	add	x0, x0, lC53@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 16112]
	adrp	x0, lC4@PAGE
	add	x0, x0, lC4@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 16120]
	sub	x0, x29, #512
	ldp	x0, x1, [x0, 240]
	bl	_ada__text_io__put_line__2
LBE34:
LBB35:
	.loc 1 223 4
	adrp	x0, lC54@PAGE
	add	x0, x0, lC54@PAGEOFF;
	str	x0, [x29, -256]
	adrp	x0, lC4@PAGE
	add	x0, x0, lC4@PAGEOFF;
	str	x0, [x29, -248]
	ldp	x0, x1, [x29, -256]
	bl	_ada__text_io__put_line__2
LBE35:
LBB36:
	.loc 1 224 4
	adrp	x0, lC55@PAGE
	add	x0, x0, lC55@PAGEOFF;
	str	x0, [x29, -240]
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	str	x0, [x29, -232]
	ldp	x0, x1, [x29, -240]
	bl	_ada__text_io__put_line__2
LBE36:
LBB37:
	.loc 1 225 4
	adrp	x0, lC48@PAGE
	add	x0, x0, lC48@PAGEOFF;
	str	x0, [x29, -224]
	adrp	x0, lC0@PAGE
	add	x0, x0, lC0@PAGEOFF;
	str	x0, [x29, -216]
	ldp	x0, x1, [x29, -224]
	bl	_ada__text_io__put_line__2
LBE37:
LBB38:
	.loc 1 228 4
	adrp	x0, lC56@PAGE
	add	x0, x0, lC56@PAGEOFF;
	str	x0, [x29, -208]
	adrp	x0, lC5@PAGE
	add	x0, x0, lC5@PAGEOFF;
	str	x0, [x29, -200]
	ldp	x0, x1, [x29, -208]
	bl	_ada__text_io__put_line__2
LBE38:
	.loc 1 229 4
	bl	_OQS_init
LBB39:
	.loc 1 230 4
	adrp	x0, lC57@PAGE
	add	x0, x0, lC57@PAGEOFF;
	str	x0, [x29, -192]
	adrp	x0, lC5@PAGE
	add	x0, x0, lC5@PAGEOFF;
	str	x0, [x29, -184]
	ldp	x0, x1, [x29, -192]
	bl	_ada__text_io__put_line__2
LBE39:
	.loc 1 231 4
	mov	w0, 1
	bl	_ada__text_io__new_line__2
	.loc 1 234 4
	sub	x0, x29, #24
	mov	x16, x0
	bl	_test_pqc__test_ml_kem.0
	.loc 1 235 4
	sub	x0, x29, #24
	mov	x16, x0
	bl	_test_pqc__test_ml_dsa.1
LBB40:
	.loc 1 238 4
	adrp	x0, lC31@PAGE
	add	x0, x0, lC31@PAGEOFF;
	str	x0, [x29, -176]
	adrp	x0, lC6@PAGE
	add	x0, x0, lC6@PAGEOFF;
	str	x0, [x29, -168]
	ldp	x0, x1, [x29, -176]
	bl	_ada__text_io__put_line__2
LBE40:
LBB41:
	.loc 1 239 4
	adrp	x0, lC58@PAGE
	add	x0, x0, lC58@PAGEOFF;
	str	x0, [x29, -160]
	adrp	x0, lC7@PAGE
	add	x0, x0, lC7@PAGEOFF;
	str	x0, [x29, -152]
	ldp	x0, x1, [x29, -160]
	bl	_ada__text_io__put_line__2
LBE41:
LBB42:
	.loc 1 240 4
	adrp	x0, lC31@PAGE
	add	x0, x0, lC31@PAGEOFF;
	str	x0, [x29, -144]
	adrp	x0, lC6@PAGE
	add	x0, x0, lC6@PAGEOFF;
	str	x0, [x29, -136]
	ldp	x0, x1, [x29, -144]
	bl	_ada__text_io__put_line__2
LBE42:
LBB43:
	.loc 1 241 4
	adrp	x0, lC48@PAGE
	add	x0, x0, lC48@PAGEOFF;
	str	x0, [x29, -128]
	adrp	x0, lC0@PAGE
	add	x0, x0, lC0@PAGEOFF;
	str	x0, [x29, -120]
	ldp	x0, x1, [x29, -128]
	bl	_ada__text_io__put_line__2
LBE43:
LBB44:
	.loc 1 242 4
	adrp	x0, lC59@PAGE
	add	x0, x0, lC59@PAGEOFF;
	str	x0, [x29, -112]
	adrp	x0, lC8@PAGE
	add	x0, x0, lC8@PAGEOFF;
	str	x0, [x29, -104]
	ldp	x0, x1, [x29, -112]
	bl	_ada__text_io__put_line__2
LBE44:
LBB45:
	.loc 1 243 4
	adrp	x0, lC60@PAGE
	add	x0, x0, lC60@PAGEOFF;
	str	x0, [x29, -96]
	adrp	x0, lC9@PAGE
	add	x0, x0, lC9@PAGEOFF;
	str	x0, [x29, -88]
	ldp	x0, x1, [x29, -96]
	bl	_ada__text_io__put_line__2
LBE45:
LBB46:
	.loc 1 244 4
	adrp	x0, lC61@PAGE
	add	x0, x0, lC61@PAGEOFF;
	str	x0, [x29, -80]
	adrp	x0, lC10@PAGE
	add	x0, x0, lC10@PAGEOFF;
	str	x0, [x29, -72]
	ldp	x0, x1, [x29, -80]
	bl	_ada__text_io__put_line__2
LBE46:
LBB47:
	.loc 1 245 4
	adrp	x0, lC48@PAGE
	add	x0, x0, lC48@PAGEOFF;
	str	x0, [x29, -64]
	adrp	x0, lC0@PAGE
	add	x0, x0, lC0@PAGEOFF;
	str	x0, [x29, -56]
	ldp	x0, x1, [x29, -64]
	bl	_ada__text_io__put_line__2
LBE47:
LBB48:
	.loc 1 246 4
	adrp	x0, lC62@PAGE
	add	x0, x0, lC62@PAGEOFF;
	str	x0, [x29, -48]
	adrp	x0, lC11@PAGE
	add	x0, x0, lC11@PAGEOFF;
	str	x0, [x29, -40]
	ldp	x0, x1, [x29, -48]
	bl	_ada__text_io__put_line__2
LBE48:
	.loc 1 249 4
	bl	_OQS_destroy
	.loc 1 251 5
	nop
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L20
	bl	___stack_chk_fail
L20:
	ldp	x29, x30, [sp, 272]
	ldp	x20, x21, [sp, 288]
	ldp	x22, x23, [sp, 304]
	ldp	x24, x25, [sp, 320]
	ldp	x26, x27, [sp, 336]
	add	sp, sp, 352
LCFI8:
	ret
LFE1:
	.const
	.align	2
lC0:
	.word	1
	.word	0
	.align	2
lC1:
	.word	1
	.word	180
	.align	2
lC2:
	.word	1
	.word	63
	.align	2
lC3:
	.word	1
	.word	64
	.align	2
lC4:
	.word	1
	.word	65
	.align	2
lC5:
	.word	1
	.word	22
	.align	2
lC7:
	.word	1
	.word	20
	.align	2
lC8:
	.word	1
	.word	49
	.align	2
lC9:
	.word	1
	.word	47
	.align	2
lC10:
	.word	1
	.word	31
	.align	2
lC11:
	.word	1
	.word	54
	.text
	.const
	.align	3
lC63:
	.ascii " Testing ML-DSA-87 (NIST Level 5)"
	.align	3
lC64:
	.ascii "1. Generating signing keypair... "
	.align	3
lC65:
	.ascii "   Public key:  2,592 bytes"
	.align	3
lC66:
	.ascii "   Secret key:  4,896 bytes (VALID)"
	.align	3
lC67:
	.ascii "2. Signing message \"Hello, PQC!\"... "
	.align	3
lC68:
	.ascii "   Signature:   4,627 bytes"
	.align	3
lC69:
	.ascii "3. Verifying signature... "
	.align	3
lC70:
	.ascii "   Signature is VALID and authentic"
	.align	3
lC71:
	.ascii "\342\234\227 FAILED - Signature verification failed!"
	.align	3
lC72:
	.ascii "4. Verifying with tampered message... "
	.align	3
lC73:
	.ascii "   Signature correctly REJECTED for tampered message"
	.align	3
lC74:
	.ascii "\342\234\227 FAILED - Signature should have been rejected!"
	.align	3
lC75:
	.ascii "5. Securely zeroizing signing key... "
	.align	3
lC76:
	.ascii "   Secret key marked as INVALID"
	.text
	.align	2
_test_pqc__test_ml_dsa.1:
LFB3:
	.loc 1 117 4
	sub	x10, sp, #16384
	str	xzr, [x10]
	sub	x10, x10, #4096
	str	xzr, [x10]
	sub	x10, x10, #4096
	str	xzr, [x10]
	sub	x10, x10, #4096
	str	xzr, [x10, 3744]
	stp	x29, x30, [sp, -96]!
LCFI9:
	mov	x29, sp
LCFI10:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
	stp	x25, x26, [sp, 64]
	str	x27, [sp, 80]
	mov	x13, 12544
	sub	sp, sp, x13
LCFI11:
	mov	x19, x16
	sub	x0, x29, #16384
	str	x16, [x0, 4216]
	.loc 1 117 4
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x1, [x0]
	str	x1, [x29, -8]
	mov	x1, 0
	.loc 1 120 7
	sub	x0, x29, #4096
	sub	x0, x0, #816
	bl	_anubis_types__ml_dsa_secret_keyIP
LBB49:
	.loc 1 140 7
	adrp	x0, lC31@PAGE
	add	x20, x0, lC31@PAGEOFF;
	adrp	x0, lC6@PAGE
	add	x21, x0, lC6@PAGEOFF;
	mov	x0, x20
	mov	x1, x21
	bl	_ada__text_io__put_line__2
LBE49:
LBB50:
	.loc 1 141 7
	adrp	x0, lC63@PAGE
	add	x22, x0, lC63@PAGEOFF;
	adrp	x0, lC12@PAGE
	add	x23, x0, lC12@PAGEOFF;
	mov	x0, x22
	mov	x1, x23
	bl	_ada__text_io__put_line__2
LBE50:
LBB51:
	.loc 1 142 7
	adrp	x0, lC31@PAGE
	add	x24, x0, lC31@PAGEOFF;
	adrp	x0, lC6@PAGE
	add	x25, x0, lC6@PAGEOFF;
	mov	x0, x24
	mov	x1, x25
	bl	_ada__text_io__put_line__2
LBE51:
	.loc 1 143 7
	mov	w0, 1
	bl	_ada__text_io__new_line__2
LBB52:
	.loc 1 146 7
	adrp	x0, lC64@PAGE
	add	x26, x0, lC64@PAGEOFF;
	adrp	x0, lC12@PAGE
	add	x27, x0, lC12@PAGEOFF;
	mov	x0, x26
	mov	x1, x27
	bl	_ada__text_io__put__4
LBE52:
	.loc 1 147 7
	sub	x1, x29, #4096
	sub	x1, x1, #816
	sub	x0, x29, #8192
	sub	x0, x0, #3944
	bl	_anubis_types__pqc__ml_dsa_generate_keypair
	.loc 1 147 7 is_stmt 0 discriminator 1
	strb	w0, [x19]
	.loc 1 149 7 is_stmt 1
	ldrb	w0, [x19]
	cmp	w0, 0
	beq	L22
	.loc 1 149 39 discriminator 1
	sub	x0, x29, #4096
	sub	x0, x0, #816
	bl	_anubis_types__is_valid__8
	.loc 1 149 18 discriminator 2
	cmp	w0, 0
	beq	L22
LBB53:
	.loc 1 150 10
	adrp	x0, lC34@PAGE
	add	x0, x0, lC34@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3840]
	adrp	x0, lC13@PAGE
	add	x0, x0, lC13@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3848]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -256]
	bl	_ada__text_io__put_line__2
LBE53:
LBB54:
	.loc 1 151 10
	adrp	x0, lC65@PAGE
	add	x0, x0, lC65@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3856]
	adrp	x0, lC14@PAGE
	add	x0, x0, lC14@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3864]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -240]
	bl	_ada__text_io__put_line__2
LBE54:
LBB55:
	.loc 1 152 10
	adrp	x0, lC66@PAGE
	add	x0, x0, lC66@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3872]
	adrp	x0, lC15@PAGE
	add	x0, x0, lC15@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3880]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -224]
	bl	_ada__text_io__put_line__2
LBE55:
	nop
	.loc 1 157 7
	mov	w0, 1
	bl	_ada__text_io__new_line__2
LBB56:
	.loc 1 160 7
	adrp	x0, lC67@PAGE
	add	x0, x0, lC67@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3904]
	adrp	x0, lC17@PAGE
	add	x0, x0, lC17@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3912]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -192]
	bl	_ada__text_io__put__4
LBE56:
	.loc 1 161 7
	adrp	x0, _message.2@PAGE
	add	x0, x0, _message.2@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3920]
	adrp	x0, lC18@PAGE
	add	x0, x0, lC18@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3928]
	sub	x1, x29, #8192
	sub	x1, x1, #1352
	sub	x0, x29, #4096
	sub	x0, x0, #816
	mov	x3, x1
	mov	x2, x0
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -176]
	bl	_anubis_types__pqc__ml_dsa_sign
	.loc 1 161 7 is_stmt 0 discriminator 1
	strb	w0, [x19]
	.loc 1 163 7 is_stmt 1
	ldrb	w0, [x19]
	cmp	w0, 0
	beq	L25
	b	L36
L22:
LBB57:
	.loc 1 154 10
	adrp	x0, lC37@PAGE
	add	x0, x0, lC37@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3888]
	adrp	x0, lC16@PAGE
	add	x0, x0, lC16@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3896]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -208]
	bl	_ada__text_io__put_line__2
LBE57:
	.loc 1 155 10
	b	L21
L36:
LBB58:
	.loc 1 164 10
	adrp	x0, lC34@PAGE
	add	x0, x0, lC34@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3936]
	adrp	x0, lC13@PAGE
	add	x0, x0, lC13@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3944]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -160]
	bl	_ada__text_io__put_line__2
LBE58:
LBB59:
	.loc 1 165 10
	adrp	x0, lC68@PAGE
	add	x0, x0, lC68@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3952]
	adrp	x0, lC14@PAGE
	add	x0, x0, lC14@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3960]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -144]
	bl	_ada__text_io__put_line__2
LBE59:
	.loc 1 171 7
	mov	w0, 1
	bl	_ada__text_io__new_line__2
LBB60:
	.loc 1 174 7
	adrp	x0, lC69@PAGE
	add	x0, x0, lC69@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3984]
	adrp	x0, lC19@PAGE
	add	x0, x0, lC19@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3992]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -112]
	bl	_ada__text_io__put__4
LBE60:
	.loc 1 175 23
	adrp	x0, _message.2@PAGE
	add	x0, x0, _message.2@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4000]
	adrp	x0, lC18@PAGE
	add	x0, x0, lC18@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4008]
	sub	x1, x29, #8192
	sub	x1, x1, #3944
	sub	x0, x29, #8192
	sub	x0, x0, #1352
	mov	x3, x1
	mov	x2, x0
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -96]
	bl	_anubis_types__pqc__ml_dsa_verify
	.loc 1 175 23 is_stmt 0 discriminator 1
	sub	x1, x29, #12288
	strb	w0, [x1, 135]
	.loc 1 177 7 is_stmt 1
	sub	x0, x29, #12288
	ldrb	w0, [x0, 135]
	cmp	w0, 0
	bne	L26
	b	L37
L25:
LBB61:
	.loc 1 167 10
	adrp	x0, lC37@PAGE
	add	x0, x0, lC37@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3968]
	adrp	x0, lC16@PAGE
	add	x0, x0, lC16@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3976]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -128]
	bl	_ada__text_io__put_line__2
LBE61:
	.loc 1 168 10
	sub	x0, x29, #4096
	sub	x0, x0, #816
	bl	_anubis_types__pqc__zeroize_ml_dsa_secret
	.loc 1 169 10
	b	L21
L26:
LBB62:
	.loc 1 178 10
	adrp	x0, lC34@PAGE
	add	x0, x0, lC34@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4016]
	adrp	x0, lC13@PAGE
	add	x0, x0, lC13@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4024]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -80]
	bl	_ada__text_io__put_line__2
LBE62:
LBB63:
	.loc 1 179 10
	adrp	x0, lC70@PAGE
	add	x0, x0, lC70@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4032]
	adrp	x0, lC15@PAGE
	add	x0, x0, lC15@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4040]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -64]
	bl	_ada__text_io__put_line__2
	b	L28
L37:
LBE63:
LBB64:
	.loc 1 181 10
	adrp	x0, lC71@PAGE
	add	x0, x0, lC71@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4048]
	adrp	x0, lC20@PAGE
	add	x0, x0, lC20@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4056]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -48]
	bl	_ada__text_io__put_line__2
L28:
LBE64:
	.loc 1 183 7
	mov	w0, 1
	bl	_ada__text_io__new_line__2
LBB65:
	.loc 1 186 7
	adrp	x0, lC72@PAGE
	add	x0, x0, lC72@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4064]
	adrp	x0, lC21@PAGE
	add	x0, x0, lC21@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4072]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -32]
	bl	_ada__text_io__put__4
LBE65:
LBB66:
	.loc 1 188 10
	mov	w0, 72
	sub	x1, x29, #12288
	strb	w0, [x1, 136]
	mov	w0, 101
	sub	x1, x29, #12288
	strb	w0, [x1, 137]
	mov	w0, 108
	sub	x1, x29, #12288
	strb	w0, [x1, 138]
	mov	w0, 108
	sub	x1, x29, #12288
	strb	w0, [x1, 139]
	mov	w0, 111
	sub	x1, x29, #12288
	strb	w0, [x1, 140]
	mov	w0, 44
	sub	x1, x29, #12288
	strb	w0, [x1, 141]
	mov	w0, 32
	sub	x1, x29, #12288
	strb	w0, [x1, 142]
	mov	w0, 80
	sub	x1, x29, #12288
	strb	w0, [x1, 143]
	mov	w0, 81
	sub	x1, x29, #12288
	strb	w0, [x1, 144]
	mov	w0, 67
	sub	x1, x29, #12288
	strb	w0, [x1, 145]
	mov	w0, 33
	sub	x1, x29, #12288
	strb	w0, [x1, 146]
	.loc 1 190 52
	mov	w0, 88
	sub	x1, x29, #12288
	strb	w0, [x1, 136]
	.loc 1 191 26
	sub	x0, x29, #8192
	sub	x0, x0, #3960
	sub	x1, x29, #16384
	str	x0, [x1, 4080]
	adrp	x0, lC18@PAGE
	add	x0, x0, lC18@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4088]
	sub	x1, x29, #8192
	sub	x1, x1, #3944
	sub	x0, x29, #8192
	sub	x0, x0, #1352
	mov	x3, x1
	mov	x2, x0
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -16]
	bl	_anubis_types__pqc__ml_dsa_verify
	.loc 1 191 26 is_stmt 0 discriminator 1
	sub	x1, x29, #12288
	strb	w0, [x1, 135]
	.loc 1 193 13 is_stmt 1
	sub	x0, x29, #12288
	ldrb	w0, [x0, 135]
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 193 10
	cmp	w0, 0
	beq	L29
LBB67:
	.loc 1 194 13
	adrp	x0, lC34@PAGE
	add	x0, x0, lC34@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4096]
	adrp	x0, lC13@PAGE
	add	x0, x0, lC13@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4104]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0]
	bl	_ada__text_io__put_line__2
LBE67:
LBB68:
	.loc 1 195 13
	adrp	x0, lC73@PAGE
	add	x0, x0, lC73@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4112]
	adrp	x0, lC22@PAGE
	add	x0, x0, lC22@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4120]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, 16]
	bl	_ada__text_io__put_line__2
	b	L35
L29:
LBE68:
LBB69:
	.loc 1 197 13
	adrp	x0, lC74@PAGE
	add	x0, x0, lC74@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4128]
	adrp	x0, lC8@PAGE
	add	x0, x0, lC8@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4136]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, 32]
	bl	_ada__text_io__put_line__2
L35:
LBE69:
LBE66:
	.loc 1 200 7
	mov	w0, 1
	bl	_ada__text_io__new_line__2
LBB70:
	.loc 1 203 7
	adrp	x0, lC75@PAGE
	add	x0, x0, lC75@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4144]
	adrp	x0, lC23@PAGE
	add	x0, x0, lC23@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4152]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, 48]
	bl	_ada__text_io__put__4
LBE70:
	.loc 1 204 7
	sub	x0, x29, #4096
	sub	x0, x0, #816
	bl	_anubis_types__pqc__zeroize_ml_dsa_secret
	.loc 1 206 26
	sub	x0, x29, #4096
	sub	x0, x0, #816
	bl	_anubis_types__is_valid__8
	.loc 1 206 26 is_stmt 0 discriminator 1
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 206 7 is_stmt 1 discriminator 1
	cmp	w0, 0
	beq	L31
LBB71:
	.loc 1 207 10
	adrp	x0, lC34@PAGE
	add	x0, x0, lC34@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4160]
	adrp	x0, lC13@PAGE
	add	x0, x0, lC13@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4168]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, 64]
	bl	_ada__text_io__put_line__2
LBE71:
LBB72:
	.loc 1 208 10
	adrp	x0, lC76@PAGE
	add	x0, x0, lC76@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4176]
	adrp	x0, lC10@PAGE
	add	x0, x0, lC10@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4184]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, 80]
	bl	_ada__text_io__put_line__2
	b	L32
L31:
LBE72:
LBB73:
	.loc 1 210 10
	adrp	x0, lC47@PAGE
	add	x0, x0, lC47@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4192]
	adrp	x0, lC24@PAGE
	add	x0, x0, lC24@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4200]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, 96]
	bl	_ada__text_io__put_line__2
L32:
LBE73:
	.loc 1 212 7
	mov	w0, 1
	bl	_ada__text_io__new_line__2
	.loc 1 214 8
	nop
L21:
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L34
	bl	___stack_chk_fail
L34:
	mov	sp, x29
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldr	x27, [sp, 80]
	ldp	x29, x30, [sp], 96
LCFI12:
	ret
LFE3:
	.const
	.align	2
lC12:
	.word	1
	.word	33
	.align	2
lC17:
	.word	1
	.word	36
	.align	2
lC18:
	.word	0
	.word	10
	.align	2
lC19:
	.word	1
	.word	26
	.align	2
lC20:
	.word	1
	.word	43
	.align	2
lC21:
	.word	1
	.word	38
	.align	2
lC22:
	.word	1
	.word	52
	.text
	.const
	.align	3
_message.2:
	.byte	72
	.byte	101
	.byte	108
	.byte	108
	.byte	111
	.byte	44
	.byte	32
	.byte	80
	.byte	81
	.byte	67
	.byte	33
	.space 5
	.section __DWARF,__debug_frame,regular,debug
Lsection__debug_frame:
Lframe0:
	.set L$set$0,LECIE0-LSCIE0
	.long L$set$0
LSCIE0:
	.long	0xffffffff
	.byte	0x3
	.ascii "\0"
	.uleb128 0x1
	.sleb128 -8
	.uleb128 0x1e
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LECIE0:
LSFDE0:
	.set L$set$1,LEFDE0-LASFDE0
	.long L$set$1
LASFDE0:
	.set L$set$2,Lframe0-Lsection__debug_frame
	.long L$set$2
	.quad	LFB2
	.set L$set$3,LFE2-LFB2
	.quad L$set$3
	.byte	0x4
	.set L$set$4,LCFI0-LFB2
	.long L$set$4
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$5,LCFI1-LCFI0
	.long L$set$5
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$6,LCFI2-LCFI1
	.long L$set$6
	.byte	0x93
	.uleb128 0xa
	.byte	0x94
	.uleb128 0x9
	.byte	0x95
	.uleb128 0x8
	.byte	0x96
	.uleb128 0x7
	.byte	0x97
	.uleb128 0x6
	.byte	0x98
	.uleb128 0x5
	.byte	0x99
	.uleb128 0x4
	.byte	0x9a
	.uleb128 0x3
	.byte	0x9b
	.uleb128 0x2
	.byte	0x4
	.set L$set$7,LCFI3-LCFI2
	.long L$set$7
	.byte	0xde
	.byte	0xdd
	.byte	0xdb
	.byte	0xd9
	.byte	0xda
	.byte	0xd7
	.byte	0xd8
	.byte	0xd5
	.byte	0xd6
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE0:
LSFDE2:
	.set L$set$8,LEFDE2-LASFDE2
	.long L$set$8
LASFDE2:
	.set L$set$9,Lframe0-Lsection__debug_frame
	.long L$set$9
	.quad	LFB1
	.set L$set$10,LFE1-LFB1
	.quad L$set$10
	.byte	0x4
	.set L$set$11,LCFI4-LFB1
	.long L$set$11
	.byte	0xe
	.uleb128 0x160
	.byte	0x4
	.set L$set$12,LCFI5-LCFI4
	.long L$set$12
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$13,LCFI6-LCFI5
	.long L$set$13
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x50
	.byte	0x4
	.set L$set$14,LCFI7-LCFI6
	.long L$set$14
	.byte	0x94
	.uleb128 0x8
	.byte	0x95
	.uleb128 0x7
	.byte	0x96
	.uleb128 0x6
	.byte	0x97
	.uleb128 0x5
	.byte	0x98
	.uleb128 0x4
	.byte	0x99
	.uleb128 0x3
	.byte	0x9a
	.uleb128 0x2
	.byte	0x9b
	.uleb128 0x1
	.byte	0x4
	.set L$set$15,LCFI8-LCFI7
	.long L$set$15
	.byte	0xda
	.byte	0xdb
	.byte	0xd8
	.byte	0xd9
	.byte	0xd6
	.byte	0xd7
	.byte	0xd4
	.byte	0xd5
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE2:
LSFDE4:
	.set L$set$16,LEFDE4-LASFDE4
	.long L$set$16
LASFDE4:
	.set L$set$17,Lframe0-Lsection__debug_frame
	.long L$set$17
	.quad	LFB3
	.set L$set$18,LFE3-LFB3
	.quad L$set$18
	.byte	0x4
	.set L$set$19,LCFI9-LFB3
	.long L$set$19
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$20,LCFI10-LCFI9
	.long L$set$20
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$21,LCFI11-LCFI10
	.long L$set$21
	.byte	0x93
	.uleb128 0xa
	.byte	0x94
	.uleb128 0x9
	.byte	0x95
	.uleb128 0x8
	.byte	0x96
	.uleb128 0x7
	.byte	0x97
	.uleb128 0x6
	.byte	0x98
	.uleb128 0x5
	.byte	0x99
	.uleb128 0x4
	.byte	0x9a
	.uleb128 0x3
	.byte	0x9b
	.uleb128 0x2
	.byte	0x4
	.set L$set$22,LCFI12-LCFI11
	.long L$set$22
	.byte	0xde
	.byte	0xdd
	.byte	0xdb
	.byte	0xd9
	.byte	0xda
	.byte	0xd7
	.byte	0xd8
	.byte	0xd5
	.byte	0xd6
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE4:
	.section __TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
EH_frame1:
	.set L$set$23,LECIE1-LSCIE1
	.long L$set$23
LSCIE1:
	.long	0
	.byte	0x3
	.ascii "zR\0"
	.uleb128 0x1
	.sleb128 -8
	.uleb128 0x1e
	.uleb128 0x1
	.byte	0x10
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LECIE1:
LSFDE7:
	.set L$set$24,LEFDE7-LASFDE7
	.long L$set$24
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB2-.
	.set L$set$25,LFE2-LFB2
	.quad L$set$25
	.uleb128 0
	.byte	0x4
	.set L$set$26,LCFI0-LFB2
	.long L$set$26
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$27,LCFI1-LCFI0
	.long L$set$27
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$28,LCFI2-LCFI1
	.long L$set$28
	.byte	0x93
	.uleb128 0xa
	.byte	0x94
	.uleb128 0x9
	.byte	0x95
	.uleb128 0x8
	.byte	0x96
	.uleb128 0x7
	.byte	0x97
	.uleb128 0x6
	.byte	0x98
	.uleb128 0x5
	.byte	0x99
	.uleb128 0x4
	.byte	0x9a
	.uleb128 0x3
	.byte	0x9b
	.uleb128 0x2
	.byte	0x4
	.set L$set$29,LCFI3-LCFI2
	.long L$set$29
	.byte	0xde
	.byte	0xdd
	.byte	0xdb
	.byte	0xd9
	.byte	0xda
	.byte	0xd7
	.byte	0xd8
	.byte	0xd5
	.byte	0xd6
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$30,LEFDE9-LASFDE9
	.long L$set$30
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB1-.
	.set L$set$31,LFE1-LFB1
	.quad L$set$31
	.uleb128 0
	.byte	0x4
	.set L$set$32,LCFI4-LFB1
	.long L$set$32
	.byte	0xe
	.uleb128 0x160
	.byte	0x4
	.set L$set$33,LCFI5-LCFI4
	.long L$set$33
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$34,LCFI6-LCFI5
	.long L$set$34
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x50
	.byte	0x4
	.set L$set$35,LCFI7-LCFI6
	.long L$set$35
	.byte	0x94
	.uleb128 0x8
	.byte	0x95
	.uleb128 0x7
	.byte	0x96
	.uleb128 0x6
	.byte	0x97
	.uleb128 0x5
	.byte	0x98
	.uleb128 0x4
	.byte	0x99
	.uleb128 0x3
	.byte	0x9a
	.uleb128 0x2
	.byte	0x9b
	.uleb128 0x1
	.byte	0x4
	.set L$set$36,LCFI8-LCFI7
	.long L$set$36
	.byte	0xda
	.byte	0xdb
	.byte	0xd8
	.byte	0xd9
	.byte	0xd6
	.byte	0xd7
	.byte	0xd4
	.byte	0xd5
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE9:
LSFDE11:
	.set L$set$37,LEFDE11-LASFDE11
	.long L$set$37
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB3-.
	.set L$set$38,LFE3-LFB3
	.quad L$set$38
	.uleb128 0
	.byte	0x4
	.set L$set$39,LCFI9-LFB3
	.long L$set$39
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$40,LCFI10-LCFI9
	.long L$set$40
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$41,LCFI11-LCFI10
	.long L$set$41
	.byte	0x93
	.uleb128 0xa
	.byte	0x94
	.uleb128 0x9
	.byte	0x95
	.uleb128 0x8
	.byte	0x96
	.uleb128 0x7
	.byte	0x97
	.uleb128 0x6
	.byte	0x98
	.uleb128 0x5
	.byte	0x99
	.uleb128 0x4
	.byte	0x9a
	.uleb128 0x3
	.byte	0x9b
	.uleb128 0x2
	.byte	0x4
	.set L$set$42,LCFI12-LCFI11
	.long L$set$42
	.byte	0xde
	.byte	0xdd
	.byte	0xdb
	.byte	0xd9
	.byte	0xda
	.byte	0xd7
	.byte	0xd8
	.byte	0xd5
	.byte	0xd6
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE11:
	.text
Letext0:
	.file 2 "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_types.ads"
	.section __DWARF,__debug_info,regular,debug
Lsection__debug_info:
Ldebug_info0:
	.long	0x7d1
	.short	0x4
	.set L$set$43,Ldebug_abbrev0-Lsection__debug_abbrev
	.long L$set$43
	.byte	0x8
	.uleb128 0x1
	.ascii "GNU Ada 14.1.0 -O0 -gnatA -g -gnat2012 -gnatwa -gnatwe -fstack-check=specific -fstack-protector-strong -gnatR2js -gnatws -gnatec=/private/var/folders/bg/pt9l6y1j47q642kp3z5blrmh0000gn/T/GPR.71672/GNAT-TEMP-000003.TMP -gnatem=/private/var/folders/bg/pt9l6y1j47q642kp3z5blrmh0000gn/T/GPR.71672/GNAT-TEMP-000004.TMP -mmacosx-version-min=16.0.0 -mcpu=apple-m1 -mlittle-endian -mabi=lp64 -fPIC\0"
	.byte	0xd
	.ascii "/Users/sicarii/Desktop/anubis-spark/src/test_pqc.adb\0"
	.ascii "/Users/sicarii/Desktop/anubis-spark/obj/gnatprove/data_representation\0"
	.quad	Ltext0
	.set L$set$44,Letext0-Ltext0
	.quad L$set$44
	.set L$set$45,Ldebug_line0-Lsection__debug_line
	.long L$set$45
	.uleb128 0x2
	.byte	0x1
	.byte	0x7
	.ascii "interfaces__unsigned_8\0"
	.uleb128 0x3
	.byte	0
	.byte	0xff
	.ascii "anubis_types__byte\0"
	.long	0x255
	.uleb128 0x4
	.byte	0x1
	.byte	0x7
	.ascii "anubis_types__TbyteB\0"
	.uleb128 0x2
	.byte	0x4
	.byte	0x5
	.ascii "integer\0"
	.uleb128 0x5
	.ascii "anubis_types__ml_kem_public_key__T33s\0"
	.long	0x23b
	.long	0x2af
	.uleb128 0x6
	.long	0x26d
	.sleb128 1568
	.byte	0
	.uleb128 0x7
	.ascii "anubis_types__ml_kem_public_key\0"
	.short	0x620
	.byte	0x2
	.byte	0xb8
	.byte	0x9
	.long	0x2e7
	.uleb128 0x8
	.set L$set$46,LASF0-Lsection__debug_str
	.long L$set$46
	.byte	0x2
	.byte	0xb9
	.byte	0x7
	.long	0x278
	.byte	0
	.byte	0
	.uleb128 0x5
	.ascii "anubis_types__ml_kem_secret_key__T35s\0"
	.long	0x23b
	.long	0x31e
	.uleb128 0x6
	.long	0x26d
	.sleb128 3168
	.byte	0
	.uleb128 0x7
	.ascii "anubis_types__ml_kem_secret_key\0"
	.short	0xc61
	.byte	0x2
	.byte	0xbc
	.byte	0x9
	.long	0x366
	.uleb128 0x8
	.set L$set$47,LASF0-Lsection__debug_str
	.long L$set$47
	.byte	0x2
	.byte	0xbd
	.byte	0x7
	.long	0x2e7
	.byte	0
	.uleb128 0x9
	.ascii "valid\0"
	.byte	0x2
	.byte	0xbe
	.byte	0x7
	.long	0x366
	.short	0xc60
	.byte	0
	.uleb128 0x2
	.byte	0x1
	.byte	0x2
	.ascii "boolean\0"
	.uleb128 0x5
	.ascii "anubis_types__ml_kem_ciphertext__T37s\0"
	.long	0x23b
	.long	0x3a8
	.uleb128 0x6
	.long	0x26d
	.sleb128 1568
	.byte	0
	.uleb128 0x7
	.ascii "anubis_types__ml_kem_ciphertext\0"
	.short	0x620
	.byte	0x2
	.byte	0xc1
	.byte	0x9
	.long	0x3e0
	.uleb128 0x8
	.set L$set$48,LASF0-Lsection__debug_str
	.long L$set$48
	.byte	0x2
	.byte	0xc2
	.byte	0x7
	.long	0x371
	.byte	0
	.byte	0
	.uleb128 0x5
	.ascii "anubis_types__ml_kem_shared_secret__T39s\0"
	.long	0x23b
	.long	0x419
	.uleb128 0x6
	.long	0x26d
	.sleb128 32
	.byte	0
	.uleb128 0xa
	.ascii "anubis_types__ml_kem_shared_secret\0"
	.byte	0x21
	.byte	0x2
	.byte	0xc5
	.byte	0x9
	.long	0x462
	.uleb128 0x8
	.set L$set$49,LASF0-Lsection__debug_str
	.long L$set$49
	.byte	0x2
	.byte	0xc6
	.byte	0x7
	.long	0x3e0
	.byte	0
	.uleb128 0xb
	.ascii "valid\0"
	.byte	0x2
	.byte	0xc7
	.byte	0x7
	.long	0x366
	.byte	0x20
	.byte	0
	.uleb128 0x4
	.byte	0x4
	.byte	0x5
	.ascii "ada__text_io__TcountB\0"
	.uleb128 0x5
	.ascii "anubis_types__ml_dsa_public_key__T41s\0"
	.long	0x23b
	.long	0x4b2
	.uleb128 0x6
	.long	0x26d
	.sleb128 2592
	.byte	0
	.uleb128 0x7
	.ascii "anubis_types__ml_dsa_public_key\0"
	.short	0xa20
	.byte	0x2
	.byte	0xca
	.byte	0x9
	.long	0x4ea
	.uleb128 0x8
	.set L$set$50,LASF0-Lsection__debug_str
	.long L$set$50
	.byte	0x2
	.byte	0xcb
	.byte	0x7
	.long	0x47b
	.byte	0
	.byte	0
	.uleb128 0x5
	.ascii "anubis_types__ml_dsa_secret_key__T43s\0"
	.long	0x23b
	.long	0x521
	.uleb128 0x6
	.long	0x26d
	.sleb128 4896
	.byte	0
	.uleb128 0x7
	.ascii "anubis_types__ml_dsa_secret_key\0"
	.short	0x1321
	.byte	0x2
	.byte	0xce
	.byte	0x9
	.long	0x569
	.uleb128 0x8
	.set L$set$51,LASF0-Lsection__debug_str
	.long L$set$51
	.byte	0x2
	.byte	0xcf
	.byte	0x7
	.long	0x4ea
	.byte	0
	.uleb128 0x9
	.ascii "valid\0"
	.byte	0x2
	.byte	0xd0
	.byte	0x7
	.long	0x366
	.short	0x1320
	.byte	0
	.uleb128 0x5
	.ascii "anubis_types__ml_dsa_signature__T45s\0"
	.long	0x23b
	.long	0x59f
	.uleb128 0x6
	.long	0x26d
	.sleb128 4627
	.byte	0
	.uleb128 0x7
	.ascii "anubis_types__ml_dsa_signature\0"
	.short	0x1213
	.byte	0x2
	.byte	0xd3
	.byte	0x9
	.long	0x5d6
	.uleb128 0x8
	.set L$set$52,LASF0-Lsection__debug_str
	.long L$set$52
	.byte	0x2
	.byte	0xd4
	.byte	0x7
	.long	0x569
	.byte	0
	.byte	0
	.uleb128 0xc
	.ascii "test_pqc\0"
	.byte	0x1
	.byte	0xb
	.byte	0x1
	.ascii "_ada_test_pqc\0"
	.quad	LFB1
	.set L$set$53,LFE1-LFB1
	.quad L$set$53
	.uleb128 0x1
	.byte	0x9c
	.uleb128 0xd
	.ascii "success\0"
	.byte	0x1
	.byte	0xd
	.byte	0x4
	.long	0x366
	.uleb128 0x3
	.byte	0x91
	.sleb128 -104
	.uleb128 0xe
	.ascii "test_pqc__test_ml_kem\0"
	.byte	0x1
	.byte	0x12
	.byte	0x4
	.quad	LFB2
	.set L$set$54,LFE2-LFB2
	.quad L$set$54
	.uleb128 0x1
	.byte	0x9c
	.uleb128 0x8
	.byte	0x70
	.sleb128 9960
	.byte	0x6
	.byte	0x23
	.uleb128 0x8
	.byte	0x6
	.long	0x6d0
	.uleb128 0xf
	.set L$set$55,LASF1-Lsection__debug_str
	.long L$set$55
	.byte	0x1
	.byte	0x14
	.byte	0x7
	.long	0x2af
	.uleb128 0x3
	.byte	0x91
	.sleb128 -6416
	.uleb128 0xf
	.set L$set$56,LASF2-Lsection__debug_str
	.long L$set$56
	.byte	0x1
	.byte	0x15
	.byte	0x7
	.long	0x31e
	.uleb128 0x3
	.byte	0x91
	.sleb128 -3280
	.uleb128 0xd
	.ascii "ciphertext\0"
	.byte	0x1
	.byte	0x18
	.byte	0x7
	.long	0x3a8
	.uleb128 0x3
	.byte	0x91
	.sleb128 -4848
	.uleb128 0xd
	.ascii "bob_shared\0"
	.byte	0x1
	.byte	0x19
	.byte	0x7
	.long	0x419
	.uleb128 0x3
	.byte	0x91
	.sleb128 -6496
	.uleb128 0xd
	.ascii "alice_shared\0"
	.byte	0x1
	.byte	0x1c
	.byte	0x7
	.long	0x419
	.uleb128 0x3
	.byte	0x91
	.sleb128 -6456
	.uleb128 0x10
	.set L$set$57,Ldebug_ranges0+0-Lsection__debug_ranges
	.long L$set$57
	.uleb128 0xd
	.ascii "match\0"
	.byte	0x1
	.byte	0x52
	.byte	0xa
	.long	0x366
	.uleb128 0x3
	.byte	0x91
	.sleb128 -6497
	.byte	0
	.byte	0
	.uleb128 0x11
	.ascii "test_pqc__test_ml_dsa\0"
	.byte	0x1
	.byte	0x75
	.byte	0x4
	.quad	LFB3
	.set L$set$58,LFE3-LFB3
	.quad L$set$58
	.uleb128 0x1
	.byte	0x9c
	.uleb128 0x7
	.byte	0x70
	.sleb128 4216
	.byte	0x6
	.byte	0x23
	.uleb128 0x8
	.byte	0x6
	.uleb128 0xf
	.set L$set$59,LASF1-Lsection__debug_str
	.long L$set$59
	.byte	0x1
	.byte	0x77
	.byte	0x7
	.long	0x4b2
	.uleb128 0x4
	.byte	0x91
	.sleb128 -12232
	.uleb128 0xf
	.set L$set$60,LASF2-Lsection__debug_str
	.long L$set$60
	.byte	0x1
	.byte	0x78
	.byte	0x7
	.long	0x521
	.uleb128 0x3
	.byte	0x91
	.sleb128 -5008
	.uleb128 0x5
	.ascii "test_pqc__test_ml_dsa__T36b\0"
	.long	0x23b
	.long	0x752
	.uleb128 0x12
	.long	0x26d
	.sleb128 0
	.sleb128 10
	.byte	0
	.uleb128 0x13
	.long	0x725
	.uleb128 0xd
	.ascii "message\0"
	.byte	0x1
	.byte	0x7b
	.byte	0x7
	.long	0x752
	.uleb128 0x9
	.byte	0x3
	.quad	_message.2
	.uleb128 0xd
	.ascii "signature\0"
	.byte	0x1
	.byte	0x89
	.byte	0x7
	.long	0x59f
	.uleb128 0x4
	.byte	0x91
	.sleb128 -9640
	.uleb128 0xd
	.ascii "is_valid_sig\0"
	.byte	0x1
	.byte	0x8a
	.byte	0x7
	.long	0x366
	.uleb128 0x4
	.byte	0x91
	.sleb128 -12249
	.uleb128 0x14
	.quad	LBB66
	.set L$set$61,LBE66-LBB66
	.quad L$set$61
	.uleb128 0xd
	.ascii "tampered_message\0"
	.byte	0x1
	.byte	0xbc
	.byte	0xa
	.long	0x725
	.uleb128 0x4
	.byte	0x91
	.sleb128 -12248
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.section __DWARF,__debug_abbrev,regular,debug
Lsection__debug_abbrev:
Ldebug_abbrev0:
	.uleb128 0x1
	.uleb128 0x11
	.byte	0x1
	.uleb128 0x25
	.uleb128 0x8
	.uleb128 0x13
	.uleb128 0xb
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x1b
	.uleb128 0x8
	.uleb128 0x2134
	.uleb128 0x19
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x7
	.uleb128 0x10
	.uleb128 0x17
	.byte	0
	.byte	0
	.uleb128 0x2
	.uleb128 0x24
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x3e
	.uleb128 0xb
	.uleb128 0x3
	.uleb128 0x8
	.byte	0
	.byte	0
	.uleb128 0x3
	.uleb128 0x21
	.byte	0
	.uleb128 0x22
	.uleb128 0xb
	.uleb128 0x2f
	.uleb128 0xb
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x4
	.uleb128 0x24
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x3e
	.uleb128 0xb
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x34
	.uleb128 0x19
	.byte	0
	.byte	0
	.uleb128 0x5
	.uleb128 0x1
	.byte	0x1
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x6
	.uleb128 0x21
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2f
	.uleb128 0xd
	.byte	0
	.byte	0
	.uleb128 0x7
	.uleb128 0x13
	.byte	0x1
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0xb
	.uleb128 0x5
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x8
	.uleb128 0xd
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x38
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0x9
	.uleb128 0xd
	.byte	0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x38
	.uleb128 0x5
	.byte	0
	.byte	0
	.uleb128 0xa
	.uleb128 0x13
	.byte	0x1
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0xb
	.uleb128 0xd
	.byte	0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x38
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0xc
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x6e
	.uleb128 0x8
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x7
	.uleb128 0x40
	.uleb128 0x18
	.byte	0
	.byte	0
	.uleb128 0xd
	.uleb128 0x34
	.byte	0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0x18
	.byte	0
	.byte	0
	.uleb128 0xe
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x7
	.uleb128 0x40
	.uleb128 0x18
	.uleb128 0x48
	.uleb128 0x18
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0xf
	.uleb128 0x34
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0x18
	.byte	0
	.byte	0
	.uleb128 0x10
	.uleb128 0xb
	.byte	0x1
	.uleb128 0x55
	.uleb128 0x17
	.byte	0
	.byte	0
	.uleb128 0x11
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x7
	.uleb128 0x40
	.uleb128 0x18
	.uleb128 0x48
	.uleb128 0x18
	.byte	0
	.byte	0
	.uleb128 0x12
	.uleb128 0x21
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x22
	.uleb128 0xd
	.uleb128 0x2f
	.uleb128 0xd
	.byte	0
	.byte	0
	.uleb128 0x13
	.uleb128 0x26
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x34
	.uleb128 0x19
	.byte	0
	.byte	0
	.uleb128 0x14
	.uleb128 0xb
	.byte	0x1
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x7
	.byte	0
	.byte	0
	.byte	0
	.section __DWARF,__debug_pubnames,regular,debug
Lsection__debug_pubnames:
	.long	0x1b
	.short	0x2
	.set L$set$62,Ldebug_info0-Lsection__debug_info
	.long L$set$62
	.long	0x7d5
	.long	0x5d6
	.ascii "test_pqc\0"
	.long	0
	.section __DWARF,__debug_pubtypes,regular,debug
Lsection__debug_pubtypes:
	.long	0x29a
	.short	0x2
	.set L$set$63,Ldebug_info0-Lsection__debug_info
	.long L$set$63
	.long	0x7d5
	.long	0x221
	.ascii "interfaces__unsigned_8\0"
	.long	0x255
	.ascii "anubis_types__TbyteB\0"
	.long	0x26d
	.ascii "integer\0"
	.long	0x278
	.ascii "anubis_types__ml_kem_public_key__T33s\0"
	.long	0x2af
	.ascii "anubis_types__ml_kem_public_key\0"
	.long	0x2e7
	.ascii "anubis_types__ml_kem_secret_key__T35s\0"
	.long	0x366
	.ascii "boolean\0"
	.long	0x31e
	.ascii "anubis_types__ml_kem_secret_key\0"
	.long	0x371
	.ascii "anubis_types__ml_kem_ciphertext__T37s\0"
	.long	0x3a8
	.ascii "anubis_types__ml_kem_ciphertext\0"
	.long	0x3e0
	.ascii "anubis_types__ml_kem_shared_secret__T39s\0"
	.long	0x419
	.ascii "anubis_types__ml_kem_shared_secret\0"
	.long	0x462
	.ascii "ada__text_io__TcountB\0"
	.long	0x47b
	.ascii "anubis_types__ml_dsa_public_key__T41s\0"
	.long	0x4b2
	.ascii "anubis_types__ml_dsa_public_key\0"
	.long	0x4ea
	.ascii "anubis_types__ml_dsa_secret_key__T43s\0"
	.long	0x521
	.ascii "anubis_types__ml_dsa_secret_key\0"
	.long	0x569
	.ascii "anubis_types__ml_dsa_signature__T45s\0"
	.long	0x59f
	.ascii "anubis_types__ml_dsa_signature\0"
	.long	0
	.section __DWARF,__debug_aranges,regular,debug
Lsection__debug_aranges:
	.long	0x2c
	.short	0x2
	.set L$set$64,Ldebug_info0-Lsection__debug_info
	.long L$set$64
	.byte	0x8
	.byte	0
	.short	0
	.short	0
	.quad	Ltext0
	.set L$set$65,Letext0-Ltext0
	.quad L$set$65
	.quad	0
	.quad	0
	.section __DWARF,__debug_ranges,regular,debug
Lsection__debug_ranges:
Ldebug_ranges0:
	.set L$set$66,LBB19-Ltext0
	.quad L$set$66
	.set L$set$67,LBE19-Ltext0
	.quad L$set$67
	.set L$set$68,LBB24-Ltext0
	.quad L$set$68
	.set L$set$69,LBE24-Ltext0
	.quad L$set$69
	.quad	0
	.quad	0
	.section __DWARF,__debug_line,regular,debug
Lsection__debug_line:
Ldebug_line0:
	.section __DWARF,__debug_str,regular,debug
Lsection__debug_str:
LASF0:
	.ascii "data\0"
LASF2:
	.ascii "alice_secret\0"
LASF1:
	.ascii "alice_public\0"
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
