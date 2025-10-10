	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
Ltext0:
	.file 1 "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_key_manager.adb"
	.align	2
	.globl _anubis_key_manager__rotation_policyIP
_anubis_key_manager__rotation_policyIP:
LFB2:
	sub	sp, sp, #32
LCFI0:
	add	x0, sp, 16
	mov	x1, sp
	ldr	x4, [x1]
	ldr	w1, [x1, 8]
	str	x4, [x0]
	str	w1, [x0, 8]
	ldr	x4, [sp, 16]
	mov	x0, 0
	ldr	w1, [sp, 24]
	bfi	x0, x1, 0, 32
	mov	x2, x4
	mov	x3, x0
	mov	x0, x2
	mov	x1, x3
	add	sp, sp, 32
LCFI1:
	ret
LFE2:
	.align	2
	.globl _anubis_key_manager__key_purposeH
_anubis_key_manager__key_purposeH:
LFB3:
	sub	sp, sp, #16
LCFI2:
	stp	x0, x1, [sp]
	ldr	x0, [sp, 8]
	ldr	w0, [x0]
	ldr	x1, [sp, 8]
	ldr	w1, [x1, 4]
	cmp	w1, w0
	blt	L3
	sub	w6, w1, w0
	add	w6, w6, 1
	b	L4
L3:
	mov	w6, 0
L4:
	sxtw	x7, w0
	cmp	w1, w0
	cmp	w1, w0
	blt	L8
	sxtw	x9, w1
	sxtw	x8, w0
	sub	x8, x9, x8
	add	x8, x8, 1
	mov	x2, x8
	mov	x3, 0
	lsr	x8, x2, 61
	lsl	x5, x3, 3
	orr	x5, x8, x5
	lsl	x4, x2, 3
L8:
	cmp	w1, w0
	sub	w8, w0, #1
	mov	w3, 0
	mov	w2, 0
	mov	w0, 0
L12:
	cmp	w0, 0
	bgt	L11
	sxtw	x4, w0
	adrp	x1, _key_purposeP.3@PAGE
	add	x1, x1, _key_purposeP.3@PAGEOFF;
	ldr	w1, [x1, x4, lsl 2]
	cmp	w6, w1
	blt	L11
	ldr	x4, [sp]
	sxtw	x5, w0
	adrp	x1, _key_purposeP.3@PAGE
	add	x1, x1, _key_purposeP.3@PAGEOFF;
	ldr	w1, [x1, x5, lsl 2]
	add	w1, w8, w1
	sxtw	x1, w1
	sub	x1, x1, x7
	ldrsb	w1, [x4, x1]
	and	w1, w1, 255
	mov	w4, w1
	sxtw	x1, w0
	adrp	x5, _key_purposeT1.2@PAGE
	add	x5, x5, _key_purposeT1.2@PAGEOFF;
	ldrb	w1, [x5, x1]
	mul	w1, w4, w1
	add	w3, w3, w1
	mov	w1, 9
	sdiv	w5, w3, w1
	mov	w1, w5
	lsl	w1, w1, 3
	add	w1, w1, w5
	sub	w3, w3, w1
	sxtw	x1, w0
	adrp	x5, _key_purposeT2.1@PAGE
	add	x5, x5, _key_purposeT2.1@PAGEOFF;
	ldrb	w1, [x5, x1]
	mul	w1, w4, w1
	add	w2, w2, w1
	mov	w1, 9
	sdiv	w4, w2, w1
	mov	w1, w4
	lsl	w1, w1, 3
	add	w1, w1, w4
	sub	w2, w2, w1
	add	w0, w0, 1
	b	L12
L11:
	sxtw	x0, w3
	adrp	x1, _key_purposeG.0@PAGE
	add	x1, x1, _key_purposeG.0@PAGEOFF;
	ldrb	w1, [x1, x0]
	sxtw	x0, w2
	adrp	x2, _key_purposeG.0@PAGE
	add	x2, x2, _key_purposeG.0@PAGEOFF;
	ldrb	w0, [x2, x0]
	add	w0, w1, w0
	and	w0, w0, 255
	and	w0, w0, 3
	add	sp, sp, 16
LCFI3:
	ret
LFE3:
	.align	2
	.globl _anubis_key_manager__managed_keyIP
_anubis_key_manager__managed_keyIP:
LFB4:
	sub	sp, sp, #16
LCFI4:
	str	x0, [sp, 8]
	ldr	x0, [sp, 8]
	str	wzr, [x0, 8192]
	ldr	x0, [sp, 8]
	add	x0, x0, 8192
	strb	wzr, [x0, 4]
	ldr	x0, [sp, 8]
	add	x0, x0, 8192
	strb	wzr, [x0, 5]
	ldr	x0, [sp, 8]
	str	wzr, [x0, 8200]
	ldr	x0, [sp, 8]
	add	x0, x0, 8192
	add	x2, x0, 12
	adrp	x0, lC0@PAGE
	add	x1, x0, lC0@PAGEOFF;
	mov	x0, x2
	ldr	x2, [x1]
	ldr	w1, [x1, 8]
	str	x2, [x0]
	str	w1, [x0, 8]
	ldr	x0, [sp, 8]
	add	x0, x0, 8192
	strb	wzr, [x0, 24]
	add	sp, sp, 16
LCFI5:
	ret
LFE4:
	.const
	.align	2
lC0:
	.word	90
	.word	1000000
	.byte	1
	.space 3
	.text
	.align	2
	.globl _anubis_key_manager__initialize
_anubis_key_manager__initialize:
LFB5:
	.loc 1 9 4
	sub	x10, sp, #16384
	str	xzr, [x10, 4064]
	stp	x29, x30, [sp, -32]!
LCFI6:
	mov	x29, sp
LCFI7:
	str	x0, [x29, 24]
	.loc 1 11 24
	ldr	x0, [x29, 24]
	mov	x3, x0
	mov	x0, 8192
	mov	x2, x0
	mov	w1, 0
	mov	x0, x3
	bl	_memset
	.loc 1 12 18
	ldr	x0, [x29, 24]
	str	wzr, [x0, 8192]
	.loc 1 13 19
	ldr	x0, [x29, 24]
	add	x0, x0, 8192
	strb	wzr, [x0, 4]
	.loc 1 14 18
	ldr	x0, [x29, 24]
	add	x0, x0, 8192
	strb	wzr, [x0, 5]
	.loc 1 15 23
	ldr	x0, [x29, 24]
	str	wzr, [x0, 8200]
	.loc 1 16 18
	ldr	x0, [x29, 24]
	add	x0, x0, 8192
	add	x2, x0, 12
	adrp	x0, lC0@PAGE
	add	x1, x0, lC0@PAGEOFF;
	mov	x0, x2
	ldr	x2, [x1]
	ldr	w1, [x1, 8]
	str	x2, [x0]
	str	w1, [x0, 8]
	.loc 1 17 17
	ldr	x0, [x29, 24]
	add	x0, x0, 8192
	strb	wzr, [x0, 24]
	.loc 1 18 8
	nop
	ldp	x29, x30, [sp], 32
LCFI8:
	ret
LFE5:
	.const
	.align	3
lC1:
	.ascii "anubis_key_manager.adb"
	.space 1
	.text
	.align	2
	.globl _anubis_key_manager__create_managed_key
_anubis_key_manager__create_managed_key:
LFB6:
	.loc 1 20 4
	sub	x10, sp, #16384
	str	xzr, [x10, 3952]
	sub	sp, sp, #144
LCFI9:
	stp	x29, x30, [sp, 80]
LCFI10:
	add	x29, sp, 80
LCFI11:
	stp	x19, x20, [sp, 96]
	stp	x21, x22, [sp, 112]
	str	x23, [sp, 128]
LCFI12:
	stp	x0, x1, [x29, -48]
	mov	w1, w2
	mov	x0, x3
	mov	x2, x4
	str	x5, [x29, -80]
	strb	w1, [x29, -49]
	str	x0, [x29, -68]
	ldr	w0, [x29, -60]
	mov	w1, w2
	bfi	w0, w1, 0, 32
	str	w0, [x29, -60]
	.loc 1 20 4
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x1, [x0]
	str	x1, [x29, -8]
	mov	x1, 0
	ldr	x0, [x29, -40]
	ldr	w19, [x0]
	ldr	x0, [x29, -40]
	ldr	w20, [x0, 4]
	cmp	w20, w19
	blt	L19
	.loc 1 20 4 is_stmt 0 discriminator 1
	sxtw	x0, w20
	mov	x14, x0
	asr	x0, x0, 63
	mov	x15, x0
	sxtw	x0, w19
	mov	x12, x0
	asr	x0, x0, 63
	mov	x13, x0
	subs	x1, x14, x12
	sbc	x0, x15, x13
	mov	x10, x1
	mov	x11, x0
	mov	x0, 1
	adds	x1, x10, x0
	mov	x0, 0
	adc	x0, x11, x0
	mov	x22, x1
	mov	x23, x0
	b	L20
L19:
	.loc 1 20 4 discriminator 2
	mov	x22, 0
	mov	x23, 0
L20:
LBB4:
	.loc 1 20 4 discriminator 4
	sxtw	x21, w19
	cmp	w20, w19
	.loc 1 20 4 discriminator 8
	cmp	w20, w19
	blt	L24
	.loc 1 20 4 discriminator 9
	sxtw	x1, w20
	sxtw	x0, w19
	sub	x0, x1, x0
	add	x0, x0, 1
	mov	x6, x0
	mov	x7, 0
	lsr	x0, x6, 61
	lsl	x9, x7, 3
	orr	x9, x0, x9
	lsl	x8, x6, 3
L24:
	.loc 1 20 4 discriminator 12
	cmp	w20, w19
	.loc 1 28 7 is_stmt 1
	ldr	x0, [x29, -80]
	bl	_anubis_key_manager__initialize
	.loc 1 30 26
	sxtw	x1, w19
	mov	x0, 8191
	add	x1, x1, x0
	sxtw	x0, w20
	.loc 1 30 7
	cmp	x1, x0
	bge	L27
	.loc 1 31 18
	strb	wzr, [x29, -25]
	.loc 1 32 10
	b	L28
L27:
	.loc 1 36 11
	str	w19, [x29, -20]
	str	w20, [x29, -16]
	ldr	w1, [x29, -20]
	ldr	w0, [x29, -16]
	cmp	w1, w0
	bgt	L29
LBB5:
	.loc 1 36 11 is_stmt 0 discriminator 1
	ldr	w0, [x29, -20]
	str	w0, [x29, -24]
L40:
LBB6:
	.loc 1 42 49 is_stmt 1
	mov	w1, 0
	ldr	w0, [x29, -24]
	subs	w0, w0, w19
	bvc	L30
	mov	w1, 1
L30:
	mov	w2, w0
	.loc 1 42 49 is_stmt 0 discriminator 1
	mov	w0, w1
	cmp	w0, 0
	beq	L32
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L33
	bl	___stack_chk_fail
L33:
	mov	w1, 42
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L32:
	.loc 1 42 49 discriminator 2
	mov	w0, w2
	.loc 1 42 66 is_stmt 1 discriminator 4
	mov	w1, 2147483647
	cmp	w0, w1
	bne	L34
	.loc 1 42 66 is_stmt 0 discriminator 5
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L35
	bl	___stack_chk_fail
L35:
	mov	w1, 42
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L34:
	.loc 1 42 66 discriminator 6
	add	w0, w0, 1
	.loc 1 42 66 discriminator 8
	cmp	w0, 0
	bgt	L36
	.loc 1 42 66 discriminator 9
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L37
	bl	___stack_chk_fail
L37:
	mov	w1, 42
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L36:
	.loc 1 42 13 is_stmt 1 discriminator 10
	str	w0, [x29, -12]
	.loc 1 45 20
	ldr	w0, [x29, -12]
	cmp	w0, 8192
	ble	L38
	.loc 1 45 20 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L39
	bl	___stack_chk_fail
L39:
	mov	w1, 45
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L38:
	.loc 1 45 47 is_stmt 1 discriminator 2
	ldr	x2, [x29, -48]
	ldrsw	x1, [x29, -24]
	ldrsw	x0, [x29, -12]
	sub	x1, x1, x21
	ldrb	w2, [x2, x1]
	.loc 1 45 47 is_stmt 0 discriminator 3
	ldr	x1, [x29, -80]
	add	x0, x1, x0
	mov	w1, w2
	strb	w1, [x0, -1]
LBE6:
	.loc 1 36 11 is_stmt 1
	ldr	w1, [x29, -24]
	ldr	w0, [x29, -16]
	cmp	w1, w0
	beq	L29
	.loc 1 36 11 is_stmt 0 discriminator 2
	ldr	w0, [x29, -24]
	add	w0, w0, 1
	str	w0, [x29, -24]
	.loc 1 47 15 is_stmt 1
	b	L40
L29:
LBE5:
	.loc 1 49 33
	mov	x0, x22
	mov	x1, 2147483647
	cmp	x0, x1
	ble	L41
	.loc 1 49 33 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L42
	bl	___stack_chk_fail
L42:
	mov	w1, 49
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L41:
	.loc 1 49 22 is_stmt 1 discriminator 2
	mov	w1, w0
	ldr	x0, [x29, -80]
	str	w1, [x0, 8192]
	.loc 1 50 23
	ldr	x0, [x29, -80]
	add	x0, x0, 8192
	ldrb	w1, [x29, -49]
	strb	w1, [x0, 4]
	.loc 1 51 22
	ldr	x0, [x29, -80]
	add	x0, x0, 8192
	add	x0, x0, 12
	mov	x1, x0
	sub	x0, x29, #68
	ldr	x2, [x0]
	ldr	w0, [x0, 8]
	str	x2, [x1]
	str	w0, [x1, 8]
	.loc 1 52 22
	ldr	x0, [x29, -80]
	add	x0, x0, 8192
	mov	w1, 1
	strb	w1, [x0, 5]
	.loc 1 53 27
	ldr	x0, [x29, -80]
	str	wzr, [x0, 8200]
	.loc 1 54 21
	ldr	x0, [x29, -80]
	add	x0, x0, 8192
	mov	w1, 1
	strb	w1, [x0, 24]
	.loc 1 56 15
	mov	w0, 1
	strb	w0, [x29, -25]
	.loc 1 57 8
	nop
L28:
LBE4:
	ldrb	w0, [x29, -25]
	.loc 1 57 8 is_stmt 0 discriminator 2
	mov	w1, w0
	.loc 1 57 8
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x3, [x29, -8]
	ldr	x2, [x0]
	subs	x3, x3, x2
	mov	x2, 0
	beq	L44
	bl	___stack_chk_fail
L44:
	mov	w0, w1
	ldp	x29, x30, [sp, 80]
	ldp	x19, x20, [sp, 96]
	ldp	x21, x22, [sp, 112]
	ldr	x23, [sp, 128]
	add	sp, sp, 144
LCFI13:
	ret
LFE6:
	.align	2
	.globl _anubis_key_manager__needs_rotation
_anubis_key_manager__needs_rotation:
LFB7:
	.loc 1 59 4 is_stmt 1
	sub	sp, sp, #16
LCFI14:
	str	x0, [sp, 8]
	.loc 1 61 10
	ldr	x0, [sp, 8]
	add	x0, x0, 8192
	ldrb	w0, [x0, 20]
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 61 7
	cmp	w0, 0
	beq	L46
	.loc 1 62 10
	mov	w0, 0
	b	L47
L46:
	.loc 1 65 39
	ldr	x0, [sp, 8]
	ldr	w0, [x0, 8208]
	.loc 1 65 7
	cmp	w0, 0
	ble	L48
	.loc 1 66 26
	ldr	x0, [sp, 8]
	ldr	w1, [x0, 8200]
	ldr	x0, [sp, 8]
	ldr	w0, [x0, 8208]
	.loc 1 65 43 discriminator 1
	cmp	w1, w0
	blt	L48
	.loc 1 68 10
	mov	w0, 1
	b	L47
L48:
	.loc 1 71 7
	mov	w0, 0
L47:
	.loc 1 72 8
	add	sp, sp, 16
LCFI15:
	ret
LFE7:
	.align	2
	.globl _anubis_key_manager__get_key_status
_anubis_key_manager__get_key_status:
LFB8:
	.loc 1 74 4
	sub	sp, sp, #16
LCFI16:
	str	x0, [sp, 8]
	.loc 1 75 11
	ldr	x0, [sp, 8]
	add	x0, x0, 8192
	ldrb	w0, [x0, 5]
	.loc 1 74 4
	add	sp, sp, 16
LCFI17:
	ret
LFE8:
	.align	2
	.globl _anubis_key_manager__get_usage_count
_anubis_key_manager__get_usage_count:
LFB9:
	.loc 1 77 4
	sub	sp, sp, #16
LCFI18:
	str	x0, [sp, 8]
	.loc 1 78 11
	ldr	x0, [sp, 8]
	ldr	w0, [x0, 8200]
	.loc 1 77 4
	add	sp, sp, 16
LCFI19:
	ret
LFE9:
	.align	2
	.globl _anubis_key_manager__record_usage
_anubis_key_manager__record_usage:
LFB10:
	.loc 1 80 4
	sub	x10, sp, #16384
	str	xzr, [x10, 4064]
	stp	x29, x30, [sp, -32]!
LCFI20:
	mov	x29, sp
LCFI21:
	str	x0, [x29, 24]
	.loc 1 82 21
	ldr	x0, [x29, 24]
	add	x0, x0, 8192
	ldrb	w0, [x0, 5]
	cmp	w0, 1
	cset	w0, eq
	and	w1, w0, 255
	.loc 1 82 50
	ldr	x0, [x29, 24]
	ldr	w2, [x0, 8200]
	mov	w0, 2147483647
	cmp	w2, w0
	cset	w0, ne
	and	w0, w0, 255
	.loc 1 82 30
	cmp	w1, 0
	ccmp	w0, 0, 4, ne
	cset	w0, ne
	and	w0, w0, 255
	.loc 1 82 7
	cmp	w0, 0
	beq	L57
	.loc 1 83 45
	ldr	x0, [x29, 24]
	ldr	w1, [x0, 8200]
	mov	w0, 2147483647
	cmp	w1, w0
	bne	L55
	.loc 1 83 45 is_stmt 0 discriminator 1
	mov	w1, 83
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L55:
	.loc 1 83 45 discriminator 2
	ldr	x0, [x29, 24]
	ldr	w0, [x0, 8200]
	add	w1, w0, 1
	.loc 1 83 26 is_stmt 1 discriminator 4
	ldr	x0, [x29, 24]
	str	w1, [x0, 8200]
	.loc 1 85 8
	nop
L57:
	nop
	ldp	x29, x30, [sp], 32
LCFI22:
	ret
LFE10:
	.align	2
	.globl _anubis_key_manager__expire_key
_anubis_key_manager__expire_key:
LFB11:
	.loc 1 87 4
	sub	sp, sp, #16
LCFI23:
	str	x0, [sp, 8]
	.loc 1 89 18
	ldr	x0, [sp, 8]
	add	x0, x0, 8192
	mov	w1, 2
	strb	w1, [x0, 5]
	.loc 1 90 8
	nop
	add	sp, sp, 16
LCFI24:
	ret
LFE11:
	.align	2
	.globl _anubis_key_manager__destroy_key
_anubis_key_manager__destroy_key:
LFB12:
	.loc 1 92 4
	sub	sp, sp, #32
LCFI25:
	str	x0, [sp, 8]
LBB7:
	.loc 1 95 11
	mov	w0, 1
	str	w0, [sp, 28]
L62:
	.loc 1 95 11 is_stmt 0 discriminator 3
	ldr	w0, [sp, 28]
	cmp	w0, 8192
	bgt	L61
	.loc 1 99 31 is_stmt 1
	ldrsw	x0, [sp, 28]
	ldr	x1, [sp, 8]
	add	x0, x1, x0
	strb	wzr, [x0, -1]
	.loc 1 95 11 discriminator 2
	ldr	w0, [sp, 28]
	add	w0, w0, 1
	str	w0, [sp, 28]
	.loc 1 100 15
	b	L62
L61:
LBE7:
	.loc 1 102 18
	ldr	x0, [sp, 8]
	str	wzr, [x0, 8192]
	.loc 1 103 23
	ldr	x0, [sp, 8]
	str	wzr, [x0, 8200]
	.loc 1 104 18
	ldr	x0, [sp, 8]
	add	x0, x0, 8192
	mov	w1, 4
	strb	w1, [x0, 5]
	.loc 1 105 17
	ldr	x0, [sp, 8]
	add	x0, x0, 8192
	strb	wzr, [x0, 24]
	.loc 1 106 8
	nop
	add	sp, sp, 32
LCFI26:
	ret
LFE12:
	.globl _anubis_key_manager_E
	.data
	.align	1
_anubis_key_manager_E:
	.space 2
	.globl _anubis_key_manager__default_rotation
	.const
	.align	3
_anubis_key_manager__default_rotation:
	.word	90
	.word	1000000
	.byte	1
	.space 3
	.space 4
	.globl _anubis_key_manager__key_purposeS
	.align	3
_anubis_key_manager__key_purposeS:
	.ascii "ENCRYPTIONSIGNINGKEY_EXCHANGEDERIVATION"
	.globl _anubis_key_manager__key_purposeN
	.align	3
_anubis_key_manager__key_purposeN:
	.byte	1
	.byte	11
	.byte	18
	.byte	30
	.byte	40
	.space 3
	.align	2
_key_purposeP.3:
	.word	1
_key_purposeT1.2:
	.byte	4
_key_purposeT2.1:
	.byte	6
	.align	3
_key_purposeG.0:
	.byte	0
	.byte	0
	.byte	1
	.byte	2
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	3
	.space 7
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
	.uleb128 0x20
	.byte	0x4
	.set L$set$5,LCFI1-LCFI0
	.long L$set$5
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE0:
LSFDE2:
	.set L$set$6,LEFDE2-LASFDE2
	.long L$set$6
LASFDE2:
	.set L$set$7,Lframe0-Lsection__debug_frame
	.long L$set$7
	.quad	LFB3
	.set L$set$8,LFE3-LFB3
	.quad L$set$8
	.byte	0x4
	.set L$set$9,LCFI2-LFB3
	.long L$set$9
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$10,LCFI3-LCFI2
	.long L$set$10
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE2:
LSFDE4:
	.set L$set$11,LEFDE4-LASFDE4
	.long L$set$11
LASFDE4:
	.set L$set$12,Lframe0-Lsection__debug_frame
	.long L$set$12
	.quad	LFB4
	.set L$set$13,LFE4-LFB4
	.quad L$set$13
	.byte	0x4
	.set L$set$14,LCFI4-LFB4
	.long L$set$14
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$15,LCFI5-LCFI4
	.long L$set$15
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE4:
LSFDE6:
	.set L$set$16,LEFDE6-LASFDE6
	.long L$set$16
LASFDE6:
	.set L$set$17,Lframe0-Lsection__debug_frame
	.long L$set$17
	.quad	LFB5
	.set L$set$18,LFE5-LFB5
	.quad L$set$18
	.byte	0x4
	.set L$set$19,LCFI6-LFB5
	.long L$set$19
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$20,LCFI7-LCFI6
	.long L$set$20
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$21,LCFI8-LCFI7
	.long L$set$21
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE6:
LSFDE8:
	.set L$set$22,LEFDE8-LASFDE8
	.long L$set$22
LASFDE8:
	.set L$set$23,Lframe0-Lsection__debug_frame
	.long L$set$23
	.quad	LFB6
	.set L$set$24,LFE6-LFB6
	.quad L$set$24
	.byte	0x4
	.set L$set$25,LCFI9-LFB6
	.long L$set$25
	.byte	0xe
	.uleb128 0x90
	.byte	0x4
	.set L$set$26,LCFI10-LCFI9
	.long L$set$26
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$27,LCFI11-LCFI10
	.long L$set$27
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x40
	.byte	0x4
	.set L$set$28,LCFI12-LCFI11
	.long L$set$28
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x95
	.uleb128 0x4
	.byte	0x96
	.uleb128 0x3
	.byte	0x97
	.uleb128 0x2
	.byte	0x4
	.set L$set$29,LCFI13-LCFI12
	.long L$set$29
	.byte	0xd7
	.byte	0xd5
	.byte	0xd6
	.byte	0xd3
	.byte	0xd4
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE8:
LSFDE10:
	.set L$set$30,LEFDE10-LASFDE10
	.long L$set$30
LASFDE10:
	.set L$set$31,Lframe0-Lsection__debug_frame
	.long L$set$31
	.quad	LFB7
	.set L$set$32,LFE7-LFB7
	.quad L$set$32
	.byte	0x4
	.set L$set$33,LCFI14-LFB7
	.long L$set$33
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$34,LCFI15-LCFI14
	.long L$set$34
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE10:
LSFDE12:
	.set L$set$35,LEFDE12-LASFDE12
	.long L$set$35
LASFDE12:
	.set L$set$36,Lframe0-Lsection__debug_frame
	.long L$set$36
	.quad	LFB8
	.set L$set$37,LFE8-LFB8
	.quad L$set$37
	.byte	0x4
	.set L$set$38,LCFI16-LFB8
	.long L$set$38
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$39,LCFI17-LCFI16
	.long L$set$39
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE12:
LSFDE14:
	.set L$set$40,LEFDE14-LASFDE14
	.long L$set$40
LASFDE14:
	.set L$set$41,Lframe0-Lsection__debug_frame
	.long L$set$41
	.quad	LFB9
	.set L$set$42,LFE9-LFB9
	.quad L$set$42
	.byte	0x4
	.set L$set$43,LCFI18-LFB9
	.long L$set$43
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$44,LCFI19-LCFI18
	.long L$set$44
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE14:
LSFDE16:
	.set L$set$45,LEFDE16-LASFDE16
	.long L$set$45
LASFDE16:
	.set L$set$46,Lframe0-Lsection__debug_frame
	.long L$set$46
	.quad	LFB10
	.set L$set$47,LFE10-LFB10
	.quad L$set$47
	.byte	0x4
	.set L$set$48,LCFI20-LFB10
	.long L$set$48
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$49,LCFI21-LCFI20
	.long L$set$49
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$50,LCFI22-LCFI21
	.long L$set$50
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE16:
LSFDE18:
	.set L$set$51,LEFDE18-LASFDE18
	.long L$set$51
LASFDE18:
	.set L$set$52,Lframe0-Lsection__debug_frame
	.long L$set$52
	.quad	LFB11
	.set L$set$53,LFE11-LFB11
	.quad L$set$53
	.byte	0x4
	.set L$set$54,LCFI23-LFB11
	.long L$set$54
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$55,LCFI24-LCFI23
	.long L$set$55
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE18:
LSFDE20:
	.set L$set$56,LEFDE20-LASFDE20
	.long L$set$56
LASFDE20:
	.set L$set$57,Lframe0-Lsection__debug_frame
	.long L$set$57
	.quad	LFB12
	.set L$set$58,LFE12-LFB12
	.quad L$set$58
	.byte	0x4
	.set L$set$59,LCFI25-LFB12
	.long L$set$59
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.set L$set$60,LCFI26-LCFI25
	.long L$set$60
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE20:
	.section __TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
EH_frame1:
	.set L$set$61,LECIE1-LSCIE1
	.long L$set$61
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
LSFDE23:
	.set L$set$62,LEFDE23-LASFDE23
	.long L$set$62
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB2-.
	.set L$set$63,LFE2-LFB2
	.quad L$set$63
	.uleb128 0
	.byte	0x4
	.set L$set$64,LCFI0-LFB2
	.long L$set$64
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.set L$set$65,LCFI1-LCFI0
	.long L$set$65
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE23:
LSFDE25:
	.set L$set$66,LEFDE25-LASFDE25
	.long L$set$66
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB3-.
	.set L$set$67,LFE3-LFB3
	.quad L$set$67
	.uleb128 0
	.byte	0x4
	.set L$set$68,LCFI2-LFB3
	.long L$set$68
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$69,LCFI3-LCFI2
	.long L$set$69
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE25:
LSFDE27:
	.set L$set$70,LEFDE27-LASFDE27
	.long L$set$70
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB4-.
	.set L$set$71,LFE4-LFB4
	.quad L$set$71
	.uleb128 0
	.byte	0x4
	.set L$set$72,LCFI4-LFB4
	.long L$set$72
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$73,LCFI5-LCFI4
	.long L$set$73
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE27:
LSFDE29:
	.set L$set$74,LEFDE29-LASFDE29
	.long L$set$74
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB5-.
	.set L$set$75,LFE5-LFB5
	.quad L$set$75
	.uleb128 0
	.byte	0x4
	.set L$set$76,LCFI6-LFB5
	.long L$set$76
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$77,LCFI7-LCFI6
	.long L$set$77
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$78,LCFI8-LCFI7
	.long L$set$78
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE29:
LSFDE31:
	.set L$set$79,LEFDE31-LASFDE31
	.long L$set$79
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB6-.
	.set L$set$80,LFE6-LFB6
	.quad L$set$80
	.uleb128 0
	.byte	0x4
	.set L$set$81,LCFI9-LFB6
	.long L$set$81
	.byte	0xe
	.uleb128 0x90
	.byte	0x4
	.set L$set$82,LCFI10-LCFI9
	.long L$set$82
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$83,LCFI11-LCFI10
	.long L$set$83
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x40
	.byte	0x4
	.set L$set$84,LCFI12-LCFI11
	.long L$set$84
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x95
	.uleb128 0x4
	.byte	0x96
	.uleb128 0x3
	.byte	0x97
	.uleb128 0x2
	.byte	0x4
	.set L$set$85,LCFI13-LCFI12
	.long L$set$85
	.byte	0xd7
	.byte	0xd5
	.byte	0xd6
	.byte	0xd3
	.byte	0xd4
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE31:
LSFDE33:
	.set L$set$86,LEFDE33-LASFDE33
	.long L$set$86
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB7-.
	.set L$set$87,LFE7-LFB7
	.quad L$set$87
	.uleb128 0
	.byte	0x4
	.set L$set$88,LCFI14-LFB7
	.long L$set$88
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$89,LCFI15-LCFI14
	.long L$set$89
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE33:
LSFDE35:
	.set L$set$90,LEFDE35-LASFDE35
	.long L$set$90
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB8-.
	.set L$set$91,LFE8-LFB8
	.quad L$set$91
	.uleb128 0
	.byte	0x4
	.set L$set$92,LCFI16-LFB8
	.long L$set$92
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$93,LCFI17-LCFI16
	.long L$set$93
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE35:
LSFDE37:
	.set L$set$94,LEFDE37-LASFDE37
	.long L$set$94
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB9-.
	.set L$set$95,LFE9-LFB9
	.quad L$set$95
	.uleb128 0
	.byte	0x4
	.set L$set$96,LCFI18-LFB9
	.long L$set$96
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$97,LCFI19-LCFI18
	.long L$set$97
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE37:
LSFDE39:
	.set L$set$98,LEFDE39-LASFDE39
	.long L$set$98
LASFDE39:
	.long	LASFDE39-EH_frame1
	.quad	LFB10-.
	.set L$set$99,LFE10-LFB10
	.quad L$set$99
	.uleb128 0
	.byte	0x4
	.set L$set$100,LCFI20-LFB10
	.long L$set$100
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$101,LCFI21-LCFI20
	.long L$set$101
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$102,LCFI22-LCFI21
	.long L$set$102
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE39:
LSFDE41:
	.set L$set$103,LEFDE41-LASFDE41
	.long L$set$103
LASFDE41:
	.long	LASFDE41-EH_frame1
	.quad	LFB11-.
	.set L$set$104,LFE11-LFB11
	.quad L$set$104
	.uleb128 0
	.byte	0x4
	.set L$set$105,LCFI23-LFB11
	.long L$set$105
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$106,LCFI24-LCFI23
	.long L$set$106
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE41:
LSFDE43:
	.set L$set$107,LEFDE43-LASFDE43
	.long L$set$107
LASFDE43:
	.long	LASFDE43-EH_frame1
	.quad	LFB12-.
	.set L$set$108,LFE12-LFB12
	.quad L$set$108
	.uleb128 0
	.byte	0x4
	.set L$set$109,LCFI25-LFB12
	.long L$set$109
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.set L$set$110,LCFI26-LCFI25
	.long L$set$110
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE43:
	.text
Letext0:
	.file 2 "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_key_manager.ads"
	.file 3 "/Users/sicarii/.local/share/alire/releases/gnatprove_14.1.1_91818ed8/libexec/spark/lib/gcc/aarch64-apple-darwin23.5.0/14.1.0/adainclude/interfac.ads"
	.file 4 "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_types.ads"
	.section __DWARF,__debug_info,regular,debug
Lsection__debug_info:
Ldebug_info0:
	.long	0xa32
	.short	0x4
	.set L$set$111,Ldebug_abbrev0-Lsection__debug_abbrev
	.long L$set$111
	.byte	0x8
	.uleb128 0x1
	.ascii "GNU Ada 14.1.0 -O0 -gnatA -g -gnat2012 -gnatwa -gnatwe -fstack-check=specific -fstack-protector-strong -gnatR2js -gnatws -gnatec=/private/var/folders/bg/pt9l6y1j47q642kp3z5blrmh0000gn/T/GPR.75209/GNAT-TEMP-000003.TMP -gnatem=/private/var/folders/bg/pt9l6y1j47q642kp3z5blrmh0000gn/T/GPR.75209/GNAT-TEMP-000004.TMP -mmacosx-version-min=16.0.0 -mcpu=apple-m1 -mlittle-endian -mabi=lp64 -fPIC\0"
	.byte	0xd
	.ascii "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_key_manager.adb\0"
	.ascii "/Users/sicarii/Desktop/anubis-spark/obj/gnatprove/data_representation\0"
	.quad	Ltext0
	.set L$set$112,Letext0-Ltext0
	.quad L$set$112
	.set L$set$113,Ldebug_line0-Lsection__debug_line
	.long L$set$113
	.uleb128 0x2
	.ascii "anubis_key_manager__rotation_policy\0"
	.byte	0xc
	.byte	0x2
	.byte	0x12
	.byte	0x9
	.long	0x2b8
	.uleb128 0x3
	.ascii "time_based_days\0"
	.byte	0x2
	.byte	0x13
	.byte	0x7
	.long	0x278
	.byte	0
	.uleb128 0x4
	.sleb128 0
	.sleb128 2147483647
	.ascii "natural\0"
	.long	0x2bd
	.uleb128 0x3
	.ascii "usage_based_count\0"
	.byte	0x2
	.byte	0x14
	.byte	0x7
	.long	0x278
	.byte	0x4
	.uleb128 0x3
	.ascii "enabled\0"
	.byte	0x2
	.byte	0x15
	.byte	0x7
	.long	0x2c8
	.byte	0x8
	.byte	0
	.uleb128 0x5
	.long	0x232
	.uleb128 0x6
	.byte	0x4
	.byte	0x5
	.ascii "integer\0"
	.uleb128 0x6
	.byte	0x1
	.byte	0x2
	.ascii "boolean\0"
	.uleb128 0x7
	.ascii "anubis_key_manager__key_purpose\0"
	.byte	0x1
	.byte	0x2
	.byte	0x10
	.byte	0x9
	.long	0x380
	.uleb128 0x8
	.ascii "anubis_key_manager__encryption\0"
	.byte	0
	.uleb128 0x8
	.ascii "anubis_key_manager__signing\0"
	.byte	0x1
	.uleb128 0x8
	.ascii "anubis_key_manager__key_exchange\0"
	.byte	0x2
	.uleb128 0x8
	.ascii "anubis_key_manager__derivation\0"
	.byte	0x3
	.byte	0
	.uleb128 0x5
	.long	0x2d3
	.uleb128 0x6
	.byte	0x1
	.byte	0x7
	.ascii "interfaces__unsigned_8\0"
	.uleb128 0x9
	.byte	0
	.byte	0xff
	.ascii "anubis_types__byte\0"
	.long	0x3b9
	.uleb128 0xa
	.byte	0x1
	.byte	0x7
	.ascii "anubis_types__TbyteB\0"
	.uleb128 0xb
	.ascii "anubis_types__byte_array\0"
	.byte	0x10
	.byte	0x4
	.byte	0x12
	.byte	0x9
	.long	0x41d
	.uleb128 0x3
	.ascii "P_ARRAY\0"
	.byte	0x3
	.byte	0x46
	.byte	0x1d
	.long	0x404
	.byte	0
	.uleb128 0xc
	.byte	0x8
	.long	0x422
	.uleb128 0x3
	.ascii "P_BOUNDS\0"
	.byte	0x3
	.byte	0x46
	.byte	0x1d
	.long	0x4a3
	.byte	0x8
	.byte	0
	.uleb128 0xd
	.long	0x3d1
	.uleb128 0xe
	.ascii "anubis_types__byte_array___XUA\0"
	.long	0x39f
	.long	0x460
	.uleb128 0xf
	.long	0x2bd
	.uleb128 0x6
	.byte	0x97
	.byte	0x23
	.uleb128 0x8
	.byte	0x6
	.byte	0x94
	.byte	0x4
	.uleb128 0x8
	.byte	0x97
	.byte	0x23
	.uleb128 0x8
	.byte	0x6
	.byte	0x23
	.uleb128 0x4
	.byte	0x94
	.byte	0x4
	.byte	0
	.uleb128 0xb
	.ascii "anubis_types__byte_array___XUB\0"
	.byte	0x8
	.byte	0x3
	.byte	0x46
	.byte	0x1d
	.long	0x4a3
	.uleb128 0x3
	.ascii "LB0\0"
	.byte	0x4
	.byte	0x12
	.byte	0x9
	.long	0x278
	.byte	0
	.uleb128 0x3
	.ascii "UB0\0"
	.byte	0x4
	.byte	0x12
	.byte	0x9
	.long	0x278
	.byte	0x4
	.byte	0
	.uleb128 0xc
	.byte	0x8
	.long	0x460
	.uleb128 0xe
	.ascii "anubis_key_manager__managed_key__T5s\0"
	.long	0x39f
	.long	0x4e0
	.uleb128 0x10
	.long	0x2bd
	.sleb128 8192
	.byte	0
	.uleb128 0x7
	.ascii "anubis_types__key_status\0"
	.byte	0x1
	.byte	0x4
	.byte	0x4b
	.byte	0x9
	.long	0x582
	.uleb128 0x8
	.ascii "anubis_types__uninitialized\0"
	.byte	0
	.uleb128 0x8
	.ascii "anubis_types__active\0"
	.byte	0x1
	.uleb128 0x8
	.ascii "anubis_types__expired\0"
	.byte	0x2
	.uleb128 0x8
	.ascii "anubis_types__revoked\0"
	.byte	0x3
	.uleb128 0x8
	.ascii "anubis_types__destroyed\0"
	.byte	0x4
	.byte	0
	.uleb128 0x11
	.ascii "anubis_key_manager__managed_key\0"
	.short	0x201c
	.byte	0x2
	.byte	0x44
	.byte	0x9
	.long	0x62e
	.uleb128 0x3
	.ascii "key_material\0"
	.byte	0x2
	.byte	0x45
	.byte	0x7
	.long	0x4a9
	.byte	0
	.uleb128 0x12
	.ascii "length\0"
	.byte	0x2
	.byte	0x46
	.byte	0x7
	.long	0x278
	.short	0x2000
	.uleb128 0x12
	.ascii "purpose\0"
	.byte	0x2
	.byte	0x47
	.byte	0x7
	.long	0x2d3
	.short	0x2004
	.uleb128 0x12
	.ascii "status\0"
	.byte	0x2
	.byte	0x48
	.byte	0x7
	.long	0x4e0
	.short	0x2005
	.uleb128 0x12
	.ascii "usage_count\0"
	.byte	0x2
	.byte	0x49
	.byte	0x7
	.long	0x278
	.short	0x2008
	.uleb128 0x12
	.ascii "policy\0"
	.byte	0x2
	.byte	0x4a
	.byte	0x7
	.long	0x232
	.short	0x200c
	.uleb128 0x12
	.ascii "valid\0"
	.byte	0x2
	.byte	0x4b
	.byte	0x7
	.long	0x2c8
	.short	0x2018
	.byte	0
	.uleb128 0x13
	.ascii "anubis_key_manager__default_rotation\0"
	.byte	0x2
	.byte	0x18
	.byte	0x4
	.long	0x2b8
	.uleb128 0x9
	.byte	0x3
	.quad	_anubis_key_manager__default_rotation
	.uleb128 0x14
	.ascii "anubis_key_manager__destroy_key\0"
	.byte	0x1
	.byte	0x5c
	.byte	0x4
	.quad	LFB12
	.set L$set$114,LFE12-LFB12
	.quad L$set$114
	.uleb128 0x1
	.byte	0x9c
	.long	0x6ce
	.uleb128 0x15
	.ascii "key\0"
	.byte	0x2
	.byte	0x3e
	.byte	0x1b
	.long	0x6ce
	.uleb128 0x2
	.byte	0x91
	.sleb128 -24
	.uleb128 0x16
	.quad	LBB7
	.set L$set$115,LBE7-LBB7
	.quad L$set$115
	.uleb128 0x17
	.ascii "i\0"
	.byte	0x1
	.byte	0x5f
	.byte	0xb
	.long	0x2bd
	.uleb128 0x2
	.byte	0x91
	.sleb128 -4
	.byte	0
	.byte	0
	.uleb128 0x18
	.byte	0x8
	.long	0x582
	.uleb128 0x14
	.ascii "anubis_key_manager__expire_key\0"
	.byte	0x1
	.byte	0x57
	.byte	0x4
	.quad	LFB11
	.set L$set$116,LFE11-LFB11
	.quad L$set$116
	.uleb128 0x1
	.byte	0x9c
	.long	0x71d
	.uleb128 0x15
	.ascii "key\0"
	.byte	0x2
	.byte	0x33
	.byte	0x1a
	.long	0x6ce
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.byte	0
	.uleb128 0x14
	.ascii "anubis_key_manager__record_usage\0"
	.byte	0x1
	.byte	0x50
	.byte	0x4
	.quad	LFB10
	.set L$set$117,LFE10-LFB10
	.quad L$set$117
	.uleb128 0x1
	.byte	0x9c
	.long	0x768
	.uleb128 0x15
	.ascii "key\0"
	.byte	0x2
	.byte	0x32
	.byte	0x1c
	.long	0x6ce
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.byte	0
	.uleb128 0x19
	.ascii "anubis_key_manager__get_usage_count\0"
	.byte	0x1
	.byte	0x4d
	.byte	0x4
	.long	0x278
	.quad	LFB9
	.set L$set$118,LFE9-LFB9
	.quad L$set$118
	.uleb128 0x1
	.byte	0x9c
	.long	0x7ba
	.uleb128 0x15
	.ascii "key\0"
	.byte	0x2
	.byte	0x30
	.byte	0x1e
	.long	0x6ce
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.byte	0
	.uleb128 0x19
	.ascii "anubis_key_manager__get_key_status\0"
	.byte	0x1
	.byte	0x4a
	.byte	0x4
	.long	0x4e0
	.quad	LFB8
	.set L$set$119,LFE8-LFB8
	.quad L$set$119
	.uleb128 0x1
	.byte	0x9c
	.long	0x80b
	.uleb128 0x15
	.ascii "key\0"
	.byte	0x2
	.byte	0x2f
	.byte	0x1d
	.long	0x6ce
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.byte	0
	.uleb128 0x19
	.ascii "anubis_key_manager__needs_rotation\0"
	.byte	0x1
	.byte	0x3b
	.byte	0x4
	.long	0x2c8
	.quad	LFB7
	.set L$set$120,LFE7-LFB7
	.quad L$set$120
	.uleb128 0x1
	.byte	0x9c
	.long	0x85c
	.uleb128 0x15
	.ascii "key\0"
	.byte	0x2
	.byte	0x2e
	.byte	0x1d
	.long	0x6ce
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.byte	0
	.uleb128 0x19
	.ascii "anubis_key_manager__create_managed_key\0"
	.byte	0x1
	.byte	0x14
	.byte	0x4
	.long	0x2c8
	.quad	LFB6
	.set L$set$121,LFE6-LFB6
	.quad L$set$121
	.uleb128 0x1
	.byte	0x9c
	.long	0x9f0
	.uleb128 0x15
	.ascii "key_data\0"
	.byte	0x2
	.byte	0x26
	.byte	0x7
	.long	0x41d
	.uleb128 0x3
	.byte	0x91
	.sleb128 -112
	.uleb128 0x15
	.ascii "purpose\0"
	.byte	0x2
	.byte	0x27
	.byte	0x7
	.long	0x380
	.uleb128 0x3
	.byte	0x91
	.sleb128 -113
	.uleb128 0x15
	.ascii "policy\0"
	.byte	0x2
	.byte	0x28
	.byte	0x7
	.long	0x2b8
	.uleb128 0x3
	.byte	0x91
	.sleb128 -132
	.uleb128 0x15
	.ascii "managed\0"
	.byte	0x2
	.byte	0x29
	.byte	0x7
	.long	0x6ce
	.uleb128 0x3
	.byte	0x91
	.sleb128 -144
	.uleb128 0x15
	.ascii "success\0"
	.byte	0x2
	.byte	0x2a
	.byte	0x7
	.long	0x2c8
	.uleb128 0x3
	.byte	0x91
	.sleb128 -89
	.uleb128 0x16
	.quad	LBB4
	.set L$set$122,LBE4-LBB4
	.quad L$set$122
	.uleb128 0x1a
	.ascii "anubis_key_manager__create_managed_key__L_1__T5b___L\0"
	.long	0x2bd
	.uleb128 0x3
	.byte	0x91
	.sleb128 -84
	.uleb128 0x1a
	.ascii "anubis_key_manager__create_managed_key__L_1__T5b___U\0"
	.long	0x2bd
	.uleb128 0x3
	.byte	0x91
	.sleb128 -80
	.uleb128 0x16
	.quad	LBB5
	.set L$set$123,LBE5-LBB5
	.quad L$set$123
	.uleb128 0x17
	.ascii "i\0"
	.byte	0x1
	.byte	0x24
	.byte	0xb
	.long	0x2bd
	.uleb128 0x3
	.byte	0x91
	.sleb128 -88
	.uleb128 0x16
	.quad	LBB6
	.set L$set$124,LBE6-LBB6
	.quad L$set$124
	.uleb128 0x17
	.ascii "dest_index\0"
	.byte	0x1
	.byte	0x2a
	.byte	0xd
	.long	0x9d9
	.uleb128 0x3
	.byte	0x91
	.sleb128 -76
	.uleb128 0x1b
	.sleb128 2147483647
	.ascii "positive\0"
	.long	0x2bd
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0x1c
	.ascii "anubis_key_manager__initialize\0"
	.byte	0x1
	.byte	0x9
	.byte	0x4
	.quad	LFB5
	.set L$set$125,LFE5-LFB5
	.quad L$set$125
	.uleb128 0x1
	.byte	0x9c
	.uleb128 0x15
	.ascii "key\0"
	.byte	0x2
	.byte	0x20
	.byte	0x1a
	.long	0x6ce
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
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
	.uleb128 0x3
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
	.uleb128 0x4
	.uleb128 0x21
	.byte	0
	.uleb128 0x22
	.uleb128 0xd
	.uleb128 0x2f
	.uleb128 0xd
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x34
	.uleb128 0x19
	.byte	0
	.byte	0
	.uleb128 0x5
	.uleb128 0x26
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x6
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
	.uleb128 0x7
	.uleb128 0x4
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
	.uleb128 0x8
	.uleb128 0x28
	.byte	0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x1c
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0x9
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
	.uleb128 0xa
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
	.uleb128 0xb
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
	.uleb128 0x34
	.uleb128 0x19
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0xc
	.uleb128 0xf
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0xd
	.uleb128 0x26
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x34
	.uleb128 0x19
	.byte	0
	.byte	0
	.uleb128 0xe
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
	.uleb128 0xf
	.uleb128 0x21
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x22
	.uleb128 0x18
	.uleb128 0x2f
	.uleb128 0x18
	.byte	0
	.byte	0
	.uleb128 0x10
	.uleb128 0x21
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2f
	.uleb128 0xd
	.byte	0
	.byte	0
	.uleb128 0x11
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
	.uleb128 0x12
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
	.uleb128 0x13
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
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x2
	.uleb128 0x18
	.byte	0
	.byte	0
	.uleb128 0x14
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
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x7
	.uleb128 0x40
	.uleb128 0x18
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x15
	.uleb128 0x5
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
	.uleb128 0x16
	.uleb128 0xb
	.byte	0x1
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x7
	.byte	0
	.byte	0
	.uleb128 0x17
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
	.uleb128 0x18
	.uleb128 0x10
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x19
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
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x7
	.uleb128 0x40
	.uleb128 0x18
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x1a
	.uleb128 0x34
	.byte	0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x34
	.uleb128 0x19
	.uleb128 0x2
	.uleb128 0x18
	.byte	0
	.byte	0
	.uleb128 0x1b
	.uleb128 0x21
	.byte	0
	.uleb128 0x2f
	.uleb128 0xd
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x34
	.uleb128 0x19
	.byte	0
	.byte	0
	.uleb128 0x1c
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
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x7
	.uleb128 0x40
	.uleb128 0x18
	.byte	0
	.byte	0
	.byte	0
	.section __DWARF,__debug_pubnames,regular,debug
Lsection__debug_pubnames:
	.long	0x167
	.short	0x2
	.set L$set$126,Ldebug_info0-Lsection__debug_info
	.long L$set$126
	.long	0xa36
	.long	0x62e
	.ascii "anubis_key_manager__default_rotation\0"
	.long	0x665
	.ascii "anubis_key_manager__destroy_key\0"
	.long	0x6d4
	.ascii "anubis_key_manager__expire_key\0"
	.long	0x71d
	.ascii "anubis_key_manager__record_usage\0"
	.long	0x768
	.ascii "anubis_key_manager__get_usage_count\0"
	.long	0x7ba
	.ascii "anubis_key_manager__get_key_status\0"
	.long	0x80b
	.ascii "anubis_key_manager__needs_rotation\0"
	.long	0x85c
	.ascii "anubis_key_manager__create_managed_key\0"
	.long	0x9f0
	.ascii "anubis_key_manager__initialize\0"
	.long	0
	.section __DWARF,__debug_pubtypes,regular,debug
Lsection__debug_pubtypes:
	.long	0x173
	.short	0x2
	.set L$set$127,Ldebug_info0-Lsection__debug_info
	.long L$set$127
	.long	0xa36
	.long	0x2bd
	.ascii "integer\0"
	.long	0x2c8
	.ascii "boolean\0"
	.long	0x232
	.ascii "anubis_key_manager__rotation_policy\0"
	.long	0x2d3
	.ascii "anubis_key_manager__key_purpose\0"
	.long	0x385
	.ascii "interfaces__unsigned_8\0"
	.long	0x3b9
	.ascii "anubis_types__TbyteB\0"
	.long	0x422
	.ascii "anubis_types__byte_array___XUA\0"
	.long	0x460
	.ascii "anubis_types__byte_array___XUB\0"
	.long	0x3d1
	.ascii "anubis_types__byte_array\0"
	.long	0x4a9
	.ascii "anubis_key_manager__managed_key__T5s\0"
	.long	0x4e0
	.ascii "anubis_types__key_status\0"
	.long	0x582
	.ascii "anubis_key_manager__managed_key\0"
	.long	0
	.section __DWARF,__debug_aranges,regular,debug
Lsection__debug_aranges:
	.long	0x2c
	.short	0x2
	.set L$set$128,Ldebug_info0-Lsection__debug_info
	.long L$set$128
	.byte	0x8
	.byte	0
	.short	0
	.short	0
	.quad	Ltext0
	.set L$set$129,Letext0-Ltext0
	.quad L$set$129
	.quad	0
	.quad	0
	.section __DWARF,__debug_line,regular,debug
Lsection__debug_line:
Ldebug_line0:
	.section __DWARF,__debug_str,regular,debug
Lsection__debug_str:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
