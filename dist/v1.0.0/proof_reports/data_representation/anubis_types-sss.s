	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
Ltext0:
	.file 1 "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_types-sss.adb"
	.align	2
	.globl _anubis_types__sss__secret_shareIP
_anubis_types__sss__secret_shareIP:
LFB2:
	sub	sp, sp, #16
LCFI0:
	str	x0, [sp, 8]
	ldr	x0, [sp, 8]
	str	wzr, [x0, 260]
	ldr	x0, [sp, 8]
	strb	wzr, [x0, 264]
	add	sp, sp, 16
LCFI1:
	ret
LFE2:
	.align	2
	.globl _anubis_types__sss__share_arrayIP
_anubis_types__sss__share_arrayIP:
LFB3:
	sub	x10, sp, #16384
	str	xzr, [x10, 4032]
	stp	x29, x30, [sp, -64]!
LCFI2:
	mov	x29, sp
LCFI3:
	stp	x19, x20, [sp, 16]
	str	x21, [sp, 32]
LCFI4:
	stp	x0, x1, [x29, 48]
	ldr	x0, [x29, 56]
	ldrsh	w0, [x0]
	sxth	x21, w0
	ldr	x0, [x29, 56]
	ldrsh	w1, [x0, 2]
	ldr	x0, [x29, 56]
	ldrsh	w0, [x0]
	cmp	w1, w0
	ldr	x0, [x29, 56]
	ldrsh	w1, [x0, 2]
	ldr	x0, [x29, 56]
	ldrsh	w0, [x0]
	cmp	w1, w0
	blt	L7
	ldr	x0, [x29, 56]
	ldrsh	w0, [x0, 2]
	sxth	x1, w0
	ldr	x0, [x29, 56]
	ldrsh	w0, [x0]
	sxth	x0, w0
	sub	x0, x1, x0
	add	x0, x0, 1
	mov	x2, x0
	mov	x3, 0
	mov	x0, 2144
	mov	x1, 0
	mul	x5, x2, x0
	umulh	x4, x2, x0
	madd	x4, x3, x0, x4
	madd	x4, x2, x1, x4
	mov	x6, x5
	mov	x7, x4
L7:
	ldr	x0, [x29, 56]
	ldrsh	w1, [x0, 2]
	ldr	x0, [x29, 56]
	ldrsh	w0, [x0]
	cmp	w1, w0
	ldr	x0, [x29, 56]
	ldrsh	w19, [x0]
	ldr	x0, [x29, 56]
	ldrsh	w20, [x0, 2]
	cmp	w19, w20
	bgt	L3
L11:
	ldr	x2, [x29, 48]
	sxth	x0, w19
	sub	x1, x0, x21
	mov	x0, x1
	lsl	x0, x0, 4
	add	x0, x0, x1
	lsl	x0, x0, 2
	sub	x0, x0, x1
	lsl	x0, x0, 2
	add	x0, x2, x0
	bl	_anubis_types__sss__secret_shareIP
	cmp	w19, w20
	beq	L3
	add	w0, w19, 1
	sxth	w19, w0
	b	L11
L3:
	ldp	x19, x20, [sp, 16]
	ldr	x21, [sp, 32]
	ldp	x29, x30, [sp], 64
LCFI5:
	ret
LFE3:
	.align	2
_anubis_types__sss__gf_mult:
LFB4:
	.loc 1 28 4
	sub	sp, sp, #32
LCFI6:
	strb	w0, [sp, 15]
	mov	w0, w1
	strb	w0, [sp, 14]
	.loc 1 29 7
	strb	wzr, [sp, 24]
	.loc 1 30 7
	ldrb	w0, [sp, 15]
	strb	w0, [sp, 25]
	.loc 1 31 7
	ldrb	w0, [sp, 14]
	strb	w0, [sp, 26]
LBB3:
	.loc 1 33 11
	mov	w0, 1
	str	w0, [sp, 28]
L17:
	.loc 1 33 11 is_stmt 0 discriminator 1
	ldr	w0, [sp, 28]
	cmp	w0, 8
	bgt	L14
	.loc 1 37 17 is_stmt 1
	ldrb	w0, [sp, 26]
	and	w0, w0, 1
	and	w0, w0, 255
	.loc 1 37 10
	cmp	w0, 0
	beq	L15
	.loc 1 38 20
	ldrb	w1, [sp, 24]
	ldrb	w0, [sp, 25]
	eor	w0, w1, w0
	strb	w0, [sp, 24]
L15:
LBB4:
	.loc 1 43 64
	ldrsb	w0, [sp, 25]
	.loc 1 43 13
	and	w0, w0, 255
	lsr	w0, w0, 7
	strb	w0, [sp, 27]
	.loc 1 45 16
	ldrb	w0, [sp, 25]
	ubfiz	w0, w0, 1, 7
	strb	w0, [sp, 25]
	.loc 1 46 13
	ldrb	w0, [sp, 27]
	cmp	w0, 0
	beq	L16
	.loc 1 47 19
	ldrb	w1, [sp, 25]
	mov	w0, 27
	eor	w0, w1, w0
	strb	w0, [sp, 25]
L16:
LBE4:
	.loc 1 51 13
	ldrb	w0, [sp, 26]
	lsr	w0, w0, 1
	strb	w0, [sp, 26]
	.loc 1 33 11 discriminator 2
	ldr	w0, [sp, 28]
	add	w0, w0, 1
	str	w0, [sp, 28]
	.loc 1 52 15
	b	L17
L14:
LBE3:
	.loc 1 54 7
	ldrb	w0, [sp, 24]
	.loc 1 55 8
	add	sp, sp, 32
LCFI7:
	ret
LFE4:
	.align	2
_anubis_types__sss__gf_div:
LFB5:
	.loc 1 59 4
	sub	x10, sp, #16384
	str	xzr, [x10, 4048]
	stp	x29, x30, [sp, -48]!
LCFI8:
	mov	x29, sp
LCFI9:
	strb	w0, [x29, 31]
	mov	w0, w1
	strb	w0, [x29, 30]
	.loc 1 63 7
	ldrb	w0, [x29, 30]
	cmp	w0, 0
	bne	L20
	.loc 1 64 10
	mov	w0, 0
	b	L21
L20:
	.loc 1 65 7
	ldrb	w0, [x29, 30]
	cmp	w0, 1
	bne	L22
	.loc 1 66 10
	ldrb	w0, [x29, 31]
	b	L21
L22:
	.loc 1 69 14
	mov	w0, 1
	strb	w0, [x29, 43]
LBB5:
	.loc 1 70 14
	mov	w0, 1
	str	w0, [x29, 44]
L24:
	.loc 1 70 14 is_stmt 0 discriminator 3
	ldr	w0, [x29, 44]
	cmp	w0, 254
	bgt	L23
	.loc 1 73 20 is_stmt 1
	ldrb	w1, [x29, 30]
	ldrb	w0, [x29, 43]
	bl	_anubis_types__sss__gf_mult
	.loc 1 73 20 is_stmt 0 discriminator 1
	strb	w0, [x29, 43]
	.loc 1 70 14 is_stmt 1 discriminator 2
	ldr	w0, [x29, 44]
	add	w0, w0, 1
	str	w0, [x29, 44]
	.loc 1 74 18
	b	L24
L23:
LBE5:
	.loc 1 75 17
	ldrb	w1, [x29, 43]
	ldrb	w0, [x29, 31]
	bl	_anubis_types__sss__gf_mult
	.loc 1 75 10
	nop
L21:
	.loc 1 77 8
	ldp	x29, x30, [sp], 48
LCFI10:
	ret
LFE5:
	.align	2
_anubis_types__sss__eval_polynomial:
LFB6:
	.loc 1 85 4
	sub	x10, sp, #16384
	str	xzr, [x10, 4016]
	stp	x29, x30, [sp, -80]!
LCFI11:
	mov	x29, sp
LCFI12:
	str	x19, [sp, 16]
LCFI13:
	stp	x0, x1, [x29, 48]
	mov	w0, w2
	strb	w0, [x29, 47]
	.loc 1 85 4
	ldr	x0, [x29, 56]
	ldr	w0, [x0]
	ldr	x1, [x29, 56]
	ldr	w1, [x1, 4]
LBB6:
	sxtw	x19, w0
	cmp	w1, w0
	.loc 1 85 4 is_stmt 0 discriminator 4
	cmp	w1, w0
	blt	L29
	.loc 1 85 4 discriminator 5
	sxtw	x3, w1
	sxtw	x2, w0
	sub	x2, x3, x2
	add	x2, x2, 1
	mov	x4, x2
	mov	x5, 0
	lsr	x2, x4, 61
	lsl	x7, x5, 3
	orr	x7, x2, x7
	lsl	x6, x4, 3
L29:
	.loc 1 85 4 discriminator 8
	cmp	w1, w0
	.loc 1 89 7 is_stmt 1
	strb	wzr, [x29, 66]
	.loc 1 90 7
	mov	w2, 1
	strb	w2, [x29, 67]
	.loc 1 92 11
	str	w0, [x29, 72]
	str	w1, [x29, 76]
	ldr	w1, [x29, 72]
	ldr	w0, [x29, 76]
	cmp	w1, w0
	bgt	L32
LBB7:
	.loc 1 92 11 is_stmt 0 discriminator 1
	ldr	w0, [x29, 72]
	str	w0, [x29, 68]
L33:
	.loc 1 97 20 is_stmt 1
	ldr	x1, [x29, 48]
	ldrsw	x0, [x29, 68]
	sub	x0, x0, x19
	ldrb	w0, [x1, x0]
	ldrb	w1, [x29, 67]
	bl	_anubis_types__sss__gf_mult
	mov	w1, w0
	.loc 1 97 20 is_stmt 0 discriminator 1
	ldrb	w0, [x29, 66]
	bl	_anubis_types__sss__gf_add
	.loc 1 97 20 discriminator 2
	strb	w0, [x29, 66]
	.loc 1 100 21 is_stmt 1
	ldrb	w1, [x29, 47]
	ldrb	w0, [x29, 67]
	bl	_anubis_types__sss__gf_mult
	.loc 1 100 21 is_stmt 0 discriminator 1
	strb	w0, [x29, 67]
	.loc 1 92 11 is_stmt 1 discriminator 4
	ldr	w1, [x29, 68]
	ldr	w0, [x29, 76]
	cmp	w1, w0
	beq	L32
	.loc 1 92 11 is_stmt 0 discriminator 3
	ldr	w0, [x29, 68]
	add	w0, w0, 1
	str	w0, [x29, 68]
	.loc 1 101 15 is_stmt 1
	b	L33
L32:
LBE7:
	.loc 1 103 7
	ldrb	w0, [x29, 66]
LBE6:
	.loc 1 104 8
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 80
LCFI14:
	ret
LFE6:
	.const
	.align	3
lC0:
	.ascii "anubis_types-sss.adb"
	.space 1
	.text
	.align	2
	.globl _anubis_types__sss__split_secret
_anubis_types__sss__split_secret:
LFB7:
	.loc 1 183 4
	sub	x10, sp, #16384
	str	xzr, [x10, 3824]
	stp	x29, x30, [sp, -96]!
LCFI15:
	mov	x29, sp
LCFI16:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
	stp	x25, x26, [sp, 64]
	str	x27, [sp, 80]
	sub	sp, sp, #176
LCFI17:
	stp	x0, x1, [x29, -96]
	str	w2, [x29, -100]
	str	w3, [x29, -104]
	stp	x4, x5, [x29, -120]
	.loc 1 183 4
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x1, [x0]
	str	x1, [x29, -8]
	mov	x1, 0
	ldr	x0, [x29, -88]
	ldr	w19, [x0]
	ldr	x0, [x29, -88]
	ldr	w2, [x0, 4]
	cmp	w2, w19
	blt	L36
	.loc 1 183 4 is_stmt 0 discriminator 1
	sxtw	x0, w2
	mov	x26, x0
	asr	x0, x0, 63
	mov	x27, x0
	sxtw	x0, w19
	mov	x24, x0
	asr	x0, x0, 63
	mov	x25, x0
	subs	x1, x26, x24
	sbc	x0, x27, x25
	mov	x20, x1
	mov	x21, x0
	mov	x0, 1
	adds	x1, x20, x0
	mov	x0, 0
	adc	x0, x21, x0
	str	x1, [x29, -144]
	str	x0, [x29, -136]
	b	L37
L36:
	.loc 1 183 4 discriminator 2
	stp	xzr, xzr, [x29, -144]
L37:
LBB8:
	mov	x0, sp
	mov	x24, x0
	.loc 1 183 4 discriminator 4
	ldr	x0, [x29, -112]
	ldrsh	w0, [x0]
	sxth	x20, w0
	ldr	x0, [x29, -112]
	ldrsh	w1, [x0, 2]
	ldr	x0, [x29, -112]
	ldrsh	w0, [x0]
	cmp	w1, w0
	.loc 1 183 4 discriminator 8
	ldr	x0, [x29, -112]
	ldrsh	w1, [x0, 2]
	ldr	x0, [x29, -112]
	ldrsh	w0, [x0]
	cmp	w1, w0
	blt	L41
	.loc 1 183 4 discriminator 9
	ldr	x0, [x29, -112]
	ldrsh	w0, [x0, 2]
	sxth	x1, w0
	ldr	x0, [x29, -112]
	ldrsh	w0, [x0]
	sxth	x0, w0
	sub	x0, x1, x0
	add	x0, x0, 1
	mov	x6, x0
	mov	x7, 0
	mov	x0, 2144
	mov	x1, 0
	mul	x4, x6, x0
	umulh	x3, x6, x0
	madd	x3, x7, x0, x3
	madd	x3, x6, x1, x3
	str	x4, [x29, -176]
	str	x3, [x29, -168]
L41:
	.loc 1 183 4 discriminator 12
	ldr	x0, [x29, -112]
	ldrsh	w1, [x0, 2]
	ldr	x0, [x29, -112]
	ldrsh	w0, [x0]
	cmp	w1, w0
	.loc 1 183 4 discriminator 16
	sxtw	x21, w19
	cmp	w2, w19
	.loc 1 183 4 discriminator 20
	cmp	w2, w19
	blt	L47
	.loc 1 183 4 discriminator 21
	sxtw	x1, w2
	sxtw	x0, w19
	sub	x0, x1, x0
	add	x0, x0, 1
	mov	x12, x0
	mov	x13, 0
	lsr	x0, x12, 61
	lsl	x23, x13, 3
	orr	x23, x0, x23
	lsl	x22, x12, 3
L47:
	.loc 1 183 4 discriminator 24
	cmp	w2, w19
	.loc 1 195 7 is_stmt 1
	ldr	w0, [x29, -100]
	sub	w0, w0, #1
	str	w0, [x29, -48]
	ldr	w0, [x29, -48]
	cmp	w0, 0
	blt	L50
	.loc 1 195 7 is_stmt 0 discriminator 1
	ldrsw	x0, [x29, -48]
	str	x0, [x29, -24]
	b	L51
L50:
	.loc 1 195 7 discriminator 2
	mov	x0, -1
	str	x0, [x29, -24]
L51:
	.loc 1 195 7 discriminator 4
	ldr	w0, [x29, -48]
	cmp	w0, 0
	blt	L53
	.loc 1 195 7 discriminator 5
	ldrsw	x0, [x29, -48]
	add	x0, x0, 1
	mov	x10, x0
	mov	x11, 0
	lsr	x0, x10, 61
	lsl	x17, x11, 3
	orr	x17, x0, x17
	lsl	x16, x10, 3
L53:
	.loc 1 195 7 discriminator 8
	ldr	w0, [x29, -48]
	cmp	w0, 0
	.loc 1 195 7 discriminator 12
	ldr	w0, [x29, -48]
	cmp	w0, 0
	blt	L57
	.loc 1 195 7 discriminator 13
	ldrsw	x0, [x29, -48]
	add	x0, x0, 1
	mov	x8, x0
	mov	x9, 0
	lsr	x0, x8, 61
	lsl	x15, x9, 3
	orr	x15, x0, x15
	lsl	x14, x8, 3
L57:
	.loc 1 195 7 discriminator 16
	ldr	w0, [x29, -48]
	cmp	w0, 0
	blt	L58
	.loc 1 195 7 discriminator 17
	ldrsw	x0, [x29, -48]
	add	x0, x0, 1
	b	L59
L58:
	.loc 1 195 7 discriminator 18
	mov	x0, 0
L59:
	.loc 1 195 7 discriminator 20
	add	x0, x0, 15
	lsr	x0, x0, 4
	lsl	x1, x0, 4
	and	x4, x1, -4096
	sub	x0, sp, #12288
	sub	x3, x0, x4
L60:
	cmp	x0, x3
	beq	L61
	sub	x0, x0, #4096
	str	xzr, [x0]
	b	L60
L61:
	sub	x0, x1, x4
	sub	x0, x3, x0
	str	xzr, [x0]
	sub	sp, sp, x1
	mov	x0, sp
	.loc 1 195 7 discriminator 21
	str	x0, [x29, -16]
	.loc 1 198 11 is_stmt 1
	str	w19, [x29, -44]
	str	w2, [x29, -40]
	ldr	w1, [x29, -44]
	ldr	w0, [x29, -40]
	cmp	w1, w0
	bgt	L62
LBB9:
	.loc 1 198 11 is_stmt 0 discriminator 1
	ldr	w0, [x29, -44]
	str	w0, [x29, -60]
L88:
LBB10:
	.loc 1 201 10 is_stmt 1
	ldr	w0, [x29, -48]
	cmp	w0, 0
	bge	L63
	.loc 1 201 10 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L64
	bl	___stack_chk_fail
L64:
	mov	w1, 201
	adrp	x0, lC0@PAGE
	add	x0, x0, lC0@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L63:
	.loc 1 201 21 is_stmt 1 discriminator 2
	ldr	x1, [x29, -96]
	ldrsw	x0, [x29, -60]
	sub	x0, x0, x21
	ldrb	w1, [x1, x0]
	.loc 1 201 21 is_stmt 0 discriminator 3
	ldr	x0, [x29, -16]
	strb	w1, [x0]
	.loc 1 204 14 is_stmt 1
	ldr	w0, [x29, -100]
	sub	w0, w0, #1
	str	w0, [x29, -36]
	ldr	w0, [x29, -36]
	cmp	w0, 0
	ble	L65
LBB11:
	.loc 1 204 14 is_stmt 0 discriminator 1
	mov	w0, 1
	str	w0, [x29, -56]
L68:
LBB12:
	.loc 1 206 46 is_stmt 1
	mov	w0, 256
	bl	_randombytes_uniform
	mov	w1, w0
	.loc 1 206 46 is_stmt 0 discriminator 1
	cmp	w1, 255
	bls	L66
	.loc 1 206 46 discriminator 2
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L67
	bl	___stack_chk_fail
L67:
	mov	w1, 206
	adrp	x0, lC0@PAGE
	add	x0, x0, lC0@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L66:
	.loc 1 206 24 is_stmt 1 discriminator 3
	ldrsw	x0, [x29, -56]
	and	w2, w1, 255
	ldr	x1, [x29, -16]
	strb	w2, [x1, x0]
LBE12:
	.loc 1 204 14
	ldr	w1, [x29, -56]
	ldr	w0, [x29, -36]
	cmp	w1, w0
	beq	L65
	.loc 1 204 14 is_stmt 0 discriminator 2
	ldr	w0, [x29, -56]
	add	w0, w0, 1
	str	w0, [x29, -56]
	.loc 1 207 18 is_stmt 1
	b	L68
L65:
LBE11:
	.loc 1 210 14
	ldr	x0, [x29, -112]
	ldrh	w0, [x0]
	strh	w0, [x29, -64]
	ldr	x0, [x29, -112]
	ldrh	w0, [x0, 2]
	strh	w0, [x29, -62]
	ldrsh	w1, [x29, -64]
	ldrsh	w0, [x29, -62]
	cmp	w1, w0
	bgt	L69
LBB13:
	.loc 1 210 14 is_stmt 0 discriminator 1
	ldrh	w0, [x29, -64]
	strh	w0, [x29, -66]
L87:
LBB14:
	.loc 1 213 47 is_stmt 1
	ldrsh	w0, [x29, -66]
	cmp	w0, 0
	blt	L70
	.loc 1 213 47 is_stmt 0 discriminator 2
	ldrsh	w0, [x29, -66]
	cmp	w0, 255
	ble	L71
L70:
	.loc 1 213 47 discriminator 3
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L72
	bl	___stack_chk_fail
L72:
	mov	w1, 213
	adrp	x0, lC0@PAGE
	add	x0, x0, lC0@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L71:
	.loc 1 213 16 is_stmt 1 discriminator 4
	ldrh	w0, [x29, -66]
	strb	w0, [x29, -68]
	.loc 1 214 41
	ldr	x0, [x29, -16]
	str	x0, [x29, -160]
	str	wzr, [x29, -32]
	ldr	w0, [x29, -48]
	str	w0, [x29, -28]
	sub	x0, x29, #32
	str	x0, [x29, -152]
	ldrb	w0, [x29, -68]
	mov	w2, w0
	ldp	x0, x1, [x29, -160]
	bl	_anubis_types__sss__eval_polynomial
	.loc 1 214 41 is_stmt 0 discriminator 1
	strb	w0, [x29, -67]
	.loc 1 216 40 is_stmt 1
	ldrsh	w0, [x29, -66]
	cmp	w0, 0
	ble	L73
	.loc 1 216 40 is_stmt 0 discriminator 2
	ldrsh	w0, [x29, -66]
	cmp	w0, 255
	ble	L74
L73:
	.loc 1 216 40 discriminator 3
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L75
	bl	___stack_chk_fail
L75:
	mov	w1, 216
	adrp	x0, lC0@PAGE
	add	x0, x0, lC0@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L74:
	.loc 1 216 37 is_stmt 1 discriminator 4
	ldr	x2, [x29, -120]
	ldrsh	x0, [x29, -66]
	sub	x1, x0, x20
	mov	x0, x1
	lsl	x0, x0, 4
	add	x0, x0, x1
	lsl	x0, x0, 2
	sub	x0, x0, x1
	lsl	x0, x0, 2
	add	x0, x2, x0
	ldrh	w1, [x29, -66]
	strh	w1, [x0]
	.loc 1 217 47
	mov	w1, 0
	ldr	w0, [x29, -60]
	subs	w0, w0, w19
	bvc	L76
	mov	w1, 1
L76:
	mov	w2, w0
	.loc 1 217 47 is_stmt 0 discriminator 1
	mov	w0, w1
	cmp	w0, 0
	beq	L78
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L79
	bl	___stack_chk_fail
L79:
	mov	w1, 217
	adrp	x0, lC0@PAGE
	add	x0, x0, lC0@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L78:
	.loc 1 217 47 discriminator 2
	mov	w0, w2
	.loc 1 217 62 is_stmt 1 discriminator 4
	mov	w1, 2147483647
	cmp	w0, w1
	bne	L80
	.loc 1 217 62 is_stmt 0 discriminator 5
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L81
	bl	___stack_chk_fail
L81:
	mov	w1, 217
	adrp	x0, lC0@PAGE
	add	x0, x0, lC0@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L80:
	.loc 1 217 62 discriminator 6
	add	w0, w0, 1
	.loc 1 217 34 is_stmt 1 discriminator 8
	cmp	w0, 0
	ble	L82
	.loc 1 217 34 is_stmt 0 discriminator 10
	cmp	w0, 256
	ble	L83
L82:
	.loc 1 217 34 discriminator 11
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L84
	bl	___stack_chk_fail
L84:
	mov	w1, 217
	adrp	x0, lC0@PAGE
	add	x0, x0, lC0@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L83:
	.loc 1 217 67 is_stmt 1 discriminator 12
	ldr	x3, [x29, -120]
	ldrsh	x0, [x29, -66]
	.loc 1 217 47 discriminator 12
	ldr	w1, [x29, -60]
	sub	w1, w1, w19
	.loc 1 217 62 discriminator 12
	add	w1, w1, 1
	sxtw	x2, w1
	.loc 1 217 67 discriminator 12
	sub	x1, x0, x20
	mov	x0, x1
	lsl	x0, x0, 4
	add	x0, x0, x1
	lsl	x0, x0, 2
	sub	x0, x0, x1
	lsl	x0, x0, 2
	add	x0, x3, x0
	add	x0, x0, x2
	ldrb	w1, [x29, -67]
	strb	w1, [x0, 1]
	.loc 1 218 51
	ldr	x1, [x29, -144]
	mov	x0, 2147483647
	cmp	x1, x0
	ble	L85
	.loc 1 218 51 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L86
	bl	___stack_chk_fail
L86:
	mov	w1, 218
	adrp	x0, lC0@PAGE
	add	x0, x0, lC0@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L85:
	.loc 1 218 42 is_stmt 1 discriminator 2
	ldr	x2, [x29, -120]
	ldrsh	x0, [x29, -66]
	mov	w3, w1
	sub	x1, x0, x20
	mov	x0, x1
	lsl	x0, x0, 4
	add	x0, x0, x1
	lsl	x0, x0, 2
	sub	x0, x0, x1
	lsl	x0, x0, 2
	add	x0, x2, x0
	str	w3, [x0, 260]
	.loc 1 219 41
	ldr	x2, [x29, -120]
	ldrsh	x0, [x29, -66]
	sub	x1, x0, x20
	mov	x0, x1
	lsl	x0, x0, 4
	add	x0, x0, x1
	lsl	x0, x0, 2
	sub	x0, x0, x1
	lsl	x0, x0, 2
	add	x0, x2, x0
	mov	w1, 1
	strb	w1, [x0, 264]
LBE14:
	.loc 1 210 14
	ldrsh	w1, [x29, -66]
	ldrsh	w0, [x29, -62]
	cmp	w1, w0
	beq	L69
	.loc 1 210 14 is_stmt 0 discriminator 2
	ldrh	w0, [x29, -66]
	add	w0, w0, 1
	strh	w0, [x29, -66]
	.loc 1 221 18 is_stmt 1
	b	L87
L69:
LBE13:
LBE10:
	.loc 1 198 11
	ldr	w1, [x29, -60]
	ldr	w0, [x29, -40]
	cmp	w1, w0
	beq	L62
	.loc 1 198 11 is_stmt 0 discriminator 2
	ldr	w0, [x29, -60]
	add	w0, w0, 1
	str	w0, [x29, -60]
	.loc 1 222 15 is_stmt 1
	b	L88
L62:
LBE9:
	.loc 1 225 11
	ldr	w0, [x29, -48]
	cmp	w0, 0
	blt	L89
LBB15:
	.loc 1 225 11 is_stmt 0 discriminator 1
	str	wzr, [x29, -52]
L92:
	.loc 1 226 10 is_stmt 1
	ldr	w1, [x29, -52]
	ldr	w0, [x29, -48]
	cmp	w1, w0
	ble	L90
	.loc 1 226 10 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L91
	bl	___stack_chk_fail
L91:
	mov	w1, 226
	adrp	x0, lC0@PAGE
	add	x0, x0, lC0@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L90:
	.loc 1 226 21 is_stmt 1 discriminator 2
	ldrsw	x0, [x29, -52]
	ldr	x1, [x29, -16]
	strb	wzr, [x1, x0]
	.loc 1 225 11
	ldr	w1, [x29, -52]
	ldr	w0, [x29, -48]
	cmp	w1, w0
	beq	L89
	.loc 1 225 11 is_stmt 0 discriminator 2
	ldr	w0, [x29, -52]
	add	w0, w0, 1
	str	w0, [x29, -52]
	.loc 1 227 15 is_stmt 1
	b	L92
L89:
LBE15:
	.loc 1 229 15
	mov	w0, 1
	strb	w0, [x29, -69]
	.loc 1 230 8
	mov	sp, x24
LBE8:
	.loc 1 230 8 is_stmt 0 discriminator 1
	ldrb	w0, [x29, -69]
	.loc 1 230 8 discriminator 3
	mov	w1, w0
	.loc 1 230 8
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x3, [x29, -8]
	ldr	x2, [x0]
	subs	x3, x3, x2
	mov	x2, 0
	beq	L94
	bl	___stack_chk_fail
L94:
	mov	w0, w1
	mov	sp, x29
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldr	x27, [sp, 80]
	ldp	x29, x30, [sp], 96
LCFI18:
	ret
LFE7:
	.align	2
	.globl _anubis_types__sss__combine_shares
_anubis_types__sss__combine_shares:
LFB8:
	.loc 1 236 4 is_stmt 1
	sub	x10, sp, #16384
	str	xzr, [x10, 3936]
	sub	sp, sp, #160
LCFI19:
	stp	x29, x30, [sp, 96]
LCFI20:
	add	x29, sp, 96
LCFI21:
	stp	x19, x20, [sp, 112]
	stp	x21, x22, [sp, 128]
	str	x23, [sp, 144]
LCFI22:
	stp	x0, x1, [x29, -64]
	str	w2, [x29, -68]
	stp	x3, x4, [x29, -88]
	.loc 1 236 4
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x1, [x0]
	str	x1, [x29, -8]
	mov	x1, 0
	ldr	x0, [x29, -56]
	ldrsh	w19, [x0]
	ldr	x0, [x29, -56]
	ldrsh	w20, [x0, 2]
LBB16:
	ldr	x0, [x29, -80]
	ldr	w0, [x0]
	sxtw	x22, w0
	ldr	x0, [x29, -80]
	ldr	w1, [x0, 4]
	ldr	x0, [x29, -80]
	ldr	w0, [x0]
	cmp	w1, w0
	.loc 1 236 4 is_stmt 0 discriminator 4
	ldr	x0, [x29, -80]
	ldr	w1, [x0, 4]
	ldr	x0, [x29, -80]
	ldr	w0, [x0]
	cmp	w1, w0
	blt	L99
	.loc 1 236 4 discriminator 5
	ldr	x0, [x29, -80]
	ldr	w0, [x0, 4]
	sxtw	x1, w0
	ldr	x0, [x29, -80]
	ldr	w0, [x0]
	sxtw	x0, w0
	sub	x0, x1, x0
	add	x0, x0, 1
	mov	x8, x0
	mov	x9, 0
	lsr	x0, x8, 61
	lsl	x11, x9, 3
	orr	x11, x0, x11
	lsl	x10, x8, 3
L99:
	.loc 1 236 4 discriminator 8
	ldr	x0, [x29, -80]
	ldr	w1, [x0, 4]
	ldr	x0, [x29, -80]
	ldr	w0, [x0]
	cmp	w1, w0
	.loc 1 236 4 discriminator 12
	sxth	x21, w19
	cmp	w20, w19
	.loc 1 236 4 discriminator 16
	cmp	w20, w19
	blt	L105
	.loc 1 236 4 discriminator 17
	sxth	x1, w20
	sxth	x0, w19
	sub	x0, x1, x0
	add	x0, x0, 1
	mov	x6, x0
	mov	x7, 0
	mov	x0, 2144
	mov	x1, 0
	mul	x3, x6, x0
	umulh	x2, x6, x0
	madd	x2, x7, x0, x2
	madd	x2, x6, x1, x2
	mov	x12, x3
	mov	x13, x2
L105:
	.loc 1 236 4 discriminator 20
	cmp	w20, w19
	.loc 1 243 7 is_stmt 1
	strb	wzr, [x29, -37]
	.loc 1 245 21
	ldr	x23, [x29, -88]
	ldr	x0, [x29, -80]
	ldr	w2, [x0, 4]
	ldr	x0, [x29, -80]
	ldr	w0, [x0]
	mov	w1, w0
	mov	w0, w2
	bl	__GLOBAL__SZ3_anubis_types__sss
	mov	x1, x0
	.loc 1 245 21 is_stmt 0 discriminator 1
	mov	x0, x23
	mov	x2, x1
	mov	w1, 0
	bl	_memset
	.loc 1 247 24 is_stmt 1
	sxth	x1, w19
	ldrsw	x0, [x29, -68]
	sub	x0, x0, #1
	add	x1, x1, x0
	sxth	x0, w20
	.loc 1 247 7
	cmp	x1, x0
	ble	L108
	.loc 1 248 18
	strb	wzr, [x29, -39]
	.loc 1 249 10
	b	L109
L108:
	.loc 1 253 11
	ldr	x0, [x29, -80]
	ldr	w0, [x0]
	str	w0, [x29, -16]
	ldr	x0, [x29, -80]
	ldr	w0, [x0, 4]
	str	w0, [x29, -12]
	ldr	w1, [x29, -16]
	ldr	w0, [x29, -12]
	cmp	w1, w0
	bgt	L110
LBB17:
	.loc 1 253 11 is_stmt 0 discriminator 1
	ldr	w0, [x29, -16]
	str	w0, [x29, -20]
L151:
	.loc 1 258 15 is_stmt 1
	strb	wzr, [x29, -38]
LBB18:
	.loc 1 262 44
	mov	w0, w19
	cmp	w0, 0
	bge	L111
	.loc 1 262 44 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L112
	bl	___stack_chk_fail
L112:
	mov	w1, 262
	adrp	x0, lC0@PAGE
	add	x0, x0, lC0@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L111:
	.loc 1 262 64 is_stmt 1 discriminator 2
	mov	w2, 0
	ldr	w1, [x29, -68]
	adds	w1, w0, w1
	bvc	L113
	mov	w2, 1
L113:
	.loc 1 262 64 is_stmt 0 discriminator 1
	mov	w1, w2
	cmp	w1, 0
	beq	L115
	.loc 1 262 64 discriminator 3
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L116
	bl	___stack_chk_fail
L116:
	mov	w1, 262
	adrp	x0, lC0@PAGE
	add	x0, x0, lC0@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L115:
	.loc 1 262 64 discriminator 4
	ldr	w1, [x29, -68]
	adds	w0, w0, w1
	.loc 1 262 64 discriminator 1
	sub	w0, w0, #1
	.loc 1 262 64 discriminator 6
	cmp	w0, 0
	ble	L119
	.loc 1 262 64 discriminator 8
	cmp	w0, 255
	ble	L120
L119:
	.loc 1 262 64 discriminator 9
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L121
	bl	___stack_chk_fail
L121:
	mov	w1, 262
	adrp	x0, lC0@PAGE
	add	x0, x0, lC0@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L120:
	.loc 1 261 13 is_stmt 1
	strh	w0, [x29, -26]
	.loc 1 264 17
	mov	w0, w19
	strh	w0, [x29, -24]
LBB19:
	ldrh	w0, [x29, -24]
	strh	w0, [x29, -30]
L150:
	.loc 1 264 17 is_stmt 0 discriminator 1
	ldrsh	w1, [x29, -30]
	ldrsh	w0, [x29, -26]
	cmp	w1, w0
	bgt	L122
	.loc 1 268 23 is_stmt 1
	ldrsh	w0, [x29, -30]
	cmp	w0, w19
	blt	L123
	.loc 1 268 23 is_stmt 0 discriminator 2
	ldrsh	w0, [x29, -30]
	cmp	w0, w20
	ble	L124
L123:
	.loc 1 268 23 discriminator 3
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L125
	bl	___stack_chk_fail
L125:
	mov	w1, 268
	adrp	x0, lC0@PAGE
	add	x0, x0, lC0@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L124:
	.loc 1 268 19 is_stmt 1 discriminator 4
	ldr	x2, [x29, -64]
	ldrsh	x0, [x29, -30]
	sub	x1, x0, x21
	mov	x0, x1
	lsl	x0, x0, 4
	add	x0, x0, x1
	lsl	x0, x0, 2
	sub	x0, x0, x1
	lsl	x0, x0, 2
	add	x0, x2, x0
	ldrb	w0, [x0, 264]
	.loc 1 268 19 is_stmt 0 discriminator 5
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 268 16 is_stmt 1 discriminator 5
	cmp	w0, 0
	beq	L126
	.loc 1 269 26
	mov	w0, 1
	strb	w0, [x29, -37]
	.loc 1 270 19
	b	L110
L126:
LBB20:
	.loc 1 274 47
	ldrsh	w0, [x29, -30]
	cmp	w0, w19
	blt	L128
	.loc 1 274 47 is_stmt 0 discriminator 2
	ldrsh	w0, [x29, -30]
	cmp	w0, w20
	ble	L129
L128:
	.loc 1 274 47 discriminator 3
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L130
	bl	___stack_chk_fail
L130:
	mov	w1, 274
	adrp	x0, lC0@PAGE
	add	x0, x0, lC0@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L129:
	.loc 1 274 19 is_stmt 1 discriminator 4
	ldr	x2, [x29, -64]
	ldrsh	x0, [x29, -30]
	sub	x1, x0, x21
	mov	x0, x1
	lsl	x0, x0, 4
	add	x0, x0, x1
	lsl	x0, x0, 2
	sub	x0, x0, x1
	lsl	x0, x0, 2
	add	x0, x2, x0
	ldrsh	w0, [x0]
	.loc 1 274 19 is_stmt 0 discriminator 5
	strb	w0, [x29, -35]
	.loc 1 275 41 is_stmt 1
	ldrsh	w0, [x29, -30]
	cmp	w0, w19
	blt	L131
	.loc 1 275 41 is_stmt 0 discriminator 2
	ldrsh	w0, [x29, -30]
	cmp	w0, w20
	ble	L132
L131:
	.loc 1 275 41 discriminator 3
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L133
	bl	___stack_chk_fail
L133:
	mov	w1, 275
	adrp	x0, lC0@PAGE
	add	x0, x0, lC0@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L132:
	.loc 1 275 64 is_stmt 1 discriminator 4
	ldr	x0, [x29, -80]
	ldr	w0, [x0]
	mov	w2, 0
	ldr	w1, [x29, -20]
	subs	w0, w1, w0
	bvc	L134
	mov	w2, 1
L134:
	mov	w1, w0
	.loc 1 275 64 is_stmt 0 discriminator 1
	mov	w0, w2
	cmp	w0, 0
	beq	L136
	.loc 1 275 64 discriminator 5
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L137
	bl	___stack_chk_fail
L137:
	mov	w1, 275
	adrp	x0, lC0@PAGE
	add	x0, x0, lC0@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L136:
	.loc 1 275 64 discriminator 6
	mov	w0, w1
	.loc 1 275 86 is_stmt 1 discriminator 8
	mov	w1, 2147483647
	cmp	w0, w1
	bne	L138
	.loc 1 275 86 is_stmt 0 discriminator 9
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L139
	bl	___stack_chk_fail
L139:
	mov	w1, 275
	adrp	x0, lC0@PAGE
	add	x0, x0, lC0@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L138:
	.loc 1 275 86 discriminator 10
	add	w0, w0, 1
	.loc 1 275 51 is_stmt 1 discriminator 12
	cmp	w0, 0
	ble	L140
	.loc 1 275 51 is_stmt 0 discriminator 14
	cmp	w0, 256
	ble	L141
L140:
	.loc 1 275 51 discriminator 15
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L142
	bl	___stack_chk_fail
L142:
	mov	w1, 275
	adrp	x0, lC0@PAGE
	add	x0, x0, lC0@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L141:
	.loc 1 275 19 is_stmt 1 discriminator 16
	ldr	x3, [x29, -64]
	ldrsh	x0, [x29, -30]
	.loc 1 275 64 discriminator 16
	ldr	x1, [x29, -80]
	ldr	w1, [x1]
	ldr	w2, [x29, -20]
	sub	w1, w2, w1
	.loc 1 275 86 discriminator 16
	add	w1, w1, 1
	sxtw	x2, w1
	.loc 1 275 19 discriminator 16
	sub	x1, x0, x21
	mov	x0, x1
	lsl	x0, x0, 4
	add	x0, x0, x1
	lsl	x0, x0, 2
	sub	x0, x0, x1
	lsl	x0, x0, 2
	add	x0, x3, x0
	add	x0, x0, x2
	ldrb	w0, [x0, 1]
	.loc 1 275 19 is_stmt 0 discriminator 17
	strb	w0, [x29, -34]
	.loc 1 276 19 is_stmt 1
	ldrb	w0, [x29, -34]
	strb	w0, [x29, -36]
	.loc 1 278 23
	mov	w0, w19
	strh	w0, [x29, -22]
LBB21:
	ldrh	w0, [x29, -22]
	strh	w0, [x29, -28]
L149:
	.loc 1 278 23 is_stmt 0 discriminator 1
	ldrsh	w1, [x29, -28]
	ldrsh	w0, [x29, -26]
	cmp	w1, w0
	bgt	L143
	.loc 1 282 22 is_stmt 1
	ldrsh	w1, [x29, -30]
	ldrsh	w0, [x29, -28]
	cmp	w1, w0
	beq	L144
LBB22:
	.loc 1 284 56
	ldrsh	w0, [x29, -28]
	cmp	w0, w19
	blt	L145
	.loc 1 284 56 is_stmt 0 discriminator 2
	ldrsh	w0, [x29, -28]
	cmp	w0, w20
	ble	L146
L145:
	.loc 1 284 56 discriminator 3
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L147
	bl	___stack_chk_fail
L147:
	mov	w1, 284
	adrp	x0, lC0@PAGE
	add	x0, x0, lC0@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L146:
	.loc 1 284 28 is_stmt 1 discriminator 4
	ldr	x2, [x29, -64]
	ldrsh	x0, [x29, -28]
	sub	x1, x0, x21
	mov	x0, x1
	lsl	x0, x0, 4
	add	x0, x0, x1
	lsl	x0, x0, 2
	sub	x0, x0, x1
	lsl	x0, x0, 2
	add	x0, x2, x0
	ldrsh	w0, [x0]
	.loc 1 284 28 is_stmt 0 discriminator 5
	strb	w0, [x29, -33]
	.loc 1 285 57 is_stmt 1
	ldrb	w0, [x29, -33]
	mov	w1, w0
	mov	w0, 0
	bl	_anubis_types__sss__gf_sub
	.loc 1 285 57 is_stmt 0 discriminator 1
	strb	w0, [x29, -32]
	.loc 1 286 59 is_stmt 1
	ldrb	w1, [x29, -33]
	ldrb	w0, [x29, -35]
	bl	_anubis_types__sss__gf_sub
	.loc 1 286 59 is_stmt 0 discriminator 1
	strb	w0, [x29, -31]
	.loc 1 288 28 is_stmt 1
	ldrb	w0, [x29, -31]
	cmp	w0, 0
	bne	L148
	.loc 1 289 38
	mov	w0, 1
	strb	w0, [x29, -37]
	.loc 1 290 31
	b	L110
L148:
	.loc 1 293 37
	ldrb	w1, [x29, -31]
	ldrb	w0, [x29, -32]
	bl	_anubis_types__sss__gf_div
	mov	w1, w0
	.loc 1 293 37 is_stmt 0 discriminator 1
	ldrb	w0, [x29, -36]
	bl	_anubis_types__sss__gf_mult
	.loc 1 293 37 discriminator 2
	strb	w0, [x29, -36]
L144:
LBE22:
	.loc 1 278 23 is_stmt 1 discriminator 2
	ldrh	w0, [x29, -28]
	add	w0, w0, 1
	strh	w0, [x29, -28]
	.loc 1 296 27
	b	L149
L143:
LBE21:
	.loc 1 298 27
	ldrb	w1, [x29, -36]
	ldrb	w0, [x29, -38]
	bl	_anubis_types__sss__gf_add
	.loc 1 298 27 is_stmt 0 discriminator 1
	strb	w0, [x29, -38]
LBE20:
	.loc 1 264 17 is_stmt 1 discriminator 2
	ldrh	w0, [x29, -30]
	add	w0, w0, 1
	strh	w0, [x29, -30]
	.loc 1 300 21
	b	L150
L122:
LBE19:
LBE18:
	.loc 1 303 10
	ldrb	w0, [x29, -37]
	cmp	w0, 0
	bne	L110
	.loc 1 304 35
	ldr	x1, [x29, -88]
	ldrsw	x0, [x29, -20]
	sub	x0, x0, x22
	ldrb	w2, [x29, -38]
	strb	w2, [x1, x0]
	.loc 1 253 11
	ldr	w1, [x29, -20]
	ldr	w0, [x29, -12]
	cmp	w1, w0
	beq	L110
	.loc 1 253 11 is_stmt 0 discriminator 2
	ldr	w0, [x29, -20]
	add	w0, w0, 1
	str	w0, [x29, -20]
	.loc 1 305 16 is_stmt 1
	b	L151
L110:
LBE17:
	.loc 1 307 18
	ldrb	w0, [x29, -37]
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 307 15
	strb	w0, [x29, -39]
	.loc 1 309 10
	ldrb	w0, [x29, -39]
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 309 7
	cmp	w0, 0
	beq	L155
	.loc 1 310 24
	ldr	x19, [x29, -88]
	ldr	x0, [x29, -80]
	ldr	w2, [x0, 4]
	ldr	x0, [x29, -80]
	ldr	w0, [x0]
	mov	w1, w0
	mov	w0, w2
	bl	__GLOBAL__SZ3_anubis_types__sss
	mov	x1, x0
	.loc 1 310 24 is_stmt 0 discriminator 1
	mov	x0, x19
	mov	x2, x1
	mov	w1, 0
	bl	_memset
L155:
	.loc 1 312 8 is_stmt 1
	nop
L109:
LBE16:
	ldrb	w0, [x29, -39]
	.loc 1 312 8 is_stmt 0 discriminator 2
	mov	w1, w0
	.loc 1 312 8
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x3, [x29, -8]
	ldr	x2, [x0]
	subs	x3, x3, x2
	mov	x2, 0
	beq	L154
	bl	___stack_chk_fail
L154:
	mov	w0, w1
	ldp	x29, x30, [sp, 96]
	ldp	x19, x20, [sp, 112]
	ldp	x21, x22, [sp, 128]
	ldr	x23, [sp, 144]
	add	sp, sp, 160
LCFI23:
	ret
LFE8:
	.align	2
	.globl _anubis_types__sss__get_share_index
_anubis_types__sss__get_share_index:
LFB9:
	.loc 1 318 4 is_stmt 1
	sub	sp, sp, #16
LCFI24:
	str	x0, [sp, 8]
	.loc 1 319 13
	ldr	x0, [sp, 8]
	ldrsh	w0, [x0]
	.loc 1 318 4
	add	sp, sp, 16
LCFI25:
	ret
LFE9:
	.align	2
	.globl _anubis_types__sss__make_share
_anubis_types__sss__make_share:
LFB10:
	.loc 1 321 4
	sub	x10, sp, #16384
	str	xzr, [x10, 3696]
	sub	sp, sp, #400
LCFI26:
	stp	x29, x30, [sp, 336]
LCFI27:
	add	x29, sp, 336
LCFI28:
	stp	x19, x20, [sp, 352]
	stp	x21, x22, [sp, 368]
	stp	x23, x24, [sp, 384]
LCFI29:
	mov	x24, x8
	sub	x3, x29, #512
	stp	x1, x2, [x3, 184]
	sub	x1, x29, #4096
	strh	w0, [x1, 3790]
	.loc 1 321 4
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x1, [x0]
	str	x1, [x29, -8]
	mov	x1, 0
	sub	x0, x29, #16384
	ldr	x0, [x0, 16064]
	ldr	w19, [x0]
	sub	x0, x29, #16384
	ldr	x0, [x0, 16064]
	ldr	w20, [x0, 4]
	cmp	w20, w19
	blt	L159
	.loc 1 321 4 is_stmt 0 discriminator 1
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
	b	L160
L159:
	.loc 1 321 4 discriminator 2
	mov	x22, 0
	mov	x23, 0
L160:
LBB23:
	.loc 1 321 4 discriminator 4
	sxtw	x21, w19
	cmp	w20, w19
	.loc 1 321 4 discriminator 8
	cmp	w20, w19
	blt	L164
	.loc 1 321 4 discriminator 9
	sxtw	x1, w20
	sxtw	x0, w19
	sub	x0, x1, x0
	add	x0, x0, 1
	mov	x4, x0
	mov	x5, 0
	lsr	x0, x4, 61
	lsl	x7, x5, 3
	orr	x7, x0, x7
	lsl	x6, x4, 3
L164:
	.loc 1 321 4 discriminator 12
	cmp	w20, w19
	.loc 1 325 7 is_stmt 1
	sub	x0, x29, #280
	bl	_anubis_types__sss__secret_shareIP
	.loc 1 327 15
	sub	x0, x29, #4096
	ldrh	w0, [x0, 3790]
	sub	x1, x29, #4096
	strh	w0, [x1, 3816]
	.loc 1 328 27
	mov	x0, x22
	mov	x1, 2147483647
	cmp	x0, x1
	ble	L167
	.loc 1 328 27 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L168
	bl	___stack_chk_fail
L168:
	mov	w1, 328
	adrp	x0, lC0@PAGE
	add	x0, x0, lC0@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L167:
	.loc 1 328 20 is_stmt 1 discriminator 2
	str	w0, [x29, -20]
	.loc 1 329 15
	sub	x0, x29, #280
	add	x0, x0, 2
	movi	v31.4s, 0
	str	q31, [x0]
	str	q31, [x0, 16]
	str	q31, [x0, 32]
	str	q31, [x0, 48]
	str	q31, [x0, 64]
	str	q31, [x0, 80]
	str	q31, [x0, 96]
	str	q31, [x0, 112]
	str	q31, [x0, 128]
	str	q31, [x0, 144]
	str	q31, [x0, 160]
	str	q31, [x0, 176]
	str	q31, [x0, 192]
	str	q31, [x0, 208]
	str	q31, [x0, 224]
	str	q31, [x0, 240]
	.loc 1 330 11
	sub	x0, x29, #16384
	str	w19, [x0, 16096]
	sub	x0, x29, #16384
	str	w20, [x0, 16100]
	sub	x0, x29, #16384
	ldr	w1, [x0, 16096]
	sub	x0, x29, #16384
	ldr	w0, [x0, 16100]
	cmp	w1, w0
	bgt	L169
LBB24:
	.loc 1 330 11 is_stmt 0 discriminator 1
	sub	x0, x29, #16384
	ldr	w0, [x0, 16096]
	sub	x1, x29, #16384
	str	w0, [x1, 16092]
L179:
	.loc 1 333 21 is_stmt 1
	mov	w1, 0
	sub	x0, x29, #16384
	ldr	w0, [x0, 16092]
	subs	w0, w0, w19
	bvc	L170
	mov	w1, 1
L170:
	mov	w2, w0
	.loc 1 333 21 is_stmt 0 discriminator 1
	mov	w0, w1
	cmp	w0, 0
	beq	L172
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L173
	bl	___stack_chk_fail
L173:
	mov	w1, 333
	adrp	x0, lC0@PAGE
	add	x0, x0, lC0@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L172:
	.loc 1 333 21 discriminator 2
	mov	w0, w2
	.loc 1 333 34 is_stmt 1 discriminator 4
	mov	w1, 2147483647
	cmp	w0, w1
	bne	L174
	.loc 1 333 34 is_stmt 0 discriminator 5
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L175
	bl	___stack_chk_fail
L175:
	mov	w1, 333
	adrp	x0, lC0@PAGE
	add	x0, x0, lC0@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L174:
	.loc 1 333 34 discriminator 6
	add	w0, w0, 1
	.loc 1 333 15 is_stmt 1 discriminator 8
	cmp	w0, 0
	ble	L176
	.loc 1 333 15 is_stmt 0 discriminator 10
	cmp	w0, 256
	ble	L177
L176:
	.loc 1 333 15 discriminator 11
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L178
	bl	___stack_chk_fail
L178:
	mov	w1, 333
	adrp	x0, lC0@PAGE
	add	x0, x0, lC0@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L177:
	.loc 1 333 39 is_stmt 1 discriminator 12
	sub	x0, x29, #16384
	ldr	x2, [x0, 16056]
	sub	x0, x29, #16384
	ldrsw	x1, [x0, 16092]
	.loc 1 333 21 discriminator 12
	sub	x0, x29, #16384
	ldr	w0, [x0, 16092]
	sub	w0, w0, w19
	.loc 1 333 34 discriminator 12
	add	w0, w0, 1
	sxtw	x0, w0
	.loc 1 333 39 discriminator 12
	sub	x1, x1, x21
	ldrb	w2, [x2, x1]
	.loc 1 333 39 is_stmt 0 discriminator 13
	sub	x1, x29, #279
	strb	w2, [x1, x0]
	.loc 1 330 11 is_stmt 1
	sub	x0, x29, #16384
	ldr	w1, [x0, 16092]
	sub	x0, x29, #16384
	ldr	w0, [x0, 16100]
	cmp	w1, w0
	beq	L169
	.loc 1 330 11 is_stmt 0 discriminator 2
	sub	x0, x29, #16384
	ldr	w0, [x0, 16092]
	add	w0, w0, 1
	sub	x1, x29, #16384
	str	w0, [x1, 16092]
	.loc 1 334 15 is_stmt 1
	b	L179
L169:
LBE24:
	.loc 1 335 19
	mov	w0, 1
	strb	w0, [x29, -16]
	.loc 1 336 7
	mov	x3, x24
	sub	x0, x29, #280
	mov	x1, 268
	mov	x2, x1
	mov	x1, x0
	mov	x0, x3
	bl	_memcpy
LBE23:
	.loc 1 337 8
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L181
	bl	___stack_chk_fail
L181:
	ldp	x29, x30, [sp, 336]
	ldp	x19, x20, [sp, 352]
	ldp	x21, x22, [sp, 368]
	ldp	x23, x24, [sp, 384]
	add	sp, sp, 400
LCFI30:
	ret
LFE10:
	.align	2
	.globl _anubis_types__sss__get_share_length
_anubis_types__sss__get_share_length:
LFB11:
	.loc 1 339 4
	sub	sp, sp, #16
LCFI31:
	str	x0, [sp, 8]
	.loc 1 340 13
	ldr	x0, [sp, 8]
	ldr	w0, [x0, 260]
	.loc 1 339 4
	add	sp, sp, 16
LCFI32:
	ret
LFE11:
	.align	2
	.globl _anubis_types__sss__zeroize_share
_anubis_types__sss__zeroize_share:
LFB12:
	.loc 1 342 4
	sub	sp, sp, #32
LCFI33:
	str	x0, [sp, 8]
LBB25:
	.loc 1 344 11
	mov	w0, 1
	str	w0, [sp, 28]
L186:
	.loc 1 344 11 is_stmt 0 discriminator 3
	ldr	w0, [sp, 28]
	cmp	w0, 256
	bgt	L185
	.loc 1 347 22 is_stmt 1
	ldrsw	x0, [sp, 28]
	ldr	x1, [sp, 8]
	add	x0, x1, x0
	strb	wzr, [x0, 1]
	.loc 1 344 11 discriminator 2
	ldr	w0, [sp, 28]
	add	w0, w0, 1
	str	w0, [sp, 28]
	.loc 1 348 15
	b	L186
L185:
LBE25:
	.loc 1 349 19
	ldr	x0, [sp, 8]
	strb	wzr, [x0, 264]
	.loc 1 350 20
	ldr	x0, [sp, 8]
	str	wzr, [x0, 260]
	.loc 1 351 8
	nop
	add	sp, sp, 32
LCFI34:
	ret
LFE12:
	.align	2
_anubis_types__sss__gf_add:
LFB13:
	.loc 1 18 4
	sub	sp, sp, #16
LCFI35:
	strb	w0, [sp, 15]
	mov	w0, w1
	strb	w0, [sp, 14]
	.loc 1 19 10
	ldrb	w1, [sp, 15]
	ldrb	w0, [sp, 14]
	eor	w0, w1, w0
	and	w0, w0, 255
	.loc 1 18 4
	add	sp, sp, 16
LCFI36:
	ret
LFE13:
	.align	2
_anubis_types__sss__gf_sub:
LFB14:
	.loc 1 23 4
	sub	sp, sp, #16
LCFI37:
	strb	w0, [sp, 15]
	mov	w0, w1
	strb	w0, [sp, 14]
	.loc 1 24 10
	ldrb	w1, [sp, 15]
	ldrb	w0, [sp, 14]
	eor	w0, w1, w0
	and	w0, w0, 255
	.loc 1 23 4
	add	sp, sp, 16
LCFI38:
	ret
LFE14:
	.globl _anubis_types__sss_E
	.data
	.align	1
_anubis_types__sss_E:
	.space 2
	.text
	.align	2
__GLOBAL__SZ3_anubis_types__sss:
LFB18:
	.file 2 "/Users/sicarii/.local/share/alire/releases/gnatprove_14.1.1_91818ed8/libexec/spark/lib/gcc/aarch64-apple-darwin23.5.0/14.1.0/adainclude/interfac.ads"
	.loc 2 70 29
	str	x19, [sp, -32]!
LCFI39:
	str	w0, [sp, 28]
	str	w1, [sp, 24]
	ldr	w1, [sp, 24]
	ldr	w0, [sp, 28]
	cmp	w1, w0
	bgt	L193
	ldrsw	x1, [sp, 28]
	ldrsw	x0, [sp, 24]
	sub	x0, x1, x0
	add	x0, x0, 1
	b	L195
L193:
	mov	x0, 0
L195:
	mov	x19, x0
	mov	x0, x19
	ldr	x19, [sp], 32
LCFI40:
	ret
LFE18:
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
	.uleb128 0x10
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
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$10,LCFI3-LCFI2
	.long L$set$10
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$11,LCFI4-LCFI3
	.long L$set$11
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x95
	.uleb128 0x4
	.byte	0x4
	.set L$set$12,LCFI5-LCFI4
	.long L$set$12
	.byte	0xde
	.byte	0xdd
	.byte	0xd5
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE2:
LSFDE4:
	.set L$set$13,LEFDE4-LASFDE4
	.long L$set$13
LASFDE4:
	.set L$set$14,Lframe0-Lsection__debug_frame
	.long L$set$14
	.quad	LFB4
	.set L$set$15,LFE4-LFB4
	.quad L$set$15
	.byte	0x4
	.set L$set$16,LCFI6-LFB4
	.long L$set$16
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.set L$set$17,LCFI7-LCFI6
	.long L$set$17
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE4:
LSFDE6:
	.set L$set$18,LEFDE6-LASFDE6
	.long L$set$18
LASFDE6:
	.set L$set$19,Lframe0-Lsection__debug_frame
	.long L$set$19
	.quad	LFB5
	.set L$set$20,LFE5-LFB5
	.quad L$set$20
	.byte	0x4
	.set L$set$21,LCFI8-LFB5
	.long L$set$21
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$22,LCFI9-LCFI8
	.long L$set$22
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$23,LCFI10-LCFI9
	.long L$set$23
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE6:
LSFDE8:
	.set L$set$24,LEFDE8-LASFDE8
	.long L$set$24
LASFDE8:
	.set L$set$25,Lframe0-Lsection__debug_frame
	.long L$set$25
	.quad	LFB6
	.set L$set$26,LFE6-LFB6
	.quad L$set$26
	.byte	0x4
	.set L$set$27,LCFI11-LFB6
	.long L$set$27
	.byte	0xe
	.uleb128 0x50
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$28,LCFI12-LCFI11
	.long L$set$28
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$29,LCFI13-LCFI12
	.long L$set$29
	.byte	0x93
	.uleb128 0x8
	.byte	0x4
	.set L$set$30,LCFI14-LCFI13
	.long L$set$30
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE8:
LSFDE10:
	.set L$set$31,LEFDE10-LASFDE10
	.long L$set$31
LASFDE10:
	.set L$set$32,Lframe0-Lsection__debug_frame
	.long L$set$32
	.quad	LFB7
	.set L$set$33,LFE7-LFB7
	.quad L$set$33
	.byte	0x4
	.set L$set$34,LCFI15-LFB7
	.long L$set$34
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$35,LCFI16-LCFI15
	.long L$set$35
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$36,LCFI17-LCFI16
	.long L$set$36
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
	.set L$set$37,LCFI18-LCFI17
	.long L$set$37
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
LEFDE10:
LSFDE12:
	.set L$set$38,LEFDE12-LASFDE12
	.long L$set$38
LASFDE12:
	.set L$set$39,Lframe0-Lsection__debug_frame
	.long L$set$39
	.quad	LFB8
	.set L$set$40,LFE8-LFB8
	.quad L$set$40
	.byte	0x4
	.set L$set$41,LCFI19-LFB8
	.long L$set$41
	.byte	0xe
	.uleb128 0xa0
	.byte	0x4
	.set L$set$42,LCFI20-LCFI19
	.long L$set$42
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$43,LCFI21-LCFI20
	.long L$set$43
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x40
	.byte	0x4
	.set L$set$44,LCFI22-LCFI21
	.long L$set$44
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
	.set L$set$45,LCFI23-LCFI22
	.long L$set$45
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
LEFDE12:
LSFDE14:
	.set L$set$46,LEFDE14-LASFDE14
	.long L$set$46
LASFDE14:
	.set L$set$47,Lframe0-Lsection__debug_frame
	.long L$set$47
	.quad	LFB9
	.set L$set$48,LFE9-LFB9
	.quad L$set$48
	.byte	0x4
	.set L$set$49,LCFI24-LFB9
	.long L$set$49
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$50,LCFI25-LCFI24
	.long L$set$50
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE14:
LSFDE16:
	.set L$set$51,LEFDE16-LASFDE16
	.long L$set$51
LASFDE16:
	.set L$set$52,Lframe0-Lsection__debug_frame
	.long L$set$52
	.quad	LFB10
	.set L$set$53,LFE10-LFB10
	.quad L$set$53
	.byte	0x4
	.set L$set$54,LCFI26-LFB10
	.long L$set$54
	.byte	0xe
	.uleb128 0x190
	.byte	0x4
	.set L$set$55,LCFI27-LCFI26
	.long L$set$55
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$56,LCFI28-LCFI27
	.long L$set$56
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x40
	.byte	0x4
	.set L$set$57,LCFI29-LCFI28
	.long L$set$57
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
	.byte	0x98
	.uleb128 0x1
	.byte	0x4
	.set L$set$58,LCFI30-LCFI29
	.long L$set$58
	.byte	0xd7
	.byte	0xd8
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
LEFDE16:
LSFDE18:
	.set L$set$59,LEFDE18-LASFDE18
	.long L$set$59
LASFDE18:
	.set L$set$60,Lframe0-Lsection__debug_frame
	.long L$set$60
	.quad	LFB11
	.set L$set$61,LFE11-LFB11
	.quad L$set$61
	.byte	0x4
	.set L$set$62,LCFI31-LFB11
	.long L$set$62
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$63,LCFI32-LCFI31
	.long L$set$63
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE18:
LSFDE20:
	.set L$set$64,LEFDE20-LASFDE20
	.long L$set$64
LASFDE20:
	.set L$set$65,Lframe0-Lsection__debug_frame
	.long L$set$65
	.quad	LFB12
	.set L$set$66,LFE12-LFB12
	.quad L$set$66
	.byte	0x4
	.set L$set$67,LCFI33-LFB12
	.long L$set$67
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.set L$set$68,LCFI34-LCFI33
	.long L$set$68
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE20:
LSFDE22:
	.set L$set$69,LEFDE22-LASFDE22
	.long L$set$69
LASFDE22:
	.set L$set$70,Lframe0-Lsection__debug_frame
	.long L$set$70
	.quad	LFB13
	.set L$set$71,LFE13-LFB13
	.quad L$set$71
	.byte	0x4
	.set L$set$72,LCFI35-LFB13
	.long L$set$72
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$73,LCFI36-LCFI35
	.long L$set$73
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE22:
LSFDE24:
	.set L$set$74,LEFDE24-LASFDE24
	.long L$set$74
LASFDE24:
	.set L$set$75,Lframe0-Lsection__debug_frame
	.long L$set$75
	.quad	LFB14
	.set L$set$76,LFE14-LFB14
	.quad L$set$76
	.byte	0x4
	.set L$set$77,LCFI37-LFB14
	.long L$set$77
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$78,LCFI38-LCFI37
	.long L$set$78
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE24:
LSFDE26:
	.set L$set$79,LEFDE26-LASFDE26
	.long L$set$79
LASFDE26:
	.set L$set$80,Lframe0-Lsection__debug_frame
	.long L$set$80
	.quad	LFB18
	.set L$set$81,LFE18-LFB18
	.quad L$set$81
	.byte	0x4
	.set L$set$82,LCFI39-LFB18
	.long L$set$82
	.byte	0xe
	.uleb128 0x20
	.byte	0x93
	.uleb128 0x4
	.byte	0x4
	.set L$set$83,LCFI40-LCFI39
	.long L$set$83
	.byte	0xd3
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE26:
	.section __TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
EH_frame1:
	.set L$set$84,LECIE1-LSCIE1
	.long L$set$84
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
LSFDE29:
	.set L$set$85,LEFDE29-LASFDE29
	.long L$set$85
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB2-.
	.set L$set$86,LFE2-LFB2
	.quad L$set$86
	.uleb128 0
	.byte	0x4
	.set L$set$87,LCFI0-LFB2
	.long L$set$87
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$88,LCFI1-LCFI0
	.long L$set$88
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE29:
LSFDE31:
	.set L$set$89,LEFDE31-LASFDE31
	.long L$set$89
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB3-.
	.set L$set$90,LFE3-LFB3
	.quad L$set$90
	.uleb128 0
	.byte	0x4
	.set L$set$91,LCFI2-LFB3
	.long L$set$91
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$92,LCFI3-LCFI2
	.long L$set$92
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$93,LCFI4-LCFI3
	.long L$set$93
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x95
	.uleb128 0x4
	.byte	0x4
	.set L$set$94,LCFI5-LCFI4
	.long L$set$94
	.byte	0xde
	.byte	0xdd
	.byte	0xd5
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE31:
LSFDE33:
	.set L$set$95,LEFDE33-LASFDE33
	.long L$set$95
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB4-.
	.set L$set$96,LFE4-LFB4
	.quad L$set$96
	.uleb128 0
	.byte	0x4
	.set L$set$97,LCFI6-LFB4
	.long L$set$97
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.set L$set$98,LCFI7-LCFI6
	.long L$set$98
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE33:
LSFDE35:
	.set L$set$99,LEFDE35-LASFDE35
	.long L$set$99
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB5-.
	.set L$set$100,LFE5-LFB5
	.quad L$set$100
	.uleb128 0
	.byte	0x4
	.set L$set$101,LCFI8-LFB5
	.long L$set$101
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$102,LCFI9-LCFI8
	.long L$set$102
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$103,LCFI10-LCFI9
	.long L$set$103
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE35:
LSFDE37:
	.set L$set$104,LEFDE37-LASFDE37
	.long L$set$104
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB6-.
	.set L$set$105,LFE6-LFB6
	.quad L$set$105
	.uleb128 0
	.byte	0x4
	.set L$set$106,LCFI11-LFB6
	.long L$set$106
	.byte	0xe
	.uleb128 0x50
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$107,LCFI12-LCFI11
	.long L$set$107
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$108,LCFI13-LCFI12
	.long L$set$108
	.byte	0x93
	.uleb128 0x8
	.byte	0x4
	.set L$set$109,LCFI14-LCFI13
	.long L$set$109
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE37:
LSFDE39:
	.set L$set$110,LEFDE39-LASFDE39
	.long L$set$110
LASFDE39:
	.long	LASFDE39-EH_frame1
	.quad	LFB7-.
	.set L$set$111,LFE7-LFB7
	.quad L$set$111
	.uleb128 0
	.byte	0x4
	.set L$set$112,LCFI15-LFB7
	.long L$set$112
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$113,LCFI16-LCFI15
	.long L$set$113
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$114,LCFI17-LCFI16
	.long L$set$114
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
	.set L$set$115,LCFI18-LCFI17
	.long L$set$115
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
LEFDE39:
LSFDE41:
	.set L$set$116,LEFDE41-LASFDE41
	.long L$set$116
LASFDE41:
	.long	LASFDE41-EH_frame1
	.quad	LFB8-.
	.set L$set$117,LFE8-LFB8
	.quad L$set$117
	.uleb128 0
	.byte	0x4
	.set L$set$118,LCFI19-LFB8
	.long L$set$118
	.byte	0xe
	.uleb128 0xa0
	.byte	0x4
	.set L$set$119,LCFI20-LCFI19
	.long L$set$119
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$120,LCFI21-LCFI20
	.long L$set$120
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x40
	.byte	0x4
	.set L$set$121,LCFI22-LCFI21
	.long L$set$121
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
	.set L$set$122,LCFI23-LCFI22
	.long L$set$122
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
LEFDE41:
LSFDE43:
	.set L$set$123,LEFDE43-LASFDE43
	.long L$set$123
LASFDE43:
	.long	LASFDE43-EH_frame1
	.quad	LFB9-.
	.set L$set$124,LFE9-LFB9
	.quad L$set$124
	.uleb128 0
	.byte	0x4
	.set L$set$125,LCFI24-LFB9
	.long L$set$125
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$126,LCFI25-LCFI24
	.long L$set$126
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE43:
LSFDE45:
	.set L$set$127,LEFDE45-LASFDE45
	.long L$set$127
LASFDE45:
	.long	LASFDE45-EH_frame1
	.quad	LFB10-.
	.set L$set$128,LFE10-LFB10
	.quad L$set$128
	.uleb128 0
	.byte	0x4
	.set L$set$129,LCFI26-LFB10
	.long L$set$129
	.byte	0xe
	.uleb128 0x190
	.byte	0x4
	.set L$set$130,LCFI27-LCFI26
	.long L$set$130
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$131,LCFI28-LCFI27
	.long L$set$131
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x40
	.byte	0x4
	.set L$set$132,LCFI29-LCFI28
	.long L$set$132
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
	.byte	0x98
	.uleb128 0x1
	.byte	0x4
	.set L$set$133,LCFI30-LCFI29
	.long L$set$133
	.byte	0xd7
	.byte	0xd8
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
LEFDE45:
LSFDE47:
	.set L$set$134,LEFDE47-LASFDE47
	.long L$set$134
LASFDE47:
	.long	LASFDE47-EH_frame1
	.quad	LFB11-.
	.set L$set$135,LFE11-LFB11
	.quad L$set$135
	.uleb128 0
	.byte	0x4
	.set L$set$136,LCFI31-LFB11
	.long L$set$136
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$137,LCFI32-LCFI31
	.long L$set$137
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE47:
LSFDE49:
	.set L$set$138,LEFDE49-LASFDE49
	.long L$set$138
LASFDE49:
	.long	LASFDE49-EH_frame1
	.quad	LFB12-.
	.set L$set$139,LFE12-LFB12
	.quad L$set$139
	.uleb128 0
	.byte	0x4
	.set L$set$140,LCFI33-LFB12
	.long L$set$140
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.set L$set$141,LCFI34-LCFI33
	.long L$set$141
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE49:
LSFDE51:
	.set L$set$142,LEFDE51-LASFDE51
	.long L$set$142
LASFDE51:
	.long	LASFDE51-EH_frame1
	.quad	LFB13-.
	.set L$set$143,LFE13-LFB13
	.quad L$set$143
	.uleb128 0
	.byte	0x4
	.set L$set$144,LCFI35-LFB13
	.long L$set$144
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$145,LCFI36-LCFI35
	.long L$set$145
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE51:
LSFDE53:
	.set L$set$146,LEFDE53-LASFDE53
	.long L$set$146
LASFDE53:
	.long	LASFDE53-EH_frame1
	.quad	LFB14-.
	.set L$set$147,LFE14-LFB14
	.quad L$set$147
	.uleb128 0
	.byte	0x4
	.set L$set$148,LCFI37-LFB14
	.long L$set$148
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$149,LCFI38-LCFI37
	.long L$set$149
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE53:
LSFDE55:
	.set L$set$150,LEFDE55-LASFDE55
	.long L$set$150
LASFDE55:
	.long	LASFDE55-EH_frame1
	.quad	LFB18-.
	.set L$set$151,LFE18-LFB18
	.quad L$set$151
	.uleb128 0
	.byte	0x4
	.set L$set$152,LCFI39-LFB18
	.long L$set$152
	.byte	0xe
	.uleb128 0x20
	.byte	0x93
	.uleb128 0x4
	.byte	0x4
	.set L$set$153,LCFI40-LCFI39
	.long L$set$153
	.byte	0xd3
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE55:
	.text
Letext0:
	.file 3 "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_types.ads"
	.file 4 "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_types-sss.ads"
	.section __DWARF,__debug_info,regular,debug
Lsection__debug_info:
Ldebug_info0:
	.long	0x118c
	.short	0x4
	.set L$set$154,Ldebug_abbrev0-Lsection__debug_abbrev
	.long L$set$154
	.byte	0x8
	.uleb128 0x1
	.ascii "GNU Ada 14.1.0 -O0 -gnatA -g -gnat2012 -gnatwa -gnatwe -fstack-check=specific -fstack-protector-strong -gnatR2js -gnatws -gnatec=/private/var/folders/bg/pt9l6y1j47q642kp3z5blrmh0000gn/T/GPR.77809/GNAT-TEMP-000003.TMP -gnatem=/private/var/folders/bg/pt9l6y1j47q642kp3z5blrmh0000gn/T/GPR.77809/GNAT-TEMP-000004.TMP -mmacosx-version-min=16.0.0 -mcpu=apple-m1 -mlittle-endian -mabi=lp64 -fPIC\0"
	.byte	0xd
	.ascii "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_types-sss.adb\0"
	.ascii "/Users/sicarii/Desktop/anubis-spark/obj/gnatprove/data_representation\0"
	.quad	Ltext0
	.set L$set$155,Letext0-Ltext0
	.quad L$set$155
	.set L$set$156,Ldebug_line0-Lsection__debug_line
	.long L$set$156
	.uleb128 0x2
	.sleb128 255
	.ascii "anubis_types__sss__share_index\0"
	.long	0x25b
	.uleb128 0x3
	.long	0x230
	.uleb128 0x4
	.byte	0x2
	.byte	0x5
	.ascii "anubis_types__sss__Tshare_indexB\0"
	.uleb128 0x5
	.byte	0x1
	.byte	0x7
	.ascii "interfaces__unsigned_8\0"
	.uleb128 0x6
	.byte	0
	.byte	0xff
	.ascii "anubis_types__byte\0"
	.long	0x2b8
	.uleb128 0x3
	.long	0x299
	.uleb128 0x4
	.byte	0x1
	.byte	0x7
	.ascii "anubis_types__TbyteB\0"
	.uleb128 0x7
	.ascii "anubis_types__byte_array\0"
	.byte	0x10
	.byte	0x3
	.byte	0x12
	.byte	0x9
	.long	0x317
	.uleb128 0x8
	.ascii "P_ARRAY\0"
	.byte	0x2
	.byte	0x46
	.byte	0x1d
	.long	0x303
	.byte	0
	.uleb128 0x9
	.byte	0x8
	.long	0x32b
	.uleb128 0xa
	.set L$set$157,LASF0-Lsection__debug_str
	.long L$set$157
	.byte	0x2
	.byte	0x46
	.byte	0x1d
	.long	0x3ca
	.byte	0x8
	.byte	0
	.uleb128 0xb
	.long	0x2d0
	.uleb128 0xb
	.long	0x2d0
	.uleb128 0xb
	.long	0x2d0
	.uleb128 0xb
	.long	0x2d0
	.uleb128 0xc
	.ascii "anubis_types__byte_array___XUA\0"
	.long	0x299
	.long	0x369
	.uleb128 0xd
	.long	0x369
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
	.uleb128 0x5
	.byte	0x4
	.byte	0x5
	.ascii "integer\0"
	.uleb128 0x7
	.ascii "anubis_types__byte_array___XUB\0"
	.byte	0x8
	.byte	0x2
	.byte	0x46
	.byte	0x1d
	.long	0x3ca
	.uleb128 0x8
	.ascii "LB0\0"
	.byte	0x3
	.byte	0x12
	.byte	0x9
	.long	0x3a9
	.byte	0
	.uleb128 0xe
	.sleb128 0
	.sleb128 2147483647
	.ascii "natural\0"
	.long	0x369
	.uleb128 0x8
	.ascii "UB0\0"
	.byte	0x3
	.byte	0x12
	.byte	0x9
	.long	0x3a9
	.byte	0x4
	.byte	0
	.uleb128 0x9
	.byte	0x8
	.long	0x374
	.uleb128 0xc
	.ascii "anubis_types__sss__secret_share__T7s\0"
	.long	0x299
	.long	0x406
	.uleb128 0xf
	.long	0x369
	.sleb128 256
	.byte	0
	.uleb128 0x10
	.ascii "anubis_types__sss__secret_share\0"
	.short	0x10c
	.byte	0x4
	.byte	0x8c
	.byte	0x9
	.long	0x468
	.uleb128 0x8
	.ascii "x\0"
	.byte	0x4
	.byte	0x8d
	.byte	0x7
	.long	0x230
	.byte	0
	.uleb128 0x8
	.ascii "y\0"
	.byte	0x4
	.byte	0x8e
	.byte	0x7
	.long	0x3d0
	.byte	0x2
	.uleb128 0x11
	.ascii "length\0"
	.byte	0x4
	.byte	0x8f
	.byte	0x7
	.long	0x3a9
	.short	0x104
	.uleb128 0x11
	.ascii "valid\0"
	.byte	0x4
	.byte	0x90
	.byte	0x7
	.long	0x468
	.short	0x108
	.byte	0
	.uleb128 0x5
	.byte	0x1
	.byte	0x2
	.ascii "boolean\0"
	.uleb128 0x7
	.ascii "anubis_types__sss__share_array\0"
	.byte	0x10
	.byte	0x4
	.byte	0x2a
	.byte	0x9
	.long	0x4c0
	.uleb128 0x8
	.ascii "P_ARRAY\0"
	.byte	0x4
	.byte	0x2a
	.byte	0x9
	.long	0x4ac
	.byte	0
	.uleb128 0x9
	.byte	0x8
	.long	0x4ca
	.uleb128 0xa
	.set L$set$158,LASF0-Lsection__debug_str
	.long L$set$158
	.byte	0x4
	.byte	0x2a
	.byte	0x9
	.long	0x557
	.byte	0x8
	.byte	0
	.uleb128 0xb
	.long	0x473
	.uleb128 0xb
	.long	0x473
	.uleb128 0xc
	.ascii "anubis_types__sss__share_array___XUA\0"
	.long	0x406
	.long	0x50e
	.uleb128 0xd
	.long	0x25b
	.uleb128 0x6
	.byte	0x97
	.byte	0x23
	.uleb128 0x8
	.byte	0x6
	.byte	0x94
	.byte	0x2
	.uleb128 0x8
	.byte	0x97
	.byte	0x23
	.uleb128 0x8
	.byte	0x6
	.byte	0x23
	.uleb128 0x2
	.byte	0x94
	.byte	0x2
	.byte	0
	.uleb128 0x7
	.ascii "anubis_types__sss__share_array___XUB\0"
	.byte	0x4
	.byte	0x4
	.byte	0x2a
	.byte	0x9
	.long	0x557
	.uleb128 0x8
	.ascii "LB0\0"
	.byte	0x4
	.byte	0x2a
	.byte	0x9
	.long	0x230
	.byte	0
	.uleb128 0x8
	.ascii "UB0\0"
	.byte	0x4
	.byte	0x2a
	.byte	0x9
	.long	0x230
	.byte	0x2
	.byte	0
	.uleb128 0x9
	.byte	0x8
	.long	0x50e
	.uleb128 0x5
	.byte	0x4
	.byte	0x7
	.ascii "interfaces__c__unsigned\0"
	.uleb128 0x12
	.ascii "anubis_types__sss__max_shares\0"
	.byte	0x4
	.byte	0x1d
	.byte	0x4
	.long	0x5b9
	.byte	0xff
	.uleb128 0x4
	.byte	0x10
	.byte	0x5
	.ascii "universal_integer\0"
	.uleb128 0xb
	.long	0x59f
	.uleb128 0xb
	.long	0x59f
	.uleb128 0x12
	.ascii "anubis_types__sss__min_threshold\0"
	.byte	0x4
	.byte	0x1e
	.byte	0x4
	.long	0x5b4
	.byte	0x2
	.uleb128 0x13
	.uleb128 0x14
	.byte	0x14
	.byte	0x14
	.byte	0x2c
	.byte	0x28
	.short	0x4
	.byte	0x30
	.byte	0x2f
	.short	0x6
	.byte	0x12
	.byte	0x15
	.byte	0x2
	.byte	0x1c
	.byte	0x23
	.uleb128 0x1
	.byte	0x16
	.byte	0x13
	.byte	0x16
	.byte	0x13
	.uleb128 0x14
	.ascii "anubis_types__sss__gf_sub\0"
	.byte	0x1
	.byte	0x17
	.byte	0x4
	.long	0x299
	.quad	LFB14
	.set L$set$159,LFE14-LFB14
	.quad L$set$159
	.uleb128 0x1
	.byte	0x9c
	.long	0x651
	.uleb128 0x15
	.ascii "a\0"
	.byte	0x1
	.byte	0x17
	.byte	0x15
	.long	0x2b3
	.uleb128 0x2
	.byte	0x91
	.sleb128 -1
	.uleb128 0x15
	.ascii "b\0"
	.byte	0x1
	.byte	0x17
	.byte	0x18
	.long	0x2b3
	.uleb128 0x2
	.byte	0x91
	.sleb128 -2
	.byte	0
	.uleb128 0x14
	.ascii "anubis_types__sss__gf_add\0"
	.byte	0x1
	.byte	0x12
	.byte	0x4
	.long	0x299
	.quad	LFB13
	.set L$set$160,LFE13-LFB13
	.quad L$set$160
	.uleb128 0x1
	.byte	0x9c
	.long	0x6a4
	.uleb128 0x15
	.ascii "a\0"
	.byte	0x1
	.byte	0x12
	.byte	0x15
	.long	0x2b3
	.uleb128 0x2
	.byte	0x91
	.sleb128 -1
	.uleb128 0x15
	.ascii "b\0"
	.byte	0x1
	.byte	0x12
	.byte	0x18
	.long	0x2b3
	.uleb128 0x2
	.byte	0x91
	.sleb128 -2
	.byte	0
	.uleb128 0x16
	.ascii "anubis_types__sss__zeroize_share\0"
	.byte	0x1
	.short	0x156
	.byte	0x4
	.quad	LFB12
	.set L$set$161,LFE12-LFB12
	.quad L$set$161
	.uleb128 0x1
	.byte	0x9c
	.long	0x710
	.uleb128 0x17
	.set L$set$162,LASF1-Lsection__debug_str
	.long L$set$162
	.byte	0x4
	.byte	0x84
	.byte	0x1d
	.long	0x710
	.uleb128 0x2
	.byte	0x91
	.sleb128 -24
	.uleb128 0x18
	.quad	LBB25
	.set L$set$163,LBE25-LBB25
	.quad L$set$163
	.uleb128 0x19
	.ascii "i\0"
	.byte	0x1
	.short	0x158
	.byte	0xb
	.long	0x369
	.uleb128 0x2
	.byte	0x91
	.sleb128 -4
	.byte	0
	.byte	0
	.uleb128 0x1a
	.byte	0x8
	.long	0x406
	.uleb128 0x1b
	.ascii "anubis_types__sss__get_share_length\0"
	.byte	0x1
	.short	0x153
	.byte	0x4
	.long	0x3a9
	.quad	LFB11
	.set L$set$164,LFE11-LFB11
	.quad L$set$164
	.uleb128 0x1
	.byte	0x9c
	.long	0x769
	.uleb128 0x17
	.set L$set$165,LASF1-Lsection__debug_str
	.long L$set$165
	.byte	0x4
	.byte	0x81
	.byte	0x1f
	.long	0x710
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.byte	0
	.uleb128 0x1b
	.ascii "anubis_types__sss__make_share\0"
	.byte	0x1
	.short	0x141
	.byte	0x4
	.long	0x406
	.quad	LFB10
	.set L$set$166,LFE10-LFB10
	.quad L$set$166
	.uleb128 0x1
	.byte	0x9c
	.long	0x87c
	.uleb128 0x15
	.ascii "index\0"
	.byte	0x4
	.byte	0x7b
	.byte	0x7
	.long	0x256
	.uleb128 0x3
	.byte	0x91
	.sleb128 -370
	.uleb128 0x15
	.ascii "data\0"
	.byte	0x4
	.byte	0x7c
	.byte	0x7
	.long	0x326
	.uleb128 0x3
	.byte	0x91
	.sleb128 -392
	.uleb128 0x18
	.quad	LBB23
	.set L$set$167,LBE23-LBB23
	.quad L$set$167
	.uleb128 0x1c
	.set L$set$168,LASF1-Lsection__debug_str
	.long L$set$168
	.byte	0x1
	.short	0x145
	.byte	0x7
	.long	0x406
	.uleb128 0x3
	.byte	0x91
	.sleb128 -344
	.uleb128 0x1d
	.ascii "anubis_types__sss__make_share__L_19__T64b___L\0"
	.long	0x369
	.uleb128 0x3
	.byte	0x91
	.sleb128 -352
	.uleb128 0x1d
	.ascii "anubis_types__sss__make_share__L_19__T64b___U\0"
	.long	0x369
	.uleb128 0x3
	.byte	0x91
	.sleb128 -348
	.uleb128 0x18
	.quad	LBB24
	.set L$set$169,LBE24-LBB24
	.quad L$set$169
	.uleb128 0x19
	.ascii "i\0"
	.byte	0x1
	.short	0x14a
	.byte	0xb
	.long	0x369
	.uleb128 0x3
	.byte	0x91
	.sleb128 -356
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0x1b
	.ascii "anubis_types__sss__get_share_index\0"
	.byte	0x1
	.short	0x13e
	.byte	0x4
	.long	0x230
	.quad	LFB9
	.set L$set$170,LFE9-LFB9
	.quad L$set$170
	.uleb128 0x1
	.byte	0x9c
	.long	0x8ce
	.uleb128 0x17
	.set L$set$171,LASF1-Lsection__debug_str
	.long L$set$171
	.byte	0x4
	.byte	0x77
	.byte	0x1e
	.long	0x710
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.byte	0
	.uleb128 0x1e
	.ascii "anubis_types__sss__combine_shares\0"
	.byte	0x1
	.byte	0xec
	.byte	0x4
	.long	0x468
	.quad	LFB8
	.set L$set$172,LFE8-LFB8
	.quad L$set$172
	.uleb128 0x1
	.byte	0x9c
	.long	0xbd6
	.uleb128 0x15
	.ascii "shares\0"
	.byte	0x4
	.byte	0x65
	.byte	0x7
	.long	0x4c5
	.uleb128 0x3
	.byte	0x91
	.sleb128 -128
	.uleb128 0x17
	.set L$set$173,LASF2-Lsection__debug_str
	.long L$set$173
	.byte	0x4
	.byte	0x66
	.byte	0x7
	.long	0xbd6
	.uleb128 0x3
	.byte	0x91
	.sleb128 -132
	.uleb128 0x1f
	.sleb128 2147483647
	.ascii "positive\0"
	.long	0x369
	.uleb128 0x15
	.ascii "reconstructed\0"
	.byte	0x4
	.byte	0x67
	.byte	0x7
	.long	0x321
	.uleb128 0x3
	.byte	0x91
	.sleb128 -152
	.uleb128 0x15
	.ascii "success\0"
	.byte	0x4
	.byte	0x68
	.byte	0x7
	.long	0x468
	.uleb128 0x3
	.byte	0x91
	.sleb128 -103
	.uleb128 0x18
	.quad	LBB16
	.set L$set$174,LBE16-LBB16
	.quad L$set$174
	.uleb128 0x20
	.ascii "temp\0"
	.byte	0x1
	.byte	0xf2
	.byte	0x7
	.long	0x299
	.uleb128 0x3
	.byte	0x91
	.sleb128 -102
	.uleb128 0x20
	.ascii "failed\0"
	.byte	0x1
	.byte	0xf3
	.byte	0x7
	.long	0x468
	.uleb128 0x3
	.byte	0x91
	.sleb128 -101
	.uleb128 0x1d
	.ascii "anubis_types__sss__combine_shares__byte_loop__T44b___L\0"
	.long	0x369
	.uleb128 0x3
	.byte	0x91
	.sleb128 -80
	.uleb128 0x1d
	.ascii "anubis_types__sss__combine_shares__byte_loop__T44b___U\0"
	.long	0x369
	.uleb128 0x3
	.byte	0x91
	.sleb128 -76
	.uleb128 0x18
	.quad	LBB17
	.set L$set$175,LBE17-LBB17
	.quad L$set$175
	.uleb128 0x21
	.set L$set$176,LASF3-Lsection__debug_str
	.long L$set$176
	.byte	0x1
	.byte	0xfd
	.byte	0xb
	.long	0x369
	.uleb128 0x3
	.byte	0x91
	.sleb128 -84
	.uleb128 0x18
	.quad	LBB18
	.set L$set$177,LBE18-LBB18
	.quad L$set$177
	.uleb128 0x19
	.ascii "last_index\0"
	.byte	0x1
	.short	0x105
	.byte	0xd
	.long	0x230
	.uleb128 0x3
	.byte	0x91
	.sleb128 -90
	.uleb128 0x1d
	.ascii "anubis_types__sss__combine_shares__B_14__L_15__T55b___L\0"
	.long	0x25b
	.uleb128 0x3
	.byte	0x91
	.sleb128 -88
	.uleb128 0x18
	.quad	LBB19
	.set L$set$178,LBE19-LBB19
	.quad L$set$178
	.uleb128 0x19
	.ascii "i\0"
	.byte	0x1
	.short	0x108
	.byte	0x11
	.long	0x25b
	.uleb128 0x3
	.byte	0x91
	.sleb128 -94
	.uleb128 0x18
	.quad	LBB20
	.set L$set$179,LBE20-LBB20
	.quad L$set$179
	.uleb128 0x19
	.ascii "xi\0"
	.byte	0x1
	.short	0x112
	.byte	0x13
	.long	0x299
	.uleb128 0x3
	.byte	0x91
	.sleb128 -99
	.uleb128 0x19
	.ascii "yi\0"
	.byte	0x1
	.short	0x113
	.byte	0x13
	.long	0x299
	.uleb128 0x3
	.byte	0x91
	.sleb128 -98
	.uleb128 0x19
	.ascii "basis\0"
	.byte	0x1
	.short	0x114
	.byte	0x13
	.long	0x299
	.uleb128 0x3
	.byte	0x91
	.sleb128 -100
	.uleb128 0x1d
	.ascii "anubis_types__sss__combine_shares__B_14__B_16__L_17__T56b___L\0"
	.long	0x25b
	.uleb128 0x3
	.byte	0x91
	.sleb128 -86
	.uleb128 0x18
	.quad	LBB21
	.set L$set$180,LBE21-LBB21
	.quad L$set$180
	.uleb128 0x19
	.ascii "j\0"
	.byte	0x1
	.short	0x116
	.byte	0x17
	.long	0x25b
	.uleb128 0x3
	.byte	0x91
	.sleb128 -92
	.uleb128 0x18
	.quad	LBB22
	.set L$set$181,LBE22-LBB22
	.quad L$set$181
	.uleb128 0x19
	.ascii "xj\0"
	.byte	0x1
	.short	0x11c
	.byte	0x1c
	.long	0x299
	.uleb128 0x3
	.byte	0x91
	.sleb128 -97
	.uleb128 0x19
	.ascii "numerator\0"
	.byte	0x1
	.short	0x11d
	.byte	0x1c
	.long	0x299
	.uleb128 0x3
	.byte	0x91
	.sleb128 -96
	.uleb128 0x19
	.ascii "denominator\0"
	.byte	0x1
	.short	0x11e
	.byte	0x1c
	.long	0x299
	.uleb128 0x3
	.byte	0x91
	.sleb128 -95
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0xb
	.long	0x931
	.uleb128 0x1e
	.ascii "anubis_types__sss__split_secret\0"
	.byte	0x1
	.byte	0xb7
	.byte	0x4
	.long	0x468
	.quad	LFB7
	.set L$set$182,LFE7-LFB7
	.quad L$set$182
	.uleb128 0x1
	.byte	0x9c
	.long	0xf0e
	.uleb128 0x15
	.ascii "secret\0"
	.byte	0x4
	.byte	0x56
	.byte	0x7
	.long	0x31c
	.uleb128 0x3
	.byte	0x91
	.sleb128 -192
	.uleb128 0x17
	.set L$set$183,LASF2-Lsection__debug_str
	.long L$set$183
	.byte	0x4
	.byte	0x57
	.byte	0x7
	.long	0xf0e
	.uleb128 0x3
	.byte	0x91
	.sleb128 -196
	.uleb128 0x15
	.ascii "num_shares\0"
	.byte	0x4
	.byte	0x58
	.byte	0x7
	.long	0xf13
	.uleb128 0x3
	.byte	0x91
	.sleb128 -200
	.uleb128 0x15
	.ascii "shares\0"
	.byte	0x4
	.byte	0x59
	.byte	0x7
	.long	0x4c0
	.uleb128 0x3
	.byte	0x91
	.sleb128 -216
	.uleb128 0x15
	.ascii "success\0"
	.byte	0x4
	.byte	0x5a
	.byte	0x7
	.long	0x468
	.uleb128 0x3
	.byte	0x91
	.sleb128 -165
	.uleb128 0x18
	.quad	LBB8
	.set L$set$184,LBE8-LBB8
	.quad L$set$184
	.uleb128 0x1d
	.ascii "anubis_types__sss__split_secret__TTcoeffsSP1___U\0"
	.long	0x369
	.uleb128 0x3
	.byte	0x91
	.sleb128 -144
	.uleb128 0xc
	.ascii "anubis_types__sss__split_secret__TcoeffsS\0"
	.long	0x299
	.long	0xd03
	.uleb128 0x22
	.long	0x369
	.sleb128 0
	.long	0xc8b
	.byte	0
	.uleb128 0x20
	.ascii "coeffs\0"
	.byte	0x1
	.byte	0xc3
	.byte	0x7
	.long	0xcc5
	.uleb128 0x4
	.byte	0x91
	.sleb128 -112
	.byte	0x6
	.uleb128 0x1d
	.ascii "anubis_types__sss__split_secret__L_9__T33b___L\0"
	.long	0x369
	.uleb128 0x3
	.byte	0x91
	.sleb128 -140
	.uleb128 0x1d
	.ascii "anubis_types__sss__split_secret__L_9__T33b___U\0"
	.long	0x369
	.uleb128 0x3
	.byte	0x91
	.sleb128 -136
	.uleb128 0x23
	.quad	LBB9
	.set L$set$185,LBE9-LBB9
	.quad L$set$185
	.long	0xeec
	.uleb128 0x21
	.set L$set$186,LASF3-Lsection__debug_str
	.long L$set$186
	.byte	0x1
	.byte	0xc6
	.byte	0xb
	.long	0x369
	.uleb128 0x3
	.byte	0x91
	.sleb128 -156
	.uleb128 0x18
	.quad	LBB10
	.set L$set$187,LBE10-LBB10
	.quad L$set$187
	.uleb128 0x1d
	.ascii "anubis_types__sss__split_secret__L_10__T34b___U\0"
	.long	0x369
	.uleb128 0x3
	.byte	0x91
	.sleb128 -132
	.uleb128 0x1d
	.ascii "anubis_types__sss__split_secret__L_11__T36b___L\0"
	.long	0x25b
	.uleb128 0x3
	.byte	0x91
	.sleb128 -160
	.uleb128 0x1d
	.ascii "anubis_types__sss__split_secret__L_11__T36b___U\0"
	.long	0x25b
	.uleb128 0x3
	.byte	0x91
	.sleb128 -158
	.uleb128 0x23
	.quad	LBB11
	.set L$set$188,LBE11-LBB11
	.quad L$set$188
	.long	0xe8c
	.uleb128 0x20
	.ascii "i\0"
	.byte	0x1
	.byte	0xcc
	.byte	0xe
	.long	0x369
	.uleb128 0x3
	.byte	0x91
	.sleb128 -152
	.byte	0
	.uleb128 0x18
	.quad	LBB13
	.set L$set$189,LBE13-LBB13
	.quad L$set$189
	.uleb128 0x20
	.ascii "share_idx\0"
	.byte	0x1
	.byte	0xd2
	.byte	0xe
	.long	0x25b
	.uleb128 0x3
	.byte	0x91
	.sleb128 -162
	.uleb128 0x18
	.quad	LBB14
	.set L$set$190,LBE14-LBB14
	.quad L$set$190
	.uleb128 0x20
	.ascii "x_val\0"
	.byte	0x1
	.byte	0xd5
	.byte	0x10
	.long	0x299
	.uleb128 0x3
	.byte	0x91
	.sleb128 -164
	.uleb128 0x20
	.ascii "y_val\0"
	.byte	0x1
	.byte	0xd6
	.byte	0x10
	.long	0x299
	.uleb128 0x3
	.byte	0x91
	.sleb128 -163
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0x18
	.quad	LBB15
	.set L$set$191,LBE15-LBB15
	.quad L$set$191
	.uleb128 0x20
	.ascii "i\0"
	.byte	0x1
	.byte	0xe1
	.byte	0xb
	.long	0x369
	.uleb128 0x3
	.byte	0x91
	.sleb128 -148
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0xb
	.long	0x931
	.uleb128 0xb
	.long	0x931
	.uleb128 0x14
	.ascii "anubis_types__sss__eval_polynomial\0"
	.byte	0x1
	.byte	0x55
	.byte	0x4
	.long	0x299
	.quad	LFB6
	.set L$set$192,LFE6-LFB6
	.quad L$set$192
	.uleb128 0x1
	.byte	0x9c
	.long	0x1047
	.uleb128 0x15
	.ascii "coefficients\0"
	.byte	0x1
	.byte	0x56
	.byte	0x7
	.long	0x317
	.uleb128 0x2
	.byte	0x91
	.sleb128 -32
	.uleb128 0x15
	.ascii "x\0"
	.byte	0x1
	.byte	0x57
	.byte	0x7
	.long	0x2b3
	.uleb128 0x2
	.byte	0x91
	.sleb128 -33
	.uleb128 0x18
	.quad	LBB6
	.set L$set$193,LBE6-LBB6
	.quad L$set$193
	.uleb128 0x20
	.ascii "result\0"
	.byte	0x1
	.byte	0x59
	.byte	0x7
	.long	0x299
	.uleb128 0x2
	.byte	0x91
	.sleb128 -14
	.uleb128 0x20
	.ascii "x_power\0"
	.byte	0x1
	.byte	0x5a
	.byte	0x7
	.long	0x299
	.uleb128 0x2
	.byte	0x91
	.sleb128 -13
	.uleb128 0x1d
	.ascii "anubis_types__sss__eval_polynomial__L_4__T8b___L\0"
	.long	0x369
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.uleb128 0x1d
	.ascii "anubis_types__sss__eval_polynomial__L_4__T8b___U\0"
	.long	0x369
	.uleb128 0x2
	.byte	0x91
	.sleb128 -4
	.uleb128 0x18
	.quad	LBB7
	.set L$set$194,LBE7-LBB7
	.quad L$set$194
	.uleb128 0x20
	.ascii "i\0"
	.byte	0x1
	.byte	0x5c
	.byte	0xb
	.long	0x369
	.uleb128 0x2
	.byte	0x91
	.sleb128 -12
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0x14
	.ascii "anubis_types__sss__gf_div\0"
	.byte	0x1
	.byte	0x3b
	.byte	0x4
	.long	0x299
	.quad	LFB5
	.set L$set$195,LFE5-LFB5
	.quad L$set$195
	.uleb128 0x1
	.byte	0x9c
	.long	0x10c8
	.uleb128 0x15
	.ascii "a\0"
	.byte	0x1
	.byte	0x3b
	.byte	0x15
	.long	0x2b3
	.uleb128 0x2
	.byte	0x91
	.sleb128 -17
	.uleb128 0x15
	.ascii "b\0"
	.byte	0x1
	.byte	0x3b
	.byte	0x18
	.long	0x2b3
	.uleb128 0x2
	.byte	0x91
	.sleb128 -18
	.uleb128 0x20
	.ascii "inv\0"
	.byte	0x1
	.byte	0x3d
	.byte	0x7
	.long	0x299
	.uleb128 0x2
	.byte	0x91
	.sleb128 -5
	.uleb128 0x18
	.quad	LBB5
	.set L$set$196,LBE5-LBB5
	.quad L$set$196
	.uleb128 0x20
	.ascii "i\0"
	.byte	0x1
	.byte	0x46
	.byte	0xe
	.long	0x369
	.uleb128 0x2
	.byte	0x91
	.sleb128 -4
	.byte	0
	.byte	0
	.uleb128 0x24
	.ascii "anubis_types__sss__gf_mult\0"
	.byte	0x1
	.byte	0x1c
	.byte	0x4
	.long	0x299
	.quad	LFB4
	.set L$set$197,LFE4-LFB4
	.quad L$set$197
	.uleb128 0x1
	.byte	0x9c
	.uleb128 0x15
	.ascii "a\0"
	.byte	0x1
	.byte	0x1c
	.byte	0x16
	.long	0x2b3
	.uleb128 0x2
	.byte	0x91
	.sleb128 -17
	.uleb128 0x15
	.ascii "b\0"
	.byte	0x1
	.byte	0x1c
	.byte	0x19
	.long	0x2b3
	.uleb128 0x2
	.byte	0x91
	.sleb128 -18
	.uleb128 0x20
	.ascii "result\0"
	.byte	0x1
	.byte	0x1d
	.byte	0x7
	.long	0x299
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.uleb128 0x20
	.ascii "aa\0"
	.byte	0x1
	.byte	0x1e
	.byte	0x7
	.long	0x299
	.uleb128 0x2
	.byte	0x91
	.sleb128 -7
	.uleb128 0x20
	.ascii "bb\0"
	.byte	0x1
	.byte	0x1f
	.byte	0x7
	.long	0x299
	.uleb128 0x2
	.byte	0x91
	.sleb128 -6
	.uleb128 0x18
	.quad	LBB3
	.set L$set$198,LBE3-LBB3
	.quad L$set$198
	.uleb128 0x20
	.ascii "i\0"
	.byte	0x1
	.byte	0x21
	.byte	0xb
	.long	0x369
	.uleb128 0x2
	.byte	0x91
	.sleb128 -4
	.uleb128 0x18
	.quad	LBB4
	.set L$set$199,LBE4-LBB4
	.quad L$set$199
	.uleb128 0x20
	.ascii "high_bit_set\0"
	.byte	0x1
	.byte	0x2b
	.byte	0xd
	.long	0x468
	.uleb128 0x2
	.byte	0x91
	.sleb128 -5
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
	.uleb128 0x21
	.byte	0
	.uleb128 0x2f
	.uleb128 0xd
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x3
	.uleb128 0x26
	.byte	0
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
	.uleb128 0x6
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
	.uleb128 0x7
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
	.uleb128 0x8
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
	.uleb128 0x9
	.uleb128 0xf
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0xa
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
	.uleb128 0xb
	.uleb128 0x26
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x34
	.uleb128 0x19
	.byte	0
	.byte	0
	.uleb128 0xc
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
	.uleb128 0xd
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
	.uleb128 0xe
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
	.uleb128 0xf
	.uleb128 0x21
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2f
	.uleb128 0xd
	.byte	0
	.byte	0
	.uleb128 0x10
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
	.uleb128 0x11
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
	.uleb128 0x12
	.uleb128 0x27
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
	.uleb128 0x1c
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0x13
	.uleb128 0x36
	.byte	0
	.uleb128 0x2
	.uleb128 0x18
	.byte	0
	.byte	0
	.uleb128 0x14
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
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0x5
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
	.uleb128 0x17
	.uleb128 0x5
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
	.uleb128 0x18
	.uleb128 0xb
	.byte	0x1
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x7
	.byte	0
	.byte	0
	.uleb128 0x19
	.uleb128 0x34
	.byte	0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0x5
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0x18
	.byte	0
	.byte	0
	.uleb128 0x1a
	.uleb128 0x10
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x1b
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0x5
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
	.uleb128 0x1c
	.uleb128 0x34
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0x5
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0x18
	.byte	0
	.byte	0
	.uleb128 0x1d
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
	.uleb128 0x1e
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
	.uleb128 0x1f
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
	.uleb128 0x20
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
	.uleb128 0x21
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
	.uleb128 0x22
	.uleb128 0x21
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x22
	.uleb128 0xd
	.uleb128 0x2f
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x23
	.uleb128 0xb
	.byte	0x1
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x7
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x24
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
	.uleb128 0x49
	.uleb128 0x13
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
	.long	0x18e
	.short	0x2
	.set L$set$200,Ldebug_info0-Lsection__debug_info
	.long L$set$200
	.long	0x1190
	.long	0x5fe
	.ascii "anubis_types__sss__gf_sub\0"
	.long	0x651
	.ascii "anubis_types__sss__gf_add\0"
	.long	0x6a4
	.ascii "anubis_types__sss__zeroize_share\0"
	.long	0x716
	.ascii "anubis_types__sss__get_share_length\0"
	.long	0x769
	.ascii "anubis_types__sss__make_share\0"
	.long	0x87c
	.ascii "anubis_types__sss__get_share_index\0"
	.long	0x8ce
	.ascii "anubis_types__sss__combine_shares\0"
	.long	0xbdb
	.ascii "anubis_types__sss__split_secret\0"
	.long	0xf18
	.ascii "anubis_types__sss__eval_polynomial\0"
	.long	0x1047
	.ascii "anubis_types__sss__gf_div\0"
	.long	0x10c8
	.ascii "anubis_types__sss__gf_mult\0"
	.long	0
	.section __DWARF,__debug_pubtypes,regular,debug
Lsection__debug_pubtypes:
	.long	0x1d6
	.short	0x2
	.set L$set$201,Ldebug_info0-Lsection__debug_info
	.long L$set$201
	.long	0x1190
	.long	0x25b
	.ascii "anubis_types__sss__Tshare_indexB\0"
	.long	0x27f
	.ascii "interfaces__unsigned_8\0"
	.long	0x2b8
	.ascii "anubis_types__TbyteB\0"
	.long	0x369
	.ascii "integer\0"
	.long	0x32b
	.ascii "anubis_types__byte_array___XUA\0"
	.long	0x374
	.ascii "anubis_types__byte_array___XUB\0"
	.long	0x2d0
	.ascii "anubis_types__byte_array\0"
	.long	0x3d0
	.ascii "anubis_types__sss__secret_share__T7s\0"
	.long	0x468
	.ascii "boolean\0"
	.long	0x406
	.ascii "anubis_types__sss__secret_share\0"
	.long	0x4ca
	.ascii "anubis_types__sss__share_array___XUA\0"
	.long	0x50e
	.ascii "anubis_types__sss__share_array___XUB\0"
	.long	0x473
	.ascii "anubis_types__sss__share_array\0"
	.long	0x55d
	.ascii "interfaces__c__unsigned\0"
	.long	0x59f
	.ascii "universal_integer\0"
	.long	0
	.section __DWARF,__debug_aranges,regular,debug
Lsection__debug_aranges:
	.long	0x2c
	.short	0x2
	.set L$set$202,Ldebug_info0-Lsection__debug_info
	.long L$set$202
	.byte	0x8
	.byte	0
	.short	0
	.short	0
	.quad	Ltext0
	.set L$set$203,Letext0-Ltext0
	.quad L$set$203
	.quad	0
	.quad	0
	.section __DWARF,__debug_line,regular,debug
Lsection__debug_line:
Ldebug_line0:
	.section __DWARF,__debug_str,regular,debug
Lsection__debug_str:
LASF0:
	.ascii "P_BOUNDS\0"
LASF3:
	.ascii "byte_idx\0"
LASF1:
	.ascii "share\0"
LASF2:
	.ascii "threshold\0"
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
