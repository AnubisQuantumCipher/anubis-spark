	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
Ltext0:
	.file 1 "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_types-pqc.adb"
	.align	2
	.globl _anubis_types__pqc__hybrid_signatureIP
_anubis_types__pqc__hybrid_signatureIP:
LFB2:
	sub	sp, sp, #16
LCFI0:
	str	x0, [sp, 8]
	add	sp, sp, 16
LCFI1:
	ret
LFE2:
	.align	2
	.globl _anubis_types__pqc__hybrid_shared_secretIP
_anubis_types__pqc__hybrid_shared_secretIP:
LFB3:
	sub	sp, sp, #16
LCFI2:
	str	x0, [sp, 8]
	ldr	x0, [sp, 8]
	strb	wzr, [x0, 64]
	add	sp, sp, 16
LCFI3:
	ret
LFE3:
	.align	2
	.globl _anubis_types__pqc__is_valid
_anubis_types__pqc__is_valid:
LFB4:
	.loc 1 21 4
	sub	sp, sp, #16
LCFI4:
	str	x0, [sp, 8]
	.loc 1 23 7
	ldr	x0, [sp, 8]
	ldrb	w0, [x0, 64]
	.loc 1 24 8
	add	sp, sp, 16
LCFI5:
	ret
LFE4:
	.align	2
	.globl _anubis_types__pqc__ml_kem_generate_keypair
_anubis_types__pqc__ml_kem_generate_keypair:
LFB5:
	.loc 1 30 4
	sub	x10, sp, #16384
	str	xzr, [x10, 4032]
	sub	sp, sp, #64
LCFI6:
	stp	x29, x30, [sp, 48]
LCFI7:
	add	x29, sp, 48
LCFI8:
	str	x0, [x29, -40]
	str	x1, [x29, -48]
	.loc 1 30 4
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x1, [x0]
	str	x1, [x29, -8]
	mov	x1, 0
LBB2:
	.loc 1 38 31
	ldr	x1, [x29, -48]
	ldr	x0, [x29, -40]
	bl	_OQS_KEM_ml_kem_1024_keypair
	.loc 1 38 31 is_stmt 0 discriminator 1
	str	w0, [x29, -12]
	.loc 1 43 7 is_stmt 1
	ldr	w0, [x29, -12]
	cmp	w0, 0
	bne	L8
	.loc 1 44 27
	ldr	x0, [x29, -48]
	mov	w1, 1
	strb	w1, [x0, 3168]
	.loc 1 45 18
	mov	w0, 1
	strb	w0, [x29, -17]
	.loc 1 54 8
	b	L15
L8:
	.loc 1 48 27
	ldr	x0, [x29, -48]
	strb	wzr, [x0, 3168]
LBB3:
	.loc 1 49 14
	mov	w0, 1
	str	w0, [x29, -16]
L11:
	.loc 1 49 14 is_stmt 0 discriminator 1
	ldr	w0, [x29, -16]
	cmp	w0, 3168
	bgt	L10
	.loc 1 50 33 is_stmt 1
	ldrsw	x0, [x29, -16]
	ldr	x1, [x29, -48]
	add	x0, x1, x0
	strb	wzr, [x0, -1]
	.loc 1 49 14 discriminator 2
	ldr	w0, [x29, -16]
	add	w0, w0, 1
	str	w0, [x29, -16]
	.loc 1 51 18
	b	L11
L10:
LBE3:
	.loc 1 52 18
	strb	wzr, [x29, -17]
L15:
	.loc 1 54 8
	nop
LBE2:
	ldrb	w0, [x29, -17]
	.loc 1 54 8 is_stmt 0 discriminator 2
	mov	w1, w0
	.loc 1 54 8
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x3, [x29, -8]
	ldr	x2, [x0]
	subs	x3, x3, x2
	mov	x2, 0
	beq	L13
	bl	___stack_chk_fail
L13:
	mov	w0, w1
	ldp	x29, x30, [sp, 48]
	add	sp, sp, 64
LCFI9:
	ret
LFE5:
	.align	2
	.globl _anubis_types__pqc__ml_kem_encapsulate
_anubis_types__pqc__ml_kem_encapsulate:
LFB6:
	.loc 1 56 4 is_stmt 1
	sub	x10, sp, #16384
	str	xzr, [x10, 4016]
	sub	sp, sp, #80
LCFI10:
	stp	x29, x30, [sp, 64]
LCFI11:
	add	x29, sp, 64
LCFI12:
	str	x0, [x29, -40]
	str	x1, [x29, -48]
	str	x2, [x29, -56]
	.loc 1 56 4
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x1, [x0]
	str	x1, [x29, -8]
	mov	x1, 0
LBB4:
	.loc 1 65 31
	ldr	x2, [x29, -40]
	ldr	x1, [x29, -56]
	ldr	x0, [x29, -48]
	bl	_OQS_KEM_ml_kem_1024_encaps
	.loc 1 65 31 is_stmt 0 discriminator 1
	str	w0, [x29, -12]
	.loc 1 71 7 is_stmt 1
	ldr	w0, [x29, -12]
	cmp	w0, 0
	bne	L17
	.loc 1 72 30
	ldr	x0, [x29, -56]
	mov	w1, 1
	strb	w1, [x0, 32]
	.loc 1 73 18
	mov	w0, 1
	strb	w0, [x29, -17]
	.loc 1 82 8
	b	L24
L17:
	.loc 1 76 30
	ldr	x0, [x29, -56]
	strb	wzr, [x0, 32]
LBB5:
	.loc 1 77 14
	mov	w0, 1
	str	w0, [x29, -16]
L20:
	.loc 1 77 14 is_stmt 0 discriminator 1
	ldr	w0, [x29, -16]
	cmp	w0, 32
	bgt	L19
	.loc 1 78 36 is_stmt 1
	ldrsw	x0, [x29, -16]
	ldr	x1, [x29, -56]
	add	x0, x1, x0
	strb	wzr, [x0, -1]
	.loc 1 77 14 discriminator 2
	ldr	w0, [x29, -16]
	add	w0, w0, 1
	str	w0, [x29, -16]
	.loc 1 79 18
	b	L20
L19:
LBE5:
	.loc 1 80 18
	strb	wzr, [x29, -17]
L24:
	.loc 1 82 8
	nop
LBE4:
	ldrb	w0, [x29, -17]
	.loc 1 82 8 is_stmt 0 discriminator 2
	mov	w1, w0
	.loc 1 82 8
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x3, [x29, -8]
	ldr	x2, [x0]
	subs	x3, x3, x2
	mov	x2, 0
	beq	L22
	bl	___stack_chk_fail
L22:
	mov	w0, w1
	ldp	x29, x30, [sp, 64]
	add	sp, sp, 80
LCFI13:
	ret
LFE6:
	.align	2
	.globl _anubis_types__pqc__ml_kem_decapsulate
_anubis_types__pqc__ml_kem_decapsulate:
LFB7:
	.loc 1 84 4 is_stmt 1
	sub	x10, sp, #16384
	str	xzr, [x10, 4016]
	sub	sp, sp, #80
LCFI14:
	stp	x29, x30, [sp, 64]
LCFI15:
	add	x29, sp, 64
LCFI16:
	str	x0, [x29, -40]
	str	x1, [x29, -48]
	str	x2, [x29, -56]
	.loc 1 84 4
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x1, [x0]
	str	x1, [x29, -8]
	mov	x1, 0
LBB6:
	.loc 1 93 31
	ldr	x2, [x29, -48]
	ldr	x1, [x29, -40]
	ldr	x0, [x29, -56]
	bl	_OQS_KEM_ml_kem_1024_decaps
	.loc 1 93 31 is_stmt 0 discriminator 1
	str	w0, [x29, -12]
	.loc 1 99 7 is_stmt 1
	ldr	w0, [x29, -12]
	cmp	w0, 0
	bne	L26
	.loc 1 100 30
	ldr	x0, [x29, -56]
	mov	w1, 1
	strb	w1, [x0, 32]
	.loc 1 101 18
	mov	w0, 1
	strb	w0, [x29, -17]
	.loc 1 110 8
	b	L33
L26:
	.loc 1 104 30
	ldr	x0, [x29, -56]
	strb	wzr, [x0, 32]
LBB7:
	.loc 1 105 14
	mov	w0, 1
	str	w0, [x29, -16]
L29:
	.loc 1 105 14 is_stmt 0 discriminator 1
	ldr	w0, [x29, -16]
	cmp	w0, 32
	bgt	L28
	.loc 1 106 36 is_stmt 1
	ldrsw	x0, [x29, -16]
	ldr	x1, [x29, -56]
	add	x0, x1, x0
	strb	wzr, [x0, -1]
	.loc 1 105 14 discriminator 2
	ldr	w0, [x29, -16]
	add	w0, w0, 1
	str	w0, [x29, -16]
	.loc 1 107 18
	b	L29
L28:
LBE7:
	.loc 1 108 18
	strb	wzr, [x29, -17]
L33:
	.loc 1 110 8
	nop
LBE6:
	ldrb	w0, [x29, -17]
	.loc 1 110 8 is_stmt 0 discriminator 2
	mov	w1, w0
	.loc 1 110 8
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x3, [x29, -8]
	ldr	x2, [x0]
	subs	x3, x3, x2
	mov	x2, 0
	beq	L31
	bl	___stack_chk_fail
L31:
	mov	w0, w1
	ldp	x29, x30, [sp, 64]
	add	sp, sp, 80
LCFI17:
	ret
LFE7:
	.align	2
	.globl _anubis_types__pqc__ml_dsa_generate_keypair
_anubis_types__pqc__ml_dsa_generate_keypair:
LFB8:
	.loc 1 116 4 is_stmt 1
	sub	x10, sp, #16384
	str	xzr, [x10, 4032]
	sub	sp, sp, #64
LCFI18:
	stp	x29, x30, [sp, 48]
LCFI19:
	add	x29, sp, 48
LCFI20:
	str	x0, [x29, -40]
	str	x1, [x29, -48]
	.loc 1 116 4
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x1, [x0]
	str	x1, [x29, -8]
	mov	x1, 0
LBB8:
	.loc 1 124 31
	ldr	x1, [x29, -48]
	ldr	x0, [x29, -40]
	bl	_OQS_SIG_ml_dsa_87_keypair
	.loc 1 124 31 is_stmt 0 discriminator 1
	str	w0, [x29, -12]
	.loc 1 129 7 is_stmt 1
	ldr	w0, [x29, -12]
	cmp	w0, 0
	bne	L35
	.loc 1 130 27
	ldr	x0, [x29, -48]
	add	x0, x0, 4096
	mov	w1, 1
	strb	w1, [x0, 800]
	.loc 1 131 18
	mov	w0, 1
	strb	w0, [x29, -17]
	.loc 1 140 8
	b	L42
L35:
	.loc 1 134 27
	ldr	x0, [x29, -48]
	add	x0, x0, 4096
	strb	wzr, [x0, 800]
LBB9:
	.loc 1 135 14
	mov	w0, 1
	str	w0, [x29, -16]
L38:
	.loc 1 135 14 is_stmt 0 discriminator 1
	ldr	w1, [x29, -16]
	mov	w0, 4896
	cmp	w1, w0
	bgt	L37
	.loc 1 136 33 is_stmt 1
	ldrsw	x0, [x29, -16]
	ldr	x1, [x29, -48]
	add	x0, x1, x0
	strb	wzr, [x0, -1]
	.loc 1 135 14 discriminator 2
	ldr	w0, [x29, -16]
	add	w0, w0, 1
	str	w0, [x29, -16]
	.loc 1 137 18
	b	L38
L37:
LBE9:
	.loc 1 138 18
	strb	wzr, [x29, -17]
L42:
	.loc 1 140 8
	nop
LBE8:
	ldrb	w0, [x29, -17]
	.loc 1 140 8 is_stmt 0 discriminator 2
	mov	w1, w0
	.loc 1 140 8
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x3, [x29, -8]
	ldr	x2, [x0]
	subs	x3, x3, x2
	mov	x2, 0
	beq	L40
	bl	___stack_chk_fail
L40:
	mov	w0, w1
	ldp	x29, x30, [sp, 48]
	add	sp, sp, 64
LCFI21:
	ret
LFE8:
	.const
	.align	3
lC4:
	.ascii "anubis_types-pqc.adb"
	.space 1
	.text
	.align	2
	.globl _anubis_types__pqc__ml_dsa_sign
_anubis_types__pqc__ml_dsa_sign:
LFB9:
	.loc 1 142 4 is_stmt 1
	sub	x10, sp, #16384
	str	xzr, [x10, 4016]
	sub	sp, sp, #80
LCFI22:
	stp	x29, x30, [sp, 64]
LCFI23:
	add	x29, sp, 64
LCFI24:
	stp	x0, x1, [x29, -48]
	str	x2, [x29, -56]
	str	x3, [x29, -64]
	.loc 1 142 4
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x1, [x0]
	str	x1, [x29, -8]
	mov	x1, 0
	ldr	x0, [x29, -40]
	ldr	w1, [x0]
	ldr	x0, [x29, -40]
	ldr	w0, [x0, 4]
	cmp	w0, w1
	blt	L44
	.loc 1 142 4 is_stmt 0 discriminator 1
	sxtw	x2, w0
	mov	x12, x2
	asr	x2, x2, 63
	mov	x13, x2
	sxtw	x2, w1
	mov	x10, x2
	asr	x2, x2, 63
	mov	x11, x2
	subs	x3, x12, x10
	sbc	x2, x13, x11
	mov	x8, x3
	mov	x9, x2
	mov	x2, 1
	adds	x3, x8, x2
	mov	x2, 0
	adc	x2, x9, x2
	mov	x14, x3
	mov	x15, x2
	b	L45
L44:
	.loc 1 142 4 discriminator 2
	mov	x14, 0
	mov	x15, 0
L45:
LBB10:
	.loc 1 142 4 discriminator 4
	cmp	w0, w1
	.loc 1 142 4 discriminator 8
	cmp	w0, w1
	blt	L49
	.loc 1 142 4 discriminator 9
	sxtw	x3, w0
	sxtw	x2, w1
	sub	x2, x3, x2
	add	x2, x2, 1
	mov	x4, x2
	mov	x5, 0
	lsr	x2, x4, 61
	lsl	x7, x5, 3
	orr	x7, x2, x7
	lsl	x6, x4, 3
L49:
	.loc 1 142 4 discriminator 12
	cmp	w0, w1
	.loc 1 149 7 is_stmt 1
	mov	x2, 4627
	str	x2, [x29, -16]
	.loc 1 155 27
	cmp	w1, w0
	ble	L52
	.loc 1 155 27 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L53
	bl	___stack_chk_fail
L53:
	mov	w1, 155
	adrp	x0, lC4@PAGE
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L52:
	.loc 1 152 31 is_stmt 1
	ldr	x1, [x29, -48]
	mov	x0, x14
	cmp	x0, 0
	bge	L54
	.loc 1 152 31 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L55
	bl	___stack_chk_fail
L55:
	mov	w1, 156
	adrp	x0, lC4@PAGE
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L54:
	.loc 1 152 31 discriminator 2
	mov	x2, x14
	.loc 1 152 31 discriminator 4
	sub	x0, x29, #16
	ldr	x4, [x29, -56]
	mov	x3, x2
	mov	x2, x1
	mov	x1, x0
	ldr	x0, [x29, -64]
	bl	_OQS_SIG_ml_dsa_87_sign
	.loc 1 152 31 discriminator 5
	str	w0, [x29, -20]
	.loc 1 160 26 is_stmt 1
	ldr	w0, [x29, -20]
	cmp	w0, 0
	cset	w0, eq
	and	w0, w0, 255
	.loc 1 160 15
	strb	w0, [x29, -21]
LBE10:
	.loc 1 161 8 discriminator 1
	ldrb	w0, [x29, -21]
	.loc 1 161 8 is_stmt 0 discriminator 3
	mov	w1, w0
	.loc 1 161 8
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x3, [x29, -8]
	ldr	x2, [x0]
	subs	x3, x3, x2
	mov	x2, 0
	beq	L57
	bl	___stack_chk_fail
L57:
	mov	w0, w1
	ldp	x29, x30, [sp, 64]
	add	sp, sp, 80
LCFI25:
	ret
LFE9:
	.align	2
	.globl _anubis_types__pqc__ml_dsa_verify
_anubis_types__pqc__ml_dsa_verify:
LFB10:
	.loc 1 163 4 is_stmt 1
	sub	x10, sp, #16384
	str	xzr, [x10, 4032]
	stp	x29, x30, [sp, -64]!
LCFI26:
	mov	x29, sp
LCFI27:
	stp	x0, x1, [x29, 32]
	str	x2, [x29, 24]
	str	x3, [x29, 16]
	.loc 1 163 4
	ldr	x0, [x29, 40]
	ldr	w1, [x0]
	ldr	x0, [x29, 40]
	ldr	w0, [x0, 4]
	cmp	w0, w1
	blt	L59
	.loc 1 163 4 is_stmt 0 discriminator 1
	sxtw	x2, w0
	mov	x12, x2
	asr	x2, x2, 63
	mov	x13, x2
	sxtw	x2, w1
	mov	x10, x2
	asr	x2, x2, 63
	mov	x11, x2
	subs	x3, x12, x10
	sbc	x2, x13, x11
	mov	x8, x3
	mov	x9, x2
	mov	x2, 1
	adds	x3, x8, x2
	mov	x2, 0
	adc	x2, x9, x2
	mov	x14, x3
	mov	x15, x2
	b	L60
L59:
	.loc 1 163 4 discriminator 2
	mov	x14, 0
	mov	x15, 0
L60:
LBB11:
	.loc 1 163 4 discriminator 4
	cmp	w0, w1
	.loc 1 163 4 discriminator 8
	cmp	w0, w1
	blt	L64
	.loc 1 163 4 discriminator 9
	sxtw	x3, w0
	sxtw	x2, w1
	sub	x2, x3, x2
	add	x2, x2, 1
	mov	x4, x2
	mov	x5, 0
	lsr	x2, x4, 61
	lsl	x7, x5, 3
	orr	x7, x2, x7
	lsl	x6, x4, 3
L64:
	.loc 1 163 4 discriminator 12
	cmp	w0, w1
	.loc 1 173 27 is_stmt 1
	cmp	w1, w0
	ble	L67
	.loc 1 173 27 is_stmt 0 discriminator 1
	mov	w1, 173
	adrp	x0, lC4@PAGE
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L67:
	.loc 1 172 31 is_stmt 1
	ldr	x0, [x29, 32]
	mov	x1, x14
	cmp	x1, 0
	bge	L68
	.loc 1 172 31 is_stmt 0 discriminator 1
	mov	w1, 174
	adrp	x0, lC4@PAGE
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L68:
	.loc 1 172 31 discriminator 2
	mov	x1, x14
	.loc 1 172 31 discriminator 4
	ldr	x4, [x29, 16]
	mov	x3, 4627
	ldr	x2, [x29, 24]
	bl	_OQS_SIG_ml_dsa_87_verify
	.loc 1 172 31 discriminator 1
	str	w0, [x29, 60]
	.loc 1 180 7 is_stmt 1
	ldr	w0, [x29, 60]
	cmp	w0, 0
	cset	w0, eq
	and	w0, w0, 255
LBE11:
	.loc 1 181 8
	ldp	x29, x30, [sp], 64
LCFI28:
	ret
LFE10:
	.align	2
	.globl _anubis_types__pqc__secrets_match
_anubis_types__pqc__secrets_match:
LFB11:
	.loc 1 187 4
	sub	x10, sp, #16384
	str	xzr, [x10, 4032]
	stp	x29, x30, [sp, -64]!
LCFI29:
	mov	x29, sp
LCFI30:
	str	x0, [x29, 24]
	str	x1, [x29, 16]
	.loc 1 193 7
	ldr	x0, [x29, 24]
	str	x0, [x29, 48]
	.loc 1 194 7
	ldr	x0, [x29, 16]
	str	x0, [x29, 56]
	.loc 1 198 17
	ldr	x0, [x29, 48]
	ldr	x1, [x29, 56]
	mov	x2, 32
	bl	_OQS_MEM_secure_bcmp
	.loc 1 198 17 is_stmt 0 discriminator 1
	str	w0, [x29, 44]
	.loc 1 199 7 is_stmt 1
	ldr	w0, [x29, 44]
	cmp	w0, 0
	cset	w0, eq
	and	w0, w0, 255
	.loc 1 200 8
	ldp	x29, x30, [sp], 64
LCFI31:
	ret
LFE11:
	.align	2
	.globl _anubis_types__pqc__zeroize_shared_secret
_anubis_types__pqc__zeroize_shared_secret:
LFB12:
	.loc 1 206 4
	sub	x10, sp, #16384
	str	xzr, [x10, 4048]
	stp	x29, x30, [sp, -48]!
LCFI32:
	mov	x29, sp
LCFI33:
	str	x0, [x29, 24]
	.loc 1 209 7
	ldr	x0, [x29, 24]
	str	x0, [x29, 40]
	.loc 1 212 7
	ldr	x0, [x29, 40]
	mov	x1, 32
	bl	_OQS_MEM_cleanse
	.loc 1 213 20
	ldr	x0, [x29, 24]
	strb	wzr, [x0, 32]
	.loc 1 214 8
	nop
	ldp	x29, x30, [sp], 48
LCFI34:
	ret
LFE12:
	.align	2
	.globl _anubis_types__pqc__zeroize_ml_kem_secret
_anubis_types__pqc__zeroize_ml_kem_secret:
LFB13:
	.loc 1 216 4
	sub	x10, sp, #16384
	str	xzr, [x10, 4048]
	stp	x29, x30, [sp, -48]!
LCFI35:
	mov	x29, sp
LCFI36:
	str	x0, [x29, 24]
	.loc 1 219 7
	ldr	x0, [x29, 24]
	str	x0, [x29, 40]
	.loc 1 221 7
	ldr	x0, [x29, 40]
	mov	x1, 3168
	bl	_OQS_MEM_cleanse
	.loc 1 222 24
	ldr	x0, [x29, 24]
	strb	wzr, [x0, 3168]
	.loc 1 223 8
	nop
	ldp	x29, x30, [sp], 48
LCFI37:
	ret
LFE13:
	.align	2
	.globl _anubis_types__pqc__zeroize_ml_dsa_secret
_anubis_types__pqc__zeroize_ml_dsa_secret:
LFB14:
	.loc 1 225 4
	sub	x10, sp, #16384
	str	xzr, [x10, 4048]
	stp	x29, x30, [sp, -48]!
LCFI38:
	mov	x29, sp
LCFI39:
	str	x0, [x29, 24]
	.loc 1 228 7
	ldr	x0, [x29, 24]
	str	x0, [x29, 40]
	.loc 1 230 7
	ldr	x0, [x29, 40]
	mov	x1, 4896
	bl	_OQS_MEM_cleanse
	.loc 1 231 24
	ldr	x0, [x29, 24]
	add	x0, x0, 4096
	strb	wzr, [x0, 800]
	.loc 1 232 8
	nop
	ldp	x29, x30, [sp], 48
LCFI40:
	ret
LFE14:
	.const
	.align	3
lC5:
	.ascii "anubis-hybrid-kem-v1"
	.text
	.align	2
	.globl _anubis_types__pqc__hybrid_encapsulate
_anubis_types__pqc__hybrid_encapsulate:
LFB15:
	.loc 1 238 4
	sub	x10, sp, #16384
	str	xzr, [x10, 3760]
	sub	sp, sp, #336
LCFI41:
	stp	x29, x30, [sp, 272]
LCFI42:
	add	x29, sp, 272
LCFI43:
	stp	x20, x21, [sp, 288]
	stp	x22, x23, [sp, 304]
	stp	x24, x25, [sp, 320]
LCFI44:
	str	x0, [x29, -232]
	str	x1, [x29, -240]
	str	x2, [x29, -248]
	str	x3, [x29, -256]
	sub	x0, x29, #16384
	str	x4, [x0, 16120]
	.loc 1 238 4
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x1, [x0]
	str	x1, [x29, -8]
	mov	x1, 0
LBB12:
	.loc 1 246 7
	sub	x0, x29, #152
	bl	_anubis_types__ml_kem_shared_secretIP
	.loc 1 247 7
	sub	x0, x29, #112
	bl	_anubis_types__x25519_shared_secretIP
	.loc 1 254 16
	sub	x0, x29, #216
	ldr	x1, [x29, -248]
	bl	_anubis_types__classical__x25519_generate_keypair
	.loc 1 254 16 is_stmt 0 discriminator 1
	strb	w0, [x29, -222]
	.loc 1 260 10 is_stmt 1
	ldrb	w0, [x29, -222]
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 260 7
	cmp	w0, 0
	beq	L79
	.loc 1 261 18
	strb	wzr, [x29, -223]
	.loc 1 262 30
	sub	x0, x29, #16384
	ldr	x0, [x0, 16120]
	strb	wzr, [x0, 64]
	.loc 1 263 10
	b	L87
L79:
	.loc 1 267 16
	sub	x0, x29, #112
	mov	x2, x0
	ldr	x1, [x29, -232]
	ldr	x0, [x29, -248]
	bl	_anubis_types__classical__x25519_compute_shared
	.loc 1 267 16 is_stmt 0 discriminator 1
	strb	w0, [x29, -222]
	.loc 1 274 10 is_stmt 1
	ldrb	w0, [x29, -222]
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 274 7
	cmp	w0, 0
	beq	L81
	.loc 1 275 18
	strb	wzr, [x29, -223]
	.loc 1 276 30
	sub	x0, x29, #16384
	ldr	x0, [x0, 16120]
	strb	wzr, [x0, 64]
	.loc 1 277 19
	ldr	x0, [x29, -248]
	bl	_anubis_types__classical__zeroize_x25519_secret
	.loc 1 278 10
	b	L87
L81:
	.loc 1 282 7
	sub	x0, x29, #152
	mov	x2, x0
	ldr	x1, [x29, -256]
	ldr	x0, [x29, -240]
	bl	_anubis_types__pqc__ml_kem_encapsulate
	.loc 1 282 7 is_stmt 0 discriminator 1
	strb	w0, [x29, -221]
	.loc 1 289 10 is_stmt 1
	ldrb	w0, [x29, -221]
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 289 7
	cmp	w0, 0
	beq	L82
	.loc 1 290 18
	strb	wzr, [x29, -223]
	.loc 1 291 30
	sub	x0, x29, #16384
	ldr	x0, [x0, 16120]
	strb	wzr, [x0, 64]
	.loc 1 292 19
	ldr	x0, [x29, -248]
	bl	_anubis_types__classical__zeroize_x25519_secret
	.loc 1 293 19
	sub	x0, x29, #112
	bl	_anubis_types__classical__zeroize_x25519_shared
	.loc 1 294 10
	b	L87
L82:
	.loc 1 299 32
	sub	x0, x29, #72
	sub	x1, x29, #112
	ldr	q30, [x1]
	ldr	q31, [x1, 16]
	str	q30, [x0]
	str	q31, [x0, 16]
	.loc 1 300 33
	sub	x0, x29, #40
	sub	x1, x29, #152
	ldr	q30, [x1]
	ldr	q31, [x1, 16]
	str	q30, [x0]
	str	q31, [x0, 16]
LBB13:
LBB14:
	.loc 1 306 19
	sub	x0, x29, #72
	mov	x22, x0
	adrp	x0, lC0@PAGE
	add	x23, x0, lC0@PAGEOFF;
	adrp	x0, lC5@PAGE
	add	x20, x0, lC5@PAGEOFF;
	adrp	x0, lC1@PAGE
	add	x21, x0, lC1@PAGEOFF;
	sub	x0, x29, #184
	mov	x24, x0
	adrp	x0, lC2@PAGE
	add	x25, x0, lC2@PAGEOFF;
	mov	x4, x24
	mov	x5, x25
	mov	x2, x20
	mov	x3, x21
	mov	x0, x22
	mov	x1, x23
	bl	_anubis_types__classical__hkdf_derive
	.loc 1 306 19 is_stmt 0 discriminator 1
	strb	w0, [x29, -223]
LBE14:
	.loc 1 312 34 is_stmt 1
	sub	x0, x29, #16384
	ldr	x0, [x0, 16120]
	add	x0, x0, 32
	mov	x1, x0
	sub	x0, x29, #184
	ldr	q30, [x0]
	ldr	q31, [x0, 16]
	str	q30, [x1]
	str	q31, [x1, 16]
LBE13:
	.loc 1 315 7
	ldrb	w0, [x29, -223]
	cmp	w0, 0
	beq	L83
	.loc 1 316 30
	sub	x0, x29, #16384
	ldr	x0, [x0, 16120]
	mov	w1, 1
	strb	w1, [x0, 64]
	b	L84
L83:
	.loc 1 318 30
	sub	x0, x29, #16384
	ldr	x0, [x0, 16120]
	strb	wzr, [x0, 64]
L84:
	.loc 1 322 7
	sub	x0, x29, #152
	bl	_anubis_types__pqc__zeroize_shared_secret
	.loc 1 323 16
	sub	x0, x29, #112
	bl	_anubis_types__classical__zeroize_x25519_shared
LBB15:
	.loc 1 324 11
	mov	w0, 1
	str	w0, [x29, -220]
L86:
	.loc 1 324 11 is_stmt 0 discriminator 1
	ldr	w0, [x29, -220]
	cmp	w0, 64
	bgt	L90
	.loc 1 325 29 is_stmt 1
	ldrsw	x0, [x29, -220]
	sub	x1, x29, #73
	strb	wzr, [x1, x0]
	.loc 1 324 11 discriminator 2
	ldr	w0, [x29, -220]
	add	w0, w0, 1
	str	w0, [x29, -220]
	.loc 1 326 15
	b	L86
L90:
LBE15:
	.loc 1 327 8
	nop
L87:
LBE12:
	.loc 1 327 8 is_stmt 0 discriminator 1
	ldrb	w0, [x29, -223]
	.loc 1 327 8 discriminator 3
	mov	w1, w0
	.loc 1 327 8
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x3, [x29, -8]
	ldr	x2, [x0]
	subs	x3, x3, x2
	mov	x2, 0
	beq	L89
	bl	___stack_chk_fail
L89:
	mov	w0, w1
	ldp	x29, x30, [sp, 272]
	ldp	x20, x21, [sp, 288]
	ldp	x22, x23, [sp, 304]
	ldp	x24, x25, [sp, 320]
	add	sp, sp, 336
LCFI45:
	ret
LFE15:
	.const
	.align	2
lC0:
	.word	1
	.word	64
	.align	2
lC1:
	.word	1
	.word	20
	.align	2
lC2:
	.word	1
	.word	32
	.text
	.const
	.align	3
lC6:
	.ascii "anubis-xchacha20-key-v1"
	.text
	.align	2
	.globl _anubis_types__pqc__derive_encryption_key
_anubis_types__pqc__derive_encryption_key:
LFB16:
	.loc 1 329 4 is_stmt 1
	sub	x10, sp, #16384
	str	xzr, [x10, 3984]
	sub	sp, sp, #112
LCFI46:
	stp	x29, x30, [sp, 96]
LCFI47:
	add	x29, sp, 96
LCFI48:
	str	x0, [x29, -88]
	str	x1, [x29, -96]
	.loc 1 329 4
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x1, [x0]
	str	x1, [x29, -8]
	mov	x1, 0
	.loc 1 335 10
	ldr	x0, [x29, -88]
	ldrb	w0, [x0, 64]
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 335 7
	cmp	w0, 0
	beq	L92
	.loc 1 336 18
	strb	wzr, [x29, -73]
	.loc 1 337 31
	ldr	x0, [x29, -96]
	strb	wzr, [x0, 32]
	.loc 1 338 10
	b	L93
L92:
LBB16:
	.loc 1 347 21
	ldr	x0, [x29, -88]
	add	x1, x0, 32
	sub	x0, x29, #72
	ldr	q30, [x1]
	ldr	q31, [x1, 16]
	str	q30, [x0]
	str	q31, [x0, 16]
LBB17:
	.loc 1 349 19
	sub	x0, x29, #72
	mov	x6, x0
	adrp	x0, lC2@PAGE
	add	x7, x0, lC2@PAGEOFF;
	adrp	x0, lC6@PAGE
	add	x2, x0, lC6@PAGEOFF;
	adrp	x0, lC3@PAGE
	add	x3, x0, lC3@PAGEOFF;
	sub	x0, x29, #40
	mov	x4, x0
	adrp	x0, lC2@PAGE
	add	x5, x0, lC2@PAGEOFF;
	mov	x0, x6
	mov	x1, x7
	bl	_anubis_types__classical__hkdf_derive
	.loc 1 349 19 is_stmt 0 discriminator 1
	strb	w0, [x29, -73]
LBE17:
	.loc 1 355 30 is_stmt 1
	ldr	x0, [x29, -96]
	mov	x1, x0
	sub	x0, x29, #40
	ldr	q30, [x0]
	ldr	q31, [x0, 16]
	str	q30, [x1]
	str	q31, [x1, 16]
LBE16:
	.loc 1 358 7
	ldrb	w0, [x29, -73]
	cmp	w0, 0
	beq	L94
	.loc 1 359 31
	ldr	x0, [x29, -96]
	mov	w1, 1
	strb	w1, [x0, 32]
	.loc 1 363 8
	b	L93
L94:
	.loc 1 361 31
	ldr	x0, [x29, -96]
	strb	wzr, [x0, 32]
L93:
	.loc 1 363 8
	ldrb	w0, [x29, -73]
	.loc 1 363 8 is_stmt 0 discriminator 2
	mov	w1, w0
	.loc 1 363 8
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x3, [x29, -8]
	ldr	x2, [x0]
	subs	x3, x3, x2
	mov	x2, 0
	beq	L97
	bl	___stack_chk_fail
L97:
	mov	w0, w1
	ldp	x29, x30, [sp, 96]
	add	sp, sp, 112
LCFI49:
	ret
LFE16:
	.const
	.align	2
lC3:
	.word	1
	.word	23
	.text
	.align	2
	.globl _anubis_types__pqc__hybrid_sign
_anubis_types__pqc__hybrid_sign:
LFB17:
	.loc 1 370 4 is_stmt 1
	sub	x10, sp, #16384
	str	xzr, [x10, 4000]
	sub	sp, sp, #96
LCFI50:
	stp	x29, x30, [sp, 80]
LCFI51:
	add	x29, sp, 80
LCFI52:
	stp	x0, x1, [x29, -48]
	str	x2, [x29, -56]
	str	x3, [x29, -64]
	str	x4, [x29, -72]
	.loc 1 370 4
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x1, [x0]
	str	x1, [x29, -8]
	mov	x1, 0
	ldr	x0, [x29, -40]
	ldr	w0, [x0]
	ldr	x1, [x29, -40]
	ldr	w1, [x1, 4]
LBB18:
	cmp	w1, w0
	.loc 1 370 4 is_stmt 0 discriminator 4
	cmp	w1, w0
	blt	L102
	.loc 1 370 4 discriminator 5
	sxtw	x3, w1
	sxtw	x2, w0
	sub	x2, x3, x2
	add	x2, x2, 1
	mov	x6, x2
	mov	x7, 0
	lsr	x2, x6, 61
	lsl	x9, x7, 3
	orr	x9, x2, x9
	lsl	x8, x6, 3
L102:
	.loc 1 370 4 discriminator 8
	cmp	w1, w0
	.loc 1 381 16 is_stmt 1
	ldr	x0, [x29, -72]
	mov	x3, x0
	ldr	x2, [x29, -56]
	ldp	x0, x1, [x29, -48]
	bl	_anubis_types__classical__ed25519_sign
	.loc 1 381 16 is_stmt 0 discriminator 1
	strb	w0, [x29, -26]
	.loc 1 388 10 is_stmt 1
	ldrb	w0, [x29, -26]
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 388 7
	cmp	w0, 0
	beq	L105
LBB19:
	.loc 1 390 14
	mov	w0, 1
	str	w0, [x29, -24]
L107:
	.loc 1 390 14 is_stmt 0 discriminator 1
	ldr	w0, [x29, -24]
	cmp	w0, 64
	bgt	L106
	.loc 1 391 44 is_stmt 1
	ldrsw	x0, [x29, -24]
	ldr	x1, [x29, -72]
	add	x0, x1, x0
	strb	wzr, [x0, -1]
	.loc 1 390 14 discriminator 2
	ldr	w0, [x29, -24]
	add	w0, w0, 1
	str	w0, [x29, -24]
	.loc 1 392 18
	b	L107
L106:
LBE19:
LBB20:
	.loc 1 394 14
	mov	w0, 1
	str	w0, [x29, -20]
L109:
	.loc 1 394 14 is_stmt 0 discriminator 1
	ldr	w1, [x29, -20]
	mov	w0, 4627
	cmp	w1, w0
	bgt	L108
	.loc 1 395 43 is_stmt 1
	ldrsw	x0, [x29, -20]
	ldr	x1, [x29, -72]
	add	x0, x1, x0
	strb	wzr, [x0, 63]
	.loc 1 394 14 discriminator 2
	ldr	w0, [x29, -20]
	add	w0, w0, 1
	str	w0, [x29, -20]
	.loc 1 396 18
	b	L109
L108:
LBE20:
	.loc 1 397 18
	strb	wzr, [x29, -27]
	.loc 1 398 10
	b	L110
L105:
	.loc 1 402 7
	ldr	x0, [x29, -72]
	add	x0, x0, 64
	mov	x3, x0
	ldr	x2, [x29, -64]
	ldp	x0, x1, [x29, -48]
	bl	_anubis_types__pqc__ml_dsa_sign
	.loc 1 402 7 is_stmt 0 discriminator 1
	strb	w0, [x29, -25]
	.loc 1 409 10 is_stmt 1
	ldrb	w0, [x29, -25]
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 409 7
	cmp	w0, 0
	beq	L111
LBB21:
	.loc 1 411 14
	mov	w0, 1
	str	w0, [x29, -16]
L113:
	.loc 1 411 14 is_stmt 0 discriminator 1
	ldr	w0, [x29, -16]
	cmp	w0, 64
	bgt	L112
	.loc 1 412 44 is_stmt 1
	ldrsw	x0, [x29, -16]
	ldr	x1, [x29, -72]
	add	x0, x1, x0
	strb	wzr, [x0, -1]
	.loc 1 411 14 discriminator 2
	ldr	w0, [x29, -16]
	add	w0, w0, 1
	str	w0, [x29, -16]
	.loc 1 413 18
	b	L113
L112:
LBE21:
LBB22:
	.loc 1 414 14
	mov	w0, 1
	str	w0, [x29, -12]
L115:
	.loc 1 414 14 is_stmt 0 discriminator 1
	ldr	w1, [x29, -12]
	mov	w0, 4627
	cmp	w1, w0
	bgt	L114
	.loc 1 415 43 is_stmt 1
	ldrsw	x0, [x29, -12]
	ldr	x1, [x29, -72]
	add	x0, x1, x0
	strb	wzr, [x0, 63]
	.loc 1 414 14 discriminator 2
	ldr	w0, [x29, -12]
	add	w0, w0, 1
	str	w0, [x29, -12]
	.loc 1 416 18
	b	L115
L114:
LBE22:
	.loc 1 417 18
	strb	wzr, [x29, -27]
	.loc 1 418 10
	b	L110
L111:
	.loc 1 422 15
	mov	w0, 1
	strb	w0, [x29, -27]
	.loc 1 423 8
	nop
L110:
LBE18:
	ldrb	w0, [x29, -27]
	.loc 1 423 8 is_stmt 0 discriminator 2
	mov	w1, w0
	.loc 1 423 8
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x3, [x29, -8]
	ldr	x2, [x0]
	subs	x3, x3, x2
	mov	x2, 0
	beq	L117
	bl	___stack_chk_fail
L117:
	mov	w0, w1
	ldp	x29, x30, [sp, 80]
	add	sp, sp, 96
LCFI53:
	ret
LFE17:
	.align	2
	.globl _anubis_types__pqc__hybrid_verify
_anubis_types__pqc__hybrid_verify:
LFB18:
	.loc 1 425 4 is_stmt 1
	sub	x10, sp, #16384
	str	xzr, [x10, 4016]
	stp	x29, x30, [sp, -80]!
LCFI54:
	mov	x29, sp
LCFI55:
	stp	x0, x1, [x29, 48]
	str	x2, [x29, 40]
	str	x3, [x29, 32]
	str	x4, [x29, 24]
	.loc 1 425 4
	ldr	x0, [x29, 56]
	ldr	w0, [x0]
	ldr	x1, [x29, 56]
	ldr	w1, [x1, 4]
LBB23:
	cmp	w1, w0
	.loc 1 425 4 is_stmt 0 discriminator 4
	cmp	w1, w0
	blt	L122
	.loc 1 425 4 discriminator 5
	sxtw	x3, w1
	sxtw	x2, w0
	sub	x2, x3, x2
	add	x2, x2, 1
	mov	x6, x2
	mov	x7, 0
	lsr	x2, x6, 61
	lsl	x9, x7, 3
	orr	x9, x2, x9
	lsl	x8, x6, 3
L122:
	.loc 1 425 4 discriminator 8
	cmp	w1, w0
	.loc 1 436 33 is_stmt 1
	ldr	x0, [x29, 40]
	ldr	x3, [x29, 32]
	mov	x2, x0
	ldp	x0, x1, [x29, 48]
	bl	_anubis_types__classical__ed25519_verify
	.loc 1 436 33 is_stmt 0 discriminator 1
	strb	w0, [x29, 78]
	.loc 1 443 10 is_stmt 1
	ldrb	w0, [x29, 78]
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 443 7
	cmp	w0, 0
	beq	L125
	.loc 1 444 10
	mov	w0, 0
	b	L126
L125:
	.loc 1 448 23
	ldr	x0, [x29, 40]
	add	x0, x0, 64
	ldr	x3, [x29, 24]
	mov	x2, x0
	ldp	x0, x1, [x29, 48]
	bl	_anubis_types__pqc__ml_dsa_verify
	.loc 1 448 23 is_stmt 0 discriminator 1
	strb	w0, [x29, 79]
	.loc 1 455 7 is_stmt 1
	ldrb	w1, [x29, 78]
	ldrb	w0, [x29, 79]
	cmp	w1, 0
	ccmp	w0, 0, 4, ne
	cset	w0, ne
	and	w0, w0, 255
L126:
LBE23:
	.loc 1 456 8
	ldp	x29, x30, [sp], 80
LCFI56:
	ret
LFE18:
	.globl _anubis_types__pqc_E
	.data
	.align	1
_anubis_types__pqc_E:
	.space 2
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
	.uleb128 0x40
	.byte	0x4
	.set L$set$20,LCFI7-LCFI6
	.long L$set$20
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$21,LCFI8-LCFI7
	.long L$set$21
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x4
	.set L$set$22,LCFI9-LCFI8
	.long L$set$22
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE6:
LSFDE8:
	.set L$set$23,LEFDE8-LASFDE8
	.long L$set$23
LASFDE8:
	.set L$set$24,Lframe0-Lsection__debug_frame
	.long L$set$24
	.quad	LFB6
	.set L$set$25,LFE6-LFB6
	.quad L$set$25
	.byte	0x4
	.set L$set$26,LCFI10-LFB6
	.long L$set$26
	.byte	0xe
	.uleb128 0x50
	.byte	0x4
	.set L$set$27,LCFI11-LCFI10
	.long L$set$27
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$28,LCFI12-LCFI11
	.long L$set$28
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x4
	.set L$set$29,LCFI13-LCFI12
	.long L$set$29
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
	.uleb128 0x50
	.byte	0x4
	.set L$set$34,LCFI15-LCFI14
	.long L$set$34
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$35,LCFI16-LCFI15
	.long L$set$35
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x4
	.set L$set$36,LCFI17-LCFI16
	.long L$set$36
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE10:
LSFDE12:
	.set L$set$37,LEFDE12-LASFDE12
	.long L$set$37
LASFDE12:
	.set L$set$38,Lframe0-Lsection__debug_frame
	.long L$set$38
	.quad	LFB8
	.set L$set$39,LFE8-LFB8
	.quad L$set$39
	.byte	0x4
	.set L$set$40,LCFI18-LFB8
	.long L$set$40
	.byte	0xe
	.uleb128 0x40
	.byte	0x4
	.set L$set$41,LCFI19-LCFI18
	.long L$set$41
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$42,LCFI20-LCFI19
	.long L$set$42
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x4
	.set L$set$43,LCFI21-LCFI20
	.long L$set$43
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE12:
LSFDE14:
	.set L$set$44,LEFDE14-LASFDE14
	.long L$set$44
LASFDE14:
	.set L$set$45,Lframe0-Lsection__debug_frame
	.long L$set$45
	.quad	LFB9
	.set L$set$46,LFE9-LFB9
	.quad L$set$46
	.byte	0x4
	.set L$set$47,LCFI22-LFB9
	.long L$set$47
	.byte	0xe
	.uleb128 0x50
	.byte	0x4
	.set L$set$48,LCFI23-LCFI22
	.long L$set$48
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$49,LCFI24-LCFI23
	.long L$set$49
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x4
	.set L$set$50,LCFI25-LCFI24
	.long L$set$50
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
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
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$55,LCFI27-LCFI26
	.long L$set$55
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$56,LCFI28-LCFI27
	.long L$set$56
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE16:
LSFDE18:
	.set L$set$57,LEFDE18-LASFDE18
	.long L$set$57
LASFDE18:
	.set L$set$58,Lframe0-Lsection__debug_frame
	.long L$set$58
	.quad	LFB11
	.set L$set$59,LFE11-LFB11
	.quad L$set$59
	.byte	0x4
	.set L$set$60,LCFI29-LFB11
	.long L$set$60
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$61,LCFI30-LCFI29
	.long L$set$61
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$62,LCFI31-LCFI30
	.long L$set$62
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE18:
LSFDE20:
	.set L$set$63,LEFDE20-LASFDE20
	.long L$set$63
LASFDE20:
	.set L$set$64,Lframe0-Lsection__debug_frame
	.long L$set$64
	.quad	LFB12
	.set L$set$65,LFE12-LFB12
	.quad L$set$65
	.byte	0x4
	.set L$set$66,LCFI32-LFB12
	.long L$set$66
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$67,LCFI33-LCFI32
	.long L$set$67
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$68,LCFI34-LCFI33
	.long L$set$68
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
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
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$73,LCFI36-LCFI35
	.long L$set$73
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$74,LCFI37-LCFI36
	.long L$set$74
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE22:
LSFDE24:
	.set L$set$75,LEFDE24-LASFDE24
	.long L$set$75
LASFDE24:
	.set L$set$76,Lframe0-Lsection__debug_frame
	.long L$set$76
	.quad	LFB14
	.set L$set$77,LFE14-LFB14
	.quad L$set$77
	.byte	0x4
	.set L$set$78,LCFI38-LFB14
	.long L$set$78
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$79,LCFI39-LCFI38
	.long L$set$79
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$80,LCFI40-LCFI39
	.long L$set$80
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE24:
LSFDE26:
	.set L$set$81,LEFDE26-LASFDE26
	.long L$set$81
LASFDE26:
	.set L$set$82,Lframe0-Lsection__debug_frame
	.long L$set$82
	.quad	LFB15
	.set L$set$83,LFE15-LFB15
	.quad L$set$83
	.byte	0x4
	.set L$set$84,LCFI41-LFB15
	.long L$set$84
	.byte	0xe
	.uleb128 0x150
	.byte	0x4
	.set L$set$85,LCFI42-LCFI41
	.long L$set$85
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$86,LCFI43-LCFI42
	.long L$set$86
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x40
	.byte	0x4
	.set L$set$87,LCFI44-LCFI43
	.long L$set$87
	.byte	0x94
	.uleb128 0x6
	.byte	0x95
	.uleb128 0x5
	.byte	0x96
	.uleb128 0x4
	.byte	0x97
	.uleb128 0x3
	.byte	0x98
	.uleb128 0x2
	.byte	0x99
	.uleb128 0x1
	.byte	0x4
	.set L$set$88,LCFI45-LCFI44
	.long L$set$88
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
LEFDE26:
LSFDE28:
	.set L$set$89,LEFDE28-LASFDE28
	.long L$set$89
LASFDE28:
	.set L$set$90,Lframe0-Lsection__debug_frame
	.long L$set$90
	.quad	LFB16
	.set L$set$91,LFE16-LFB16
	.quad L$set$91
	.byte	0x4
	.set L$set$92,LCFI46-LFB16
	.long L$set$92
	.byte	0xe
	.uleb128 0x70
	.byte	0x4
	.set L$set$93,LCFI47-LCFI46
	.long L$set$93
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$94,LCFI48-LCFI47
	.long L$set$94
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x4
	.set L$set$95,LCFI49-LCFI48
	.long L$set$95
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE28:
LSFDE30:
	.set L$set$96,LEFDE30-LASFDE30
	.long L$set$96
LASFDE30:
	.set L$set$97,Lframe0-Lsection__debug_frame
	.long L$set$97
	.quad	LFB17
	.set L$set$98,LFE17-LFB17
	.quad L$set$98
	.byte	0x4
	.set L$set$99,LCFI50-LFB17
	.long L$set$99
	.byte	0xe
	.uleb128 0x60
	.byte	0x4
	.set L$set$100,LCFI51-LCFI50
	.long L$set$100
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$101,LCFI52-LCFI51
	.long L$set$101
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x4
	.set L$set$102,LCFI53-LCFI52
	.long L$set$102
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE30:
LSFDE32:
	.set L$set$103,LEFDE32-LASFDE32
	.long L$set$103
LASFDE32:
	.set L$set$104,Lframe0-Lsection__debug_frame
	.long L$set$104
	.quad	LFB18
	.set L$set$105,LFE18-LFB18
	.quad L$set$105
	.byte	0x4
	.set L$set$106,LCFI54-LFB18
	.long L$set$106
	.byte	0xe
	.uleb128 0x50
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$107,LCFI55-LCFI54
	.long L$set$107
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$108,LCFI56-LCFI55
	.long L$set$108
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE32:
	.section __TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
EH_frame1:
	.set L$set$109,LECIE1-LSCIE1
	.long L$set$109
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
LSFDE35:
	.set L$set$110,LEFDE35-LASFDE35
	.long L$set$110
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB2-.
	.set L$set$111,LFE2-LFB2
	.quad L$set$111
	.uleb128 0
	.byte	0x4
	.set L$set$112,LCFI0-LFB2
	.long L$set$112
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$113,LCFI1-LCFI0
	.long L$set$113
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE35:
LSFDE37:
	.set L$set$114,LEFDE37-LASFDE37
	.long L$set$114
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB3-.
	.set L$set$115,LFE3-LFB3
	.quad L$set$115
	.uleb128 0
	.byte	0x4
	.set L$set$116,LCFI2-LFB3
	.long L$set$116
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$117,LCFI3-LCFI2
	.long L$set$117
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE37:
LSFDE39:
	.set L$set$118,LEFDE39-LASFDE39
	.long L$set$118
LASFDE39:
	.long	LASFDE39-EH_frame1
	.quad	LFB4-.
	.set L$set$119,LFE4-LFB4
	.quad L$set$119
	.uleb128 0
	.byte	0x4
	.set L$set$120,LCFI4-LFB4
	.long L$set$120
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$121,LCFI5-LCFI4
	.long L$set$121
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE39:
LSFDE41:
	.set L$set$122,LEFDE41-LASFDE41
	.long L$set$122
LASFDE41:
	.long	LASFDE41-EH_frame1
	.quad	LFB5-.
	.set L$set$123,LFE5-LFB5
	.quad L$set$123
	.uleb128 0
	.byte	0x4
	.set L$set$124,LCFI6-LFB5
	.long L$set$124
	.byte	0xe
	.uleb128 0x40
	.byte	0x4
	.set L$set$125,LCFI7-LCFI6
	.long L$set$125
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$126,LCFI8-LCFI7
	.long L$set$126
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x4
	.set L$set$127,LCFI9-LCFI8
	.long L$set$127
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE41:
LSFDE43:
	.set L$set$128,LEFDE43-LASFDE43
	.long L$set$128
LASFDE43:
	.long	LASFDE43-EH_frame1
	.quad	LFB6-.
	.set L$set$129,LFE6-LFB6
	.quad L$set$129
	.uleb128 0
	.byte	0x4
	.set L$set$130,LCFI10-LFB6
	.long L$set$130
	.byte	0xe
	.uleb128 0x50
	.byte	0x4
	.set L$set$131,LCFI11-LCFI10
	.long L$set$131
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$132,LCFI12-LCFI11
	.long L$set$132
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x4
	.set L$set$133,LCFI13-LCFI12
	.long L$set$133
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE43:
LSFDE45:
	.set L$set$134,LEFDE45-LASFDE45
	.long L$set$134
LASFDE45:
	.long	LASFDE45-EH_frame1
	.quad	LFB7-.
	.set L$set$135,LFE7-LFB7
	.quad L$set$135
	.uleb128 0
	.byte	0x4
	.set L$set$136,LCFI14-LFB7
	.long L$set$136
	.byte	0xe
	.uleb128 0x50
	.byte	0x4
	.set L$set$137,LCFI15-LCFI14
	.long L$set$137
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$138,LCFI16-LCFI15
	.long L$set$138
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x4
	.set L$set$139,LCFI17-LCFI16
	.long L$set$139
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE45:
LSFDE47:
	.set L$set$140,LEFDE47-LASFDE47
	.long L$set$140
LASFDE47:
	.long	LASFDE47-EH_frame1
	.quad	LFB8-.
	.set L$set$141,LFE8-LFB8
	.quad L$set$141
	.uleb128 0
	.byte	0x4
	.set L$set$142,LCFI18-LFB8
	.long L$set$142
	.byte	0xe
	.uleb128 0x40
	.byte	0x4
	.set L$set$143,LCFI19-LCFI18
	.long L$set$143
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$144,LCFI20-LCFI19
	.long L$set$144
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x4
	.set L$set$145,LCFI21-LCFI20
	.long L$set$145
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE47:
LSFDE49:
	.set L$set$146,LEFDE49-LASFDE49
	.long L$set$146
LASFDE49:
	.long	LASFDE49-EH_frame1
	.quad	LFB9-.
	.set L$set$147,LFE9-LFB9
	.quad L$set$147
	.uleb128 0
	.byte	0x4
	.set L$set$148,LCFI22-LFB9
	.long L$set$148
	.byte	0xe
	.uleb128 0x50
	.byte	0x4
	.set L$set$149,LCFI23-LCFI22
	.long L$set$149
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$150,LCFI24-LCFI23
	.long L$set$150
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x4
	.set L$set$151,LCFI25-LCFI24
	.long L$set$151
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE49:
LSFDE51:
	.set L$set$152,LEFDE51-LASFDE51
	.long L$set$152
LASFDE51:
	.long	LASFDE51-EH_frame1
	.quad	LFB10-.
	.set L$set$153,LFE10-LFB10
	.quad L$set$153
	.uleb128 0
	.byte	0x4
	.set L$set$154,LCFI26-LFB10
	.long L$set$154
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$155,LCFI27-LCFI26
	.long L$set$155
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$156,LCFI28-LCFI27
	.long L$set$156
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE51:
LSFDE53:
	.set L$set$157,LEFDE53-LASFDE53
	.long L$set$157
LASFDE53:
	.long	LASFDE53-EH_frame1
	.quad	LFB11-.
	.set L$set$158,LFE11-LFB11
	.quad L$set$158
	.uleb128 0
	.byte	0x4
	.set L$set$159,LCFI29-LFB11
	.long L$set$159
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$160,LCFI30-LCFI29
	.long L$set$160
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$161,LCFI31-LCFI30
	.long L$set$161
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE53:
LSFDE55:
	.set L$set$162,LEFDE55-LASFDE55
	.long L$set$162
LASFDE55:
	.long	LASFDE55-EH_frame1
	.quad	LFB12-.
	.set L$set$163,LFE12-LFB12
	.quad L$set$163
	.uleb128 0
	.byte	0x4
	.set L$set$164,LCFI32-LFB12
	.long L$set$164
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$165,LCFI33-LCFI32
	.long L$set$165
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$166,LCFI34-LCFI33
	.long L$set$166
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE55:
LSFDE57:
	.set L$set$167,LEFDE57-LASFDE57
	.long L$set$167
LASFDE57:
	.long	LASFDE57-EH_frame1
	.quad	LFB13-.
	.set L$set$168,LFE13-LFB13
	.quad L$set$168
	.uleb128 0
	.byte	0x4
	.set L$set$169,LCFI35-LFB13
	.long L$set$169
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$170,LCFI36-LCFI35
	.long L$set$170
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$171,LCFI37-LCFI36
	.long L$set$171
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE57:
LSFDE59:
	.set L$set$172,LEFDE59-LASFDE59
	.long L$set$172
LASFDE59:
	.long	LASFDE59-EH_frame1
	.quad	LFB14-.
	.set L$set$173,LFE14-LFB14
	.quad L$set$173
	.uleb128 0
	.byte	0x4
	.set L$set$174,LCFI38-LFB14
	.long L$set$174
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$175,LCFI39-LCFI38
	.long L$set$175
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$176,LCFI40-LCFI39
	.long L$set$176
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE59:
LSFDE61:
	.set L$set$177,LEFDE61-LASFDE61
	.long L$set$177
LASFDE61:
	.long	LASFDE61-EH_frame1
	.quad	LFB15-.
	.set L$set$178,LFE15-LFB15
	.quad L$set$178
	.uleb128 0
	.byte	0x4
	.set L$set$179,LCFI41-LFB15
	.long L$set$179
	.byte	0xe
	.uleb128 0x150
	.byte	0x4
	.set L$set$180,LCFI42-LCFI41
	.long L$set$180
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$181,LCFI43-LCFI42
	.long L$set$181
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x40
	.byte	0x4
	.set L$set$182,LCFI44-LCFI43
	.long L$set$182
	.byte	0x94
	.uleb128 0x6
	.byte	0x95
	.uleb128 0x5
	.byte	0x96
	.uleb128 0x4
	.byte	0x97
	.uleb128 0x3
	.byte	0x98
	.uleb128 0x2
	.byte	0x99
	.uleb128 0x1
	.byte	0x4
	.set L$set$183,LCFI45-LCFI44
	.long L$set$183
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
LEFDE61:
LSFDE63:
	.set L$set$184,LEFDE63-LASFDE63
	.long L$set$184
LASFDE63:
	.long	LASFDE63-EH_frame1
	.quad	LFB16-.
	.set L$set$185,LFE16-LFB16
	.quad L$set$185
	.uleb128 0
	.byte	0x4
	.set L$set$186,LCFI46-LFB16
	.long L$set$186
	.byte	0xe
	.uleb128 0x70
	.byte	0x4
	.set L$set$187,LCFI47-LCFI46
	.long L$set$187
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$188,LCFI48-LCFI47
	.long L$set$188
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x4
	.set L$set$189,LCFI49-LCFI48
	.long L$set$189
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE63:
LSFDE65:
	.set L$set$190,LEFDE65-LASFDE65
	.long L$set$190
LASFDE65:
	.long	LASFDE65-EH_frame1
	.quad	LFB17-.
	.set L$set$191,LFE17-LFB17
	.quad L$set$191
	.uleb128 0
	.byte	0x4
	.set L$set$192,LCFI50-LFB17
	.long L$set$192
	.byte	0xe
	.uleb128 0x60
	.byte	0x4
	.set L$set$193,LCFI51-LCFI50
	.long L$set$193
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$194,LCFI52-LCFI51
	.long L$set$194
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x4
	.set L$set$195,LCFI53-LCFI52
	.long L$set$195
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE65:
LSFDE67:
	.set L$set$196,LEFDE67-LASFDE67
	.long L$set$196
LASFDE67:
	.long	LASFDE67-EH_frame1
	.quad	LFB18-.
	.set L$set$197,LFE18-LFB18
	.quad L$set$197
	.uleb128 0
	.byte	0x4
	.set L$set$198,LCFI54-LFB18
	.long L$set$198
	.byte	0xe
	.uleb128 0x50
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$199,LCFI55-LCFI54
	.long L$set$199
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$200,LCFI56-LCFI55
	.long L$set$200
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE67:
	.text
Letext0:
	.file 2 "/Users/sicarii/.local/share/alire/releases/gnatprove_14.1.1_91818ed8/libexec/spark/lib/gcc/aarch64-apple-darwin23.5.0/14.1.0/adainclude/interfac.ads"
	.file 3 "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_types.ads"
	.file 4 "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_types-pqc.ads"
	.section __DWARF,__debug_info,regular,debug
Lsection__debug_info:
Ldebug_info0:
	.long	0x1878
	.short	0x4
	.set L$set$201,Ldebug_abbrev0-Lsection__debug_abbrev
	.long L$set$201
	.byte	0x8
	.uleb128 0x1
	.ascii "GNU Ada 14.1.0 -O0 -gnatA -g -gnat2012 -gnatwa -gnatwe -fstack-check=specific -fstack-protector-strong -gnatR2js -gnatws -gnatec=/private/var/folders/bg/pt9l6y1j47q642kp3z5blrmh0000gn/T/GPR.71672/GNAT-TEMP-000003.TMP -gnatem=/private/var/folders/bg/pt9l6y1j47q642kp3z5blrmh0000gn/T/GPR.71672/GNAT-TEMP-000004.TMP -mmacosx-version-min=16.0.0 -mcpu=apple-m1 -mlittle-endian -mabi=lp64 -fPIC\0"
	.byte	0xd
	.ascii "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_types-pqc.adb\0"
	.ascii "/Users/sicarii/Desktop/anubis-spark/obj/gnatprove/data_representation\0"
	.quad	Ltext0
	.set L$set$202,Letext0-Ltext0
	.quad L$set$202
	.set L$set$203,Ldebug_line0-Lsection__debug_line
	.long L$set$203
	.uleb128 0x2
	.byte	0x1
	.byte	0x7
	.ascii "interfaces__unsigned_8\0"
	.uleb128 0x3
	.byte	0
	.byte	0xff
	.ascii "anubis_types__byte\0"
	.long	0x264
	.uleb128 0x4
	.byte	0x1
	.byte	0x7
	.ascii "anubis_types__TbyteB\0"
	.uleb128 0x5
	.ascii "anubis_types__byte_array\0"
	.byte	0x10
	.byte	0x3
	.byte	0x12
	.byte	0x9
	.long	0x2c8
	.uleb128 0x6
	.ascii "P_ARRAY\0"
	.byte	0x2
	.byte	0x46
	.byte	0x1d
	.long	0x2af
	.byte	0
	.uleb128 0x7
	.byte	0x8
	.long	0x2dc
	.uleb128 0x6
	.ascii "P_BOUNDS\0"
	.byte	0x2
	.byte	0x46
	.byte	0x1d
	.long	0x37b
	.byte	0x8
	.byte	0
	.uleb128 0x8
	.long	0x27c
	.uleb128 0x8
	.long	0x27c
	.uleb128 0x8
	.long	0x27c
	.uleb128 0x8
	.long	0x27c
	.uleb128 0x9
	.ascii "anubis_types__byte_array___XUA\0"
	.long	0x24a
	.long	0x31a
	.uleb128 0xa
	.long	0x31a
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
	.uleb128 0x2
	.byte	0x4
	.byte	0x5
	.ascii "integer\0"
	.uleb128 0x5
	.ascii "anubis_types__byte_array___XUB\0"
	.byte	0x8
	.byte	0x2
	.byte	0x46
	.byte	0x1d
	.long	0x37b
	.uleb128 0x6
	.ascii "LB0\0"
	.byte	0x3
	.byte	0x12
	.byte	0x9
	.long	0x35a
	.byte	0
	.uleb128 0xb
	.sleb128 0
	.sleb128 2147483647
	.ascii "natural\0"
	.long	0x31a
	.uleb128 0x6
	.ascii "UB0\0"
	.byte	0x3
	.byte	0x12
	.byte	0x9
	.long	0x35a
	.byte	0x4
	.byte	0
	.uleb128 0x7
	.byte	0x8
	.long	0x325
	.uleb128 0x9
	.ascii "anubis_types__ml_kem_public_key__T33s\0"
	.long	0x24a
	.long	0x3b8
	.uleb128 0xc
	.long	0x31a
	.sleb128 1568
	.byte	0
	.uleb128 0xd
	.ascii "anubis_types__ml_kem_public_key\0"
	.short	0x620
	.byte	0x3
	.byte	0xb8
	.byte	0x9
	.long	0x3f0
	.uleb128 0xe
	.set L$set$204,LASF0-Lsection__debug_str
	.long L$set$204
	.byte	0x3
	.byte	0xb9
	.byte	0x7
	.long	0x381
	.byte	0
	.byte	0
	.uleb128 0x9
	.ascii "anubis_types__ml_kem_secret_key__T35s\0"
	.long	0x24a
	.long	0x427
	.uleb128 0xc
	.long	0x31a
	.sleb128 3168
	.byte	0
	.uleb128 0xd
	.ascii "anubis_types__ml_kem_secret_key\0"
	.short	0xc61
	.byte	0x3
	.byte	0xbc
	.byte	0x9
	.long	0x46d
	.uleb128 0xe
	.set L$set$205,LASF0-Lsection__debug_str
	.long L$set$205
	.byte	0x3
	.byte	0xbd
	.byte	0x7
	.long	0x3f0
	.byte	0
	.uleb128 0xf
	.set L$set$206,LASF1-Lsection__debug_str
	.long L$set$206
	.byte	0x3
	.byte	0xbe
	.byte	0x7
	.long	0x46d
	.short	0xc60
	.byte	0
	.uleb128 0x2
	.byte	0x1
	.byte	0x2
	.ascii "boolean\0"
	.uleb128 0x9
	.ascii "anubis_types__ml_kem_ciphertext__T37s\0"
	.long	0x24a
	.long	0x4af
	.uleb128 0xc
	.long	0x31a
	.sleb128 1568
	.byte	0
	.uleb128 0xd
	.ascii "anubis_types__ml_kem_ciphertext\0"
	.short	0x620
	.byte	0x3
	.byte	0xc1
	.byte	0x9
	.long	0x4e7
	.uleb128 0xe
	.set L$set$207,LASF0-Lsection__debug_str
	.long L$set$207
	.byte	0x3
	.byte	0xc2
	.byte	0x7
	.long	0x478
	.byte	0
	.byte	0
	.uleb128 0x9
	.ascii "anubis_types__ml_kem_shared_secret__T39s\0"
	.long	0x24a
	.long	0x520
	.uleb128 0xc
	.long	0x31a
	.sleb128 32
	.byte	0
	.uleb128 0x10
	.ascii "anubis_types__ml_kem_shared_secret\0"
	.byte	0x21
	.byte	0x3
	.byte	0xc5
	.byte	0x9
	.long	0x567
	.uleb128 0xe
	.set L$set$208,LASF0-Lsection__debug_str
	.long L$set$208
	.byte	0x3
	.byte	0xc6
	.byte	0x7
	.long	0x4e7
	.byte	0
	.uleb128 0xe
	.set L$set$209,LASF1-Lsection__debug_str
	.long L$set$209
	.byte	0x3
	.byte	0xc7
	.byte	0x7
	.long	0x46d
	.byte	0x20
	.byte	0
	.uleb128 0x9
	.ascii "anubis_types__ml_dsa_public_key__T41s\0"
	.long	0x24a
	.long	0x59e
	.uleb128 0xc
	.long	0x31a
	.sleb128 2592
	.byte	0
	.uleb128 0xd
	.ascii "anubis_types__ml_dsa_public_key\0"
	.short	0xa20
	.byte	0x3
	.byte	0xca
	.byte	0x9
	.long	0x5d6
	.uleb128 0xe
	.set L$set$210,LASF0-Lsection__debug_str
	.long L$set$210
	.byte	0x3
	.byte	0xcb
	.byte	0x7
	.long	0x567
	.byte	0
	.byte	0
	.uleb128 0x9
	.ascii "anubis_types__ml_dsa_secret_key__T43s\0"
	.long	0x24a
	.long	0x60d
	.uleb128 0xc
	.long	0x31a
	.sleb128 4896
	.byte	0
	.uleb128 0xd
	.ascii "anubis_types__ml_dsa_secret_key\0"
	.short	0x1321
	.byte	0x3
	.byte	0xce
	.byte	0x9
	.long	0x653
	.uleb128 0xe
	.set L$set$211,LASF0-Lsection__debug_str
	.long L$set$211
	.byte	0x3
	.byte	0xcf
	.byte	0x7
	.long	0x5d6
	.byte	0
	.uleb128 0xf
	.set L$set$212,LASF1-Lsection__debug_str
	.long L$set$212
	.byte	0x3
	.byte	0xd0
	.byte	0x7
	.long	0x46d
	.short	0x1320
	.byte	0
	.uleb128 0x9
	.ascii "anubis_types__ml_dsa_signature__T45s\0"
	.long	0x24a
	.long	0x689
	.uleb128 0xc
	.long	0x31a
	.sleb128 4627
	.byte	0
	.uleb128 0xd
	.ascii "anubis_types__ml_dsa_signature\0"
	.short	0x1213
	.byte	0x3
	.byte	0xd3
	.byte	0x9
	.long	0x6c0
	.uleb128 0xe
	.set L$set$213,LASF0-Lsection__debug_str
	.long L$set$213
	.byte	0x3
	.byte	0xd4
	.byte	0x7
	.long	0x653
	.byte	0
	.byte	0
	.uleb128 0x9
	.ascii "anubis_types__ed25519_signature__T25s\0"
	.long	0x24a
	.long	0x6f7
	.uleb128 0xc
	.long	0x31a
	.sleb128 64
	.byte	0
	.uleb128 0x10
	.ascii "anubis_types__ed25519_signature\0"
	.byte	0x40
	.byte	0x3
	.byte	0xa7
	.byte	0x9
	.long	0x72e
	.uleb128 0xe
	.set L$set$214,LASF0-Lsection__debug_str
	.long L$set$214
	.byte	0x3
	.byte	0xa8
	.byte	0x7
	.long	0x6c0
	.byte	0
	.byte	0
	.uleb128 0xd
	.ascii "anubis_types__pqc__hybrid_signature\0"
	.short	0x1253
	.byte	0x4
	.byte	0xc6
	.byte	0x9
	.long	0x786
	.uleb128 0x6
	.ascii "ed25519_sig\0"
	.byte	0x4
	.byte	0xc7
	.byte	0x7
	.long	0x6f7
	.byte	0
	.uleb128 0x6
	.ascii "ml_dsa_sig\0"
	.byte	0x4
	.byte	0xc8
	.byte	0x7
	.long	0x689
	.byte	0x40
	.byte	0
	.uleb128 0x9
	.ascii "anubis_types__pqc__hybrid_shared_secret__T18s\0"
	.long	0x24a
	.long	0x7c4
	.uleb128 0xc
	.long	0x31a
	.sleb128 32
	.byte	0
	.uleb128 0x9
	.ascii "anubis_types__pqc__hybrid_shared_secret__T20s\0"
	.long	0x24a
	.long	0x802
	.uleb128 0xc
	.long	0x31a
	.sleb128 32
	.byte	0
	.uleb128 0x10
	.ascii "anubis_types__pqc__hybrid_shared_secret\0"
	.byte	0x41
	.byte	0x4
	.byte	0xbe
	.byte	0x9
	.long	0x86e
	.uleb128 0x6
	.ascii "classical_secret\0"
	.byte	0x4
	.byte	0xbf
	.byte	0x7
	.long	0x786
	.byte	0
	.uleb128 0x6
	.ascii "pq_secret\0"
	.byte	0x4
	.byte	0xc0
	.byte	0x7
	.long	0x7c4
	.byte	0x20
	.uleb128 0xe
	.set L$set$215,LASF1-Lsection__debug_str
	.long L$set$215
	.byte	0x4
	.byte	0xc1
	.byte	0x7
	.long	0x46d
	.byte	0x40
	.byte	0
	.uleb128 0x9
	.ascii "anubis_types__x25519_public_key__T15s\0"
	.long	0x24a
	.long	0x8a4
	.uleb128 0xc
	.long	0x31a
	.sleb128 32
	.byte	0
	.uleb128 0x10
	.ascii "anubis_types__x25519_public_key\0"
	.byte	0x20
	.byte	0x3
	.byte	0x90
	.byte	0x9
	.long	0x8db
	.uleb128 0xe
	.set L$set$216,LASF0-Lsection__debug_str
	.long L$set$216
	.byte	0x3
	.byte	0x91
	.byte	0x7
	.long	0x86e
	.byte	0
	.byte	0
	.uleb128 0x9
	.ascii "anubis_types__x25519_secret_key__T17s\0"
	.long	0x24a
	.long	0x911
	.uleb128 0xc
	.long	0x31a
	.sleb128 32
	.byte	0
	.uleb128 0x10
	.ascii "anubis_types__x25519_secret_key\0"
	.byte	0x21
	.byte	0x3
	.byte	0x94
	.byte	0x9
	.long	0x955
	.uleb128 0xe
	.set L$set$217,LASF0-Lsection__debug_str
	.long L$set$217
	.byte	0x3
	.byte	0x95
	.byte	0x7
	.long	0x8db
	.byte	0
	.uleb128 0xe
	.set L$set$218,LASF1-Lsection__debug_str
	.long L$set$218
	.byte	0x3
	.byte	0x96
	.byte	0x7
	.long	0x46d
	.byte	0x20
	.byte	0
	.uleb128 0x9
	.ascii "anubis_types__xchacha20_key__T27s\0"
	.long	0x24a
	.long	0x987
	.uleb128 0xc
	.long	0x31a
	.sleb128 32
	.byte	0
	.uleb128 0x10
	.ascii "anubis_types__xchacha20_key\0"
	.byte	0x21
	.byte	0x3
	.byte	0xab
	.byte	0x9
	.long	0x9c7
	.uleb128 0xe
	.set L$set$219,LASF0-Lsection__debug_str
	.long L$set$219
	.byte	0x3
	.byte	0xac
	.byte	0x7
	.long	0x955
	.byte	0
	.uleb128 0xe
	.set L$set$220,LASF1-Lsection__debug_str
	.long L$set$220
	.byte	0x3
	.byte	0xad
	.byte	0x7
	.long	0x46d
	.byte	0x20
	.byte	0
	.uleb128 0x9
	.ascii "anubis_types__ed25519_secret_key__T23s\0"
	.long	0x24a
	.long	0x9fe
	.uleb128 0xc
	.long	0x31a
	.sleb128 32
	.byte	0
	.uleb128 0x10
	.ascii "anubis_types__ed25519_secret_key\0"
	.byte	0x21
	.byte	0x3
	.byte	0xa2
	.byte	0x9
	.long	0xa43
	.uleb128 0xe
	.set L$set$221,LASF0-Lsection__debug_str
	.long L$set$221
	.byte	0x3
	.byte	0xa3
	.byte	0x7
	.long	0x9c7
	.byte	0
	.uleb128 0xe
	.set L$set$222,LASF1-Lsection__debug_str
	.long L$set$222
	.byte	0x3
	.byte	0xa4
	.byte	0x7
	.long	0x46d
	.byte	0x20
	.byte	0
	.uleb128 0x9
	.ascii "anubis_types__ed25519_public_key__T21s\0"
	.long	0x24a
	.long	0xa7a
	.uleb128 0xc
	.long	0x31a
	.sleb128 32
	.byte	0
	.uleb128 0x10
	.ascii "anubis_types__ed25519_public_key\0"
	.byte	0x20
	.byte	0x3
	.byte	0x9e
	.byte	0x9
	.long	0xab2
	.uleb128 0xe
	.set L$set$223,LASF0-Lsection__debug_str
	.long L$set$223
	.byte	0x3
	.byte	0x9f
	.byte	0x7
	.long	0xa43
	.byte	0
	.byte	0
	.uleb128 0x11
	.sleb128 -2147483648
	.sleb128 2147483647
	.ascii "oqs_common__oqs_status\0"
	.long	0xad8
	.uleb128 0x4
	.byte	0x4
	.byte	0x5
	.ascii "oqs_common__Toqs_statusB\0"
	.uleb128 0x2
	.byte	0x8
	.byte	0x7
	.ascii "system__address\0"
	.uleb128 0x2
	.byte	0x8
	.byte	0x7
	.ascii "interfaces__c__size_t\0"
	.uleb128 0x11
	.sleb128 -2147483648
	.sleb128 2147483647
	.ascii "interfaces__c__int\0"
	.long	0xb42
	.uleb128 0x4
	.byte	0x4
	.byte	0x5
	.ascii "interfaces__c__TintB\0"
	.uleb128 0x9
	.ascii "anubis_types__x25519_shared_secret__T19s\0"
	.long	0x24a
	.long	0xb93
	.uleb128 0xc
	.long	0x31a
	.sleb128 32
	.byte	0
	.uleb128 0x10
	.ascii "anubis_types__x25519_shared_secret\0"
	.byte	0x21
	.byte	0x3
	.byte	0x99
	.byte	0x9
	.long	0xbda
	.uleb128 0xe
	.set L$set$224,LASF0-Lsection__debug_str
	.long L$set$224
	.byte	0x3
	.byte	0x9a
	.byte	0x7
	.long	0xb5a
	.byte	0
	.uleb128 0xe
	.set L$set$225,LASF1-Lsection__debug_str
	.long L$set$225
	.byte	0x3
	.byte	0x9b
	.byte	0x7
	.long	0x46d
	.byte	0x20
	.byte	0
	.uleb128 0x12
	.ascii "anubis_types__pqc__hybrid_verify\0"
	.byte	0x1
	.short	0x1a9
	.byte	0x4
	.long	0x46d
	.quad	LFB18
	.set L$set$226,LFE18-LFB18
	.quad L$set$226
	.uleb128 0x1
	.byte	0x9c
	.long	0xca9
	.uleb128 0x13
	.set L$set$227,LASF2-Lsection__debug_str
	.long L$set$227
	.byte	0x4
	.byte	0xb2
	.byte	0x7
	.long	0x2d7
	.uleb128 0x2
	.byte	0x91
	.sleb128 -32
	.uleb128 0x13
	.set L$set$228,LASF3-Lsection__debug_str
	.long L$set$228
	.byte	0x4
	.byte	0xb3
	.byte	0x7
	.long	0xca9
	.uleb128 0x2
	.byte	0x91
	.sleb128 -40
	.uleb128 0x14
	.ascii "ed25519_pk\0"
	.byte	0x4
	.byte	0xb4
	.byte	0x7
	.long	0xcaf
	.uleb128 0x2
	.byte	0x91
	.sleb128 -48
	.uleb128 0x14
	.ascii "ml_dsa_pk\0"
	.byte	0x4
	.byte	0xb5
	.byte	0x7
	.long	0xcb5
	.uleb128 0x2
	.byte	0x91
	.sleb128 -56
	.uleb128 0x15
	.quad	LBB23
	.set L$set$229,LBE23-LBB23
	.quad L$set$229
	.uleb128 0x16
	.ascii "ed25519_valid\0"
	.byte	0x1
	.short	0x1b0
	.byte	0x7
	.long	0x46d
	.uleb128 0x2
	.byte	0x91
	.sleb128 -2
	.uleb128 0x16
	.ascii "ml_dsa_valid\0"
	.byte	0x1
	.short	0x1b1
	.byte	0x7
	.long	0x46d
	.uleb128 0x2
	.byte	0x91
	.sleb128 -1
	.byte	0
	.byte	0
	.uleb128 0x17
	.byte	0x8
	.long	0x72e
	.uleb128 0x17
	.byte	0x8
	.long	0xa7a
	.uleb128 0x17
	.byte	0x8
	.long	0x59e
	.uleb128 0x12
	.ascii "anubis_types__pqc__hybrid_sign\0"
	.byte	0x1
	.short	0x172
	.byte	0x4
	.long	0x46d
	.quad	LFB17
	.set L$set$230,LFE17-LFB17
	.quad L$set$230
	.uleb128 0x1
	.byte	0x9c
	.long	0xe2a
	.uleb128 0x13
	.set L$set$231,LASF2-Lsection__debug_str
	.long L$set$231
	.byte	0x4
	.byte	0xa3
	.byte	0x7
	.long	0x2d2
	.uleb128 0x2
	.byte	0x91
	.sleb128 -64
	.uleb128 0x14
	.ascii "ed25519_sk\0"
	.byte	0x4
	.byte	0xa4
	.byte	0x7
	.long	0xe2a
	.uleb128 0x3
	.byte	0x91
	.sleb128 -72
	.uleb128 0x14
	.ascii "ml_dsa_sk\0"
	.byte	0x4
	.byte	0xa5
	.byte	0x7
	.long	0xe30
	.uleb128 0x3
	.byte	0x91
	.sleb128 -80
	.uleb128 0x13
	.set L$set$232,LASF3-Lsection__debug_str
	.long L$set$232
	.byte	0x4
	.byte	0xa6
	.byte	0x7
	.long	0xca9
	.uleb128 0x3
	.byte	0x91
	.sleb128 -88
	.uleb128 0x13
	.set L$set$233,LASF4-Lsection__debug_str
	.long L$set$233
	.byte	0x4
	.byte	0xa7
	.byte	0x7
	.long	0x46d
	.uleb128 0x2
	.byte	0x91
	.sleb128 -43
	.uleb128 0x15
	.quad	LBB18
	.set L$set$234,LBE18-LBB18
	.quad L$set$234
	.uleb128 0x16
	.ascii "ed25519_success\0"
	.byte	0x1
	.short	0x179
	.byte	0x7
	.long	0x46d
	.uleb128 0x2
	.byte	0x91
	.sleb128 -42
	.uleb128 0x16
	.ascii "ml_dsa_success\0"
	.byte	0x1
	.short	0x17a
	.byte	0x7
	.long	0x46d
	.uleb128 0x2
	.byte	0x91
	.sleb128 -41
	.uleb128 0x18
	.quad	LBB19
	.set L$set$235,LBE19-LBB19
	.quad L$set$235
	.long	0xdc0
	.uleb128 0x16
	.ascii "i\0"
	.byte	0x1
	.short	0x186
	.byte	0xe
	.long	0x31a
	.uleb128 0x2
	.byte	0x91
	.sleb128 -40
	.byte	0
	.uleb128 0x18
	.quad	LBB20
	.set L$set$236,LBE20-LBB20
	.quad L$set$236
	.long	0xde4
	.uleb128 0x16
	.ascii "i\0"
	.byte	0x1
	.short	0x18a
	.byte	0xe
	.long	0x31a
	.uleb128 0x2
	.byte	0x91
	.sleb128 -36
	.byte	0
	.uleb128 0x18
	.quad	LBB21
	.set L$set$237,LBE21-LBB21
	.quad L$set$237
	.long	0xe08
	.uleb128 0x16
	.ascii "i\0"
	.byte	0x1
	.short	0x19b
	.byte	0xe
	.long	0x31a
	.uleb128 0x2
	.byte	0x91
	.sleb128 -32
	.byte	0
	.uleb128 0x15
	.quad	LBB22
	.set L$set$238,LBE22-LBB22
	.quad L$set$238
	.uleb128 0x16
	.ascii "i\0"
	.byte	0x1
	.short	0x19e
	.byte	0xe
	.long	0x31a
	.uleb128 0x2
	.byte	0x91
	.sleb128 -28
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0x17
	.byte	0x8
	.long	0x9fe
	.uleb128 0x17
	.byte	0x8
	.long	0x60d
	.uleb128 0x12
	.ascii "anubis_types__pqc__derive_encryption_key\0"
	.byte	0x1
	.short	0x149
	.byte	0x4
	.long	0x46d
	.quad	LFB16
	.set L$set$239,LFE16-LFB16
	.quad L$set$239
	.uleb128 0x1
	.byte	0x9c
	.long	0xf8a
	.uleb128 0x13
	.set L$set$240,LASF5-Lsection__debug_str
	.long L$set$240
	.byte	0x4
	.byte	0x84
	.byte	0x7
	.long	0xf8a
	.uleb128 0x3
	.byte	0x91
	.sleb128 -104
	.uleb128 0x14
	.ascii "encryption_key\0"
	.byte	0x4
	.byte	0x85
	.byte	0x7
	.long	0xf90
	.uleb128 0x3
	.byte	0x91
	.sleb128 -112
	.uleb128 0x13
	.set L$set$241,LASF4-Lsection__debug_str
	.long L$set$241
	.byte	0x4
	.byte	0x86
	.byte	0x7
	.long	0x46d
	.uleb128 0x3
	.byte	0x91
	.sleb128 -89
	.uleb128 0x15
	.quad	LBB16
	.set L$set$242,LBE16-LBB16
	.quad L$set$242
	.uleb128 0x9
	.ascii "anubis_types__pqc__derive_encryption_key__B_7__Ttemp_inputS\0"
	.long	0x24a
	.long	0xf16
	.uleb128 0xc
	.long	0x31a
	.sleb128 32
	.byte	0
	.uleb128 0x16
	.ascii "temp_input\0"
	.byte	0x1
	.short	0x157
	.byte	0xa
	.long	0xeca
	.uleb128 0x3
	.byte	0x91
	.sleb128 -88
	.uleb128 0x9
	.ascii "anubis_types__pqc__derive_encryption_key__B_7__Ttemp_keyS\0"
	.long	0x24a
	.long	0xf78
	.uleb128 0xc
	.long	0x31a
	.sleb128 32
	.byte	0
	.uleb128 0x19
	.set L$set$243,LASF6-Lsection__debug_str
	.long L$set$243
	.byte	0x1
	.short	0x158
	.byte	0xa
	.long	0xf2e
	.uleb128 0x2
	.byte	0x91
	.sleb128 -56
	.byte	0
	.byte	0
	.uleb128 0x17
	.byte	0x8
	.long	0x802
	.uleb128 0x17
	.byte	0x8
	.long	0x987
	.uleb128 0x1a
	.ascii "anubis_types__pqc__hybrid_encapsulate\0"
	.byte	0x1
	.byte	0xee
	.byte	0x4
	.long	0x46d
	.quad	LFB15
	.set L$set$244,LFE15-LFB15
	.quad L$set$244
	.uleb128 0x1
	.byte	0x9c
	.long	0x11f2
	.uleb128 0x14
	.ascii "x25519_public\0"
	.byte	0x4
	.byte	0x79
	.byte	0x7
	.long	0x11f2
	.uleb128 0x3
	.byte	0x91
	.sleb128 -296
	.uleb128 0x14
	.ascii "ml_kem_public\0"
	.byte	0x4
	.byte	0x7a
	.byte	0x7
	.long	0x11f8
	.uleb128 0x3
	.byte	0x91
	.sleb128 -304
	.uleb128 0x14
	.ascii "x25519_ephemeral_secret\0"
	.byte	0x4
	.byte	0x7b
	.byte	0x7
	.long	0x11fe
	.uleb128 0x3
	.byte	0x91
	.sleb128 -312
	.uleb128 0x13
	.set L$set$245,LASF7-Lsection__debug_str
	.long L$set$245
	.byte	0x4
	.byte	0x7c
	.byte	0x7
	.long	0x1204
	.uleb128 0x3
	.byte	0x91
	.sleb128 -320
	.uleb128 0x13
	.set L$set$246,LASF5-Lsection__debug_str
	.long L$set$246
	.byte	0x4
	.byte	0x7d
	.byte	0x7
	.long	0xf8a
	.uleb128 0x3
	.byte	0x91
	.sleb128 -328
	.uleb128 0x13
	.set L$set$247,LASF4-Lsection__debug_str
	.long L$set$247
	.byte	0x4
	.byte	0x7e
	.byte	0x7
	.long	0x46d
	.uleb128 0x3
	.byte	0x91
	.sleb128 -287
	.uleb128 0x15
	.quad	LBB12
	.set L$set$248,LBE12-LBB12
	.quad L$set$248
	.uleb128 0x1b
	.ascii "ml_kem_shared\0"
	.byte	0x1
	.byte	0xf6
	.byte	0x7
	.long	0x520
	.uleb128 0x3
	.byte	0x91
	.sleb128 -216
	.uleb128 0x1b
	.ascii "x25519_shared\0"
	.byte	0x1
	.byte	0xf7
	.byte	0x7
	.long	0xb93
	.uleb128 0x3
	.byte	0x91
	.sleb128 -176
	.uleb128 0x1b
	.ascii "x25519_ephemeral_pub\0"
	.byte	0x1
	.byte	0xf8
	.byte	0x7
	.long	0x8a4
	.uleb128 0x3
	.byte	0x91
	.sleb128 -280
	.uleb128 0x1b
	.ascii "pq_success\0"
	.byte	0x1
	.byte	0xf9
	.byte	0x7
	.long	0x46d
	.uleb128 0x3
	.byte	0x91
	.sleb128 -285
	.uleb128 0x1b
	.ascii "classical_success\0"
	.byte	0x1
	.byte	0xfa
	.byte	0x7
	.long	0x46d
	.uleb128 0x3
	.byte	0x91
	.sleb128 -286
	.uleb128 0x9
	.ascii "anubis_types__pqc__hybrid_encapsulate__Tcombined_inputS\0"
	.long	0x24a
	.long	0x1146
	.uleb128 0xc
	.long	0x31a
	.sleb128 64
	.byte	0
	.uleb128 0x1b
	.ascii "combined_input\0"
	.byte	0x1
	.byte	0xfb
	.byte	0x7
	.long	0x10fd
	.uleb128 0x3
	.byte	0x91
	.sleb128 -136
	.uleb128 0x18
	.quad	LBB13
	.set L$set$249,LBE13-LBB13
	.quad L$set$249
	.long	0x11cf
	.uleb128 0x9
	.ascii "anubis_types__pqc__hybrid_encapsulate__B_5__Ttemp_keyS\0"
	.long	0x24a
	.long	0x11bd
	.uleb128 0xc
	.long	0x31a
	.sleb128 32
	.byte	0
	.uleb128 0x19
	.set L$set$250,LASF6-Lsection__debug_str
	.long L$set$250
	.byte	0x1
	.short	0x130
	.byte	0xa
	.long	0x1176
	.uleb128 0x3
	.byte	0x91
	.sleb128 -248
	.byte	0
	.uleb128 0x15
	.quad	LBB15
	.set L$set$251,LBE15-LBB15
	.quad L$set$251
	.uleb128 0x16
	.ascii "i\0"
	.byte	0x1
	.short	0x144
	.byte	0xb
	.long	0x31a
	.uleb128 0x3
	.byte	0x91
	.sleb128 -284
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0x17
	.byte	0x8
	.long	0x8a4
	.uleb128 0x17
	.byte	0x8
	.long	0x3b8
	.uleb128 0x17
	.byte	0x8
	.long	0x911
	.uleb128 0x17
	.byte	0x8
	.long	0x4af
	.uleb128 0x1c
	.ascii "anubis_types__pqc__zeroize_ml_dsa_secret\0"
	.byte	0x1
	.byte	0xe1
	.byte	0x4
	.quad	LFB14
	.set L$set$252,LFE14-LFB14
	.quad L$set$252
	.uleb128 0x1
	.byte	0x9c
	.long	0x126c
	.uleb128 0x13
	.set L$set$253,LASF8-Lsection__debug_str
	.long L$set$253
	.byte	0x4
	.byte	0x6a
	.byte	0x7
	.long	0xe30
	.uleb128 0x2
	.byte	0x91
	.sleb128 -24
	.uleb128 0x1d
	.set L$set$254,LASF9-Lsection__debug_str
	.long L$set$254
	.byte	0x1
	.byte	0xe4
	.byte	0x7
	.long	0xaf4
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.byte	0
	.uleb128 0x1c
	.ascii "anubis_types__pqc__zeroize_ml_kem_secret\0"
	.byte	0x1
	.byte	0xd8
	.byte	0x4
	.quad	LFB13
	.set L$set$255,LFE13-LFB13
	.quad L$set$255
	.uleb128 0x1
	.byte	0x9c
	.long	0x12ce
	.uleb128 0x13
	.set L$set$256,LASF8-Lsection__debug_str
	.long L$set$256
	.byte	0x4
	.byte	0x64
	.byte	0x7
	.long	0x12ce
	.uleb128 0x2
	.byte	0x91
	.sleb128 -24
	.uleb128 0x1d
	.set L$set$257,LASF9-Lsection__debug_str
	.long L$set$257
	.byte	0x1
	.byte	0xdb
	.byte	0x7
	.long	0xaf4
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.byte	0
	.uleb128 0x17
	.byte	0x8
	.long	0x427
	.uleb128 0x1c
	.ascii "anubis_types__pqc__zeroize_shared_secret\0"
	.byte	0x1
	.byte	0xce
	.byte	0x4
	.quad	LFB12
	.set L$set$258,LFE12-LFB12
	.quad L$set$258
	.uleb128 0x1
	.byte	0x9c
	.long	0x1341
	.uleb128 0x14
	.ascii "secret\0"
	.byte	0x4
	.byte	0x5e
	.byte	0x7
	.long	0x1341
	.uleb128 0x2
	.byte	0x91
	.sleb128 -24
	.uleb128 0x1b
	.ascii "secret_addr\0"
	.byte	0x1
	.byte	0xd1
	.byte	0x7
	.long	0xaf4
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.byte	0
	.uleb128 0x17
	.byte	0x8
	.long	0x520
	.uleb128 0x1a
	.ascii "anubis_types__pqc__secrets_match\0"
	.byte	0x1
	.byte	0xbb
	.byte	0x4
	.long	0x46d
	.quad	LFB11
	.set L$set$259,LFE11-LFB11
	.quad L$set$259
	.uleb128 0x1
	.byte	0x9c
	.long	0x13e5
	.uleb128 0x14
	.ascii "secret_a\0"
	.byte	0x4
	.byte	0x58
	.byte	0x7
	.long	0x1341
	.uleb128 0x2
	.byte	0x91
	.sleb128 -40
	.uleb128 0x14
	.ascii "secret_b\0"
	.byte	0x4
	.byte	0x59
	.byte	0x7
	.long	0x1341
	.uleb128 0x2
	.byte	0x91
	.sleb128 -48
	.uleb128 0x1b
	.ascii "addr_a\0"
	.byte	0x1
	.byte	0xc1
	.byte	0x7
	.long	0xaf4
	.uleb128 0x2
	.byte	0x91
	.sleb128 -16
	.uleb128 0x1b
	.ascii "addr_b\0"
	.byte	0x1
	.byte	0xc2
	.byte	0x7
	.long	0xaf4
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.uleb128 0x1b
	.ascii "result\0"
	.byte	0x1
	.byte	0xc3
	.byte	0x7
	.long	0xb20
	.uleb128 0x2
	.byte	0x91
	.sleb128 -20
	.byte	0
	.uleb128 0x1a
	.ascii "anubis_types__pqc__ml_dsa_verify\0"
	.byte	0x1
	.byte	0xa3
	.byte	0x4
	.long	0x46d
	.quad	LFB10
	.set L$set$260,LFE10-LFB10
	.quad L$set$260
	.uleb128 0x1
	.byte	0x9c
	.long	0x1473
	.uleb128 0x13
	.set L$set$261,LASF2-Lsection__debug_str
	.long L$set$261
	.byte	0x4
	.byte	0x4b
	.byte	0x7
	.long	0x2cd
	.uleb128 0x2
	.byte	0x91
	.sleb128 -32
	.uleb128 0x13
	.set L$set$262,LASF3-Lsection__debug_str
	.long L$set$262
	.byte	0x4
	.byte	0x4c
	.byte	0x7
	.long	0x1473
	.uleb128 0x2
	.byte	0x91
	.sleb128 -40
	.uleb128 0x13
	.set L$set$263,LASF10-Lsection__debug_str
	.long L$set$263
	.byte	0x4
	.byte	0x4d
	.byte	0x7
	.long	0xcb5
	.uleb128 0x2
	.byte	0x91
	.sleb128 -48
	.uleb128 0x15
	.quad	LBB11
	.set L$set$264,LBE11-LBB11
	.quad L$set$264
	.uleb128 0x1d
	.set L$set$265,LASF11-Lsection__debug_str
	.long L$set$265
	.byte	0x1
	.byte	0xa9
	.byte	0x7
	.long	0xab2
	.uleb128 0x2
	.byte	0x91
	.sleb128 -4
	.byte	0
	.byte	0
	.uleb128 0x17
	.byte	0x8
	.long	0x689
	.uleb128 0x1a
	.ascii "anubis_types__pqc__ml_dsa_sign\0"
	.byte	0x1
	.byte	0x8e
	.byte	0x4
	.long	0x46d
	.quad	LFB9
	.set L$set$266,LFE9-LFB9
	.quad L$set$266
	.uleb128 0x1
	.byte	0x9c
	.long	0x152f
	.uleb128 0x13
	.set L$set$267,LASF2-Lsection__debug_str
	.long L$set$267
	.byte	0x4
	.byte	0x41
	.byte	0x7
	.long	0x2c8
	.uleb128 0x2
	.byte	0x91
	.sleb128 -64
	.uleb128 0x13
	.set L$set$268,LASF8-Lsection__debug_str
	.long L$set$268
	.byte	0x4
	.byte	0x42
	.byte	0x7
	.long	0xe30
	.uleb128 0x3
	.byte	0x91
	.sleb128 -72
	.uleb128 0x13
	.set L$set$269,LASF3-Lsection__debug_str
	.long L$set$269
	.byte	0x4
	.byte	0x43
	.byte	0x7
	.long	0x1473
	.uleb128 0x3
	.byte	0x91
	.sleb128 -80
	.uleb128 0x13
	.set L$set$270,LASF4-Lsection__debug_str
	.long L$set$270
	.byte	0x4
	.byte	0x44
	.byte	0x7
	.long	0x46d
	.uleb128 0x2
	.byte	0x91
	.sleb128 -37
	.uleb128 0x15
	.quad	LBB10
	.set L$set$271,LBE10-LBB10
	.quad L$set$271
	.uleb128 0x1d
	.set L$set$272,LASF11-Lsection__debug_str
	.long L$set$272
	.byte	0x1
	.byte	0x94
	.byte	0x7
	.long	0xab2
	.uleb128 0x2
	.byte	0x91
	.sleb128 -36
	.uleb128 0x1b
	.ascii "signature_len\0"
	.byte	0x1
	.byte	0x95
	.byte	0x7
	.long	0xb07
	.uleb128 0x2
	.byte	0x91
	.sleb128 -32
	.byte	0
	.byte	0
	.uleb128 0x1a
	.ascii "anubis_types__pqc__ml_dsa_generate_keypair\0"
	.byte	0x1
	.byte	0x74
	.byte	0x4
	.long	0x46d
	.quad	LFB8
	.set L$set$273,LFE8-LFB8
	.quad L$set$273
	.uleb128 0x1
	.byte	0x9c
	.long	0x15e6
	.uleb128 0x13
	.set L$set$274,LASF10-Lsection__debug_str
	.long L$set$274
	.byte	0x4
	.byte	0x35
	.byte	0x7
	.long	0xcb5
	.uleb128 0x2
	.byte	0x91
	.sleb128 -56
	.uleb128 0x13
	.set L$set$275,LASF8-Lsection__debug_str
	.long L$set$275
	.byte	0x4
	.byte	0x36
	.byte	0x7
	.long	0xe30
	.uleb128 0x2
	.byte	0x91
	.sleb128 -64
	.uleb128 0x13
	.set L$set$276,LASF4-Lsection__debug_str
	.long L$set$276
	.byte	0x4
	.byte	0x37
	.byte	0x7
	.long	0x46d
	.uleb128 0x2
	.byte	0x91
	.sleb128 -33
	.uleb128 0x15
	.quad	LBB8
	.set L$set$277,LBE8-LBB8
	.quad L$set$277
	.uleb128 0x1d
	.set L$set$278,LASF11-Lsection__debug_str
	.long L$set$278
	.byte	0x1
	.byte	0x79
	.byte	0x7
	.long	0xab2
	.uleb128 0x2
	.byte	0x91
	.sleb128 -28
	.uleb128 0x15
	.quad	LBB9
	.set L$set$279,LBE9-LBB9
	.quad L$set$279
	.uleb128 0x1b
	.ascii "i\0"
	.byte	0x1
	.byte	0x87
	.byte	0xe
	.long	0x31a
	.uleb128 0x2
	.byte	0x91
	.sleb128 -32
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0x1a
	.ascii "anubis_types__pqc__ml_kem_decapsulate\0"
	.byte	0x1
	.byte	0x54
	.byte	0x4
	.long	0x46d
	.quad	LFB7
	.set L$set$280,LFE7-LFB7
	.quad L$set$280
	.uleb128 0x1
	.byte	0x9c
	.long	0x16a8
	.uleb128 0x13
	.set L$set$281,LASF7-Lsection__debug_str
	.long L$set$281
	.byte	0x4
	.byte	0x27
	.byte	0x7
	.long	0x1204
	.uleb128 0x2
	.byte	0x91
	.sleb128 -56
	.uleb128 0x13
	.set L$set$282,LASF8-Lsection__debug_str
	.long L$set$282
	.byte	0x4
	.byte	0x28
	.byte	0x7
	.long	0x12ce
	.uleb128 0x2
	.byte	0x91
	.sleb128 -64
	.uleb128 0x13
	.set L$set$283,LASF12-Lsection__debug_str
	.long L$set$283
	.byte	0x4
	.byte	0x29
	.byte	0x7
	.long	0x1341
	.uleb128 0x3
	.byte	0x91
	.sleb128 -72
	.uleb128 0x13
	.set L$set$284,LASF4-Lsection__debug_str
	.long L$set$284
	.byte	0x4
	.byte	0x2a
	.byte	0x7
	.long	0x46d
	.uleb128 0x2
	.byte	0x91
	.sleb128 -33
	.uleb128 0x15
	.quad	LBB6
	.set L$set$285,LBE6-LBB6
	.quad L$set$285
	.uleb128 0x1d
	.set L$set$286,LASF11-Lsection__debug_str
	.long L$set$286
	.byte	0x1
	.byte	0x5a
	.byte	0x7
	.long	0xab2
	.uleb128 0x2
	.byte	0x91
	.sleb128 -28
	.uleb128 0x15
	.quad	LBB7
	.set L$set$287,LBE7-LBB7
	.quad L$set$287
	.uleb128 0x1b
	.ascii "i\0"
	.byte	0x1
	.byte	0x69
	.byte	0xe
	.long	0x31a
	.uleb128 0x2
	.byte	0x91
	.sleb128 -32
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0x1a
	.ascii "anubis_types__pqc__ml_kem_encapsulate\0"
	.byte	0x1
	.byte	0x38
	.byte	0x4
	.long	0x46d
	.quad	LFB6
	.set L$set$288,LFE6-LFB6
	.quad L$set$288
	.uleb128 0x1
	.byte	0x9c
	.long	0x177b
	.uleb128 0x14
	.ascii "recipient_public_key\0"
	.byte	0x4
	.byte	0x1d
	.byte	0x7
	.long	0x11f8
	.uleb128 0x2
	.byte	0x91
	.sleb128 -56
	.uleb128 0x13
	.set L$set$289,LASF7-Lsection__debug_str
	.long L$set$289
	.byte	0x4
	.byte	0x1e
	.byte	0x7
	.long	0x1204
	.uleb128 0x2
	.byte	0x91
	.sleb128 -64
	.uleb128 0x13
	.set L$set$290,LASF12-Lsection__debug_str
	.long L$set$290
	.byte	0x4
	.byte	0x1f
	.byte	0x7
	.long	0x1341
	.uleb128 0x3
	.byte	0x91
	.sleb128 -72
	.uleb128 0x13
	.set L$set$291,LASF4-Lsection__debug_str
	.long L$set$291
	.byte	0x4
	.byte	0x20
	.byte	0x7
	.long	0x46d
	.uleb128 0x2
	.byte	0x91
	.sleb128 -33
	.uleb128 0x15
	.quad	LBB4
	.set L$set$292,LBE4-LBB4
	.quad L$set$292
	.uleb128 0x1d
	.set L$set$293,LASF11-Lsection__debug_str
	.long L$set$293
	.byte	0x1
	.byte	0x3e
	.byte	0x7
	.long	0xab2
	.uleb128 0x2
	.byte	0x91
	.sleb128 -28
	.uleb128 0x15
	.quad	LBB5
	.set L$set$294,LBE5-LBB5
	.quad L$set$294
	.uleb128 0x1b
	.ascii "i\0"
	.byte	0x1
	.byte	0x4d
	.byte	0xe
	.long	0x31a
	.uleb128 0x2
	.byte	0x91
	.sleb128 -32
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0x1a
	.ascii "anubis_types__pqc__ml_kem_generate_keypair\0"
	.byte	0x1
	.byte	0x1e
	.byte	0x4
	.long	0x46d
	.quad	LFB5
	.set L$set$295,LFE5-LFB5
	.quad L$set$295
	.uleb128 0x1
	.byte	0x9c
	.long	0x1832
	.uleb128 0x13
	.set L$set$296,LASF10-Lsection__debug_str
	.long L$set$296
	.byte	0x4
	.byte	0x11
	.byte	0x7
	.long	0x11f8
	.uleb128 0x2
	.byte	0x91
	.sleb128 -56
	.uleb128 0x13
	.set L$set$297,LASF8-Lsection__debug_str
	.long L$set$297
	.byte	0x4
	.byte	0x12
	.byte	0x7
	.long	0x12ce
	.uleb128 0x2
	.byte	0x91
	.sleb128 -64
	.uleb128 0x13
	.set L$set$298,LASF4-Lsection__debug_str
	.long L$set$298
	.byte	0x4
	.byte	0x13
	.byte	0x7
	.long	0x46d
	.uleb128 0x2
	.byte	0x91
	.sleb128 -33
	.uleb128 0x15
	.quad	LBB2
	.set L$set$299,LBE2-LBB2
	.quad L$set$299
	.uleb128 0x1d
	.set L$set$300,LASF11-Lsection__debug_str
	.long L$set$300
	.byte	0x1
	.byte	0x23
	.byte	0x7
	.long	0xab2
	.uleb128 0x2
	.byte	0x91
	.sleb128 -28
	.uleb128 0x15
	.quad	LBB3
	.set L$set$301,LBE3-LBB3
	.quad L$set$301
	.uleb128 0x1b
	.ascii "i\0"
	.byte	0x1
	.byte	0x31
	.byte	0xe
	.long	0x31a
	.uleb128 0x2
	.byte	0x91
	.sleb128 -32
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0x1e
	.ascii "anubis_types__pqc__is_valid\0"
	.byte	0x1
	.byte	0x15
	.byte	0x4
	.long	0x46d
	.quad	LFB4
	.set L$set$302,LFE4-LFB4
	.quad L$set$302
	.uleb128 0x1
	.byte	0x9c
	.uleb128 0x14
	.ascii "secret\0"
	.byte	0x4
	.byte	0x76
	.byte	0x17
	.long	0xf8a
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
	.uleb128 0x6
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
	.uleb128 0x7
	.uleb128 0xf
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x8
	.uleb128 0x26
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x34
	.uleb128 0x19
	.byte	0
	.byte	0
	.uleb128 0x9
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
	.uleb128 0xa
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
	.uleb128 0xb
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
	.uleb128 0xc
	.uleb128 0x21
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2f
	.uleb128 0xd
	.byte	0
	.byte	0
	.uleb128 0xd
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
	.uleb128 0xe
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
	.uleb128 0xf
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
	.uleb128 0x5
	.byte	0
	.byte	0
	.uleb128 0x10
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
	.uleb128 0x11
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
	.byte	0
	.byte	0
	.uleb128 0x12
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
	.uleb128 0x13
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
	.uleb128 0x14
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
	.uleb128 0x15
	.uleb128 0xb
	.byte	0x1
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x7
	.byte	0
	.byte	0
	.uleb128 0x16
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
	.uleb128 0x17
	.uleb128 0x10
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x18
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
	.uleb128 0x19
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
	.uleb128 0x1a
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
	.uleb128 0x1b
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
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x1d
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
	.byte	0
	.byte	0
	.byte	0
	.section __DWARF,__debug_pubnames,regular,debug
Lsection__debug_pubnames:
	.long	0x273
	.short	0x2
	.set L$set$303,Ldebug_info0-Lsection__debug_info
	.long L$set$303
	.long	0x187c
	.long	0xbda
	.ascii "anubis_types__pqc__hybrid_verify\0"
	.long	0xcbb
	.ascii "anubis_types__pqc__hybrid_sign\0"
	.long	0xe36
	.ascii "anubis_types__pqc__derive_encryption_key\0"
	.long	0xf96
	.ascii "anubis_types__pqc__hybrid_encapsulate\0"
	.long	0x120a
	.ascii "anubis_types__pqc__zeroize_ml_dsa_secret\0"
	.long	0x126c
	.ascii "anubis_types__pqc__zeroize_ml_kem_secret\0"
	.long	0x12d4
	.ascii "anubis_types__pqc__zeroize_shared_secret\0"
	.long	0x1347
	.ascii "anubis_types__pqc__secrets_match\0"
	.long	0x13e5
	.ascii "anubis_types__pqc__ml_dsa_verify\0"
	.long	0x1479
	.ascii "anubis_types__pqc__ml_dsa_sign\0"
	.long	0x152f
	.ascii "anubis_types__pqc__ml_dsa_generate_keypair\0"
	.long	0x15e6
	.ascii "anubis_types__pqc__ml_kem_decapsulate\0"
	.long	0x16a8
	.ascii "anubis_types__pqc__ml_kem_encapsulate\0"
	.long	0x177b
	.ascii "anubis_types__pqc__ml_kem_generate_keypair\0"
	.long	0x1832
	.ascii "anubis_types__pqc__is_valid\0"
	.long	0
	.section __DWARF,__debug_pubtypes,regular,debug
Lsection__debug_pubtypes:
	.long	0x623
	.short	0x2
	.set L$set$304,Ldebug_info0-Lsection__debug_info
	.long L$set$304
	.long	0x187c
	.long	0x230
	.ascii "interfaces__unsigned_8\0"
	.long	0x264
	.ascii "anubis_types__TbyteB\0"
	.long	0x31a
	.ascii "integer\0"
	.long	0x2dc
	.ascii "anubis_types__byte_array___XUA\0"
	.long	0x325
	.ascii "anubis_types__byte_array___XUB\0"
	.long	0x27c
	.ascii "anubis_types__byte_array\0"
	.long	0x381
	.ascii "anubis_types__ml_kem_public_key__T33s\0"
	.long	0x3b8
	.ascii "anubis_types__ml_kem_public_key\0"
	.long	0x3f0
	.ascii "anubis_types__ml_kem_secret_key__T35s\0"
	.long	0x46d
	.ascii "boolean\0"
	.long	0x427
	.ascii "anubis_types__ml_kem_secret_key\0"
	.long	0x478
	.ascii "anubis_types__ml_kem_ciphertext__T37s\0"
	.long	0x4af
	.ascii "anubis_types__ml_kem_ciphertext\0"
	.long	0x4e7
	.ascii "anubis_types__ml_kem_shared_secret__T39s\0"
	.long	0x520
	.ascii "anubis_types__ml_kem_shared_secret\0"
	.long	0x567
	.ascii "anubis_types__ml_dsa_public_key__T41s\0"
	.long	0x59e
	.ascii "anubis_types__ml_dsa_public_key\0"
	.long	0x5d6
	.ascii "anubis_types__ml_dsa_secret_key__T43s\0"
	.long	0x60d
	.ascii "anubis_types__ml_dsa_secret_key\0"
	.long	0x653
	.ascii "anubis_types__ml_dsa_signature__T45s\0"
	.long	0x689
	.ascii "anubis_types__ml_dsa_signature\0"
	.long	0x6c0
	.ascii "anubis_types__ed25519_signature__T25s\0"
	.long	0x6f7
	.ascii "anubis_types__ed25519_signature\0"
	.long	0x72e
	.ascii "anubis_types__pqc__hybrid_signature\0"
	.long	0x786
	.ascii "anubis_types__pqc__hybrid_shared_secret__T18s\0"
	.long	0x7c4
	.ascii "anubis_types__pqc__hybrid_shared_secret__T20s\0"
	.long	0x802
	.ascii "anubis_types__pqc__hybrid_shared_secret\0"
	.long	0x86e
	.ascii "anubis_types__x25519_public_key__T15s\0"
	.long	0x8a4
	.ascii "anubis_types__x25519_public_key\0"
	.long	0x8db
	.ascii "anubis_types__x25519_secret_key__T17s\0"
	.long	0x911
	.ascii "anubis_types__x25519_secret_key\0"
	.long	0x955
	.ascii "anubis_types__xchacha20_key__T27s\0"
	.long	0x987
	.ascii "anubis_types__xchacha20_key\0"
	.long	0x9c7
	.ascii "anubis_types__ed25519_secret_key__T23s\0"
	.long	0x9fe
	.ascii "anubis_types__ed25519_secret_key\0"
	.long	0xa43
	.ascii "anubis_types__ed25519_public_key__T21s\0"
	.long	0xa7a
	.ascii "anubis_types__ed25519_public_key\0"
	.long	0xad8
	.ascii "oqs_common__Toqs_statusB\0"
	.long	0xaf4
	.ascii "system__address\0"
	.long	0xb07
	.ascii "interfaces__c__size_t\0"
	.long	0xb42
	.ascii "interfaces__c__TintB\0"
	.long	0xb5a
	.ascii "anubis_types__x25519_shared_secret__T19s\0"
	.long	0xb93
	.ascii "anubis_types__x25519_shared_secret\0"
	.long	0
	.section __DWARF,__debug_aranges,regular,debug
Lsection__debug_aranges:
	.long	0x2c
	.short	0x2
	.set L$set$305,Ldebug_info0-Lsection__debug_info
	.long L$set$305
	.byte	0x8
	.byte	0
	.short	0
	.short	0
	.quad	Ltext0
	.set L$set$306,Letext0-Ltext0
	.quad L$set$306
	.quad	0
	.quad	0
	.section __DWARF,__debug_line,regular,debug
Lsection__debug_line:
Ldebug_line0:
	.section __DWARF,__debug_str,regular,debug
Lsection__debug_str:
LASF7:
	.ascii "ciphertext\0"
LASF9:
	.ascii "key_addr\0"
LASF2:
	.ascii "message\0"
LASF6:
	.ascii "temp_key\0"
LASF10:
	.ascii "public_key\0"
LASF3:
	.ascii "signature\0"
LASF5:
	.ascii "hybrid_secret\0"
LASF1:
	.ascii "valid\0"
LASF8:
	.ascii "secret_key\0"
LASF11:
	.ascii "status\0"
LASF4:
	.ascii "success\0"
LASF0:
	.ascii "data\0"
LASF12:
	.ascii "shared_secret\0"
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
