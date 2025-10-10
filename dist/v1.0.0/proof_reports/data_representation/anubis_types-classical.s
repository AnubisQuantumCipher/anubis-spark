	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
Ltext0:
	.file 1 "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_types-classical.adb"
	.align	2
	.globl _anubis_types__classical__x25519_generate_keypair
_anubis_types__classical__x25519_generate_keypair:
LFB2:
	.loc 1 23 4
	sub	x10, sp, #16384
	str	xzr, [x10, 4032]
	sub	sp, sp, #64
LCFI0:
	stp	x29, x30, [sp, 48]
LCFI1:
	add	x29, sp, 48
LCFI2:
	str	x0, [x29, -40]
	str	x1, [x29, -48]
	.loc 1 23 4
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x1, [x0]
	str	x1, [x29, -8]
	mov	x1, 0
LBB2:
	.loc 1 31 34
	ldr	x1, [x29, -48]
	ldr	x0, [x29, -40]
	bl	_crypto_box_keypair
	.loc 1 31 34 is_stmt 0 discriminator 1
	str	w0, [x29, -12]
	.loc 1 36 7 is_stmt 1
	ldr	w0, [x29, -12]
	cmp	w0, 0
	bne	L2
	.loc 1 37 27
	ldr	x0, [x29, -48]
	mov	w1, 1
	strb	w1, [x0, 32]
	.loc 1 38 18
	mov	w0, 1
	strb	w0, [x29, -17]
	.loc 1 50 8
	b	L9
L2:
	.loc 1 41 27
	ldr	x0, [x29, -48]
	strb	wzr, [x0, 32]
LBB3:
	.loc 1 42 14
	mov	w0, 1
	str	w0, [x29, -16]
L5:
	.loc 1 42 14 is_stmt 0 discriminator 1
	ldr	w0, [x29, -16]
	cmp	w0, 32
	bgt	L4
	.loc 1 46 33 is_stmt 1
	ldrsw	x0, [x29, -16]
	ldr	x1, [x29, -48]
	add	x0, x1, x0
	strb	wzr, [x0, -1]
	.loc 1 42 14 discriminator 2
	ldr	w0, [x29, -16]
	add	w0, w0, 1
	str	w0, [x29, -16]
	.loc 1 47 18
	b	L5
L4:
LBE3:
	.loc 1 48 18
	strb	wzr, [x29, -17]
L9:
	.loc 1 50 8
	nop
LBE2:
	ldrb	w0, [x29, -17]
	.loc 1 50 8 is_stmt 0 discriminator 2
	mov	w1, w0
	.loc 1 50 8
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x3, [x29, -8]
	ldr	x2, [x0]
	subs	x3, x3, x2
	mov	x2, 0
	beq	L7
	bl	___stack_chk_fail
L7:
	mov	w0, w1
	ldp	x29, x30, [sp, 48]
	add	sp, sp, 64
LCFI3:
	ret
LFE2:
	.align	2
	.globl _anubis_types__classical__x25519_compute_shared
_anubis_types__classical__x25519_compute_shared:
LFB3:
	.loc 1 52 4 is_stmt 1
	sub	x10, sp, #16384
	str	xzr, [x10, 4016]
	sub	sp, sp, #80
LCFI4:
	stp	x29, x30, [sp, 64]
LCFI5:
	add	x29, sp, 64
LCFI6:
	str	x0, [x29, -40]
	str	x1, [x29, -48]
	str	x2, [x29, -56]
	.loc 1 52 4
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x1, [x0]
	str	x1, [x29, -8]
	mov	x1, 0
LBB4:
	.loc 1 61 34
	ldr	x2, [x29, -48]
	ldr	x1, [x29, -40]
	ldr	x0, [x29, -56]
	bl	_crypto_scalarmult
	.loc 1 61 34 is_stmt 0 discriminator 1
	str	w0, [x29, -12]
	.loc 1 67 7 is_stmt 1
	ldr	w0, [x29, -12]
	cmp	w0, 0
	bne	L11
	.loc 1 68 30
	ldr	x0, [x29, -56]
	mov	w1, 1
	strb	w1, [x0, 32]
	.loc 1 69 18
	mov	w0, 1
	strb	w0, [x29, -17]
	.loc 1 81 8
	b	L18
L11:
	.loc 1 72 30
	ldr	x0, [x29, -56]
	strb	wzr, [x0, 32]
LBB5:
	.loc 1 73 14
	mov	w0, 1
	str	w0, [x29, -16]
L14:
	.loc 1 73 14 is_stmt 0 discriminator 1
	ldr	w0, [x29, -16]
	cmp	w0, 32
	bgt	L13
	.loc 1 77 36 is_stmt 1
	ldrsw	x0, [x29, -16]
	ldr	x1, [x29, -56]
	add	x0, x1, x0
	strb	wzr, [x0, -1]
	.loc 1 73 14 discriminator 2
	ldr	w0, [x29, -16]
	add	w0, w0, 1
	str	w0, [x29, -16]
	.loc 1 78 18
	b	L14
L13:
LBE5:
	.loc 1 79 18
	strb	wzr, [x29, -17]
L18:
	.loc 1 81 8
	nop
LBE4:
	ldrb	w0, [x29, -17]
	.loc 1 81 8 is_stmt 0 discriminator 2
	mov	w1, w0
	.loc 1 81 8
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x3, [x29, -8]
	ldr	x2, [x0]
	subs	x3, x3, x2
	mov	x2, 0
	beq	L16
	bl	___stack_chk_fail
L16:
	mov	w0, w1
	ldp	x29, x30, [sp, 64]
	add	sp, sp, 80
LCFI7:
	ret
LFE3:
	.align	2
	.globl _anubis_types__classical__zeroize_x25519_secret
_anubis_types__classical__zeroize_x25519_secret:
LFB4:
	.loc 1 83 4 is_stmt 1
	sub	x10, sp, #16384
	str	xzr, [x10, 4048]
	stp	x29, x30, [sp, -48]!
LCFI8:
	mov	x29, sp
LCFI9:
	str	x0, [x29, 24]
	.loc 1 86 7
	ldr	x0, [x29, 24]
	str	x0, [x29, 40]
	.loc 1 88 7
	ldr	x0, [x29, 40]
	mov	x1, 32
	bl	_sodium_memzero
	.loc 1 89 24
	ldr	x0, [x29, 24]
	strb	wzr, [x0, 32]
	.loc 1 90 8
	nop
	ldp	x29, x30, [sp], 48
LCFI10:
	ret
LFE4:
	.align	2
	.globl _anubis_types__classical__zeroize_x25519_shared
_anubis_types__classical__zeroize_x25519_shared:
LFB5:
	.loc 1 92 4
	sub	x10, sp, #16384
	str	xzr, [x10, 4048]
	stp	x29, x30, [sp, -48]!
LCFI11:
	mov	x29, sp
LCFI12:
	str	x0, [x29, 24]
	.loc 1 95 7
	ldr	x0, [x29, 24]
	str	x0, [x29, 40]
	.loc 1 97 7
	ldr	x0, [x29, 40]
	mov	x1, 32
	bl	_sodium_memzero
	.loc 1 98 27
	ldr	x0, [x29, 24]
	strb	wzr, [x0, 32]
	.loc 1 99 8
	nop
	ldp	x29, x30, [sp], 48
LCFI13:
	ret
LFE5:
	.align	2
	.globl _anubis_types__classical__ed25519_generate_keypair
_anubis_types__classical__ed25519_generate_keypair:
LFB6:
	.loc 1 105 4
	sub	x10, sp, #16384
	str	xzr, [x10, 3968]
	sub	sp, sp, #128
LCFI14:
	stp	x29, x30, [sp, 112]
LCFI15:
	add	x29, sp, 112
LCFI16:
	str	x0, [x29, -104]
	str	x1, [x29, -112]
	.loc 1 105 4
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x1, [x0]
	str	x1, [x29, -8]
	mov	x1, 0
LBB6:
	.loc 1 116 28
	sub	x0, x29, #72
	mov	x1, x0
	ldr	x0, [x29, -104]
	bl	_crypto_sign_keypair
	.loc 1 116 28 is_stmt 0 discriminator 1
	str	w0, [x29, -76]
	.loc 1 121 7 is_stmt 1
	ldr	w0, [x29, -76]
	cmp	w0, 0
	bne	L24
LBB7:
	.loc 1 123 14
	mov	w0, 1
	str	w0, [x29, -84]
L26:
	.loc 1 123 14 is_stmt 0 discriminator 1
	ldr	w0, [x29, -84]
	cmp	w0, 32
	bgt	L25
LBB8:
	.loc 1 131 33 is_stmt 1
	ldrsw	x1, [x29, -84]
	ldrsw	x0, [x29, -84]
	sub	x2, x29, #73
	ldrb	w2, [x2, x1]
	.loc 1 131 33 is_stmt 0 discriminator 1
	ldr	x1, [x29, -112]
	add	x0, x1, x0
	mov	w1, w2
	strb	w1, [x0, -1]
LBE8:
	.loc 1 123 14 is_stmt 1 discriminator 2
	ldr	w0, [x29, -84]
	add	w0, w0, 1
	str	w0, [x29, -84]
	.loc 1 132 18
	b	L26
L25:
LBE7:
	.loc 1 133 27
	ldr	x0, [x29, -112]
	mov	w1, 1
	strb	w1, [x0, 32]
	.loc 1 134 18
	mov	w0, 1
	strb	w0, [x29, -85]
	.loc 1 137 10
	sub	x0, x29, #72
	mov	x1, 64
	bl	_sodium_memzero
	b	L27
L24:
	.loc 1 140 27
	ldr	x0, [x29, -112]
	strb	wzr, [x0, 32]
LBB9:
	.loc 1 141 14
	mov	w0, 1
	str	w0, [x29, -80]
L29:
	.loc 1 141 14 is_stmt 0 discriminator 1
	ldr	w0, [x29, -80]
	cmp	w0, 32
	bgt	L28
	.loc 1 145 33 is_stmt 1
	ldrsw	x0, [x29, -80]
	ldr	x1, [x29, -112]
	add	x0, x1, x0
	strb	wzr, [x0, -1]
	.loc 1 141 14 discriminator 2
	ldr	w0, [x29, -80]
	add	w0, w0, 1
	str	w0, [x29, -80]
	.loc 1 146 18
	b	L29
L28:
LBE9:
	.loc 1 147 18
	strb	wzr, [x29, -85]
L27:
LBE6:
	.loc 1 149 8 discriminator 1
	ldrb	w0, [x29, -85]
	.loc 1 149 8 is_stmt 0 discriminator 3
	mov	w1, w0
	.loc 1 149 8
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
	ldp	x29, x30, [sp, 112]
	add	sp, sp, 128
LCFI17:
	ret
LFE6:
	.const
	.align	3
lC1:
	.ascii "anubis_types-classical.adb"
	.space 1
	.text
	.align	2
	.globl _anubis_types__classical__ed25519_sign
_anubis_types__classical__ed25519_sign:
LFB7:
	.loc 1 151 4 is_stmt 1
	sub	x10, sp, #16384
	str	xzr, [x10, 3888]
	sub	sp, sp, #208
LCFI18:
	stp	x29, x30, [sp, 160]
LCFI19:
	add	x29, sp, 160
LCFI20:
	stp	x19, x20, [sp, 176]
	stp	x22, x23, [sp, 192]
LCFI21:
	stp	x0, x1, [x29, -144]
	str	x2, [x29, -152]
	str	x3, [x29, -160]
	.loc 1 151 4
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x1, [x0]
	str	x1, [x29, -8]
	mov	x1, 0
	ldr	x0, [x29, -136]
	ldr	w20, [x0]
	ldr	x0, [x29, -136]
	ldr	w19, [x0, 4]
	cmp	w19, w20
	blt	L33
	.loc 1 151 4 is_stmt 0 discriminator 1
	sxtw	x0, w19
	mov	x12, x0
	asr	x0, x0, 63
	mov	x13, x0
	sxtw	x0, w20
	mov	x10, x0
	asr	x0, x0, 63
	mov	x11, x0
	subs	x1, x12, x10
	sbc	x0, x13, x11
	mov	x8, x1
	mov	x9, x0
	mov	x0, 1
	adds	x1, x8, x0
	mov	x0, 0
	adc	x0, x9, x0
	mov	x22, x1
	mov	x23, x0
	b	L34
L33:
	.loc 1 151 4 discriminator 2
	mov	x22, 0
	mov	x23, 0
L34:
LBB10:
	.loc 1 151 4 discriminator 4
	cmp	w19, w20
	.loc 1 151 4 discriminator 8
	cmp	w19, w20
	blt	L38
	.loc 1 151 4 discriminator 9
	sxtw	x1, w19
	sxtw	x0, w20
	sub	x0, x1, x0
	add	x0, x0, 1
	mov	x4, x0
	mov	x5, 0
	lsr	x0, x4, 61
	lsl	x7, x5, 3
	orr	x7, x0, x7
	lsl	x6, x4, 3
L38:
	.loc 1 151 4 discriminator 12
	cmp	w19, w20
	.loc 1 158 7 is_stmt 1
	mov	x0, 64
	str	x0, [x29, -112]
	.loc 1 164 28
	sub	x1, x29, #72
	sub	x0, x29, #104
	ldr	x2, [x29, -152]
	bl	_crypto_sign_seed_keypair
	.loc 1 164 28 is_stmt 0 discriminator 1
	str	w0, [x29, -116]
	.loc 1 170 7 is_stmt 1
	ldr	w0, [x29, -116]
	cmp	w0, 0
	beq	L41
	.loc 1 171 18
	strb	wzr, [x29, -117]
	.loc 1 172 10
	b	L47
L41:
	.loc 1 179 27
	cmp	w20, w19
	ble	L43
	.loc 1 179 27 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L44
	bl	___stack_chk_fail
L44:
	mov	w1, 179
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L43:
	.loc 1 176 28 is_stmt 1
	ldr	x1, [x29, -144]
	mov	x0, x22
	cmp	x0, 0
	bge	L45
	.loc 1 176 28 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L46
	bl	___stack_chk_fail
L46:
	mov	w1, 180
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L45:
	.loc 1 176 28 discriminator 2
	mov	x3, x22
	.loc 1 176 28 discriminator 4
	sub	x2, x29, #72
	sub	x0, x29, #112
	mov	x4, x2
	mov	x2, x1
	mov	x1, x0
	ldr	x0, [x29, -160]
	bl	_crypto_sign_detached
	.loc 1 176 28 discriminator 5
	str	w0, [x29, -116]
	.loc 1 184 26 is_stmt 1
	ldr	w0, [x29, -116]
	cmp	w0, 0
	cset	w0, eq
	and	w0, w0, 255
	.loc 1 184 15
	strb	w0, [x29, -117]
	.loc 1 187 7
	sub	x0, x29, #72
	mov	x1, 64
	bl	_sodium_memzero
	.loc 1 188 7
	sub	x0, x29, #104
	mov	x1, 32
	bl	_sodium_memzero
	.loc 1 189 8
	nop
L47:
LBE10:
	.loc 1 189 8 is_stmt 0 discriminator 1
	ldrb	w0, [x29, -117]
	.loc 1 189 8 discriminator 3
	mov	w1, w0
	.loc 1 189 8
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x3, [x29, -8]
	ldr	x2, [x0]
	subs	x3, x3, x2
	mov	x2, 0
	beq	L49
	bl	___stack_chk_fail
L49:
	mov	w0, w1
	ldp	x29, x30, [sp, 160]
	ldp	x19, x20, [sp, 176]
	ldp	x22, x23, [sp, 192]
	add	sp, sp, 208
LCFI22:
	ret
LFE7:
	.align	2
	.globl _anubis_types__classical__ed25519_verify
_anubis_types__classical__ed25519_verify:
LFB8:
	.loc 1 191 4 is_stmt 1
	sub	x10, sp, #16384
	str	xzr, [x10, 4032]
	stp	x29, x30, [sp, -64]!
LCFI23:
	mov	x29, sp
LCFI24:
	stp	x0, x1, [x29, 32]
	str	x2, [x29, 24]
	str	x3, [x29, 16]
	.loc 1 191 4
	ldr	x0, [x29, 40]
	ldr	w1, [x0]
	ldr	x0, [x29, 40]
	ldr	w0, [x0, 4]
	cmp	w0, w1
	blt	L51
	.loc 1 191 4 is_stmt 0 discriminator 1
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
	b	L52
L51:
	.loc 1 191 4 discriminator 2
	mov	x14, 0
	mov	x15, 0
L52:
LBB11:
	.loc 1 191 4 discriminator 4
	cmp	w0, w1
	.loc 1 191 4 discriminator 8
	cmp	w0, w1
	blt	L56
	.loc 1 191 4 discriminator 9
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
L56:
	.loc 1 191 4 discriminator 12
	cmp	w0, w1
	.loc 1 202 25 is_stmt 1
	cmp	w1, w0
	ble	L59
	.loc 1 202 25 is_stmt 0 discriminator 1
	mov	w1, 202
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L59:
	.loc 1 200 28 is_stmt 1
	ldr	x0, [x29, 32]
	mov	x1, x14
	cmp	x1, 0
	bge	L60
	.loc 1 200 28 is_stmt 0 discriminator 1
	mov	w1, 203
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L60:
	.loc 1 200 28 discriminator 2
	mov	x1, x14
	.loc 1 200 28 discriminator 4
	ldr	x3, [x29, 16]
	mov	x2, x1
	mov	x1, x0
	ldr	x0, [x29, 24]
	bl	_crypto_sign_verify_detached
	.loc 1 200 28 discriminator 1
	str	w0, [x29, 60]
	.loc 1 207 7 is_stmt 1
	ldr	w0, [x29, 60]
	cmp	w0, 0
	cset	w0, eq
	and	w0, w0, 255
LBE11:
	.loc 1 208 8
	ldp	x29, x30, [sp], 64
LCFI25:
	ret
LFE8:
	.align	2
	.globl _anubis_types__classical__zeroize_ed25519_secret
_anubis_types__classical__zeroize_ed25519_secret:
LFB9:
	.loc 1 210 4
	sub	x10, sp, #16384
	str	xzr, [x10, 4048]
	stp	x29, x30, [sp, -48]!
LCFI26:
	mov	x29, sp
LCFI27:
	str	x0, [x29, 24]
	.loc 1 213 7
	ldr	x0, [x29, 24]
	str	x0, [x29, 40]
	.loc 1 215 7
	ldr	x0, [x29, 40]
	mov	x1, 32
	bl	_sodium_memzero
	.loc 1 216 24
	ldr	x0, [x29, 24]
	strb	wzr, [x0, 32]
	.loc 1 217 8
	nop
	ldp	x29, x30, [sp], 48
LCFI28:
	ret
LFE9:
	.align	2
	.globl _anubis_types__classical__xchacha20_generate_key
_anubis_types__classical__xchacha20_generate_key:
LFB10:
	.loc 1 223 4
	sub	x10, sp, #16384
	str	xzr, [x10, 4048]
	sub	sp, sp, #48
LCFI29:
	stp	x29, x30, [sp, 32]
LCFI30:
	add	x29, sp, 32
LCFI31:
	str	x0, [x29, -24]
	.loc 1 223 4
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x1, [x0]
	str	x1, [x29, -8]
	mov	x1, 0
	.loc 1 229 18
	ldr	x0, [x29, -24]
	bl	_crypto_aead_xchacha20poly1305_ietf_keygen
	.loc 1 233 17
	ldr	x0, [x29, -24]
	mov	w1, 1
	strb	w1, [x0, 32]
	.loc 1 234 15
	mov	w0, 1
	strb	w0, [x29, -9]
	.loc 1 235 8
	ldrb	w0, [x29, -9]
	.loc 1 235 8 is_stmt 0 discriminator 2
	mov	w1, w0
	.loc 1 235 8
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x3, [x29, -8]
	ldr	x2, [x0]
	subs	x3, x3, x2
	mov	x2, 0
	beq	L66
	bl	___stack_chk_fail
L66:
	mov	w0, w1
	ldp	x29, x30, [sp, 32]
	add	sp, sp, 48
LCFI32:
	ret
LFE10:
	.align	2
	.globl _anubis_types__classical__xchacha20_encrypt
_anubis_types__classical__xchacha20_encrypt:
LFB11:
	.loc 1 237 4 is_stmt 1
	sub	x10, sp, #16384
	str	xzr, [x10, 3776]
	stp	x29, x30, [sp, -96]!
LCFI33:
	mov	x29, sp
LCFI34:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
	stp	x25, x26, [sp, 64]
	str	x27, [sp, 80]
	sub	sp, sp, #224
LCFI35:
	stp	x0, x1, [x29, -80]
	str	x2, [x29, -88]
	str	x3, [x29, -96]
	stp	x4, x5, [x29, -112]
	str	x6, [x29, -120]
	.loc 1 237 4
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x1, [x0]
	str	x1, [x29, -8]
	mov	x1, 0
	ldr	x0, [x29, -72]
	ldr	w5, [x0]
	ldr	x0, [x29, -72]
	ldr	w4, [x0, 4]
	cmp	w4, w5
	blt	L68
	.loc 1 237 4 is_stmt 0 discriminator 1
	sxtw	x0, w4
	str	x0, [x29, -192]
	asr	x0, x0, 63
	str	x0, [x29, -184]
	sxtw	x0, w5
	str	x0, [x29, -208]
	asr	x0, x0, 63
	str	x0, [x29, -200]
	ldp	x6, x7, [x29, -192]
	mov	x0, x6
	ldp	x2, x3, [x29, -208]
	mov	x1, x2
	subs	x1, x0, x1
	mov	x0, x7
	mov	x2, x3
	sbc	x0, x0, x2
	mov	x20, x1
	mov	x21, x0
	mov	x0, 1
	adds	x1, x20, x0
	mov	x0, 0
	adc	x0, x21, x0
	str	x1, [x29, -144]
	str	x0, [x29, -136]
	b	L69
L68:
	.loc 1 237 4 discriminator 2
	stp	xzr, xzr, [x29, -144]
L69:
LBB12:
	mov	x0, sp
	mov	x20, x0
	.loc 1 237 4 discriminator 4
	ldr	x0, [x29, -104]
	ldr	w0, [x0]
	sxtw	x19, w0
	ldr	x0, [x29, -104]
	ldr	w1, [x0, 4]
	ldr	x0, [x29, -104]
	ldr	w0, [x0]
	cmp	w1, w0
	.loc 1 237 4 discriminator 8
	ldr	x0, [x29, -104]
	ldr	w1, [x0, 4]
	ldr	x0, [x29, -104]
	ldr	w0, [x0]
	cmp	w1, w0
	blt	L73
	.loc 1 237 4 discriminator 9
	ldr	x0, [x29, -104]
	ldr	w0, [x0, 4]
	sxtw	x1, w0
	ldr	x0, [x29, -104]
	ldr	w0, [x0]
	sxtw	x0, w0
	sub	x0, x1, x0
	add	x0, x0, 1
	mov	x14, x0
	mov	x15, 0
	lsr	x0, x14, 61
	lsl	x1, x15, 3
	str	x1, [x29, -152]
	ldr	x1, [x29, -152]
	orr	x0, x0, x1
	str	x0, [x29, -152]
	lsl	x0, x14, 3
	str	x0, [x29, -160]
L73:
	.loc 1 237 4 discriminator 12
	ldr	x0, [x29, -104]
	ldr	w1, [x0, 4]
	ldr	x0, [x29, -104]
	ldr	w0, [x0]
	cmp	w1, w0
	.loc 1 237 4 discriminator 16
	cmp	w4, w5
	.loc 1 237 4 discriminator 20
	cmp	w4, w5
	blt	L79
	.loc 1 237 4 discriminator 21
	sxtw	x1, w4
	sxtw	x0, w5
	sub	x0, x1, x0
	add	x0, x0, 1
	mov	x12, x0
	mov	x13, 0
	lsr	x0, x12, 61
	lsl	x1, x13, 3
	str	x1, [x29, -168]
	ldr	x1, [x29, -168]
	orr	x0, x0, x1
	str	x0, [x29, -168]
	lsl	x0, x12, 3
	str	x0, [x29, -176]
L79:
	.loc 1 237 4 discriminator 24
	cmp	w4, w5
	.loc 1 248 50 is_stmt 1
	ldr	x0, [x29, -144]
	mov	x1, 2147483647
	cmp	x0, x1
	ble	L82
	.loc 1 248 50 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L83
	bl	___stack_chk_fail
L83:
	mov	w1, 248
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L82:
	.loc 1 248 58 is_stmt 1 discriminator 2
	mov	w2, w0
	mov	w1, 2147483632
	cmp	w2, w1
	blt	L84
	.loc 1 248 58 is_stmt 0 discriminator 3
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L85
	bl	___stack_chk_fail
L85:
	mov	w1, 248
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L84:
	.loc 1 248 58 discriminator 4
	add	w0, w0, 16
	.loc 1 248 7 is_stmt 1 discriminator 6
	str	w0, [x29, -48]
	ldr	w0, [x29, -48]
	bic	w0, w0, w0, asr #31
	sxtw	x0, w0
	str	x0, [x29, -24]
	ldr	w0, [x29, -48]
	bic	w0, w0, w0, asr #31
	sxtw	x0, w0
	mov	x10, x0
	mov	x11, 0
	lsr	x0, x10, 61
	lsl	x27, x11, 3
	orr	x27, x0, x27
	lsl	x26, x10, 3
	ldr	w0, [x29, -48]
	bic	w0, w0, w0, asr #31
	sxtw	x0, w0
	mov	x8, x0
	mov	x9, 0
	lsr	x0, x8, 61
	lsl	x17, x9, 3
	orr	x17, x0, x17
	lsl	x16, x8, 3
	ldr	w0, [x29, -48]
	bic	w0, w0, w0, asr #31
	sxtw	x0, w0
	add	x0, x0, 15
	lsr	x0, x0, 4
	lsl	x1, x0, 4
	and	x3, x1, -4096
	sub	x0, sp, #12288
	sub	x2, x0, x3
L86:
	cmp	x0, x2
	beq	L87
	sub	x0, x0, #4096
	str	xzr, [x0]
	b	L86
L87:
	sub	x0, x1, x3
	sub	x0, x2, x0
	str	xzr, [x0]
	sub	sp, sp, x1
	add	x0, sp, 16
	.loc 1 248 7 is_stmt 0 discriminator 7
	str	x0, [x29, -16]
	.loc 1 252 33 is_stmt 1
	ldr	w0, [x29, -48]
	cmp	w0, 0
	bgt	L88
	.loc 1 252 33 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L89
	bl	___stack_chk_fail
L89:
	mov	w1, 252
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L88:
	.loc 1 255 33 is_stmt 1
	cmp	w5, w4
	ble	L90
	.loc 1 255 33 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L91
	bl	___stack_chk_fail
L91:
	mov	w1, 255
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L90:
	.loc 1 251 28 is_stmt 1
	ldr	x1, [x29, -80]
	ldr	x0, [x29, -144]
	cmp	x0, 0
	bge	L92
	.loc 1 251 28 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L93
	bl	___stack_chk_fail
L93:
	mov	w1, 256
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L92:
	.loc 1 251 28 discriminator 2
	ldp	x26, x27, [x29, -144]
	mov	x3, x26
	.loc 1 251 28 discriminator 4
	sub	x0, x29, #32
	ldr	x2, [x29, -88]
	str	x2, [sp, 8]
	ldr	x2, [x29, -96]
	str	x2, [sp]
	mov	x7, 0
	mov	x6, 0
	mov	x5, 0
	mov	x4, x3
	mov	x3, x1
	mov	x2, x0
	ldr	x1, [x29, -120]
	ldr	x0, [x29, -16]
	bl	_crypto_aead_xchacha20poly1305_ietf_encrypt_detached
	.loc 1 251 28 discriminator 5
	str	w0, [x29, -44]
	.loc 1 264 7 is_stmt 1
	ldr	w0, [x29, -44]
	cmp	w0, 0
	bne	L94
	.loc 1 266 47
	mov	x0, x26
	mov	x1, 2147483647
	cmp	x0, x1
	ble	L95
	.loc 1 266 47 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L96
	bl	___stack_chk_fail
L96:
	mov	w1, 266
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L95:
	.loc 1 266 35 is_stmt 1 discriminator 2
	mov	w1, w0
	cmp	w1, 0
	ble	L97
	.loc 1 266 35 is_stmt 0 discriminator 3
	mov	w2, w0
	ldr	w1, [x29, -48]
	cmp	w1, w2
	bge	L97
	.loc 1 266 35 discriminator 5
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L98
	bl	___stack_chk_fail
L98:
	mov	w1, 266
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L97:
	.loc 1 266 24 is_stmt 1 discriminator 6
	mov	w1, w0
	bic	w1, w1, w1, asr #31
	sxtw	x1, w1
	mov	x22, x1
	mov	x23, 0
	lsr	x1, x22, 61
	lsl	x25, x23, 3
	orr	x25, x1, x25
	lsl	x24, x22, 3
	mov	w1, w0
	bic	w1, w1, w1, asr #31
	sxtw	x3, w1
	ldr	x1, [x29, -104]
	ldr	w2, [x1, 4]
	ldr	x1, [x29, -104]
	ldr	w1, [x1]
	cmp	w2, w1
	blt	L99
	.loc 1 266 24 is_stmt 0 discriminator 7
	ldr	x1, [x29, -104]
	ldr	w1, [x1, 4]
	sxtw	x2, w1
	ldr	x1, [x29, -104]
	ldr	w1, [x1]
	sxtw	x1, w1
	sub	x1, x2, x1
	add	x1, x1, 1
	b	L100
L99:
	.loc 1 266 24 discriminator 8
	mov	x1, 0
L100:
	.loc 1 266 24 discriminator 10
	bic	w0, w0, w0, asr #31
	sxtw	x0, w0
	cmp	x1, x0
	beq	L101
	.loc 1 266 24 discriminator 11
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L102
	bl	___stack_chk_fail
L102:
	mov	w1, 266
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Length_Check
L101:
	.loc 1 266 21 is_stmt 1 discriminator 12
	ldr	x0, [x29, -112]
	ldr	x1, [x29, -16]
	mov	x2, x3
	bl	_memcpy
	.loc 1 267 18
	mov	w0, 1
	strb	w0, [x29, -57]
	b	L103
L94:
	.loc 1 270 14
	ldr	x0, [x29, -104]
	ldr	w0, [x0]
	str	w0, [x29, -40]
	ldr	x0, [x29, -104]
	ldr	w0, [x0, 4]
	str	w0, [x29, -36]
	ldr	w1, [x29, -40]
	ldr	w0, [x29, -36]
	cmp	w1, w0
	bgt	L104
LBB13:
	.loc 1 270 14 is_stmt 0 discriminator 1
	ldr	w0, [x29, -40]
	str	w0, [x29, -56]
L105:
	.loc 1 274 28 is_stmt 1
	ldr	x1, [x29, -112]
	ldrsw	x0, [x29, -56]
	sub	x0, x0, x19
	strb	wzr, [x1, x0]
	.loc 1 270 14
	ldr	w1, [x29, -56]
	ldr	w0, [x29, -36]
	cmp	w1, w0
	beq	L104
	.loc 1 270 14 is_stmt 0 discriminator 2
	ldr	w0, [x29, -56]
	add	w0, w0, 1
	str	w0, [x29, -56]
	.loc 1 275 18 is_stmt 1
	b	L105
L104:
LBE13:
LBB14:
	.loc 1 276 14
	mov	w0, 1
	str	w0, [x29, -52]
L107:
	.loc 1 276 14 is_stmt 0 discriminator 1
	ldr	w0, [x29, -52]
	cmp	w0, 16
	bgt	L106
	.loc 1 280 31 is_stmt 1
	ldrsw	x0, [x29, -52]
	ldr	x1, [x29, -120]
	add	x0, x1, x0
	strb	wzr, [x0, -1]
	.loc 1 276 14 discriminator 2
	ldr	w0, [x29, -52]
	add	w0, w0, 1
	str	w0, [x29, -52]
	.loc 1 281 18
	b	L107
L106:
LBE14:
	.loc 1 282 18
	strb	wzr, [x29, -57]
L103:
	.loc 1 286 23
	ldr	w0, [x29, -48]
	cmp	w0, 0
	bgt	L108
	.loc 1 286 23 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L109
	bl	___stack_chk_fail
L109:
	mov	w1, 286
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L108:
	.loc 1 286 63 is_stmt 1 discriminator 2
	ldr	w0, [x29, -48]
	bic	w0, w0, w0, asr #31
	.loc 1 286 7 discriminator 2
	sxtw	x0, w0
	mov	x1, x0
	ldr	x0, [x29, -16]
	bl	_sodium_memzero
	.loc 1 287 8
	mov	sp, x20
LBE12:
	.loc 1 287 8 is_stmt 0 discriminator 1
	ldrb	w0, [x29, -57]
	.loc 1 287 8 discriminator 3
	mov	w1, w0
	.loc 1 287 8
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x3, [x29, -8]
	ldr	x2, [x0]
	subs	x3, x3, x2
	mov	x2, 0
	beq	L111
	bl	___stack_chk_fail
L111:
	mov	w0, w1
	mov	sp, x29
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldr	x27, [sp, 80]
	ldp	x29, x30, [sp], 96
LCFI36:
	ret
LFE11:
	.align	2
	.globl _anubis_types__classical__xchacha20_decrypt
_anubis_types__classical__xchacha20_decrypt:
LFB12:
	.loc 1 289 4 is_stmt 1
	sub	x10, sp, #16384
	str	xzr, [x10, 3904]
	sub	sp, sp, #192
LCFI37:
	stp	x29, x30, [sp, 112]
LCFI38:
	add	x29, sp, 112
LCFI39:
	stp	x19, x20, [sp, 128]
	stp	x21, x22, [sp, 144]
	stp	x23, x24, [sp, 160]
	str	x25, [sp, 176]
LCFI40:
	stp	x0, x1, [x29, -48]
	str	x2, [x29, -56]
	str	x3, [x29, -64]
	str	x4, [x29, -72]
	stp	x5, x6, [x29, -88]
	.loc 1 289 4
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
	blt	L113
	.loc 1 289 4 is_stmt 0 discriminator 1
	sxtw	x2, w0
	mov	x22, x2
	asr	x2, x2, 63
	mov	x23, x2
	sxtw	x2, w1
	mov	x20, x2
	asr	x2, x2, 63
	mov	x21, x2
	subs	x3, x22, x20
	sbc	x2, x23, x21
	mov	x16, x3
	mov	x17, x2
	mov	x2, 1
	adds	x3, x16, x2
	mov	x2, 0
	adc	x2, x17, x2
	mov	x24, x3
	mov	x25, x2
	b	L114
L113:
	.loc 1 289 4 discriminator 2
	mov	x24, 0
	mov	x25, 0
L114:
LBB15:
	.loc 1 289 4 discriminator 4
	ldr	x2, [x29, -80]
	ldr	w2, [x2]
	sxtw	x19, w2
	ldr	x2, [x29, -80]
	ldr	w3, [x2, 4]
	ldr	x2, [x29, -80]
	ldr	w2, [x2]
	cmp	w3, w2
	.loc 1 289 4 discriminator 8
	ldr	x2, [x29, -80]
	ldr	w3, [x2, 4]
	ldr	x2, [x29, -80]
	ldr	w2, [x2]
	cmp	w3, w2
	blt	L118
	.loc 1 289 4 discriminator 9
	ldr	x2, [x29, -80]
	ldr	w2, [x2, 4]
	sxtw	x3, w2
	ldr	x2, [x29, -80]
	ldr	w2, [x2]
	sxtw	x2, w2
	sub	x2, x3, x2
	add	x2, x2, 1
	mov	x10, x2
	mov	x11, 0
	lsr	x2, x10, 61
	lsl	x15, x11, 3
	orr	x15, x2, x15
	lsl	x14, x10, 3
L118:
	.loc 1 289 4 discriminator 12
	ldr	x2, [x29, -80]
	ldr	w3, [x2, 4]
	ldr	x2, [x29, -80]
	ldr	w2, [x2]
	cmp	w3, w2
	.loc 1 289 4 discriminator 16
	cmp	w0, w1
	.loc 1 289 4 discriminator 20
	cmp	w0, w1
	blt	L124
	.loc 1 289 4 discriminator 21
	sxtw	x3, w0
	sxtw	x2, w1
	sub	x2, x3, x2
	add	x2, x2, 1
	mov	x8, x2
	mov	x9, 0
	lsr	x2, x8, 61
	lsl	x13, x9, 3
	orr	x13, x2, x13
	lsl	x12, x8, 3
L124:
	.loc 1 289 4 discriminator 24
	cmp	w0, w1
	.loc 1 301 33 is_stmt 1
	ldr	x2, [x29, -80]
	ldr	w3, [x2]
	ldr	x2, [x29, -80]
	ldr	w2, [x2, 4]
	cmp	w3, w2
	ble	L127
	.loc 1 301 33 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L128
	bl	___stack_chk_fail
L128:
	mov	w1, 301
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L127:
	.loc 1 303 33 is_stmt 1
	cmp	w1, w0
	ble	L129
	.loc 1 303 33 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L130
	bl	___stack_chk_fail
L130:
	mov	w1, 303
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L129:
	.loc 1 300 28 is_stmt 1
	ldr	x0, [x29, -88]
	ldr	x1, [x29, -48]
	mov	x2, x24
	cmp	x2, 0
	bge	L131
	.loc 1 300 28 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L132
	bl	___stack_chk_fail
L132:
	mov	w1, 304
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L131:
	.loc 1 300 28 discriminator 2
	mov	x3, x24
	.loc 1 300 28 discriminator 4
	ldr	x2, [x29, -64]
	str	x2, [sp]
	ldr	x7, [x29, -72]
	mov	x6, 0
	mov	x5, 0
	ldr	x4, [x29, -56]
	mov	x2, x1
	mov	x1, 0
	bl	_crypto_aead_xchacha20poly1305_ietf_decrypt_detached
	.loc 1 300 28 discriminator 5
	str	w0, [x29, -20]
	.loc 1 312 7 is_stmt 1
	ldr	w0, [x29, -20]
	cmp	w0, 0
	bne	L133
	.loc 1 313 18
	mov	w0, 1
	strb	w0, [x29, -25]
	.loc 1 324 8
	b	L140
L133:
	.loc 1 316 14
	ldr	x0, [x29, -80]
	ldr	w0, [x0]
	str	w0, [x29, -16]
	ldr	x0, [x29, -80]
	ldr	w0, [x0, 4]
	str	w0, [x29, -12]
	ldr	w1, [x29, -16]
	ldr	w0, [x29, -12]
	cmp	w1, w0
	bgt	L135
LBB16:
	.loc 1 316 14 is_stmt 0 discriminator 1
	ldr	w0, [x29, -16]
	str	w0, [x29, -24]
L136:
	.loc 1 320 27 is_stmt 1
	ldr	x1, [x29, -88]
	ldrsw	x0, [x29, -24]
	sub	x0, x0, x19
	strb	wzr, [x1, x0]
	.loc 1 316 14
	ldr	w1, [x29, -24]
	ldr	w0, [x29, -12]
	cmp	w1, w0
	beq	L135
	.loc 1 316 14 is_stmt 0 discriminator 2
	ldr	w0, [x29, -24]
	add	w0, w0, 1
	str	w0, [x29, -24]
	.loc 1 321 18 is_stmt 1
	b	L136
L135:
LBE16:
	.loc 1 322 18
	strb	wzr, [x29, -25]
L140:
	.loc 1 324 8
	nop
LBE15:
	ldrb	w0, [x29, -25]
	.loc 1 324 8 is_stmt 0 discriminator 2
	mov	w1, w0
	.loc 1 324 8
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x3, [x29, -8]
	ldr	x2, [x0]
	subs	x3, x3, x2
	mov	x2, 0
	beq	L138
	bl	___stack_chk_fail
L138:
	mov	w0, w1
	ldp	x29, x30, [sp, 112]
	ldp	x19, x20, [sp, 128]
	ldp	x21, x22, [sp, 144]
	ldp	x23, x24, [sp, 160]
	ldr	x25, [sp, 176]
	add	sp, sp, 192
LCFI41:
	ret
LFE12:
	.align	2
	.globl _anubis_types__classical__zeroize_xchacha20_key
_anubis_types__classical__zeroize_xchacha20_key:
LFB13:
	.loc 1 326 4 is_stmt 1
	sub	x10, sp, #16384
	str	xzr, [x10, 4048]
	stp	x29, x30, [sp, -48]!
LCFI42:
	mov	x29, sp
LCFI43:
	str	x0, [x29, 24]
	.loc 1 329 7
	ldr	x0, [x29, 24]
	str	x0, [x29, 40]
	.loc 1 331 7
	ldr	x0, [x29, 40]
	mov	x1, 32
	bl	_sodium_memzero
	.loc 1 332 17
	ldr	x0, [x29, 24]
	strb	wzr, [x0, 32]
	.loc 1 333 8
	nop
	ldp	x29, x30, [sp], 48
LCFI44:
	ret
LFE13:
	.align	2
	.globl _anubis_types__classical__argon2id_derive_key
_anubis_types__classical__argon2id_derive_key:
LFB14:
	.loc 1 339 4
	sub	x10, sp, #16384
	str	xzr, [x10, 3920]
	stp	x29, x30, [sp, -80]!
LCFI45:
	mov	x29, sp
LCFI46:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
	str	x25, [sp, 64]
	sub	sp, sp, #96
LCFI47:
	stp	x0, x1, [x29, -80]
	str	x2, [x29, -88]
	str	x3, [x29, -96]
	.loc 1 339 4
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x1, [x0]
	str	x1, [x29, -8]
	mov	x1, 0
	ldr	x0, [x29, -72]
	ldr	w0, [x0]
	ldr	x1, [x29, -72]
	ldr	w1, [x1, 4]
	cmp	w1, w0
	blt	L144
	.loc 1 339 4 is_stmt 0 discriminator 1
	sxtw	x2, w1
	mov	x22, x2
	asr	x2, x2, 63
	mov	x23, x2
	sxtw	x2, w0
	mov	x20, x2
	asr	x2, x2, 63
	mov	x21, x2
	subs	x3, x22, x20
	sbc	x2, x23, x21
	mov	x16, x3
	mov	x17, x2
	mov	x2, 1
	adds	x3, x16, x2
	mov	x2, 0
	adc	x2, x17, x2
	mov	x24, x3
	mov	x25, x2
	b	L145
L144:
	.loc 1 339 4 discriminator 2
	mov	x24, 0
	mov	x25, 0
L145:
LBB17:
	mov	x2, sp
	mov	x19, x2
	.loc 1 339 4 discriminator 4
	sxtw	x16, w0
	cmp	w1, w0
	.loc 1 339 4 discriminator 8
	cmp	w1, w0
	blt	L149
	.loc 1 339 4 discriminator 9
	sxtw	x3, w1
	sxtw	x2, w0
	sub	x2, x3, x2
	add	x2, x2, 1
	mov	x8, x2
	mov	x9, 0
	lsr	x2, x8, 61
	lsl	x15, x9, 3
	orr	x15, x2, x15
	lsl	x14, x8, 3
L149:
	.loc 1 339 4 discriminator 12
	cmp	w1, w0
	.loc 1 347 7 is_stmt 1
	mov	w2, w24
	str	w2, [x29, -40]
	ldr	w2, [x29, -40]
	bic	w2, w2, w2, asr #31
	sxtw	x2, w2
	str	x2, [x29, -24]
	ldr	w2, [x29, -40]
	bic	w2, w2, w2, asr #31
	sxtw	x2, w2
	mov	x6, x2
	mov	x7, 0
	lsr	x2, x6, 61
	lsl	x13, x7, 3
	orr	x13, x2, x13
	lsl	x12, x6, 3
	ldr	w2, [x29, -40]
	bic	w2, w2, w2, asr #31
	sxtw	x2, w2
	mov	x4, x2
	mov	x5, 0
	lsr	x2, x4, 61
	lsl	x11, x5, 3
	orr	x11, x2, x11
	lsl	x10, x4, 3
	ldr	w2, [x29, -40]
	bic	w2, w2, w2, asr #31
	sxtw	x2, w2
	add	x2, x2, 15
	lsr	x2, x2, 4
	lsl	x3, x2, 4
	and	x5, x3, -4096
	sub	x2, sp, #12288
	sub	x4, x2, x5
L152:
	cmp	x2, x4
	beq	L153
	sub	x2, x2, #4096
	str	xzr, [x2]
	b	L152
L153:
	sub	x2, x3, x5
	sub	x2, x4, x2
	str	xzr, [x2]
	sub	sp, sp, x3
	mov	x2, sp
	.loc 1 347 7 is_stmt 0 discriminator 1
	str	x2, [x29, -16]
	.loc 1 350 11 is_stmt 1
	str	w0, [x29, -36]
	str	w1, [x29, -32]
	ldr	w2, [x29, -36]
	ldr	w1, [x29, -32]
	cmp	w2, w1
	bgt	L154
LBB18:
	.loc 1 350 11 is_stmt 0 discriminator 1
	ldr	w1, [x29, -36]
	str	w1, [x29, -48]
L164:
LBB19:
	.loc 1 359 24 is_stmt 1
	mov	w2, 0
	ldr	w1, [x29, -48]
	subs	w1, w1, w0
	bvc	L155
	mov	w2, 1
L155:
	mov	w3, w1
	.loc 1 359 24 is_stmt 0 discriminator 1
	mov	w1, w2
	cmp	w1, 0
	beq	L157
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L158
	bl	___stack_chk_fail
L158:
	mov	w1, 359
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L157:
	.loc 1 359 24 discriminator 2
	mov	w1, w3
	.loc 1 359 43 is_stmt 1 discriminator 4
	mov	w2, 2147483647
	cmp	w1, w2
	bne	L159
	.loc 1 359 43 is_stmt 0 discriminator 5
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L160
	bl	___stack_chk_fail
L160:
	mov	w1, 359
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L159:
	.loc 1 359 43 discriminator 6
	add	w1, w1, 1
	.loc 1 359 10 is_stmt 1 discriminator 8
	cmp	w1, 0
	ble	L161
	.loc 1 359 10 is_stmt 0 discriminator 10
	ldr	w2, [x29, -40]
	cmp	w1, w2
	ble	L162
L161:
	.loc 1 359 10 discriminator 11
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L163
	bl	___stack_chk_fail
L163:
	mov	w1, 359
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L162:
	.loc 1 359 48 is_stmt 1 discriminator 12
	ldr	x2, [x29, -80]
	ldrsw	x1, [x29, -48]
	sub	x1, x1, x16
	ldrsb	w2, [x2, x1]
	.loc 1 359 24 discriminator 13
	ldr	w1, [x29, -48]
	sub	w1, w1, w0
	.loc 1 359 43 discriminator 13
	add	w1, w1, 1
	sxtw	x1, w1
	.loc 1 359 48 discriminator 13
	and	w3, w2, 255
	ldr	x2, [x29, -16]
	add	x1, x2, x1
	mov	w2, w3
	strb	w2, [x1, -1]
LBE19:
	.loc 1 350 11
	ldr	w2, [x29, -48]
	ldr	w1, [x29, -32]
	cmp	w2, w1
	beq	L154
	.loc 1 350 11 is_stmt 0 discriminator 2
	ldr	w1, [x29, -48]
	add	w1, w1, 1
	str	w1, [x29, -48]
	.loc 1 360 15 is_stmt 1
	b	L164
L154:
LBE18:
	.loc 1 366 26
	ldr	w0, [x29, -40]
	cmp	w0, 0
	bgt	L165
	.loc 1 366 26 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L166
	bl	___stack_chk_fail
L166:
	mov	w1, 366
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L165:
	.loc 1 367 64 is_stmt 1
	ldr	w0, [x29, -40]
	bic	w0, w0, w0, asr #31
	.loc 1 363 30
	sxtw	x0, w0
	mov	w7, 2
	mov	x6, 268435456
	mov	x5, 3
	ldr	x4, [x29, -88]
	mov	x3, x0
	ldr	x2, [x29, -16]
	mov	x1, 32
	ldr	x0, [x29, -96]
	bl	_crypto_pwhash
	.loc 1 363 30 is_stmt 0 discriminator 1
	str	w0, [x29, -28]
	.loc 1 374 7 is_stmt 1
	ldr	w0, [x29, -28]
	cmp	w0, 0
	bne	L167
	.loc 1 375 20
	ldr	x0, [x29, -96]
	mov	w1, 1
	strb	w1, [x0, 32]
	.loc 1 376 18
	mov	w0, 1
	strb	w0, [x29, -49]
	b	L168
L167:
	.loc 1 379 20
	ldr	x0, [x29, -96]
	strb	wzr, [x0, 32]
LBB20:
	.loc 1 380 14
	mov	w0, 1
	str	w0, [x29, -44]
L170:
	.loc 1 380 14 is_stmt 0 discriminator 1
	ldr	w0, [x29, -44]
	cmp	w0, 32
	bgt	L169
	.loc 1 384 26 is_stmt 1
	ldrsw	x0, [x29, -44]
	ldr	x1, [x29, -96]
	add	x0, x1, x0
	strb	wzr, [x0, -1]
	.loc 1 380 14 discriminator 2
	ldr	w0, [x29, -44]
	add	w0, w0, 1
	str	w0, [x29, -44]
	.loc 1 385 18
	b	L170
L169:
LBE20:
	.loc 1 386 18
	strb	wzr, [x29, -49]
L168:
	.loc 1 390 23
	ldr	w0, [x29, -40]
	cmp	w0, 0
	bgt	L171
	.loc 1 390 23 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L172
	bl	___stack_chk_fail
L172:
	mov	w1, 390
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L171:
	.loc 1 390 72 is_stmt 1 discriminator 2
	ldr	w0, [x29, -40]
	bic	w0, w0, w0, asr #31
	.loc 1 390 7 discriminator 2
	sxtw	x0, w0
	mov	x1, x0
	ldr	x0, [x29, -16]
	bl	_sodium_memzero
	.loc 1 391 8
	mov	sp, x19
LBE17:
	.loc 1 391 8 is_stmt 0 discriminator 1
	ldrb	w0, [x29, -49]
	.loc 1 391 8 discriminator 3
	mov	w1, w0
	.loc 1 391 8
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x3, [x29, -8]
	ldr	x2, [x0]
	subs	x3, x3, x2
	mov	x2, 0
	beq	L174
	bl	___stack_chk_fail
L174:
	mov	w0, w1
	mov	sp, x29
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldr	x25, [sp, 64]
	ldp	x29, x30, [sp], 80
LCFI48:
	ret
LFE14:
	.align	2
	.globl _anubis_types__classical__zeroize_argon2_key
_anubis_types__classical__zeroize_argon2_key:
LFB15:
	.loc 1 393 4 is_stmt 1
	sub	x10, sp, #16384
	str	xzr, [x10, 4048]
	stp	x29, x30, [sp, -48]!
LCFI49:
	mov	x29, sp
LCFI50:
	str	x0, [x29, 24]
	.loc 1 396 7
	ldr	x0, [x29, 24]
	str	x0, [x29, 40]
	.loc 1 398 7
	ldr	x0, [x29, 40]
	mov	x1, 32
	bl	_sodium_memzero
	.loc 1 399 17
	ldr	x0, [x29, 24]
	strb	wzr, [x0, 32]
	.loc 1 400 8
	nop
	ldp	x29, x30, [sp], 48
LCFI51:
	ret
LFE15:
	.align	2
	.globl _anubis_types__classical__hkdf_derive
_anubis_types__classical__hkdf_derive:
LFB16:
	.loc 1 406 4
	sub	x10, sp, #16384
	str	xzr, [x10, 3696]
	stp	x29, x30, [sp, -96]!
LCFI52:
	mov	x29, sp
LCFI53:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
	stp	x25, x26, [sp, 64]
	str	x27, [sp, 80]
	sub	sp, sp, #304
LCFI54:
	stp	x0, x1, [x29, -128]
	stp	x2, x3, [x29, -144]
	stp	x4, x5, [x29, -160]
	.loc 1 406 4
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x1, [x0]
	str	x1, [x29, -8]
	mov	x1, 0
	ldr	x0, [x29, -136]
	ldr	w12, [x0]
	ldr	x0, [x29, -136]
	ldr	w13, [x0, 4]
	cmp	w13, w12
	blt	L178
	.loc 1 406 4 is_stmt 0 discriminator 1
	sxtw	x0, w13
	mov	x8, x0
	asr	x0, x0, 63
	mov	x9, x0
	sxtw	x0, w12
	mov	x6, x0
	asr	x0, x0, 63
	mov	x7, x0
	subs	x1, x8, x6
	sbc	x0, x9, x7
	mov	x10, x1
	mov	x11, x0
	mov	x0, 1
	adds	x1, x10, x0
	mov	x0, 0
	adc	x0, x11, x0
	str	x1, [x29, -224]
	str	x0, [x29, -216]
	b	L179
L178:
	.loc 1 406 4 discriminator 2
	stp	xzr, xzr, [x29, -224]
L179:
	.loc 1 406 4 discriminator 4
	ldr	x0, [x29, -120]
	ldr	w6, [x0]
	ldr	x0, [x29, -120]
	ldr	w5, [x0, 4]
	cmp	w5, w6
	blt	L180
	.loc 1 406 4 discriminator 5
	sxtw	x0, w5
	mov	x20, x0
	asr	x0, x0, 63
	mov	x21, x0
	sxtw	x0, w6
	mov	x14, x0
	asr	x0, x0, 63
	mov	x15, x0
	subs	x1, x20, x14
	sbc	x0, x21, x15
	mov	x16, x1
	mov	x17, x0
	mov	x0, 1
	adds	x1, x16, x0
	mov	x0, 0
	adc	x0, x17, x0
	str	x1, [x29, -176]
	str	x0, [x29, -168]
	b	L181
L180:
	.loc 1 406 4 discriminator 6
	stp	xzr, xzr, [x29, -176]
L181:
LBB21:
	mov	x0, sp
	mov	x20, x0
	.loc 1 406 4 discriminator 8
	ldr	x0, [x29, -152]
	ldr	w0, [x0]
	sxtw	x19, w0
	ldr	x0, [x29, -152]
	ldr	w1, [x0, 4]
	ldr	x0, [x29, -152]
	ldr	w0, [x0]
	cmp	w1, w0
	.loc 1 406 4 discriminator 12
	ldr	x0, [x29, -152]
	ldr	w1, [x0, 4]
	ldr	x0, [x29, -152]
	ldr	w0, [x0]
	cmp	w1, w0
	blt	L185
	.loc 1 406 4 discriminator 13
	ldr	x0, [x29, -152]
	ldr	w0, [x0, 4]
	sxtw	x1, w0
	ldr	x0, [x29, -152]
	ldr	w0, [x0]
	sxtw	x0, w0
	sub	x0, x1, x0
	add	x0, x0, 1
	mov	x22, x0
	mov	x23, 0
	lsr	x0, x22, 61
	lsl	x1, x23, 3
	str	x1, [x29, -232]
	ldr	x1, [x29, -232]
	orr	x0, x0, x1
	str	x0, [x29, -232]
	lsl	x0, x22, 3
	str	x0, [x29, -240]
L185:
	.loc 1 406 4 discriminator 16
	ldr	x0, [x29, -152]
	ldr	w1, [x0, 4]
	ldr	x0, [x29, -152]
	ldr	w0, [x0]
	cmp	w1, w0
	.loc 1 406 4 discriminator 20
	sxtw	x4, w12
	cmp	w13, w12
	.loc 1 406 4 discriminator 24
	cmp	w13, w12
	blt	L191
	.loc 1 406 4 discriminator 25
	sxtw	x1, w13
	sxtw	x0, w12
	sub	x0, x1, x0
	add	x0, x0, 1
	sub	x1, x29, #16384
	str	x0, [x1, 16096]
	sub	x0, x29, #16384
	str	xzr, [x0, 16104]
	sub	x0, x29, #512
	ldp	x1, x2, [x0, 224]
	mov	x0, x1
	lsr	x0, x0, 61
	mov	x3, x2
	lsl	x3, x3, 3
	str	x3, [x29, -248]
	ldr	x3, [x29, -248]
	orr	x0, x0, x3
	str	x0, [x29, -248]
	mov	x0, x1
	lsl	x0, x0, 3
	str	x0, [x29, -256]
L191:
	.loc 1 406 4 discriminator 28
	cmp	w13, w12
	.loc 1 406 4 discriminator 32
	cmp	w5, w6
	.loc 1 406 4 discriminator 36
	cmp	w5, w6
	blt	L197
	.loc 1 406 4 discriminator 37
	sxtw	x1, w5
	sxtw	x0, w6
	sub	x0, x1, x0
	add	x0, x0, 1
	sub	x1, x29, #16384
	str	x0, [x1, 16080]
	sub	x0, x29, #16384
	str	xzr, [x0, 16088]
	sub	x0, x29, #512
	ldp	x1, x2, [x0, 208]
	mov	x0, x1
	lsr	x0, x0, 61
	mov	x3, x2
	lsl	x3, x3, 3
	sub	x7, x29, #16384
	str	x3, [x7, 16120]
	sub	x3, x29, #16384
	ldr	x3, [x3, 16120]
	orr	x0, x0, x3
	sub	x3, x29, #16384
	str	x0, [x3, 16120]
	mov	x0, x1
	lsl	x0, x0, 3
	sub	x1, x29, #16384
	str	x0, [x1, 16112]
L197:
	.loc 1 406 4 discriminator 40
	cmp	w5, w6
	.loc 1 416 7 is_stmt 1
	ldr	w0, [x29, -224]
	str	w0, [x29, -88]
	ldr	w0, [x29, -88]
	bic	w0, w0, w0, asr #31
	sxtw	x0, w0
	str	x0, [x29, -56]
	ldr	w0, [x29, -88]
	bic	w0, w0, w0, asr #31
	sxtw	x0, w0
	mov	x26, x0
	mov	x27, 0
	lsr	x0, x26, 61
	lsl	x1, x27, 3
	str	x1, [x29, -184]
	ldr	x1, [x29, -184]
	orr	x0, x0, x1
	str	x0, [x29, -184]
	lsl	x0, x26, 3
	str	x0, [x29, -192]
	ldr	w0, [x29, -88]
	bic	w0, w0, w0, asr #31
	sxtw	x0, w0
	mov	x24, x0
	mov	x25, 0
	lsr	x0, x24, 61
	lsl	x1, x25, 3
	str	x1, [x29, -200]
	ldr	x1, [x29, -200]
	orr	x0, x0, x1
	str	x0, [x29, -200]
	lsl	x0, x24, 3
	str	x0, [x29, -208]
	ldr	w0, [x29, -88]
	bic	w0, w0, w0, asr #31
	sxtw	x0, w0
	add	x0, x0, 15
	lsr	x0, x0, 4
	lsl	x1, x0, 4
	and	x3, x1, -4096
	sub	x0, sp, #12288
	sub	x2, x0, x3
L200:
	cmp	x0, x2
	beq	L201
	sub	x0, x0, #4096
	str	xzr, [x0]
	b	L200
L201:
	sub	x0, x1, x3
	sub	x0, x2, x0
	str	xzr, [x0]
	sub	sp, sp, x1
	mov	x0, sp
	.loc 1 416 7 is_stmt 0 discriminator 1
	str	x0, [x29, -48]
	.loc 1 419 11 is_stmt 1
	str	w12, [x29, -84]
	str	w13, [x29, -80]
	ldr	w1, [x29, -84]
	ldr	w0, [x29, -80]
	cmp	w1, w0
	bgt	L202
LBB22:
	.loc 1 419 11 is_stmt 0 discriminator 1
	ldr	w0, [x29, -84]
	str	w0, [x29, -100]
L212:
LBB23:
	.loc 1 428 23 is_stmt 1
	mov	w1, 0
	ldr	w0, [x29, -100]
	subs	w0, w0, w12
	bvc	L203
	mov	w1, 1
L203:
	mov	w2, w0
	.loc 1 428 23 is_stmt 0 discriminator 1
	mov	w0, w1
	cmp	w0, 0
	beq	L205
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L206
	bl	___stack_chk_fail
L206:
	mov	w1, 428
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L205:
	.loc 1 428 23 discriminator 2
	mov	w0, w2
	.loc 1 428 46 is_stmt 1 discriminator 4
	mov	w1, 2147483647
	cmp	w0, w1
	bne	L207
	.loc 1 428 46 is_stmt 0 discriminator 5
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L208
	bl	___stack_chk_fail
L208:
	mov	w1, 428
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L207:
	.loc 1 428 46 discriminator 6
	add	w0, w0, 1
	.loc 1 428 10 is_stmt 1 discriminator 8
	cmp	w0, 0
	ble	L209
	.loc 1 428 10 is_stmt 0 discriminator 10
	ldr	w1, [x29, -88]
	cmp	w0, w1
	ble	L210
L209:
	.loc 1 428 10 discriminator 11
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L211
	bl	___stack_chk_fail
L211:
	mov	w1, 428
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L210:
	.loc 1 428 51 is_stmt 1 discriminator 12
	ldr	x1, [x29, -144]
	ldrsw	x0, [x29, -100]
	sub	x0, x0, x4
	ldrsb	w1, [x1, x0]
	.loc 1 428 23 discriminator 13
	ldr	w0, [x29, -100]
	sub	w0, w0, w12
	.loc 1 428 46 discriminator 13
	add	w0, w0, 1
	sxtw	x0, w0
	.loc 1 428 51 discriminator 13
	and	w2, w1, 255
	ldr	x1, [x29, -48]
	add	x0, x1, x0
	mov	w1, w2
	strb	w1, [x0, -1]
LBE23:
	.loc 1 419 11
	ldr	w1, [x29, -100]
	ldr	w0, [x29, -80]
	cmp	w1, w0
	beq	L202
	.loc 1 419 11 is_stmt 0 discriminator 2
	ldr	w0, [x29, -100]
	add	w0, w0, 1
	str	w0, [x29, -100]
	.loc 1 429 15 is_stmt 1
	b	L212
L202:
LBE22:
	.loc 1 436 22
	cmp	w6, w5
	ble	L213
	.loc 1 436 22 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L214
	bl	___stack_chk_fail
L214:
	mov	w1, 436
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L213:
	.loc 1 432 27 is_stmt 1
	ldr	x1, [x29, -128]
	ldr	x0, [x29, -176]
	cmp	x0, 0
	bge	L215
	.loc 1 432 27 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L216
	bl	___stack_chk_fail
L216:
	mov	w1, 437
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L215:
	.loc 1 432 27 discriminator 2
	ldr	x2, [x29, -176]
	.loc 1 432 27 discriminator 4
	sub	x0, x29, #40
	mov	x4, x2
	mov	x3, x1
	mov	x2, 0
	mov	x1, 0
	bl	_crypto_kdf_hkdf_sha256_extract
	.loc 1 432 27 discriminator 5
	str	w0, [x29, -76]
	.loc 1 440 7 is_stmt 1
	ldr	w0, [x29, -76]
	cmp	w0, 0
	beq	L217
	.loc 1 442 10
	sub	x0, x29, #40
	mov	x1, 32
	bl	_sodium_memzero
	.loc 1 443 14
	ldr	x0, [x29, -152]
	ldr	w0, [x0]
	str	w0, [x29, -64]
	ldr	x0, [x29, -152]
	ldr	w0, [x0, 4]
	str	w0, [x29, -60]
	ldr	w1, [x29, -64]
	ldr	w0, [x29, -60]
	cmp	w1, w0
	bgt	L218
LBB24:
	.loc 1 443 14 is_stmt 0 discriminator 1
	ldr	w0, [x29, -64]
	str	w0, [x29, -96]
L219:
LBB25:
	.loc 1 450 28 is_stmt 1
	ldr	x1, [x29, -160]
	ldrsw	x0, [x29, -96]
	sub	x0, x0, x19
	strb	wzr, [x1, x0]
LBE25:
	.loc 1 443 14
	ldr	w1, [x29, -96]
	ldr	w0, [x29, -60]
	cmp	w1, w0
	beq	L218
	.loc 1 443 14 is_stmt 0 discriminator 2
	ldr	w0, [x29, -96]
	add	w0, w0, 1
	str	w0, [x29, -96]
	.loc 1 451 18 is_stmt 1
	b	L219
L218:
LBE24:
	.loc 1 452 18
	strb	wzr, [x29, -101]
	.loc 1 453 10
	b	L220
L217:
	.loc 1 458 21
	ldr	x0, [x29, -152]
	ldr	w1, [x0]
	ldr	x0, [x29, -152]
	ldr	w0, [x0, 4]
	cmp	w1, w0
	ble	L221
	.loc 1 458 21 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L222
	bl	___stack_chk_fail
L222:
	mov	w1, 458
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L221:
	.loc 1 460 21 is_stmt 1
	ldr	w0, [x29, -88]
	cmp	w0, 0
	bgt	L223
	.loc 1 460 21 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L224
	bl	___stack_chk_fail
L224:
	mov	w1, 460
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L223:
	.loc 1 457 27 is_stmt 1
	ldr	x5, [x29, -160]
	.loc 1 459 52
	ldr	x0, [x29, -152]
	ldr	w1, [x0, 4]
	ldr	x0, [x29, -152]
	ldr	w0, [x0]
	.loc 1 457 27
	cmp	w1, w0
	blt	L225
	.loc 1 457 27 is_stmt 0 discriminator 1
	ldr	x0, [x29, -152]
	ldr	w0, [x0, 4]
	sxtw	x1, w0
	ldr	x0, [x29, -152]
	ldr	w0, [x0]
	sxtw	x0, w0
	sub	x0, x1, x0
	add	x0, x0, 1
	cmp	x0, 0
	bge	L225
	.loc 1 457 27 discriminator 3
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L226
	bl	___stack_chk_fail
L226:
	mov	w1, 459
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L225:
	.loc 1 459 52 is_stmt 1
	ldr	x0, [x29, -152]
	ldr	w1, [x0, 4]
	ldr	x0, [x29, -152]
	ldr	w0, [x0]
	.loc 1 457 27 discriminator 4
	cmp	w1, w0
	blt	L227
	.loc 1 457 27 is_stmt 0 discriminator 6
	ldr	x0, [x29, -152]
	ldr	w0, [x0, 4]
	sxtw	x1, w0
	ldr	x0, [x29, -152]
	ldr	w0, [x0]
	sxtw	x0, w0
	sub	x0, x1, x0
	add	x0, x0, 1
	b	L228
L227:
	.loc 1 457 27 discriminator 7
	mov	x0, 0
L228:
	.loc 1 461 51 is_stmt 1
	ldr	w1, [x29, -88]
	bic	w1, w1, w1, asr #31
	.loc 1 457 27 discriminator 10
	sxtw	x1, w1
	sub	x2, x29, #40
	mov	x4, x2
	mov	x3, x1
	ldr	x2, [x29, -48]
	mov	x1, x0
	mov	x0, x5
	bl	_crypto_kdf_hkdf_sha256_expand
	.loc 1 457 27 is_stmt 0 discriminator 11
	str	w0, [x29, -76]
	.loc 1 465 7 is_stmt 1
	ldr	w0, [x29, -76]
	cmp	w0, 0
	bne	L229
	.loc 1 466 18
	mov	w0, 1
	strb	w0, [x29, -101]
	b	L230
L229:
	.loc 1 469 14
	ldr	x0, [x29, -152]
	ldr	w0, [x0]
	str	w0, [x29, -72]
	ldr	x0, [x29, -152]
	ldr	w0, [x0, 4]
	str	w0, [x29, -68]
	ldr	w1, [x29, -72]
	ldr	w0, [x29, -68]
	cmp	w1, w0
	bgt	L231
LBB26:
	.loc 1 469 14 is_stmt 0 discriminator 1
	ldr	w0, [x29, -72]
	str	w0, [x29, -92]
L232:
	.loc 1 473 28 is_stmt 1
	ldr	x1, [x29, -160]
	ldrsw	x0, [x29, -92]
	sub	x0, x0, x19
	strb	wzr, [x1, x0]
	.loc 1 469 14
	ldr	w1, [x29, -92]
	ldr	w0, [x29, -68]
	cmp	w1, w0
	beq	L231
	.loc 1 469 14 is_stmt 0 discriminator 2
	ldr	w0, [x29, -92]
	add	w0, w0, 1
	str	w0, [x29, -92]
	.loc 1 474 18 is_stmt 1
	b	L232
L231:
LBE26:
	.loc 1 475 18
	strb	wzr, [x29, -101]
L230:
	.loc 1 479 7
	sub	x0, x29, #40
	mov	x1, 32
	bl	_sodium_memzero
L220:
	.loc 1 480 8
	mov	sp, x20
LBE21:
	.loc 1 480 8 is_stmt 0 discriminator 1
	ldrb	w0, [x29, -101]
	.loc 1 480 8 discriminator 3
	mov	w1, w0
	.loc 1 480 8
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x3, [x29, -8]
	ldr	x2, [x0]
	subs	x3, x3, x2
	mov	x2, 0
	beq	L235
	bl	___stack_chk_fail
L235:
	mov	w0, w1
	mov	sp, x29
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldr	x27, [sp, 80]
	ldp	x29, x30, [sp], 96
LCFI55:
	ret
LFE16:
	.const
	.align	3
lC2:
	.ascii "Failed to initialize libsodium"
	.text
	.align	2
	.globl _anubis_types__classical___elabb
_anubis_types__classical___elabb:
LFB0:
	.loc 1 17 1 is_stmt 1
	sub	x10, sp, #16384
	str	xzr, [x10, 4048]
	stp	x29, x30, [sp, -48]!
LCFI56:
	mov	x29, sp
LCFI57:
	stp	x20, x21, [sp, 16]
LCFI58:
LBB27:
	.loc 1 488 22
	bl	_sodium_init
	.loc 1 488 22 is_stmt 0 discriminator 1
	str	w0, [x29, 44]
	.loc 1 489 7 is_stmt 1
	ldr	w0, [x29, 44]
	cmp	w0, 0
	bge	L238
LBB28:
	.loc 1 490 10
	adrp	x0, lC2@PAGE
	add	x20, x0, lC2@PAGEOFF;
	adrp	x0, lC0@PAGE
	add	x21, x0, lC0@PAGEOFF;
	mov	x1, x20
	mov	x2, x21
	adrp	x0, _program_error@GOTPAGE
	ldr	x0, [x0, _program_error@GOTPAGEOFF]
	bl	___gnat_raise_exception
L238:
LBE28:
LBE27:
	.loc 1 494 17
	nop
	ldp	x20, x21, [sp, 16]
	ldp	x29, x30, [sp], 48
LCFI59:
	ret
LFE0:
	.const
	.align	2
lC0:
	.word	1
	.word	30
	.text
	.globl _anubis_types__classical_E
	.data
	.align	1
_anubis_types__classical_E:
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
	.uleb128 0x40
	.byte	0x4
	.set L$set$5,LCFI1-LCFI0
	.long L$set$5
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$6,LCFI2-LCFI1
	.long L$set$6
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x4
	.set L$set$7,LCFI3-LCFI2
	.long L$set$7
	.byte	0xdd
	.byte	0xde
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
	.quad	LFB3
	.set L$set$10,LFE3-LFB3
	.quad L$set$10
	.byte	0x4
	.set L$set$11,LCFI4-LFB3
	.long L$set$11
	.byte	0xe
	.uleb128 0x50
	.byte	0x4
	.set L$set$12,LCFI5-LCFI4
	.long L$set$12
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$13,LCFI6-LCFI5
	.long L$set$13
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x4
	.set L$set$14,LCFI7-LCFI6
	.long L$set$14
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE2:
LSFDE4:
	.set L$set$15,LEFDE4-LASFDE4
	.long L$set$15
LASFDE4:
	.set L$set$16,Lframe0-Lsection__debug_frame
	.long L$set$16
	.quad	LFB4
	.set L$set$17,LFE4-LFB4
	.quad L$set$17
	.byte	0x4
	.set L$set$18,LCFI8-LFB4
	.long L$set$18
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$19,LCFI9-LCFI8
	.long L$set$19
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$20,LCFI10-LCFI9
	.long L$set$20
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE4:
LSFDE6:
	.set L$set$21,LEFDE6-LASFDE6
	.long L$set$21
LASFDE6:
	.set L$set$22,Lframe0-Lsection__debug_frame
	.long L$set$22
	.quad	LFB5
	.set L$set$23,LFE5-LFB5
	.quad L$set$23
	.byte	0x4
	.set L$set$24,LCFI11-LFB5
	.long L$set$24
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$25,LCFI12-LCFI11
	.long L$set$25
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$26,LCFI13-LCFI12
	.long L$set$26
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE6:
LSFDE8:
	.set L$set$27,LEFDE8-LASFDE8
	.long L$set$27
LASFDE8:
	.set L$set$28,Lframe0-Lsection__debug_frame
	.long L$set$28
	.quad	LFB6
	.set L$set$29,LFE6-LFB6
	.quad L$set$29
	.byte	0x4
	.set L$set$30,LCFI14-LFB6
	.long L$set$30
	.byte	0xe
	.uleb128 0x80
	.byte	0x4
	.set L$set$31,LCFI15-LCFI14
	.long L$set$31
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$32,LCFI16-LCFI15
	.long L$set$32
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x4
	.set L$set$33,LCFI17-LCFI16
	.long L$set$33
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE8:
LSFDE10:
	.set L$set$34,LEFDE10-LASFDE10
	.long L$set$34
LASFDE10:
	.set L$set$35,Lframe0-Lsection__debug_frame
	.long L$set$35
	.quad	LFB7
	.set L$set$36,LFE7-LFB7
	.quad L$set$36
	.byte	0x4
	.set L$set$37,LCFI18-LFB7
	.long L$set$37
	.byte	0xe
	.uleb128 0xd0
	.byte	0x4
	.set L$set$38,LCFI19-LCFI18
	.long L$set$38
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$39,LCFI20-LCFI19
	.long L$set$39
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x30
	.byte	0x4
	.set L$set$40,LCFI21-LCFI20
	.long L$set$40
	.byte	0x93
	.uleb128 0x4
	.byte	0x94
	.uleb128 0x3
	.byte	0x96
	.uleb128 0x2
	.byte	0x97
	.uleb128 0x1
	.byte	0x4
	.set L$set$41,LCFI22-LCFI21
	.long L$set$41
	.byte	0xd6
	.byte	0xd7
	.byte	0xd3
	.byte	0xd4
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE10:
LSFDE12:
	.set L$set$42,LEFDE12-LASFDE12
	.long L$set$42
LASFDE12:
	.set L$set$43,Lframe0-Lsection__debug_frame
	.long L$set$43
	.quad	LFB8
	.set L$set$44,LFE8-LFB8
	.quad L$set$44
	.byte	0x4
	.set L$set$45,LCFI23-LFB8
	.long L$set$45
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$46,LCFI24-LCFI23
	.long L$set$46
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$47,LCFI25-LCFI24
	.long L$set$47
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE12:
LSFDE14:
	.set L$set$48,LEFDE14-LASFDE14
	.long L$set$48
LASFDE14:
	.set L$set$49,Lframe0-Lsection__debug_frame
	.long L$set$49
	.quad	LFB9
	.set L$set$50,LFE9-LFB9
	.quad L$set$50
	.byte	0x4
	.set L$set$51,LCFI26-LFB9
	.long L$set$51
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$52,LCFI27-LCFI26
	.long L$set$52
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$53,LCFI28-LCFI27
	.long L$set$53
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE14:
LSFDE16:
	.set L$set$54,LEFDE16-LASFDE16
	.long L$set$54
LASFDE16:
	.set L$set$55,Lframe0-Lsection__debug_frame
	.long L$set$55
	.quad	LFB10
	.set L$set$56,LFE10-LFB10
	.quad L$set$56
	.byte	0x4
	.set L$set$57,LCFI29-LFB10
	.long L$set$57
	.byte	0xe
	.uleb128 0x30
	.byte	0x4
	.set L$set$58,LCFI30-LCFI29
	.long L$set$58
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$59,LCFI31-LCFI30
	.long L$set$59
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x4
	.set L$set$60,LCFI32-LCFI31
	.long L$set$60
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE16:
LSFDE18:
	.set L$set$61,LEFDE18-LASFDE18
	.long L$set$61
LASFDE18:
	.set L$set$62,Lframe0-Lsection__debug_frame
	.long L$set$62
	.quad	LFB11
	.set L$set$63,LFE11-LFB11
	.quad L$set$63
	.byte	0x4
	.set L$set$64,LCFI33-LFB11
	.long L$set$64
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$65,LCFI34-LCFI33
	.long L$set$65
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$66,LCFI35-LCFI34
	.long L$set$66
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
	.set L$set$67,LCFI36-LCFI35
	.long L$set$67
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
LEFDE18:
LSFDE20:
	.set L$set$68,LEFDE20-LASFDE20
	.long L$set$68
LASFDE20:
	.set L$set$69,Lframe0-Lsection__debug_frame
	.long L$set$69
	.quad	LFB12
	.set L$set$70,LFE12-LFB12
	.quad L$set$70
	.byte	0x4
	.set L$set$71,LCFI37-LFB12
	.long L$set$71
	.byte	0xe
	.uleb128 0xc0
	.byte	0x4
	.set L$set$72,LCFI38-LCFI37
	.long L$set$72
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$73,LCFI39-LCFI38
	.long L$set$73
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x50
	.byte	0x4
	.set L$set$74,LCFI40-LCFI39
	.long L$set$74
	.byte	0x93
	.uleb128 0x8
	.byte	0x94
	.uleb128 0x7
	.byte	0x95
	.uleb128 0x6
	.byte	0x96
	.uleb128 0x5
	.byte	0x97
	.uleb128 0x4
	.byte	0x98
	.uleb128 0x3
	.byte	0x99
	.uleb128 0x2
	.byte	0x4
	.set L$set$75,LCFI41-LCFI40
	.long L$set$75
	.byte	0xd9
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
LEFDE20:
LSFDE22:
	.set L$set$76,LEFDE22-LASFDE22
	.long L$set$76
LASFDE22:
	.set L$set$77,Lframe0-Lsection__debug_frame
	.long L$set$77
	.quad	LFB13
	.set L$set$78,LFE13-LFB13
	.quad L$set$78
	.byte	0x4
	.set L$set$79,LCFI42-LFB13
	.long L$set$79
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$80,LCFI43-LCFI42
	.long L$set$80
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$81,LCFI44-LCFI43
	.long L$set$81
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE22:
LSFDE24:
	.set L$set$82,LEFDE24-LASFDE24
	.long L$set$82
LASFDE24:
	.set L$set$83,Lframe0-Lsection__debug_frame
	.long L$set$83
	.quad	LFB14
	.set L$set$84,LFE14-LFB14
	.quad L$set$84
	.byte	0x4
	.set L$set$85,LCFI45-LFB14
	.long L$set$85
	.byte	0xe
	.uleb128 0x50
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$86,LCFI46-LCFI45
	.long L$set$86
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$87,LCFI47-LCFI46
	.long L$set$87
	.byte	0x93
	.uleb128 0x8
	.byte	0x94
	.uleb128 0x7
	.byte	0x95
	.uleb128 0x6
	.byte	0x96
	.uleb128 0x5
	.byte	0x97
	.uleb128 0x4
	.byte	0x98
	.uleb128 0x3
	.byte	0x99
	.uleb128 0x2
	.byte	0x4
	.set L$set$88,LCFI48-LCFI47
	.long L$set$88
	.byte	0xde
	.byte	0xdd
	.byte	0xd9
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
LEFDE24:
LSFDE26:
	.set L$set$89,LEFDE26-LASFDE26
	.long L$set$89
LASFDE26:
	.set L$set$90,Lframe0-Lsection__debug_frame
	.long L$set$90
	.quad	LFB15
	.set L$set$91,LFE15-LFB15
	.quad L$set$91
	.byte	0x4
	.set L$set$92,LCFI49-LFB15
	.long L$set$92
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$93,LCFI50-LCFI49
	.long L$set$93
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$94,LCFI51-LCFI50
	.long L$set$94
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE26:
LSFDE28:
	.set L$set$95,LEFDE28-LASFDE28
	.long L$set$95
LASFDE28:
	.set L$set$96,Lframe0-Lsection__debug_frame
	.long L$set$96
	.quad	LFB16
	.set L$set$97,LFE16-LFB16
	.quad L$set$97
	.byte	0x4
	.set L$set$98,LCFI52-LFB16
	.long L$set$98
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$99,LCFI53-LCFI52
	.long L$set$99
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$100,LCFI54-LCFI53
	.long L$set$100
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
	.set L$set$101,LCFI55-LCFI54
	.long L$set$101
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
LEFDE28:
LSFDE30:
	.set L$set$102,LEFDE30-LASFDE30
	.long L$set$102
LASFDE30:
	.set L$set$103,Lframe0-Lsection__debug_frame
	.long L$set$103
	.quad	LFB0
	.set L$set$104,LFE0-LFB0
	.quad L$set$104
	.byte	0x4
	.set L$set$105,LCFI56-LFB0
	.long L$set$105
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$106,LCFI57-LCFI56
	.long L$set$106
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$107,LCFI58-LCFI57
	.long L$set$107
	.byte	0x94
	.uleb128 0x4
	.byte	0x95
	.uleb128 0x3
	.byte	0x4
	.set L$set$108,LCFI59-LCFI58
	.long L$set$108
	.byte	0xde
	.byte	0xdd
	.byte	0xd4
	.byte	0xd5
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE30:
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
LSFDE33:
	.set L$set$110,LEFDE33-LASFDE33
	.long L$set$110
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB2-.
	.set L$set$111,LFE2-LFB2
	.quad L$set$111
	.uleb128 0
	.byte	0x4
	.set L$set$112,LCFI0-LFB2
	.long L$set$112
	.byte	0xe
	.uleb128 0x40
	.byte	0x4
	.set L$set$113,LCFI1-LCFI0
	.long L$set$113
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$114,LCFI2-LCFI1
	.long L$set$114
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x4
	.set L$set$115,LCFI3-LCFI2
	.long L$set$115
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE33:
LSFDE35:
	.set L$set$116,LEFDE35-LASFDE35
	.long L$set$116
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB3-.
	.set L$set$117,LFE3-LFB3
	.quad L$set$117
	.uleb128 0
	.byte	0x4
	.set L$set$118,LCFI4-LFB3
	.long L$set$118
	.byte	0xe
	.uleb128 0x50
	.byte	0x4
	.set L$set$119,LCFI5-LCFI4
	.long L$set$119
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$120,LCFI6-LCFI5
	.long L$set$120
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x4
	.set L$set$121,LCFI7-LCFI6
	.long L$set$121
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE35:
LSFDE37:
	.set L$set$122,LEFDE37-LASFDE37
	.long L$set$122
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB4-.
	.set L$set$123,LFE4-LFB4
	.quad L$set$123
	.uleb128 0
	.byte	0x4
	.set L$set$124,LCFI8-LFB4
	.long L$set$124
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$125,LCFI9-LCFI8
	.long L$set$125
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$126,LCFI10-LCFI9
	.long L$set$126
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE37:
LSFDE39:
	.set L$set$127,LEFDE39-LASFDE39
	.long L$set$127
LASFDE39:
	.long	LASFDE39-EH_frame1
	.quad	LFB5-.
	.set L$set$128,LFE5-LFB5
	.quad L$set$128
	.uleb128 0
	.byte	0x4
	.set L$set$129,LCFI11-LFB5
	.long L$set$129
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$130,LCFI12-LCFI11
	.long L$set$130
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$131,LCFI13-LCFI12
	.long L$set$131
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE39:
LSFDE41:
	.set L$set$132,LEFDE41-LASFDE41
	.long L$set$132
LASFDE41:
	.long	LASFDE41-EH_frame1
	.quad	LFB6-.
	.set L$set$133,LFE6-LFB6
	.quad L$set$133
	.uleb128 0
	.byte	0x4
	.set L$set$134,LCFI14-LFB6
	.long L$set$134
	.byte	0xe
	.uleb128 0x80
	.byte	0x4
	.set L$set$135,LCFI15-LCFI14
	.long L$set$135
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$136,LCFI16-LCFI15
	.long L$set$136
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x4
	.set L$set$137,LCFI17-LCFI16
	.long L$set$137
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE41:
LSFDE43:
	.set L$set$138,LEFDE43-LASFDE43
	.long L$set$138
LASFDE43:
	.long	LASFDE43-EH_frame1
	.quad	LFB7-.
	.set L$set$139,LFE7-LFB7
	.quad L$set$139
	.uleb128 0
	.byte	0x4
	.set L$set$140,LCFI18-LFB7
	.long L$set$140
	.byte	0xe
	.uleb128 0xd0
	.byte	0x4
	.set L$set$141,LCFI19-LCFI18
	.long L$set$141
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$142,LCFI20-LCFI19
	.long L$set$142
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x30
	.byte	0x4
	.set L$set$143,LCFI21-LCFI20
	.long L$set$143
	.byte	0x93
	.uleb128 0x4
	.byte	0x94
	.uleb128 0x3
	.byte	0x96
	.uleb128 0x2
	.byte	0x97
	.uleb128 0x1
	.byte	0x4
	.set L$set$144,LCFI22-LCFI21
	.long L$set$144
	.byte	0xd6
	.byte	0xd7
	.byte	0xd3
	.byte	0xd4
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE43:
LSFDE45:
	.set L$set$145,LEFDE45-LASFDE45
	.long L$set$145
LASFDE45:
	.long	LASFDE45-EH_frame1
	.quad	LFB8-.
	.set L$set$146,LFE8-LFB8
	.quad L$set$146
	.uleb128 0
	.byte	0x4
	.set L$set$147,LCFI23-LFB8
	.long L$set$147
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$148,LCFI24-LCFI23
	.long L$set$148
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$149,LCFI25-LCFI24
	.long L$set$149
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE45:
LSFDE47:
	.set L$set$150,LEFDE47-LASFDE47
	.long L$set$150
LASFDE47:
	.long	LASFDE47-EH_frame1
	.quad	LFB9-.
	.set L$set$151,LFE9-LFB9
	.quad L$set$151
	.uleb128 0
	.byte	0x4
	.set L$set$152,LCFI26-LFB9
	.long L$set$152
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$153,LCFI27-LCFI26
	.long L$set$153
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$154,LCFI28-LCFI27
	.long L$set$154
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE47:
LSFDE49:
	.set L$set$155,LEFDE49-LASFDE49
	.long L$set$155
LASFDE49:
	.long	LASFDE49-EH_frame1
	.quad	LFB10-.
	.set L$set$156,LFE10-LFB10
	.quad L$set$156
	.uleb128 0
	.byte	0x4
	.set L$set$157,LCFI29-LFB10
	.long L$set$157
	.byte	0xe
	.uleb128 0x30
	.byte	0x4
	.set L$set$158,LCFI30-LCFI29
	.long L$set$158
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$159,LCFI31-LCFI30
	.long L$set$159
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x4
	.set L$set$160,LCFI32-LCFI31
	.long L$set$160
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE49:
LSFDE51:
	.set L$set$161,LEFDE51-LASFDE51
	.long L$set$161
LASFDE51:
	.long	LASFDE51-EH_frame1
	.quad	LFB11-.
	.set L$set$162,LFE11-LFB11
	.quad L$set$162
	.uleb128 0
	.byte	0x4
	.set L$set$163,LCFI33-LFB11
	.long L$set$163
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$164,LCFI34-LCFI33
	.long L$set$164
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$165,LCFI35-LCFI34
	.long L$set$165
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
	.set L$set$166,LCFI36-LCFI35
	.long L$set$166
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
LEFDE51:
LSFDE53:
	.set L$set$167,LEFDE53-LASFDE53
	.long L$set$167
LASFDE53:
	.long	LASFDE53-EH_frame1
	.quad	LFB12-.
	.set L$set$168,LFE12-LFB12
	.quad L$set$168
	.uleb128 0
	.byte	0x4
	.set L$set$169,LCFI37-LFB12
	.long L$set$169
	.byte	0xe
	.uleb128 0xc0
	.byte	0x4
	.set L$set$170,LCFI38-LCFI37
	.long L$set$170
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$171,LCFI39-LCFI38
	.long L$set$171
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x50
	.byte	0x4
	.set L$set$172,LCFI40-LCFI39
	.long L$set$172
	.byte	0x93
	.uleb128 0x8
	.byte	0x94
	.uleb128 0x7
	.byte	0x95
	.uleb128 0x6
	.byte	0x96
	.uleb128 0x5
	.byte	0x97
	.uleb128 0x4
	.byte	0x98
	.uleb128 0x3
	.byte	0x99
	.uleb128 0x2
	.byte	0x4
	.set L$set$173,LCFI41-LCFI40
	.long L$set$173
	.byte	0xd9
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
LEFDE53:
LSFDE55:
	.set L$set$174,LEFDE55-LASFDE55
	.long L$set$174
LASFDE55:
	.long	LASFDE55-EH_frame1
	.quad	LFB13-.
	.set L$set$175,LFE13-LFB13
	.quad L$set$175
	.uleb128 0
	.byte	0x4
	.set L$set$176,LCFI42-LFB13
	.long L$set$176
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$177,LCFI43-LCFI42
	.long L$set$177
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$178,LCFI44-LCFI43
	.long L$set$178
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE55:
LSFDE57:
	.set L$set$179,LEFDE57-LASFDE57
	.long L$set$179
LASFDE57:
	.long	LASFDE57-EH_frame1
	.quad	LFB14-.
	.set L$set$180,LFE14-LFB14
	.quad L$set$180
	.uleb128 0
	.byte	0x4
	.set L$set$181,LCFI45-LFB14
	.long L$set$181
	.byte	0xe
	.uleb128 0x50
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$182,LCFI46-LCFI45
	.long L$set$182
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$183,LCFI47-LCFI46
	.long L$set$183
	.byte	0x93
	.uleb128 0x8
	.byte	0x94
	.uleb128 0x7
	.byte	0x95
	.uleb128 0x6
	.byte	0x96
	.uleb128 0x5
	.byte	0x97
	.uleb128 0x4
	.byte	0x98
	.uleb128 0x3
	.byte	0x99
	.uleb128 0x2
	.byte	0x4
	.set L$set$184,LCFI48-LCFI47
	.long L$set$184
	.byte	0xde
	.byte	0xdd
	.byte	0xd9
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
LEFDE57:
LSFDE59:
	.set L$set$185,LEFDE59-LASFDE59
	.long L$set$185
LASFDE59:
	.long	LASFDE59-EH_frame1
	.quad	LFB15-.
	.set L$set$186,LFE15-LFB15
	.quad L$set$186
	.uleb128 0
	.byte	0x4
	.set L$set$187,LCFI49-LFB15
	.long L$set$187
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$188,LCFI50-LCFI49
	.long L$set$188
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$189,LCFI51-LCFI50
	.long L$set$189
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE59:
LSFDE61:
	.set L$set$190,LEFDE61-LASFDE61
	.long L$set$190
LASFDE61:
	.long	LASFDE61-EH_frame1
	.quad	LFB16-.
	.set L$set$191,LFE16-LFB16
	.quad L$set$191
	.uleb128 0
	.byte	0x4
	.set L$set$192,LCFI52-LFB16
	.long L$set$192
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$193,LCFI53-LCFI52
	.long L$set$193
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$194,LCFI54-LCFI53
	.long L$set$194
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
	.set L$set$195,LCFI55-LCFI54
	.long L$set$195
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
LEFDE61:
LSFDE63:
	.set L$set$196,LEFDE63-LASFDE63
	.long L$set$196
LASFDE63:
	.long	LASFDE63-EH_frame1
	.quad	LFB0-.
	.set L$set$197,LFE0-LFB0
	.quad L$set$197
	.uleb128 0
	.byte	0x4
	.set L$set$198,LCFI56-LFB0
	.long L$set$198
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$199,LCFI57-LCFI56
	.long L$set$199
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$200,LCFI58-LCFI57
	.long L$set$200
	.byte	0x94
	.uleb128 0x4
	.byte	0x95
	.uleb128 0x3
	.byte	0x4
	.set L$set$201,LCFI59-LCFI58
	.long L$set$201
	.byte	0xde
	.byte	0xdd
	.byte	0xd4
	.byte	0xd5
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE63:
	.text
Letext0:
	.file 2 "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_types.ads"
	.file 3 "/Users/sicarii/.local/share/alire/releases/gnatprove_14.1.1_91818ed8/libexec/spark/lib/gcc/aarch64-apple-darwin23.5.0/14.1.0/adainclude/interfac.ads"
	.file 4 "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_types-classical.ads"
	.file 5 "<built-in>"
	.section __DWARF,__debug_info,regular,debug
Lsection__debug_info:
Ldebug_info0:
	.long	0x1b88
	.short	0x4
	.set L$set$202,Ldebug_abbrev0-Lsection__debug_abbrev
	.long L$set$202
	.byte	0x8
	.uleb128 0x1
	.ascii "GNU Ada 14.1.0 -O0 -gnatA -g -gnat2012 -gnatwa -gnatwe -fstack-check=specific -fstack-protector-strong -gnatR2js -gnatws -gnatec=/private/var/folders/bg/pt9l6y1j47q642kp3z5blrmh0000gn/T/GPR.70489/GNAT-TEMP-000003.TMP -gnatem=/private/var/folders/bg/pt9l6y1j47q642kp3z5blrmh0000gn/T/GPR.70489/GNAT-TEMP-000004.TMP -mmacosx-version-min=16.0.0 -mcpu=apple-m1 -mlittle-endian -mabi=lp64 -fPIC\0"
	.byte	0xd
	.ascii "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_types-classical.adb\0"
	.ascii "/Users/sicarii/Desktop/anubis-spark/obj/gnatprove/data_representation\0"
	.quad	Ltext0
	.set L$set$203,Letext0-Ltext0
	.quad L$set$203
	.set L$set$204,Ldebug_line0-Lsection__debug_line
	.long L$set$204
	.uleb128 0x2
	.byte	0x1
	.byte	0x7
	.ascii "interfaces__unsigned_8\0"
	.uleb128 0x3
	.byte	0
	.byte	0xff
	.ascii "anubis_types__byte\0"
	.long	0x26a
	.uleb128 0x4
	.byte	0x1
	.byte	0x7
	.ascii "anubis_types__TbyteB\0"
	.uleb128 0x5
	.ascii "anubis_types__byte_array\0"
	.byte	0x10
	.byte	0x2
	.byte	0x12
	.byte	0x9
	.long	0x2c9
	.uleb128 0x6
	.ascii "P_ARRAY\0"
	.byte	0x3
	.byte	0x46
	.byte	0x1d
	.long	0x2b5
	.byte	0
	.uleb128 0x7
	.byte	0x8
	.long	0x2f1
	.uleb128 0x8
	.set L$set$205,LASF0-Lsection__debug_str
	.long L$set$205
	.byte	0x3
	.byte	0x46
	.byte	0x1d
	.long	0x390
	.byte	0x8
	.byte	0
	.uleb128 0x9
	.long	0x282
	.uleb128 0x9
	.long	0x282
	.uleb128 0x9
	.long	0x282
	.uleb128 0x9
	.long	0x282
	.uleb128 0x9
	.long	0x282
	.uleb128 0x9
	.long	0x282
	.uleb128 0x9
	.long	0x282
	.uleb128 0x9
	.long	0x282
	.uleb128 0xa
	.ascii "anubis_types__byte_array___XUA\0"
	.long	0x250
	.long	0x32f
	.uleb128 0xb
	.long	0x32f
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
	.byte	0x3
	.byte	0x46
	.byte	0x1d
	.long	0x390
	.uleb128 0x6
	.ascii "LB0\0"
	.byte	0x2
	.byte	0x12
	.byte	0x9
	.long	0x36f
	.byte	0
	.uleb128 0xc
	.sleb128 0
	.sleb128 2147483647
	.ascii "natural\0"
	.long	0x32f
	.uleb128 0x6
	.ascii "UB0\0"
	.byte	0x2
	.byte	0x12
	.byte	0x9
	.long	0x36f
	.byte	0x4
	.byte	0
	.uleb128 0x7
	.byte	0x8
	.long	0x33a
	.uleb128 0xa
	.ascii "anubis_types__x25519_public_key__T15s\0"
	.long	0x250
	.long	0x3cc
	.uleb128 0xd
	.long	0x32f
	.sleb128 32
	.byte	0
	.uleb128 0xe
	.ascii "anubis_types__x25519_public_key\0"
	.byte	0x20
	.byte	0x2
	.byte	0x90
	.byte	0x9
	.long	0x403
	.uleb128 0x8
	.set L$set$206,LASF1-Lsection__debug_str
	.long L$set$206
	.byte	0x2
	.byte	0x91
	.byte	0x7
	.long	0x396
	.byte	0
	.byte	0
	.uleb128 0xa
	.ascii "anubis_types__x25519_secret_key__T17s\0"
	.long	0x250
	.long	0x439
	.uleb128 0xd
	.long	0x32f
	.sleb128 32
	.byte	0
	.uleb128 0xe
	.ascii "anubis_types__x25519_secret_key\0"
	.byte	0x21
	.byte	0x2
	.byte	0x94
	.byte	0x9
	.long	0x47d
	.uleb128 0x8
	.set L$set$207,LASF1-Lsection__debug_str
	.long L$set$207
	.byte	0x2
	.byte	0x95
	.byte	0x7
	.long	0x403
	.byte	0
	.uleb128 0x8
	.set L$set$208,LASF2-Lsection__debug_str
	.long L$set$208
	.byte	0x2
	.byte	0x96
	.byte	0x7
	.long	0x47d
	.byte	0x20
	.byte	0
	.uleb128 0x2
	.byte	0x1
	.byte	0x2
	.ascii "boolean\0"
	.uleb128 0xa
	.ascii "anubis_types__x25519_shared_secret__T19s\0"
	.long	0x250
	.long	0x4c1
	.uleb128 0xd
	.long	0x32f
	.sleb128 32
	.byte	0
	.uleb128 0xe
	.ascii "anubis_types__x25519_shared_secret\0"
	.byte	0x21
	.byte	0x2
	.byte	0x99
	.byte	0x9
	.long	0x508
	.uleb128 0x8
	.set L$set$209,LASF1-Lsection__debug_str
	.long L$set$209
	.byte	0x2
	.byte	0x9a
	.byte	0x7
	.long	0x488
	.byte	0
	.uleb128 0x8
	.set L$set$210,LASF2-Lsection__debug_str
	.long L$set$210
	.byte	0x2
	.byte	0x9b
	.byte	0x7
	.long	0x47d
	.byte	0x20
	.byte	0
	.uleb128 0xa
	.ascii "anubis_types__ed25519_public_key__T21s\0"
	.long	0x250
	.long	0x53f
	.uleb128 0xd
	.long	0x32f
	.sleb128 32
	.byte	0
	.uleb128 0xe
	.ascii "anubis_types__ed25519_public_key\0"
	.byte	0x20
	.byte	0x2
	.byte	0x9e
	.byte	0x9
	.long	0x577
	.uleb128 0x8
	.set L$set$211,LASF1-Lsection__debug_str
	.long L$set$211
	.byte	0x2
	.byte	0x9f
	.byte	0x7
	.long	0x508
	.byte	0
	.byte	0
	.uleb128 0xa
	.ascii "anubis_types__ed25519_secret_key__T23s\0"
	.long	0x250
	.long	0x5ae
	.uleb128 0xd
	.long	0x32f
	.sleb128 32
	.byte	0
	.uleb128 0xe
	.ascii "anubis_types__ed25519_secret_key\0"
	.byte	0x21
	.byte	0x2
	.byte	0xa2
	.byte	0x9
	.long	0x5f3
	.uleb128 0x8
	.set L$set$212,LASF1-Lsection__debug_str
	.long L$set$212
	.byte	0x2
	.byte	0xa3
	.byte	0x7
	.long	0x577
	.byte	0
	.uleb128 0x8
	.set L$set$213,LASF2-Lsection__debug_str
	.long L$set$213
	.byte	0x2
	.byte	0xa4
	.byte	0x7
	.long	0x47d
	.byte	0x20
	.byte	0
	.uleb128 0xa
	.ascii "anubis_types__ed25519_signature__T25s\0"
	.long	0x250
	.long	0x62a
	.uleb128 0xd
	.long	0x32f
	.sleb128 64
	.byte	0
	.uleb128 0xe
	.ascii "anubis_types__ed25519_signature\0"
	.byte	0x40
	.byte	0x2
	.byte	0xa7
	.byte	0x9
	.long	0x661
	.uleb128 0x8
	.set L$set$214,LASF1-Lsection__debug_str
	.long L$set$214
	.byte	0x2
	.byte	0xa8
	.byte	0x7
	.long	0x5f3
	.byte	0
	.byte	0
	.uleb128 0xa
	.ascii "anubis_types__xchacha20_key__T27s\0"
	.long	0x250
	.long	0x693
	.uleb128 0xd
	.long	0x32f
	.sleb128 32
	.byte	0
	.uleb128 0xe
	.ascii "anubis_types__xchacha20_key\0"
	.byte	0x21
	.byte	0x2
	.byte	0xab
	.byte	0x9
	.long	0x6d3
	.uleb128 0x8
	.set L$set$215,LASF1-Lsection__debug_str
	.long L$set$215
	.byte	0x2
	.byte	0xac
	.byte	0x7
	.long	0x661
	.byte	0
	.uleb128 0x8
	.set L$set$216,LASF2-Lsection__debug_str
	.long L$set$216
	.byte	0x2
	.byte	0xad
	.byte	0x7
	.long	0x47d
	.byte	0x20
	.byte	0
	.uleb128 0xa
	.ascii "anubis_types__xchacha20_nonce__T29s\0"
	.long	0x250
	.long	0x707
	.uleb128 0xd
	.long	0x32f
	.sleb128 24
	.byte	0
	.uleb128 0xe
	.ascii "anubis_types__xchacha20_nonce\0"
	.byte	0x18
	.byte	0x2
	.byte	0xb0
	.byte	0x9
	.long	0x73c
	.uleb128 0x8
	.set L$set$217,LASF1-Lsection__debug_str
	.long L$set$217
	.byte	0x2
	.byte	0xb1
	.byte	0x7
	.long	0x6d3
	.byte	0
	.byte	0
	.uleb128 0xa
	.ascii "anubis_types__poly1305_tag__T31s\0"
	.long	0x250
	.long	0x76d
	.uleb128 0xd
	.long	0x32f
	.sleb128 16
	.byte	0
	.uleb128 0xe
	.ascii "anubis_types__poly1305_tag\0"
	.byte	0x10
	.byte	0x2
	.byte	0xb4
	.byte	0x9
	.long	0x79f
	.uleb128 0x8
	.set L$set$218,LASF1-Lsection__debug_str
	.long L$set$218
	.byte	0x2
	.byte	0xb5
	.byte	0x7
	.long	0x73c
	.byte	0
	.byte	0
	.uleb128 0xa
	.ascii "anubis_types__argon2_salt__T49s\0"
	.long	0x250
	.long	0x7cf
	.uleb128 0xd
	.long	0x32f
	.sleb128 32
	.byte	0
	.uleb128 0xe
	.ascii "anubis_types__argon2_salt\0"
	.byte	0x20
	.byte	0x2
	.byte	0xdc
	.byte	0x9
	.long	0x800
	.uleb128 0x8
	.set L$set$219,LASF1-Lsection__debug_str
	.long L$set$219
	.byte	0x2
	.byte	0xdd
	.byte	0x7
	.long	0x79f
	.byte	0
	.byte	0
	.uleb128 0xa
	.ascii "anubis_types__argon2_derived_key__T51s\0"
	.long	0x250
	.long	0x837
	.uleb128 0xd
	.long	0x32f
	.sleb128 32
	.byte	0
	.uleb128 0xe
	.ascii "anubis_types__argon2_derived_key\0"
	.byte	0x21
	.byte	0x2
	.byte	0xe0
	.byte	0x9
	.long	0x87c
	.uleb128 0x8
	.set L$set$220,LASF1-Lsection__debug_str
	.long L$set$220
	.byte	0x2
	.byte	0xe1
	.byte	0x7
	.long	0x800
	.byte	0
	.uleb128 0x8
	.set L$set$221,LASF2-Lsection__debug_str
	.long L$set$221
	.byte	0x2
	.byte	0xe2
	.byte	0x7
	.long	0x47d
	.byte	0x20
	.byte	0
	.uleb128 0xf
	.sleb128 -2147483648
	.sleb128 2147483647
	.ascii "interfaces__c__int\0"
	.long	0x89e
	.uleb128 0x4
	.byte	0x4
	.byte	0x5
	.ascii "interfaces__c__TintB\0"
	.uleb128 0x2
	.byte	0x8
	.byte	0x7
	.ascii "system__address\0"
	.uleb128 0x2
	.byte	0x8
	.byte	0x7
	.ascii "interfaces__c__size_t\0"
	.uleb128 0x2
	.byte	0x8
	.byte	0x7
	.ascii "interfaces__c__unsigned_long\0"
	.uleb128 0x2
	.byte	0x1
	.byte	0x8
	.ascii "character\0"
	.uleb128 0x10
	.ascii "anubis_types__classical___elabb\0"
	.byte	0x1
	.byte	0x11
	.byte	0x1
	.quad	LFB0
	.set L$set$222,LFE0-LFB0
	.quad L$set$222
	.uleb128 0x1
	.byte	0x9c
	.long	0x974
	.uleb128 0x11
	.quad	LBB27
	.set L$set$223,LBE27-LBB27
	.quad L$set$223
	.uleb128 0x12
	.ascii "init_result\0"
	.byte	0x1
	.short	0x1e6
	.byte	0x7
	.long	0x87c
	.uleb128 0x2
	.byte	0x91
	.sleb128 -4
	.byte	0
	.byte	0
	.uleb128 0x13
	.ascii "anubis_types__classical__hkdf_derive\0"
	.byte	0x1
	.short	0x196
	.byte	0x4
	.long	0x47d
	.quad	LFB16
	.set L$set$224,LFE16-LFB16
	.quad L$set$224
	.uleb128 0x1
	.byte	0x9c
	.long	0xd12
	.uleb128 0x14
	.ascii "input_key_material\0"
	.byte	0x4
	.byte	0xaa
	.byte	0x7
	.long	0x2ec
	.uleb128 0x3
	.byte	0x91
	.sleb128 -224
	.uleb128 0x14
	.ascii "context_string\0"
	.byte	0x4
	.byte	0xab
	.byte	0x7
	.long	0xd49
	.uleb128 0x3
	.byte	0x91
	.sleb128 -240
	.uleb128 0x14
	.ascii "output_key\0"
	.byte	0x4
	.byte	0xac
	.byte	0x7
	.long	0x2e7
	.uleb128 0x3
	.byte	0x91
	.sleb128 -256
	.uleb128 0x15
	.set L$set$225,LASF3-Lsection__debug_str
	.long L$set$225
	.byte	0x4
	.byte	0xad
	.byte	0x7
	.long	0x47d
	.uleb128 0x3
	.byte	0x91
	.sleb128 -197
	.uleb128 0x11
	.quad	LBB21
	.set L$set$226,LBE21-LBB21
	.quad L$set$226
	.uleb128 0x16
	.set L$set$227,LASF4-Lsection__debug_str
	.long L$set$227
	.byte	0x1
	.short	0x19c
	.byte	0x7
	.long	0x87c
	.uleb128 0x3
	.byte	0x91
	.sleb128 -172
	.uleb128 0xa
	.ascii "anubis_types__classical__hkdf_derive__TprkT113b\0"
	.long	0x250
	.long	0xa7b
	.uleb128 0xd
	.long	0x32f
	.sleb128 32
	.byte	0
	.uleb128 0x12
	.ascii "prk\0"
	.byte	0x1
	.short	0x19e
	.byte	0x7
	.long	0xa3b
	.uleb128 0x3
	.byte	0x91
	.sleb128 -136
	.uleb128 0x17
	.ascii "anubis_types__classical__hkdf_derive__TTctx_bytesSP1___U\0"
	.long	0x32f
	.uleb128 0x3
	.byte	0x91
	.sleb128 -184
	.uleb128 0xa
	.ascii "anubis_types__classical__hkdf_derive__Tctx_bytesS\0"
	.long	0x250
	.long	0xb13
	.uleb128 0x18
	.long	0x32f
	.long	0xa8c
	.byte	0
	.uleb128 0x12
	.ascii "ctx_bytes\0"
	.byte	0x1
	.short	0x1a0
	.byte	0x7
	.long	0xace
	.uleb128 0x4
	.byte	0x91
	.sleb128 -144
	.byte	0x6
	.uleb128 0x17
	.ascii "anubis_types__classical__hkdf_derive__L_10__T116b___L\0"
	.long	0x32f
	.uleb128 0x3
	.byte	0x91
	.sleb128 -180
	.uleb128 0x17
	.ascii "anubis_types__classical__hkdf_derive__L_10__T116b___U\0"
	.long	0x32f
	.uleb128 0x3
	.byte	0x91
	.sleb128 -176
	.uleb128 0x17
	.ascii "anubis_types__classical__hkdf_derive__L_11__T127b___L\0"
	.long	0x32f
	.uleb128 0x3
	.byte	0x91
	.sleb128 -160
	.uleb128 0x17
	.ascii "anubis_types__classical__hkdf_derive__L_11__T127b___U\0"
	.long	0x32f
	.uleb128 0x3
	.byte	0x91
	.sleb128 -156
	.uleb128 0x17
	.ascii "anubis_types__classical__hkdf_derive__L_12__T139b___L\0"
	.long	0x32f
	.uleb128 0x3
	.byte	0x91
	.sleb128 -168
	.uleb128 0x17
	.ascii "anubis_types__classical__hkdf_derive__L_12__T139b___U\0"
	.long	0x32f
	.uleb128 0x3
	.byte	0x91
	.sleb128 -164
	.uleb128 0x19
	.quad	LBB22
	.set L$set$228,LBE22-LBB22
	.quad L$set$228
	.long	0xcca
	.uleb128 0x12
	.ascii "i\0"
	.byte	0x1
	.short	0x1a3
	.byte	0xb
	.long	0x32f
	.uleb128 0x3
	.byte	0x91
	.sleb128 -196
	.byte	0
	.uleb128 0x19
	.quad	LBB24
	.set L$set$229,LBE24-LBB24
	.quad L$set$229
	.long	0xcef
	.uleb128 0x12
	.ascii "i\0"
	.byte	0x1
	.short	0x1bb
	.byte	0xe
	.long	0x32f
	.uleb128 0x3
	.byte	0x91
	.sleb128 -192
	.byte	0
	.uleb128 0x11
	.quad	LBB26
	.set L$set$230,LBE26-LBB26
	.quad L$set$230
	.uleb128 0x12
	.ascii "i\0"
	.byte	0x1
	.short	0x1d5
	.byte	0xe
	.long	0x32f
	.uleb128 0x3
	.byte	0x91
	.sleb128 -188
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0x1a
	.ascii "string\0"
	.byte	0x10
	.byte	0x5
	.byte	0
	.long	0xd44
	.uleb128 0x1b
	.ascii "P_ARRAY\0"
	.byte	0x5
	.byte	0
	.long	0xd31
	.byte	0
	.uleb128 0x7
	.byte	0x8
	.long	0xd4e
	.uleb128 0x1c
	.set L$set$231,LASF0-Lsection__debug_str
	.long L$set$231
	.byte	0x5
	.byte	0
	.long	0xdbb
	.byte	0x8
	.byte	0
	.uleb128 0x9
	.long	0xd12
	.uleb128 0x9
	.long	0xd12
	.uleb128 0xa
	.ascii "string___XUA\0"
	.long	0x902
	.long	0xd7a
	.uleb128 0xb
	.long	0x32f
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
	.uleb128 0x1a
	.ascii "string___XUB\0"
	.byte	0x8
	.byte	0x5
	.byte	0
	.long	0xdbb
	.uleb128 0x1b
	.ascii "LB0\0"
	.byte	0x5
	.byte	0
	.long	0xd9b
	.byte	0
	.uleb128 0x1d
	.sleb128 2147483647
	.ascii "positive\0"
	.long	0x32f
	.uleb128 0x1b
	.ascii "UB0\0"
	.byte	0x5
	.byte	0
	.long	0xd9b
	.byte	0x4
	.byte	0
	.uleb128 0x7
	.byte	0x8
	.long	0xd7a
	.uleb128 0x1e
	.ascii "anubis_types__classical__zeroize_argon2_key\0"
	.byte	0x1
	.short	0x189
	.byte	0x4
	.quad	LFB15
	.set L$set$232,LFE15-LFB15
	.quad L$set$232
	.uleb128 0x1
	.byte	0x9c
	.long	0xe28
	.uleb128 0x14
	.ascii "key\0"
	.byte	0x4
	.byte	0x9f
	.byte	0x7
	.long	0xe28
	.uleb128 0x2
	.byte	0x91
	.sleb128 -24
	.uleb128 0x16
	.set L$set$233,LASF5-Lsection__debug_str
	.long L$set$233
	.byte	0x1
	.short	0x18c
	.byte	0x7
	.long	0x8b6
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.byte	0
	.uleb128 0x1f
	.byte	0x8
	.long	0x837
	.uleb128 0x13
	.ascii "anubis_types__classical__argon2id_derive_key\0"
	.byte	0x1
	.short	0x153
	.byte	0x4
	.long	0x47d
	.quad	LFB14
	.set L$set$234,LFE14-LFB14
	.quad L$set$234
	.uleb128 0x1
	.byte	0x9c
	.long	0x1068
	.uleb128 0x14
	.ascii "passphrase\0"
	.byte	0x4
	.byte	0x94
	.byte	0x7
	.long	0xd44
	.uleb128 0x3
	.byte	0x91
	.sleb128 -160
	.uleb128 0x14
	.ascii "salt\0"
	.byte	0x4
	.byte	0x95
	.byte	0x7
	.long	0x1068
	.uleb128 0x3
	.byte	0x91
	.sleb128 -168
	.uleb128 0x14
	.ascii "key\0"
	.byte	0x4
	.byte	0x96
	.byte	0x7
	.long	0xe28
	.uleb128 0x3
	.byte	0x91
	.sleb128 -176
	.uleb128 0x15
	.set L$set$235,LASF3-Lsection__debug_str
	.long L$set$235
	.byte	0x4
	.byte	0x97
	.byte	0x7
	.long	0x47d
	.uleb128 0x3
	.byte	0x91
	.sleb128 -129
	.uleb128 0x11
	.quad	LBB17
	.set L$set$236,LBE17-LBB17
	.quad L$set$236
	.uleb128 0x16
	.set L$set$237,LASF4-Lsection__debug_str
	.long L$set$237
	.byte	0x1
	.short	0x159
	.byte	0x7
	.long	0x87c
	.uleb128 0x3
	.byte	0x91
	.sleb128 -108
	.uleb128 0x17
	.ascii "anubis_types__classical__argon2id_derive_key__TTpass_bytesSP1___U\0"
	.long	0x32f
	.uleb128 0x3
	.byte	0x91
	.sleb128 -120
	.uleb128 0xa
	.ascii "anubis_types__classical__argon2id_derive_key__Tpass_bytesS\0"
	.long	0x250
	.long	0xf7d
	.uleb128 0x18
	.long	0x32f
	.long	0xee4
	.byte	0
	.uleb128 0x12
	.ascii "pass_bytes\0"
	.byte	0x1
	.short	0x15b
	.byte	0x7
	.long	0xf2f
	.uleb128 0x4
	.byte	0x91
	.sleb128 -96
	.byte	0x6
	.uleb128 0x17
	.ascii "anubis_types__classical__argon2id_derive_key__L_8__T90b___L\0"
	.long	0x32f
	.uleb128 0x3
	.byte	0x91
	.sleb128 -116
	.uleb128 0x17
	.ascii "anubis_types__classical__argon2id_derive_key__L_8__T90b___U\0"
	.long	0x32f
	.uleb128 0x3
	.byte	0x91
	.sleb128 -112
	.uleb128 0x19
	.quad	LBB18
	.set L$set$238,LBE18-LBB18
	.quad L$set$238
	.long	0x1045
	.uleb128 0x12
	.ascii "i\0"
	.byte	0x1
	.short	0x15e
	.byte	0xb
	.long	0x32f
	.uleb128 0x3
	.byte	0x91
	.sleb128 -128
	.byte	0
	.uleb128 0x11
	.quad	LBB20
	.set L$set$239,LBE20-LBB20
	.quad L$set$239
	.uleb128 0x12
	.ascii "i\0"
	.byte	0x1
	.short	0x17c
	.byte	0xe
	.long	0x32f
	.uleb128 0x3
	.byte	0x91
	.sleb128 -124
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0x1f
	.byte	0x8
	.long	0x7cf
	.uleb128 0x1e
	.ascii "anubis_types__classical__zeroize_xchacha20_key\0"
	.byte	0x1
	.short	0x146
	.byte	0x4
	.quad	LFB13
	.set L$set$240,LFE13-LFB13
	.quad L$set$240
	.uleb128 0x1
	.byte	0x9c
	.long	0x10d8
	.uleb128 0x14
	.ascii "key\0"
	.byte	0x4
	.byte	0x8a
	.byte	0x7
	.long	0x10d8
	.uleb128 0x2
	.byte	0x91
	.sleb128 -24
	.uleb128 0x16
	.set L$set$241,LASF5-Lsection__debug_str
	.long L$set$241
	.byte	0x1
	.short	0x149
	.byte	0x7
	.long	0x8b6
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.byte	0
	.uleb128 0x1f
	.byte	0x8
	.long	0x693
	.uleb128 0x13
	.ascii "anubis_types__classical__xchacha20_decrypt\0"
	.byte	0x1
	.short	0x121
	.byte	0x4
	.long	0x47d
	.quad	LFB12
	.set L$set$242,LFE12-LFB12
	.quad L$set$242
	.uleb128 0x1
	.byte	0x9c
	.long	0x1255
	.uleb128 0x15
	.set L$set$243,LASF6-Lsection__debug_str
	.long L$set$243
	.byte	0x4
	.byte	0x7c
	.byte	0x7
	.long	0x2e2
	.uleb128 0x3
	.byte	0x91
	.sleb128 -128
	.uleb128 0x15
	.set L$set$244,LASF7-Lsection__debug_str
	.long L$set$244
	.byte	0x4
	.byte	0x7d
	.byte	0x7
	.long	0x1255
	.uleb128 0x3
	.byte	0x91
	.sleb128 -136
	.uleb128 0x14
	.ascii "key\0"
	.byte	0x4
	.byte	0x7e
	.byte	0x7
	.long	0x10d8
	.uleb128 0x3
	.byte	0x91
	.sleb128 -144
	.uleb128 0x14
	.ascii "nonce\0"
	.byte	0x4
	.byte	0x7f
	.byte	0x7
	.long	0x125b
	.uleb128 0x3
	.byte	0x91
	.sleb128 -152
	.uleb128 0x15
	.set L$set$245,LASF8-Lsection__debug_str
	.long L$set$245
	.byte	0x4
	.byte	0x80
	.byte	0x7
	.long	0x2dd
	.uleb128 0x3
	.byte	0x91
	.sleb128 -168
	.uleb128 0x15
	.set L$set$246,LASF3-Lsection__debug_str
	.long L$set$246
	.byte	0x4
	.byte	0x81
	.byte	0x7
	.long	0x47d
	.uleb128 0x3
	.byte	0x91
	.sleb128 -105
	.uleb128 0x11
	.quad	LBB15
	.set L$set$247,LBE15-LBB15
	.quad L$set$247
	.uleb128 0x16
	.set L$set$248,LASF4-Lsection__debug_str
	.long L$set$248
	.byte	0x1
	.short	0x129
	.byte	0x7
	.long	0x87c
	.uleb128 0x3
	.byte	0x91
	.sleb128 -100
	.uleb128 0x17
	.ascii "anubis_types__classical__xchacha20_decrypt__L_7__T80b___L\0"
	.long	0x32f
	.uleb128 0x3
	.byte	0x91
	.sleb128 -96
	.uleb128 0x17
	.ascii "anubis_types__classical__xchacha20_decrypt__L_7__T80b___U\0"
	.long	0x32f
	.uleb128 0x3
	.byte	0x91
	.sleb128 -92
	.uleb128 0x11
	.quad	LBB16
	.set L$set$249,LBE16-LBB16
	.quad L$set$249
	.uleb128 0x12
	.ascii "i\0"
	.byte	0x1
	.short	0x13c
	.byte	0xe
	.long	0x32f
	.uleb128 0x3
	.byte	0x91
	.sleb128 -104
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0x1f
	.byte	0x8
	.long	0x76d
	.uleb128 0x1f
	.byte	0x8
	.long	0x707
	.uleb128 0x20
	.ascii "anubis_types__classical__xchacha20_encrypt\0"
	.byte	0x1
	.byte	0xed
	.byte	0x4
	.long	0x47d
	.quad	LFB11
	.set L$set$250,LFE11-LFB11
	.quad L$set$250
	.uleb128 0x1
	.byte	0x9c
	.long	0x14ba
	.uleb128 0x15
	.set L$set$251,LASF8-Lsection__debug_str
	.long L$set$251
	.byte	0x4
	.byte	0x70
	.byte	0x7
	.long	0x2d8
	.uleb128 0x3
	.byte	0x91
	.sleb128 -176
	.uleb128 0x14
	.ascii "key\0"
	.byte	0x4
	.byte	0x71
	.byte	0x7
	.long	0x10d8
	.uleb128 0x3
	.byte	0x91
	.sleb128 -184
	.uleb128 0x14
	.ascii "nonce\0"
	.byte	0x4
	.byte	0x72
	.byte	0x7
	.long	0x125b
	.uleb128 0x3
	.byte	0x91
	.sleb128 -192
	.uleb128 0x15
	.set L$set$252,LASF6-Lsection__debug_str
	.long L$set$252
	.byte	0x4
	.byte	0x73
	.byte	0x7
	.long	0x2d3
	.uleb128 0x3
	.byte	0x91
	.sleb128 -208
	.uleb128 0x15
	.set L$set$253,LASF7-Lsection__debug_str
	.long L$set$253
	.byte	0x4
	.byte	0x74
	.byte	0x7
	.long	0x1255
	.uleb128 0x3
	.byte	0x91
	.sleb128 -216
	.uleb128 0x15
	.set L$set$254,LASF3-Lsection__debug_str
	.long L$set$254
	.byte	0x4
	.byte	0x75
	.byte	0x7
	.long	0x47d
	.uleb128 0x3
	.byte	0x91
	.sleb128 -153
	.uleb128 0x11
	.quad	LBB12
	.set L$set$255,LBE12-LBB12
	.quad L$set$255
	.uleb128 0x21
	.set L$set$256,LASF4-Lsection__debug_str
	.long L$set$256
	.byte	0x1
	.byte	0xf5
	.byte	0x7
	.long	0x87c
	.uleb128 0x3
	.byte	0x91
	.sleb128 -140
	.uleb128 0x22
	.ascii "ciphertext_len\0"
	.byte	0x1
	.byte	0xf6
	.byte	0x7
	.long	0x8e2
	.uleb128 0x3
	.byte	0x91
	.sleb128 -128
	.uleb128 0x17
	.ascii "anubis_types__classical__xchacha20_encrypt__TTtemp_ctSP1___U\0"
	.long	0x32f
	.uleb128 0x3
	.byte	0x91
	.sleb128 -144
	.uleb128 0xa
	.ascii "anubis_types__classical__xchacha20_encrypt__Ttemp_ctS\0"
	.long	0x250
	.long	0x13d7
	.uleb128 0x18
	.long	0x32f
	.long	0x1348
	.byte	0
	.uleb128 0x22
	.ascii "temp_ct\0"
	.byte	0x1
	.byte	0xf8
	.byte	0x7
	.long	0x138e
	.uleb128 0x4
	.byte	0x91
	.sleb128 -112
	.byte	0x6
	.uleb128 0x17
	.ascii "anubis_types__classical__xchacha20_encrypt__L_5__T60b___L\0"
	.long	0x32f
	.uleb128 0x3
	.byte	0x91
	.sleb128 -136
	.uleb128 0x17
	.ascii "anubis_types__classical__xchacha20_encrypt__L_5__T60b___U\0"
	.long	0x32f
	.uleb128 0x3
	.byte	0x91
	.sleb128 -132
	.uleb128 0x19
	.quad	LBB13
	.set L$set$257,LBE13-LBB13
	.quad L$set$257
	.long	0x1497
	.uleb128 0x12
	.ascii "i\0"
	.byte	0x1
	.short	0x10e
	.byte	0xe
	.long	0x32f
	.uleb128 0x3
	.byte	0x91
	.sleb128 -152
	.byte	0
	.uleb128 0x11
	.quad	LBB14
	.set L$set$258,LBE14-LBB14
	.quad L$set$258
	.uleb128 0x12
	.ascii "i\0"
	.byte	0x1
	.short	0x114
	.byte	0xe
	.long	0x32f
	.uleb128 0x3
	.byte	0x91
	.sleb128 -148
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0x20
	.ascii "anubis_types__classical__xchacha20_generate_key\0"
	.byte	0x1
	.byte	0xdf
	.byte	0x4
	.long	0x47d
	.quad	LFB10
	.set L$set$259,LFE10-LFB10
	.quad L$set$259
	.uleb128 0x1
	.byte	0x9c
	.long	0x1527
	.uleb128 0x14
	.ascii "key\0"
	.byte	0x4
	.byte	0x67
	.byte	0x7
	.long	0x10d8
	.uleb128 0x2
	.byte	0x91
	.sleb128 -40
	.uleb128 0x15
	.set L$set$260,LASF3-Lsection__debug_str
	.long L$set$260
	.byte	0x4
	.byte	0x68
	.byte	0x7
	.long	0x47d
	.uleb128 0x2
	.byte	0x91
	.sleb128 -25
	.byte	0
	.uleb128 0x10
	.ascii "anubis_types__classical__zeroize_ed25519_secret\0"
	.byte	0x1
	.byte	0xd2
	.byte	0x4
	.quad	LFB9
	.set L$set$261,LFE9-LFB9
	.quad L$set$261
	.uleb128 0x1
	.byte	0x9c
	.long	0x1590
	.uleb128 0x15
	.set L$set$262,LASF9-Lsection__debug_str
	.long L$set$262
	.byte	0x4
	.byte	0x5d
	.byte	0x7
	.long	0x1590
	.uleb128 0x2
	.byte	0x91
	.sleb128 -24
	.uleb128 0x21
	.set L$set$263,LASF5-Lsection__debug_str
	.long L$set$263
	.byte	0x1
	.byte	0xd5
	.byte	0x7
	.long	0x8b6
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.byte	0
	.uleb128 0x1f
	.byte	0x8
	.long	0x5ae
	.uleb128 0x20
	.ascii "anubis_types__classical__ed25519_verify\0"
	.byte	0x1
	.byte	0xbf
	.byte	0x4
	.long	0x47d
	.quad	LFB8
	.set L$set$264,LFE8-LFB8
	.quad L$set$264
	.uleb128 0x1
	.byte	0x9c
	.long	0x162f
	.uleb128 0x14
	.ascii "message\0"
	.byte	0x4
	.byte	0x56
	.byte	0x7
	.long	0x2ce
	.uleb128 0x2
	.byte	0x91
	.sleb128 -32
	.uleb128 0x15
	.set L$set$265,LASF10-Lsection__debug_str
	.long L$set$265
	.byte	0x4
	.byte	0x57
	.byte	0x7
	.long	0x162f
	.uleb128 0x2
	.byte	0x91
	.sleb128 -40
	.uleb128 0x15
	.set L$set$266,LASF11-Lsection__debug_str
	.long L$set$266
	.byte	0x4
	.byte	0x58
	.byte	0x7
	.long	0x1635
	.uleb128 0x2
	.byte	0x91
	.sleb128 -48
	.uleb128 0x11
	.quad	LBB11
	.set L$set$267,LBE11-LBB11
	.quad L$set$267
	.uleb128 0x21
	.set L$set$268,LASF4-Lsection__debug_str
	.long L$set$268
	.byte	0x1
	.byte	0xc5
	.byte	0x7
	.long	0x87c
	.uleb128 0x2
	.byte	0x91
	.sleb128 -4
	.byte	0
	.byte	0
	.uleb128 0x1f
	.byte	0x8
	.long	0x62a
	.uleb128 0x1f
	.byte	0x8
	.long	0x53f
	.uleb128 0x20
	.ascii "anubis_types__classical__ed25519_sign\0"
	.byte	0x1
	.byte	0x97
	.byte	0x4
	.long	0x47d
	.quad	LFB7
	.set L$set$269,LFE7-LFB7
	.quad L$set$269
	.uleb128 0x1
	.byte	0x9c
	.long	0x17b9
	.uleb128 0x14
	.ascii "message\0"
	.byte	0x4
	.byte	0x4d
	.byte	0x7
	.long	0x2c9
	.uleb128 0x3
	.byte	0x91
	.sleb128 -192
	.uleb128 0x15
	.set L$set$270,LASF9-Lsection__debug_str
	.long L$set$270
	.byte	0x4
	.byte	0x4e
	.byte	0x7
	.long	0x1590
	.uleb128 0x3
	.byte	0x91
	.sleb128 -200
	.uleb128 0x15
	.set L$set$271,LASF10-Lsection__debug_str
	.long L$set$271
	.byte	0x4
	.byte	0x4f
	.byte	0x7
	.long	0x162f
	.uleb128 0x3
	.byte	0x91
	.sleb128 -208
	.uleb128 0x15
	.set L$set$272,LASF3-Lsection__debug_str
	.long L$set$272
	.byte	0x4
	.byte	0x50
	.byte	0x7
	.long	0x47d
	.uleb128 0x3
	.byte	0x91
	.sleb128 -165
	.uleb128 0x11
	.quad	LBB10
	.set L$set$273,LBE10-LBB10
	.quad L$set$273
	.uleb128 0x21
	.set L$set$274,LASF4-Lsection__debug_str
	.long L$set$274
	.byte	0x1
	.byte	0x9d
	.byte	0x7
	.long	0x87c
	.uleb128 0x3
	.byte	0x91
	.sleb128 -164
	.uleb128 0x22
	.ascii "signature_len\0"
	.byte	0x1
	.byte	0x9e
	.byte	0x7
	.long	0x8e2
	.uleb128 0x3
	.byte	0x91
	.sleb128 -160
	.uleb128 0xa
	.ascii "anubis_types__classical__ed25519_sign__Tfull_secretT43b\0"
	.long	0x250
	.long	0x1747
	.uleb128 0xd
	.long	0x32f
	.sleb128 64
	.byte	0
	.uleb128 0x21
	.set L$set$275,LASF12-Lsection__debug_str
	.long L$set$275
	.byte	0x1
	.byte	0xa0
	.byte	0x7
	.long	0x16fe
	.uleb128 0x3
	.byte	0x91
	.sleb128 -120
	.uleb128 0xa
	.ascii "anubis_types__classical__ed25519_sign__Ttemp_publicT46b\0"
	.long	0x250
	.long	0x179f
	.uleb128 0xd
	.long	0x32f
	.sleb128 32
	.byte	0
	.uleb128 0x22
	.ascii "temp_public\0"
	.byte	0x1
	.byte	0xa1
	.byte	0x7
	.long	0x1757
	.uleb128 0x3
	.byte	0x91
	.sleb128 -152
	.byte	0
	.byte	0
	.uleb128 0x20
	.ascii "anubis_types__classical__ed25519_generate_keypair\0"
	.byte	0x1
	.byte	0x69
	.byte	0x4
	.long	0x47d
	.quad	LFB6
	.set L$set$276,LFE6-LFB6
	.quad L$set$276
	.uleb128 0x1
	.byte	0x9c
	.long	0x1905
	.uleb128 0x15
	.set L$set$277,LASF11-Lsection__debug_str
	.long L$set$277
	.byte	0x4
	.byte	0x43
	.byte	0x7
	.long	0x1635
	.uleb128 0x3
	.byte	0x91
	.sleb128 -120
	.uleb128 0x15
	.set L$set$278,LASF9-Lsection__debug_str
	.long L$set$278
	.byte	0x4
	.byte	0x44
	.byte	0x7
	.long	0x1590
	.uleb128 0x3
	.byte	0x91
	.sleb128 -128
	.uleb128 0x15
	.set L$set$279,LASF3-Lsection__debug_str
	.long L$set$279
	.byte	0x4
	.byte	0x45
	.byte	0x7
	.long	0x47d
	.uleb128 0x3
	.byte	0x91
	.sleb128 -101
	.uleb128 0x11
	.quad	LBB6
	.set L$set$280,LBE6-LBB6
	.quad L$set$280
	.uleb128 0x21
	.set L$set$281,LASF4-Lsection__debug_str
	.long L$set$281
	.byte	0x1
	.byte	0x6e
	.byte	0x7
	.long	0x87c
	.uleb128 0x3
	.byte	0x91
	.sleb128 -92
	.uleb128 0xa
	.ascii "anubis_types__classical__ed25519_generate_keypair__Tfull_secretT19b\0"
	.long	0x250
	.long	0x18af
	.uleb128 0xd
	.long	0x32f
	.sleb128 64
	.byte	0
	.uleb128 0x21
	.set L$set$282,LASF12-Lsection__debug_str
	.long L$set$282
	.byte	0x1
	.byte	0x71
	.byte	0x7
	.long	0x185a
	.uleb128 0x3
	.byte	0x91
	.sleb128 -88
	.uleb128 0x19
	.quad	LBB7
	.set L$set$283,LBE7-LBB7
	.quad L$set$283
	.long	0x18e3
	.uleb128 0x22
	.ascii "i\0"
	.byte	0x1
	.byte	0x7b
	.byte	0xe
	.long	0x32f
	.uleb128 0x3
	.byte	0x91
	.sleb128 -100
	.byte	0
	.uleb128 0x11
	.quad	LBB9
	.set L$set$284,LBE9-LBB9
	.quad L$set$284
	.uleb128 0x22
	.ascii "i\0"
	.byte	0x1
	.byte	0x8d
	.byte	0xe
	.long	0x32f
	.uleb128 0x3
	.byte	0x91
	.sleb128 -96
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0x10
	.ascii "anubis_types__classical__zeroize_x25519_shared\0"
	.byte	0x1
	.byte	0x5c
	.byte	0x4
	.quad	LFB5
	.set L$set$285,LFE5-LFB5
	.quad L$set$285
	.uleb128 0x1
	.byte	0x9c
	.long	0x1975
	.uleb128 0x15
	.set L$set$286,LASF13-Lsection__debug_str
	.long L$set$286
	.byte	0x4
	.byte	0x39
	.byte	0x7
	.long	0x1975
	.uleb128 0x2
	.byte	0x91
	.sleb128 -24
	.uleb128 0x22
	.ascii "secret_addr\0"
	.byte	0x1
	.byte	0x5f
	.byte	0x7
	.long	0x8b6
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.byte	0
	.uleb128 0x1f
	.byte	0x8
	.long	0x4c1
	.uleb128 0x10
	.ascii "anubis_types__classical__zeroize_x25519_secret\0"
	.byte	0x1
	.byte	0x53
	.byte	0x4
	.quad	LFB4
	.set L$set$287,LFE4-LFB4
	.quad L$set$287
	.uleb128 0x1
	.byte	0x9c
	.long	0x19e3
	.uleb128 0x15
	.set L$set$288,LASF9-Lsection__debug_str
	.long L$set$288
	.byte	0x4
	.byte	0x33
	.byte	0x7
	.long	0x19e3
	.uleb128 0x2
	.byte	0x91
	.sleb128 -24
	.uleb128 0x21
	.set L$set$289,LASF5-Lsection__debug_str
	.long L$set$289
	.byte	0x1
	.byte	0x56
	.byte	0x7
	.long	0x8b6
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.byte	0
	.uleb128 0x1f
	.byte	0x8
	.long	0x439
	.uleb128 0x20
	.ascii "anubis_types__classical__x25519_compute_shared\0"
	.byte	0x1
	.byte	0x34
	.byte	0x4
	.long	0x47d
	.quad	LFB3
	.set L$set$290,LFE3-LFB3
	.quad L$set$290
	.uleb128 0x1
	.byte	0x9c
	.long	0x1acc
	.uleb128 0x14
	.ascii "our_secret_key\0"
	.byte	0x4
	.byte	0x28
	.byte	0x7
	.long	0x19e3
	.uleb128 0x2
	.byte	0x91
	.sleb128 -56
	.uleb128 0x14
	.ascii "their_public_key\0"
	.byte	0x4
	.byte	0x29
	.byte	0x7
	.long	0x1acc
	.uleb128 0x2
	.byte	0x91
	.sleb128 -64
	.uleb128 0x15
	.set L$set$291,LASF13-Lsection__debug_str
	.long L$set$291
	.byte	0x4
	.byte	0x2a
	.byte	0x7
	.long	0x1975
	.uleb128 0x3
	.byte	0x91
	.sleb128 -72
	.uleb128 0x15
	.set L$set$292,LASF3-Lsection__debug_str
	.long L$set$292
	.byte	0x4
	.byte	0x2b
	.byte	0x7
	.long	0x47d
	.uleb128 0x2
	.byte	0x91
	.sleb128 -33
	.uleb128 0x11
	.quad	LBB4
	.set L$set$293,LBE4-LBB4
	.quad L$set$293
	.uleb128 0x21
	.set L$set$294,LASF4-Lsection__debug_str
	.long L$set$294
	.byte	0x1
	.byte	0x3a
	.byte	0x7
	.long	0x87c
	.uleb128 0x2
	.byte	0x91
	.sleb128 -28
	.uleb128 0x11
	.quad	LBB5
	.set L$set$295,LBE5-LBB5
	.quad L$set$295
	.uleb128 0x22
	.ascii "i\0"
	.byte	0x1
	.byte	0x49
	.byte	0xe
	.long	0x32f
	.uleb128 0x2
	.byte	0x91
	.sleb128 -32
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0x1f
	.byte	0x8
	.long	0x3cc
	.uleb128 0x23
	.ascii "anubis_types__classical__x25519_generate_keypair\0"
	.byte	0x1
	.byte	0x17
	.byte	0x4
	.long	0x47d
	.quad	LFB2
	.set L$set$296,LFE2-LFB2
	.quad L$set$296
	.uleb128 0x1
	.byte	0x9c
	.uleb128 0x15
	.set L$set$297,LASF11-Lsection__debug_str
	.long L$set$297
	.byte	0x4
	.byte	0x1e
	.byte	0x7
	.long	0x1acc
	.uleb128 0x2
	.byte	0x91
	.sleb128 -56
	.uleb128 0x15
	.set L$set$298,LASF9-Lsection__debug_str
	.long L$set$298
	.byte	0x4
	.byte	0x1f
	.byte	0x7
	.long	0x19e3
	.uleb128 0x2
	.byte	0x91
	.sleb128 -64
	.uleb128 0x15
	.set L$set$299,LASF3-Lsection__debug_str
	.long L$set$299
	.byte	0x4
	.byte	0x20
	.byte	0x7
	.long	0x47d
	.uleb128 0x2
	.byte	0x91
	.sleb128 -33
	.uleb128 0x11
	.quad	LBB2
	.set L$set$300,LBE2-LBB2
	.quad L$set$300
	.uleb128 0x21
	.set L$set$301,LASF4-Lsection__debug_str
	.long L$set$301
	.byte	0x1
	.byte	0x1c
	.byte	0x7
	.long	0x87c
	.uleb128 0x2
	.byte	0x91
	.sleb128 -28
	.uleb128 0x11
	.quad	LBB3
	.set L$set$302,LBE3-LBB3
	.quad L$set$302
	.uleb128 0x22
	.ascii "i\0"
	.byte	0x1
	.byte	0x2a
	.byte	0xe
	.long	0x32f
	.uleb128 0x2
	.byte	0x91
	.sleb128 -32
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
	.uleb128 0x26
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x34
	.uleb128 0x19
	.byte	0
	.byte	0
	.uleb128 0xa
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
	.uleb128 0xb
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
	.uleb128 0xc
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
	.uleb128 0xd
	.uleb128 0x21
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2f
	.uleb128 0xd
	.byte	0
	.byte	0
	.uleb128 0xe
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
	.uleb128 0xf
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
	.uleb128 0x10
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
	.uleb128 0x11
	.uleb128 0xb
	.byte	0x1
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x7
	.byte	0
	.byte	0
	.uleb128 0x12
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
	.uleb128 0x13
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
	.uleb128 0x16
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
	.uleb128 0x17
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
	.uleb128 0x18
	.uleb128 0x21
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2f
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x19
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
	.uleb128 0x1a
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
	.uleb128 0x34
	.uleb128 0x19
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x1b
	.uleb128 0xd
	.byte	0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x38
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0x1c
	.uleb128 0xd
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x38
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0x1d
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
	.uleb128 0x1f
	.uleb128 0x10
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x20
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
	.uleb128 0x23
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
	.long	0x30f
	.short	0x2
	.set L$set$303,Ldebug_info0-Lsection__debug_info
	.long L$set$303
	.long	0x1b8c
	.long	0x90f
	.ascii "anubis_types__classical___elabb\0"
	.long	0x974
	.ascii "anubis_types__classical__hkdf_derive\0"
	.long	0xdc1
	.ascii "anubis_types__classical__zeroize_argon2_key\0"
	.long	0xe2e
	.ascii "anubis_types__classical__argon2id_derive_key\0"
	.long	0x106e
	.ascii "anubis_types__classical__zeroize_xchacha20_key\0"
	.long	0x10de
	.ascii "anubis_types__classical__xchacha20_decrypt\0"
	.long	0x1261
	.ascii "anubis_types__classical__xchacha20_encrypt\0"
	.long	0x14ba
	.ascii "anubis_types__classical__xchacha20_generate_key\0"
	.long	0x1527
	.ascii "anubis_types__classical__zeroize_ed25519_secret\0"
	.long	0x1596
	.ascii "anubis_types__classical__ed25519_verify\0"
	.long	0x163b
	.ascii "anubis_types__classical__ed25519_sign\0"
	.long	0x17b9
	.ascii "anubis_types__classical__ed25519_generate_keypair\0"
	.long	0x1905
	.ascii "anubis_types__classical__zeroize_x25519_shared\0"
	.long	0x197b
	.ascii "anubis_types__classical__zeroize_x25519_secret\0"
	.long	0x19e9
	.ascii "anubis_types__classical__x25519_compute_shared\0"
	.long	0x1ad2
	.ascii "anubis_types__classical__x25519_generate_keypair\0"
	.long	0
	.section __DWARF,__debug_pubtypes,regular,debug
Lsection__debug_pubtypes:
	.long	0x4a4
	.short	0x2
	.set L$set$304,Ldebug_info0-Lsection__debug_info
	.long L$set$304
	.long	0x1b8c
	.long	0x236
	.ascii "interfaces__unsigned_8\0"
	.long	0x26a
	.ascii "anubis_types__TbyteB\0"
	.long	0x32f
	.ascii "integer\0"
	.long	0x2f1
	.ascii "anubis_types__byte_array___XUA\0"
	.long	0x33a
	.ascii "anubis_types__byte_array___XUB\0"
	.long	0x282
	.ascii "anubis_types__byte_array\0"
	.long	0x396
	.ascii "anubis_types__x25519_public_key__T15s\0"
	.long	0x3cc
	.ascii "anubis_types__x25519_public_key\0"
	.long	0x403
	.ascii "anubis_types__x25519_secret_key__T17s\0"
	.long	0x47d
	.ascii "boolean\0"
	.long	0x439
	.ascii "anubis_types__x25519_secret_key\0"
	.long	0x488
	.ascii "anubis_types__x25519_shared_secret__T19s\0"
	.long	0x4c1
	.ascii "anubis_types__x25519_shared_secret\0"
	.long	0x508
	.ascii "anubis_types__ed25519_public_key__T21s\0"
	.long	0x53f
	.ascii "anubis_types__ed25519_public_key\0"
	.long	0x577
	.ascii "anubis_types__ed25519_secret_key__T23s\0"
	.long	0x5ae
	.ascii "anubis_types__ed25519_secret_key\0"
	.long	0x5f3
	.ascii "anubis_types__ed25519_signature__T25s\0"
	.long	0x62a
	.ascii "anubis_types__ed25519_signature\0"
	.long	0x661
	.ascii "anubis_types__xchacha20_key__T27s\0"
	.long	0x693
	.ascii "anubis_types__xchacha20_key\0"
	.long	0x6d3
	.ascii "anubis_types__xchacha20_nonce__T29s\0"
	.long	0x707
	.ascii "anubis_types__xchacha20_nonce\0"
	.long	0x73c
	.ascii "anubis_types__poly1305_tag__T31s\0"
	.long	0x76d
	.ascii "anubis_types__poly1305_tag\0"
	.long	0x79f
	.ascii "anubis_types__argon2_salt__T49s\0"
	.long	0x7cf
	.ascii "anubis_types__argon2_salt\0"
	.long	0x800
	.ascii "anubis_types__argon2_derived_key__T51s\0"
	.long	0x837
	.ascii "anubis_types__argon2_derived_key\0"
	.long	0x89e
	.ascii "interfaces__c__TintB\0"
	.long	0x8b6
	.ascii "system__address\0"
	.long	0x8c9
	.ascii "interfaces__c__size_t\0"
	.long	0x8e2
	.ascii "interfaces__c__unsigned_long\0"
	.long	0x902
	.ascii "character\0"
	.long	0xd4e
	.ascii "string___XUA\0"
	.long	0xd7a
	.ascii "string___XUB\0"
	.long	0xd12
	.ascii "string\0"
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
LASF0:
	.ascii "P_BOUNDS\0"
LASF7:
	.ascii "auth_tag\0"
LASF8:
	.ascii "plaintext\0"
LASF5:
	.ascii "key_addr\0"
LASF4:
	.ascii "status\0"
LASF10:
	.ascii "signature\0"
LASF13:
	.ascii "shared_secret\0"
LASF1:
	.ascii "data\0"
LASF3:
	.ascii "success\0"
LASF9:
	.ascii "secret_key\0"
LASF2:
	.ascii "valid\0"
LASF11:
	.ascii "public_key\0"
LASF6:
	.ascii "ciphertext\0"
LASF12:
	.ascii "full_secret\0"
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
