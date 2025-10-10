	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
Ltext0:
	.file 1 "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_types-storage.adb"
	.align	2
	.globl _anubis_types__storage__identity_keypairIP
_anubis_types__storage__identity_keypairIP:
LFB2:
	sub	x10, sp, #16384
	str	xzr, [x10, 4064]
	stp	x29, x30, [sp, -32]!
LCFI0:
	mov	x29, sp
LCFI1:
	str	x0, [x29, 24]
	ldr	x0, [x29, 24]
	add	x0, x0, 32
	bl	_anubis_types__x25519_secret_keyIP
	ldr	x0, [x29, 24]
	add	x0, x0, 97
	bl	_anubis_types__ed25519_secret_keyIP
	ldr	x0, [x29, 24]
	add	x0, x0, 1698
	bl	_anubis_types__ml_kem_secret_keyIP
	ldr	x1, [x29, 24]
	mov	x0, 7459
	add	x0, x1, x0
	bl	_anubis_types__ml_dsa_secret_keyIP
	ldr	x0, [x29, 24]
	add	x0, x0, 12288
	strb	wzr, [x0, 68]
	ldp	x29, x30, [sp], 32
LCFI2:
	ret
LFE2:
	.align	2
	.globl _anubis_types__storage__generate_identity
_anubis_types__storage__generate_identity:
LFB3:
	.loc 1 22 4
	sub	x10, sp, #16384
	str	xzr, [x10, 4048]
	sub	sp, sp, #48
LCFI3:
	stp	x29, x30, [sp, 32]
LCFI4:
	add	x29, sp, 32
LCFI5:
	str	x0, [x29, -24]
	.loc 1 22 4
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x1, [x0]
	str	x1, [x29, -8]
	mov	x1, 0
LBB2:
	.loc 1 29 16
	ldr	x2, [x29, -24]
	ldr	x0, [x29, -24]
	add	x0, x0, 32
	mov	x1, x0
	mov	x0, x2
	bl	_anubis_types__classical__x25519_generate_keypair
	.loc 1 29 16 is_stmt 0 discriminator 1
	strb	w0, [x29, -9]
	.loc 1 35 10 is_stmt 1
	ldrb	w0, [x29, -9]
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 35 7
	cmp	w0, 0
	beq	L4
	.loc 1 36 18
	strb	wzr, [x29, -10]
	.loc 1 37 10
	b	L5
L4:
	.loc 1 41 16
	ldr	x0, [x29, -24]
	add	x2, x0, 65
	ldr	x0, [x29, -24]
	add	x0, x0, 97
	mov	x1, x0
	mov	x0, x2
	bl	_anubis_types__classical__ed25519_generate_keypair
	.loc 1 41 16 is_stmt 0 discriminator 1
	strb	w0, [x29, -9]
	.loc 1 47 10 is_stmt 1
	ldrb	w0, [x29, -9]
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 47 7
	cmp	w0, 0
	beq	L6
	.loc 1 48 19
	ldr	x0, [x29, -24]
	add	x0, x0, 32
	bl	_anubis_types__classical__zeroize_x25519_secret
	.loc 1 49 18
	strb	wzr, [x29, -10]
	.loc 1 50 10
	b	L5
L6:
	.loc 1 54 10
	ldr	x0, [x29, -24]
	add	x2, x0, 130
	ldr	x0, [x29, -24]
	add	x0, x0, 1698
	mov	x1, x0
	mov	x0, x2
	bl	_anubis_types__pqc__ml_kem_generate_keypair
	.loc 1 54 10 is_stmt 0 discriminator 1
	strb	w0, [x29, -9]
	.loc 1 60 10 is_stmt 1
	ldrb	w0, [x29, -9]
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 60 7
	cmp	w0, 0
	beq	L7
	.loc 1 61 19
	ldr	x0, [x29, -24]
	add	x0, x0, 32
	bl	_anubis_types__classical__zeroize_x25519_secret
	.loc 1 62 19
	ldr	x0, [x29, -24]
	add	x0, x0, 97
	bl	_anubis_types__classical__zeroize_ed25519_secret
	.loc 1 63 18
	strb	wzr, [x29, -10]
	.loc 1 64 10
	b	L5
L7:
	.loc 1 68 10
	ldr	x1, [x29, -24]
	mov	x0, 4867
	add	x2, x1, x0
	ldr	x1, [x29, -24]
	mov	x0, 7459
	add	x0, x1, x0
	mov	x1, x0
	mov	x0, x2
	bl	_anubis_types__pqc__ml_dsa_generate_keypair
	.loc 1 68 10 is_stmt 0 discriminator 1
	strb	w0, [x29, -9]
	.loc 1 74 10 is_stmt 1
	ldrb	w0, [x29, -9]
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 74 7
	cmp	w0, 0
	beq	L8
	.loc 1 75 19
	ldr	x0, [x29, -24]
	add	x0, x0, 32
	bl	_anubis_types__classical__zeroize_x25519_secret
	.loc 1 76 19
	ldr	x0, [x29, -24]
	add	x0, x0, 97
	bl	_anubis_types__classical__zeroize_ed25519_secret
	.loc 1 77 13
	ldr	x0, [x29, -24]
	add	x0, x0, 1698
	bl	_anubis_types__pqc__zeroize_ml_kem_secret
	.loc 1 78 18
	strb	wzr, [x29, -10]
	.loc 1 79 10
	b	L5
L8:
	.loc 1 82 22
	ldr	x0, [x29, -24]
	add	x0, x0, 12288
	mov	w1, 1
	strb	w1, [x0, 68]
	.loc 1 83 15
	mov	w0, 1
	strb	w0, [x29, -10]
	.loc 1 84 8
	nop
L5:
LBE2:
	ldrb	w0, [x29, -10]
	.loc 1 84 8 is_stmt 0 discriminator 2
	mov	w1, w0
	.loc 1 84 8
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x3, [x29, -8]
	ldr	x2, [x0]
	subs	x3, x3, x2
	mov	x2, 0
	beq	L10
	bl	___stack_chk_fail
L10:
	mov	w0, w1
	ldp	x29, x30, [sp, 32]
	add	sp, sp, 48
LCFI6:
	ret
LFE3:
	.const
	.align	3
lC1:
	.space	1
	.align	3
lC2:
	.ascii "anubis_types-storage.adb"
	.space 1
	.text
	.align	2
	.globl _anubis_types__storage__save_identity
_anubis_types__storage__save_identity:
LFB4:
	.loc 1 90 4 is_stmt 1
	sub	x10, sp, #16384
LEHB0:
	str	xzr, [x10, 3952]
	sub	sp, sp, #144
LCFI7:
	stp	x29, x30, [sp, 128]
LCFI8:
	add	x29, sp, 128
LCFI9:
	str	x0, [x29, -104]
	stp	x1, x2, [x29, -120]
	.loc 1 90 4
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x1, [x0]
	str	x1, [x29, -8]
	mov	x1, 0
LEHE0:
	ldr	x0, [x29, -112]
	ldr	w0, [x0]
	ldr	x1, [x29, -112]
	ldr	w1, [x1, 4]
LBB3:
	cmp	w1, w0
	.loc 1 90 4 is_stmt 0 discriminator 4
	cmp	w1, w0
	blt	L15
	.loc 1 90 4 discriminator 5
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
L15:
	.loc 1 90 4 discriminator 8
	cmp	w1, w0
	.loc 1 95 7 is_stmt 1
	str	xzr, [x29, -40]
	.loc 1 96 7
	str	xzr, [x29, -32]
	.loc 1 98 10
	ldr	x0, [x29, -104]
	add	x0, x0, 12288
	ldrb	w0, [x0, 68]
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 98 7
	cmp	w0, 0
	beq	L18
	.loc 1 99 18
	strb	wzr, [x29, -81]
	.loc 1 100 10
	b	L62
L18:
LBB4:
LBB5:
	.loc 1 104 10
	ldr	x6, [x29, -40]
	adrp	x0, lC1@PAGE
	add	x8, x0, lC1@PAGEOFF;
	adrp	x0, lC0@PAGE
	add	x9, x0, lC0@PAGEOFF;
	mov	x4, x8
	mov	x5, x9
	ldp	x2, x3, [x29, -120]
	mov	w1, 2
	mov	x0, x6
LEHB1:
	bl	_ada__streams__stream_io__create
LEHE1:
	.loc 1 104 10 is_stmt 0 discriminator 1
	str	x0, [x29, -40]
LBE5:
LBE4:
	.loc 1 111 22 is_stmt 1
	ldr	x0, [x29, -40]
LEHB2:
	bl	_ada__streams__stream_io__stream
	.loc 1 111 22 is_stmt 0 discriminator 1
	str	x0, [x29, -32]
LBB6:
	.loc 1 114 11 is_stmt 1
	mov	w0, 1
	str	w0, [x29, -80]
L23:
	.loc 1 114 11 is_stmt 0 discriminator 1
	ldr	w0, [x29, -80]
	cmp	w0, 8
	bgt	L20
	.loc 1 115 24 is_stmt 1
	ldr	x0, [x29, -32]
	cmp	x0, 0
	bne	L21
	.loc 1 115 24 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L22
	bl	___stack_chk_fail
L22:
	mov	w1, 115
	adrp	x0, lC2@PAGE
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Access_Check
L21:
	.loc 1 115 24 discriminator 2
	ldrsw	x0, [x29, -80]
	adrp	x1, _anubis_types__storage__magic@PAGE
	add	x1, x1, _anubis_types__storage__magic@PAGEOFF;
	add	x0, x1, x0
	ldrb	w0, [x0, -1]
	.loc 1 115 24 discriminator 3
	mov	w1, w0
	ldr	x0, [x29, -32]
	bl	_system__stream_attributes__w_ssu
	.loc 1 114 11 is_stmt 1 discriminator 2
	ldr	w0, [x29, -80]
	add	w0, w0, 1
	str	w0, [x29, -80]
	.loc 1 116 15
	b	L23
L20:
LBE6:
LBB7:
	.loc 1 119 11
	mov	w0, 1
	str	w0, [x29, -76]
L27:
	.loc 1 119 11 is_stmt 0 discriminator 1
	ldr	w0, [x29, -76]
	cmp	w0, 2
	bgt	L24
	.loc 1 120 24 is_stmt 1
	ldr	x0, [x29, -32]
	cmp	x0, 0
	bne	L25
	.loc 1 120 24 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L26
	bl	___stack_chk_fail
L26:
	mov	w1, 120
	adrp	x0, lC2@PAGE
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Access_Check
L25:
	.loc 1 120 24 discriminator 2
	ldrsw	x0, [x29, -76]
	adrp	x1, _anubis_types__storage__version@PAGE
	add	x1, x1, _anubis_types__storage__version@PAGEOFF;
	add	x0, x1, x0
	ldrb	w0, [x0, -1]
	.loc 1 120 24 discriminator 3
	mov	w1, w0
	ldr	x0, [x29, -32]
	bl	_system__stream_attributes__w_ssu
	.loc 1 119 11 is_stmt 1 discriminator 2
	ldr	w0, [x29, -76]
	add	w0, w0, 1
	str	w0, [x29, -76]
	.loc 1 121 15
	b	L27
L24:
LBE7:
	.loc 1 124 21
	ldr	x0, [x29, -32]
	cmp	x0, 0
	bne	L28
	.loc 1 124 21 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L29
	bl	___stack_chk_fail
L29:
	mov	w1, 124
	adrp	x0, lC2@PAGE
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Access_Check
L28:
	.loc 1 124 21 discriminator 2
	mov	w1, 1
	ldr	x0, [x29, -32]
	bl	_system__stream_attributes__w_ssu
LBB8:
	.loc 1 127 11 is_stmt 1
	mov	w0, 1
	str	w0, [x29, -72]
L33:
	.loc 1 127 11 is_stmt 0 discriminator 1
	ldr	w0, [x29, -72]
	cmp	w0, 32
	bgt	L30
	.loc 1 128 24 is_stmt 1
	ldr	x0, [x29, -32]
	cmp	x0, 0
	bne	L31
	.loc 1 128 24 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L32
	bl	___stack_chk_fail
L32:
	mov	w1, 128
	adrp	x0, lC2@PAGE
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Access_Check
L31:
	.loc 1 128 24 discriminator 2
	ldrsw	x0, [x29, -72]
	ldr	x1, [x29, -104]
	add	x0, x1, x0
	ldrb	w0, [x0, -1]
	.loc 1 128 24 discriminator 3
	mov	w1, w0
	ldr	x0, [x29, -32]
	bl	_system__stream_attributes__w_ssu
	.loc 1 127 11 is_stmt 1 discriminator 2
	ldr	w0, [x29, -72]
	add	w0, w0, 1
	str	w0, [x29, -72]
	.loc 1 129 15
	b	L33
L30:
LBE8:
LBB9:
	.loc 1 130 11
	mov	w0, 1
	str	w0, [x29, -68]
L37:
	.loc 1 130 11 is_stmt 0 discriminator 1
	ldr	w0, [x29, -68]
	cmp	w0, 32
	bgt	L34
	.loc 1 131 24 is_stmt 1
	ldr	x0, [x29, -32]
	cmp	x0, 0
	bne	L35
	.loc 1 131 24 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L36
	bl	___stack_chk_fail
L36:
	mov	w1, 131
	adrp	x0, lC2@PAGE
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Access_Check
L35:
	.loc 1 131 24 discriminator 2
	ldrsw	x0, [x29, -68]
	ldr	x1, [x29, -104]
	add	x0, x1, x0
	ldrb	w0, [x0, 31]
	.loc 1 131 24 discriminator 3
	mov	w1, w0
	ldr	x0, [x29, -32]
	bl	_system__stream_attributes__w_ssu
	.loc 1 130 11 is_stmt 1 discriminator 2
	ldr	w0, [x29, -68]
	add	w0, w0, 1
	str	w0, [x29, -68]
	.loc 1 132 15
	b	L37
L34:
LBE9:
LBB10:
	.loc 1 135 11
	mov	w0, 1
	str	w0, [x29, -64]
L41:
	.loc 1 135 11 is_stmt 0 discriminator 1
	ldr	w0, [x29, -64]
	cmp	w0, 1568
	bgt	L38
	.loc 1 136 24 is_stmt 1
	ldr	x0, [x29, -32]
	cmp	x0, 0
	bne	L39
	.loc 1 136 24 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L40
	bl	___stack_chk_fail
L40:
	mov	w1, 136
	adrp	x0, lC2@PAGE
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Access_Check
L39:
	.loc 1 136 24 discriminator 2
	ldrsw	x0, [x29, -64]
	ldr	x1, [x29, -104]
	add	x0, x1, x0
	ldrb	w0, [x0, 129]
	.loc 1 136 24 discriminator 3
	mov	w1, w0
	ldr	x0, [x29, -32]
	bl	_system__stream_attributes__w_ssu
	.loc 1 135 11 is_stmt 1 discriminator 2
	ldr	w0, [x29, -64]
	add	w0, w0, 1
	str	w0, [x29, -64]
	.loc 1 137 15
	b	L41
L38:
LBE10:
LBB11:
	.loc 1 138 11
	mov	w0, 1
	str	w0, [x29, -60]
L45:
	.loc 1 138 11 is_stmt 0 discriminator 1
	ldr	w0, [x29, -60]
	cmp	w0, 3168
	bgt	L42
	.loc 1 139 24 is_stmt 1
	ldr	x0, [x29, -32]
	cmp	x0, 0
	bne	L43
	.loc 1 139 24 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L44
	bl	___stack_chk_fail
L44:
	mov	w1, 139
	adrp	x0, lC2@PAGE
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Access_Check
L43:
	.loc 1 139 24 discriminator 2
	ldrsw	x0, [x29, -60]
	ldr	x1, [x29, -104]
	add	x0, x1, x0
	ldrb	w0, [x0, 1697]
	.loc 1 139 24 discriminator 3
	mov	w1, w0
	ldr	x0, [x29, -32]
	bl	_system__stream_attributes__w_ssu
	.loc 1 138 11 is_stmt 1 discriminator 2
	ldr	w0, [x29, -60]
	add	w0, w0, 1
	str	w0, [x29, -60]
	.loc 1 140 15
	b	L45
L42:
LBE11:
LBB12:
	.loc 1 143 11
	mov	w0, 1
	str	w0, [x29, -56]
L49:
	.loc 1 143 11 is_stmt 0 discriminator 1
	ldr	w0, [x29, -56]
	cmp	w0, 32
	bgt	L46
	.loc 1 144 24 is_stmt 1
	ldr	x0, [x29, -32]
	cmp	x0, 0
	bne	L47
	.loc 1 144 24 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L48
	bl	___stack_chk_fail
L48:
	mov	w1, 144
	adrp	x0, lC2@PAGE
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Access_Check
L47:
	.loc 1 144 24 discriminator 2
	ldrsw	x0, [x29, -56]
	ldr	x1, [x29, -104]
	add	x0, x1, x0
	ldrb	w0, [x0, 64]
	.loc 1 144 24 discriminator 3
	mov	w1, w0
	ldr	x0, [x29, -32]
	bl	_system__stream_attributes__w_ssu
	.loc 1 143 11 is_stmt 1 discriminator 2
	ldr	w0, [x29, -56]
	add	w0, w0, 1
	str	w0, [x29, -56]
	.loc 1 145 15
	b	L49
L46:
LBE12:
LBB13:
	.loc 1 146 11
	mov	w0, 1
	str	w0, [x29, -52]
L53:
	.loc 1 146 11 is_stmt 0 discriminator 1
	ldr	w0, [x29, -52]
	cmp	w0, 32
	bgt	L50
	.loc 1 147 24 is_stmt 1
	ldr	x0, [x29, -32]
	cmp	x0, 0
	bne	L51
	.loc 1 147 24 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L52
	bl	___stack_chk_fail
L52:
	mov	w1, 147
	adrp	x0, lC2@PAGE
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Access_Check
L51:
	.loc 1 147 24 discriminator 2
	ldrsw	x0, [x29, -52]
	ldr	x1, [x29, -104]
	add	x0, x1, x0
	ldrb	w0, [x0, 96]
	.loc 1 147 24 discriminator 3
	mov	w1, w0
	ldr	x0, [x29, -32]
	bl	_system__stream_attributes__w_ssu
	.loc 1 146 11 is_stmt 1 discriminator 2
	ldr	w0, [x29, -52]
	add	w0, w0, 1
	str	w0, [x29, -52]
	.loc 1 148 15
	b	L53
L50:
LBE13:
LBB14:
	.loc 1 151 11
	mov	w0, 1
	str	w0, [x29, -48]
L57:
	.loc 1 151 11 is_stmt 0 discriminator 1
	ldr	w0, [x29, -48]
	cmp	w0, 2592
	bgt	L54
	.loc 1 152 24 is_stmt 1
	ldr	x0, [x29, -32]
	cmp	x0, 0
	bne	L55
	.loc 1 152 24 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L56
	bl	___stack_chk_fail
L56:
	mov	w1, 152
	adrp	x0, lC2@PAGE
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Access_Check
L55:
	.loc 1 152 24 discriminator 2
	ldrsw	x0, [x29, -48]
	ldr	x1, [x29, -104]
	add	x0, x1, x0
	add	x0, x0, 4096
	ldrb	w0, [x0, 770]
	.loc 1 152 24 discriminator 3
	mov	w1, w0
	ldr	x0, [x29, -32]
	bl	_system__stream_attributes__w_ssu
	.loc 1 151 11 is_stmt 1 discriminator 2
	ldr	w0, [x29, -48]
	add	w0, w0, 1
	str	w0, [x29, -48]
	.loc 1 153 15
	b	L57
L54:
LBE14:
LBB15:
	.loc 1 154 11
	mov	w0, 1
	str	w0, [x29, -44]
L61:
	.loc 1 154 11 is_stmt 0 discriminator 1
	ldr	w1, [x29, -44]
	mov	w0, 4896
	cmp	w1, w0
	bgt	L58
	.loc 1 155 24 is_stmt 1
	ldr	x0, [x29, -32]
	cmp	x0, 0
	bne	L59
	.loc 1 155 24 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L60
	bl	___stack_chk_fail
L60:
	mov	w1, 155
	adrp	x0, lC2@PAGE
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Access_Check
L59:
	.loc 1 155 24 discriminator 2
	ldrsw	x0, [x29, -44]
	ldr	x1, [x29, -104]
	add	x0, x1, x0
	add	x0, x0, 4096
	ldrb	w0, [x0, 3362]
	.loc 1 155 24 discriminator 3
	mov	w1, w0
	ldr	x0, [x29, -32]
	bl	_system__stream_attributes__w_ssu
	.loc 1 154 11 is_stmt 1 discriminator 2
	ldr	w0, [x29, -44]
	add	w0, w0, 1
	str	w0, [x29, -44]
	.loc 1 156 15
	b	L61
L58:
LBE15:
	.loc 1 158 7
	sub	x0, x29, #40
	bl	_ada__streams__stream_io__close
	.loc 1 159 15
	mov	w0, 1
	strb	w0, [x29, -81]
	.loc 1 160 8
	nop
L62:
LBE3:
	.loc 1 160 8 is_stmt 0 discriminator 1
	ldrb	w0, [x29, -81]
	.loc 1 160 8 discriminator 3
	mov	w1, w0
	.loc 1 160 8
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x3, [x29, -8]
	ldr	x2, [x0]
	subs	x3, x3, x2
	mov	x2, 0
	beq	L67
	b	L69
L68:
	.loc 1 106 10 is_stmt 1
	cmp	x1, 1
	beq	L65
	mov	x1, x0
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x3, [x29, -8]
	ldr	x2, [x0]
	subs	x3, x3, x2
	mov	x2, 0
	beq	L66
	bl	___stack_chk_fail
L66:
	mov	x0, x1
	bl	__Unwind_Resume
L65:
LBB17:
LBB16:
	.loc 1 106 10 is_stmt 0 discriminator 1
	str	x0, [x29, -24]
	ldr	x0, [x29, -24]
	bl	___gnat_begin_handler_v1
	str	x0, [x29, -16]
	.loc 1 107 21 is_stmt 1
	strb	wzr, [x29, -81]
	.loc 1 108 13
	nop
	.loc 1 106 10
	mov	x2, 0
	ldr	x1, [x29, -16]
	ldr	x0, [x29, -24]
	bl	___gnat_end_handler_v1
	b	L62
L69:
LBE16:
LBE17:
	.loc 1 160 8
	bl	___stack_chk_fail
L67:
	mov	w0, w1
	ldp	x29, x30, [sp, 128]
LEHE2:
	add	sp, sp, 144
LCFI10:
	ret
LFE4:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table0:
	.align	2
LLSDA4:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT4-LLSDATTD4
LLSDATTD4:
	.byte	0x1
	.uleb128 LLSDACSE4-LLSDACSB4
LLSDACSB4:
	.uleb128 LEHB0-LFB4
	.uleb128 LEHE0-LEHB0
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB1-LFB4
	.uleb128 LEHE1-LEHB1
	.uleb128 L68-LFB4
	.uleb128 0x1
	.uleb128 LEHB2-LFB4
	.uleb128 LEHE2-LEHB2
	.uleb128 0
	.uleb128 0
LLSDACSE4:
	.byte	0x1
	.byte	0
	.align	2
L_got_pcr0:
	.long	___gnat_others_value@GOT-L_got_pcr0
LLSDATT4:
	.text
	.const
	.align	2
lC0:
	.word	1
	.word	0
	.text
	.align	2
	.globl _anubis_types__storage__load_identity
_anubis_types__storage__load_identity:
LFB5:
	.loc 1 166 4
	sub	x10, sp, #16384
LEHB3:
	str	xzr, [x10, 3920]
	sub	sp, sp, #176
LCFI11:
	stp	x29, x30, [sp, 144]
LCFI12:
	add	x29, sp, 144
LCFI13:
	stp	x19, x20, [sp, 160]
LCFI14:
	stp	x0, x1, [x29, -128]
	str	x2, [x29, -136]
	.loc 1 166 4
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x1, [x0]
	str	x1, [x29, -8]
	mov	x1, 0
LEHE3:
	ldr	x0, [x29, -120]
	ldr	w0, [x0]
	ldr	x1, [x29, -120]
	ldr	w1, [x1, 4]
LBB18:
	cmp	w1, w0
	.loc 1 166 4 is_stmt 0 discriminator 4
	cmp	w1, w0
	blt	L74
	.loc 1 166 4 discriminator 5
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
L74:
	.loc 1 166 4 discriminator 8
	cmp	w1, w0
	.loc 1 171 7 is_stmt 1
	str	xzr, [x29, -56]
	.loc 1 172 7
	str	xzr, [x29, -48]
LBB19:
LBB20:
	.loc 1 179 10
	ldr	x6, [x29, -56]
	adrp	x0, lC1@PAGE
	add	x8, x0, lC1@PAGEOFF;
	adrp	x0, lC0@PAGE
	add	x9, x0, lC0@PAGEOFF;
	mov	x4, x8
	mov	x5, x9
	ldp	x2, x3, [x29, -128]
	mov	w1, 0
	mov	x0, x6
LEHB4:
	bl	_ada__streams__stream_io__open
LEHE4:
	.loc 1 179 10 is_stmt 0 discriminator 1
	str	x0, [x29, -56]
LBE20:
LBE19:
	.loc 1 186 22 is_stmt 1
	ldr	x0, [x29, -56]
LEHB5:
	bl	_ada__streams__stream_io__stream
	.loc 1 186 22 is_stmt 0 discriminator 1
	str	x0, [x29, -48]
LBB21:
	.loc 1 189 11 is_stmt 1
	mov	w0, 1
	str	w0, [x29, -96]
L80:
	.loc 1 189 11 is_stmt 0 discriminator 1
	ldr	w0, [x29, -96]
	cmp	w0, 8
	bgt	L77
	.loc 1 190 24 is_stmt 1
	ldr	x0, [x29, -48]
	cmp	x0, 0
	bne	L78
	.loc 1 190 24 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L79
	bl	___stack_chk_fail
L79:
	mov	w1, 190
	adrp	x0, lC2@PAGE
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Access_Check
L78:
	.loc 1 190 24 discriminator 2
	ldr	x0, [x29, -48]
	bl	_system__stream_attributes__i_ssu
	.loc 1 190 24 discriminator 3
	strb	w0, [x29, -98]
	.loc 1 191 26 is_stmt 1
	ldrsw	x0, [x29, -96]
	sub	x1, x29, #17
	ldrb	w2, [x29, -98]
	strb	w2, [x1, x0]
	.loc 1 189 11 discriminator 2
	ldr	w0, [x29, -96]
	add	w0, w0, 1
	str	w0, [x29, -96]
	.loc 1 192 15
	b	L80
L77:
LBE21:
	.loc 1 194 22
	ldr	x0, [x29, -16]
	mov	x1, 65
	bfi	x19, x1, 0, 8
	mov	x1, 78
	bfi	x19, x1, 8, 8
	mov	x1, 85
	bfi	x19, x1, 16, 8
	mov	x1, 66
	bfi	x19, x1, 24, 8
	mov	x1, 73
	bfi	x19, x1, 32, 8
	mov	x1, 83
	bfi	x19, x1, 40, 8
	mov	x1, 75
	bfi	x19, x1, 48, 8
	mov	x1, 1
	bfi	x19, x1, 56, 8
	mov	x1, x19
	.loc 1 194 7
	cmp	x0, x1
	beq	L81
	.loc 1 195 10
	sub	x0, x29, #56
	bl	_ada__streams__stream_io__close
	.loc 1 196 18
	strb	wzr, [x29, -99]
	.loc 1 197 10
	b	L123
L81:
LBB22:
	.loc 1 201 11
	mov	w0, 1
	str	w0, [x29, -92]
L86:
	.loc 1 201 11 is_stmt 0 discriminator 1
	ldr	w0, [x29, -92]
	cmp	w0, 2
	bgt	L83
	.loc 1 202 24 is_stmt 1
	ldr	x0, [x29, -48]
	cmp	x0, 0
	bne	L84
	.loc 1 202 24 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L85
	bl	___stack_chk_fail
L85:
	mov	w1, 202
	adrp	x0, lC2@PAGE
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Access_Check
L84:
	.loc 1 202 24 discriminator 2
	ldr	x0, [x29, -48]
	bl	_system__stream_attributes__i_ssu
	.loc 1 202 24 discriminator 3
	strb	w0, [x29, -98]
	.loc 1 203 28 is_stmt 1
	ldrsw	x0, [x29, -92]
	sub	x1, x29, #25
	ldrb	w2, [x29, -98]
	strb	w2, [x1, x0]
	.loc 1 201 11 discriminator 2
	ldr	w0, [x29, -92]
	add	w0, w0, 1
	str	w0, [x29, -92]
	.loc 1 204 15
	b	L86
L83:
LBE22:
	.loc 1 206 24
	ldrh	w1, [x29, -24]
	and	w0, w20, -256
	mov	w20, w0
	mov	w0, 1
	bfi	w20, w0, 8, 8
	and	w0, w20, 65535
	.loc 1 206 7
	cmp	w1, w0
	beq	L87
	.loc 1 207 10
	sub	x0, x29, #56
	bl	_ada__streams__stream_io__close
	.loc 1 208 18
	strb	wzr, [x29, -99]
	.loc 1 209 10
	b	L123
L87:
	.loc 1 213 21
	ldr	x0, [x29, -48]
	cmp	x0, 0
	bne	L88
	.loc 1 213 21 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L89
	bl	___stack_chk_fail
L89:
	mov	w1, 213
	adrp	x0, lC2@PAGE
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Access_Check
L88:
	.loc 1 213 21 discriminator 2
	ldr	x0, [x29, -48]
	bl	_system__stream_attributes__i_ssu
	.loc 1 213 21 discriminator 3
	strb	w0, [x29, -98]
	.loc 1 214 16 is_stmt 1
	ldrb	w0, [x29, -98]
	strb	w0, [x29, -97]
	.loc 1 216 7
	ldrb	w0, [x29, -97]
	cmp	w0, 1
	beq	L90
	.loc 1 217 10
	sub	x0, x29, #56
	bl	_ada__streams__stream_io__close
	.loc 1 218 18
	strb	wzr, [x29, -99]
	.loc 1 219 10
	b	L123
L90:
LBB23:
	.loc 1 223 11
	mov	w0, 1
	str	w0, [x29, -88]
L94:
	.loc 1 223 11 is_stmt 0 discriminator 1
	ldr	w0, [x29, -88]
	cmp	w0, 32
	bgt	L91
	.loc 1 224 24 is_stmt 1
	ldr	x0, [x29, -48]
	cmp	x0, 0
	bne	L92
	.loc 1 224 24 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L93
	bl	___stack_chk_fail
L93:
	mov	w1, 224
	adrp	x0, lC2@PAGE
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Access_Check
L92:
	.loc 1 224 24 discriminator 2
	ldr	x0, [x29, -48]
	bl	_system__stream_attributes__i_ssu
	.loc 1 224 24 discriminator 3
	strb	w0, [x29, -98]
	.loc 1 225 38 is_stmt 1
	ldrsw	x0, [x29, -88]
	ldr	x1, [x29, -136]
	add	x0, x1, x0
	ldrb	w1, [x29, -98]
	strb	w1, [x0, -1]
	.loc 1 223 11 discriminator 2
	ldr	w0, [x29, -88]
	add	w0, w0, 1
	str	w0, [x29, -88]
	.loc 1 226 15
	b	L94
L91:
LBE23:
LBB24:
	.loc 1 227 11
	mov	w0, 1
	str	w0, [x29, -84]
L98:
	.loc 1 227 11 is_stmt 0 discriminator 1
	ldr	w0, [x29, -84]
	cmp	w0, 32
	bgt	L95
	.loc 1 228 24 is_stmt 1
	ldr	x0, [x29, -48]
	cmp	x0, 0
	bne	L96
	.loc 1 228 24 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L97
	bl	___stack_chk_fail
L97:
	mov	w1, 228
	adrp	x0, lC2@PAGE
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Access_Check
L96:
	.loc 1 228 24 discriminator 2
	ldr	x0, [x29, -48]
	bl	_system__stream_attributes__i_ssu
	.loc 1 228 24 discriminator 3
	strb	w0, [x29, -98]
	.loc 1 229 38 is_stmt 1
	ldrsw	x0, [x29, -84]
	ldr	x1, [x29, -136]
	add	x0, x1, x0
	ldrb	w1, [x29, -98]
	strb	w1, [x0, 31]
	.loc 1 227 11 discriminator 2
	ldr	w0, [x29, -84]
	add	w0, w0, 1
	str	w0, [x29, -84]
	.loc 1 230 15
	b	L98
L95:
LBE24:
	.loc 1 231 32
	ldr	x0, [x29, -136]
	mov	w1, 1
	strb	w1, [x0, 64]
LBB25:
	.loc 1 234 11
	mov	w0, 1
	str	w0, [x29, -80]
L102:
	.loc 1 234 11 is_stmt 0 discriminator 1
	ldr	w0, [x29, -80]
	cmp	w0, 1568
	bgt	L99
	.loc 1 235 24 is_stmt 1
	ldr	x0, [x29, -48]
	cmp	x0, 0
	bne	L100
	.loc 1 235 24 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L101
	bl	___stack_chk_fail
L101:
	mov	w1, 235
	adrp	x0, lC2@PAGE
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Access_Check
L100:
	.loc 1 235 24 discriminator 2
	ldr	x0, [x29, -48]
	bl	_system__stream_attributes__i_ssu
	.loc 1 235 24 discriminator 3
	strb	w0, [x29, -98]
	.loc 1 236 38 is_stmt 1
	ldrsw	x0, [x29, -80]
	ldr	x1, [x29, -136]
	add	x0, x1, x0
	ldrb	w1, [x29, -98]
	strb	w1, [x0, 129]
	.loc 1 234 11 discriminator 2
	ldr	w0, [x29, -80]
	add	w0, w0, 1
	str	w0, [x29, -80]
	.loc 1 237 15
	b	L102
L99:
LBE25:
LBB26:
	.loc 1 238 11
	mov	w0, 1
	str	w0, [x29, -76]
L106:
	.loc 1 238 11 is_stmt 0 discriminator 1
	ldr	w0, [x29, -76]
	cmp	w0, 3168
	bgt	L103
	.loc 1 239 24 is_stmt 1
	ldr	x0, [x29, -48]
	cmp	x0, 0
	bne	L104
	.loc 1 239 24 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L105
	bl	___stack_chk_fail
L105:
	mov	w1, 239
	adrp	x0, lC2@PAGE
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Access_Check
L104:
	.loc 1 239 24 discriminator 2
	ldr	x0, [x29, -48]
	bl	_system__stream_attributes__i_ssu
	.loc 1 239 24 discriminator 3
	strb	w0, [x29, -98]
	.loc 1 240 38 is_stmt 1
	ldrsw	x0, [x29, -76]
	ldr	x1, [x29, -136]
	add	x0, x1, x0
	ldrb	w1, [x29, -98]
	strb	w1, [x0, 1697]
	.loc 1 238 11 discriminator 2
	ldr	w0, [x29, -76]
	add	w0, w0, 1
	str	w0, [x29, -76]
	.loc 1 241 15
	b	L106
L103:
LBE26:
	.loc 1 242 32
	ldr	x0, [x29, -136]
	add	x0, x0, 4096
	mov	w1, 1
	strb	w1, [x0, 770]
LBB27:
	.loc 1 245 11
	mov	w0, 1
	str	w0, [x29, -72]
L110:
	.loc 1 245 11 is_stmt 0 discriminator 1
	ldr	w0, [x29, -72]
	cmp	w0, 32
	bgt	L107
	.loc 1 246 24 is_stmt 1
	ldr	x0, [x29, -48]
	cmp	x0, 0
	bne	L108
	.loc 1 246 24 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L109
	bl	___stack_chk_fail
L109:
	mov	w1, 246
	adrp	x0, lC2@PAGE
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Access_Check
L108:
	.loc 1 246 24 discriminator 2
	ldr	x0, [x29, -48]
	bl	_system__stream_attributes__i_ssu
	.loc 1 246 24 discriminator 3
	strb	w0, [x29, -98]
	.loc 1 247 39 is_stmt 1
	ldrsw	x0, [x29, -72]
	ldr	x1, [x29, -136]
	add	x0, x1, x0
	ldrb	w1, [x29, -98]
	strb	w1, [x0, 64]
	.loc 1 245 11 discriminator 2
	ldr	w0, [x29, -72]
	add	w0, w0, 1
	str	w0, [x29, -72]
	.loc 1 248 15
	b	L110
L107:
LBE27:
LBB28:
	.loc 1 249 11
	mov	w0, 1
	str	w0, [x29, -68]
L114:
	.loc 1 249 11 is_stmt 0 discriminator 1
	ldr	w0, [x29, -68]
	cmp	w0, 32
	bgt	L111
	.loc 1 250 24 is_stmt 1
	ldr	x0, [x29, -48]
	cmp	x0, 0
	bne	L112
	.loc 1 250 24 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L113
	bl	___stack_chk_fail
L113:
	mov	w1, 250
	adrp	x0, lC2@PAGE
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Access_Check
L112:
	.loc 1 250 24 discriminator 2
	ldr	x0, [x29, -48]
	bl	_system__stream_attributes__i_ssu
	.loc 1 250 24 discriminator 3
	strb	w0, [x29, -98]
	.loc 1 251 39 is_stmt 1
	ldrsw	x0, [x29, -68]
	ldr	x1, [x29, -136]
	add	x0, x1, x0
	ldrb	w1, [x29, -98]
	strb	w1, [x0, 96]
	.loc 1 249 11 discriminator 2
	ldr	w0, [x29, -68]
	add	w0, w0, 1
	str	w0, [x29, -68]
	.loc 1 252 15
	b	L114
L111:
LBE28:
	.loc 1 253 33
	ldr	x0, [x29, -136]
	mov	w1, 1
	strb	w1, [x0, 129]
LBB29:
	.loc 1 256 11
	mov	w0, 1
	str	w0, [x29, -64]
L118:
	.loc 1 256 11 is_stmt 0 discriminator 1
	ldr	w0, [x29, -64]
	cmp	w0, 2592
	bgt	L115
	.loc 1 257 24 is_stmt 1
	ldr	x0, [x29, -48]
	cmp	x0, 0
	bne	L116
	.loc 1 257 24 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L117
	bl	___stack_chk_fail
L117:
	mov	w1, 257
	adrp	x0, lC2@PAGE
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Access_Check
L116:
	.loc 1 257 24 discriminator 2
	ldr	x0, [x29, -48]
	bl	_system__stream_attributes__i_ssu
	.loc 1 257 24 discriminator 3
	strb	w0, [x29, -98]
	.loc 1 258 38 is_stmt 1
	ldrsw	x0, [x29, -64]
	ldr	x1, [x29, -136]
	add	x0, x1, x0
	add	x0, x0, 4096
	ldrb	w1, [x29, -98]
	strb	w1, [x0, 770]
	.loc 1 256 11 discriminator 2
	ldr	w0, [x29, -64]
	add	w0, w0, 1
	str	w0, [x29, -64]
	.loc 1 259 15
	b	L118
L115:
LBE29:
LBB30:
	.loc 1 260 11
	mov	w0, 1
	str	w0, [x29, -60]
L122:
	.loc 1 260 11 is_stmt 0 discriminator 1
	ldr	w1, [x29, -60]
	mov	w0, 4896
	cmp	w1, w0
	bgt	L119
	.loc 1 261 24 is_stmt 1
	ldr	x0, [x29, -48]
	cmp	x0, 0
	bne	L120
	.loc 1 261 24 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L121
	bl	___stack_chk_fail
L121:
	mov	w1, 261
	adrp	x0, lC2@PAGE
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Access_Check
L120:
	.loc 1 261 24 discriminator 2
	ldr	x0, [x29, -48]
	bl	_system__stream_attributes__i_ssu
	.loc 1 261 24 discriminator 3
	strb	w0, [x29, -98]
	.loc 1 262 38 is_stmt 1
	ldrsw	x0, [x29, -60]
	ldr	x1, [x29, -136]
	add	x0, x1, x0
	add	x0, x0, 4096
	ldrb	w1, [x29, -98]
	strb	w1, [x0, 3362]
	.loc 1 260 11 discriminator 2
	ldr	w0, [x29, -60]
	add	w0, w0, 1
	str	w0, [x29, -60]
	.loc 1 263 15
	b	L122
L119:
LBE30:
	.loc 1 264 32
	ldr	x0, [x29, -136]
	add	x0, x0, 12288
	mov	w1, 1
	strb	w1, [x0, 67]
	.loc 1 266 7
	sub	x0, x29, #56
	bl	_ada__streams__stream_io__close
	.loc 1 267 22
	ldr	x0, [x29, -136]
	add	x0, x0, 12288
	mov	w1, 1
	strb	w1, [x0, 68]
	.loc 1 268 15
	mov	w0, 1
	strb	w0, [x29, -99]
	.loc 1 269 8
	nop
L123:
LBE18:
	.loc 1 269 8 is_stmt 0 discriminator 1
	ldrb	w0, [x29, -99]
	.loc 1 269 8 discriminator 3
	mov	w1, w0
	.loc 1 269 8
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x3, [x29, -8]
	ldr	x2, [x0]
	subs	x3, x3, x2
	mov	x2, 0
	beq	L128
	b	L130
L129:
	.loc 1 181 10 is_stmt 1
	cmp	x1, 1
	beq	L126
	mov	x1, x0
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x3, [x29, -8]
	ldr	x2, [x0]
	subs	x3, x3, x2
	mov	x2, 0
	beq	L127
	bl	___stack_chk_fail
L127:
	mov	x0, x1
	bl	__Unwind_Resume
L126:
LBB32:
LBB31:
	.loc 1 181 10 is_stmt 0 discriminator 1
	str	x0, [x29, -40]
	ldr	x0, [x29, -40]
	bl	___gnat_begin_handler_v1
	str	x0, [x29, -32]
	.loc 1 182 21 is_stmt 1
	strb	wzr, [x29, -99]
	.loc 1 183 13
	nop
	.loc 1 181 10
	mov	x2, 0
	ldr	x1, [x29, -32]
	ldr	x0, [x29, -40]
	bl	___gnat_end_handler_v1
	b	L123
L130:
LBE31:
LBE32:
	.loc 1 269 8
	bl	___stack_chk_fail
L128:
	mov	w0, w1
	ldp	x29, x30, [sp, 144]
	ldp	x19, x20, [sp, 160]
LEHE5:
	add	sp, sp, 176
LCFI15:
	ret
LFE5:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table1:
	.align	2
LLSDA5:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT5-LLSDATTD5
LLSDATTD5:
	.byte	0x1
	.uleb128 LLSDACSE5-LLSDACSB5
LLSDACSB5:
	.uleb128 LEHB3-LFB5
	.uleb128 LEHE3-LEHB3
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB4-LFB5
	.uleb128 LEHE4-LEHB4
	.uleb128 L129-LFB5
	.uleb128 0x1
	.uleb128 LEHB5-LFB5
	.uleb128 LEHE5-LEHB5
	.uleb128 0
	.uleb128 0
LLSDACSE5:
	.byte	0x1
	.byte	0
	.align	2
L_got_pcr1:
	.long	___gnat_others_value@GOT-L_got_pcr1
LLSDATT5:
	.text
	.align	2
	.globl _anubis_types__storage__get_x25519_public
_anubis_types__storage__get_x25519_public:
LFB6:
	.loc 1 275 4
	sub	sp, sp, #16
LCFI16:
	mov	x2, x8
	str	x0, [sp, 8]
	.loc 1 277 7
	ldr	x1, [sp, 8]
	mov	x0, x2
	ldr	q30, [x1]
	ldr	q31, [x1, 16]
	str	q30, [x0]
	str	q31, [x0, 16]
	.loc 1 278 8
	add	sp, sp, 16
LCFI17:
	ret
LFE6:
	.align	2
	.globl _anubis_types__storage__get_x25519_secret
_anubis_types__storage__get_x25519_secret:
LFB7:
	.loc 1 280 4
	sub	sp, sp, #16
LCFI18:
	mov	x2, x8
	str	x0, [sp, 8]
	.loc 1 282 7
	ldr	x0, [sp, 8]
	add	x1, x0, 32
	mov	x0, x2
	ldr	q30, [x1]
	ldr	q31, [x1, 16]
	ldrb	w1, [x1, 32]
	str	q30, [x0]
	str	q31, [x0, 16]
	strb	w1, [x0, 32]
	.loc 1 283 8
	add	sp, sp, 16
LCFI19:
	ret
LFE7:
	.align	2
	.globl _anubis_types__storage__get_ml_kem_public
_anubis_types__storage__get_ml_kem_public:
LFB8:
	.loc 1 285 4
	sub	x10, sp, #16384
	str	xzr, [x10, 4064]
	stp	x29, x30, [sp, -32]!
LCFI20:
	mov	x29, sp
LCFI21:
	mov	x1, x8
	str	x0, [x29, 24]
	.loc 1 287 7
	ldr	x0, [x29, 24]
	add	x0, x0, 130
	mov	x3, x1
	mov	x1, x0
	mov	x0, 1568
	mov	x2, x0
	mov	x0, x3
	bl	_memcpy
	.loc 1 288 8
	ldp	x29, x30, [sp], 32
LCFI22:
	ret
LFE8:
	.align	2
	.globl _anubis_types__storage__get_ml_kem_secret
_anubis_types__storage__get_ml_kem_secret:
LFB9:
	.loc 1 290 4
	sub	x10, sp, #16384
	str	xzr, [x10, 4064]
	stp	x29, x30, [sp, -32]!
LCFI23:
	mov	x29, sp
LCFI24:
	mov	x1, x8
	str	x0, [x29, 24]
	.loc 1 292 7
	ldr	x0, [x29, 24]
	add	x0, x0, 2048
	sub	x0, x0, #350
	mov	x3, x1
	mov	x1, x0
	mov	x0, 3169
	mov	x2, x0
	mov	x0, x3
	bl	_memcpy
	.loc 1 293 8
	ldp	x29, x30, [sp], 32
LCFI25:
	ret
LFE9:
	.align	2
	.globl _anubis_types__storage__get_ed25519_public
_anubis_types__storage__get_ed25519_public:
LFB10:
	.loc 1 295 4
	sub	sp, sp, #16
LCFI26:
	mov	x2, x8
	str	x0, [sp, 8]
	.loc 1 297 7
	ldr	x0, [sp, 8]
	add	x1, x0, 65
	mov	x0, x2
	ldr	q30, [x1]
	ldr	q31, [x1, 16]
	str	q30, [x0]
	str	q31, [x0, 16]
	.loc 1 298 8
	add	sp, sp, 16
LCFI27:
	ret
LFE10:
	.align	2
	.globl _anubis_types__storage__get_ed25519_secret
_anubis_types__storage__get_ed25519_secret:
LFB11:
	.loc 1 300 4
	sub	sp, sp, #16
LCFI28:
	mov	x2, x8
	str	x0, [sp, 8]
	.loc 1 302 7
	ldr	x0, [sp, 8]
	add	x1, x0, 97
	mov	x0, x2
	ldr	q30, [x1]
	ldr	q31, [x1, 16]
	ldrb	w1, [x1, 32]
	str	q30, [x0]
	str	q31, [x0, 16]
	strb	w1, [x0, 32]
	.loc 1 303 8
	add	sp, sp, 16
LCFI29:
	ret
LFE11:
	.align	2
	.globl _anubis_types__storage__get_ml_dsa_public
_anubis_types__storage__get_ml_dsa_public:
LFB12:
	.loc 1 305 4
	sub	x10, sp, #16384
	str	xzr, [x10, 4064]
	stp	x29, x30, [sp, -32]!
LCFI30:
	mov	x29, sp
LCFI31:
	mov	x2, x8
	str	x0, [x29, 24]
	.loc 1 307 7
	ldr	x1, [x29, 24]
	mov	x0, 5120
	add	x0, x1, x0
	sub	x0, x0, #253
	mov	x3, x2
	mov	x1, x0
	mov	x0, 2592
	mov	x2, x0
	mov	x0, x3
	bl	_memcpy
	.loc 1 308 8
	ldp	x29, x30, [sp], 32
LCFI32:
	ret
LFE12:
	.align	2
	.globl _anubis_types__storage__get_ml_dsa_secret
_anubis_types__storage__get_ml_dsa_secret:
LFB13:
	.loc 1 310 4
	sub	x10, sp, #16384
	str	xzr, [x10, 4064]
	stp	x29, x30, [sp, -32]!
LCFI33:
	mov	x29, sp
LCFI34:
	mov	x2, x8
	str	x0, [x29, 24]
	.loc 1 312 7
	ldr	x1, [x29, 24]
	mov	x0, 7168
	add	x0, x1, x0
	add	x0, x0, 291
	mov	x3, x2
	mov	x1, x0
	mov	x0, 4897
	mov	x2, x0
	mov	x0, x3
	bl	_memcpy
	.loc 1 313 8
	ldp	x29, x30, [sp], 32
LCFI35:
	ret
LFE13:
	.align	2
	.globl _anubis_types__storage__zeroize_identity
_anubis_types__storage__zeroize_identity:
LFB14:
	.loc 1 319 4
	sub	x10, sp, #16384
	str	xzr, [x10, 4064]
	stp	x29, x30, [sp, -32]!
LCFI36:
	mov	x29, sp
LCFI37:
	str	x0, [x29, 24]
	.loc 1 321 16
	ldr	x0, [x29, 24]
	add	x0, x0, 32
	bl	_anubis_types__classical__zeroize_x25519_secret
	.loc 1 322 16
	ldr	x0, [x29, 24]
	add	x0, x0, 97
	bl	_anubis_types__classical__zeroize_ed25519_secret
	.loc 1 323 10
	ldr	x0, [x29, 24]
	add	x0, x0, 1698
	bl	_anubis_types__pqc__zeroize_ml_kem_secret
	.loc 1 324 10
	ldr	x1, [x29, 24]
	mov	x0, 7459
	add	x0, x1, x0
	bl	_anubis_types__pqc__zeroize_ml_dsa_secret
	.loc 1 325 22
	ldr	x0, [x29, 24]
	add	x0, x0, 12288
	strb	wzr, [x0, 68]
	.loc 1 326 8
	nop
	ldp	x29, x30, [sp], 32
LCFI38:
	ret
LFE14:
	.globl _anubis_types__storage_E
	.data
	.align	1
_anubis_types__storage_E:
	.space 2
	.const
_anubis_types__storage__key_type_identity:
	.byte	1
	.align	3
_anubis_types__storage__magic:
	.byte	65
	.byte	78
	.byte	85
	.byte	66
	.byte	73
	.byte	83
	.byte	75
	.byte	1
	.align	1
_anubis_types__storage__version:
	.byte	0
	.byte	1
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
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$5,LCFI1-LCFI0
	.long L$set$5
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$6,LCFI2-LCFI1
	.long L$set$6
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE0:
LSFDE2:
	.set L$set$7,LEFDE2-LASFDE2
	.long L$set$7
LASFDE2:
	.set L$set$8,Lframe0-Lsection__debug_frame
	.long L$set$8
	.quad	LFB3
	.set L$set$9,LFE3-LFB3
	.quad L$set$9
	.byte	0x4
	.set L$set$10,LCFI3-LFB3
	.long L$set$10
	.byte	0xe
	.uleb128 0x30
	.byte	0x4
	.set L$set$11,LCFI4-LCFI3
	.long L$set$11
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$12,LCFI5-LCFI4
	.long L$set$12
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x4
	.set L$set$13,LCFI6-LCFI5
	.long L$set$13
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE2:
LSFDE4:
	.set L$set$14,LEFDE4-LASFDE4
	.long L$set$14
LASFDE4:
	.set L$set$15,Lframe0-Lsection__debug_frame
	.long L$set$15
	.quad	LFB4
	.set L$set$16,LFE4-LFB4
	.quad L$set$16
	.byte	0x4
	.set L$set$17,LCFI7-LFB4
	.long L$set$17
	.byte	0xe
	.uleb128 0x90
	.byte	0x4
	.set L$set$18,LCFI8-LCFI7
	.long L$set$18
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$19,LCFI9-LCFI8
	.long L$set$19
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x4
	.set L$set$20,LCFI10-LCFI9
	.long L$set$20
	.byte	0xdd
	.byte	0xde
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
	.uleb128 0xb0
	.byte	0x4
	.set L$set$25,LCFI12-LCFI11
	.long L$set$25
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$26,LCFI13-LCFI12
	.long L$set$26
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x20
	.byte	0x4
	.set L$set$27,LCFI14-LCFI13
	.long L$set$27
	.byte	0x93
	.uleb128 0x2
	.byte	0x94
	.uleb128 0x1
	.byte	0x4
	.set L$set$28,LCFI15-LCFI14
	.long L$set$28
	.byte	0xd3
	.byte	0xd4
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE6:
LSFDE8:
	.set L$set$29,LEFDE8-LASFDE8
	.long L$set$29
LASFDE8:
	.set L$set$30,Lframe0-Lsection__debug_frame
	.long L$set$30
	.quad	LFB6
	.set L$set$31,LFE6-LFB6
	.quad L$set$31
	.byte	0x4
	.set L$set$32,LCFI16-LFB6
	.long L$set$32
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$33,LCFI17-LCFI16
	.long L$set$33
	.byte	0xe
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
	.uleb128 0x10
	.byte	0x4
	.set L$set$38,LCFI19-LCFI18
	.long L$set$38
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE10:
LSFDE12:
	.set L$set$39,LEFDE12-LASFDE12
	.long L$set$39
LASFDE12:
	.set L$set$40,Lframe0-Lsection__debug_frame
	.long L$set$40
	.quad	LFB8
	.set L$set$41,LFE8-LFB8
	.quad L$set$41
	.byte	0x4
	.set L$set$42,LCFI20-LFB8
	.long L$set$42
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$43,LCFI21-LCFI20
	.long L$set$43
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$44,LCFI22-LCFI21
	.long L$set$44
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE12:
LSFDE14:
	.set L$set$45,LEFDE14-LASFDE14
	.long L$set$45
LASFDE14:
	.set L$set$46,Lframe0-Lsection__debug_frame
	.long L$set$46
	.quad	LFB9
	.set L$set$47,LFE9-LFB9
	.quad L$set$47
	.byte	0x4
	.set L$set$48,LCFI23-LFB9
	.long L$set$48
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$49,LCFI24-LCFI23
	.long L$set$49
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$50,LCFI25-LCFI24
	.long L$set$50
	.byte	0xde
	.byte	0xdd
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
	.uleb128 0x10
	.byte	0x4
	.set L$set$55,LCFI27-LCFI26
	.long L$set$55
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE16:
LSFDE18:
	.set L$set$56,LEFDE18-LASFDE18
	.long L$set$56
LASFDE18:
	.set L$set$57,Lframe0-Lsection__debug_frame
	.long L$set$57
	.quad	LFB11
	.set L$set$58,LFE11-LFB11
	.quad L$set$58
	.byte	0x4
	.set L$set$59,LCFI28-LFB11
	.long L$set$59
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$60,LCFI29-LCFI28
	.long L$set$60
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE18:
LSFDE20:
	.set L$set$61,LEFDE20-LASFDE20
	.long L$set$61
LASFDE20:
	.set L$set$62,Lframe0-Lsection__debug_frame
	.long L$set$62
	.quad	LFB12
	.set L$set$63,LFE12-LFB12
	.quad L$set$63
	.byte	0x4
	.set L$set$64,LCFI30-LFB12
	.long L$set$64
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$65,LCFI31-LCFI30
	.long L$set$65
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$66,LCFI32-LCFI31
	.long L$set$66
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE20:
LSFDE22:
	.set L$set$67,LEFDE22-LASFDE22
	.long L$set$67
LASFDE22:
	.set L$set$68,Lframe0-Lsection__debug_frame
	.long L$set$68
	.quad	LFB13
	.set L$set$69,LFE13-LFB13
	.quad L$set$69
	.byte	0x4
	.set L$set$70,LCFI33-LFB13
	.long L$set$70
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$71,LCFI34-LCFI33
	.long L$set$71
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$72,LCFI35-LCFI34
	.long L$set$72
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE22:
LSFDE24:
	.set L$set$73,LEFDE24-LASFDE24
	.long L$set$73
LASFDE24:
	.set L$set$74,Lframe0-Lsection__debug_frame
	.long L$set$74
	.quad	LFB14
	.set L$set$75,LFE14-LFB14
	.quad L$set$75
	.byte	0x4
	.set L$set$76,LCFI36-LFB14
	.long L$set$76
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$77,LCFI37-LCFI36
	.long L$set$77
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$78,LCFI38-LCFI37
	.long L$set$78
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE24:
	.section __TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
EH_frame1:
	.set L$set$79,LECIE1-LSCIE1
	.long L$set$79
LSCIE1:
	.long	0
	.byte	0x3
	.ascii "zPLR\0"
	.uleb128 0x1
	.sleb128 -8
	.uleb128 0x1e
	.uleb128 0x7
	.byte	0x9b
L_got_pcr2:
	.long	___gnat_personality_v0@GOT-L_got_pcr2
	.byte	0x10
	.byte	0x10
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LECIE1:
LSFDE27:
	.set L$set$80,LEFDE27-LASFDE27
	.long L$set$80
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB2-.
	.set L$set$81,LFE2-LFB2
	.quad L$set$81
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$82,LCFI0-LFB2
	.long L$set$82
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$83,LCFI1-LCFI0
	.long L$set$83
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$84,LCFI2-LCFI1
	.long L$set$84
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE27:
LSFDE29:
	.set L$set$85,LEFDE29-LASFDE29
	.long L$set$85
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB3-.
	.set L$set$86,LFE3-LFB3
	.quad L$set$86
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$87,LCFI3-LFB3
	.long L$set$87
	.byte	0xe
	.uleb128 0x30
	.byte	0x4
	.set L$set$88,LCFI4-LCFI3
	.long L$set$88
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$89,LCFI5-LCFI4
	.long L$set$89
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x4
	.set L$set$90,LCFI6-LCFI5
	.long L$set$90
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE29:
LSFDE31:
	.set L$set$91,LEFDE31-LASFDE31
	.long L$set$91
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB4-.
	.set L$set$92,LFE4-LFB4
	.quad L$set$92
	.uleb128 0x8
	.quad	LLSDA4-.
	.byte	0x4
	.set L$set$93,LCFI7-LFB4
	.long L$set$93
	.byte	0xe
	.uleb128 0x90
	.byte	0x4
	.set L$set$94,LCFI8-LCFI7
	.long L$set$94
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$95,LCFI9-LCFI8
	.long L$set$95
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x4
	.set L$set$96,LCFI10-LCFI9
	.long L$set$96
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE31:
LSFDE33:
	.set L$set$97,LEFDE33-LASFDE33
	.long L$set$97
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB5-.
	.set L$set$98,LFE5-LFB5
	.quad L$set$98
	.uleb128 0x8
	.quad	LLSDA5-.
	.byte	0x4
	.set L$set$99,LCFI11-LFB5
	.long L$set$99
	.byte	0xe
	.uleb128 0xb0
	.byte	0x4
	.set L$set$100,LCFI12-LCFI11
	.long L$set$100
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$101,LCFI13-LCFI12
	.long L$set$101
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x20
	.byte	0x4
	.set L$set$102,LCFI14-LCFI13
	.long L$set$102
	.byte	0x93
	.uleb128 0x2
	.byte	0x94
	.uleb128 0x1
	.byte	0x4
	.set L$set$103,LCFI15-LCFI14
	.long L$set$103
	.byte	0xd3
	.byte	0xd4
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE33:
LSFDE35:
	.set L$set$104,LEFDE35-LASFDE35
	.long L$set$104
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB6-.
	.set L$set$105,LFE6-LFB6
	.quad L$set$105
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$106,LCFI16-LFB6
	.long L$set$106
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$107,LCFI17-LCFI16
	.long L$set$107
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE35:
LSFDE37:
	.set L$set$108,LEFDE37-LASFDE37
	.long L$set$108
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB7-.
	.set L$set$109,LFE7-LFB7
	.quad L$set$109
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$110,LCFI18-LFB7
	.long L$set$110
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$111,LCFI19-LCFI18
	.long L$set$111
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE37:
LSFDE39:
	.set L$set$112,LEFDE39-LASFDE39
	.long L$set$112
LASFDE39:
	.long	LASFDE39-EH_frame1
	.quad	LFB8-.
	.set L$set$113,LFE8-LFB8
	.quad L$set$113
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$114,LCFI20-LFB8
	.long L$set$114
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$115,LCFI21-LCFI20
	.long L$set$115
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$116,LCFI22-LCFI21
	.long L$set$116
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE39:
LSFDE41:
	.set L$set$117,LEFDE41-LASFDE41
	.long L$set$117
LASFDE41:
	.long	LASFDE41-EH_frame1
	.quad	LFB9-.
	.set L$set$118,LFE9-LFB9
	.quad L$set$118
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$119,LCFI23-LFB9
	.long L$set$119
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$120,LCFI24-LCFI23
	.long L$set$120
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$121,LCFI25-LCFI24
	.long L$set$121
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE41:
LSFDE43:
	.set L$set$122,LEFDE43-LASFDE43
	.long L$set$122
LASFDE43:
	.long	LASFDE43-EH_frame1
	.quad	LFB10-.
	.set L$set$123,LFE10-LFB10
	.quad L$set$123
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$124,LCFI26-LFB10
	.long L$set$124
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$125,LCFI27-LCFI26
	.long L$set$125
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE43:
LSFDE45:
	.set L$set$126,LEFDE45-LASFDE45
	.long L$set$126
LASFDE45:
	.long	LASFDE45-EH_frame1
	.quad	LFB11-.
	.set L$set$127,LFE11-LFB11
	.quad L$set$127
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$128,LCFI28-LFB11
	.long L$set$128
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$129,LCFI29-LCFI28
	.long L$set$129
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE45:
LSFDE47:
	.set L$set$130,LEFDE47-LASFDE47
	.long L$set$130
LASFDE47:
	.long	LASFDE47-EH_frame1
	.quad	LFB12-.
	.set L$set$131,LFE12-LFB12
	.quad L$set$131
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$132,LCFI30-LFB12
	.long L$set$132
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$133,LCFI31-LCFI30
	.long L$set$133
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$134,LCFI32-LCFI31
	.long L$set$134
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE47:
LSFDE49:
	.set L$set$135,LEFDE49-LASFDE49
	.long L$set$135
LASFDE49:
	.long	LASFDE49-EH_frame1
	.quad	LFB13-.
	.set L$set$136,LFE13-LFB13
	.quad L$set$136
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$137,LCFI33-LFB13
	.long L$set$137
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$138,LCFI34-LCFI33
	.long L$set$138
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$139,LCFI35-LCFI34
	.long L$set$139
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE49:
LSFDE51:
	.set L$set$140,LEFDE51-LASFDE51
	.long L$set$140
LASFDE51:
	.long	LASFDE51-EH_frame1
	.quad	LFB14-.
	.set L$set$141,LFE14-LFB14
	.quad L$set$141
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$142,LCFI36-LFB14
	.long L$set$142
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$143,LCFI37-LCFI36
	.long L$set$143
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$144,LCFI38-LCFI37
	.long L$set$144
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE51:
	.text
Letext0:
	.file 2 "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_types.ads"
	.file 3 "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_types-storage.ads"
	.file 4 "/Users/sicarii/.local/share/alire/releases/gnatprove_14.1.1_91818ed8/libexec/spark/lib/gcc/aarch64-apple-darwin23.5.0/14.1.0/adainclude/a-tags.ads"
	.file 5 "/Users/sicarii/.local/share/alire/releases/gnatprove_14.1.1_91818ed8/libexec/spark/lib/gcc/aarch64-apple-darwin23.5.0/14.1.0/adainclude/a-stream.ads"
	.file 6 "/Users/sicarii/.local/share/alire/releases/gnatprove_14.1.1_91818ed8/libexec/spark/lib/gcc/aarch64-apple-darwin23.5.0/14.1.0/adainclude/s-ficobl.ads"
	.file 7 "<built-in>"
	.file 8 "/Users/sicarii/.local/share/alire/releases/gnatprove_14.1.1_91818ed8/libexec/spark/lib/gcc/aarch64-apple-darwin23.5.0/14.1.0/adainclude/s-crtl.ads"
	.file 9 "/Users/sicarii/.local/share/alire/releases/gnatprove_14.1.1_91818ed8/libexec/spark/lib/gcc/aarch64-apple-darwin23.5.0/14.1.0/adainclude/i-cstrea.ads"
	.file 10 "/Users/sicarii/.local/share/alire/releases/gnatprove_14.1.1_91818ed8/libexec/spark/lib/gcc/aarch64-apple-darwin23.5.0/14.1.0/adainclude/a-ststio.ads"
	.section __DWARF,__debug_info,regular,debug
Lsection__debug_info:
Ldebug_info0:
	.long	0x19d0
	.short	0x4
	.set L$set$145,Ldebug_abbrev0-Lsection__debug_abbrev
	.long L$set$145
	.byte	0x8
	.uleb128 0x1
	.ascii "GNU Ada 14.1.0 -O0 -gnatA -g -gnat2012 -gnatwa -gnatwe -fstack-check=specific -fstack-protector-strong -gnatR2js -gnatws -gnatec=/private/var/folders/bg/pt9l6y1j47q642kp3z5blrmh0000gn/T/GPR.71672/GNAT-TEMP-000003.TMP -gnatem=/private/var/folders/bg/pt9l6y1j47q642kp3z5blrmh0000gn/T/GPR.71672/GNAT-TEMP-000004.TMP -mmacosx-version-min=16.0.0 -mcpu=apple-m1 -mlittle-endian -mabi=lp64 -fPIC\0"
	.byte	0xd
	.ascii "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_types-storage.adb\0"
	.ascii "/Users/sicarii/Desktop/anubis-spark/obj/gnatprove/data_representation\0"
	.quad	Ltext0
	.set L$set$146,Letext0-Ltext0
	.quad L$set$146
	.set L$set$147,Ldebug_line0-Lsection__debug_line
	.long L$set$147
	.uleb128 0x2
	.ascii "anubis_types__storage__key_type_identity\0"
	.byte	0x1
	.byte	0x10
	.byte	0x4
	.long	0x289
	.uleb128 0x9
	.byte	0x3
	.quad	_anubis_types__storage__key_type_identity
	.uleb128 0x3
	.byte	0
	.byte	0xff
	.ascii "anubis_types__byte\0"
	.long	0x28e
	.uleb128 0x4
	.long	0x26f
	.uleb128 0x5
	.byte	0x1
	.byte	0x7
	.ascii "anubis_types__TbyteB\0"
	.uleb128 0x6
	.byte	0x1
	.byte	0x7
	.ascii "interfaces__unsigned_8\0"
	.uleb128 0x6
	.byte	0x4
	.byte	0x5
	.ascii "integer\0"
	.uleb128 0x7
	.ascii "anubis_types__x25519_public_key__T15s\0"
	.long	0x26f
	.long	0x301
	.uleb128 0x8
	.long	0x2c0
	.sleb128 32
	.byte	0
	.uleb128 0x9
	.ascii "anubis_types__x25519_public_key\0"
	.byte	0x20
	.byte	0x2
	.byte	0x90
	.byte	0x9
	.long	0x338
	.uleb128 0xa
	.set L$set$148,LASF0-Lsection__debug_str
	.long L$set$148
	.byte	0x2
	.byte	0x91
	.byte	0x7
	.long	0x2cb
	.byte	0
	.byte	0
	.uleb128 0x7
	.ascii "anubis_types__x25519_secret_key__T17s\0"
	.long	0x26f
	.long	0x36e
	.uleb128 0x8
	.long	0x2c0
	.sleb128 32
	.byte	0
	.uleb128 0x9
	.ascii "anubis_types__x25519_secret_key\0"
	.byte	0x21
	.byte	0x2
	.byte	0x94
	.byte	0x9
	.long	0x3b2
	.uleb128 0xa
	.set L$set$149,LASF0-Lsection__debug_str
	.long L$set$149
	.byte	0x2
	.byte	0x95
	.byte	0x7
	.long	0x338
	.byte	0
	.uleb128 0xa
	.set L$set$150,LASF1-Lsection__debug_str
	.long L$set$150
	.byte	0x2
	.byte	0x96
	.byte	0x7
	.long	0x3b2
	.byte	0x20
	.byte	0
	.uleb128 0x6
	.byte	0x1
	.byte	0x2
	.ascii "boolean\0"
	.uleb128 0x7
	.ascii "anubis_types__ed25519_public_key__T21s\0"
	.long	0x26f
	.long	0x3f4
	.uleb128 0x8
	.long	0x2c0
	.sleb128 32
	.byte	0
	.uleb128 0x9
	.ascii "anubis_types__ed25519_public_key\0"
	.byte	0x20
	.byte	0x2
	.byte	0x9e
	.byte	0x9
	.long	0x42c
	.uleb128 0xa
	.set L$set$151,LASF0-Lsection__debug_str
	.long L$set$151
	.byte	0x2
	.byte	0x9f
	.byte	0x7
	.long	0x3bd
	.byte	0
	.byte	0
	.uleb128 0x7
	.ascii "anubis_types__ed25519_secret_key__T23s\0"
	.long	0x26f
	.long	0x463
	.uleb128 0x8
	.long	0x2c0
	.sleb128 32
	.byte	0
	.uleb128 0x9
	.ascii "anubis_types__ed25519_secret_key\0"
	.byte	0x21
	.byte	0x2
	.byte	0xa2
	.byte	0x9
	.long	0x4a8
	.uleb128 0xa
	.set L$set$152,LASF0-Lsection__debug_str
	.long L$set$152
	.byte	0x2
	.byte	0xa3
	.byte	0x7
	.long	0x42c
	.byte	0
	.uleb128 0xa
	.set L$set$153,LASF1-Lsection__debug_str
	.long L$set$153
	.byte	0x2
	.byte	0xa4
	.byte	0x7
	.long	0x3b2
	.byte	0x20
	.byte	0
	.uleb128 0x7
	.ascii "anubis_types__ml_kem_public_key__T33s\0"
	.long	0x26f
	.long	0x4df
	.uleb128 0x8
	.long	0x2c0
	.sleb128 1568
	.byte	0
	.uleb128 0xb
	.ascii "anubis_types__ml_kem_public_key\0"
	.short	0x620
	.byte	0x2
	.byte	0xb8
	.byte	0x9
	.long	0x517
	.uleb128 0xa
	.set L$set$154,LASF0-Lsection__debug_str
	.long L$set$154
	.byte	0x2
	.byte	0xb9
	.byte	0x7
	.long	0x4a8
	.byte	0
	.byte	0
	.uleb128 0x7
	.ascii "anubis_types__ml_kem_secret_key__T35s\0"
	.long	0x26f
	.long	0x54e
	.uleb128 0x8
	.long	0x2c0
	.sleb128 3168
	.byte	0
	.uleb128 0xb
	.ascii "anubis_types__ml_kem_secret_key\0"
	.short	0xc61
	.byte	0x2
	.byte	0xbc
	.byte	0x9
	.long	0x594
	.uleb128 0xa
	.set L$set$155,LASF0-Lsection__debug_str
	.long L$set$155
	.byte	0x2
	.byte	0xbd
	.byte	0x7
	.long	0x517
	.byte	0
	.uleb128 0xc
	.set L$set$156,LASF1-Lsection__debug_str
	.long L$set$156
	.byte	0x2
	.byte	0xbe
	.byte	0x7
	.long	0x3b2
	.short	0xc60
	.byte	0
	.uleb128 0x7
	.ascii "anubis_types__ml_dsa_public_key__T41s\0"
	.long	0x26f
	.long	0x5cb
	.uleb128 0x8
	.long	0x2c0
	.sleb128 2592
	.byte	0
	.uleb128 0xb
	.ascii "anubis_types__ml_dsa_public_key\0"
	.short	0xa20
	.byte	0x2
	.byte	0xca
	.byte	0x9
	.long	0x603
	.uleb128 0xa
	.set L$set$157,LASF0-Lsection__debug_str
	.long L$set$157
	.byte	0x2
	.byte	0xcb
	.byte	0x7
	.long	0x594
	.byte	0
	.byte	0
	.uleb128 0x7
	.ascii "anubis_types__ml_dsa_secret_key__T43s\0"
	.long	0x26f
	.long	0x63a
	.uleb128 0x8
	.long	0x2c0
	.sleb128 4896
	.byte	0
	.uleb128 0xb
	.ascii "anubis_types__ml_dsa_secret_key\0"
	.short	0x1321
	.byte	0x2
	.byte	0xce
	.byte	0x9
	.long	0x680
	.uleb128 0xa
	.set L$set$158,LASF0-Lsection__debug_str
	.long L$set$158
	.byte	0x2
	.byte	0xcf
	.byte	0x7
	.long	0x603
	.byte	0
	.uleb128 0xc
	.set L$set$159,LASF1-Lsection__debug_str
	.long L$set$159
	.byte	0x2
	.byte	0xd0
	.byte	0x7
	.long	0x3b2
	.short	0x1320
	.byte	0
	.uleb128 0xb
	.ascii "anubis_types__storage__identity_keypair\0"
	.short	0x3045
	.byte	0x3
	.byte	0x42
	.byte	0x9
	.long	0x75e
	.uleb128 0xd
	.ascii "x25519_pk\0"
	.byte	0x3
	.byte	0x44
	.byte	0x7
	.long	0x301
	.byte	0
	.uleb128 0xd
	.ascii "x25519_sk\0"
	.byte	0x3
	.byte	0x45
	.byte	0x7
	.long	0x36e
	.byte	0x20
	.uleb128 0xd
	.ascii "ed25519_pk\0"
	.byte	0x3
	.byte	0x46
	.byte	0x7
	.long	0x3f4
	.byte	0x41
	.uleb128 0xd
	.ascii "ed25519_sk\0"
	.byte	0x3
	.byte	0x47
	.byte	0x7
	.long	0x463
	.byte	0x61
	.uleb128 0xd
	.ascii "ml_kem_pk\0"
	.byte	0x3
	.byte	0x4a
	.byte	0x7
	.long	0x4df
	.byte	0x82
	.uleb128 0xe
	.ascii "ml_kem_sk\0"
	.byte	0x3
	.byte	0x4b
	.byte	0x7
	.long	0x54e
	.short	0x6a2
	.uleb128 0xe
	.ascii "ml_dsa_pk\0"
	.byte	0x3
	.byte	0x4c
	.byte	0x7
	.long	0x5cb
	.short	0x1303
	.uleb128 0xe
	.ascii "ml_dsa_sk\0"
	.byte	0x3
	.byte	0x4d
	.byte	0x7
	.long	0x63a
	.short	0x1d23
	.uleb128 0xc
	.set L$set$160,LASF1-Lsection__debug_str
	.long L$set$160
	.byte	0x3
	.byte	0x50
	.byte	0x7
	.long	0x3b2
	.short	0x3044
	.byte	0
	.uleb128 0x7
	.ascii "anubis_types__storage__TmagicS\0"
	.long	0x26f
	.long	0x78d
	.uleb128 0x8
	.long	0x2c0
	.sleb128 8
	.byte	0
	.uleb128 0xf
	.long	0x75e
	.uleb128 0x7
	.ascii "anubis_types__storage__TversionS\0"
	.long	0x26f
	.long	0x7c3
	.uleb128 0x8
	.long	0x2c0
	.sleb128 2
	.byte	0
	.uleb128 0xf
	.long	0x792
	.uleb128 0x10
	.ascii "ada__tags__tag\0"
	.byte	0x4
	.short	0x10d
	.byte	0x9
	.long	0x7e0
	.uleb128 0x11
	.byte	0x8
	.long	0x7e6
	.uleb128 0x7
	.ascii "ada__tags__dispatch_table\0"
	.long	0x810
	.long	0x810
	.uleb128 0x8
	.long	0x2c0
	.sleb128 1
	.byte	0
	.uleb128 0x10
	.ascii "ada__tags__prim_ptr\0"
	.byte	0x4
	.short	0x105
	.byte	0x9
	.long	0x82d
	.uleb128 0x11
	.byte	0x8
	.long	0x833
	.uleb128 0x12
	.uleb128 0x9
	.ascii "ada__streams__root_stream_type\0"
	.byte	0x8
	.byte	0x5
	.byte	0x46
	.byte	0x9
	.long	0x86b
	.uleb128 0xd
	.ascii "_tag\0"
	.byte	0x5
	.byte	0x46
	.byte	0x35
	.long	0x7c8
	.byte	0
	.byte	0
	.uleb128 0x6
	.byte	0x8
	.byte	0x7
	.ascii "system__address\0"
	.uleb128 0x13
	.byte	0
	.quad	0xffffffffffffffff
	.ascii "interfaces__c_streams__files\0"
	.long	0x86b
	.uleb128 0x14
	.ascii "system__file_control_block__pstring\0"
	.byte	0x6
	.byte	0x3c
	.byte	0x9
	.long	0x8d5
	.uleb128 0x15
	.ascii "string\0"
	.byte	0x10
	.byte	0x7
	.byte	0
	.long	0x90c
	.uleb128 0x16
	.ascii "P_ARRAY\0"
	.byte	0x7
	.byte	0
	.long	0x8f4
	.byte	0
	.uleb128 0x11
	.byte	0x8
	.long	0x916
	.uleb128 0x16
	.ascii "P_BOUNDS\0"
	.byte	0x7
	.byte	0
	.long	0x990
	.byte	0x8
	.byte	0
	.uleb128 0xf
	.long	0x8d5
	.uleb128 0xf
	.long	0x8d5
	.uleb128 0x7
	.ascii "string___XUA\0"
	.long	0x942
	.long	0x942
	.uleb128 0x17
	.long	0x2c0
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
	.uleb128 0x6
	.byte	0x1
	.byte	0x8
	.ascii "character\0"
	.uleb128 0x15
	.ascii "string___XUB\0"
	.byte	0x8
	.byte	0x7
	.byte	0
	.long	0x990
	.uleb128 0x16
	.ascii "LB0\0"
	.byte	0x7
	.byte	0
	.long	0x970
	.byte	0
	.uleb128 0x18
	.sleb128 2147483647
	.ascii "positive\0"
	.long	0x2c0
	.uleb128 0x16
	.ascii "UB0\0"
	.byte	0x7
	.byte	0
	.long	0x970
	.byte	0x4
	.byte	0
	.uleb128 0x11
	.byte	0x8
	.long	0x94f
	.uleb128 0x19
	.ascii "system__crtl__filename_encoding\0"
	.byte	0x4
	.byte	0x8
	.byte	0x45
	.byte	0x9
	.long	0xa0d
	.uleb128 0x1a
	.ascii "system__crtl__utf8\0"
	.byte	0
	.uleb128 0x1a
	.ascii "system__crtl__ascii_8bits\0"
	.byte	0x1
	.uleb128 0x1a
	.ascii "system__crtl__unspecified\0"
	.byte	0x2
	.byte	0
	.uleb128 0x19
	.ascii "system__file_control_block__file_mode\0"
	.byte	0x1
	.byte	0x6
	.byte	0x3f
	.byte	0x9
	.long	0xadd
	.uleb128 0x1a
	.ascii "system__file_control_block__in_file\0"
	.byte	0
	.uleb128 0x1a
	.ascii "system__file_control_block__inout_file\0"
	.byte	0x1
	.uleb128 0x1a
	.ascii "system__file_control_block__out_file\0"
	.byte	0x2
	.uleb128 0x1a
	.ascii "system__file_control_block__append_file\0"
	.byte	0x3
	.byte	0
	.uleb128 0x19
	.ascii "interfaces__c_streams__content_encoding\0"
	.byte	0x4
	.byte	0x9
	.byte	0xe5
	.byte	0x9
	.long	0xbd1
	.uleb128 0x1a
	.ascii "interfaces__c_streams__none\0"
	.byte	0
	.uleb128 0x1a
	.ascii "interfaces__c_streams__default_text\0"
	.byte	0x1
	.uleb128 0x1a
	.ascii "interfaces__c_streams__text\0"
	.byte	0x2
	.uleb128 0x1a
	.ascii "interfaces__c_streams__u8text\0"
	.byte	0x3
	.uleb128 0x1a
	.ascii "interfaces__c_streams__wtext\0"
	.byte	0x4
	.uleb128 0x1a
	.ascii "interfaces__c_streams__u16text\0"
	.byte	0x5
	.byte	0
	.uleb128 0x19
	.ascii "system__file_control_block__shared_status_type\0"
	.byte	0x1
	.byte	0x6
	.byte	0x45
	.byte	0x9
	.long	0xc70
	.uleb128 0x1a
	.ascii "system__file_control_block__yes\0"
	.byte	0
	.uleb128 0x1a
	.ascii "system__file_control_block__no\0"
	.byte	0x1
	.uleb128 0x1a
	.ascii "system__file_control_block__none\0"
	.byte	0x2
	.byte	0
	.uleb128 0x14
	.ascii "system__file_control_block__afcb_ptr\0"
	.byte	0x6
	.byte	0x52
	.byte	0x9
	.long	0xc9d
	.uleb128 0x11
	.byte	0x8
	.long	0xca3
	.uleb128 0x9
	.ascii "system__file_control_block__afcb\0"
	.byte	0x58
	.byte	0x6
	.byte	0x54
	.byte	0x9
	.long	0xdd9
	.uleb128 0x1b
	.ascii "_parent\0"
	.byte	0x4
	.short	0x10d
	.byte	0x9
	.long	0x834
	.byte	0
	.uleb128 0xd
	.ascii "stream\0"
	.byte	0x6
	.byte	0x56
	.byte	0x7
	.long	0x87e
	.byte	0x8
	.uleb128 0xd
	.ascii "name\0"
	.byte	0x6
	.byte	0x59
	.byte	0x7
	.long	0x8a9
	.byte	0x10
	.uleb128 0xd
	.ascii "encoding\0"
	.byte	0x6
	.byte	0x5e
	.byte	0x7
	.long	0x996
	.byte	0x20
	.uleb128 0xd
	.ascii "form\0"
	.byte	0x6
	.byte	0x61
	.byte	0x7
	.long	0x8a9
	.byte	0x28
	.uleb128 0xd
	.ascii "mode\0"
	.byte	0x6
	.byte	0x66
	.byte	0x7
	.long	0xa0d
	.byte	0x38
	.uleb128 0xd
	.ascii "is_regular_file\0"
	.byte	0x6
	.byte	0x6a
	.byte	0x7
	.long	0x3b2
	.byte	0x39
	.uleb128 0xd
	.ascii "is_temporary_file\0"
	.byte	0x6
	.byte	0x6d
	.byte	0x7
	.long	0x3b2
	.byte	0x3a
	.uleb128 0xd
	.ascii "is_system_file\0"
	.byte	0x6
	.byte	0x71
	.byte	0x7
	.long	0x3b2
	.byte	0x3b
	.uleb128 0xd
	.ascii "text_encoding\0"
	.byte	0x6
	.byte	0x74
	.byte	0x7
	.long	0xadd
	.byte	0x3c
	.uleb128 0xd
	.ascii "shared_status\0"
	.byte	0x6
	.byte	0x77
	.byte	0x7
	.long	0xbd1
	.byte	0x40
	.uleb128 0xd
	.ascii "access_method\0"
	.byte	0x6
	.byte	0x7a
	.byte	0x7
	.long	0x942
	.byte	0x41
	.uleb128 0xd
	.ascii "next\0"
	.byte	0x6
	.byte	0x7e
	.byte	0x7
	.long	0xc70
	.byte	0x48
	.uleb128 0xd
	.ascii "prev\0"
	.byte	0x6
	.byte	0x7f
	.byte	0x7
	.long	0xc70
	.byte	0x50
	.byte	0
	.uleb128 0x1c
	.sleb128 0
	.sleb128 9223372036854775807
	.ascii "ada__streams__stream_io__count\0"
	.long	0xe08
	.uleb128 0x5
	.byte	0x8
	.byte	0x5
	.ascii "ada__streams__stream_io__TcountB\0"
	.uleb128 0x1c
	.sleb128 -9223372036854775808
	.sleb128 9223372036854775807
	.ascii "ada__streams__stream_element_offset\0"
	.long	0xe69
	.uleb128 0x5
	.byte	0x8
	.byte	0x5
	.ascii "ada__streams__Tstream_element_offsetB\0"
	.uleb128 0x19
	.ascii "ada__streams__stream_io__operation\0"
	.byte	0x1
	.byte	0xa
	.byte	0xb7
	.byte	0x9
	.long	0xf2a
	.uleb128 0x1a
	.ascii "ada__streams__stream_io__op_read\0"
	.byte	0
	.uleb128 0x1a
	.ascii "ada__streams__stream_io__op_write\0"
	.byte	0x1
	.uleb128 0x1a
	.ascii "ada__streams__stream_io__op_other\0"
	.byte	0x2
	.byte	0
	.uleb128 0x9
	.ascii "ada__streams__stream_io__stream_afcb\0"
	.byte	0x70
	.byte	0xa
	.byte	0xba
	.byte	0x9
	.long	0xfb2
	.uleb128 0xd
	.ascii "_parent\0"
	.byte	0x5
	.byte	0x46
	.byte	0x35
	.long	0xca3
	.byte	0
	.uleb128 0xd
	.ascii "index\0"
	.byte	0xa
	.byte	0xbb
	.byte	0x7
	.long	0xdd9
	.byte	0x58
	.uleb128 0xd
	.ascii "file_size\0"
	.byte	0xa
	.byte	0xbe
	.byte	0x7
	.long	0xe2c
	.byte	0x60
	.uleb128 0xd
	.ascii "last_op\0"
	.byte	0xa
	.byte	0xc3
	.byte	0x7
	.long	0xe92
	.byte	0x68
	.uleb128 0xd
	.ascii "update_mode\0"
	.byte	0xa
	.byte	0xc7
	.byte	0x7
	.long	0x3b2
	.byte	0x69
	.byte	0
	.uleb128 0x14
	.ascii "ada__streams__stream_io__file_type\0"
	.byte	0xa
	.byte	0xcd
	.byte	0x9
	.long	0xfdd
	.uleb128 0x11
	.byte	0x8
	.long	0xf2a
	.uleb128 0x14
	.ascii "ada__streams__stream_io__stream_access\0"
	.byte	0xa
	.byte	0x2a
	.byte	0x9
	.long	0x1012
	.uleb128 0x1d
	.byte	0x8
	.ascii "system__stream_attributes__T43s\0"
	.long	0x834
	.uleb128 0x6
	.byte	0x1
	.byte	0x7
	.ascii "system__unsigned_types__short_short_unsigned\0"
	.uleb128 0x6
	.byte	0x1
	.byte	0x7
	.ascii "ada__streams__stream_element\0"
	.uleb128 0x2
	.ascii "anubis_types__storage__magic\0"
	.byte	0x1
	.byte	0xe
	.byte	0x4
	.long	0x78d
	.uleb128 0x9
	.byte	0x3
	.quad	_anubis_types__storage__magic
	.uleb128 0x2
	.ascii "anubis_types__storage__version\0"
	.byte	0x1
	.byte	0xf
	.byte	0x4
	.long	0x7c3
	.uleb128 0x9
	.byte	0x3
	.quad	_anubis_types__storage__version
	.uleb128 0x1e
	.ascii "anubis_types__storage__zeroize_identity\0"
	.byte	0x1
	.short	0x13f
	.byte	0x4
	.quad	LFB14
	.set L$set$161,LFE14-LFB14
	.quad L$set$161
	.uleb128 0x1
	.byte	0x9c
	.long	0x113b
	.uleb128 0x1f
	.set L$set$162,LASF2-Lsection__debug_str
	.long L$set$162
	.byte	0x3
	.byte	0x3e
	.byte	0x20
	.long	0x113b
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.byte	0
	.uleb128 0x20
	.byte	0x8
	.long	0x680
	.uleb128 0x21
	.ascii "anubis_types__storage__get_ml_dsa_secret\0"
	.byte	0x1
	.short	0x136
	.byte	0x4
	.long	0x63a
	.quad	LFB13
	.set L$set$163,LFE13-LFB13
	.quad L$set$163
	.uleb128 0x1
	.byte	0x9c
	.long	0x1199
	.uleb128 0x1f
	.set L$set$164,LASF2-Lsection__debug_str
	.long L$set$164
	.byte	0x3
	.byte	0x3b
	.byte	0x20
	.long	0x113b
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.byte	0
	.uleb128 0x21
	.ascii "anubis_types__storage__get_ml_dsa_public\0"
	.byte	0x1
	.short	0x131
	.byte	0x4
	.long	0x5cb
	.quad	LFB12
	.set L$set$165,LFE12-LFB12
	.quad L$set$165
	.uleb128 0x1
	.byte	0x9c
	.long	0x11f1
	.uleb128 0x1f
	.set L$set$166,LASF2-Lsection__debug_str
	.long L$set$166
	.byte	0x3
	.byte	0x3a
	.byte	0x20
	.long	0x113b
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.byte	0
	.uleb128 0x21
	.ascii "anubis_types__storage__get_ed25519_secret\0"
	.byte	0x1
	.short	0x12c
	.byte	0x4
	.long	0x463
	.quad	LFB11
	.set L$set$167,LFE11-LFB11
	.quad L$set$167
	.uleb128 0x1
	.byte	0x9c
	.long	0x124a
	.uleb128 0x1f
	.set L$set$168,LASF2-Lsection__debug_str
	.long L$set$168
	.byte	0x3
	.byte	0x39
	.byte	0x21
	.long	0x113b
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.byte	0
	.uleb128 0x21
	.ascii "anubis_types__storage__get_ed25519_public\0"
	.byte	0x1
	.short	0x127
	.byte	0x4
	.long	0x3f4
	.quad	LFB10
	.set L$set$169,LFE10-LFB10
	.quad L$set$169
	.uleb128 0x1
	.byte	0x9c
	.long	0x12a3
	.uleb128 0x1f
	.set L$set$170,LASF2-Lsection__debug_str
	.long L$set$170
	.byte	0x3
	.byte	0x38
	.byte	0x21
	.long	0x113b
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.byte	0
	.uleb128 0x21
	.ascii "anubis_types__storage__get_ml_kem_secret\0"
	.byte	0x1
	.short	0x122
	.byte	0x4
	.long	0x54e
	.quad	LFB9
	.set L$set$171,LFE9-LFB9
	.quad L$set$171
	.uleb128 0x1
	.byte	0x9c
	.long	0x12fb
	.uleb128 0x1f
	.set L$set$172,LASF2-Lsection__debug_str
	.long L$set$172
	.byte	0x3
	.byte	0x37
	.byte	0x20
	.long	0x113b
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.byte	0
	.uleb128 0x21
	.ascii "anubis_types__storage__get_ml_kem_public\0"
	.byte	0x1
	.short	0x11d
	.byte	0x4
	.long	0x4df
	.quad	LFB8
	.set L$set$173,LFE8-LFB8
	.quad L$set$173
	.uleb128 0x1
	.byte	0x9c
	.long	0x1353
	.uleb128 0x1f
	.set L$set$174,LASF2-Lsection__debug_str
	.long L$set$174
	.byte	0x3
	.byte	0x36
	.byte	0x20
	.long	0x113b
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.byte	0
	.uleb128 0x21
	.ascii "anubis_types__storage__get_x25519_secret\0"
	.byte	0x1
	.short	0x118
	.byte	0x4
	.long	0x36e
	.quad	LFB7
	.set L$set$175,LFE7-LFB7
	.quad L$set$175
	.uleb128 0x1
	.byte	0x9c
	.long	0x13ab
	.uleb128 0x1f
	.set L$set$176,LASF2-Lsection__debug_str
	.long L$set$176
	.byte	0x3
	.byte	0x35
	.byte	0x20
	.long	0x113b
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.byte	0
	.uleb128 0x21
	.ascii "anubis_types__storage__get_x25519_public\0"
	.byte	0x1
	.short	0x113
	.byte	0x4
	.long	0x301
	.quad	LFB6
	.set L$set$177,LFE6-LFB6
	.quad L$set$177
	.uleb128 0x1
	.byte	0x9c
	.long	0x1403
	.uleb128 0x1f
	.set L$set$178,LASF2-Lsection__debug_str
	.long L$set$178
	.byte	0x3
	.byte	0x34
	.byte	0x20
	.long	0x113b
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.byte	0
	.uleb128 0x22
	.ascii "anubis_types__storage__load_identity\0"
	.byte	0x1
	.byte	0xa6
	.byte	0x4
	.long	0x3b2
	.quad	LFB5
	.set L$set$179,LFE5-LFB5
	.quad L$set$179
	.uleb128 0x1
	.byte	0x9c
	.long	0x171c
	.uleb128 0x1f
	.set L$set$180,LASF3-Lsection__debug_str
	.long L$set$180
	.byte	0x3
	.byte	0x2e
	.byte	0x7
	.long	0x911
	.uleb128 0x3
	.byte	0x91
	.sleb128 -160
	.uleb128 0x1f
	.set L$set$181,LASF2-Lsection__debug_str
	.long L$set$181
	.byte	0x3
	.byte	0x2f
	.byte	0x7
	.long	0x113b
	.uleb128 0x3
	.byte	0x91
	.sleb128 -168
	.uleb128 0x1f
	.set L$set$182,LASF4-Lsection__debug_str
	.long L$set$182
	.byte	0x3
	.byte	0x30
	.byte	0x7
	.long	0x3b2
	.uleb128 0x3
	.byte	0x91
	.sleb128 -131
	.uleb128 0x23
	.set L$set$183,Ldebug_ranges0+0x60-Lsection__debug_ranges
	.long L$set$183
	.uleb128 0x24
	.set L$set$184,LASF5-Lsection__debug_str
	.long L$set$184
	.byte	0x1
	.byte	0xab
	.byte	0x7
	.long	0xfb2
	.uleb128 0x3
	.byte	0x91
	.sleb128 -88
	.uleb128 0x24
	.set L$set$185,LASF6-Lsection__debug_str
	.long L$set$185
	.byte	0x1
	.byte	0xac
	.byte	0x7
	.long	0xfe3
	.uleb128 0x3
	.byte	0x91
	.sleb128 -80
	.uleb128 0x7
	.ascii "anubis_types__storage__load_identity__Tmagic_bytesS\0"
	.long	0x26f
	.long	0x14df
	.uleb128 0x8
	.long	0x2c0
	.sleb128 8
	.byte	0
	.uleb128 0x2
	.ascii "magic_bytes\0"
	.byte	0x1
	.byte	0xad
	.byte	0x7
	.long	0x149b
	.uleb128 0x2
	.byte	0x91
	.sleb128 -48
	.uleb128 0x7
	.ascii "anubis_types__storage__load_identity__Tversion_bytesS\0"
	.long	0x26f
	.long	0x153c
	.uleb128 0x8
	.long	0x2c0
	.sleb128 2
	.byte	0
	.uleb128 0x2
	.ascii "version_bytes\0"
	.byte	0x1
	.byte	0xae
	.byte	0x7
	.long	0x14f6
	.uleb128 0x2
	.byte	0x91
	.sleb128 -56
	.uleb128 0x2
	.ascii "key_type\0"
	.byte	0x1
	.byte	0xaf
	.byte	0x7
	.long	0x26f
	.uleb128 0x3
	.byte	0x91
	.sleb128 -129
	.uleb128 0x2
	.ascii "stream_byte\0"
	.byte	0x1
	.byte	0xb0
	.byte	0x7
	.long	0x1068
	.uleb128 0x3
	.byte	0x91
	.sleb128 -130
	.uleb128 0x25
	.set L$set$186,Ldebug_ranges0+0x90-Lsection__debug_ranges
	.long L$set$186
	.long	0x15b4
	.uleb128 0x26
	.ascii "EXPTR\0"
	.long	0x171c
	.uleb128 0x3
	.byte	0x91
	.sleb128 -72
	.uleb128 0x26
	.ascii "EXCLN\0"
	.long	0x171c
	.uleb128 0x2
	.byte	0x91
	.sleb128 -64
	.uleb128 0x27
	.ascii "EXPRP\0"
	.long	0x171c
	.byte	0
	.uleb128 0x28
	.quad	LBB21
	.set L$set$187,LBE21-LBB21
	.quad L$set$187
	.long	0x15d8
	.uleb128 0x2
	.ascii "i\0"
	.byte	0x1
	.byte	0xbd
	.byte	0xb
	.long	0x2c0
	.uleb128 0x3
	.byte	0x91
	.sleb128 -128
	.byte	0
	.uleb128 0x28
	.quad	LBB22
	.set L$set$188,LBE22-LBB22
	.quad L$set$188
	.long	0x15fc
	.uleb128 0x2
	.ascii "i\0"
	.byte	0x1
	.byte	0xc9
	.byte	0xb
	.long	0x2c0
	.uleb128 0x3
	.byte	0x91
	.sleb128 -124
	.byte	0
	.uleb128 0x28
	.quad	LBB23
	.set L$set$189,LBE23-LBB23
	.quad L$set$189
	.long	0x1620
	.uleb128 0x2
	.ascii "i\0"
	.byte	0x1
	.byte	0xdf
	.byte	0xb
	.long	0x2c0
	.uleb128 0x3
	.byte	0x91
	.sleb128 -120
	.byte	0
	.uleb128 0x28
	.quad	LBB24
	.set L$set$190,LBE24-LBB24
	.quad L$set$190
	.long	0x1644
	.uleb128 0x2
	.ascii "i\0"
	.byte	0x1
	.byte	0xe3
	.byte	0xb
	.long	0x2c0
	.uleb128 0x3
	.byte	0x91
	.sleb128 -116
	.byte	0
	.uleb128 0x28
	.quad	LBB25
	.set L$set$191,LBE25-LBB25
	.quad L$set$191
	.long	0x1668
	.uleb128 0x2
	.ascii "i\0"
	.byte	0x1
	.byte	0xea
	.byte	0xb
	.long	0x2c0
	.uleb128 0x3
	.byte	0x91
	.sleb128 -112
	.byte	0
	.uleb128 0x28
	.quad	LBB26
	.set L$set$192,LBE26-LBB26
	.quad L$set$192
	.long	0x168c
	.uleb128 0x2
	.ascii "i\0"
	.byte	0x1
	.byte	0xee
	.byte	0xb
	.long	0x2c0
	.uleb128 0x3
	.byte	0x91
	.sleb128 -108
	.byte	0
	.uleb128 0x28
	.quad	LBB27
	.set L$set$193,LBE27-LBB27
	.quad L$set$193
	.long	0x16b0
	.uleb128 0x2
	.ascii "i\0"
	.byte	0x1
	.byte	0xf5
	.byte	0xb
	.long	0x2c0
	.uleb128 0x3
	.byte	0x91
	.sleb128 -104
	.byte	0
	.uleb128 0x28
	.quad	LBB28
	.set L$set$194,LBE28-LBB28
	.quad L$set$194
	.long	0x16d4
	.uleb128 0x2
	.ascii "i\0"
	.byte	0x1
	.byte	0xf9
	.byte	0xb
	.long	0x2c0
	.uleb128 0x3
	.byte	0x91
	.sleb128 -100
	.byte	0
	.uleb128 0x28
	.quad	LBB29
	.set L$set$195,LBE29-LBB29
	.quad L$set$195
	.long	0x16f9
	.uleb128 0x29
	.ascii "i\0"
	.byte	0x1
	.short	0x100
	.byte	0xb
	.long	0x2c0
	.uleb128 0x3
	.byte	0x91
	.sleb128 -96
	.byte	0
	.uleb128 0x2a
	.quad	LBB30
	.set L$set$196,LBE30-LBB30
	.quad L$set$196
	.uleb128 0x29
	.ascii "i\0"
	.byte	0x1
	.short	0x104
	.byte	0xb
	.long	0x2c0
	.uleb128 0x3
	.byte	0x91
	.sleb128 -92
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0x2b
	.byte	0x8
	.uleb128 0x22
	.ascii "anubis_types__storage__save_identity\0"
	.byte	0x1
	.byte	0x5a
	.byte	0x4
	.long	0x3b2
	.quad	LFB4
	.set L$set$197,LFE4-LFB4
	.quad L$set$197
	.uleb128 0x1
	.byte	0x9c
	.long	0x1949
	.uleb128 0x1f
	.set L$set$198,LASF2-Lsection__debug_str
	.long L$set$198
	.byte	0x3
	.byte	0x27
	.byte	0x7
	.long	0x113b
	.uleb128 0x3
	.byte	0x91
	.sleb128 -120
	.uleb128 0x1f
	.set L$set$199,LASF3-Lsection__debug_str
	.long L$set$199
	.byte	0x3
	.byte	0x28
	.byte	0x7
	.long	0x90c
	.uleb128 0x3
	.byte	0x91
	.sleb128 -136
	.uleb128 0x1f
	.set L$set$200,LASF4-Lsection__debug_str
	.long L$set$200
	.byte	0x3
	.byte	0x29
	.byte	0x7
	.long	0x3b2
	.uleb128 0x3
	.byte	0x91
	.sleb128 -97
	.uleb128 0x23
	.set L$set$201,Ldebug_ranges0+0-Lsection__debug_ranges
	.long L$set$201
	.uleb128 0x24
	.set L$set$202,LASF5-Lsection__debug_str
	.long L$set$202
	.byte	0x1
	.byte	0x5f
	.byte	0x7
	.long	0xfb2
	.uleb128 0x2
	.byte	0x91
	.sleb128 -56
	.uleb128 0x24
	.set L$set$203,LASF6-Lsection__debug_str
	.long L$set$203
	.byte	0x1
	.byte	0x60
	.byte	0x7
	.long	0xfe3
	.uleb128 0x2
	.byte	0x91
	.sleb128 -48
	.uleb128 0x25
	.set L$set$204,Ldebug_ranges0+0x30-Lsection__debug_ranges
	.long L$set$204
	.long	0x17e5
	.uleb128 0x26
	.ascii "EXPTR\0"
	.long	0x171c
	.uleb128 0x2
	.byte	0x91
	.sleb128 -40
	.uleb128 0x26
	.ascii "EXCLN\0"
	.long	0x171c
	.uleb128 0x2
	.byte	0x91
	.sleb128 -32
	.uleb128 0x27
	.ascii "EXPRP\0"
	.long	0x171c
	.byte	0
	.uleb128 0x28
	.quad	LBB6
	.set L$set$205,LBE6-LBB6
	.quad L$set$205
	.long	0x1809
	.uleb128 0x2
	.ascii "i\0"
	.byte	0x1
	.byte	0x72
	.byte	0xb
	.long	0x2c0
	.uleb128 0x3
	.byte	0x91
	.sleb128 -96
	.byte	0
	.uleb128 0x28
	.quad	LBB7
	.set L$set$206,LBE7-LBB7
	.quad L$set$206
	.long	0x182d
	.uleb128 0x2
	.ascii "i\0"
	.byte	0x1
	.byte	0x77
	.byte	0xb
	.long	0x2c0
	.uleb128 0x3
	.byte	0x91
	.sleb128 -92
	.byte	0
	.uleb128 0x28
	.quad	LBB8
	.set L$set$207,LBE8-LBB8
	.quad L$set$207
	.long	0x1851
	.uleb128 0x2
	.ascii "i\0"
	.byte	0x1
	.byte	0x7f
	.byte	0xb
	.long	0x2c0
	.uleb128 0x3
	.byte	0x91
	.sleb128 -88
	.byte	0
	.uleb128 0x28
	.quad	LBB9
	.set L$set$208,LBE9-LBB9
	.quad L$set$208
	.long	0x1875
	.uleb128 0x2
	.ascii "i\0"
	.byte	0x1
	.byte	0x82
	.byte	0xb
	.long	0x2c0
	.uleb128 0x3
	.byte	0x91
	.sleb128 -84
	.byte	0
	.uleb128 0x28
	.quad	LBB10
	.set L$set$209,LBE10-LBB10
	.quad L$set$209
	.long	0x1899
	.uleb128 0x2
	.ascii "i\0"
	.byte	0x1
	.byte	0x87
	.byte	0xb
	.long	0x2c0
	.uleb128 0x3
	.byte	0x91
	.sleb128 -80
	.byte	0
	.uleb128 0x28
	.quad	LBB11
	.set L$set$210,LBE11-LBB11
	.quad L$set$210
	.long	0x18bd
	.uleb128 0x2
	.ascii "i\0"
	.byte	0x1
	.byte	0x8a
	.byte	0xb
	.long	0x2c0
	.uleb128 0x3
	.byte	0x91
	.sleb128 -76
	.byte	0
	.uleb128 0x28
	.quad	LBB12
	.set L$set$211,LBE12-LBB12
	.quad L$set$211
	.long	0x18e1
	.uleb128 0x2
	.ascii "i\0"
	.byte	0x1
	.byte	0x8f
	.byte	0xb
	.long	0x2c0
	.uleb128 0x3
	.byte	0x91
	.sleb128 -72
	.byte	0
	.uleb128 0x28
	.quad	LBB13
	.set L$set$212,LBE13-LBB13
	.quad L$set$212
	.long	0x1905
	.uleb128 0x2
	.ascii "i\0"
	.byte	0x1
	.byte	0x92
	.byte	0xb
	.long	0x2c0
	.uleb128 0x3
	.byte	0x91
	.sleb128 -68
	.byte	0
	.uleb128 0x28
	.quad	LBB14
	.set L$set$213,LBE14-LBB14
	.quad L$set$213
	.long	0x1928
	.uleb128 0x2
	.ascii "i\0"
	.byte	0x1
	.byte	0x97
	.byte	0xb
	.long	0x2c0
	.uleb128 0x2
	.byte	0x91
	.sleb128 -64
	.byte	0
	.uleb128 0x2a
	.quad	LBB15
	.set L$set$214,LBE15-LBB15
	.quad L$set$214
	.uleb128 0x2
	.ascii "i\0"
	.byte	0x1
	.byte	0x9a
	.byte	0xb
	.long	0x2c0
	.uleb128 0x2
	.byte	0x91
	.sleb128 -60
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0x2c
	.ascii "anubis_types__storage__generate_identity\0"
	.byte	0x1
	.byte	0x16
	.byte	0x4
	.long	0x3b2
	.quad	LFB3
	.set L$set$215,LFE3-LFB3
	.quad L$set$215
	.uleb128 0x1
	.byte	0x9c
	.uleb128 0x1f
	.set L$set$216,LASF2-Lsection__debug_str
	.long L$set$216
	.byte	0x3
	.byte	0x21
	.byte	0x7
	.long	0x113b
	.uleb128 0x2
	.byte	0x91
	.sleb128 -40
	.uleb128 0x1f
	.set L$set$217,LASF4-Lsection__debug_str
	.long L$set$217
	.byte	0x3
	.byte	0x22
	.byte	0x7
	.long	0x3b2
	.uleb128 0x2
	.byte	0x91
	.sleb128 -26
	.uleb128 0x2a
	.quad	LBB2
	.set L$set$218,LBE2-LBB2
	.quad L$set$218
	.uleb128 0x2
	.ascii "op_success\0"
	.byte	0x1
	.byte	0x1a
	.byte	0x7
	.long	0x3b2
	.uleb128 0x2
	.byte	0x91
	.sleb128 -25
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
	.uleb128 0x26
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
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
	.uleb128 0x34
	.uleb128 0x19
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
	.uleb128 0x8
	.uleb128 0x21
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2f
	.uleb128 0xd
	.byte	0
	.byte	0
	.uleb128 0x9
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
	.uleb128 0xc
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
	.uleb128 0xd
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
	.uleb128 0xe
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
	.uleb128 0xf
	.uleb128 0x26
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x34
	.uleb128 0x19
	.byte	0
	.byte	0
	.uleb128 0x10
	.uleb128 0x16
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
	.byte	0
	.byte	0
	.uleb128 0x11
	.uleb128 0xf
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x12
	.uleb128 0x15
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0x13
	.uleb128 0x21
	.byte	0
	.uleb128 0x22
	.uleb128 0xb
	.uleb128 0x2f
	.uleb128 0x7
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x14
	.uleb128 0x16
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
	.byte	0
	.byte	0
	.uleb128 0x15
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
	.uleb128 0x16
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
	.uleb128 0x17
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
	.uleb128 0x18
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
	.uleb128 0x19
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
	.uleb128 0x1a
	.uleb128 0x28
	.byte	0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x1c
	.uleb128 0xb
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
	.uleb128 0x5
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x38
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0x1c
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
	.uleb128 0x1d
	.uleb128 0xf
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
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
	.uleb128 0x20
	.uleb128 0x10
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x21
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
	.uleb128 0x22
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
	.uleb128 0x23
	.uleb128 0xb
	.byte	0x1
	.uleb128 0x55
	.uleb128 0x17
	.byte	0
	.byte	0
	.uleb128 0x24
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
	.uleb128 0x25
	.uleb128 0xb
	.byte	0x1
	.uleb128 0x55
	.uleb128 0x17
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x26
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
	.uleb128 0x27
	.uleb128 0x34
	.byte	0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x34
	.uleb128 0x19
	.byte	0
	.byte	0
	.uleb128 0x28
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
	.uleb128 0x29
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
	.uleb128 0x2a
	.uleb128 0xb
	.byte	0x1
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x7
	.byte	0
	.byte	0
	.uleb128 0x2b
	.uleb128 0xf
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0x2c
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
	.long	0x294
	.short	0x2
	.set L$set$219,Ldebug_info0-Lsection__debug_info
	.long L$set$219
	.long	0x19d4
	.long	0x234
	.ascii "anubis_types__storage__key_type_identity\0"
	.long	0x1088
	.ascii "anubis_types__storage__magic\0"
	.long	0x10b7
	.ascii "anubis_types__storage__version\0"
	.long	0x10e8
	.ascii "anubis_types__storage__zeroize_identity\0"
	.long	0x1141
	.ascii "anubis_types__storage__get_ml_dsa_secret\0"
	.long	0x1199
	.ascii "anubis_types__storage__get_ml_dsa_public\0"
	.long	0x11f1
	.ascii "anubis_types__storage__get_ed25519_secret\0"
	.long	0x124a
	.ascii "anubis_types__storage__get_ed25519_public\0"
	.long	0x12a3
	.ascii "anubis_types__storage__get_ml_kem_secret\0"
	.long	0x12fb
	.ascii "anubis_types__storage__get_ml_kem_public\0"
	.long	0x1353
	.ascii "anubis_types__storage__get_x25519_secret\0"
	.long	0x13ab
	.ascii "anubis_types__storage__get_x25519_public\0"
	.long	0x1403
	.ascii "anubis_types__storage__load_identity\0"
	.long	0x171e
	.ascii "anubis_types__storage__save_identity\0"
	.long	0x1949
	.ascii "anubis_types__storage__generate_identity\0"
	.long	0
	.section __DWARF,__debug_pubtypes,regular,debug
Lsection__debug_pubtypes:
	.long	0x663
	.short	0x2
	.set L$set$220,Ldebug_info0-Lsection__debug_info
	.long L$set$220
	.long	0x19d4
	.long	0x28e
	.ascii "anubis_types__TbyteB\0"
	.long	0x2a6
	.ascii "interfaces__unsigned_8\0"
	.long	0x2c0
	.ascii "integer\0"
	.long	0x2cb
	.ascii "anubis_types__x25519_public_key__T15s\0"
	.long	0x301
	.ascii "anubis_types__x25519_public_key\0"
	.long	0x338
	.ascii "anubis_types__x25519_secret_key__T17s\0"
	.long	0x3b2
	.ascii "boolean\0"
	.long	0x36e
	.ascii "anubis_types__x25519_secret_key\0"
	.long	0x3bd
	.ascii "anubis_types__ed25519_public_key__T21s\0"
	.long	0x3f4
	.ascii "anubis_types__ed25519_public_key\0"
	.long	0x42c
	.ascii "anubis_types__ed25519_secret_key__T23s\0"
	.long	0x463
	.ascii "anubis_types__ed25519_secret_key\0"
	.long	0x4a8
	.ascii "anubis_types__ml_kem_public_key__T33s\0"
	.long	0x4df
	.ascii "anubis_types__ml_kem_public_key\0"
	.long	0x517
	.ascii "anubis_types__ml_kem_secret_key__T35s\0"
	.long	0x54e
	.ascii "anubis_types__ml_kem_secret_key\0"
	.long	0x594
	.ascii "anubis_types__ml_dsa_public_key__T41s\0"
	.long	0x5cb
	.ascii "anubis_types__ml_dsa_public_key\0"
	.long	0x603
	.ascii "anubis_types__ml_dsa_secret_key__T43s\0"
	.long	0x63a
	.ascii "anubis_types__ml_dsa_secret_key\0"
	.long	0x680
	.ascii "anubis_types__storage__identity_keypair\0"
	.long	0x75e
	.ascii "anubis_types__storage__TmagicS\0"
	.long	0x792
	.ascii "anubis_types__storage__TversionS\0"
	.long	0x810
	.ascii "ada__tags__prim_ptr\0"
	.long	0x7e6
	.ascii "ada__tags__dispatch_table\0"
	.long	0x7c8
	.ascii "ada__tags__tag\0"
	.long	0x834
	.ascii "ada__streams__root_stream_type\0"
	.long	0x86b
	.ascii "system__address\0"
	.long	0x942
	.ascii "character\0"
	.long	0x916
	.ascii "string___XUA\0"
	.long	0x94f
	.ascii "string___XUB\0"
	.long	0x8d5
	.ascii "string\0"
	.long	0x8a9
	.ascii "system__file_control_block__pstring\0"
	.long	0x996
	.ascii "system__crtl__filename_encoding\0"
	.long	0xa0d
	.ascii "system__file_control_block__file_mode\0"
	.long	0xadd
	.ascii "interfaces__c_streams__content_encoding\0"
	.long	0xbd1
	.ascii "system__file_control_block__shared_status_type\0"
	.long	0xca3
	.ascii "system__file_control_block__afcb\0"
	.long	0xc70
	.ascii "system__file_control_block__afcb_ptr\0"
	.long	0xe08
	.ascii "ada__streams__stream_io__TcountB\0"
	.long	0xe69
	.ascii "ada__streams__Tstream_element_offsetB\0"
	.long	0xe92
	.ascii "ada__streams__stream_io__operation\0"
	.long	0xf2a
	.ascii "ada__streams__stream_io__stream_afcb\0"
	.long	0xfb2
	.ascii "ada__streams__stream_io__file_type\0"
	.long	0xfe3
	.ascii "ada__streams__stream_io__stream_access\0"
	.long	0x1038
	.ascii "system__unsigned_types__short_short_unsigned\0"
	.long	0x1068
	.ascii "ada__streams__stream_element\0"
	.long	0
	.section __DWARF,__debug_aranges,regular,debug
Lsection__debug_aranges:
	.long	0x2c
	.short	0x2
	.set L$set$221,Ldebug_info0-Lsection__debug_info
	.long L$set$221
	.byte	0x8
	.byte	0
	.short	0
	.short	0
	.quad	Ltext0
	.set L$set$222,Letext0-Ltext0
	.quad L$set$222
	.quad	0
	.quad	0
	.section __DWARF,__debug_ranges,regular,debug
Lsection__debug_ranges:
Ldebug_ranges0:
	.set L$set$223,LBB3-Ltext0
	.quad L$set$223
	.set L$set$224,LBE3-Ltext0
	.quad L$set$224
	.set L$set$225,LBB17-Ltext0
	.quad L$set$225
	.set L$set$226,LBE17-Ltext0
	.quad L$set$226
	.quad	0
	.quad	0
	.set L$set$227,LBB4-Ltext0
	.quad L$set$227
	.set L$set$228,LBE4-Ltext0
	.quad L$set$228
	.set L$set$229,LBB16-Ltext0
	.quad L$set$229
	.set L$set$230,LBE16-Ltext0
	.quad L$set$230
	.quad	0
	.quad	0
	.set L$set$231,LBB18-Ltext0
	.quad L$set$231
	.set L$set$232,LBE18-Ltext0
	.quad L$set$232
	.set L$set$233,LBB32-Ltext0
	.quad L$set$233
	.set L$set$234,LBE32-Ltext0
	.quad L$set$234
	.quad	0
	.quad	0
	.set L$set$235,LBB19-Ltext0
	.quad L$set$235
	.set L$set$236,LBE19-Ltext0
	.quad L$set$236
	.set L$set$237,LBB31-Ltext0
	.quad L$set$237
	.set L$set$238,LBE31-Ltext0
	.quad L$set$238
	.quad	0
	.quad	0
	.section __DWARF,__debug_line,regular,debug
Lsection__debug_line:
Ldebug_line0:
	.section __DWARF,__debug_str,regular,debug
Lsection__debug_str:
LASF2:
	.ascii "identity\0"
LASF6:
	.ascii "file_stream\0"
LASF1:
	.ascii "valid\0"
LASF5:
	.ascii "file_handle\0"
LASF4:
	.ascii "success\0"
LASF0:
	.ascii "data\0"
LASF3:
	.ascii "filename\0"
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
