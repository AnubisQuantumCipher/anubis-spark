	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
Ltext0:
	.file 1 "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_types.adb"
	.align	2
	.globl _anubis_types__byte_arrayIP
_anubis_types__byte_arrayIP:
LFB2:
	sub	sp, sp, #16
LCFI0:
	stp	x0, x1, [sp]
	add	sp, sp, 16
LCFI1:
	ret
LFE2:
	.align	2
	.globl _anubis_types__x25519_public_keyIP
_anubis_types__x25519_public_keyIP:
LFB3:
	sub	sp, sp, #16
LCFI2:
	str	x0, [sp, 8]
	add	sp, sp, 16
LCFI3:
	ret
LFE3:
	.align	2
	.globl _anubis_types__x25519_secret_keyIP
_anubis_types__x25519_secret_keyIP:
LFB4:
	sub	sp, sp, #16
LCFI4:
	str	x0, [sp, 8]
	ldr	x0, [sp, 8]
	strb	wzr, [x0, 32]
	add	sp, sp, 16
LCFI5:
	ret
LFE4:
	.align	2
	.globl _anubis_types__x25519_shared_secretIP
_anubis_types__x25519_shared_secretIP:
LFB5:
	sub	sp, sp, #16
LCFI6:
	str	x0, [sp, 8]
	ldr	x0, [sp, 8]
	strb	wzr, [x0, 32]
	add	sp, sp, 16
LCFI7:
	ret
LFE5:
	.align	2
	.globl _anubis_types__ed25519_public_keyIP
_anubis_types__ed25519_public_keyIP:
LFB6:
	sub	sp, sp, #16
LCFI8:
	str	x0, [sp, 8]
	add	sp, sp, 16
LCFI9:
	ret
LFE6:
	.align	2
	.globl _anubis_types__ed25519_secret_keyIP
_anubis_types__ed25519_secret_keyIP:
LFB7:
	sub	sp, sp, #16
LCFI10:
	str	x0, [sp, 8]
	ldr	x0, [sp, 8]
	strb	wzr, [x0, 32]
	add	sp, sp, 16
LCFI11:
	ret
LFE7:
	.align	2
	.globl _anubis_types__ed25519_signatureIP
_anubis_types__ed25519_signatureIP:
LFB8:
	sub	sp, sp, #16
LCFI12:
	str	x0, [sp, 8]
	add	sp, sp, 16
LCFI13:
	ret
LFE8:
	.align	2
	.globl _anubis_types__xchacha20_keyIP
_anubis_types__xchacha20_keyIP:
LFB9:
	sub	sp, sp, #16
LCFI14:
	str	x0, [sp, 8]
	ldr	x0, [sp, 8]
	strb	wzr, [x0, 32]
	add	sp, sp, 16
LCFI15:
	ret
LFE9:
	.align	2
	.globl _anubis_types__xchacha20_nonceIP
_anubis_types__xchacha20_nonceIP:
LFB10:
	sub	sp, sp, #16
LCFI16:
	str	x0, [sp, 8]
	add	sp, sp, 16
LCFI17:
	ret
LFE10:
	.align	2
	.globl _anubis_types__poly1305_tagIP
_anubis_types__poly1305_tagIP:
LFB11:
	sub	sp, sp, #16
LCFI18:
	str	x0, [sp, 8]
	add	sp, sp, 16
LCFI19:
	ret
LFE11:
	.align	2
	.globl _anubis_types__argon2_derived_keyIP
_anubis_types__argon2_derived_keyIP:
LFB12:
	sub	sp, sp, #16
LCFI20:
	str	x0, [sp, 8]
	ldr	x0, [sp, 8]
	strb	wzr, [x0, 32]
	add	sp, sp, 16
LCFI21:
	ret
LFE12:
	.align	2
	.globl _anubis_types__ml_kem_public_keyIP
_anubis_types__ml_kem_public_keyIP:
LFB13:
	sub	sp, sp, #16
LCFI22:
	str	x0, [sp, 8]
	add	sp, sp, 16
LCFI23:
	ret
LFE13:
	.align	2
	.globl _anubis_types__ml_kem_secret_keyIP
_anubis_types__ml_kem_secret_keyIP:
LFB14:
	sub	sp, sp, #16
LCFI24:
	str	x0, [sp, 8]
	ldr	x0, [sp, 8]
	strb	wzr, [x0, 3168]
	add	sp, sp, 16
LCFI25:
	ret
LFE14:
	.align	2
	.globl _anubis_types__ml_kem_ciphertextIP
_anubis_types__ml_kem_ciphertextIP:
LFB15:
	sub	sp, sp, #16
LCFI26:
	str	x0, [sp, 8]
	add	sp, sp, 16
LCFI27:
	ret
LFE15:
	.align	2
	.globl _anubis_types__ml_kem_shared_secretIP
_anubis_types__ml_kem_shared_secretIP:
LFB16:
	sub	sp, sp, #16
LCFI28:
	str	x0, [sp, 8]
	ldr	x0, [sp, 8]
	strb	wzr, [x0, 32]
	add	sp, sp, 16
LCFI29:
	ret
LFE16:
	.align	2
	.globl _anubis_types__ml_dsa_public_keyIP
_anubis_types__ml_dsa_public_keyIP:
LFB17:
	sub	sp, sp, #16
LCFI30:
	str	x0, [sp, 8]
	add	sp, sp, 16
LCFI31:
	ret
LFE17:
	.align	2
	.globl _anubis_types__ml_dsa_secret_keyIP
_anubis_types__ml_dsa_secret_keyIP:
LFB18:
	sub	sp, sp, #16
LCFI32:
	str	x0, [sp, 8]
	ldr	x0, [sp, 8]
	add	x0, x0, 4096
	strb	wzr, [x0, 800]
	add	sp, sp, 16
LCFI33:
	ret
LFE18:
	.align	2
	.globl _anubis_types__ml_dsa_signatureIP
_anubis_types__ml_dsa_signatureIP:
LFB19:
	sub	sp, sp, #16
LCFI34:
	str	x0, [sp, 8]
	add	sp, sp, 16
LCFI35:
	ret
LFE19:
	.align	2
	.globl _anubis_types__master_keyIP
_anubis_types__master_keyIP:
LFB20:
	sub	sp, sp, #16
LCFI36:
	str	x0, [sp, 8]
	ldr	x0, [sp, 8]
	strb	wzr, [x0, 64]
	add	sp, sp, 16
LCFI37:
	ret
LFE20:
	.align	2
	.globl _anubis_types__argon2_saltIP
_anubis_types__argon2_saltIP:
LFB21:
	sub	sp, sp, #16
LCFI38:
	str	x0, [sp, 8]
	add	sp, sp, 16
LCFI39:
	ret
LFE21:
	.align	2
	.globl _anubis_types__key_statusH
_anubis_types__key_statusH:
LFB22:
	sub	sp, sp, #16
LCFI40:
	stp	x0, x1, [sp]
	ldr	x0, [sp, 8]
	ldr	w0, [x0]
	ldr	x1, [sp, 8]
	ldr	w1, [x1, 4]
	cmp	w1, w0
	blt	L42
	sub	w6, w1, w0
	add	w6, w6, 1
	b	L43
L42:
	mov	w6, 0
L43:
	sxtw	x7, w0
	cmp	w1, w0
	cmp	w1, w0
	blt	L47
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
L47:
	cmp	w1, w0
	sub	w8, w0, #1
	mov	w4, 0
	mov	w3, 0
	mov	w1, 0
L51:
	cmp	w1, 0
	bgt	L50
	sxtw	x2, w1
	adrp	x0, _key_statusP.3@PAGE
	add	x0, x0, _key_statusP.3@PAGEOFF;
	ldr	w0, [x0, x2, lsl 2]
	cmp	w6, w0
	blt	L50
	ldr	x2, [sp]
	sxtw	x5, w1
	adrp	x0, _key_statusP.3@PAGE
	add	x0, x0, _key_statusP.3@PAGEOFF;
	ldr	w0, [x0, x5, lsl 2]
	add	w0, w8, w0
	sxtw	x0, w0
	sub	x0, x0, x7
	ldrsb	w0, [x2, x0]
	and	w0, w0, 255
	mov	w5, w0
	sxtw	x0, w1
	adrp	x2, _key_statusT1.2@PAGE
	add	x2, x2, _key_statusT1.2@PAGEOFF;
	ldrb	w0, [x2, x0]
	mul	w0, w5, w0
	add	w4, w4, w0
	mov	w0, 11
	sdiv	w2, w4, w0
	mov	w0, w2
	lsl	w0, w0, 2
	add	w0, w0, w2
	lsl	w0, w0, 1
	add	w0, w0, w2
	sub	w4, w4, w0
	sxtw	x0, w1
	adrp	x2, _key_statusT2.1@PAGE
	add	x2, x2, _key_statusT2.1@PAGEOFF;
	ldrb	w0, [x2, x0]
	mul	w0, w5, w0
	add	w3, w3, w0
	mov	w0, 11
	sdiv	w2, w3, w0
	mov	w0, w2
	lsl	w0, w0, 2
	add	w0, w0, w2
	lsl	w0, w0, 1
	add	w0, w0, w2
	sub	w3, w3, w0
	add	w1, w1, 1
	b	L51
L50:
	sxtw	x0, w4
	adrp	x1, _key_statusG.0@PAGE
	add	x1, x1, _key_statusG.0@PAGEOFF;
	ldrb	w0, [x1, x0]
	mov	w2, w0
	sxtw	x0, w3
	adrp	x1, _key_statusG.0@PAGE
	add	x1, x1, _key_statusG.0@PAGEOFF;
	ldrb	w0, [x1, x0]
	add	w1, w2, w0
	mov	w0, 5
	sdiv	w2, w1, w0
	mov	w0, w2
	lsl	w0, w0, 2
	add	w0, w0, w2
	sub	w0, w1, w0
	add	sp, sp, 16
LCFI41:
	ret
LFE22:
	.align	2
	.globl _anubis_types__is_valid
_anubis_types__is_valid:
LFB23:
	.loc 1 14 4
	sub	sp, sp, #16
LCFI42:
	str	x0, [sp, 8]
	.loc 1 14 70
	ldr	x0, [sp, 8]
	ldrb	w0, [x0, 32]
	.loc 1 14 4
	add	sp, sp, 16
LCFI43:
	ret
LFE23:
	.align	2
	.globl _anubis_types__is_valid__2
_anubis_types__is_valid__2:
LFB24:
	.loc 1 15 4
	sub	sp, sp, #16
LCFI44:
	str	x0, [sp, 8]
	.loc 1 15 79
	ldr	x0, [sp, 8]
	ldrb	w0, [x0, 32]
	.loc 1 15 4
	add	sp, sp, 16
LCFI45:
	ret
LFE24:
	.align	2
	.globl _anubis_types__is_valid__3
_anubis_types__is_valid__3:
LFB25:
	.loc 1 16 4
	sub	sp, sp, #16
LCFI46:
	str	x0, [sp, 8]
	.loc 1 16 71
	ldr	x0, [sp, 8]
	ldrb	w0, [x0, 32]
	.loc 1 16 4
	add	sp, sp, 16
LCFI47:
	ret
LFE25:
	.align	2
	.globl _anubis_types__is_valid__4
_anubis_types__is_valid__4:
LFB26:
	.loc 1 17 4
	sub	sp, sp, #16
LCFI48:
	str	x0, [sp, 8]
	.loc 1 17 66
	ldr	x0, [sp, 8]
	ldrb	w0, [x0, 32]
	.loc 1 17 4
	add	sp, sp, 16
LCFI49:
	ret
LFE26:
	.align	2
	.globl _anubis_types__is_valid__5
_anubis_types__is_valid__5:
LFB27:
	.loc 1 18 4
	sub	sp, sp, #16
LCFI50:
	str	x0, [sp, 8]
	.loc 1 18 71
	ldr	x0, [sp, 8]
	ldrb	w0, [x0, 32]
	.loc 1 18 4
	add	sp, sp, 16
LCFI51:
	ret
LFE27:
	.align	2
	.globl _anubis_types__is_valid__6
_anubis_types__is_valid__6:
LFB28:
	.loc 1 19 4
	sub	sp, sp, #16
LCFI52:
	str	x0, [sp, 8]
	.loc 1 19 70
	ldr	x0, [sp, 8]
	ldrb	w0, [x0, 3168]
	.loc 1 19 4
	add	sp, sp, 16
LCFI53:
	ret
LFE28:
	.align	2
	.globl _anubis_types__is_valid__7
_anubis_types__is_valid__7:
LFB29:
	.loc 1 20 4
	sub	sp, sp, #16
LCFI54:
	str	x0, [sp, 8]
	.loc 1 20 79
	ldr	x0, [sp, 8]
	ldrb	w0, [x0, 32]
	.loc 1 20 4
	add	sp, sp, 16
LCFI55:
	ret
LFE29:
	.align	2
	.globl _anubis_types__is_valid__8
_anubis_types__is_valid__8:
LFB30:
	.loc 1 21 4
	sub	sp, sp, #16
LCFI56:
	str	x0, [sp, 8]
	.loc 1 21 70
	ldr	x0, [sp, 8]
	add	x0, x0, 4096
	ldrb	w0, [x0, 800]
	.loc 1 21 4
	add	sp, sp, 16
LCFI57:
	ret
LFE30:
	.align	2
	.globl _anubis_types__is_valid__9
_anubis_types__is_valid__9:
LFB31:
	.loc 1 22 4
	sub	sp, sp, #16
LCFI58:
	str	x0, [sp, 8]
	.loc 1 22 63
	ldr	x0, [sp, 8]
	ldrb	w0, [x0, 64]
	.loc 1 22 4
	add	sp, sp, 16
LCFI59:
	ret
LFE31:
	.align	2
	.globl _anubis_types__zeroize
_anubis_types__zeroize:
LFB32:
	.loc 1 48 4
	sub	sp, sp, #32
LCFI60:
	str	x0, [sp, 8]
LBB4:
	.loc 1 50 11
	mov	w0, 1
	str	w0, [sp, 28]
L73:
	.loc 1 50 11 is_stmt 0 discriminator 3
	ldr	w0, [sp, 28]
	cmp	w0, 32
	bgt	L72
	.loc 1 53 23 is_stmt 1
	ldrsw	x0, [sp, 28]
	ldr	x1, [sp, 8]
	add	x0, x1, x0
	strb	wzr, [x0, -1]
	.loc 1 50 11 discriminator 2
	ldr	w0, [sp, 28]
	add	w0, w0, 1
	str	w0, [sp, 28]
	.loc 1 54 15
	b	L73
L72:
LBE4:
	.loc 1 55 17
	ldr	x0, [sp, 8]
	strb	wzr, [x0, 32]
	.loc 1 56 8
	nop
	add	sp, sp, 32
LCFI61:
	ret
LFE32:
	.align	2
	.globl _anubis_types__zeroize__2
_anubis_types__zeroize__2:
LFB33:
	.loc 1 58 4
	sub	sp, sp, #32
LCFI62:
	str	x0, [sp, 8]
LBB5:
	.loc 1 60 11
	mov	w0, 1
	str	w0, [sp, 28]
L77:
	.loc 1 60 11 is_stmt 0 discriminator 3
	ldr	w0, [sp, 28]
	cmp	w0, 32
	bgt	L76
	.loc 1 63 23 is_stmt 1
	ldrsw	x0, [sp, 28]
	ldr	x1, [sp, 8]
	add	x0, x1, x0
	strb	wzr, [x0, -1]
	.loc 1 60 11 discriminator 2
	ldr	w0, [sp, 28]
	add	w0, w0, 1
	str	w0, [sp, 28]
	.loc 1 64 15
	b	L77
L76:
LBE5:
	.loc 1 65 17
	ldr	x0, [sp, 8]
	strb	wzr, [x0, 32]
	.loc 1 66 8
	nop
	add	sp, sp, 32
LCFI63:
	ret
LFE33:
	.align	2
	.globl _anubis_types__zeroize__3
_anubis_types__zeroize__3:
LFB34:
	.loc 1 68 4
	sub	sp, sp, #32
LCFI64:
	str	x0, [sp, 8]
LBB6:
	.loc 1 70 11
	mov	w0, 1
	str	w0, [sp, 28]
L81:
	.loc 1 70 11 is_stmt 0 discriminator 3
	ldr	w0, [sp, 28]
	cmp	w0, 3168
	bgt	L80
	.loc 1 73 23 is_stmt 1
	ldrsw	x0, [sp, 28]
	ldr	x1, [sp, 8]
	add	x0, x1, x0
	strb	wzr, [x0, -1]
	.loc 1 70 11 discriminator 2
	ldr	w0, [sp, 28]
	add	w0, w0, 1
	str	w0, [sp, 28]
	.loc 1 74 15
	b	L81
L80:
LBE6:
	.loc 1 75 17
	ldr	x0, [sp, 8]
	strb	wzr, [x0, 3168]
	.loc 1 76 8
	nop
	add	sp, sp, 32
LCFI65:
	ret
LFE34:
	.align	2
	.globl _anubis_types__zeroize__4
_anubis_types__zeroize__4:
LFB35:
	.loc 1 78 4
	sub	sp, sp, #32
LCFI66:
	str	x0, [sp, 8]
LBB7:
	.loc 1 80 11
	mov	w0, 1
	str	w0, [sp, 28]
L85:
	.loc 1 80 11 is_stmt 0 discriminator 3
	ldr	w1, [sp, 28]
	mov	w0, 4896
	cmp	w1, w0
	bgt	L84
	.loc 1 83 23 is_stmt 1
	ldrsw	x0, [sp, 28]
	ldr	x1, [sp, 8]
	add	x0, x1, x0
	strb	wzr, [x0, -1]
	.loc 1 80 11 discriminator 2
	ldr	w0, [sp, 28]
	add	w0, w0, 1
	str	w0, [sp, 28]
	.loc 1 84 15
	b	L85
L84:
LBE7:
	.loc 1 85 17
	ldr	x0, [sp, 8]
	add	x0, x0, 4096
	strb	wzr, [x0, 800]
	.loc 1 86 8
	nop
	add	sp, sp, 32
LCFI67:
	ret
LFE35:
	.align	2
	.globl _anubis_types__zeroize__5
_anubis_types__zeroize__5:
LFB36:
	.loc 1 88 4
	sub	sp, sp, #32
LCFI68:
	str	x0, [sp, 8]
LBB8:
	.loc 1 90 11
	mov	w0, 1
	str	w0, [sp, 28]
L89:
	.loc 1 90 11 is_stmt 0 discriminator 3
	ldr	w0, [sp, 28]
	cmp	w0, 64
	bgt	L88
	.loc 1 93 23 is_stmt 1
	ldrsw	x0, [sp, 28]
	ldr	x1, [sp, 8]
	add	x0, x1, x0
	strb	wzr, [x0, -1]
	.loc 1 90 11 discriminator 2
	ldr	w0, [sp, 28]
	add	w0, w0, 1
	str	w0, [sp, 28]
	.loc 1 94 15
	b	L89
L88:
LBE8:
	.loc 1 95 17
	ldr	x0, [sp, 8]
	strb	wzr, [x0, 64]
	.loc 1 96 8
	nop
	add	sp, sp, 32
LCFI69:
	ret
LFE36:
	.align	2
	.globl _anubis_types__zeroize__6
_anubis_types__zeroize__6:
LFB37:
	.loc 1 98 4
	sub	sp, sp, #32
LCFI70:
	stp	x0, x1, [sp]
	.loc 1 98 4
	ldr	x0, [sp, 8]
	ldr	w0, [x0]
	sxtw	x1, w0
	ldr	x0, [sp, 8]
	ldr	w6, [x0, 4]
	ldr	x0, [sp, 8]
	ldr	w0, [x0]
	cmp	w6, w0
	.loc 1 98 4 is_stmt 0 discriminator 4
	ldr	x0, [sp, 8]
	ldr	w6, [x0, 4]
	ldr	x0, [sp, 8]
	ldr	w0, [x0]
	cmp	w6, w0
	blt	L95
	.loc 1 98 4 discriminator 5
	ldr	x0, [sp, 8]
	ldr	w0, [x0, 4]
	sxtw	x6, w0
	ldr	x0, [sp, 8]
	ldr	w0, [x0]
	sxtw	x0, w0
	sub	x0, x6, x0
	add	x0, x0, 1
	mov	x2, x0
	mov	x3, 0
	lsr	x0, x2, 61
	lsl	x5, x3, 3
	orr	x5, x0, x5
	lsl	x4, x2, 3
L95:
	.loc 1 98 4 discriminator 8
	ldr	x0, [sp, 8]
	ldr	w2, [x0, 4]
	ldr	x0, [sp, 8]
	ldr	w0, [x0]
	cmp	w2, w0
	.loc 1 100 11 is_stmt 1
	ldr	x0, [sp, 8]
	ldr	w0, [x0]
	str	w0, [sp, 24]
	ldr	x0, [sp, 8]
	ldr	w0, [x0, 4]
	str	w0, [sp, 28]
	ldr	w2, [sp, 24]
	ldr	w0, [sp, 28]
	cmp	w2, w0
	bgt	L101
LBB9:
	.loc 1 100 11 is_stmt 0 discriminator 1
	ldr	w0, [sp, 24]
	str	w0, [sp, 20]
L99:
	.loc 1 103 19 is_stmt 1
	ldr	x2, [sp]
	ldrsw	x0, [sp, 20]
	sub	x0, x0, x1
	strb	wzr, [x2, x0]
	.loc 1 100 11 discriminator 4
	ldr	w2, [sp, 20]
	ldr	w0, [sp, 28]
	cmp	w2, w0
	beq	L101
	.loc 1 100 11 is_stmt 0 discriminator 3
	ldr	w0, [sp, 20]
	add	w0, w0, 1
	str	w0, [sp, 20]
	.loc 1 104 15 is_stmt 1
	b	L99
L101:
LBE9:
	.loc 1 105 8
	nop
	add	sp, sp, 32
LCFI71:
	ret
LFE37:
	.globl _anubis_types_E
	.data
	.align	1
_anubis_types_E:
	.space 2
	.globl _anubis_types__key_statusS
	.const
	.align	3
_anubis_types__key_statusS:
	.ascii "UNINITIALIZEDACTIVEEXPIREDREVOKEDDESTROYED"
	.globl _anubis_types__key_statusN
	.align	3
_anubis_types__key_statusN:
	.byte	1
	.byte	14
	.byte	20
	.byte	27
	.byte	34
	.byte	43
	.space 2
	.align	2
_key_statusP.3:
	.word	1
_key_statusT1.2:
	.byte	3
_key_statusT2.1:
	.byte	9
	.align	3
_key_statusG.0:
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	3
	.byte	0
	.byte	0
	.byte	4
	.byte	1
	.byte	2
	.byte	0
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
	.uleb128 0x10
	.byte	0x4
	.set L$set$20,LCFI7-LCFI6
	.long L$set$20
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE6:
LSFDE8:
	.set L$set$21,LEFDE8-LASFDE8
	.long L$set$21
LASFDE8:
	.set L$set$22,Lframe0-Lsection__debug_frame
	.long L$set$22
	.quad	LFB6
	.set L$set$23,LFE6-LFB6
	.quad L$set$23
	.byte	0x4
	.set L$set$24,LCFI8-LFB6
	.long L$set$24
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$25,LCFI9-LCFI8
	.long L$set$25
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE8:
LSFDE10:
	.set L$set$26,LEFDE10-LASFDE10
	.long L$set$26
LASFDE10:
	.set L$set$27,Lframe0-Lsection__debug_frame
	.long L$set$27
	.quad	LFB7
	.set L$set$28,LFE7-LFB7
	.quad L$set$28
	.byte	0x4
	.set L$set$29,LCFI10-LFB7
	.long L$set$29
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$30,LCFI11-LCFI10
	.long L$set$30
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE10:
LSFDE12:
	.set L$set$31,LEFDE12-LASFDE12
	.long L$set$31
LASFDE12:
	.set L$set$32,Lframe0-Lsection__debug_frame
	.long L$set$32
	.quad	LFB8
	.set L$set$33,LFE8-LFB8
	.quad L$set$33
	.byte	0x4
	.set L$set$34,LCFI12-LFB8
	.long L$set$34
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$35,LCFI13-LCFI12
	.long L$set$35
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE12:
LSFDE14:
	.set L$set$36,LEFDE14-LASFDE14
	.long L$set$36
LASFDE14:
	.set L$set$37,Lframe0-Lsection__debug_frame
	.long L$set$37
	.quad	LFB9
	.set L$set$38,LFE9-LFB9
	.quad L$set$38
	.byte	0x4
	.set L$set$39,LCFI14-LFB9
	.long L$set$39
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$40,LCFI15-LCFI14
	.long L$set$40
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE14:
LSFDE16:
	.set L$set$41,LEFDE16-LASFDE16
	.long L$set$41
LASFDE16:
	.set L$set$42,Lframe0-Lsection__debug_frame
	.long L$set$42
	.quad	LFB10
	.set L$set$43,LFE10-LFB10
	.quad L$set$43
	.byte	0x4
	.set L$set$44,LCFI16-LFB10
	.long L$set$44
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$45,LCFI17-LCFI16
	.long L$set$45
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE16:
LSFDE18:
	.set L$set$46,LEFDE18-LASFDE18
	.long L$set$46
LASFDE18:
	.set L$set$47,Lframe0-Lsection__debug_frame
	.long L$set$47
	.quad	LFB11
	.set L$set$48,LFE11-LFB11
	.quad L$set$48
	.byte	0x4
	.set L$set$49,LCFI18-LFB11
	.long L$set$49
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$50,LCFI19-LCFI18
	.long L$set$50
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE18:
LSFDE20:
	.set L$set$51,LEFDE20-LASFDE20
	.long L$set$51
LASFDE20:
	.set L$set$52,Lframe0-Lsection__debug_frame
	.long L$set$52
	.quad	LFB12
	.set L$set$53,LFE12-LFB12
	.quad L$set$53
	.byte	0x4
	.set L$set$54,LCFI20-LFB12
	.long L$set$54
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$55,LCFI21-LCFI20
	.long L$set$55
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE20:
LSFDE22:
	.set L$set$56,LEFDE22-LASFDE22
	.long L$set$56
LASFDE22:
	.set L$set$57,Lframe0-Lsection__debug_frame
	.long L$set$57
	.quad	LFB13
	.set L$set$58,LFE13-LFB13
	.quad L$set$58
	.byte	0x4
	.set L$set$59,LCFI22-LFB13
	.long L$set$59
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$60,LCFI23-LCFI22
	.long L$set$60
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE22:
LSFDE24:
	.set L$set$61,LEFDE24-LASFDE24
	.long L$set$61
LASFDE24:
	.set L$set$62,Lframe0-Lsection__debug_frame
	.long L$set$62
	.quad	LFB14
	.set L$set$63,LFE14-LFB14
	.quad L$set$63
	.byte	0x4
	.set L$set$64,LCFI24-LFB14
	.long L$set$64
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$65,LCFI25-LCFI24
	.long L$set$65
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE24:
LSFDE26:
	.set L$set$66,LEFDE26-LASFDE26
	.long L$set$66
LASFDE26:
	.set L$set$67,Lframe0-Lsection__debug_frame
	.long L$set$67
	.quad	LFB15
	.set L$set$68,LFE15-LFB15
	.quad L$set$68
	.byte	0x4
	.set L$set$69,LCFI26-LFB15
	.long L$set$69
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$70,LCFI27-LCFI26
	.long L$set$70
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE26:
LSFDE28:
	.set L$set$71,LEFDE28-LASFDE28
	.long L$set$71
LASFDE28:
	.set L$set$72,Lframe0-Lsection__debug_frame
	.long L$set$72
	.quad	LFB16
	.set L$set$73,LFE16-LFB16
	.quad L$set$73
	.byte	0x4
	.set L$set$74,LCFI28-LFB16
	.long L$set$74
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$75,LCFI29-LCFI28
	.long L$set$75
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE28:
LSFDE30:
	.set L$set$76,LEFDE30-LASFDE30
	.long L$set$76
LASFDE30:
	.set L$set$77,Lframe0-Lsection__debug_frame
	.long L$set$77
	.quad	LFB17
	.set L$set$78,LFE17-LFB17
	.quad L$set$78
	.byte	0x4
	.set L$set$79,LCFI30-LFB17
	.long L$set$79
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$80,LCFI31-LCFI30
	.long L$set$80
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE30:
LSFDE32:
	.set L$set$81,LEFDE32-LASFDE32
	.long L$set$81
LASFDE32:
	.set L$set$82,Lframe0-Lsection__debug_frame
	.long L$set$82
	.quad	LFB18
	.set L$set$83,LFE18-LFB18
	.quad L$set$83
	.byte	0x4
	.set L$set$84,LCFI32-LFB18
	.long L$set$84
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$85,LCFI33-LCFI32
	.long L$set$85
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE32:
LSFDE34:
	.set L$set$86,LEFDE34-LASFDE34
	.long L$set$86
LASFDE34:
	.set L$set$87,Lframe0-Lsection__debug_frame
	.long L$set$87
	.quad	LFB19
	.set L$set$88,LFE19-LFB19
	.quad L$set$88
	.byte	0x4
	.set L$set$89,LCFI34-LFB19
	.long L$set$89
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$90,LCFI35-LCFI34
	.long L$set$90
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE34:
LSFDE36:
	.set L$set$91,LEFDE36-LASFDE36
	.long L$set$91
LASFDE36:
	.set L$set$92,Lframe0-Lsection__debug_frame
	.long L$set$92
	.quad	LFB20
	.set L$set$93,LFE20-LFB20
	.quad L$set$93
	.byte	0x4
	.set L$set$94,LCFI36-LFB20
	.long L$set$94
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$95,LCFI37-LCFI36
	.long L$set$95
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE36:
LSFDE38:
	.set L$set$96,LEFDE38-LASFDE38
	.long L$set$96
LASFDE38:
	.set L$set$97,Lframe0-Lsection__debug_frame
	.long L$set$97
	.quad	LFB21
	.set L$set$98,LFE21-LFB21
	.quad L$set$98
	.byte	0x4
	.set L$set$99,LCFI38-LFB21
	.long L$set$99
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$100,LCFI39-LCFI38
	.long L$set$100
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE38:
LSFDE40:
	.set L$set$101,LEFDE40-LASFDE40
	.long L$set$101
LASFDE40:
	.set L$set$102,Lframe0-Lsection__debug_frame
	.long L$set$102
	.quad	LFB22
	.set L$set$103,LFE22-LFB22
	.quad L$set$103
	.byte	0x4
	.set L$set$104,LCFI40-LFB22
	.long L$set$104
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$105,LCFI41-LCFI40
	.long L$set$105
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE40:
LSFDE42:
	.set L$set$106,LEFDE42-LASFDE42
	.long L$set$106
LASFDE42:
	.set L$set$107,Lframe0-Lsection__debug_frame
	.long L$set$107
	.quad	LFB23
	.set L$set$108,LFE23-LFB23
	.quad L$set$108
	.byte	0x4
	.set L$set$109,LCFI42-LFB23
	.long L$set$109
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$110,LCFI43-LCFI42
	.long L$set$110
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE42:
LSFDE44:
	.set L$set$111,LEFDE44-LASFDE44
	.long L$set$111
LASFDE44:
	.set L$set$112,Lframe0-Lsection__debug_frame
	.long L$set$112
	.quad	LFB24
	.set L$set$113,LFE24-LFB24
	.quad L$set$113
	.byte	0x4
	.set L$set$114,LCFI44-LFB24
	.long L$set$114
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$115,LCFI45-LCFI44
	.long L$set$115
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE44:
LSFDE46:
	.set L$set$116,LEFDE46-LASFDE46
	.long L$set$116
LASFDE46:
	.set L$set$117,Lframe0-Lsection__debug_frame
	.long L$set$117
	.quad	LFB25
	.set L$set$118,LFE25-LFB25
	.quad L$set$118
	.byte	0x4
	.set L$set$119,LCFI46-LFB25
	.long L$set$119
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$120,LCFI47-LCFI46
	.long L$set$120
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE46:
LSFDE48:
	.set L$set$121,LEFDE48-LASFDE48
	.long L$set$121
LASFDE48:
	.set L$set$122,Lframe0-Lsection__debug_frame
	.long L$set$122
	.quad	LFB26
	.set L$set$123,LFE26-LFB26
	.quad L$set$123
	.byte	0x4
	.set L$set$124,LCFI48-LFB26
	.long L$set$124
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$125,LCFI49-LCFI48
	.long L$set$125
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE48:
LSFDE50:
	.set L$set$126,LEFDE50-LASFDE50
	.long L$set$126
LASFDE50:
	.set L$set$127,Lframe0-Lsection__debug_frame
	.long L$set$127
	.quad	LFB27
	.set L$set$128,LFE27-LFB27
	.quad L$set$128
	.byte	0x4
	.set L$set$129,LCFI50-LFB27
	.long L$set$129
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$130,LCFI51-LCFI50
	.long L$set$130
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE50:
LSFDE52:
	.set L$set$131,LEFDE52-LASFDE52
	.long L$set$131
LASFDE52:
	.set L$set$132,Lframe0-Lsection__debug_frame
	.long L$set$132
	.quad	LFB28
	.set L$set$133,LFE28-LFB28
	.quad L$set$133
	.byte	0x4
	.set L$set$134,LCFI52-LFB28
	.long L$set$134
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$135,LCFI53-LCFI52
	.long L$set$135
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE52:
LSFDE54:
	.set L$set$136,LEFDE54-LASFDE54
	.long L$set$136
LASFDE54:
	.set L$set$137,Lframe0-Lsection__debug_frame
	.long L$set$137
	.quad	LFB29
	.set L$set$138,LFE29-LFB29
	.quad L$set$138
	.byte	0x4
	.set L$set$139,LCFI54-LFB29
	.long L$set$139
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$140,LCFI55-LCFI54
	.long L$set$140
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE54:
LSFDE56:
	.set L$set$141,LEFDE56-LASFDE56
	.long L$set$141
LASFDE56:
	.set L$set$142,Lframe0-Lsection__debug_frame
	.long L$set$142
	.quad	LFB30
	.set L$set$143,LFE30-LFB30
	.quad L$set$143
	.byte	0x4
	.set L$set$144,LCFI56-LFB30
	.long L$set$144
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$145,LCFI57-LCFI56
	.long L$set$145
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE56:
LSFDE58:
	.set L$set$146,LEFDE58-LASFDE58
	.long L$set$146
LASFDE58:
	.set L$set$147,Lframe0-Lsection__debug_frame
	.long L$set$147
	.quad	LFB31
	.set L$set$148,LFE31-LFB31
	.quad L$set$148
	.byte	0x4
	.set L$set$149,LCFI58-LFB31
	.long L$set$149
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$150,LCFI59-LCFI58
	.long L$set$150
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE58:
LSFDE60:
	.set L$set$151,LEFDE60-LASFDE60
	.long L$set$151
LASFDE60:
	.set L$set$152,Lframe0-Lsection__debug_frame
	.long L$set$152
	.quad	LFB32
	.set L$set$153,LFE32-LFB32
	.quad L$set$153
	.byte	0x4
	.set L$set$154,LCFI60-LFB32
	.long L$set$154
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.set L$set$155,LCFI61-LCFI60
	.long L$set$155
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE60:
LSFDE62:
	.set L$set$156,LEFDE62-LASFDE62
	.long L$set$156
LASFDE62:
	.set L$set$157,Lframe0-Lsection__debug_frame
	.long L$set$157
	.quad	LFB33
	.set L$set$158,LFE33-LFB33
	.quad L$set$158
	.byte	0x4
	.set L$set$159,LCFI62-LFB33
	.long L$set$159
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.set L$set$160,LCFI63-LCFI62
	.long L$set$160
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE62:
LSFDE64:
	.set L$set$161,LEFDE64-LASFDE64
	.long L$set$161
LASFDE64:
	.set L$set$162,Lframe0-Lsection__debug_frame
	.long L$set$162
	.quad	LFB34
	.set L$set$163,LFE34-LFB34
	.quad L$set$163
	.byte	0x4
	.set L$set$164,LCFI64-LFB34
	.long L$set$164
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.set L$set$165,LCFI65-LCFI64
	.long L$set$165
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE64:
LSFDE66:
	.set L$set$166,LEFDE66-LASFDE66
	.long L$set$166
LASFDE66:
	.set L$set$167,Lframe0-Lsection__debug_frame
	.long L$set$167
	.quad	LFB35
	.set L$set$168,LFE35-LFB35
	.quad L$set$168
	.byte	0x4
	.set L$set$169,LCFI66-LFB35
	.long L$set$169
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.set L$set$170,LCFI67-LCFI66
	.long L$set$170
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE66:
LSFDE68:
	.set L$set$171,LEFDE68-LASFDE68
	.long L$set$171
LASFDE68:
	.set L$set$172,Lframe0-Lsection__debug_frame
	.long L$set$172
	.quad	LFB36
	.set L$set$173,LFE36-LFB36
	.quad L$set$173
	.byte	0x4
	.set L$set$174,LCFI68-LFB36
	.long L$set$174
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.set L$set$175,LCFI69-LCFI68
	.long L$set$175
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE68:
LSFDE70:
	.set L$set$176,LEFDE70-LASFDE70
	.long L$set$176
LASFDE70:
	.set L$set$177,Lframe0-Lsection__debug_frame
	.long L$set$177
	.quad	LFB37
	.set L$set$178,LFE37-LFB37
	.quad L$set$178
	.byte	0x4
	.set L$set$179,LCFI70-LFB37
	.long L$set$179
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.set L$set$180,LCFI71-LCFI70
	.long L$set$180
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE70:
	.section __TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
EH_frame1:
	.set L$set$181,LECIE1-LSCIE1
	.long L$set$181
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
LSFDE73:
	.set L$set$182,LEFDE73-LASFDE73
	.long L$set$182
LASFDE73:
	.long	LASFDE73-EH_frame1
	.quad	LFB2-.
	.set L$set$183,LFE2-LFB2
	.quad L$set$183
	.uleb128 0
	.byte	0x4
	.set L$set$184,LCFI0-LFB2
	.long L$set$184
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$185,LCFI1-LCFI0
	.long L$set$185
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE73:
LSFDE75:
	.set L$set$186,LEFDE75-LASFDE75
	.long L$set$186
LASFDE75:
	.long	LASFDE75-EH_frame1
	.quad	LFB3-.
	.set L$set$187,LFE3-LFB3
	.quad L$set$187
	.uleb128 0
	.byte	0x4
	.set L$set$188,LCFI2-LFB3
	.long L$set$188
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$189,LCFI3-LCFI2
	.long L$set$189
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE75:
LSFDE77:
	.set L$set$190,LEFDE77-LASFDE77
	.long L$set$190
LASFDE77:
	.long	LASFDE77-EH_frame1
	.quad	LFB4-.
	.set L$set$191,LFE4-LFB4
	.quad L$set$191
	.uleb128 0
	.byte	0x4
	.set L$set$192,LCFI4-LFB4
	.long L$set$192
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$193,LCFI5-LCFI4
	.long L$set$193
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE77:
LSFDE79:
	.set L$set$194,LEFDE79-LASFDE79
	.long L$set$194
LASFDE79:
	.long	LASFDE79-EH_frame1
	.quad	LFB5-.
	.set L$set$195,LFE5-LFB5
	.quad L$set$195
	.uleb128 0
	.byte	0x4
	.set L$set$196,LCFI6-LFB5
	.long L$set$196
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$197,LCFI7-LCFI6
	.long L$set$197
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE79:
LSFDE81:
	.set L$set$198,LEFDE81-LASFDE81
	.long L$set$198
LASFDE81:
	.long	LASFDE81-EH_frame1
	.quad	LFB6-.
	.set L$set$199,LFE6-LFB6
	.quad L$set$199
	.uleb128 0
	.byte	0x4
	.set L$set$200,LCFI8-LFB6
	.long L$set$200
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$201,LCFI9-LCFI8
	.long L$set$201
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE81:
LSFDE83:
	.set L$set$202,LEFDE83-LASFDE83
	.long L$set$202
LASFDE83:
	.long	LASFDE83-EH_frame1
	.quad	LFB7-.
	.set L$set$203,LFE7-LFB7
	.quad L$set$203
	.uleb128 0
	.byte	0x4
	.set L$set$204,LCFI10-LFB7
	.long L$set$204
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$205,LCFI11-LCFI10
	.long L$set$205
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE83:
LSFDE85:
	.set L$set$206,LEFDE85-LASFDE85
	.long L$set$206
LASFDE85:
	.long	LASFDE85-EH_frame1
	.quad	LFB8-.
	.set L$set$207,LFE8-LFB8
	.quad L$set$207
	.uleb128 0
	.byte	0x4
	.set L$set$208,LCFI12-LFB8
	.long L$set$208
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$209,LCFI13-LCFI12
	.long L$set$209
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE85:
LSFDE87:
	.set L$set$210,LEFDE87-LASFDE87
	.long L$set$210
LASFDE87:
	.long	LASFDE87-EH_frame1
	.quad	LFB9-.
	.set L$set$211,LFE9-LFB9
	.quad L$set$211
	.uleb128 0
	.byte	0x4
	.set L$set$212,LCFI14-LFB9
	.long L$set$212
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$213,LCFI15-LCFI14
	.long L$set$213
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE87:
LSFDE89:
	.set L$set$214,LEFDE89-LASFDE89
	.long L$set$214
LASFDE89:
	.long	LASFDE89-EH_frame1
	.quad	LFB10-.
	.set L$set$215,LFE10-LFB10
	.quad L$set$215
	.uleb128 0
	.byte	0x4
	.set L$set$216,LCFI16-LFB10
	.long L$set$216
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$217,LCFI17-LCFI16
	.long L$set$217
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE89:
LSFDE91:
	.set L$set$218,LEFDE91-LASFDE91
	.long L$set$218
LASFDE91:
	.long	LASFDE91-EH_frame1
	.quad	LFB11-.
	.set L$set$219,LFE11-LFB11
	.quad L$set$219
	.uleb128 0
	.byte	0x4
	.set L$set$220,LCFI18-LFB11
	.long L$set$220
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$221,LCFI19-LCFI18
	.long L$set$221
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE91:
LSFDE93:
	.set L$set$222,LEFDE93-LASFDE93
	.long L$set$222
LASFDE93:
	.long	LASFDE93-EH_frame1
	.quad	LFB12-.
	.set L$set$223,LFE12-LFB12
	.quad L$set$223
	.uleb128 0
	.byte	0x4
	.set L$set$224,LCFI20-LFB12
	.long L$set$224
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$225,LCFI21-LCFI20
	.long L$set$225
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE93:
LSFDE95:
	.set L$set$226,LEFDE95-LASFDE95
	.long L$set$226
LASFDE95:
	.long	LASFDE95-EH_frame1
	.quad	LFB13-.
	.set L$set$227,LFE13-LFB13
	.quad L$set$227
	.uleb128 0
	.byte	0x4
	.set L$set$228,LCFI22-LFB13
	.long L$set$228
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$229,LCFI23-LCFI22
	.long L$set$229
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE95:
LSFDE97:
	.set L$set$230,LEFDE97-LASFDE97
	.long L$set$230
LASFDE97:
	.long	LASFDE97-EH_frame1
	.quad	LFB14-.
	.set L$set$231,LFE14-LFB14
	.quad L$set$231
	.uleb128 0
	.byte	0x4
	.set L$set$232,LCFI24-LFB14
	.long L$set$232
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$233,LCFI25-LCFI24
	.long L$set$233
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE97:
LSFDE99:
	.set L$set$234,LEFDE99-LASFDE99
	.long L$set$234
LASFDE99:
	.long	LASFDE99-EH_frame1
	.quad	LFB15-.
	.set L$set$235,LFE15-LFB15
	.quad L$set$235
	.uleb128 0
	.byte	0x4
	.set L$set$236,LCFI26-LFB15
	.long L$set$236
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$237,LCFI27-LCFI26
	.long L$set$237
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE99:
LSFDE101:
	.set L$set$238,LEFDE101-LASFDE101
	.long L$set$238
LASFDE101:
	.long	LASFDE101-EH_frame1
	.quad	LFB16-.
	.set L$set$239,LFE16-LFB16
	.quad L$set$239
	.uleb128 0
	.byte	0x4
	.set L$set$240,LCFI28-LFB16
	.long L$set$240
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$241,LCFI29-LCFI28
	.long L$set$241
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE101:
LSFDE103:
	.set L$set$242,LEFDE103-LASFDE103
	.long L$set$242
LASFDE103:
	.long	LASFDE103-EH_frame1
	.quad	LFB17-.
	.set L$set$243,LFE17-LFB17
	.quad L$set$243
	.uleb128 0
	.byte	0x4
	.set L$set$244,LCFI30-LFB17
	.long L$set$244
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$245,LCFI31-LCFI30
	.long L$set$245
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE103:
LSFDE105:
	.set L$set$246,LEFDE105-LASFDE105
	.long L$set$246
LASFDE105:
	.long	LASFDE105-EH_frame1
	.quad	LFB18-.
	.set L$set$247,LFE18-LFB18
	.quad L$set$247
	.uleb128 0
	.byte	0x4
	.set L$set$248,LCFI32-LFB18
	.long L$set$248
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$249,LCFI33-LCFI32
	.long L$set$249
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE105:
LSFDE107:
	.set L$set$250,LEFDE107-LASFDE107
	.long L$set$250
LASFDE107:
	.long	LASFDE107-EH_frame1
	.quad	LFB19-.
	.set L$set$251,LFE19-LFB19
	.quad L$set$251
	.uleb128 0
	.byte	0x4
	.set L$set$252,LCFI34-LFB19
	.long L$set$252
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$253,LCFI35-LCFI34
	.long L$set$253
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE107:
LSFDE109:
	.set L$set$254,LEFDE109-LASFDE109
	.long L$set$254
LASFDE109:
	.long	LASFDE109-EH_frame1
	.quad	LFB20-.
	.set L$set$255,LFE20-LFB20
	.quad L$set$255
	.uleb128 0
	.byte	0x4
	.set L$set$256,LCFI36-LFB20
	.long L$set$256
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$257,LCFI37-LCFI36
	.long L$set$257
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE109:
LSFDE111:
	.set L$set$258,LEFDE111-LASFDE111
	.long L$set$258
LASFDE111:
	.long	LASFDE111-EH_frame1
	.quad	LFB21-.
	.set L$set$259,LFE21-LFB21
	.quad L$set$259
	.uleb128 0
	.byte	0x4
	.set L$set$260,LCFI38-LFB21
	.long L$set$260
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$261,LCFI39-LCFI38
	.long L$set$261
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE111:
LSFDE113:
	.set L$set$262,LEFDE113-LASFDE113
	.long L$set$262
LASFDE113:
	.long	LASFDE113-EH_frame1
	.quad	LFB22-.
	.set L$set$263,LFE22-LFB22
	.quad L$set$263
	.uleb128 0
	.byte	0x4
	.set L$set$264,LCFI40-LFB22
	.long L$set$264
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$265,LCFI41-LCFI40
	.long L$set$265
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE113:
LSFDE115:
	.set L$set$266,LEFDE115-LASFDE115
	.long L$set$266
LASFDE115:
	.long	LASFDE115-EH_frame1
	.quad	LFB23-.
	.set L$set$267,LFE23-LFB23
	.quad L$set$267
	.uleb128 0
	.byte	0x4
	.set L$set$268,LCFI42-LFB23
	.long L$set$268
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$269,LCFI43-LCFI42
	.long L$set$269
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE115:
LSFDE117:
	.set L$set$270,LEFDE117-LASFDE117
	.long L$set$270
LASFDE117:
	.long	LASFDE117-EH_frame1
	.quad	LFB24-.
	.set L$set$271,LFE24-LFB24
	.quad L$set$271
	.uleb128 0
	.byte	0x4
	.set L$set$272,LCFI44-LFB24
	.long L$set$272
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$273,LCFI45-LCFI44
	.long L$set$273
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE117:
LSFDE119:
	.set L$set$274,LEFDE119-LASFDE119
	.long L$set$274
LASFDE119:
	.long	LASFDE119-EH_frame1
	.quad	LFB25-.
	.set L$set$275,LFE25-LFB25
	.quad L$set$275
	.uleb128 0
	.byte	0x4
	.set L$set$276,LCFI46-LFB25
	.long L$set$276
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$277,LCFI47-LCFI46
	.long L$set$277
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE119:
LSFDE121:
	.set L$set$278,LEFDE121-LASFDE121
	.long L$set$278
LASFDE121:
	.long	LASFDE121-EH_frame1
	.quad	LFB26-.
	.set L$set$279,LFE26-LFB26
	.quad L$set$279
	.uleb128 0
	.byte	0x4
	.set L$set$280,LCFI48-LFB26
	.long L$set$280
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$281,LCFI49-LCFI48
	.long L$set$281
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE121:
LSFDE123:
	.set L$set$282,LEFDE123-LASFDE123
	.long L$set$282
LASFDE123:
	.long	LASFDE123-EH_frame1
	.quad	LFB27-.
	.set L$set$283,LFE27-LFB27
	.quad L$set$283
	.uleb128 0
	.byte	0x4
	.set L$set$284,LCFI50-LFB27
	.long L$set$284
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$285,LCFI51-LCFI50
	.long L$set$285
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE123:
LSFDE125:
	.set L$set$286,LEFDE125-LASFDE125
	.long L$set$286
LASFDE125:
	.long	LASFDE125-EH_frame1
	.quad	LFB28-.
	.set L$set$287,LFE28-LFB28
	.quad L$set$287
	.uleb128 0
	.byte	0x4
	.set L$set$288,LCFI52-LFB28
	.long L$set$288
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$289,LCFI53-LCFI52
	.long L$set$289
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE125:
LSFDE127:
	.set L$set$290,LEFDE127-LASFDE127
	.long L$set$290
LASFDE127:
	.long	LASFDE127-EH_frame1
	.quad	LFB29-.
	.set L$set$291,LFE29-LFB29
	.quad L$set$291
	.uleb128 0
	.byte	0x4
	.set L$set$292,LCFI54-LFB29
	.long L$set$292
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$293,LCFI55-LCFI54
	.long L$set$293
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE127:
LSFDE129:
	.set L$set$294,LEFDE129-LASFDE129
	.long L$set$294
LASFDE129:
	.long	LASFDE129-EH_frame1
	.quad	LFB30-.
	.set L$set$295,LFE30-LFB30
	.quad L$set$295
	.uleb128 0
	.byte	0x4
	.set L$set$296,LCFI56-LFB30
	.long L$set$296
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$297,LCFI57-LCFI56
	.long L$set$297
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE129:
LSFDE131:
	.set L$set$298,LEFDE131-LASFDE131
	.long L$set$298
LASFDE131:
	.long	LASFDE131-EH_frame1
	.quad	LFB31-.
	.set L$set$299,LFE31-LFB31
	.quad L$set$299
	.uleb128 0
	.byte	0x4
	.set L$set$300,LCFI58-LFB31
	.long L$set$300
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$301,LCFI59-LCFI58
	.long L$set$301
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE131:
LSFDE133:
	.set L$set$302,LEFDE133-LASFDE133
	.long L$set$302
LASFDE133:
	.long	LASFDE133-EH_frame1
	.quad	LFB32-.
	.set L$set$303,LFE32-LFB32
	.quad L$set$303
	.uleb128 0
	.byte	0x4
	.set L$set$304,LCFI60-LFB32
	.long L$set$304
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.set L$set$305,LCFI61-LCFI60
	.long L$set$305
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE133:
LSFDE135:
	.set L$set$306,LEFDE135-LASFDE135
	.long L$set$306
LASFDE135:
	.long	LASFDE135-EH_frame1
	.quad	LFB33-.
	.set L$set$307,LFE33-LFB33
	.quad L$set$307
	.uleb128 0
	.byte	0x4
	.set L$set$308,LCFI62-LFB33
	.long L$set$308
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.set L$set$309,LCFI63-LCFI62
	.long L$set$309
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE135:
LSFDE137:
	.set L$set$310,LEFDE137-LASFDE137
	.long L$set$310
LASFDE137:
	.long	LASFDE137-EH_frame1
	.quad	LFB34-.
	.set L$set$311,LFE34-LFB34
	.quad L$set$311
	.uleb128 0
	.byte	0x4
	.set L$set$312,LCFI64-LFB34
	.long L$set$312
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.set L$set$313,LCFI65-LCFI64
	.long L$set$313
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE137:
LSFDE139:
	.set L$set$314,LEFDE139-LASFDE139
	.long L$set$314
LASFDE139:
	.long	LASFDE139-EH_frame1
	.quad	LFB35-.
	.set L$set$315,LFE35-LFB35
	.quad L$set$315
	.uleb128 0
	.byte	0x4
	.set L$set$316,LCFI66-LFB35
	.long L$set$316
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.set L$set$317,LCFI67-LCFI66
	.long L$set$317
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE139:
LSFDE141:
	.set L$set$318,LEFDE141-LASFDE141
	.long L$set$318
LASFDE141:
	.long	LASFDE141-EH_frame1
	.quad	LFB36-.
	.set L$set$319,LFE36-LFB36
	.quad L$set$319
	.uleb128 0
	.byte	0x4
	.set L$set$320,LCFI68-LFB36
	.long L$set$320
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.set L$set$321,LCFI69-LCFI68
	.long L$set$321
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE141:
LSFDE143:
	.set L$set$322,LEFDE143-LASFDE143
	.long L$set$322
LASFDE143:
	.long	LASFDE143-EH_frame1
	.quad	LFB37-.
	.set L$set$323,LFE37-LFB37
	.quad L$set$323
	.uleb128 0
	.byte	0x4
	.set L$set$324,LCFI70-LFB37
	.long L$set$324
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.set L$set$325,LCFI71-LCFI70
	.long L$set$325
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE143:
	.text
Letext0:
	.file 2 "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_types.ads"
	.section __DWARF,__debug_info,regular,debug
Lsection__debug_info:
Ldebug_info0:
	.long	0x1062
	.short	0x4
	.set L$set$326,Ldebug_abbrev0-Lsection__debug_abbrev
	.long L$set$326
	.byte	0x8
	.uleb128 0x1
	.ascii "GNU Ada 14.1.0 -O0 -gnatA -g -gnat2012 -gnatwa -gnatwe -fstack-check=specific -fstack-protector-strong -gnatR2js -gnatws -gnatec=/private/var/folders/bg/pt9l6y1j47q642kp3z5blrmh0000gn/T/GPR.70489/GNAT-TEMP-000003.TMP -gnatem=/private/var/folders/bg/pt9l6y1j47q642kp3z5blrmh0000gn/T/GPR.70489/GNAT-TEMP-000004.TMP -mmacosx-version-min=16.0.0 -mcpu=apple-m1 -mlittle-endian -mabi=lp64 -fPIC\0"
	.byte	0xd
	.ascii "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_types.adb\0"
	.ascii "/Users/sicarii/Desktop/anubis-spark/obj/gnatprove/data_representation\0"
	.quad	Ltext0
	.set L$set$327,Letext0-Ltext0
	.quad L$set$327
	.set L$set$328,Ldebug_line0-Lsection__debug_line
	.long L$set$328
	.uleb128 0x2
	.byte	0x1
	.byte	0x7
	.ascii "interfaces__unsigned_8\0"
	.uleb128 0x3
	.byte	0
	.byte	0xff
	.ascii "anubis_types__byte\0"
	.long	0x260
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
	.long	0x2c4
	.uleb128 0x6
	.ascii "P_ARRAY\0"
	.byte	0x2
	.byte	0x12
	.byte	0x9
	.long	0x2ab
	.byte	0
	.uleb128 0x7
	.byte	0x8
	.long	0x2c9
	.uleb128 0x6
	.ascii "P_BOUNDS\0"
	.byte	0x2
	.byte	0x12
	.byte	0x9
	.long	0x368
	.byte	0x8
	.byte	0
	.uleb128 0x8
	.long	0x278
	.uleb128 0x9
	.ascii "anubis_types__byte_array___XUA\0"
	.long	0x246
	.long	0x307
	.uleb128 0xa
	.long	0x307
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
	.byte	0x12
	.byte	0x9
	.long	0x368
	.uleb128 0x6
	.ascii "LB0\0"
	.byte	0x2
	.byte	0x12
	.byte	0x9
	.long	0x347
	.byte	0
	.uleb128 0xb
	.sleb128 0
	.sleb128 2147483647
	.ascii "natural\0"
	.long	0x307
	.uleb128 0x6
	.ascii "UB0\0"
	.byte	0x2
	.byte	0x12
	.byte	0x9
	.long	0x347
	.byte	0x4
	.byte	0
	.uleb128 0x7
	.byte	0x8
	.long	0x312
	.uleb128 0x9
	.ascii "anubis_types__x25519_secret_key__T17s\0"
	.long	0x246
	.long	0x3a4
	.uleb128 0xc
	.long	0x307
	.sleb128 32
	.byte	0
	.uleb128 0xd
	.ascii "anubis_types__x25519_secret_key\0"
	.byte	0x21
	.byte	0x2
	.byte	0x94
	.byte	0x9
	.long	0x3e8
	.uleb128 0xe
	.set L$set$329,LASF0-Lsection__debug_str
	.long L$set$329
	.byte	0x2
	.byte	0x95
	.byte	0x7
	.long	0x36e
	.byte	0
	.uleb128 0xe
	.set L$set$330,LASF1-Lsection__debug_str
	.long L$set$330
	.byte	0x2
	.byte	0x96
	.byte	0x7
	.long	0x3e8
	.byte	0x20
	.byte	0
	.uleb128 0x2
	.byte	0x1
	.byte	0x2
	.ascii "boolean\0"
	.uleb128 0x9
	.ascii "anubis_types__x25519_shared_secret__T19s\0"
	.long	0x246
	.long	0x42c
	.uleb128 0xc
	.long	0x307
	.sleb128 32
	.byte	0
	.uleb128 0xd
	.ascii "anubis_types__x25519_shared_secret\0"
	.byte	0x21
	.byte	0x2
	.byte	0x99
	.byte	0x9
	.long	0x473
	.uleb128 0xe
	.set L$set$331,LASF0-Lsection__debug_str
	.long L$set$331
	.byte	0x2
	.byte	0x9a
	.byte	0x7
	.long	0x3f3
	.byte	0
	.uleb128 0xe
	.set L$set$332,LASF1-Lsection__debug_str
	.long L$set$332
	.byte	0x2
	.byte	0x9b
	.byte	0x7
	.long	0x3e8
	.byte	0x20
	.byte	0
	.uleb128 0x9
	.ascii "anubis_types__ed25519_secret_key__T23s\0"
	.long	0x246
	.long	0x4aa
	.uleb128 0xc
	.long	0x307
	.sleb128 32
	.byte	0
	.uleb128 0xd
	.ascii "anubis_types__ed25519_secret_key\0"
	.byte	0x21
	.byte	0x2
	.byte	0xa2
	.byte	0x9
	.long	0x4ef
	.uleb128 0xe
	.set L$set$333,LASF0-Lsection__debug_str
	.long L$set$333
	.byte	0x2
	.byte	0xa3
	.byte	0x7
	.long	0x473
	.byte	0
	.uleb128 0xe
	.set L$set$334,LASF1-Lsection__debug_str
	.long L$set$334
	.byte	0x2
	.byte	0xa4
	.byte	0x7
	.long	0x3e8
	.byte	0x20
	.byte	0
	.uleb128 0x9
	.ascii "anubis_types__xchacha20_key__T27s\0"
	.long	0x246
	.long	0x521
	.uleb128 0xc
	.long	0x307
	.sleb128 32
	.byte	0
	.uleb128 0xd
	.ascii "anubis_types__xchacha20_key\0"
	.byte	0x21
	.byte	0x2
	.byte	0xab
	.byte	0x9
	.long	0x561
	.uleb128 0xe
	.set L$set$335,LASF0-Lsection__debug_str
	.long L$set$335
	.byte	0x2
	.byte	0xac
	.byte	0x7
	.long	0x4ef
	.byte	0
	.uleb128 0xe
	.set L$set$336,LASF1-Lsection__debug_str
	.long L$set$336
	.byte	0x2
	.byte	0xad
	.byte	0x7
	.long	0x3e8
	.byte	0x20
	.byte	0
	.uleb128 0x9
	.ascii "anubis_types__argon2_derived_key__T51s\0"
	.long	0x246
	.long	0x598
	.uleb128 0xc
	.long	0x307
	.sleb128 32
	.byte	0
	.uleb128 0xd
	.ascii "anubis_types__argon2_derived_key\0"
	.byte	0x21
	.byte	0x2
	.byte	0xe0
	.byte	0x9
	.long	0x5dd
	.uleb128 0xe
	.set L$set$337,LASF0-Lsection__debug_str
	.long L$set$337
	.byte	0x2
	.byte	0xe1
	.byte	0x7
	.long	0x561
	.byte	0
	.uleb128 0xe
	.set L$set$338,LASF1-Lsection__debug_str
	.long L$set$338
	.byte	0x2
	.byte	0xe2
	.byte	0x7
	.long	0x3e8
	.byte	0x20
	.byte	0
	.uleb128 0x9
	.ascii "anubis_types__ml_kem_secret_key__T35s\0"
	.long	0x246
	.long	0x614
	.uleb128 0xc
	.long	0x307
	.sleb128 3168
	.byte	0
	.uleb128 0xf
	.ascii "anubis_types__ml_kem_secret_key\0"
	.short	0xc61
	.byte	0x2
	.byte	0xbc
	.byte	0x9
	.long	0x65a
	.uleb128 0xe
	.set L$set$339,LASF0-Lsection__debug_str
	.long L$set$339
	.byte	0x2
	.byte	0xbd
	.byte	0x7
	.long	0x5dd
	.byte	0
	.uleb128 0x10
	.set L$set$340,LASF1-Lsection__debug_str
	.long L$set$340
	.byte	0x2
	.byte	0xbe
	.byte	0x7
	.long	0x3e8
	.short	0xc60
	.byte	0
	.uleb128 0x9
	.ascii "anubis_types__ml_kem_shared_secret__T39s\0"
	.long	0x246
	.long	0x693
	.uleb128 0xc
	.long	0x307
	.sleb128 32
	.byte	0
	.uleb128 0xd
	.ascii "anubis_types__ml_kem_shared_secret\0"
	.byte	0x21
	.byte	0x2
	.byte	0xc5
	.byte	0x9
	.long	0x6da
	.uleb128 0xe
	.set L$set$341,LASF0-Lsection__debug_str
	.long L$set$341
	.byte	0x2
	.byte	0xc6
	.byte	0x7
	.long	0x65a
	.byte	0
	.uleb128 0xe
	.set L$set$342,LASF1-Lsection__debug_str
	.long L$set$342
	.byte	0x2
	.byte	0xc7
	.byte	0x7
	.long	0x3e8
	.byte	0x20
	.byte	0
	.uleb128 0x9
	.ascii "anubis_types__ml_dsa_secret_key__T43s\0"
	.long	0x246
	.long	0x711
	.uleb128 0xc
	.long	0x307
	.sleb128 4896
	.byte	0
	.uleb128 0xf
	.ascii "anubis_types__ml_dsa_secret_key\0"
	.short	0x1321
	.byte	0x2
	.byte	0xce
	.byte	0x9
	.long	0x757
	.uleb128 0xe
	.set L$set$343,LASF0-Lsection__debug_str
	.long L$set$343
	.byte	0x2
	.byte	0xcf
	.byte	0x7
	.long	0x6da
	.byte	0
	.uleb128 0x10
	.set L$set$344,LASF1-Lsection__debug_str
	.long L$set$344
	.byte	0x2
	.byte	0xd0
	.byte	0x7
	.long	0x3e8
	.short	0x1320
	.byte	0
	.uleb128 0x9
	.ascii "anubis_types__master_key__T47s\0"
	.long	0x246
	.long	0x787
	.uleb128 0xc
	.long	0x307
	.sleb128 64
	.byte	0
	.uleb128 0xd
	.ascii "anubis_types__master_key\0"
	.byte	0x41
	.byte	0x2
	.byte	0xd7
	.byte	0x9
	.long	0x7c4
	.uleb128 0xe
	.set L$set$345,LASF0-Lsection__debug_str
	.long L$set$345
	.byte	0x2
	.byte	0xd8
	.byte	0x7
	.long	0x757
	.byte	0
	.uleb128 0xe
	.set L$set$346,LASF1-Lsection__debug_str
	.long L$set$346
	.byte	0x2
	.byte	0xd9
	.byte	0x7
	.long	0x3e8
	.byte	0x40
	.byte	0
	.uleb128 0x11
	.ascii "anubis_types__x25519_key_size\0"
	.byte	0x2
	.byte	0x15
	.byte	0x4
	.long	0x84b
	.byte	0x20
	.uleb128 0x4
	.byte	0x10
	.byte	0x5
	.ascii "universal_integer\0"
	.uleb128 0x8
	.long	0x7eb
	.uleb128 0x8
	.long	0x7eb
	.uleb128 0x8
	.long	0x7eb
	.uleb128 0x8
	.long	0x7eb
	.uleb128 0x8
	.long	0x7eb
	.uleb128 0x8
	.long	0x7eb
	.uleb128 0x8
	.long	0x7eb
	.uleb128 0x8
	.long	0x7eb
	.uleb128 0x8
	.long	0x7eb
	.uleb128 0x8
	.long	0x7eb
	.uleb128 0x8
	.long	0x7eb
	.uleb128 0x8
	.long	0x7eb
	.uleb128 0x8
	.long	0x7eb
	.uleb128 0x8
	.long	0x7eb
	.uleb128 0x8
	.long	0x7eb
	.uleb128 0x8
	.long	0x7eb
	.uleb128 0x11
	.ascii "anubis_types__ed25519_key_size\0"
	.byte	0x2
	.byte	0x16
	.byte	0x4
	.long	0x846
	.byte	0x20
	.uleb128 0x11
	.ascii "anubis_types__ed25519_sig_size\0"
	.byte	0x2
	.byte	0x17
	.byte	0x4
	.long	0x841
	.byte	0x40
	.uleb128 0x11
	.ascii "anubis_types__xchacha20_key_size\0"
	.byte	0x2
	.byte	0x18
	.byte	0x4
	.long	0x83c
	.byte	0x20
	.uleb128 0x11
	.ascii "anubis_types__xchacha20_nonce_size\0"
	.byte	0x2
	.byte	0x19
	.byte	0x4
	.long	0x837
	.byte	0x18
	.uleb128 0x11
	.ascii "anubis_types__poly1305_tag_size\0"
	.byte	0x2
	.byte	0x1a
	.byte	0x4
	.long	0x832
	.byte	0x10
	.uleb128 0x12
	.ascii "anubis_types__ml_kem_1024_public_key_size\0"
	.byte	0x2
	.byte	0x1d
	.byte	0x4
	.long	0x82d
	.short	0x620
	.uleb128 0x12
	.ascii "anubis_types__ml_kem_1024_secret_key_size\0"
	.byte	0x2
	.byte	0x1e
	.byte	0x4
	.long	0x828
	.short	0xc60
	.uleb128 0x12
	.ascii "anubis_types__ml_kem_1024_ciphertext_size\0"
	.byte	0x2
	.byte	0x1f
	.byte	0x4
	.long	0x823
	.short	0x620
	.uleb128 0x11
	.ascii "anubis_types__ml_kem_1024_shared_secret_size\0"
	.byte	0x2
	.byte	0x20
	.byte	0x4
	.long	0x81e
	.byte	0x20
	.uleb128 0x12
	.ascii "anubis_types__ml_dsa_87_public_key_size\0"
	.byte	0x2
	.byte	0x22
	.byte	0x4
	.long	0x819
	.short	0xa20
	.uleb128 0x12
	.ascii "anubis_types__ml_dsa_87_secret_key_size\0"
	.byte	0x2
	.byte	0x23
	.byte	0x4
	.long	0x814
	.short	0x1320
	.uleb128 0x12
	.ascii "anubis_types__ml_dsa_87_signature_size\0"
	.byte	0x2
	.byte	0x24
	.byte	0x4
	.long	0x80f
	.short	0x1213
	.uleb128 0x11
	.ascii "anubis_types__argon2_salt_size\0"
	.byte	0x2
	.byte	0x27
	.byte	0x4
	.long	0x80a
	.byte	0x20
	.uleb128 0x11
	.ascii "anubis_types__argon2_output_size\0"
	.byte	0x2
	.byte	0x28
	.byte	0x4
	.long	0x805
	.byte	0x20
	.uleb128 0x11
	.ascii "anubis_types__master_key_size\0"
	.byte	0x2
	.byte	0x29
	.byte	0x4
	.long	0x800
	.byte	0x40
	.uleb128 0x13
	.ascii "anubis_types__zeroize__6\0"
	.byte	0x1
	.byte	0x62
	.byte	0x4
	.quad	LFB37
	.set L$set$347,LFE37-LFB37
	.quad L$set$347
	.uleb128 0x1
	.byte	0x9c
	.long	0xbc1
	.uleb128 0x14
	.set L$set$348,LASF0-Lsection__debug_str
	.long L$set$348
	.byte	0x2
	.byte	0x87
	.byte	0x17
	.long	0x2c4
	.uleb128 0x2
	.byte	0x91
	.sleb128 -32
	.uleb128 0x15
	.ascii "anubis_types__zeroize__6__L_6__T61b___L\0"
	.long	0x307
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.uleb128 0x15
	.ascii "anubis_types__zeroize__6__L_6__T61b___U\0"
	.long	0x307
	.uleb128 0x2
	.byte	0x91
	.sleb128 -4
	.uleb128 0x16
	.quad	LBB9
	.set L$set$349,LBE9-LBB9
	.quad L$set$349
	.uleb128 0x17
	.ascii "i\0"
	.byte	0x1
	.byte	0x64
	.byte	0xb
	.long	0x307
	.uleb128 0x2
	.byte	0x91
	.sleb128 -12
	.byte	0
	.byte	0
	.uleb128 0x13
	.ascii "anubis_types__zeroize__5\0"
	.byte	0x1
	.byte	0x58
	.byte	0x4
	.quad	LFB36
	.set L$set$350,LFE36-LFB36
	.quad L$set$350
	.uleb128 0x1
	.byte	0x9c
	.long	0xc23
	.uleb128 0x18
	.ascii "key\0"
	.byte	0x2
	.byte	0x82
	.byte	0x17
	.long	0xc23
	.uleb128 0x2
	.byte	0x91
	.sleb128 -24
	.uleb128 0x16
	.quad	LBB8
	.set L$set$351,LBE8-LBB8
	.quad L$set$351
	.uleb128 0x17
	.ascii "i\0"
	.byte	0x1
	.byte	0x5a
	.byte	0xb
	.long	0x307
	.uleb128 0x2
	.byte	0x91
	.sleb128 -4
	.byte	0
	.byte	0
	.uleb128 0x19
	.byte	0x8
	.long	0x787
	.uleb128 0x13
	.ascii "anubis_types__zeroize__4\0"
	.byte	0x1
	.byte	0x4e
	.byte	0x4
	.quad	LFB35
	.set L$set$352,LFE35-LFB35
	.quad L$set$352
	.uleb128 0x1
	.byte	0x9c
	.long	0xc8b
	.uleb128 0x18
	.ascii "key\0"
	.byte	0x2
	.byte	0x7f
	.byte	0x17
	.long	0xc8b
	.uleb128 0x2
	.byte	0x91
	.sleb128 -24
	.uleb128 0x16
	.quad	LBB7
	.set L$set$353,LBE7-LBB7
	.quad L$set$353
	.uleb128 0x17
	.ascii "i\0"
	.byte	0x1
	.byte	0x50
	.byte	0xb
	.long	0x307
	.uleb128 0x2
	.byte	0x91
	.sleb128 -4
	.byte	0
	.byte	0
	.uleb128 0x19
	.byte	0x8
	.long	0x711
	.uleb128 0x13
	.ascii "anubis_types__zeroize__3\0"
	.byte	0x1
	.byte	0x44
	.byte	0x4
	.quad	LFB34
	.set L$set$354,LFE34-LFB34
	.quad L$set$354
	.uleb128 0x1
	.byte	0x9c
	.long	0xcf3
	.uleb128 0x18
	.ascii "key\0"
	.byte	0x2
	.byte	0x7c
	.byte	0x17
	.long	0xcf3
	.uleb128 0x2
	.byte	0x91
	.sleb128 -24
	.uleb128 0x16
	.quad	LBB6
	.set L$set$355,LBE6-LBB6
	.quad L$set$355
	.uleb128 0x17
	.ascii "i\0"
	.byte	0x1
	.byte	0x46
	.byte	0xb
	.long	0x307
	.uleb128 0x2
	.byte	0x91
	.sleb128 -4
	.byte	0
	.byte	0
	.uleb128 0x19
	.byte	0x8
	.long	0x614
	.uleb128 0x13
	.ascii "anubis_types__zeroize__2\0"
	.byte	0x1
	.byte	0x3a
	.byte	0x4
	.quad	LFB33
	.set L$set$356,LFE33-LFB33
	.quad L$set$356
	.uleb128 0x1
	.byte	0x9c
	.long	0xd5b
	.uleb128 0x18
	.ascii "key\0"
	.byte	0x2
	.byte	0x79
	.byte	0x17
	.long	0xd5b
	.uleb128 0x2
	.byte	0x91
	.sleb128 -24
	.uleb128 0x16
	.quad	LBB5
	.set L$set$357,LBE5-LBB5
	.quad L$set$357
	.uleb128 0x17
	.ascii "i\0"
	.byte	0x1
	.byte	0x3c
	.byte	0xb
	.long	0x307
	.uleb128 0x2
	.byte	0x91
	.sleb128 -4
	.byte	0
	.byte	0
	.uleb128 0x19
	.byte	0x8
	.long	0x4aa
	.uleb128 0x13
	.ascii "anubis_types__zeroize\0"
	.byte	0x1
	.byte	0x30
	.byte	0x4
	.quad	LFB32
	.set L$set$358,LFE32-LFB32
	.quad L$set$358
	.uleb128 0x1
	.byte	0x9c
	.long	0xdc0
	.uleb128 0x18
	.ascii "key\0"
	.byte	0x2
	.byte	0x76
	.byte	0x17
	.long	0xdc0
	.uleb128 0x2
	.byte	0x91
	.sleb128 -24
	.uleb128 0x16
	.quad	LBB4
	.set L$set$359,LBE4-LBB4
	.quad L$set$359
	.uleb128 0x17
	.ascii "i\0"
	.byte	0x1
	.byte	0x32
	.byte	0xb
	.long	0x307
	.uleb128 0x2
	.byte	0x91
	.sleb128 -4
	.byte	0
	.byte	0
	.uleb128 0x19
	.byte	0x8
	.long	0x3a4
	.uleb128 0x1a
	.ascii "anubis_types__is_valid__9\0"
	.byte	0x1
	.byte	0x16
	.byte	0x4
	.long	0x3e8
	.quad	LFB31
	.set L$set$360,LFE31-LFB31
	.quad L$set$360
	.uleb128 0x1
	.byte	0x9c
	.long	0xe0e
	.uleb128 0x18
	.ascii "key\0"
	.byte	0x2
	.byte	0x68
	.byte	0x17
	.long	0xc23
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.byte	0
	.uleb128 0x1a
	.ascii "anubis_types__is_valid__8\0"
	.byte	0x1
	.byte	0x15
	.byte	0x4
	.long	0x3e8
	.quad	LFB30
	.set L$set$361,LFE30-LFB30
	.quad L$set$361
	.uleb128 0x1
	.byte	0x9c
	.long	0xe56
	.uleb128 0x18
	.ascii "key\0"
	.byte	0x2
	.byte	0x67
	.byte	0x17
	.long	0xc8b
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.byte	0
	.uleb128 0x1a
	.ascii "anubis_types__is_valid__7\0"
	.byte	0x1
	.byte	0x14
	.byte	0x4
	.long	0x3e8
	.quad	LFB29
	.set L$set$362,LFE29-LFB29
	.quad L$set$362
	.uleb128 0x1
	.byte	0x9c
	.long	0xea1
	.uleb128 0x18
	.ascii "secret\0"
	.byte	0x2
	.byte	0x66
	.byte	0x17
	.long	0xea1
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.byte	0
	.uleb128 0x19
	.byte	0x8
	.long	0x693
	.uleb128 0x1a
	.ascii "anubis_types__is_valid__6\0"
	.byte	0x1
	.byte	0x13
	.byte	0x4
	.long	0x3e8
	.quad	LFB28
	.set L$set$363,LFE28-LFB28
	.quad L$set$363
	.uleb128 0x1
	.byte	0x9c
	.long	0xeef
	.uleb128 0x18
	.ascii "key\0"
	.byte	0x2
	.byte	0x65
	.byte	0x17
	.long	0xcf3
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.byte	0
	.uleb128 0x1a
	.ascii "anubis_types__is_valid__5\0"
	.byte	0x1
	.byte	0x12
	.byte	0x4
	.long	0x3e8
	.quad	LFB27
	.set L$set$364,LFE27-LFB27
	.quad L$set$364
	.uleb128 0x1
	.byte	0x9c
	.long	0xf37
	.uleb128 0x18
	.ascii "key\0"
	.byte	0x2
	.byte	0x64
	.byte	0x17
	.long	0xf37
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.byte	0
	.uleb128 0x19
	.byte	0x8
	.long	0x598
	.uleb128 0x1a
	.ascii "anubis_types__is_valid__4\0"
	.byte	0x1
	.byte	0x11
	.byte	0x4
	.long	0x3e8
	.quad	LFB26
	.set L$set$365,LFE26-LFB26
	.quad L$set$365
	.uleb128 0x1
	.byte	0x9c
	.long	0xf85
	.uleb128 0x18
	.ascii "key\0"
	.byte	0x2
	.byte	0x63
	.byte	0x17
	.long	0xf85
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.byte	0
	.uleb128 0x19
	.byte	0x8
	.long	0x521
	.uleb128 0x1a
	.ascii "anubis_types__is_valid__3\0"
	.byte	0x1
	.byte	0x10
	.byte	0x4
	.long	0x3e8
	.quad	LFB25
	.set L$set$366,LFE25-LFB25
	.quad L$set$366
	.uleb128 0x1
	.byte	0x9c
	.long	0xfd3
	.uleb128 0x18
	.ascii "key\0"
	.byte	0x2
	.byte	0x62
	.byte	0x17
	.long	0xd5b
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.byte	0
	.uleb128 0x1a
	.ascii "anubis_types__is_valid__2\0"
	.byte	0x1
	.byte	0xf
	.byte	0x4
	.long	0x3e8
	.quad	LFB24
	.set L$set$367,LFE24-LFB24
	.quad L$set$367
	.uleb128 0x1
	.byte	0x9c
	.long	0x101e
	.uleb128 0x18
	.ascii "secret\0"
	.byte	0x2
	.byte	0x61
	.byte	0x17
	.long	0x101e
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.byte	0
	.uleb128 0x19
	.byte	0x8
	.long	0x42c
	.uleb128 0x1b
	.ascii "anubis_types__is_valid\0"
	.byte	0x1
	.byte	0xe
	.byte	0x4
	.long	0x3e8
	.quad	LFB23
	.set L$set$368,LFE23-LFB23
	.quad L$set$368
	.uleb128 0x1
	.byte	0x9c
	.uleb128 0x18
	.ascii "key\0"
	.byte	0x2
	.byte	0x60
	.byte	0x17
	.long	0xdc0
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
	.uleb128 0x10
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
	.uleb128 0x11
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
	.uleb128 0x5
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
	.uleb128 0x14
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
	.uleb128 0x15
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
	.uleb128 0x19
	.uleb128 0x10
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
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
	.long	0x1c4
	.short	0x2
	.set L$set$369,Ldebug_info0-Lsection__debug_info
	.long L$set$369
	.long	0x1066
	.long	0xaff
	.ascii "anubis_types__zeroize__6\0"
	.long	0xbc1
	.ascii "anubis_types__zeroize__5\0"
	.long	0xc29
	.ascii "anubis_types__zeroize__4\0"
	.long	0xc91
	.ascii "anubis_types__zeroize__3\0"
	.long	0xcf9
	.ascii "anubis_types__zeroize__2\0"
	.long	0xd61
	.ascii "anubis_types__zeroize\0"
	.long	0xdc6
	.ascii "anubis_types__is_valid__9\0"
	.long	0xe0e
	.ascii "anubis_types__is_valid__8\0"
	.long	0xe56
	.ascii "anubis_types__is_valid__7\0"
	.long	0xea7
	.ascii "anubis_types__is_valid__6\0"
	.long	0xeef
	.ascii "anubis_types__is_valid__5\0"
	.long	0xf3d
	.ascii "anubis_types__is_valid__4\0"
	.long	0xf8b
	.ascii "anubis_types__is_valid__3\0"
	.long	0xfd3
	.ascii "anubis_types__is_valid__2\0"
	.long	0x1024
	.ascii "anubis_types__is_valid\0"
	.long	0
	.section __DWARF,__debug_pubtypes,regular,debug
Lsection__debug_pubtypes:
	.long	0x38b
	.short	0x2
	.set L$set$370,Ldebug_info0-Lsection__debug_info
	.long L$set$370
	.long	0x1066
	.long	0x22c
	.ascii "interfaces__unsigned_8\0"
	.long	0x260
	.ascii "anubis_types__TbyteB\0"
	.long	0x307
	.ascii "integer\0"
	.long	0x2c9
	.ascii "anubis_types__byte_array___XUA\0"
	.long	0x312
	.ascii "anubis_types__byte_array___XUB\0"
	.long	0x278
	.ascii "anubis_types__byte_array\0"
	.long	0x36e
	.ascii "anubis_types__x25519_secret_key__T17s\0"
	.long	0x3e8
	.ascii "boolean\0"
	.long	0x3a4
	.ascii "anubis_types__x25519_secret_key\0"
	.long	0x3f3
	.ascii "anubis_types__x25519_shared_secret__T19s\0"
	.long	0x42c
	.ascii "anubis_types__x25519_shared_secret\0"
	.long	0x473
	.ascii "anubis_types__ed25519_secret_key__T23s\0"
	.long	0x4aa
	.ascii "anubis_types__ed25519_secret_key\0"
	.long	0x4ef
	.ascii "anubis_types__xchacha20_key__T27s\0"
	.long	0x521
	.ascii "anubis_types__xchacha20_key\0"
	.long	0x561
	.ascii "anubis_types__argon2_derived_key__T51s\0"
	.long	0x598
	.ascii "anubis_types__argon2_derived_key\0"
	.long	0x5dd
	.ascii "anubis_types__ml_kem_secret_key__T35s\0"
	.long	0x614
	.ascii "anubis_types__ml_kem_secret_key\0"
	.long	0x65a
	.ascii "anubis_types__ml_kem_shared_secret__T39s\0"
	.long	0x693
	.ascii "anubis_types__ml_kem_shared_secret\0"
	.long	0x6da
	.ascii "anubis_types__ml_dsa_secret_key__T43s\0"
	.long	0x711
	.ascii "anubis_types__ml_dsa_secret_key\0"
	.long	0x757
	.ascii "anubis_types__master_key__T47s\0"
	.long	0x787
	.ascii "anubis_types__master_key\0"
	.long	0x7eb
	.ascii "universal_integer\0"
	.long	0
	.section __DWARF,__debug_aranges,regular,debug
Lsection__debug_aranges:
	.long	0x2c
	.short	0x2
	.set L$set$371,Ldebug_info0-Lsection__debug_info
	.long L$set$371
	.byte	0x8
	.byte	0
	.short	0
	.short	0
	.quad	Ltext0
	.set L$set$372,Letext0-Ltext0
	.quad L$set$372
	.quad	0
	.quad	0
	.section __DWARF,__debug_line,regular,debug
Lsection__debug_line:
Ldebug_line0:
	.section __DWARF,__debug_str,regular,debug
Lsection__debug_str:
LASF0:
	.ascii "data\0"
LASF1:
	.ascii "valid\0"
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
