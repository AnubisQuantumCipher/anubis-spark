	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
Ltext0:
	.file 1 "/Users/sicarii/Desktop/anubis-spark/src/anubis_main.adb"
	.const
	.align	3
lC48:
	.ascii "\342\225\224\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\227"
	.align	3
lC49:
	.ascii "\342\225\221  ANUBIS-SPARK v0.2.0 - Quantum-Resistant File Encryption     \342\225\221"
	.align	3
lC50:
	.ascii "\342\225\221  PLATINUM-LEVEL SPARK VERIFICATION + NIST POST-QUANTUM       \342\225\221"
	.align	3
lC51:
	.ascii "\342\225\232\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\235"
	.align	3
lC52:
	.ascii "Security Architecture:"
	.align	3
lC53:
	.ascii "  Classical:     X25519 + Ed25519 + XChaCha20-Poly1305"
	.align	3
lC54:
	.ascii "  Post-Quantum:  ML-KEM-1024 + ML-DSA-87 (NIST Level 5)"
	.align	3
lC55:
	.ascii "  Key Derivation: HKDF-SHA256 + Argon2id"
	.align	3
lC56:
	.ascii "  Verification:  SPARK Gold Level (31/31 proofs)"
	.text
	.align	2
_anubis_main__print_banner.3:
LFB2:
	.loc 1 17 4
	sub	x10, sp, #16384
	str	xzr, [x10, 3936]
	stp	x29, x30, [sp, -160]!
LCFI0:
	mov	x29, sp
LCFI1:
	stp	x20, x21, [sp, 16]
	stp	x22, x23, [sp, 32]
	stp	x24, x25, [sp, 48]
	stp	x26, x27, [sp, 64]
LCFI2:
	str	x16, [x29, 152]
LBB2:
	.loc 1 19 7
	adrp	x2, lC48@PAGE
	add	x0, x2, lC48@PAGEOFF;
	adrp	x2, lC45@PAGE
	add	x1, x2, lC45@PAGEOFF;
	bl	_ada__text_io__put_line__2
LBE2:
LBB3:
	.loc 1 20 7
	adrp	x0, lC49@PAGE
	add	x0, x0, lC49@PAGEOFF;
	str	x0, [x29, 80]
	adrp	x0, lC46@PAGE
	add	x0, x0, lC46@PAGEOFF;
	str	x0, [x29, 88]
	ldp	x0, x1, [x29, 80]
	bl	_ada__text_io__put_line__2
LBE3:
LBB4:
	.loc 1 21 7
	adrp	x0, lC50@PAGE
	add	x0, x0, lC50@PAGEOFF;
	str	x0, [x29, 96]
	adrp	x0, lC46@PAGE
	add	x0, x0, lC46@PAGEOFF;
	str	x0, [x29, 104]
	ldp	x0, x1, [x29, 96]
	bl	_ada__text_io__put_line__2
LBE4:
LBB5:
	.loc 1 22 7
	adrp	x0, lC51@PAGE
	add	x0, x0, lC51@PAGEOFF;
	str	x0, [x29, 112]
	adrp	x0, lC45@PAGE
	add	x0, x0, lC45@PAGEOFF;
	str	x0, [x29, 120]
	ldp	x0, x1, [x29, 112]
	bl	_ada__text_io__put_line__2
LBE5:
	.loc 1 23 7
	mov	w0, 1
	bl	_ada__text_io__new_line__2
LBB6:
	.loc 1 24 7
	adrp	x0, lC52@PAGE
	add	x0, x0, lC52@PAGEOFF;
	str	x0, [x29, 128]
	adrp	x0, lC24@PAGE
	add	x0, x0, lC24@PAGEOFF;
	str	x0, [x29, 136]
	ldp	x0, x1, [x29, 128]
	bl	_ada__text_io__put_line__2
LBE6:
LBB7:
	.loc 1 25 7
	adrp	x0, lC53@PAGE
	add	x26, x0, lC53@PAGEOFF;
	adrp	x0, lC47@PAGE
	add	x27, x0, lC47@PAGEOFF;
	mov	x0, x26
	mov	x1, x27
	bl	_ada__text_io__put_line__2
LBE7:
LBB8:
	.loc 1 26 7
	adrp	x0, lC54@PAGE
	add	x24, x0, lC54@PAGEOFF;
	adrp	x0, lC16@PAGE
	add	x25, x0, lC16@PAGEOFF;
	mov	x0, x24
	mov	x1, x25
	bl	_ada__text_io__put_line__2
LBE8:
LBB9:
	.loc 1 27 7
	adrp	x0, lC55@PAGE
	add	x22, x0, lC55@PAGEOFF;
	adrp	x0, lC39@PAGE
	add	x23, x0, lC39@PAGEOFF;
	mov	x0, x22
	mov	x1, x23
	bl	_ada__text_io__put_line__2
LBE9:
LBB10:
	.loc 1 28 7
	adrp	x0, lC56@PAGE
	add	x20, x0, lC56@PAGEOFF;
	adrp	x0, lC17@PAGE
	add	x21, x0, lC17@PAGEOFF;
	mov	x0, x20
	mov	x1, x21
	bl	_ada__text_io__put_line__2
LBE10:
	.loc 1 29 7
	mov	w0, 1
	bl	_ada__text_io__new_line__2
	.loc 1 30 8
	nop
	ldp	x20, x21, [sp, 16]
	ldp	x22, x23, [sp, 32]
	ldp	x24, x25, [sp, 48]
	ldp	x26, x27, [sp, 64]
	ldp	x29, x30, [sp], 160
LCFI3:
	ret
LFE2:
	.const
	.align	2
lC45:
	.word	1
	.word	195
	.align	2
lC46:
	.word	1
	.word	68
	.align	2
lC24:
	.word	1
	.word	22
	.align	2
lC47:
	.word	1
	.word	54
	.align	2
lC16:
	.word	1
	.word	55
	.align	2
lC39:
	.word	1
	.word	40
	.align	2
lC17:
	.word	1
	.word	48
	.text
	.const
	.align	3
lC57:
	.ascii "anubis_main.adb"
	.space 1
	.align	3
lC58:
	.ascii "--help"
	.align	3
lC59:
	.ascii "version"
	.align	3
lC60:
	.ascii "--version"
	.align	3
lC61:
	.ascii "keygen"
	.align	3
lC62:
	.ascii "identity.key"
	.align	3
lC63:
	.ascii "Generating Hybrid Post-Quantum Identity..."
	.align	3
lC64:
	.ascii "\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220"
	.align	3
lC65:
	.ascii "Generating hybrid keypairs... "
	.align	3
lC66:
	.ascii "\342\234\227 FAILED"
	.align	3
lC67:
	.ascii "ERROR: Failed to generate identity keypair."
	.align	3
lC68:
	.ascii "\342\234\223"
	.align	3
lC69:
	.ascii "Identity generated successfully!"
	.align	3
lC70:
	.ascii "Key Details:"
	.align	3
lC71:
	.ascii "  X25519 (ECDH):          32-byte public key"
	.align	3
lC72:
	.ascii "  ML-KEM-1024 (PQ-KEM):   1568-byte public key"
	.align	3
lC73:
	.ascii "  Ed25519 (Signatures):   32-byte public key"
	.align	3
lC74:
	.ascii "  ML-DSA-87 (PQ-Sig):     2592-byte public key"
	.align	3
lC75:
	.ascii "Saving identity to "
	.align	3
lC76:
	.ascii "... "
	.align	3
lC77:
	.ascii "ERROR: Failed to save identity to file."
	.align	3
lC78:
	.ascii "Identity saved successfully!"
	.align	3
lC79:
	.ascii "File: "
	.align	3
lC80:
	.ascii "\342\232\240\357\270\217  SECURITY NOTICE:"
	.align	3
lC81:
	.ascii "  This file contains SECRET KEYS. Protect it carefully!"
	.align	3
lC82:
	.ascii "  - Store in a secure location"
	.align	3
lC83:
	.ascii "  - Set restrictive file permissions (chmod 600)"
	.align	3
lC84:
	.ascii "  - Consider encrypting with passphrase (future feature)"
	.align	3
lC85:
	.ascii "  - Keep backups in secure locations"
	.align	3
lC86:
	.ascii "encrypt"
	.align	3
lC87:
	.ascii "Encrypt command not yet implemented."
	.align	3
lC88:
	.ascii "File encryption infrastructure is ready but needs file I/O."
	.align	3
lC89:
	.ascii "decrypt"
	.align	3
lC90:
	.ascii "Decrypt command not yet implemented."
	.align	3
lC91:
	.ascii "File decryption infrastructure is ready but needs file I/O."
	.align	3
lC92:
	.ascii "Sign command not yet implemented."
	.align	3
lC93:
	.ascii "Use: anubis-spark test (includes hybrid signature test)"
	.align	3
lC94:
	.ascii "verify"
	.align	3
lC95:
	.ascii "Verify command not yet implemented."
	.align	3
lC96:
	.ascii "Unknown command: "
	.align	3
lC97:
	.ascii "Use: anubis-spark help"
	.text
	.align	2
	.globl __ada_anubis_main
__ada_anubis_main:
LFB1:
	.loc 1 15 1
	sub	x10, sp, #16384
LEHB0:
	str	xzr, [x10]
	sub	x10, x10, #4096
	str	xzr, [x10]
	sub	x10, x10, #4096
	str	xzr, [x10]
	sub	x10, x10, #4096
	str	xzr, [x10, 2672]
	stp	x29, x30, [sp, -96]!
LCFI4:
	mov	x29, sp
LCFI5:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
	stp	x25, x26, [sp, 64]
	stp	x27, x28, [sp, 80]
	mov	x13, 13616
	sub	sp, sp, x13
LCFI6:
	.loc 1 15 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x1, [x0]
	str	x1, [x29, -8]
	mov	x1, 0
	add	x0, x29, 96
	.loc 1 15 1 is_stmt 0 discriminator 1
	sub	x1, x29, #16384
	str	x0, [x1, 4008]
	.loc 1 234 7 is_stmt 1
	bl	_ada__command_line__argument_count
	.loc 1 234 4 discriminator 1
	cmp	w0, 0
	bne	L4
	.loc 1 235 7
	sub	x0, x29, #12288
	sub	x0, x0, #136
	mov	x16, x0
	bl	_anubis_main__print_usage.0
LEHE0:
	.loc 1 236 7
	b	L3
L4:
LBB11:
	sub	x0, x29, #12288
	sub	x0, x0, #112
	mov	x8, x0
LEHB1:
	bl	_system__secondary_stack__ss_mark
	.loc 1 240 36
	mov	w0, 1
	bl	_ada__command_line__argument
	.loc 1 240 36 is_stmt 0 discriminator 1
	mov	x24, x0
	mov	x25, x1
	mov	x0, x25
	ldr	w0, [x0]
	sub	x1, x29, #16384
	str	w0, [x1, 3764]
	mov	x0, x25
	ldr	w0, [x0, 4]
	sub	x1, x29, #16384
	str	w0, [x1, 3768]
	.loc 1 240 7 is_stmt 1 discriminator 1
	sub	x0, x29, #16384
	ldrsw	x0, [x0, 3764]
	sub	x1, x29, #16384
	str	x0, [x1, 3864]
	sub	x0, x29, #16384
	ldr	w1, [x0, 3768]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3764]
	cmp	w1, w0
	blt	L6
	.loc 1 240 7 is_stmt 0 discriminator 2
	sub	x0, x29, #16384
	ldrsw	x0, [x0, 3768]
	sub	x1, x29, #16384
	str	x0, [x1, 3872]
	b	L7
L6:
	.loc 1 240 7 discriminator 3
	sub	x0, x29, #16384
	ldrsw	x0, [x0, 3764]
	sub	x0, x0, #1
	sub	x1, x29, #16384
	str	x0, [x1, 3872]
L7:
	.loc 1 240 7 discriminator 5
	sub	x0, x29, #16384
	ldr	w1, [x0, 3768]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3764]
	cmp	w1, w0
	blt	L9
	.loc 1 240 7 discriminator 6
	sub	x0, x29, #16384
	ldrsw	x1, [x0, 3768]
	sub	x0, x29, #16384
	ldrsw	x0, [x0, 3764]
	sub	x0, x1, x0
	add	x0, x0, 1
	mov	x20, x0
	mov	x21, 0
	lsr	x0, x20, 61
	lsl	x23, x21, 3
	orr	x23, x0, x23
	lsl	x22, x20, 3
L9:
	.loc 1 240 7 discriminator 9
	sub	x0, x29, #16384
	ldr	w1, [x0, 3768]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3764]
	cmp	w1, w0
	.loc 1 240 36 is_stmt 1 discriminator 13
	sub	x0, x29, #16384
	ldr	w1, [x0, 3768]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3764]
	cmp	w1, w0
	blt	L12
	.loc 1 240 36 is_stmt 0 discriminator 14
	sub	x0, x29, #16384
	ldr	w0, [x0, 3764]
	cmp	w0, 0
	bgt	L12
	.loc 1 240 36 discriminator 16
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L13
	bl	___stack_chk_fail
L13:
	mov	w1, 240
	adrp	x0, lC57@PAGE
	add	x0, x0, lC57@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L12:
	.loc 1 240 7 is_stmt 1 discriminator 17
	mov	x0, x24
	sub	x1, x29, #16384
	str	x0, [x1, 3880]
	.loc 1 242 18
	sub	x0, x29, #16384
	ldr	w1, [x0, 3768]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3764]
	sub	w0, w1, w0
	cmp	w0, 3
	bne	L14
	.loc 1 242 18 is_stmt 0 discriminator 1
	mov	x0, x24
	ldr	w1, [x0]
	.loc 1 242 18 discriminator 3
	mov	w0, 25960
	movk	w0, 0x706c, lsl 16
	cmp	w1, w0
	bne	L14
	.loc 1 242 18 discriminator 4
	mov	w19, 1
	.loc 1 242 18
	b	L15
L14:
	.loc 1 242 18 discriminator 5
	mov	w19, 0
L15:
	.loc 1 242 38 is_stmt 1 discriminator 7
	sub	x0, x29, #16384
	ldr	w1, [x0, 3768]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3764]
	sub	w0, w1, w0
	cmp	w0, 5
	bne	L16
	.loc 1 242 38 is_stmt 0 discriminator 8
	mov	x3, x24
	mov	x2, 6
	adrp	x0, lC58@PAGE
	add	x1, x0, lC58@PAGEOFF;
	mov	x0, x3
	bl	_memcmp
	.loc 1 242 38 discriminator 1
	cmp	w0, 0
	bne	L16
	.loc 1 242 38 discriminator 10
	mov	w0, 1
	.loc 1 242 38
	b	L17
L16:
	.loc 1 242 38 discriminator 11
	mov	w0, 0
L17:
	.loc 1 242 27 is_stmt 1 discriminator 13
	cmp	w19, 0
	ccmp	w0, 0, 0, eq
	cset	w0, ne
	and	w2, w0, 255
	.loc 1 242 60 discriminator 13
	sub	x0, x29, #16384
	ldr	w1, [x0, 3768]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3764]
	sub	w0, w1, w0
	cmp	w0, 1
	bne	L18
	.loc 1 242 60 is_stmt 0 discriminator 14
	mov	x0, x24
	ldrh	w1, [x0]
	.loc 1 242 60 discriminator 16
	mov	w0, 26669
	cmp	w1, w0
	bne	L18
	.loc 1 242 60 discriminator 17
	mov	w0, 1
	.loc 1 242 60
	b	L19
L18:
	.loc 1 242 60 discriminator 18
	mov	w0, 0
L19:
	.loc 1 242 49 is_stmt 1 discriminator 20
	cmp	w0, 0
	ccmp	w2, 0, 0, eq
	cset	w0, ne
	and	w0, w0, 255
	.loc 1 242 7 discriminator 20
	cmp	w0, 0
	beq	L20
	.loc 1 243 10
	sub	x0, x29, #12288
	sub	x0, x0, #136
	mov	x16, x0
	bl	_anubis_main__print_usage.0
	b	L21
L20:
	.loc 1 244 21
	sub	x0, x29, #16384
	ldr	w1, [x0, 3768]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3764]
	sub	w0, w1, w0
	cmp	w0, 6
	bne	L22
	.loc 1 244 21 is_stmt 0 discriminator 1
	mov	x3, x24
	mov	x2, 7
	adrp	x0, lC59@PAGE
	add	x1, x0, lC59@PAGEOFF;
	mov	x0, x3
	bl	_memcmp
	cmp	w0, 0
	bne	L22
	.loc 1 244 21 discriminator 3
	mov	w19, 1
	.loc 1 244 21
	b	L23
L22:
	.loc 1 244 21 discriminator 4
	mov	w19, 0
L23:
	.loc 1 244 44 is_stmt 1 discriminator 6
	sub	x0, x29, #16384
	ldr	w1, [x0, 3768]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3764]
	sub	w0, w1, w0
	cmp	w0, 8
	bne	L24
	.loc 1 244 44 is_stmt 0 discriminator 7
	mov	x3, x24
	mov	x2, 9
	adrp	x0, lC60@PAGE
	add	x1, x0, lC60@PAGEOFF;
	mov	x0, x3
	bl	_memcmp
	.loc 1 244 44 discriminator 1
	cmp	w0, 0
	bne	L24
	.loc 1 244 44 discriminator 9
	mov	w0, 1
	.loc 1 244 44
	b	L25
L24:
	.loc 1 244 44 discriminator 10
	mov	w0, 0
L25:
	.loc 1 244 33 is_stmt 1 discriminator 12
	cmp	w19, 0
	ccmp	w0, 0, 0, eq
	cset	w0, ne
	and	w2, w0, 255
	.loc 1 244 69 discriminator 12
	sub	x0, x29, #16384
	ldr	w1, [x0, 3768]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3764]
	sub	w0, w1, w0
	cmp	w0, 1
	bne	L26
	.loc 1 244 69 is_stmt 0 discriminator 13
	mov	x0, x24
	ldrh	w1, [x0]
	.loc 1 244 69 discriminator 15
	mov	w0, 30253
	cmp	w1, w0
	bne	L26
	.loc 1 244 69 discriminator 16
	mov	w0, 1
	.loc 1 244 69
	b	L27
L26:
	.loc 1 244 69 discriminator 17
	mov	w0, 0
L27:
	.loc 1 244 58 is_stmt 1 discriminator 19
	cmp	w0, 0
	ccmp	w2, 0, 0, eq
	cset	w0, ne
	and	w0, w0, 255
	.loc 1 244 7 discriminator 19
	cmp	w0, 0
	beq	L28
	.loc 1 245 10
	sub	x0, x29, #12288
	sub	x0, x0, #136
	mov	x16, x0
	bl	_anubis_main__print_version.1
	b	L21
L28:
	.loc 1 246 21
	sub	x0, x29, #16384
	ldr	w1, [x0, 3768]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3764]
	sub	w0, w1, w0
	cmp	w0, 3
	bne	L29
	.loc 1 246 21 is_stmt 0 discriminator 1
	mov	x0, x24
	ldr	w1, [x0]
	.loc 1 246 21 discriminator 3
	mov	w0, 25972
	movk	w0, 0x7473, lsl 16
	cmp	w1, w0
	bne	L29
	.loc 1 246 21 discriminator 4
	mov	w2, 1
	.loc 1 246 21
	b	L30
L29:
	.loc 1 246 21 discriminator 5
	mov	w2, 0
L30:
	.loc 1 246 41 is_stmt 1 discriminator 7
	sub	x0, x29, #16384
	ldr	w1, [x0, 3768]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3764]
	sub	w0, w1, w0
	cmp	w0, 7
	bne	L31
	.loc 1 246 41 is_stmt 0 discriminator 8
	mov	x0, x24
	ldr	x1, [x0]
	.loc 1 246 41 discriminator 10
	mov	x0, 25971
	movk	x0, 0x666c, lsl 16
	movk	x0, 0x6574, lsl 32
	movk	x0, 0x7473, lsl 48
	cmp	x1, x0
	bne	L31
	.loc 1 246 41 discriminator 11
	mov	w0, 1
	.loc 1 246 41
	b	L32
L31:
	.loc 1 246 41 discriminator 12
	mov	w0, 0
L32:
	.loc 1 246 30 is_stmt 1 discriminator 14
	cmp	w2, 0
	ccmp	w0, 0, 0, eq
	cset	w0, ne
	and	w0, w0, 255
	.loc 1 246 7 discriminator 14
	cmp	w0, 0
	beq	L33
	.loc 1 247 10
	sub	x0, x29, #12288
	sub	x0, x0, #136
	mov	x16, x0
	bl	_anubis_main__run_self_test.2
LEHE1:
	b	L21
L33:
	.loc 1 248 21
	sub	x0, x29, #16384
	ldr	w1, [x0, 3768]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3764]
	sub	w0, w1, w0
	.loc 1 248 7
	cmp	w0, 5
	bne	L34
	.loc 1 248 21 discriminator 1
	mov	x3, x24
	mov	x2, 6
	adrp	x0, lC61@PAGE
	add	x1, x0, lC61@PAGEOFF;
	mov	x0, x3
	bl	_memcmp
	cmp	w0, 0
	bne	L34
LBB12:
	.loc 1 250 10
	mov	x0, sp
	mov	x20, x0
	sub	x0, x29, #12288
	sub	x0, x0, #136
	mov	x8, x0
LEHB2:
	bl	_system__secondary_stack__ss_mark
	.loc 1 251 50
	bl	_ada__command_line__argument_count
	.loc 1 251 65 discriminator 1
	cmp	w0, 2
	cset	w0, gt
	and	w0, w0, 255
	.loc 1 251 47 discriminator 1
	cmp	w0, 0
	beq	L35
	.loc 1 251 79 discriminator 2
	mov	w0, 2
	bl	_ada__command_line__argument
	mov	x2, x0
	mov	x3, x1
	.loc 1 251 92 discriminator 4
	mov	x0, x3
	ldr	w1, [x0, 4]
	mov	x0, x3
	ldr	w0, [x0]
	cmp	w1, w0
	.loc 1 251 92 is_stmt 0 discriminator 12
	mov	x0, x3
	ldr	w1, [x0, 4]
	mov	x0, x3
	ldr	w0, [x0]
	sub	w0, w1, w0
	cmp	w0, 7
	bne	L38
	.loc 1 251 92 discriminator 13
	mov	x0, x2
	ldr	x1, [x0]
	.loc 1 251 92 discriminator 15
	mov	x0, 11565
	movk	x0, 0x756f, lsl 16
	movk	x0, 0x7074, lsl 32
	movk	x0, 0x7475, lsl 48
	cmp	x1, x0
	bne	L38
	.loc 1 251 92 discriminator 16
	mov	w0, 1
	.loc 1 251 92
	b	L39
L38:
	.loc 1 251 92 discriminator 17
	mov	w0, 0
L39:
	.loc 1 251 70 is_stmt 1 discriminator 19
	cmp	w0, 0
	beq	L35
	.loc 1 252 53
	mov	w0, 3
	bl	_ada__command_line__argument
	.loc 1 252 53 is_stmt 0 discriminator 1
	mov	x2, x0
	mov	x3, x1
	.loc 1 251 47 is_stmt 1
	mov	x0, x2
	mov	x1, x3
	sub	x2, x29, #12288
	sub	x2, x2, #512
	stp	x0, x1, [x2, 32]
	b	L40
L35:
	.loc 1 251 47 is_stmt 0 discriminator 20
	adrp	x0, lC62@PAGE
	add	x0, x0, lC62@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3616]
	adrp	x0, lC0@PAGE
	add	x0, x0, lC0@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3624]
L40:
	.loc 1 251 47 discriminator 22
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 32]
	mov	x4, x0
	mov	x5, x1
	mov	x0, x5
	ldr	w0, [x0]
	sub	x1, x29, #16384
	str	w0, [x1, 3772]
	mov	x0, x5
	ldr	w0, [x0, 4]
	sub	x1, x29, #16384
	str	w0, [x1, 3776]
	.loc 1 251 13 is_stmt 1 discriminator 22
	sub	x0, x29, #16384
	ldrsw	x0, [x0, 3772]
	sub	x1, x29, #16384
	str	x0, [x1, 3888]
	sub	x0, x29, #16384
	ldr	w1, [x0, 3776]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3772]
	cmp	w1, w0
	blt	L41
	.loc 1 251 13 is_stmt 0 discriminator 23
	sub	x0, x29, #16384
	ldrsw	x0, [x0, 3776]
	sub	x1, x29, #16384
	str	x0, [x1, 3896]
	b	L42
L41:
	.loc 1 251 13 discriminator 24
	sub	x0, x29, #16384
	ldrsw	x0, [x0, 3772]
	sub	x0, x0, #1
	sub	x1, x29, #16384
	str	x0, [x1, 3896]
L42:
	.loc 1 251 13 discriminator 26
	sub	x0, x29, #16384
	ldr	w1, [x0, 3776]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3772]
	cmp	w1, w0
	blt	L44
	.loc 1 251 13 discriminator 27
	sub	x0, x29, #16384
	ldrsw	x1, [x0, 3776]
	sub	x0, x29, #16384
	ldrsw	x0, [x0, 3772]
	sub	x0, x1, x0
	add	x0, x0, 1
	sub	x1, x29, #16384
	str	x0, [x1, 3584]
	sub	x0, x29, #16384
	str	xzr, [x0, 3592]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x1, x2, [x0]
	mov	x0, x1
	lsr	x0, x0, 61
	mov	x3, x2
	lsl	x3, x3, 3
	sub	x6, x29, #16384
	str	x3, [x6, 3656]
	sub	x3, x29, #16384
	ldr	x3, [x3, 3656]
	orr	x0, x0, x3
	sub	x3, x29, #16384
	str	x0, [x3, 3656]
	mov	x0, x1
	lsl	x0, x0, 3
	sub	x1, x29, #16384
	str	x0, [x1, 3648]
L44:
	.loc 1 251 13 discriminator 30
	sub	x0, x29, #16384
	ldr	w1, [x0, 3776]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3772]
	cmp	w1, w0
	blt	L45
	.loc 1 251 13 discriminator 31
	sub	x0, x29, #16384
	ldrsw	x1, [x0, 3776]
	sub	x0, x29, #16384
	ldrsw	x0, [x0, 3772]
	sub	x0, x1, x0
	add	x6, x0, 1
	b	L46
L45:
	.loc 1 251 13 discriminator 32
	mov	x6, 0
L46:
	.loc 1 251 47 is_stmt 1 discriminator 34
	sub	x0, x29, #16384
	ldr	w1, [x0, 3776]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3772]
	cmp	w1, w0
	blt	L47
	.loc 1 251 47 is_stmt 0 discriminator 35
	sub	x0, x29, #16384
	ldr	w0, [x0, 3772]
	cmp	w0, 0
	bgt	L47
	.loc 1 251 47 discriminator 37
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L48
	bl	___stack_chk_fail
L48:
	mov	w1, 251
	adrp	x0, lC57@PAGE
	add	x0, x0, lC57@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L47:
	.loc 1 251 13 is_stmt 1 discriminator 38
	sub	x0, x29, #16384
	ldr	w1, [x0, 3776]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3772]
	cmp	w1, w0
	blt	L50
	.loc 1 251 13 is_stmt 0 discriminator 39
	sub	x0, x29, #16384
	ldrsw	x1, [x0, 3776]
	sub	x0, x29, #16384
	ldrsw	x0, [x0, 3772]
	sub	x0, x1, x0
	add	x0, x0, 1
	sub	x1, x29, #16384
	str	x0, [x1, 3568]
	sub	x0, x29, #16384
	str	xzr, [x0, 3576]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x1, x2, [x0, -16]
	mov	x0, x1
	lsr	x0, x0, 61
	mov	x3, x2
	lsl	x3, x3, 3
	sub	x7, x29, #16384
	str	x3, [x7, 3640]
	sub	x3, x29, #16384
	ldr	x3, [x3, 3640]
	orr	x0, x0, x3
	sub	x3, x29, #16384
	str	x0, [x3, 3640]
	mov	x0, x1
	lsl	x0, x0, 3
	sub	x1, x29, #16384
	str	x0, [x1, 3632]
L50:
	.loc 1 251 13 discriminator 42
	sub	x0, x29, #16384
	ldr	w1, [x0, 3776]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3772]
	cmp	w1, w0
	blt	L51
	.loc 1 251 13 discriminator 43
	sub	x0, x29, #16384
	ldrsw	x1, [x0, 3776]
	sub	x0, x29, #16384
	ldrsw	x0, [x0, 3772]
	sub	x0, x1, x0
	add	x0, x0, 1
	b	L52
L51:
	.loc 1 251 13 discriminator 44
	mov	x0, 0
L52:
	.loc 1 251 13 discriminator 46
	add	x0, x0, 15
	lsr	x0, x0, 4
	lsl	x1, x0, 4
	and	x3, x1, -4096
	sub	x0, sp, #12288
	sub	x2, x0, x3
L53:
	cmp	x0, x2
	beq	L54
	sub	x0, x0, #4096
	str	xzr, [x0]
	b	L53
L54:
	sub	x0, x1, x3
	sub	x0, x2, x0
	str	xzr, [x0]
	sub	sp, sp, x1
	mov	x0, sp
	.loc 1 251 13 discriminator 47
	sub	x1, x29, #16384
	str	x0, [x1, 3904]
	mov	x1, x4
	sub	x0, x29, #16384
	ldr	x0, [x0, 3904]
	mov	x3, x0
	mov	x0, x6
	mov	x2, x0
	mov	x0, x3
	bl	_memcpy
	.loc 1 254 13 is_stmt 1
	sub	x0, x29, #12288
	sub	x0, x0, #80
	bl	_anubis_types__storage__identity_keypairIP
	.loc 1 257 13
	sub	x0, x29, #12288
	sub	x0, x0, #136
	mov	x16, x0
	bl	_anubis_main__print_banner.3
LBB13:
	.loc 1 258 13
	adrp	x0, lC63@PAGE
	add	x0, x0, lC63@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 2784]
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 2792]
	sub	x0, x29, #12288
	sub	x0, x0, #1536
	ldp	x0, x1, [x0, 224]
	bl	_ada__text_io__put_line__2
LBE13:
LBB14:
	.loc 1 259 13
	adrp	x0, lC64@PAGE
	add	x0, x0, lC64@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 2800]
	adrp	x0, lC2@PAGE
	add	x0, x0, lC2@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 2808]
	sub	x0, x29, #12288
	sub	x0, x0, #1536
	ldp	x0, x1, [x0, 240]
	bl	_ada__text_io__put_line__2
LBE14:
	.loc 1 260 13
	mov	w0, 1
	bl	_ada__text_io__new_line__2
LBB15:
	.loc 1 262 13
	adrp	x0, lC65@PAGE
	add	x0, x0, lC65@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 2816]
	adrp	x0, lC3@PAGE
	add	x0, x0, lC3@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 2824]
	sub	x0, x29, #12288
	sub	x0, x0, #1024
	ldp	x0, x1, [x0, -256]
	bl	_ada__text_io__put__4
LBE15:
	.loc 1 263 20
	sub	x0, x29, #12288
	sub	x0, x0, #80
	bl	_anubis_types__storage__generate_identity
	.loc 1 263 20 is_stmt 0 discriminator 1
	sub	x1, x29, #16384
	strb	w0, [x1, 3763]
	.loc 1 265 16 is_stmt 1
	sub	x0, x29, #16384
	ldrb	w0, [x0, 3763]
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 265 13
	cmp	w0, 0
	beq	L55
LBB16:
	.loc 1 266 16
	adrp	x0, lC66@PAGE
	add	x0, x0, lC66@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 2832]
	adrp	x0, lC4@PAGE
	add	x0, x0, lC4@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 2840]
	sub	x0, x29, #12288
	sub	x0, x0, #1024
	ldp	x0, x1, [x0, -240]
	bl	_ada__text_io__put_line__2
LBE16:
LBB17:
	.loc 1 267 16
	adrp	x0, lC67@PAGE
	add	x0, x0, lC67@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 2848]
	adrp	x0, lC5@PAGE
	add	x0, x0, lC5@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 2856]
	sub	x0, x29, #12288
	sub	x0, x0, #1024
	ldp	x0, x1, [x0, -224]
	bl	_ada__text_io__put_line__2
LBE17:
	.loc 1 268 16
	mov	w19, 0
	b	L56
L55:
LBB18:
	.loc 1 271 13
	adrp	x0, lC68@PAGE
	add	x0, x0, lC68@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 2864]
	adrp	x0, lC6@PAGE
	add	x0, x0, lC6@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 2872]
	sub	x0, x29, #12288
	sub	x0, x0, #1024
	ldp	x0, x1, [x0, -208]
	bl	_ada__text_io__put_line__2
LBE18:
LBB19:
	.loc 1 272 13
	adrp	x0, lC69@PAGE
	add	x0, x0, lC69@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 2880]
	adrp	x0, lC7@PAGE
	add	x0, x0, lC7@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 2888]
	sub	x0, x29, #12288
	sub	x0, x0, #1024
	ldp	x0, x1, [x0, -192]
	bl	_ada__text_io__put_line__2
LBE19:
	.loc 1 273 13
	mov	w0, 1
	bl	_ada__text_io__new_line__2
LBB20:
	.loc 1 275 13
	adrp	x0, lC70@PAGE
	add	x0, x0, lC70@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 2896]
	adrp	x0, lC0@PAGE
	add	x0, x0, lC0@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 2904]
	sub	x0, x29, #12288
	sub	x0, x0, #1024
	ldp	x0, x1, [x0, -176]
	bl	_ada__text_io__put_line__2
LBE20:
LBB21:
	.loc 1 276 13
	adrp	x0, lC71@PAGE
	add	x0, x0, lC71@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 2912]
	adrp	x0, lC8@PAGE
	add	x0, x0, lC8@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 2920]
	sub	x0, x29, #12288
	sub	x0, x0, #1024
	ldp	x0, x1, [x0, -160]
	bl	_ada__text_io__put_line__2
LBE21:
LBB22:
	.loc 1 277 13
	adrp	x0, lC72@PAGE
	add	x0, x0, lC72@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 2928]
	adrp	x0, lC9@PAGE
	add	x0, x0, lC9@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 2936]
	sub	x0, x29, #12288
	sub	x0, x0, #1024
	ldp	x0, x1, [x0, -144]
	bl	_ada__text_io__put_line__2
LBE22:
LBB23:
	.loc 1 278 13
	adrp	x0, lC73@PAGE
	add	x0, x0, lC73@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 2944]
	adrp	x0, lC8@PAGE
	add	x0, x0, lC8@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 2952]
	sub	x0, x29, #12288
	sub	x0, x0, #1024
	ldp	x0, x1, [x0, -128]
	bl	_ada__text_io__put_line__2
LBE23:
LBB24:
	.loc 1 279 13
	adrp	x0, lC74@PAGE
	add	x0, x0, lC74@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 2960]
	adrp	x0, lC9@PAGE
	add	x0, x0, lC9@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 2968]
	sub	x0, x29, #12288
	sub	x0, x0, #1024
	ldp	x0, x1, [x0, -112]
	bl	_ada__text_io__put_line__2
LBE24:
	.loc 1 280 13
	mov	w0, 1
	bl	_ada__text_io__new_line__2
LEHE2:
LBB25:
	.loc 1 282 13
	mov	x0, sp
	mov	x19, x0
	.loc 1 282 54 discriminator 1
	sub	x0, x29, #16384
	ldr	w1, [x0, 3776]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3772]
	cmp	w1, w0
	blt	L57
	sub	x0, x29, #16384
	ldr	w1, [x0, 3776]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3772]
	sub	w0, w1, w0
	add	w0, w0, 1
	b	L58
L57:
	.loc 1 282 54 is_stmt 0 discriminator 2
	mov	w0, 0
L58:
	.loc 1 282 54 discriminator 4
	add	w0, w0, 19
	add	w0, w0, 4
	sub	x1, x29, #16384
	str	w0, [x1, 3780]
	sub	x0, x29, #16384
	ldrsw	x0, [x0, 3780]
	sub	x1, x29, #16384
	str	x0, [x1, 3912]
	sub	x0, x29, #16384
	ldrsw	x0, [x0, 3780]
	sub	x1, x29, #16384
	str	x0, [x1, 3552]
	sub	x0, x29, #16384
	str	xzr, [x0, 3560]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x1, x2, [x0, -32]
	mov	x0, x1
	lsr	x0, x0, 61
	mov	x3, x2
	lsl	x3, x3, 3
	sub	x4, x29, #16384
	str	x3, [x4, 3752]
	sub	x3, x29, #16384
	ldr	x3, [x3, 3752]
	orr	x0, x0, x3
	sub	x3, x29, #16384
	str	x0, [x3, 3752]
	mov	x0, x1
	lsl	x0, x0, 3
	sub	x1, x29, #16384
	str	x0, [x1, 3744]
	sub	x0, x29, #16384
	ldrsw	x0, [x0, 3780]
	sub	x1, x29, #16384
	str	x0, [x1, 3536]
	sub	x0, x29, #16384
	str	xzr, [x0, 3544]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x1, x2, [x0, -48]
	mov	x0, x1
	lsr	x0, x0, 61
	mov	x3, x2
	lsl	x3, x3, 3
	sub	x4, x29, #16384
	str	x3, [x4, 3736]
	sub	x3, x29, #16384
	ldr	x3, [x3, 3736]
	orr	x0, x0, x3
	sub	x3, x29, #16384
	str	x0, [x3, 3736]
	mov	x0, x1
	lsl	x0, x0, 3
	sub	x1, x29, #16384
	str	x0, [x1, 3728]
	sub	x0, x29, #16384
	ldrsw	x0, [x0, 3780]
	add	x0, x0, 15
	lsr	x0, x0, 4
	lsl	x1, x0, 4
	and	x3, x1, -4096
	sub	x0, sp, #12288
	sub	x2, x0, x3
L59:
	cmp	x0, x2
	beq	L60
	sub	x0, x0, #4096
LEHB3:
	str	xzr, [x0]
	b	L59
L60:
	sub	x0, x1, x3
	sub	x0, x2, x0
	str	xzr, [x0]
	sub	sp, sp, x1
	mov	x0, sp
	.loc 1 282 54 discriminator 6
	sub	x1, x29, #16384
	str	x0, [x1, 3920]
LBB26:
	sub	x0, x29, #16384
	ldr	x0, [x0, 3920]
	sub	x1, x29, #16384
	str	x0, [x1, 2976]
	mov	w0, 1
	sub	x1, x29, #16384
	str	w0, [x1, 3792]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3780]
	sub	x1, x29, #16384
	str	w0, [x1, 3796]
	sub	x0, x29, #12288
	sub	x0, x0, #304
	sub	x1, x29, #16384
	str	x0, [x1, 2984]
	adrp	x0, lC75@PAGE
	add	x0, x0, lC75@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 2992]
	adrp	x0, lC10@PAGE
	add	x0, x0, lC10@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3000]
	sub	x0, x29, #16384
	ldr	x0, [x0, 3904]
	sub	x1, x29, #16384
	str	x0, [x1, 3008]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3772]
	sub	x1, x29, #16384
	str	w0, [x1, 3800]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3776]
	sub	x1, x29, #16384
	str	w0, [x1, 3804]
	sub	x0, x29, #12288
	sub	x0, x0, #296
	sub	x1, x29, #16384
	str	x0, [x1, 3016]
	adrp	x0, lC76@PAGE
	add	x0, x0, lC76@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3024]
	adrp	x0, lC11@PAGE
	add	x0, x0, lC11@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3032]
	sub	x0, x29, #12288
	sub	x0, x0, #1024
	ldp	x6, x7, [x0, -48]
	sub	x0, x29, #12288
	sub	x0, x0, #1024
	ldp	x4, x5, [x0, -64]
	sub	x0, x29, #12288
	sub	x0, x0, #1024
	ldp	x2, x3, [x0, -80]
	sub	x0, x29, #12288
	sub	x0, x0, #1024
	ldp	x0, x1, [x0, -96]
	bl	_system__concat_3__str_concat_3
LBE26:
	.loc 1 282 13 is_stmt 1 discriminator 8
	sub	x0, x29, #16384
	ldr	x0, [x0, 3920]
	sub	x1, x29, #16384
	str	x0, [x1, 3040]
	mov	w0, 1
	sub	x1, x29, #16384
	str	w0, [x1, 3808]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3780]
	sub	x1, x29, #16384
	str	w0, [x1, 3812]
	sub	x0, x29, #12288
	sub	x0, x0, #288
	sub	x1, x29, #16384
	str	x0, [x1, 3048]
	sub	x0, x29, #12288
	sub	x0, x0, #1024
	ldp	x0, x1, [x0, -32]
	bl	_ada__text_io__put__4
LEHE3:
LEHB4:
LEHE4:
	.loc 1 282 0 discriminator 10
	mov	sp, x19
LBE25:
	.loc 1 283 20
	sub	x0, x29, #16384
	ldr	x0, [x0, 3904]
	sub	x1, x29, #16384
	str	x0, [x1, 3056]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3772]
	sub	x1, x29, #16384
	str	w0, [x1, 3816]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3776]
	sub	x1, x29, #16384
	str	w0, [x1, 3820]
	sub	x0, x29, #12288
	sub	x0, x0, #280
	sub	x1, x29, #16384
	str	x0, [x1, 3064]
	sub	x0, x29, #12288
	sub	x0, x0, #80
	sub	x1, x29, #12288
	sub	x1, x1, #1024
	ldp	x1, x2, [x1, -16]
LEHB5:
	bl	_anubis_types__storage__save_identity
	.loc 1 283 20 is_stmt 0 discriminator 1
	sub	x1, x29, #16384
	strb	w0, [x1, 3763]
	.loc 1 285 16 is_stmt 1
	sub	x0, x29, #16384
	ldrb	w0, [x0, 3763]
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 285 13
	cmp	w0, 0
	beq	L61
LBB27:
	.loc 1 286 16
	adrp	x0, lC66@PAGE
	add	x0, x0, lC66@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3072]
	adrp	x0, lC4@PAGE
	add	x0, x0, lC4@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3080]
	sub	x0, x29, #12288
	sub	x0, x0, #1024
	ldp	x0, x1, [x0]
	bl	_ada__text_io__put_line__2
LBE27:
LBB28:
	.loc 1 287 16
	adrp	x0, lC77@PAGE
	add	x0, x0, lC77@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3088]
	adrp	x0, lC12@PAGE
	add	x0, x0, lC12@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3096]
	sub	x0, x29, #12288
	sub	x0, x0, #1024
	ldp	x0, x1, [x0, 16]
	bl	_ada__text_io__put_line__2
LBE28:
	.loc 1 288 23
	sub	x0, x29, #12288
	sub	x0, x0, #80
	bl	_anubis_types__storage__zeroize_identity
	.loc 1 289 16
	mov	w19, 0
	b	L56
L61:
LBB29:
	.loc 1 292 13
	adrp	x0, lC68@PAGE
	add	x0, x0, lC68@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3104]
	adrp	x0, lC6@PAGE
	add	x0, x0, lC6@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3112]
	sub	x0, x29, #12288
	sub	x0, x0, #1024
	ldp	x0, x1, [x0, 32]
	bl	_ada__text_io__put_line__2
LBE29:
	.loc 1 293 13
	mov	w0, 1
	bl	_ada__text_io__new_line__2
LBB30:
	.loc 1 294 13
	adrp	x0, lC64@PAGE
	add	x0, x0, lC64@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3120]
	adrp	x0, lC2@PAGE
	add	x0, x0, lC2@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3128]
	sub	x0, x29, #12288
	sub	x0, x0, #1024
	ldp	x0, x1, [x0, 48]
	bl	_ada__text_io__put_line__2
LBE30:
LBB31:
	.loc 1 295 13
	adrp	x0, lC78@PAGE
	add	x0, x0, lC78@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3136]
	adrp	x0, lC13@PAGE
	add	x0, x0, lC13@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3144]
	sub	x0, x29, #12288
	sub	x0, x0, #1024
	ldp	x0, x1, [x0, 64]
	bl	_ada__text_io__put_line__2
LEHE5:
LBE31:
LBB32:
	.loc 1 296 13
	mov	x0, sp
	mov	x19, x0
	.loc 1 296 32 discriminator 1
	sub	x0, x29, #16384
	ldr	w1, [x0, 3776]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3772]
	cmp	w1, w0
	blt	L62
	sub	x0, x29, #16384
	ldr	w1, [x0, 3776]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3772]
	sub	w0, w1, w0
	add	w0, w0, 1
	b	L63
L62:
	.loc 1 296 32 is_stmt 0 discriminator 2
	mov	w0, 0
L63:
	.loc 1 296 32 discriminator 4
	add	w0, w0, 6
	sub	x1, x29, #16384
	str	w0, [x1, 3784]
	sub	x0, x29, #16384
	ldrsw	x0, [x0, 3784]
	sub	x1, x29, #16384
	str	x0, [x1, 3928]
	sub	x0, x29, #16384
	ldrsw	x0, [x0, 3784]
	sub	x1, x29, #16384
	str	x0, [x1, 3520]
	sub	x0, x29, #16384
	str	xzr, [x0, 3528]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x1, x2, [x0, -64]
	mov	x0, x1
	lsr	x0, x0, 61
	mov	x3, x2
	lsl	x3, x3, 3
	sub	x4, x29, #16384
	str	x3, [x4, 3720]
	sub	x3, x29, #16384
	ldr	x3, [x3, 3720]
	orr	x0, x0, x3
	sub	x3, x29, #16384
	str	x0, [x3, 3720]
	mov	x0, x1
	lsl	x0, x0, 3
	sub	x1, x29, #16384
	str	x0, [x1, 3712]
	sub	x0, x29, #16384
	ldrsw	x0, [x0, 3784]
	sub	x1, x29, #16384
	str	x0, [x1, 3504]
	sub	x0, x29, #16384
	str	xzr, [x0, 3512]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x1, x2, [x0, -80]
	mov	x0, x1
	lsr	x0, x0, 61
	mov	x3, x2
	lsl	x3, x3, 3
	sub	x4, x29, #16384
	str	x3, [x4, 3704]
	sub	x3, x29, #16384
	ldr	x3, [x3, 3704]
	orr	x0, x0, x3
	sub	x3, x29, #16384
	str	x0, [x3, 3704]
	mov	x0, x1
	lsl	x0, x0, 3
	sub	x1, x29, #16384
	str	x0, [x1, 3696]
	sub	x0, x29, #16384
	ldrsw	x0, [x0, 3784]
	add	x0, x0, 15
	lsr	x0, x0, 4
	lsl	x1, x0, 4
	and	x3, x1, -4096
	sub	x0, sp, #12288
	sub	x2, x0, x3
L64:
	cmp	x0, x2
	beq	L65
	sub	x0, x0, #4096
LEHB6:
	str	xzr, [x0]
	b	L64
L65:
	sub	x0, x1, x3
	sub	x0, x2, x0
	str	xzr, [x0]
	sub	sp, sp, x1
	mov	x0, sp
	.loc 1 296 32 discriminator 6
	sub	x1, x29, #16384
	str	x0, [x1, 3936]
LBB33:
	sub	x0, x29, #16384
	ldr	x0, [x0, 3936]
	sub	x1, x29, #16384
	str	x0, [x1, 3152]
	mov	w0, 1
	sub	x1, x29, #16384
	str	w0, [x1, 3824]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3784]
	sub	x1, x29, #16384
	str	w0, [x1, 3828]
	sub	x0, x29, #12288
	sub	x0, x0, #272
	sub	x1, x29, #16384
	str	x0, [x1, 3160]
	adrp	x0, lC79@PAGE
	add	x0, x0, lC79@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3168]
	adrp	x0, lC14@PAGE
	add	x0, x0, lC14@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3176]
	sub	x0, x29, #16384
	ldr	x0, [x0, 3904]
	sub	x1, x29, #16384
	str	x0, [x1, 3184]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3772]
	sub	x1, x29, #16384
	str	w0, [x1, 3832]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3776]
	sub	x1, x29, #16384
	str	w0, [x1, 3836]
	sub	x0, x29, #12288
	sub	x0, x0, #264
	sub	x1, x29, #16384
	str	x0, [x1, 3192]
	sub	x0, x29, #12288
	sub	x0, x0, #1024
	ldp	x4, x5, [x0, 112]
	sub	x0, x29, #12288
	sub	x0, x0, #1024
	ldp	x2, x3, [x0, 96]
	sub	x0, x29, #12288
	sub	x0, x0, #1024
	ldp	x0, x1, [x0, 80]
	bl	_system__concat_2__str_concat_2
LBE33:
	.loc 1 296 13 is_stmt 1 discriminator 8
	sub	x0, x29, #16384
	ldr	x0, [x0, 3936]
	sub	x1, x29, #16384
	str	x0, [x1, 3200]
	mov	w0, 1
	sub	x1, x29, #16384
	str	w0, [x1, 3840]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3784]
	sub	x1, x29, #16384
	str	w0, [x1, 3844]
	sub	x0, x29, #12288
	sub	x0, x0, #256
	sub	x1, x29, #16384
	str	x0, [x1, 3208]
	sub	x0, x29, #12288
	sub	x0, x0, #1024
	ldp	x0, x1, [x0, 128]
	bl	_ada__text_io__put_line__2
LEHE6:
LEHB7:
LEHE7:
	.loc 1 296 0 discriminator 10
	mov	sp, x19
LBE32:
	.loc 1 297 13
	mov	w0, 1
LEHB8:
	bl	_ada__text_io__new_line__2
LBB34:
	.loc 1 298 13
	adrp	x0, lC80@PAGE
	add	x0, x0, lC80@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3216]
	adrp	x0, lC15@PAGE
	add	x0, x0, lC15@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3224]
	sub	x0, x29, #12288
	sub	x0, x0, #1024
	ldp	x0, x1, [x0, 144]
	bl	_ada__text_io__put_line__2
LBE34:
LBB35:
	.loc 1 299 13
	adrp	x0, lC81@PAGE
	add	x0, x0, lC81@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3232]
	adrp	x0, lC16@PAGE
	add	x0, x0, lC16@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3240]
	sub	x0, x29, #12288
	sub	x0, x0, #1024
	ldp	x0, x1, [x0, 160]
	bl	_ada__text_io__put_line__2
LBE35:
LBB36:
	.loc 1 300 13
	adrp	x0, lC82@PAGE
	add	x0, x0, lC82@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3248]
	adrp	x0, lC3@PAGE
	add	x0, x0, lC3@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3256]
	sub	x0, x29, #12288
	sub	x0, x0, #1024
	ldp	x0, x1, [x0, 176]
	bl	_ada__text_io__put_line__2
LBE36:
LBB37:
	.loc 1 301 13
	adrp	x0, lC83@PAGE
	add	x0, x0, lC83@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3264]
	adrp	x0, lC17@PAGE
	add	x0, x0, lC17@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3272]
	sub	x0, x29, #12288
	sub	x0, x0, #1024
	ldp	x0, x1, [x0, 192]
	bl	_ada__text_io__put_line__2
LBE37:
LBB38:
	.loc 1 302 13
	adrp	x0, lC84@PAGE
	add	x0, x0, lC84@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3280]
	adrp	x0, lC18@PAGE
	add	x0, x0, lC18@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3288]
	sub	x0, x29, #12288
	sub	x0, x0, #1024
	ldp	x0, x1, [x0, 208]
	bl	_ada__text_io__put_line__2
LBE38:
LBB39:
	.loc 1 303 13
	adrp	x0, lC85@PAGE
	add	x0, x0, lC85@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3296]
	adrp	x0, lC19@PAGE
	add	x0, x0, lC19@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3304]
	sub	x0, x29, #12288
	sub	x0, x0, #1024
	ldp	x0, x1, [x0, 224]
	bl	_ada__text_io__put_line__2
LBE39:
	.loc 1 304 13
	mov	w0, 1
	bl	_ada__text_io__new_line__2
	.loc 1 307 20
	sub	x0, x29, #12288
	sub	x0, x0, #80
	bl	_anubis_types__storage__zeroize_identity
LEHE8:
	.loc 1 308 0
	mov	w19, 2
L56:
	.loc 1 308 0 is_stmt 0 discriminator 1
	sub	x0, x29, #12288
	sub	x0, x0, #136
	mov	x16, x0
LEHB9:
	bl	_anubis_main__B_5__B_6___finalizer.4
LEHE9:
	.loc 1 308 0 discriminator 2
	cmp	w19, 1
	beq	L66
	cmp	w19, 2
	beq	L104
	mov	w0, 0
	b	L69
L104:
	.loc 1 308 13 is_stmt 1
	mov	w0, 2
L69:
LEHB10:
LEHE10:
	.loc 1 308 13 is_stmt 0 discriminator 3
	mov	sp, x20
	.loc 1 308 13 discriminator 1
	cmp	w0, 1
	beq	L70
	cmp	w0, 2
	beq	L105
	mov	w19, 0
	b	L73
L105:
LBE12:
	.loc 1 250 10 is_stmt 1
	b	L21
L34:
	.loc 1 309 21
	sub	x0, x29, #16384
	ldr	w1, [x0, 3768]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3764]
	sub	w0, w1, w0
	.loc 1 309 7
	cmp	w0, 6
	bne	L74
	.loc 1 309 21 discriminator 1
	mov	x3, x24
	mov	x2, 7
	adrp	x0, lC86@PAGE
	add	x1, x0, lC86@PAGEOFF;
	mov	x0, x3
	bl	_memcmp
	cmp	w0, 0
	bne	L74
LBB42:
	.loc 1 310 10
	adrp	x0, lC87@PAGE
	add	x0, x0, lC87@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3312]
	adrp	x0, lC19@PAGE
	add	x0, x0, lC19@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3320]
	sub	x0, x29, #12288
	sub	x0, x0, #1024
	ldp	x0, x1, [x0, 240]
LEHB11:
	bl	_ada__text_io__put_line__2
LBE42:
LBB43:
	.loc 1 311 10
	adrp	x0, lC88@PAGE
	add	x0, x0, lC88@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3328]
	adrp	x0, lC20@PAGE
	add	x0, x0, lC20@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3336]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, -256]
	bl	_ada__text_io__put_line__2
LBE43:
	b	L21
L74:
	.loc 1 312 21
	sub	x0, x29, #16384
	ldr	w1, [x0, 3768]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3764]
	sub	w0, w1, w0
	.loc 1 312 7
	cmp	w0, 6
	bne	L75
	.loc 1 312 21 discriminator 1
	mov	x3, x24
	mov	x2, 7
	adrp	x0, lC89@PAGE
	add	x1, x0, lC89@PAGEOFF;
	mov	x0, x3
	bl	_memcmp
	cmp	w0, 0
	bne	L75
LBB44:
	.loc 1 313 10
	adrp	x0, lC90@PAGE
	add	x0, x0, lC90@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3344]
	adrp	x0, lC19@PAGE
	add	x0, x0, lC19@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3352]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, -240]
	bl	_ada__text_io__put_line__2
LBE44:
LBB45:
	.loc 1 314 10
	adrp	x0, lC91@PAGE
	add	x0, x0, lC91@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3360]
	adrp	x0, lC20@PAGE
	add	x0, x0, lC20@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3368]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, -224]
	bl	_ada__text_io__put_line__2
LBE45:
	b	L21
L75:
	.loc 1 315 21
	sub	x0, x29, #16384
	ldr	w1, [x0, 3768]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3764]
	sub	w0, w1, w0
	.loc 1 315 7
	cmp	w0, 3
	bne	L76
	.loc 1 315 21 discriminator 1
	mov	x0, x24
	ldr	w1, [x0]
	.loc 1 315 21 is_stmt 0 discriminator 2
	mov	w0, 26995
	movk	w0, 0x6e67, lsl 16
	cmp	w1, w0
	bne	L76
LBB46:
	.loc 1 316 10 is_stmt 1
	adrp	x0, lC92@PAGE
	add	x0, x0, lC92@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3376]
	adrp	x0, lC21@PAGE
	add	x0, x0, lC21@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3384]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, -208]
	bl	_ada__text_io__put_line__2
LBE46:
LBB47:
	.loc 1 317 10
	adrp	x0, lC93@PAGE
	add	x0, x0, lC93@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3392]
	adrp	x0, lC16@PAGE
	add	x0, x0, lC16@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3400]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, -192]
	bl	_ada__text_io__put_line__2
LBE47:
	b	L21
L76:
	.loc 1 318 21
	sub	x0, x29, #16384
	ldr	w1, [x0, 3768]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3764]
	sub	w0, w1, w0
	.loc 1 318 7
	cmp	w0, 5
	bne	L77
	.loc 1 318 21 discriminator 1
	mov	x3, x24
	mov	x2, 6
	adrp	x0, lC94@PAGE
	add	x1, x0, lC94@PAGEOFF;
	mov	x0, x3
	bl	_memcmp
	cmp	w0, 0
	bne	L77
LBB48:
	.loc 1 319 10
	adrp	x0, lC95@PAGE
	add	x0, x0, lC95@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3408]
	adrp	x0, lC22@PAGE
	add	x0, x0, lC22@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3416]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, -176]
	bl	_ada__text_io__put_line__2
LEHE11:
LBE48:
	b	L21
L77:
LBB49:
	.loc 1 321 10
	mov	x0, sp
	mov	x19, x0
	.loc 1 321 40 discriminator 1
	sub	x0, x29, #16384
	ldr	w1, [x0, 3768]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3764]
	cmp	w1, w0
	blt	L78
	sub	x0, x29, #16384
	ldr	w1, [x0, 3768]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3764]
	sub	w0, w1, w0
	add	w0, w0, 1
	b	L79
L78:
	.loc 1 321 40 is_stmt 0 discriminator 2
	mov	w0, 0
L79:
	.loc 1 321 40 discriminator 4
	add	w0, w0, 17
	sub	x1, x29, #16384
	str	w0, [x1, 3788]
	sub	x0, x29, #16384
	ldrsw	x0, [x0, 3788]
	sub	x1, x29, #16384
	str	x0, [x1, 3944]
	sub	x0, x29, #16384
	ldrsw	x0, [x0, 3788]
	sub	x1, x29, #16384
	str	x0, [x1, 3488]
	sub	x0, x29, #16384
	str	xzr, [x0, 3496]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x1, x2, [x0, -96]
	mov	x0, x1
	lsr	x0, x0, 61
	mov	x3, x2
	lsl	x3, x3, 3
	sub	x4, x29, #16384
	str	x3, [x4, 3688]
	sub	x3, x29, #16384
	ldr	x3, [x3, 3688]
	orr	x0, x0, x3
	sub	x3, x29, #16384
	str	x0, [x3, 3688]
	mov	x0, x1
	lsl	x0, x0, 3
	sub	x1, x29, #16384
	str	x0, [x1, 3680]
	sub	x0, x29, #16384
	ldrsw	x0, [x0, 3788]
	mov	x26, x0
	mov	x27, 0
	lsr	x0, x26, 61
	lsl	x1, x27, 3
	sub	x2, x29, #16384
	str	x1, [x2, 3672]
	sub	x1, x29, #16384
	ldr	x1, [x1, 3672]
	orr	x0, x0, x1
	sub	x1, x29, #16384
	str	x0, [x1, 3672]
	lsl	x0, x26, 3
	sub	x1, x29, #16384
	str	x0, [x1, 3664]
	sub	x0, x29, #16384
	ldrsw	x0, [x0, 3788]
	add	x0, x0, 15
	lsr	x0, x0, 4
	lsl	x1, x0, 4
	and	x3, x1, -4096
	sub	x0, sp, #12288
	sub	x2, x0, x3
L80:
	cmp	x0, x2
	beq	L81
	sub	x0, x0, #4096
LEHB12:
	str	xzr, [x0]
	b	L80
L81:
	sub	x0, x1, x3
	sub	x0, x2, x0
	str	xzr, [x0]
	sub	sp, sp, x1
	mov	x0, sp
	.loc 1 321 40 discriminator 6
	sub	x1, x29, #16384
	str	x0, [x1, 3952]
LBB50:
	sub	x0, x29, #16384
	ldr	x0, [x0, 3952]
	sub	x1, x29, #16384
	str	x0, [x1, 3424]
	mov	w0, 1
	sub	x1, x29, #16384
	str	w0, [x1, 3848]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3788]
	sub	x1, x29, #16384
	str	w0, [x1, 3852]
	sub	x0, x29, #12288
	sub	x0, x0, #248
	sub	x1, x29, #16384
	str	x0, [x1, 3432]
	adrp	x0, lC96@PAGE
	add	x0, x0, lC96@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3440]
	adrp	x0, lC23@PAGE
	add	x0, x0, lC23@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3448]
	mov	x4, x24
	mov	x5, x25
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x2, x3, [x0, -144]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, -160]
	bl	_system__concat_2__str_concat_2
LBE50:
	.loc 1 321 10 is_stmt 1 discriminator 8
	sub	x0, x29, #16384
	ldr	x0, [x0, 3952]
	sub	x1, x29, #16384
	str	x0, [x1, 3456]
	mov	w0, 1
	sub	x1, x29, #16384
	str	w0, [x1, 3856]
	sub	x0, x29, #16384
	ldr	w0, [x0, 3788]
	sub	x1, x29, #16384
	str	w0, [x1, 3860]
	sub	x0, x29, #12288
	sub	x0, x0, #240
	sub	x1, x29, #16384
	str	x0, [x1, 3464]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, -128]
	bl	_ada__text_io__put_line__2
LEHE12:
LEHB13:
LEHE13:
	.loc 1 321 0 discriminator 10
	mov	sp, x19
LBE49:
LBB51:
	.loc 1 322 10
	adrp	x0, lC97@PAGE
	add	x0, x0, lC97@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3472]
	adrp	x0, lC24@PAGE
	add	x0, x0, lC24@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3480]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, -112]
LEHB14:
	bl	_ada__text_io__put_line__2
LEHE14:
L21:
LBE51:
	.loc 1 324 0
	mov	w19, 2
L73:
	.loc 1 324 0 is_stmt 0 discriminator 1
	sub	x0, x29, #12288
	sub	x0, x0, #136
	mov	x16, x0
LEHB15:
	bl	_anubis_main__B_5___finalizer.5
LEHE15:
	.loc 1 324 0 discriminator 2
	cmp	w19, 1
	beq	L82
	cmp	w19, 2
	beq	L106
	mov	w0, 0
	b	L85
L106:
	.loc 1 324 7 is_stmt 1
	mov	w0, 2
L85:
	.loc 1 324 7 is_stmt 0 discriminator 3
	cmp	w0, 1
	beq	L86
	cmp	w0, 2
	bne	L3
	.loc 1 324 7
	nop
LBE11:
	.loc 1 325 5 is_stmt 1
	b	L3
L98:
LEHB16:
LBB54:
LBB52:
LBB40:
	.loc 1 282 0 discriminator 9
	mov	sp, x19
	mov	x28, x0
	b	L88
L99:
LBE40:
LBB41:
	.loc 1 296 0 discriminator 9
	mov	sp, x19
	mov	x28, x0
	b	L88
L97:
	mov	x28, x0
L88:
	mov	w19, 1
LBE41:
	.loc 1 250 10
	b	L56
L66:
	sub	x0, x29, #16384
	str	x28, [x0, 3600]
	b	L90
L100:
	sub	x1, x29, #16384
	str	x0, [x1, 3600]
L90:
	mov	w0, 1
	b	L69
L70:
	sub	x0, x29, #16384
	ldr	x0, [x0, 3600]
	sub	x1, x29, #16384
	str	x0, [x1, 3608]
	b	L91
L101:
LBE52:
LBB53:
	.loc 1 321 0 discriminator 9
	mov	sp, x19
	sub	x1, x29, #16384
	str	x0, [x1, 3608]
	b	L91
L96:
	sub	x1, x29, #16384
	str	x0, [x1, 3608]
L91:
	mov	w19, 1
LBE53:
	.loc 1 239 4
	b	L73
L82:
	sub	x0, x29, #16384
	ldr	x0, [x0, 3608]
	sub	x1, x29, #16384
	str	x0, [x1, 2776]
	b	L93
L102:
	sub	x1, x29, #16384
	str	x0, [x1, 2776]
L93:
	mov	w0, 1
	b	L85
L86:
	sub	x0, x29, #16384
	ldr	x1, [x0, 2776]
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x3, [x29, -8]
	ldr	x2, [x0]
	subs	x3, x3, x2
	mov	x2, 0
	beq	L94
	bl	___stack_chk_fail
L94:
	mov	x0, x1
	bl	__Unwind_Resume
L3:
LBE54:
	.loc 1 325 5
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L95
	bl	___stack_chk_fail
L95:
LEHE16:
	mov	sp, x29
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
	ldp	x29, x30, [sp], 96
LCFI7:
	ret
LFE1:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table0:
LLSDA1:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 LLSDACSE1-LLSDACSB1
LLSDACSB1:
	.uleb128 LEHB0-LFB1
	.uleb128 LEHE0-LEHB0
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB1-LFB1
	.uleb128 LEHE1-LEHB1
	.uleb128 L96-LFB1
	.uleb128 0
	.uleb128 LEHB2-LFB1
	.uleb128 LEHE2-LEHB2
	.uleb128 L97-LFB1
	.uleb128 0
	.uleb128 LEHB3-LFB1
	.uleb128 LEHE3-LEHB3
	.uleb128 L98-LFB1
	.uleb128 0
	.uleb128 LEHB4-LFB1
	.uleb128 LEHE4-LEHB4
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB5-LFB1
	.uleb128 LEHE5-LEHB5
	.uleb128 L97-LFB1
	.uleb128 0
	.uleb128 LEHB6-LFB1
	.uleb128 LEHE6-LEHB6
	.uleb128 L99-LFB1
	.uleb128 0
	.uleb128 LEHB7-LFB1
	.uleb128 LEHE7-LEHB7
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB8-LFB1
	.uleb128 LEHE8-LEHB8
	.uleb128 L97-LFB1
	.uleb128 0
	.uleb128 LEHB9-LFB1
	.uleb128 LEHE9-LEHB9
	.uleb128 L100-LFB1
	.uleb128 0
	.uleb128 LEHB10-LFB1
	.uleb128 LEHE10-LEHB10
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB11-LFB1
	.uleb128 LEHE11-LEHB11
	.uleb128 L96-LFB1
	.uleb128 0
	.uleb128 LEHB12-LFB1
	.uleb128 LEHE12-LEHB12
	.uleb128 L101-LFB1
	.uleb128 0
	.uleb128 LEHB13-LFB1
	.uleb128 LEHE13-LEHB13
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB14-LFB1
	.uleb128 LEHE14-LEHB14
	.uleb128 L96-LFB1
	.uleb128 0
	.uleb128 LEHB15-LFB1
	.uleb128 LEHE15-LEHB15
	.uleb128 L102-LFB1
	.uleb128 0
	.uleb128 LEHB16-LFB1
	.uleb128 LEHE16-LEHB16
	.uleb128 0
	.uleb128 0
LLSDACSE1:
	.text
	.const
	.align	2
lC0:
	.word	1
	.word	12
	.align	2
lC1:
	.word	1
	.word	42
	.align	2
lC2:
	.word	1
	.word	153
	.align	2
lC3:
	.word	1
	.word	30
	.align	2
lC4:
	.word	1
	.word	10
	.align	2
lC5:
	.word	1
	.word	43
	.align	2
lC6:
	.word	1
	.word	3
	.align	2
lC7:
	.word	1
	.word	32
	.align	2
lC8:
	.word	1
	.word	44
	.align	2
lC9:
	.word	1
	.word	46
	.align	2
lC10:
	.word	1
	.word	19
	.align	2
lC11:
	.word	1
	.word	4
	.align	2
lC12:
	.word	1
	.word	39
	.align	2
lC13:
	.word	1
	.word	28
	.align	2
lC14:
	.word	1
	.word	6
	.align	2
lC15:
	.word	1
	.word	24
	.align	2
lC18:
	.word	1
	.word	56
	.align	2
lC19:
	.word	1
	.word	36
	.align	2
lC20:
	.word	1
	.word	59
	.align	2
lC21:
	.word	1
	.word	33
	.align	2
lC22:
	.word	1
	.word	35
	.align	2
lC23:
	.word	1
	.word	17
	.text
	.const
	.align	3
lC98:
	.ascii "Usage: anubis-spark <command> [options]"
	.align	3
lC99:
	.ascii "Commands:"
	.align	3
lC100:
	.ascii "  keygen           Generate new hybrid keypair"
	.align	3
lC101:
	.ascii "  encrypt          Encrypt file with hybrid PQ protection"
	.align	3
lC102:
	.ascii "  decrypt          Decrypt and verify file"
	.align	3
lC103:
	.ascii "  sign             Create hybrid signature"
	.align	3
lC104:
	.ascii "  verify           Verify hybrid signature"
	.align	3
lC105:
	.ascii "  test             Run cryptographic self-tests"
	.align	3
lC106:
	.ascii "  version          Show version and security info"
	.align	3
lC107:
	.ascii "  help             Show this help message"
	.align	3
lC108:
	.ascii "Examples:"
	.align	3
lC109:
	.ascii "  anubis-spark keygen --output my_identity.key"
	.align	3
lC110:
	.ascii "  anubis-spark encrypt --key alice.key --input secret.txt"
	.align	3
lC111:
	.ascii "  anubis-spark decrypt --key bob.key --input secret.txt.anubis"
	.align	3
lC112:
	.ascii "For detailed help on a command:"
	.align	3
lC113:
	.ascii "  anubis-spark <command> --help"
	.text
	.align	2
_anubis_main__print_usage.0:
LFB3:
	.loc 1 32 4
	sub	x10, sp, #16384
	str	xzr, [x10, 3808]
	stp	x29, x30, [sp, -288]!
LCFI8:
	mov	x29, sp
LCFI9:
	stp	x20, x21, [sp, 16]
	stp	x22, x23, [sp, 32]
	stp	x24, x25, [sp, 48]
	stp	x26, x27, [sp, 64]
LCFI10:
	mov	x0, x16
	str	x16, [x29, 280]
	.loc 1 34 7
	mov	x16, x0
	bl	_anubis_main__print_banner.3
LBB55:
	.loc 1 35 7
	adrp	x0, lC98@PAGE
	add	x20, x0, lC98@PAGEOFF;
	adrp	x0, lC12@PAGE
	add	x21, x0, lC12@PAGEOFF;
	mov	x0, x20
	mov	x1, x21
	bl	_ada__text_io__put_line__2
LBE55:
	.loc 1 36 7
	mov	w0, 1
	bl	_ada__text_io__new_line__2
LBB56:
	.loc 1 37 7
	adrp	x0, lC99@PAGE
	add	x22, x0, lC99@PAGEOFF;
	adrp	x0, lC41@PAGE
	add	x23, x0, lC41@PAGEOFF;
	mov	x0, x22
	mov	x1, x23
	bl	_ada__text_io__put_line__2
LBE56:
LBB57:
	.loc 1 38 7
	adrp	x0, lC100@PAGE
	add	x24, x0, lC100@PAGEOFF;
	adrp	x0, lC9@PAGE
	add	x25, x0, lC9@PAGEOFF;
	mov	x0, x24
	mov	x1, x25
	bl	_ada__text_io__put_line__2
LBE57:
LBB58:
	.loc 1 39 7
	adrp	x0, lC101@PAGE
	add	x0, x0, lC101@PAGEOFF;
	str	x0, [x29, 80]
	adrp	x0, lC42@PAGE
	add	x0, x0, lC42@PAGEOFF;
	str	x0, [x29, 88]
	ldp	x0, x1, [x29, 80]
	bl	_ada__text_io__put_line__2
LBE58:
LBB59:
	.loc 1 40 7
	adrp	x0, lC102@PAGE
	add	x0, x0, lC102@PAGEOFF;
	str	x0, [x29, 96]
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	str	x0, [x29, 104]
	ldp	x0, x1, [x29, 96]
	bl	_ada__text_io__put_line__2
LBE59:
LBB60:
	.loc 1 41 7
	adrp	x0, lC103@PAGE
	add	x0, x0, lC103@PAGEOFF;
	str	x0, [x29, 112]
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	str	x0, [x29, 120]
	ldp	x0, x1, [x29, 112]
	bl	_ada__text_io__put_line__2
LBE60:
LBB61:
	.loc 1 42 7
	adrp	x0, lC104@PAGE
	add	x0, x0, lC104@PAGEOFF;
	str	x0, [x29, 128]
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	str	x0, [x29, 136]
	ldp	x0, x1, [x29, 128]
	bl	_ada__text_io__put_line__2
LBE61:
LBB62:
	.loc 1 43 7
	adrp	x0, lC105@PAGE
	add	x0, x0, lC105@PAGEOFF;
	str	x0, [x29, 144]
	adrp	x0, lC43@PAGE
	add	x0, x0, lC43@PAGEOFF;
	str	x0, [x29, 152]
	ldp	x0, x1, [x29, 144]
	bl	_ada__text_io__put_line__2
LBE62:
LBB63:
	.loc 1 44 7
	adrp	x0, lC106@PAGE
	add	x0, x0, lC106@PAGEOFF;
	str	x0, [x29, 160]
	adrp	x0, lC35@PAGE
	add	x0, x0, lC35@PAGEOFF;
	str	x0, [x29, 168]
	ldp	x0, x1, [x29, 160]
	bl	_ada__text_io__put_line__2
LBE63:
LBB64:
	.loc 1 45 7
	adrp	x0, lC107@PAGE
	add	x0, x0, lC107@PAGEOFF;
	str	x0, [x29, 176]
	adrp	x0, lC36@PAGE
	add	x0, x0, lC36@PAGEOFF;
	str	x0, [x29, 184]
	ldp	x0, x1, [x29, 176]
	bl	_ada__text_io__put_line__2
LBE64:
	.loc 1 46 7
	mov	w0, 1
	bl	_ada__text_io__new_line__2
LBB65:
	.loc 1 47 7
	adrp	x0, lC108@PAGE
	add	x0, x0, lC108@PAGEOFF;
	str	x0, [x29, 192]
	adrp	x0, lC41@PAGE
	add	x0, x0, lC41@PAGEOFF;
	str	x0, [x29, 200]
	ldp	x0, x1, [x29, 192]
	bl	_ada__text_io__put_line__2
LBE65:
LBB66:
	.loc 1 48 7
	adrp	x0, lC109@PAGE
	add	x0, x0, lC109@PAGEOFF;
	str	x0, [x29, 208]
	adrp	x0, lC9@PAGE
	add	x0, x0, lC9@PAGEOFF;
	str	x0, [x29, 216]
	ldp	x0, x1, [x29, 208]
	bl	_ada__text_io__put_line__2
LBE66:
LBB67:
	.loc 1 49 7
	adrp	x0, lC110@PAGE
	add	x0, x0, lC110@PAGEOFF;
	str	x0, [x29, 224]
	adrp	x0, lC42@PAGE
	add	x0, x0, lC42@PAGEOFF;
	str	x0, [x29, 232]
	ldp	x0, x1, [x29, 224]
	bl	_ada__text_io__put_line__2
LBE67:
LBB68:
	.loc 1 50 7
	adrp	x0, lC111@PAGE
	add	x0, x0, lC111@PAGEOFF;
	str	x0, [x29, 240]
	adrp	x0, lC44@PAGE
	add	x0, x0, lC44@PAGEOFF;
	str	x0, [x29, 248]
	ldp	x0, x1, [x29, 240]
	bl	_ada__text_io__put_line__2
LBE68:
	.loc 1 51 7
	mov	w0, 1
	bl	_ada__text_io__new_line__2
LBB69:
	.loc 1 52 7
	adrp	x0, lC112@PAGE
	add	x0, x0, lC112@PAGEOFF;
	str	x0, [x29, 256]
	adrp	x0, lC37@PAGE
	add	x0, x0, lC37@PAGEOFF;
	str	x0, [x29, 264]
	ldp	x0, x1, [x29, 256]
	bl	_ada__text_io__put_line__2
LBE69:
LBB70:
	.loc 1 53 7
	adrp	x0, lC113@PAGE
	add	x26, x0, lC113@PAGEOFF;
	adrp	x0, lC37@PAGE
	add	x27, x0, lC37@PAGEOFF;
	mov	x0, x26
	mov	x1, x27
	bl	_ada__text_io__put_line__2
LBE70:
	.loc 1 54 7
	mov	w0, 1
	bl	_ada__text_io__new_line__2
	.loc 1 55 8
	nop
	ldp	x20, x21, [sp, 16]
	ldp	x22, x23, [sp, 32]
	ldp	x24, x25, [sp, 48]
	ldp	x26, x27, [sp, 64]
	ldp	x29, x30, [sp], 288
LCFI11:
	ret
LFE3:
	.const
	.align	2
lC41:
	.word	1
	.word	9
	.align	2
lC42:
	.word	1
	.word	57
	.align	2
lC43:
	.word	1
	.word	47
	.align	2
lC35:
	.word	1
	.word	49
	.align	2
lC36:
	.word	1
	.word	41
	.align	2
lC44:
	.word	1
	.word	62
	.align	2
lC37:
	.word	1
	.word	31
	.text
	.const
	.align	3
lC114:
	.ascii "Version Information:"
	.align	3
lC115:
	.ascii "  Anubis-SPARK:  0.2.0 (Phase 2 - Hybrid Operations)"
	.align	3
lC116:
	.ascii "  liboqs:        0.14.0"
	.align	3
lC117:
	.ascii "  libsodium:     1.0.20"
	.align	3
lC118:
	.ascii "  SPARK:         2024"
	.align	3
lC119:
	.ascii "  GNAT:          14.2.1"
	.align	3
lC120:
	.ascii "Cryptographic Algorithms:"
	.align	3
lC121:
	.ascii "  ML-KEM-1024:   NIST FIPS 203 (Post-Quantum KEM)"
	.align	3
lC122:
	.ascii "  ML-DSA-87:     NIST FIPS 204 (Post-Quantum Signatures)"
	.align	3
lC123:
	.ascii "  X25519:        RFC 7748 (ECDH)"
	.align	3
lC124:
	.ascii "  Ed25519:       RFC 8032 (EdDSA)"
	.align	3
lC125:
	.ascii "  XChaCha20:     RFC 8439 (Stream Cipher)"
	.align	3
lC126:
	.ascii "  Poly1305:      RFC 8439 (MAC)"
	.align	3
lC127:
	.ascii "  Argon2id:      RFC 9106 (Password Hashing)"
	.align	3
lC128:
	.ascii "  HKDF-SHA256:   RFC 5869 (Key Derivation)"
	.align	3
lC129:
	.ascii "SPARK Verification Status:"
	.align	3
lC130:
	.ascii "  Stone:   \342\234\223 Valid SPARK subset"
	.align	3
lC131:
	.ascii "  Bronze:  \342\234\223 Flow analysis (no uninitialized vars)"
	.align	3
lC132:
	.ascii "  Silver:  \342\234\223 Absence of Runtime Errors"
	.align	3
lC133:
	.ascii "  Gold:    \342\234\223 Integrity properties (31/31 proofs)"
	.align	3
lC134:
	.ascii "  Platinum: \342\217\263 In progress (functional correctness)"
	.text
	.align	2
_anubis_main__print_version.1:
LFB4:
	.loc 1 57 4
	sub	x10, sp, #16384
	str	xzr, [x10, 3728]
	stp	x29, x30, [sp, -368]!
LCFI12:
	mov	x29, sp
LCFI13:
	stp	x20, x21, [sp, 16]
	stp	x22, x23, [sp, 32]
	stp	x24, x25, [sp, 48]
	stp	x26, x27, [sp, 64]
LCFI14:
	mov	x0, x16
	str	x16, [x29, 360]
	.loc 1 59 7
	mov	x16, x0
	bl	_anubis_main__print_banner.3
LBB71:
	.loc 1 60 7
	adrp	x0, lC114@PAGE
	add	x20, x0, lC114@PAGEOFF;
	adrp	x0, lC31@PAGE
	add	x21, x0, lC31@PAGEOFF;
	mov	x0, x20
	mov	x1, x21
	bl	_ada__text_io__put_line__2
LBE71:
LBB72:
	.loc 1 61 7
	adrp	x0, lC115@PAGE
	add	x22, x0, lC115@PAGEOFF;
	adrp	x0, lC32@PAGE
	add	x23, x0, lC32@PAGEOFF;
	mov	x0, x22
	mov	x1, x23
	bl	_ada__text_io__put_line__2
LBE72:
LBB73:
	.loc 1 62 7
	adrp	x0, lC116@PAGE
	add	x24, x0, lC116@PAGEOFF;
	adrp	x0, lC33@PAGE
	add	x25, x0, lC33@PAGEOFF;
	mov	x0, x24
	mov	x1, x25
	bl	_ada__text_io__put_line__2
LBE73:
LBB74:
	.loc 1 63 7
	adrp	x0, lC117@PAGE
	add	x26, x0, lC117@PAGEOFF;
	adrp	x0, lC33@PAGE
	add	x27, x0, lC33@PAGEOFF;
	mov	x0, x26
	mov	x1, x27
	bl	_ada__text_io__put_line__2
LBE74:
LBB75:
	.loc 1 64 7
	adrp	x0, lC118@PAGE
	add	x0, x0, lC118@PAGEOFF;
	str	x0, [x29, 80]
	adrp	x0, lC34@PAGE
	add	x0, x0, lC34@PAGEOFF;
	str	x0, [x29, 88]
	ldp	x0, x1, [x29, 80]
	bl	_ada__text_io__put_line__2
LBE75:
LBB76:
	.loc 1 65 7
	adrp	x0, lC119@PAGE
	add	x0, x0, lC119@PAGEOFF;
	str	x0, [x29, 96]
	adrp	x0, lC33@PAGE
	add	x0, x0, lC33@PAGEOFF;
	str	x0, [x29, 104]
	ldp	x0, x1, [x29, 96]
	bl	_ada__text_io__put_line__2
LBE76:
	.loc 1 66 7
	mov	w0, 1
	bl	_ada__text_io__new_line__2
LBB77:
	.loc 1 67 7
	adrp	x0, lC120@PAGE
	add	x0, x0, lC120@PAGEOFF;
	str	x0, [x29, 112]
	adrp	x0, lC29@PAGE
	add	x0, x0, lC29@PAGEOFF;
	str	x0, [x29, 120]
	ldp	x0, x1, [x29, 112]
	bl	_ada__text_io__put_line__2
LBE77:
LBB78:
	.loc 1 68 7
	adrp	x0, lC121@PAGE
	add	x0, x0, lC121@PAGEOFF;
	str	x0, [x29, 128]
	adrp	x0, lC35@PAGE
	add	x0, x0, lC35@PAGEOFF;
	str	x0, [x29, 136]
	ldp	x0, x1, [x29, 128]
	bl	_ada__text_io__put_line__2
LBE78:
LBB79:
	.loc 1 69 7
	adrp	x0, lC122@PAGE
	add	x0, x0, lC122@PAGEOFF;
	str	x0, [x29, 144]
	adrp	x0, lC18@PAGE
	add	x0, x0, lC18@PAGEOFF;
	str	x0, [x29, 152]
	ldp	x0, x1, [x29, 144]
	bl	_ada__text_io__put_line__2
LBE79:
LBB80:
	.loc 1 70 7
	adrp	x0, lC123@PAGE
	add	x0, x0, lC123@PAGEOFF;
	str	x0, [x29, 160]
	adrp	x0, lC7@PAGE
	add	x0, x0, lC7@PAGEOFF;
	str	x0, [x29, 168]
	ldp	x0, x1, [x29, 160]
	bl	_ada__text_io__put_line__2
LBE80:
LBB81:
	.loc 1 71 7
	adrp	x0, lC124@PAGE
	add	x0, x0, lC124@PAGEOFF;
	str	x0, [x29, 176]
	adrp	x0, lC21@PAGE
	add	x0, x0, lC21@PAGEOFF;
	str	x0, [x29, 184]
	ldp	x0, x1, [x29, 176]
	bl	_ada__text_io__put_line__2
LBE81:
LBB82:
	.loc 1 72 7
	adrp	x0, lC125@PAGE
	add	x0, x0, lC125@PAGEOFF;
	str	x0, [x29, 192]
	adrp	x0, lC36@PAGE
	add	x0, x0, lC36@PAGEOFF;
	str	x0, [x29, 200]
	ldp	x0, x1, [x29, 192]
	bl	_ada__text_io__put_line__2
LBE82:
LBB83:
	.loc 1 73 7
	adrp	x0, lC126@PAGE
	add	x0, x0, lC126@PAGEOFF;
	str	x0, [x29, 208]
	adrp	x0, lC37@PAGE
	add	x0, x0, lC37@PAGEOFF;
	str	x0, [x29, 216]
	ldp	x0, x1, [x29, 208]
	bl	_ada__text_io__put_line__2
LBE83:
LBB84:
	.loc 1 74 7
	adrp	x0, lC127@PAGE
	add	x0, x0, lC127@PAGEOFF;
	str	x0, [x29, 224]
	adrp	x0, lC8@PAGE
	add	x0, x0, lC8@PAGEOFF;
	str	x0, [x29, 232]
	ldp	x0, x1, [x29, 224]
	bl	_ada__text_io__put_line__2
LBE84:
LBB85:
	.loc 1 75 7
	adrp	x0, lC128@PAGE
	add	x0, x0, lC128@PAGEOFF;
	str	x0, [x29, 240]
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	str	x0, [x29, 248]
	ldp	x0, x1, [x29, 240]
	bl	_ada__text_io__put_line__2
LBE85:
	.loc 1 76 7
	mov	w0, 1
	bl	_ada__text_io__new_line__2
LBB86:
	.loc 1 77 7
	adrp	x0, lC129@PAGE
	add	x0, x0, lC129@PAGEOFF;
	str	x0, [x29, 256]
	adrp	x0, lC38@PAGE
	add	x0, x0, lC38@PAGEOFF;
	str	x0, [x29, 264]
	ldp	x0, x1, [x29, 256]
	bl	_ada__text_io__put_line__2
LBE86:
LBB87:
	.loc 1 78 7
	adrp	x0, lC130@PAGE
	add	x0, x0, lC130@PAGEOFF;
	str	x0, [x29, 272]
	adrp	x0, lC21@PAGE
	add	x0, x0, lC21@PAGEOFF;
	str	x0, [x29, 280]
	ldp	x0, x1, [x29, 272]
	bl	_ada__text_io__put_line__2
LBE87:
LBB88:
	.loc 1 79 7
	adrp	x0, lC131@PAGE
	add	x0, x0, lC131@PAGEOFF;
	str	x0, [x29, 288]
	adrp	x0, lC32@PAGE
	add	x0, x0, lC32@PAGEOFF;
	str	x0, [x29, 296]
	ldp	x0, x1, [x29, 288]
	bl	_ada__text_io__put_line__2
LBE88:
LBB89:
	.loc 1 80 7
	adrp	x0, lC132@PAGE
	add	x0, x0, lC132@PAGEOFF;
	str	x0, [x29, 304]
	adrp	x0, lC39@PAGE
	add	x0, x0, lC39@PAGEOFF;
	str	x0, [x29, 312]
	ldp	x0, x1, [x29, 304]
	bl	_ada__text_io__put_line__2
LBE89:
LBB90:
	.loc 1 81 7
	adrp	x0, lC133@PAGE
	add	x0, x0, lC133@PAGEOFF;
	str	x0, [x29, 320]
	adrp	x0, lC40@PAGE
	add	x0, x0, lC40@PAGEOFF;
	str	x0, [x29, 328]
	ldp	x0, x1, [x29, 320]
	bl	_ada__text_io__put_line__2
LBE90:
LBB91:
	.loc 1 82 7
	adrp	x0, lC134@PAGE
	add	x0, x0, lC134@PAGEOFF;
	str	x0, [x29, 336]
	adrp	x0, lC32@PAGE
	add	x0, x0, lC32@PAGEOFF;
	str	x0, [x29, 344]
	ldp	x0, x1, [x29, 336]
	bl	_ada__text_io__put_line__2
LBE91:
	.loc 1 83 7
	mov	w0, 1
	bl	_ada__text_io__new_line__2
	.loc 1 84 8
	nop
	ldp	x20, x21, [sp, 16]
	ldp	x22, x23, [sp, 32]
	ldp	x24, x25, [sp, 48]
	ldp	x26, x27, [sp, 64]
	ldp	x29, x30, [sp], 368
LCFI15:
	ret
LFE4:
	.const
	.align	2
lC31:
	.word	1
	.word	20
	.align	2
lC32:
	.word	1
	.word	52
	.align	2
lC33:
	.word	1
	.word	23
	.align	2
lC34:
	.word	1
	.word	21
	.align	2
lC29:
	.word	1
	.word	25
	.align	2
lC38:
	.word	1
	.word	26
	.align	2
lC40:
	.word	1
	.word	50
	.text
	.const
	.align	3
lC135:
	.ascii "Running Cryptographic Self-Tests..."
	.align	3
lC136:
	.ascii "1. ML-KEM-1024 Key Generation... "
	.align	3
lC137:
	.ascii "\342\234\223 PASS"
	.align	3
lC138:
	.ascii "\342\234\227 FAIL"
	.align	3
lC139:
	.ascii "2. ML-KEM-1024 Encap/Decap... "
	.align	3
lC140:
	.ascii "\342\234\227 FAIL (keygen)"
	.align	3
lC141:
	.ascii "\342\234\227 FAIL (encap)"
	.align	3
lC142:
	.ascii "\342\234\227 FAIL (decap)"
	.align	3
lC143:
	.ascii "\342\234\227 FAIL (mismatch)"
	.align	3
lC144:
	.ascii "3. ML-DSA-87 Sign/Verify... "
	.align	3
lC145:
	.ascii "\342\234\227 FAIL (sign)"
	.align	3
lC146:
	.ascii "\342\234\227 FAIL (verify)"
	.align	3
lC147:
	.ascii "4. Hybrid Signatures (Ed25519 + ML-DSA)... "
	.align	3
lC148:
	.ascii "\342\234\227 FAIL (Ed25519 keygen)"
	.align	3
lC149:
	.ascii "\342\234\227 FAIL (ML-DSA keygen)"
	.align	3
lC150:
	.ascii "All Self-Tests Passed Successfully!"
	.align	3
lC151:
	.ascii "System is functioning correctly."
	.text
	.align	2
_anubis_main__run_self_test.2:
LFB5:
	.loc 1 86 4
	sub	x10, sp, #16384
	str	xzr, [x10]
	sub	x10, x10, #4096
	str	xzr, [x10]
	sub	x10, x10, #4096
	str	xzr, [x10]
	sub	x10, x10, #4096
	str	xzr, [x10, 3600]
	stp	x29, x30, [sp, -80]!
LCFI16:
	mov	x29, sp
LCFI17:
	stp	x20, x21, [sp, 16]
	stp	x22, x23, [sp, 32]
	stp	x24, x25, [sp, 48]
	stp	x26, x27, [sp, 64]
	mov	x13, 12704
	sub	sp, sp, x13
LCFI18:
	mov	x1, x16
	sub	x0, x29, #16384
	str	x16, [x0, 4088]
	.loc 1 86 4
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x0]
	str	x2, [x29, -8]
	mov	x2, 0
	.loc 1 89 7
	mov	x16, x1
	bl	_anubis_main__print_banner.3
LBB92:
	.loc 1 90 7
	adrp	x0, lC135@PAGE
	add	x20, x0, lC135@PAGEOFF;
	adrp	x0, lC22@PAGE
	add	x21, x0, lC22@PAGEOFF;
	mov	x0, x20
	mov	x1, x21
	bl	_ada__text_io__put_line__2
LBE92:
LBB93:
	.loc 1 91 7
	adrp	x0, lC64@PAGE
	add	x22, x0, lC64@PAGEOFF;
	adrp	x0, lC2@PAGE
	add	x23, x0, lC2@PAGEOFF;
	mov	x0, x22
	mov	x1, x23
	bl	_ada__text_io__put_line__2
LBE93:
	.loc 1 92 7
	mov	w0, 1
	bl	_ada__text_io__new_line__2
LBB94:
	.loc 1 95 7
	adrp	x0, lC136@PAGE
	add	x24, x0, lC136@PAGEOFF;
	adrp	x0, lC21@PAGE
	add	x25, x0, lC21@PAGEOFF;
	mov	x0, x24
	mov	x1, x25
	bl	_ada__text_io__put__4
LBE94:
LBB95:
	.loc 1 98 10
	sub	x0, x29, #4096
	sub	x0, x0, #816
	bl	_anubis_types__ml_kem_secret_keyIP
	.loc 1 100 13
	sub	x1, x29, #4096
	sub	x1, x1, #816
	sub	x0, x29, #8192
	sub	x0, x0, #1416
	bl	_anubis_types__pqc__ml_kem_generate_keypair
	.loc 1 100 13 is_stmt 0 discriminator 1
	sub	x1, x29, #12288
	strb	w0, [x1, 5]
	.loc 1 101 10 is_stmt 1
	sub	x0, x29, #12288
	ldrb	w0, [x0, 5]
	cmp	w0, 0
	beq	L112
	.loc 1 101 30 discriminator 1
	sub	x0, x29, #4096
	sub	x0, x0, #816
	bl	_anubis_types__is_valid__6
	.loc 1 101 21 discriminator 2
	cmp	w0, 0
	beq	L112
LBB96:
	.loc 1 102 13
	adrp	x0, lC137@PAGE
	add	x0, x0, lC137@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3680]
	adrp	x0, lC25@PAGE
	add	x0, x0, lC25@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3688]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 96]
	bl	_ada__text_io__put_line__2
LBE96:
	.loc 1 103 16
	sub	x0, x29, #4096
	sub	x0, x0, #816
	bl	_anubis_types__pqc__zeroize_ml_kem_secret
LBE95:
LBB98:
	.loc 1 111 7
	adrp	x0, lC139@PAGE
	add	x0, x0, lC139@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3696]
	adrp	x0, lC3@PAGE
	add	x0, x0, lC3@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3704]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 112]
	bl	_ada__text_io__put__4
LBE98:
LBB99:
	.loc 1 114 10
	sub	x0, x29, #4096
	sub	x0, x0, #816
	bl	_anubis_types__ml_kem_secret_keyIP
	.loc 1 116 10
	sub	x0, x29, #8192
	sub	x0, x0, #4088
	bl	_anubis_types__ml_kem_shared_secretIP
	.loc 1 117 10
	sub	x0, x29, #8192
	sub	x0, x0, #4048
	bl	_anubis_types__ml_kem_shared_secretIP
	.loc 1 119 13
	sub	x1, x29, #4096
	sub	x1, x1, #816
	sub	x0, x29, #8192
	sub	x0, x0, #4008
	bl	_anubis_types__pqc__ml_kem_generate_keypair
	.loc 1 119 13 is_stmt 0 discriminator 1
	sub	x1, x29, #12288
	strb	w0, [x1, 5]
	.loc 1 120 13 is_stmt 1
	sub	x0, x29, #12288
	ldrb	w0, [x0, 5]
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 120 10
	cmp	w0, 0
	beq	L115
	b	L136
L112:
LBE99:
LBB105:
LBB97:
	.loc 1 105 13
	adrp	x0, lC138@PAGE
	add	x26, x0, lC138@PAGEOFF;
	adrp	x0, lC25@PAGE
	add	x27, x0, lC25@PAGEOFF;
	mov	x0, x26
	mov	x1, x27
	bl	_ada__text_io__put_line__2
	b	L111
L136:
LBE97:
LBE105:
LBB106:
LBB100:
	.loc 1 121 13
	adrp	x0, lC140@PAGE
	add	x0, x0, lC140@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3712]
	adrp	x0, lC23@PAGE
	add	x0, x0, lC23@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3720]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 128]
	bl	_ada__text_io__put_line__2
LBE100:
	.loc 1 122 13
	b	L111
L115:
	.loc 1 125 13
	sub	x2, x29, #8192
	sub	x2, x2, #4088
	sub	x1, x29, #8192
	sub	x1, x1, #1416
	sub	x0, x29, #8192
	sub	x0, x0, #4008
	bl	_anubis_types__pqc__ml_kem_encapsulate
	.loc 1 125 13 is_stmt 0 discriminator 1
	sub	x1, x29, #12288
	strb	w0, [x1, 5]
	.loc 1 126 13 is_stmt 1
	sub	x0, x29, #12288
	ldrb	w0, [x0, 5]
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 126 10
	cmp	w0, 0
	beq	L117
LBB101:
	.loc 1 127 13
	adrp	x0, lC141@PAGE
	add	x0, x0, lC141@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3728]
	adrp	x0, lC26@PAGE
	add	x0, x0, lC26@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3736]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 144]
	bl	_ada__text_io__put_line__2
LBE101:
	.loc 1 128 16
	sub	x0, x29, #4096
	sub	x0, x0, #816
	bl	_anubis_types__pqc__zeroize_ml_kem_secret
	.loc 1 129 13
	b	L111
L117:
	.loc 1 132 13
	sub	x2, x29, #8192
	sub	x2, x2, #4048
	sub	x1, x29, #4096
	sub	x1, x1, #816
	sub	x0, x29, #8192
	sub	x0, x0, #1416
	bl	_anubis_types__pqc__ml_kem_decapsulate
	.loc 1 132 13 is_stmt 0 discriminator 1
	sub	x1, x29, #12288
	strb	w0, [x1, 5]
	.loc 1 133 13 is_stmt 1
	sub	x0, x29, #12288
	ldrb	w0, [x0, 5]
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 133 10
	cmp	w0, 0
	beq	L118
LBB102:
	.loc 1 134 13
	adrp	x0, lC142@PAGE
	add	x0, x0, lC142@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3744]
	adrp	x0, lC26@PAGE
	add	x0, x0, lC26@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3752]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 160]
	bl	_ada__text_io__put_line__2
LBE102:
	.loc 1 135 16
	sub	x0, x29, #4096
	sub	x0, x0, #816
	bl	_anubis_types__pqc__zeroize_ml_kem_secret
	.loc 1 136 16
	sub	x0, x29, #8192
	sub	x0, x0, #4088
	bl	_anubis_types__pqc__zeroize_shared_secret
	.loc 1 137 13
	b	L111
L118:
	.loc 1 140 16
	sub	x1, x29, #8192
	sub	x1, x1, #4048
	sub	x0, x29, #8192
	sub	x0, x0, #4088
	bl	_anubis_types__pqc__secrets_match
	.loc 1 140 10 discriminator 1
	cmp	w0, 0
	beq	L119
LBB103:
	.loc 1 141 13
	adrp	x0, lC137@PAGE
	add	x0, x0, lC137@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3760]
	adrp	x0, lC25@PAGE
	add	x0, x0, lC25@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3768]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 176]
	bl	_ada__text_io__put_line__2
	b	L120
L119:
LBE103:
LBB104:
	.loc 1 143 13
	adrp	x0, lC143@PAGE
	add	x0, x0, lC143@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3776]
	adrp	x0, lC10@PAGE
	add	x0, x0, lC10@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3784]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 192]
	bl	_ada__text_io__put_line__2
L120:
LBE104:
	.loc 1 146 13
	sub	x0, x29, #4096
	sub	x0, x0, #816
	bl	_anubis_types__pqc__zeroize_ml_kem_secret
	.loc 1 147 13
	sub	x0, x29, #8192
	sub	x0, x0, #4088
	bl	_anubis_types__pqc__zeroize_shared_secret
	.loc 1 148 13
	sub	x0, x29, #8192
	sub	x0, x0, #4048
	bl	_anubis_types__pqc__zeroize_shared_secret
LBE106:
LBB107:
	.loc 1 152 7
	adrp	x0, lC144@PAGE
	add	x0, x0, lC144@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3792]
	adrp	x0, lC13@PAGE
	add	x0, x0, lC13@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3800]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 208]
	bl	_ada__text_io__put__4
LBE107:
LBB108:
	.loc 1 155 10
	sub	x0, x29, #4096
	sub	x0, x0, #816
	bl	_anubis_types__ml_dsa_secret_keyIP
	.loc 1 160 13
	sub	x1, x29, #4096
	sub	x1, x1, #816
	sub	x0, x29, #8192
	sub	x0, x0, #4008
	bl	_anubis_types__pqc__ml_dsa_generate_keypair
	.loc 1 160 13 is_stmt 0 discriminator 1
	sub	x1, x29, #12288
	strb	w0, [x1, 5]
	.loc 1 161 13 is_stmt 1
	sub	x0, x29, #12288
	ldrb	w0, [x0, 5]
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 161 10
	cmp	w0, 0
	beq	L122
LBB109:
	.loc 1 162 13
	adrp	x0, lC140@PAGE
	add	x0, x0, lC140@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3808]
	adrp	x0, lC23@PAGE
	add	x0, x0, lC23@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3816]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 224]
	bl	_ada__text_io__put_line__2
LBE109:
	.loc 1 163 13
	b	L111
L122:
	.loc 1 166 13
	adrp	x0, _msg.7@PAGE
	add	x0, x0, _msg.7@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3824]
	adrp	x0, lC27@PAGE
	add	x0, x0, lC27@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3832]
	sub	x1, x29, #8192
	sub	x1, x1, #1416
	sub	x0, x29, #4096
	sub	x0, x0, #816
	mov	x3, x1
	mov	x2, x0
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 240]
	bl	_anubis_types__pqc__ml_dsa_sign
	.loc 1 166 13 is_stmt 0 discriminator 1
	sub	x1, x29, #12288
	strb	w0, [x1, 5]
	.loc 1 167 13 is_stmt 1
	sub	x0, x29, #12288
	ldrb	w0, [x0, 5]
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 167 10
	cmp	w0, 0
	beq	L124
LBB110:
	.loc 1 168 13
	adrp	x0, lC145@PAGE
	add	x0, x0, lC145@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3840]
	adrp	x0, lC28@PAGE
	add	x0, x0, lC28@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3848]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -256]
	bl	_ada__text_io__put_line__2
LBE110:
	.loc 1 169 16
	sub	x0, x29, #4096
	sub	x0, x0, #816
	bl	_anubis_types__pqc__zeroize_ml_dsa_secret
	.loc 1 170 13
	b	L111
L124:
	.loc 1 173 22
	adrp	x0, _msg.7@PAGE
	add	x0, x0, _msg.7@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3856]
	adrp	x0, lC27@PAGE
	add	x0, x0, lC27@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3864]
	sub	x0, x29, #8192
	sub	x0, x0, #4008
	sub	x1, x29, #8192
	sub	x1, x1, #1416
	mov	x3, x0
	mov	x2, x1
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -240]
	bl	_anubis_types__pqc__ml_dsa_verify
	.loc 1 173 22 is_stmt 0 discriminator 1
	sub	x1, x29, #12288
	strb	w0, [x1, 6]
	.loc 1 174 10 is_stmt 1
	sub	x0, x29, #12288
	ldrb	w0, [x0, 6]
	cmp	w0, 0
	beq	L125
LBB111:
	.loc 1 175 13
	adrp	x0, lC137@PAGE
	add	x0, x0, lC137@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3872]
	adrp	x0, lC25@PAGE
	add	x0, x0, lC25@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3880]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -224]
	bl	_ada__text_io__put_line__2
	b	L126
L125:
LBE111:
LBB112:
	.loc 1 177 13
	adrp	x0, lC146@PAGE
	add	x0, x0, lC146@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3888]
	adrp	x0, lC23@PAGE
	add	x0, x0, lC23@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3896]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -208]
	bl	_ada__text_io__put_line__2
L126:
LBE112:
	.loc 1 180 13
	sub	x0, x29, #4096
	sub	x0, x0, #816
	bl	_anubis_types__pqc__zeroize_ml_dsa_secret
LBE108:
LBB113:
	.loc 1 184 7
	adrp	x0, lC147@PAGE
	add	x0, x0, lC147@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3904]
	adrp	x0, lC5@PAGE
	add	x0, x0, lC5@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3912]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -192]
	bl	_ada__text_io__put__4
LBE113:
LBB114:
	.loc 1 187 10
	sub	x0, x29, #8192
	sub	x0, x0, #4048
	bl	_anubis_types__ed25519_secret_keyIP
	.loc 1 189 10
	sub	x0, x29, #4096
	sub	x0, x0, #816
	bl	_anubis_types__ml_dsa_secret_keyIP
	.loc 1 194 19
	sub	x1, x29, #8192
	sub	x1, x1, #4048
	sub	x0, x29, #8192
	sub	x0, x0, #4088
	bl	_anubis_types__classical__ed25519_generate_keypair
	.loc 1 194 19 is_stmt 0 discriminator 1
	sub	x1, x29, #12288
	strb	w0, [x1, 5]
	.loc 1 195 13 is_stmt 1
	sub	x0, x29, #12288
	ldrb	w0, [x0, 5]
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 195 10
	cmp	w0, 0
	beq	L128
LBB115:
	.loc 1 196 13
	adrp	x0, lC148@PAGE
	add	x0, x0, lC148@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3920]
	adrp	x0, lC29@PAGE
	add	x0, x0, lC29@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3928]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -176]
	bl	_ada__text_io__put_line__2
LBE115:
	.loc 1 197 13
	b	L111
L128:
	.loc 1 200 13
	sub	x1, x29, #4096
	sub	x1, x1, #816
	sub	x0, x29, #8192
	sub	x0, x0, #4008
	bl	_anubis_types__pqc__ml_dsa_generate_keypair
	.loc 1 200 13 is_stmt 0 discriminator 1
	sub	x1, x29, #12288
	strb	w0, [x1, 5]
	.loc 1 201 13 is_stmt 1
	sub	x0, x29, #12288
	ldrb	w0, [x0, 5]
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 201 10
	cmp	w0, 0
	beq	L130
LBB116:
	.loc 1 202 13
	adrp	x0, lC149@PAGE
	add	x0, x0, lC149@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3936]
	adrp	x0, lC15@PAGE
	add	x0, x0, lC15@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3944]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -160]
	bl	_ada__text_io__put_line__2
LBE116:
	.loc 1 203 22
	sub	x0, x29, #8192
	sub	x0, x0, #4048
	bl	_anubis_types__classical__zeroize_ed25519_secret
	.loc 1 204 13
	b	L111
L130:
	.loc 1 207 13
	adrp	x0, _msg.6@PAGE
	add	x0, x0, _msg.6@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3952]
	adrp	x0, lC30@PAGE
	add	x0, x0, lC30@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3960]
	sub	x2, x29, #8192
	sub	x2, x2, #1416
	sub	x1, x29, #4096
	sub	x1, x1, #816
	sub	x0, x29, #8192
	sub	x0, x0, #4048
	mov	x4, x2
	mov	x3, x1
	mov	x2, x0
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -144]
	bl	_anubis_types__pqc__hybrid_sign
	.loc 1 207 13 is_stmt 0 discriminator 1
	sub	x1, x29, #12288
	strb	w0, [x1, 5]
	.loc 1 208 13 is_stmt 1
	sub	x0, x29, #12288
	ldrb	w0, [x0, 5]
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 208 10
	cmp	w0, 0
	beq	L131
LBB117:
	.loc 1 209 13
	adrp	x0, lC145@PAGE
	add	x0, x0, lC145@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3968]
	adrp	x0, lC28@PAGE
	add	x0, x0, lC28@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3976]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -128]
	bl	_ada__text_io__put_line__2
LBE117:
	.loc 1 210 22
	sub	x0, x29, #8192
	sub	x0, x0, #4048
	bl	_anubis_types__classical__zeroize_ed25519_secret
	.loc 1 211 16
	sub	x0, x29, #4096
	sub	x0, x0, #816
	bl	_anubis_types__pqc__zeroize_ml_dsa_secret
	.loc 1 212 13
	b	L111
L131:
	.loc 1 215 22
	adrp	x0, _msg.6@PAGE
	add	x0, x0, _msg.6@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3984]
	adrp	x0, lC30@PAGE
	add	x0, x0, lC30@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3992]
	sub	x2, x29, #8192
	sub	x2, x2, #4008
	sub	x1, x29, #8192
	sub	x1, x1, #4088
	sub	x0, x29, #8192
	sub	x0, x0, #1416
	mov	x4, x2
	mov	x3, x1
	mov	x2, x0
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -112]
	bl	_anubis_types__pqc__hybrid_verify
	.loc 1 215 22 is_stmt 0 discriminator 1
	sub	x1, x29, #12288
	strb	w0, [x1, 7]
	.loc 1 216 10 is_stmt 1
	sub	x0, x29, #12288
	ldrb	w0, [x0, 7]
	cmp	w0, 0
	beq	L132
LBB118:
	.loc 1 217 13
	adrp	x0, lC137@PAGE
	add	x0, x0, lC137@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4000]
	adrp	x0, lC25@PAGE
	add	x0, x0, lC25@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4008]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -96]
	bl	_ada__text_io__put_line__2
	b	L133
L132:
LBE118:
LBB119:
	.loc 1 219 13
	adrp	x0, lC146@PAGE
	add	x0, x0, lC146@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4016]
	adrp	x0, lC23@PAGE
	add	x0, x0, lC23@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4024]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -80]
	bl	_ada__text_io__put_line__2
L133:
LBE119:
	.loc 1 222 19
	sub	x0, x29, #8192
	sub	x0, x0, #4048
	bl	_anubis_types__classical__zeroize_ed25519_secret
	.loc 1 223 13
	sub	x0, x29, #4096
	sub	x0, x0, #816
	bl	_anubis_types__pqc__zeroize_ml_dsa_secret
LBE114:
	.loc 1 226 7
	mov	w0, 1
	bl	_ada__text_io__new_line__2
LBB120:
	.loc 1 227 7
	adrp	x0, lC64@PAGE
	add	x0, x0, lC64@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4032]
	adrp	x0, lC2@PAGE
	add	x0, x0, lC2@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4040]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -64]
	bl	_ada__text_io__put_line__2
LBE120:
LBB121:
	.loc 1 228 7
	adrp	x0, lC150@PAGE
	add	x0, x0, lC150@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4048]
	adrp	x0, lC22@PAGE
	add	x0, x0, lC22@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4056]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -48]
	bl	_ada__text_io__put_line__2
LBE121:
LBB122:
	.loc 1 229 7
	adrp	x0, lC151@PAGE
	add	x0, x0, lC151@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4064]
	adrp	x0, lC7@PAGE
	add	x0, x0, lC7@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4072]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -32]
	bl	_ada__text_io__put_line__2
LBE122:
	.loc 1 230 7
	mov	w0, 1
	bl	_ada__text_io__new_line__2
	.loc 1 231 8
	nop
L111:
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L135
	bl	___stack_chk_fail
L135:
	mov	sp, x29
	ldp	x20, x21, [sp, 16]
	ldp	x22, x23, [sp, 32]
	ldp	x24, x25, [sp, 48]
	ldp	x26, x27, [sp, 64]
	ldp	x29, x30, [sp], 80
LCFI19:
	ret
LFE5:
	.const
	.align	2
lC25:
	.word	1
	.word	8
	.align	2
lC26:
	.word	1
	.word	16
	.align	2
lC27:
	.word	0
	.word	4
	.align	2
lC28:
	.word	1
	.word	15
	.align	2
lC30:
	.word	0
	.word	3
	.text
	.align	2
_anubis_main__B_5___finalizer.5:
LFB6:
	sub	x10, sp, #16384
	str	xzr, [x10, 4064]
	stp	x29, x30, [sp, -32]!
LCFI20:
	mov	x29, sp
LCFI21:
	mov	x0, x16
	str	x16, [x29, 24]
	add	x0, x0, 24
	bl	_system__secondary_stack__ss_release
	ldp	x29, x30, [sp], 32
LCFI22:
	ret
LFE6:
	.align	2
_anubis_main__B_5__B_6___finalizer.4:
LFB7:
	sub	x10, sp, #16384
	str	xzr, [x10, 4064]
	stp	x29, x30, [sp, -32]!
LCFI23:
	mov	x29, sp
LCFI24:
	mov	x0, x16
	str	x16, [x29, 24]
	bl	_system__secondary_stack__ss_release
	ldp	x29, x30, [sp], 32
LCFI25:
	ret
LFE7:
	.const
	.align	3
_msg.7:
	.byte	72
	.byte	101
	.byte	108
	.byte	108
	.byte	111
	.space 3
	.align	2
_msg.6:
	.byte	84
	.byte	101
	.byte	115
	.byte	116
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
	.uleb128 0xa0
	.byte	0x9d
	.uleb128 0x14
	.byte	0x9e
	.uleb128 0x13
	.byte	0x4
	.set L$set$5,LCFI1-LCFI0
	.long L$set$5
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$6,LCFI2-LCFI1
	.long L$set$6
	.byte	0x94
	.uleb128 0x12
	.byte	0x95
	.uleb128 0x11
	.byte	0x96
	.uleb128 0x10
	.byte	0x97
	.uleb128 0xf
	.byte	0x98
	.uleb128 0xe
	.byte	0x99
	.uleb128 0xd
	.byte	0x9a
	.uleb128 0xc
	.byte	0x9b
	.uleb128 0xb
	.byte	0x4
	.set L$set$7,LCFI3-LCFI2
	.long L$set$7
	.byte	0xde
	.byte	0xdd
	.byte	0xda
	.byte	0xdb
	.byte	0xd8
	.byte	0xd9
	.byte	0xd6
	.byte	0xd7
	.byte	0xd4
	.byte	0xd5
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
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$12,LCFI5-LCFI4
	.long L$set$12
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$13,LCFI6-LCFI5
	.long L$set$13
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
	.byte	0x9c
	.uleb128 0x1
	.byte	0x4
	.set L$set$14,LCFI7-LCFI6
	.long L$set$14
	.byte	0xde
	.byte	0xdd
	.byte	0xdb
	.byte	0xdc
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
LEFDE2:
LSFDE4:
	.set L$set$15,LEFDE4-LASFDE4
	.long L$set$15
LASFDE4:
	.set L$set$16,Lframe0-Lsection__debug_frame
	.long L$set$16
	.quad	LFB3
	.set L$set$17,LFE3-LFB3
	.quad L$set$17
	.byte	0x4
	.set L$set$18,LCFI8-LFB3
	.long L$set$18
	.byte	0xe
	.uleb128 0x120
	.byte	0x9d
	.uleb128 0x24
	.byte	0x9e
	.uleb128 0x23
	.byte	0x4
	.set L$set$19,LCFI9-LCFI8
	.long L$set$19
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$20,LCFI10-LCFI9
	.long L$set$20
	.byte	0x94
	.uleb128 0x22
	.byte	0x95
	.uleb128 0x21
	.byte	0x96
	.uleb128 0x20
	.byte	0x97
	.uleb128 0x1f
	.byte	0x98
	.uleb128 0x1e
	.byte	0x99
	.uleb128 0x1d
	.byte	0x9a
	.uleb128 0x1c
	.byte	0x9b
	.uleb128 0x1b
	.byte	0x4
	.set L$set$21,LCFI11-LCFI10
	.long L$set$21
	.byte	0xde
	.byte	0xdd
	.byte	0xda
	.byte	0xdb
	.byte	0xd8
	.byte	0xd9
	.byte	0xd6
	.byte	0xd7
	.byte	0xd4
	.byte	0xd5
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE4:
LSFDE6:
	.set L$set$22,LEFDE6-LASFDE6
	.long L$set$22
LASFDE6:
	.set L$set$23,Lframe0-Lsection__debug_frame
	.long L$set$23
	.quad	LFB4
	.set L$set$24,LFE4-LFB4
	.quad L$set$24
	.byte	0x4
	.set L$set$25,LCFI12-LFB4
	.long L$set$25
	.byte	0xe
	.uleb128 0x170
	.byte	0x9d
	.uleb128 0x2e
	.byte	0x9e
	.uleb128 0x2d
	.byte	0x4
	.set L$set$26,LCFI13-LCFI12
	.long L$set$26
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$27,LCFI14-LCFI13
	.long L$set$27
	.byte	0x94
	.uleb128 0x2c
	.byte	0x95
	.uleb128 0x2b
	.byte	0x96
	.uleb128 0x2a
	.byte	0x97
	.uleb128 0x29
	.byte	0x98
	.uleb128 0x28
	.byte	0x99
	.uleb128 0x27
	.byte	0x9a
	.uleb128 0x26
	.byte	0x9b
	.uleb128 0x25
	.byte	0x4
	.set L$set$28,LCFI15-LCFI14
	.long L$set$28
	.byte	0xde
	.byte	0xdd
	.byte	0xda
	.byte	0xdb
	.byte	0xd8
	.byte	0xd9
	.byte	0xd6
	.byte	0xd7
	.byte	0xd4
	.byte	0xd5
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
	.quad	LFB5
	.set L$set$31,LFE5-LFB5
	.quad L$set$31
	.byte	0x4
	.set L$set$32,LCFI16-LFB5
	.long L$set$32
	.byte	0xe
	.uleb128 0x50
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$33,LCFI17-LCFI16
	.long L$set$33
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$34,LCFI18-LCFI17
	.long L$set$34
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
	.set L$set$35,LCFI19-LCFI18
	.long L$set$35
	.byte	0xde
	.byte	0xdd
	.byte	0xda
	.byte	0xdb
	.byte	0xd8
	.byte	0xd9
	.byte	0xd6
	.byte	0xd7
	.byte	0xd4
	.byte	0xd5
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE8:
LSFDE10:
	.set L$set$36,LEFDE10-LASFDE10
	.long L$set$36
LASFDE10:
	.set L$set$37,Lframe0-Lsection__debug_frame
	.long L$set$37
	.quad	LFB6
	.set L$set$38,LFE6-LFB6
	.quad L$set$38
	.byte	0x4
	.set L$set$39,LCFI20-LFB6
	.long L$set$39
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$40,LCFI21-LCFI20
	.long L$set$40
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$41,LCFI22-LCFI21
	.long L$set$41
	.byte	0xde
	.byte	0xdd
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
	.quad	LFB7
	.set L$set$44,LFE7-LFB7
	.quad L$set$44
	.byte	0x4
	.set L$set$45,LCFI23-LFB7
	.long L$set$45
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
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
	.section __TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
EH_frame1:
	.set L$set$48,LECIE1-LSCIE1
	.long L$set$48
LSCIE1:
	.long	0
	.byte	0x3
	.ascii "zPLR\0"
	.uleb128 0x1
	.sleb128 -8
	.uleb128 0x1e
	.uleb128 0x7
	.byte	0x9b
L_got_pcr0:
	.long	___gnat_personality_v0@GOT-L_got_pcr0
	.byte	0x10
	.byte	0x10
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LECIE1:
LSFDE15:
	.set L$set$49,LEFDE15-LASFDE15
	.long L$set$49
LASFDE15:
	.long	LASFDE15-EH_frame1
	.quad	LFB2-.
	.set L$set$50,LFE2-LFB2
	.quad L$set$50
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$51,LCFI0-LFB2
	.long L$set$51
	.byte	0xe
	.uleb128 0xa0
	.byte	0x9d
	.uleb128 0x14
	.byte	0x9e
	.uleb128 0x13
	.byte	0x4
	.set L$set$52,LCFI1-LCFI0
	.long L$set$52
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$53,LCFI2-LCFI1
	.long L$set$53
	.byte	0x94
	.uleb128 0x12
	.byte	0x95
	.uleb128 0x11
	.byte	0x96
	.uleb128 0x10
	.byte	0x97
	.uleb128 0xf
	.byte	0x98
	.uleb128 0xe
	.byte	0x99
	.uleb128 0xd
	.byte	0x9a
	.uleb128 0xc
	.byte	0x9b
	.uleb128 0xb
	.byte	0x4
	.set L$set$54,LCFI3-LCFI2
	.long L$set$54
	.byte	0xde
	.byte	0xdd
	.byte	0xda
	.byte	0xdb
	.byte	0xd8
	.byte	0xd9
	.byte	0xd6
	.byte	0xd7
	.byte	0xd4
	.byte	0xd5
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE15:
LSFDE17:
	.set L$set$55,LEFDE17-LASFDE17
	.long L$set$55
LASFDE17:
	.long	LASFDE17-EH_frame1
	.quad	LFB1-.
	.set L$set$56,LFE1-LFB1
	.quad L$set$56
	.uleb128 0x8
	.quad	LLSDA1-.
	.byte	0x4
	.set L$set$57,LCFI4-LFB1
	.long L$set$57
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$58,LCFI5-LCFI4
	.long L$set$58
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$59,LCFI6-LCFI5
	.long L$set$59
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
	.byte	0x9c
	.uleb128 0x1
	.byte	0x4
	.set L$set$60,LCFI7-LCFI6
	.long L$set$60
	.byte	0xde
	.byte	0xdd
	.byte	0xdb
	.byte	0xdc
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
LEFDE17:
LSFDE19:
	.set L$set$61,LEFDE19-LASFDE19
	.long L$set$61
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB3-.
	.set L$set$62,LFE3-LFB3
	.quad L$set$62
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$63,LCFI8-LFB3
	.long L$set$63
	.byte	0xe
	.uleb128 0x120
	.byte	0x9d
	.uleb128 0x24
	.byte	0x9e
	.uleb128 0x23
	.byte	0x4
	.set L$set$64,LCFI9-LCFI8
	.long L$set$64
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$65,LCFI10-LCFI9
	.long L$set$65
	.byte	0x94
	.uleb128 0x22
	.byte	0x95
	.uleb128 0x21
	.byte	0x96
	.uleb128 0x20
	.byte	0x97
	.uleb128 0x1f
	.byte	0x98
	.uleb128 0x1e
	.byte	0x99
	.uleb128 0x1d
	.byte	0x9a
	.uleb128 0x1c
	.byte	0x9b
	.uleb128 0x1b
	.byte	0x4
	.set L$set$66,LCFI11-LCFI10
	.long L$set$66
	.byte	0xde
	.byte	0xdd
	.byte	0xda
	.byte	0xdb
	.byte	0xd8
	.byte	0xd9
	.byte	0xd6
	.byte	0xd7
	.byte	0xd4
	.byte	0xd5
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE19:
LSFDE21:
	.set L$set$67,LEFDE21-LASFDE21
	.long L$set$67
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB4-.
	.set L$set$68,LFE4-LFB4
	.quad L$set$68
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$69,LCFI12-LFB4
	.long L$set$69
	.byte	0xe
	.uleb128 0x170
	.byte	0x9d
	.uleb128 0x2e
	.byte	0x9e
	.uleb128 0x2d
	.byte	0x4
	.set L$set$70,LCFI13-LCFI12
	.long L$set$70
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$71,LCFI14-LCFI13
	.long L$set$71
	.byte	0x94
	.uleb128 0x2c
	.byte	0x95
	.uleb128 0x2b
	.byte	0x96
	.uleb128 0x2a
	.byte	0x97
	.uleb128 0x29
	.byte	0x98
	.uleb128 0x28
	.byte	0x99
	.uleb128 0x27
	.byte	0x9a
	.uleb128 0x26
	.byte	0x9b
	.uleb128 0x25
	.byte	0x4
	.set L$set$72,LCFI15-LCFI14
	.long L$set$72
	.byte	0xde
	.byte	0xdd
	.byte	0xda
	.byte	0xdb
	.byte	0xd8
	.byte	0xd9
	.byte	0xd6
	.byte	0xd7
	.byte	0xd4
	.byte	0xd5
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE21:
LSFDE23:
	.set L$set$73,LEFDE23-LASFDE23
	.long L$set$73
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB5-.
	.set L$set$74,LFE5-LFB5
	.quad L$set$74
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$75,LCFI16-LFB5
	.long L$set$75
	.byte	0xe
	.uleb128 0x50
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$76,LCFI17-LCFI16
	.long L$set$76
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$77,LCFI18-LCFI17
	.long L$set$77
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
	.set L$set$78,LCFI19-LCFI18
	.long L$set$78
	.byte	0xde
	.byte	0xdd
	.byte	0xda
	.byte	0xdb
	.byte	0xd8
	.byte	0xd9
	.byte	0xd6
	.byte	0xd7
	.byte	0xd4
	.byte	0xd5
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE23:
LSFDE25:
	.set L$set$79,LEFDE25-LASFDE25
	.long L$set$79
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB6-.
	.set L$set$80,LFE6-LFB6
	.quad L$set$80
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$81,LCFI20-LFB6
	.long L$set$81
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$82,LCFI21-LCFI20
	.long L$set$82
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$83,LCFI22-LCFI21
	.long L$set$83
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE25:
LSFDE27:
	.set L$set$84,LEFDE27-LASFDE27
	.long L$set$84
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB7-.
	.set L$set$85,LFE7-LFB7
	.quad L$set$85
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$86,LCFI23-LFB7
	.long L$set$86
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$87,LCFI24-LCFI23
	.long L$set$87
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$88,LCFI25-LCFI24
	.long L$set$88
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE27:
	.text
Letext0:
	.file 2 "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_types.ads"
	.file 3 "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_types-pqc.ads"
	.file 4 "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_types-storage.ads"
	.section __DWARF,__debug_info,regular,debug
Lsection__debug_info:
Ldebug_info0:
	.long	0xf9b
	.short	0x4
	.set L$set$89,Ldebug_abbrev0-Lsection__debug_abbrev
	.long L$set$89
	.byte	0x8
	.uleb128 0x1
	.ascii "GNU Ada 14.1.0 -O0 -gnatA -g -gnat2012 -gnatwa -gnatwe -fstack-check=specific -fstack-protector-strong -gnatR2js -gnatws -gnatec=/private/var/folders/bg/pt9l6y1j47q642kp3z5blrmh0000gn/T/GPR.71672/GNAT-TEMP-000003.TMP -gnatem=/private/var/folders/bg/pt9l6y1j47q642kp3z5blrmh0000gn/T/GPR.71672/GNAT-TEMP-000004.TMP -mmacosx-version-min=16.0.0 -mcpu=apple-m1 -mlittle-endian -mabi=lp64 -fPIC\0"
	.byte	0xd
	.ascii "/Users/sicarii/Desktop/anubis-spark/src/anubis_main.adb\0"
	.ascii "/Users/sicarii/Desktop/anubis-spark/obj/gnatprove/data_representation\0"
	.quad	Ltext0
	.set L$set$90,LFE5-Ltext0
	.quad L$set$90
	.set L$set$91,Ldebug_line0-Lsection__debug_line
	.long L$set$91
	.uleb128 0x2
	.byte	0x4
	.byte	0x5
	.ascii "ada__text_io__TcountB\0"
	.uleb128 0x3
	.byte	0x1
	.byte	0x7
	.ascii "interfaces__unsigned_8\0"
	.uleb128 0x4
	.byte	0
	.byte	0xff
	.ascii "anubis_types__byte\0"
	.long	0x271
	.uleb128 0x2
	.byte	0x1
	.byte	0x7
	.ascii "anubis_types__TbyteB\0"
	.uleb128 0x3
	.byte	0x4
	.byte	0x5
	.ascii "integer\0"
	.uleb128 0x5
	.ascii "anubis_types__ml_kem_public_key__T33s\0"
	.long	0x257
	.long	0x2cb
	.uleb128 0x6
	.long	0x289
	.sleb128 1568
	.byte	0
	.uleb128 0x7
	.ascii "anubis_types__ml_kem_public_key\0"
	.short	0x620
	.byte	0x2
	.byte	0xb8
	.byte	0x9
	.long	0x303
	.uleb128 0x8
	.set L$set$92,LASF0-Lsection__debug_str
	.long L$set$92
	.byte	0x2
	.byte	0xb9
	.byte	0x7
	.long	0x294
	.byte	0
	.byte	0
	.uleb128 0x5
	.ascii "anubis_types__ml_kem_secret_key__T35s\0"
	.long	0x257
	.long	0x33a
	.uleb128 0x6
	.long	0x289
	.sleb128 3168
	.byte	0
	.uleb128 0x7
	.ascii "anubis_types__ml_kem_secret_key\0"
	.short	0xc61
	.byte	0x2
	.byte	0xbc
	.byte	0x9
	.long	0x380
	.uleb128 0x8
	.set L$set$93,LASF0-Lsection__debug_str
	.long L$set$93
	.byte	0x2
	.byte	0xbd
	.byte	0x7
	.long	0x303
	.byte	0
	.uleb128 0x9
	.set L$set$94,LASF1-Lsection__debug_str
	.long L$set$94
	.byte	0x2
	.byte	0xbe
	.byte	0x7
	.long	0x380
	.short	0xc60
	.byte	0
	.uleb128 0x3
	.byte	0x1
	.byte	0x2
	.ascii "boolean\0"
	.uleb128 0x5
	.ascii "anubis_types__ml_kem_ciphertext__T37s\0"
	.long	0x257
	.long	0x3c2
	.uleb128 0x6
	.long	0x289
	.sleb128 1568
	.byte	0
	.uleb128 0x7
	.ascii "anubis_types__ml_kem_ciphertext\0"
	.short	0x620
	.byte	0x2
	.byte	0xc1
	.byte	0x9
	.long	0x3fa
	.uleb128 0x8
	.set L$set$95,LASF0-Lsection__debug_str
	.long L$set$95
	.byte	0x2
	.byte	0xc2
	.byte	0x7
	.long	0x38b
	.byte	0
	.byte	0
	.uleb128 0x5
	.ascii "anubis_types__ml_kem_shared_secret__T39s\0"
	.long	0x257
	.long	0x433
	.uleb128 0x6
	.long	0x289
	.sleb128 32
	.byte	0
	.uleb128 0xa
	.ascii "anubis_types__ml_kem_shared_secret\0"
	.byte	0x21
	.byte	0x2
	.byte	0xc5
	.byte	0x9
	.long	0x47a
	.uleb128 0x8
	.set L$set$96,LASF0-Lsection__debug_str
	.long L$set$96
	.byte	0x2
	.byte	0xc6
	.byte	0x7
	.long	0x3fa
	.byte	0
	.uleb128 0x8
	.set L$set$97,LASF1-Lsection__debug_str
	.long L$set$97
	.byte	0x2
	.byte	0xc7
	.byte	0x7
	.long	0x380
	.byte	0x20
	.byte	0
	.uleb128 0x5
	.ascii "anubis_types__ml_dsa_public_key__T41s\0"
	.long	0x257
	.long	0x4b1
	.uleb128 0x6
	.long	0x289
	.sleb128 2592
	.byte	0
	.uleb128 0x7
	.ascii "anubis_types__ml_dsa_public_key\0"
	.short	0xa20
	.byte	0x2
	.byte	0xca
	.byte	0x9
	.long	0x4e9
	.uleb128 0x8
	.set L$set$98,LASF0-Lsection__debug_str
	.long L$set$98
	.byte	0x2
	.byte	0xcb
	.byte	0x7
	.long	0x47a
	.byte	0
	.byte	0
	.uleb128 0x5
	.ascii "anubis_types__ml_dsa_secret_key__T43s\0"
	.long	0x257
	.long	0x520
	.uleb128 0x6
	.long	0x289
	.sleb128 4896
	.byte	0
	.uleb128 0x7
	.ascii "anubis_types__ml_dsa_secret_key\0"
	.short	0x1321
	.byte	0x2
	.byte	0xce
	.byte	0x9
	.long	0x566
	.uleb128 0x8
	.set L$set$99,LASF0-Lsection__debug_str
	.long L$set$99
	.byte	0x2
	.byte	0xcf
	.byte	0x7
	.long	0x4e9
	.byte	0
	.uleb128 0x9
	.set L$set$100,LASF1-Lsection__debug_str
	.long L$set$100
	.byte	0x2
	.byte	0xd0
	.byte	0x7
	.long	0x380
	.short	0x1320
	.byte	0
	.uleb128 0x5
	.ascii "anubis_types__ml_dsa_signature__T45s\0"
	.long	0x257
	.long	0x59c
	.uleb128 0x6
	.long	0x289
	.sleb128 4627
	.byte	0
	.uleb128 0x7
	.ascii "anubis_types__ml_dsa_signature\0"
	.short	0x1213
	.byte	0x2
	.byte	0xd3
	.byte	0x9
	.long	0x5d3
	.uleb128 0x8
	.set L$set$101,LASF0-Lsection__debug_str
	.long L$set$101
	.byte	0x2
	.byte	0xd4
	.byte	0x7
	.long	0x566
	.byte	0
	.byte	0
	.uleb128 0x5
	.ascii "anubis_types__ed25519_public_key__T21s\0"
	.long	0x257
	.long	0x60a
	.uleb128 0x6
	.long	0x289
	.sleb128 32
	.byte	0
	.uleb128 0xa
	.ascii "anubis_types__ed25519_public_key\0"
	.byte	0x20
	.byte	0x2
	.byte	0x9e
	.byte	0x9
	.long	0x642
	.uleb128 0x8
	.set L$set$102,LASF0-Lsection__debug_str
	.long L$set$102
	.byte	0x2
	.byte	0x9f
	.byte	0x7
	.long	0x5d3
	.byte	0
	.byte	0
	.uleb128 0x5
	.ascii "anubis_types__ed25519_secret_key__T23s\0"
	.long	0x257
	.long	0x679
	.uleb128 0x6
	.long	0x289
	.sleb128 32
	.byte	0
	.uleb128 0xa
	.ascii "anubis_types__ed25519_secret_key\0"
	.byte	0x21
	.byte	0x2
	.byte	0xa2
	.byte	0x9
	.long	0x6be
	.uleb128 0x8
	.set L$set$103,LASF0-Lsection__debug_str
	.long L$set$103
	.byte	0x2
	.byte	0xa3
	.byte	0x7
	.long	0x642
	.byte	0
	.uleb128 0x8
	.set L$set$104,LASF1-Lsection__debug_str
	.long L$set$104
	.byte	0x2
	.byte	0xa4
	.byte	0x7
	.long	0x380
	.byte	0x20
	.byte	0
	.uleb128 0x5
	.ascii "anubis_types__ed25519_signature__T25s\0"
	.long	0x257
	.long	0x6f5
	.uleb128 0x6
	.long	0x289
	.sleb128 64
	.byte	0
	.uleb128 0xa
	.ascii "anubis_types__ed25519_signature\0"
	.byte	0x40
	.byte	0x2
	.byte	0xa7
	.byte	0x9
	.long	0x72c
	.uleb128 0x8
	.set L$set$105,LASF0-Lsection__debug_str
	.long L$set$105
	.byte	0x2
	.byte	0xa8
	.byte	0x7
	.long	0x6be
	.byte	0
	.byte	0
	.uleb128 0x7
	.ascii "anubis_types__pqc__hybrid_signature\0"
	.short	0x1253
	.byte	0x3
	.byte	0xc6
	.byte	0x9
	.long	0x784
	.uleb128 0xb
	.ascii "ed25519_sig\0"
	.byte	0x3
	.byte	0xc7
	.byte	0x7
	.long	0x6f5
	.byte	0
	.uleb128 0xb
	.ascii "ml_dsa_sig\0"
	.byte	0x3
	.byte	0xc8
	.byte	0x7
	.long	0x59c
	.byte	0x40
	.byte	0
	.uleb128 0x2
	.byte	0x8
	.byte	0x5
	.ascii "system__parameters__Tsize_typeB\0"
	.uleb128 0x3
	.byte	0x1
	.byte	0x7
	.ascii "system__storage_elements__storage_element\0"
	.uleb128 0x5
	.ascii "anubis_types__x25519_public_key__T15s\0"
	.long	0x257
	.long	0x80a
	.uleb128 0x6
	.long	0x289
	.sleb128 32
	.byte	0
	.uleb128 0xa
	.ascii "anubis_types__x25519_public_key\0"
	.byte	0x20
	.byte	0x2
	.byte	0x90
	.byte	0x9
	.long	0x841
	.uleb128 0x8
	.set L$set$106,LASF0-Lsection__debug_str
	.long L$set$106
	.byte	0x2
	.byte	0x91
	.byte	0x7
	.long	0x7d4
	.byte	0
	.byte	0
	.uleb128 0x5
	.ascii "anubis_types__x25519_secret_key__T17s\0"
	.long	0x257
	.long	0x877
	.uleb128 0x6
	.long	0x289
	.sleb128 32
	.byte	0
	.uleb128 0xa
	.ascii "anubis_types__x25519_secret_key\0"
	.byte	0x21
	.byte	0x2
	.byte	0x94
	.byte	0x9
	.long	0x8bb
	.uleb128 0x8
	.set L$set$107,LASF0-Lsection__debug_str
	.long L$set$107
	.byte	0x2
	.byte	0x95
	.byte	0x7
	.long	0x841
	.byte	0
	.uleb128 0x8
	.set L$set$108,LASF1-Lsection__debug_str
	.long L$set$108
	.byte	0x2
	.byte	0x96
	.byte	0x7
	.long	0x380
	.byte	0x20
	.byte	0
	.uleb128 0x7
	.ascii "anubis_types__storage__identity_keypair\0"
	.short	0x3045
	.byte	0x4
	.byte	0x42
	.byte	0x9
	.long	0x999
	.uleb128 0xb
	.ascii "x25519_pk\0"
	.byte	0x4
	.byte	0x44
	.byte	0x7
	.long	0x80a
	.byte	0
	.uleb128 0xb
	.ascii "x25519_sk\0"
	.byte	0x4
	.byte	0x45
	.byte	0x7
	.long	0x877
	.byte	0x20
	.uleb128 0xb
	.ascii "ed25519_pk\0"
	.byte	0x4
	.byte	0x46
	.byte	0x7
	.long	0x60a
	.byte	0x41
	.uleb128 0xb
	.ascii "ed25519_sk\0"
	.byte	0x4
	.byte	0x47
	.byte	0x7
	.long	0x679
	.byte	0x61
	.uleb128 0xb
	.ascii "ml_kem_pk\0"
	.byte	0x4
	.byte	0x4a
	.byte	0x7
	.long	0x2cb
	.byte	0x82
	.uleb128 0xc
	.ascii "ml_kem_sk\0"
	.byte	0x4
	.byte	0x4b
	.byte	0x7
	.long	0x33a
	.short	0x6a2
	.uleb128 0xc
	.ascii "ml_dsa_pk\0"
	.byte	0x4
	.byte	0x4c
	.byte	0x7
	.long	0x4b1
	.short	0x1303
	.uleb128 0xc
	.ascii "ml_dsa_sk\0"
	.byte	0x4
	.byte	0x4d
	.byte	0x7
	.long	0x520
	.short	0x1d23
	.uleb128 0x9
	.set L$set$109,LASF1-Lsection__debug_str
	.long L$set$109
	.byte	0x4
	.byte	0x50
	.byte	0x7
	.long	0x380
	.short	0x3044
	.byte	0
	.uleb128 0xd
	.ascii "anubis_main\0"
	.byte	0x1
	.byte	0xf
	.byte	0x1
	.ascii "_ada_anubis_main\0"
	.quad	LFB1
	.set L$set$110,LFE1-LFB1
	.quad L$set$110
	.uleb128 0x1
	.byte	0x9c
	.long	0xf91
	.uleb128 0xe
	.ascii "anubis_main__print_banner\0"
	.byte	0x1
	.byte	0x11
	.byte	0x4
	.quad	LFB2
	.set L$set$111,LFE2-LFB2
	.quad L$set$111
	.uleb128 0x1
	.byte	0x9c
	.uleb128 0x6
	.byte	0x91
	.sleb128 -8
	.byte	0x6
	.byte	0x23
	.uleb128 0x30
	.byte	0x6
	.uleb128 0xe
	.ascii "anubis_main__print_usage\0"
	.byte	0x1
	.byte	0x20
	.byte	0x4
	.quad	LFB3
	.set L$set$112,LFE3-LFB3
	.quad L$set$112
	.uleb128 0x1
	.byte	0x9c
	.uleb128 0x6
	.byte	0x91
	.sleb128 -8
	.byte	0x6
	.byte	0x23
	.uleb128 0x30
	.byte	0x6
	.uleb128 0xe
	.ascii "anubis_main__print_version\0"
	.byte	0x1
	.byte	0x39
	.byte	0x4
	.quad	LFB4
	.set L$set$113,LFE4-LFB4
	.quad L$set$113
	.uleb128 0x1
	.byte	0x9c
	.uleb128 0x6
	.byte	0x91
	.sleb128 -8
	.byte	0x6
	.byte	0x23
	.uleb128 0x30
	.byte	0x6
	.uleb128 0xf
	.ascii "anubis_main__run_self_test\0"
	.byte	0x1
	.byte	0x56
	.byte	0x4
	.quad	LFB5
	.set L$set$114,LFE5-LFB5
	.quad L$set$114
	.uleb128 0x1
	.byte	0x9c
	.uleb128 0x7
	.byte	0x70
	.sleb128 4088
	.byte	0x6
	.byte	0x23
	.uleb128 0x30
	.byte	0x6
	.long	0xce4
	.uleb128 0x10
	.ascii "success\0"
	.byte	0x1
	.byte	0x57
	.byte	0x7
	.long	0x380
	.uleb128 0x4
	.byte	0x91
	.sleb128 -12363
	.uleb128 0x11
	.set L$set$115,Ldebug_ranges0+0xf0-Lsection__debug_ranges
	.long L$set$115
	.long	0xaf0
	.uleb128 0x10
	.ascii "pk\0"
	.byte	0x1
	.byte	0x61
	.byte	0xa
	.long	0x2cb
	.uleb128 0x4
	.byte	0x91
	.sleb128 -9688
	.uleb128 0x10
	.ascii "sk\0"
	.byte	0x1
	.byte	0x62
	.byte	0xa
	.long	0x33a
	.uleb128 0x3
	.byte	0x91
	.sleb128 -4992
	.byte	0
	.uleb128 0x11
	.set L$set$116,Ldebug_ranges0+0x120-Lsection__debug_ranges
	.long L$set$116
	.long	0xb67
	.uleb128 0x10
	.ascii "alice_pk\0"
	.byte	0x1
	.byte	0x71
	.byte	0xa
	.long	0x2cb
	.uleb128 0x4
	.byte	0x91
	.sleb128 -12280
	.uleb128 0x10
	.ascii "alice_sk\0"
	.byte	0x1
	.byte	0x72
	.byte	0xa
	.long	0x33a
	.uleb128 0x3
	.byte	0x91
	.sleb128 -4992
	.uleb128 0x10
	.ascii "ct\0"
	.byte	0x1
	.byte	0x73
	.byte	0xa
	.long	0x3c2
	.uleb128 0x4
	.byte	0x91
	.sleb128 -9688
	.uleb128 0x10
	.ascii "bob_secret\0"
	.byte	0x1
	.byte	0x74
	.byte	0xa
	.long	0x433
	.uleb128 0x4
	.byte	0x91
	.sleb128 -12360
	.uleb128 0x10
	.ascii "alice_secret\0"
	.byte	0x1
	.byte	0x75
	.byte	0xa
	.long	0x433
	.uleb128 0x4
	.byte	0x91
	.sleb128 -12320
	.byte	0
	.uleb128 0x12
	.quad	LBB108
	.set L$set$117,LBE108-LBB108
	.quad L$set$117
	.long	0xc10
	.uleb128 0x10
	.ascii "pk\0"
	.byte	0x1
	.byte	0x9a
	.byte	0xa
	.long	0x4b1
	.uleb128 0x4
	.byte	0x91
	.sleb128 -12280
	.uleb128 0x10
	.ascii "sk\0"
	.byte	0x1
	.byte	0x9b
	.byte	0xa
	.long	0x520
	.uleb128 0x3
	.byte	0x91
	.sleb128 -4992
	.uleb128 0x5
	.ascii "anubis_main__run_self_test__B_3__T66b\0"
	.long	0x257
	.long	0xbd2
	.uleb128 0x13
	.long	0x289
	.sleb128 0
	.sleb128 4
	.byte	0
	.uleb128 0x14
	.long	0xb9b
	.uleb128 0x10
	.ascii "msg\0"
	.byte	0x1
	.byte	0x9c
	.byte	0xa
	.long	0xbd2
	.uleb128 0x9
	.byte	0x3
	.quad	_msg.7
	.uleb128 0x10
	.ascii "sig\0"
	.byte	0x1
	.byte	0x9d
	.byte	0xa
	.long	0x59c
	.uleb128 0x4
	.byte	0x91
	.sleb128 -9688
	.uleb128 0x15
	.set L$set$118,LASF1-Lsection__debug_str
	.long L$set$118
	.byte	0x1
	.byte	0x9e
	.byte	0xa
	.long	0x380
	.uleb128 0x4
	.byte	0x91
	.sleb128 -12362
	.byte	0
	.uleb128 0x16
	.quad	LBB114
	.set L$set$119,LBE114-LBB114
	.quad L$set$119
	.uleb128 0x10
	.ascii "ed_pk\0"
	.byte	0x1
	.byte	0xba
	.byte	0xa
	.long	0x60a
	.uleb128 0x4
	.byte	0x91
	.sleb128 -12360
	.uleb128 0x10
	.ascii "ed_sk\0"
	.byte	0x1
	.byte	0xbb
	.byte	0xa
	.long	0x679
	.uleb128 0x4
	.byte	0x91
	.sleb128 -12320
	.uleb128 0x10
	.ascii "dsa_pk\0"
	.byte	0x1
	.byte	0xbc
	.byte	0xa
	.long	0x4b1
	.uleb128 0x4
	.byte	0x91
	.sleb128 -12280
	.uleb128 0x10
	.ascii "dsa_sk\0"
	.byte	0x1
	.byte	0xbd
	.byte	0xa
	.long	0x520
	.uleb128 0x3
	.byte	0x91
	.sleb128 -4992
	.uleb128 0x5
	.ascii "anubis_main__run_self_test__B_4__T75b\0"
	.long	0x257
	.long	0xca5
	.uleb128 0x13
	.long	0x289
	.sleb128 0
	.sleb128 3
	.byte	0
	.uleb128 0x14
	.long	0xc6e
	.uleb128 0x10
	.ascii "msg\0"
	.byte	0x1
	.byte	0xbe
	.byte	0xa
	.long	0xca5
	.uleb128 0x9
	.byte	0x3
	.quad	_msg.6
	.uleb128 0x10
	.ascii "sig\0"
	.byte	0x1
	.byte	0xbf
	.byte	0xa
	.long	0x72c
	.uleb128 0x4
	.byte	0x91
	.sleb128 -9688
	.uleb128 0x15
	.set L$set$120,LASF1-Lsection__debug_str
	.long L$set$120
	.byte	0x1
	.byte	0xc0
	.byte	0xa
	.long	0x380
	.uleb128 0x4
	.byte	0x91
	.sleb128 -12361
	.byte	0
	.byte	0
	.uleb128 0x17
	.set L$set$121,Ldebug_ranges0+0-Lsection__debug_ranges
	.long L$set$121
	.uleb128 0x18
	.ascii "B89b\0"
	.long	0x289
	.uleb128 0x4
	.byte	0x91
	.sleb128 -12716
	.uleb128 0x18
	.ascii "B93b\0"
	.long	0x289
	.uleb128 0x4
	.byte	0x91
	.sleb128 -12712
	.uleb128 0x5
	.ascii "anubis_main__B_5__TcommandS\0"
	.long	0xf91
	.long	0xd3a
	.uleb128 0x19
	.long	0x289
	.long	0xce9
	.long	0xcf8
	.byte	0
	.uleb128 0x10
	.ascii "command\0"
	.byte	0x1
	.byte	0xf0
	.byte	0x7
	.long	0xd4f
	.uleb128 0x4
	.byte	0x91
	.sleb128 -12600
	.uleb128 0x1a
	.byte	0x8
	.long	0xd07
	.uleb128 0x11
	.set L$set$122,Ldebug_ranges0+0x30-Lsection__debug_ranges
	.long L$set$122
	.long	0xf12
	.uleb128 0x18
	.ascii "B113b\0"
	.long	0x289
	.uleb128 0x4
	.byte	0x91
	.sleb128 -12708
	.uleb128 0x18
	.ascii "B117b\0"
	.long	0x289
	.uleb128 0x4
	.byte	0x91
	.sleb128 -12704
	.uleb128 0x5
	.ascii "anubis_main__B_5__B_6__Toutput_fileS\0"
	.long	0xf91
	.long	0xdba
	.uleb128 0x19
	.long	0x289
	.long	0xd5e
	.long	0xd6e
	.byte	0
	.uleb128 0x10
	.ascii "output_file\0"
	.byte	0x1
	.byte	0xfb
	.byte	0xd
	.long	0xd7e
	.uleb128 0x5
	.byte	0x91
	.sleb128 -12576
	.byte	0x6
	.uleb128 0x10
	.ascii "identity\0"
	.byte	0x1
	.byte	0xfe
	.byte	0xd
	.long	0x8bb
	.uleb128 0x4
	.byte	0x91
	.sleb128 -12464
	.uleb128 0x10
	.ascii "success\0"
	.byte	0x1
	.byte	0xff
	.byte	0xd
	.long	0x380
	.uleb128 0x4
	.byte	0x91
	.sleb128 -12717
	.uleb128 0x11
	.set L$set$123,Ldebug_ranges0+0x60-Lsection__debug_ranges
	.long L$set$123
	.long	0xe8a
	.uleb128 0x18
	.ascii "anubis_main__B_5__B_6__B131b__TTS137bSP1___U\0"
	.long	0x289
	.uleb128 0x4
	.byte	0x91
	.sleb128 -12700
	.uleb128 0x5
	.ascii "anubis_main__B_5__B_6__B131b__TS137bS\0"
	.long	0xf91
	.long	0xe78
	.uleb128 0x1b
	.long	0x289
	.long	0xe08
	.byte	0
	.uleb128 0x18
	.ascii "S137b\0"
	.long	0xe3f
	.uleb128 0x5
	.byte	0x91
	.sleb128 -12560
	.byte	0x6
	.byte	0
	.uleb128 0x17
	.set L$set$124,Ldebug_ranges0+0x90-Lsection__debug_ranges
	.long L$set$124
	.uleb128 0x18
	.ascii "anubis_main__B_5__B_6__B144b__TTS148bSP1___U\0"
	.long	0x289
	.uleb128 0x4
	.byte	0x91
	.sleb128 -12696
	.uleb128 0x5
	.ascii "anubis_main__B_5__B_6__B144b__TS148bS\0"
	.long	0xf91
	.long	0xeff
	.uleb128 0x1b
	.long	0x289
	.long	0xe8f
	.byte	0
	.uleb128 0x18
	.ascii "S148b\0"
	.long	0xec6
	.uleb128 0x5
	.byte	0x91
	.sleb128 -12544
	.byte	0x6
	.byte	0
	.byte	0
	.uleb128 0x17
	.set L$set$125,Ldebug_ranges0+0xc0-Lsection__debug_ranges
	.long L$set$125
	.uleb128 0x18
	.ascii "anubis_main__B_5__B167b__TTS171bSP1___U\0"
	.long	0x289
	.uleb128 0x4
	.byte	0x91
	.sleb128 -12692
	.uleb128 0x5
	.ascii "anubis_main__B_5__B167b__TS171bS\0"
	.long	0xf91
	.long	0xf7d
	.uleb128 0x1b
	.long	0x289
	.long	0xf17
	.byte	0
	.uleb128 0x18
	.ascii "S171b\0"
	.long	0xf49
	.uleb128 0x5
	.byte	0x91
	.sleb128 -12528
	.byte	0x6
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0x3
	.byte	0x1
	.byte	0x8
	.ascii "character\0"
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
	.uleb128 0x34
	.uleb128 0x19
	.byte	0
	.byte	0
	.uleb128 0x3
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
	.uleb128 0x4
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
	.uleb128 0xd
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
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0xe
	.uleb128 0x2e
	.byte	0
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
	.uleb128 0xf
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
	.uleb128 0x10
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
	.uleb128 0x11
	.uleb128 0xb
	.byte	0x1
	.uleb128 0x55
	.uleb128 0x17
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x12
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
	.uleb128 0x13
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
	.uleb128 0x14
	.uleb128 0x26
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x34
	.uleb128 0x19
	.byte	0
	.byte	0
	.uleb128 0x15
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
	.uleb128 0xb
	.byte	0x1
	.uleb128 0x55
	.uleb128 0x17
	.byte	0
	.byte	0
	.uleb128 0x18
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
	.uleb128 0x19
	.uleb128 0x21
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x22
	.uleb128 0x13
	.uleb128 0x2f
	.uleb128 0x13
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
	.uleb128 0x21
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2f
	.uleb128 0x13
	.byte	0
	.byte	0
	.byte	0
	.section __DWARF,__debug_pubnames,regular,debug
Lsection__debug_pubnames:
	.long	0x1e
	.short	0x2
	.set L$set$126,Ldebug_info0-Lsection__debug_info
	.long L$set$126
	.long	0xf9f
	.long	0x999
	.ascii "anubis_main\0"
	.long	0
	.section __DWARF,__debug_pubtypes,regular,debug
Lsection__debug_pubtypes:
	.long	0x4d8
	.short	0x2
	.set L$set$127,Ldebug_info0-Lsection__debug_info
	.long L$set$127
	.long	0xf9f
	.long	0x224
	.ascii "ada__text_io__TcountB\0"
	.long	0x23d
	.ascii "interfaces__unsigned_8\0"
	.long	0x271
	.ascii "anubis_types__TbyteB\0"
	.long	0x289
	.ascii "integer\0"
	.long	0x294
	.ascii "anubis_types__ml_kem_public_key__T33s\0"
	.long	0x2cb
	.ascii "anubis_types__ml_kem_public_key\0"
	.long	0x303
	.ascii "anubis_types__ml_kem_secret_key__T35s\0"
	.long	0x380
	.ascii "boolean\0"
	.long	0x33a
	.ascii "anubis_types__ml_kem_secret_key\0"
	.long	0x38b
	.ascii "anubis_types__ml_kem_ciphertext__T37s\0"
	.long	0x3c2
	.ascii "anubis_types__ml_kem_ciphertext\0"
	.long	0x3fa
	.ascii "anubis_types__ml_kem_shared_secret__T39s\0"
	.long	0x433
	.ascii "anubis_types__ml_kem_shared_secret\0"
	.long	0x47a
	.ascii "anubis_types__ml_dsa_public_key__T41s\0"
	.long	0x4b1
	.ascii "anubis_types__ml_dsa_public_key\0"
	.long	0x4e9
	.ascii "anubis_types__ml_dsa_secret_key__T43s\0"
	.long	0x520
	.ascii "anubis_types__ml_dsa_secret_key\0"
	.long	0x566
	.ascii "anubis_types__ml_dsa_signature__T45s\0"
	.long	0x59c
	.ascii "anubis_types__ml_dsa_signature\0"
	.long	0x5d3
	.ascii "anubis_types__ed25519_public_key__T21s\0"
	.long	0x60a
	.ascii "anubis_types__ed25519_public_key\0"
	.long	0x642
	.ascii "anubis_types__ed25519_secret_key__T23s\0"
	.long	0x679
	.ascii "anubis_types__ed25519_secret_key\0"
	.long	0x6be
	.ascii "anubis_types__ed25519_signature__T25s\0"
	.long	0x6f5
	.ascii "anubis_types__ed25519_signature\0"
	.long	0x72c
	.ascii "anubis_types__pqc__hybrid_signature\0"
	.long	0x784
	.ascii "system__parameters__Tsize_typeB\0"
	.long	0x7a7
	.ascii "system__storage_elements__storage_element\0"
	.long	0x7d4
	.ascii "anubis_types__x25519_public_key__T15s\0"
	.long	0x80a
	.ascii "anubis_types__x25519_public_key\0"
	.long	0x841
	.ascii "anubis_types__x25519_secret_key__T17s\0"
	.long	0x877
	.ascii "anubis_types__x25519_secret_key\0"
	.long	0x8bb
	.ascii "anubis_types__storage__identity_keypair\0"
	.long	0xf91
	.ascii "character\0"
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
	.set L$set$129,LFE5-Ltext0
	.quad L$set$129
	.quad	0
	.quad	0
	.section __DWARF,__debug_ranges,regular,debug
Lsection__debug_ranges:
Ldebug_ranges0:
	.set L$set$130,LBB11-Ltext0
	.quad L$set$130
	.set L$set$131,LBE11-Ltext0
	.quad L$set$131
	.set L$set$132,LBB54-Ltext0
	.quad L$set$132
	.set L$set$133,LBE54-Ltext0
	.quad L$set$133
	.quad	0
	.quad	0
	.set L$set$134,LBB12-Ltext0
	.quad L$set$134
	.set L$set$135,LBE12-Ltext0
	.quad L$set$135
	.set L$set$136,LBB52-Ltext0
	.quad L$set$136
	.set L$set$137,LBE52-Ltext0
	.quad L$set$137
	.quad	0
	.quad	0
	.set L$set$138,LBB25-Ltext0
	.quad L$set$138
	.set L$set$139,LBE25-Ltext0
	.quad L$set$139
	.set L$set$140,LBB40-Ltext0
	.quad L$set$140
	.set L$set$141,LBE40-Ltext0
	.quad L$set$141
	.quad	0
	.quad	0
	.set L$set$142,LBB32-Ltext0
	.quad L$set$142
	.set L$set$143,LBE32-Ltext0
	.quad L$set$143
	.set L$set$144,LBB41-Ltext0
	.quad L$set$144
	.set L$set$145,LBE41-Ltext0
	.quad L$set$145
	.quad	0
	.quad	0
	.set L$set$146,LBB49-Ltext0
	.quad L$set$146
	.set L$set$147,LBE49-Ltext0
	.quad L$set$147
	.set L$set$148,LBB53-Ltext0
	.quad L$set$148
	.set L$set$149,LBE53-Ltext0
	.quad L$set$149
	.quad	0
	.quad	0
	.set L$set$150,LBB95-Ltext0
	.quad L$set$150
	.set L$set$151,LBE95-Ltext0
	.quad L$set$151
	.set L$set$152,LBB105-Ltext0
	.quad L$set$152
	.set L$set$153,LBE105-Ltext0
	.quad L$set$153
	.quad	0
	.quad	0
	.set L$set$154,LBB99-Ltext0
	.quad L$set$154
	.set L$set$155,LBE99-Ltext0
	.quad L$set$155
	.set L$set$156,LBB106-Ltext0
	.quad L$set$156
	.set L$set$157,LBE106-Ltext0
	.quad L$set$157
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
