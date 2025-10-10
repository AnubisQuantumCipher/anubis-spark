	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
Ltext0:
	.file 1 "/Users/sicarii/Desktop/anubis-spark/src/test_comprehensive.adb"
	.const
	.align	3
lC32:
	.ascii "test_comprehensive.adb"
	.space 1
	.align	3
lC33:
	.ascii "... "
	.align	3
lC34:
	.ascii "PASS"
	.align	3
lC35:
	.ascii "FAIL"
	.text
	.align	2
_test_comprehensive__test.1:
LFB2:
	.loc 1 19 4
	sub	x10, sp, #16384
	str	xzr, [x10, 3888]
	stp	x29, x30, [sp, -96]!
LCFI0:
	mov	x29, sp
LCFI1:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
	stp	x25, x26, [sp, 64]
	stp	x27, x28, [sp, 80]
	sub	sp, sp, #112
LCFI2:
	stp	x0, x1, [x29, -80]
	mov	w0, w2
	strb	w0, [x29, -81]
	mov	x19, x16
	str	x16, [x29, -96]
	.loc 1 19 4
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
	blt	L2
	.loc 1 19 4 is_stmt 0 discriminator 1
	sub	w2, w1, w0
	add	w2, w2, 1
	b	L3
L2:
	.loc 1 19 4 discriminator 2
	mov	w2, 0
L3:
LBB2:
	.loc 1 19 4 discriminator 4
	cmp	w1, w0
	.loc 1 19 4 discriminator 8
	cmp	w1, w0
	blt	L7
	.loc 1 19 4 discriminator 9
	sxtw	x16, w1
	sxtw	x3, w0
	sub	x3, x16, x3
	add	x3, x3, 1
	mov	x8, x3
	mov	x9, 0
	lsr	x3, x8, 61
	lsl	x15, x9, 3
	orr	x15, x3, x15
	lsl	x14, x8, 3
L7:
	.loc 1 19 4 discriminator 12
	cmp	w1, w0
	.loc 1 21 32 is_stmt 1
	ldr	w3, [x19, 4]
	mov	w1, 2147483647
	cmp	w3, w1
	bne	L10
	.loc 1 21 32 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L11
	bl	___stack_chk_fail
L11:
	mov	w1, 21
	adrp	x0, lC32@PAGE
	add	x0, x0, lC32@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L10:
	.loc 1 21 32 discriminator 2
	ldr	w1, [x19, 4]
	add	w1, w1, 1
	.loc 1 21 18 is_stmt 1 discriminator 4
	str	w1, [x19, 4]
LBB3:
	.loc 1 22 7
	mov	x1, sp
	mov	x28, x1
	.loc 1 22 17 discriminator 1
	add	w1, w2, 4
	cmp	w2, 0
	bne	L13
	.loc 1 22 17 is_stmt 0 discriminator 2
	mov	w0, 1
L13:
	.loc 1 22 17 discriminator 4
	str	w0, [x29, -56]
	sub	w0, w1, #1
	mov	w3, 0
	ldr	w2, [x29, -56]
	adds	w0, w2, w0
	bvc	L14
	mov	w3, 1
L14:
	.loc 1 22 17 discriminator 1
	mov	w0, w3
	cmp	w0, 0
	beq	L16
	.loc 1 22 17 discriminator 5
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L17
	bl	___stack_chk_fail
L17:
	mov	w1, 22
	adrp	x0, lC32@PAGE
	add	x0, x0, lC32@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L16:
	.loc 1 22 17 discriminator 6
	sub	w0, w1, #1
	ldr	w2, [x29, -56]
	adds	w0, w2, w0
	.loc 1 22 17 discriminator 9
	str	w0, [x29, -52]
	ldrsw	x0, [x29, -56]
	str	x0, [x29, -32]
	ldrsw	x0, [x29, -52]
	str	x0, [x29, -24]
	ldrsw	x2, [x29, -52]
	ldrsw	x0, [x29, -56]
	sub	x0, x2, x0
	add	x0, x0, 1
	mov	x6, x0
	mov	x7, 0
	lsr	x0, x6, 61
	lsl	x13, x7, 3
	orr	x13, x0, x13
	lsl	x12, x6, 3
	cmp	w1, 0
	ble	L20
	.loc 1 22 17 discriminator 10
	ldr	w0, [x29, -56]
	cmp	w0, 0
	bgt	L20
	.loc 1 22 17 discriminator 12
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L21
	bl	___stack_chk_fail
L21:
	mov	w1, 22
	adrp	x0, lC32@PAGE
	add	x0, x0, lC32@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L20:
	.loc 1 22 17 discriminator 13
	ldrsw	x1, [x29, -52]
	ldrsw	x0, [x29, -56]
	sub	x0, x1, x0
	add	x0, x0, 1
	mov	x4, x0
	mov	x5, 0
	lsr	x0, x4, 61
	lsl	x11, x5, 3
	orr	x11, x0, x11
	lsl	x10, x4, 3
	ldrsw	x1, [x29, -52]
	ldrsw	x0, [x29, -56]
	sub	x0, x1, x0
	add	x0, x0, 1
	add	x0, x0, 15
	lsr	x0, x0, 4
	lsl	x1, x0, 4
	and	x3, x1, -4096
	sub	x0, sp, #12288
	sub	x2, x0, x3
L22:
	cmp	x0, x2
	beq	L23
	sub	x0, x0, #4096
	str	xzr, [x0]
	b	L22
L23:
	sub	x0, x1, x3
	sub	x0, x2, x0
	str	xzr, [x0]
	sub	sp, sp, x1
	mov	x0, sp
	.loc 1 22 17 discriminator 16
	str	x0, [x29, -16]
LBB4:
	ldr	x0, [x29, -16]
	str	x0, [x29, -112]
	ldr	w0, [x29, -56]
	str	w0, [x29, -48]
	ldr	w0, [x29, -52]
	str	w0, [x29, -44]
	sub	x0, x29, #48
	str	x0, [x29, -104]
	adrp	x0, lC33@PAGE
	add	x24, x0, lC33@PAGEOFF;
	adrp	x0, lC31@PAGE
	add	x25, x0, lC31@PAGEOFF;
	mov	x4, x24
	mov	x5, x25
	ldp	x2, x3, [x29, -80]
	ldp	x0, x1, [x29, -112]
	bl	_system__concat_2__str_concat_2
LBE4:
	.loc 1 22 7 is_stmt 1 discriminator 18
	ldr	x26, [x29, -16]
	ldr	w0, [x29, -56]
	str	w0, [x29, -40]
	ldr	w0, [x29, -52]
	str	w0, [x29, -36]
	sub	x0, x29, #40
	mov	x27, x0
	mov	x0, x26
	mov	x1, x27
	bl	_ada__text_io__put__4
	.loc 1 22 0 discriminator 20
	mov	sp, x28
LBE3:
	.loc 1 23 7
	ldrb	w0, [x29, -81]
	cmp	w0, 0
	beq	L24
LBB5:
	.loc 1 24 10
	adrp	x0, lC34@PAGE
	add	x22, x0, lC34@PAGEOFF;
	adrp	x0, lC31@PAGE
	add	x23, x0, lC31@PAGEOFF;
	mov	x0, x22
	mov	x1, x23
	bl	_ada__text_io__put_line__2
LBE5:
	.loc 1 25 35
	ldr	w1, [x19]
	mov	w0, 2147483647
	cmp	w1, w0
	bne	L25
	.loc 1 25 35 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L26
	bl	___stack_chk_fail
L26:
	mov	w1, 25
	adrp	x0, lC32@PAGE
	add	x0, x0, lC32@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L25:
	.loc 1 25 35 discriminator 2
	ldr	w0, [x19]
	add	w0, w0, 1
	.loc 1 25 21 is_stmt 1 discriminator 4
	str	w0, [x19]
	.loc 1 29 8
	b	L1
L24:
LBB6:
	.loc 1 27 10
	adrp	x0, lC35@PAGE
	add	x20, x0, lC35@PAGEOFF;
	adrp	x0, lC31@PAGE
	add	x21, x0, lC31@PAGEOFF;
	mov	x0, x20
	mov	x1, x21
	bl	_ada__text_io__put_line__2
LBE6:
	.loc 1 29 8
	nop
L1:
LBE2:
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L29
	bl	___stack_chk_fail
L29:
	mov	sp, x29
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
	ldp	x29, x30, [sp], 96
LCFI3:
	ret
LFE2:
	.const
	.align	2
lC31:
	.word	1
	.word	4
	.text
	.const
	.align	3
lC36:
	.ascii "\342\225\224\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\227"
	.align	3
lC37:
	.ascii "\342\225\221  ANUBIS-SPARK COMPREHENSIVE TEST SUITE               \342\225\221"
	.align	3
lC38:
	.ascii "\342\225\232\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\235"
	.align	3
lC39:
	.ascii "Core Type System"
	.align	3
lC40:
	.ascii "Core types use private implementation (good security)"
	.align	3
lC41:
	.ascii "Classical Cryptography"
	.align	3
lC42:
	.ascii "Ed25519 keygen"
	.align	3
lC43:
	.ascii "Ed25519 sign"
	.align	3
lC44:
	.ascii "Ed25519 verify"
	.align	3
lC45:
	.ascii "Post-Quantum Cryptography"
	.align	3
lC46:
	.ascii "ML-KEM-1024 keygen"
	.align	3
lC47:
	.ascii "ML-KEM-1024 encapsulate"
	.align	3
lC48:
	.ascii "ML-KEM-1024 decapsulate"
	.align	3
lC49:
	.ascii "ML-KEM-1024 secrets match"
	.align	3
lC50:
	.ascii "ML-DSA-87 keygen"
	.align	3
lC51:
	.ascii "ML-DSA-87 sign"
	.align	3
lC52:
	.ascii "ML-DSA-87 verify"
	.align	3
lC53:
	.ascii "Hybrid Operations"
	.align	3
lC54:
	.ascii "ML-DSA keygen"
	.align	3
lC55:
	.ascii "Hybrid sign"
	.align	3
lC56:
	.ascii "Hybrid verify"
	.align	3
lC57:
	.ascii "Shamir Secret Sharing"
	.align	3
lC58:
	.ascii "SSS split (3-of-5)"
	.align	3
lC59:
	.ascii "SSS combine (using shares 1-3)"
	.align	3
lC60:
	.ascii "SSS reconstruction correct"
	.align	3
lC61:
	.ascii "Key Lifecycle Manager"
	.align	3
lC62:
	.ascii "Key manager create"
	.align	3
lC63:
	.ascii "Key manager status active"
	.align	3
lC64:
	.ascii "Key manager initial usage zero"
	.align	3
lC65:
	.ascii "Key manager usage increments"
	.align	3
lC66:
	.ascii "Key manager expire"
	.align	3
lC67:
	.ascii "Key manager destroy"
	.align	3
lC68:
	.ascii "\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220\342\225\220"
	.align	3
lC69:
	.ascii "Test Results:"
	.align	3
lC70:
	.ascii "  Total Tests: "
	.align	3
lC71:
	.ascii "  Passed:      "
	.align	3
lC72:
	.ascii "  Failed:      "
	.align	3
lC73:
	.ascii "  Success Rate:"
	.align	3
lC74:
	.ascii "%"
	.align	3
lC75:
	.ascii "\342\234\223 ALL TESTS PASSED - System Fully Operational"
	.align	3
lC76:
	.ascii "\342\234\227 SOME TESTS FAILED - Review Results Above"
	.text
	.align	2
	.globl __ada_test_comprehensive
__ada_test_comprehensive:
LFB1:
	.loc 1 14 1
	sub	x10, sp, #12288
	mov	x11, -28672
	add	x11, sp, x11
LPSRL0:
	sub	x10, x10, 4096
	str	xzr, [x10, 0]
	cmp	x10, x11
	b.ne	LPSRL0
	sub	x11, x11, #4096
	str	xzr, [x11, 3216]
	stp	x29, x30, [sp, -96]!
LCFI4:
	mov	x29, sp
LCFI5:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
	stp	x25, x26, [sp, 64]
	str	x27, [sp, 80]
	mov	x13, 17168
	sub	sp, sp, x13
LCFI6:
	.loc 1 14 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x1, [x0]
	str	x1, [x29, -8]
	mov	x1, 0
	add	x0, x29, 96
	.loc 1 14 1 is_stmt 0 discriminator 1
	sub	x1, x29, #16384
	str	x0, [x1, 744]
	.loc 1 16 4 is_stmt 1
	mov	w0, 0
	sub	x1, x29, #16384
	str	w0, [x1, 740]
	.loc 1 17 4
	mov	w0, 0
	sub	x1, x29, #16384
	str	w0, [x1, 736]
LBB7:
	.loc 1 38 4
	adrp	x0, lC36@PAGE
	add	x2, x0, lC36@PAGEOFF;
	adrp	x0, lC0@PAGE
	add	x3, x0, lC0@PAGEOFF;
	mov	x0, x2
	mov	x1, x3
	bl	_ada__text_io__put_line__2
LBE7:
LBB8:
	.loc 1 39 4
	adrp	x0, lC37@PAGE
	add	x20, x0, lC37@PAGEOFF;
	adrp	x0, lC1@PAGE
	add	x21, x0, lC1@PAGEOFF;
	mov	x0, x20
	mov	x1, x21
	bl	_ada__text_io__put_line__2
LBE8:
LBB9:
	.loc 1 40 4
	adrp	x0, lC38@PAGE
	add	x22, x0, lC38@PAGEOFF;
	adrp	x0, lC0@PAGE
	add	x23, x0, lC0@PAGEOFF;
	mov	x0, x22
	mov	x1, x23
	bl	_ada__text_io__put_line__2
LBE9:
	.loc 1 41 4
	mov	w0, 1
	bl	_ada__text_io__new_line__2
LBB10:
	.loc 1 47 4
	adrp	x0, lC39@PAGE
	add	x24, x0, lC39@PAGEOFF;
	adrp	x0, lC2@PAGE
	add	x25, x0, lC2@PAGEOFF;
	sub	x0, x29, #12288
	sub	x0, x0, #3360
	mov	x16, x0
	mov	x0, x24
	mov	x1, x25
	bl	_test_comprehensive__print_header.0
LBE10:
LBB11:
	.loc 1 53 4
	adrp	x0, lC40@PAGE
	add	x26, x0, lC40@PAGEOFF;
	adrp	x0, lC3@PAGE
	add	x27, x0, lC3@PAGEOFF;
	mov	x0, x26
	mov	x1, x27
	bl	_ada__text_io__put_line__2
LBE11:
LBB12:
	.loc 1 59 4
	adrp	x0, lC41@PAGE
	add	x0, x0, lC41@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15600]
	adrp	x0, lC4@PAGE
	add	x0, x0, lC4@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15608]
	sub	x0, x29, #12288
	sub	x0, x0, #3360
	mov	x16, x0
	sub	x0, x29, #16384
	sub	x0, x0, #1024
	ldp	x0, x1, [x0, 240]
	bl	_test_comprehensive__print_header.0
LBE12:
LBB13:
	.loc 1 63 7
	sub	x0, x29, #12288
	sub	x0, x0, #672
	bl	_anubis_types__ed25519_secret_keyIP
	.loc 1 68 16
	sub	x1, x29, #12288
	sub	x1, x1, #672
	sub	x0, x29, #12288
	sub	x0, x0, #3264
	bl	_anubis_types__classical__ed25519_generate_keypair
	.loc 1 68 16 is_stmt 0 discriminator 1
	sub	x1, x29, #16384
	strb	w0, [x1, 660]
LBB14:
	.loc 1 69 7 is_stmt 1
	adrp	x0, lC42@PAGE
	add	x0, x0, lC42@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15616]
	adrp	x0, lC5@PAGE
	add	x0, x0, lC5@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15624]
	sub	x0, x29, #16384
	ldrb	w1, [x0, 660]
	sub	x0, x29, #12288
	sub	x0, x0, #3360
	mov	x16, x0
	mov	w2, w1
	sub	x0, x29, #16384
	sub	x0, x0, #512
	ldp	x0, x1, [x0, -256]
	bl	_test_comprehensive__test.1
LBE14:
	.loc 1 71 16
	adrp	x0, _msg.6@PAGE
	add	x0, x0, _msg.6@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15632]
	adrp	x0, lC6@PAGE
	add	x0, x0, lC6@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15640]
	sub	x1, x29, #8192
	sub	x1, x1, #72
	sub	x0, x29, #12288
	sub	x0, x0, #672
	mov	x3, x1
	mov	x2, x0
	sub	x0, x29, #16384
	sub	x0, x0, #512
	ldp	x0, x1, [x0, -240]
	bl	_anubis_types__classical__ed25519_sign
	.loc 1 71 16 is_stmt 0 discriminator 1
	sub	x1, x29, #16384
	strb	w0, [x1, 660]
LBB15:
	.loc 1 72 7 is_stmt 1
	adrp	x0, lC43@PAGE
	add	x0, x0, lC43@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15648]
	adrp	x0, lC7@PAGE
	add	x0, x0, lC7@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15656]
	sub	x0, x29, #16384
	ldrb	w1, [x0, 660]
	sub	x0, x29, #12288
	sub	x0, x0, #3360
	mov	x16, x0
	mov	w2, w1
	sub	x0, x29, #16384
	sub	x0, x0, #512
	ldp	x0, x1, [x0, -224]
	bl	_test_comprehensive__test.1
LBE15:
LBB16:
	.loc 1 74 7
	adrp	x0, lC44@PAGE
	add	x0, x0, lC44@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15664]
	adrp	x0, lC5@PAGE
	add	x0, x0, lC5@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15672]
	adrp	x0, _msg.6@PAGE
	add	x0, x0, _msg.6@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15680]
	adrp	x0, lC6@PAGE
	add	x0, x0, lC6@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15688]
	sub	x1, x29, #12288
	sub	x1, x1, #3264
	sub	x0, x29, #8192
	sub	x0, x0, #72
	mov	x3, x1
	mov	x2, x0
	sub	x0, x29, #16384
	sub	x0, x0, #512
	ldp	x0, x1, [x0, -192]
	bl	_anubis_types__classical__ed25519_verify
	mov	w1, w0
	.loc 1 74 7 is_stmt 0 discriminator 1
	sub	x0, x29, #12288
	sub	x0, x0, #3360
	mov	x16, x0
	mov	w2, w1
	sub	x0, x29, #16384
	sub	x0, x0, #512
	ldp	x0, x1, [x0, -208]
	bl	_test_comprehensive__test.1
LBE16:
	.loc 1 76 16 is_stmt 1
	sub	x0, x29, #12288
	sub	x0, x0, #672
	bl	_anubis_types__classical__zeroize_ed25519_secret
LBE13:
LBB17:
	.loc 1 83 4
	adrp	x0, lC45@PAGE
	add	x0, x0, lC45@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15696]
	adrp	x0, lC8@PAGE
	add	x0, x0, lC8@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15704]
	sub	x0, x29, #12288
	sub	x0, x0, #3360
	mov	x16, x0
	sub	x0, x29, #16384
	sub	x0, x0, #512
	ldp	x0, x1, [x0, -176]
	bl	_test_comprehensive__print_header.0
LBE17:
LBB18:
	.loc 1 87 7
	sub	x0, x29, #8192
	sub	x0, x0, #72
	bl	_anubis_types__ml_kem_secret_keyIP
	.loc 1 89 7
	sub	x0, x29, #12288
	sub	x0, x0, #3344
	bl	_anubis_types__ml_kem_shared_secretIP
	.loc 1 89 7 is_stmt 0 discriminator 1
	sub	x0, x29, #12288
	sub	x0, x0, #3304
	bl	_anubis_types__ml_kem_shared_secretIP
	.loc 1 92 10 is_stmt 1
	sub	x1, x29, #8192
	sub	x1, x1, #72
	sub	x0, x29, #12288
	sub	x0, x0, #3264
	bl	_anubis_types__pqc__ml_kem_generate_keypair
	.loc 1 92 10 is_stmt 0 discriminator 1
	sub	x1, x29, #16384
	strb	w0, [x1, 661]
LBB19:
	.loc 1 93 7 is_stmt 1
	adrp	x0, lC46@PAGE
	add	x0, x0, lC46@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15712]
	adrp	x0, lC9@PAGE
	add	x0, x0, lC9@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15720]
	sub	x0, x29, #16384
	ldrb	w1, [x0, 661]
	sub	x0, x29, #12288
	sub	x0, x0, #3360
	mov	x16, x0
	mov	w2, w1
	sub	x0, x29, #16384
	sub	x0, x0, #512
	ldp	x0, x1, [x0, -160]
	bl	_test_comprehensive__test.1
LBE19:
	.loc 1 95 10
	sub	x2, x29, #12288
	sub	x2, x2, #3344
	sub	x1, x29, #12288
	sub	x1, x1, #672
	sub	x0, x29, #12288
	sub	x0, x0, #3264
	bl	_anubis_types__pqc__ml_kem_encapsulate
	.loc 1 95 10 is_stmt 0 discriminator 1
	sub	x1, x29, #16384
	strb	w0, [x1, 661]
LBB20:
	.loc 1 96 7 is_stmt 1
	adrp	x0, lC47@PAGE
	add	x0, x0, lC47@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15728]
	adrp	x0, lC10@PAGE
	add	x0, x0, lC10@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15736]
	sub	x0, x29, #16384
	ldrb	w1, [x0, 661]
	sub	x0, x29, #12288
	sub	x0, x0, #3360
	mov	x16, x0
	mov	w2, w1
	sub	x0, x29, #16384
	sub	x0, x0, #512
	ldp	x0, x1, [x0, -144]
	bl	_test_comprehensive__test.1
LBE20:
	.loc 1 98 10
	sub	x2, x29, #12288
	sub	x2, x2, #3304
	sub	x1, x29, #8192
	sub	x1, x1, #72
	sub	x0, x29, #12288
	sub	x0, x0, #672
	bl	_anubis_types__pqc__ml_kem_decapsulate
	.loc 1 98 10 is_stmt 0 discriminator 1
	sub	x1, x29, #16384
	strb	w0, [x1, 661]
LBB21:
	.loc 1 99 7 is_stmt 1
	adrp	x0, lC48@PAGE
	add	x0, x0, lC48@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15744]
	adrp	x0, lC10@PAGE
	add	x0, x0, lC10@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15752]
	sub	x0, x29, #16384
	ldrb	w1, [x0, 661]
	sub	x0, x29, #12288
	sub	x0, x0, #3360
	mov	x16, x0
	mov	w2, w1
	sub	x0, x29, #16384
	sub	x0, x0, #512
	ldp	x0, x1, [x0, -128]
	bl	_test_comprehensive__test.1
LBE21:
LBB22:
	.loc 1 101 7
	adrp	x0, lC49@PAGE
	add	x0, x0, lC49@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15760]
	adrp	x0, lC8@PAGE
	add	x0, x0, lC8@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15768]
	sub	x1, x29, #12288
	sub	x1, x1, #3304
	sub	x0, x29, #12288
	sub	x0, x0, #3344
	bl	_anubis_types__pqc__secrets_match
	mov	w1, w0
	.loc 1 101 7 is_stmt 0 discriminator 1
	sub	x0, x29, #12288
	sub	x0, x0, #3360
	mov	x16, x0
	mov	w2, w1
	sub	x0, x29, #16384
	sub	x0, x0, #512
	ldp	x0, x1, [x0, -112]
	bl	_test_comprehensive__test.1
LBE22:
	.loc 1 103 10 is_stmt 1
	sub	x0, x29, #8192
	sub	x0, x0, #72
	bl	_anubis_types__pqc__zeroize_ml_kem_secret
LBE18:
LBB23:
	.loc 1 108 7
	sub	x0, x29, #8192
	sub	x0, x0, #72
	bl	_anubis_types__ml_dsa_secret_keyIP
	.loc 1 113 10
	sub	x1, x29, #8192
	sub	x1, x1, #72
	sub	x0, x29, #12288
	sub	x0, x0, #3264
	bl	_anubis_types__pqc__ml_dsa_generate_keypair
	.loc 1 113 10 is_stmt 0 discriminator 1
	sub	x1, x29, #16384
	strb	w0, [x1, 662]
LBB24:
	.loc 1 114 7 is_stmt 1
	adrp	x0, lC50@PAGE
	add	x0, x0, lC50@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15776]
	adrp	x0, lC2@PAGE
	add	x0, x0, lC2@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15784]
	sub	x0, x29, #16384
	ldrb	w1, [x0, 662]
	sub	x0, x29, #12288
	sub	x0, x0, #3360
	mov	x16, x0
	mov	w2, w1
	sub	x0, x29, #16384
	sub	x0, x0, #512
	ldp	x0, x1, [x0, -96]
	bl	_test_comprehensive__test.1
LBE24:
	.loc 1 116 10
	adrp	x0, _msg.5@PAGE
	add	x0, x0, _msg.5@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15792]
	adrp	x0, lC6@PAGE
	add	x0, x0, lC6@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15800]
	sub	x1, x29, #12288
	sub	x1, x1, #672
	sub	x0, x29, #8192
	sub	x0, x0, #72
	mov	x3, x1
	mov	x2, x0
	sub	x0, x29, #16384
	sub	x0, x0, #512
	ldp	x0, x1, [x0, -80]
	bl	_anubis_types__pqc__ml_dsa_sign
	.loc 1 116 10 is_stmt 0 discriminator 1
	sub	x1, x29, #16384
	strb	w0, [x1, 662]
LBB25:
	.loc 1 117 7 is_stmt 1
	adrp	x0, lC51@PAGE
	add	x0, x0, lC51@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15808]
	adrp	x0, lC5@PAGE
	add	x0, x0, lC5@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15816]
	sub	x0, x29, #16384
	ldrb	w1, [x0, 662]
	sub	x0, x29, #12288
	sub	x0, x0, #3360
	mov	x16, x0
	mov	w2, w1
	sub	x0, x29, #16384
	sub	x0, x0, #512
	ldp	x0, x1, [x0, -64]
	bl	_test_comprehensive__test.1
LBE25:
LBB26:
	.loc 1 119 7
	adrp	x0, lC52@PAGE
	add	x0, x0, lC52@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15824]
	adrp	x0, lC2@PAGE
	add	x0, x0, lC2@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15832]
	adrp	x0, _msg.5@PAGE
	add	x0, x0, _msg.5@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15840]
	adrp	x0, lC6@PAGE
	add	x0, x0, lC6@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15848]
	sub	x1, x29, #12288
	sub	x1, x1, #3264
	sub	x0, x29, #12288
	sub	x0, x0, #672
	mov	x3, x1
	mov	x2, x0
	sub	x0, x29, #16384
	sub	x0, x0, #512
	ldp	x0, x1, [x0, -32]
	bl	_anubis_types__pqc__ml_dsa_verify
	mov	w1, w0
	.loc 1 119 7 is_stmt 0 discriminator 1
	sub	x0, x29, #12288
	sub	x0, x0, #3360
	mov	x16, x0
	mov	w2, w1
	sub	x0, x29, #16384
	sub	x0, x0, #512
	ldp	x0, x1, [x0, -48]
	bl	_test_comprehensive__test.1
LBE26:
	.loc 1 121 10 is_stmt 1
	sub	x0, x29, #8192
	sub	x0, x0, #72
	bl	_anubis_types__pqc__zeroize_ml_dsa_secret
LBE23:
LBB27:
	.loc 1 128 4
	adrp	x0, lC53@PAGE
	add	x0, x0, lC53@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15856]
	adrp	x0, lC11@PAGE
	add	x0, x0, lC11@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15864]
	sub	x0, x29, #12288
	sub	x0, x0, #3360
	mov	x16, x0
	sub	x0, x29, #16384
	sub	x0, x0, #512
	ldp	x0, x1, [x0, -16]
	bl	_test_comprehensive__print_header.0
LBE27:
LBB28:
	.loc 1 132 7
	sub	x0, x29, #12288
	sub	x0, x0, #3304
	bl	_anubis_types__ed25519_secret_keyIP
	.loc 1 134 7
	sub	x0, x29, #8192
	sub	x0, x0, #72
	bl	_anubis_types__ml_dsa_secret_keyIP
	.loc 1 139 16
	sub	x1, x29, #12288
	sub	x1, x1, #3304
	sub	x0, x29, #12288
	sub	x0, x0, #3344
	bl	_anubis_types__classical__ed25519_generate_keypair
	.loc 1 139 16 is_stmt 0 discriminator 1
	sub	x1, x29, #16384
	strb	w0, [x1, 663]
LBB29:
	.loc 1 140 7 is_stmt 1
	adrp	x0, lC42@PAGE
	add	x0, x0, lC42@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15872]
	adrp	x0, lC5@PAGE
	add	x0, x0, lC5@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15880]
	sub	x0, x29, #16384
	ldrb	w1, [x0, 663]
	sub	x0, x29, #12288
	sub	x0, x0, #3360
	mov	x16, x0
	mov	w2, w1
	sub	x0, x29, #16384
	sub	x0, x0, #512
	ldp	x0, x1, [x0]
	bl	_test_comprehensive__test.1
LBE29:
	.loc 1 142 10
	sub	x1, x29, #8192
	sub	x1, x1, #72
	sub	x0, x29, #12288
	sub	x0, x0, #3264
	bl	_anubis_types__pqc__ml_dsa_generate_keypair
	.loc 1 142 10 is_stmt 0 discriminator 1
	sub	x1, x29, #16384
	strb	w0, [x1, 663]
LBB30:
	.loc 1 143 7 is_stmt 1
	adrp	x0, lC54@PAGE
	add	x0, x0, lC54@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15888]
	adrp	x0, lC12@PAGE
	add	x0, x0, lC12@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15896]
	sub	x0, x29, #16384
	ldrb	w1, [x0, 663]
	sub	x0, x29, #12288
	sub	x0, x0, #3360
	mov	x16, x0
	mov	w2, w1
	sub	x0, x29, #16384
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 16]
	bl	_test_comprehensive__test.1
LBE30:
	.loc 1 145 10
	adrp	x0, _msg.4@PAGE
	add	x0, x0, _msg.4@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15904]
	adrp	x0, lC13@PAGE
	add	x0, x0, lC13@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15912]
	sub	x2, x29, #12288
	sub	x2, x2, #672
	sub	x1, x29, #8192
	sub	x1, x1, #72
	sub	x0, x29, #12288
	sub	x0, x0, #3304
	mov	x4, x2
	mov	x3, x1
	mov	x2, x0
	sub	x0, x29, #16384
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 32]
	bl	_anubis_types__pqc__hybrid_sign
	.loc 1 145 10 is_stmt 0 discriminator 1
	sub	x1, x29, #16384
	strb	w0, [x1, 663]
LBB31:
	.loc 1 146 7 is_stmt 1
	adrp	x0, lC55@PAGE
	add	x0, x0, lC55@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15920]
	adrp	x0, lC14@PAGE
	add	x0, x0, lC14@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15928]
	sub	x0, x29, #16384
	ldrb	w1, [x0, 663]
	sub	x0, x29, #12288
	sub	x0, x0, #3360
	mov	x16, x0
	mov	w2, w1
	sub	x0, x29, #16384
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 48]
	bl	_test_comprehensive__test.1
LBE31:
LBB32:
	.loc 1 148 7
	adrp	x0, lC56@PAGE
	add	x0, x0, lC56@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15936]
	adrp	x0, lC12@PAGE
	add	x0, x0, lC12@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15944]
	adrp	x0, _msg.4@PAGE
	add	x0, x0, _msg.4@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15952]
	adrp	x0, lC13@PAGE
	add	x0, x0, lC13@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15960]
	sub	x2, x29, #12288
	sub	x2, x2, #3264
	sub	x1, x29, #12288
	sub	x1, x1, #3344
	sub	x0, x29, #12288
	sub	x0, x0, #672
	mov	x4, x2
	mov	x3, x1
	mov	x2, x0
	sub	x0, x29, #16384
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 80]
	bl	_anubis_types__pqc__hybrid_verify
	mov	w1, w0
	.loc 1 148 7 is_stmt 0 discriminator 1
	sub	x0, x29, #12288
	sub	x0, x0, #3360
	mov	x16, x0
	mov	w2, w1
	sub	x0, x29, #16384
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 64]
	bl	_test_comprehensive__test.1
LBE32:
	.loc 1 150 16 is_stmt 1
	sub	x0, x29, #12288
	sub	x0, x0, #3304
	bl	_anubis_types__classical__zeroize_ed25519_secret
	.loc 1 151 10
	sub	x0, x29, #8192
	sub	x0, x0, #72
	bl	_anubis_types__pqc__zeroize_ml_dsa_secret
LBE28:
LBB33:
	.loc 1 158 4
	adrp	x0, lC57@PAGE
	add	x0, x0, lC57@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15968]
	adrp	x0, lC15@PAGE
	add	x0, x0, lC15@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15976]
	sub	x0, x29, #12288
	sub	x0, x0, #3360
	mov	x16, x0
	sub	x0, x29, #16384
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 96]
	bl	_test_comprehensive__print_header.0
LBE33:
LBB34:
	.loc 1 162 7
	sub	x0, x29, #8192
	sub	x0, x0, #72
	sub	x1, x29, #32768
	str	x0, [x1, 15984]
	adrp	x0, lC16@PAGE
	add	x0, x0, lC16@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 15992]
	sub	x0, x29, #16384
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 112]
	bl	_anubis_types__sss__share_arrayIP
	.loc 1 167 10
	adrp	x0, _secret.3@PAGE
	add	x0, x0, _secret.3@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16000]
	adrp	x0, lC17@PAGE
	add	x0, x0, lC17@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16008]
	sub	x0, x29, #8192
	sub	x0, x0, #72
	sub	x1, x29, #32768
	str	x0, [x1, 16016]
	adrp	x0, lC16@PAGE
	add	x0, x0, lC16@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16024]
	sub	x0, x29, #16384
	sub	x0, x0, #512
	ldp	x4, x5, [x0, 144]
	mov	w3, 5
	mov	w2, 3
	sub	x0, x29, #16384
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 128]
	bl	_anubis_types__sss__split_secret
	.loc 1 167 10 is_stmt 0 discriminator 1
	sub	x1, x29, #16384
	strb	w0, [x1, 664]
LBB35:
	.loc 1 174 7 is_stmt 1
	adrp	x0, lC58@PAGE
	add	x0, x0, lC58@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16032]
	adrp	x0, lC9@PAGE
	add	x0, x0, lC9@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16040]
	sub	x0, x29, #16384
	ldrb	w1, [x0, 664]
	sub	x0, x29, #12288
	sub	x0, x0, #3360
	mov	x16, x0
	mov	w2, w1
	sub	x0, x29, #16384
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 160]
	bl	_test_comprehensive__test.1
LBE35:
	.loc 1 176 10
	sub	x0, x29, #8192
	sub	x0, x0, #72
	sub	x1, x29, #32768
	str	x0, [x1, 16048]
	adrp	x0, lC18@PAGE
	add	x0, x0, lC18@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16056]
	sub	x0, x29, #12288
	sub	x0, x0, #672
	sub	x1, x29, #32768
	str	x0, [x1, 16064]
	adrp	x0, lC19@PAGE
	add	x0, x0, lC19@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16072]
	sub	x0, x29, #16384
	sub	x0, x0, #512
	ldp	x3, x4, [x0, 192]
	mov	w2, 3
	sub	x0, x29, #16384
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 176]
	bl	_anubis_types__sss__combine_shares
	.loc 1 176 10 is_stmt 0 discriminator 1
	sub	x1, x29, #16384
	strb	w0, [x1, 664]
LBB36:
	.loc 1 182 7 is_stmt 1
	adrp	x0, lC59@PAGE
	add	x0, x0, lC59@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16080]
	adrp	x0, lC20@PAGE
	add	x0, x0, lC20@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16088]
	sub	x0, x29, #16384
	ldrb	w1, [x0, 664]
	sub	x0, x29, #12288
	sub	x0, x0, #3360
	mov	x16, x0
	mov	w2, w1
	sub	x0, x29, #16384
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 208]
	bl	_test_comprehensive__test.1
LBE36:
	.loc 1 185 13
	mov	w0, 1
	sub	x1, x29, #16384
	strb	w0, [x1, 659]
LBB37:
	.loc 1 186 11
	sub	x0, x29, #16384
	str	wzr, [x0, 668]
L35:
	.loc 1 186 11 is_stmt 0 discriminator 1
	sub	x0, x29, #16384
	ldr	w0, [x0, 668]
	cmp	w0, 7
	bgt	L31
	.loc 1 187 27 is_stmt 1
	sub	x0, x29, #16384
	ldr	w0, [x0, 668]
	cmp	w0, 0
	bgt	L32
	.loc 1 187 27 is_stmt 0 discriminator 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L33
	bl	___stack_chk_fail
L33:
	mov	w1, 187
	adrp	x0, lC32@PAGE
	add	x0, x0, lC32@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L32:
	.loc 1 187 24 is_stmt 1 discriminator 2
	sub	x0, x29, #16384
	ldrsw	x1, [x0, 668]
	adrp	x0, _secret.3@PAGE
	add	x0, x0, _secret.3@PAGEOFF;
	ldrb	w2, [x0, x1]
	.loc 1 187 24 is_stmt 0 discriminator 3
	sub	x0, x29, #16384
	ldrsw	x1, [x0, 668]
	sub	x0, x29, #12288
	sub	x0, x0, #673
	ldrb	w0, [x0, x1]
	.loc 1 187 10 is_stmt 1 discriminator 4
	cmp	w2, w0
	beq	L34
	.loc 1 188 19
	sub	x0, x29, #16384
	strb	wzr, [x0, 659]
L34:
	.loc 1 186 11 discriminator 2
	sub	x0, x29, #16384
	ldr	w0, [x0, 668]
	add	w0, w0, 1
	sub	x1, x29, #16384
	str	w0, [x1, 668]
	.loc 1 190 15
	b	L35
L31:
LBE37:
LBB38:
	.loc 1 191 7
	adrp	x0, lC60@PAGE
	add	x0, x0, lC60@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16096]
	adrp	x0, lC21@PAGE
	add	x0, x0, lC21@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16104]
	sub	x0, x29, #16384
	ldrb	w1, [x0, 659]
	sub	x0, x29, #12288
	sub	x0, x0, #3360
	mov	x16, x0
	mov	w2, w1
	sub	x0, x29, #16384
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 224]
	bl	_test_comprehensive__test.1
LBE38:
LBB39:
	.loc 1 193 11
	mov	w0, 1
	sub	x1, x29, #16384
	strh	w0, [x1, 666]
L37:
	.loc 1 193 11 is_stmt 0 discriminator 1
	sub	x0, x29, #16384
	ldrsh	w0, [x0, 666]
	cmp	w0, 5
	bgt	L58
	.loc 1 194 13 is_stmt 1
	sub	x0, x29, #16384
	ldrsh	x1, [x0, 666]
	sub	x2, x29, #8192
	sub	x2, x2, #72
	mov	x0, x1
	lsl	x0, x0, 4
	add	x0, x0, x1
	lsl	x0, x0, 2
	sub	x0, x0, x1
	lsl	x0, x0, 2
	sub	x0, x0, #268
	add	x0, x2, x0
	bl	_anubis_types__sss__zeroize_share
	.loc 1 193 11 discriminator 2
	sub	x0, x29, #16384
	ldrh	w0, [x0, 666]
	add	w0, w0, 1
	sub	x1, x29, #16384
	strh	w0, [x1, 666]
	.loc 1 195 15
	b	L37
L58:
LBE39:
LBE34:
LBB40:
	.loc 1 202 4
	adrp	x0, lC61@PAGE
	add	x0, x0, lC61@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16112]
	adrp	x0, lC15@PAGE
	add	x0, x0, lC15@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16120]
	sub	x0, x29, #12288
	sub	x0, x0, #3360
	mov	x16, x0
	sub	x0, x29, #16384
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 240]
	bl	_test_comprehensive__print_header.0
LBE40:
LBB41:
	.loc 1 207 7
	sub	x0, x29, #8192
	sub	x0, x0, #72
	bl	_anubis_key_manager__managed_keyIP
	.loc 1 210 7
	adrp	x0, _key_data.2@PAGE
	add	x0, x0, _key_data.2@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16128]
	adrp	x0, lC22@PAGE
	add	x0, x0, lC22@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16136]
	sub	x2, x29, #8192
	sub	x2, x2, #72
	adrp	x0, _anubis_key_manager__default_rotation@GOTPAGE
	ldr	x0, [x0, _anubis_key_manager__default_rotation@GOTPAGEOFF]
	ldr	x1, [x0]
	ldr	w0, [x0, 8]
	mov	x5, x2
	mov	x3, x1
	mov	x4, x0
	mov	w2, 0
	sub	x0, x29, #16384
	ldp	x0, x1, [x0, -256]
	bl	_anubis_key_manager__create_managed_key
	.loc 1 210 7 is_stmt 0 discriminator 1
	sub	x1, x29, #16384
	strb	w0, [x1, 665]
LBB42:
	.loc 1 217 7 is_stmt 1
	adrp	x0, lC62@PAGE
	add	x0, x0, lC62@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16144]
	adrp	x0, lC9@PAGE
	add	x0, x0, lC9@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16152]
	sub	x0, x29, #16384
	ldrb	w1, [x0, 665]
	sub	x0, x29, #12288
	sub	x0, x0, #3360
	mov	x16, x0
	mov	w2, w1
	sub	x0, x29, #16384
	ldp	x0, x1, [x0, -240]
	bl	_test_comprehensive__test.1
LBE42:
LBB43:
	.loc 1 218 7
	adrp	x0, lC63@PAGE
	add	x0, x0, lC63@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16160]
	adrp	x0, lC8@PAGE
	add	x0, x0, lC8@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16168]
	.loc 1 218 42
	sub	x0, x29, #8192
	sub	x0, x0, #72
	bl	_anubis_key_manager__get_key_status
	.loc 1 218 7 discriminator 1
	cmp	w0, 1
	cset	w0, eq
	and	w1, w0, 255
	sub	x0, x29, #12288
	sub	x0, x0, #3360
	mov	x16, x0
	mov	w2, w1
	sub	x0, x29, #16384
	ldp	x0, x1, [x0, -224]
	bl	_test_comprehensive__test.1
LBE43:
LBB44:
	.loc 1 219 7
	adrp	x0, lC64@PAGE
	add	x0, x0, lC64@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16176]
	adrp	x0, lC20@PAGE
	add	x0, x0, lC20@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16184]
	.loc 1 219 47
	sub	x0, x29, #8192
	sub	x0, x0, #72
	bl	_anubis_key_manager__get_usage_count
	.loc 1 219 7 discriminator 1
	cmp	w0, 0
	cset	w0, eq
	and	w1, w0, 255
	sub	x0, x29, #12288
	sub	x0, x0, #3360
	mov	x16, x0
	mov	w2, w1
	sub	x0, x29, #16384
	ldp	x0, x1, [x0, -208]
	bl	_test_comprehensive__test.1
LBE44:
	.loc 1 221 7
	sub	x0, x29, #8192
	sub	x0, x0, #72
	bl	_anubis_key_manager__record_usage
LBB45:
	.loc 1 222 7
	adrp	x0, lC65@PAGE
	add	x0, x0, lC65@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16192]
	adrp	x0, lC23@PAGE
	add	x0, x0, lC23@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16200]
	.loc 1 222 45
	sub	x0, x29, #8192
	sub	x0, x0, #72
	bl	_anubis_key_manager__get_usage_count
	.loc 1 222 7 discriminator 1
	cmp	w0, 1
	cset	w0, eq
	and	w1, w0, 255
	sub	x0, x29, #12288
	sub	x0, x0, #3360
	mov	x16, x0
	mov	w2, w1
	sub	x0, x29, #16384
	ldp	x0, x1, [x0, -192]
	bl	_test_comprehensive__test.1
LBE45:
	.loc 1 224 7
	sub	x0, x29, #8192
	sub	x0, x0, #72
	bl	_anubis_key_manager__expire_key
LBB46:
	.loc 1 225 7
	adrp	x0, lC66@PAGE
	add	x0, x0, lC66@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16208]
	adrp	x0, lC9@PAGE
	add	x0, x0, lC9@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16216]
	.loc 1 225 35
	sub	x0, x29, #8192
	sub	x0, x0, #72
	bl	_anubis_key_manager__get_key_status
	.loc 1 225 7 discriminator 1
	cmp	w0, 2
	cset	w0, eq
	and	w1, w0, 255
	sub	x0, x29, #12288
	sub	x0, x0, #3360
	mov	x16, x0
	mov	w2, w1
	sub	x0, x29, #16384
	ldp	x0, x1, [x0, -176]
	bl	_test_comprehensive__test.1
LBE46:
	.loc 1 227 7
	sub	x0, x29, #8192
	sub	x0, x0, #72
	bl	_anubis_key_manager__destroy_key
LBB47:
	.loc 1 228 7
	adrp	x0, lC67@PAGE
	add	x0, x0, lC67@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16224]
	adrp	x0, lC24@PAGE
	add	x0, x0, lC24@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16232]
	.loc 1 228 36
	sub	x0, x29, #8192
	sub	x0, x0, #72
	bl	_anubis_key_manager__get_key_status
	.loc 1 228 7 discriminator 1
	cmp	w0, 4
	cset	w0, eq
	and	w1, w0, 255
	sub	x0, x29, #12288
	sub	x0, x0, #3360
	mov	x16, x0
	mov	w2, w1
	sub	x0, x29, #16384
	ldp	x0, x1, [x0, -160]
	bl	_test_comprehensive__test.1
LBE47:
LBE41:
	.loc 1 235 4
	mov	w0, 1
	bl	_ada__text_io__new_line__2
LBB48:
	.loc 1 236 4
	adrp	x0, lC68@PAGE
	add	x0, x0, lC68@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16240]
	adrp	x0, lC25@PAGE
	add	x0, x0, lC25@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16248]
	sub	x0, x29, #16384
	ldp	x0, x1, [x0, -144]
	bl	_ada__text_io__put_line__2
LBE48:
LBB49:
	.loc 1 237 4
	adrp	x0, lC69@PAGE
	add	x0, x0, lC69@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16256]
	adrp	x0, lC12@PAGE
	add	x0, x0, lC12@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16264]
	sub	x0, x29, #16384
	ldp	x0, x1, [x0, -128]
	bl	_ada__text_io__put_line__2
LBE49:
LBB50:
	.loc 1 238 41
	sub	x0, x29, #8192
	sub	x0, x0, #72
	sub	x1, x29, #32768
	str	x0, [x1, 16272]
	adrp	x0, lC14@PAGE
	add	x0, x0, lC14@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16280]
	sub	x0, x29, #16384
	ldr	w0, [x0, 740]
	sub	x1, x29, #16384
	ldp	x1, x2, [x1, -112]
	bl	_system__img_int__impl__image_integer
	mov	w1, w0
	.loc 1 238 41 is_stmt 0 discriminator 2
	bic	w0, w1, w1, asr #31
	sxtw	x0, w0
	sub	x2, x29, #16384
	str	x0, [x2, 448]
	sub	x0, x29, #16384
	str	xzr, [x0, 456]
	sub	x0, x29, #12288
	sub	x0, x0, #3584
	ldp	x2, x3, [x0, -64]
	mov	x0, x2
	lsr	x0, x0, 61
	mov	x4, x3
	lsl	x4, x4, 3
	sub	x5, x29, #16384
	str	x4, [x5, 648]
	sub	x4, x29, #16384
	ldr	x4, [x4, 648]
	orr	x0, x0, x4
	sub	x4, x29, #16384
	str	x0, [x4, 648]
	mov	x0, x2
	lsl	x0, x0, 3
	sub	x2, x29, #16384
	str	x0, [x2, 640]
	bic	w0, w1, w1, asr #31
	sxtw	x0, w0
	sub	x2, x29, #16384
	str	x0, [x2, 432]
	sub	x0, x29, #16384
	str	xzr, [x0, 440]
	sub	x0, x29, #12288
	sub	x0, x0, #3584
	ldp	x2, x3, [x0, -80]
	mov	x0, x2
	lsr	x0, x0, 61
	mov	x4, x3
	lsl	x4, x4, 3
	sub	x5, x29, #16384
	str	x4, [x5, 632]
	sub	x4, x29, #16384
	ldr	x4, [x4, 632]
	orr	x0, x0, x4
	sub	x4, x29, #16384
	str	x0, [x4, 632]
	mov	x0, x2
	lsl	x0, x0, 3
	sub	x2, x29, #16384
	str	x0, [x2, 624]
	.loc 1 238 32 is_stmt 1 discriminator 2
	bic	w0, w1, w1, asr #31
	add	w19, w0, 15
LBB51:
	sub	x0, x29, #40
	sub	x2, x29, #32768
	str	x0, [x2, 16288]
	adrp	x0, lC21@PAGE
	add	x0, x0, lC21@PAGEOFF;
	sub	x2, x29, #32768
	str	x0, [x2, 16296]
	adrp	x0, lC70@PAGE
	add	x0, x0, lC70@PAGEOFF;
	sub	x2, x29, #32768
	str	x0, [x2, 16304]
	adrp	x0, lC26@PAGE
	add	x0, x0, lC26@PAGEOFF;
	sub	x2, x29, #32768
	str	x0, [x2, 16312]
	sub	x0, x29, #8192
	sub	x0, x0, #72
	sub	x2, x29, #32768
	str	x0, [x2, 16320]
	mov	w0, 1
	sub	x2, x29, #16384
	str	w0, [x2, 672]
	sub	x0, x29, #16384
	str	w1, [x0, 676]
	sub	x0, x29, #12288
	sub	x0, x0, #3424
	sub	x1, x29, #32768
	str	x0, [x1, 16328]
	sub	x0, x29, #16384
	ldp	x4, x5, [x0, -64]
	sub	x0, x29, #16384
	ldp	x2, x3, [x0, -80]
	sub	x0, x29, #16384
	ldp	x0, x1, [x0, -96]
	bl	_system__concat_2__str_concat_2
LBE51:
	.loc 1 238 32 is_stmt 0 discriminator 4
	cmp	w19, 26
	ble	L38
	.loc 1 238 32 discriminator 5
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L39
	bl	___stack_chk_fail
L39:
	mov	w1, 238
	adrp	x0, lC32@PAGE
	add	x0, x0, lC32@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L38:
	.loc 1 238 32 discriminator 6
	bic	w0, w19, w19, asr #31
	sxtw	x0, w0
	sub	x1, x29, #16384
	str	x0, [x1, 416]
	sub	x0, x29, #16384
	str	xzr, [x0, 424]
	sub	x0, x29, #12288
	sub	x0, x0, #3584
	ldp	x1, x2, [x0, -96]
	mov	x0, x1
	lsr	x0, x0, 61
	mov	x3, x2
	lsl	x3, x3, 3
	sub	x4, x29, #16384
	str	x3, [x4, 616]
	sub	x3, x29, #16384
	ldr	x3, [x3, 616]
	orr	x0, x0, x3
	sub	x3, x29, #16384
	str	x0, [x3, 616]
	mov	x0, x1
	lsl	x0, x0, 3
	sub	x1, x29, #16384
	str	x0, [x1, 608]
	.loc 1 238 4 is_stmt 1 discriminator 6
	sub	x0, x29, #40
	sub	x1, x29, #32768
	str	x0, [x1, 16336]
	mov	w0, 1
	sub	x1, x29, #16384
	str	w0, [x1, 680]
	sub	x0, x29, #16384
	str	w19, [x0, 684]
	sub	x0, x29, #12288
	sub	x0, x0, #3416
	sub	x1, x29, #32768
	str	x0, [x1, 16344]
	sub	x0, x29, #16384
	ldp	x0, x1, [x0, -48]
	bl	_ada__text_io__put_line__2
LBE50:
LBB52:
	.loc 1 239 41
	sub	x0, x29, #8192
	sub	x0, x0, #72
	sub	x1, x29, #32768
	str	x0, [x1, 16352]
	adrp	x0, lC14@PAGE
	add	x0, x0, lC14@PAGEOFF;
	sub	x1, x29, #32768
	str	x0, [x1, 16360]
	sub	x0, x29, #16384
	ldr	w0, [x0, 736]
	sub	x1, x29, #16384
	ldp	x1, x2, [x1, -32]
	bl	_system__img_int__impl__image_integer
	mov	w1, w0
	.loc 1 239 41 is_stmt 0 discriminator 2
	bic	w0, w1, w1, asr #31
	sxtw	x0, w0
	sub	x2, x29, #16384
	str	x0, [x2, 400]
	sub	x0, x29, #16384
	str	xzr, [x0, 408]
	sub	x0, x29, #12288
	sub	x0, x0, #3584
	ldp	x2, x3, [x0, -112]
	mov	x0, x2
	lsr	x0, x0, 61
	mov	x4, x3
	lsl	x4, x4, 3
	sub	x5, x29, #16384
	str	x4, [x5, 600]
	sub	x4, x29, #16384
	ldr	x4, [x4, 600]
	orr	x0, x0, x4
	sub	x4, x29, #16384
	str	x0, [x4, 600]
	mov	x0, x2
	lsl	x0, x0, 3
	sub	x2, x29, #16384
	str	x0, [x2, 592]
	bic	w0, w1, w1, asr #31
	sxtw	x0, w0
	sub	x2, x29, #16384
	str	x0, [x2, 384]
	sub	x0, x29, #16384
	str	xzr, [x0, 392]
	sub	x0, x29, #12288
	sub	x0, x0, #3584
	ldp	x2, x3, [x0, -128]
	mov	x0, x2
	lsr	x0, x0, 61
	mov	x4, x3
	lsl	x4, x4, 3
	sub	x5, x29, #16384
	str	x4, [x5, 584]
	sub	x4, x29, #16384
	ldr	x4, [x4, 584]
	orr	x0, x0, x4
	sub	x4, x29, #16384
	str	x0, [x4, 584]
	mov	x0, x2
	lsl	x0, x0, 3
	sub	x2, x29, #16384
	str	x0, [x2, 576]
	.loc 1 239 32 is_stmt 1 discriminator 2
	bic	w0, w1, w1, asr #31
	add	w19, w0, 15
LBB53:
	sub	x0, x29, #40
	sub	x2, x29, #32768
	str	x0, [x2, 16368]
	adrp	x0, lC21@PAGE
	add	x0, x0, lC21@PAGEOFF;
	sub	x2, x29, #32768
	str	x0, [x2, 16376]
	adrp	x0, lC71@PAGE
	add	x0, x0, lC71@PAGEOFF;
	sub	x2, x29, #16384
	str	x0, [x2]
	adrp	x0, lC26@PAGE
	add	x0, x0, lC26@PAGEOFF;
	sub	x2, x29, #16384
	str	x0, [x2, 8]
	sub	x0, x29, #8192
	sub	x0, x0, #72
	sub	x2, x29, #16384
	str	x0, [x2, 16]
	mov	w0, 1
	sub	x2, x29, #16384
	str	w0, [x2, 688]
	sub	x0, x29, #16384
	str	w1, [x0, 692]
	sub	x0, x29, #12288
	sub	x0, x0, #3408
	sub	x1, x29, #16384
	str	x0, [x1, 24]
	sub	x0, x29, #16384
	ldp	x4, x5, [x0, 16]
	sub	x0, x29, #16384
	ldp	x2, x3, [x0]
	sub	x0, x29, #16384
	ldp	x0, x1, [x0, -16]
	bl	_system__concat_2__str_concat_2
LBE53:
	.loc 1 239 32 is_stmt 0 discriminator 4
	cmp	w19, 26
	ble	L40
	.loc 1 239 32 discriminator 5
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L41
	bl	___stack_chk_fail
L41:
	mov	w1, 239
	adrp	x0, lC32@PAGE
	add	x0, x0, lC32@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L40:
	.loc 1 239 32 discriminator 6
	bic	w0, w19, w19, asr #31
	sxtw	x0, w0
	sub	x1, x29, #16384
	str	x0, [x1, 368]
	sub	x0, x29, #16384
	str	xzr, [x0, 376]
	sub	x0, x29, #12288
	sub	x0, x0, #3584
	ldp	x1, x2, [x0, -144]
	mov	x0, x1
	lsr	x0, x0, 61
	mov	x3, x2
	lsl	x3, x3, 3
	sub	x4, x29, #16384
	str	x3, [x4, 568]
	sub	x3, x29, #16384
	ldr	x3, [x3, 568]
	orr	x0, x0, x3
	sub	x3, x29, #16384
	str	x0, [x3, 568]
	mov	x0, x1
	lsl	x0, x0, 3
	sub	x1, x29, #16384
	str	x0, [x1, 560]
	.loc 1 239 4 is_stmt 1 discriminator 6
	sub	x0, x29, #40
	sub	x1, x29, #16384
	str	x0, [x1, 32]
	mov	w0, 1
	sub	x1, x29, #16384
	str	w0, [x1, 696]
	sub	x0, x29, #16384
	str	w19, [x0, 700]
	sub	x0, x29, #12288
	sub	x0, x0, #3400
	sub	x1, x29, #16384
	str	x0, [x1, 40]
	sub	x0, x29, #16384
	ldp	x0, x1, [x0, 32]
	bl	_ada__text_io__put_line__2
LBE52:
LBB54:
	.loc 1 240 41
	sub	x0, x29, #16384
	ldr	w1, [x0, 740]
	sub	x0, x29, #16384
	ldr	w0, [x0, 736]
	mov	w2, 0
	subs	w0, w1, w0
	bvc	L42
	mov	w2, 1
L42:
	mov	w1, w0
	.loc 1 240 41 is_stmt 0 discriminator 1
	mov	w0, w2
	cmp	w0, 0
	beq	L44
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L45
	bl	___stack_chk_fail
L45:
	mov	w1, 240
	adrp	x0, lC32@PAGE
	add	x0, x0, lC32@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L44:
	.loc 1 240 41 discriminator 2
	mov	w3, w1
	.loc 1 240 41 discriminator 5
	sub	x0, x29, #8192
	sub	x0, x0, #72
	sub	x1, x29, #16384
	str	x0, [x1, 48]
	adrp	x0, lC14@PAGE
	add	x0, x0, lC14@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 56]
	sub	x0, x29, #16384
	ldp	x1, x2, [x0, 48]
	mov	w0, w3
	bl	_system__img_int__impl__image_integer
	mov	w1, w0
	.loc 1 240 41 discriminator 7
	bic	w0, w1, w1, asr #31
	sxtw	x0, w0
	sub	x2, x29, #16384
	str	x0, [x2, 352]
	sub	x0, x29, #16384
	str	xzr, [x0, 360]
	sub	x0, x29, #12288
	sub	x0, x0, #3584
	ldp	x2, x3, [x0, -160]
	mov	x0, x2
	lsr	x0, x0, 61
	mov	x4, x3
	lsl	x4, x4, 3
	sub	x5, x29, #16384
	str	x4, [x5, 552]
	sub	x4, x29, #16384
	ldr	x4, [x4, 552]
	orr	x0, x0, x4
	sub	x4, x29, #16384
	str	x0, [x4, 552]
	mov	x0, x2
	lsl	x0, x0, 3
	sub	x2, x29, #16384
	str	x0, [x2, 544]
	bic	w0, w1, w1, asr #31
	sxtw	x0, w0
	sub	x2, x29, #16384
	str	x0, [x2, 336]
	sub	x0, x29, #16384
	str	xzr, [x0, 344]
	sub	x0, x29, #12288
	sub	x0, x0, #3584
	ldp	x2, x3, [x0, -176]
	mov	x0, x2
	lsr	x0, x0, 61
	mov	x4, x3
	lsl	x4, x4, 3
	sub	x5, x29, #16384
	str	x4, [x5, 536]
	sub	x4, x29, #16384
	ldr	x4, [x4, 536]
	orr	x0, x0, x4
	sub	x4, x29, #16384
	str	x0, [x4, 536]
	mov	x0, x2
	lsl	x0, x0, 3
	sub	x2, x29, #16384
	str	x0, [x2, 528]
	.loc 1 240 32 is_stmt 1 discriminator 7
	bic	w0, w1, w1, asr #31
	add	w19, w0, 15
LBB55:
	sub	x0, x29, #40
	sub	x2, x29, #16384
	str	x0, [x2, 64]
	adrp	x0, lC21@PAGE
	add	x0, x0, lC21@PAGEOFF;
	sub	x2, x29, #16384
	str	x0, [x2, 72]
	adrp	x0, lC72@PAGE
	add	x0, x0, lC72@PAGEOFF;
	sub	x2, x29, #16384
	str	x0, [x2, 80]
	adrp	x0, lC26@PAGE
	add	x0, x0, lC26@PAGEOFF;
	sub	x2, x29, #16384
	str	x0, [x2, 88]
	sub	x0, x29, #8192
	sub	x0, x0, #72
	sub	x2, x29, #16384
	str	x0, [x2, 96]
	mov	w0, 1
	sub	x2, x29, #16384
	str	w0, [x2, 704]
	sub	x0, x29, #16384
	str	w1, [x0, 708]
	sub	x0, x29, #12288
	sub	x0, x0, #3392
	sub	x1, x29, #16384
	str	x0, [x1, 104]
	sub	x0, x29, #16384
	ldp	x4, x5, [x0, 96]
	sub	x0, x29, #16384
	ldp	x2, x3, [x0, 80]
	sub	x0, x29, #16384
	ldp	x0, x1, [x0, 64]
	bl	_system__concat_2__str_concat_2
LBE55:
	.loc 1 240 32 is_stmt 0 discriminator 9
	cmp	w19, 26
	ble	L46
	.loc 1 240 32 discriminator 10
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L47
	bl	___stack_chk_fail
L47:
	mov	w1, 240
	adrp	x0, lC32@PAGE
	add	x0, x0, lC32@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L46:
	.loc 1 240 32 discriminator 11
	bic	w0, w19, w19, asr #31
	sxtw	x0, w0
	sub	x1, x29, #16384
	str	x0, [x1, 320]
	sub	x0, x29, #16384
	str	xzr, [x0, 328]
	sub	x0, x29, #12288
	sub	x0, x0, #3584
	ldp	x1, x2, [x0, -192]
	mov	x0, x1
	lsr	x0, x0, 61
	mov	x3, x2
	lsl	x3, x3, 3
	sub	x4, x29, #16384
	str	x3, [x4, 520]
	sub	x3, x29, #16384
	ldr	x3, [x3, 520]
	orr	x0, x0, x3
	sub	x3, x29, #16384
	str	x0, [x3, 520]
	mov	x0, x1
	lsl	x0, x0, 3
	sub	x1, x29, #16384
	str	x0, [x1, 512]
	.loc 1 240 4 is_stmt 1 discriminator 11
	sub	x0, x29, #40
	sub	x1, x29, #16384
	str	x0, [x1, 112]
	mov	w0, 1
	sub	x1, x29, #16384
	str	w0, [x1, 712]
	sub	x0, x29, #16384
	str	w19, [x0, 716]
	sub	x0, x29, #12288
	sub	x0, x0, #3384
	sub	x1, x29, #16384
	str	x0, [x1, 120]
	sub	x0, x29, #16384
	ldp	x0, x1, [x0, 112]
	bl	_ada__text_io__put_line__2
LBE54:
	.loc 1 241 4
	sub	x0, x29, #16384
	ldr	w0, [x0, 740]
	cmp	w0, 0
	ble	L48
LBB56:
	.loc 1 242 64
	sub	x0, x29, #16384
	ldr	w1, [x0, 736]
	mov	w0, 20972
	movk	w0, 0xfeb8, lsl 16
	cmp	w1, w0
	blt	L49
	.loc 1 242 64 is_stmt 0 discriminator 2
	sub	x0, x29, #16384
	ldr	w1, [x0, 736]
	mov	w0, 44564
	movk	w0, 0x147, lsl 16
	cmp	w1, w0
	ble	L50
L49:
	.loc 1 242 64 discriminator 3
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L51
	bl	___stack_chk_fail
L51:
	mov	w1, 242
	adrp	x0, lC32@PAGE
	add	x0, x0, lC32@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L50:
	.loc 1 242 64 discriminator 4
	sub	x0, x29, #16384
	ldr	w1, [x0, 736]
	mov	w0, 100
	mul	w1, w1, w0
	.loc 1 242 71 is_stmt 1 discriminator 7
	sub	x0, x29, #16384
	ldr	w0, [x0, 740]
	sdiv	w3, w1, w0
	.loc 1 242 44 discriminator 9
	sub	x0, x29, #8192
	sub	x0, x0, #72
	sub	x1, x29, #16384
	str	x0, [x1, 128]
	adrp	x0, lC14@PAGE
	add	x0, x0, lC14@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 136]
	sub	x0, x29, #16384
	ldp	x1, x2, [x0, 128]
	mov	w0, w3
	bl	_system__img_int__impl__image_integer
	mov	w1, w0
	.loc 1 242 44 is_stmt 0 discriminator 11
	bic	w0, w1, w1, asr #31
	sxtw	x0, w0
	sub	x2, x29, #16384
	str	x0, [x2, 304]
	sub	x0, x29, #16384
	str	xzr, [x0, 312]
	sub	x0, x29, #12288
	sub	x0, x0, #3584
	ldp	x2, x3, [x0, -208]
	mov	x0, x2
	lsr	x0, x0, 61
	mov	x4, x3
	lsl	x4, x4, 3
	sub	x5, x29, #16384
	str	x4, [x5, 504]
	sub	x4, x29, #16384
	ldr	x4, [x4, 504]
	orr	x0, x0, x4
	sub	x4, x29, #16384
	str	x0, [x4, 504]
	mov	x0, x2
	lsl	x0, x0, 3
	sub	x2, x29, #16384
	str	x0, [x2, 496]
	bic	w0, w1, w1, asr #31
	sxtw	x0, w0
	sub	x2, x29, #16384
	str	x0, [x2, 288]
	sub	x0, x29, #16384
	str	xzr, [x0, 296]
	sub	x0, x29, #12288
	sub	x0, x0, #3584
	ldp	x2, x3, [x0, -224]
	mov	x0, x2
	lsr	x0, x0, 61
	mov	x4, x3
	lsl	x4, x4, 3
	sub	x5, x29, #16384
	str	x4, [x5, 488]
	sub	x4, x29, #16384
	ldr	x4, [x4, 488]
	orr	x0, x0, x4
	sub	x4, x29, #16384
	str	x0, [x4, 488]
	mov	x0, x2
	lsl	x0, x0, 3
	sub	x2, x29, #16384
	str	x0, [x2, 480]
	.loc 1 242 85 is_stmt 1 discriminator 11
	bic	w0, w1, w1, asr #31
	add	w0, w0, 15
	add	w19, w0, 1
LBB57:
	sub	x0, x29, #40
	sub	x2, x29, #16384
	str	x0, [x2, 144]
	adrp	x0, lC27@PAGE
	add	x0, x0, lC27@PAGEOFF;
	sub	x2, x29, #16384
	str	x0, [x2, 152]
	adrp	x0, lC73@PAGE
	add	x0, x0, lC73@PAGEOFF;
	sub	x2, x29, #16384
	str	x0, [x2, 160]
	adrp	x0, lC26@PAGE
	add	x0, x0, lC26@PAGEOFF;
	sub	x2, x29, #16384
	str	x0, [x2, 168]
	sub	x0, x29, #8192
	sub	x0, x0, #72
	sub	x2, x29, #16384
	str	x0, [x2, 176]
	mov	w0, 1
	sub	x2, x29, #16384
	str	w0, [x2, 720]
	sub	x0, x29, #16384
	str	w1, [x0, 724]
	sub	x0, x29, #12288
	sub	x0, x0, #3376
	sub	x1, x29, #16384
	str	x0, [x1, 184]
	adrp	x0, lC74@PAGE
	add	x0, x0, lC74@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 192]
	adrp	x0, lC28@PAGE
	add	x0, x0, lC28@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 200]
	sub	x0, x29, #16384
	ldp	x6, x7, [x0, 192]
	sub	x0, x29, #16384
	ldp	x4, x5, [x0, 176]
	sub	x0, x29, #16384
	ldp	x2, x3, [x0, 160]
	sub	x0, x29, #16384
	ldp	x0, x1, [x0, 144]
	bl	_system__concat_3__str_concat_3
LBE57:
	.loc 1 242 85 is_stmt 0 discriminator 13
	cmp	w19, 27
	ble	L52
	.loc 1 242 85 discriminator 14
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L53
	bl	___stack_chk_fail
L53:
	mov	w1, 242
	adrp	x0, lC32@PAGE
	add	x0, x0, lC32@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L52:
	.loc 1 242 85 discriminator 15
	bic	w0, w19, w19, asr #31
	sxtw	x0, w0
	sub	x1, x29, #16384
	str	x0, [x1, 272]
	sub	x0, x29, #16384
	str	xzr, [x0, 280]
	sub	x0, x29, #12288
	sub	x0, x0, #3584
	ldp	x1, x2, [x0, -240]
	mov	x0, x1
	lsr	x0, x0, 61
	mov	x3, x2
	lsl	x3, x3, 3
	sub	x4, x29, #16384
	str	x3, [x4, 472]
	sub	x3, x29, #16384
	ldr	x3, [x3, 472]
	orr	x0, x0, x3
	sub	x3, x29, #16384
	str	x0, [x3, 472]
	mov	x0, x1
	lsl	x0, x0, 3
	sub	x1, x29, #16384
	str	x0, [x1, 464]
	.loc 1 242 7 is_stmt 1 discriminator 15
	sub	x0, x29, #40
	sub	x1, x29, #16384
	str	x0, [x1, 208]
	mov	w0, 1
	sub	x1, x29, #16384
	str	w0, [x1, 728]
	sub	x0, x29, #16384
	str	w19, [x0, 732]
	sub	x0, x29, #12288
	sub	x0, x0, #3368
	sub	x1, x29, #16384
	str	x0, [x1, 216]
	sub	x0, x29, #16384
	ldp	x0, x1, [x0, 208]
	bl	_ada__text_io__put_line__2
L48:
LBE56:
LBB58:
	.loc 1 244 4
	adrp	x0, lC68@PAGE
	add	x0, x0, lC68@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 224]
	adrp	x0, lC25@PAGE
	add	x0, x0, lC25@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 232]
	sub	x0, x29, #16384
	ldp	x0, x1, [x0, 224]
	bl	_ada__text_io__put_line__2
LBE58:
	.loc 1 246 4
	sub	x0, x29, #16384
	ldr	w1, [x0, 736]
	sub	x0, x29, #16384
	ldr	w0, [x0, 740]
	cmp	w1, w0
	bne	L54
LBB59:
	.loc 1 247 7
	adrp	x0, lC75@PAGE
	add	x0, x0, lC75@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 240]
	adrp	x0, lC29@PAGE
	add	x0, x0, lC29@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 248]
	sub	x0, x29, #16384
	ldp	x0, x1, [x0, 240]
	bl	_ada__text_io__put_line__2
LBE59:
	b	L55
L54:
LBB60:
	.loc 1 249 7
	adrp	x0, lC76@PAGE
	add	x0, x0, lC76@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 256]
	adrp	x0, lC30@PAGE
	add	x0, x0, lC30@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 264]
	sub	x0, x29, #12288
	sub	x0, x0, #3584
	ldp	x0, x1, [x0, -256]
	bl	_ada__text_io__put_line__2
L55:
LBE60:
	.loc 1 252 4
	mov	w0, 1
	bl	_ada__text_io__new_line__2
	.loc 1 254 5
	nop
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L57
	bl	___stack_chk_fail
L57:
	mov	sp, x29
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldr	x27, [sp, 80]
	ldp	x29, x30, [sp], 96
LCFI7:
	ret
LFE1:
	.const
	.align	2
lC0:
	.word	1
	.word	174
	.align	2
lC1:
	.word	1
	.word	60
	.align	2
lC2:
	.word	1
	.word	16
	.align	2
lC3:
	.word	1
	.word	53
	.align	2
lC4:
	.word	1
	.word	22
	.align	2
lC5:
	.word	1
	.word	14
	.align	2
lC6:
	.word	0
	.word	4
	.align	2
lC7:
	.word	1
	.word	12
	.align	2
lC8:
	.word	1
	.word	25
	.align	2
lC9:
	.word	1
	.word	18
	.align	2
lC10:
	.word	1
	.word	23
	.align	2
lC11:
	.word	1
	.word	17
	.align	2
lC12:
	.word	1
	.word	13
	.align	2
lC13:
	.word	0
	.word	3
	.align	2
lC14:
	.word	1
	.word	11
	.align	2
lC15:
	.word	1
	.word	21
	.align	1
lC16:
	.hword	1
	.hword	5
	.align	2
lC17:
	.word	0
	.word	7
	.align	1
lC18:
	.hword	1
	.hword	3
	.align	2
lC19:
	.word	1
	.word	8
	.align	2
lC20:
	.word	1
	.word	30
	.align	2
lC21:
	.word	1
	.word	26
	.align	2
lC22:
	.word	1
	.word	32
	.align	2
lC23:
	.word	1
	.word	28
	.align	2
lC24:
	.word	1
	.word	19
	.align	2
lC25:
	.word	1
	.word	168
	.align	2
lC26:
	.word	1
	.word	15
	.align	2
lC27:
	.word	1
	.word	27
	.align	2
lC28:
	.word	1
	.word	1
	.align	2
lC29:
	.word	1
	.word	47
	.align	2
lC30:
	.word	1
	.word	44
	.text
	.const
	.align	3
lC77:
	.ascii "=== "
	.align	3
lC78:
	.ascii " ==="
	.text
	.align	2
_test_comprehensive__print_header.0:
LFB3:
	.loc 1 31 4
	sub	x10, sp, #16384
	str	xzr, [x10, 3856]
	stp	x29, x30, [sp, -96]!
LCFI8:
	mov	x29, sp
LCFI9:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
	stp	x25, x26, [sp, 64]
	stp	x27, x28, [sp, 80]
	sub	sp, sp, #144
LCFI10:
	stp	x0, x1, [x29, -64]
	str	x16, [x29, -72]
	.loc 1 31 4
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x1, [x0]
	str	x1, [x29, -8]
	mov	x1, 0
	ldr	x0, [x29, -56]
	ldr	w0, [x0]
	ldr	x1, [x29, -56]
	ldr	w1, [x1, 4]
	cmp	w1, w0
	blt	L60
	.loc 1 31 4 is_stmt 0 discriminator 1
	sub	w6, w1, w0
	add	w19, w6, 1
	b	L61
L60:
	.loc 1 31 4 discriminator 2
	mov	w19, 0
L61:
LBB61:
	.loc 1 31 4 discriminator 4
	cmp	w1, w0
	.loc 1 31 4 discriminator 8
	cmp	w1, w0
	blt	L65
	.loc 1 31 4 discriminator 9
	sxtw	x7, w1
	sxtw	x6, w0
	sub	x6, x7, x6
	add	x6, x6, 1
	mov	x2, x6
	mov	x3, 0
	lsr	x6, x2, 61
	lsl	x5, x3, 3
	orr	x5, x6, x5
	lsl	x4, x2, 3
L65:
	.loc 1 31 4 discriminator 12
	cmp	w1, w0
	.loc 1 33 7 is_stmt 1
	mov	w0, 1
	bl	_ada__text_io__new_line__2
LBB62:
	.loc 1 34 7
	mov	x0, sp
	mov	x28, x0
	.loc 1 34 32 discriminator 1
	add	w0, w19, 4
	add	w0, w0, 4
	str	w0, [x29, -44]
	ldrsw	x0, [x29, -44]
	str	x0, [x29, -24]
	ldrsw	x0, [x29, -44]
	mov	x22, x0
	mov	x23, 0
	lsr	x0, x22, 61
	lsl	x27, x23, 3
	orr	x27, x0, x27
	lsl	x26, x22, 3
	ldrsw	x0, [x29, -44]
	mov	x20, x0
	mov	x21, 0
	lsr	x0, x20, 61
	lsl	x25, x21, 3
	orr	x25, x0, x25
	lsl	x24, x20, 3
	ldrsw	x0, [x29, -44]
	add	x0, x0, 15
	lsr	x0, x0, 4
	lsl	x1, x0, 4
	and	x3, x1, -4096
	sub	x0, sp, #12288
	sub	x2, x0, x3
L68:
	cmp	x0, x2
	beq	L69
	sub	x0, x0, #4096
	str	xzr, [x0]
	b	L68
L69:
	sub	x0, x1, x3
	sub	x0, x2, x0
	str	xzr, [x0]
	sub	sp, sp, x1
	mov	x0, sp
	.loc 1 34 32 is_stmt 0 discriminator 2
	str	x0, [x29, -16]
LBB63:
	ldr	x0, [x29, -16]
	str	x0, [x29, -144]
	mov	w0, 1
	str	w0, [x29, -40]
	ldr	w0, [x29, -44]
	str	w0, [x29, -36]
	sub	x0, x29, #40
	str	x0, [x29, -136]
	adrp	x0, lC77@PAGE
	add	x0, x0, lC77@PAGEOFF;
	str	x0, [x29, -128]
	adrp	x0, lC31@PAGE
	add	x0, x0, lC31@PAGEOFF;
	str	x0, [x29, -120]
	adrp	x0, lC78@PAGE
	add	x0, x0, lC78@PAGEOFF;
	str	x0, [x29, -112]
	adrp	x0, lC31@PAGE
	add	x0, x0, lC31@PAGEOFF;
	str	x0, [x29, -104]
	ldp	x6, x7, [x29, -112]
	ldp	x4, x5, [x29, -64]
	ldp	x2, x3, [x29, -128]
	ldp	x0, x1, [x29, -144]
	bl	_system__concat_3__str_concat_3
LBE63:
	.loc 1 34 7 is_stmt 1 discriminator 4
	ldr	x0, [x29, -16]
	str	x0, [x29, -96]
	mov	w0, 1
	str	w0, [x29, -32]
	ldr	w0, [x29, -44]
	str	w0, [x29, -28]
	sub	x0, x29, #32
	str	x0, [x29, -88]
	ldp	x0, x1, [x29, -96]
	bl	_ada__text_io__put_line__2
	.loc 1 34 0 discriminator 6
	mov	sp, x28
LBE62:
	.loc 1 35 8
	nop
LBE61:
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L71
	bl	___stack_chk_fail
L71:
	mov	sp, x29
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
	ldp	x29, x30, [sp], 96
LCFI11:
	ret
LFE3:
	.const
	.align	3
_msg.6:
	.byte	1
	.byte	2
	.byte	3
	.byte	4
	.byte	5
	.space 3
	.align	3
_msg.5:
	.byte	1
	.byte	2
	.byte	3
	.byte	4
	.byte	5
	.space 3
	.align	2
_msg.4:
	.byte	10
	.byte	20
	.byte	30
	.byte	40
	.align	3
_secret.3:
	.byte	1
	.byte	2
	.byte	3
	.byte	4
	.byte	5
	.byte	6
	.byte	7
	.byte	8
_key_data.2:
	.byte	42
	.byte	42
	.byte	42
	.byte	42
	.byte	42
	.byte	42
	.byte	42
	.byte	42
	.byte	42
	.byte	42
	.byte	42
	.byte	42
	.byte	42
	.byte	42
	.byte	42
	.byte	42
	.byte	42
	.byte	42
	.byte	42
	.byte	42
	.byte	42
	.byte	42
	.byte	42
	.byte	42
	.byte	42
	.byte	42
	.byte	42
	.byte	42
	.byte	42
	.byte	42
	.byte	42
	.byte	42
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
	.byte	0x9c
	.uleb128 0x1
	.byte	0x4
	.set L$set$7,LCFI3-LCFI2
	.long L$set$7
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
	.byte	0x4
	.set L$set$14,LCFI7-LCFI6
	.long L$set$14
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
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$19,LCFI9-LCFI8
	.long L$set$19
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$20,LCFI10-LCFI9
	.long L$set$20
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
	.set L$set$21,LCFI11-LCFI10
	.long L$set$21
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
LEFDE4:
	.section __TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
EH_frame1:
	.set L$set$22,LECIE1-LSCIE1
	.long L$set$22
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
	.set L$set$23,LEFDE7-LASFDE7
	.long L$set$23
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB2-.
	.set L$set$24,LFE2-LFB2
	.quad L$set$24
	.uleb128 0
	.byte	0x4
	.set L$set$25,LCFI0-LFB2
	.long L$set$25
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$26,LCFI1-LCFI0
	.long L$set$26
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$27,LCFI2-LCFI1
	.long L$set$27
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
	.set L$set$28,LCFI3-LCFI2
	.long L$set$28
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
LEFDE7:
LSFDE9:
	.set L$set$29,LEFDE9-LASFDE9
	.long L$set$29
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB1-.
	.set L$set$30,LFE1-LFB1
	.quad L$set$30
	.uleb128 0
	.byte	0x4
	.set L$set$31,LCFI4-LFB1
	.long L$set$31
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$32,LCFI5-LCFI4
	.long L$set$32
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$33,LCFI6-LCFI5
	.long L$set$33
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
	.set L$set$34,LCFI7-LCFI6
	.long L$set$34
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
LEFDE9:
LSFDE11:
	.set L$set$35,LEFDE11-LASFDE11
	.long L$set$35
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB3-.
	.set L$set$36,LFE3-LFB3
	.quad L$set$36
	.uleb128 0
	.byte	0x4
	.set L$set$37,LCFI8-LFB3
	.long L$set$37
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$38,LCFI9-LCFI8
	.long L$set$38
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$39,LCFI10-LCFI9
	.long L$set$39
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
	.set L$set$40,LCFI11-LCFI10
	.long L$set$40
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
LEFDE11:
	.text
Letext0:
	.file 2 "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_types.ads"
	.file 3 "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_types-pqc.ads"
	.file 4 "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_types-sss.ads"
	.file 5 "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_key_manager.ads"
	.file 6 "<built-in>"
	.file 7 "/Users/sicarii/.local/share/alire/releases/gnatprove_14.1.1_91818ed8/libexec/spark/lib/gcc/aarch64-apple-darwin23.5.0/14.1.0/adainclude/interfac.ads"
	.section __DWARF,__debug_info,regular,debug
Lsection__debug_info:
Ldebug_info0:
	.long	0x1470
	.short	0x4
	.set L$set$41,Ldebug_abbrev0-Lsection__debug_abbrev
	.long L$set$41
	.byte	0x8
	.uleb128 0x1
	.ascii "GNU Ada 14.1.0 -O0 -gnatA -g -gnat2012 -gnatwa -gnatwe -fstack-check=specific -fstack-protector-strong -gnatR2js -gnatws -gnatec=/private/var/folders/bg/pt9l6y1j47q642kp3z5blrmh0000gn/T/GPR.75209/GNAT-TEMP-000003.TMP -gnatem=/private/var/folders/bg/pt9l6y1j47q642kp3z5blrmh0000gn/T/GPR.75209/GNAT-TEMP-000004.TMP -mmacosx-version-min=16.0.0 -mcpu=apple-m1 -mlittle-endian -mabi=lp64 -fPIC\0"
	.byte	0xd
	.ascii "/Users/sicarii/Desktop/anubis-spark/src/test_comprehensive.adb\0"
	.ascii "/Users/sicarii/Desktop/anubis-spark/obj/gnatprove/data_representation\0"
	.quad	Ltext0
	.set L$set$42,Letext0-Ltext0
	.quad L$set$42
	.set L$set$43,Ldebug_line0-Lsection__debug_line
	.long L$set$43
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
	.long	0x278
	.uleb128 0x2
	.byte	0x1
	.byte	0x7
	.ascii "anubis_types__TbyteB\0"
	.uleb128 0x3
	.byte	0x4
	.byte	0x5
	.ascii "integer\0"
	.uleb128 0x5
	.ascii "anubis_types__ed25519_public_key__T21s\0"
	.long	0x25e
	.long	0x2d2
	.uleb128 0x6
	.long	0x290
	.sleb128 32
	.byte	0
	.uleb128 0x7
	.ascii "anubis_types__ed25519_public_key\0"
	.byte	0x20
	.byte	0x2
	.byte	0x9e
	.byte	0x9
	.long	0x30a
	.uleb128 0x8
	.set L$set$44,LASF0-Lsection__debug_str
	.long L$set$44
	.byte	0x2
	.byte	0x9f
	.byte	0x7
	.long	0x29b
	.byte	0
	.byte	0
	.uleb128 0x5
	.ascii "anubis_types__ed25519_secret_key__T23s\0"
	.long	0x25e
	.long	0x341
	.uleb128 0x6
	.long	0x290
	.sleb128 32
	.byte	0
	.uleb128 0x7
	.ascii "anubis_types__ed25519_secret_key\0"
	.byte	0x21
	.byte	0x2
	.byte	0xa2
	.byte	0x9
	.long	0x386
	.uleb128 0x8
	.set L$set$45,LASF0-Lsection__debug_str
	.long L$set$45
	.byte	0x2
	.byte	0xa3
	.byte	0x7
	.long	0x30a
	.byte	0
	.uleb128 0x8
	.set L$set$46,LASF1-Lsection__debug_str
	.long L$set$46
	.byte	0x2
	.byte	0xa4
	.byte	0x7
	.long	0x386
	.byte	0x20
	.byte	0
	.uleb128 0x3
	.byte	0x1
	.byte	0x2
	.ascii "boolean\0"
	.uleb128 0x9
	.long	0x386
	.uleb128 0x5
	.ascii "anubis_types__ed25519_signature__T25s\0"
	.long	0x25e
	.long	0x3cd
	.uleb128 0x6
	.long	0x290
	.sleb128 64
	.byte	0
	.uleb128 0x7
	.ascii "anubis_types__ed25519_signature\0"
	.byte	0x40
	.byte	0x2
	.byte	0xa7
	.byte	0x9
	.long	0x404
	.uleb128 0x8
	.set L$set$47,LASF0-Lsection__debug_str
	.long L$set$47
	.byte	0x2
	.byte	0xa8
	.byte	0x7
	.long	0x396
	.byte	0
	.byte	0
	.uleb128 0x5
	.ascii "anubis_types__ml_kem_public_key__T33s\0"
	.long	0x25e
	.long	0x43b
	.uleb128 0x6
	.long	0x290
	.sleb128 1568
	.byte	0
	.uleb128 0xa
	.ascii "anubis_types__ml_kem_public_key\0"
	.short	0x620
	.byte	0x2
	.byte	0xb8
	.byte	0x9
	.long	0x473
	.uleb128 0x8
	.set L$set$48,LASF0-Lsection__debug_str
	.long L$set$48
	.byte	0x2
	.byte	0xb9
	.byte	0x7
	.long	0x404
	.byte	0
	.byte	0
	.uleb128 0x5
	.ascii "anubis_types__ml_kem_secret_key__T35s\0"
	.long	0x25e
	.long	0x4aa
	.uleb128 0x6
	.long	0x290
	.sleb128 3168
	.byte	0
	.uleb128 0xa
	.ascii "anubis_types__ml_kem_secret_key\0"
	.short	0xc61
	.byte	0x2
	.byte	0xbc
	.byte	0x9
	.long	0x4f0
	.uleb128 0x8
	.set L$set$49,LASF0-Lsection__debug_str
	.long L$set$49
	.byte	0x2
	.byte	0xbd
	.byte	0x7
	.long	0x473
	.byte	0
	.uleb128 0xb
	.set L$set$50,LASF1-Lsection__debug_str
	.long L$set$50
	.byte	0x2
	.byte	0xbe
	.byte	0x7
	.long	0x386
	.short	0xc60
	.byte	0
	.uleb128 0x5
	.ascii "anubis_types__ml_kem_ciphertext__T37s\0"
	.long	0x25e
	.long	0x527
	.uleb128 0x6
	.long	0x290
	.sleb128 1568
	.byte	0
	.uleb128 0xa
	.ascii "anubis_types__ml_kem_ciphertext\0"
	.short	0x620
	.byte	0x2
	.byte	0xc1
	.byte	0x9
	.long	0x55f
	.uleb128 0x8
	.set L$set$51,LASF0-Lsection__debug_str
	.long L$set$51
	.byte	0x2
	.byte	0xc2
	.byte	0x7
	.long	0x4f0
	.byte	0
	.byte	0
	.uleb128 0x5
	.ascii "anubis_types__ml_kem_shared_secret__T39s\0"
	.long	0x25e
	.long	0x598
	.uleb128 0x6
	.long	0x290
	.sleb128 32
	.byte	0
	.uleb128 0x7
	.ascii "anubis_types__ml_kem_shared_secret\0"
	.byte	0x21
	.byte	0x2
	.byte	0xc5
	.byte	0x9
	.long	0x5df
	.uleb128 0x8
	.set L$set$52,LASF0-Lsection__debug_str
	.long L$set$52
	.byte	0x2
	.byte	0xc6
	.byte	0x7
	.long	0x55f
	.byte	0
	.uleb128 0x8
	.set L$set$53,LASF1-Lsection__debug_str
	.long L$set$53
	.byte	0x2
	.byte	0xc7
	.byte	0x7
	.long	0x386
	.byte	0x20
	.byte	0
	.uleb128 0x5
	.ascii "anubis_types__ml_dsa_public_key__T41s\0"
	.long	0x25e
	.long	0x616
	.uleb128 0x6
	.long	0x290
	.sleb128 2592
	.byte	0
	.uleb128 0xa
	.ascii "anubis_types__ml_dsa_public_key\0"
	.short	0xa20
	.byte	0x2
	.byte	0xca
	.byte	0x9
	.long	0x64e
	.uleb128 0x8
	.set L$set$54,LASF0-Lsection__debug_str
	.long L$set$54
	.byte	0x2
	.byte	0xcb
	.byte	0x7
	.long	0x5df
	.byte	0
	.byte	0
	.uleb128 0x5
	.ascii "anubis_types__ml_dsa_secret_key__T43s\0"
	.long	0x25e
	.long	0x685
	.uleb128 0x6
	.long	0x290
	.sleb128 4896
	.byte	0
	.uleb128 0xa
	.ascii "anubis_types__ml_dsa_secret_key\0"
	.short	0x1321
	.byte	0x2
	.byte	0xce
	.byte	0x9
	.long	0x6cb
	.uleb128 0x8
	.set L$set$55,LASF0-Lsection__debug_str
	.long L$set$55
	.byte	0x2
	.byte	0xcf
	.byte	0x7
	.long	0x64e
	.byte	0
	.uleb128 0xb
	.set L$set$56,LASF1-Lsection__debug_str
	.long L$set$56
	.byte	0x2
	.byte	0xd0
	.byte	0x7
	.long	0x386
	.short	0x1320
	.byte	0
	.uleb128 0x5
	.ascii "anubis_types__ml_dsa_signature__T45s\0"
	.long	0x25e
	.long	0x701
	.uleb128 0x6
	.long	0x290
	.sleb128 4627
	.byte	0
	.uleb128 0xa
	.ascii "anubis_types__ml_dsa_signature\0"
	.short	0x1213
	.byte	0x2
	.byte	0xd3
	.byte	0x9
	.long	0x738
	.uleb128 0x8
	.set L$set$57,LASF0-Lsection__debug_str
	.long L$set$57
	.byte	0x2
	.byte	0xd4
	.byte	0x7
	.long	0x6cb
	.byte	0
	.byte	0
	.uleb128 0xa
	.ascii "anubis_types__pqc__hybrid_signature\0"
	.short	0x1253
	.byte	0x3
	.byte	0xc6
	.byte	0x9
	.long	0x790
	.uleb128 0xc
	.ascii "ed25519_sig\0"
	.byte	0x3
	.byte	0xc7
	.byte	0x7
	.long	0x3cd
	.byte	0
	.uleb128 0xc
	.ascii "ml_dsa_sig\0"
	.byte	0x3
	.byte	0xc8
	.byte	0x7
	.long	0x701
	.byte	0x40
	.byte	0
	.uleb128 0xd
	.sleb128 255
	.ascii "anubis_types__sss__share_index\0"
	.long	0x7b6
	.uleb128 0x2
	.byte	0x2
	.byte	0x5
	.ascii "anubis_types__sss__Tshare_indexB\0"
	.uleb128 0x5
	.ascii "anubis_types__sss__secret_share__T7s\0"
	.long	0x25e
	.long	0x810
	.uleb128 0x6
	.long	0x290
	.sleb128 256
	.byte	0
	.uleb128 0xa
	.ascii "anubis_types__sss__secret_share\0"
	.short	0x10c
	.byte	0x4
	.byte	0x8c
	.byte	0x9
	.long	0x870
	.uleb128 0xc
	.ascii "x\0"
	.byte	0x4
	.byte	0x8d
	.byte	0x7
	.long	0x790
	.byte	0
	.uleb128 0xc
	.ascii "y\0"
	.byte	0x4
	.byte	0x8e
	.byte	0x7
	.long	0x7da
	.byte	0x2
	.uleb128 0xe
	.ascii "length\0"
	.byte	0x4
	.byte	0x8f
	.byte	0x7
	.long	0x1452
	.short	0x104
	.uleb128 0xb
	.set L$set$58,LASF1-Lsection__debug_str
	.long L$set$58
	.byte	0x4
	.byte	0x90
	.byte	0x7
	.long	0x386
	.short	0x108
	.byte	0
	.uleb128 0x5
	.ascii "anubis_key_manager__managed_key__T5s\0"
	.long	0x25e
	.long	0x8a7
	.uleb128 0x6
	.long	0x290
	.sleb128 8192
	.byte	0
	.uleb128 0xf
	.ascii "anubis_key_manager__key_purpose\0"
	.byte	0x1
	.byte	0x5
	.byte	0x10
	.byte	0x9
	.long	0x954
	.uleb128 0x10
	.ascii "anubis_key_manager__encryption\0"
	.byte	0
	.uleb128 0x10
	.ascii "anubis_key_manager__signing\0"
	.byte	0x1
	.uleb128 0x10
	.ascii "anubis_key_manager__key_exchange\0"
	.byte	0x2
	.uleb128 0x10
	.ascii "anubis_key_manager__derivation\0"
	.byte	0x3
	.byte	0
	.uleb128 0xf
	.ascii "anubis_types__key_status\0"
	.byte	0x1
	.byte	0x2
	.byte	0x4b
	.byte	0x9
	.long	0x9f6
	.uleb128 0x10
	.ascii "anubis_types__uninitialized\0"
	.byte	0
	.uleb128 0x10
	.ascii "anubis_types__active\0"
	.byte	0x1
	.uleb128 0x10
	.ascii "anubis_types__expired\0"
	.byte	0x2
	.uleb128 0x10
	.ascii "anubis_types__revoked\0"
	.byte	0x3
	.uleb128 0x10
	.ascii "anubis_types__destroyed\0"
	.byte	0x4
	.byte	0
	.uleb128 0x7
	.ascii "anubis_key_manager__rotation_policy\0"
	.byte	0xc
	.byte	0x5
	.byte	0x12
	.byte	0x9
	.long	0xa69
	.uleb128 0xc
	.ascii "time_based_days\0"
	.byte	0x5
	.byte	0x13
	.byte	0x7
	.long	0x1452
	.byte	0
	.uleb128 0xc
	.ascii "usage_based_count\0"
	.byte	0x5
	.byte	0x14
	.byte	0x7
	.long	0x1452
	.byte	0x4
	.uleb128 0xc
	.ascii "enabled\0"
	.byte	0x5
	.byte	0x15
	.byte	0x7
	.long	0x386
	.byte	0x8
	.byte	0
	.uleb128 0xa
	.ascii "anubis_key_manager__managed_key\0"
	.short	0x201c
	.byte	0x5
	.byte	0x44
	.byte	0x9
	.long	0xb13
	.uleb128 0xc
	.ascii "key_material\0"
	.byte	0x5
	.byte	0x45
	.byte	0x7
	.long	0x870
	.byte	0
	.uleb128 0xe
	.ascii "length\0"
	.byte	0x5
	.byte	0x46
	.byte	0x7
	.long	0x1452
	.short	0x2000
	.uleb128 0xe
	.ascii "purpose\0"
	.byte	0x5
	.byte	0x47
	.byte	0x7
	.long	0x8a7
	.short	0x2004
	.uleb128 0xe
	.ascii "status\0"
	.byte	0x5
	.byte	0x48
	.byte	0x7
	.long	0x954
	.short	0x2005
	.uleb128 0xe
	.ascii "usage_count\0"
	.byte	0x5
	.byte	0x49
	.byte	0x7
	.long	0x1452
	.short	0x2008
	.uleb128 0xe
	.ascii "policy\0"
	.byte	0x5
	.byte	0x4a
	.byte	0x7
	.long	0x9f6
	.short	0x200c
	.uleb128 0xb
	.set L$set$59,LASF1-Lsection__debug_str
	.long L$set$59
	.byte	0x5
	.byte	0x4b
	.byte	0x7
	.long	0x386
	.short	0x2018
	.byte	0
	.uleb128 0x11
	.ascii "test_comprehensive\0"
	.byte	0x1
	.byte	0xe
	.byte	0x1
	.ascii "_ada_test_comprehensive\0"
	.quad	LFB1
	.set L$set$60,LFE1-LFB1
	.quad L$set$60
	.uleb128 0x1
	.byte	0x9c
	.long	0x1360
	.uleb128 0x12
	.ascii "test_count\0"
	.byte	0x1
	.byte	0x10
	.byte	0x4
	.long	0x1452
	.uleb128 0x6
	.byte	0x91
	.sleb128 -15744
	.byte	0x23
	.uleb128 0x4
	.uleb128 0x12
	.ascii "pass_count\0"
	.byte	0x1
	.byte	0x11
	.byte	0x4
	.long	0x1452
	.uleb128 0x4
	.byte	0x91
	.sleb128 -15744
	.uleb128 0x13
	.ascii "test_comprehensive__test\0"
	.byte	0x1
	.byte	0x13
	.byte	0x4
	.quad	LFB2
	.set L$set$61,LFE2-LFB2
	.quad L$set$61
	.uleb128 0x1
	.byte	0x9c
	.uleb128 0x7
	.byte	0x91
	.sleb128 -192
	.byte	0x6
	.byte	0x23
	.uleb128 0x8
	.byte	0x6
	.long	0xc8b
	.uleb128 0x14
	.ascii "name\0"
	.byte	0x1
	.byte	0x13
	.byte	0x14
	.long	0x1397
	.uleb128 0x3
	.byte	0x91
	.sleb128 -176
	.uleb128 0x14
	.ascii "condition\0"
	.byte	0x1
	.byte	0x13
	.byte	0x23
	.long	0x391
	.uleb128 0x3
	.byte	0x91
	.sleb128 -177
	.uleb128 0x15
	.quad	LBB3
	.set L$set$62,LBE3-LBB3
	.quad L$set$62
	.uleb128 0x16
	.ascii "L6b\0"
	.long	0x290
	.uleb128 0x3
	.byte	0x91
	.sleb128 -152
	.uleb128 0x16
	.ascii "test_comprehensive__test__B2b__TTS7bSP1___U\0"
	.long	0x290
	.uleb128 0x3
	.byte	0x91
	.sleb128 -148
	.uleb128 0x5
	.ascii "test_comprehensive__test__B2b__TS7bS\0"
	.long	0x13cd
	.long	0xc7b
	.uleb128 0x17
	.long	0x290
	.long	0xbfd
	.long	0xc0a
	.byte	0
	.uleb128 0x16
	.ascii "S7b\0"
	.long	0xc3f
	.uleb128 0x4
	.byte	0x91
	.sleb128 -112
	.byte	0x6
	.byte	0
	.byte	0
	.uleb128 0x13
	.ascii "test_comprehensive__print_header\0"
	.byte	0x1
	.byte	0x1f
	.byte	0x4
	.quad	LFB3
	.set L$set$63,LFE3-LFB3
	.quad L$set$63
	.uleb128 0x1
	.byte	0x9c
	.uleb128 0x7
	.byte	0x91
	.sleb128 -168
	.byte	0x6
	.byte	0x23
	.uleb128 0x8
	.byte	0x6
	.long	0xd83
	.uleb128 0x14
	.ascii "title\0"
	.byte	0x1
	.byte	0x1f
	.byte	0x1c
	.long	0x139c
	.uleb128 0x3
	.byte	0x91
	.sleb128 -160
	.uleb128 0x15
	.quad	LBB62
	.set L$set$64,LBE62-LBB62
	.quad L$set$64
	.uleb128 0x16
	.ascii "test_comprehensive__print_header__B11b__TTS17bSP1___U\0"
	.long	0x290
	.uleb128 0x3
	.byte	0x91
	.sleb128 -140
	.uleb128 0x5
	.ascii "test_comprehensive__print_header__B11b__TS17bS\0"
	.long	0x13cd
	.long	0xd72
	.uleb128 0x18
	.long	0x290
	.long	0xcf1
	.byte	0
	.uleb128 0x16
	.ascii "S17b\0"
	.long	0xd30
	.uleb128 0x4
	.byte	0x91
	.sleb128 -112
	.byte	0x6
	.byte	0
	.byte	0
	.uleb128 0x19
	.quad	LBB13
	.set L$set$65,LBE13-LBB13
	.quad L$set$65
	.long	0xe31
	.uleb128 0x12
	.ascii "alice_pk\0"
	.byte	0x1
	.byte	0x3e
	.byte	0x7
	.long	0x2d2
	.uleb128 0x4
	.byte	0x91
	.sleb128 -15648
	.uleb128 0x12
	.ascii "alice_sk\0"
	.byte	0x1
	.byte	0x3f
	.byte	0x7
	.long	0x341
	.uleb128 0x4
	.byte	0x91
	.sleb128 -13056
	.uleb128 0x5
	.ascii "test_comprehensive__B_1__T25b\0"
	.long	0x25e
	.long	0xdf3
	.uleb128 0x1a
	.long	0x290
	.sleb128 0
	.sleb128 4
	.byte	0
	.uleb128 0x1b
	.long	0xdc4
	.uleb128 0x12
	.ascii "msg\0"
	.byte	0x1
	.byte	0x40
	.byte	0x7
	.long	0xdf3
	.uleb128 0x9
	.byte	0x3
	.quad	_msg.6
	.uleb128 0x12
	.ascii "sig\0"
	.byte	0x1
	.byte	0x41
	.byte	0x7
	.long	0x3cd
	.uleb128 0x4
	.byte	0x91
	.sleb128 -8360
	.uleb128 0x1c
	.set L$set$66,LASF2-Lsection__debug_str
	.long L$set$66
	.byte	0x1
	.byte	0x42
	.byte	0x7
	.long	0x386
	.uleb128 0x4
	.byte	0x91
	.sleb128 -15820
	.byte	0
	.uleb128 0x19
	.quad	LBB18
	.set L$set$67,LBE18-LBB18
	.quad L$set$67
	.long	0xeaa
	.uleb128 0x12
	.ascii "pk\0"
	.byte	0x1
	.byte	0x56
	.byte	0x7
	.long	0x43b
	.uleb128 0x4
	.byte	0x91
	.sleb128 -15648
	.uleb128 0x12
	.ascii "sk\0"
	.byte	0x1
	.byte	0x57
	.byte	0x7
	.long	0x4aa
	.uleb128 0x4
	.byte	0x91
	.sleb128 -8360
	.uleb128 0x12
	.ascii "ct\0"
	.byte	0x1
	.byte	0x58
	.byte	0x7
	.long	0x527
	.uleb128 0x4
	.byte	0x91
	.sleb128 -13056
	.uleb128 0x12
	.ascii "ss1\0"
	.byte	0x1
	.byte	0x59
	.byte	0x7
	.long	0x598
	.uleb128 0x4
	.byte	0x91
	.sleb128 -15728
	.uleb128 0x12
	.ascii "ss2\0"
	.byte	0x1
	.byte	0x59
	.byte	0xc
	.long	0x598
	.uleb128 0x4
	.byte	0x91
	.sleb128 -15688
	.uleb128 0x1c
	.set L$set$68,LASF2-Lsection__debug_str
	.long L$set$68
	.byte	0x1
	.byte	0x5a
	.byte	0x7
	.long	0x386
	.uleb128 0x4
	.byte	0x91
	.sleb128 -15819
	.byte	0
	.uleb128 0x19
	.quad	LBB23
	.set L$set$69,LBE23-LBB23
	.quad L$set$69
	.long	0xf4c
	.uleb128 0x12
	.ascii "pk\0"
	.byte	0x1
	.byte	0x6b
	.byte	0x7
	.long	0x616
	.uleb128 0x4
	.byte	0x91
	.sleb128 -15648
	.uleb128 0x12
	.ascii "sk\0"
	.byte	0x1
	.byte	0x6c
	.byte	0x7
	.long	0x685
	.uleb128 0x4
	.byte	0x91
	.sleb128 -8360
	.uleb128 0x5
	.ascii "test_comprehensive__B_3__T35b\0"
	.long	0x25e
	.long	0xf0e
	.uleb128 0x1a
	.long	0x290
	.sleb128 0
	.sleb128 4
	.byte	0
	.uleb128 0x1b
	.long	0xedf
	.uleb128 0x12
	.ascii "msg\0"
	.byte	0x1
	.byte	0x6d
	.byte	0x7
	.long	0xf0e
	.uleb128 0x9
	.byte	0x3
	.quad	_msg.5
	.uleb128 0x12
	.ascii "sig\0"
	.byte	0x1
	.byte	0x6e
	.byte	0x7
	.long	0x701
	.uleb128 0x4
	.byte	0x91
	.sleb128 -13056
	.uleb128 0x1c
	.set L$set$70,LASF2-Lsection__debug_str
	.long L$set$70
	.byte	0x1
	.byte	0x6f
	.byte	0x7
	.long	0x386
	.uleb128 0x4
	.byte	0x91
	.sleb128 -15818
	.byte	0
	.uleb128 0x19
	.quad	LBB28
	.set L$set$71,LBE28-LBB28
	.quad L$set$71
	.long	0x101c
	.uleb128 0x12
	.ascii "ed_pk\0"
	.byte	0x1
	.byte	0x83
	.byte	0x7
	.long	0x2d2
	.uleb128 0x4
	.byte	0x91
	.sleb128 -15728
	.uleb128 0x12
	.ascii "ed_sk\0"
	.byte	0x1
	.byte	0x84
	.byte	0x7
	.long	0x341
	.uleb128 0x4
	.byte	0x91
	.sleb128 -15688
	.uleb128 0x12
	.ascii "dsa_pk\0"
	.byte	0x1
	.byte	0x85
	.byte	0x7
	.long	0x616
	.uleb128 0x4
	.byte	0x91
	.sleb128 -15648
	.uleb128 0x12
	.ascii "dsa_sk\0"
	.byte	0x1
	.byte	0x86
	.byte	0x7
	.long	0x685
	.uleb128 0x4
	.byte	0x91
	.sleb128 -8360
	.uleb128 0x5
	.ascii "test_comprehensive__B_4__T41b\0"
	.long	0x25e
	.long	0xfde
	.uleb128 0x1a
	.long	0x290
	.sleb128 0
	.sleb128 3
	.byte	0
	.uleb128 0x1b
	.long	0xfaf
	.uleb128 0x12
	.ascii "msg\0"
	.byte	0x1
	.byte	0x87
	.byte	0x7
	.long	0xfde
	.uleb128 0x9
	.byte	0x3
	.quad	_msg.4
	.uleb128 0x12
	.ascii "sig\0"
	.byte	0x1
	.byte	0x88
	.byte	0x7
	.long	0x738
	.uleb128 0x4
	.byte	0x91
	.sleb128 -13056
	.uleb128 0x1c
	.set L$set$72,LASF2-Lsection__debug_str
	.long L$set$72
	.byte	0x1
	.byte	0x89
	.byte	0x7
	.long	0x386
	.uleb128 0x4
	.byte	0x91
	.sleb128 -15817
	.byte	0
	.uleb128 0x19
	.quad	LBB34
	.set L$set$73,LBE34-LBB34
	.quad L$set$73
	.long	0x1183
	.uleb128 0x5
	.ascii "test_comprehensive__B_5__T48b\0"
	.long	0x25e
	.long	0x1060
	.uleb128 0x1a
	.long	0x290
	.sleb128 0
	.sleb128 7
	.byte	0
	.uleb128 0x1b
	.long	0x1031
	.uleb128 0x12
	.ascii "secret\0"
	.byte	0x1
	.byte	0xa1
	.byte	0x7
	.long	0x1060
	.uleb128 0x9
	.byte	0x3
	.quad	_secret.3
	.uleb128 0x5
	.ascii "test_comprehensive__B_5__TsharesS\0"
	.long	0x810
	.long	0x10b0
	.uleb128 0x6
	.long	0x7b6
	.sleb128 5
	.byte	0
	.uleb128 0x12
	.ascii "shares\0"
	.byte	0x1
	.byte	0xa2
	.byte	0x7
	.long	0x107e
	.uleb128 0x4
	.byte	0x91
	.sleb128 -8360
	.uleb128 0x5
	.ascii "test_comprehensive__B_5__TreconstructedS\0"
	.long	0x25e
	.long	0x10fd
	.uleb128 0x6
	.long	0x290
	.sleb128 8
	.byte	0
	.uleb128 0x12
	.ascii "reconstructed\0"
	.byte	0x1
	.byte	0xa3
	.byte	0x7
	.long	0x10c4
	.uleb128 0x4
	.byte	0x91
	.sleb128 -13056
	.uleb128 0x1c
	.set L$set$74,LASF2-Lsection__debug_str
	.long L$set$74
	.byte	0x1
	.byte	0xa4
	.byte	0x7
	.long	0x386
	.uleb128 0x4
	.byte	0x91
	.sleb128 -15816
	.uleb128 0x12
	.ascii "match\0"
	.byte	0x1
	.byte	0xa5
	.byte	0x7
	.long	0x386
	.uleb128 0x4
	.byte	0x91
	.sleb128 -15821
	.uleb128 0x19
	.quad	LBB37
	.set L$set$75,LBE37-LBB37
	.quad L$set$75
	.long	0x1161
	.uleb128 0x12
	.ascii "i\0"
	.byte	0x1
	.byte	0xba
	.byte	0xb
	.long	0x290
	.uleb128 0x4
	.byte	0x91
	.sleb128 -15812
	.byte	0
	.uleb128 0x15
	.quad	LBB39
	.set L$set$76,LBE39-LBB39
	.quad L$set$76
	.uleb128 0x12
	.ascii "i\0"
	.byte	0x1
	.byte	0xc1
	.byte	0xb
	.long	0x7b6
	.uleb128 0x4
	.byte	0x91
	.sleb128 -15814
	.byte	0
	.byte	0
	.uleb128 0x19
	.quad	LBB41
	.set L$set$77,LBE41-LBB41
	.quad L$set$77
	.long	0x1209
	.uleb128 0x5
	.ascii "test_comprehensive__B_8__T60b\0"
	.long	0x25e
	.long	0x11c6
	.uleb128 0x6
	.long	0x290
	.sleb128 32
	.byte	0
	.uleb128 0x1b
	.long	0x1198
	.uleb128 0x12
	.ascii "key_data\0"
	.byte	0x1
	.byte	0xce
	.byte	0x7
	.long	0x11c6
	.uleb128 0x9
	.byte	0x3
	.quad	_key_data.2
	.uleb128 0x12
	.ascii "key\0"
	.byte	0x1
	.byte	0xcf
	.byte	0x7
	.long	0xa69
	.uleb128 0x4
	.byte	0x91
	.sleb128 -8360
	.uleb128 0x1c
	.set L$set$78,LASF2-Lsection__debug_str
	.long L$set$78
	.byte	0x1
	.byte	0xd0
	.byte	0x7
	.long	0x386
	.uleb128 0x4
	.byte	0x91
	.sleb128 -15815
	.byte	0
	.uleb128 0x19
	.quad	LBB50
	.set L$set$79,LBE50-LBB50
	.quad L$set$79
	.long	0x125e
	.uleb128 0x5
	.ascii "test_comprehensive__B69b__TS82bS\0"
	.long	0x13cd
	.long	0x124f
	.uleb128 0x6
	.long	0x290
	.sleb128 26
	.byte	0
	.uleb128 0x16
	.ascii "S82b\0"
	.long	0x121e
	.uleb128 0x3
	.byte	0x91
	.sleb128 -136
	.byte	0
	.uleb128 0x19
	.quad	LBB52
	.set L$set$80,LBE52-LBB52
	.quad L$set$80
	.long	0x12b3
	.uleb128 0x5
	.ascii "test_comprehensive__B85b__TS98bS\0"
	.long	0x13cd
	.long	0x12a4
	.uleb128 0x6
	.long	0x290
	.sleb128 26
	.byte	0
	.uleb128 0x16
	.ascii "S98b\0"
	.long	0x1273
	.uleb128 0x3
	.byte	0x91
	.sleb128 -136
	.byte	0
	.uleb128 0x19
	.quad	LBB54
	.set L$set$81,LBE54-LBB54
	.quad L$set$81
	.long	0x130b
	.uleb128 0x5
	.ascii "test_comprehensive__B101b__TS114bS\0"
	.long	0x13cd
	.long	0x12fb
	.uleb128 0x6
	.long	0x290
	.sleb128 26
	.byte	0
	.uleb128 0x16
	.ascii "S114b\0"
	.long	0x12c8
	.uleb128 0x3
	.byte	0x91
	.sleb128 -136
	.byte	0
	.uleb128 0x15
	.quad	LBB56
	.set L$set$82,LBE56-LBB56
	.quad L$set$82
	.uleb128 0x5
	.ascii "test_comprehensive__B117b__TS132bS\0"
	.long	0x13cd
	.long	0x134f
	.uleb128 0x6
	.long	0x290
	.sleb128 27
	.byte	0
	.uleb128 0x16
	.ascii "S132b\0"
	.long	0x131c
	.uleb128 0x3
	.byte	0x91
	.sleb128 -136
	.byte	0
	.byte	0
	.uleb128 0x1d
	.ascii "string\0"
	.byte	0x10
	.byte	0x6
	.byte	0
	.long	0x1397
	.uleb128 0x1e
	.ascii "P_ARRAY\0"
	.byte	0x6
	.byte	0
	.long	0x137f
	.byte	0
	.uleb128 0x1f
	.byte	0x8
	.long	0x13a1
	.uleb128 0x1e
	.ascii "P_BOUNDS\0"
	.byte	0x6
	.byte	0
	.long	0x141b
	.byte	0x8
	.byte	0
	.uleb128 0x1b
	.long	0x1360
	.uleb128 0x1b
	.long	0x1360
	.uleb128 0x5
	.ascii "string___XUA\0"
	.long	0x13cd
	.long	0x13cd
	.uleb128 0x20
	.long	0x290
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
	.uleb128 0x3
	.byte	0x1
	.byte	0x8
	.ascii "character\0"
	.uleb128 0x1d
	.ascii "string___XUB\0"
	.byte	0x8
	.byte	0x6
	.byte	0
	.long	0x141b
	.uleb128 0x1e
	.ascii "LB0\0"
	.byte	0x6
	.byte	0
	.long	0x13fb
	.byte	0
	.uleb128 0x21
	.sleb128 2147483647
	.ascii "positive\0"
	.long	0x290
	.uleb128 0x1e
	.ascii "UB0\0"
	.byte	0x6
	.byte	0
	.long	0x13fb
	.byte	0x4
	.byte	0
	.uleb128 0x1f
	.byte	0x8
	.long	0x13da
	.uleb128 0x22
	.ascii "anubis_types__byte_array___XUB\0"
	.byte	0x8
	.byte	0x7
	.byte	0x46
	.byte	0x1d
	.uleb128 0xc
	.ascii "LB0\0"
	.byte	0x2
	.byte	0x12
	.byte	0x9
	.long	0x1452
	.byte	0
	.uleb128 0x23
	.sleb128 0
	.sleb128 2147483647
	.ascii "natural\0"
	.long	0x290
	.uleb128 0xc
	.ascii "UB0\0"
	.byte	0x2
	.byte	0x12
	.byte	0x9
	.long	0x1452
	.byte	0x4
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
	.byte	0
	.byte	0
	.uleb128 0xa
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
	.uleb128 0xb
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
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0xd
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
	.uleb128 0x10
	.uleb128 0x28
	.byte	0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x1c
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0x11
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
	.uleb128 0x12
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
	.uleb128 0x13
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
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x34
	.uleb128 0x19
	.uleb128 0x2
	.uleb128 0x18
	.byte	0
	.byte	0
	.uleb128 0x17
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
	.uleb128 0x1b
	.uleb128 0x26
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x34
	.uleb128 0x19
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
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0x18
	.byte	0
	.byte	0
	.uleb128 0x1d
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
	.uleb128 0x1e
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
	.uleb128 0x1f
	.uleb128 0xf
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x20
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
	.uleb128 0x21
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
	.uleb128 0x22
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
	.byte	0
	.byte	0
	.uleb128 0x23
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
	.byte	0
	.section __DWARF,__debug_pubnames,regular,debug
Lsection__debug_pubnames:
	.long	0x25
	.short	0x2
	.set L$set$83,Ldebug_info0-Lsection__debug_info
	.long L$set$83
	.long	0x1474
	.long	0xb13
	.ascii "test_comprehensive\0"
	.long	0
	.section __DWARF,__debug_pubtypes,regular,debug
Lsection__debug_pubtypes:
	.long	0x513
	.short	0x2
	.set L$set$84,Ldebug_info0-Lsection__debug_info
	.long L$set$84
	.long	0x1474
	.long	0x22b
	.ascii "ada__text_io__TcountB\0"
	.long	0x244
	.ascii "interfaces__unsigned_8\0"
	.long	0x278
	.ascii "anubis_types__TbyteB\0"
	.long	0x290
	.ascii "integer\0"
	.long	0x29b
	.ascii "anubis_types__ed25519_public_key__T21s\0"
	.long	0x2d2
	.ascii "anubis_types__ed25519_public_key\0"
	.long	0x30a
	.ascii "anubis_types__ed25519_secret_key__T23s\0"
	.long	0x386
	.ascii "boolean\0"
	.long	0x341
	.ascii "anubis_types__ed25519_secret_key\0"
	.long	0x396
	.ascii "anubis_types__ed25519_signature__T25s\0"
	.long	0x3cd
	.ascii "anubis_types__ed25519_signature\0"
	.long	0x404
	.ascii "anubis_types__ml_kem_public_key__T33s\0"
	.long	0x43b
	.ascii "anubis_types__ml_kem_public_key\0"
	.long	0x473
	.ascii "anubis_types__ml_kem_secret_key__T35s\0"
	.long	0x4aa
	.ascii "anubis_types__ml_kem_secret_key\0"
	.long	0x4f0
	.ascii "anubis_types__ml_kem_ciphertext__T37s\0"
	.long	0x527
	.ascii "anubis_types__ml_kem_ciphertext\0"
	.long	0x55f
	.ascii "anubis_types__ml_kem_shared_secret__T39s\0"
	.long	0x598
	.ascii "anubis_types__ml_kem_shared_secret\0"
	.long	0x5df
	.ascii "anubis_types__ml_dsa_public_key__T41s\0"
	.long	0x616
	.ascii "anubis_types__ml_dsa_public_key\0"
	.long	0x64e
	.ascii "anubis_types__ml_dsa_secret_key__T43s\0"
	.long	0x685
	.ascii "anubis_types__ml_dsa_secret_key\0"
	.long	0x6cb
	.ascii "anubis_types__ml_dsa_signature__T45s\0"
	.long	0x701
	.ascii "anubis_types__ml_dsa_signature\0"
	.long	0x738
	.ascii "anubis_types__pqc__hybrid_signature\0"
	.long	0x7b6
	.ascii "anubis_types__sss__Tshare_indexB\0"
	.long	0x7da
	.ascii "anubis_types__sss__secret_share__T7s\0"
	.long	0x810
	.ascii "anubis_types__sss__secret_share\0"
	.long	0x870
	.ascii "anubis_key_manager__managed_key__T5s\0"
	.long	0x8a7
	.ascii "anubis_key_manager__key_purpose\0"
	.long	0x954
	.ascii "anubis_types__key_status\0"
	.long	0x9f6
	.ascii "anubis_key_manager__rotation_policy\0"
	.long	0xa69
	.ascii "anubis_key_manager__managed_key\0"
	.long	0x13cd
	.ascii "character\0"
	.long	0x13a1
	.ascii "string___XUA\0"
	.long	0x13da
	.ascii "string___XUB\0"
	.long	0x1360
	.ascii "string\0"
	.long	0
	.section __DWARF,__debug_aranges,regular,debug
Lsection__debug_aranges:
	.long	0x2c
	.short	0x2
	.set L$set$85,Ldebug_info0-Lsection__debug_info
	.long L$set$85
	.byte	0x8
	.byte	0
	.short	0
	.short	0
	.quad	Ltext0
	.set L$set$86,Letext0-Ltext0
	.quad L$set$86
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
LASF2:
	.ascii "success\0"
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
