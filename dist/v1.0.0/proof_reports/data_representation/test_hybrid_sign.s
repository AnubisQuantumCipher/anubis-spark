	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
Ltext0:
	.file 1 "/Users/sicarii/Desktop/anubis-spark/src/test_hybrid_sign.adb"
	.const
	.align	3
lC23:
	.ascii "============================================================="
	.align	3
lC24:
	.ascii "ANUBIS-SPARK: Hybrid Signature Test (Ed25519 + ML-DSA-87)"
	.align	3
lC25:
	.ascii "PLATINUM LEVEL: Dual signatures for quantum resistance"
	.align	3
lC26:
	.ascii "1. Generating Ed25519 keypair..."
	.align	3
lC27:
	.ascii "   [FAIL] Ed25519 keypair generation failed"
	.align	3
lC28:
	.ascii "   [OK] Ed25519 keypair generated"
	.align	3
lC29:
	.ascii "2. Generating ML-DSA-87 keypair..."
	.align	3
lC30:
	.ascii "   [FAIL] ML-DSA-87 keypair generation failed"
	.align	3
lC31:
	.ascii "   [OK] ML-DSA-87 keypair generated"
	.align	3
lC32:
	.ascii "3. Creating hybrid signature..."
	.align	3
lC33:
	.ascii "   Message: \"Hello, World!\""
	.align	3
lC34:
	.ascii "   [FAIL] Hybrid signature creation failed"
	.align	3
lC35:
	.ascii "   [OK] Hybrid signature created"
	.align	3
lC36:
	.ascii "   - Ed25519 signature: 64 bytes"
	.align	3
lC37:
	.ascii "   - ML-DSA-87 signature: 4627 bytes"
	.align	3
lC38:
	.ascii "   - Total: 4691 bytes"
	.align	3
lC39:
	.ascii "4. Verifying hybrid signature..."
	.align	3
lC40:
	.ascii "   [FAIL] Hybrid signature verification failed"
	.align	3
lC41:
	.ascii "   [OK] Hybrid signature verified successfully"
	.align	3
lC42:
	.ascii "   - Both Ed25519 AND ML-DSA-87 verified"
	.align	3
lC43:
	.ascii "5. Testing tamper detection..."
	.align	3
lC44:
	.ascii "   [FAIL] Tampered message was accepted!"
	.align	3
lC45:
	.ascii "   [OK] Tampered message correctly rejected"
	.align	3
lC46:
	.ascii "6. Secure cleanup..."
	.align	3
lC47:
	.ascii "   [OK] Secret keys zeroized"
	.align	3
lC48:
	.ascii "PLATINUM LEVEL HYBRID SIGNATURE TEST: PASSED"
	.align	3
lC49:
	.ascii "Security Properties:"
	.align	3
lC50:
	.ascii "  - Quantum Resistant: ML-DSA-87 (NIST Level 5)"
	.align	3
lC51:
	.ascii "  - Classical Security: Ed25519 (256-bit)"
	.align	3
lC52:
	.ascii "  - Defense in Depth: BOTH must be broken"
	.align	3
lC53:
	.ascii "  - Tamper Detection: Verified"
	.align	3
lC54:
	.ascii "  - Memory Safety: SPARK-proven zeroization"
	.text
	.align	2
	.globl __ada_test_hybrid_sign
__ada_test_hybrid_sign:
LFB1:
	.loc 1 13 1
	sub	x10, sp, #16384
	str	xzr, [x10]
	sub	x10, x10, #4096
	str	xzr, [x10]
	sub	x10, x10, #4096
	str	xzr, [x10]
	sub	x10, x10, #4096
	str	xzr, [x10, 3440]
	stp	x29, x30, [sp, -80]!
LCFI0:
	mov	x29, sp
LCFI1:
	stp	x20, x21, [sp, 16]
	stp	x22, x23, [sp, 32]
	stp	x24, x25, [sp, 48]
	stp	x26, x27, [sp, 64]
	mov	x13, 12864
	sub	sp, sp, x13
LCFI2:
	.loc 1 13 1
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x1, [x0]
	str	x1, [x29, -8]
	mov	x1, 0
	.loc 1 21 4
	sub	x0, x29, #8192
	sub	x0, x0, #4048
	bl	_anubis_types__ed25519_secret_keyIP
	.loc 1 25 4
	sub	x0, x29, #4096
	sub	x0, x0, #816
	bl	_anubis_types__ml_dsa_secret_keyIP
LBB2:
	.loc 1 35 4
	adrp	x0, lC23@PAGE
	add	x20, x0, lC23@PAGEOFF;
	adrp	x0, lC0@PAGE
	add	x21, x0, lC0@PAGEOFF;
	mov	x0, x20
	mov	x1, x21
	bl	_ada__text_io__put_line__2
LBE2:
LBB3:
	.loc 1 36 4
	adrp	x0, lC24@PAGE
	add	x22, x0, lC24@PAGEOFF;
	adrp	x0, lC1@PAGE
	add	x23, x0, lC1@PAGEOFF;
	mov	x0, x22
	mov	x1, x23
	bl	_ada__text_io__put_line__2
LBE3:
LBB4:
	.loc 1 37 4
	adrp	x0, lC25@PAGE
	add	x24, x0, lC25@PAGEOFF;
	adrp	x0, lC2@PAGE
	add	x25, x0, lC2@PAGEOFF;
	mov	x0, x24
	mov	x1, x25
	bl	_ada__text_io__put_line__2
LBE4:
LBB5:
	.loc 1 38 4
	adrp	x0, lC23@PAGE
	add	x26, x0, lC23@PAGEOFF;
	adrp	x0, lC0@PAGE
	add	x27, x0, lC0@PAGEOFF;
	mov	x0, x26
	mov	x1, x27
	bl	_ada__text_io__put_line__2
LBE5:
	.loc 1 39 4
	mov	w0, 1
	bl	_ada__text_io__new_line__2
LBB6:
	.loc 1 42 4
	adrp	x0, lC26@PAGE
	add	x0, x0, lC26@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3520]
	adrp	x0, lC3@PAGE
	add	x0, x0, lC3@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3528]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, -64]
	bl	_ada__text_io__put_line__2
LBE6:
	.loc 1 43 13
	sub	x1, x29, #8192
	sub	x1, x1, #4048
	sub	x0, x29, #8192
	sub	x0, x0, #4080
	bl	_anubis_types__classical__ed25519_generate_keypair
	.loc 1 43 13 is_stmt 0 discriminator 1
	sub	x1, x29, #16384
	strb	w0, [x1, 4094]
	.loc 1 49 7 is_stmt 1
	sub	x0, x29, #16384
	ldrb	w0, [x0, 4094]
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 49 4
	cmp	w0, 0
	beq	L2
LBB7:
	.loc 1 50 7
	adrp	x0, lC27@PAGE
	add	x0, x0, lC27@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3536]
	adrp	x0, lC4@PAGE
	add	x0, x0, lC4@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3544]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, -48]
	bl	_ada__text_io__put_line__2
LBE7:
	.loc 1 51 7
	b	L1
L2:
LBB8:
	.loc 1 53 4
	adrp	x0, lC28@PAGE
	add	x0, x0, lC28@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3552]
	adrp	x0, lC5@PAGE
	add	x0, x0, lC5@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3560]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, -32]
	bl	_ada__text_io__put_line__2
LBE8:
	.loc 1 54 4
	mov	w0, 1
	bl	_ada__text_io__new_line__2
LBB9:
	.loc 1 57 4
	adrp	x0, lC29@PAGE
	add	x0, x0, lC29@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3568]
	adrp	x0, lC6@PAGE
	add	x0, x0, lC6@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3576]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, -16]
	bl	_ada__text_io__put_line__2
LBE9:
	.loc 1 58 7
	sub	x1, x29, #4096
	sub	x1, x1, #816
	sub	x0, x29, #8192
	sub	x0, x0, #4008
	bl	_anubis_types__pqc__ml_dsa_generate_keypair
	.loc 1 58 7 is_stmt 0 discriminator 1
	sub	x1, x29, #16384
	strb	w0, [x1, 4094]
	.loc 1 64 7 is_stmt 1
	sub	x0, x29, #16384
	ldrb	w0, [x0, 4094]
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 64 4
	cmp	w0, 0
	beq	L4
LBB10:
	.loc 1 65 7
	adrp	x0, lC30@PAGE
	add	x0, x0, lC30@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3584]
	adrp	x0, lC7@PAGE
	add	x0, x0, lC7@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3592]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0]
	bl	_ada__text_io__put_line__2
LBE10:
	.loc 1 66 16
	sub	x0, x29, #8192
	sub	x0, x0, #4048
	bl	_anubis_types__classical__zeroize_ed25519_secret
	.loc 1 67 7
	b	L1
L4:
LBB11:
	.loc 1 69 4
	adrp	x0, lC31@PAGE
	add	x0, x0, lC31@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3600]
	adrp	x0, lC8@PAGE
	add	x0, x0, lC8@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3608]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 16]
	bl	_ada__text_io__put_line__2
LBE11:
	.loc 1 70 4
	mov	w0, 1
	bl	_ada__text_io__new_line__2
LBB12:
	.loc 1 73 4
	adrp	x0, lC32@PAGE
	add	x0, x0, lC32@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3616]
	adrp	x0, lC9@PAGE
	add	x0, x0, lC9@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3624]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 32]
	bl	_ada__text_io__put_line__2
LBE12:
LBB13:
	.loc 1 74 4
	adrp	x0, lC33@PAGE
	add	x0, x0, lC33@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3632]
	adrp	x0, lC10@PAGE
	add	x0, x0, lC10@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3640]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 48]
	bl	_ada__text_io__put_line__2
LBE13:
	.loc 1 75 7
	adrp	x0, _test_message.0@PAGE
	add	x0, x0, _test_message.0@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3648]
	adrp	x0, lC11@PAGE
	add	x0, x0, lC11@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3656]
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
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 64]
	bl	_anubis_types__pqc__hybrid_sign
	.loc 1 75 7 is_stmt 0 discriminator 1
	sub	x1, x29, #16384
	strb	w0, [x1, 4094]
	.loc 1 83 7 is_stmt 1
	sub	x0, x29, #16384
	ldrb	w0, [x0, 4094]
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 83 4
	cmp	w0, 0
	beq	L5
LBB14:
	.loc 1 84 7
	adrp	x0, lC34@PAGE
	add	x0, x0, lC34@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3664]
	adrp	x0, lC12@PAGE
	add	x0, x0, lC12@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3672]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 80]
	bl	_ada__text_io__put_line__2
LBE14:
	.loc 1 85 16
	sub	x0, x29, #8192
	sub	x0, x0, #4048
	bl	_anubis_types__classical__zeroize_ed25519_secret
	.loc 1 86 10
	sub	x0, x29, #4096
	sub	x0, x0, #816
	bl	_anubis_types__pqc__zeroize_ml_dsa_secret
	.loc 1 87 7
	b	L1
L5:
LBB15:
	.loc 1 89 4
	adrp	x0, lC35@PAGE
	add	x0, x0, lC35@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3680]
	adrp	x0, lC3@PAGE
	add	x0, x0, lC3@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3688]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 96]
	bl	_ada__text_io__put_line__2
LBE15:
LBB16:
	.loc 1 90 4
	adrp	x0, lC36@PAGE
	add	x0, x0, lC36@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3696]
	adrp	x0, lC3@PAGE
	add	x0, x0, lC3@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3704]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 112]
	bl	_ada__text_io__put_line__2
LBE16:
LBB17:
	.loc 1 91 4
	adrp	x0, lC37@PAGE
	add	x0, x0, lC37@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3712]
	adrp	x0, lC13@PAGE
	add	x0, x0, lC13@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3720]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 128]
	bl	_ada__text_io__put_line__2
LBE17:
LBB18:
	.loc 1 92 4
	adrp	x0, lC38@PAGE
	add	x0, x0, lC38@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3728]
	adrp	x0, lC14@PAGE
	add	x0, x0, lC14@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3736]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 144]
	bl	_ada__text_io__put_line__2
LBE18:
	.loc 1 93 4
	mov	w0, 1
	bl	_ada__text_io__new_line__2
LBB19:
	.loc 1 96 4
	adrp	x0, lC39@PAGE
	add	x0, x0, lC39@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3744]
	adrp	x0, lC3@PAGE
	add	x0, x0, lC3@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3752]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 160]
	bl	_ada__text_io__put_line__2
LBE19:
	.loc 1 97 24
	adrp	x0, _test_message.0@PAGE
	add	x0, x0, _test_message.0@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3760]
	adrp	x0, lC11@PAGE
	add	x0, x0, lC11@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3768]
	sub	x2, x29, #8192
	sub	x2, x2, #4008
	sub	x1, x29, #8192
	sub	x1, x1, #4080
	sub	x0, x29, #8192
	sub	x0, x0, #1416
	mov	x4, x2
	mov	x3, x1
	mov	x2, x0
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 176]
	bl	_anubis_types__pqc__hybrid_verify
	.loc 1 97 24 is_stmt 0 discriminator 1
	sub	x1, x29, #16384
	strb	w0, [x1, 4095]
	.loc 1 104 7 is_stmt 1
	sub	x0, x29, #16384
	ldrb	w0, [x0, 4095]
	eor	w0, w0, 1
	and	w0, w0, 255
	.loc 1 104 4
	cmp	w0, 0
	beq	L6
LBB20:
	.loc 1 105 7
	adrp	x0, lC40@PAGE
	add	x0, x0, lC40@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3776]
	adrp	x0, lC15@PAGE
	add	x0, x0, lC15@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3784]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 192]
	bl	_ada__text_io__put_line__2
LBE20:
	.loc 1 106 16
	sub	x0, x29, #8192
	sub	x0, x0, #4048
	bl	_anubis_types__classical__zeroize_ed25519_secret
	.loc 1 107 10
	sub	x0, x29, #4096
	sub	x0, x0, #816
	bl	_anubis_types__pqc__zeroize_ml_dsa_secret
	.loc 1 108 7
	b	L1
L6:
LBB21:
	.loc 1 110 4
	adrp	x0, lC41@PAGE
	add	x0, x0, lC41@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3792]
	adrp	x0, lC15@PAGE
	add	x0, x0, lC15@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3800]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 208]
	bl	_ada__text_io__put_line__2
LBE21:
LBB22:
	.loc 1 111 4
	adrp	x0, lC42@PAGE
	add	x0, x0, lC42@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3808]
	adrp	x0, lC16@PAGE
	add	x0, x0, lC16@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3816]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 224]
	bl	_ada__text_io__put_line__2
LBE22:
	.loc 1 112 4
	mov	w0, 1
	bl	_ada__text_io__new_line__2
LBB23:
	.loc 1 115 4
	adrp	x0, lC43@PAGE
	add	x0, x0, lC43@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3824]
	adrp	x0, lC17@PAGE
	add	x0, x0, lC17@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3832]
	sub	x0, x29, #12288
	sub	x0, x0, #512
	ldp	x0, x1, [x0, 240]
	bl	_ada__text_io__put_line__2
LBE23:
LBB24:
	.loc 1 117 7
	mov	w0, 72
	sub	x1, x29, #12288
	strb	w0, [x1]
	mov	w0, 101
	sub	x1, x29, #12288
	strb	w0, [x1, 1]
	mov	w0, 108
	sub	x1, x29, #12288
	strb	w0, [x1, 2]
	mov	w0, 108
	sub	x1, x29, #12288
	strb	w0, [x1, 3]
	mov	w0, 111
	sub	x1, x29, #12288
	strb	w0, [x1, 4]
	mov	w0, 44
	sub	x1, x29, #12288
	strb	w0, [x1, 5]
	mov	w0, 32
	sub	x1, x29, #12288
	strb	w0, [x1, 6]
	mov	w0, 87
	sub	x1, x29, #12288
	strb	w0, [x1, 7]
	mov	w0, 111
	sub	x1, x29, #12288
	strb	w0, [x1, 8]
	mov	w0, 114
	sub	x1, x29, #12288
	strb	w0, [x1, 9]
	mov	w0, 108
	sub	x1, x29, #12288
	strb	w0, [x1, 10]
	mov	w0, 100
	sub	x1, x29, #12288
	strb	w0, [x1, 11]
	mov	w0, 33
	sub	x1, x29, #12288
	strb	w0, [x1, 12]
	.loc 1 120 52
	sub	x0, x29, #12288
	ldrb	w0, [x0, 1]
	add	w0, w0, 1
	and	w0, w0, 255
	.loc 1 120 28
	sub	x1, x29, #12288
	strb	w0, [x1, 1]
	.loc 1 122 27
	sub	x0, x29, #12288
	sub	x1, x29, #16384
	str	x0, [x1, 3840]
	adrp	x0, lC11@PAGE
	add	x0, x0, lC11@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3848]
	sub	x2, x29, #8192
	sub	x2, x2, #4008
	sub	x1, x29, #8192
	sub	x1, x1, #4080
	sub	x0, x29, #8192
	sub	x0, x0, #1416
	mov	x4, x2
	mov	x3, x1
	mov	x2, x0
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -256]
	bl	_anubis_types__pqc__hybrid_verify
	.loc 1 122 27 is_stmt 0 discriminator 1
	sub	x1, x29, #16384
	strb	w0, [x1, 4095]
	.loc 1 129 7 is_stmt 1
	sub	x0, x29, #16384
	ldrb	w0, [x0, 4095]
	cmp	w0, 0
	beq	L7
LBB25:
	.loc 1 130 10
	adrp	x0, lC44@PAGE
	add	x0, x0, lC44@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3856]
	adrp	x0, lC16@PAGE
	add	x0, x0, lC16@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3864]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -240]
	bl	_ada__text_io__put_line__2
LBE25:
	.loc 1 131 19
	sub	x0, x29, #8192
	sub	x0, x0, #4048
	bl	_anubis_types__classical__zeroize_ed25519_secret
	.loc 1 132 13
	sub	x0, x29, #4096
	sub	x0, x0, #816
	bl	_anubis_types__pqc__zeroize_ml_dsa_secret
	b	L1
L7:
LBB26:
	.loc 1 135 7
	adrp	x0, lC45@PAGE
	add	x0, x0, lC45@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3872]
	adrp	x0, lC4@PAGE
	add	x0, x0, lC4@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3880]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -224]
	bl	_ada__text_io__put_line__2
LBE26:
LBE24:
	.loc 1 137 4
	mov	w0, 1
	bl	_ada__text_io__new_line__2
LBB27:
	.loc 1 140 4
	adrp	x0, lC46@PAGE
	add	x0, x0, lC46@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3888]
	adrp	x0, lC18@PAGE
	add	x0, x0, lC18@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3896]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -208]
	bl	_ada__text_io__put_line__2
LBE27:
	.loc 1 141 13
	sub	x0, x29, #8192
	sub	x0, x0, #4048
	bl	_anubis_types__classical__zeroize_ed25519_secret
	.loc 1 142 7
	sub	x0, x29, #4096
	sub	x0, x0, #816
	bl	_anubis_types__pqc__zeroize_ml_dsa_secret
LBB28:
	.loc 1 143 4
	adrp	x0, lC47@PAGE
	add	x0, x0, lC47@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3904]
	adrp	x0, lC19@PAGE
	add	x0, x0, lC19@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3912]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -192]
	bl	_ada__text_io__put_line__2
LBE28:
	.loc 1 144 4
	mov	w0, 1
	bl	_ada__text_io__new_line__2
LBB29:
	.loc 1 147 4
	adrp	x0, lC23@PAGE
	add	x0, x0, lC23@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3920]
	adrp	x0, lC0@PAGE
	add	x0, x0, lC0@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3928]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -176]
	bl	_ada__text_io__put_line__2
LBE29:
LBB30:
	.loc 1 148 4
	adrp	x0, lC48@PAGE
	add	x0, x0, lC48@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3936]
	adrp	x0, lC20@PAGE
	add	x0, x0, lC20@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3944]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -160]
	bl	_ada__text_io__put_line__2
LBE30:
LBB31:
	.loc 1 149 4
	adrp	x0, lC23@PAGE
	add	x0, x0, lC23@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3952]
	adrp	x0, lC0@PAGE
	add	x0, x0, lC0@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3960]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -144]
	bl	_ada__text_io__put_line__2
LBE31:
LBB32:
	.loc 1 150 4
	adrp	x0, lC49@PAGE
	add	x0, x0, lC49@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3968]
	adrp	x0, lC18@PAGE
	add	x0, x0, lC18@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3976]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -128]
	bl	_ada__text_io__put_line__2
LBE32:
LBB33:
	.loc 1 151 4
	adrp	x0, lC50@PAGE
	add	x0, x0, lC50@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3984]
	adrp	x0, lC21@PAGE
	add	x0, x0, lC21@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 3992]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -112]
	bl	_ada__text_io__put_line__2
LBE33:
LBB34:
	.loc 1 152 4
	adrp	x0, lC51@PAGE
	add	x0, x0, lC51@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4000]
	adrp	x0, lC22@PAGE
	add	x0, x0, lC22@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4008]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -96]
	bl	_ada__text_io__put_line__2
LBE34:
LBB35:
	.loc 1 153 4
	adrp	x0, lC52@PAGE
	add	x0, x0, lC52@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4016]
	adrp	x0, lC22@PAGE
	add	x0, x0, lC22@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4024]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -80]
	bl	_ada__text_io__put_line__2
LBE35:
LBB36:
	.loc 1 154 4
	adrp	x0, lC53@PAGE
	add	x0, x0, lC53@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4032]
	adrp	x0, lC17@PAGE
	add	x0, x0, lC17@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4040]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -64]
	bl	_ada__text_io__put_line__2
LBE36:
LBB37:
	.loc 1 155 4
	adrp	x0, lC54@PAGE
	add	x0, x0, lC54@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4048]
	adrp	x0, lC4@PAGE
	add	x0, x0, lC4@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4056]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -48]
	bl	_ada__text_io__put_line__2
LBE37:
LBB38:
	.loc 1 156 4
	adrp	x0, lC23@PAGE
	add	x0, x0, lC23@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4064]
	adrp	x0, lC0@PAGE
	add	x0, x0, lC0@PAGEOFF;
	sub	x1, x29, #16384
	str	x0, [x1, 4072]
	sub	x0, x29, #12288
	ldp	x0, x1, [x0, -32]
	bl	_ada__text_io__put_line__2
LBE38:
	.loc 1 158 5
	nop
L1:
	adrp	x0, ___stack_chk_guard@GOTPAGE
	ldr	x0, [x0, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x2, [x29, -8]
	ldr	x1, [x0]
	subs	x2, x2, x1
	mov	x1, 0
	beq	L9
	bl	___stack_chk_fail
L9:
	mov	sp, x29
	ldp	x20, x21, [sp, 16]
	ldp	x22, x23, [sp, 32]
	ldp	x24, x25, [sp, 48]
	ldp	x26, x27, [sp, 64]
	ldp	x29, x30, [sp], 80
LCFI3:
	ret
LFE1:
	.const
	.align	2
lC0:
	.word	1
	.word	61
	.align	2
lC1:
	.word	1
	.word	57
	.align	2
lC2:
	.word	1
	.word	54
	.align	2
lC3:
	.word	1
	.word	32
	.align	2
lC4:
	.word	1
	.word	43
	.align	2
lC5:
	.word	1
	.word	33
	.align	2
lC6:
	.word	1
	.word	34
	.align	2
lC7:
	.word	1
	.word	45
	.align	2
lC8:
	.word	1
	.word	35
	.align	2
lC9:
	.word	1
	.word	31
	.align	2
lC10:
	.word	1
	.word	27
	.align	2
lC11:
	.word	0
	.word	12
	.align	2
lC12:
	.word	1
	.word	42
	.align	2
lC13:
	.word	1
	.word	36
	.align	2
lC14:
	.word	1
	.word	22
	.align	2
lC15:
	.word	1
	.word	46
	.align	2
lC16:
	.word	1
	.word	40
	.align	2
lC17:
	.word	1
	.word	30
	.align	2
lC18:
	.word	1
	.word	20
	.align	2
lC19:
	.word	1
	.word	28
	.align	2
lC20:
	.word	1
	.word	44
	.align	2
lC21:
	.word	1
	.word	47
	.align	2
lC22:
	.word	1
	.word	41
	.text
	.const
	.align	3
_test_message.0:
	.byte	72
	.byte	101
	.byte	108
	.byte	108
	.byte	111
	.byte	44
	.byte	32
	.byte	87
	.byte	111
	.byte	114
	.byte	108
	.byte	100
	.byte	33
	.space 3
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
	.quad	LFB1
	.set L$set$3,LFE1-LFB1
	.quad L$set$3
	.byte	0x4
	.set L$set$4,LCFI0-LFB1
	.long L$set$4
	.byte	0xe
	.uleb128 0x50
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$5,LCFI1-LCFI0
	.long L$set$5
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$6,LCFI2-LCFI1
	.long L$set$6
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
	.section __TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
EH_frame1:
	.set L$set$8,LECIE1-LSCIE1
	.long L$set$8
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
LSFDE3:
	.set L$set$9,LEFDE3-LASFDE3
	.long L$set$9
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB1-.
	.set L$set$10,LFE1-LFB1
	.quad L$set$10
	.uleb128 0
	.byte	0x4
	.set L$set$11,LCFI0-LFB1
	.long L$set$11
	.byte	0xe
	.uleb128 0x50
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$12,LCFI1-LCFI0
	.long L$set$12
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$13,LCFI2-LCFI1
	.long L$set$13
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
	.set L$set$14,LCFI3-LCFI2
	.long L$set$14
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
LEFDE3:
	.text
Letext0:
	.file 2 "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_types.ads"
	.file 3 "/Users/sicarii/Desktop/anubis-spark/src/crypto/anubis_types-pqc.ads"
	.section __DWARF,__debug_info,regular,debug
Lsection__debug_info:
Ldebug_info0:
	.long	0x71c
	.short	0x4
	.set L$set$15,Ldebug_abbrev0-Lsection__debug_abbrev
	.long L$set$15
	.byte	0x8
	.uleb128 0x1
	.ascii "GNU Ada 14.1.0 -O0 -gnatA -g -gnat2012 -gnatwa -gnatwe -fstack-check=specific -fstack-protector-strong -gnatR2js -gnatws -gnatec=/private/var/folders/bg/pt9l6y1j47q642kp3z5blrmh0000gn/T/GPR.71672/GNAT-TEMP-000003.TMP -gnatem=/private/var/folders/bg/pt9l6y1j47q642kp3z5blrmh0000gn/T/GPR.71672/GNAT-TEMP-000004.TMP -mmacosx-version-min=16.0.0 -mcpu=apple-m1 -mlittle-endian -mabi=lp64 -fPIC\0"
	.byte	0xd
	.ascii "/Users/sicarii/Desktop/anubis-spark/src/test_hybrid_sign.adb\0"
	.ascii "/Users/sicarii/Desktop/anubis-spark/obj/gnatprove/data_representation\0"
	.quad	Ltext0
	.set L$set$16,Letext0-Ltext0
	.quad L$set$16
	.set L$set$17,Ldebug_line0-Lsection__debug_line
	.long L$set$17
	.uleb128 0x2
	.byte	0x1
	.byte	0x7
	.ascii "interfaces__unsigned_8\0"
	.uleb128 0x3
	.byte	0
	.byte	0xff
	.ascii "anubis_types__byte\0"
	.long	0x25d
	.uleb128 0x4
	.byte	0x1
	.byte	0x7
	.ascii "anubis_types__TbyteB\0"
	.uleb128 0x2
	.byte	0x4
	.byte	0x5
	.ascii "integer\0"
	.uleb128 0x5
	.ascii "anubis_types__ed25519_public_key__T21s\0"
	.long	0x243
	.long	0x2b7
	.uleb128 0x6
	.long	0x275
	.sleb128 32
	.byte	0
	.uleb128 0x7
	.ascii "anubis_types__ed25519_public_key\0"
	.byte	0x20
	.byte	0x2
	.byte	0x9e
	.byte	0x9
	.long	0x2ef
	.uleb128 0x8
	.set L$set$18,LASF0-Lsection__debug_str
	.long L$set$18
	.byte	0x2
	.byte	0x9f
	.byte	0x7
	.long	0x280
	.byte	0
	.byte	0
	.uleb128 0x5
	.ascii "anubis_types__ed25519_secret_key__T23s\0"
	.long	0x243
	.long	0x326
	.uleb128 0x6
	.long	0x275
	.sleb128 32
	.byte	0
	.uleb128 0x7
	.ascii "anubis_types__ed25519_secret_key\0"
	.byte	0x21
	.byte	0x2
	.byte	0xa2
	.byte	0x9
	.long	0x36d
	.uleb128 0x8
	.set L$set$19,LASF0-Lsection__debug_str
	.long L$set$19
	.byte	0x2
	.byte	0xa3
	.byte	0x7
	.long	0x2ef
	.byte	0
	.uleb128 0x9
	.ascii "valid\0"
	.byte	0x2
	.byte	0xa4
	.byte	0x7
	.long	0x36d
	.byte	0x20
	.byte	0
	.uleb128 0x2
	.byte	0x1
	.byte	0x2
	.ascii "boolean\0"
	.uleb128 0x5
	.ascii "anubis_types__ml_dsa_public_key__T41s\0"
	.long	0x243
	.long	0x3af
	.uleb128 0x6
	.long	0x275
	.sleb128 2592
	.byte	0
	.uleb128 0xa
	.ascii "anubis_types__ml_dsa_public_key\0"
	.short	0xa20
	.byte	0x2
	.byte	0xca
	.byte	0x9
	.long	0x3e7
	.uleb128 0x8
	.set L$set$20,LASF0-Lsection__debug_str
	.long L$set$20
	.byte	0x2
	.byte	0xcb
	.byte	0x7
	.long	0x378
	.byte	0
	.byte	0
	.uleb128 0x5
	.ascii "anubis_types__ml_dsa_secret_key__T43s\0"
	.long	0x243
	.long	0x41e
	.uleb128 0x6
	.long	0x275
	.sleb128 4896
	.byte	0
	.uleb128 0xa
	.ascii "anubis_types__ml_dsa_secret_key\0"
	.short	0x1321
	.byte	0x2
	.byte	0xce
	.byte	0x9
	.long	0x466
	.uleb128 0x8
	.set L$set$21,LASF0-Lsection__debug_str
	.long L$set$21
	.byte	0x2
	.byte	0xcf
	.byte	0x7
	.long	0x3e7
	.byte	0
	.uleb128 0xb
	.ascii "valid\0"
	.byte	0x2
	.byte	0xd0
	.byte	0x7
	.long	0x36d
	.short	0x1320
	.byte	0
	.uleb128 0x5
	.ascii "anubis_types__ed25519_signature__T25s\0"
	.long	0x243
	.long	0x49d
	.uleb128 0x6
	.long	0x275
	.sleb128 64
	.byte	0
	.uleb128 0x7
	.ascii "anubis_types__ed25519_signature\0"
	.byte	0x40
	.byte	0x2
	.byte	0xa7
	.byte	0x9
	.long	0x4d4
	.uleb128 0x8
	.set L$set$22,LASF0-Lsection__debug_str
	.long L$set$22
	.byte	0x2
	.byte	0xa8
	.byte	0x7
	.long	0x466
	.byte	0
	.byte	0
	.uleb128 0x5
	.ascii "anubis_types__ml_dsa_signature__T45s\0"
	.long	0x243
	.long	0x50a
	.uleb128 0x6
	.long	0x275
	.sleb128 4627
	.byte	0
	.uleb128 0xa
	.ascii "anubis_types__ml_dsa_signature\0"
	.short	0x1213
	.byte	0x2
	.byte	0xd3
	.byte	0x9
	.long	0x541
	.uleb128 0x8
	.set L$set$23,LASF0-Lsection__debug_str
	.long L$set$23
	.byte	0x2
	.byte	0xd4
	.byte	0x7
	.long	0x4d4
	.byte	0
	.byte	0
	.uleb128 0xa
	.ascii "anubis_types__pqc__hybrid_signature\0"
	.short	0x1253
	.byte	0x3
	.byte	0xc6
	.byte	0x9
	.long	0x599
	.uleb128 0x9
	.ascii "ed25519_sig\0"
	.byte	0x3
	.byte	0xc7
	.byte	0x7
	.long	0x49d
	.byte	0
	.uleb128 0x9
	.ascii "ml_dsa_sig\0"
	.byte	0x3
	.byte	0xc8
	.byte	0x7
	.long	0x50a
	.byte	0x40
	.byte	0
	.uleb128 0x4
	.byte	0x4
	.byte	0x5
	.ascii "ada__text_io__TcountB\0"
	.uleb128 0xc
	.ascii "test_hybrid_sign\0"
	.byte	0x1
	.byte	0xd
	.byte	0x1
	.ascii "_ada_test_hybrid_sign\0"
	.quad	LFB1
	.set L$set$24,LFE1-LFB1
	.quad L$set$24
	.uleb128 0x1
	.byte	0x9c
	.uleb128 0x5
	.ascii "test_hybrid_sign__T2b\0"
	.long	0x243
	.long	0x616
	.uleb128 0xd
	.long	0x275
	.sleb128 0
	.sleb128 12
	.byte	0
	.uleb128 0xe
	.long	0x5ef
	.uleb128 0xf
	.ascii "test_message\0"
	.byte	0x1
	.byte	0xf
	.byte	0x4
	.long	0x616
	.uleb128 0x9
	.byte	0x3
	.quad	_test_message.0
	.uleb128 0xf
	.ascii "ed25519_public\0"
	.byte	0x1
	.byte	0x14
	.byte	0x4
	.long	0x2b7
	.uleb128 0x4
	.byte	0x91
	.sleb128 -12352
	.uleb128 0xf
	.ascii "ed25519_secret\0"
	.byte	0x1
	.byte	0x15
	.byte	0x4
	.long	0x326
	.uleb128 0x4
	.byte	0x91
	.sleb128 -12320
	.uleb128 0xf
	.ascii "ml_dsa_public\0"
	.byte	0x1
	.byte	0x18
	.byte	0x4
	.long	0x3af
	.uleb128 0x4
	.byte	0x91
	.sleb128 -12280
	.uleb128 0xf
	.ascii "ml_dsa_secret\0"
	.byte	0x1
	.byte	0x19
	.byte	0x4
	.long	0x41e
	.uleb128 0x3
	.byte	0x91
	.sleb128 -4992
	.uleb128 0xf
	.ascii "signature\0"
	.byte	0x1
	.byte	0x1c
	.byte	0x4
	.long	0x541
	.uleb128 0x4
	.byte	0x91
	.sleb128 -9688
	.uleb128 0xf
	.ascii "success\0"
	.byte	0x1
	.byte	0x1f
	.byte	0x4
	.long	0x36d
	.uleb128 0x4
	.byte	0x91
	.sleb128 -12370
	.uleb128 0xf
	.ascii "verify_result\0"
	.byte	0x1
	.byte	0x20
	.byte	0x4
	.long	0x36d
	.uleb128 0x4
	.byte	0x91
	.sleb128 -12369
	.uleb128 0x10
	.quad	LBB24
	.set L$set$25,LBE24-LBB24
	.quad L$set$25
	.uleb128 0xf
	.ascii "tampered_message\0"
	.byte	0x1
	.byte	0x75
	.byte	0x7
	.long	0x5ef
	.uleb128 0x4
	.byte	0x91
	.sleb128 -12368
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
	.uleb128 0xe
	.uleb128 0x26
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x34
	.uleb128 0x19
	.byte	0
	.byte	0
	.uleb128 0xf
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
	.uleb128 0x10
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
	.long	0x23
	.short	0x2
	.set L$set$26,Ldebug_info0-Lsection__debug_info
	.long L$set$26
	.long	0x720
	.long	0x5b2
	.ascii "test_hybrid_sign\0"
	.long	0
	.section __DWARF,__debug_pubtypes,regular,debug
Lsection__debug_pubtypes:
	.long	0x272
	.short	0x2
	.set L$set$27,Ldebug_info0-Lsection__debug_info
	.long L$set$27
	.long	0x720
	.long	0x229
	.ascii "interfaces__unsigned_8\0"
	.long	0x25d
	.ascii "anubis_types__TbyteB\0"
	.long	0x275
	.ascii "integer\0"
	.long	0x280
	.ascii "anubis_types__ed25519_public_key__T21s\0"
	.long	0x2b7
	.ascii "anubis_types__ed25519_public_key\0"
	.long	0x2ef
	.ascii "anubis_types__ed25519_secret_key__T23s\0"
	.long	0x36d
	.ascii "boolean\0"
	.long	0x326
	.ascii "anubis_types__ed25519_secret_key\0"
	.long	0x378
	.ascii "anubis_types__ml_dsa_public_key__T41s\0"
	.long	0x3af
	.ascii "anubis_types__ml_dsa_public_key\0"
	.long	0x3e7
	.ascii "anubis_types__ml_dsa_secret_key__T43s\0"
	.long	0x41e
	.ascii "anubis_types__ml_dsa_secret_key\0"
	.long	0x466
	.ascii "anubis_types__ed25519_signature__T25s\0"
	.long	0x49d
	.ascii "anubis_types__ed25519_signature\0"
	.long	0x4d4
	.ascii "anubis_types__ml_dsa_signature__T45s\0"
	.long	0x50a
	.ascii "anubis_types__ml_dsa_signature\0"
	.long	0x541
	.ascii "anubis_types__pqc__hybrid_signature\0"
	.long	0x599
	.ascii "ada__text_io__TcountB\0"
	.long	0
	.section __DWARF,__debug_aranges,regular,debug
Lsection__debug_aranges:
	.long	0x2c
	.short	0x2
	.set L$set$28,Ldebug_info0-Lsection__debug_info
	.long L$set$28
	.byte	0x8
	.byte	0
	.short	0
	.short	0
	.quad	Ltext0
	.set L$set$29,Letext0-Ltext0
	.quad L$set$29
	.quad	0
	.quad	0
	.section __DWARF,__debug_line,regular,debug
Lsection__debug_line:
Ldebug_line0:
	.section __DWARF,__debug_str,regular,debug
Lsection__debug_str:
LASF0:
	.ascii "data\0"
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
