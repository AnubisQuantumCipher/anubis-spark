	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
Ltext0:
	.file 1 "/Users/sicarii/Desktop/anubis-spark/src/test_simple_output.adb"
	.const
	.align	3
lC2:
	.ascii "Hello from Anubis-SPARK!"
	.align	3
lC3:
	.ascii "If you can see this, Ada.Text_IO is working."
	.text
	.align	2
	.globl __ada_test_simple_output
__ada_test_simple_output:
LFB1:
	.loc 1 3 1
	sub	x10, sp, #16384
	str	xzr, [x10, 4064]
	stp	x29, x30, [sp, -32]!
LCFI0:
	mov	x29, sp
LCFI1:
	stp	x20, x21, [sp, 16]
LCFI2:
LBB2:
	.loc 1 5 4
	adrp	x2, lC2@PAGE
	add	x0, x2, lC2@PAGEOFF;
	adrp	x2, lC0@PAGE
	add	x1, x2, lC0@PAGEOFF;
	bl	_ada__text_io__put_line__2
LBE2:
LBB3:
	.loc 1 6 4
	adrp	x0, lC3@PAGE
	add	x20, x0, lC3@PAGEOFF;
	adrp	x0, lC1@PAGE
	add	x21, x0, lC1@PAGEOFF;
	mov	x0, x20
	mov	x1, x21
	bl	_ada__text_io__put_line__2
LBE3:
	.loc 1 7 5
	nop
	ldp	x20, x21, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI3:
	ret
LFE1:
	.const
	.align	2
lC0:
	.word	1
	.word	24
	.align	2
lC1:
	.word	1
	.word	44
	.text
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
	.byte	0x94
	.uleb128 0x2
	.byte	0x95
	.uleb128 0x1
	.byte	0x4
	.set L$set$7,LCFI3-LCFI2
	.long L$set$7
	.byte	0xde
	.byte	0xdd
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
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$12,LCFI1-LCFI0
	.long L$set$12
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$13,LCFI2-LCFI1
	.long L$set$13
	.byte	0x94
	.uleb128 0x2
	.byte	0x95
	.uleb128 0x1
	.byte	0x4
	.set L$set$14,LCFI3-LCFI2
	.long L$set$14
	.byte	0xde
	.byte	0xdd
	.byte	0xd4
	.byte	0xd5
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE3:
	.text
Letext0:
	.section __DWARF,__debug_info,regular,debug
Lsection__debug_info:
Ldebug_info0:
	.long	0x269
	.short	0x4
	.set L$set$15,Ldebug_abbrev0-Lsection__debug_abbrev
	.long L$set$15
	.byte	0x8
	.uleb128 0x1
	.ascii "GNU Ada 14.1.0 -O0 -gnatA -g -gnat2012 -gnatwa -gnatwe -fstack-check=specific -fstack-protector-strong -gnatR2js -gnatws -gnatec=/private/var/folders/bg/pt9l6y1j47q642kp3z5blrmh0000gn/T/GPR.70489/GNAT-TEMP-000003.TMP -gnatem=/private/var/folders/bg/pt9l6y1j47q642kp3z5blrmh0000gn/T/GPR.70489/GNAT-TEMP-000004.TMP -mmacosx-version-min=16.0.0 -mcpu=apple-m1 -mlittle-endian -mabi=lp64 -fPIC\0"
	.byte	0xd
	.ascii "/Users/sicarii/Desktop/anubis-spark/src/test_simple_output.adb\0"
	.ascii "/Users/sicarii/Desktop/anubis-spark/obj/gnatprove/data_representation\0"
	.quad	Ltext0
	.set L$set$16,Letext0-Ltext0
	.quad L$set$16
	.set L$set$17,Ldebug_line0-Lsection__debug_line
	.long L$set$17
	.uleb128 0x2
	.ascii "test_simple_output\0"
	.byte	0x1
	.byte	0x3
	.byte	0x1
	.ascii "_ada_test_simple_output\0"
	.quad	LFB1
	.set L$set$18,LFE1-LFB1
	.quad L$set$18
	.uleb128 0x1
	.byte	0x9c
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
	.uleb128 0x2e
	.byte	0
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
	.byte	0
	.section __DWARF,__debug_pubnames,regular,debug
Lsection__debug_pubnames:
	.long	0x25
	.short	0x2
	.set L$set$19,Ldebug_info0-Lsection__debug_info
	.long L$set$19
	.long	0x26d
	.long	0x22b
	.ascii "test_simple_output\0"
	.long	0
	.section __DWARF,__debug_pubtypes,regular,debug
Lsection__debug_pubtypes:
	.long	0xe
	.short	0x2
	.set L$set$20,Ldebug_info0-Lsection__debug_info
	.long L$set$20
	.long	0x26d
	.long	0
	.section __DWARF,__debug_aranges,regular,debug
Lsection__debug_aranges:
	.long	0x2c
	.short	0x2
	.set L$set$21,Ldebug_info0-Lsection__debug_info
	.long L$set$21
	.byte	0x8
	.byte	0
	.short	0
	.short	0
	.quad	Ltext0
	.set L$set$22,Letext0-Ltext0
	.quad L$set$22
	.quad	0
	.quad	0
	.section __DWARF,__debug_line,regular,debug
Lsection__debug_line:
Ldebug_line0:
	.section __DWARF,__debug_str,regular,debug
Lsection__debug_str:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
