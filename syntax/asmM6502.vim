" Vim syntax file
" Language:	MOS 6502 assembler
" By: Oyd E. Leven <oyd11@softhome.net>
" Creation date: 29-Jan-2005
" Version 0.01

" Remove any old syntax stuff hanging about
syn clear
syn case ignore
"
" registers
syn keyword asm65Reg X Y A S VSYNC VBLANK WSYNC RSYNC NUSIZ0 NUSIZ1 COLUP0 COLUP1 COLUPF COLUBK CTRLPF REFP0 REFP1 PF0 PF1 PF2 RESP0 RESP1 RESM0 RESM1 RESBL AUDC0 AUDC1 AUDF0 AUDF1 AUDV0 AUDV1 GRP0 GRP1 ENAM0 ENAM1 ENABL HMP0 HMP1 HMM0 HMM1 HMBL VDELP0 VDELP1 VDELBL RESMP0 RESMP1 HMOVE HMCLR CXCLR CXM0P CXM1P CXP0FB CXP1FB CXM0FB CXM1FB CXBLPF CXPPMM INPT0 INPT1 INPT2 INPT3 INPT4 INPT5 SWCHA SWACNT SWCHB SWBCNT INTIM TIMINT TIM1T TIM8T TIM64T T1024T DC0 PC0 PC1 DC PC
syn keyword asmPreproc PROCESSOR INCLUDE INCDIR INCBIN IF IFCONST SESTR ELSE ENDIF MAC MACRO ENDM ECHO ERR MEXIT SET ALIGN SEG REND RORG REPEND REPEAT ECHO SUBROUTINE HEX RES EQM EQU NAME
syn match asmPreproc ".word"
syn match asmPreproc ".byte"
syn match asmPreproc ".name"
syn match asmPreproc "nnn$"

" opcodes:

syn keyword asm65Op  adc and asl bit brk clc cld cli clv cmp cpx cpy dec dex dey eor inc inx iny  lda ldx ldy lsr nop ora pha php pla plp rol ror rti rts sbc sec sed sei sta stx sty tax tay tsx txa txs tya 

syn keyword asm65Branch bcc bcs beq bmi bne bpl bvc bvs jmp jsr

" Atari 800XL 'Sally' undocumented opcodes
" mnemonics taken from Trevin Beattie's 'Atari Technical Information' page
" at "http://www.xmission.com/~trevin/atari/atari.shtml"
syn keyword asmSallyUndoc anc arr asr asx ax7 axe brk dcp jam las lax rla rra sax slo sre sx7 sy7 xea xs7

syn match asmLabel		"^[a-z_][a-z0-9_]*:"
syn match asmLabel		"^\.[a-z_][a-z0-9_]*:"
syn match asmComment		";.*"hs=s contains=asmTodo
syn keyword asmTodo	contained todo fixme xxx warning danger notice bug
syn region asmString		start=+"+ skip=+\\"+ end=+"+
syn keyword asmSettings		opt org

syn match decNumber	"\<\d\+\>"
syn match hexNumber	"\$\x\+\>" " 'bug', but adding \< doesn't behave!
syn match binNumber	"%[01]\+\>" 
syn match asmImmediate	"#%[01]\+\>" 
syn match asmImmediate	"#\$\x\+\>"
syn match asmImmediate	"#\d\+\>"
syn match asmImmediate	"<\$\x\+\>"
syn match asmImmediate	"<\d\+\>"
syn match asmImmediate	">\$\x\+\>"
syn match asmImmediate	">\d\+\>"
syn match asmImmediate	"#<\$\x\+\>"
syn match asmImmediate	"#>\$\x\+\>"
syn match asmImmediate	"#<\d\+\>"
syn match asmImmediate	"#>\d\+\>"

"
"syn case match
if !exists("did_asm65_syntax_inits")
	let did_rgb_asm_syntax_inits = 1

	" The default methods for highlighting.  Can be overridden later
	hi link asmLabel		Type
  	hi link asmString		String
  	hi link asmComment		Comment
  	hi link asmSettings		PreProc
  	hi link asm65Op			Statement
  	hi link asmSallyUndoc	Special
  	hi link asm65Reg		Identifier
  	hi link asm65Branch		Conditional
  	hi link asmTodo			Debug

  	hi link asmImmediate	Special
	hi link asmPreproc		PreProc

  	hi link hexNumber		Number
  	hi link binNumber		Number
  	hi link decNumber		Number

	" My default color overrides:
	"  hi asmSpecialComment ctermfg=red
	"  hi asmIdentifier ctermfg=lightcyan
	"  hi asmType ctermbg=black ctermfg=brown

endif

let b:current_syntax = "rgbasm"

