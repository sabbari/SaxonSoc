	.global read_mtval
	.global read_mepc
	.global read_mcause
	.global read_stackpointer
	.global read_threadpointer
	.global read_framepointer
read_mtval:
	csrr a0, mtval
	jr ra
read_mepc:
	csrr a0, mepc
	jr ra

read_mcause:
	csrr a0, mcause
	jr ra
read_stackpointer:
    mv a0, sp
	jr ra
read_threadpointer:
	mv a0, tp
	jr ra
read_framepointer:
	mv a0, fp
	jr ra
