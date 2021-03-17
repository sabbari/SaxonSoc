PROJ_NAME=meomryTest
BSP_PATH=../../../bsp/digilent/ArtyA7SmpLinux
STANDALONE = ..


SRCS = 	$(wildcard src/*.c) \
		$(wildcard src/*.cpp) \
		$(wildcard src/*.S) \
        start_withmtvec.S
        
LDSCRIPT ?= memloader.ld


include ${STANDALONE}/common/bsp.mk
include ${STANDALONE}/common/riscv64-unknown-elf.mk
include ${STANDALONE}/common/standalone.mk

