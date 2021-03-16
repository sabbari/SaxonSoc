PROJ_NAME=blinkAndEcho

STANDALONE = ..


SRCS = 	$(wildcard src/*.c) \
		$(wildcard src/*.cpp) \
		$(wildcard src/*.S) \
		${STANDALONE}/common/start.S

BSP_PATH=../../../bsp/digilent/ArtyA7SmpLinux
LDSCRIPT ?= ${BSP_PATH}/linker/bootloader.ld

include ${STANDALONE}/common/bsp.mk
include ${STANDALONE}/common/riscv64-unknown-elf.mk
include ${STANDALONE}/common/standalone.mk
