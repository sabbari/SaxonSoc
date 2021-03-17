
#include "type.h"

extern uint32_t read_mtval();
extern uint32_t read_mepc();
extern uint32_t read_mcause();
extern uint32_t read_stackpointer();
extern uint32_t read_threadpointer();
extern uint32_t read_framepointer();



void print_mcause(uint32_t mcause) {
  print(" CSR : mcause value : 0x") ;
  print_hex(mcause,8);
  print(" Exception Description: ");
  if  (mcause == 0) {
    print("Instruction address misaligned \n");
  }
  else if  (mcause == 1) {
    print("Instruction access fault \n");
  }
  else if  (mcause == 2) {
    print("Illegal instruction  \n");
  }
  else if  (mcause == 3) {
    print("Breakpoint   \n");
  }
  else if  (mcause == 4) {
    print("Load address misaligned \n");
  }
  else if  (mcause == 5) {
    print("Load access fault \n");
  }
  else if  (mcause == 6) {
    print("Store/AMO address misaligned \n");
  }
  else if  (mcause == 7) {
    print("Store/AMO access fault \n");
  }
  else if  (mcause == 8) {
    print("Environment call from U-mode\n");
  }
  else if  (mcause == 9) {
    print("Environment call from S-mode\n");
  }
  else if  (mcause == 10) {
    print("Reserved   \n");
  }
  else if  (mcause == 11) {
    print("Environment call from M-mode\n");
  }
  else if  (mcause == 12) {
    print("Instruction page fault \n");
  }
  else if  (mcause == 13) {
    print("Load page fault \n");
  }
  else if  (mcause == 14) {
    print("Reserved   \n");
  }
  else if  (mcause == 15) {
    print("Store/AMO page fault \n");
  }
  else if  (mcause >= 16 || mcause <= 23) {
    print("Reserved   \n");
  }
  else if  (mcause >= 24 || mcause <= 31) {
    print("Designated for custom use\n");
  }
  else if  (mcause >= 32 || mcause <= 47) {
    print("Reserved   \n");
  }
  else if  (mcause >= 48 || mcause <= 63) {
    print("Designated for custom use\n");
  }
  else if  (mcause >= 64) {
    print("Reserved   \n");
  }
}



void  my_function_handler(){
	print("\n *************Exception handler *************\n");
	uint32_t mcause			=read_mcause();
	uint32_t mtval 			=read_mtval();
	uint32_t mepc  			=read_mepc();
	uint32_t stackpointer   =read_stackpointer();
	uint32_t threadpointer  =read_threadpointer();
	uint32_t framepointer	=read_framepointer();
	print_mcause(mcause);
	print(" CSR: mpec: Virtual address of the instruction that was interrupted : 0x ");
	print_hex(mepc,8);
	print(" \n CSR: mtval: Virtual address of instruction or memory depending on the error : 0x ");
	print_hex(mtval,8);
	print("\n Stack pointer : 0x ");
	print_hex(stackpointer,8);
	print("\n Thread pointer : 0x ");
	print_hex(threadpointer,8);
	print("\n Frame pointer : 0x ");
	print_hex(framepointer,8);
	print("   \n");

	while(1);
}



