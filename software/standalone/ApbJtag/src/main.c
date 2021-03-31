#include <stdint.h>
//#include "saxon.h"
#include "gpio.h"
#include "bsp.h"


//#include "print.h"

#include "csr_function.h"

// extern volatile uint32_t start_of_ramtest[];
// extern  volatile uint32_t length_of_ramtest;
//
// #define MAX_WORDS ((int)&length_of_ramtest)// >> 2)
// #define mem start_of_ramtest



int write_read_test(uint32_t* addr)
{
	uint32_t temp=0;
	uint32_t test_values[]={0xffffffff,0x12345678,0x1};
	for(int i=0;i<3;i++){
		*addr=test_values[i];
		temp=*addr;
		if(temp!=test_values[i]){
			print("Error** Read : 0x");
			print_uint_as_hex(temp,8);
			print(" Write : 0x");
			print_uint_as_hex(test_values[i],8);
			print("\n");
			return 0;
		}
	}
	print("Test success for address : 0x");
	print_uint_as_hex(addr,8);
	print("\n");
}

int write_read_test_2adress(uint32_t* read_addr,uint32_t* write_addr)
{
	uint32_t temp=0;
	uint32_t test_values[]={0xffffffff,0x12345678,0x1};
	for(int i=0;i<3;i++){
		*write_addr=test_values[i];
		temp=*read_addr;
		if(temp!=test_values[i]){
			print("Error** Read : 0x");
			print_uint_as_hex(temp,8);
			print(" at read adress  : 0x");
			print_uint_as_hex(read_addr,8);
			print(" Write : 0x");
			print_uint_as_hex(test_values[i],8);
			print(" at write adress  : 0x");
			print_uint_as_hex(write_addr,8);
			print("\n");
			return 0;
		}
	}
	print("Test success for read address : 0x");
	print_uint_as_hex(read_addr ,8);
	print(" and write address : 0x");
	print_uint_as_hex(write_addr ,8);
	print("\n");
}

void read_address(uint32_t* read_addr){
	print("Read : 0x");
	print_uint_as_hex(*read_addr,8);
	print("  at  : 0x");
	print_uint_as_hex(read_addr,8);
	print(" \n");

}
void write_address(uint32_t* write_addr, uint32_t value){
	*write_addr=value;
	print("write  : 0x");
	print_uint_as_hex(value,8);
	print("  at  : 0x");
	print_uint_as_hex(write_addr,8);
	print(" \n");

}

void main() {

	// bsp_init();
  //   gpio_setOutputEnable(BSP_LED_GPIO, BSP_LED_MASK);
  //   gpio_setOutput(BSP_LED_GPIO, 0x00000000);

volatile uint32_t* volatile  apbreg=JTAGSS_TAP_TAP_APB_BRIDGE_INPUT;
  //  println("Apb test\n");
		//setting led to red
		volatile uint32_t* volatile read_addr=apbreg+(0x18>>2);
		volatile uint32_t* volatile write_addr=apbreg +(0x1c>>2);
		volatile uint32_t* volatile status =apbreg +(0x10>>2);
    //gpio_setOutput(BSP_LED_GPIO,1<<23);
		uint16_t t2h_occup, h2t_occup ,status_temp=0;
		uint32_t temp_read=0;
	//	write_read_test_2adress(read_addr,write_addr);
	println("\n TEST ");
		while(1){
		status_temp=(uint16_t)*status;
		t2h_occup=(status_temp & 0xF000)>>12;
		h2t_occup=(status_temp & 0x0F00)>>8;
		//println("\n status 0x");
		//print_uint_as_hex(status_temp,8);
		gpio_setOutput(BSP_LED_GPIO, status_temp);
		if(h2t_occup){
			// println("found something h2t_occup");
			// print_uint_as_hex(h2t_occup,8);
			//temp_read=*read_addr;
			*write_addr=*read_addr;
			// println("\n found and write 0x");
			// print_uint_as_hex(temp_read,8);
			//write_address(write_addr,temp_read);
		}
		//read_address(read_addr);
	//	write_read_test(apbreg);

}
println("something happpened ");
}
