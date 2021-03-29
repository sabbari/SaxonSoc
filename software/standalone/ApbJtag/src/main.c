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
		if(*addr!=test_values[i]){
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





void main() {

	// bsp_init();
  //   gpio_setOutputEnable(BSP_LED_GPIO, BSP_LED_MASK);
  //   gpio_setOutput(BSP_LED_GPIO, 0x00000000);

uint32_t* apbreg=SYSTEM_MY_APB_BMB_INPUT;
    println("Apb test\n");
		//setting led to red
    //gpio_setOutput(BSP_LED_GPIO,1<<23);

		write_read_test(apbreg);
		write_read_test(apbreg+1);


}
