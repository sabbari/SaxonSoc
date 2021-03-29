#include <stdint.h>
//#include "saxon.h"
#include "gpio.h"
#include "bsp.h"


#include "print.h"

#include "csr_function.h"

extern volatile uint32_t start_of_ramtest[];
extern  volatile uint32_t length_of_ramtest;

#define MAX_WORDS ((int)&length_of_ramtest)>> 2
#define mem start_of_ramtest









void main() {

	bsp_init();
    gpio_setOutputEnable(BSP_LED_GPIO, BSP_LED_MASK);
    gpio_setOutput(BSP_LED_GPIO, 0x00000000);


    println("Memory test\n");
//setting led to red
    //gpio_setOutput(BSP_LED_GPIO,1<<23);
    print_uint_as_hex(&mem,8);


   for(int i=0;i<MAX_WORDS;i++) {

	     mem[i] = i;
   }
  println("write successful \n");
    for(int i=0;i<MAX_WORDS;i++) {
    //print("testing : ");
    //print_hex(i+mem,8);
    //print("\n");
     if (mem[i] != i) {
			/*print("Failed at address 0x");
			print_hex(i, 8);
			print(" with value 0x");
			print_hex(mem[i], 8);
			print("\n");*/

			while(1){}
      }

    }

    // Set the Green led for success
   // gpio_setOutput(BSP_LED_GPIO,1<<22);;
    println("Success\n");

    while(1){}
}

