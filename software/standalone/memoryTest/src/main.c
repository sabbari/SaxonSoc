#include <stdint.h>
//#include "saxon.h"
#include "gpio.h"
#include "bsp.h"

#ifdef SPINAL_SIM
#define LOOP_UDELAY 100
#else
#define LOOP_UDELAY 100000
#endif
#define SYSTEM_UART_A_APB 0x10010000
#define SYSTEM_GPIO_A_APB 0x10000000
extern volatile uint32_t start_of_ramtest[];

#define MAX_WORDS (512)
#define mem start_of_ramtest
//#define mem ((volatile uint32_t*)0x10A00800)


void print_hex(uint32_t val, uint32_t digits)
{
    for (int i = (4*digits)-4; i >= 0; i -= 4)
        uart_write(BSP_UART_TERMINAL, "0123456789ABCDEF"[(val >> i) % 16]);
}




void print(uint8_t * data) {
  uart_writeStr(BSP_UART_TERMINAL, data);
}

void main() {

	bsp_init();
    gpio_setOutputEnable(BSP_LED_GPIO, BSP_LED_MASK);
    gpio_setOutput(BSP_LED_GPIO, 0x00000000);


    print("Memory test\n");
//setting led to red
    //gpio_setOutput(BSP_LED_GPIO,1<<23);



   for(int i=0;i<MAX_WORDS;i++) {

	     mem[i] = i;
   }
  print("write successful \n");
    for(int i=0;i<MAX_WORDS;i++) {

     if (mem[i] != i) {
			print("Failed at address 0x");
			print_hex(i, 8);
			print(" with value 0x");
			print_hex(mem[i], 8);
			print("\n");

			while(1){}
      }

    }

    // Set the Green led for success
    gpio_setOutput(BSP_LED_GPIO,1<<22);;
    print("Success\n");

    while(1){}
}

