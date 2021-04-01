#include <stdint.h>
#include "gpio.h"
#include "bsp.h"
#include "csr_function.h"


//gpio_setOutput(BSP_LED_GPIO, status_temp);
#define JTAG_APB_ADDRESS JTAGSS_TAP_TAP_APB_BRIDGE_INPUT
volatile uint32_t * volatile read_addr 	= (uint32_t * volatile) JTAG_APB_ADDRESS + (0x18 >> 2);
volatile uint32_t * volatile write_addr = (uint32_t * volatile) JTAG_APB_ADDRESS + (0x1c >> 2);
volatile uint32_t * volatile status 		= (uint32_t * volatile) JTAG_APB_ADDRESS + (0x10 >> 2);



void main() {

  uint16_t t2h_occupancy, h2t_occupancy, status_temp = 0;
  println("\n TEST \n ");
  while (1) {
    status_temp = (uint16_t) * status;
    t2h_occupancy = (status_temp & 0xF000) >> 12;
    h2t_occupancy = (status_temp & 0x0F00) >> 8;

    if (h2t_occupancy) {

      * write_addr = * read_addr;

    }

  }
  println("something happpened ");
}
