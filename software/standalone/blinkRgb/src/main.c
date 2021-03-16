#include <stdint.h>

#include "saxon.h"
#include "io.h"
//#define SYSTEM_RGB_A_APB 0x10010000
#define SYSTEM_RGB_A_APB 0x10000000
void main() {

    uint32_t counter = 0;
    uint8_t ledValue = 0;

    while(1){
        if(counter++ == 100000){
            write_u32(ledValue++, SYSTEM_RGB_A_APB);
            write_u32(0xfffff, SYSTEM_RGB_A_APB);
            counter = 0;
        }
    }
}

