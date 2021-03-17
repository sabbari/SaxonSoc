#include <stdint.h>
#include "bsp.h"

#ifndef print_h
#define print_h
void print_hex(uint32_t val, uint32_t digits)
{
    for (int i = (4*digits)-4; i >= 0; i -= 4)
        uart_write(BSP_UART_TERMINAL, "0123456789ABCDEF"[(val >> i) % 16]);
}
/*
uint32_t numPlaces (uint32_t n) {
    //if (n < 0) n = (n == INT_MIN) ? INT_MAX : -n;
    if (n < 10) return 1;
    if (n < 100) return 2;
    if (n < 1000) return 3;
    if (n < 10000) return 4;
    if (n < 100000) return 5;
    if (n < 1000000) return 6;
    if (n < 10000000) return 7;
    if (n < 100000000) return 8;
    if (n < 1000000000) return 9;
    //      2147483647 is 2^31-1 - add more ifs as needed
    //   and adjust this final return as well.
    return 10;
}
void print_int(uint32_t val) {
    for (int i =numPlaces(val); i > 0; i -= 1)
        uart_write(BSP_UART_TERMINAL, "0123456789"[val  % 10*i]);
}  */


void print(uint8_t * data) {
  uart_writeStr(BSP_UART_TERMINAL, data);
}

#endif
