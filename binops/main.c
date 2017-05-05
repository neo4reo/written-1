#include <stdio.h>

int GLOBAL asm("GLOBAL");

extern int our_code_starts_here() asm("our_code_starts_here");

int main(int argc, char** argv) {
  GLOBAL = 44;
  printf("%d\n", our_code_starts_here());
  return 0;
}
