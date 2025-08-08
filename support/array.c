#include <stdlib.h>

void* getAtIndex(void** arr, int index) { arr[index]; }
void setAtIndex(void** arr, int index, void* value) { arr[index] = value; }
void* allocWithLength(int length) {
    void* arr = malloc(length * sizeof(void*));
    return arr;
}

int isNull(void* ptr) {
  if (ptr == NULL) {
    return 1;
      } else {
    return 0;
      }
  }
      
void* nullPtr() { 
  return NULL; 
}
char* getString(void* ptr) {
  return (char*)ptr;
}
