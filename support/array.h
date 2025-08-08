#pragma once 
#include <stdlib.h>

void* getAtIndex(void** arr, int index);
void setAtIndex(void** arr, int index, void* value);
void* allocWithLength(int length);
int isNull(void* ptr);
void* nullPtr(void* ptr);
char* getString(void* ptr);
