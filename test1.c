#include "types.h"
#include "user.h"
#include "stat.h"

#define NULL ((void *)0)

int main(int argc, char *argv[]) {
	int *a = NULL;
	int b = *a;
	b = b + 1;
	printf(1,"null ptr dereference %d\n", b);
	exit();
}