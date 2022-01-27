#include <stdio.h>

int	main() {
	unsigned int num, resp_parcial;

	resp_parcial = 1;

	scanf("%u", &num);

	while (num > 0) {
		resp_parcial = resp_parcial * num;

		num = num - 1;
	}

	printf("%u\n", resp_parcial);

	return 0;
}
