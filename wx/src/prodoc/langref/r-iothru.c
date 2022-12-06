#include <stdio.h>

#define  MAX(a,b)       ( (a < b) ? b : a )

main()

{
	float   cost;

	/* This is important so that buffering does not defeat */
	/* our attempts to actually write on the pipe          */
	setbuf(stdout, (char *) NULL);


	while (scanf("%f", &cost) == 1) {
	    /* Here the item cost is manipulated.*/
	    /* We are simply increasing the cost */
	    /* by a fixed percentage (with a     */
	    /* minimum increase), to provide an  */
	    /* an example.                       */
	    cost = cost + MAX( 0.03 * cost, .50);
	    printf("%10.2f\n", cost);
	}
}
