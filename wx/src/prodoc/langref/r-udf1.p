


/* udf1.p */
/* Demonstrates defining and referencing a user-defined function */

/* Define doubler() */
FUNCTION doubler RETURNS INTEGER (INPUT parm1 AS INTEGER).
    RETURN (2 * parm1). 
END FUNCTION.   
    
/* Reference doubler() */
DISPLAY	"doubler(0)=" doubler(0) skip
	"doubler(1)=" doubler(1) skip
	"doubler(2)=" doubler(2) skip.





