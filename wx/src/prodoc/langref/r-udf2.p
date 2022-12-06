


/* r-udf2.p */
/* Demonstrates forward-declaring, referencing, and defining 
 * a user-defined function
 */

/* Forward declare doubler() */
FUNCTION doubler RETURNS INTEGER (INPUT parm1 AS INTEGER) FORWARD.    
    
/* Reference doubler() */
DISPLAY "doubler(0)=" doubler(0).
DISPLAY "doubler(1)=" doubler(1).
DISPLAY "doubler(2)=" doubler(2).

/* Define doubler() */
FUNCTION doubler RETURNS INTEGER. 
    RETURN (2 * parm1). 
END FUNCTION.    





