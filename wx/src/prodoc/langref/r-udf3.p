


/* r-udf3.p 
 * references an externally-defined user-defined function 
 */

/* define items */
DEFINE VARIABLE myhand AS HANDLE.
DEFINE VARIABLE mystr  AS CHARACTER FORMAT "x(20)".

/* forward declare doubler() */
FUNCTION doubler RETURNS INTEGER (INPUT parm1 AS INTEGER) IN myhand. 

/* run the procedure that doubler() */
RUN src\prodoc\langref\r-udfdef.p PERSISTENT SET myhand. 
    
/* reference doubler() */
DISPLAY "doubler(0)=" doubler(0) skip
        "doubler(1)=" doubler(1) skip
        "doubler(17)=" doubler(17) skip.

/* delete the procedure that defines doubler */
DELETE PROCEDURE(myhand).   





