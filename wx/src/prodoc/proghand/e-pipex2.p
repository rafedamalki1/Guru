/* e-pipex2.p */

DEF VAR sql-stmt AS CHAR FORMAT "x(220)". /* Target variable for the request*/
DEF VAR out-pipe AS CHAR FORMAT "x(32)".  /* Holds the output file or FIFO */

REPEAT:                                 /* Do forever: */
  INPUT FROM inpipe NO-ECHO.            /* Set up to read from in-FIFO
                                           named "inpipe". */
  REPEAT:                               /* For each request received: */
    IMPORT out-pipe sql-stmt.           /* Get the output name
                                           and the request. */
    OUTPUT TO VALUE(out-pipe) APPEND.   /* Set up to write results. */
    RUN e-do-sql.p sql-stmt.            /* Pass SQL request to sub-proc. */
    OUTPUT CLOSE.
  END.                                  /* This loop ends when the in-FIFO
                                           is empty. Just reopen it and */
END.                                    /*  wait for the next request. */
