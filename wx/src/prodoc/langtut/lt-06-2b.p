/* DO Block Solution to Language Tutorial Problem 6-2 */

DEFINE VARIABLE not-done AS LOGICAL INITIAL TRUE.
DO PRESELECT EACH Customer WHERE Credit-Limit > 15000 
  WHILE not-done WITH DOWN:    
    FIND NEXT Customer NO-ERROR.
    IF AVAILABLE Customer
    THEN DISPLAY Cust-Num Name Credit-Limit.
    ELSE not-done = FALSE.
END.
            