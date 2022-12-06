/* Solution to Language Tutorial Problem 6-4 */

/**********  DEFINE WIDGETS  **********/
DEFINE VARIABLE Ctr-BBB AS INTEGER LABEL "Total for BBB".
DEFINE VARIABLE Ctr-SLS AS INTEGER LABEL "Total for SLS".
DEFINE VARIABLE Ctr-Others AS INTEGER LABEL "Total Others".

/**********  MAIN LOGIC  **********/
FOR EACH Customer:
    IF Sales-Rep = "BBB" THEN
        Ctr-BBB = Ctr-BBB + 1.
    ELSE IF Sales-Rep = "SLS" THEN
             Ctr-SLS = Ctr-SLS + 1.        
         ELSE
             Ctr-Others = Ctr-Others + 1.
END.

DISPLAY Ctr-BBB Ctr-SLS Ctr-Others WITH USE-TEXT. 
