/* Solution to Language Tutorial Problem 6-5 */

/**********  DEFINE WIDGETS  **********/
DEFINE VARIABLE Ctr-BBB AS INTEGER LABEL "Total for BBB".
DEFINE VARIABLE Ctr-SLS AS INTEGER LABEL "Total for SLS".
DEFINE VARIABLE Ctr-Others AS INTEGER LABEL "Total Others".

/**********  MAIN LOGIC  **********/
FOR EACH Customer:
    CASE Sales-Rep:
    WHEN "BBB" THEN
        Ctr-BBB = Ctr-BBB + 1.
    WHEN "SLS" THEN
        Ctr-SLS = Ctr-SLS + 1.        
    OTHERWISE
        Ctr-Others = Ctr-Others + 1.
    END CASE.
END.

DISPLAY Ctr-BBB Ctr-SLS Ctr-Others WITH USE-TEXT. 
