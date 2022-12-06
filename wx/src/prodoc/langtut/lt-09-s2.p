/* Solution to Language Tutorial Problem 9-2 */

/**********  DEFINE QUERIES  **********/
DEFINE QUERY New-Query FOR Customer, Salesrep.
        
/**********  DEFINE WIDGETS  **********/
DEFINE BROWSE New-Browse QUERY New-Query
    DISPLAY Name SPACE(3) Rep-Name WITH 10 DOWN.
DEFINE BUTTON btn-Exit LABEL "Exit".

/**********  DEFINE FRAMES  **********/
DEFINE FRAME Frame1 
    New-Browse SKIP(1)
    btn-Exit
        WITH NO-BOX CENTERED THREE-D.

/**********  MAIN LOGIC  **********/
OPEN QUERY New-Query FOR EACH Customer, FIRST Salesrep OF Customer.
DISPLAY New-Browse WITH FRAME Frame1.
ENABLE ALL WITH FRAME Frame1.
WAIT-FOR CHOOSE OF btn-Exit.



