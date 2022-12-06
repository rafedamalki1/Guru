/* Solution to Language Tutorial Problem 9-3 */

/**********  DEFINE QUERIES  **********/
DEFINE QUERY New-Query FOR Customer, Order, Order-line, Item.
        
/**********  DEFINE WIDGETS  **********/
DEFINE BROWSE New-Browse QUERY New-Query
    DISPLAY Name SPACE(3) Item-Name WITH 10 DOWN.
DEFINE BUTTON btn-Exit LABEL "Exit".

/**********  DEFINE FRAMES  **********/
DEFINE FRAME Frame1 
    New-Browse SKIP(1)
    btn-Exit
        WITH NO-BOX CENTERED THREE-D.

/**********  MAIN LOGIC  **********/
OPEN QUERY New-Query FOR EACH Customer, EACH Order OF Customer,
                EACH Order-line OF Order, FIRST Item of Order-line.
DISPLAY New-Browse WITH FRAME Frame1.
ENABLE ALL WITH FRAME Frame1.
WAIT-FOR CHOOSE OF btn-Exit.



