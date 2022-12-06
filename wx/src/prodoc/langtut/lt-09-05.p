/**********  DEFINE QUERIES  **********/
DEFINE QUERY New-Query FOR Customer FIELDS (Name Cust-Num),
                           Order FIELDS (Order-Num Cust-Num).

/**********  DEFINE WIDGETS  **********/
DEFINE BROWSE New-Browse QUERY New-Query
    DISPLAY Name SPACE(3) Order-Num WITH 12 DOWN.
DEFINE BUTTON btn-Exit LABEL "Exit".

/**********  DEFINE FRAMES  **********/
DEFINE FRAME Frame1 
    New-Browse AT ROW 1 COLUMN 2
    btn-Exit   AT ROW 1 COLUMN 50
        WITH NO-BOX CENTERED THREE-D.

/**********  MAIN LOGIC  **********/
OPEN QUERY New-Query FOR EACH Customer, EACH Order 
            WHERE Order.Cust-Num = Customer.Cust-Num BY Name.
DISPLAY New-Browse WITH FRAME Frame1.
ENABLE ALL WITH FRAME Frame1.
WAIT-FOR CHOOSE OF btn-Exit.



