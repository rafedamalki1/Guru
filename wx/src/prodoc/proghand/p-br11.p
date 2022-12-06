/* p-br11.p */

DEFINE VARIABLE method-return AS LOGICAL.
DEFINE BUTTON b-delete LABEL "Delete All Selected Rows".

DEFINE QUERY q1 FOR customer SCROLLING.                                                 
DEFINE BROWSE b1 QUERY q1 DISPLAY cust-num name ENABLE name
    WITH 10 DOWN SEPARATORS MULTIPLE.

DEFINE FRAME f1
     b1 SKIP(.5)
     b-delete
        WITH NO-BOX SIDE-LABELS ROW 2 CENTERED.

ON CHOOSE OF b-delete IN FRAME f1
DO:                                             
    DEFINE VARIABLE i AS INTEGER.
    DO i = b1:NUM-SELECTED-ROWS TO 1 by -1:
        method-return = b1:FETCH-SELECTED-ROW(i).
        GET CURRENT q1 EXCLUSIVE-LOCK. 
        DELETE customer.
    END.   
    method-return = b1:DELETE-SELECTED-ROWS(). 
END.

OPEN QUERY q1 FOR EACH customer.
ENABLE b1 b-delete WITH FRAME f1.
WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW

