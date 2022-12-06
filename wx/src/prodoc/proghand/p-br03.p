/* p-br03.p */

DEFINE QUERY q1 FOR customer SCROLLING.
DEFINE BROWSE b1 QUERY q1 DISPLAY cust-num name credit-limit balance
    ENABLE credit-limit WITH 10 DOWN SEPARATORS MULTIPLE
    TITLE "Update Credit Limits".
    
DEFINE FRAME f1
    b1
        WITH SIDE-LABELS ROW 2 CENTERED NO-BOX. 

OPEN QUERY q1 FOR EACH customer NO-LOCK.
ENABLE ALL WITH FRAME f1.
WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.

