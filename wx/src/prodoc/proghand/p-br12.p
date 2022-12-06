/* p-br12.p */

DEFINE QUERY q1 FOR customer SCROLLING.
DEFINE BROWSE b1 QUERY q1 DISPLAY cust-num name address address2 city 
    state postal-code country contact phone sales-rep credit-limit balance
    ENABLE ALL WITH 10 DOWN width 60 SEPARATORS
    TITLE "Update Customer".

DEFINE FRAME f1
    b1
        WITH SIDE-LABELS AT ROW 2 COLUMN 10 NO-BOX. 
        
ASSIGN b1:NUM-LOCKED-COLUMNS = 2.  
 
OPEN QUERY q1 FOR EACH customer NO-LOCK.  
ENABLE ALL WITH FRAME f1.
WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.


