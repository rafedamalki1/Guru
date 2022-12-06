/* p-tbox2.p */

DEFINE VARIABLE cnt AS INTEGER FORMAT ">>9".
DEFINE VARIABLE must-have-balance AS LOGICAL INITIAL no
        VIEW-AS TOGGLE-BOX LABEL "Only if Balance Due".
DEFINE VARIABLE must-be-local AS LOGICAL INITIAL no
        VIEW-AS TOGGLE-BOX LABEL "Only if in Local Country".
DEFINE VARIABLE must-get-discount AS LOGICAL INITIAL no
        VIEW-AS TOGGLE-BOX LABEL "Only if Discount".
DEFINE VARIABLE local-country AS CHARACTER INITIAL "USA".

FORM 
   must-have-balance SKIP
   must-be-local SKIP
   must-get-discount SKIP(1)
   "There are" cnt "customers that meet these criteria."
   WITH FRAME count-frame TITLE "Counting Customers" NO-LABELS.

ON VALUE-CHANGED OF must-have-balance, must-be-local, must-get-discount
   DO:
     DEFINE VARIABLE meets-criteria AS LOGICAL.
     cnt = 0.
     FOR EACH customer:
         meets-criteria = yes.
         IF INPUT must-have-balance AND customer.balance <= 0 
         THEN meets-criteria = no.
         ELSE IF INPUT must-be-local AND customer.country <> local-country
         THEN meets-criteria = no.
         ELSE IF INPUT must-get-discount AND customer.discount = 0 
         THEN meets-criteria = no.

         IF meets-criteria 
         THEN cnt = cnt + 1.
     END.
     DISPLAY cnt WITH FRAME count-frame.
   END.

cnt = 0.
FOR EACH customer:
    cnt = cnt + 1.
END.

DISPLAY must-have-balance must-be-local must-get-discount cnt 
        WITH FRAME count-frame.
ENABLE must-have-balance must-be-local must-get-discount  
       WITH FRAME count-frame.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
