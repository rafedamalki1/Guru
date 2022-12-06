/* p-blocks.p */

/* BEGIN EXTERNAL PROCEDURE BLOCK */
DEFINE BUTTON bSum LABEL "Sum Customer Balances".
DEFINE VARIABLE balsum AS DECIMAL. 
DEFINE FRAME A bSum.              

ON CHOOSE OF bSum IN FRAME A DO:    /* BEGIN Trigger Block */ 
    RUN SumBalances(OUTPUT balsum).
    MESSAGE "Corporate Receivables Owed:" 
        STRING(balsum, "$>,>>>,>>>,>>9.99") VIEW-AS ALERT-BOX.
END.                                /* END Trigger Block */
  
ENABLE bSum WITH FRAME A. 
WAIT-FOR WINDOW-CLOSE OF FRAME A. 

PROCEDURE SumBalances:  /* BEGIN Internal Procedure Block */
DEFINE OUTPUT PARAMETER balance-sum AS DECIMAL INITIAL 0.

    FOR EACH customer FIELD (Balance):  /* BEGIN Iterative Block */
        balance-sum = balance-sum + Balance.
    END.                                /* END Iterative Block */
    
END.                    /* END Internal Procedure Block */
/* END EXTERNAL PROCEDURE BLOCK */