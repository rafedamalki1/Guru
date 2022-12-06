/* r-ostop2.p */

DEFINE BUTTON buta LABEL "Find Next".
DEFINE BUTTON butb LABEL "Assign".
DEFINE BUTTON butc LABEL "Done".

DEFINE VARIABLE methRtn AS LOGICAL NO-UNDO.
DEFINE FRAME a
	Salesrep.Sales-rep SKIP Salesrep.Rep-Name SKIP Salesrep.Region SKIP 
	Month-Quota[1] Month-Quota[7] SKIP 
	Month-Quota[2] Month-Quota[8] SKIP
	Month-Quota[3] Month-Quota[9] SKIP 
	Month-Quota[4] Month-Quota[10] SKIP 
	Month-Quota[5] Month-Quota[11] SKIP 
	Month-Quota[6] Month-Quota[12] SKIP(1)     
	buta     butb    Butc
	WITH 1 DOWN NO-BOX SIDE-LABELS.

/*******TRIGGERS*******/
ON CHOOSE OF buta DO:
	FIND NEXT Salesrep SHARE-LOCK.
	IF NOT AVAILABLE(Salesrep) THEN MESSAGE "No Next Salesrep".
	DISPLAY Salesrep WITH FRAME a.
END.

ON CHOOSE OF butb DO:
	DO TRANSACTION ON STOP UNDO, LEAVE:
		ASSIGN Salesrep.Sales-rep Salesrep.Rep-Name Salesrep.Region.
	END.
END.

ON CHOOSE OF butc DO:
	APPLY "ENDKEY" TO FRAME a.
END.

/*******MAIN BLOCK*******/
FIND FIRST Salesrep SHARE-LOCK.
DISPLAY Salesrep WITH FRAME a.
ENABLE ALL WITH FRAME a.
WAIT-FOR ENDKEY OF FRAME a FOCUS buta.
