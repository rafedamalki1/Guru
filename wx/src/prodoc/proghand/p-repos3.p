/* p-repos3.p */

DEFINE BUTTON back-one  LABEL "<".
DEFINE BUTTON back-fast LABEL "<<".
DEFINE BUTTON fore-one  LABEL ">".
DEFINE BUTTON fore-fast LABEL ">>".

DEFINE QUERY scroll-cust FOR Customer SCROLLING.

FORM
  Customer.Cust-num Customer.Name SKIP
  back-fast back-one fore-one fore-fast
  WITH FRAME cust-frame.
  
  
OPEN QUERY scroll-cust FOR EACH Customer NO-LOCK.

ENABLE back-fast back-one fore-one fore-fast WITH FRAME cust-frame.

ON CHOOSE OF back-fast
  DO:
    /* Position back 10 records (or to beginning) and
       fetch  the next record from there.            */
    REPOSITION scroll-cust BACKWARD 10.
    RUN next-one.
  END.
  
ON CHOOSE OF back-one
  DO:
     /* Position to previous record and fetch it. */
     REPOSITION scroll-cust BACKWARD 2.
     RUN next-one.
  END.

ON CHOOSE OF fore-one
   DO:
      RUN next-one.
   END.

ON CHOOSE OF fore-fast
   DO:
      /* Position forward 10 records (or to last record) and fetch. */
      REPOSITION scroll-cust FORWARD 9.
      RUN next-one.
     
   END.

/* Fetch the first record. */
RUN next-one.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.


PROCEDURE next-one:
      /* Fetch the next record. */
      GET NEXT scroll-cust.
      IF QUERY-OFF-END("scroll-cust") THEN
        GET PREV scroll-cust.
      DISPLAY Customer.Cust-num Customer.Name WITH FRAME cust-frame.
END PROCEDURE.
