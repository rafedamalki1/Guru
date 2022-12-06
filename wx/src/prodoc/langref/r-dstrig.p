/* r-dstrig.p */

DEFINE SUB-MENU file
  MENU-ITEM viewit LABEL "&View Data"
  MENU-ITEM dumpit LABEL "&Dump Data"
  MENU-ITEM loadit LABEL "&Load Data".
  MENU-ITEM exit   LABEL "E&xit".
  
DEFINE MENU mbar MENUBAR
  SUB-MENU file LABEL "&File".
  
DEFINE BUTTON b_more LABEL "Next".
DEFINE BUTTON b_exit LABEL "Cancel".

DEFINE FRAME cust-frame
  customer.cust-num SKIP
  customer.name  SKIP
  customer.phone SKIP
  b_more b_exit
  WITH CENTERED SIDE-LABELS ROW 3.
   
DEFINE STREAM cust.

DEFINE VARIABLE i AS INTEGER NO-UNDO.

PAUSE 0 BEFORE-HIDE.

ON CHOOSE OF b_exit IN FRAME cust-frame
DO:
  HIDE FRAME cust-frame NO-PAUSE.
  DISABLE ALL WITH FRAME cust-frame.
  LEAVE.
END.  

ON CHOOSE OF b_more IN FRAME cust-frame
DO:
  FIND NEXT customer NO-LOCK NO-ERROR.
  IF NOT AVAILABLE(customer) THEN
    RETURN.
  DISPLAY customer.cust-num customer.name customer.phone
    WITH FRAME cust-frame.
END.

ON CHOOSE OF MENU-ITEM viewit
DO:
  ENABLE ALL WITH FRAME cust-frame.
  FIND FIRST customer NO-LOCK NO-ERROR.
  DISP customer.cust-num customer.name customer.phone 
    WITH FRAME cust-frame.
  APPLY "ENTRY" TO b_more.
END.

ON CHOOSE OF MENU-ITEM dumpit
DO:
  DISABLE TRIGGERS FOR DUMP OF customer.
  i = 1.
  SESSION:IMMEDIATE-DISPLAY = TRUE.
  OUTPUT STREAM cust TO "customer.d".
  FOR EACH customer NO-LOCK:
    EXPORT STREAM cust customer.
    DISP i LABEL "Records Processed" 
      WITH FRAME rec-info SIDE-LABELS ROW SCREEN-LINES / 2 CENTERED.
    i = i + 1.
  END.
  SESSION:IMMEDIATE-DISPLAY = FALSE.
  OUTPUT STREAM cust CLOSE.
END.

ON CHOOSE OF MENU-ITEM loadit
DO:
  DISABLE TRIGGERS FOR LOAD OF customer.
  INPUT FROM "customer.d".
  SESSION:IMMEDIATE-DISPLAY = TRUE.
  REPEAT:
    CREATE customer.
    IMPORT customer.
    DISP i LABEL "Records Processed"
      WITH FRAME rec-info SIDE-LABELS ROW SCREEN-LINES / 2 CENTERED.
    i = i + 1.
  END.
  INPUT CLOSE.
  SESSION:IMMEDIATE-DISPLAY = FALSE.
END.

IF NOT RETRY THEN  
ASSIGN CURRENT-WINDOW:MENUBAR = MENU mbar:HANDLE
       CURRENT-WINDOW:VISIBLE = TRUE.
 
WAIT-FOR CHOOSE OF MENU-ITEM exit.
