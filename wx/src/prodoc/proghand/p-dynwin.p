/* p-dynwin.p */

DEFINE VARIABLE curr-rec    AS RECID.
DEFINE VARIABLE first-win   AS WIDGET-HANDLE.
DEFINE VARIABLE ok-status   AS LOGICAL. 

DEFINE BUTTON show-detail   LABEL "Detail".

DEFINE QUERY    custq       FOR customer.
DEFINE BROWSE   custb       QUERY custq DISPLAY cust-num name
                            WITH 15 DOWN.

FORM 
    custb show-detail
    WITH FRAME names-frame.


ON CHOOSE OF show-detail
   DO:
       RUN show-det. 
   END.

first-win = CURRENT-WINDOW.    
SESSION:APPL-ALERT-BOXES = FALSE.
STATUS INPUT "Choose a customer or press " + KBLABEL("END-ERROR") +
             " to end.". 
 
OPEN QUERY custq FOR EACH customer.
ENABLE ALL WITH FRAME names-frame.

CREATE WIDGET-POOL.

DO ON ERROR UNDO, LEAVE ON ENDKEY UNDO, LEAVE ON STOP UNDO, LEAVE:
   WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW. 
END.

DELETE WIDGET-POOL.

PROCEDURE show-det:
  DEFINE VARIABLE new-win     AS WIDGET-HANDLE.

  FORM
    customer
    WITH FRAME full-frame.

  CREATE WINDOW new-win
     ASSIGN TITLE = "Customer Detail"
            MESSAGE-AREA = FALSE
            STATUS-AREA = FALSE 
            VIRTUAL-HEIGHT-CHARS = FRAME full-frame:HEIGHT-CHARS
            VIRTUAL-WIDTH-CHARS = FRAME full-frame:WIDTH-CHARS
     TRIGGERS:
         ON WINDOW-CLOSE
             PERSISTENT RUN del-win. 
     END TRIGGERS.

  CURRENT-WINDOW = new-win.
  DISPLAY customer WITH FRAME full-frame SIDE-LABELS.
  CURRENT-WINDOW = first-win. 
END PROCEDURE.

PROCEDURE del-win:
   DELETE WIDGET SELF.
END PROCEDURE.
