/* p-dybut3.p */

DEFINE VARIABLE report-month AS INTEGER LABEL "Month" FORMAT ">9".
DEFINE VARIABLE report-year  AS INTEGER LABEL "Year" FORMAT "9999".
DEFINE VARIABLE temp-hand    AS WIDGET-HANDLE.

FORM
  report-month report-year
  WITH FRAME prompt-frame SIDE-LABELS.

DEFINE FRAME butt-frame WITH WIDTH 80.

ON GO OF FRAME prompt-frame
   DO:
       IF report-month:MODIFIED or report-year:MODIFIED
       THEN DO:
          ASSIGN report-month report-year.
          DELETE WIDGET-POOL "order-pool".
          RUN make-buttons.
       END.
   END.

ASSIGN report-month = MONTH(TODAY)
       report-year = YEAR(TODAY).

DISPLAY report-month report-year WITH FRAME prompt-frame. 

RUN make-buttons.

ENABLE report-month report-year WITH FRAME prompt-frame.

DO ON ERROR UNDO, LEAVE ON ENDKEY UNDO, LEAVE ON STOP UNDO, LEAVE:
   WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
END.

DELETE WIDGET-POOL "order-pool".

PROCEDURE make-buttons: 
DEFINE VARIABLE num-buts  AS INTEGER.

   CREATE WIDGET-POOL "order-pool" PERSISTENT.
   num-buts = 0.
   FRAME butt-frame:HIDDEN = TRUE.
   FOR EACH order WHERE MONTH(order-date) = report-month AND
                        YEAR(order-date) = report-year:

      FRAME butt-frame:HEIGHT-CHARS = TRUNC(num-buts / 5, 0) + 3.

      CREATE BUTTON temp-hand IN WIDGET-POOL "order-pool"
              ASSIGN LABEL = STRING(order-num) + "/" + STRING(cust-num)
                     FRAME = FRAME butt-frame:HANDLE
                     ROW = TRUNC(num-buts / 5, 0) + 1
                     COLUMN = ((num-buts MOD 5) * 15) + 1
                     SENSITIVE = TRUE
              TRIGGERS:
                 ON CHOOSE
                    PERSISTENT RUN disp-order.
              END TRIGGERS.
      num-buts = num-buts + 1.
   END.

   FRAME butt-frame:HIDDEN = FALSE.
   IF num-buts = 0
   THEN MESSAGE "No orders found for" STRING(report-month) + "/" +
            STRING(report-year) VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
END PROCEDURE.

PROCEDURE disp-order:

FORM
   order
   WITH FRAME order-frame SIDE-LABELS.


   FIND order WHERE order-num =
          INTEGER(SUBSTRING(SELF:LABEL, 1, INDEX(SELF:LABEL, "/") - 1)).

   CREATE WINDOW temp-hand IN WIDGET-POOL "order-pool"
         ASSIGN TITLE = "Order Detail"
                VIRTUAL-HEIGHT-CHARS = FRAME order-frame:HEIGHT-CHARS.
   CURRENT-WINDOW = temp-hand.
   DISPLAY order WITH FRAME order-frame.
   CURRENT-WINDOW = DEFAULT-WINDOW.
END PROCEDURE.
