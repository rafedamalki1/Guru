/* p-dybuts.p */

DEFINE VARIABLE num-buts  AS INTEGER.
DEFINE VARIABLE temp-hand AS WIDGET-HANDLE.

DEFINE FRAME butt-frame
   WITH WIDTH 60 CENTERED TITLE "Sales Representatives".

FORM
   salesrep
   WITH FRAME rep-frame.

num-buts = 0.
FOR EACH salesrep:
   CREATE BUTTON temp-hand
         ASSIGN LABEL = salesrep.sales-rep
                FRAME = FRAME butt-frame:HANDLE
                ROW = TRUNC(num-buts / 3, 0) + 1
                COLUMN = ((num-buts MOD 3) * 20) + 1
                SENSITIVE = TRUE
         TRIGGERS:
            ON CHOOSE
               DO:
                 FIND salesrep WHERE salesrep.sales-rep = SELF:LABEL.
                 DISPLAY salesrep WITH FRAME rep-frame.
               END.
         END TRIGGERS.
   num-buts = num-buts + 1.
END.

FRAME butt-frame:HEIGHT-CHARS = (num-buts / 3) + 2.

VIEW FRAME butt-frame.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
