/* p-mansrs.p */

&GLOBAL-DEFINE trash-right-edge trashbut:FRAME-X IN FRAME main-frame ~
            + trashbut:WIDTH-PIXELS IN FRAME main-frame
&GLOBAL-DEFINE trash-left-edge trashbut:FRAME-X IN FRAME main-frame
&GLOBAL-DEFINE trash-top-edge trashbut:FRAME-Y IN FRAME main-frame
&GLOBAL-DEFINE trash-bottom-edge trashbut:FRAME-Y IN FRAME main-frame ~
            + trashbut:HEIGHT-PIXELS IN FRAME main-frame

DEFINE VARIABLE confirm AS LOGICAL.
DEFINE VARIABLE i AS INTEGER.
DEFINE VARIABLE orig-x AS INTEGER.
DEFINE VARIABLE orig-y AS INTEGER.
DEFINE VARIABLE temp-handle AS WIDGET-HANDLE.

DEFINE BUTTON cancel-button LABEL "Cancel" AUTO-ENDKEY.
DEFINE BUTTON ok-button LABEL "OK" AUTO-GO.
DEFINE BUTTON trashbut LABEL "Trash" IMAGE FILE "trash".

FORM
   trashbut AT ROW 12 COLUMN 65
   WITH FRAME main-frame WIDTH 75 TITLE "Sales Rep Management".

FORM
   salesrep.sales-rep rep-name salesrep.region month-quota SKIP(1)
   ok-button AT 30 cancel-button
   WITH FRAME update-frame VIEW-AS DIALOG-BOX TITLE "Update Sales Rep".

FORM
   salesrep.sales-rep
   WITH FRAME delete-frame VIEW-AS DIALOG-BOX TITLE "Delete Sales Rep".

ON CHOOSE OF trashbut DO:
   PROMPT-FOR salesrep.sales-rep WITH FRAME delete-frame.
   FIND salesrep EXCLUSIVE-LOCK WHERE salesrep.sales-rep =
         INPUT sales-rep.
    
   temp-handle = FRAME main-frame:FIRST-CHILD.
   DO WHILE NOT temp-handle:FOREGROUND:
      temp-handle = temp-handle:NEXT-SIBLING.
   END.
   temp-handle = temp-handle:FIRST-CHILD.
   DO WHILE temp-handle:LABEL <> salesrep.sales-rep:
      temp-handle = temp-handle:NEXT-SIBLING.
   END.
   DELETE WIDGET temp-handle.
   DELETE salesrep.
END.

STATUS INPUT "Press SPACEBAR to update or " + KBLABEL("END-ERROR") +
             " to end. Use trash to delete.".
   
i = 1.
FOR EACH salesrep NO-LOCK:
  CREATE BUTTON temp-handle
      ASSIGN LABEL = salesrep.sales-rep
             ROW = i
             FRAME = FRAME main-frame:HANDLE
             MOVABLE = TRUE
      TRIGGERS:
         ON END-MOVE DO:
            IF SELF:FRAME-X < {&trash-right-edge} AND
               SELF:FRAME-X + SELF:WIDTH-PIXELS > {&trash-left-edge} AND
               SELF:FRAME-Y < {&trash-bottom-edge} AND
               SELF:FRAME-Y + SELF:HEIGHT-PIXELS > {&trash-top-edge} 
            THEN DO:
               MESSAGE "Do you really want to delete" SELF:LABEL "?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE confirm.
               IF confirm
               THEN DO:
                  FIND salesrep EXCLUSIVE-LOCK
                        WHERE salesrep.sales-rep = SELF:LABEL.
                  DELETE salesrep.
                  DELETE WIDGET SELF.
               END.
               ELSE DO:
                  ASSIGN SELF:X = orig-x
                         SELF:Y = orig-y.
               END.
            END.             
         END.
         ON START-MOVE DO:
            ASSIGN orig-x = SELF:X
                   orig-y = SELF:Y.
         END.
         ON CHOOSE DO:
            FIND salesrep EXCLUSIVE-LOCK
                WHERE salesrep.sales-rep = SELF:LABEL.
            UPDATE salesrep.sales-rep rep-name region month-quota
                   ok-button cancel-button WITH FRAME update-frame.
         END.
      END TRIGGERS.
   i = i + 1.
END.

ENABLE ALL WITH FRAME main-frame.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
