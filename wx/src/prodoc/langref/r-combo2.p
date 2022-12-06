/* r-combo2.p */

DEFINE VARIABLE i AS INTEGER.
DEFINE VARIABLE rep AS CHARACTER LABEL "Rep" VIEW-AS COMBO-BOX.
DEFINE VARIABLE temp-string AS CHARACTER.
          
FORM
   rep
   WITH FRAME main-frame SIDE-LABELS.   
   
ON ANY-PRINTABLE OF rep
   DO:
      /* Find the first entry in the drop down list
         that begins with the character typed. Set
         the SCREEN-VALUE of the combo box to that value. */
      seek-item:
      DO i = 1 TO SELF:NUM-ITEMS:
         IF SELF:ENTRY(i) BEGINS LAST-EVENT:FUNCTION
         THEN DO:
             SELF:SCREEN-VALUE = SELF:ENTRY(i).
             LEAVE seek-item.
         END.
      END.
      
      IF i > SELF:NUM-ITEMS
      THEN BELL.
   END.
   
ON CURSOR-DOWN OF rep
   DO:
      /* Change the SCREEN-VALUE of the combo box
         to the next value from the drop down list. */
      i = SELF:LOOKUP(SELF:SCREEN-VALUE).
      IF i < SELF:NUM-ITEMS
      THEN SELF:SCREEN-VALUE = SELF:ENTRY(i + 1).
   END.
   
ON CURSOR-UP OF rep
   DO:
      /* Change the SCREEN-VALUE of the combo box
         to the prev value from the drop down list. */     
      i = SELF:LOOKUP(SELF:SCREEN-VALUE).
      IF i > 1
      THEN SELF:SCREEN-VALUE = SELF:ENTRY(i - 1).
   END.

temp-string = "".
FOR EACH Salesrep NO-LOCK:
   IF temp-string = ""
   THEN temp-string = Salesrep.sales-rep.
   ELSE temp-string = temp-string + "," + Salesrep.sales-rep.
END.

ASSIGN rep:LIST-ITEMS IN FRAME main-frame = temp-string.
      
ENABLE rep WITH FRAME main-frame.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
