DEFINE VARIABLE inv-price LIKE item.price.
DEFINE VARIABLE inv-value LIKE item.price.
DEFINE VARIABLE report-type AS INTEGER INITIAL 1.

DEFINE BUTTON ok-butt LABEL "OK" AUTO-GO.
DEFINE BUTTON cancel-butt LABEL "CANCEL" AUTO-ENDKEY.
      
FORM
   inv-price LABEL "Price"
      AT ROW 1.25 COLUMN 2
   report-type LABEL "Report Sorted ..."
      AT ROW 2.25 COLUMN 2 
      VIEW-AS RADIO-SET RADIO-BUTTONS
	 "By Catalog Page",   1,
	 "By Inventory Value", 2
   SKIP
   ok-butt cancel-butt
   WITH FRAME select-frame SIDE-LABELS.

ON ? ANYWHERE
  DO:
     MESSAGE "This is a" FOCUS:TYPE + ". VALUE-CHANGED is"
         (IF VALID-EVENT(FOCUS, "VALUE-CHANGED") THEN "a" ELSE "NOT a")
         "valid event for this widget."
         VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
     RETURN NO-APPLY.  
END.

ENABLE ALL WITH FRAME select-frame.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
