/* r-levent.p */

DEFINE VARIABLE inv-price LIKE item.price.
DEFINE VARIABLE inv-value LIKE item.price.
DEFINE VARIABLE report-type AS INTEGER INITIAL 1.

DEFINE VARIABLE event-list AS CHARACTER VIEW-AS SELECTION-LIST
                           INNER-CHARS 20 INNER-LINES 5
                           SCROLLBAR-VERTICAL.

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

FORM
   event-list
   WITH FRAME list-frame NO-LABELS TITLE "Events" WIDTH 30.

ON ? ANYWHERE
  DO:
     FRAME list-frame:TITLE = "Events for " + FOCUS:TYPE. 
     event-list:LIST-ITEMS IN FRAME list-frame = LIST-EVENTS(FOCUS).
     DISPLAY event-list WITH FRAME list-frame.
     ENABLE event-list WITH FRAME list-frame.
     RETURN NO-APPLY.  
END.

ENABLE ALL WITH FRAME select-frame.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
