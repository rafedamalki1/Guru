/* p-tmptbl.p */

DEFINE TEMP-TABLE temp-item
   FIELD cat-page LIKE item.cat-page
   FIELD inventory LIKE item.price LABEL "Inventory Value"
   INDEX cat-page AS PRIMARY cat-page ASC
   INDEX inventory-value inventory DESCENDING.

DEFINE VARIABLE cutoff LIKE item.price.
DEFINE VARIABLE inv-price LIKE item.price.
DEFINE VARIABLE report-type AS INTEGER INITIAL 1.
      
FORM
   cutoff LABEL "Inventory Lower Cutoff for each Catalog Page"
      AT ROW 1.25 COLUMN 2
   report-type LABEL "Report Sorted ..."
      AT ROW 2.25 COLUMN 2 
      VIEW-AS RADIO-SET RADIO-BUTTONS
	 "By Catalog Page",   1,
	 "By Inventory Value", 2
WITH FRAME select-frame
      SIDE-LABELS
      WIDTH 70
      TITLE "Specify Report ..." 
      VIEW-AS DIALOG-BOX.

FOR EACH item BREAK BY item.cat-page:
   ACCUMULATE price * on-hand (SUB-TOTAL BY item.cat-page).
   IF LAST-OF(item.cat-page) THEN DO:
      inv-price = ACCUM SUB-TOTAL BY item.cat-page (price * on-hand).
      CREATE temp-item.
      temp-item.cat-page = item.cat-page.
      inventory 	  = inv-price.
   END.
END. /* FOR EACH item */

REPEAT:

   UPDATE cutoff report-type WITH FRAME select-frame.

   IF report-type = 1 THEN
      FOR EACH temp-item USE-INDEX cat-page WITH FRAME rpt1-frame:
         IF inventory >= cutoff THEN
            DISPLAY temp-item.cat-page inventory.
      END.
   ELSE
      FOR EACH temp-item USE-INDEX inventory-value WITH FRAME rpt2-frame:
         IF inventory >= cutoff THEN
            DISPLAY temp-item.cat-page inventory.
      END.

END. /* REPEAT */
