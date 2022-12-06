/* p-dymenu.p */

DEFINE VARIABLE exit-item-ptr    AS WIDGET-HANDLE.
DEFINE VARIABLE srep-menu-ptr	 AS WIDGET-HANDLE.
DEFINE VARIABLE main-bar-ptr	 AS WIDGET-HANDLE.
DEFINE VARIABLE temp-hand-ptr	 AS WIDGET-HANDLE. 

FORM
   salesrep.sales-rep rep-name salesrep.region month-quota
   WITH FRAME x WITH SIDE-LABELS ROW 5 CENTERED.

VIEW FRAME x.

/* Create the main menu bar. */
CREATE MENU main-bar-ptr.
     
/* Create a pull-down menu to list all sales reps. */
CREATE SUB-MENU srep-menu-ptr
     ASSIGN PARENT = main-bar-ptr
	    LABEL = "Reps".

/* Create a menu item for each record in the Salesrep file. */		
FOR EACH Salesrep BY rep-name:
   CREATE MENU-ITEM temp-hand-ptr
	ASSIGN PARENT = srep-menu-ptr
	       LABEL = salesrep.rep-name
	TRIGGERS:
	    ON CHOOSE
	       DO: 
		   FIND FIRST salesrep WHERE rep-name = SELF:LABEL.
		   DISPLAY salesrep WITH FRAME x.
	       END.
	END TRIGGERS.
END.

/* Add a rule to the srep-menu-ptr. */
CREATE MENU-ITEM temp-hand-ptr
     ASSIGN SUBTYPE = "RULE"
	    PARENT = srep-menu-ptr.

/* Add an exit item to the srep-menu-ptr. */
CREATE MENU-ITEM exit-item-ptr
    ASSIGN PARENT = srep-menu-ptr
	   LABEL = "E&xit"
	   SENSITIVE = TRUE.
	   
 
/* Set up the menu bar. */
CURRENT-WINDOW:MENUBAR = main-bar-ptr.

/* Disable menu items for all west coast sales reps. 
   To begin, find the first item in srep-men-ptr.		 */
temp-hand-ptr = srep-menu-ptr:FIRST-CHILD.

test-items:
DO WHILE temp-hand-ptr <> ?:
   /* Find the Salesrep record for this item (if any). */
   IF temp-hand-ptr:SUBTYPE = "NORMAL"
   THEN DO:
       FIND FIRST salesrep WHERE salesrep.rep-name =
                                 temp-hand-ptr:LABEL NO-ER~
ROR.
   
      /* Check if this rep is in the West region.
	 If so, disable the menu item.		    */
      IF AVAILABLE(salesrep)
      THEN IF salesrep.region = "West"
	   THEN temp-hand-ptr:SENSITIVE = FALSE.
   END.
   
   /* Find the next item in srep-men. */
   temp-hand-ptr = temp-hand-ptr:NEXT-SIBLING.
END.

/* Wait for the user to select Exit. */
WAIT-FOR CHOOSE OF exit-item-ptr.
