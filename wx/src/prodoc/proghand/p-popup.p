/* p-popup.p */

DEFINE BUTTON hi     LABEL "Hello".

DEFINE MENU popmenu TITLE "Button State"
   MENU-ITEM ve      LABEL "Hello"
   MENU-ITEM vd      LABEL "Howdy"
   MENU-ITEM iv      LABEL "Hey"
   RULE
   MENU-ITEM ep      LABEL "Exclamation point" TOGGLE-BOX
   RULE
   MENU-ITEM ex      LABEL "Exit".

FORM
   hi  AT ROW 4 COLUMN 5
     WITH FRAME button-frame. 

/* Set popmenu to be the pop-up menu for hi. */   
ASSIGN hi:POPUP-MENU = MENU popmenu:HANDLE.

/* Define action for menu selections. */
ON CHOOSE OF MENU-ITEM ve, MENU-ITEM vd, MENU-ITEM iv
   ASSIGN hi:LABEL IN FRAME button-frame = SELF:LABEL.

/* Define action for button selection. When the button is
   selected, display the current button label as a message.
   If Exclamation Point is checked, add an exclamation point
   to the message; otherwise, add a period.			  */
ON CHOOSE OF hi
   MESSAGE hi:LABEL IN FRAME button-frame +
	     (IF MENU-ITEM ep:CHECKED IN MENU popmenu THEN "!" ELSE ".").
     
/* Enable input on the button and wait for the user to select 
   Exit from menu. */		    
ENABLE hi WITH FRAME button-frame.

WAIT-FOR CHOOSE OF MENU-ITEM ex.
