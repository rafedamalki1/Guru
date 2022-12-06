/* p-popup2.p */

DEFINE BUTTON hi1    LABEL "Hello".
DEFINE BUTTON hi2    LABEL "Hi".

DEFINE MENU popmenu1 TITLE "Button1 State"
   MENU-ITEM ve      LABEL "Hello"
   MENU-ITEM vd      LABEL "Howdy"
   MENU-ITEM iv      LABEL "Hey"
   RULE
   MENU-ITEM ep      LABEL "Exclamation point" TOGGLE-BOX
   RULE
   MENU-ITEM ex      LABEL "Exit".

DEFINE MENU popmenu2 TITLE "Button2 State" LIKE popmenu1.
 
FORM
   hi1          AT x 30   Y 70
   hi2          AT x 130  Y 70 
   WITH FRAME button-frame WIDTH 30.

/* Set popmenu1 and popmenu2 to be the pop-up menus for hi1 and hi2. */   
ASSIGN hi1:POPUP-MENU  = MENU popmenu1:HANDLE
       hi2:POPUP-MENU  = MENU popmenu2:HANDLE.

/* Define action for menu selections. */
ON CHOOSE OF MENU-ITEM ve IN MENU popmenu1, MENU-ITEM vd IN MENU popmenu1,
 MENU-ITEM iv IN MENU popmenu1
   ASSIGN hi1:LABEL IN FRAME button-frame = SELF:LABEL.

ON CHOOSE OF MENU-ITEM ve IN MENU popmenu2, MENU-ITEM vd IN MENU popmenu2, 
MENU-ITEM iv IN MENU popmenu2
   ASSIGN hi2:LABEL IN FRAME button-frame = SELF:LABEL.

/* Define action for button selection. When the button is
   selected, display the current button label as a message.
   If Exclamation Point is checked, add an exclamation point
   to the message; otherwise, add a period.                       */
ON CHOOSE OF hi1
   MESSAGE hi1:LABEL IN FRAME button-frame +
           (IF MENU-ITEM ep:CHECKED IN MENU popmenu1 THEN "!" ELSE ".").
           
ON CHOOSE OF hi2
   MESSAGE hi2:LABEL IN FRAME button-frame +
           (IF MENU-ITEM ep:CHECKED IN MENU popmenu2 THEN "!" ELSE ".").          

/* Enable input on the button and wait
   for the user to select Exit from menu. */              
ENABLE hi1 hi2 WITH FRAME button-frame.

WAIT-FOR CHOOSE OF MENU-ITEM ex IN MENU popmenu1,
                   MENU-ITEM ex IN MENU popmenu2.
