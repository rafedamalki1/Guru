/* p-dypop.p */

DEFINE BUTTON hi LABEL "Hello".

DEFINE VARIABLE pop-menu AS WIDGET-HANDLE.
  DEFINE VARIABLE ve AS WIDGET-HANDLE.
  DEFINE VARIABLE vd AS WIDGET-HANDLE.
  DEFINE VARIABLE iv AS WIDGET-HANDLE.
  DEFINE VARIABLE ep AS WIDGET-HANDLE.
  DEFINE VARIABLE ex AS WIDGET-HANDLE.
  DEFINE VARIABLE dummy-rule AS WIDGET-HANDLE.

FORM 
  hi AT x 30 y 70
  WITH FRAME button-frame.

CREATE MENU pop-menu
  ASSIGN POPUP-ONLY = TRUE
         TITLE = "Button State".

CREATE MENU-ITEM ve
ASSIGN
  PARENT = pop-menu
  LABEL = "Hello".

CREATE MENU-ITEM vd 
ASSIGN
  PARENT = pop-menu
  LABEL = "Howdy".

CREATE MENU-ITEM iv
ASSIGN
  PARENT = pop-menu
  LABEL = "Hey".

CREATE MENU-ITEM dummy-rule
ASSIGN
  PARENT = pop-menu
  SUBTYPE = "RULE".

CREATE MENU-ITEM ep
ASSIGN
  PARENT = pop-menu
  LABEL = "Exclamation point"
  TOGGLE-BOX = yes.

/* Create another rule--okay to re-use same widget-handle,
   because we aren't going to ever refer to the first rule again. */
CREATE MENU-ITEM dummy-rule
ASSIGN
  PARENT = pop-menu
  SUBTYPE = "RULE".

CREATE MENU-ITEM ex
ASSIGN
  PARENT = pop-menu
  LABEL = "Exit".

/* Set pop-menu to be the popup menu for hi-but*/
ASSIGN hi:POPUP-MENU = pop-menu.

/* Define action for menu selections */
ON CHOOSE OF ve, vd, iv
  ASSIGN hi:LABEL IN FRAME button-frame = SELF:LABEL.

/* Define action for button selection.	When the button is
   selected, display the current button label as a message.
   If Exclamation Point is checked, add an exclamation point
   to the message; otherwise, add a period.   */
ON CHOOSE of hi
  MESSAGE hi:LABEL IN FRAME button-frame +
          (IF ep:CHECKED THEN "!" ELSE ".").

/* Enable input on the button and wait
   for the user to choose Exit from menu. */
ENABLE hi WITH FRAME button-frame.

WAIT-FOR CHOOSE of ex.
