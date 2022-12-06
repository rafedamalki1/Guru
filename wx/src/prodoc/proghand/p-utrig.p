/* p-utrig.p */

DEFINE VARIABLE mess-string AS CHARACTER.
DEFINE VARIABLE text-str    AS CHARACTER FORMAT "x(12)" LABEL "String".
DEFINE VARIABLE verb-mode   AS LOGICAL.
DEFINE VARIABLE widget-type AS CHARACTER.

DEFINE BUTTON ok-butt LABEL "OK".
DEFINE BUTTON cancel-butt LABEL "CANCEL" AUTO-ENDKEY.

DEFINE SUB-MENU opt-menu
   MENU-ITEM opt-verbose TOGGLE-BOX LABEL "Verbose".

DEFINE MENU mainbar MENUBAR
   SUB-MENU opt-menu LABEL "Options".
   
CURRENT-WINDOW:MENUBAR = MENU mainbar:HANDLE.   
ENABLE ok-butt cancel-butt WITH FRAME x TITLE "Frame 1".
ENABLE text-str WITH FRAME y TITLE "Frame 2".

ON VALUE-CHANGED OF MENU-ITEM opt-verbose
   verb-mode = NOT verb-mode.

ON ENTRY ANYWHERE
   DO:
      IF verb-mode
      THEN DO:
         widget-type = SELF:TYPE.
    
         mess-string = "You are entering a " + widget-type + " (".
     
          
         IF CAN-QUERY(SELF, "TITLE")
         THEN mess-string = mess-string +
              (IF SELF:TITLE <> ? THEN SELF:TITLE ELSE " ") + ")".
         ELSE IF CAN-QUERY(SELF, "LABEL")
         THEN mess-string = mess-string + 
              (IF SELF:LABEL <> ? THEN SELF:LABEL ELSE " ") + ")".
         ELSE mess-string = mess-string + ")".
         
         MESSAGE mess-string.         
      END.
   END.    

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
