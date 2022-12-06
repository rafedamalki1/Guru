/* p-says */

DEFINE SUB-MENU sounds
   MENU-ITEM sound-cat LABEL "Cat"
   MENU-ITEM sound-cow LABEL "Cow"
   MENU-ITEM sound-dog LABEL "Dog".
   
DEFINE SUB-MENU exit-all
   MENU-ITEM exit-confirm LABEL "Really?".
     
DEFINE MENU mainbar  MENUBAR
   SUB-MENU sounds   LABEL "Sounds"
   SUB-MENU exit-all LABEL "Exit".

ASSIGN MENU-ITEM sound-cat:PRIVATE-DATA = "Meow"
       MENU-ITEM sound-cow:PRIVATE-DATA = "Mooo"
       MENU-ITEM sound-dog:PRIVATE-DATA = "Bow-wow".
   
ON CHOOSE OF MENU-ITEM sound-cat, MENU-ITEM sound-cow,
                MENU-ITEM sound-dog
   DO:
      MESSAGE SELF:PRIVATE-DATA.
      ASSIGN SELF:SENSITIVE = FALSE.
   END.
   
ON CHOOSE OF MENU-ITEM exit-confirm
   DO:
      QUIT.
   END.

CURRENT-WINDOW:MENUBAR = MENU mainbar:HANDLE. 

DISPLAY "BARNYARD FUN" WITH FRAME title-frame CENTERED.
  
WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
