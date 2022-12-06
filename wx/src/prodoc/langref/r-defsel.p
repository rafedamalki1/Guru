/* r-defsel.p */

DEFINE VARIABLE i AS INTEGER NO-UNDO.

DEFINE VARIABLE clubs AS CHARACTER
  VIEW-AS SELECTION-LIST SIZE 20 BY 5 MULTIPLE
    SCROLLBAR-VERTICAL NO-DRAG
    LIST-ITEMS "One Iron", "Two Iron", "Three Iron", "Four Iron",
               "Five Iron", "Six Iron", "Seven Iron", "Eight Iron",
               "Nine Iron", "Pitching Wedge"
  LABEL "Golf Clubs Available"
  TRIGGERS:
    ON GO 
    DO:
      IF SELF:SCREEN-VALUE <> "" THEN
      DO i = 1 TO NUM-ENTRIES(SELF:SCREEN-VALUE) :
        DISPLAY ENTRY(i, SELF:SCREEN-VALUE) FORMAT "X(16)" 
          WITH FRAME clubs-sel CENTERED
            NUM-ENTRIES(SELF:SCREEN-VALUE) + 1 DOWN
            TITLE "Clubs Selected" USE-TEXT.
        DOWN 1 WITH FRAME clubs-sel. 
      END.
    END.
  END TRIGGERS.
    
ENABLE clubs WITH FRAME get-info TITLE "Select the Desired Club(s)".
  
WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
