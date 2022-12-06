/* p-sel1.p */

DEFINE VARIABLE position AS CHARACTER
   LABEL "Position"
   INITIAL "Pitcher"
   VIEW-AS SELECTION-LIST
      INNER-CHARS 18
      INNER-LINES 10
      LIST-ITEMS
         "Pitcher", "Catcher", 
         "First Base", "Second Base",
         "Third Base", "Shortstop",
         "Left Field", "Center Field",
         "Right Field", "Designated Hitter".                                 
      
FORM
   position
   WITH FRAME sel-frame.

FORM
   position FORMAT "x(18)" VIEW-AS TEXT
   WITH FRAME text-frame.

ON GO OF FRAME sel-frame
   DO:
      ASSIGN position.
      DISPLAY position WITH FRAME text-frame.
   END.

DISPLAY position WITH FRAME sel-frame.
ENABLE position WITH FRAME sel-frame.
STATUS INPUT "Select a position and GO.".

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
