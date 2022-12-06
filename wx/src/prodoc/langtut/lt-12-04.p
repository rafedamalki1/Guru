
DEFINE VARIABLE parent1 AS HANDLE.
DEFINE VARIABLE child1 AS HANDLE.
DEFINE VARIABLE grandchild1 AS HANDLE.
DEFINE BUTTON btn-Exit LABEL "Exit".

CREATE WINDOW parent1 
  ASSIGN TITLE =  "Parent Window"
         HEIGHT-CHARS = 5
         WIDTH-CHARS = 35.

CREATE WINDOW child1 
  ASSIGN TITLE =  "Child Window"
         HEIGHT-CHARS = 5
         WIDTH-CHARS = 35
         PARENT = parent1.

CREATE WINDOW grandchild1 
  ASSIGN TITLE =  "Grandchild Window"
         HEIGHT-CHARS = 5
         WIDTH-CHARS = 35
         PARENT = child1.

DISPLAY btn-Exit WITH FRAME framea.
ENABLE ALL WITH FRAME framea.
VIEW parent1.
VIEW child1.
VIEW grandchild1.
WAIT-FOR CHOOSE OF btn-Exit.
