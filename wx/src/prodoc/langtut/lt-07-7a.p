 /**********  DEFINE WIDGETS  **********/
DEFINE VARIABLE X-Coord AS INTEGER INITIAL 1 VIEW-AS SLIDER 
    MIN-VALUE 1 MAX-VALUE 20 HORIZONTAL SIZE-CHARS 20 BY 2.5
    TIC-MARKS BOTTOM FREQUENCY 5 NO-CURRENT-VALUE.
DEFINE VARIABLE Y-Coord AS INTEGER INITIAL 1 VIEW-AS SLIDER 
    MIN-VALUE 1 MAX-VALUE 10 VERTICAL SIZE-CHARS 10 BY 10
    TIC-MARKS RIGHT FREQUENCY 1 LARGE-TO-SMALL NO-CURRENT-VALUE.
DEFINE VARIABLE Point AS CHARACTER INITIAL "X"  FORMAT "x" 
    VIEW-AS TEXT.
DEFINE VARIABLE Coords AS CHAR FORMAT "X(5)" VIEW-AS TEXT.    
DEFINE BUTTON btn-Exit LABEL "Exit".

/**********  DEFINE FRAMES  **********/
DEFINE FRAME Frame1
    Coords  NO-LABEL AT ROW 1 COLUMN 1
    X-Coord NO-LABEL AT ROW 1 COLUMN 12 SKIP
    Y-Coord NO-LABEL
    btn-Exit AT ROW 14 COLUMN 1
    Point NO-LABEL
        WITH SIZE-CHARS 36 BY 15 CENTERED THREE-D.

/**********  DEFINE TRIGGERS  **********/
ON VALUE-CHANGED OF X-Coord, Y-Coord
DO:
    ASSIGN X-Coord 
           Y-Coord
           Point:COL = (X-Coord:COL) + (X-Coord - 1)
                       /* (Starting Position) + (Offset) */
           Point:ROW = (Y-Coord:ROW) + (Y-Coord - 1)
           Coords:SCREEN-VALUE = STRING(X-Coord, "99") + "," +
                                 STRING(Y-Coord, "99").
END.   

/**********  MAIN LOGIC  **********/
DISPLAY X-Coord Y-Coord Point WITH FRAME Frame1.
ENABLE ALL WITH FRAME Frame1.
APPLY "VALUE-CHANGED" TO X-Coord.
WAIT-FOR CHOOSE OF btn-Exit.



