/**********  DEFINE WIDGETS  **********/
DEFINE VARIABLE Zone1 AS CHARACTER LABEL "Departure Time Zone" 
	FORMAT "x(10)" INITIAL "Eastern" VIEW-AS COMBO-BOX 
	LIST-ITEMS "Eastern","Central","Mountain","Pacific".
DEFINE VARIABLE Zone2 LIKE Zone1 LABEL "Arrival Time Zone". 
DEFINE VARIABLE Dtime AS DECIMAL LABEL "Departure Time".
DEFINE VARIABLE Ftime AS DECIMAL LABEL "Flight Time".
DEFINE VARIABLE Atime AS DECIMAL LABEL "Arrival Time".
DEFINE BUTTON btn-Calc LABEL "Calculate".
DEFINE BUTTON btn-Exit LABEL "Exit".
/**********  DEFINE FRAMES  **********/
DEFINE FRAME Frame1
SKIP(2) Zone1 COLON 22 SPACE(2) Zone2 SKIP(3)
Dtime COLON 22 SKIP(1) Ftime COLON 22 SKIP(1) Atime COLON 22 SKIP(1)
btn-Calc TO 22 btn-Exit SKIP(1)
    WITH SIDE-LABELS CENTERED THREE-D
        TITLE "Calculating Local Arrival Times for U.S. Domestic Flights".
/**********  DEFINE TRIGGERS  **********/
ON CHOOSE OF btn-Calc
DO:
    Atime = (DECIMAL(Dtime:SCREEN-VALUE) + 
            DECIMAL(Ftime:SCREEN-VALUE) + 
            (Zone1:LOOKUP(Zone1:SCREEN-VALUE) -
            Zone2:LOOKUP(Zone2:SCREEN-VALUE))) MOD 24.
    DISPLAY Atime WITH FRAME Frame1.
END.
/**********  MAIN LOGIC  **********/
DISPLAY Zone1 Zone2 WITH FRAME Frame1.
ENABLE ALL WITH FRAME Frame1.
WAIT-FOR CHOOSE OF btn-Exit.



