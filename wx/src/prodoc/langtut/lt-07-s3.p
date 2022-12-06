/* Solution to Language Tutorial Problem 7-3 */

/**********  DEFINE VARIABLES  **********/
DEFINE VARIABLE Equipment AS INTEGER INITIAL 0 
    VIEW-AS SLIDER MIN-VALUE 0 MAX-VALUE 100 SIZE-CHARS 25 BY 2.5
    TIC-MARKS BOTTOM FREQUENCY 10.
DEFINE VARIABLE Supply AS INTEGER INITIAL 0 
    VIEW-AS SLIDER  MIN-VALUE 0 MAX-VALUE 100 SIZE-CHARS 25 BY 2.5
    TIC-MARKS BOTTOM FREQUENCY 10.
DEFINE VARIABLE Apparel AS INTEGER INITIAL 0 
    VIEW-AS SLIDER MIN-VALUE 0 MAX-VALUE 100 SIZE-CHARS 25 BY 2.5
    TIC-MARKS BOTTOM FREQUENCY 10.
DEFINE VARIABLE Business AS CHARACTER VIEW-AS SELECTION-LIST 
    SINGLE LIST-ITEMS "Department Store", "General Sports Retailer",
    "Specialty Sports Retailer", "Distributor", "Mail Order Distributor", 
    "Secondary Distributor", "Sports Club" 
    INNER-CHARS 25 INNER-LINES 7 SORT.
DEFINE VARIABLE Season AS CHARACTER VIEW-AS COMBO-BOX
    LIST-ITEMS "Winter", "Spring", "Summer", "Fall".
DEFINE VARIABLE Comment AS CHARACTER VIEW-AS EDITOR
    INNER-CHARS 23 INNER-LINES 8 BUFFER-CHARS 23 BUFFER-LINES 15
    MAX-CHARS 300 SCROLLBAR-VERTICAL.
DEFINE BUTTON btn-Exit LABEL "Exit".

/**********  DEFINE FRAMES  **********/
DEFINE FRAME Frame1
    "Percent of Business in:"   AT ROW  1 COL  2
    "Sports Equipment?"         AT ROW  2 COL  2 
    Equipment                   AT ROW  3 COL  2        
    "Sports Supplies?"          AT ROW  2 COL 30
    Supply                      AT ROW  3 COL 28
    "Sports Apparel?"           AT ROW  2 COL 55
    Apparel                     AT ROW  3 COL 54
    "Business Description"      AT ROW  6 COL  2
    Business                    AT ROW  7 COL  2
    "Best Season"               AT ROW  6 COL 33
    Season                      AT ROW  7 COL 33
    "Comments"                  AT ROW  6 COL 50
    Comment                     AT ROW  7 COL 50
    btn-Exit                    AT ROW 15 COL  2
        WITH NO-LABELS CENTERED TITLE "All Around Sports Questionaire" THREE-D.

/**********  MAIN LOGIC  **********/
ENABLE ALL WITH FRAME Frame1.
WAIT-FOR CHOOSE OF btn-Exit.



