/* Form definition for Chapter 8 browse widget programming example */

/**********  DEFINE WIDGETS  **********/
DEFINE BUTTON btn-Update LABEL "Update".
DEFINE BUTTON btn-Add LABEL "Add".
DEFINE BUTTON btn-Delete LABEL "Delete".
DEFINE BUTTON btn-Exit LABEL "Exit".
DEFINE BUTTON btn-OK LABEL "OK" AUTO-GO.
DEFINE BUTTON btn-Cancel LABEL "Cancel" AUTO-ENDKEY.

/**********  DEFINE FRAMES  **********/
DEFINE FRAME Frame1
    SKIP(1)
    Item.Item-Num LABEL "Item No." COLON 45 SKIP 
    Item.Item-Name LABEL "Name" COLON 45 SKIP
    Item.Price COLON 45 SKIP
    Item.On-Hand LABEL "On Hand" COLON 45 SKIP
    Item.Allocated COLON 45 SKIP
    Item.Re-Order LABEL "Reorder At" COLON 45 SKIP
    Item.On-Order LABEL "On Order" COLON 45 SKIP
    Item.Cat-Page LABEL "Catalog Page" COLON 45 SKIP
    Item.Cat-Description LABEL "Description" FORMAT "x(25)"  
         VIEW-AS FILL-IN COLON 45 SPACE SKIP(2)
    btn-Update COLON 35 btn-Add btn-Delete btn-Exit SKIP(1) 
    Item-Browse AT ROW 2 COLUMN 2
        WITH SIDE-LABELS USE-TEXT CENTERED 
        ROW 2 TITLE "Database Access Form for the Item Table".

DEFINE FRAME Dialog1
    SKIP(1)
    Item.Item-Name LABEL "Name" COLON 15
    Item.Price COLON 50 SPACE(2)
    Item.On-Hand LABEL "On Hand" COLON 15
    Item.Allocated  COLON 50
    Item.Re-Order LABEL "Reorder At" COLON 15
    Item.On-Order LABEL "On Order" COLON 50
    Item.Cat-Page LABEL "Catalog Page" COLON 15
    Item.Cat-Description LABEL "Description" VIEW-AS EDITOR 
        INNER-LINES 6 INNER-CHARS 50 COLON 15 SPACE(2) SKIP(1)
    btn-OK TO 35 btn-Cancel SKIP(1) 
        WITH SIDE-LABELS VIEW-AS DIALOG-BOX TITLE "Update Item Record".
