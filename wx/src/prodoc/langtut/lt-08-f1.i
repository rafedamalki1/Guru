/* Form definition for Chapter 8 programming examples */

/**********  DEFINE WIDGETS  **********/
DEFINE VARIABLE Answer AS LOGICAL.
DEFINE BUTTON btn-Prev LABEL "Prev".
DEFINE BUTTON btn-Next LABEL "Next".
DEFINE BUTTON btn-Update LABEL "Update".
DEFINE BUTTON btn-Add LABEL "Add".
DEFINE BUTTON btn-Delete LABEL "Delete".
DEFINE BUTTON btn-Exit LABEL "Exit".
DEFINE BUTTON btn-OK LABEL "OK" AUTO-GO.
DEFINE BUTTON btn-Cancel LABEL "Cancel" AUTO-ENDKEY.

/**********  DEFINE FRAMES  **********/
DEFINE FRAME Frame1
    SKIP(1) 
    sports.Item.Item-Num LABEL "Item No." COLON 15
    sports.Item.Item-Name LABEL "Name" COLON 15
    sports.Item.Price COLON 15
    sports.Item.On-Hand LABEL "On Hand" COLON 15
    sports.Item.Allocated  COLON 15
    sports.Item.Re-Order LABEL "Reorder At" COLON 15
    sports.Item.On-Order LABEL "On Order" COLON 15
    sports.Item.Cat-Page LABEL "Catalog Page" COLON 15
    sports.Item.Cat-Description LABEL "Description" FORMAT "x(40)"  
        VIEW-AS FILL-IN COLON 15  SPACE(2) SKIP(1)
    btn-Prev TO 12 
    btn-Next 
    btn-Update 
    btn-Add
    btn-Delete
    btn-Exit SKIP(1)
        WITH SIDE-LABELS CENTERED ROW 2 THREE-D
        TITLE "Database Access Form for the Item Table".

DEFINE FRAME Dialog1
    SKIP(1)
    sports.Item.Item-Name LABEL "Name" COLON 15
    sports.Item.Price COLON 50 SPACE(2)
    sports.Item.On-Hand LABEL "On Hand" COLON 15
    sports.Item.Allocated  COLON 50
    sports.Item.Re-Order LABEL "Reorder At" COLON 15
    sports.Item.On-Order LABEL "On Order" COLON 50
    sports.Item.Cat-Page LABEL "Catalog Page" COLON 15
    sports.Item.Cat-Description LABEL "Description" VIEW-AS EDITOR 
        INNER-LINES 6 INNER-CHARS 50 COLON 15  SPACE(2) SKIP(1)
    btn-OK TO 35 btn-Cancel SKIP(1)  
        WITH SIDE-LABELS VIEW-AS DIALOG-BOX TITLE "Update Item Record" 
         THREE-D.
         
         
         
