/**********  DEFINE WIDGETS  **********/
DEFINE VARIABLE Chapter AS CHARACTER LABEL "Description"
    VIEW-AS EDITOR INNER-CHARS 25 INNER-LINES 6.
DEFINE VARIABLE Tut-List AS CHARACTER LABEL "Tutorial" 
    INITIAL "Chapter 7" VIEW-AS SELECTION-LIST INNER-CHARS 12 
    INNER-LINES 6 LIST-ITEMS "Chapter 7", "Chapter 8", "Chapter 9", 
    "Chapter 10", "Chapter 11", "Chapter 12".
DEFINE BUTTON btn-Exit LABEL "Exit".
  
/**********  DEFINE FRAMES  **********/                 
DEFINE FRAME Frame1
    Tut-List AT ROW 2 COLUMN 2 Chapter AT ROW 2 COLUMN 30
    btn-Exit AT ROW 12 COLUMN 2
        WITH SIDE-LABELS TITLE "Tutorial: Coming Attractions"
         CENTERED THREE-D.

/**********  DEFINE TRIGGERS  **********/
ON VALUE-CHANGED OF Tut-List DO:    
    CASE Tut-List:SCREEN-VALUE:
    WHEN "Chapter 7" THEN 
         ASSIGN Chapter = "Thoroughly covers the syntax, attributes, " +
                   "events, and programming techniques of each " +
                   "data widget.".
    WHEN "Chapter 8" THEN 
         ASSIGN Chapter = "Covers PROGRESS database access " +
                          "techniques.".
    WHEN "Chapter 9" THEN 
         ASSIGN Chapter = "Covers selecting, sorting, and relating " +
                          "records with the record phrase.".
    WHEN "Chapter 10" THEN 
         ASSIGN Chapter = "Covers the 4GL statements used to create " +
                          "reports.".
    WHEN "Chapter 11" THEN 
         ASSIGN Chapter = "Shows you how to build and customize " +
                          "a menu bar, menus, and menu items.".
    WHEN "Chapter 12" THEN 
         ASSIGN Chapter = "Covers programming issues that arise " +
                          "when developing large-scale applications.".
    END CASE.                         
    DISPLAY Chapter WITH FRAME Frame1.
END.

/**********  MAIN LOGIC  *********/
Tut-List:SCREEN-VALUE = "Chapter 7".
ENABLE ALL WITH FRAME Frame1.
APPLY "VALUE-CHANGED" TO Tut-List IN FRAME Frame1.
WAIT-FOR CHOOSE OF btn-Exit.


