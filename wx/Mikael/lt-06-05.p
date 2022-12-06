/* lt-06-05.p */

/****************** DEFINE WIDGETS **********************/
DEFINE VARIABLE Chapter AS CHARACTER LABEL "Description"
   VIEW-AS EDITOR INNER-CHARS 25 INNER-LINES 3.
DEFINE VARIABLE Tut-List AS CHARACTER LABEL "Tutorial"
   INITIAL "Chapter 7" VIEW-AS SELECTION-LIST INNER-CHARS 15
   INNER-LINES 6 LIST-ITEMS "Chapter 7", "Chapter 8", "Chapter 9",
   "Chapter 10", "Chapter 11", "Chapter 12".
DEFINE BUTTON BTN-EXIT LABEL "Exit".

/******************* DEFINE FRAME ***********************/
DEFINE FRAME Frame1
   Tut-List AT ROW 2 COLUMN 2 Chapter AT ROW 8 COLUMN 2
   BTN-EXIT AT ROW 11 COLUMN 35
      WITH SIDE-LABELS TITLE "Tutorial: Coming Attractions" CENTERED THREE-D.

/****************** DEFINE TRIGGERS *********************/
ON VALUE-CHANGED OF Tut-List DO:
   CASE Tut-List:SCREEN-VALUE:
      WHEN "Chapter 7" THEN
         ASSIGN
         Chapter = "Chapter 7 describes programming techniques!".
      WHEN "Chapter 8" THEN
         ASSIGN
         Chapter = "Chapter 8 describes Progress data handling!".
      WHEN "Chapter 9" THEN
         ASSIGN
         Chapter = "Chapter 9 describes selection , sorting, etc!".
      WHEN "Chapter 10" THEN
         ASSIGN
         Chapter = "Chapter 10 summarizes what you already know about frames!".
      WHEN "Chapter 11" THEN
         ASSIGN
         Chapter = "Chapter 11 shows how to define and use menu, menu bars and menu items!".
      WHEN "Chapter 12" THEN
         ASSIGN
         Chapter = "Chapter 12 covers 4GL features to create reports!".
   END CASE.
   DISPLAY Chapter WITH FRAME Frame1.
END.

/******************** MAIN LOGIC ************************/
Tut-List:SCREEN-VALUE = "Chapter 7".
ENABLE ALL WITH FRAME Frame1.
APPLY "VALUE-CHANGED" TO Tut-List IN FRAME Frame1.
WAIT-FOR CHOOSE OF BTN-EXIT.
