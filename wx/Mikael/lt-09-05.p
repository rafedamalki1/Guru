/* lt-09-05.p */

/****************** DEFINE QUERIES **********************/
DEFINE QUERY newq FOR PERSONALTAB FIELDS 
   (TELEFON FORNAMN PERSONALKOD), 
   PERSONALPRIS FIELDS (PRIS STARTAD PERSONALKOD).

/****************** DEFINE WIDGETS **********************/
DEFINE BROWSE newbrw QUERY newq
   DISPLAY FORNAMN SPACE(1) TELEFON SPACE(1) PRIS SPACE(1)
   STARTAD WITH 17 DOWN.
DEFINE BUTTON BTN-EXIT LABEL "Exit".

/******************* DEFINE FRAMES **********************/
DEFINE FRAME FRAME1
   newbrw AT ROW 1 COLUMN 2
   BTN-EXIT AT ROW 14 COLUMN 55
   SPACE(2) SKIP(1)
   WITH NO-BOX CENTERED THREE-D.

/******************** MAIN LOGIC ************************/
OPEN QUERY newq FOR EACH PERSONALTAB, EACH PERSONALPRIS
   WHERE PERSONALPRIS.PERSONALKOD = PERSONALTAB.PERSONALKOD BY PRIS.
DISPLAY newbrw WITH FRAME FRAME1.
ENABLE ALL WITH FRAME FRAME1.

/******************** WAIT-FOR ************************/
WAIT-FOR CHOOSE OF BTN-EXIT IN FRAME Frame1.
CLOSE QUERY newq.