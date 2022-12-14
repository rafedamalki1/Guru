/* lt-08-01.p */

/******************* DEFINE QUERY ***********************/
DEFINE QUERY persq FOR PERSONALTAB.

/****************** DEFINE WIDGETS **********************/
DEFINE BUTTON BTN-PREV LABEL "Prev".
DEFINE BUTTON BTN-NEXT LABEL "Next".
DEFINE BUTTON BTN-EXIT LABEL "Exit".

/******************* DEFINE FRAMES **********************/
DEFINE FRAME Frame1
   SKIP(1)
   PERSONALTAB.PERSONALKOD LABEL "Personalkod" COLON 15
   PERSONALTAB.EFTERNAMN COLON 15
   PERSONALTAB.FORNAMN LABEL "F�rnamn" COLON 15
   PERSONALTAB.GATUADRESS COLON 15
   PERSONALTAB.POSTNUMMER COLON 15
   PERSONALTAB.POSTADRESS COLON 15 
   SKIP(1)
   BTN-PREV TO 12
   BTN-NEXT 
   BTN-EXIT 
   SKIP(1) 
   WITH SIDE-LABELS CENTERED ROW 2 THREE-D
   TITLE "Database Access Form for the Personaltab Table".

/****************** DEFINE TRIGGERS *********************/
ON CHOOSE OF BTN-PREV
DO:
   GET PREV persq.
   IF QUERY-OFF-END(persq) THEN GET LAST persq.
   DISPLAY PERSONALTAB.PERSONALKOD PERSONALTAB.EFTERNAMN PERSONALTAB.FORNAMN 
      PERSONALTAB.GATUADRESS PERSONALTAB.POSTNUMMER PERSONALTAB.POSTADRESS 
      WITH FRAME Frame1.
END.

ON CHOOSE OF BTN-NEXT
DO:
   GET NEXT persq.
   IF QUERY-OFF-END(persq) THEN GET FIRST persq.
   DISPLAY PERSONALTAB.PERSONALKOD PERSONALTAB.EFTERNAMN PERSONALTAB.FORNAMN 
      PERSONALTAB.GATUADRESS PERSONALTAB.POSTNUMMER PERSONALTAB.POSTADRESS 
      WITH FRAME Frame1.

END.

/******************** MAIN LOGIC ************************/
OPEN QUERY persq FOR EACH PERSONALTAB.
GET FIRST persq.
DISPLAY PERSONALTAB.PERSONALKOD PERSONALTAB.EFTERNAMN PERSONALTAB.FORNAMN 
   PERSONALTAB.GATUADRESS PERSONALTAB.POSTNUMMER PERSONALTAB.POSTADRESS 
   WITH FRAME Frame1 USE-TEXT.
ENABLE BTN-PREV BTN-NEXT BTN-EXIT WITH FRAME Frame1.

/******************** WAIT-FOR ************************/
WAIT-FOR CHOOSE OF BTN-EXIT IN FRAME Frame1.
CLOSE QUERY persq.

