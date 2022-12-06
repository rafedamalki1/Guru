DEFINE VARIABLE namn AS CHARACTER NO-UNDO.
DEFINE VARIABLE NAMN2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE rakn AS INTEGER NO-UNDO.
DEFINE VARIABLE hjvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE forsta AS LOGICAL NO-UNDO.
rakn = 1.
namn = "LENA-maria OLSSON Pettersson".
namn2 = "".
/*
REPEAT:
   IF rakn > 50 THEN LEAVE.

   IF rakn = 1 THEN namn2 = namn2 + CAPS(SUBSTRING(namn,rakn,1)).
   ELSE IF hjvar = " " OR hjvar = "-" THEN namn2 = namn2 + CAPS(SUBSTRING(namn,rakn,1)).
   ELSE namn2 = namn2 + LC(SUBSTRING(namn,rakn,1)).
   hjvar = SUBSTRING(namn,rakn,1).
   rakn = rakn + 1.
END.
MESSAGE namn namn2 VIEW-AS ALERT-BOX.
    */
namn = CAPS(SUBSTRING(namn,1,1)) + LC(SUBSTRING(namn,2)).
rakn = 2.
forsta = TRUE.
REPEAT:

   IF INDEX(namn," ",rakn) = 0 THEN DO:
      IF forsta = TRUE THEN DO:
         rakn = 2.
         forsta = FALSE.
      END.
      IF INDEX(namn,"-",rakn) = 0 THEN LEAVE.
      ELSE rakn = INDEX(namn,"-",rakn).
   END.
   ELSE rakn = INDEX(namn," ",rakn).
   rakn = rakn + 1.
   SUBSTRING(namn,rakn,1) = CAPS(SUBSTRING(namn,rakn,1)).
   
END.
MESSAGE namn namn2 VIEW-AS ALERT-BOX.
