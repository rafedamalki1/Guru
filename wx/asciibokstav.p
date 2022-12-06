/*
               KSV Editor
    Copyright: (C) 2000-2003 Serguey Klimoff (bulkl0DD)
     Filename: ASCIIBOKSTAV.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 2008.12.02 16:58 ELPAO   
     Modified: 2008.12.02 17:33 ELPAO    
     M DISP CHR(68) + CHR(85) + CHR(32) + CHR(77) + CHR(197) + CHR(83) + CHR(84) + CHR(69) + CHR(32) + CHR(76) + CHR(79) + CHR(71) + CHR(71) + CHR(65) + CHR(32) + CHR(73) + CHR(78) + CHR(32) + CHR(73) + CHR(71) + CHR(69) + CHR(78) + CHR(33)
   FORMAT "x(69)".
*/


RUN Prott_UI (INPUT "DU MÅSTE LOGGA IN IGEN!"). 





PROCEDURE Prott_UI :
   DEFINE INPUT  PARAMETER intxt AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ascivarde AS CHARACTER FORMAT "x(256)" NO-UNDO.
   DEFINE VARIABLE hh AS CHARACTER NO-UNDO.
   DEFINE VARIABLE itxt AS INTEGER NO-UNDO.
   DEFINE VARIABLE dummy AS CHARACTER NO-UNDO.
   itxt = 0.
   intxt = TRIM(intxt). 
      
   REPEAT:
      itxt = itxt + 1. 
      IF itxt > LENGTH(intxt) THEN LEAVE. 
      ascivarde = ascivarde  + "CHR(" + STRING(ASC(SUBSTRING(intxt,itxt,1))) + ") " .
      IF itxt < LENGTH(intxt) THEN ascivarde = ascivarde + "+ ".
   END.    

UPDATE ascivarde VIEW-AS EDITOR INNER-CHARS 68 INNER-LINES 17 WITH FRAME DDD WIDTH 80.

MESSAGE STRING(ascivarde) 
   
VIEW-AS ALERT-BOX.

END PROCEDURE.

PROCEDURE Prottold_UI :
   DEFINE INPUT  PARAMETER intxt AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ascivarde AS CHARACTER FORMAT "x(256)" NO-UNDO.
   DEFINE VARIABLE hh AS CHARACTER NO-UNDO.
   DEFINE VARIABLE itxt AS INTEGER NO-UNDO.
   itxt = 0.
   intxt = TRIM(intxt). 
      
   REPEAT:
      itxt = itxt + 1. 
      IF itxt > LENGTH(intxt) THEN LEAVE. 
      ascivarde = ascivarde  + "CHR(" + STRING(ASC(SUBSTRING(intxt,itxt,1))) + ") " .
      IF itxt < LENGTH(intxt) THEN ascivarde = ascivarde + "+ ".
   END.    

DEFINE VARIABLE ascivarde1 AS CHARACTER FORMAT "x(68)"  NO-UNDO.
DEFINE VARIABLE ascivarde2 AS CHARACTER FORMAT "x(68)"  NO-UNDO.
DEFINE VARIABLE ascivarde3 AS CHARACTER FORMAT "x(68)"  NO-UNDO.
DEFINE VARIABLE ascivarde4 AS CHARACTER FORMAT "x(68)"  NO-UNDO.
ascivarde1 = SUBSTRING(ascivarde,1,68).
ascivarde2 = SUBSTRING(ascivarde,69,68).
ascivarde3 = SUBSTRING(ascivarde,137,68).
ascivarde4 = SUBSTRING(ascivarde,205,68).

UPDATE ascivarde1 WITH FRAME cc   DOWN .
DOWN 1 WITH FRAME cc.
UPDATE ascivarde2 WITH FRAME cc   DOWN.
DOWN 1 WITH FRAME cc.
UPDATE ascivarde3 WITH FRAME cc   DOWN.
DOWN 1 WITH FRAME cc.
UPDATE ascivarde4 WITH FRAME cc   DOWN.
DOWN 1 WITH FRAME cc.

MESSAGE STRING(ascivarde) 
   
VIEW-AS ALERT-BOX.

END PROCEDURE.


PROCEDURE tt_UI :
   
DEFINE VARIABLE ascivarde AS CHARACTER FORMAT "x(256)" NO-UNDO.
DEFINE VARIABLE ascivarde1 AS CHARACTER FORMAT "x(68)"  NO-UNDO.
DEFINE VARIABLE ascivarde2 AS CHARACTER FORMAT "x(68)"  NO-UNDO.
DEFINE VARIABLE ascivarde3 AS CHARACTER FORMAT "x(68)"  NO-UNDO.
ascivarde = "CHR(" + STRING(ASC("J")) + ") + " .
ascivarde = ascivarde + "CHR(" + STRING(ASC("a")) + ") + ".
ascivarde = ascivarde + "CHR(" + STRING(ASC("g")) + ") + ".
ascivarde = ascivarde + "CHR(" + STRING(ASC("g")) + ") + ".
ascivarde = ascivarde + "CHR(" + STRING(ASC("i")) + ") + ".
ascivarde = ascivarde + "CHR(" + STRING(ASC("l")) + ") + ".
ascivarde = ascivarde + "CHR(" + STRING(ASC("l")) + ") + ".
ascivarde = ascivarde + "CHR(" + STRING(ASC("a")) + ") + ".
ascivarde = ascivarde + "CHR(" + STRING(ASC("r")) + ")".
ascivarde1 = SUBSTRING(ascivarde,1,68).
ascivarde2 = SUBSTRING(ascivarde,69,68).
ascivarde3 = SUBSTRING(ascivarde,137,68).


UPDATE ascivarde1 WITH FRAME cc   DOWN .
DOWN 1 WITH FRAME cc.
UPDATE ascivarde2 WITH FRAME cc   DOWN.
DOWN 1 WITH FRAME cc.
UPDATE ascivarde3 WITH FRAME cc   DOWN.
DOWN 1 WITH FRAME cc.

MESSAGE ascivarde
VIEW-AS ALERT-BOX.

END PROCEDURE.

PROCEDURE isSiffraBokstav_UI :
   DEFINE INPUT PARAMETER varde AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER artecken AS LOGICAL NO-UNDO.
   DEFINE VARIABLE ascivarde AS INTEGER NO-UNDO.
   
   ascivarde = ASC(varde).
   /*siffror*/
   IF ascivarde >= 48 AND ascivarde <= 57 THEN artecken = FALSE.
   
   /*stora bokstäver*/
   ELSE IF ascivarde >= 65 AND ascivarde <= 90 THEN artecken = TRUE.
   
   /*små bokstäver*/
   ELSE IF ascivarde >= 97 AND ascivarde <= 122 THEN artecken = TRUE.
   
   /*Å Ä Ö å ä ö*/
   ELSE IF ascivarde = 197 OR ascivarde = 196 OR ascivarde = 214 OR ascivarde = 229 OR 
      ascivarde = 228 OR ascivarde = 246 THEN artecken = TRUE.
   ELSE artecken = FALSE.
END PROCEDURE.


PROCEDURE goromSiffraBokstav_UI :
   DEFINE INPUT-OUTPUT PARAMETER varde AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ascivarde AS INTEGER NO-UNDO.
   DEFINE VARIABLE artecken AS LOGICAL NO-UNDO.
   
   ascivarde = ASC(varde).

   /*siffror*/
   IF ascivarde >= 48 AND ascivarde <= 57 THEN artecken = TRUE.
   
   /*stora bokstäver*/
   ELSE IF ascivarde >= 65 AND ascivarde <= 90 THEN artecken = TRUE.
   
   /*små bokstäver*/
   ELSE IF ascivarde >= 97 AND ascivarde <= 122 THEN artecken = TRUE.
   
   /*Å Ä Ö å ä ö*/
   ELSE IF ascivarde = 197 OR ascivarde = 196 OR ascivarde = 214 OR ascivarde = 229 OR 
      ascivarde = 228 OR ascivarde = 246 THEN artecken = TRUE.

   ELSE artecken = FALSE.

   IF artecken = FALSE THEN DO:
      varde = "".
   END.
   
END PROCEDURE.

PROCEDURE goromChar_UI :
   DEFINE INPUT-OUTPUT PARAMETER varde AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ascivarde AS INTEGER NO-UNDO.
   DEFINE VARIABLE artecken AS LOGICAL NO-UNDO.
   DEFINE VARIABLE vardearray AS CHARACTER EXTENT 50 NO-UNDO.
   DEFINE VARIABLE langd AS INTEGER NO-UNDO.
   DEFINE VARIABLE i AS INTEGER NO-UNDO.

   i = 1.

   langd = LENGTH(varde).
   /*MESSAGE langd VIEW-AS ALERT-BOX.*/

   DO WHILE i <= (langd):

      vardearray[i] = SUBSTRING(varde,1, 1).
      varde = SUBSTRING(varde,2).

   
      /*MESSAGE varde vardearray[i] VIEW-AS ALERT-BOX.*/

      i = i + 1.
   END.

   varde = "".
   i = 1.
   DO WHILE i <= langd:
      ascivarde = ASC(vardearray[i]).

      /*siffror*/
      IF ascivarde >= 48 AND ascivarde <= 57 THEN artecken = TRUE.
      
      /*stora bokstäver*/
      ELSE IF ascivarde >= 65 AND ascivarde <= 90 THEN artecken = TRUE.
      
      /*små bokstäver*/
      ELSE IF ascivarde >= 97 AND ascivarde <= 122 THEN artecken = TRUE.
      
      /*Å Ä Ö å ä ö*/
      ELSE IF ascivarde = 197 OR ascivarde = 196 OR ascivarde = 214 OR ascivarde = 229 OR 
         ascivarde = 228 OR ascivarde = 246 THEN artecken = TRUE.
   
      ELSE artecken = FALSE.
   
      IF artecken = FALSE THEN DO:
         /*varde = "".*/
      END.
      ELSE varde = varde + vardearray[i].
      i = i + 1.
   

      
  END.      
      /*MESSAGE "Värde: " varde VIEW-AS ALERT-BOX.*/
END PROCEDURE.


PROCEDURE goromDec_UI :
   DEFINE INPUT-OUTPUT PARAMETER varde AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ascivarde AS INTEGER NO-UNDO.
   DEFINE VARIABLE artecken AS LOGICAL NO-UNDO.
   DEFINE VARIABLE vardearray AS CHARACTER EXTENT 50 NO-UNDO.
   DEFINE VARIABLE langd AS INTEGER NO-UNDO.
   DEFINE VARIABLE i AS INTEGER NO-UNDO.

   i = 1.

   langd = LENGTH(varde).
   /*MESSAGE langd VIEW-AS ALERT-BOX.*/

   DO WHILE i <= (langd):

      vardearray[i] = SUBSTRING(varde,1, 1).
      varde = SUBSTRING(varde,2).

   
      /*MESSAGE varde vardearray[i] VIEW-AS ALERT-BOX.*/

      i = i + 1.
   END.

   varde = "".
   i = 1.
   DO WHILE i <= langd:
      ascivarde = ASC(vardearray[i]).

      /*siffror*/
      IF ascivarde >= 48 AND ascivarde <= 57 THEN artecken = TRUE.
      
      ELSE artecken = FALSE.
   
      IF artecken = FALSE THEN DO:
         /*varde = "".*/
      END.
      ELSE varde = varde + vardearray[i].
      
      IF vardearray[i] = "," THEN DO:
         vardearray[i] = ".".
         varde = varde + vardearray[i].
      END.
           
      i = i + 1.      
  END.      
      /*MESSAGE "Värde: " varde VIEW-AS ALERT-BOX.*/
END PROCEDURE.



PROCEDURE goromInt_UI :
   DEFINE INPUT-OUTPUT PARAMETER varde AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ascivarde AS INTEGER NO-UNDO.
   DEFINE VARIABLE artecken AS LOGICAL NO-UNDO.
   DEFINE VARIABLE vardearray AS CHARACTER EXTENT 50 NO-UNDO.
   DEFINE VARIABLE langd AS INTEGER NO-UNDO.
   DEFINE VARIABLE i AS INTEGER NO-UNDO.

   i = 1.

   langd = LENGTH(varde).
   /*MESSAGE langd VIEW-AS ALERT-BOX.*/

   DO WHILE i <= (langd):

      vardearray[i] = SUBSTRING(varde,1, 1).
      varde = SUBSTRING(varde,2).

   
      /*MESSAGE varde vardearray[i] VIEW-AS ALERT-BOX.*/

      i = i + 1.
   END.

   varde = "".
   i = 1.
   DO WHILE i <= langd:
      ascivarde = ASC(vardearray[i]).

      /*siffror*/
      IF ascivarde >= 48 AND ascivarde <= 57 THEN artecken = TRUE.
      
      ELSE artecken = FALSE.
   
      varde = varde + vardearray[i].
      
      i = i + 1.      
  END.      
      /*MESSAGE "Värde: " varde VIEW-AS ALERT-BOX.*/
END PROCEDURE.



