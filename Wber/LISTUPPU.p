/*LISTUPPU.P.*/
{KONVALTEMP.I}
{TEMPUPP.I}
/*DEFINE TEMP-TABLE temp_upp
   FIELD UPPLAG LIKE BERUPP.UPPLAG
   FIELD ADRESS LIKE BERUPP.ADRESS
   FIELD ANMARK LIKE BERUPP.ANMARK
   INDEX OMR UPPLAG ASCENDING.*/


DEFINE INPUT PARAMETER valaonr LIKE AONRTAB.AONR NO-UNDO.
DEFINE INPUT PARAMETER valomrade LIKE AONRTAB.OMRADE NO-UNDO.
DEFINE INPUT PARAMETER valkonst AS LOGICAL.
DEFINE INPUT PARAMETER TABLE FOR kon_val.

DEFINE OUTPUT PARAMETER TABLE FOR temp_upp.
DEFINE VARIABLE nyanm AS CHARACTER NO-UNDO.
   IF valkonst = FALSE THEN DO:   
      OPEN QUERY berq FOR EACH BERUPP WHERE BERUPP.AONR = valaonr AND
      BERUPP.OMRADE = valomrade USE-INDEX OMR NO-LOCK.
      GET FIRST berq NO-LOCK.
      DO WHILE AVAILABLE(BERUPP):
         IF INDEX (BERUPP.ANMARK,"$") > 0 THEN nyanm = SUBSTRING(BERUPP.ANMARK,1,INDEX(BERUPP.ANMARK,"$") - 1).
         ELSE nyanm = BERUPP.ANMARK.
         CREATE temp_upp.
         ASSIGN
         temp_upp.UPPLAG = BERUPP.UPPLAG
         temp_upp.ADRESS = BERUPP.ADRESS
         temp_upp.ANMARK = nyanm.
         
         /*qString = "WHERE AONR = '" + BERUPP.AONR + "' AND OMRADE = '" + BERUPP.OMRADE + "'  AND NUM = " + STRING(BERUPP.UPPLAG).
         berpBuffer:FIND-FIRST(qString,NO-LOCK) NO-ERROR.
         IF berpBuffer:AVAILABLE THEN DO:
            BUFFER temp_upp:HANDLE:BUFFER-COPY(berpBuffer,"DELNR,ANMARK,OMRADE").
            ASSIGN         
            temp_upp.XKORD   =   berpBuffer:BUFFER-FIELD("XKORD"):BUFFER-VALUE / 10000000   
            temp_upp.YKORD   =   berpBuffer:BUFFER-FIELD("YKORD"):BUFFER-VALUE / 10000000.
            IF temp_upp.XKORD > 0 AND temp_upp.XKORDCH = "" THEN  temp_upp.XKORDCH = STRING(temp_upp.XKORD).
            IF temp_upp.YKORD > 0 AND temp_upp.YKORDCH = "" THEN  temp_upp.YKORDCH = STRING(temp_upp.YKORD).
            
         END.*/

         GET NEXT berq NO-LOCK.
      END.
      CLOSE QUERY berq.
   END.
   ELSE DO:
      FOR EACH kon_val:
         OPEN QUERY berq FOR EACH BERUPP WHERE BERUPP.AONR = kon_val.BERAONR AND
         BERUPP.OMRADE = kon_val.OMRADE NO-LOCK.
         GET FIRST berq NO-LOCK.
         DO WHILE AVAILABLE(BERUPP):
            FIND FIRST temp_upp WHERE temp_upp.UPPLAG = BERUPP.UPPLAG
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE temp_upp THEN DO: 
               IF INDEX (BERUPP.ANMARK,"$") > 0 THEN nyanm = SUBSTRING(BERUPP.ANMARK,1,INDEX(BERUPP.ANMARK,"$")).
               ELSE nyanm = BERUPP.ANMARK.                          
               CREATE temp_upp.
               ASSIGN
               temp_upp.UPPLAG = BERUPP.UPPLAG
               temp_upp.ADRESS = BERUPP.ADRESS
               temp_upp.ANMARK = nyanm.
/*               qString = "WHERE AONR = '" + BERUPP.AONR + "' AND OMRADE = '" + BERUPP.OMRADE + "'  AND NUM = " + STRING(BERUPP.UPPLAG).
               berpBuffer:FIND-FIRST(qString,NO-LOCK) NO-ERROR.
               IF berpBuffer:AVAILABLE THEN DO:
                  BUFFER temp_upp:HANDLE:BUFFER-COPY(berpBuffer,"DELNR,ANMARK,OMRADE").
                  ASSIGN         
                  temp_upp.XKORD   =   berpBuffer:BUFFER-FIELD("XKORD"):BUFFER-VALUE / 10000000   
                  temp_upp.YKORD   =   berpBuffer:BUFFER-FIELD("YKORD"):BUFFER-VALUE / 10000000.
                  IF temp_upp.XKORD > 0 AND temp_upp.XKORDCH = "" THEN  temp_upp.XKORDCH = STRING(temp_upp.XKORD).
                  IF temp_upp.YKORD > 0 AND temp_upp.YKORDCH = "" THEN  temp_upp.YKORDCH = STRING(temp_upp.YKORD).                  
               END.*/               
            END.
            GET NEXT berq NO-LOCK.
         END.
         CLOSE QUERY berq.
      END.
   END.
