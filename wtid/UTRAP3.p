/*utrap3.p*/
&Scoped-define NEW NEW 
&Scoped-define SHARED SHARED 
{GLOBVAR2DEL1.I}
{PHMT.I}
FIND FIRST foretag NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
RUN STYRFORE.P (INPUT Guru.Konstanter:globforetag).
DEFINE TEMP-TABLE tidut1
   FIELD UT AS CHARACTER FORMAT "X(92)".
DEFINE TEMP-TABLE tidut2
   FIELD UT AS CHARACTER FORMAT "X(92)".
DEFINE TEMP-TABLE tidut3
   FIELD UT AS CHARACTER FORMAT "X(92)".
DEFINE INPUT PARAMETER RAD_SORTPERS AS INTEGER FORMAT "X(9)".
DEFINE INPUT PARAMETER RAD_VAL AS INTEGER FORMAT "X(9)".
DEFINE INPUT PARAMETER TABLE FOR valperstemp.
DEFINE OUTPUT PARAMETER TABLE FOR tidut1.
DEFINE OUTPUT PARAMETER TABLE FOR tidut2.
DEFINE OUTPUT PARAMETER TABLE FOR tidut3.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE str1 AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE VARIABLE str2 AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE VARIABLE str3 AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE VARIABLE vpers AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE breddantal AS INTEGER NO-UNDO.
DEFINE VARIABLE breddantal2 AS INTEGER NO-UNDO.
DEFINE VARIABLE breddantal3 AS INTEGER NO-UNDO.
DEFINE VARIABLE nrcol AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE nrcol2 AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE nrcol3 AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE utnr AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE utnr2 AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE utnr3 AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE bredd AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE bredd2 AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE bredd3 AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.
{EXTRADATA.I}
/*Columner för tidut1*/
ASSIGN
nrcol[1] = 1
nrcol[2] = 2
nrcol[3] = 3
nrcol[4] = 4
nrcol[5] = 5   /*ny kolumn*/
breddantal = 5   /*antal kolumner*/
bredd[1] = 25
bredd[2] = 15
bredd[3] = 8
bredd[4] = 20
bredd[5] = 12.
ASSIGN
i = 2.
utnr[nrcol[1]] = 1.
DO WHILE i <= breddantal:
   utnr[i] = utnr[i - 1] + bredd[i - 1] + 1.
   i = i + 1.
END.
ASSIGN
str1 = "".
i = 1.
DO WHILE i <= utnr[breddantal] + bredd[breddantal] - 1:
   str1 = str1 + "=".
   i = i + 1.
END.
i = 2.
DO WHILE i <= breddantal:
   SUBSTRING(str1,(utnr[i] - 1),1) = ".".
   i = i + 1.
END.
/*Columner för tidut2*/
ASSIGN
nrcol2[1] = 1
nrcol2[2] = 2 
nrcol2[3] = 3      /*ny kolumn*/
breddantal2 = 3   /*antal kolumner*/
bredd2[1] = 25
bredd2[2] = 25
bredd2[3] = 25.

ASSIGN
i = 2.
utnr2[nrcol2[1]] = 1.
DO WHILE i <= breddantal2:
   utnr2[i] = utnr2[i - 1] + bredd2[i - 1] + 1.
   i = i + 1.
END.
ASSIGN
str2 = "".
i = 1.
DO WHILE i <= utnr2[breddantal2] + bredd2[breddantal2] - 1:
   str2 = str2 + "=".
   i = i + 1.
END.
i = 2.
DO WHILE i <= breddantal2:
   SUBSTRING(str2,(utnr2[i] - 1),1) = ".".
   i = i + 1.
END.
/*Columner för tidut3*/
ASSIGN
nrcol3[1] = 1
nrcol3[2] = 2
nrcol3[3] = 3     /*ny kolumn*/
breddantal3 = 3   /*antal kolumner*/
bredd3[1] = 25
bredd3[2] = 45
bredd3[3] = 22.
ASSIGN
i = 2.
utnr3[nrcol3[1]] = 1.
DO WHILE i <= breddantal3:
   utnr3[i] = utnr3[i - 1] + bredd3[i - 1] + 1.
   i = i + 1.
END.
ASSIGN
str3 = "".
i = 1.
DO WHILE i <= utnr3[breddantal3] + bredd3[breddantal3] - 1:
   str3 = str3 + "=".
   i = i + 1.
END.
i = 2.
DO WHILE i <= breddantal3:
   SUBSTRING(str3,(utnr3[i] - 1),1) = ".".
   i = i + 1.
END.

RUN person_UI.

{GDPRLOGGCLIENT.I}
PROCEDURE person_UI :
   RUN rubrik_UI.   
   IF RAD_SORTPERS = 1 THEN DO: 
      OPEN QUERY pq FOR EACH valperstemp,                                            
      EACH PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valperstemp.PERSONALKOD NO-LOCK BY valperstemp.EFTERNAMN. 
      GET FIRST pq NO-LOCK.                                                             
      DO WHILE AVAILABLE(PERSONALTAB):
         RUN skaput_UI.                                        
         GET NEXT pq NO-LOCK.
      END.
   END. 
   IF RAD_SORTPERS = 2 THEN DO:
      OPEN QUERY pq FOR EACH valperstemp,                                            
      EACH PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valperstemp.PERSONALKOD NO-LOCK BY valperstemp.PERSONALKOD. 
      GET FIRST pq NO-LOCK.                                                             
      DO WHILE AVAILABLE(PERSONALTAB):
         RUN skaput_UI.                                          
         GET NEXT pq NO-LOCK.                                                           
      END.  
   END.
   IF RAD_SORTPERS = 3 THEN DO:
      OPEN QUERY pq FOR EACH valperstemp,                                            
      EACH PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valperstemp.PERSONALKOD NO-LOCK BY valperstemp.ANSTNR. 
      GET FIRST pq NO-LOCK.                                                             
      DO WHILE AVAILABLE(PERSONALTAB):
         RUN skaput_UI.                                       
         GET NEXT pq NO-LOCK.                                                           
      END.  
   END.
   IF RAD_SORTPERS = 4 THEN DO:
      FIND FIRST valperstemp USE-INDEX ANSV NO-LOCK NO-ERROR.
      REPEAT:
         FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valperstemp.ANSVARIGTIDR    USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
         IF AVAILABLE PERSONALTAB THEN DO:
            Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
            vpers = PERSONALTAB.PERSONALKOD.
            CREATE tidut1.
            ASSIGN
            SUBSTRING(tidut1.UT,1,22) = "ANSVARIG TIDREDOVISARE"  
            SUBSTRING(tidut1.UT,utnr[nrcol[3]]) = TRIM(PERSONALTAB.PERSONALKOD) 
            SUBSTRING(tidut1.UT,utnr[nrcol[4]]) = TRIM(PERSONALTAB.FORNAMN) 
            SUBSTRING(tidut1.UT,utnr[nrcol[5]]) = TRIM(PERSONALTAB.EFTERNAMN).
            IF RAD_VAL = 1 THEN DO:
               RUN rubrik2_UI.
            END.
            OPEN QUERY pq FOR EACH valperstemp WHERE valperstemp.ANSVARIGTIDR = vpers,                                            
            EACH PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valperstemp.PERSONALKOD NO-LOCK BY valperstemp.EFTERNAMN. 
            GET FIRST pq NO-LOCK.                                                             
            DO WHILE AVAILABLE(PERSONALTAB): 
               RUN skaput_UI.                                                   
               GET NEXT pq NO-LOCK.                                                           
            END. 
            CREATE tidut1.
            CREATE tidut1.
            FIND FIRST valperstemp WHERE valperstemp.ANSVARIGTIDR = vpers USE-INDEX ANSV NO-LOCK NO-ERROR.
            FIND NEXT valperstemp WHERE valperstemp.ANSVARIGTIDR NE vpers USE-INDEX ANSV NO-LOCK NO-ERROR.
            IF NOT AVAILABLE valperstemp THEN LEAVE.
            ELSE vpers = valperstemp.ANSVARIGTIDR.
         END.
         ELSE DO:
            FIND NEXT valperstemp  USE-INDEX ANSV NO-LOCK NO-ERROR.
            IF NOT AVAILABLE valperstemp THEN LEAVE.
            ELSE vpers = valperstemp.ANSVARIGTIDR.
         END.      
      END.
   END.
   IF RAD_SORTPERS = 5 THEN DO:
      FIND FIRST valperstemp USE-INDEX ANSV NO-LOCK NO-ERROR.
      REPEAT:
         FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valperstemp.ANSVARIGTIDR  USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
         IF AVAILABLE PERSONALTAB THEN DO:
            Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
            vpers = PERSONALTAB.PERSONALKOD.
            CREATE tidut1.
            ASSIGN
            SUBSTRING(tidut1.UT,1,22) = "ANSVARIG TIDREDOVISARE"
            SUBSTRING(tidut1.UT,utnr[nrcol[3]]) = TRIM(PERSONALTAB.PERSONALKOD) 
            SUBSTRING(tidut1.UT,utnr[nrcol[4]]) = TRIM(PERSONALTAB.FORNAMN) 
            SUBSTRING(tidut1.UT,utnr[nrcol[5]]) = TRIM(PERSONALTAB.EFTERNAMN).
            IF RAD_VAL = 1 THEN DO:
               RUN rubrik2_UI.
            END.
            OPEN QUERY pq FOR EACH valperstemp WHERE valperstemp.ANSVARIGTIDR = vpers,                                            
            EACH PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valperstemp.PERSONALKOD NO-LOCK BY valperstemp.PERSONALKOD. 
            GET FIRST pq NO-LOCK.                                                             
            DO WHILE AVAILABLE(PERSONALTAB): 
               RUN skaput_UI.                                               
               GET NEXT pq NO-LOCK.                                                           
            END.
            CREATE tidut1.
            CREATE tidut1.
            FIND FIRST valperstemp WHERE valperstemp.ANSVARIGTIDR = vpers USE-INDEX ANSV NO-LOCK NO-ERROR.
            FIND NEXT valperstemp WHERE valperstemp.ANSVARIGTIDR NE vpers USE-INDEX ANSV NO-LOCK NO-ERROR.
            IF NOT AVAILABLE valperstemp THEN LEAVE.
            ELSE vpers = valperstemp.ANSVARIGTIDR.
         END.
         ELSE DO:
            FIND NEXT valperstemp USE-INDEX ANSV NO-LOCK NO-ERROR.
            IF NOT AVAILABLE valperstemp THEN LEAVE.
            ELSE vpers = valperstemp.ANSVARIGTIDR.            
         END.               
      END.
   END.
   IF RAD_SORTPERS = 6 THEN DO:
      FIND FIRST valperstemp USE-INDEX ANSV NO-LOCK NO-ERROR.
      REPEAT:
         FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valperstemp.ANSVARIGTIDR USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
         IF AVAILABLE PERSONALTAB THEN DO:
            Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
            vpers = PERSONALTAB.PERSONALKOD.
            CREATE tidut1.
            ASSIGN
            SUBSTRING(tidut1.UT,1,22) = "ANSVARIG TIDREDOVISARE"
            SUBSTRING(tidut1.UT,utnr[nrcol[3]]) = TRIM(PERSONALTAB.PERSONALKOD) 
            SUBSTRING(tidut1.UT,utnr[nrcol[4]]) = TRIM(PERSONALTAB.FORNAMN) 
            SUBSTRING(tidut1.UT,utnr[nrcol[5]]) = TRIM(PERSONALTAB.EFTERNAMN).
            IF RAD_VAL = 1 THEN DO:
               RUN rubrik2_UI.
            END.
            OPEN QUERY pq FOR EACH valperstemp WHERE valperstemp.ANSVARIGTIDR = vpers,                                            
            EACH PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valperstemp.PERSONALKOD NO-LOCK BY valperstemp.ANSTNR. 
            GET FIRST pq NO-LOCK.                                                             
            DO WHILE AVAILABLE(PERSONALTAB): 
               RUN skaput_UI.                                                 
               GET NEXT pq NO-LOCK.                                                           
            END.
            CREATE tidut1.
            CREATE tidut1.
            FIND FIRST valperstemp WHERE valperstemp.ANSVARIGTIDR = vpers USE-INDEX ANSV NO-LOCK NO-ERROR.
            FIND NEXT valperstemp WHERE valperstemp.ANSVARIGTIDR NE vpers USE-INDEX ANSV NO-LOCK NO-ERROR.
            IF NOT AVAILABLE valperstemp THEN LEAVE.
            ELSE vpers = valperstemp.ANSVARIGTIDR.
         END.
         ELSE DO:
            FIND NEXT valperstemp  USE-INDEX ANSV NO-LOCK NO-ERROR.
            IF NOT AVAILABLE valperstemp THEN LEAVE.
            ELSE vpers = valperstemp.ANSVARIGTIDR.
         END.      
      END.
   END.
   IF RAD_SORTPERS = 7 THEN DO:
      FIND FIRST valperstemp USE-INDEX GODK NO-LOCK NO-ERROR.
      REPEAT:
         FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valperstemp.TIDSGODK  USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
         IF AVAILABLE PERSONALTAB THEN DO:
            Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
            vpers = PERSONALTAB.PERSONALKOD.
            CREATE tidut1.
            ASSIGN
            SUBSTRING(tidut1.UT,1,22) = "GODKÄNNER TIDSEDLAR"
            SUBSTRING(tidut1.UT,utnr[nrcol[3]]) = TRIM(PERSONALTAB.PERSONALKOD) 
            SUBSTRING(tidut1.UT,utnr[nrcol[4]]) = TRIM(PERSONALTAB.FORNAMN) 
            SUBSTRING(tidut1.UT,utnr[nrcol[5]]) = TRIM(PERSONALTAB.EFTERNAMN).
            IF RAD_VAL = 1 THEN DO:
               RUN rubrik2_UI.
            END.
            OPEN QUERY pq FOR EACH valperstemp WHERE valperstemp.TIDSGODK = vpers,                                            
            EACH PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valperstemp.PERSONALKOD NO-LOCK BY valperstemp.EFTERNAMN. 
            GET FIRST pq NO-LOCK.                                                             
            DO WHILE AVAILABLE(PERSONALTAB): 
               RUN skaput_UI.                                                
               GET NEXT pq NO-LOCK.                                                           
            END.
            CREATE tidut1.
            CREATE tidut1.
            FIND FIRST valperstemp WHERE valperstemp.TIDSGODK = vpers USE-INDEX GODK NO-LOCK NO-ERROR.
            FIND NEXT valperstemp WHERE valperstemp.TIDSGODK NE vpers USE-INDEX GODK NO-LOCK NO-ERROR.
            IF NOT AVAILABLE valperstemp THEN LEAVE.
            ELSE vpers = valperstemp.TIDSGODK.
         END.
         ELSE DO:
            FIND NEXT valperstemp USE-INDEX GODK NO-LOCK NO-ERROR.
            IF NOT AVAILABLE valperstemp THEN LEAVE.
            ELSE vpers = valperstemp.TIDSGODK.            
         END.      
      END.
   END.
   IF RAD_SORTPERS = 8 THEN DO:
      FIND FIRST valperstemp USE-INDEX GODK NO-LOCK NO-ERROR.
      REPEAT:
         FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valperstemp.TIDSGODK   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
         IF AVAILABLE PERSONALTAB THEN DO:
            Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
            vpers = PERSONALTAB.PERSONALKOD.
            CREATE tidut1.
            ASSIGN
            SUBSTRING(tidut1.UT,1,22) = "GODKÄNNER TIDSEDLAR"
            SUBSTRING(tidut1.UT,utnr[nrcol[3]]) = TRIM(PERSONALTAB.PERSONALKOD) 
            SUBSTRING(tidut1.UT,utnr[nrcol[4]]) = TRIM(PERSONALTAB.FORNAMN) 
            SUBSTRING(tidut1.UT,utnr[nrcol[5]]) = TRIM(PERSONALTAB.EFTERNAMN).
            IF RAD_VAL = 1 THEN DO:
               RUN rubrik2_UI.
            END.
            OPEN QUERY pq FOR EACH valperstemp WHERE valperstemp.TIDSGODK = vpers,                                            
            EACH PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valperstemp.PERSONALKOD NO-LOCK BY valperstemp.PERSONALKOD. 
            GET FIRST pq NO-LOCK.                                                             
            DO WHILE AVAILABLE(PERSONALTAB): 
               RUN skaput_UI.                                                  
               GET NEXT pq NO-LOCK.                                                           
            END.
            CREATE tidut1.
            CREATE tidut1.
            FIND FIRST valperstemp WHERE valperstemp.TIDSGODK = vpers USE-INDEX GODK NO-LOCK NO-ERROR.
            FIND NEXT valperstemp WHERE valperstemp.TIDSGODK NE vpers USE-INDEX GODK NO-LOCK NO-ERROR.
            IF NOT AVAILABLE valperstemp THEN LEAVE.
            ELSE vpers = valperstemp.TIDSGODK.
         END.
         ELSE DO:
            FIND NEXT valperstemp  USE-INDEX GODK NO-LOCK NO-ERROR.
            IF NOT AVAILABLE valperstemp THEN LEAVE.
            ELSE vpers = valperstemp.TIDSGODK.
         END.      
      END.
   END.
   IF RAD_SORTPERS = 9 THEN DO:
      FIND FIRST valperstemp USE-INDEX GODK NO-LOCK NO-ERROR.
      REPEAT:
         FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valperstemp.TIDSGODK  USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
         IF AVAILABLE PERSONALTAB THEN DO:
            Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
            vpers = PERSONALTAB.PERSONALKOD.
            CREATE tidut1.
            ASSIGN
            SUBSTRING(tidut1.UT,1,22) = "GODKÄNNER TIDSEDLAR"
            SUBSTRING(tidut1.UT,utnr[nrcol[3]]) = TRIM(PERSONALTAB.PERSONALKOD) 
            SUBSTRING(tidut1.UT,utnr[nrcol[4]]) = TRIM(PERSONALTAB.FORNAMN) 
            SUBSTRING(tidut1.UT,utnr[nrcol[5]]) = TRIM(PERSONALTAB.EFTERNAMN).
            IF RAD_VAL = 1 THEN DO:
               RUN rubrik2_UI.
            END.
            OPEN QUERY pq FOR EACH valperstemp WHERE valperstemp.TIDSGODK = vpers,                                            
            EACH PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valperstemp.PERSONALKOD NO-LOCK BY valperstemp.ANSTNR. 
            GET FIRST pq NO-LOCK.                                                             
            DO WHILE AVAILABLE(PERSONALTAB): 
               RUN skaput_UI.                                             
               GET NEXT pq NO-LOCK.                                                           
            END.
            CREATE tidut1.
            CREATE tidut1.
            FIND FIRST valperstemp WHERE valperstemp.TIDSGODK = vpers USE-INDEX GODK NO-LOCK NO-ERROR.
            FIND NEXT valperstemp WHERE valperstemp.TIDSGODK NE vpers USE-INDEX GODK NO-LOCK NO-ERROR.
            IF NOT AVAILABLE valperstemp THEN LEAVE.
            ELSE vpers = valperstemp.TIDSGODK.
         END.
         ELSE DO:
            FIND NEXT valperstemp  USE-INDEX GODK NO-LOCK NO-ERROR.
            IF NOT AVAILABLE valperstemp THEN LEAVE.
            ELSE vpers = valperstemp.TIDSGODK.
         END.   
      END.
   END.
   IF RAD_SORTPERS = 10 THEN DO:
      FIND FIRST valperstemp USE-INDEX OMR NO-LOCK NO-ERROR.
      REPEAT:
         FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = valperstemp.OMRADE   USE-INDEX OMR NO-LOCK NO-ERROR.
         vpers = OMRADETAB.OMRADE.
         CREATE tidut1.
         ASSIGN
         SUBSTRING(tidut1.UT,1,22) = CAPS(Guru.Konstanter:gomrk) + " / ORGANISATION"
         SUBSTRING(tidut1.UT,utnr[nrcol[3]]) = TRIM(OMRADETAB.OMRADE) 
         SUBSTRING(tidut1.UT,utnr[nrcol[4]]) = TRIM(OMRADETAB.NAMN).
         IF RAD_VAL = 1 THEN DO:
            RUN rubrik2_UI.
         END.
         OPEN QUERY pq FOR EACH valperstemp WHERE valperstemp.OMRADE = vpers,                                            
         EACH PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valperstemp.PERSONALKOD NO-LOCK BY valperstemp.EFTERNAMN. 
         GET FIRST pq NO-LOCK.                                                             
         DO WHILE AVAILABLE(PERSONALTAB): 
            RUN skaput_UI.                                             
            GET NEXT pq NO-LOCK.                                                           
         END.
         CREATE tidut1.
         CREATE tidut1.
         FIND FIRST valperstemp WHERE valperstemp.OMRADE = vpers USE-INDEX OMR NO-LOCK NO-ERROR.
         FIND NEXT valperstemp WHERE valperstemp.OMRADE NE vpers USE-INDEX OMR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE valperstemp THEN LEAVE.
         ELSE vpers = valperstemp.OMRADE.
      END.
   END.
   IF RAD_SORTPERS = 11 THEN DO:
      FIND FIRST valperstemp USE-INDEX OMR NO-LOCK NO-ERROR.
      REPEAT:
         FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = valperstemp.OMRADE  USE-INDEX OMR NO-LOCK NO-ERROR.
         vpers = OMRADETAB.OMRADE.
         CREATE tidut1.
         ASSIGN
         SUBSTRING(tidut1.UT,1,22) = CAPS(Guru.Konstanter:gomrk) + " / ORGANISATION"
         SUBSTRING(tidut1.UT,utnr[nrcol[3]]) = TRIM(OMRADETAB.OMRADE) 
         SUBSTRING(tidut1.UT,utnr[nrcol[4]]) = TRIM(OMRADETAB.NAMN).
         IF RAD_VAL = 1 THEN DO:
            RUN rubrik2_UI.
         END.
         OPEN QUERY pq FOR EACH valperstemp WHERE valperstemp.OMRADE = vpers,                                            
         EACH PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valperstemp.PERSONALKOD NO-LOCK BY valperstemp.PERSONALKOD. 
         GET FIRST pq NO-LOCK.                                                             
         DO WHILE AVAILABLE(PERSONALTAB): 
            RUN skaput_UI.                                                 
            GET NEXT pq NO-LOCK.                                                           
         END.
         CREATE tidut1.
         CREATE tidut1.
         FIND FIRST valperstemp WHERE valperstemp.OMRADE = vpers USE-INDEX OMR NO-LOCK NO-ERROR.
         FIND NEXT valperstemp WHERE valperstemp.OMRADE NE vpers USE-INDEX OMR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE valperstemp THEN LEAVE.
         ELSE vpers = valperstemp.OMRADE.
      END.
   END.
   IF RAD_SORTPERS = 12 THEN DO:
      FIND FIRST valperstemp USE-INDEX OMR NO-LOCK NO-ERROR.
      REPEAT:
         FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = valperstemp.OMRADE  USE-INDEX OMR NO-LOCK NO-ERROR.
         vpers = OMRADETAB.OMRADE.
         CREATE tidut1.
         ASSIGN
         SUBSTRING(tidut1.UT,1,22)= CAPS(Guru.Konstanter:gomrk) + " / ORGANISATION"
         SUBSTRING(tidut1.UT,utnr[nrcol[3]]) = TRIM(OMRADETAB.OMRADE) 
         SUBSTRING(tidut1.UT,utnr[nrcol[4]]) = TRIM(OMRADETAB.NAMN).
         IF RAD_VAL = 1 THEN DO:
            RUN rubrik2_UI.
         END.
         OPEN QUERY pq FOR EACH valperstemp WHERE valperstemp.OMRADE = vpers,                                            
         EACH PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valperstemp.PERSONALKOD NO-LOCK BY valperstemp.PERSONALKOD. 
         GET FIRST pq NO-LOCK.                                                             
         DO WHILE AVAILABLE(PERSONALTAB): 
            RUN skaput_UI.                                            
            GET NEXT pq NO-LOCK.                                                           
         END.
         CREATE tidut1.
         CREATE tidut1.
         FIND FIRST valperstemp WHERE valperstemp.OMRADE = vpers USE-INDEX OMR NO-LOCK NO-ERROR.
         FIND NEXT valperstemp WHERE valperstemp.OMRADE NE vpers USE-INDEX OMR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE valperstemp THEN LEAVE.
         ELSE vpers = valperstemp.OMRADE.
      END.
   END.
   IF RAD_SORTPERS = 13 THEN DO:
      RUN rubrik3_UI.
      OPEN QUERY pq FOR EACH valperstemp,                                            
      EACH PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valperstemp.PERSONALKOD NO-LOCK BY valperstemp.PERSONALKOD. 
      GET FIRST pq NO-LOCK.                                                             
      DO WHILE AVAILABLE(PERSONALTAB):
         IF PERSONALTAB.OVERTIDUTTAG = "I" THEN DO:         
            RUN skaput_UI.                                          
         END.
         GET NEXT pq NO-LOCK.                                                           
      END.  
   END.   
   IF RAD_SORTPERS = 14 THEN DO:
      RUN rubrik3_UI.
      OPEN QUERY pq FOR EACH valperstemp,                                            
      EACH PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valperstemp.PERSONALKOD NO-LOCK BY valperstemp.PERSONALKOD. 
      GET FIRST pq NO-LOCK.                                                             
      DO WHILE AVAILABLE(PERSONALTAB):
         IF PERSONALTAB.OMREGTID = 1 THEN DO:         
            RUN skaput_UI.                                          
         END.
         GET NEXT pq NO-LOCK.                                                           
      END.  
   END.   
   IF RAD_SORTPERS = 15 THEN DO:
      RUN rubrik3_UI.
      OPEN QUERY pq FOR EACH valperstemp,                                            
      EACH PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valperstemp.PERSONALKOD NO-LOCK BY valperstemp.PERSONALKOD. 
      GET FIRST pq NO-LOCK.                                                             
      DO WHILE AVAILABLE(PERSONALTAB):
         IF PERSONALTAB.DELTID = TRUE THEN DO:         
            RUN skaput_UI.                                          
         END.
         GET NEXT pq NO-LOCK.                                                           
      END.  
   END.
   IF RAD_SORTPERS = 16 THEN DO:
      RUN rubrik3_UI.
      OPEN QUERY fq FOR EACH valperstemp,                                            
      EACH PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valperstemp.PERSONALKOD NO-LOCK , 
      EACH FLEXAVT WHERE FLEXAVT.PERSONALKOD = valperstemp.PERSONALKOD NO-LOCK BY valperstemp.PERSONALKOD. 
      GET FIRST fq NO-LOCK.                                                             
      DO WHILE AVAILABLE(FLEXAVT):
         IF FLEXAVT.FLEXTID = TRUE THEN DO:         
            RUN skaput_UI.                                          
         END.
         GET NEXT fq NO-LOCK.                                                           
      END.  
   END.
   IF RAD_SORTPERS = 17 THEN DO:
      RUN rubrik3_UI.
      RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
      IF bloblog = FALSE THEN RETURN.
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
      OPEN QUERY pq FOR EACH valperstemp,                                            
      EACH PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valperstemp.PERSONALKOD NO-LOCK BY valperstemp.PERSONALKOD. 
      GET FIRST pq NO-LOCK.                                                             
      DO WHILE AVAILABLE(PERSONALTAB):
         EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
         CREATE inextradatatemp.          
         ASSIGN
         inextradatatemp.PROGRAM = "DISPENSÖ"                   
         inextradatatemp.HUVUDCH = valperstemp.PERSONALKOD.         
         RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
         FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
         IF AVAILABLE extradatatemp THEN DO:      
            IF extradatatemp.SOKLOG[1] = TRUE  THEN DO:
               RUN skaput_UI.                                          
            END.
         END.            
         GET NEXT pq NO-LOCK.                                                           
      END.  
   END.
   IF RAD_SORTPERS = 18 THEN DO:
      RUN rubrik3_UI.
      RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
      IF bloblog = FALSE THEN RETURN.
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
      OPEN QUERY pq FOR EACH valperstemp,                                            
      EACH PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valperstemp.PERSONALKOD NO-LOCK BY valperstemp.PERSONALKOD. 
      GET FIRST pq NO-LOCK.                                                             
      DO WHILE AVAILABLE(PERSONALTAB):
         EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
         CREATE inextradatatemp.          
         ASSIGN
         inextradatatemp.PROGRAM = "DISPENSLÄ"                   
         inextradatatemp.HUVUDCH = valperstemp.PERSONALKOD.            
         RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
         FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
         IF AVAILABLE extradatatemp THEN DO:      
            IF extradatatemp.SOKLOG[1] = TRUE  THEN DO:
               RUN skaput_UI.                                          
            END.
         END.                    
         GET NEXT pq NO-LOCK.                                                           
      END.  
   END.
   IF RAD_SORTPERS = 19 THEN DO:
      RUN rubrik3_UI.
      RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
      IF bloblog = FALSE THEN RETURN.
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
      OPEN QUERY pq FOR EACH valperstemp,                                            
      EACH PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valperstemp.PERSONALKOD NO-LOCK BY valperstemp.PERSONALKOD. 
      GET FIRST pq NO-LOCK.                                                             
      DO WHILE AVAILABLE(PERSONALTAB):
         EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
         CREATE inextradatatemp.          
         ASSIGN
         inextradatatemp.PROGRAM = "AVAFOR"                   
         inextradatatemp.HUVUDCH = valperstemp.PERSONALKOD.            
         RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
         FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
         IF AVAILABLE extradatatemp THEN DO:                  
            IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "gkal" OR Guru.Konstanter:globforetag = "LULE"   THEN DO:            
               IF extradatatemp.SOKINT[1] = 0 OR extradatatemp.SOKINT[1] = 54 THEN.
               ELSE DO:
                  RUN skaput_UI.                                          
               END.
            END.
            IF Guru.Konstanter:globforetag = "misv"  THEN DO:            
               IF extradatatemp.SOKINT[1] = 0 OR extradatatemp.SOKINT[1] = 27 THEN.
               ELSE DO:
                  RUN skaput_UI.                                          
               END.
            END.
         END.                    
         GET NEXT pq NO-LOCK.                                                           
      END.  
   END.
   IF RAD_SORTPERS = 20 THEN DO:
      RUN rubrik3_UI.
      RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
      IF bloblog = FALSE THEN RETURN.
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
      OPEN QUERY pq FOR EACH valperstemp,                                            
      EACH PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valperstemp.PERSONALKOD NO-LOCK BY valperstemp.PERSONALKOD. 
      GET FIRST pq NO-LOCK.                                                             
      DO WHILE AVAILABLE(PERSONALTAB):
         EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
         CREATE inextradatatemp.          
         ASSIGN
         inextradatatemp.PROGRAM = "SPFRISK"
         inextradatatemp.HUVUDCH = valperstemp.PERSONALKOD.            
         RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
         FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
         IF AVAILABLE extradatatemp THEN DO:                              
            IF extradatatemp.SOKLOG[1] = TRUE THEN DO:            
               RUN skaput_UI.                                          
            END.            
         END.                    
         GET NEXT pq NO-LOCK.                                                           
      END.  
   END.
END PROCEDURE.

PROCEDURE rubrik_UI :
      CREATE tidut1.
      CREATE tidut1.
      SUBSTRING(tidut1.UT,33,20) = STRING(TODAY).
      CREATE tidut1.
      ASSIGN
      SUBSTRING(tidut1.UT,33,20) = "Personaluppgifter".
      CREATE tidut1.
      IF RAD_VAL = 1 AND RAD_SORTPERS <= 3 THEN DO:
         CREATE tidut1.
         ASSIGN
         SUBSTRING(tidut1.UT,utnr[nrcol[1]]) = "Efternamn"
         SUBSTRING(tidut1.UT,utnr[nrcol[2]]) = "Förnamn"
         SUBSTRING(tidut1.UT,utnr[nrcol[3]]) = "P-kod"
         SUBSTRING(tidut1.UT,utnr[nrcol[4]]) = "Anst.nummer"
         SUBSTRING(tidut1.UT,utnr[nrcol[5]]) = "Personnummer".
         CREATE tidut1.
         tidut1.UT = str1.
      END.

      CREATE tidut2.
      CREATE tidut2.
      SUBSTRING(tidut2.UT,33,20) = STRING(TODAY).
      CREATE tidut2.
      ASSIGN
      SUBSTRING(tidut2.UT,33,20) = "Adresser för personal".
      CREATE tidut2.      
      
      CREATE tidut3.
      CREATE tidut3.
      SUBSTRING(tidut3.UT,33,20) = STRING(TODAY).
      CREATE tidut3.
      ASSIGN
      SUBSTRING(tidut3.UT,33,20) = "Avtal för personal".
      CREATE tidut3.
END PROCEDURE.


PROCEDURE rubrik2_UI :
   CREATE tidut1.
   CREATE tidut1.
   ASSIGN
   SUBSTRING(tidut1.UT,utnr[nrcol[1]]) = "Efternamn"
   SUBSTRING(tidut1.UT,utnr[nrcol[2]]) = "Förnamn"
   SUBSTRING(tidut1.UT,utnr[nrcol[3]]) = "P-kod"
   SUBSTRING(tidut1.UT,utnr[nrcol[4]]) = "Anst.nummer"
   SUBSTRING(tidut1.UT,utnr[nrcol[5]]) = "Personnummer".
   CREATE tidut1.
   tidut1.UT = str1.
END PROCEDURE.

PROCEDURE rubrik3_UI :
      /*CREATE tidut1.
      CREATE tidut1.
      SUBSTRING(tidut1.UT,33,20) = STRING(TODAY).
      CREATE tidut1.
      ASSIGN
      SUBSTRING(tidut1.UT,33,20) = "Personaluppgifter".*/
      IF RAD_VAL = 1 AND RAD_SORTPERS = 13 THEN DO:
         CREATE tidut1.      
         CREATE tidut1.      
         ASSIGN
         SUBSTRING(tidut1.UT,1) = "Personal med övertiduttag = ej övertid".
         CREATE tidut1.      
      END.      
      IF RAD_VAL = 1 AND RAD_SORTPERS = 14 THEN DO:
         CREATE tidut1.      
         CREATE tidut1.      
         ASSIGN
         SUBSTRING(tidut1.UT,1) = "Personal med spärr arbetstidsförkortning".
         CREATE tidut1.      
      END.      
      IF RAD_VAL = 1 AND RAD_SORTPERS = 15 THEN DO:
         CREATE tidut1.      
         CREATE tidut1.      
         ASSIGN
         SUBSTRING(tidut1.UT,1) = "Personal med deltid".
         CREATE tidut1.      
      END.
      IF RAD_VAL = 1 AND RAD_SORTPERS = 16 THEN DO:
         CREATE tidut1.      
         CREATE tidut1.      
         ASSIGN
         SUBSTRING(tidut1.UT,1) = "Personal med flexavtal".
         CREATE tidut1.      
      END.
      IF RAD_VAL = 1 AND RAD_SORTPERS = 17 THEN DO:
         CREATE tidut1.      
         CREATE tidut1.      
         ASSIGN
         SUBSTRING(tidut1.UT,1) = "Personal med dispens övertid".
         CREATE tidut1.      
      END.
      IF RAD_VAL = 1 AND RAD_SORTPERS = 18 THEN DO:
         CREATE tidut1.      
         CREATE tidut1.      
         ASSIGN
         SUBSTRING(tidut1.UT,1) = "Personal med dispens läkarbesök".
         CREATE tidut1.      
      END.
      CREATE tidut1.      
      IF RAD_VAL = 1 AND RAD_SORTPERS GE 13  THEN DO:
         CREATE tidut1.
         ASSIGN
         SUBSTRING(tidut1.UT,utnr[nrcol[1]]) = "Efternamn"
         SUBSTRING(tidut1.UT,utnr[nrcol[2]]) = "Förnamn"
         SUBSTRING(tidut1.UT,utnr[nrcol[3]]) = "P-kod"
         SUBSTRING(tidut1.UT,utnr[nrcol[4]]) = "Anst.nummer"
         SUBSTRING(tidut1.UT,utnr[nrcol[5]]) = "Personnummer".
         CREATE tidut1.
         tidut1.UT = str1.
      END.
      
END PROCEDURE.


/*Skaput*/
PROCEDURE skaput_UI :
   Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
      CREATE tidut1.
      ASSIGN
      SUBSTRING(tidut1.UT,utnr[nrcol[1]]) = TRIM(PERSONALTAB.EFTERNAMN)
      SUBSTRING(tidut1.UT,utnr[nrcol[2]]) = TRIM(PERSONALTAB.FORNAMN)  
      SUBSTRING(tidut1.UT,utnr[nrcol[3]]) = TRIM(PERSONALTAB.PERSONALKOD)  
      SUBSTRING(tidut1.UT,utnr[nrcol[4]]) = TRIM(PERSONALTAB.ANSTNR)  
      SUBSTRING(tidut1.UT,utnr[nrcol[5]]) = TRIM(STRING(PERSONALTAB.PERSONNUMMER ,"XXXXXX-XXXX")).
      
      CREATE tidut2.
      ASSIGN
      SUBSTRING(tidut2.UT,utnr2[nrcol2[1]]) = TRIM(PERSONALTAB.EFTERNAMN) 
      SUBSTRING(tidut2.UT,utnr2[nrcol2[2]]) = TRIM(PERSONALTAB.FORNAMN).
      CREATE tidut2.
      ASSIGN
      SUBSTRING(tidut2.UT,utnr2[nrcol2[1]]) = TRIM(PERSONALTAB.GATUADRESS) 
      SUBSTRING(tidut2.UT,utnr2[nrcol2[2]]) = "Box:" + TRIM(PERSONALTAB.BOXEN).
      CREATE tidut2.
      ASSIGN
      SUBSTRING(tidut2.UT,utnr2[nrcol2[1]]) = TRIM(PERSONALTAB.POSTNUMMER)  
      SUBSTRING(tidut2.UT,utnr2[nrcol2[2]]) = TRIM(PERSONALTAB.POSTADRESS).
      CREATE tidut2.
      ASSIGN
      SUBSTRING(tidut2.UT,utnr2[nrcol2[1]]) = "Ord.tfn:" + TRIM(PERSONALTAB.TELEFON)
      SUBSTRING(tidut2.UT,utnr2[nrcol2[2]]) = "Alt.tfn:" + TRIM(PERSONALTAB.TELEFON2)
      SUBSTRING(tidut2.UT,utnr2[nrcol2[3]]) = "Mobil:" + TRIM(PERSONALTAB.MOBILTEL).
      CREATE tidut2.
      ASSIGN 
      SUBSTRING(tidut2.UT,utnr2[nrcol2[1]]) = "Mob.radio:" + TRIM(PERSONALTAB.MOBILRADIO)
      SUBSTRING(tidut2.UT,utnr2[nrcol2[2]]) = "E-post:" + TRIM(SUBSTRING(PERSONALTAB.PERSONSOK,20))
      SUBSTRING(tidut2.UT,utnr2[nrcol2[3]]) = "Sem.tfn:" + TRIM(PERSONALTAB.TELEFONSEM).
      CREATE tidut2.
      IF Guru.Konstanter:varforetypval[2] = 0 THEN DO:
         FIND FIRST BEFATTNINGSTAB WHERE BEFATTNINGSTAB.BEFATTNING = valperstemp.BEFATTNING
         USE-INDEX BEF NO-LOCK NO-ERROR.
         FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = valperstemp.ANSTALLNING
         USE-INDEX ANSTF NO-LOCK NO-ERROR.
         FIND FIRST BERTAB WHERE BERTAB.BEREDSKAPSAVTAL = valperstemp.BEREDSKAPSAVTAL
         USE-INDEX BERTAB NO-LOCK NO-ERROR.
         FIND FIRST TRAAVTAB WHERE TRAAVTAB.TRAAVTAL = valperstemp.TRAAVTAL
         USE-INDEX TRAAVTAB NO-LOCK NO-ERROR.
         CREATE tidut3.
         ASSIGN
         SUBSTRING(tidut3.UT,utnr3[nrcol3[1]]) = TRIM(PERSONALTAB.EFTERNAMN)
         SUBSTRING(tidut3.UT,utnr3[nrcol3[2]]) = TRIM(PERSONALTAB.FORNAMN)
         SUBSTRING(tidut3.UT,utnr3[nrcol3[3]]) = TRIM(BEFATTNINGSTAB.NAMN).
         CREATE tidut3.
         ASSIGN
         SUBSTRING(tidut3.UT,utnr3[nrcol3[1]]) = "Anställningsform:"
         SUBSTRING(tidut3.UT,utnr3[nrcol3[2]]) = TRIM(ANSTFORMTAB.ANSTALLNING). 
         CREATE tidut3.
         ASSIGN
         SUBSTRING(tidut3.UT,utnr3[nrcol3[1]]) = "Beredskapsavtal:"
         SUBSTRING(tidut3.UT,utnr3[nrcol3[2]]) = TRIM(BERTAB.FORKL). 
         CREATE tidut3.
         ASSIGN
         SUBSTRING(tidut3.UT,utnr3[nrcol3[1]]) = "Traktamentsavtal:"
         SUBSTRING(tidut3.UT,utnr3[nrcol3[2]]) = TRIM(TRAAVTAB.FORKLARING). 
         CREATE tidut3.
         ASSIGN
         SUBSTRING(tidut3.UT,utnr3[nrcol3[1]]) = "Veckoschema:".
         ASSIGN SUBSTRING(tidut3.UT,utnr3[nrcol3[2]]) = STRING(PERSONALTAB.VECKOSCHEMA,">99").    
         CREATE tidut3.
         ASSIGN
         SUBSTRING(tidut3.UT,utnr3[nrcol3[1]]) = "Övertiduttag:".
         IF valperstemp.OVERTIDUTTAG = "K" THEN ASSIGN SUBSTRING(tidut3.UT,utnr3[nrcol3[2]]) = "Komp".
         IF valperstemp.OVERTIDUTTAG = "Ö" THEN ASSIGN SUBSTRING(tidut3.UT,utnr3[nrcol3[2]]) = "Över".
         IF valperstemp.OVERTIDUTTAG = "F" THEN ASSIGN SUBSTRING(tidut3.UT,utnr3[nrcol3[2]]) = "Flex".
         IF valperstemp.OVERTIDUTTAG = "I" THEN ASSIGN SUBSTRING(tidut3.UT,utnr3[nrcol3[2]]) = "Ejöv".
         
         
         CREATE tidut3.
         CREATE tidut3.   
      END.
END PROCEDURE.    
    
  
