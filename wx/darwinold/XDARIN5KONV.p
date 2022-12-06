/*XDARIN5KONV.p INLÄSNING AV MODERDARS störningsrapporter*/       
/*
konertering as störnkg .

1 töm storningstab i darwin databsen (deldad\darwin\db)

2 läs in filerna till storningstab i darvin databasen.
3 dummpa ut enligt nedan i darwin db
4 läs in nya db enligt detta prog.
OUTPUT TO a:\stor.txt 
CONVERT TARGET "iso8859-1" SOURCE "iso8859-1" 
NO-ECHO.
FOR EACH storningstab no-lock:
    PUT UNFORMATTED 
       storningstab.foret-id ";" storningstab.distr-id ";" storningstab.lopnr-id ";"
       storningstab.rapp-typ ";" storningstab.dat-id ";" storningstab.tid-id FORMAT "9999" ";"
       storningstab.C1 ";" storningstab.C2 ";" storningstab.D1 ";" storningstab.D2 ";"
       storningstab.E1 ";" storningstab.E2 ";" storningstab.F1 ";" storningstab.F2 ";"
       storningstab.G1 ";" storningstab.G2 ";" storningstab.H ";" 
       storningstab.J1 ";" storningstab.J2 ";" storningstab.J3 ";"
       storningstab.K1 ";" storningstab.K2 ";" storningstab.L1 ";" storningstab.L2 ";"
       storningstab.M1 ";" storningstab.M2 ";" storningstab.N1 ";" storningstab.N2 ";"
       storningstab.P1 ";" storningstab.P2 ";" storningstab.Q ";" storningstab.R ";"
       storningstab.S ";" storningstab.T1 ";" storningstab.T2 ";" storningstab.T3
    SKIP.
END.
OUTPUT CLOSE.*/
DEFINE NEW SHARED VARIABLE quotervar AS CHARACTER FORMAT "X(256)" NO-UNDO.




DEFINE VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE VARIABLE rad AS INTEGER NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamndat AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER FORMAT "X(20)" NO-UNDO.                
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommandoprog AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE satsvar AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE enrvar AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE melvar AS INTEGER NO-UNDO.
DEFINE VARIABLE melvar2 AS INTEGER NO-UNDO.
DEFINE VARIABLE langd AS INTEGER NO-UNDO.
DEFINE VARIABLE pos1 AS INTEGER NO-UNDO. 
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE nrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE nat1 AS INTEGER NO-UNDO.
DEFINE VARIABLE nat2 AS INTEGER NO-UNDO.
DEFINE VARIABLE nat3 AS INTEGER NO-UNDO.
DEFINE VARIABLE nat4 AS INTEGER NO-UNDO.
DEFINE VARIABLE felvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE stornr AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE tidin
   FIELD FORET-ID           AS INTEGER 
   FIELD DISTR-ID           AS CHARACTER
   FIELD LOPNR-ID           AS INTEGER 
   FIELD RAPP-TYP           AS CHARACTER 
   FIELD DAT-ID             AS DATE FORMAT "9999/99/99"
   FIELD TID-ID             AS INTEGER FORMAT "9999"
   FIELD C1                 AS INTEGER
   FIELD C2                 AS CHARACTER
   FIELD D1                 AS CHARACTER
   FIELD D2                 AS CHARACTER 
   FIELD E1                 AS INTEGER
   FIELD E2                 AS CHARACTER
   FIELD F1                 AS INTEGER
   FIELD F2                 AS CHARACTER
   FIELD G1                 AS INTEGER
   FIELD G2                 AS INTEGER
   FIELD H                  AS INTEGER
   FIELD J1                 AS CHARACTER
   FIELD J2                 AS CHARACTER
   FIELD J3                 AS DECIMAL
   FIELD K1                 AS INTEGER
   FIELD K2                 AS INTEGER
   FIELD L1                 AS INTEGER
   FIELD L2                 AS INTEGER
   FIELD M1                 AS INTEGER
   FIELD M2                 AS INTEGER
   FIELD N1                 AS INTEGER
   FIELD N2                 AS CHARACTER
   FIELD P1                 AS INTEGER
   FIELD P2                 AS INTEGER
   FIELD Q                  AS INTEGER
   FIELD R                  AS INTEGER
   FIELD S                  AS INTEGER
   FIELD T1                 AS INTEGER
   FIELD T2                 AS INTEGER
   FIELD T3                 AS INTEGER
   FIELD FEL                AS CHARACTER
   FIELD FEL2               AS LOGICAL INITIAL FALSE.
   
DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
   
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .
   
DEFINE TEMP-TABLE temp_TEXT
   FIELD PROGNAMN AS CHARACTER FORMAT "X(100)".   

DEFINE BUFFER distbuff FOR STORDISTRIKT.   
{AMERICANEUROPEAN.I}
{muswait.i}         
RUN in_UI.
{musarrow.i}
{EUROPEANAMERICAN.I}
PROCEDURE in_UI: 
   FOR EACH intid:
      DELETE intid.
   END.
   FOR EACH tidin:
      DELETE tidin.
   END. 
   FIND LAST STORNINGSTAB
   USE-INDEX STORNUMMERID NO-LOCK NO-ERROR.   
   IF AVAILABLE  STORDISTRIKT THEN stornr = STORNINGSTAB.STORNUMMERID.
   ELSE stornr = 1.
   filnamn = "a:\stor.txt".
   RUN PROVAG.P.
   ASSIGN
   dlcvar = dlcvar + "QUOTER.EXE"
   wtidvar = wtidvar + "kabpris.q".   
   
   OS-COMMAND SILENT VALUE(dlcvar)
   VALUE(filnamn) > VALUE(wtidvar).   
   INPUT FROM VALUE(wtidvar) NO-ECHO.    
   /*CONVERT TARGET "iso8859-1" SOURCE "ibm850" NO-ECHO.
   iso8859-1 swedish-7-bit ibm850"*/
   REPEAT:
      DO TRANSACTION: 
         SET words VIEW-AS EDITOR INNER-CHARS 78 INNER-LINES 80 WITH FRAME DDD WIDTH 80.   
         CREATE intid.   
         ASSIGN intid.TIN = words.   
      END.
   END.
   INPUT CLOSE.               
   OUTPUT TO VALUE(wtidvar).
   FOR EACH intid:          
      PUT UNFORMATTED intid.TIN SKIP.     
   END.
   OUTPUT CLOSE.
   INPUT FROM VALUE(wtidvar) NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE tidin.
         ASSIGN.
         IMPORT DELIMITER ";" tidin   NO-ERROR.
      END.               
   END.
   RUN skapasats_UI.           
   OS-DELETE VALUE(wtidvar).
END PROCEDURE.

PROCEDURE skapasats_UI:      
   FOR EACH tidin NO-LOCK:      
     FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = tidin.FORET-ID NO-LOCK NO-ERROR.
        IF AVAILABLE AVDELNING THEN DO:
           FIND FIRST STORDISTRIKT WHERE STORDISTRIKT.AVDELNINGNR = tidin.FORET-ID AND
           STORDISTRIKT.VIDISTRIKT = tidin.DISTR-ID AND STORDISTRIKT.ARTAL = YEAR(tidin.DAT-ID)
           NO-LOCK NO-ERROR.
           IF AVAILABLE STORDISTRIKT THEN DO:              
              RUN skapakund_UI.
           END.
           ELSE DO:                                   
              FIND LAST STORDISTRIKT USE-INDEX DISTRIKTID NO-LOCK NO-ERROR.
              IF AVAILABLE  STORDISTRIKT THEN nrvar = STORDISTRIKT.DISTRIKTID + 1.
              ELSE nrvar.
              FIND FIRST STORDISTRIKT WHERE STORDISTRIKT.AVDELNINGNR = tidin.FORET-ID AND
              STORDISTRIKT.VIDISTRIKT = tidin.DISTR-ID AND STORDISTRIKT.ARTAL = 1999
              NO-LOCK NO-ERROR.
              IF AVAILABLE STORDISTRIKT THEN DO:
                 CREATE distbuff.
                 ASSIGN
                 distbuff.AVDELNINGNR = STORDISTRIKT.AVDELNINGNR
                 distbuff.DISTRIKTID = nrvar
                 distbuff.VIDISTRIKT = tidin.DISTR-ID
                 distbuff.NAMN = STORDISTRIKT.NAMN
                 distbuff.ARTAL = YEAR(tidin.DAT-ID).
                 FIND STORDISTRIKT WHERE STORDISTRIKT.DISTRIKTID = nrvar
                 NO-LOCK NO-ERROR. 
                 RUN skapakund_UI.
              END.
              ELSE DO:
                 CREATE STORDISTRIKT.
                 ASSIGN
                 STORDISTRIKT.AVDELNINGNR = tidin.FORET-ID
                 STORDISTRIKT.DISTRIKTID = nrvar
                 STORDISTRIKT.VIDISTRIKT = tidin.DISTR-ID
                 STORDISTRIKT.NAMN = tidin.DISTR-ID
                 STORDISTRIKT.ARTAL = YEAR(tidin.DAT-ID).
                 RUN skapakund_UI.
              END.
           END.   
        END.
        ELSE DO:
           PUT UNFORMATTED tidin.FORET-ID ";" tidin.DISTR-ID ";" tidin.LOPNR-ID ";" tidin.RAPP-TYP 
           ";" tidin.DAT-ID ";" tidin.TID-ID ";" tidin.C1 ";" tidin.C2 ";" tidin.D1 ";" tidin.D2 
           ";" tidin.E1 ";" tidin.E2 ";" tidin.F1 ";" tidin.F2 ";" tidin.G1 ";" tidin.G2
           ";" tidin.H ";" tidin.J1 ";" tidin.J2 ";" tidin.J3 ";" tidin.K1 ";" tidin.K2 
           ";" tidin.L1 ";" tidin.L2 ";" tidin.M1 ";" tidin.M2 ";" tidin.N1 ";" tidin.N2 
           ";" tidin.P1 ";" tidin.P2 ";" tidin.Q ";" tidin.R ";" tidin.S ";" tidin.T1 
           ";" tidin.T2 ";" tidin.T3 ";" tidin.FEL SKIP. 
       END.   
   END.
   OUTPUT TO "a:\FELOVRIGT.txt" 
   APPEND CONVERT TARGET "iso8859-1" SOURCE "iso8859-1" 
   NO-ECHO.
   FOR EACH tidin WHERE tidin.FEL NE " ":
      IF tidin.FEL2 = FALSE THEN DO:
         PUT UNFORMATTED tidin.FORET-ID ";" tidin.DISTR-ID ";" tidin.LOPNR-ID ";" tidin.RAPP-TYP 
         ";" tidin.DAT-ID ";" tidin.TID-ID ";" tidin.C1 ";" tidin.C2 ";" tidin.D1 ";" tidin.D2 
         ";" tidin.E1 ";" tidin.E2 ";" tidin.F1 ";" tidin.F2 ";" tidin.G1 ";" tidin.G2
         ";" tidin.H ";" tidin.J1 ";" tidin.J2 ";" tidin.J3 ";" tidin.K1 ";" tidin.K2 
         ";" tidin.L1 ";" tidin.L2 ";" tidin.M1 ";" tidin.M2 ";" tidin.N1 ";" tidin.N2 
         ";" tidin.P1 ";" tidin.P2 ";" tidin.Q ";" tidin.R ";" tidin.S ";" tidin.T1 
         ";" tidin.T2 ";" tidin.T3 ";" tidin.FEL SKIP.
      END.   
   END.
   OUTPUT CLOSE.    
   OUTPUT TO "a:\FELTID.txt" 
   APPEND CONVERT TARGET "iso8859-1" SOURCE "iso8859-1" 
   NO-ECHO.
   FOR EACH tidin WHERE tidin.FEL2 = TRUE:
      PUT UNFORMATTED tidin.FORET-ID ";" tidin.DISTR-ID ";" tidin.LOPNR-ID ";" tidin.RAPP-TYP 
      ";" tidin.DAT-ID ";" tidin.TID-ID ";" tidin.C1 ";" tidin.C2 ";" tidin.D1 ";" tidin.D2 
      ";" tidin.E1 ";" tidin.E2 ";" tidin.F1 ";" tidin.F2 ";" tidin.G1 ";" tidin.G2
      ";" tidin.H ";" tidin.J1 ";" tidin.J2 ";" tidin.J3 ";" tidin.K1 ";" tidin.K2 
      ";" tidin.L1 ";" tidin.L2 ";" tidin.M1 ";" tidin.M2 ";" tidin.N1 ";" tidin.N2 
      ";" tidin.P1 ";" tidin.P2 ";" tidin.Q ";" tidin.R ";" tidin.S ";" tidin.T1 
      ";" tidin.T2 ";" tidin.T3 ";" tidin.FEL SKIP.
   END.  
   OUTPUT CLOSE.
END PROCEDURE.

PROCEDURE skapakund_UI: 
   ASSIGN
   felvar = FALSE
   tidin.FEL2 = FALSE
   tidin.FEL = " ".
   
   IF tidin.C1 = 1 OR tidin.C1 = 2 THEN felvar = felvar.
   ELSE DO:
      tidin.FEL = "C1 ".
      felvar = TRUE.
   END.   
      
   IF tidin.D1 NE "" THEN DO:  
      FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "D" AND INLASTAB.INKODTYP = "1" AND
      INLASTAB.INKODPOSCH = tidin.D1 USE-INDEX INKOD
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE INLASTAB THEN DO:
         tidin.FEL = tidin.FEL + "D1 ".
         felvar = TRUE.   
      END.
   END.
   ELSE DO:
      IF tidin.D2 NE "" THEN DO:
         FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "D" AND INLASTAB.INKODTYP = "1" AND
         INLASTAB.INKODPOSCH = tidin.D2 USE-INDEX INKOD
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE INLASTAB THEN DO:
            tidin.FEL = tidin.FEL + "D1 ".
            felvar = TRUE.   
         END.
      END.
      ELSE DO:
         tidin.FEL = tidin.FEL + "D1 ".
         felvar = TRUE.   
      END.   
   END.   
   
   IF tidin.D2 NE "" THEN DO:     
      FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "D" AND INLASTAB.INKODTYP = "2" AND
      INLASTAB.INKODPOSCH = tidin.D2 USE-INDEX INKOD
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE INLASTAB THEN DO:
         tidin.FEL = tidin.FEL + "D2 ".
         felvar = TRUE.   
      END.
   END.
   ELSE DO:
      IF tidin.C1 = 1 THEN DO:
         IF tidin.D1 NE "" THEN DO:
            FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "D" AND INLASTAB.INKODTYP = "2" AND
            INLASTAB.INKODPOSCH = tidin.D1 USE-INDEX INKOD
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE INLASTAB THEN DO:
               tidin.FEL = tidin.FEL + "D2 ".
               felvar = TRUE.   
            END.
         END.
         ELSE DO:
            tidin.FEL = tidin.FEL + "D2 ".
            felvar = TRUE.
         END.   
      END.
   END.  
   
   IF tidin.DAT-ID = ? THEN DO:
      tidin.FEL = tidin.FEL + "DAT-ID ".
      felvar = TRUE.
   END.   
   
   IF tidin.TID-ID = ? THEN DO:
      tidin.FEL = tidin.FEL + "TID-ID ".
      felvar = TRUE.
   END.   
   
   IF SUBSTRING(STRING(tidin.TID-ID,"9999"),1,2) > "24" THEN DO:
      ASSIGN
      tidin.FEL2 = TRUE
      tidin.FEL = tidin.FEL + "TID-ID "
      felvar = TRUE.
   END.   
   
   IF SUBSTRING(STRING(tidin.TID-ID,"9999"),3,2) > "59" THEN DO:
      ASSIGN
      tidin.FEL2 = TRUE
      tidin.FEL = tidin.FEL + "TID-ID "
      felvar = TRUE.
   END.   
   

   IF tidin.E1 >= 0 AND tidin.E1 <= 5 THEN DO:      
      felvar = felvar.
   END.
   ELSE DO:
      tidin.FEL = tidin.FEL + "E1 ".
      felvar = TRUE.
   END.
   
   IF tidin.C1 = 1 THEN DO:
      IF tidin.F1 >= 0 AND tidin.F1 <= 4 THEN DO:
         felvar = felvar.
      END.
      ELSE DO:
         tidin.FEL = tidin.FEL + "F1 ".
         felvar = TRUE.
      END.
   END.
   ELSE DO:
      tidin.F1 = 0.
   END.      
   
   IF tidin.C1 = 1 THEN DO:
      IF tidin.G1 >= 0 AND tidin.G1 <= 2 THEN DO:           
         felvar = felvar.
      END.
      ELSE DO:
         tidin.FEL = tidin.FEL + "G1 ".
         felvar = TRUE.
      END.
   END.
   ELSE DO:
      tidin.G1 = 0.
   END.   
   
   IF tidin.C1 = 1 THEN DO:
      IF tidin.G2 >= 0 AND tidin.G2 <= 6 THEN DO:
         felvar = felvar.
      END.
      ELSE DO:
         tidin.FEL = tidin.FEL + "G2 ".
         felvar = TRUE.
      END.
   END.
   ELSE DO:
      tidin.G2 = 0.
   END.      
   
   IF tidin.J1 NE "" THEN DO:
      IF SUBSTRING(STRING(tidin.J1,"999999"),1,2) >= "01" AND
      SUBSTRING(STRING(tidin.J1,"999999"),1,2) <= "31" THEN DO:
         IF SUBSTRING(STRING(tidin.J1,"999999"),3,2) > "24" THEN DO:
            ASSIGN
            tidin.FEL2 = TRUE
            tidin.FEL = tidin.FEL + "J1 "
            felvar = TRUE.
         END.
         ELSE DO:
            IF SUBSTRING(STRING(tidin.J1,"999999"),5,2) > "59" THEN DO:
               ASSIGN
               tidin.FEL2 = TRUE
               tidin.FEL = tidin.FEL + "J1 "
               felvar = TRUE.
            END.
            ELSE DO:
               IF SUBSTRING(STRING(tidin.J1,"999999"),1,2) = SUBSTRING(STRING(tidin.DAT-ID,"999999"),5,2) THEN DO:
                  IF SUBSTRING(STRING(tidin.J1,"999999"),3,4) < SUBSTRING(STRING(tidin.TID-ID,"9999"),1,4) THEN DO:
                     ASSIGN
                     tidin.FEL2 = TRUE
                     tidin.FEL = tidin.FEL + "J1 "
                     felvar = TRUE.
                  END.
               END.
               ELSE IF SUBSTRING(STRING(tidin.J1,"999999"),1,2) < SUBSTRING(STRING(tidin.DAT-ID,"999999"),5,2) THEN DO:
                   ASSIGN
                   tidin.FEL2 = TRUE
                   tidin.FEL = tidin.FEL + "J1 "
                   felvar = TRUE.
               END.
               ELSE DO:
                  felvar = felvar.
               END.
            END.
         END.
      END.
      ELSE DO:
         ASSIGN
         tidin.FEL2 = TRUE
         tidin.FEL = tidin.FEL + "J1 "
         felvar = TRUE.
      END.
   END.   
   
   
   IF SUBSTRING(STRING(tidin.J2,"999999"),1,2) >= "01" AND
   SUBSTRING(STRING(tidin.J2,"999999"),1,2) <= "31" THEN DO:
      IF SUBSTRING(STRING(tidin.J2,"999999"),3,2) > "24" THEN DO:
         ASSIGN
         tidin.FEL2 = TRUE
         tidin.FEL = tidin.FEL + "J2 "
         felvar = TRUE.
      END.
      ELSE DO:
         IF SUBSTRING(STRING(tidin.J2,"999999"),5,2) > "59" THEN DO:
            ASSIGN
            tidin.FEL2 = TRUE
            tidin.FEL = tidin.FEL + "J2 "
            felvar = TRUE.
         END.
         ELSE DO:
            IF SUBSTRING(STRING(tidin.J2,"999999"),1,2) = SUBSTRING(STRING(tidin.DAT-ID,"999999"),5,2) THEN DO:
               IF SUBSTRING(STRING(tidin.J2,"999999"),3,4) < SUBSTRING(STRING(tidin.TID-ID,"9999"),1,4) THEN DO:
                  ASSIGN
                  tidin.FEL2 = TRUE
                  tidin.FEL = tidin.FEL + "J2 "
                  felvar = TRUE.
               END.
            END.
            ELSE IF SUBSTRING(STRING(tidin.J2,"999999"),1,2) < SUBSTRING(STRING(tidin.DAT-ID,"999999"),5,2) THEN DO:
                ASSIGN
                tidin.FEL2 = TRUE
                tidin.FEL = tidin.FEL + "J2 "
                felvar = TRUE.
            END.
            ELSE DO:
               IF tidin.J2 >= tidin.J1 THEN DO:
                  felvar = felvar.
               END.
               ELSE DO:
                  ASSIGN
                  tidin.FEL2 = TRUE
                  tidin.FEL = tidin.FEL + "J2 "
                  felvar = TRUE.
               END.   
            END.
         END.
      END.
   END.
   ELSE DO:
      ASSIGN
      tidin.FEL2 = TRUE
      tidin.FEL = tidin.FEL + "J2 "
      felvar = TRUE.
   END.
   
   IF tidin.N1 >= 1 AND tidin.N1 <= 5 THEN DO:      
      felvar = felvar.
   END.
   ELSE DO:
      IF tidin.N1 = 0 THEN DO:      
         felvar = felvar.
      END.
      ELSE DO:
         tidin.FEL = tidin.FEL + "N1 ".
         felvar = TRUE.
      END.   
   END.
   IF tidin.P1 >= 1 AND tidin.P1 <= 3 THEN DO:      
      felvar = felvar.
   END.
   ELSE DO:
      IF tidin.P1 = 0 THEN DO:      
         felvar = felvar.
      END.
      ELSE DO:
         tidin.FEL = tidin.FEL + "P1 ".
         felvar = TRUE.
      END.   
   END.
   IF tidin.P2 >= 1 AND tidin.P2 <= 3 THEN DO:      
      felvar = felvar.
   END.
   ELSE DO:
      IF tidin.P2 = 0 THEN DO:      
         felvar = felvar.
      END.
      ELSE DO:
         tidin.FEL = tidin.FEL + "P2 ".
         felvar = TRUE.
      END.   
   END.
   IF tidin.Q >= 1 AND tidin.Q <= 5 THEN DO:      
      felvar = felvar.
   END.
   ELSE DO:
      IF tidin.Q = 0 THEN DO:      
         felvar = felvar.
      END.
      ELSE DO:
         tidin.FEL = tidin.FEL + "Q ".
         felvar = TRUE.
      END.   
   END.
   IF tidin.R NE 0 THEN DO:      
      FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "R" AND
      INLASTAB.INKODPOSCH = STRING(tidin.R) USE-INDEX INKOD
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE INLASTAB THEN DO:
         tidin.FEL = tidin.FEL + "R ".
         felvar = TRUE.   
      END.
   END.
   ELSE DO:
      tidin.FEL = tidin.FEL + "R ".
      felvar = TRUE.
   END.
   IF tidin.S NE 0 THEN DO:      
      IF tidin.C1 = 2 THEN DO:
         tidin.S = 0.
      END.
      ELSE DO:
         FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "S" AND
         INLASTAB.INKODPOSCH = STRING(tidin.S) USE-INDEX INKOD
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE INLASTAB THEN DO:
            tidin.S = 93.   
         END.
      END.   
   END.
   ELSE DO:
      IF tidin.C1 = 1 THEN DO:
         tidin.S = 93.
      END.   
   END.
   IF tidin.T1 >= 1 AND tidin.T1 <= 2 THEN DO:      
      felvar = felvar.
   END.
   ELSE DO:
      IF tidin.T1 = 0 THEN DO:      
         felvar = felvar.
      END.
      ELSE DO:
         tidin.FEL = tidin.FEL + "T1 ".
         felvar = TRUE.
      END.   
   END.   
   IF felvar = FALSE THEN DO:
      RUN skapa_UI.
   END.
END PROCEDURE.   

PROCEDURE skapa_UI: 
   stornr = stornr + 1.
   FIND FIRST STORDISTRIKT WHERE STORDISTRIKT.AVDELNINGNR = tidin.FORET-ID AND
   STORDISTRIKT.VIDISTRIKT = tidin.DISTR-ID AND STORDISTRIKT.ARTAL = YEAR(tidin.DAT-ID)
   NO-LOCK NO-ERROR.

   CREATE STORNINGSTAB.
   ASSIGN
   STORNINGSTAB.DISTRIKTID = STORDISTRIKT.DISTRIKTID
   STORNINGSTAB.STORNUMMERID = stornr
   STORNINGSTAB.VSTORNUMMER = stornr
   STORNINGSTAB.INDATUM = TODAY
   STORNINGSTAB.INKLOCKAN = TIME
   STORNINGSTAB.HDATUM = tidin.DAT-ID
   STORNINGSTAB.HKLOCKAN = tidin.TID-ID / 100
   STORNINGSTAB.STORTYPID = tidin.C1
   STORNINGSTAB.MERJOBB = FALSE.
   
   FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "D" AND INLASTAB.INKODTYP = "1" AND
   INLASTAB.INKODPOSCH = tidin.D1 USE-INDEX INKOD
   NO-LOCK NO-ERROR.
   FIND FIRST SPANNINGSNIV WHERE SPANNINGSNIV.INKODID = INLASTAB.INKODID
   NO-LOCK NO-ERROR.
   STORNINGSTAB.FRANSPANID = SPANNINGSNIV.SPANID.
   
   IF tidin.D2 NE " " THEN DO:
      FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "D" AND INLASTAB.INKODTYP = "2" AND
      INLASTAB.INKODPOSCH = tidin.D2 USE-INDEX INKOD
      NO-LOCK NO-ERROR.
      IF AVAILABLE INLASTAB THEN DO:
         FIND FIRST SPANNINGSNIV WHERE SPANNINGSNIV.INKODID = INLASTAB.INKODID
         NO-LOCK NO-ERROR.
         STORNINGSTAB.FELSPANID = SPANNINGSNIV.SPANID.
      END.   
   END.   
   
   FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "E" AND INLASTAB.INKODTYP = "1" AND
   INLASTAB.INKODPOSCH = STRING(tidin.E1) USE-INDEX INKOD
   NO-LOCK NO-ERROR.
   FIND FIRST BRYTORGAN WHERE BRYTORGAN.INKODID = INLASTAB.INKODID
   NO-LOCK NO-ERROR.
   STORNINGSTAB.BRYTOID = BRYTORGAN.BRYTOID.
   STORNINGSTAB.BRYTORGLIT = tidin.E2.
   
   FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "F" AND INLASTAB.INKODTYP = "1" AND
   INLASTAB.INKODPOSCH = STRING(tidin.F1) USE-INDEX INKOD
   NO-LOCK NO-ERROR.
   IF AVAILABLE INLASTAB THEN DO:
      FIND FIRST SEKTIONERING WHERE SEKTIONERING.INKODID = INLASTAB.INKODID
      NO-LOCK NO-ERROR.
      STORNINGSTAB.SEKTIONERID = SEKTIONERING.SEKTIONERID.
      STORNINGSTAB.SEKTIONLIT = tidin.F2.
   END.   
   
   FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "G" AND INLASTAB.INKODTYP = "1" AND
   INLASTAB.INKODPOSCH = STRING(tidin.G1) USE-INDEX INKOD
   NO-LOCK NO-ERROR.
   IF AVAILABLE INLASTAB THEN DO:
      FIND FIRST UTLOSNINGSKYDD WHERE UTLOSNINGSKYDD.INKODID = INLASTAB.INKODID
      NO-LOCK NO-ERROR.
      STORNINGSTAB.UTLOSID = UTLOSNINGSKYDD.UTLOSID.
   END.   
   
   FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "G" AND INLASTAB.INKODTYP = "2" AND
   INLASTAB.INKODPOSCH = STRING(tidin.G2) USE-INDEX INKOD
   NO-LOCK NO-ERROR.
   IF AVAILABLE INLASTAB THEN DO:
      FIND FIRST RELAINDIKERING WHERE RELAINDIKERING.INKODID = INLASTAB.INKODID
      NO-LOCK NO-ERROR.
      ASSIGN
      STORNINGSTAB.RELINID = RELAINDIKERING.RELINID.
   END.   
   
   ASSIGN
   STORNINGSTAB.BORTMW = INTEGER(SUBSTRING(STRING(tidin.H,"999999"),1,3))
   STORNINGSTAB.BORTKW = INTEGER(SUBSTRING(STRING(tidin.H,"999999"),4,3)).
   
   IF tidin.J1 = "" THEN DO:
      ASSIGN
      STORNINGSTAB.DATUM70% = ?
      STORNINGSTAB.KLOCKAN70% = ?.
      STORNINGSTAB.DATUM100% = DATE(MONTH(tidin.DAT-ID),INTEGER(SUBSTRING(STRING(tidin.J2,"999999"),1,2)),YEAR(tidin.DAT-ID)).
      STORNINGSTAB.KLOCKAN100% = INTEGER(SUBSTRING(STRING(tidin.J2,"999999"),3,4)) / 100.
   END.
   ELSE DO:
      IF tidin.J1 = tidin.J2 THEN DO:
         ASSIGN
         STORNINGSTAB.DATUM70% = ?
         STORNINGSTAB.KLOCKAN70% = ?.
         STORNINGSTAB.DATUM100% = DATE(MONTH(tidin.DAT-ID),INTEGER(SUBSTRING(STRING(tidin.J2,"999999"),1,2)),YEAR(tidin.DAT-ID)).
         STORNINGSTAB.KLOCKAN100% = INTEGER(SUBSTRING(STRING(tidin.J2,"999999"),3,4)) / 100.
      END.
      ELSE DO:
         STORNINGSTAB.DATUM70% = DATE(MONTH(tidin.DAT-ID),INTEGER(SUBSTRING(STRING(tidin.J1,"999999"),1,2)),YEAR(tidin.DAT-ID)).
         STORNINGSTAB.KLOCKAN70% = INTEGER(SUBSTRING(STRING(tidin.J1,"999999"),3,4)) / 100.
         STORNINGSTAB.DATUM100% = DATE(MONTH(tidin.DAT-ID),INTEGER(SUBSTRING(STRING(tidin.J2,"999999"),1,2)),YEAR(tidin.DAT-ID)).
         STORNINGSTAB.KLOCKAN100% = INTEGER(SUBSTRING(STRING(tidin.J2,"999999"),3,4)) / 100.
      END.      
   END.   
   ASSIGN
   STORNINGSTAB.AVBROTTSTID = tidin.J3
   STORNINGSTAB.ANTALHSP = tidin.K1
   STORNINGSTAB.ANTALLSP = tidin.K2
   STORNINGSTAB.ANTALREGSTN = tidin.L1
   STORNINGSTAB.ANTALNATSTN = tidin.L2
   STORNINGSTAB.EJBORTKUND = tidin.M1
   STORNINGSTAB.EJBORTMW = INTEGER(SUBSTRING(STRING(tidin.M2,"99999"),1,2))
   STORNINGSTAB.EJBORTKW = INTEGER(SUBSTRING(STRING(tidin.M2,"99999"),3,3)).
   
   
   FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "N" AND INLASTAB.INKODTYP = "1" AND
   INLASTAB.INKODPOSCH = STRING(tidin.N1) USE-INDEX INKOD
   NO-LOCK NO-ERROR.
   IF AVAILABLE INLASTAB THEN DO:
      FIND FIRST STORDRIFTOMR WHERE STORDRIFTOMR.INKODID = INLASTAB.INKODID
      NO-LOCK NO-ERROR.
      STORNINGSTAB.STDRIFTID = STORDRIFTOMR.STDRIFTID.
      STORNINGSTAB.STDRIFTLIT = tidin.N2.
   END.   
   
   FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "P" AND INLASTAB.INKODTYP = "1" AND
   INLASTAB.INKODPOSCH = STRING(tidin.P1) USE-INDEX INKOD
   NO-LOCK NO-ERROR.
   IF AVAILABLE INLASTAB THEN DO:
      FIND FIRST NATSTRUKTUR WHERE NATSTRUKTUR.INKODID = INLASTAB.INKODID
      NO-LOCK NO-ERROR.
      STORNINGSTAB.STRUKIDP1 = NATSTRUKTUR.STRUKID.
   END.   
   
   FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "P" AND INLASTAB.INKODTYP = "2" AND
   INLASTAB.INKODPOSCH = STRING(tidin.P2) USE-INDEX INKOD
   NO-LOCK NO-ERROR.
   IF AVAILABLE INLASTAB THEN DO:
      FIND FIRST NATSTRUKTUR WHERE NATSTRUKTUR.INKODID = INLASTAB.INKODID
      NO-LOCK NO-ERROR.
      STORNINGSTAB.STRUKIDP2 = NATSTRUKTUR.STRUKID.
   END.   

   FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "Q" AND INLASTAB.INKODTYP = "1" AND
   INLASTAB.INKODPOSCH = STRING(tidin.Q) USE-INDEX INKOD
   NO-LOCK NO-ERROR.
   IF AVAILABLE INLASTAB THEN DO:
      FIND FIRST NATTYP WHERE NATTYP.INKODID = INLASTAB.INKODID
      NO-LOCK NO-ERROR.
      STORNINGSTAB.NATTYPID = NATTYP.NATTYPID.
   END.   

   FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "R" AND
   INLASTAB.INKODPOSCH = STRING(tidin.R) USE-INDEX INKOD
   NO-LOCK NO-ERROR.
   FIND FIRST ANLAGGNINGSDEL WHERE ANLAGGNINGSDEL.INKODID = INLASTAB.INKODID
   NO-LOCK NO-ERROR.
   STORNINGSTAB.ADELID = ANLAGGNINGSDEL.ADELID.
   
   FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "S" AND
   INLASTAB.INKODPOSCH = STRING(tidin.S) USE-INDEX INKOD
   NO-LOCK NO-ERROR.
   IF AVAILABLE INLASTAB THEN DO:
      FIND FIRST FELORSAK WHERE FELORSAK.INKODID = INLASTAB.INKODID
      NO-LOCK NO-ERROR.
      STORNINGSTAB.FELOID = FELORSAK.FELOID.
   END.   
   
   FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "T" AND INLASTAB.INKODTYP = "1" AND
   INLASTAB.INKODPOSCH = STRING(tidin.T1) USE-INDEX INKOD
   NO-LOCK NO-ERROR.
   IF AVAILABLE INLASTAB THEN DO:
      FIND FIRST RESERVKRAFTMETOD WHERE RESERVKRAFTMETOD.INKODID = INLASTAB.INKODID
      NO-LOCK NO-ERROR.
      STORNINGSTAB.RESERVKID = RESERVKRAFTMETOD.RESERVKID. 
   END.   
   ASSIGN
   STORNINGSTAB.ANTALRESERVKRAFT = tidin.T2
   STORNINGSTAB.TIDRESERVKRAFT = tidin.T3.
      
END PROCEDURE.
