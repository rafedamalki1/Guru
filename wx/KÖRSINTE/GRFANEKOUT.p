/*GRFANEKOUT.P*/


DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO. 
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO. 
DEFINE VARIABLE ekrid AS RECID EXTENT 50 NO-UNDO.
DEFINE VARIABLE aoomrade LIKE AONRTAB.OMRADE NO-UNDO.   
DEFINE VARIABLE org LIKE AONRTAB.OMRADE NO-UNDO.         
DEFINE VARIABLE timtid AS DECIMAL FORMAT "99.99" NO-UNDO. 
DEFINE VARIABLE multi AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE berkostnad AS DECIMAL FORMAT "99.99" NO-UNDO.

DEFINE VARIABLE intpristim AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE reskod LIKE TIDREGITAB.LONTILLAGG NO-UNDO. 
DEFINE VARIABLE resantal LIKE TIDREGITAB.LONTILLANTAL NO-UNDO.
DEFINE VARIABLE resbelopp LIKE EKRAPPRESULT.ELONBELOPP NO-UNDO. 
DEFINE VARIABLE ovkod LIKE TIDREGITAB.OVERTIDTILL NO-UNDO. 
DEFINE VARIABLE ovantal LIKE TIDREGITAB.OVERANTAL NO-UNDO.
DEFINE VARIABLE ovbelopp LIKE EKRAPPRESULT.EOVERBELOPP NO-UNDO. 
DEFINE VARIABLE trakod LIKE TIDREGITAB.TRAKTKOD NO-UNDO. 
DEFINE VARIABLE traantal LIKE TIDREGITAB.TRAKTANTAL NO-UNDO.
DEFINE VARIABLE trabelopp LIKE EKRAPPRESULT.ETRAKTBELOPP NO-UNDO. 
DEFINE VARIABLE lonkod LIKE TIDREGITAB.LONTILLAGG NO-UNDO. 
DEFINE VARIABLE lonantal LIKE TIDREGITAB.LONTILLANTAL NO-UNDO.
DEFINE VARIABLE lonbelopp LIKE EKRAPPRESULT.ELONBELOPP NO-UNDO. 
DEFINE VARIABLE bbantal LIKE TIDREGITAB.BERANTAL NO-UNDO.
DEFINE VARIABLE berbelopp LIKE EKRAPPRESULT.EBERBELOPP NO-UNDO.
DEFINE VARIABLE timantal LIKE EKRAPPRESULT.EANTAL NO-UNDO.
DEFINE VARIABLE timbelopp LIKE EKRAPPRESULT.EBELOPP NO-UNDO.
DEFINE VARIABLE kontokod LIKE EKRAPPRESULT.EKOSTNADSSLAG NO-UNDO. 
DEFINE VARIABLE kontokode LIKE EKRAPPRESULT.EKOSTNADSSLAG NO-UNDO. 
DEFINE VARIABLE pkoder LIKE PERSONALTAB.PERSONALKOD NO-UNDO.  
DEFINE VARIABLE fk1 LIKE AONRKONTKO.K1 NO-UNDO. 
DEFINE VARIABLE fk2 LIKE AONRKONTKO.K2 NO-UNDO.
DEFINE VARIABLE fk3 LIKE AONRKONTKO.K3 NO-UNDO.
DEFINE VARIABLE fk4 LIKE AONRKONTKO.K4 NO-UNDO. 
DEFINE VARIABLE fk4i AS INTEGER format "999" NO-UNDO.
DEFINE VARIABLE fk5 LIKE AONRKONTKO.K5 NO-UNDO.  
DEFINE VARIABLE kodanst LIKE ANSTFORMTAB.KOD NO-UNDO.
DEFINE TEMP-TABLE sumkont
   FIELD AONR LIKE AONRKONTKOD.AONR 
   FIELD DELNR LIKE AONRKONTKOD.DELNR
   FIELD DEBKRED LIKE EKRAPPRESULT.EDEBKRED
   FIELD FELDEBKRED LIKE EKRAPPRESULT.EDEBKRED
   FIELD ORG LIKE EKRAPPRESULT.EORG 
   FIELD RGL LIKE EKRAPPRESULT.ERGL            /*ANVÄNDS FÖR KONTSTRÄNG*/
   FIELD TD LIKE EKRAPPRESULT.ETD   
   FIELD KOSTNADSSLAG LIKE EKRAPPRESULT.EKOSTNADSSLAG 
   FIELD TRANSDATUM LIKE EKRAPPRESULT.ETRANSDATUM  
   FIELD BELOPP AS DECIMAL
   FIELD ANTAL AS DECIMAL
   INDEX AONR IS PRIMARY DEBKRED ORG TRANSDATUM KOSTNADSSLAG AONR.

DEFINE TEMP-TABLE ekoforst
   FIELD ENY LIKE EKRAPPRESULT.ENY 
   FIELD EPERSONALKOD LIKE EKRAPPRESULT.EPERSONALKOD 
   FIELD EPROJEKT LIKE EKRAPPRESULT.EPROJEKT 
   FIELD EORG LIKE EKRAPPRESULT.EORG  
   FIELD EGEO LIKE EKRAPPRESULT.EGEO  
   FIELD FELDEBKRED AS LOGICAL
   FIELD ETRANSDATUM LIKE EKRAPPRESULT.ETRANSDATUM
   FIELD EOVERJA LIKE EKRAPPRESULT.EOVERJA
   FIELD ERESULTENH LIKE EKRAPPRESULT.ERESULTENH 
   FIELD EBELOPP LIKE  EKRAPPRESULT.EBELOPP 
   FIELD EANTAL  LIKE EKRAPPRESULT.EANTAL 	       
   FIELD ELONTILLAGG LIKE EKRAPPRESULT.ELONTILLAGG 
   FIELD ELONTILLANTAL LIKE EKRAPPRESULT.ELONTILLANTAL    
   FIELD ELONBELOPP LIKE EKRAPPRESULT.ELONBELOPP         
   INDEX PERSORG IS PRIMARY EPERSONALKOD EORG EGEO EPROJEKT ASCENDING.
DEFINE TEMP-TABLE eko
   FIELD EDEBKRED LIKE EKRAPPRESULT.EDEBKRED
   FIELD ENY LIKE EKRAPPRESULT.ENY       
   FIELD ETRANSDATUM LIKE EKRAPPRESULT.ETRANSDATUM  
   FIELD EPROJEKT LIKE EKRAPPRESULT.EPROJEKT 
   FIELD EORG LIKE EKRAPPRESULT.EORG 
/*   FIELD EGEO LIKE EKRAPPRESULT.EGEO       */    
   FIELD ERGL LIKE EKRAPPRESULT.ERGL            /*ANVÄNDS FÖR KONTSTRÄNG*/
   FIELD ETD LIKE EKRAPPRESULT.ETD   
   FIELD EKOSTNADSSLAG LIKE EKRAPPRESULT.EKOSTNADSSLAG    
   FIELD EBELOPP LIKE  EKRAPPRESULT.EBELOPP 
   FIELD EANTAL  LIKE EKRAPPRESULT.EANTAL 	       
   FIELD ELONTILLAGG LIKE EKRAPPRESULT.ELONTILLAGG 
   FIELD ELONTILLANTAL LIKE EKRAPPRESULT.ELONTILLANTAL    
   FIELD ELONBELOPP LIKE EKRAPPRESULT.ELONBELOPP         
   FIELD FELDEBKRED AS LOGICAL
   INDEX ORG IS PRIMARY ETRANSDATUM EORG EPROJEKT EKOSTNADSSLAG ASCENDING.   
DEFINE BUFFER eko2 FOR eko.
DEFINE INPUT PARAMETER vkdatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR ekoforst.
DEFINE VARIABLE filut AS CHARACTER NO-UNDO.
DEFINE VARIABLE filutkopia AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnk AS CHARACTER NO-UNDO.

DEFINE VARIABLE str AS CHARACTER FORMAT "X(140)" NO-UNDO.
DEFINE VARIABLE str2 AS CHARACTER FORMAT "X(140)" NO-UNDO.
DEFINE VARIABLE prognamnIBTI AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnkIBTI AS CHARACTER NO-UNDO.
DEFINE VARIABLE breddantalIBT AS INTEGER NO-UNDO.
DEFINE VARIABLE utnrIBT AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE breddIBT AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE nrcolIBT AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
{AMERICANEUROPEAN.I}
FIND FIRST FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
ASSIGN
nrcolIBT[1] = 1
nrcolIBT[2] = 2
nrcolIBT[3] = 3
nrcolIBT[4] = 4
nrcolIBT[5] = 5
nrcolIBT[6] = 6
nrcolIBT[7] = 7
nrcolIBT[8] = 8
nrcolIBT[9] = 9
nrcolIBT[10] = 10
nrcolIBT[11] = 11
nrcolIBT[12] = 12
nrcolIBT[13] = 13
nrcolIBT[14] = 14
nrcolIBT[15] = 15
nrcolIBT[16] = 16
nrcolIBT[17] = 17
nrcolIBT[18] = 18
nrcolIBT[19] = 19
nrcolIBT[20] = 20
nrcolIBT[21] = 21
nrcolIBT[22] = 22
nrcolIBT[23] = 23
nrcolIBT[24] = 24.
ASSIGN
/*ny kolumn*/
breddantalIBT = 24   /*antal kolumner*/
breddIBT[1] =  1
breddIBT[2] =  5
breddIBT[3] =  3
breddIBT[4] =  4
breddIBT[5] =  6
breddIBT[6] =  1
breddIBT[7] =  1
breddIBT[8] =  1
breddIBT[9] =  1
breddIBT[10] = 3
breddIBT[11] = 7
breddIBT[12] = 4
breddIBT[13] = 8
breddIBT[14] = 13
breddIBT[15] = 17
breddIBT[16] = 17
breddIBT[17] = 17
breddIBT[18] = 4
breddIBT[19] = 30
breddIBT[20] = 4
breddIBT[21] = 6
breddIBT[22] = 4
breddIBT[23] = 1
breddIBT[24] = 9.
ASSIGN
i = 2.     
utnrIBT[nrcolIBT[1]] = 1.
DO WHILE i <= breddantalIBT:             
   utnrIBT[i] = utnrIBT[i - 1] + breddIBT[i - 1].            
   i = i + 1.
END.
IF Guru.Konstanter:globforetag = "ELPA" THEN DO: 
   ASSIGN
   prognamn = "\\pc112\DELAD\PRO9\guru\EXPORT\"
   prognamnk = "\\pc112\DELAD\PRO9\guru\EXPORT\EXKOPIA\"                                                     
   prognamnIBTI = "SROIBTRV.txt".
   prognamnkIBTI = "SROIBTRV".
END.
IF Guru.Konstanter:globforetag = "GRAN" THEN DO: 
   ASSIGN     
   prognamn = "\\GRANGURU\guru_ser\server\PRO9s\EXPORT\" 
   prognamnk = "\\GRANGURU\guru_ser\server\PRO9s\EXPORT\EXKOPIA\"
   prognamnIBTI = "SROIBTRV.txt".     
   prognamnkIBTI = "SROIBTRV".
END.   

pkoder = "". 
FIND FIRST ekoforst WHERE ekoforst.ENY = FALSE USE-INDEX PERSORG NO-LOCK NO-ERROR.
IF NOT AVAILABLE ekoforst THEN RETURN.        
ekrid[1] = RECID(ekoforst).
REPEAT:
   IF pkoder NE ekoforst.EPERSONALKOD THEN DO:      
      pkoder = ekoforst.EPERSONALKOD.      
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = pkoder 
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.        
      org = PERSONALTAB.OMRADE. 
      persrec = RECID(PERSONALTAB).
      FIND FIRST ANSTFORMTAB WHERE
      ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
      USE-INDEX ANSTF NO-LOCK NO-ERROR.
      kodanst = ANSTFORMTAB.KOD.
   END.      
   FIND FIRST AONRTAB WHERE AONRTAB.AONR = ekoforst.EPROJEKT AND
   AONRTAB.DELNR = 000 USE-INDEX AONR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE AONRTAB THEN DO:
      aoomrade = org. 
      IF ekoforst.EPROJEKT = "" THEN aoomrade = org. 
      ELSE DO:
         MESSAGE CAPS(Guru.Konstanter:gaol) ekoforst.EPROJEKT "000 FINNS EJ! KONTAKTA ELPOOL"
         VIEW-AS ALERT-BOX.
      END.
   END.                                           
   ASSIGN                    
   fk1 = "" 
   fk2 = "".         
   FIND FIRST AONRKONTKOD WHERE AONRKONTKOD.AONR = ekoforst.EPROJEKT AND 
   AONRKONTKOD.DELNR = 000 USE-INDEX AONRKONT NO-LOCK NO-ERROR. 
   IF AVAILABLE AONRKONTKOD THEN DO:
      ASSIGN
      fk1 = AONRKONTKOD.K1 
      fk2 = AONRKONTKOD.K2.                
   END.             
   /*TIMMAR OCH PENNGAR*/
   IF ekoforst.EBELOPP = 0 THEN DO:
      persrec = persrec.
   END.                                   
   ELSE DO:
      IF kodanst BEGINS 'T' THEN DO:
         kontokod = "997" + SUBSTRING(ekoforst.EORG,1,2).
      END.       
      ELSE DO:
         kontokod = "995" + SUBSTRING(ekoforst.EORG,1,2).
      END.      
      RUN tim_UI.      
   END.         
   DO TRANSACTION: 		    
      FIND ekoforst WHERE RECID(ekoforst) = ekrid[1]
      NO-LOCK NO-ERROR. 
      DELETE ekoforst.
   END.
   FIND FIRST ekoforst WHERE ekoforst.ENY = FALSE USE-INDEX PERSORG NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ekoforst THEN LEAVE.           
   ekrid[1] = RECID(ekoforst).
END.   
/*CCCC*/
FOR EACH eko:
   FIND FIRST sumkont WHERE 
   sumkont.DEBKRED = eko.EDEBKRED AND 
   sumkont.FELDEBKRED = eko.FELDEBKRED AND
   sumkont.ORG = eko.EORG AND 
   sumkont.TRANSDATUM = eko.ETRANSDATUM AND
   sumkont.AONR = eko.EPROJEKT AND
   sumkont.KOSTNADSSLAG = eko.EKOSTNADSSLAG AND
   sumkont.RGL = eko.ERGL AND
   sumkont.TD = eko.ETD   
   NO-ERROR.
   IF NOT AVAILABLE sumkont THEN DO:
      CREATE sumkont.
   END.                                                              
   IF eko.ERGL = "" THEN DO:
      ASSIGN
      sumkont.RGL = "0" + eko.EORG.            /*ANVÄNDS FÖR KONTSTRÄNG*/
   END.    
   ELSE DO:                        
      ASSIGN
      sumkont.RGL = eko.ERGL.            /*ANVÄNDS FÖR KONTSTRÄNG*/
   END.                    
   IF eko.EDEBKRED = TRUE THEN DO:                                           
      IF INTEGER(eko.EPROJEKT) <= 999 THEN DO:         
         IF SUBSTRING(eko.EPROJEKT,1,3) = "000" THEN DO:
            sumkont.AONR = eko.EPROJEKT.
         END.
         ELSE DO:
            sumkont.AONR = sumkont.RGL + eko.EPROJEKT.
         END.
      END.
      ELSE sumkont.AONR = eko.EPROJEKT.                
   END.   
   ELSE sumkont.AONR = eko.EPROJEKT.
   ASSIGN            
   sumkont.FELDEBKRED = eko.FELDEBKRED
   sumkont.DEBKRED = eko.EDEBKRED
   sumkont.ORG = eko.EORG         
   sumkont.TRANSDATUM = eko.ETRANSDATUM
   sumkont.KOSTNADSSLAG = eko.EKOSTNADSSLAG       
   sumkont.TD = eko.ETD   
   sumkont.ANTAL = sumkont.ANTAL + eko.EANTAL   
   sumkont.BELOPP = sumkont.BELOPP + eko.EBELOPP.                    
END.

/*DEBET*/  /*KREDIT*/
FOR EACH sumkont:
         /*SROIBTRV*/
   ASSIGN
   str = ""
   str2 = "".                 
   ASSIGN                        
  /*iedimi1*/
   SUBSTRING(str,utnrIBT[nrcolIBT[2]]) = STRING(sumkont.KOSTNADSSLAG,"x(5)")
  
   /*iedimi2*/  /*organisation som posten till hör*/
   /*SUBSTRING(str,utnrIBT[nrcolIBT[3]]) = sumkont.ORG*/
   SUBSTRING(str,utnrIBT[nrcolIBT[3]]) = sumkont.RGL.
   /*iedimi3*/ /*aonrkont.k2*/
   IF INTEGER(sumkont.TD) = 0 THEN SUBSTRING(str,utnrIBT[nrcolIBT[4]]) = "".
   ELSE SUBSTRING(str,utnrIBT[nrcolIBT[4]]) = sumkont.TD. 
   /*iedimi4*/ /*aonrkont.k1*/
   IF INTEGER(sumkont.AONR) = 0 THEN SUBSTRING(str,utnrIBT[nrcolIBT[4]]) = "".
   ELSE SUBSTRING(str,utnrIBT[nrcolIBT[5]]) = sumkont.AONR.       
   ASSIGN 
   SUBSTRING(str,utnrIBT[nrcolIBT[13]]) = STRING(TODAY,"99999999")
   SUBSTRING(str,utnrIBT[nrcolIBT[15]]) = "+" + 
   STRING(sumkont.BELOPP * 1000,"9999999999999999")
   SUBSTRING(str,utnrIBT[nrcolIBT[18]]) = "SEK "
   SUBSTRING(str,utnrIBT[nrcolIBT[19]]) = "Tidredovisning från Guru"
   SUBSTRING(str,utnrIBT[nrcolIBT[21]]) = STRING(YEAR(sumkont.TRANSDATUM),"9999") + STRING(MONTH(sumkont.TRANSDATUM),"99")
   SUBSTRING(str,utnrIBT[nrcolIBT[22]]) = "RVGU"
   SUBSTRING(str,utnrIBT[nrcolIBT[23]]) = "X". 
   /*KREDIT*/
   IF sumkont.FELDEBKRED = FALSE THEN SUBSTRING(str,utnrIBT[nrcolIBT[15]],1) = "-".
   ASSIGN
   filut = ""
   filutkopia = "". 
   ASSIGN
   filut = prognamn + "N9\" 
   filutkopia = prognamnk + "N9\".
   IF SEARCH(filut) = ? THEN DO:
      OS-CREATE-DIR VALUE(filut).
   END.
   IF SEARCH(filutkopia) = ? THEN DO:
      OS-CREATE-DIR VALUE(filutkopia).
   END.
   ASSIGN
   filut = filut + prognamnIBTI 
   filutkopia = filutkopia + prognamnkIBTI + STRING(TODAY,"99999999") + ".txt".
   OUTPUT TO VALUE(filut) APPEND.
   PUT UNFORMATTED str AT 1 SKIP.
   OUTPUT CLOSE.
   OUTPUT TO VALUE(filutkopia) APPEND.
   PUT UNFORMATTED str AT 1 SKIP.
   OUTPUT CLOSE.
END. 
{EUROPEANAMERICAN.I}
PROCEDURE tim_UI:
   DO TRANSACTION:                 
      IF ekoforst.EORG = ekoforst.EGEO THEN DO:     
         FIND FIRST eko WHERE
         eko.ENY = TRUE AND      
         eko.EORG = ekoforst.EGEO AND 
         eko.ETRANSDATUM = ekoforst.ETRANSDATUM AND
         eko.EPROJEKT = ekoforst.EPROJEKT AND
         eko.EKOSTNADSSLAG = kontokod AND
         eko.ERGL = fk1 AND          /*GRANINGE K2 ANVÄNDS FÖR KONTSTRÄNG*/
         eko.ETD = fk2 AND  /*GRANINGE K3*/
         eko.EDEBKRED = TRUE AND
         eko.FELDEBKRED = ekoforst.FELDEBKRED
         USE-INDEX ORG EXCLUSIVE-LOCK NO-ERROR.           
         IF NOT AVAILABLE eko THEN DO:
            CREATE eko.
         END.    
         ASSIGN         
         eko.EDEBKRED = TRUE 
         eko.ENY = TRUE 
         eko.EORG = ekoforst.EGEO  
         eko.ETRANSDATUM = ekoforst.ETRANSDATUM
         eko.EPROJEKT = ekoforst.EPROJEKT 
         eko.EKOSTNADSSLAG = kontokod 
         eko.ERGL = fk1           /*ANVÄNDS FÖR KONTSTRÄNG*/
         eko.ETD = fk2                
         eko.EDEBKRED = TRUE 
         eko.EANTAL = eko.EANTAL + ekoforst.EANTAL  
         eko.EBELOPP = eko.EBELOPP + ROUND(ekoforst.EBELOPP,2)
         eko.FELDEBKRED = ekoforst.FELDEBKRED.         
         /*KREDIT*/
         IF kodanst BEGINS 'T' THEN DO:
            ASSIGN
            kontokod = "99798"
            fk2 = "9700".
         END.       
         ELSE DO:
            ASSIGN
            kontokod = "99508"
            fk2 = "9700".
         END.           
         IF ekoforst.FELDEBKRED = TRUE THEN DO:
            FIND FIRST eko2 WHERE
            eko2.ENY = TRUE AND         
            eko2.EORG = ekoforst.EGEO AND          
            eko2.ETRANSDATUM = ekoforst.ETRANSDATUM AND         
            eko2.EKOSTNADSSLAG = kontokod AND
            eko2.ERGL = fk1 AND          /*ANVÄNDS FÖR KONTSTRÄNG*/
            eko2.ETD = fk2 AND
            eko2.EDEBKRED = FALSE AND
            eko2.FELDEBKRED = FALSE
            USE-INDEX ORG EXCLUSIVE-LOCK NO-ERROR.  
         END.
         ELSE DO:
            FIND FIRST eko2 WHERE
            eko2.ENY = TRUE AND         
            eko2.EORG = ekoforst.EGEO AND          
            eko2.ETRANSDATUM = ekoforst.ETRANSDATUM AND         
            eko2.EKOSTNADSSLAG = kontokod AND
            eko2.ERGL = fk1 AND          /*ANVÄNDS FÖR KONTSTRÄNG*/
            eko2.ETD = fk2 AND
            eko2.EDEBKRED = FALSE AND
            eko2.FELDEBKRED = TRUE
            USE-INDEX ORG EXCLUSIVE-LOCK NO-ERROR.  
         END.         
         IF NOT AVAILABLE eko2 THEN DO:
            CREATE eko2.
         END.    
         ASSIGN              
         eko2.EDEBKRED = FALSE 
         eko2.ENY = TRUE         
         eko2.EORG = ekoforst.EGEO
         eko2.EPROJEKT = "000000"          
         eko2.ETRANSDATUM = ekoforst.ETRANSDATUM         
         eko2.EKOSTNADSSLAG = kontokod
         eko2.ERGL = fk1           /*ANVÄNDS FÖR KONTSTRÄNG*/
         eko2.ETD = fk2   
         eko2.EANTAL = eko2.EANTAL + ekoforst.EANTAL  
         eko2.EBELOPP = eko2.EBELOPP + ROUND(ekoforst.EBELOPP,2).                     
         IF ekoforst.FELDEBKRED = TRUE THEN ASSIGN eko2.FELDEBKRED = FALSE.
         ELSE ASSIGN eko2.FELDEBKRED = TRUE.
      END.    
      ELSE DO:     
         FIND FIRST eko WHERE
         eko.ENY = TRUE AND      
         eko.EORG = ekoforst.EGEO AND 
         eko.ETRANSDATUM = ekoforst.ETRANSDATUM AND
         eko.EPROJEKT = ekoforst.EPROJEKT AND
         eko.EKOSTNADSSLAG = kontokod AND 
         eko.ERGL = fk1 AND          /*ANVÄNDS FÖR KONTSTRÄNG*/
         eko.ETD = fk2 AND
         eko.EDEBKRED = TRUE AND
         eko.FELDEBKRED = ekoforst.FELDEBKRED
         USE-INDEX ORG EXCLUSIVE-LOCK NO-ERROR.           
         IF NOT AVAILABLE eko THEN DO:
            CREATE eko.
         END.    
         ASSIGN         
         eko.EDEBKRED = TRUE 
         eko.ENY = TRUE 
         eko.EORG = ekoforst.EGEO  
         eko.ETRANSDATUM = ekoforst.ETRANSDATUM
         eko.EPROJEKT = ekoforst.EPROJEKT 
         eko.EKOSTNADSSLAG = kontokod              
         eko.ERGL = fk1           /*ANVÄNDS FÖR KONTSTRÄNG*/
         eko.ETD = fk2   
         eko.EDEBKRED = TRUE 
         eko.EANTAL = eko.EANTAL + ekoforst.EANTAL  
         eko.EBELOPP = eko.EBELOPP + ROUND(ekoforst.EBELOPP,2).  
         eko.FELDEBKRED = ekoforst.FELDEBKRED.         
         /*KREDIT*/          
         IF kodanst BEGINS 'T' THEN DO:           
            ASSIGN
            kontokod = "99799"
            fk2 = "9700".
         END.       
         ELSE DO:            
            ASSIGN
            kontokod = "99509"
            fk2 = "9700".
         END.        
         IF ekoforst.FELDEBKRED = TRUE THEN DO:
            FIND FIRST eko2 WHERE
            eko2.ENY = TRUE AND         
            eko2.EORG = ekoforst.EORG AND          
            eko2.ETRANSDATUM = ekoforst.ETRANSDATUM AND         
            eko2.EKOSTNADSSLAG = kontokod AND
            eko2.ERGL = STRING("0") + ekoforst.EORG AND          /*ANVÄNDS FÖR KONTSTRÄNG*/
            eko2.ETD = fk2 AND
            eko2.EDEBKRED = FALSE AND
            eko2.FELDEBKRED = FALSE
            USE-INDEX ORG EXCLUSIVE-LOCK NO-ERROR.  
         END.
         ELSE DO:
            FIND FIRST eko2 WHERE
            eko2.ENY = TRUE AND         
            eko2.EORG = ekoforst.EORG AND          
            eko2.ETRANSDATUM = ekoforst.ETRANSDATUM AND         
            eko2.EKOSTNADSSLAG = kontokod AND
            eko2.ERGL = STRING("0") + ekoforst.EORG AND          /*ANVÄNDS FÖR KONTSTRÄNG*/
            eko2.ETD = fk2 AND
            eko2.EDEBKRED = FALSE AND
            eko2.FELDEBKRED = TRUE
            USE-INDEX ORG EXCLUSIVE-LOCK NO-ERROR.  
         END.         
         IF NOT AVAILABLE eko2 THEN DO:
            CREATE eko2.
         END.    
         ASSIGN              
         eko2.EDEBKRED = FALSE 
         eko2.ENY = TRUE         
         eko2.EORG = ekoforst.EORG 
         eko2.EPROJEKT = "000000"         
         eko2.ETRANSDATUM = ekoforst.ETRANSDATUM         
         eko2.EKOSTNADSSLAG = kontokod
         eko2.ERGL = STRING("0") + ekoforst.EORG           /*ANVÄNDS FÖR KONTSTRÄNG*/
         eko2.ETD = fk2   
         eko2.EANTAL = eko2.EANTAL + ekoforst.EANTAL  
         eko2.EBELOPP = eko2.EBELOPP + ROUND(ekoforst.EBELOPP,2).                     
         IF ekoforst.FELDEBKRED = TRUE THEN ASSIGN eko2.FELDEBKRED = FALSE.
         ELSE ASSIGN eko2.FELDEBKRED = TRUE.
      END.                                
   END.
END PROCEDURE.


