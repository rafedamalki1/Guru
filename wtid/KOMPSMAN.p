/*KOMPSMAN.P   KOMPSALDO KORNING*/
{LESAMMAN.I}  
DEFINE INPUT PARAMETER kordatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER koranv LIKE ANVANDARE.ANVANDARE NO-UNDO.
DEFINE INPUT PARAMETER gkorvar AS CHARACTER NO-UNDO.

RUN sammut_UI (INPUT 1).
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.       
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.

DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.  
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE vkdatum AS DATE NO-UNDO.

/*DEFINE NEW SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.*/
DEFINE NEW SHARED VARIABLE korvar AS CHARACTER NO-UNDO.


DEFINE VARIABLE kompaonr AS CHARACTER NO-UNDO.
DEFINE VARIABLE komplog AS LOGICAL NO-UNDO.
DEFINE VARIABLE kompapph AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE overtidhj2
   FIELD DATUM LIKE TIDREGITAB.DATUM
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD
   FIELD OVERANTAL AS DECIMAL
   FIELD OVERTIDTILL AS CHARACTER
   FIELD MANAD AS INTEGER
   FIELD MULTIP AS DECIMAL
   FIELD ANTMULT AS DECIMAL
   FIELD KOMPUTTAG AS DECIMAL    
   FIELD NAMN AS CHARACTER FORMAT "X(30)"
   FIELD OMRADE AS CHARACTER
   FIELD GEOMRADE AS CHARACTER
   FIELD OVERTIDUTTAG AS CHARACTER
   FIELD VERKOV AS DECIMAL
   FIELD VERKKO AS DECIMAL
   FIELD VECKOKORD AS CHARACTER 
   INDEX PKOD PERSONALKOD DATUM.

DEFINE TEMP-TABLE oversum
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD
   FIELD NAMN AS CHARACTER FORMAT "X(30)"
   FIELD MANAD AS INTEGER
   FIELD OVERANTAL AS DECIMAL
   FIELD ANTMULT AS DECIMAL   
   FIELD KOMPUTTAG AS DECIMAL       
   FIELD OMRADE AS CHARACTER
   FIELD VERKOV AS DECIMAL
   FIELD VERKKO AS DECIMAL
   INDEX PKOD PERSONALKOD MANAD.
    
DEFINE TEMP-TABLE overhelak
   FIELD KOMPTOT AS DECIMAL      
   FIELD KOMPUTTOT AS DECIMAL
   FIELD KOMPINUTTOT AS DECIMAL     
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD    
   FIELD NAMN AS CHARACTER FORMAT "X(30)"
   FIELD OMRADE AS CHARACTER
   FIELD GEOMRADE AS CHARACTER
   FIELD VERKOV AS DECIMAL
   FIELD VERKKO AS DECIMAL
   INDEX PKOD PERSONALKOD .
FUNCTION klock100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL ):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600.

END FUNCTION.
FUNCTION klock60 RETURNS DECIMAL
  ( INPUT ber100 AS DECIMAL ):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN  TRUNCATE(ber100,0) + ((ber100 - TRUNCATE(ber100,0)) * 60 / 100 ).

END FUNCTION.
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
ASSIGN

vkdatum = kordatum
korvar = gkorvar.
RUN kompsaldo_UI.
RUN sammut_UI (INPUT 2).

PROCEDURE kompsaldo_UI.   
   EMPTY TEMP-TABLE overtidhj2 NO-ERROR.
   IF Guru.Konstanter:globforetag = "GKAL" THEN kompaonr = "171".
   ELSE kompaonr = "170".    
   OPEN QUERY pq FOR EACH PERSONALTAB WHERE PERSONALTAB.PERSMASK = TRUE
   AND PERSONALTAB.BRAVO = TRUE
   USE-INDEX PERSONALKOD NO-LOCK.
   GET FIRST pq NO-LOCK.
   DO WHILE AVAILABLE(PERSONALTAB):
      FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
      USE-INDEX ANSTF NO-LOCK NO-ERROR.
      OPEN QUERY tq FOR EACH TIDREGITAB WHERE
      TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND TIDREGITAB.GODKAND BEGINS "G" AND TIDREGITAB.VECKOKORD = korvar AND  TIDREGITAB.DATUM <= vkdatum AND
      TIDREGITAB.OKOD1 NE "" AND TIDREGITAB.OVERTIDUTTAG = "K"  USE-INDEX PSTART NO-LOCK.   
      GET FIRST tq NO-LOCK.
      DO WHILE AVAILABLE(TIDREGITAB):      
         IF TIDREGITAB.AONR = kompaonr  THEN DO:
            CREATE overtidhj2.                          
            ASSIGN
            overtidhj2.PERSONALKOD = TIDREGITAB.PERSONALKOD                    
            overtidhj2.DATUM =  TIDREGITAB.DATUM.                     
            overtidhj2.MANAD = MONTH(TIDREGITAB.DATUM).
            overtidhj2.KOMPUTTAG = klock100(TIDREGITAB.TOTALT).         
            overtidhj2.MULTIP = 1.      
         END.         
         IF TIDREGITAB.OVERTIDUTTAG = "K"  THEN DO:                 
            IF TIDREGITAB.OANT1 > 0  THEN DO:      
               CREATE overtidhj2.                          
               ASSIGN
               overtidhj2.PERSONALKOD = TIDREGITAB.PERSONALKOD                                               
               overtidhj2.DATUM =  TIDREGITAB.DATUM
               overtidhj2.OVERANTAL = klock100(TIDREGITAB.OANT1) 
               overtidhj2.OVERTIDTILL = TIDREGITAB.OKOD1
               overtidhj2.MANAD = MONTH(TIDREGITAB.DATUM)
               overtidhj2.MULTIP = 1
               overtidhj2.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG.
               FIND FIRST OVERKOD WHERE OVERKOD.OVERTIDTILL = TIDREGITAB.OKOD1 AND OVERKOD.KOD = ANSTFORMTAB.KOD USE-INDEX OVER NO-LOCK NO-ERROR.
               IF AVAILABLE  OVERKOD THEN DO:                                               
                  IF Guru.Konstanter:globforetag = "gkal" THEN overtidhj2.MULTIP = OVERKOD.MULTIP.
                  ELSE overtidhj2.MULTIP =  OVERKOD.ERSATTNING.
               END.
               IF TIDREGITAB.OVERTIDUTTAG = "K" THEN DO: 
                  overtidhj2.ANTMULT = (1 + overtidhj2.MULTIP) * overtidhj2.OVERANTAL.
                  /*ändrad till verklig tid inte inte avrundad tid 20171122 Lena */               
                  overtidhj2.VERKKO = klock100(TIDREGITAB.TOTALT).
               END.               
            END.      
            IF TIDREGITAB.OANT2 > 0  THEN DO:      
               CREATE overtidhj2.                          
               ASSIGN
               overtidhj2.PERSONALKOD = TIDREGITAB.PERSONALKOD                                          
               overtidhj2.DATUM =  TIDREGITAB.DATUM            
               overtidhj2.OVERANTAL = klock100(TIDREGITAB.OANT2) 
               overtidhj2.OVERTIDTILL = TIDREGITAB.OKOD2
               overtidhj2.MANAD = MONTH(TIDREGITAB.DATUM)
               overtidhj2.MULTIP = 1
               overtidhj2.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG.
               FIND FIRST OVERKOD WHERE OVERKOD.OVERTIDTILL = TIDREGITAB.OKOD2 AND OVERKOD.KOD = ANSTFORMTAB.KOD USE-INDEX OVER NO-LOCK NO-ERROR.
               IF AVAILABLE  OVERKOD THEN DO:                                               
                  IF Guru.Konstanter:globforetag = "gkal" THEN overtidhj2.MULTIP = OVERKOD.MULTIP.
                  ELSE overtidhj2.MULTIP =  OVERKOD.ERSATTNING.
               END.               
               IF TIDREGITAB.OVERTIDUTTAG = "K" THEN DO: 
                  overtidhj2.ANTMULT = (1 + overtidhj2.MULTIP) * overtidhj2.OVERANTAL.
                  /*ändrad till verklig tid inte inte avrundad tid 20171122 Lena 
                  oant2 skall bara räknas om oant1 = 0*/
                  IF TIDREGITAB.OANT1 = 0  THEN   overtidhj2.VERKKO = klock100(TIDREGITAB.TOTALT).                
               END.
                  
            END.      
            IF TIDREGITAB.OANT3 > 0  THEN DO:      
               CREATE overtidhj2.                          
               ASSIGN
               overtidhj2.PERSONALKOD = TIDREGITAB.PERSONALKOD                                          
               overtidhj2.DATUM =  TIDREGITAB.DATUM            
               overtidhj2.OVERANTAL = klock100(TIDREGITAB.OANT3) 
               overtidhj2.OVERTIDTILL = TIDREGITAB.OKOD3
               overtidhj2.MANAD = MONTH(TIDREGITAB.DATUM)
               overtidhj2.MULTIP = 1
               overtidhj2.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG.
               FIND FIRST OVERKOD WHERE OVERKOD.OVERTIDTILL = TIDREGITAB.OKOD3 AND OVERKOD.KOD = ANSTFORMTAB.KOD USE-INDEX OVER NO-LOCK NO-ERROR.
               IF AVAILABLE  OVERKOD THEN DO:                                               
                  IF Guru.Konstanter:globforetag = "gkal" THEN overtidhj2.MULTIP = OVERKOD.MULTIP.
                  ELSE overtidhj2.MULTIP =  OVERKOD.ERSATTNING.
               END.
               IF TIDREGITAB.OVERTIDUTTAG = "K" THEN DO:                   
                  overtidhj2.ANTMULT = (1 + overtidhj2.MULTIP) * overtidhj2.OVERANTAL.
                  /*ändrad till verklig tid inte inte avrundad tid 20171122 Lena 
                  oant3 skall bara räknas om oant1 = 0 och oant2= 0*/
                  IF TIDREGITAB.OANT1 = 0 AND  TIDREGITAB.OANT2 = 0  THEN   overtidhj2.VERKKO = klock100(TIDREGITAB.TOTALT).             
               END.                                 
            END.
         END.   
         GET NEXT tq NO-LOCK.           
      END.
      OPEN QUERY tuq FOR EACH TIDREGITAB WHERE
      TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND TIDREGITAB.GODKAND BEGINS "G" AND TIDREGITAB.VECKOKORD = korvar AND  TIDREGITAB.DATUM <= vkdatum AND
      TIDREGITAB.AONR = kompaonr  USE-INDEX PSTART NO-LOCK.   
      GET FIRST tuq NO-LOCK.
      DO WHILE AVAILABLE(TIDREGITAB):         
         CREATE overtidhj2.                          
         ASSIGN
         overtidhj2.PERSONALKOD = TIDREGITAB.PERSONALKOD                    
         overtidhj2.DATUM =  TIDREGITAB.DATUM.                     
         overtidhj2.MANAD = MONTH(TIDREGITAB.DATUM).
         overtidhj2.KOMPUTTAG = klock100(TIDREGITAB.TOTALT).         
         overtidhj2.MULTIP = 1.
         GET NEXT tuq NO-LOCK.      
      END.      
      /*berdskapstolkning när veckvila inträffar under klämdag genererar lart 260 = Fyllnadslön mot ledighet vid beredskap Lena 20190611*/      
      OPEN QUERY tblq FOR EACH TIDREGITAB WHERE
      TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND TIDREGITAB.GODKAND BEGINS "G" AND TIDREGITAB.VECKOKORD = korvar AND  TIDREGITAB.DATUM <= vkdatum AND
      TIDREGITAB.LONTILLAGG = "260"  USE-INDEX PSTART NO-LOCK.   
      GET FIRST tblq NO-LOCK.
      DO WHILE AVAILABLE(TIDREGITAB):
         CREATE overtidhj2.                          
         ASSIGN
         overtidhj2.PERSONALKOD = TIDREGITAB.PERSONALKOD                                               
         overtidhj2.DATUM =  TIDREGITAB.DATUM
         overtidhj2.OVERANTAL = klock100(TIDREGITAB.LONTILLANTAL) 
         overtidhj2.OVERTIDTILL = TIDREGITAB.LONTILLAGG
         overtidhj2.MANAD = MONTH(TIDREGITAB.DATUM)
         overtidhj2.MULTIP = 0         
         overtidhj2.OVERTIDUTTAG = "K".          
         overtidhj2.ANTMULT =  overtidhj2.OVERANTAL.
         /*ändrad till verklig tid inte inte avrundad tid 20171122 Lena */               
         overtidhj2.VERKKO = 0.
         GET NEXT tblq NO-LOCK.      
      END.
      /*berdskapstolkning klämdagar halvdagar genererar lart 260 = Fyllnadslön mot ledighet vid beredksp Lena 20190611*/      
      OPEN QUERY tbq FOR EACH TIDREGITAB WHERE
      TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND TIDREGITAB.GODKAND BEGINS "G" AND TIDREGITAB.VECKOKORD = korvar AND  TIDREGITAB.DATUM <= vkdatum AND
      TIDREGITAB.BEREDSKAP = "260"  USE-INDEX PSTART NO-LOCK.   
      GET FIRST tbq NO-LOCK.
      DO WHILE AVAILABLE(TIDREGITAB):
         CREATE overtidhj2.                          
         ASSIGN
         overtidhj2.PERSONALKOD = TIDREGITAB.PERSONALKOD                                               
         overtidhj2.DATUM =  TIDREGITAB.DATUM
         overtidhj2.OVERANTAL = klock100(TIDREGITAB.BERANTAL) 
         overtidhj2.OVERTIDTILL = TIDREGITAB.BEREDSKAP
         overtidhj2.MANAD = MONTH(TIDREGITAB.DATUM)
         overtidhj2.MULTIP = 0         
         overtidhj2.OVERTIDUTTAG = "K".          
         overtidhj2.ANTMULT =  overtidhj2.OVERANTAL.
         /*ändrad till verklig tid inte inte avrundad tid 20171122 Lena */               
         overtidhj2.VERKKO = 0.
         GET NEXT tbq NO-LOCK.      
      END.        
      GET NEXT pq NO-LOCK.
   END.   
   FOR EACH overtidhj2 BREAK BY overtidhj2.PERSONALKOD BY overtidhj2.MANAD:    
      ACCUMULATE overtidhj2.OVERANTAL (TOTAL  BY overtidhj2.PERSONALKOD BY overtidhj2.MANAD ).        
      ACCUMULATE overtidhj2.ANTMULT (TOTAL  BY overtidhj2.PERSONALKOD BY overtidhj2.MANAD).
      ACCUMULATE overtidhj2.KOMPUTTAG (TOTAL  BY overtidhj2.PERSONALKOD BY overtidhj2.MANAD).
      ACCUMULATE overtidhj2.VERKKO (TOTAL  BY overtidhj2.PERSONALKOD BY overtidhj2.MANAD).
      ACCUMULATE overtidhj2.VERKOV (TOTAL  BY overtidhj2.PERSONALKOD BY overtidhj2.MANAD).              
      IF LAST-OF(overtidhj2.MANAD) THEN DO:      
         CREATE oversum.
         ASSIGN         
         oversum.PERSONALKOD = overtidhj2.PERSONALKOD                  
         oversum.NAMN = overtidhj2.NAMN        
         oversum.MANAD =   overtidhj2.MANAD.        
         oversum.OVERANTAL = (ACCUM TOTAL BY overtidhj2.MANAD overtidhj2.OVERANTAL).                  
         oversum.ANTMULT = (ACCUM TOTAL BY overtidhj2.MANAD overtidhj2.ANTMULT).
         oversum.KOMPUTTAG = (ACCUM TOTAL BY overtidhj2.MANAD overtidhj2.KOMPUTTAG).
         oversum.VERKKO = (ACCUM TOTAL BY overtidhj2.MANAD overtidhj2.VERKKO).
         oversum.VERKOV = (ACCUM TOTAL BY overtidhj2.MANAD overtidhj2.VERKOV).                                             
      END.
   END.
       
   FOR EACH oversum WHERE  BY oversum.PERSONALKOD : 
      FIND FIRST overhelak WHERE overhelak.PERSONALKOD = oversum.PERSONALKOD NO-ERROR.
      IF NOT AVAILABLE overhelak  THEN DO:
         CREATE overhelak.
         ASSIGN overhelak.PERSONALKOD  =  oversum.PERSONALKOD
         overhelak.NAMN = oversum.NAMN.         
      END.
      overhelak.VERKKO = overhelak.VERKKO + oversum.VERKKO.
      overhelak.VERKOV = overhelak.VERKOV + oversum.VERKOV.
      overhelak.KOMPTOT = overhelak.KOMPTOT + oversum.ANTMULT.
      overhelak.KOMPUTTOT = overhelak.KOMPUTTOT + oversum.KOMPUTTAG.
      overhelak.KOMPINUTTOT = overhelak.KOMPTOT - overhelak.KOMPUTTOT.       
   END.   
   RUN FINNSTABELL.P (INPUT "KOMPSALDO", OUTPUT komplog).
   IF komplog = FALSE THEN RETURN.   
   RUN KOMPSMANTAB.P PERSISTENT SET kompapph.   
   RUN skapkompsaldo_UI IN kompapph (INPUT TABLE overhelak, INPUT vkdatum).
   IF VALID-HANDLE(kompapph) THEN DELETE PROCEDURE kompapph.
   kompapph = ?.
   
   END PROCEDURE.

