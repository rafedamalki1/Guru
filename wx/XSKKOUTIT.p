                                                      /*XSKKOUITIT.P*/
{APP.I}
DEFINE NEW SHARED VARIABLE kto LIKE KONTO.KONTO NO-UNDO.
DEFINE NEW SHARED VARIABLE valar AS INTEGER NO-UNDO. 
DEFINE VARIABLE arrhjsum LIKE TIDREGITAB.TOTALT NO-UNDO.
DEFINE VARIABLE arrhjsumb LIKE SUMTID.BELOPP NO-UNDO.     
DEFINE VARIABLE arrhjmt LIKE TIDREGITAB.TOTALT NO-UNDO.
DEFINE VARIABLE arrhjmb LIKE SUMTID.BELOPP NO-UNDO.    
DEFINE VARIABLE arrhjuea LIKE SUMTID.BELOPP NO-UNDO.   
DEFINE VARIABLE arrhjmtrl LIKE SUMTID.BELOPP NO-UNDO.    
DEFINE VARIABLE arrhjovr LIKE SUMTID.BELOPP NO-UNDO.
DEFINE VARIABLE arrhjber LIKE SUMTID.TIMMAR NO-UNDO.
DEFINE VARIABLE plmont LIKE AORESTRAPP.PLANMONT NO-UNDO.  
DEFINE VARIABLE plmask LIKE AORESTRAPP.PLANMASK NO-UNDO.   
DEFINE VARIABLE plmkost AS INTEGER NO-UNDO.  
DEFINE VARIABLE plmtrl LIKE AORESTRAPP.MTRLK NO-UNDO.   
DEFINE VARIABLE plea LIKE AORESTRAPP.EA NO-UNDO.    
DEFINE VARIABLE str AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE VARIABLE forsta LIKE TIDREGITAB.LONTILLAGG NO-UNDO.
DEFINE VARIABLE vpers LIKE PERSONALTAB.PERSONALKOD NO-UNDO.
DEFINE VARIABLE vomr LIKE PERSONALTAB.OMRADE NO-UNDO.
DEFINE VARIABLE vomr1 LIKE PERSONALTAB.OMRADE NO-UNDO.
DEFINE VARIABLE maskpers LIKE PERSONALTAB.PERSMASK NO-UNDO.
DEFINE VARIABLE personal LIKE PERSONALTAB.PERSONALKOD NO-UNDO.
DEFINE VARIABLE priset AS DECIMAL NO-UNDO.
DEFINE VARIABLE tot LIKE TIDREGITAB.LONTILLANTAL NO-UNDO.  
DEFINE VARIABLE raknare1 AS INTEGER NO-UNDO.  
DEFINE VARIABLE raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE hjsum AS INTEGER NO-UNDO. 
DEFINE VARIABLE col1 AS INTEGER NO-UNDO.    
DEFINE VARIABLE col2 AS INTEGER NO-UNDO.
DEFINE VARIABLE col3 AS INTEGER NO-UNDO.
DEFINE VARIABLE col4 AS INTEGER NO-UNDO.
DEFINE VARIABLE col5 AS INTEGER NO-UNDO.
DEFINE VARIABLE col6 AS INTEGER NO-UNDO.
DEFINE VARIABLE col7 AS INTEGER NO-UNDO.  
DEFINE VARIABLE col8 AS INTEGER NO-UNDO.    
DEFINE VARIABLE col9 AS INTEGER NO-UNDO. 
DEFINE VARIABLE col10 AS INTEGER NO-UNDO.
DEFINE VARIABLE fdeln AS DECIMAL FORMAT "9.99" NO-UNDO.
DEFINE VARIABLE mopris LIKE EBRPRIS.MONT NO-UNDO.
DEFINE VARIABLE mapris LIKE EBRPRIS.MASK1 NO-UNDO.
DEFINE VARIABLE ktfakt LIKE EBRPRIS.RORLIG NO-UNDO.
DEFINE VARIABLE ato AS LOGICAL NO-UNDO.

DEFINE TEMP-TABLE aonromr    
   FIELD AONR LIKE AONRTAB.AONR 
   FIELD DELNR LIKE AONRTAB.DELNR 
   FIELD AONRREC AS RECID  
   FIELD OMRADE LIKE AONRTAB.OMRADE
   FIELD ORT LIKE AONRTAB.ORT
   FIELD AONRAVDATUM LIKE AONRTAB.AONRAVDATUM
   FIELD KONTO LIKE KONTO.KONTO
   FIELD KONTONR LIKE KONTO.KONTONR
   FIELD SATS% LIKE AONRKONTKOD.SATS%
   FIELD INDEL AS INTEGER
   FIELD FASTAAONR LIKE AONRTAB.FASTAAONR
   FIELD FORDEL AS INTEGER
   INDEX OMRADE IS PRIMARY OMRADE ASCENDING
   INDEX AONR AONR ASCENDING DELNR ASCENDING.          
   
DEFINE TEMP-TABLE suaonr    
   FIELD AONR LIKE AONRTAB.AONR 
   FIELD DELNR LIKE AONRTAB.DELNR 
   FIELD AONRREC AS RECID           
   FIELD FORETAG LIKE FORETAG.FORETAG
   FIELD OMRADE LIKE OMRADETAB.OMRADE    
   FIELD AONRAVDATUM LIKE AONRTAB.AONRAVDATUM
   FIELD PEA LIKE SUCCAONR.EA
   FIELD MTRLK LIKE SUCCAONR.MTRLK 
   FIELD PLANMASK LIKE SUCCAONR.PLANMASK
   FIELD MASKKOST AS INTEGER
   FIELD PLANMONT LIKE SUCCAONR.PLANMONT
   FIELD BERTIM LIKE SUCCAONR.PLANMONT
   FIELD MONTKOST AS INTEGER
   FIELD INDEL AS INTEGER
   FIELD SATS% LIKE AONRKONTKOD.SATS%
   FIELD KREG AS CHARACTER FORMAT "X(2)"
   FIELD RREG AS CHARACTER FORMAT "X(2)"
   FIELD OVRIGKOST LIKE KALKNATT.OVRIGKOST
   FIELD PEA2 LIKE SUCCAONR.EA
   FIELD MTRLK2 LIKE SUCCAONR.MTRLK 
   FIELD PLANMASK2 LIKE SUCCAONR.PLANMASK
   FIELD MASKKOST2 AS INTEGER
   FIELD PLANMONT2 LIKE SUCCAONR.PLANMONT
   FIELD BERTIM2 LIKE SUCCAONR.PLANMONT
   FIELD MONTKOST2 AS INTEGER
   FIELD OVRIGKOST2 LIKE KALKNATT.OVRIGKOST    
   FIELD PEA3 LIKE SUCCAONR.EA
   FIELD MTRLK3 LIKE SUCCAONR.MTRLK 
   FIELD PLANMASK3 LIKE SUCCAONR.PLANMASK
   FIELD MASKKOST3 AS INTEGER
   FIELD PLANMONT3 LIKE SUCCAONR.PLANMONT
   FIELD BERTIM3 LIKE SUCCAONR.PLANMONT
   FIELD MONTKOST3 AS INTEGER
   FIELD OVRIGKOST3 LIKE KALKNATT.OVRIGKOST
   FIELD PEAS LIKE SUCCAONR.EA
   FIELD MTRLKS LIKE SUCCAONR.MTRLK 
   FIELD PLANMASKS LIKE SUCCAONR.PLANMASK
   FIELD MASKKOSTS AS INTEGER
   FIELD PLANMONTS LIKE SUCCAONR.PLANMONT
   FIELD BERTIMS LIKE SUCCAONR.PLANMONT
   FIELD MONTKOSTS AS INTEGER
   FIELD OVRIGKOSTS LIKE KALKNATT.OVRIGKOST
   FIELD P1 AS LOGICAL INITIAL FALSE
   FIELD P2 AS LOGICAL INITIAL FALSE
   FIELD P3 AS LOGICAL INITIAL FALSE
   INDEX AONR IS PRIMARY AONR ASCENDING.   
     
DEFINE TEMP-TABLE rest    
   FIELD AONR LIKE AONRTAB.AONR 
   FIELD DELNR LIKE AONRTAB.DELNR 
   FIELD AONRAVDATUM LIKE AONRTAB.AONRAVDATUM
   FIELD EA LIKE SUCCAONR.EA
   FIELD MTRLK LIKE SUCCAONR.MTRLK 
   FIELD PLANMASK LIKE SUCCAONR.PLANMASK
   FIELD MASKKOST AS INTEGER
   FIELD PLANMONT LIKE SUCCAONR.PLANMONT  
   FIELD MONTKOST AS INTEGER
   FIELD DATUM LIKE AORESTRAPP.DATUM   
   INDEX AONR IS PRIMARY AONR ASCENDING.    

DEFINE TEMP-TABLE suindel    
   FIELD OMRADE LIKE OMRADETAB.OMRADE    
   FIELD AONRAVDATUM LIKE AONRTAB.AONRAVDATUM
   FIELD PEA LIKE SUCCAONR.EA
   FIELD MTRLK LIKE SUCCAONR.MTRLK 
   FIELD PLANMASK LIKE SUCCAONR.PLANMASK
   FIELD MASKKOST AS INTEGER
   FIELD PLANMONT LIKE SUCCAONR.PLANMONT
   FIELD MONTKOST AS INTEGER
   FIELD INDEL AS INTEGER
   FIELD INNAMN AS CHARACTER FORMAT "X(20)"
   FIELD SATS% LIKE AONRKONTKOD.SATS%
   FIELD OVRIGKOST LIKE KALKNATT.OVRIGKOST
   FIELD BERTIM LIKE SUCCAONR.PLANMONT
   INDEX INDEL IS PRIMARY INDEL ASCENDING.  

DEFINE TEMP-TABLE suindel1    
   FIELD PEA LIKE SUCCAONR.EA
   FIELD MTRLK LIKE SUCCAONR.MTRLK 
   FIELD PLANMASK LIKE SUCCAONR.PLANMASK
   FIELD MASKKOST AS INTEGER
   FIELD PLANMONT LIKE SUCCAONR.PLANMONT
   FIELD MONTKOST AS INTEGER
   FIELD INDEL AS INTEGER
   FIELD INNAMN AS CHARACTER FORMAT "X(20)"
   FIELD OVRIGKOST LIKE KALKNATT.OVRIGKOST
   FIELD BERTIM LIKE SUCCAONR.PLANMONT
   INDEX INDEL IS PRIMARY INDEL ASCENDING.  
      
DEFINE TEMP-TABLE suaonr1            
   FIELD OMRADE LIKE OMRADETAB.OMRADE        
   FIELD AONRAVDATUM LIKE AONRTAB.AONRAVDATUM
   FIELD PEA LIKE SUCCAONR.EA
   FIELD MTRLK LIKE SUCCAONR.MTRLK 
   FIELD PLANMASK LIKE SUCCAONR.PLANMASK
   FIELD MASKKOST AS INTEGER
   FIELD PLANMONT LIKE SUCCAONR.PLANMONT
   FIELD MONTKOST AS INTEGER
   FIELD OVRIGKOST LIKE KALKNATT.OVRIGKOST
   FIELD BERTIM LIKE SUCCAONR.PLANMONT
   INDEX OMRADE IS PRIMARY OMRADE ASCENDING. 

DEFINE TEMP-TABLE suaonr3           
   FIELD OMRADE LIKE OMRADETAB.OMRADE        
   FIELD AONRAVDATUM LIKE AONRTAB.AONRAVDATUM
   FIELD PEA LIKE SUCCAONR.EA
   FIELD MTRLK LIKE SUCCAONR.MTRLK 
   FIELD PLANMASK LIKE SUCCAONR.PLANMASK
   FIELD MASKKOST AS INTEGER
   FIELD PLANMONT LIKE SUCCAONR.PLANMONT
   FIELD MONTKOST AS INTEGER
   FIELD OVRIGKOST LIKE KALKNATT.OVRIGKOST
   FIELD BERTIM LIKE SUCCAONR.PLANMONT
   INDEX OMRADE IS PRIMARY OMRADE ASCENDING.    
   
DEFINE TEMP-TABLE suaonr2            
   FIELD PEA LIKE SUCCAONR.EA
   FIELD MTRLK LIKE SUCCAONR.MTRLK 
   FIELD PLANMASK LIKE SUCCAONR.PLANMASK
   FIELD MASKKOST AS INTEGER
   FIELD PLANMONT LIKE SUCCAONR.PLANMONT
   FIELD OVRIGKOST LIKE KALKNATT.OVRIGKOST
   FIELD MONTKOST AS INTEGER
   FIELD BERTIM LIKE SUCCAONR.PLANMONT.

DEFINE TEMP-TABLE suaonr4            
   FIELD PEA LIKE SUCCAONR.EA
   FIELD MTRLK LIKE SUCCAONR.MTRLK 
   FIELD PLANMASK LIKE SUCCAONR.PLANMASK
   FIELD MASKKOST AS INTEGER
   FIELD PLANMONT LIKE SUCCAONR.PLANMONT
   FIELD OVRIGKOST LIKE KALKNATT.OVRIGKOST
   FIELD MONTKOST AS INTEGER
   FIELD BERTIM LIKE SUCCAONR.PLANMONT.                
   
DEFINE TEMP-TABLE suaonr5    
   FIELD AONR LIKE AONRTAB.AONR 
   FIELD DELNR LIKE AONRTAB.DELNR 
   FIELD AONRREC AS RECID           
   FIELD FORETAG LIKE FORETAG.FORETAG
   FIELD OMRADE LIKE OMRADETAB.OMRADE    
   FIELD AONRAVDATUM LIKE AONRTAB.AONRAVDATUM
   FIELD PEA LIKE SUCCAONR.EA
   FIELD MTRLK LIKE SUCCAONR.MTRLK 
   FIELD PLANMASK LIKE SUCCAONR.PLANMASK
   FIELD MASKKOST AS INTEGER
   FIELD PLANMONT LIKE SUCCAONR.PLANMONT
   FIELD MONTKOST AS INTEGER
   FIELD SATS% LIKE AONRKONTKOD.SATS%
   FIELD OVRIGKOST LIKE KALKNATT.OVRIGKOST
   FIELD BERTIM LIKE SUCCAONR.PLANMONT
   FIELD KALKYL AS CHARACTER
   INDEX AONR IS PRIMARY AONR ASCENDING.        

DEFINE TEMP-TABLE samsum            
   FIELD TIMMAR LIKE SUMTID.TIMMAR  
   FIELD MTIMMAR LIKE SUMTID.TIMMAR  
   FIELD UEA LIKE SUMTID.TIMMAR  
   FIELD RESULTAT LIKE SUMTID.TIMMAR
   FIELD BELOPP LIKE SUMTID.BELOPP     
   FIELD MBELOPP LIKE SUMTID.BELOPP
   FIELD PERSONALKOD LIKE SUMTID.PERSONALKOD  
   FIELD INDEL AS INTEGER
   FIELD INNAMN AS CHARACTER FORMAT "X(20)"
   FIELD OMRADE LIKE AONRTAB.OMRADE
   FIELD ORT LIKE AONRTAB.ORT           
   FIELD AONRAVDATUM LIKE AONRTAB.AONRAVDATUM
   FIELD KONTONR LIKE KONTO.KONTONR 
   FIELD PERSMASK LIKE SUMTID.PERSMASK 
   FIELD SATS% LIKE AONRKONTKOD.SATS%
   FIELD OVRIGKOST LIKE KALKNATT.OVRIGKOST
   FIELD BERTIM LIKE SUMTID.TIMMAR   
   INDEX INDEL IS PRIMARY INDEL ASCENDING.   
   
DEFINE TEMP-TABLE samsum1            
   FIELD TIMMAR LIKE SUMTID.TIMMAR  
   FIELD MTIMMAR LIKE SUMTID.TIMMAR  
   FIELD UEA LIKE SUMTID.TIMMAR  
   FIELD RESULTAT LIKE SUMTID.TIMMAR
   FIELD BELOPP LIKE SUMTID.BELOPP     
   FIELD MBELOPP LIKE SUMTID.BELOPP
   FIELD PERSONALKOD LIKE SUMTID.PERSONALKOD  
   FIELD INDEL AS INTEGER
   FIELD INNAMN AS CHARACTER FORMAT "X(20)"        
   FIELD OVRIGKOST LIKE KALKNATT.OVRIGKOST
   FIELD BERTIM LIKE SUMTID.TIMMAR
   INDEX INDEL IS PRIMARY INDEL ASCENDING.    
   
DEFINE TEMP-TABLE slutsum            
   FIELD TIMMAR LIKE SUMTID.TIMMAR  
   FIELD MTIMMAR LIKE SUMTID.TIMMAR  
   FIELD UEA LIKE SUMTID.TIMMAR  
   FIELD RESULTAT LIKE SUMTID.TIMMAR
   FIELD BELOPP LIKE SUMTID.BELOPP     
   FIELD MBELOPP LIKE SUMTID.BELOPP
   FIELD PERSONALKOD LIKE SUMTID.PERSONALKOD  
   FIELD AONR LIKE AONRTAB.AONR 
   FIELD DELNR LIKE AONRTAB.DELNR 
   FIELD AONRREC AS RECID  
   FIELD OMRADE LIKE AONRTAB.OMRADE
   FIELD ORT LIKE AONRTAB.ORT           
   FIELD AONRAVDATUM LIKE AONRTAB.AONRAVDATUM
   FIELD KONTONR LIKE KONTO.KONTONR 
   FIELD PERSMASK LIKE SUMTID.PERSMASK 
   FIELD SATS% LIKE AONRKONTKOD.SATS%
   FIELD OVRIGKOST LIKE KALKNATT.OVRIGKOST
   FIELD MTRLK LIKE SUCCAONR.MTRLK
   FIELD KREG AS CHARACTER FORMAT "X(2)"
   FIELD RREG AS CHARACTER FORMAT "X(2)"
   FIELD BERTIM LIKE SUMTID.TIMMAR
   INDEX AONR IS PRIMARY AONR ASCENDING. 
     
DEFINE TEMP-TABLE slutsum1            
   FIELD TIMMAR LIKE SUMTID.TIMMAR  
   FIELD MTIMMAR LIKE SUMTID.TIMMAR  
   FIELD UEA LIKE SUMTID.TIMMAR  
   FIELD RESULTAT LIKE SUMTID.TIMMAR
   FIELD BELOPP LIKE SUMTID.BELOPP     
   FIELD MBELOPP LIKE SUMTID.BELOPP
   FIELD OVRIGKOST LIKE KALKNATT.OVRIGKOST
   FIELD OMRADE LIKE AONRTAB.OMRADE
   FIELD BERTIM LIKE SUMTID.TIMMAR
   FIELD MTRLK LIKE SUCCAONR.MTRLK
   INDEX OMRADE IS PRIMARY OMRADE ASCENDING. 
   
DEFINE TEMP-TABLE slutsum2            
   FIELD TIMMAR LIKE SUMTID.TIMMAR  
   FIELD MTIMMAR LIKE SUMTID.TIMMAR  
   FIELD UEA LIKE SUMTID.TIMMAR  
   FIELD RESULTAT LIKE SUMTID.TIMMAR
   FIELD BELOPP LIKE SUMTID.BELOPP     
   FIELD MBELOPP LIKE SUMTID.BELOPP
   FIELD OVRIGKOST LIKE KALKNATT.OVRIGKOST
   FIELD BERTIM LIKE SUMTID.TIMMAR
   FIELD MTRLK LIKE SUCCAONR.MTRLK. 
                                          
DEFINE TEMP-TABLE slutsum3            
   FIELD TIMMAR LIKE SUMTID.TIMMAR  
   FIELD MTIMMAR LIKE SUMTID.TIMMAR  
   FIELD UEA LIKE SUMTID.TIMMAR  
   FIELD RESULTAT LIKE SUMTID.TIMMAR
   FIELD BELOPP LIKE SUMTID.BELOPP     
   FIELD MBELOPP LIKE SUMTID.BELOPP
   FIELD OVRIGKOST LIKE KALKNATT.OVRIGKOST
   FIELD OMRADE LIKE AONRTAB.OMRADE
   FIELD BERTIM LIKE SUMTID.TIMMAR
   FIELD MTRLK LIKE SUCCAONR.MTRLK
   INDEX OMRADE IS PRIMARY OMRADE ASCENDING.     
                                       
DEFINE TEMP-TABLE slutsum4            
   FIELD TIMMAR LIKE SUMTID.TIMMAR  
   FIELD MTIMMAR LIKE SUMTID.TIMMAR  
   FIELD UEA LIKE SUMTID.TIMMAR  
   FIELD RESULTAT LIKE SUMTID.TIMMAR
   FIELD BELOPP LIKE SUMTID.BELOPP     
   FIELD MBELOPP LIKE SUMTID.BELOPP
   FIELD OVRIGKOST LIKE KALKNATT.OVRIGKOST
   FIELD BERTIM LIKE SUMTID.TIMMAR
   FIELD MTRLK LIKE SUCCAONR.MTRLK.     
   
DEFINE TEMP-TABLE slutsum5            
   FIELD TIMMAR LIKE SUMTID.TIMMAR  
   FIELD MTIMMAR LIKE SUMTID.TIMMAR  
   FIELD UEA LIKE SUMTID.TIMMAR  
   FIELD RESULTAT LIKE SUMTID.TIMMAR
   FIELD BELOPP LIKE SUMTID.BELOPP     
   FIELD MBELOPP LIKE SUMTID.BELOPP
   FIELD PERSONALKOD LIKE SUMTID.PERSONALKOD  
   FIELD AONR LIKE AONRTAB.AONR 
   FIELD DELNR LIKE AONRTAB.DELNR 
   FIELD AONRREC AS RECID  
   FIELD OMRADE LIKE AONRTAB.OMRADE
   FIELD ORT LIKE AONRTAB.ORT           
   FIELD AONRAVDATUM LIKE AONRTAB.AONRAVDATUM
   FIELD KONTONR LIKE KONTO.KONTONR 
   FIELD PERSMASK LIKE SUMTID.PERSMASK 
   FIELD SATS% LIKE AONRKONTKOD.SATS%
   FIELD OVRIGKOST LIKE KALKNATT.OVRIGKOST
   FIELD MTRLK LIKE SUCCAONR.MTRLK
   FIELD BERTIM LIKE SUMTID.TIMMAR
   INDEX AONR IS PRIMARY AONR ASCENDING.    
   
DEFINE TEMP-TABLE dagtemp
   FIELD DATUM LIKE SUMTID.DATUM  
   FIELD TIMMAR LIKE SUMTID.TIMMAR  
   FIELD MTIMMAR LIKE SUMTID.TIMMAR 
   FIELD BELOPP LIKE SUMTID.BELOPP  
   FIELD MBELOPP LIKE SUMTID.BELOPP
   FIELD PERSONALKOD LIKE SUMTID.PERSONALKOD  
   FIELD AONR LIKE AONRTAB.AONR 
   FIELD DELNR LIKE AONRTAB.DELNR 
   FIELD AONRREC AS RECID  
   FIELD OMRADE LIKE AONRTAB.OMRADE
   FIELD ORT LIKE AONRTAB.ORT           
   FIELD AONRAVDATUM LIKE AONRTAB.AONRAVDATUM
   FIELD KONTONR LIKE KONTO.KONTONR 
   FIELD PERSMASK LIKE SUMTID.PERSMASK 
   FIELD OVRIGKOST LIKE KALKNATT.OVRIGKOST
   FIELD MTRLK LIKE SUCCAONR.MTRLK 
   FIELD INDEL AS INTEGER
   FIELD SATS% LIKE AONRKONTKOD.SATS%
   FIELD KREG AS CHARACTER FORMAT "X(2)"
   FIELD RREG AS CHARACTER FORMAT "X(2)"   
   FIELD BERTIM LIKE SUMTID.TIMMAR
   INDEX AONR IS PRIMARY AONR ASCENDING. 
   
DEFINE TEMP-TABLE faonr    
   FIELD AONR LIKE AONRTAB.AONR 
   FIELD DELNR LIKE AONRTAB.DELNR 
   FIELD FORDEL AS INTEGER
   FIELD OMRADE LIKE SUMTID.OMRADE
   INDEX FORD IS PRIMARY AONR ASCENDING.    

      

DEFINE QUERY persoq FOR TIDREGITAB.
   
/*DEFINE QUERY omrq FOR OMRADETAB.*/

DEFINE VARIABLE RAD_PERIOD AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Visning per år", 1,
"Visning per period", 2
     SIZE 21.5 BY 2.77
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE RAD_LISTA AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
"INVESTERING", 1,
"REINVESTERING", 2,
"UNDERHÅLL", 3,
"DRIFT", 4,
"MÄTVÄRDESINSAMLING", 5,
"EXTERNA TJÄNSTER", 6,
"FASTA AONR", 7
     SIZE 20.38 BY 9.64
    BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE TOG_FAST AS LOGICAL INITIAL no 
     LABEL "Tillf aonr" 
     VIEW-AS TOGGLE-BOX
     SIZE 15.13 BY 1 NO-UNDO.
                       
DEFINE VARIABLE TOG_SAMMAN AS LOGICAL INITIAL no 
     LABEL "Sammanst tillf aonr" 
     VIEW-AS TOGGLE-BOX
     SIZE 24.25 BY 1 NO-UNDO.

{KOUTFUT.I}

/* ***************************  Main Block  *************************** */
FIND FIRST uppfoltemp NO-LOCK NO-ERROR.
ASSIGN
globforetag = uppfoltemp.FORETAG 
globanv = uppfoltemp.ANVANDARE 
kto = uppfoltemp.KONTO  
valar = uppfoltemp.VALTAR 
RAD_PERIOD = uppfoltemp.PERIOD  
TOG_FAST = uppfoltemp.FAST  
TOG_SAMMAN = uppfoltemp.SAMMAN                 
RAD_LISTA = uppfoltemp.LISTA     
bdatum = uppfoltemp.INDATUM  
avdatum = uppfoltemp.UTDATUM.
   FOR EACH aonromr:
      DELETE aonromr.
   END.   
   FIND FIRST EBRPRIS WHERE EBRPRIS.ARTAL = valar NO-LOCK NO-ERROR.
   IF AVAILABLE EBRPRIS THEN DO:
      ASSIGN
      mopris = EBRPRIS.MONT
      mapris = EBRPRIS.MASK1
      ktfakt = EBRPRIS.RORLIG.
   END.   
   ELSE DO:
      ASSIGN
      mopris = 464
      mapris = 395
      ktfakt = 232.
   END.  
   IF TOG_SAMMAN = TRUE THEN DO:
      OPEN QUERY fast FOR EACH INDEL USE-INDEX INDEL NO-LOCK.
      GET FIRST fast.
      DO WHILE AVAILABLE(INDEL):
         FIND FIRST kontkod WHERE kontkod.KONTONR = INDEL.K2 USE-INDEX KNR NO-LOCK NO-ERROR. 
         IF AVAILABLE kontkod THEN DO:
            CREATE kkod.
            ASSIGN 
            kkod.KREC = RECID(kontkod)  
            kkod.KONTO = "K2"
            kkod.KONTONR = INDEL.K2   
            kkod.INDEL = INDEL.INDEL
            kkod.BENAMNING = kontkod.BENAMNING.   
         END.   
         GET NEXT fast NO-LOCK.          
      END.   
      CLOSE QUERY fast.        
   END.     
   ELSE IF TOG_FAST = TRUE THEN DO:   
      IF RAD_LISTA = 7 THEN DO:
         IF YEAR(bdatum) <  YEAR(TODAY) THEN DO: 
            FOR EACH FORDAONR USE-INDEX FORD:
               CREATE faonr.
               ASSIGN 
               faonr.AONR = FORDAONR.AONR
               faonr.DELNR = FORDAONR.DELNR
               faonr.FORDEL = FORDAONR.M12
               faonr.OMRADE = FORDAONR.OMRADE.
            END.   
         END.
         ELSE IF YEAR(bdatum) =  YEAR(TODAY) THEN DO: 
            FOR EACH FORDAONR USE-INDEX FORD:
               CREATE faonr.
               ASSIGN 
               faonr.AONR = FORDAONR.AONR
               faonr.DELNR = FORDAONR.DELNR
               faonr.OMRADE = FORDAONR.OMRADE.
               IF MONTH(TODAY) = 1 THEN ASSIGN faonr.FORDEL = FORDAONR.M1.
               IF MONTH(TODAY) = 2 THEN ASSIGN faonr.FORDEL = FORDAONR.M1.
               IF MONTH(TODAY) = 3 THEN ASSIGN faonr.FORDEL = FORDAONR.M2.
               IF MONTH(TODAY) = 4 THEN ASSIGN faonr.FORDEL = FORDAONR.M3.
               IF MONTH(TODAY) = 5 THEN ASSIGN faonr.FORDEL = FORDAONR.M4.
               IF MONTH(TODAY) = 6 THEN ASSIGN faonr.FORDEL = FORDAONR.M5.
               IF MONTH(TODAY) = 7 THEN ASSIGN faonr.FORDEL = FORDAONR.M6.
               IF MONTH(TODAY) = 8 THEN ASSIGN faonr.FORDEL = FORDAONR.M7.
               IF MONTH(TODAY) = 9 THEN ASSIGN faonr.FORDEL = FORDAONR.M8.
               IF MONTH(TODAY) = 10 THEN ASSIGN faonr.FORDEL = FORDAONR.M9.
               IF MONTH(TODAY) = 11 THEN ASSIGN faonr.FORDEL = FORDAONR.M10.
               IF MONTH(TODAY) = 12 THEN ASSIGN faonr.FORDEL = FORDAONR.M11.
            END.   
         END.           
      END.
      ELSE DO:         
         OPEN QUERY fast FOR EACH INDEL WHERE INDEL.INDEL = RAD_LISTA 
         USE-INDEX INDEL NO-LOCK.
         GET FIRST fast.
         DO WHILE AVAILABLE(INDEL):
            FIND FIRST kontkod WHERE kontkod.KONTONR = INDEL.K2 USE-INDEX KNR NO-LOCK NO-ERROR. 
            IF AVAILABLE kontkod THEN DO:
               CREATE kkod.
               ASSIGN 
               kkod.KREC = RECID(kontkod)  
               kkod.KONTO = "K2"
               kkod.KONTONR = INDEL.K2  
               kkod.INDEL = INDEL.INDEL
               kkod.BENAMNING = kontkod.BENAMNING.                 
            END.   
            GET NEXT fast NO-LOCK.          
         END.   
         CLOSE QUERY fast.        
      END.   
   END.       
   IF kto = "K1" THEN DO:  
      FOR EACH kkod:                                  
         OPEN QUERY aonrq FOR EACH AONRKONTKOD WHERE AONRKONTKOD.K1 = kkod.KONTONR
         USE-INDEX K1K2K3K4K5 NO-LOCK, 
         EACH AONRTAB WHERE AONRTAB.AONR = AONRKONTKOD.AONR AND
         AONRTAB.DELNR = AONRKONTKOD.DELNR NO-LOCK.
         GET FIRST aonrq.
         DO WHILE AVAILABLE(AONRKONTKOD):
            FIND FIRST omrkonto WHERE omrkonto.OMRADE = AONRTAB.OMRADE NO-LOCK NO-ERROR.
            IF AVAILABLE omrkonto THEN DO:
               CREATE aonromr.
               ASSIGN    
               aonromr.AONRREC = RECID(AONRTAB)
               aonromr.AONR = AONRKONTKOD.AONR
               aonromr.DELNR = AONRKONTKOD.DELNR 
               aonromr.KONTONR = AONRKONTKOD.K1       
               aonromr.OMRADE = AONRTAB.OMRADE    
               aonromr.ORT = AONRTAB.ORT   
               aonromr.AONRAVDATUM = AONRTAB.AONRAVDATUM
               aonromr.SATS% =  AONRKONTKOD.SATS%
               aonromr.INDEL = kkod.INDEL
               aonromr.FASTAAONR = AONRTAB.FASTAAONR.                   
            END.   
            GET NEXT aonrq NO-LOCK.          
         END.   
         CLOSE QUERY aonrq.            
      END.        
   END.     
   IF kto = "K2" THEN DO:                   
      IF TOG_SAMMAN = FALSE AND TOG_FAST = TRUE AND RAD_LISTA = 7 THEN DO:
         FOR EACH faonr USE-INDEX FORD:
            FIND FIRST AONRTAB WHERE AONRTAB.AONR = faonr.AONR AND AONRTAB.DELNR =
            faonr.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.
            IF AVAILABLE AONRTAB THEN DO:
               IF AONRTAB.AONRAVDATUM > 01/01/91 AND valar > YEAR(AONRTAB.AONRAVDATUM) THEN musz = musz.
               ELSE DO:
                  CREATE aonromr.
                  ASSIGN    
                  aonromr.AONRREC = RECID(AONRTAB)
                  aonromr.AONR = faonr.AONR
                  aonromr.DELNR = faonr.DELNR               
                  aonromr.OMRADE = AONRTAB.OMRADE    
                  aonromr.ORT = AONRTAB.ORT        
                  aonromr.AONRAVDATUM = avdatum
                  aonromr.INDEL = 7           
                  aonromr.FASTAAONR = AONRTAB.FASTAAONR        
                  aonromr.FORDEL = faonr.FORDEL.                                    
                  IF aonromr.OMRADE = "" THEN aonromr.OMRADE = faonr.OMRADE.
                  IF YEAR(TODAY) > valar THEN ASSIGN aonromr.AONRAVDATUM = DATE(12,31,valar).             
               END.   
            END.   
            ELSE IF faonr.AONR BEGINS "0015" THEN DO:
               CREATE aonromr.
               ASSIGN                
               aonromr.AONR = faonr.AONR
               aonromr.DELNR = faonr.DELNR               
               aonromr.OMRADE = faonr.OMRADE    
               aonromr.ORT = "BILSERVICE"        
               aonromr.AONRAVDATUM = avdatum
               aonromr.INDEL = 7           
               aonromr.FASTAAONR = TRUE        
               aonromr.FORDEL = faonr.FORDEL.                                    
               IF aonromr.OMRADE = "" THEN aonromr.OMRADE = faonr.OMRADE.
               IF YEAR(TODAY) > valar THEN ASSIGN aonromr.AONRAVDATUM = DATE(12,31,valar).             
               FOR EACH AONRTAB WHERE AONRTAB.OMRADE = faonr.OMRADE
               AND AONRTAB.AONR BEGINS "0015" USE-INDEX OMRADE NO-LOCK:
                  CREATE aonromr.
                  ASSIGN                
                  aonromr.AONR = AONRTAB.AONR
                  aonromr.DELNR = AONRTAB.DELNR               
                  aonromr.OMRADE = faonr.OMRADE    
                  aonromr.ORT = "BILSERVICE"        
                  aonromr.AONRAVDATUM = avdatum
                  aonromr.INDEL = 7           
                  aonromr.FASTAAONR = TRUE        
                  aonromr.FORDEL = faonr.FORDEL.                                    
                  IF aonromr.OMRADE = "" THEN aonromr.OMRADE = faonr.OMRADE.
                  IF YEAR(TODAY) > valar THEN ASSIGN aonromr.AONRAVDATUM = DATE(12,31,valar).      
               END.   
            END.   
         END.
      END.
      ELSE DO:      
         FOR EACH kkod:                                  
            OPEN QUERY aonrq FOR EACH AONRKONTKOD WHERE AONRKONTKOD.K2 = kkod.KONTONR
            USE-INDEX K1K2K3K4K5 NO-LOCK, 
            EACH AONRTAB WHERE AONRTAB.AONR = AONRKONTKOD.AONR AND
            AONRTAB.DELNR = AONRKONTKOD.DELNR NO-LOCK.
            GET FIRST aonrq.
            DO WHILE AVAILABLE(AONRKONTKOD):
               FIND FIRST omrkonto WHERE omrkonto.OMRADE = AONRTAB.OMRADE NO-LOCK NO-ERROR.
               IF AVAILABLE omrkonto THEN DO:
                  CREATE aonromr.
                  ASSIGN    
                  aonromr.AONRREC = RECID(AONRTAB)
                  aonromr.AONR = AONRKONTKOD.AONR
                  aonromr.DELNR = AONRKONTKOD.DELNR 
                  aonromr.KONTONR = AONRKONTKOD.K2       
                  aonromr.OMRADE = AONRTAB.OMRADE    
                  aonromr.ORT = AONRTAB.ORT        
                  aonromr.AONRAVDATUM = AONRTAB.AONRAVDATUM
                  aonromr.SATS% =  AONRKONTKOD.SATS%
                  aonromr.INDEL = kkod.INDEL           
                  aonromr.FASTAAONR = AONRTAB.FASTAAONR.                      
               END.   
               GET NEXT aonrq NO-LOCK.          
            END.   
            CLOSE QUERY aonrq.            
         END.   
      END.        
   END.
   IF kto = "K3" THEN DO:  
      FOR EACH kkod:                                  
         OPEN QUERY aonrq FOR EACH AONRKONTKOD WHERE AONRKONTKOD.K3 = kkod.KONTONR
         USE-INDEX K1K2K3K4K5 NO-LOCK, 
         EACH AONRTAB WHERE AONRTAB.AONR = AONRKONTKOD.AONR AND
         AONRTAB.DELNR = AONRKONTKOD.DELNR NO-LOCK.
         GET FIRST aonrq.
         DO WHILE AVAILABLE(AONRKONTKOD):
            FIND FIRST omrkonto WHERE omrkonto.OMRADE = AONRTAB.OMRADE NO-LOCK NO-ERROR.
            IF AVAILABLE omrkonto THEN DO:
               CREATE aonromr.
               ASSIGN    
               aonromr.AONRREC = RECID(AONRTAB)
               aonromr.AONR = AONRKONTKOD.AONR
               aonromr.DELNR = AONRKONTKOD.DELNR 
               aonromr.KONTONR = AONRKONTKOD.K3       
               aonromr.OMRADE = AONRTAB.OMRADE    
               aonromr.ORT = AONRTAB.ORT   
               aonromr.AONRAVDATUM = AONRTAB.AONRAVDATUM
               aonromr.SATS% =  AONRKONTKOD.SATS%
               aonromr.INDEL = kkod.INDEL
               aonromr.FASTAAONR = AONRTAB.FASTAAONR.                   
            END.   
            GET NEXT aonrq NO-LOCK.          
         END.   
         CLOSE QUERY aonrq.            
      END.        
   END.
   IF kto = "K4" THEN DO:  
      FOR EACH kkod:                                  
         OPEN QUERY aonrq FOR EACH AONRKONTKOD WHERE AONRKONTKOD.K4 = kkod.KONTONR
         USE-INDEX K1K2K3K4K5 NO-LOCK, 
         EACH AONRTAB WHERE AONRTAB.AONR = AONRKONTKOD.AONR AND
         AONRTAB.DELNR = AONRKONTKOD.DELNR NO-LOCK.
         GET FIRST aonrq.
         DO WHILE AVAILABLE(AONRKONTKOD):
            FIND FIRST omrkonto WHERE omrkonto.OMRADE = AONRTAB.OMRADE NO-LOCK NO-ERROR.
            IF AVAILABLE omrkonto THEN DO:
               CREATE aonromr.
               ASSIGN    
               aonromr.AONRREC = RECID(AONRTAB)
               aonromr.AONR = AONRKONTKOD.AONR
               aonromr.DELNR = AONRKONTKOD.DELNR 
               aonromr.KONTONR = AONRKONTKOD.K4       
               aonromr.OMRADE = AONRTAB.OMRADE    
               aonromr.ORT = AONRTAB.ORT   
               aonromr.AONRAVDATUM = AONRTAB.AONRAVDATUM
               aonromr.SATS% =  AONRKONTKOD.SATS%
               aonromr.INDEL = kkod.INDEL
               aonromr.FASTAAONR = AONRTAB.FASTAAONR.                     
            END.   
            GET NEXT aonrq NO-LOCK.          
         END.   
         CLOSE QUERY aonrq.            
      END.        
   END.
   IF kto = "K5" THEN DO:  
      FOR EACH kkod:                                  
         OPEN QUERY aonrq FOR EACH AONRKONTKOD WHERE AONRKONTKOD.K5 = kkod.KONTONR
         USE-INDEX K1K2K3K4K5 NO-LOCK, 
         EACH AONRTAB WHERE AONRTAB.AONR = AONRKONTKOD.AONR AND
         AONRTAB.DELNR = AONRKONTKOD.DELNR NO-LOCK.
         GET FIRST aonrq.
         DO WHILE AVAILABLE(AONRKONTKOD):
            FIND FIRST omrkonto WHERE omrkonto.OMRADE = AONRTAB.OMRADE NO-LOCK NO-ERROR.
            IF AVAILABLE omrkonto THEN DO:
               CREATE aonromr.
               ASSIGN    
               aonromr.AONRREC = RECID(AONRTAB)
               aonromr.AONR = AONRKONTKOD.AONR
               aonromr.DELNR = AONRKONTKOD.DELNR 
               aonromr.KONTONR = AONRKONTKOD.K5       
               aonromr.OMRADE = AONRTAB.OMRADE    
               aonromr.ORT = AONRTAB.ORT   
               aonromr.AONRAVDATUM = AONRTAB.AONRAVDATUM
               aonromr.SATS% =  AONRKONTKOD.SATS%
               aonromr.INDEL = kkod.INDEL
               aonromr.FASTAAONR = AONRTAB.FASTAAONR.                     
            END.   
            GET NEXT aonrq NO-LOCK.          
         END.   
         CLOSE QUERY aonrq.            
      END.        
   END.        
   IF TOG_SAMMAN = FALSE AND RAD_LISTA = 7 THEN DO:
      FOR EACH aonromr:
        FIND FIRST omrkonto WHERE omrkonto.OMRADE = aonromr.OMRADE NO-LOCK NO-ERROR.
        IF NOT AVAILABLE omrkonto THEN DELETE aonromr.
      END.
   END.   
   ELSE DO:
      FOR EACH aonromr:
         IF aonromr.FASTAAONR = TRUE THEN DELETE aonromr.
      END.
   END.     
   IF RAD_PERIOD = 2 THEN DO:
      FOR EACH aonromr:
         IF aonromr.AONRAVDATUM < bdatum OR aonromr.AONRAVDATUM > avdatum THEN DO:
            DELETE aonromr.
         END.
      END.
   END.     
   ELSE DO:
      FOR EACH aonromr:
         IF aonromr.AONRAVDATUM > 01/01/91 AND aonromr.AONRAVDATUM < bdatum THEN DO:
            DELETE aonromr.
         END.   
      END.
   END.     
   tot = 0.   
   FOR EACH tidut:
      DELETE tidut.
   END.  
   str=
"=====================================================================================".      
   RUN huvud_UI.  
   OPEN QUERY saonr FOR EACH aonromr USE-INDEX AONR NO-LOCK.
   GET FIRST saonr NO-LOCK.
   DO WHILE AVAILABLE(aonromr):                
       IF TOG_SAMMAN = FALSE AND RAD_LISTA = 7 THEN fdeln = aonromr.FORDEL / 100.
       ELSE fdeln = 1. 
          CREATE suaonr.
          ASSIGN          
          suaonr.AONR = aonromr.AONR
          suaonr.DELNR = aonromr.DELNR
/*          suaonr.FORETAG = "GRIT"*/
          suaonr.OMRADE = aonromr.OMRADE
          suaonr.AONRAVDATUM = aonromr.AONRAVDATUM
          suaonr.PEA = 0
          suaonr.MTRLK = 0           
          suaonr.PLANMASK = 0            
          suaonr.PLANMONT = 0
          suaonr.BERTIM = 0
          suaonr.SATS% = aonromr.SATS%
          suaonr.INDEL = aonromr.INDEL
          suaonr.MONTKOST = 0   
          suaonr.MASKKOST = 0
          suaonr.PEA2 = 0
          suaonr.MTRLK2 = 0           
          suaonr.PLANMASK2 = 0           
          suaonr.PLANMONT2 = 0        
          suaonr.BERTIM2 = 0
          suaonr.MONTKOST2 = 0   
          suaonr.MASKKOST2 = 0
          suaonr.PEA3 = 0
          suaonr.MTRLK3 = 0           
          suaonr.PLANMASK3 = 0            
          suaonr.PLANMONT3 = 0        
          suaonr.BERTIM3 = 0
          suaonr.MONTKOST3 = 0   
          suaonr.MASKKOST3 = 0.  /*OBS HÅRDKODAT*/
          GET NEXT saonr NO-LOCK. 
    /*   END.   */
   END.
   CLOSE QUERY saonr.     
   OPEN QUERY kguru FOR EACH suaonr USE-INDEX AONR NO-LOCK.
   GET FIRST kguru NO-LOCK.
   DO WHILE AVAILABLE(suaonr):                            
       OPEN QUERY kalkq FOR EACH KALKNATT WHERE KALKNATT.AONR = suaonr.AONR AND
       KALKNATT.DELNR = suaonr.DELNR USE-INDEX AONR NO-LOCK.
       GET FIRST kalkq NO-LOCK.
       IF NOT AVAILABLE KALKNATT THEN DO:
          ASSIGN
          suaonr.MASKKOST2 = 0
          suaonr.MONTKOST2 = 0
          suaonr.PLANMONT2 = 0
          suaonr.BERTIM2 = 0
          suaonr.PLANMASK2 = 0
          suaonr.OVRIGKOST2 = 0
          suaonr.PEA2 = 0          
          suaonr.MTRLK2 = 0
          suaonr.MASKKOST3 = 0
          suaonr.MONTKOST3 = 0
          suaonr.PLANMONT3 = 0
          suaonr.BERTIM3 = 0
          suaonr.PLANMASK3 = 0
          suaonr.OVRIGKOST3 = 0
          suaonr.PEA3 = 0          
          suaonr.MTRLK3 = 0.

       END.
       DO WHILE AVAILABLE(KALKNATT):
          IF KALKNATT.TYP = 1 THEN DO:
             ASSIGN                   
             suaonr.MASKKOST = suaonr.MASKKOST + KALKNATT.MASKINKOST
             suaonr.MONTKOST = suaonr.MONTKOST  + KALKNATT.ARBETSKOST
             suaonr.PLANMONT = suaonr.PLANMONT  + KALKNATT.PLANMONT
             suaonr.BERTIM = suaonr.BERTIM  + KALKNATT.PLANBERE
             suaonr.PLANMASK = suaonr.PLANMASK  + KALKNATT.PLANMASK    
             suaonr.OVRIGKOST =  KALKNATT.OVRIGKOST
             suaonr.PEA = suaonr.PEA + KALKNATT.EA
             suaonr.KREG = "JA"
             suaonr.P1 = TRUE
             suaonr.MTRLK = suaonr.MTRLK + KALKNATT.MTRLKOST.
          END.
          ELSE IF KALKNATT.TYP = 2 THEN DO:    
             ASSIGN                   
             suaonr.MASKKOST2 = suaonr.MASKKOST2 + KALKNATT.MASKINKOST
             suaonr.MONTKOST2 = suaonr.MONTKOST2  + KALKNATT.ARBETSKOST
             suaonr.PLANMONT2 = suaonr.PLANMONT2  + KALKNATT.PLANMONT
             suaonr.BERTIM2 = suaonr.BERTIM2  + KALKNATT.PLANBERE
             suaonr.PLANMASK2 = suaonr.PLANMASK2  + KALKNATT.PLANMASK    
             suaonr.OVRIGKOST2 =  KALKNATT.OVRIGKOST
             suaonr.PEA2 = suaonr.PEA2 + KALKNATT.EA
             suaonr.KREG = "JA"
             suaonr.P2 = TRUE
             suaonr.MTRLK2 = suaonr.MTRLK2 + KALKNATT.MTRLKOST.       
          END.       
          ELSE IF KALKNATT.TYP = 3 THEN DO:    
             ASSIGN                   
             suaonr.MASKKOST3 = suaonr.MASKKOST3 + KALKNATT.MASKINKOST
             suaonr.MONTKOST3 = suaonr.MONTKOST3  + KALKNATT.ARBETSKOST
             suaonr.PLANMONT3 = suaonr.PLANMONT3  + KALKNATT.PLANMONT
             suaonr.BERTIM3 = suaonr.BERTIM3  + KALKNATT.PLANBERE
             suaonr.PLANMASK3 = suaonr.PLANMASK3  + KALKNATT.PLANMASK    
             suaonr.OVRIGKOST3 =  KALKNATT.OVRIGKOST
             suaonr.PEA3 = suaonr.PEA3 + KALKNATT.EA
             suaonr.KREG = "JA"
             suaonr.P3 = TRUE
             suaonr.MTRLK3 = suaonr.MTRLK3 + KALKNATT.MTRLKOST.       
          END.       
          GET NEXT kalkq NO-LOCK.
       END. 
       GET NEXT kguru NO-LOCK. 
   END.
   CLOSE QUERY kguru.
   CLOSE QUERY kalkq.
   FOR EACH suaonr:
      IF suaonr.P2 = FALSE THEN DO:
         ASSIGN
         suaonr.MASKKOST2 = 0
         suaonr.MONTKOST2 = 0
         suaonr.PLANMONT2 = 0
         suaonr.BERTIM2 = 0
         suaonr.PLANMASK2 = 0
         suaonr.OVRIGKOST2 = 0
         suaonr.PEA2 = 0          
         suaonr.MTRLK2 = 0.         
      END.
      IF suaonr.P3 = FALSE THEN DO:
         ASSIGN
         suaonr.MASKKOST3 = 0
         suaonr.MONTKOST3 = 0
         suaonr.PLANMONT3 = 0
         suaonr.BERTIM3 = 0
         suaonr.PLANMASK3 = 0
         suaonr.OVRIGKOST3 = 0
         suaonr.PEA3 = 0          
         suaonr.MTRLK3 = 0.         
      END.
      IF suaonr.P1 = FALSE AND (suaonr.P2 = TRUE OR suaonr.P3 = TRUE) THEN DO:
         ASSIGN
         suaonr.MASKKOST = 0
         suaonr.MONTKOST = 0
         suaonr.PLANMONT = 0
         suaonr.BERTIM = 0
         suaonr.PLANMASK = 0
         suaonr.OVRIGKOST = 0
         suaonr.PEA = 0          
         suaonr.MTRLK = 0.         
      END.
   END.            
   FOR EACH suaonr:
      hjsum = suaonr.MASKKOST3 + suaonr.MONTKOST3 + suaonr.OVRIGKOST2 + suaonr.MTRLK3.
      IF hjsum > 0 THEN DO:
         ASSIGN
         suaonr.MASKKOSTS = suaonr.MASKKOST3
         suaonr.MONTKOSTS = suaonr.MONTKOST3
         suaonr.PLANMONTS = suaonr.PLANMONT3
         suaonr.BERTIMS = suaonr.BERTIM3
         suaonr.PLANMASKS = suaonr.PLANMASK3    
         suaonr.OVRIGKOSTS = suaonr.OVRIGKOST3
         suaonr.PEAS = suaonr.PEA3        
         suaonr.MTRLKS = suaonr.MTRLK3.
      END.
      ELSE DO:
         hjsum = suaonr.MASKKOST2 + suaonr.MONTKOST2 + suaonr.OVRIGKOST2 + suaonr.MTRLK2.
         IF hjsum > 0 THEN DO:
            ASSIGN
            suaonr.MASKKOSTS = suaonr.MASKKOST2
            suaonr.MONTKOSTS = suaonr.MONTKOST2
            suaonr.PLANMONTS = suaonr.PLANMONT2
            suaonr.BERTIMS = suaonr.BERTIM2
            suaonr.PLANMASKS = suaonr.PLANMASK2    
            suaonr.OVRIGKOSTS = suaonr.OVRIGKOST2
            suaonr.PEAS = suaonr.PEA2        
            suaonr.MTRLKS = suaonr.MTRLK2.
         END.
         ELSE DO:
            ASSIGN
            suaonr.MASKKOSTS = suaonr.MASKKOST
            suaonr.MONTKOSTS = suaonr.MONTKOST
            suaonr.PLANMONTS = suaonr.PLANMONT
            suaonr.BERTIMS = suaonr.BERTIM
            suaonr.PLANMASKS = suaonr.PLANMASK    
            suaonr.OVRIGKOSTS = suaonr.OVRIGKOST
            suaonr.PEAS = suaonr.PEA        
            suaonr.MTRLKS = suaonr.MTRLK.
         END.
      END.   
   END.         
   
/*   OPEN QUERY sar FOR EACH suaonr USE-INDEX AONR NO-LOCK, 
   EACH KOSTREG WHERE KOSTREG.AONR = suaonr.AONR AND
   KOSTREG.DELNR = suaonr.DELNR USE-INDEX KOST NO-LOCK.
   GET FIRST sar NO-LOCK.
   DO WHILE AVAILABLE(suaonr):                      
       /*  OBS! TA BORT KOSTREG.PERSKOST -98 TRAKTKOST */
       ASSIGN                   
/*       suaonr.MASKKOST = suaonr.MASKKOST + KOSTREG.TRAKTKOST
       suaonr.MONTKOST = suaonr.MONTKOST  + KOSTREG.PERSKOST
       suaonr.PLANMONT = ( KOSTREG.PERSKOST / mopris ) + suaonr.PLANMONT   
       suaonr.PLANMASK = ( KOSTREG.TRAKTKOST / mapris) + suaonr.PLANMASK      
       suaonr.PEA = suaonr.PEA + ( KOSTREG.PERSKOST / mopris ) + ( KOSTREG.TRAKTKOST / ktfakt)*/
       suaonr.KREG = "JA"
       suaonr.MTRLK = suaonr.MTRLK + KOSTREG.MTRL.
       GET NEXT sar NO-LOCK. 
   END.
   CLOSE QUERY sar.        */
   OPEN QUERY arest FOR EACH aonromr USE-INDEX AONR NO-LOCK, 
   EACH AORESTRAPP WHERE AORESTRAPP.AONR = aonromr.AONR AND
   AORESTRAPP.DELNR = aonromr.DELNR USE-INDEX AONR NO-LOCK.
   GET FIRST arest NO-LOCK.
   DO WHILE AVAILABLE(aonromr):                      
      CREATE rest.
       ASSIGN          
       rest.AONR = AORESTRAPP.AONR
       rest.DELNR = AORESTRAPP.DELNR
       rest.AONRAVDATUM = AORESTRAPP.AONRAVDATUM
       rest.EA = AORESTRAPP.EA
       rest.MTRLK = AORESTRAPP.MTRLK
       rest.MASKKOST = AORESTRAPP.PLANMASK  
      /* rest.PLANMASK = ( AORESTRAPP.PLANMASK  / mapris )*/
       rest.PLANMONT = AORESTRAPP.PLANMONT        
       rest.MONTKOST = ( AORESTRAPP.PLANMONT * mopris )
       rest.DATUM = AORESTRAPP.DATUM.
       GET NEXT arest NO-LOCK. 
   END.
   CLOSE QUERY arest.        
   FOR EACH suaonr USE-INDEX AONR: 
      FIND FIRST rest WHERE rest.AONR = suaonr.AONR AND rest.DELNR = suaonr.DELNR
      USE-INDEX AONR NO-ERROR.
      IF AVAILABLE rest THEN DO:
         ASSIGN
         suaonr.RREG = "JA".
         FIND FIRST rest WHERE rest.AONR = suaonr.AONR AND rest.DELNR = suaonr.DELNR
         AND YEAR(rest.DATUM) = YEAR(bdatum) USE-INDEX AONR NO-ERROR.
         IF AVAILABLE rest THEN DO:
            IF rest.EA = 1 THEN DO:
               ASSIGN 
               suaonr.PLANMONT = 0
               suaonr.MONTKOST = 0
               suaonr.MASKKOST = 0
               suaonr.PLANMASK = 0.
            END.   
            ELSE DO:   
               ASSIGN 
               suaonr.PLANMONT = rest.PLANMONT    
               suaonr.MONTKOST = rest.MONTKOST  
               suaonr.MASKKOST = rest.MASKKOST
               suaonr.PLANMASK = rest.MASKKOST / mapris.
            END.               
            ASSIGN
            suaonr.MTRLK = rest.MTRLK  
            suaonr.PEA = rest.EA
            suaonr.AONRAVDATUM = rest.DATUM.
         END.   
         ELSE DO:
            ASSIGN
            plmont = 0
            plmask = 0
            plmtrl = 0
            plmkost = 0
            plea = 0.
            FOR EACH rest WHERE rest.AONR = suaonr.AONR AND rest.DELNR = suaonr.DELNR
            USE-INDEX AONR:
               IF rest.DATUM < bdatum THEN DO:
                  IF  rest.EA = 1 THEN bdatum = bdatum.
                  ELSE DO:
                     ASSIGN
                     plmont = plmont + rest.PLANMONT
                     plmask = plmask + ( rest.MASKKOST / mapris )
                     plmkost = plmkost + rest.MASKKOST                 
                     plmtrl = plmtrl + rest.MTRLK   
                     plea = plea + rest.EA.
                  END.   
               END.
            END.  
            IF plmont < suaonr.PLANMONT THEN ASSIGN suaonr.PLANMONT = suaonr.PLANMONT - plmont.
            ELSE ASSIGN suaonr.PLANMONT = 0.
            IF plmask < suaonr.PLANMASK  THEN ASSIGN suaonr.PLANMASK = suaonr.PLANMASK - plmask.
            ELSE ASSIGN suaonr.PLANMASK = 0.     
            IF plmkost < suaonr.MASKKOST  THEN ASSIGN suaonr.MASKKOST = suaonr.MASKKOST - plmkost.
            ELSE ASSIGN suaonr.PLANMASK = 0.
            IF plmtrl < suaonr.MTRLK THEN ASSIGN suaonr.MTRLK = suaonr.MTRLK - plmtrl.
            ELSE ASSIGN suaonr.MTRLK = 0.            
            ASSIGN suaonr.PEA = suaonr.PEA - plea.                        
         END. 
         /*ELSE DO:
            ASSIGN
            plmont = 0
            plmask = 0
            plmtrl = 0
            plea = 0.
            FOR EACH rest WHERE rest.AONR = suaonr.AONR AND rest.DELNR = suaonr.DELNR
            USE-INDEX AONR:
               IF rest.DATUM < bdatum THEN DO:
                  IF  rest.EA = 1 THEN bdatum = bdatum.
                  ELSE DO:
                     ASSIGN
                     plmont = plmont + rest.PLANMONT
                     plmask = plmask + rest.PLANMASK
                     plmtrl = plmtrl + rest.MTRLK   
                     plea = plea + rest.EA.
                  END.   
               END.
            END.
            ASSIGN
            suaonr.PLANMONT = plmont
            suaonr.PLANMASK = plmask
            suaonr.MTRLK = plmtrl
            suaonr.PEA = plea.               
         END. */  
      END.   
   END.                                                              
   RUN summa_UI.    
/* **********************  Internal Procedures  *********************** */


PROCEDURE allval1_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/                                    
   RAKNARE=0.
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,1) = "VALDA " + CAPS(Guru.Konstanter:gomrl) + " / ORGANISATIONER". 
   CREATE tidut.
   ASSIGN 
   SUBSTRING(tidut.UT,1) = "==============================".
   FOR EACH omrkonto USE-INDEX OMRADE:
      CREATE tidut.                                  
      ASSIGN 
      SUBSTRING(tidut.UT,1) = omrkonto.OMRADE
      SUBSTRING(tidut.UT,10) = omrkonto.NAMN.           
      raknare = raknare + 1.
   END.  
   CREATE tidut.                  
   IF TOG_SAMMAN = TRUE THEN DO:
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "SAMMANSTÄLLNINGSLISTA".
   END.  
   ELSE IF TOG_FAST = TRUE THEN DO:
      IF RAD_LISTA = 1 THEN DO:
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "INVESTERING  3200 3400".
      END.
      ELSE IF RAD_LISTA = 2 THEN DO:
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "REINVESTERING 3300".
      END.
      ELSE IF RAD_LISTA = 3 THEN DO:
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "UNDERHÅLL 3050".
      END.
      ELSE IF RAD_LISTA = 4 THEN DO:
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "DRIFT 3000".
      END.
      ELSE IF RAD_LISTA = 5 THEN DO:
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "MÄTVÄRDESINSAMLING 4700".
      END.
      ELSE IF RAD_LISTA = 6 THEN DO:
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "EXTERNA TJÄNSTER  2100 2400 2500".
      END.   
      ELSE IF RAD_LISTA = 7 THEN DO:
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "FASTA " + CAPS(Guru.Konstanter:gaok).
      END.        
   END.
   ELSE DO:       
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "VALDA KONTON". 
      CREATE tidut.
      ASSIGN 
      SUBSTRING(tidut.UT,1) = "============".
      FOR EACH kkod USE-INDEX KNR:
         CREATE tidut.                                  
         ASSIGN 
         SUBSTRING(tidut.UT,1) = kkod.KONTONR
         SUBSTRING(tidut.UT,10) = kkod.BENAMNING.           
      END.     
   END.         
END PROCEDURE.


PROCEDURE allval21_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   RUN allval1_UI.
  /* FIND FIRST samsum NO-LOCK NO-ERROR.
   IF AVAILABLE samsum THEN DO:*/
      ASSIGN    
      vomr = " "   
      raknare1 = 0
      forsta = " ".         
      FOR EACH samsum BREAK BY samsum.OMRADE BY samsum.INDEL: 
         vomr1 = samsum.OMRADE.
         FIND FIRST suindel WHERE suindel.INDEL = samsum.INDEL AND
         suindel.OMRADE = samsum.OMRADE NO-LOCK NO-ERROR.        
         IF AVAILABLE suindel THEN DO:
            IF samsum.OMRADE NE vomr AND vomr NE "" THEN DO:
               FIND FIRST slutsum1 WHERE slutsum1.OMRADE = vomr NO-LOCK
               NO-ERROR.
               IF AVAILABLE slutsum1 THEN RUN rubrik2_UI.
            END.  
            IF samsum.OMRADE NE vomr THEN DO:
               RUN omrade_UI.
               RUN rubrik1_UI.
            END. 
            RUN sort2_UI.
         END.   
      END.   
      FIND FIRST slutsum1 WHERE slutsum1.OMRADE = vomr NO-LOCK
      NO-ERROR.
      IF AVAILABLE slutsum1 THEN RUN rubrik2_UI.   
      vomr1 = " ".
      IF raknare1 > 1 THEN DO:
         RUN omrade_UI.
         RUN rubrik1_UI.
         FOR EACH samsum1 BREAK BY samsum.INDEL: 
            FIND FIRST suindel1 WHERE suindel1.INDEL = samsum1.INDEL NO-LOCK NO-ERROR.        
            IF AVAILABLE suindel1 THEN DO:
               RUN sort3_UI.
            END.   
         END.                     
         RUN totsum_UI.
      END.   
  
/*   END.
   ELSE DO:
      MESSAGE "DET FINNS INGA " + CAPS(Guru.Konstanter:gaok) + " ATT VISA" VIEW-AS ALERT-BOX.
      musz = TRUE.
      RETURN.
   END.                                        */
END PROCEDURE.


PROCEDURE sort3_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
    CREATE tidut.                        
   ASSIGN             
   SUBSTRING(tidut.UT,1) = samsum1.INNAMN
   SUBSTRING(tidut.UT,22) = "" + CAPS(Guru.Konstanter:gplk) + "/ "
/*   SUBSTRING(tidut.UT,34) = STRING(suindel1.PEA,">>>>9")*/
   SUBSTRING(tidut.UT,34) = STRING(suindel1.PLANMONT,">>>>9")   
   SUBSTRING(tidut.UT,40) = STRING(suindel1.MASKKOST,">>>>>>9")
   SUBSTRING(tidut.UT,48) = STRING(suindel1.PLANMASK,">>>>9")
   SUBSTRING(tidut.UT,55) = STRING(suindel1.MTRLK,">>>>>>9")
   SUBSTRING(tidut.UT,63) = STRING(suindel1.MONTKOST,">>>>>>>>9")
   SUBSTRING(tidut.UT,73) = STRING(suindel1.OVRIGKOST,">>>>>>>>9")
   SUBSTRING(tidut.UT,83) = STRING(suindel1.PLANMASK * mapris + (suindel1.PLANMONT * mopris) + suindel1.MTRLK + suindel1.OVRIGKOST,">>>>>>>>9" ).
   CREATE tidut. 
   ASSIGN                          
   SUBSTRING(tidut.UT,1) = samsum1.INNAMN
   SUBSTRING(tidut.UT,22) = "UTFALL"
/*   SUBSTRING(tidut.UT,34) = STRING(samsum1.UEA,">>>>9")
   SUBSTRING(tidut.UT,40) = STRING(samsum1.RESULTAT,"99.99")*/
   SUBSTRING(tidut.UT,34) = STRING(samsum1.TIMMAR,">>>>9")
   SUBSTRING(tidut.UT,40) = STRING(samsum1.MBELOPP,">>>>>>9") 
   SUBSTRING(tidut.UT,48) = STRING(samsum1.MTIMMAR,">>>>9") 
   SUBSTRING(tidut.UT,63) = STRING(samsum1.BELOPP,">>>>>>>>9") 
   SUBSTRING(tidut.UT,83) = STRING((samsum1.MBELOPP +  samsum1.BELOPP),">>>>>>>>9").
/*   SUBSTRING(tidut.UT,106) = STRING(samsum1.BERTIM,">>>>9").*/
   CREATE tidut.    
END PROCEDURE.


PROCEDURE allval2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   RUN allval1_UI.
  /* FIND FIRST slutsum NO-LOCK NO-ERROR.
   IF AVAILABLE slutsum THEN DO:*/
      ASSIGN    
      vomr = " "   
      raknare1 = 0
      forsta = " ".      
      FOR EACH slutsum BREAK BY slutsum.OMRADE BY slutsum.AONR: 
         vomr1 = slutsum.OMRADE.
         FIND FIRST suaonr WHERE suaonr.AONR = slutsum.AONR AND
         suaonr.DELNR = slutsum.DELNR NO-LOCK NO-ERROR.        
         IF AVAILABLE suaonr THEN DO:
            IF slutsum.OMRADE NE vomr AND vomr NE "" THEN DO:
               FIND FIRST slutsum1 WHERE slutsum1.OMRADE = vomr NO-LOCK
               NO-ERROR.
               IF AVAILABLE slutsum1 THEN RUN rubrik2_UI.
            END.  
            IF slutsum.OMRADE NE vomr THEN DO:
               RUN omrade_UI.
               RUN rubrik1_UI.
            END. 
            RUN sort1_UI.
         END.   
      END.                                
      RUN totsum_UI.
  
 /*END.
   ELSE DO:
      MESSAGE "DET FINNS INGA " + CAPS(Guru.Konstanter:gaok) + " ATT VISA" VIEW-AS ALERT-BOX.
      musz = TRUE.
      RETURN.
   END. */                                       
END PROCEDURE.


PROCEDURE huvud_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
     /*HUVUD*/ 
   DO TRANSACTION:   
      
      IF musz = TRUE THEN musz = musz.   
      ELSE DO:          
         CREATE tidut. 
         SUBSTRING(tidut.UT,60) = STRING(TODAY) + " " + STRING(TIME,"HH:MM").           
         CREATE tidut.                      
         ASSIGN
         SUBSTRING(tidut.UT,1) = "UPPFÖLJNING " + CAPS(Guru.Konstanter:gplk) + "-UTFALL".    
         CREATE tidut.                      
         ASSIGN
         SUBSTRING(tidut.UT,1) = "========================".
         IF RAD_PERIOD = 1 THEN DO:      
            ASSIGN
            SUBSTRING(tidut.UT,34) = "ÅR" 
            SUBSTRING(tidut.UT,37) = STRING(YEAR(bdatum),"9999").
         END.
         IF RAD_PERIOD = 2 THEN DO:
            ASSIGN
            SUBSTRING(tidut.UT,34) = Guru.Konstanter:gaok + " avslutade inom period" 
            SUBSTRING(tidut.UT,63) = STRING(bdatum) + " - " + STRING(avdatum).     
         END.                   
         CREATE tidut.                                                                                                            
      END.
   END.                    
END PROCEDURE.


PROCEDURE lontill2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/ 
   FOR EACH dagtemp WHERE dagtemp.AONRAVDATUM > 01/01/91 USE-INDEX AONR:
      FIND FIRST suaonr WHERE suaonr.AONR = dagtemp.AONR  AND
      suaonr.DELNR = dagtemp.DELNR USE-INDEX AONR NO-ERROR.           
      IF NOT AVAILABLE suaonr THEN DO:
         DELETE dagtemp.
      END.
   END.             
   ASSIGN  
   arrhjsum = 0
   arrhjmt  = 0
   arrhjsumb = 0
   arrhjmb = 0
   arrhjovr = 0
   arrhjber = 0.
   FOR EACH dagtemp WHERE dagtemp.AONRAVDATUM > 01/01/91 BREAK BY dagtemp.OMRADE BY dagtemp.INDEL :    
      ACCUMULATE dagtemp.TIMMAR(TOTAL BY dagtemp.OMRADE BY dagtemp.INDEL).   
      ACCUMULATE dagtemp.BERTIM(TOTAL BY dagtemp.OMRADE BY dagtemp.INDEL).
      ACCUMULATE dagtemp.MTIMMAR(TOTAL BY dagtemp.OMRADE BY dagtemp.INDEL).
      ACCUMULATE dagtemp.BELOPP(TOTAL  BY dagtemp.OMRADE BY dagtemp.INDEL).  
      ACCUMULATE dagtemp.MBELOPP(TOTAL BY dagtemp.OMRADE BY dagtemp.INDEL).         
      ACCUMULATE dagtemp.OVRIGKOST(TOTAL BY dagtemp.OMRADE BY dagtemp.INDEL).
      IF LAST-OF(dagtemp.INDEL) THEN DO:
         CREATE samsum.                    
         ASSIGN          
         samsum.INDEL = dagtemp.INDEL                               
         samsum.OMRADE = dagtemp.OMRADE
         samsum.SATS% = dagtemp.SATS%             
         samsum.TIMMAR = (ACCUM TOTAL dagtemp.TIMMAR) - arrhjsum                  
         arrhjsum = ACCUM TOTAL dagtemp.TIMMAR    
         samsum.BERTIM = (ACCUM TOTAL dagtemp.BERTIM) - arrhjber                  
         arrhjber = ACCUM TOTAL dagtemp.BERTIM
         samsum.MTIMMAR = (ACCUM TOTAL dagtemp.MTIMMAR) - arrhjmt                  
         arrhjmt = ACCUM TOTAL dagtemp.MTIMMAR  
         samsum.BELOPP = (ACCUM TOTAL dagtemp.BELOPP) - arrhjsumb                  
         arrhjsumb = ACCUM TOTAL dagtemp.BELOPP
         samsum.MBELOPP = (ACCUM TOTAL dagtemp.MBELOPP) - arrhjmb                  
         arrhjmb = ACCUM TOTAL dagtemp.MBELOPP
         samsum.OVRIGKOST = (ACCUM TOTAL dagtemp.OVRIGKOST) - arrhjovr                  
         arrhjovr = ACCUM TOTAL dagtemp.OVRIGKOST.                                       
      END.
   END.     
   FOR EACH samsum USE-INDEX INDEL:                            
      ASSIGN
      samsum.UEA = samsum.TIMMAR +  ( samsum.MBELOPP / ktfakt ).     /*obs hårdkodat*/
      FIND FIRST suindel WHERE suindel.INDEL = samsum.INDEL AND  
      suindel.OMRADE = samsum.OMRADE USE-INDEX INDEL NO-LOCK NO-ERROR.
      IF AVAILABLE suindel THEN DO:
         ASSIGN samsum.RESULTAT = suindel.PEA / samsum.UEA
         samsum.INNAMN = suindel.INNAMN. 
      END.
      ELSE DO:
         DELETE samsum.
      END.      
   END.  
                  
   ASSIGN  
   arrhjsum = 0
   arrhjmt  = 0
   arrhjsumb = 0
   arrhjmb = 0
   arrhjuea = 0
   arrhjovr = 0
   arrhjber = 0.     
   FOR EACH samsum BREAK BY samsum.OMRADE:    
      ACCUMULATE samsum.TIMMAR(TOTAL BY samsum.OMRADE).   
      ACCUMULATE samsum.BERTIM(TOTAL BY samsum.OMRADE).
      ACCUMULATE samsum.MTIMMAR(TOTAL BY samsum.OMRADE).
      ACCUMULATE samsum.BELOPP(TOTAL BY samsum.OMRADE).  
      ACCUMULATE samsum.MBELOPP(TOTAL BY samsum.OMRADE).
      ACCUMULATE samsum.UEA(TOTAL BY samsum.OMRADE).  
      ACCUMULATE samsum.OVRIGKOST(TOTAL BY samsum.OMRADE).       
      IF LAST-OF(samsum.OMRADE) THEN DO:
         CREATE slutsum1.                    
         ASSIGN                                  
         slutsum1.OMRADE = samsum.OMRADE           
         slutsum1.TIMMAR = (ACCUM TOTAL samsum.TIMMAR) - arrhjsum                  
         arrhjsum = ACCUM TOTAL samsum.TIMMAR  
         slutsum1.BERTIM = (ACCUM TOTAL samsum.BERTIM) - arrhjber                  
         arrhjber = ACCUM TOTAL samsum.BERTIM  
         slutsum1.MTIMMAR = (ACCUM TOTAL samsum.MTIMMAR) - arrhjmt                  
         arrhjmt = ACCUM TOTAL samsum.MTIMMAR  
         slutsum1.BELOPP = (ACCUM TOTAL samsum.BELOPP) - arrhjsumb                  
         arrhjsumb = ACCUM TOTAL samsum.BELOPP
         slutsum1.MBELOPP = (ACCUM TOTAL samsum.MBELOPP) - arrhjmb                  
         arrhjmb = ACCUM TOTAL samsum.MBELOPP
         slutsum1.UEA = (ACCUM TOTAL samsum.UEA) - arrhjuea                  
         arrhjuea = ACCUM TOTAL samsum.UEA
         slutsum1.OVRIGKOST = (ACCUM TOTAL samsum.OVRIGKOST) - arrhjovr                  
         arrhjovr = ACCUM TOTAL samsum.OVRIGKOST.                                       
      END.
   END.   
   ASSIGN  
   arrhjsum = 0
   arrhjmt  = 0
   arrhjsumb = 0
   arrhjmb = 0
   arrhjuea = 0
   arrhjovr = 0
   arrhjber = 0.     
   FOR EACH samsum BREAK BY samsum.INDEL:    
      ACCUMULATE samsum.TIMMAR(TOTAL BY samsum.INDEL).   
      ACCUMULATE samsum.BERTIM(TOTAL BY samsum.INDEL).
      ACCUMULATE samsum.MTIMMAR(TOTAL BY samsum.INDEL).
      ACCUMULATE samsum.BELOPP(TOTAL BY samsum.INDEL).  
      ACCUMULATE samsum.MBELOPP(TOTAL BY samsum.INDEL).
      ACCUMULATE samsum.UEA(TOTAL BY samsum.INDEL).    
      ACCUMULATE samsum.OVRIGKOST(TOTAL BY samsum.INDEL).     
      IF LAST-OF(samsum.INDEL) THEN DO:
         CREATE samsum1.                    
         ASSIGN                                  
         samsum1.INDEL = samsum.INDEL   
         samsum1.INNAMN = samsum.INNAMN            
         samsum1.TIMMAR = (ACCUM TOTAL samsum.TIMMAR) - arrhjsum                  
         arrhjsum = ACCUM TOTAL samsum.TIMMAR  
         samsum1.BERTIM = (ACCUM TOTAL samsum.BERTIM) - arrhjber                  
         arrhjber = ACCUM TOTAL samsum.BERTIM  
         samsum1.MTIMMAR = (ACCUM TOTAL samsum.MTIMMAR) - arrhjmt                  
         arrhjmt = ACCUM TOTAL samsum.MTIMMAR  
         samsum1.BELOPP = (ACCUM TOTAL samsum.BELOPP) - arrhjsumb                  
         arrhjsumb = ACCUM TOTAL samsum.BELOPP
         samsum1.MBELOPP = (ACCUM TOTAL samsum.MBELOPP) - arrhjmb                  
         arrhjmb = ACCUM TOTAL samsum.MBELOPP
         samsum1.UEA = (ACCUM TOTAL samsum.UEA) - arrhjuea                  
         arrhjuea = ACCUM TOTAL samsum.UEA 
         samsum1.OVRIGKOST = (ACCUM TOTAL samsum.OVRIGKOST) - arrhjovr                  
         arrhjovr = ACCUM TOTAL samsum.OVRIGKOST.                                         
      END.
   END.                                                   
   FOR EACH slutsum1:    
      ACCUMULATE slutsum1.TIMMAR(TOTAL).   
      ACCUMULATE slutsum1.BERTIM(TOTAL).
      ACCUMULATE slutsum1.MTIMMAR(TOTAL).
      ACCUMULATE slutsum1.BELOPP(TOTAL).  
      ACCUMULATE slutsum1.MBELOPP(TOTAL).
      ACCUMULATE slutsum1.UEA(TOTAL). 
   END.              
   CREATE slutsum2.                    
   ASSIGN                                         
   slutsum2.TIMMAR = ACCUM TOTAL slutsum1.TIMMAR                        
   slutsum2.BERTIM = ACCUM TOTAL slutsum1.BERTIM
   slutsum2.MTIMMAR = ACCUM TOTAL slutsum1.MTIMMAR             
   slutsum2.BELOPP = ACCUM TOTAL slutsum1.BELOPP                         
   slutsum2.MBELOPP = ACCUM TOTAL slutsum1.MBELOPP                           
   slutsum2.UEA = ACCUM TOTAL slutsum1.UEA.                                                              
    
   ASSIGN  
   arrhjsum = 0
   arrhjmt  = 0
   arrhjsumb = 0
   arrhjmb = 0
   arrhjuea = 0
   arrhjmtrl = 0
   arrhjovr = 0
   arrhjber = 0.
   FOR EACH suindel BREAK BY suindel.OMRADE:    
      ACCUMULATE suindel.PLANMONT(TOTAL BY suindel.OMRADE).   
      ACCUMULATE suindel.BERTIM(TOTAL BY suindel.OMRADE).
      ACCUMULATE suindel.PLANMASK(TOTAL BY suindel.OMRADE).
      ACCUMULATE suindel.MONTKOST(TOTAL BY suindel.OMRADE).  
      ACCUMULATE suindel.MASKKOST(TOTAL BY suindel.OMRADE). 
      ACCUMULATE suindel.MTRLK(TOTAL BY suindel.OMRADE).      
      ACCUMULATE suindel.PEA(TOTAL BY suindel.OMRADE).
      ACCUMULATE suindel.OVRIGKOST(TOTAL BY suindel.OMRADE).         
      IF LAST-OF(suindel.OMRADE) THEN DO:
         CREATE suaonr1.                    
         ASSIGN                                  
         suaonr1.OMRADE = suindel.OMRADE           
         suaonr1.PLANMONT = (ACCUM TOTAL suindel.PLANMONT) - arrhjsum                  
         arrhjsum = ACCUM TOTAL suindel.PLANMONT   
         suaonr1.BERTIM = (ACCUM TOTAL suindel.BERTIM) - arrhjber                  
         arrhjber = ACCUM TOTAL suindel.BERTIM
         suaonr1.PLANMASK = (ACCUM TOTAL suindel.PLANMASK) - arrhjmt                  
         arrhjmt = ACCUM TOTAL suindel.PLANMASK  
         suaonr1.MONTKOST = (ACCUM TOTAL suindel.MONTKOST) - arrhjsumb                  
         arrhjsumb = ACCUM TOTAL suindel.MONTKOST
         suaonr1.MASKKOST = (ACCUM TOTAL suindel.MASKKOST) - arrhjmb                  
         arrhjmb = ACCUM TOTAL suindel.MASKKOST 
         suaonr1.MTRLK = (ACCUM TOTAL suindel.MTRLK) - arrhjmtrl                  
         arrhjmtrl = ACCUM TOTAL suindel.MTRLK
         suaonr1.PEA = (ACCUM TOTAL suindel.PEA) - arrhjuea                  
         arrhjuea = ACCUM TOTAL suindel.PEA
         suaonr1.OVRIGKOST = (ACCUM TOTAL suindel.OVRIGKOST) - arrhjovr                  
         arrhjovr = ACCUM TOTAL suindel.OVRIGKOST.                                        
      END.
   END.          
   ASSIGN  
   arrhjsum = 0
   arrhjmt  = 0
   arrhjsumb = 0
   arrhjmb = 0
   arrhjuea = 0
   arrhjovr = 0
   arrhjber = 0.   
   FOR EACH suindel BREAK BY suindel.INDEL:    
      ACCUMULATE suindel.PLANMONT(TOTAL BY suindel.INDEL).   
      ACCUMULATE suindel.BERTIM(TOTAL BY suindel.INDEL).
      ACCUMULATE suindel.PLANMASK(TOTAL BY suindel.INDEL).
      ACCUMULATE suindel.MONTKOST(TOTAL BY suindel.INDEL).  
      ACCUMULATE suindel.MASKKOST(TOTAL BY suindel.INDEL).
      ACCUMULATE suindel.PEA(TOTAL BY suindel.INDEL).    
      ACCUMULATE suindel.OVRIGKOST(TOTAL BY suindel.INDEL).      
      IF LAST-OF(suindel.INDEL) THEN DO:
         CREATE suindel1.                    
         ASSIGN                                  
         suindel1.INDEL = suindel.INDEL   
         suindel1.INNAMN = suindel.INNAMN          
         suindel1.PLANMONT = (ACCUM TOTAL suindel.PLANMONT) - arrhjsum                  
         arrhjsum = ACCUM TOTAL suindel.PLANMONT  
         suindel1.BERTIM = (ACCUM TOTAL suindel.BERTIM) - arrhjber                  
         arrhjber = ACCUM TOTAL suindel.BERTIM 
         suindel1.PLANMASK = (ACCUM TOTAL suindel.PLANMASK) - arrhjmt                  
         arrhjmt = ACCUM TOTAL suindel.PLANMASK  
         suindel1.MONTKOST = (ACCUM TOTAL suindel.MONTKOST) - arrhjsumb                  
         arrhjsumb = ACCUM TOTAL suindel.MONTKOST
         suindel1.MASKKOST = (ACCUM TOTAL suindel.MASKKOST) - arrhjmb                  
         arrhjmb = ACCUM TOTAL suindel.MASKKOST
         suindel1.PEA = (ACCUM TOTAL suindel.PEA) - arrhjuea                  
         arrhjuea = ACCUM TOTAL suindel.PEA      
         suindel1.OVRIGKOST = (ACCUM TOTAL suindel.OVRIGKOST) - arrhjovr                  
         arrhjovr = ACCUM TOTAL suindel.OVRIGKOST.                                     
      END.
   END.                                                                                                      
   FOR EACH samsum1 USE-INDEX INDEL:                            
      FIND FIRST suindel1 WHERE suindel1.INDEL = samsum1.INDEL USE-INDEX INDEL NO-LOCK NO-ERROR.
      IF AVAILABLE suindel1 THEN DO:
         ASSIGN samsum1.RESULTAT = suindel1.PEA / samsum1.UEA.
      END.
   END.   
   FOR EACH suaonr1:    
      ACCUMULATE suaonr1.PLANMONT(TOTAL).  
      ACCUMULATE suaonr1.BERTIM(TOTAL).     
      ACCUMULATE suaonr1.PLANMASK(TOTAL).
      ACCUMULATE suaonr1.MONTKOST(TOTAL).        
      ACCUMULATE suaonr1.MASKKOST(TOTAL).
      ACCUMULATE suaonr1.PEA(TOTAL).
      ACCUMULATE suaonr1.OVRIGKOST(TOTAL).
   END.               
   CREATE suaonr2.                    
   ASSIGN                                         
   suaonr2.PLANMONT = ACCUM TOTAL suaonr1.PLANMONT                       
   suaonr2.BERTIM = ACCUM TOTAL suaonr1.BERTIM
   suaonr2.PLANMASK = ACCUM TOTAL suaonr1.PLANMASK            
   suaonr2.MONTKOST = ACCUM TOTAL suaonr1.MONTKOST                        
   suaonr2.MASKKOST = ACCUM TOTAL suaonr1.MASKKOST                          
   suaonr2.PEA = ACCUM TOTAL suaonr1.PEA                                                              
   suaonr2.OVRIGKOST = ACCUM TOTAL suaonr1.OVRIGKOST.
   FOR EACH slutsum1:
      FIND FIRST suaonr1 WHERE suaonr1.OMRADE = slutsum1.OMRADE NO-ERROR.
      IF AVAILABLE suaonr1 THEN DO: 
         ASSIGN slutsum1.RESULTAT = suaonr1.PEA / slutsum1.UEA.
      END.   
   END.      
   FIND FIRST slutsum2 NO-ERROR.
   FIND FIRST suaonr2 NO-ERROR.
   IF AVAILABLE slutsum2 THEN DO:
      IF AVAILABLE suaonr2 THEN DO    :
         ASSIGN slutsum2.RESULTAT = suaonr2.PEA / slutsum2.UEA.
      END.   
   END.                    
END PROCEDURE.


PROCEDURE lontill_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   ASSIGN  
   arrhjsum = 0
   arrhjmt  = 0
   arrhjsumb = 0
   arrhjmb = 0   
   arrhjovr = 0
   arrhjber = 0
   arrhjmtrl = 0.
   FOR EACH dagtemp BREAK BY dagtemp.AONR BY dagtemp.DELNR:    
      ACCUMULATE dagtemp.TIMMAR(TOTAL BY dagtemp.AONR BY dagtemp.DELNR).  
      ACCUMULATE dagtemp.BERTIM(TOTAL BY dagtemp.AONR BY dagtemp.DELNR). 
      ACCUMULATE dagtemp.MTIMMAR(TOTAL BY dagtemp.AONR BY dagtemp.DELNR).
      ACCUMULATE dagtemp.BELOPP(TOTAL BY dagtemp.AONR BY dagtemp.DELNR).  
      ACCUMULATE dagtemp.MBELOPP(TOTAL BY dagtemp.AONR BY dagtemp.DELNR).  
      ACCUMULATE dagtemp.OVRIGKOST(TOTAL BY dagtemp.AONR BY dagtemp.DELNR).       
      ACCUMULATE dagtemp.MTRLK(TOTAL BY dagtemp.AONR BY dagtemp.DELNR).
      IF LAST-OF(dagtemp.DELNR) THEN DO:
         CREATE slutsum.                    
         ASSIGN          
         slutsum.AONR = dagtemp.AONR            
         slutsum.DELNR= dagtemp.DELNR       
         slutsum.KREG = dagtemp.KREG                                   
         slutsum.OMRADE = dagtemp.OMRADE
         slutsum.AONRAVDATUM = dagtemp.AONRAVDATUM 
         slutsum.ORT = dagtemp.ORT
         slutsum.SATS% = dagtemp.SATS%             
         slutsum.TIMMAR = (ACCUM TOTAL dagtemp.TIMMAR) - arrhjsum                  
         arrhjsum = ACCUM TOTAL dagtemp.TIMMAR    
         slutsum.BERTIM = (ACCUM TOTAL dagtemp.BERTIM) - arrhjber                  
         arrhjber = ACCUM TOTAL dagtemp.BERTIM
         slutsum.MTIMMAR = (ACCUM TOTAL dagtemp.MTIMMAR) - arrhjmt                  
         arrhjmt = ACCUM TOTAL dagtemp.MTIMMAR  
         slutsum.BELOPP = (ACCUM TOTAL dagtemp.BELOPP) - arrhjsumb                  
         arrhjsumb = ACCUM TOTAL dagtemp.BELOPP
         slutsum.MBELOPP = (ACCUM TOTAL dagtemp.MBELOPP) - arrhjmb                  
         arrhjmb = ACCUM TOTAL dagtemp.MBELOPP
         slutsum.OVRIGKOST = (ACCUM TOTAL dagtemp.OVRIGKOST) - arrhjovr                  
         arrhjovr = ACCUM TOTAL dagtemp.OVRIGKOST
         slutsum.MTRLK = (ACCUM TOTAL dagtemp.MTRLK) - arrhjmtrl                  
         arrhjmtrl = ACCUM TOTAL dagtemp.MTRLK.                                            
      END.
   END.             
                               
   FOR EACH slutsum:  
      ASSIGN
      slutsum.UEA = slutsum.TIMMAR +  ( slutsum.MBELOPP / ktfakt ).
      FIND FIRST suaonr WHERE suaonr.AONR = slutsum.AONR AND suaonr.DELNR = slutsum.DELNR
      EXCLUSIVE-LOCK NO-ERROR.                            
      IF NOT AVAILABLE suaonr THEN DO:
         CREATE slutsum5.  
         ASSIGN          
         slutsum5.AONR = slutsum.AONR            
         slutsum5.DELNR= slutsum.DELNR                              
         slutsum5.OMRADE = slutsum.OMRADE
         slutsum5.AONRAVDATUM = slutsum.AONRAVDATUM 
         slutsum5.ORT = slutsum.ORT
         slutsum5.SATS% = slutsum.SATS%             
         slutsum5.TIMMAR = slutsum.TIMMAR                      
         slutsum5.BERTIM = slutsum.BERTIM
         slutsum5.MTIMMAR = slutsum.MTIMMAR                 
         slutsum5.BELOPP = slutsum.BELOPP                  
         slutsum5.MBELOPP = slutsum.MBELOPP
         slutsum5.OVRIGKOST = slutsum.OVRIGKOST
         slutsum5.MTRLK = slutsum.MTRLK.              
         DELETE slutsum.
      END.   
      ELSE DO:         
         IF suaonr.PLANMONT = 0 AND suaonr.PLANMASK = 0 AND
         suaonr.MTRLK = 0 AND suaonr.OVRIGKOST = 0  THEN DO:
            CREATE slutsum5.  
            ASSIGN          
            slutsum5.AONR = slutsum.AONR            
            slutsum5.DELNR= slutsum.DELNR                              
            slutsum5.OMRADE = slutsum.OMRADE
            slutsum5.AONRAVDATUM = slutsum.AONRAVDATUM 
            slutsum5.ORT = slutsum.ORT
            slutsum5.SATS% = slutsum.SATS%             
            slutsum5.TIMMAR = slutsum.TIMMAR                      
            slutsum5.BERTIM = slutsum.BERTIM
            slutsum5.MTIMMAR = slutsum.MTIMMAR                 
            slutsum5.BELOPP = slutsum.BELOPP                  
            slutsum5.MBELOPP = slutsum.MBELOPP
            slutsum5.OVRIGKOST = slutsum.OVRIGKOST
            slutsum5.MTRLK = slutsum.MTRLK.              
            DELETE slutsum.
         END.   
         ELSE IF  slutsum.AONRAVDATUM > 01/01/1991 THEN DO:          
            IF  suaonr.PEA > 0 THEN DO:
               ASSIGN slutsum.RESULTAT = suaonr.PEA / slutsum.UEA. 
            END.
            ELSE IF suaonr.PEA2 > 0 THEN DO:
               ASSIGN slutsum.RESULTAT = suaonr.PEA2 / slutsum.UEA.
            END.      
         END.    
      END.      
   END.                                              
   ASSIGN  
   arrhjsum = 0
   arrhjmt  = 0
   arrhjsumb = 0
   arrhjmb = 0
   arrhjuea = 0
   arrhjovr = 0
   arrhjber = 0
   arrhjmtrl = 0.   
   FOR EACH slutsum WHERE slutsum.AONRAVDATUM > 01/01/91 BREAK BY slutsum.OMRADE:    
      ACCUMULATE slutsum.TIMMAR(TOTAL BY slutsum.OMRADE).   
      ACCUMULATE slutsum.BERTIM(TOTAL BY slutsum.OMRADE).
      ACCUMULATE slutsum.MTIMMAR(TOTAL BY slutsum.OMRADE).
      ACCUMULATE slutsum.BELOPP(TOTAL BY slutsum.OMRADE).  
      ACCUMULATE slutsum.MBELOPP(TOTAL BY slutsum.OMRADE).
      ACCUMULATE slutsum.UEA(TOTAL BY slutsum.OMRADE).    
      ACCUMULATE slutsum.OVRIGKOST(TOTAL BY slutsum.OMRADE). 
      ACCUMULATE slutsum.MTRLK(TOTAL BY slutsum.OMRADE).    
      IF LAST-OF(slutsum.OMRADE) THEN DO:
         CREATE slutsum1.                    
         ASSIGN                                  
         slutsum1.OMRADE = slutsum.OMRADE           
         slutsum1.TIMMAR = (ACCUM TOTAL slutsum.TIMMAR) - arrhjsum                  
         arrhjsum = ACCUM TOTAL slutsum.TIMMAR   
         slutsum1.BERTIM = (ACCUM TOTAL slutsum.BERTIM) - arrhjber                  
         arrhjber = ACCUM TOTAL slutsum.BERTIM  
         slutsum1.MTIMMAR = (ACCUM TOTAL slutsum.MTIMMAR) - arrhjmt                  
         arrhjmt = ACCUM TOTAL slutsum.MTIMMAR  
         slutsum1.BELOPP = (ACCUM TOTAL slutsum.BELOPP) - arrhjsumb                  
         arrhjsumb = ACCUM TOTAL slutsum.BELOPP
         slutsum1.MBELOPP = (ACCUM TOTAL slutsum.MBELOPP) - arrhjmb                  
         arrhjmb = ACCUM TOTAL slutsum.MBELOPP
         slutsum1.UEA = (ACCUM TOTAL slutsum.UEA) - arrhjuea                  
         arrhjuea = ACCUM TOTAL slutsum.UEA
         slutsum1.OVRIGKOST = (ACCUM TOTAL slutsum.OVRIGKOST) - arrhjovr                  
         arrhjovr = ACCUM TOTAL slutsum.OVRIGKOST
         slutsum1.MTRLK = (ACCUM TOTAL slutsum.MTRLK) - arrhjmtrl                  
         arrhjmtrl = ACCUM TOTAL slutsum.MTRLK.                                      
      END.
   END.                 
   ASSIGN                                                    
   arrhjsum = 0
   arrhjmt  = 0
   arrhjsumb = 0
   arrhjmb = 0
   arrhjuea = 0
   arrhjovr = 0
   arrhjber = 0
   arrhjmtrl = 0.   
   FOR EACH slutsum BREAK BY slutsum.OMRADE:    
      ACCUMULATE slutsum.TIMMAR(TOTAL BY slutsum.OMRADE).   
      ACCUMULATE slutsum.BERTIM(TOTAL BY slutsum.OMRADE).
      ACCUMULATE slutsum.MTIMMAR(TOTAL BY slutsum.OMRADE).
      ACCUMULATE slutsum.BELOPP(TOTAL BY slutsum.OMRADE).  
      ACCUMULATE slutsum.MBELOPP(TOTAL BY slutsum.OMRADE).
      ACCUMULATE slutsum.UEA(TOTAL BY slutsum.OMRADE). 
      ACCUMULATE slutsum.OVRIGKOST(TOTAL BY slutsum.OMRADE).        
      ACCUMULATE slutsum.MTRLK(TOTAL BY slutsum.OMRADE).
      IF LAST-OF(slutsum.OMRADE) THEN DO:
         CREATE slutsum3.                    
         ASSIGN                                  
         slutsum3.OMRADE = slutsum.OMRADE           
         slutsum3.TIMMAR = (ACCUM TOTAL slutsum.TIMMAR) - arrhjsum                  
         arrhjsum = ACCUM TOTAL slutsum.TIMMAR   
         slutsum3.BERTIM = (ACCUM TOTAL slutsum.BERTIM) - arrhjber                  
         arrhjber = ACCUM TOTAL slutsum.BERTIM 
         slutsum3.MTIMMAR = (ACCUM TOTAL slutsum.MTIMMAR) - arrhjmt                  
         arrhjmt = ACCUM TOTAL slutsum.MTIMMAR  
         slutsum3.BELOPP = (ACCUM TOTAL slutsum.BELOPP) - arrhjsumb                  
         arrhjsumb = ACCUM TOTAL slutsum.BELOPP
         slutsum3.MBELOPP = (ACCUM TOTAL slutsum.MBELOPP) - arrhjmb                  
         arrhjmb = ACCUM TOTAL slutsum.MBELOPP
         slutsum3.UEA = (ACCUM TOTAL slutsum.UEA) - arrhjuea                  
         arrhjuea = ACCUM TOTAL slutsum.UEA
         slutsum3.OVRIGKOST = (ACCUM TOTAL slutsum.OVRIGKOST) - arrhjovr                  
         arrhjovr = ACCUM TOTAL slutsum.OVRIGKOST
         slutsum3.MTRLK = (ACCUM TOTAL slutsum.MTRLK) - arrhjmtrl                  
         arrhjmtrl = ACCUM TOTAL slutsum.MTRLK.                                          
      END.
   END.
   FOR EACH slutsum1:    
      ACCUMULATE slutsum1.TIMMAR(TOTAL).   
      ACCUMULATE slutsum1.BERTIM(TOTAL).
      ACCUMULATE slutsum1.MTIMMAR(TOTAL).
      ACCUMULATE slutsum1.BELOPP(TOTAL).  
      ACCUMULATE slutsum1.MBELOPP(TOTAL).
      ACCUMULATE slutsum1.UEA(TOTAL).  
      ACCUMULATE slutsum1.OVRIGKOST(TOTAL).          
      ACCUMULATE slutsum1.MTRLK(TOTAL).
   END.   
      CREATE slutsum2.                    
      ASSIGN                                         
      slutsum2.TIMMAR = ACCUM TOTAL slutsum1.TIMMAR 
      slutsum2.BERTIM = ACCUM TOTAL slutsum1.BERTIM                       
      slutsum2.MTIMMAR = ACCUM TOTAL slutsum1.MTIMMAR             
      slutsum2.BELOPP = ACCUM TOTAL slutsum1.BELOPP                         
      slutsum2.MBELOPP = ACCUM TOTAL slutsum1.MBELOPP                           
      slutsum2.UEA = ACCUM TOTAL slutsum1.UEA
      slutsum2.OVRIGKOST = ACCUM TOTAL slutsum1.OVRIGKOST
      slutsum2.MTRLK = ACCUM TOTAL slutsum1.MTRLK.                                                              
        
   FOR EACH slutsum3:    
      ACCUMULATE slutsum3.TIMMAR(TOTAL).   
      ACCUMULATE slutsum3.BERTIM(TOTAL).
      ACCUMULATE slutsum3.MTIMMAR(TOTAL).
      ACCUMULATE slutsum3.BELOPP(TOTAL).  
      ACCUMULATE slutsum3.MBELOPP(TOTAL).
      ACCUMULATE slutsum3.UEA(TOTAL). 
      ACCUMULATE slutsum3.OVRIGKOST(TOTAL).          
      ACCUMULATE slutsum3.MTRLK(TOTAL).
   END.    
      CREATE slutsum4.                    
      ASSIGN                                         
      slutsum4.TIMMAR = ACCUM TOTAL slutsum3.TIMMAR                        
      slutsum4.BERTIM = ACCUM TOTAL slutsum3.BERTIM
      slutsum4.MTIMMAR = ACCUM TOTAL slutsum3.MTIMMAR             
      slutsum4.BELOPP = ACCUM TOTAL slutsum3.BELOPP                         
      slutsum4.MBELOPP = ACCUM TOTAL slutsum3.MBELOPP                           
      slutsum4.UEA = ACCUM TOTAL slutsum3.UEA
      slutsum4.OVRIGKOST = ACCUM TOTAL slutsum3.OVRIGKOST
      slutsum4.MTRLK = ACCUM TOTAL slutsum3.MTRLK.                                                            
     
   FOR EACH suaonr:       
      IF suaonr.AONRAVDATUM = 01/01/91 THEN DO:
         FIND FIRST slutsum WHERE slutsum.AONR = suaonr.AONR AND slutsum.DELNR = suaonr.DELNR
         EXCLUSIVE-LOCK NO-ERROR.         
      END.
      ELSE DO:
        FIND FIRST slutsum WHERE slutsum.AONR = suaonr.AONR AND slutsum.DELNR = suaonr.DELNR
        AND (slutsum.TIMMAR > 0 OR slutsum.MTIMMAR > 0) EXCLUSIVE-LOCK NO-ERROR.         
      END.         
      IF NOT AVAILABLE slutsum THEN DO:         
         CREATE suaonr5.  
         ASSIGN          
         suaonr5.AONR = suaonr.AONR
         suaonr5.DELNR = suaonr.DELNR
         suaonr5.FORETAG = suaonr.FORETAG
         suaonr5.OMRADE = suaonr.OMRADE
         suaonr5.AONRAVDATUM = suaonr.AONRAVDATUM
         suaonr5.SATS% = suaonr.SATS%.
         IF suaonr.P1 = TRUE THEN DO:
            ASSIGN
            suaonr5.PEA = suaonr.PEA
            suaonr5.MTRLK = suaonr.MTRLK
            suaonr5.PLANMASK = suaonr.PLANMASK            
            suaonr5.PLANMONT = suaonr.PLANMONT
            suaonr5.BERTIM = suaonr.BERTIM        
            suaonr5.MONTKOST = suaonr.MONTKOST
            suaonr5.MASKKOST = suaonr.MASKKOST
            suaonr5.OVRIGKOST = suaonr.OVRIGKOST
            suaonr5.KALKYL = "P1".           
         END.
         IF suaonr.P2 = TRUE THEN DO:
            ASSIGN
            suaonr5.PEA = suaonr.PEA2
            suaonr5.MTRLK = suaonr.MTRLK2
            suaonr5.PLANMASK = suaonr.PLANMASK2            
            suaonr5.PLANMONT = suaonr.PLANMONT2
            suaonr5.BERTIM = suaonr.BERTIM2
            suaonr5.MONTKOST = suaonr.MONTKOST2
            suaonr5.MASKKOST = suaonr.MASKKOST2
            suaonr5.OVRIGKOST = suaonr.OVRIGKOST2
            suaonr5.KALKYL = "P2".           
         END.
         IF suaonr.P3 = TRUE THEN DO:
            ASSIGN
            suaonr5.PEA = suaonr.PEA3
            suaonr5.MTRLK = suaonr.MTRLK3
            suaonr5.PLANMASK = suaonr.PLANMASK3            
            suaonr5.PLANMONT = suaonr.PLANMONT3
            suaonr5.BERTIM = suaonr.BERTIM3
            suaonr5.MONTKOST = suaonr.MONTKOST3
            suaonr5.MASKKOST = suaonr.MASKKOST3
            suaonr5.OVRIGKOST = suaonr.OVRIGKOST3
            suaonr5.KALKYL = "P3".           
         END.   
         DELETE suaonr.          
      END.      
   END.
   ASSIGN  
   arrhjsum = 0
   arrhjmt  = 0
   arrhjsumb = 0
   arrhjmb = 0
   arrhjuea = 0
   arrhjmtrl = 0 
   arrhjovr = 0
   arrhjber = 0. 
   FOR EACH suaonr WHERE suaonr.AONRAVDATUM > 01/01/91 BREAK BY suaonr.OMRADE:    
      ACCUMULATE suaonr.PLANMONTS(TOTAL BY suaonr.OMRADE).   
      ACCUMULATE suaonr.BERTIMS(TOTAL BY suaonr.OMRADE).
      ACCUMULATE suaonr.PLANMASKS(TOTAL BY suaonr.OMRADE).
      ACCUMULATE suaonr.MONTKOSTS(TOTAL BY suaonr.OMRADE).  
      ACCUMULATE suaonr.MASKKOSTS(TOTAL BY suaonr.OMRADE).  
      ACCUMULATE suaonr.MTRLKS(TOTAL BY suaonr.OMRADE).
      ACCUMULATE suaonr.PEAS(TOTAL BY suaonr.OMRADE). 
      ACCUMULATE suaonr.OVRIGKOSTS(TOTAL BY suaonr.OMRADE).         
      IF LAST-OF(suaonr.OMRADE) THEN DO:
         CREATE suaonr1.                    
         ASSIGN                                  
         suaonr1.OMRADE = suaonr.OMRADE           
         suaonr1.PLANMONT = (ACCUM TOTAL suaonr.PLANMONTS) - arrhjsum                  
         arrhjsum = ACCUM TOTAL suaonr.PLANMONTS 
         suaonr1.BERTIM = (ACCUM TOTAL suaonr.BERTIMS) - arrhjber                  
         arrhjber = ACCUM TOTAL suaonr.BERTIMS  
         suaonr1.PLANMASK = (ACCUM TOTAL suaonr.PLANMASKS) - arrhjmt                  
         arrhjmt = ACCUM TOTAL suaonr.PLANMASKS  
         suaonr1.MONTKOST = (ACCUM TOTAL suaonr.MONTKOSTS) - arrhjsumb                  
         arrhjsumb = ACCUM TOTAL suaonr.MONTKOSTS
         suaonr1.MASKKOST = (ACCUM TOTAL suaonr.MASKKOSTS) - arrhjmb                  
         arrhjmb = ACCUM TOTAL suaonr.MASKKOSTS    
         suaonr1.MTRLK = (ACCUM TOTAL suaonr.MTRLKS) - arrhjmtrl                  
         arrhjmtrl = ACCUM TOTAL suaonr.MTRLKS
         suaonr1.PEA = (ACCUM TOTAL suaonr.PEAS) - arrhjuea                  
         arrhjuea = ACCUM TOTAL suaonr.PEAS    
         suaonr1.OVRIGKOST = (ACCUM TOTAL suaonr.OVRIGKOSTS) - arrhjovr                  
         arrhjovr = ACCUM TOTAL suaonr.OVRIGKOSTS.                                               
      END.
   END.                                                
   ASSIGN  
   arrhjsum = 0
   arrhjmt  = 0
   arrhjsumb = 0
   arrhjmb = 0
   arrhjuea = 0 
   arrhjmtrl = 0 
   arrhjovr = 0
   arrhjber = 0.     
   FOR EACH suaonr BREAK BY suaonr.OMRADE:    
      ACCUMULATE suaonr.PLANMONTS(TOTAL BY suaonr.OMRADE).   
      ACCUMULATE suaonr.BERTIMS(TOTAL BY suaonr.OMRADE).
      ACCUMULATE suaonr.PLANMASKS(TOTAL BY suaonr.OMRADE).
      ACCUMULATE suaonr.MONTKOSTS(TOTAL BY suaonr.OMRADE).  
      ACCUMULATE suaonr.MASKKOSTS(TOTAL BY suaonr.OMRADE).   
      ACCUMULATE suaonr.MTRLKS(TOTAL BY suaonr.OMRADE).
      ACCUMULATE suaonr.PEAS(TOTAL BY suaonr.OMRADE). 
      ACCUMULATE suaonr.OVRIGKOSTS(TOTAL BY suaonr.OMRADE).        
      IF LAST-OF(suaonr.OMRADE) THEN DO:
         CREATE suaonr3.                    
         ASSIGN                                  
         suaonr3.OMRADE = suaonr.OMRADE           
         suaonr3.PLANMONT = (ACCUM TOTAL suaonr.PLANMONTS) - arrhjsum                  
         arrhjsum = ACCUM TOTAL suaonr.PLANMONTS   
         suaonr3.BERTIM = (ACCUM TOTAL suaonr.BERTIMS) - arrhjber                  
         arrhjber = ACCUM TOTAL suaonr.BERTIMS
         suaonr3.PLANMASK = (ACCUM TOTAL suaonr.PLANMASKS) - arrhjmt                  
         arrhjmt = ACCUM TOTAL suaonr.PLANMASKS  
         suaonr3.MONTKOST = (ACCUM TOTAL suaonr.MONTKOSTS) - arrhjsumb                  
         arrhjsumb = ACCUM TOTAL suaonr.MONTKOSTS
         suaonr3.MASKKOST = (ACCUM TOTAL suaonr.MASKKOSTS) - arrhjmb                  
         arrhjmb = ACCUM TOTAL suaonr.MASKKOSTS    
         suaonr3.MTRLK = (ACCUM TOTAL suaonr.MTRLKS) - arrhjmtrl                  
         arrhjmtrl = ACCUM TOTAL suaonr.MTRLKS
         suaonr3.PEA = (ACCUM TOTAL suaonr.PEAS) - arrhjuea                  
         arrhjuea = ACCUM TOTAL suaonr.PEAS  
         suaonr3.OVRIGKOST = (ACCUM TOTAL suaonr.OVRIGKOSTS) - arrhjovr                  
         arrhjovr = ACCUM TOTAL suaonr.OVRIGKOSTS.                                         
      END.
   END.      
   FOR EACH suaonr1:    
      ACCUMULATE suaonr1.PLANMONT(TOTAL).   
      ACCUMULATE suaonr1.BERTIM(TOTAL).
      ACCUMULATE suaonr1.PLANMASK(TOTAL).
      ACCUMULATE suaonr1.MONTKOST(TOTAL).  
      ACCUMULATE suaonr1.MASKKOST(TOTAL).     
      ACCUMULATE suaonr1.MTRLK(TOTAL).
      ACCUMULATE suaonr1.PEA(TOTAL).            
      ACCUMULATE suaonr1.OVRIGKOST(TOTAL). 
   END.   
      CREATE suaonr2.                    
      ASSIGN                                         
      suaonr2.PLANMONT = ACCUM TOTAL suaonr1.PLANMONT                       
      suaonr2.BERTIM = ACCUM TOTAL suaonr1.BERTIM
      suaonr2.PLANMASK = ACCUM TOTAL suaonr1.PLANMASK            
      suaonr2.MONTKOST = ACCUM TOTAL suaonr1.MONTKOST                        
      suaonr2.MASKKOST = ACCUM TOTAL suaonr1.MASKKOST    
      suaonr2.MTRLK = ACCUM TOTAL suaonr1.MTRLK
      suaonr2.PEA = ACCUM TOTAL suaonr1.PEA
      suaonr2.OVRIGKOST = ACCUM TOTAL suaonr1.OVRIGKOST.                                                              
   
   FOR EACH slutsum1:
      FIND FIRST suaonr1 WHERE suaonr.OMRADE = slutsum1.OMRADE NO-ERROR.
      IF AVAILABLE suaonr1 THEN DO: 
         ASSIGN slutsum1.RESULTAT = suaonr1.PEA / slutsum1.UEA.
      END.   
   END.      
   FIND FIRST slutsum2 NO-ERROR.
   FIND FIRST suaonr2 NO-ERROR.
   IF AVAILABLE slutsum2 THEN DO:
      IF AVAILABLE suaonr2 THEN DO    :
         ASSIGN slutsum2.RESULTAT = suaonr2.PEA / slutsum2.UEA.
      END.   
   END.       
   FOR EACH suaonr3:    
      ACCUMULATE suaonr3.PLANMONT(TOTAL).   
      ACCUMULATE suaonr3.BERTIM(TOTAL).
      ACCUMULATE suaonr3.PLANMASK(TOTAL).
      ACCUMULATE suaonr3.MONTKOST(TOTAL).  
      ACCUMULATE suaonr3.MASKKOST(TOTAL).   
      ACCUMULATE suaonr3.MTRLK(TOTAL).
      ACCUMULATE suaonr3.PEA(TOTAL).   
      ACCUMULATE suaonr3.OVRIGKOST(TOTAL).         
   END.   
      CREATE suaonr4.                    
      ASSIGN                                         
      suaonr4.PLANMONT = ACCUM TOTAL suaonr3.PLANMONT                       
      suaonr4.BERTIM = ACCUM TOTAL suaonr3.BERTIM
      suaonr4.PLANMASK = ACCUM TOTAL suaonr3.PLANMASK            
      suaonr4.MONTKOST = ACCUM TOTAL suaonr3.MONTKOST                        
      suaonr4.MASKKOST = ACCUM TOTAL suaonr3.MASKKOST      
      suaonr4.MTRLK = ACCUM TOTAL suaonr3.MTRLK                          
      suaonr4.PEA = ACCUM TOTAL suaonr3.PEA                                                              
      suaonr4.OVRIGKOST = ACCUM TOTAL suaonr3.OVRIGKOST.        
END PROCEDURE.


PROCEDURE omrade_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
   raknare1 = raknare1 + 1.                                    
   IF vomr1 = "" AND raknare1 > 2 THEN DO:                                 
      CREATE tidut. 
      CREATE tidut.
      SUBSTRING(tidut.UT,1) = "============================================
=======================================================================================$".      
      CREATE tidut.                                  
      ASSIGN 
      forsta = " "
      SUBSTRING(tidut.UT,1) = " SAMMANSTÄLLNING FÖR " + CAPS(Guru.Konstanter:gomrl) + " / ORGANISATIONER".       
      FOR EACH omrkonto: 
         CREATE tidut.                                
         ASSIGN
         SUBSTRING(tidut.UT,27) = omrkonto.OMRADE
         SUBSTRING(tidut.UT,34) = omrkonto.NAMN.                            
      END.   
      CREATE tidut.                  
      SUBSTRING(tidut.UT,1) = "============================================
=============================================================================".
      CREATE tidut.   
   END.                         
   ELSE IF vomr1 = "" THEN raknare1 = raknare1.
   ELSE DO:
      FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = vomr1
      USE-INDEX OMR NO-LOCK NO-ERROR.                             
      vomr = OMRADETAB.OMRADE. 
      CREATE tidut. 
      CREATE tidut.                                                
      IF raknare = 1 THEN DO:
      SUBSTRING(tidut.UT,1) = "============================================
=================================================================================".      
      END.
      ELSE DO:
            SUBSTRING(tidut.UT,1) = "============================================
=======================================================================================$".  
      END.
      CREATE tidut.                                  
      ASSIGN 
      forsta = " "
      SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gomrl) + " / ORGANISATION"
      SUBSTRING(tidut.UT,27) = OMRADETAB.OMRADE
      SUBSTRING(tidut.UT,34) = OMRADETAB.NAMN.
      IF TOG_SAMMAN = TRUE THEN DO:
         ASSIGN
         SUBSTRING(tidut.UT,60) = "SAMMANSTÄLLNING".
      END.
      ELSE IF TOG_FAST = TRUE THEN DO:
         IF RAD_LISTA = 1 THEN ASSIGN SUBSTRING(tidut.UT,60) = "INVESTERING".
         ELSE IF RAD_LISTA = 2 THEN ASSIGN SUBSTRING(tidut.UT,60) = "REINVESTERING".
         ELSE IF RAD_LISTA = 3 THEN ASSIGN SUBSTRING(tidut.UT,60) = "UNDERHÅLL".
         ELSE IF RAD_LISTA = 4 THEN ASSIGN SUBSTRING(tidut.UT,60) = "DRIFT".
         ELSE IF RAD_LISTA = 5 THEN ASSIGN SUBSTRING(tidut.UT,60) = "MÄTVÄRDESINSAMLING".
         ELSE IF RAD_LISTA = 6 THEN ASSIGN SUBSTRING(tidut.UT,60) = "EXTERNA TJÄNSTER".
         ELSE IF RAD_LISTA = 7 THEN ASSIGN SUBSTRING(tidut.UT,60) = "FASTA " + CAPS(Guru.Konstanter:gaok).
      END.   
      ASSIGN
      SUBSTRING(tidut.UT,80) = STRING(YEAR(bdatum),"9999").                            
      CREATE tidut.                  
      SUBSTRING(tidut.UT,1) = "============================================
=============================================================================".
      CREATE tidut.                          
   END.
END PROCEDURE.


PROCEDURE rubrik1_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   IF TOG_SAMMAN = TRUE THEN DO:
      ASSIGN
      str=                                                                    
"================================.=====.=======.=====.========.=========.=========.=========.======".                  
      CREATE tidut.      
      ASSIGN           
     
      SUBSTRING(tidut.UT,22) = CAPS(Guru.Konstanter:gplk) + "/"
      SUBSTRING(tidut.UT,36) = "MONT"
      SUBSTRING(tidut.UT,42) = "FRÄMM"
      SUBSTRING(tidut.UT,50) = "MASK".
/*      SUBSTRING(tidut.UT,95) = "BER". */                                
      CREATE tidut.      
      ASSIGN       
      SUBSTRING(tidut.UT,1) = "LISTA"                         
      SUBSTRING(tidut.UT,22) = "UTFALL"
/*      SUBSTRING(tidut.UT,37) = "EA"
      SUBSTRING(tidut.UT,42) = "RES"*/
      SUBSTRING(tidut.UT,37) = "TIM"
      SUBSTRING(tidut.UT,42) = "TJÄNST" 
      SUBSTRING(tidut.UT,51) = "TIM" 
      SUBSTRING(tidut.UT,59) = "MTRL"
      SUBSTRING(tidut.UT,70) = "LÖN"
      SUBSTRING(tidut.UT,77) = "ÖVRIGT"
      SUBSTRING(tidut.UT,90) = "TOT".
/*      SUBSTRING(tidut.UT,106) = "TIM".                              */
      CREATE tidut.       
      SUBSTRING(tidut.UT,1) = str.  
   END.
   ELSE DO:     
      ASSIGN
      str=                                                                    
"======.=========================.======.====.========.====.======.====.=======.========.=======.=======.====.====.====.".                  
      CREATE tidut.      
      ASSIGN  
      SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gplk) + "/"
      SUBSTRING(tidut.UT,55) = "MONT"
      SUBSTRING(tidut.UT,60) = "FRÄMM"
      SUBSTRING(tidut.UT,67) = "MASK".
/*      SUBSTRING(tidut.UT,108) = "BER"
      SUBSTRING(tidut.UT,113) = "KALK/"
      SUBSTRING(tidut.UT,118) = "REST".*/                                 
      CREATE tidut.      
      ASSIGN                                
      SUBSTRING(tidut.UT,1) = "UTFALL"
      SUBSTRING(tidut.UT,8) = "ORT"                      
      SUBSTRING(tidut.UT,35) = CAPS(Guru.Konstanter:gaok)  
      SUBSTRING(tidut.UT,42) = "DNR"
      SUBSTRING(tidut.UT,51) = "AVS"
/*      SUBSTRING(tidut.UT,58) = "EA"
      SUBSTRING(tidut.UT,63) = "RES"*/
      SUBSTRING(tidut.UT,56) = "TIM"
      SUBSTRING(tidut.UT,60) = "TJÄNST"
      SUBSTRING(tidut.UT,68) = "TIM"
      SUBSTRING(tidut.UT,75) = "MTRL"
      SUBSTRING(tidut.UT,85) = "LÖN"
      SUBSTRING(tidut.UT,91) = "ÖVRIG"
      SUBSTRING(tidut.UT,101) = "TOT".
/*      SUBSTRING(tidut.UT,108) = "TIM"
      SUBSTRING(tidut.UT,113) = "KOST"
      SUBSTRING(tidut.UT,118) = "RAPP".                        */
      CREATE tidut.       
      SUBSTRING(tidut.UT,1) = str.         
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rubrik2_UI WINDOW-2 
PROCEDURE rubrik2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   IF TOG_SAMMAN = TRUE THEN DO:
      ASSIGN
      col1 = 34 
      col2 = 40
      col3 = 46
      col4 = 52
      col5 = 60
      col6 = 67
      col7 = 75
      col8 = 95
      col9 = 85
      col10 = 106.
   END.
   ELSE DO:
      ASSIGN
      col1 = 55 
      col2 = 61
      col3 = 55
      col4 = 61
      col5 = 67
      col6 = 73
      col7 = 81
      col8 = 97
      col9 = 89
      col10 = 106.
   END.   
   FIND FIRST suaonr1 WHERE suaonr1.OMRADE = vomr NO-LOCK NO-ERROR.
   IF AVAILABLE suaonr1 THEN DO:                 
      FIND FIRST slutsum1 WHERE slutsum1.OMRADE = vomr NO-LOCK NO-ERROR.
      IF AVAILABLE slutsum1 THEN DO:    
         CREATE tidut.                        
         ASSIGN   
         SUBSTRING(tidut.UT,1) = "SUMMA AVSLUTADE " + CAPS(Guru.Konstanter:gplk) + "/ " + STRING(vomr)
         /*SUBSTRING(tidut.UT,col1) = STRING(suaonr1.PEA,">>>>9")*/
         SUBSTRING(tidut.UT,col3) = STRING(suaonr1.PLANMONT,">>>>9")   
         SUBSTRING(tidut.UT,col4) = STRING(suaonr1.MASKKOST,">>>>>>9")
         SUBSTRING(tidut.UT,col5) = STRING(suaonr1.PLANMASK,">>>>9")
         SUBSTRING(tidut.UT,col6) = STRING(suaonr1.MTRLK,">>>>>>9")
         SUBSTRING(tidut.UT,col7) = STRING(suaonr1.MONTKOST,">>>>>>>>9")
         SUBSTRING(tidut.UT,col9) = STRING(suaonr1.OVRIGKOST,">>>>>>>>9")
         SUBSTRING(tidut.UT,col8) = STRING(suaonr1.PLANMASK * mapris + (suaonr1.PLANMONT * mopris) + suaonr1.MTRLK + suaonr1.OVRIGKOST,">>>>>>>>9")
         SUBSTRING(tidut.UT,col10) = STRING(suaonr1.BERTIM,">>>>9").
         CREATE tidut. 
         ASSIGN   
         SUBSTRING(tidut.UT,1) = "SUMMA AVSLUTADE UTFALL"+ STRING(vomr)
      /*   SUBSTRING(tidut.UT,col1) = STRING(slutsum1.UEA,">>>>9")
         SUBSTRING(tidut.UT,col2) = STRING(slutsum1.RESULTAT,"99.99")*/
         SUBSTRING(tidut.UT,col3) = STRING(slutsum1.TIMMAR,">>>>9")
         SUBSTRING(tidut.UT,col4) = STRING(slutsum1.MBELOPP,">>>>>>9") 
         SUBSTRING(tidut.UT,col5) = STRING(slutsum1.MTIMMAR,">>>>9") 
         SUBSTRING(tidut.UT,col7) = STRING(slutsum1.BELOPP,">>>>>>>>9") 
         SUBSTRING(tidut.UT,col8) = STRING((slutsum1.MBELOPP +  slutsum1.BELOPP + slutsum1.MTRLK),">>>>>>>>9")
         SUBSTRING(tidut.UT,col10) = STRING(slutsum1.BERTIM,">>>>9").
         CREATE tidut. 
         FIND FIRST suaonr3 WHERE suaonr3.OMRADE = vomr NO-LOCK NO-ERROR.
         IF AVAILABLE suaonr3 THEN DO:                 
            FIND FIRST slutsum3 WHERE slutsum3.OMRADE = vomr NO-LOCK NO-ERROR.
            IF AVAILABLE slutsum3 THEN DO:         
               CREATE tidut.                        
               ASSIGN   
               SUBSTRING(tidut.UT,1) = "SUMMA TOTALT " + CAPS(Guru.Konstanter:gplk) + "/ " + STRING(vomr)
/*               SUBSTRING(tidut.UT,col1) = STRING(suaonr3.PEA,">>>>>9")*/
               SUBSTRING(tidut.UT,col3) = STRING(suaonr3.PLANMONT,">>>>9")   
               SUBSTRING(tidut.UT,col4) = STRING(suaonr3.MASKKOST,">>>>>>9")
               SUBSTRING(tidut.UT,col5) = STRING(suaonr3.PLANMASK,">>>>9")
               SUBSTRING(tidut.UT,col6) = STRING(suaonr3.MTRLK,">>>>>>>9")
               SUBSTRING(tidut.UT,col7) = STRING(suaonr3.MONTKOST,">>>>>>>9")
               SUBSTRING(tidut.UT,col9) = STRING(suaonr3.OVRIGKOST,">>>>>>>9")
               SUBSTRING(tidut.UT,col8) = STRING(suaonr3.PLANMASK * mapris + (suaonr3.PLANMONT * mopris) + suaonr3.MTRLK + suaonr3.OVRIGKOST,">>>>>>>9")
               SUBSTRING(tidut.UT,col10) = STRING(suaonr3.BERTIM,">>>>9").
               CREATE tidut. 
               ASSIGN   
               SUBSTRING(tidut.UT,1) = "SUMMA TOTALT UTFALL" + STRING(vomr)
/*               SUBSTRING(tidut.UT,col1) = STRING(slutsum3.UEA,">>>>>9")
               SUBSTRING(tidut.UT,col2) = STRING(slutsum3.RESULTAT,"99.99")*/
               SUBSTRING(tidut.UT,col3) = STRING(slutsum3.TIMMAR,">>>>9")
               SUBSTRING(tidut.UT,col4) = STRING(slutsum3.MBELOPP,">>>>>>9") 
               SUBSTRING(tidut.UT,col5) = STRING(slutsum3.MTIMMAR,">>>>9") 
               SUBSTRING(tidut.UT,col7) = STRING(slutsum3.BELOPP,">>>>>>>9") 
               SUBSTRING(tidut.UT,col8) = STRING((slutsum3.MBELOPP +  slutsum3.BELOPP),">>>>>>>9")
               SUBSTRING(tidut.UT,col10) = STRING(slutsum3.BERTIM,">>>>9").
               CREATE tidut.              
            END.
         END.      
      END.
      ELSE DO:                      
         CREATE tidut.                        
         ASSIGN   
         SUBSTRING(tidut.UT,1) = "SUMMA AVSLUTADE " + CAPS(Guru.Konstanter:gplk) + "/ " + STRING(vomr).    
         CREATE tidut. 
         ASSIGN   
         SUBSTRING(tidut.UT,1) = "SUMMA AVSLUTADE UTFALL" + STRING(vomr).
         CREATE tidut. 
      END.            
   END.      
END PROCEDURE.


PROCEDURE sort1_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/       
   hjsum = suaonr.MASKKOST + suaonr.MONTKOST + suaonr.OVRIGKOST + suaonr.MTRLK.
   IF hjsum > 0 THEN DO:
      CREATE tidut.                        
      ASSIGN   
      SUBSTRING(tidut.UT,1) = "PLANP1/ "
      SUBSTRING(tidut.UT,8) = STRING(slutsum.ORT,"X(25)")
      SUBSTRING(tidut.UT,34) = slutsum.AONR
      SUBSTRING(tidut.UT,42) = STRING(slutsum.DELNR,"999").
/*      SUBSTRING(tidut.UT,55) = STRING(suaonr.PEA,">>>>9").*/
      IF slutsum.AONRAVDATUM NE 01/01/91 THEN DO:
         ASSIGN SUBSTRING(tidut.UT,47) = STRING(slutsum.AONRAVDATUM,"99/99/99").
      END.
      ASSIGN
      SUBSTRING(tidut.UT,55) = STRING(suaonr.PLANMONT,">>>9")   
      SUBSTRING(tidut.UT,60) = STRING(suaonr.MASKKOST,">>>>>9")
      SUBSTRING(tidut.UT,67) = STRING(suaonr.PLANMASK,">>>9")
      SUBSTRING(tidut.UT,72) = STRING(suaonr.MTRLK,">>>>>>9")
      SUBSTRING(tidut.UT,81) = STRING(suaonr.MONTKOST,">>>>>>9")
      SUBSTRING(tidut.UT,89) = STRING(suaonr.OVRIGKOST,">>>>>>9")
      SUBSTRING(tidut.UT,97) = 
      STRING(suaonr.PLANMASK * mapris + (suaonr.PLANMONT * mopris) + 
      suaonr.MTRLK + suaonr.OVRIGKOST,">>>>>>9")
      SUBSTRING(tidut.UT,196) = STRING(suaonr.BERTIM,">>>9")
      SUBSTRING(tidut.UT,112) = suaonr.KREG
      SUBSTRING(tidut.UT,117) = suaonr.RREG.   
   END.
   hjsum = suaonr.MASKKOST2 + suaonr.MONTKOST2 + suaonr.OVRIGKOST2 + suaonr.MTRLK2.
   IF hjsum > 0 THEN DO:
      CREATE tidut.                        
      ASSIGN   
      SUBSTRING(tidut.UT,1) = "PLANP2/ "
      SUBSTRING(tidut.UT,8) = STRING(slutsum.ORT,"X(25)")
      SUBSTRING(tidut.UT,34) = slutsum.AONR
      SUBSTRING(tidut.UT,42) = STRING(slutsum.DELNR,"999").
/*      SUBSTRING(tidut.UT,55) = STRING(suaonr.PEA2,">>>>9").*/
      IF slutsum.AONRAVDATUM NE 01/01/91 THEN DO:
         ASSIGN SUBSTRING(tidut.UT,47) = STRING(slutsum.AONRAVDATUM,"99/99/99").
      END.
      ASSIGN
      SUBSTRING(tidut.UT,55) = STRING(suaonr.PLANMONT2,">>>9")   
      SUBSTRING(tidut.UT,60) = STRING(suaonr.MASKKOST2,">>>>>9")
      SUBSTRING(tidut.UT,67) = STRING(suaonr.PLANMASK2,">>>9")
      SUBSTRING(tidut.UT,72) = STRING(suaonr.MTRLK2,">>>>>>9")
      SUBSTRING(tidut.UT,81) = STRING(suaonr.MONTKOST2,">>>>>>9")
      SUBSTRING(tidut.UT,89) = STRING(suaonr.OVRIGKOST2,">>>>>>9")
      SUBSTRING(tidut.UT,97) = STRING(suaonr.PLANMASK2 * mapris + (suaonr.PLANMONT2 * mopris) + suaonr.MTRLK2 + suaonr.OVRIGKOST2,">>>>>>9")
      SUBSTRING(tidut.UT,106) = STRING(suaonr.BERTIM2,">>>9")
      SUBSTRING(tidut.UT,112) = suaonr.KREG
      SUBSTRING(tidut.UT,117) = suaonr.RREG.   
   END.
   hjsum = suaonr.MASKKOST3 + suaonr.MONTKOST3 + suaonr.OVRIGKOST3 + suaonr.MTRLK3.
   IF hjsum > 0 THEN DO:
      CREATE tidut.                        
      ASSIGN   
      SUBSTRING(tidut.UT,1) = "PLANP3/ "
      SUBSTRING(tidut.UT,8) = STRING(slutsum.ORT,"X(25)")
      SUBSTRING(tidut.UT,34) = slutsum.AONR
      SUBSTRING(tidut.UT,42) = STRING(slutsum.DELNR,"999").
/*      SUBSTRING(tidut.UT,55) = STRING(suaonr.PEA3,">>>>9").*/
      IF slutsum.AONRAVDATUM NE 01/01/91 THEN DO:
         ASSIGN SUBSTRING(tidut.UT,47) = STRING(slutsum.AONRAVDATUM,"99/99/99").
      END.
      ASSIGN
      SUBSTRING(tidut.UT,55) = STRING(suaonr.PLANMONT3,">>>9")   
      SUBSTRING(tidut.UT,60) = STRING(suaonr.MASKKOST3,">>>>>9")
      SUBSTRING(tidut.UT,67) = STRING(suaonr.PLANMASK3,">>>9")
      SUBSTRING(tidut.UT,72) = STRING(suaonr.MTRLK3,">>>>>>9")
      SUBSTRING(tidut.UT,81) = STRING(suaonr.MONTKOST3,">>>>>>9")
      SUBSTRING(tidut.UT,89) = STRING(suaonr.OVRIGKOST3,">>>>>>9")
      SUBSTRING(tidut.UT,97) = STRING(suaonr.PLANMASK3 * mapris + (suaonr.PLANMONT3 * mopris) + suaonr.MTRLK3 + suaonr.OVRIGKOST3,">>>>>>9")
      SUBSTRING(tidut.UT,106) = STRING(suaonr.BERTIM3,">>>9")
      SUBSTRING(tidut.UT,112) = suaonr.KREG
      SUBSTRING(tidut.UT,117) = suaonr.RREG.   
   END.
   CREATE tidut. 
   ASSIGN   
   SUBSTRING(tidut.UT,1) = "UTFALL"
   SUBSTRING(tidut.UT,8) = STRING(slutsum.ORT,"X(25)")                      
   SUBSTRING(tidut.UT,34) = slutsum.AONR  
   SUBSTRING(tidut.UT,42) = STRING(slutsum.DELNR,"999").
   IF slutsum.AONRAVDATUM NE 01/01/91 THEN ASSIGN SUBSTRING(tidut.UT,47) = STRING(slutsum.AONRAVDATUM).
   ASSIGN
  /* SUBSTRING(tidut.UT,55) = STRING(slutsum.UEA,">>>>9")
   SUBSTRING(tidut.UT,61) = STRING(slutsum.RESULTAT,"99.99")*/
   SUBSTRING(tidut.UT,55) = STRING(slutsum.TIMMAR,">>>9")
   SUBSTRING(tidut.UT,60) = STRING(slutsum.MBELOPP,">>>>>9") 
   SUBSTRING(tidut.UT,67) = STRING(slutsum.MTIMMAR,">>>9") 
   SUBSTRING(tidut.UT,72) = STRING(slutsum.MTRLK,">>>>>>9")
   SUBSTRING(tidut.UT,81) = STRING(slutsum.BELOPP,">>>>>>9") 
   SUBSTRING(tidut.UT,97) = STRING((slutsum.MBELOPP +  slutsum.BELOPP + + slutsum.MTRLK),">>>>>>9")
   SUBSTRING(tidut.UT,106) = STRING(slutsum.BERTIM,">>>9")
   SUBSTRING(tidut.UT,112) = slutsum.KREG
   SUBSTRING(tidut.UT,117) = slutsum.RREG.
   CREATE tidut. 
   /*DELETE slutsum.
   DELETE suaonr.     */
END PROCEDURE.


PROCEDURE sort2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   CREATE tidut.                        
   ASSIGN             
   SUBSTRING(tidut.UT,1) = samsum.INNAMN
   SUBSTRING(tidut.UT,22) = CAPS(Guru.Konstanter:gplk) + "/ "
/*   SUBSTRING(tidut.UT,34) = STRING(suindel.PEA,">>>>9")*/
   SUBSTRING(tidut.UT,34) = STRING(suindel.PLANMONT,">>>>9")   
   SUBSTRING(tidut.UT,40) = STRING(suindel.MASKKOST,">>>>>>9")
   SUBSTRING(tidut.UT,48) = STRING(suindel.PLANMASK,">>>>9")
   SUBSTRING(tidut.UT,55) = STRING(suindel.MTRLK,">>>>>>9")
   SUBSTRING(tidut.UT,63) = STRING(suindel.MONTKOST,">>>>>>>>9")
   SUBSTRING(tidut.UT,73) = STRING(suindel.OVRIGKOST,">>>>>>>>9")
   SUBSTRING(tidut.UT,83) = STRING(suindel.PLANMASK * mapris + (suindel.PLANMONT * mopris) + suindel.MTRLK + suindel.OVRIGKOST,">>>>>>>>9")
   SUBSTRING(tidut.UT,94) = STRING(suindel.BERTIM,">>>>9").
   CREATE tidut. 
   ASSIGN                          
   SUBSTRING(tidut.UT,1) = samsum.INNAMN
   SUBSTRING(tidut.UT,22) = "UTFALL"
/*   SUBSTRING(tidut.UT,34) = STRING(samsum.UEA,">>>>9")
   SUBSTRING(tidut.UT,40) = STRING(samsum.RESULTAT,"99.99")*/
   SUBSTRING(tidut.UT,34) = STRING(samsum.TIMMAR,">>>>9")
   SUBSTRING(tidut.UT,40) = STRING(samsum.MBELOPP,">>>>>>9") 
   SUBSTRING(tidut.UT,48) = STRING(samsum.MTIMMAR,">>>>9") 
   SUBSTRING(tidut.UT,63) = STRING(samsum.BELOPP,">>>>>>>>9") 
   SUBSTRING(tidut.UT,83) = STRING((samsum.MBELOPP +  samsum.BELOPP),">>>>>>>>9")
   SUBSTRING(tidut.UT,94) = STRING(samsum.BERTIM,">>>>9").
   CREATE tidut.    

END PROCEDURE.


PROCEDURE summa_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/ 
   FOR EACH slutsum:
      DELETE slutsum.
   END.              

   FOR EACH aonromr:
      OPEN QUERY sumq FOR EACH SUMTID  WHERE SUMTID.AONR = aonromr.AONR AND
      SUMTID.DELNR = aonromr.DELNR USE-INDEX AONR NO-LOCK.
      GET FIRST sumq NO-LOCK.           
      DO WHILE AVAILABLE(SUMTID) TRANSACTION: 
         IF SUMTID.DATUM <= bdatum THEN DO:                 
            IF RAD_LISTA = 7 AND aonromr.OMRADE NE SUMTID.OMRADE THEN bdatum = bdatum. 
            ELSE IF RAD_LISTA = 7 AND SUMTID.DATUM < bdatum THEN bdatum = bdatum. 
            ELSE DO:
               CREATE dagtemp.
               ASSIGN        
               dagtemp.DATUM = SUMTID.DATUM.
               /*"EJ DEBI.."*/
               IF SUBSTRING(SUMTID.BEFATTNING,1,20) = "DRIFTTEKNIKER" OR 
               SUBSTRING(SUMTID.BEFATTNING,1,20) = "MÄTTEKNIKER" OR
               SUBSTRING(SUMTID.BEFATTNING,1,20) = "BEREDARE"  THEN DO:
                  ASSIGN dagtemp.BERTIM = SUMTID.TIMMAR + SUMTID.OTIMMAR.
               END.
               ELSE IF RAD_LISTA = 7 AND SUBSTRING(SUMTID.BEFATTNING,1,20) = "MONTÖR" THEN DO:
                  ASSIGN dagtemp.TIMMAR = SUMTID.TIMMAR + SUMTID.OTIMMAR.
               END.
               ELSE IF RAD_LISTA = 7 THEN musz = musz.
               ELSE DO:
                  ASSIGN dagtemp.TIMMAR = SUMTID.TIMMAR + SUMTID.OTIMMAR.
               END.
               IF RAD_LISTA = 7 AND SUMTID.AONR BEGINS "0015" THEN DO:
                  ASSIGN dagtemp.AONR = "0015XX"
                  dagtemp.ORT = "BILSERVICE".
               END.   
               ELSE DO:
                  ASSIGN dagtemp.ORT = aonromr.ORT
                  dagtemp.AONR = aonromr.AONR.
               END.
               ASSIGN   
               dagtemp.BELOPP = SUMTID.BELOPP + SUMTID.OBELOPP.                             
               IF SUMTID.PERSMASK = FALSE THEN DO:
                  IF SUMTID.PERSONALKOD = "71001" OR SUMTID.PERSONALKOD = "72001" OR
                  SUMTID.PERSONALKOD = "73001" OR SUMTID.PERSONALKOD = "73002" THEN DO:
                     ASSIGN                     
                     dagtemp.MBELOPP = (SUMTID.BELOPP + SUMTID.OBELOPP) / 4.
                  END.                  
                  ELSE DO:  
                     ASSIGN
                     dagtemp.MBELOPP = SUMTID.BELOPP + SUMTID.OBELOPP. 
                  END.   
                  ASSIGN
                  dagtemp.MTIMMAR = SUMTID.TIMMAR + SUMTID.OTIMMAR
                  dagtemp.BELOPP = 0
                  dagtemp.TIMMAR = 0.
               END.    
               ASSIGN
               dagtemp.PERSONALKOD = SUMTID.PERSONALKOD 
               /*dagtemp.AONR = aonromr.AONR
               dagtemp.DELNR = aonromr.DELNR*/
               dagtemp.OMRADE = aonromr.OMRADE
/*               dagtemp.ORT = aonromr.ORT    */
               dagtemp.AONRAVDATUM = aonromr.AONRAVDATUM            
               dagtemp.KONTONR = aonromr.KONTONR  
               dagtemp.PERSMASK = SUMTID.PERSMASK    
               dagtemp.SATS% = aonromr.SATS%
               dagtemp.INDEL = aonromr.INDEL.                 
            END.   
         END.   
         GET NEXT sumq NO-LOCK. 
      END.        
      CLOSE QUERY sumq.
   END.  
   FOR EACH aonromr WHERE aonromr.AONR BEGINS "0015" EXCLUSIVE-LOCK:
      IF aoNromr.AONR = "0015XX" THEN.
      ELSE DELETE aonromr.
   END.   
   FOR EACH dagtemp:
      FIND LAST rest WHERE rest.AONR = dagtemp.AONR AND rest.DELNR = dagtemp.DELNR
      USE-INDEX AONR NO-ERROR.
      IF AVAILABLE rest THEN DO:
         IF rest.DATUM >= bdatum  THEN DO: 
            ASSIGN
            dagtemp.AONRAVDATUM = rest.DATUM.
         END.
         IF bdatum > rest.DATUM AND YEAR(dagtemp.DATUM) LE YEAR(rest.DATUM) THEN DO:
            DELETE dagtemp.
         END. 
      END.
   END.              
   ato = FALSE.     
   FOR EACH aonromr:      
      OPEN QUERY koreg FOR EACH KOSTREG WHERE KOSTREG.AONR = aonromr.AONR AND
      KOSTREG.DELNR = aonromr.DELNR USE-INDEX KOST NO-LOCK BY KOSTREG.KOSTAUTO DESCENDING.
      GET FIRST koreg NO-LOCK.           
      DO WHILE AVAILABLE(KOSTREG) TRANSACTION:               
         IF KOSTREG.KOSTAUTO = TRUE THEN ato = TRUE.
         IF KOSTREG.KOSTAUTO = TRUE OR ato = FALSE THEN DO:
            FIND FIRST dagtemp WHERE dagtemp.aonr = aonromr.AONR AND dagtemp.DELNR
            = aonromr.DELNR USE-INDEX AONR EXCLUSIVE-LOCK NO-ERROR.          
            IF AVAILABLE dagtemp THEN DO:
               ASSIGN dagtemp.MBELOPP = dagtemp.MBELOPP + KOSTREG.MASKKOST
               dagtemp.MTRLK = dagtemp.MTRLK + KOSTREG.MTRL
               dagtemp.OVRIGKOST = dagtemp.OVRIGKOST + KOSTREG.OVRKR + KOSTREG.TRAKTKOST 
               dagtemp.BELOPP = dagtemp.BELOPP + KOSTREG.PERSKOST.                    
               FOR EACH  dagtemp WHERE dagtemp.aonr = aonromr.AONR AND dagtemp.DELNR
               = aonromr.DELNR USE-INDEX AONR EXCLUSIVE-LOCK.
                  ASSIGN                     
                  dagtemp.KREG = "JA".   
               END.                            
            END.   
         END.   
         GET NEXT koreg NO-LOCK. 
      END.        
      ato = FALSE.
      CLOSE QUERY koreg.
   END.         
   FOR EACH aonromr:
      FIND FIRST MARKOVR WHERE MARKOVR.AONR = aonromr.AONR AND
      MARKOVR.DELNR = aonromr.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.
      IF AVAILABLE MARKOVR THEN DO: 
         FIND FIRST dagtemp WHERE dagtemp.aonr = aonromr.AONR AND dagtemp.DELNR
         = aonromr.DELNR USE-INDEX AONR EXCLUSIVE-LOCK NO-ERROR.          
         IF AVAILABLE dagtemp THEN DO:            
            ASSIGN dagtemp.OVRIGKOST = dagtemp.OVRIGKOST + MARKOVR.OVRIGKOST.
         END.                                
      END.        
   END.         
   IF TOG_SAMMAN = TRUE THEN DO:
      FOR EACH suaonr WHERE suaonr.AONRAVDATUM > 01/01/91 USE-INDEX AONR: 
         FIND FIRST dagtemp WHERE dagtemp.AONR = suaonr.AONR AND
         dagtemp.DELNR = suaonr.DELNR USE-INDEX AONR NO-ERROR.           
         IF NOT AVAILABLE dagtemp THEN DO:
           DELETE suaonr.
         END.
      END.     
      ASSIGN  
      arrhjsum = 0
      arrhjmt  = 0
      arrhjsumb = 0
      arrhjmb = 0
      arrhjuea = 0    
      arrhjmtrl = 0    
      arrhjovr = 0.
      FOR EACH suaonr WHERE suaonr.AONRAVDATUM > 01/01/91 BREAK BY suaonr.OMRADE BY suaonr.INDEL:    
         ACCUMULATE suaonr.PLANMONTS(TOTAL BY suaonr.OMRADE BY suaonr.INDEL).   
         ACCUMULATE suaonr.PLANMASKS(TOTAL BY suaonr.OMRADE BY suaonr.INDEL).
         ACCUMULATE suaonr.MONTKOSTS(TOTAL BY suaonr.OMRADE BY suaonr.INDEL).  
         ACCUMULATE suaonr.MASKKOSTS(TOTAL BY suaonr.OMRADE BY suaonr.INDEL).  
         ACCUMULATE suaonr.MTRLKS(TOTAL BY suaonr.OMRADE BY suaonr.INDEL).
         ACCUMULATE suaonr.PEAS(TOTAL BY suaonr.OMRADE BY suaonr.INDEL).         
         ACCUMULATE suaonr.OVRIGKOSTS(TOTAL BY suaonr.OMRADE BY suaonr.INDEL).
         IF LAST-OF(suaonr.INDEL) THEN DO:
            CREATE suindel.                    
            ASSIGN   
            suindel.INDEL = suaonr.INDEL                               
            suindel.OMRADE = suaonr.OMRADE           
            suindel.PLANMONT = (ACCUM TOTAL suaonr.PLANMONTS) - arrhjsum                  
            arrhjsum = ACCUM TOTAL suaonr.PLANMONTS   
            suindel.PLANMASK = (ACCUM TOTAL suaonr.PLANMASKS) - arrhjmt                  
            arrhjmt = ACCUM TOTAL suaonr.PLANMASKS  
            suindel.MONTKOST = (ACCUM TOTAL suaonr.MONTKOSTS) - arrhjsumb                  
            arrhjsumb = ACCUM TOTAL suaonr.MONTKOSTS
            suindel.MASKKOST = (ACCUM TOTAL suaonr.MASKKOSTS) - arrhjmb                  
            arrhjmb = ACCUM TOTAL suaonr.MASKKOSTS       
            suindel.MTRLK = (ACCUM TOTAL suaonr.MTRLKS) - arrhjmtrl
            arrhjmtrl = ACCUM TOTAL suaonr.MTRLKS
            suindel.PEA = (ACCUM TOTAL suaonr.PEAS) - arrhjuea                  
            arrhjuea = ACCUM TOTAL suaonr.PEAS
            suindel.OVRIGKOST = (ACCUM TOTAL suaonr.OVRIGKOSTS) - arrhjovr                  
            arrhjovr = ACCUM TOTAL suaonr.OVRIGKOSTS.                                          
         END.
      END.   
      FOR EACH suindel:
         IF suindel.INDEL = 1 THEN ASSIGN suindel.INNAMN = "INVESTERING".
         ELSE IF suindel.INDEL = 2 THEN ASSIGN suindel.INNAMN = "REINVESTERING".
         ELSE IF suindel.INDEL = 3 THEN ASSIGN suindel.INNAMN = "UNDERHÅLL".
         ELSE IF suindel.INDEL = 4 THEN ASSIGN suindel.INNAMN = "DRIFT".
         ELSE IF suindel.INDEL = 5 THEN ASSIGN suindel.INNAMN = "MÄTVÄRDESINSAMLING".
         ELSE IF suindel.INDEL = 6 THEN ASSIGN suindel.INNAMN = "EXTERNA TJÄNSTER".
      END.            
   END.                                                              
        
   IF TOG_SAMMAN = FALSE THEN DO:     
      RUN lontill_UI.       
      RUN allval2_UI.    /*område*/   
   END.
   ELSE DO:
      RUN lontill2_UI.                
      RUN allval21_UI.    /*område*/   
   END.      
   
END PROCEDURE.

PROCEDURE totsum_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/ 
  IF TOG_SAMMAN = TRUE THEN DO:
     ASSIGN
     col1 = 34 
     col2 = 40
     col3 = 46
     col4 = 52
     col5 = 60
     col6 = 67
     col7 = 75
     col8 = 95
     col9 = 85
     col10 = 106.
  END.
  ELSE DO:
     ASSIGN
     col1 = 55 
     col2 = 61
     col3 = 55
     col4 = 61
     col5 = 68
     col6 = 73
     col7 = 80
     col8 = 97
     col9 = 89
     col10 = 106.
  END.                               
  CREATE tidut.
  SUBSTRING(tidut.UT,1) = "============================================
=============================================================================".      
   CREATE tidut.                                  
 /*summa avslutade arbeten*/
   FIND FIRST suaonr2 NO-LOCK NO-ERROR.
   IF AVAILABLE suaonr2 THEN DO:
      FIND FIRST slutsum2 NO-LOCK NO-ERROR.
      IF AVAILABLE slutsum2 THEN DO:   
         CREATE tidut.                        
         ASSIGN   
         SUBSTRING(tidut.UT,1) = "TOTALSUMMA AVSLUTADE " + CAPS(Guru.Konstanter:gplk) + "/ "
/*         SUBSTRING(tidut.UT,col1) = STRING(suaonr2.PEA,">>>>9")*/
         SUBSTRING(tidut.UT,col3) = STRING(suaonr2.PLANMONT,">>>>9")   
         SUBSTRING(tidut.UT,col4) = STRING(suaonr2.MASKKOST,">>>>>>9")  
         SUBSTRING(tidut.UT,col5) = STRING(suaonr2.PLANMASK,">>>9")
         SUBSTRING(tidut.UT,col6) = STRING(suaonr2.MTRLK,">>>>>>9")
         SUBSTRING(tidut.UT,col7) = STRING(suaonr2.MONTKOST,">>>>>>>>9")
         SUBSTRING(tidut.UT,col9) = STRING(suaonr2.OVRIGKOST,">>>>>>>>9")
         SUBSTRING(tidut.UT,col8) = STRING(suaonr2.PLANMASK * mapris + (suaonr2.PLANMONT * mopris) + suaonr2.MTRLK + suaonr2.OVRIGKOST,">>>>>>>>9")
         SUBSTRING(tidut.UT,col10) = STRING(suaonr2.BERTIM,">>>>9").
         CREATE tidut. 
         ASSIGN   
         SUBSTRING(tidut.UT,1) = "TOTALSUMMA AVSLUTADE UTFALL"
/*         SUBSTRING(tidut.UT,col1) = STRING(slutsum2.UEA,">>>>9")
         SUBSTRING(tidut.UT,col2) = STRING(slutsum2.RESULTAT,"99.99")*/
         SUBSTRING(tidut.UT,col3) = STRING(slutsum2.TIMMAR,">>>>9")
         SUBSTRING(tidut.UT,col4) = STRING(slutsum2.MBELOPP,">>>>>>9")    
         SUBSTRING(tidut.UT,col5) = STRING(slutsum2.MTIMMAR,">>>9") 
         SUBSTRING(tidut.UT,col6) = STRING(slutsum2.MTRLK,">>>>>>9")
         SUBSTRING(tidut.UT,col7) = STRING(slutsum2.BELOPP,">>>>>>>>9") 
         SUBSTRING(tidut.UT,col8) = STRING((slutsum2.MBELOPP +  slutsum2.BELOPP + slutsum2.MTRLK),">>>>>>>>9")
         SUBSTRING(tidut.UT,col10) = STRING(slutsum2.BERTIM,">>>>9").
         CREATE tidut.     
      END.      
   END.   
   ELSE DO:
      CREATE tidut.                        
      ASSIGN   
      SUBSTRING(tidut.UT,1) = "TOTALSUMMA AVSLUTADE PLAN/ ". 
      CREATE tidut. 
      ASSIGN   
      SUBSTRING(tidut.UT,1) = "TOTALSUMMA AVSLUTADE UTFALL".             
      CREATE tidut.
   END.                     
   /*summa alla arbeten*/
   IF TOG_FAST = TRUE AND RAD_LISTA = 7 THEN musz = musz.
   ELSE DO:
      FIND FIRST suaonr4 NO-LOCK NO-ERROR.
      IF AVAILABLE suaonr4 THEN DO:   
         CREATE tidut.                        
         ASSIGN   
         SUBSTRING(tidut.UT,1) = "TOTALSUMMA  " + CAPS(Guru.Konstanter:gplk) + "/ "
/*         SUBSTRING(tidut.UT,col1) = STRING(suaonr4.PEA,">>>>9")*/
         SUBSTRING(tidut.UT,col3) = STRING(suaonr4.PLANMONT,">>>>9")   
         SUBSTRING(tidut.UT,col4) = STRING(suaonr4.MASKKOST,">>>>>>9")
         SUBSTRING(tidut.UT,col5) = STRING(suaonr4.PLANMASK,">>>>9")
         SUBSTRING(tidut.UT,col6) = STRING(suaonr4.MTRLK,">>>>>>9")
         SUBSTRING(tidut.UT,col7) = STRING(suaonr4.MONTKOST,">>>>>>>>9")
         SUBSTRING(tidut.UT,col9) = STRING(suaonr4.OVRIGKOST,">>>>>>>>9")         
         SUBSTRING(tidut.UT,col8) = STRING(suaonr4.PLANMASK * mapris + (suaonr4.PLANMONT * mopris) + suaonr4.MTRLK + suaonr4.OVRIGKOST,">>>>>>>>9")
         SUBSTRING(tidut.UT,col10) = STRING(suaonr4.BERTIM,">>>>9").
         CREATE tidut. 
         ASSIGN   
         SUBSTRING(tidut.UT,1) = "TOTALSUMMA UTFALL"
/*         SUBSTRING(tidut.UT,col1) = STRING(slutsum4.UEA,">>>>>9")*/
         SUBSTRING(tidut.UT,col3) = STRING(slutsum4.TIMMAR,">>>>9")
         SUBSTRING(tidut.UT,col4) = STRING(slutsum4.MBELOPP,">>>>>>9") 
         SUBSTRING(tidut.UT,col5) = STRING(slutsum4.MTIMMAR,">>>>9") 
         SUBSTRING(tidut.UT,col6) = STRING(slutsum4.MTRLK,">>>>>>9")
         SUBSTRING(tidut.UT,col7) = STRING(slutsum4.BELOPP,">>>>>>>>9") 
         SUBSTRING(tidut.UT,col8) = STRING((slutsum4.MBELOPP +  slutsum4.BELOPP + slutsum4.MTRLK),">>>>>>>>9")
         SUBSTRING(tidut.UT,col10) = STRING(slutsum4.BERTIM,">>>>9").
         CREATE tidut. 
         ASSIGN SUBSTRING(tidut.UT,1) = "=================".
         CREATE tidut.                    
         CREATE tidut. 
      END.                  
   END.   
   FIND FIRST suaonr5 NO-LOCK NO-ERROR.
   IF AVAILABLE suaonr5 THEN DO:    
      CREATE tidut.
      CREATE tidut.                        
      ASSIGN   
      SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gaok) + " SOM SAKNAR ANTINGEN " + CAPS(Guru.Konstanter:gplk) + " ELLER UTFALL".      
      CREATE tidut.                        
      ASSIGN   
      SUBSTRING(tidut.UT,1) = "==========================================".
      FOR EACH suaonr5:                                         
         CREATE tidut.                        
         IF suaonr5.KALKYL = "P1" THEN ASSIGN SUBSTRING(tidut.UT,1) = "PLANP1/ ".
         ELSE IF suaonr5.KALKYL = "P2" THEN ASSIGN SUBSTRING(tidut.UT,1) = "PLANP2/ ".
         ELSE IF suaonr5.KALKYL = "P3" THEN ASSIGN SUBSTRING(tidut.UT,1) = "PLANP3/ ".
         ELSE ASSIGN SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gplk) + "/ ".
      
         ASSIGN
         SUBSTRING(tidut.UT,34) = suaonr5.AONR
         SUBSTRING(tidut.UT,42) = STRING(suaonr5.DELNR,"999").
/*         SUBSTRING(tidut.UT,55) = STRING(suaonr5.PEA,">>>>9").*/
         IF suaonr5.AONRAVDATUM NE 01/01/91 THEN DO:
            ASSIGN SUBSTRING(tidut.UT,47) = STRING(suaonr5.AONRAVDATUM).
         END.
         ASSIGN
         SUBSTRING(tidut.UT,55) = STRING(suaonr5.PLANMONT,">>>9")   
         SUBSTRING(tidut.UT,60) = STRING(suaonr5.MASKKOST,">>>>>9")
         SUBSTRING(tidut.UT,67) = STRING(suaonr5.PLANMASK,">>>9")
         SUBSTRING(tidut.UT,72) = STRING(suaonr5.MTRLK,">>>>>>9")
         SUBSTRING(tidut.UT,81) = STRING(suaonr5.MONTKOST,">>>>>>9")
         SUBSTRING(tidut.UT,89) = STRING(suaonr5.OVRIGKOST,">>>>>>9")
         SUBSTRING(tidut.UT,97) = STRING(suaonr5.PLANMASK * mapris + (suaonr5.PLANMONT * mopris) + suaonr5.MTRLK + suaonr5.OVRIGKOST,">>>>>>9")
         SUBSTRING(tidut.UT,105) = STRING(suaonr5.BERTIM,">>>9").
      END.
   END.   
   FIND FIRST slutsum5 NO-LOCK NO-ERROR.
   IF AVAILABLE slutsum5 THEN DO: 
      FOR EACH slutsum5:
         CREATE tidut. 
         ASSIGN   
         SUBSTRING(tidut.UT,1) = "UTFALL"
         SUBSTRING(tidut.UT,8) = STRING(slutsum5.ORT,"X(25)")                      
         SUBSTRING(tidut.UT,34) = slutsum5.AONR  
         SUBSTRING(tidut.UT,42) = STRING(slutsum5.DELNR,"999").
         IF slutsum5.AONRAVDATUM NE 01/01/91 THEN ASSIGN SUBSTRING(tidut.UT,47) = STRING(slutsum5.AONRAVDATUM).
         ASSIGN
/*         SUBSTRING(tidut.UT,55) = STRING(slutsum5.UEA,">>>>9")
         SUBSTRING(tidut.UT,61) = STRING(slutsum5.RESULTAT,"99.99")*/
         SUBSTRING(tidut.UT,55) = STRING(slutsum5.TIMMAR,">>>9")
         SUBSTRING(tidut.UT,60) = STRING(slutsum5.MBELOPP,">>>>>9") 
         SUBSTRING(tidut.UT,67) = STRING(slutsum5.MTIMMAR,">>>9")
         SUBSTRING(tidut.UT,72) = STRING(slutsum5.MTRLK,">>>>>>9")
         SUBSTRING(tidut.UT,81) = STRING(slutsum5.BELOPP,">>>>>>9") 
         SUBSTRING(tidut.UT,97) = STRING((slutsum5.MBELOPP +  slutsum5.BELOPP + slutsum5.MTRLK),">>>>>>9")
         SUBSTRING(tidut.UT,105) = STRING(slutsum5.BERTIM,">>>9").
      END.
   END.      
END PROCEDURE.



