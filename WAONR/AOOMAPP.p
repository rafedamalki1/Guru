/*AOOMAPP.P*/
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}
FUNCTION klockan100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL ):

  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600.

END FUNCTION.


FUNCTION klockan60 RETURNS DECIMAL
  ( INPUT ber100 AS DECIMAL ):
  RETURN TRUNCATE(ber100,0) + ((ber100 - TRUNCATE(ber100,0)) / 100) * 60 . 

END FUNCTION.

DEFINE TEMP-TABLE aoval NO-UNDO
   FIELD AONR LIKE AONRTAB.AONR
   FIELD DELNR LIKE AONRTAB.DELNR
   FIELD AONRREC AS RECID
   INDEX AONR IS PRIMARY AONR DELNR.   
{TIDUTTT.I}

DEFINE TEMP-TABLE dagtemp
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD
   FIELD ANST LIKE ANSTFORMTAB.KOD 
   FIELD TRAAVTAL LIKE PERSONALTAB.TRAAVTAL
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR   
   FIELD BEFATTNING LIKE TIDREGITAB.OVERTIDTILL 
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP 
   FIELD TOTALT LIKE TIDREGITAB.TOTALT      
   FIELD LONTILLAGG LIKE TIDREGITAB.LONTILLAGG 
   FIELD LONTILLANTAL LIKE TIDREGITAB.LONTILLANTAL
   FIELD LONTILLANTALORG LIKE TIDREGITAB.LONTILLANTAL
   FIELD TRAKTANTAL LIKE TIDREGITAB.TRAKTANTAL 
   FIELD TRAKTKOD LIKE TIDREGITAB.TRAKTKOD      
   FIELD OANT1 LIKE TIDREGITAB.OANT1 
   FIELD OKOD1 LIKE TIDREGITAB.OKOD1
   FIELD OMRADE AS CHARACTER
   FIELD GEOMRADE AS CHARACTER
   INDEX PERSONALKOD IS PRIMARY PERSONALKOD AONR DELNR.
DEFINE TEMP-TABLE sumpers
   FIELD RESER AS LOGICAL
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD
   FIELD ANST LIKE ANSTFORMTAB.KOD
   FIELD TRAAVTAL LIKE PERSONALTAB.TRAAVTAL 
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR   
   FIELD BEFATTNING LIKE TIDREGITAB.OVERTIDTILL
   FIELD VIBEFATTNING LIKE BEFATTNING.NAMN 
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP 
   FIELD TOTALT LIKE TIDREGITAB.TOTALT      
   FIELD LONTILLAGG LIKE TIDREGITAB.LONTILLAGG   
   FIELD LONTILLANTAL LIKE TIDREGITAB.LONTILLANTAL
   FIELD LONTILLANTALORG LIKE TIDREGITAB.LONTILLANTAL
   FIELD TRAKTANTAL LIKE TIDREGITAB.TRAKTANTAL 
   FIELD TRAKTKOD LIKE TIDREGITAB.TRAKTKOD      
   FIELD OANT1 LIKE TIDREGITAB.OANT1 
   FIELD OKOD1 LIKE TIDREGITAB.OKOD1
   FIELD T1 LIKE TIDREGITAB.TRAKTANTAL
   FIELD T2 LIKE TIDREGITAB.TRAKTANTAL
   FIELD T3 LIKE TIDREGITAB.TRAKTANTAL
   FIELD T4 LIKE TIDREGITAB.TRAKTANTAL
   FIELD T5 LIKE TIDREGITAB.TRAKTANTAL
   FIELD T6 LIKE TIDREGITAB.TRAKTANTAL
   FIELD T7 LIKE TIDREGITAB.TRAKTANTAL
   FIELD O50 LIKE TIDREGITAB.OANT1
   FIELD O75 LIKE TIDREGITAB.OANT1
   FIELD O100 LIKE TIDREGITAB.OANT1
   FIELD O150 LIKE TIDREGITAB.OANT1
   FIELD RESTIM1 LIKE TIDREGITAB.TOTALT
   FIELD RESTIM2 LIKE TIDREGITAB.TOTALT
   FIELD RESTIM3 LIKE TIDREGITAB.TOTALT
   FIELD K50 LIKE TIDREGITAB.OANT1
   FIELD K75 LIKE TIDREGITAB.OANT1
   FIELD K100 LIKE TIDREGITAB.OANT1
   FIELD K150 LIKE TIDREGITAB.OANT1
   FIELD MIL LIKE TIDREGITAB.TRAKTANTAL
   FIELD NAMN AS CHARACTER
   INDEX AONR IS PRIMARY AONR DELNR PERSONALKOD
   INDEX PERSONALKOD PERSONALKOD
   INDEX BEFATTNING BEFATTNING
   INDEX LON LONTILLAGG ANST
   INDEX OKOD OKOD1 ANST
   INDEX TRAKT TRAKTKOD TRAAVTAL.
DEFINE TEMP-TABLE sumpersut
   FIELD RESER AS LOGICAL
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD
   FIELD ANST LIKE ANSTFORMTAB.KOD
   FIELD TRAAVTAL LIKE PERSONALTAB.TRAAVTAL 
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR   
   FIELD BEFATTNING LIKE TIDREGITAB.OVERTIDTILL
   FIELD VIBEFATTNING LIKE BEFATTNING.NAMN 
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP 
   FIELD TOTALT LIKE TIDREGITAB.TOTALT      
   FIELD LONTILLAGG LIKE TIDREGITAB.LONTILLAGG   
   FIELD LONTILLANTAL LIKE TIDREGITAB.LONTILLANTAL
   FIELD LONTILLANTALORG LIKE TIDREGITAB.LONTILLANTAL
   FIELD TRAKTANTAL LIKE TIDREGITAB.TRAKTANTAL 
   FIELD TRAKTKOD LIKE TIDREGITAB.TRAKTKOD      
   FIELD OANT1 LIKE TIDREGITAB.OANT1 
   FIELD OKOD1 LIKE TIDREGITAB.OKOD1
   FIELD T1 LIKE TIDREGITAB.TRAKTANTAL
   FIELD T2 LIKE TIDREGITAB.TRAKTANTAL
   FIELD T3 LIKE TIDREGITAB.TRAKTANTAL
   FIELD T4 LIKE TIDREGITAB.TRAKTANTAL
   FIELD T5 LIKE TIDREGITAB.TRAKTANTAL
   FIELD T6 LIKE TIDREGITAB.TRAKTANTAL
   FIELD T7 LIKE TIDREGITAB.TRAKTANTAL
   FIELD O50 LIKE TIDREGITAB.OANT1
   FIELD O75 LIKE TIDREGITAB.OANT1
   FIELD O100 LIKE TIDREGITAB.OANT1
   FIELD O150 LIKE TIDREGITAB.OANT1
   FIELD RESTIM1 LIKE TIDREGITAB.TOTALT
   FIELD RESTIM2 LIKE TIDREGITAB.TOTALT
   FIELD RESTIM3 LIKE TIDREGITAB.TOTALT
   FIELD K50 LIKE TIDREGITAB.OANT1
   FIELD K75 LIKE TIDREGITAB.OANT1
   FIELD K100 LIKE TIDREGITAB.OANT1
   FIELD K150 LIKE TIDREGITAB.OANT1
   FIELD MIL LIKE TIDREGITAB.TRAKTANTAL
   FIELD NAMN AS CHARACTER
   INDEX AONR IS PRIMARY AONR DELNR PERSONALKOD
   INDEX PERSONALKOD PERSONALKOD
   INDEX BEFATTNING BEFATTNING
   INDEX LON LONTILLAGG ANST
   INDEX OKOD OKOD1 ANST
   INDEX TRAKT TRAKTKOD TRAAVTAL.   
DEFINE TEMP-TABLE sumaonr
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR   
   FIELD TOTALT LIKE TIDREGITAB.TOTALT      
   FIELD T1 LIKE TIDREGITAB.TRAKTANTAL
   FIELD T2 LIKE TIDREGITAB.TRAKTANTAL
   FIELD T3 LIKE TIDREGITAB.TRAKTANTAL
   FIELD T4 LIKE TIDREGITAB.TRAKTANTAL
   FIELD T5 LIKE TIDREGITAB.TRAKTANTAL
   FIELD T6 LIKE TIDREGITAB.TRAKTANTAL
   FIELD T7 LIKE TIDREGITAB.TRAKTANTAL
   FIELD O50 LIKE TIDREGITAB.OANT1
   FIELD O75 LIKE TIDREGITAB.OANT1
   FIELD O100 LIKE TIDREGITAB.OANT1
   FIELD O150 LIKE TIDREGITAB.OANT1
   FIELD RESTIM1 LIKE TIDREGITAB.TOTALT
   FIELD RESTIM2 LIKE TIDREGITAB.TOTALT
   FIELD RESTIM3 LIKE TIDREGITAB.TOTALT
   FIELD K50 LIKE TIDREGITAB.OANT1
   FIELD K75 LIKE TIDREGITAB.OANT1
   FIELD K100 LIKE TIDREGITAB.OANT1
   FIELD K150 LIKE TIDREGITAB.OANT1
   FIELD MIL LIKE TIDREGITAB.TRAKTANTAL   
   INDEX AONR IS PRIMARY AONR DELNR.
   
DEFINE TEMP-TABLE restemp
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD
   FIELD ANST LIKE ANSTFORMTAB.KOD 
   FIELD TRAAVTAL LIKE PERSONALTAB.TRAAVTAL
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR   
   FIELD BEFATTNING LIKE TIDREGITAB.OVERTIDTILL 
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP    
   FIELD LONTILLAGG LIKE TIDREGITAB.LONTILLAGG 
   FIELD LONTILLANTAL LIKE TIDREGITAB.LONTILLANTAL
   FIELD LONTILLANTALORG LIKE TIDREGITAB.LONTILLANTAL
   FIELD TRAKTANTAL LIKE TIDREGITAB.TRAKTANTAL 
   FIELD TRAKTKOD LIKE TIDREGITAB.TRAKTKOD      
   FIELD OANT1 LIKE TIDREGITAB.OANT1 
   FIELD OKOD1 LIKE TIDREGITAB.OKOD1
   INDEX PERSONALKOD IS PRIMARY PERSONALKOD AONR DELNR.      
DEFINE QUERY pq FOR PERSONALTAB,TIDREGITAB.
DEFINE QUERY paq FOR PERSONALTAB,aoval,TIDREGITAB. 
   
DEFINE INPUT PARAMETER RAD_PERIOD AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER utomr LIKE OMRADETAB.OMRADE NO-UNDO. 
DEFINE INPUT PARAMETER bdatum AS DATE NO-UNDO. 
DEFINE INPUT PARAMETER avdatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER val1 AS LOGICAL NO-UNDO. 
DEFINE INPUT PARAMETER TABLE FOR aoval. 
DEFINE OUTPUT PARAMETER TABLE FOR tidut.
DEFINE OUTPUT PARAMETER str AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE OUTPUT PARAMETER str2 AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE OUTPUT PARAMETER str3 AS CHARACTER FORMAT "X(92)" NO-UNDO.

DEFINE VARIABLE aovar AS LOGICAL NO-UNDO.
DEFINE VARIABLE kodanst LIKE ANSTFORMTAB.KOD NO-UNDO.
DEFINE VARIABLE pkod LIKE PERSONALTAB.PERSONALKOD NO-UNDO.
DEFINE VARIABLE lonkod LIKE TIDREGITAB.LONTILLAGG NO-UNDO.
DEFINE VARIABLE aonummer LIKE AONRTAB.AONR NO-UNDO.
DEFINE VARIABLE delnummer LIKE AONRTAB.DELNR NO-UNDO.
DEFINE VARIABLE laonummer LIKE AONRTAB.AONR NO-UNDO.
DEFINE VARIABLE ldelnummer LIKE AONRTAB.DELNR NO-UNDO.
DEFINE VARIABLE utnr AS INTEGER EXTENT 50 NO-UNDO.
 


FIND FIRST FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
RUN STYRFORE.P (INPUT Guru.Konstanter:globforetag).
RUN open_UI.
RUN summa_UI.
RUN huvud_UI.
PROCEDURE huvud_UI :
   FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = utomr NO-LOCK NO-ERROR.
   CREATE tidut. 
   SUBSTRING(tidut.UT,60) = STRING(TODAY) + " " + STRING(TIME,"HH:MM").
   CREATE tidut.
   CREATE tidut.
   tidut.UT = "Underlag för löpande räkning / " + Guru.Konstanter:gomrk + " för " + 
               OMRADETAB.OMRADE + " " + OMRADETAB.NAMN. 
   IF RAD_PERIOD = 1 THEN DO: 
      SUBSTRING(tidut.UT,64) = " " + STRING(YEAR(bdatum),"9999").
   END.
   IF RAD_PERIOD = 2 THEN DO:
      SUBSTRING(tidut.UT,64) = " " +  STRING(bdatum) + " - " + STRING(avdatum).     
   END.
   ASSIGN
   utnr[1] = 1
   utnr[2] = 12
   utnr[3] = 30
   utnr[4] = 36
   utnr[5] = 40
   utnr[6] = 44
   utnr[7] = 49
   utnr[8] = 54
   utnr[9] = 58
   utnr[10] = 62
   utnr[11] = 66
   utnr[12] = 70
   utnr[13] = 74 
   utnr[14] = 78
   utnr[15] = 82
   utnr[16] = 86
   utnr[17] = 90
   utnr[18] = 94
   utnr[19] = 98
   utnr[20] = 102
   utnr[21] = 107
   utnr[22] = 112
   utnr[23] = 117.
   str = "".
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   DO WHILE i <= 132:
      i = i + 1.
      str = str + "=".      
   END.   
   i = 2.   
   DO WHILE i <= 23:             
      SUBSTRING(str,(utnr[i] - 1),1) = ".".      
      i = i + 1.
   END.   
   CREATE tidut.  
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,utnr[1]) = CAPS(Guru.Konstanter:gaok)
   SUBSTRING(tidut.UT,utnr[2]) = "NAMN"
   SUBSTRING(tidut.UT,utnr[3]) = "TIM." 
   SUBSTRING(tidut.UT,utnr[4]) = "ÖVERTIDSTIMMAR"  
   SUBSTRING(tidut.UT,utnr[8]) = "TRAKTAMENTE"   
   SUBSTRING(tidut.UT,utnr[15]) = "RESTIDSTIM."
   SUBSTRING(tidut.UT,utnr[18]) = "KÖRTIDSTIMMAR"
   SUBSTRING(tidut.UT,utnr[22]) = "KÖR."                  
   SUBSTRING(tidut.UT,utnr[23]) = "FAKTURABEFATTN.".   
   CREATE tidut.   
   ASSIGN
   SUBSTRING(tidut.UT,utnr[4]) = "50%"
   SUBSTRING(tidut.UT,utnr[5]) = "75%"
   SUBSTRING(tidut.UT,utnr[6]) = "100%"
   SUBSTRING(tidut.UT,utnr[7]) = "150%"  
   SUBSTRING(tidut.UT,utnr[8]) = "821"
   SUBSTRING(tidut.UT,utnr[9]) = "831"
   SUBSTRING(tidut.UT,utnr[10]) = "832"
   SUBSTRING(tidut.UT,utnr[11]) = "842"
   SUBSTRING(tidut.UT,utnr[12]) = "843"
   SUBSTRING(tidut.UT,utnr[13]) = "852"
   SUBSTRING(tidut.UT,utnr[14]) = "855"   
   SUBSTRING(tidut.UT,utnr[15]) = "086"
   SUBSTRING(tidut.UT,utnr[16]) = "080"
   SUBSTRING(tidut.UT,utnr[17]) = "081"
   SUBSTRING(tidut.UT,utnr[18]) = "50%"
   SUBSTRING(tidut.UT,utnr[19]) = "75%"
   SUBSTRING(tidut.UT,utnr[20]) = "100%"
   SUBSTRING(tidut.UT,utnr[21]) = "150%"
   SUBSTRING(tidut.UT,utnr[22]) = "MIL".
   
   ASSIGN
   SUBSTRING(str2,utnr[1]) = CAPS(Guru.Konstanter:gaok)
   SUBSTRING(str2,utnr[2]) = "NAMN"
   SUBSTRING(str2,utnr[3]) = "TIM." 
   SUBSTRING(str2,utnr[4]) = "ÖVERTIDSTIMMAR"  
   SUBSTRING(str2,utnr[8]) = "TRAKTAMENTE"   
   SUBSTRING(str2,utnr[15]) = "RESTIDSTIM."
   SUBSTRING(str2,utnr[18]) = "KÖRTIDSTIMMAR"
   SUBSTRING(str2,utnr[22]) = "KÖR."                  
   SUBSTRING(str2,utnr[23]) = "FAKTURABEFATTN."
   SUBSTRING(str3,utnr[4]) = "50%"
   SUBSTRING(str3,utnr[5]) = "75%"
   SUBSTRING(str3,utnr[6]) = "100%"
   SUBSTRING(str3,utnr[7]) = "150%"  
   SUBSTRING(str3,utnr[8]) = "821"
   SUBSTRING(str3,utnr[9]) = "831"
   SUBSTRING(str3,utnr[10]) = "832"
   SUBSTRING(str3,utnr[11]) = "842"
   SUBSTRING(str3,utnr[12]) = "843"
   SUBSTRING(str3,utnr[13]) = "852"
   SUBSTRING(str3,utnr[14]) = "855"   
   SUBSTRING(str3,utnr[15]) = "086"
   SUBSTRING(str3,utnr[16]) = "080"
   SUBSTRING(str3,utnr[17]) = "081"
   SUBSTRING(str3,utnr[18]) = "50%"
   SUBSTRING(str3,utnr[19]) = "75%"
   SUBSTRING(str3,utnr[20]) = "100%"
   SUBSTRING(str3,utnr[21]) = "150%"
   SUBSTRING(str3,utnr[22]) = "MIL".

   CREATE tidut.
   tidut.UT = str.
   
   FIND LAST sumpers USE-INDEX AONR NO-ERROR.
   IF AVAILABLE sumpers THEN DO:
      ASSIGN 
      laonummer = sumpers.AONR 
      ldelnummer = sumpers.DELNR.
   END.
   FOR EACH sumpersut USE-INDEX AONR:
      IF aonummer NE "" THEN DO:
         IF aonummer = sumpersut.AONR AND delnummer = sumpersut.DELNR THEN aonummer = aonummer.
         ELSE DO:
            FIND FIRST sumaonr WHERE sumaonr.AONR = aonummer AND sumaonr.DELNR = delnummer 
            NO-ERROR.
            IF AVAILABLE sumaonr THEN DO:
               CREATE tidut.
               ASSIGN
               SUBSTRING(tidut.UT,utnr[1]) = "SUMMA " + CAPS(Guru.Konstanter:gaok)
               SUBSTRING(tidut.UT,utnr[3]) = STRING(sumaonr.TOTALT,">>>>9") 
               SUBSTRING(tidut.UT,utnr[4]) = STRING(sumaonr.O50,">>9")  
               SUBSTRING(tidut.UT,utnr[5]) = STRING(sumaonr.O75,">>9")   
               SUBSTRING(tidut.UT,utnr[6]) = STRING(sumaonr.O100,">>>9")
               SUBSTRING(tidut.UT,utnr[7]) = STRING(sumaonr.O150,">>>9")
               SUBSTRING(tidut.UT,utnr[8]) = STRING(sumaonr.T1,">>9")
               SUBSTRING(tidut.UT,utnr[9]) = STRING(sumaonr.T2,">>9")
               SUBSTRING(tidut.UT,utnr[10]) = STRING(sumaonr.T3,">>9")
               SUBSTRING(tidut.UT,utnr[11]) = STRING(sumaonr.T4,">>9")
               SUBSTRING(tidut.UT,utnr[12]) = STRING(sumaonr.T5,">>9")
               SUBSTRING(tidut.UT,utnr[13]) = STRING(sumaonr.T6,">>9")
               SUBSTRING(tidut.UT,utnr[14]) = STRING(sumaonr.T7,">>9")   
               SUBSTRING(tidut.UT,utnr[15]) = STRING(sumaonr.RESTIM1,">>9")
               SUBSTRING(tidut.UT,utnr[16]) = STRING(sumaonr.RESTIM2,">>9")
               SUBSTRING(tidut.UT,utnr[17]) = STRING(sumaonr.RESTIM3,">>9")
               SUBSTRING(tidut.UT,utnr[18]) = STRING(sumaonr.K50,">>9")
               SUBSTRING(tidut.UT,utnr[19]) = STRING(sumaonr.K75,">>9")
               SUBSTRING(tidut.UT,utnr[20]) = STRING(sumaonr.K100,">>>9")
               SUBSTRING(tidut.UT,utnr[21]) = STRING(sumaonr.K150,">>>9")
               SUBSTRING(tidut.UT,utnr[22]) = STRING(sumaonr.MIL,">>>9").
            END.
            CREATE tidut.
         END.
      END.
      IF aonummer = sumpersut.AONR AND delnummer = sumpersut.DELNR THEN aonummer = aonummer.
      ELSE DO:
         ASSIGN 
         aonummer = sumpersut.AONR 
         delnummer = sumpersut.DELNR.
         FIND FIRST AONRTAB WHERE AONRTAB.AONR = sumpersut.AONR AND 
         AONRTAB.DELNR = sumpersut.DELNR NO-LOCK NO-ERROR.
         IF AVAILABLE AONRTAB THEN DO:            
            CREATE tidut.
            ASSIGN
            SUBSTRING(tidut.UT,utnr[1]) = sumpersut.AONR   
            SUBSTRING(tidut.UT,8) = STRING(sumpersut.DELNR,Guru.Konstanter:varforetypchar[1])                                                       
            SUBSTRING(tidut.UT,utnr[2]) = SUBSTRING(AONRTAB.ORT,1,17).
         END.   
      END.  
      IF val1 = TRUE THEN DO:
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,5) = sumpersut.PERSONALKOD         
         SUBSTRING(tidut.UT,utnr[2]) = sumpersut.NAMN
         SUBSTRING(tidut.UT,utnr[3]) = STRING(sumpersut.TOTALT,">>>>9") 
         SUBSTRING(tidut.UT,utnr[4]) = STRING(sumpersut.O50,">>9")  
         SUBSTRING(tidut.UT,utnr[5]) = STRING(sumpersut.O75,">>9")   
         SUBSTRING(tidut.UT,utnr[6]) = STRING(sumpersut.O100,">>>9")
         SUBSTRING(tidut.UT,utnr[7]) = STRING(sumpersut.O150,">>>9")
         SUBSTRING(tidut.UT,utnr[8]) = STRING(sumpersut.T1,">>9")
         SUBSTRING(tidut.UT,utnr[9]) = STRING(sumpersut.T2,">>9")
         SUBSTRING(tidut.UT,utnr[10]) = STRING(sumpersut.T3,">>9")
         SUBSTRING(tidut.UT,utnr[11]) = STRING(sumpersut.T4,">>9")
         SUBSTRING(tidut.UT,utnr[12]) = STRING(sumpersut.T5,">>9")
         SUBSTRING(tidut.UT,utnr[13]) = STRING(sumpersut.T6,">>9")
         SUBSTRING(tidut.UT,utnr[14]) = STRING(sumpersut.T7,">>9")   
         SUBSTRING(tidut.UT,utnr[15]) = STRING(sumpersut.RESTIM1,">>9")
         SUBSTRING(tidut.UT,utnr[16]) = STRING(sumpersut.RESTIM2,">>9")
         SUBSTRING(tidut.UT,utnr[17]) = STRING(sumpersut.RESTIM3,">>9")
         SUBSTRING(tidut.UT,utnr[18]) = STRING(sumpersut.K50,">>9")
         SUBSTRING(tidut.UT,utnr[19]) = STRING(sumpersut.K75,">>9")
         SUBSTRING(tidut.UT,utnr[20]) = STRING(sumpersut.K100,">>>9")
         SUBSTRING(tidut.UT,utnr[21]) = STRING(sumpersut.K150,">>>9")
         SUBSTRING(tidut.UT,utnr[22]) = STRING(sumpersut.MIL,">>>9")                  
         SUBSTRING(tidut.UT,utnr[23]) = sumpersut.VIBEFATTNING.
      END.
   END.
   FIND FIRST sumaonr WHERE sumaonr.AONR = laonummer AND 
   sumaonr.DELNR = ldelnummer NO-ERROR.
   IF AVAILABLE sumaonr THEN DO:
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,utnr[1]) = "SUMMA " + CAPS(Guru.Konstanter:gaok)
      SUBSTRING(tidut.UT,utnr[3]) = STRING(sumaonr.TOTALT,">>>>9") 
      SUBSTRING(tidut.UT,utnr[4]) = STRING(sumaonr.O50,">>9")  
      SUBSTRING(tidut.UT,utnr[5]) = STRING(sumaonr.O75,">>9")   
      SUBSTRING(tidut.UT,utnr[6]) = STRING(sumaonr.O100,">>>9")
      SUBSTRING(tidut.UT,utnr[7]) = STRING(sumaonr.O150,">>>9")
      SUBSTRING(tidut.UT,utnr[8]) = STRING(sumaonr.T1,">>9")
      SUBSTRING(tidut.UT,utnr[9]) = STRING(sumaonr.T2,">>9")
      SUBSTRING(tidut.UT,utnr[10]) = STRING(sumaonr.T3,">>9")
      SUBSTRING(tidut.UT,utnr[11]) = STRING(sumaonr.T4,">>9")
      SUBSTRING(tidut.UT,utnr[12]) = STRING(sumaonr.T5,">>9")
      SUBSTRING(tidut.UT,utnr[13]) = STRING(sumaonr.T6,">>9")
      SUBSTRING(tidut.UT,utnr[14]) = STRING(sumaonr.T7,">>9")   
      SUBSTRING(tidut.UT,utnr[15]) = STRING(sumaonr.RESTIM1,">>9")
      SUBSTRING(tidut.UT,utnr[16]) = STRING(sumaonr.RESTIM2,">>9")
      SUBSTRING(tidut.UT,utnr[17]) = STRING(sumaonr.RESTIM3,">>9")
      SUBSTRING(tidut.UT,utnr[18]) = STRING(sumaonr.K50,">>9")
      SUBSTRING(tidut.UT,utnr[19]) = STRING(sumaonr.K75,">>9")
      SUBSTRING(tidut.UT,utnr[20]) = STRING(sumaonr.K100,">>>9")
      SUBSTRING(tidut.UT,utnr[21]) = STRING(sumaonr.K150,">>>9")
      SUBSTRING(tidut.UT,utnr[22]) = STRING(sumaonr.MIL,">>>9").
   END.
END PROCEDURE.
PROCEDURE open_UI :
   FIND FIRST aoval NO-LOCK NO-ERROR.
   IF NOT AVAILABLE aoval THEN DO:
      aovar = FALSE.
      IF RAD_PERIOD = 1 THEN DO:
         OPEN QUERY pq FOR EACH PERSONALTAB WHERE PERSONALTAB.OMRADE = utomr NO-LOCK,
         EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         YEAR(TIDREGITAB.DATUM) = YEAR(bdatum) AND 
         TIDREGITAB.VECKOKORD NE "" NO-LOCK BY TIDREGITAB.PERSONALKOD.          
      END.
      IF RAD_PERIOD = 2 THEN DO:         
         OPEN QUERY pq FOR EACH PERSONALTAB WHERE PERSONALTAB.OMRADE = utomr NO-LOCK,
         EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM >= bdatum AND TIDREGITAB.DATUM <= avdatum AND 
         TIDREGITAB.VECKOKORD NE "" NO-LOCK BY TIDREGITAB.PERSONALKOD.          
      END.
      RUN skapadag_UI.
   END.
   ELSE DO:
      aovar = TRUE.
      IF RAD_PERIOD = 1 THEN DO:
         OPEN QUERY paq FOR EACH PERSONALTAB WHERE PERSONALTAB.OMRADE = utomr NO-LOCK,
         EACH aoval NO-LOCK,
         EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.AONR = aoval.AONR AND TIDREGITAB.DELNR = aoval.DELNR AND
         YEAR(TIDREGITAB.DATUM) = YEAR(bdatum) AND 
         TIDREGITAB.VECKOKORD NE "" NO-LOCK BY TIDREGITAB.PERSONALKOD.          
      END.
      IF RAD_PERIOD = 2 THEN DO:         
         OPEN QUERY paq FOR EACH PERSONALTAB WHERE PERSONALTAB.OMRADE = utomr NO-LOCK,
         EACH aoval NO-LOCK,
         EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.AONR = aoval.AONR AND TIDREGITAB.DELNR = aoval.DELNR AND
         TIDREGITAB.DATUM >= bdatum AND TIDREGITAB.DATUM <= avdatum AND 
         TIDREGITAB.VECKOKORD NE "" NO-LOCK BY TIDREGITAB.PERSONALKOD. 
      END.
      RUN skapadag_UI.
   END.
END PROCEDURE.
PROCEDURE skapadag_UI :
   IF aovar = FALSE THEN DO:
      GET FIRST pq NO-LOCK.
   END.
   ELSE DO:
      GET FIRST paq NO-LOCK.      
   END.
   DO WHILE AVAILABLE(PERSONALTAB):
     
      IF TIDREGITAB.AONR NE "" THEN DO:
         IF pkod NE PERSONALTAB.PERSONALKOD THEN DO:
            pkod = PERSONALTAB.PERSONALKOD.              
            FIND FIRST ANSTFORMTAB WHERE
            ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
            USE-INDEX ANSTF NO-LOCK NO-ERROR.         
            kodanst = ANSTFORMTAB.KOD.
         END. 
         IF TIDREGITAB.PRISTYP = "RESTID..." THEN DO:
            CREATE restemp.
            ASSIGN      
            restemp.PERSONALKOD = TIDREGITAB.PERSONALKOD 
            restemp.ANST = kodanst
            restemp.TRAAVTAL = PERSONALTAB.TRAAVTAL
            restemp.AONR = TIDREGITAB.AONR
            restemp.DELNR = TIDREGITAB.DELNR   
            restemp.BEFATTNING = TIDREGITAB.OVERTIDTILL 
            restemp.PRISTYP = TIDREGITAB.PRISTYP
            restemp.TRAKTANTAL = TIDREGITAB.TRAKTANTAL 
            restemp.TRAKTKOD = TIDREGITAB.TRAKTKOD
            restemp.OKOD1 = TIDREGITAB.OKOD1
            restemp.LONTILLAGG = TIDREGITAB.LONTILLAGG
            restemp.LONTILLANTALORG = TIDREGITAB.LONTILLANTAL.
            ASSIGN         
            restemp.LONTILLANTAL = klockan100(TIDREGITAB.LONTILLANTAL)
            restemp.OANT1 = klockan100(TIDREGITAB.OANT1).         
            IF TIDREGITAB.OVERTIDTILL = "" THEN DO:
               restemp.BEFATTNING = PERSONALTAB.BEFATTNING.
            END.
            IF TIDREGITAB.OKOD2 NE "" THEN DO:  
               CREATE restemp.
               ASSIGN      
               restemp.PERSONALKOD = TIDREGITAB.PERSONALKOD 
               restemp.ANST = kodanst
               restemp.TRAAVTAL = PERSONALTAB.TRAAVTAL
               restemp.AONR = TIDREGITAB.AONR
               restemp.DELNR = TIDREGITAB.DELNR   
               restemp.BEFATTNING = TIDREGITAB.OVERTIDTILL 
               restemp.PRISTYP = TIDREGITAB.PRISTYP         
               restemp.OKOD1 = TIDREGITAB.OKOD2. 
               ASSIGN
               restemp.OANT1 = klockan100(TIDREGITAB.OANT2).
               IF TIDREGITAB.OVERTIDTILL = "" THEN DO:
                  restemp.BEFATTNING = PERSONALTAB.BEFATTNING.
               END.
            END.
            IF TIDREGITAB.OKOD3 NE "" THEN DO: 
               CREATE restemp.
               ASSIGN      
               restemp.PERSONALKOD = TIDREGITAB.PERSONALKOD 
               restemp.ANST = kodanst
               restemp.TRAAVTAL = PERSONALTAB.TRAAVTAL
               restemp.AONR = TIDREGITAB.AONR
               restemp.DELNR = TIDREGITAB.DELNR   
               restemp.BEFATTNING = TIDREGITAB.OVERTIDTILL 
               restemp.PRISTYP = TIDREGITAB.PRISTYP             
               restemp.OKOD1 = TIDREGITAB.OKOD3.
               ASSIGN
               restemp.OANT1 = klockan100(TIDREGITAB.OANT3).
               IF TIDREGITAB.OVERTIDTILL = "" THEN DO:
                  restemp.BEFATTNING = PERSONALTAB.BEFATTNING.
               END. 
            END.
         END.
         ELSE IF TIDREGITAB.PRISTYP = "FRÅNVARO." THEN DO:
            val1 = val1.
         END.
         ELSE DO:
            CREATE dagtemp.
            ASSIGN      
            dagtemp.PERSONALKOD = TIDREGITAB.PERSONALKOD 
            dagtemp.ANST = kodanst
            dagtemp.TRAAVTAL = PERSONALTAB.TRAAVTAL
            dagtemp.AONR = TIDREGITAB.AONR
            dagtemp.DELNR = TIDREGITAB.DELNR   
            dagtemp.BEFATTNING = TIDREGITAB.OVERTIDTILL 
            dagtemp.PRISTYP = TIDREGITAB.PRISTYP             
            dagtemp.LONTILLAGG = TIDREGITAB.LONTILLAGG 
            dagtemp.LONTILLANTAL = TIDREGITAB.LONTILLANTAL
            dagtemp.LONTILLANTALORG = TIDREGITAB.LONTILLANTAL
            dagtemp.TRAKTANTAL = TIDREGITAB.TRAKTANTAL 
            dagtemp.TRAKTKOD = TIDREGITAB.TRAKTKOD            
            dagtemp.OKOD1 = TIDREGITAB.OKOD1.
            ASSIGN
            dagtemp.TOTALT = klockan100(TIDREGITAB.TOTALT).
            IF TIDREGITAB.OVERTIDTILL = "" THEN DO:
               dagtemp.BEFATTNING = PERSONALTAB.BEFATTNING.
            END.            
            IF TIDREGITAB.OKOD1 NE "" THEN DO:
               dagtemp.OANT1 = klockan100(TIDREGITAB.OANT1).         
               dagtemp.TOTALT = 0.
            END.
            IF TIDREGITAB.OKOD2 NE "" THEN DO:  
               CREATE dagtemp.
               ASSIGN      
               dagtemp.PERSONALKOD = TIDREGITAB.PERSONALKOD
               dagtemp.ANST = kodanst 
               dagtemp.TRAAVTAL = PERSONALTAB.TRAAVTAL
               dagtemp.AONR = TIDREGITAB.AONR
               dagtemp.DELNR = TIDREGITAB.DELNR   
               dagtemp.BEFATTNING = TIDREGITAB.OVERTIDTILL 
               dagtemp.PRISTYP = TIDREGITAB.PRISTYP         
               dagtemp.OKOD1 = TIDREGITAB.OKOD2. 
               ASSIGN
               dagtemp.OANT1 = klockan100(TIDREGITAB.OANT2).
               IF TIDREGITAB.OVERTIDTILL = "" THEN DO:
                  dagtemp.BEFATTNING = PERSONALTAB.BEFATTNING.
               END.
            END.
            IF TIDREGITAB.OKOD3 NE "" THEN DO: 
               CREATE dagtemp.
               ASSIGN      
               dagtemp.PERSONALKOD = TIDREGITAB.PERSONALKOD 
               dagtemp.ANST = kodanst
               dagtemp.TRAAVTAL = PERSONALTAB.TRAAVTAL
               dagtemp.AONR = TIDREGITAB.AONR
               dagtemp.DELNR = TIDREGITAB.DELNR   
               dagtemp.BEFATTNING = TIDREGITAB.OVERTIDTILL 
               dagtemp.PRISTYP = TIDREGITAB.PRISTYP             
               dagtemp.OKOD1 = TIDREGITAB.OKOD3.
               ASSIGN
               dagtemp.OANT1 = klockan100(TIDREGITAB.OANT3).
               IF TIDREGITAB.OVERTIDTILL = "" THEN DO:
                  dagtemp.BEFATTNING = PERSONALTAB.BEFATTNING.
               END. 
            END.
         END.
      END. 
      IF aovar = FALSE THEN DO:
         GET NEXT pq NO-LOCK.
      END.
      ELSE DO:
         GET NEXT paq NO-LOCK.
      END.
   END.
END PROCEDURE.

PROCEDURE summa_UI.
   FOR EACH dagtemp BREAK BY 
   dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSONALKOD BY dagtemp.BEFATTNING BY 
   dagtemp.TRAKTKOD BY dagtemp.LONTILLAGG BY dagtemp.OKOD1:      
         
      ACCUMULATE dagtemp.TOTALT (TOTAL BY 
      dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSONALKOD BY dagtemp.BEFATTNING BY 
      dagtemp.TRAKTKOD BY dagtemp.LONTILLAGG BY dagtemp.OKOD1).               
      
      ACCUMULATE dagtemp.LONTILLANTAL (TOTAL BY 
      dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSONALKOD BY dagtemp.BEFATTNING BY 
      dagtemp.TRAKTKOD BY dagtemp.LONTILLAGG BY dagtemp.OKOD1).
      
      ACCUMULATE dagtemp.LONTILLANTALORG (TOTAL BY 
      dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSONALKOD BY dagtemp.BEFATTNING BY 
      dagtemp.TRAKTKOD BY dagtemp.LONTILLAGG BY dagtemp.OKOD1).
      
      ACCUMULATE dagtemp.TRAKTANTAL (TOTAL BY 
      dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSONALKOD BY dagtemp.BEFATTNING BY 
      dagtemp.TRAKTKOD BY dagtemp.LONTILLAGG BY dagtemp.OKOD1).
      
      ACCUMULATE dagtemp.OANT1 (TOTAL BY 
      dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSONALKOD BY dagtemp.BEFATTNING BY 
      dagtemp.TRAKTKOD BY dagtemp.LONTILLAGG BY dagtemp.OKOD1).
      IF LAST-OF(dagtemp.OKOD1) THEN DO:
         CREATE sumpers.         
         ASSIGN
         sumpers.RESER = FALSE         
         sumpers.PERSONALKOD = dagtemp.PERSONALKOD
         sumpers.ANST = dagtemp.ANST 
         sumpers.TRAAVTAL = dagtemp.TRAAVTAL
         sumpers.AONR = dagtemp.AONR
         sumpers.DELNR = dagtemp.DELNR   
         sumpers.BEFATTNING = dagtemp.BEFATTNING 
         sumpers.PRISTYP = dagtemp.PRISTYP 
         sumpers.LONTILLAGG = dagtemp.LONTILLAGG
         sumpers.TRAKTKOD = dagtemp.TRAKTKOD      
         sumpers.OKOD1 = dagtemp.OKOD1
         sumpers.TOTALT = (ACCUM TOTAL BY dagtemp.OKOD1 dagtemp.TOTALT)               
         sumpers.LONTILLANTAL = (ACCUM TOTAL BY dagtemp.OKOD1 dagtemp.LONTILLANTAL)
         sumpers.LONTILLANTALORG = (ACCUM TOTAL BY dagtemp.OKOD1 dagtemp.LONTILLANTALORG)
         sumpers.TRAKTANTAL = (ACCUM TOTAL BY dagtemp.OKOD1 dagtemp.TRAKTANTAL) 
         sumpers.OANT1 = (ACCUM TOTAL BY dagtemp.OKOD1 dagtemp.OANT1). 
      END.   
   END.
   FOR EACH restemp BREAK BY 
   restemp.AONR BY restemp.DELNR BY restemp.PERSONALKOD BY restemp.BEFATTNING BY 
   restemp.TRAKTKOD BY restemp.LONTILLAGG BY restemp.OKOD1:      
      
      ACCUMULATE restemp.LONTILLANTAL (TOTAL BY 
      restemp.AONR BY restemp.DELNR BY restemp.PERSONALKOD BY restemp.BEFATTNING by
      restemp.TRAKTKOD BY restemp.LONTILLAGG BY restemp.OKOD1).
      
      ACCUMULATE restemp.LONTILLANTALORG (TOTAL BY 
      restemp.AONR BY restemp.DELNR BY restemp.PERSONALKOD BY restemp.BEFATTNING by
      restemp.TRAKTKOD BY restemp.LONTILLAGG BY restemp.OKOD1).
      
      ACCUMULATE restemp.TRAKTANTAL (TOTAL BY 
      restemp.AONR BY restemp.DELNR BY restemp.PERSONALKOD BY restemp.BEFATTNING BY 
      restemp.TRAKTKOD BY restemp.LONTILLAGG BY restemp.OKOD1).
      
      ACCUMULATE restemp.OANT1 (TOTAL BY 
      restemp.AONR BY restemp.DELNR BY restemp.PERSONALKOD BY restemp.BEFATTNING BY 
      restemp.TRAKTKOD BY restemp.LONTILLAGG BY restemp.OKOD1).
      IF LAST-OF(restemp.OKOD1) THEN DO:
         CREATE sumpers.         
         ASSIGN
         sumpers.RESER = TRUE
         sumpers.PERSONALKOD = restemp.PERSONALKOD
         sumpers.ANST = restemp.ANST
         sumpers.TRAAVTAL = restemp.TRAAVTAL 
         sumpers.AONR = restemp.AONR
         sumpers.DELNR = restemp.DELNR   
         sumpers.BEFATTNING = restemp.BEFATTNING 
         sumpers.PRISTYP = restemp.PRISTYP 
         sumpers.LONTILLAGG = restemp.LONTILLAGG
         sumpers.TRAKTKOD = restemp.TRAKTKOD      
         sumpers.OKOD1 = restemp.OKOD1         
         sumpers.LONTILLANTAL = (ACCUM TOTAL BY restemp.OKOD1 restemp.LONTILLANTAL)
         sumpers.LONTILLANTALORG = (ACCUM TOTAL BY restemp.OKOD1 restemp.LONTILLANTALORG)
         sumpers.TRAKTANTAL = (ACCUM TOTAL BY restemp.OKOD1 restemp.TRAKTANTAL) 
         sumpers.OANT1 = (ACCUM TOTAL BY restemp.OKOD1 restemp.OANT1). 
      END.   
   END.
   
   /*LÖNETILLÄGG FÖR RESOR och NORMALTID*/
   REPEAT:
      FIND FIRST sumpers WHERE sumpers.LONTILLAGG NE "" NO-ERROR.
      IF NOT AVAILABLE sumpers THEN LEAVE.
      ASSIGN
      kodanst = sumpers.ANST
      lonkod = sumpers.LONTILLAGG.
      FIND FIRST LONTILL WHERE LONTILL.KOD = sumpers.ANST AND
      LONTILL.LONTILLAGG = sumpers.LONTILLAGG NO-LOCK NO-ERROR.   
      IF AVAILABLE LONTILL THEN DO:         
         FOR EACH sumpers WHERE sumpers.LONTILLAGG = lonkod AND sumpers.ANST = kodanst:
            IF SUBSTRING(LONTILL.TYPKOD,1,3) = "MIL" THEN DO:
               sumpers.MIL = sumpers.LONTILLANTALORG.
            END.
            IF sumpers.RESER = TRUE THEN DO:
               IF LONTILL.VILART = "086" THEN DO: 
                  sumpers.RESTIM1 = sumpers.LONTILLANTAL.
               END.
               ELSE IF LONTILL.VILART = "080" THEN DO: 
                  sumpers.RESTIM2 = sumpers.LONTILLANTAL.
               END.
               ELSE IF LONTILL.VILART = "081" THEN DO: 
                  sumpers.RESTIM3 = sumpers.LONTILLANTAL.
               END.
               ELSE IF LONTILL.VILART = "091" THEN DO: 
                  sumpers.K50 = sumpers.LONTILLANTAL.
               END. /*092 ????*/
               ELSE IF LONTILL.VILART = "092" THEN DO: 
                  IF kodanst BEGINS "T" THEN sumpers.K75 = sumpers.LONTILLANTAL.
                  ELSE sumpers.K100 = sumpers.LONTILLANTAL.
               END.
               ELSE IF LONTILL.VILART = "093" THEN DO:                   
                  sumpers.K100 = sumpers.LONTILLANTAL.
               END.
               ELSE IF LONTILL.VILART = "094" THEN DO: 
                  sumpers.K150 = sumpers.LONTILLANTAL.
               END.            
            END.
            ASSIGN
            sumpers.LONTILLAGG = ""
            sumpers.LONTILLANTAL = 0.            
         END.        
      END.
      ELSE DO:
         FOR EACH sumpers WHERE sumpers.RESER = TRUE AND
         sumpers.LONTILLAGG = lonkod AND sumpers.ANST = kodanst:
            ASSIGN
            sumpers.LONTILLAGG = ""
            sumpers.LONTILLANTAL = 0. 
         END.
      END.
   END.    
   /*ÖVERTIDSTILLÄGG NORMALTID OCH RESOR*/
   REPEAT:
      FIND FIRST sumpers WHERE sumpers.OKOD1 NE "" NO-ERROR.
      IF NOT AVAILABLE sumpers THEN LEAVE.
      ASSIGN
      kodanst = sumpers.ANST 
      lonkod = sumpers.OKOD1.
      FIND FIRST OVERKOD WHERE OVERKOD.KOD = kodanst AND
      OVERKOD.OVERTIDTILL = sumpers.OKOD1 NO-LOCK NO-ERROR.         
      IF AVAILABLE OVERKOD THEN DO:
         FOR EACH sumpers WHERE sumpers.OKOD1 = lonkod AND sumpers.ANST = kodanst:
            IF sumpers.RESER = TRUE THEN DO:
               IF OVERKOD.VILART = "331" OR OVERKOD.VILART = "321" THEN DO: 
                  sumpers.K50 = sumpers.OANT1.
               END.                                         /*  327  ????*/
               IF OVERKOD.VILART = "332" OR OVERKOD.VILART = "322" THEN DO: 
                  sumpers.K75 = sumpers.OANT1.
               END.
               IF OVERKOD.VILART = "333" OR OVERKOD.VILART = "323" THEN DO: 
                  sumpers.K100 = sumpers.OANT1.
               END.
               IF OVERKOD.VILART = "334" OR OVERKOD.VILART = "324" THEN DO: 
                  sumpers.K150 = sumpers.OANT1.
               END.               
            END.            
            ELSE DO:
               IF OVERKOD.VILART = "310" OR OVERKOD.VILART = "315" OR 
                  OVERKOD.VILART = "376" OR OVERKOD.VILART = "380" OR
                  OVERKOD.VILART = "382" OR OVERKOD.VILART = "317" OR
                  OVERKOD.VILART = "331" OR OVERKOD.VILART = "321" THEN DO: 
                  sumpers.O50 = sumpers.OANT1.
               END.                                         
               IF OVERKOD.VILART = "320" OR OVERKOD.VILART = "325" OR
                  OVERKOD.VILART = "327" OR 
                  OVERKOD.VILART = "332" OR OVERKOD.VILART = "322" THEN DO: 
                  sumpers.O75 = sumpers.OANT1.
               END.
               IF OVERKOD.VILART = "330" OR OVERKOD.VILART = "335" OR 
                  OVERKOD.VILART = "377" OR OVERKOD.VILART = "381" OR
                  OVERKOD.VILART = "333" OR OVERKOD.VILART = "323" THEN DO: 
                  sumpers.O100 = sumpers.OANT1.
               END.
               IF OVERKOD.VILART = "340" OR OVERKOD.VILART = "377" OR
                  OVERKOD.VILART = "334" OR OVERKOD.VILART = "324" THEN DO: 
                  sumpers.O150 = sumpers.OANT1.
               END.               
            END.
            ASSIGN
            sumpers.OKOD1 = ""
            sumpers.OANT1 = 0.            
         END. 
      END.
      ELSE DO:
         FOR EACH sumpers WHERE sumpers.OKOD1 = lonkod AND sumpers.ANST = kodanst:
            ASSIGN
            sumpers.OKOD1 = ""
            sumpers.OANT1 = 0. 
         END.
      END.
   END.
   /*TRAKTAMENTE*/
   REPEAT:
      FIND FIRST sumpers WHERE sumpers.TRAKTKOD NE "" NO-ERROR.
      IF NOT AVAILABLE sumpers THEN LEAVE.
      ASSIGN
      kodanst = sumpers.TRAAVTAL 
      lonkod = sumpers.TRAKTKOD.
      FIND FIRST TRAKTATAB WHERE TRAKTATAB.TRAAVTAL = kodanst AND
      TRAKTATAB.TRAKTKOD = sumpers.TRAKTKOD USE-INDEX TRAKTKOD NO-LOCK NO-ERROR.
      IF AVAILABLE TRAKTATAB THEN DO:
         FOR EACH sumpers WHERE sumpers.TRAKTKOD = lonkod AND sumpers.TRAAVTAL = kodanst:            
            IF TRAKTATAB.VILART = "821" THEN DO: 
               sumpers.T1 = sumpers.TRAKTANTAL.
            END.                                         
            IF TRAKTATAB.VILART = "831" THEN DO: 
               sumpers.T2 = sumpers.TRAKTANTAL.
            END.
            IF TRAKTATAB.VILART = "832" THEN DO: 
               sumpers.T3 = sumpers.TRAKTANTAL.
            END.                                         
            IF TRAKTATAB.VILART = "842" THEN DO: 
               sumpers.T4 = sumpers.TRAKTANTAL.
            END.
            IF TRAKTATAB.VILART = "843" THEN DO: 
               sumpers.T5 = sumpers.TRAKTANTAL.
            END.                                         
            IF TRAKTATAB.VILART = "852" THEN DO: 
               sumpers.T6 = sumpers.TRAKTANTAL.
            END.
            IF TRAKTATAB.VILART = "855" THEN DO: 
               sumpers.T7 = sumpers.TRAKTANTAL.
            END.                                                     
            ASSIGN
            sumpers.TRAKTKOD = ""
            sumpers.TRAKTANTAL = 0.            
         END. 
      END.
      ELSE DO:
         FOR EACH sumpers WHERE sumpers.TRAKTKOD = lonkod AND sumpers.TRAAVTAL = kodanst:
            ASSIGN
            sumpers.TRAKTKOD = ""
            sumpers.TRAKTANTAL = 0. 
         END.
      END.
   END.
   OPEN QUERY sbq FOR EACH BEFATTNING NO-LOCK,
   EACH sumpers WHERE sumpers.BEFATTNING = BEFATTNING.BEFATTNING.
   GET FIRST sbq.
   DO WHILE AVAILABLE(sumpers):
      ASSIGN
      sumpers.VIBEFATTNING = BEFATTNING.NAMN.
      GET NEXT sbq.
   END.
   OPEN QUERY spq FOR EACH PERSONALTAB WHERE PERSONALTAB.OMRADE = utomr NO-LOCK,
   EACH sumpers WHERE sumpers.PERSONALKOD = PERSONALTAB.PERSONALKOD.
   GET FIRST spq.
   DO WHILE AVAILABLE(sumpers):
      ASSIGN
      sumpers.NAMN = SUBSTRING(PERSONALTAB.FORNAMN,1,1) + "." + PERSONALTAB.EFTERNAMN.
      Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
      GET NEXT spq.
   END.
   {GDPRLOGGCLIENT.I}
   FOR EACH sumpers BREAK BY sumpers.AONR BY sumpers.DELNR:                     
      ACCUMULATE sumpers.TOTALT (TOTAL BY sumpers.AONR BY sumpers.DELNR).                     
      ACCUMULATE sumpers.MIL (TOTAL BY sumpers.AONR BY sumpers.DELNR).
      ACCUMULATE sumpers.RESTIM1 (TOTAL BY sumpers.AONR BY sumpers.DELNR).
      ACCUMULATE sumpers.RESTIM2 (TOTAL BY sumpers.AONR BY sumpers.DELNR).
      ACCUMULATE sumpers.RESTIM3 (TOTAL BY sumpers.AONR BY sumpers.DELNR).
      ACCUMULATE sumpers.T1 (TOTAL BY sumpers.AONR BY sumpers.DELNR).
      ACCUMULATE sumpers.T2 (TOTAL BY sumpers.AONR BY sumpers.DELNR).
      ACCUMULATE sumpers.T3 (TOTAL BY sumpers.AONR BY sumpers.DELNR).
      ACCUMULATE sumpers.T4 (TOTAL BY sumpers.AONR BY sumpers.DELNR).
      ACCUMULATE sumpers.T5 (TOTAL BY sumpers.AONR BY sumpers.DELNR).
      ACCUMULATE sumpers.T6 (TOTAL BY sumpers.AONR BY sumpers.DELNR).
      ACCUMULATE sumpers.T7 (TOTAL BY sumpers.AONR BY sumpers.DELNR). 
      ACCUMULATE sumpers.O50 (TOTAL BY sumpers.AONR BY sumpers.DELNR).
      ACCUMULATE sumpers.O75 (TOTAL BY sumpers.AONR BY sumpers.DELNR).
      ACCUMULATE sumpers.O100 (TOTAL BY sumpers.AONR BY sumpers.DELNR).
      ACCUMULATE sumpers.O150 (TOTAL BY sumpers.AONR BY sumpers.DELNR).
      ACCUMULATE sumpers.K50 (TOTAL BY sumpers.AONR BY sumpers.DELNR).
      ACCUMULATE sumpers.K75 (TOTAL BY sumpers.AONR BY sumpers.DELNR).
      ACCUMULATE sumpers.K100 (TOTAL BY sumpers.AONR BY sumpers.DELNR).
      ACCUMULATE sumpers.K150 (TOTAL BY sumpers.AONR BY sumpers.DELNR).
      IF LAST-OF(sumpers.DELNR) THEN DO:
         CREATE sumaonr.         
         ASSIGN
         sumaonr.AONR = sumpers.AONR
         sumaonr.DELNR = sumpers.DELNR   
         sumaonr.TOTALT = (ACCUM TOTAL BY sumpers.DELNR sumpers.TOTALT)               
         sumaonr.MIL = (ACCUM TOTAL BY sumpers.DELNR sumpers.MIL)
         sumaonr.RESTIM1 = (ACCUM TOTAL BY sumpers.DELNR sumpers.RESTIM1)
         sumaonr.RESTIM2 = (ACCUM TOTAL BY sumpers.DELNR sumpers.RESTIM2)
         sumaonr.RESTIM3 = (ACCUM TOTAL BY sumpers.DELNR sumpers.RESTIM3)
         sumaonr.T1 = (ACCUM TOTAL BY sumpers.DELNR sumpers.T1)
         sumaonr.T2 = (ACCUM TOTAL BY sumpers.DELNR sumpers.T2)
         sumaonr.T3 = (ACCUM TOTAL BY sumpers.DELNR sumpers.T3)
         sumaonr.T4 = (ACCUM TOTAL BY sumpers.DELNR sumpers.T4)
         sumaonr.T5 = (ACCUM TOTAL BY sumpers.DELNR sumpers.T5)
         sumaonr.T6 = (ACCUM TOTAL BY sumpers.DELNR sumpers.T6)
         sumaonr.T7 = (ACCUM TOTAL BY sumpers.DELNR sumpers.T7)
         sumaonr.O50 = (ACCUM TOTAL BY sumpers.DELNR sumpers.O50)
         sumaonr.O75 = (ACCUM TOTAL BY sumpers.DELNR sumpers.O75)
         sumaonr.O100 = (ACCUM TOTAL BY sumpers.DELNR sumpers.O100)
         sumaonr.O150 = (ACCUM TOTAL BY sumpers.DELNR sumpers.O150)
         sumaonr.K50 = (ACCUM TOTAL BY sumpers.DELNR sumpers.K50)
         sumaonr.K75 = (ACCUM TOTAL BY sumpers.DELNR sumpers.K75)
         sumaonr.K100 = (ACCUM TOTAL BY sumpers.DELNR sumpers.K100)
         sumaonr.K150 = (ACCUM TOTAL BY sumpers.DELNR sumpers.K150).    
      END.   
   END.
   FOR EACH sumpers BREAK BY sumpers.AONR BY sumpers.DELNR BY sumpers.PERSONALKOD BY sumpers.BEFATTNING:                     
      ACCUMULATE sumpers.TOTALT (TOTAL BY sumpers.AONR BY sumpers.DELNR BY sumpers.PERSONALKOD BY sumpers.BEFATTNING).                     
      ACCUMULATE sumpers.MIL (TOTAL BY sumpers.AONR BY sumpers.DELNR BY sumpers.PERSONALKOD BY sumpers.BEFATTNING).
      ACCUMULATE sumpers.RESTIM1 (TOTAL BY sumpers.AONR BY sumpers.DELNR BY sumpers.PERSONALKOD BY sumpers.BEFATTNING).
      ACCUMULATE sumpers.RESTIM2 (TOTAL BY sumpers.AONR BY sumpers.DELNR BY sumpers.PERSONALKOD BY sumpers.BEFATTNING).
      ACCUMULATE sumpers.RESTIM3 (TOTAL BY sumpers.AONR BY sumpers.DELNR BY sumpers.PERSONALKOD BY sumpers.BEFATTNING).
      ACCUMULATE sumpers.T1 (TOTAL BY sumpers.AONR BY sumpers.DELNR BY sumpers.PERSONALKOD BY sumpers.BEFATTNING).
      ACCUMULATE sumpers.T2 (TOTAL BY sumpers.AONR BY sumpers.DELNR BY sumpers.PERSONALKOD BY sumpers.BEFATTNING).
      ACCUMULATE sumpers.T3 (TOTAL BY sumpers.AONR BY sumpers.DELNR BY sumpers.PERSONALKOD BY sumpers.BEFATTNING).
      ACCUMULATE sumpers.T4 (TOTAL BY sumpers.AONR BY sumpers.DELNR BY sumpers.PERSONALKOD BY sumpers.BEFATTNING).
      ACCUMULATE sumpers.T5 (TOTAL BY sumpers.AONR BY sumpers.DELNR BY sumpers.PERSONALKOD BY sumpers.BEFATTNING).
      ACCUMULATE sumpers.T6 (TOTAL BY sumpers.AONR BY sumpers.DELNR BY sumpers.PERSONALKOD BY sumpers.BEFATTNING).
      ACCUMULATE sumpers.T7 (TOTAL BY sumpers.AONR BY sumpers.DELNR BY sumpers.PERSONALKOD BY sumpers.BEFATTNING). 
      ACCUMULATE sumpers.O50 (TOTAL BY sumpers.AONR BY sumpers.DELNR BY sumpers.PERSONALKOD BY sumpers.BEFATTNING).
      ACCUMULATE sumpers.O75 (TOTAL BY sumpers.AONR BY sumpers.DELNR BY sumpers.PERSONALKOD BY sumpers.BEFATTNING).
      ACCUMULATE sumpers.O100 (TOTAL BY sumpers.AONR BY sumpers.DELNR BY sumpers.PERSONALKOD BY sumpers.BEFATTNING).
      ACCUMULATE sumpers.O150 (TOTAL BY sumpers.AONR BY sumpers.DELNR BY sumpers.PERSONALKOD BY sumpers.BEFATTNING).
      ACCUMULATE sumpers.K50 (TOTAL BY sumpers.AONR BY sumpers.DELNR BY sumpers.PERSONALKOD BY sumpers.BEFATTNING).
      ACCUMULATE sumpers.K75 (TOTAL BY sumpers.AONR BY sumpers.DELNR BY sumpers.PERSONALKOD BY sumpers.BEFATTNING).
      ACCUMULATE sumpers.K100 (TOTAL BY sumpers.AONR BY sumpers.DELNR BY sumpers.PERSONALKOD BY sumpers.BEFATTNING).
      ACCUMULATE sumpers.K150 (TOTAL BY sumpers.AONR BY sumpers.DELNR BY sumpers.PERSONALKOD BY sumpers.BEFATTNING).
      IF LAST-OF(sumpers.BEFATTNING) THEN DO:
         CREATE sumpersut.         
         ASSIGN
         sumpersut.PERSONALKOD = sumpers.PERSONALKOD
         sumpersut.ANST = sumpers.ANST 
         sumpersut.TRAAVTAL = sumpers.TRAAVTAL
         sumpersut.AONR = sumpers.AONR
         sumpersut.DELNR = sumpers.DELNR   
         sumpersut.BEFATTNING = sumpers.BEFATTNING
         sumpersut.VIBEFATTNING = sumpers.VIBEFATTNING 
         sumpersut.PRISTYP = sumpers.PRISTYP
         sumpersut.AONR = sumpers.AONR
         sumpersut.DELNR = sumpers.DELNR   
         sumpersut.TOTALT = (ACCUM TOTAL BY sumpers.BEFATTNING sumpers.TOTALT)               
         sumpersut.MIL = (ACCUM TOTAL BY sumpers.BEFATTNING sumpers.MIL)
         sumpersut.RESTIM1 = (ACCUM TOTAL BY sumpers.BEFATTNING sumpers.RESTIM1)
         sumpersut.RESTIM2 = (ACCUM TOTAL BY sumpers.BEFATTNING sumpers.RESTIM2)
         sumpersut.RESTIM3 = (ACCUM TOTAL BY sumpers.BEFATTNING sumpers.RESTIM3)
         sumpersut.T1 = (ACCUM TOTAL BY sumpers.BEFATTNING sumpers.T1)
         sumpersut.T2 = (ACCUM TOTAL BY sumpers.BEFATTNING sumpers.T2)
         sumpersut.T3 = (ACCUM TOTAL BY sumpers.BEFATTNING sumpers.T3)
         sumpersut.T4 = (ACCUM TOTAL BY sumpers.BEFATTNING sumpers.T4)
         sumpersut.T5 = (ACCUM TOTAL BY sumpers.BEFATTNING sumpers.T5)
         sumpersut.T6 = (ACCUM TOTAL BY sumpers.BEFATTNING sumpers.T6)
         sumpersut.T7 = (ACCUM TOTAL BY sumpers.BEFATTNING sumpers.T7)
         sumpersut.O50 = (ACCUM TOTAL BY sumpers.BEFATTNING sumpers.O50)
         sumpersut.O75 = (ACCUM TOTAL BY sumpers.BEFATTNING sumpers.O75)
         sumpersut.O100 = (ACCUM TOTAL BY sumpers.BEFATTNING sumpers.O100)
         sumpersut.O150 = (ACCUM TOTAL BY sumpers.BEFATTNING sumpers.O150)
         sumpersut.K50 = (ACCUM TOTAL BY sumpers.BEFATTNING sumpers.K50)
         sumpersut.K75 = (ACCUM TOTAL BY sumpers.BEFATTNING sumpers.K75)
         sumpersut.K100 = (ACCUM TOTAL BY sumpers.BEFATTNING sumpers.K100)
         sumpersut.K150 = (ACCUM TOTAL BY sumpers.BEFATTNING sumpers.K150).    
      END.   
   END.
END PROCEDURE.   
