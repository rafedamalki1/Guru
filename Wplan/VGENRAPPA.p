 /*VGENRAPP.P*/
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{GLOBVAR2DEL1.I}                                          
{REGVAR.I}                                            
                                          
{SOKDEF.I}                                            
{TIDUTTTNEW.I}          

DEFINE INPUT PARAMETER vart AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR valsoktemp.
DEFINE OUTPUT PARAMETER TABLE FOR tidut.
DEFINE VARIABLE vararbstart AS INTEGER NO-UNDO.
DEFINE VARIABLE sel_omr    AS CHARACTER NO-UNDO.
DEFINE VARIABLE sel_list   AS CHARACTER NO-UNDO.
DEFINE VARIABLE tog_avs    AS LOGICAL NO-UNDO.
DEFINE VARIABLE tog_konto  AS LOGICAL NO-UNDO.
DEFINE VARIABLE tog_ben    AS LOGICAL NO-UNDO.
DEFINE VARIABLE tog_ejav   AS LOGICAL NO-UNDO.
DEFINE VARIABLE tillar     AS INTEGER NO-UNDO.
DEFINE VARIABLE franar     AS INTEGER NO-UNDO.
DEFINE VARIABLE pris_typ   AS CHARACTER NO-UNDO.
DEFINE VARIABLE rad_fast   AS LOGICAL NO-UNDO. 
DEFINE VARIABLE best_omr     AS CHARACTER NO-UNDO.
DEFINE VARIABLE in_arb     AS CHARACTER NO-UNDO.

DEFINE VARIABLE str AS CHARACTER FORMAT "X(80)" NO-UNDO.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE VARIABLE varomr AS CHARACTER NO-UNDO.
DEFINE VARIABLE slutvecko AS INTEGER NO-UNDO.
DEFINE VARIABLE startvecko AS INTEGER NO-UNDO.
DEFINE VARIABLE FILL-IN_ARBARTKOD AS INTEGER NO-UNDO.
DEFINE TEMP-TABLE tidut2
   FIELD UT AS CHARACTER FORMAT "X(79)"
   FIELD UDATUM AS DATE
   FIELD KDATUM AS DATE
   FIELD ARBARTKOD AS INTEGER
   FIELD BEN AS CHARACTER
   FIELD ORT AS CHARACTER FORMAT "X(30)"  . 
FIND FIRST valsoktemp NO-ERROR.
ASSIGN
tog_konto = valsoktemp.SOKLOG[2]
sel_omr = valsoktemp.SOKCHAR[1]    
sel_list = valsoktemp.SOKCHAR[2]
tog_avs = valsoktemp.SOKLOG[1]
tog_ben = valsoktemp.SOKLOG[3]
tog_ejav = valsoktemp.SOKLOG[4]
rad_fast = valsoktemp.SOKLOG[5]
pris_typ = valsoktemp.SOKCHAR[3]
best_omr = valsoktemp.SOKCHAR[4]
in_arb = valsoktemp.SOKCHAR[5]
tillar = valsoktemp.SOKINT[1]
franar = valsoktemp.SOKINT[2]
FILL-IN_ARBARTKOD = valsoktemp.SOKINT[3]
bdatum = valsoktemp.SOKDATE[1]
avdatum = valsoktemp.SOKDATE[2].

/*viaorg_UI*/
IF vart = 1 THEN RUN viaorg_UI.
/*videborg_UI*/
IF vart = 2 THEN DO:
   IF sel_omr NE "ALLA" THEN DO:
      FIND FIRST OMRADETAB WHERE OMRADETAB.NAMN = sel_omr AND OMRADETAB.ELVOMRKOD = 0 USE-INDEX OMRNAMN NO-LOCK NO-ERROR.
      IF AVAILABLE OMRADETAB THEN DO:
         varomr = OMRADETAB.OMRADE.
      END.                         
      ELSE varomr = "".
   END.
   DO TRANSACTION:         
      CREATE tidut.                         
      ASSIGN
      SUBSTRING(tidut.UT,50) = STRING(TODAY)
      SUBSTRING(tidut.UT,60) = STRING(TIME,"HH:MM:SS"). 
      CREATE tidut.                         
      ASSIGN
      SUBSTRING(tidut.UT,1) = CAPS(sel_list)
      SUBSTRING(tidut.UT,40) = sel_omr.
      CREATE tidut.                         
      ASSIGN
      SUBSTRING(tidut.UT,1)= "MED " + CAPS(Guru.Konstanter:gdebk)
      SUBSTRING(tidut.UT,20)= pris_typ.      
      RUN avslut_UI. 
      RUN huvud_UI.
   END.   
   IF sel_omr = "ALLA" THEN DO:
      /*EJ AVSLUTAD*/  
      IF tog_avs = FALSE AND tog_ejav = TRUE THEN DO:
         OPEN QUERY aonrq FOR EACH PLANNRTAB WHERE PLANNRTAB.PLANNRAVDATUM = 01/01/1991 AND 
         PLANNRTAB.PRISTYP = pris_typ AND
         PLANNRTAB.FASTAPLANNR = rad_fast AND 
         PLANNRTAB.ARTAL >= franar AND PLANNRTAB.ARTAL <= tillar
         USE-INDEX OMRADE NO-LOCK.
         GET FIRST aonrq NO-LOCK.
         DO WHILE AVAILABLE(PLANNRTAB):    
            RUN aonrkont_UI.                      
            GET NEXT aonrq NO-LOCK.             
         END.
         CLOSE QUERY aonrq.                
      END.
      IF tog_avs = TRUE AND tog_ejav = FALSE THEN DO:
         /*ALLA AVSLUTADE*/
         OPEN QUERY aonrq FOR EACH PLANNRTAB WHERE 
         PLANNRTAB.PRISTYP = pris_typ AND
         PLANNRTAB.PLANNRAVDATUM  >= bdatum AND 
         PLANNRTAB.PLANNRAVDATUM <= avdatum AND PLANNRTAB.FASTAPLANNR = rad_fast AND 
         PLANNRTAB.ARTAL >= franar AND PLANNRTAB.ARTAL <= tillar
         USE-INDEX OMRADE NO-LOCK.
         GET FIRST aonrq NO-LOCK.
         DO WHILE AVAILABLE(PLANNRTAB):     
            RUN aonrkont_UI.            
            GET NEXT aonrq NO-LOCK.             
         END.
         CLOSE QUERY aonrq.   
      END. 
      IF tog_avs = TRUE AND tog_ejav = TRUE THEN DO:
         /*ALLA AVSLUTADE*/
         OPEN QUERY aonrq FOR EACH PLANNRTAB WHERE PLANNRTAB.PRISTYP = pris_typ AND
         ((PLANNRTAB.PLANNRAVDATUM >= bdatum AND PLANNRTAB.PLANNRAVDATUM <= avdatum) OR 
         PLANNRTAB.PLANNRAVDATUM = 01/01/1991) AND PLANNRTAB.FASTAPLANNR = rad_fast AND 
         PLANNRTAB.ARTAL >= franar AND PLANNRTAB.ARTAL <= tillar
         USE-INDEX OMRADE NO-LOCK.
         GET FIRST aonrq NO-LOCK.
         DO WHILE AVAILABLE(PLANNRTAB):  
            RUN aonrkont_UI.             
            GET NEXT aonrq NO-LOCK.             
         END.
         CLOSE QUERY aonrq.  
      END. 
   END. 
   ELSE DO:
      /*EJ AVSLUTAD*/  
      IF tog_avs = FALSE AND tog_ejav = TRUE THEN DO:
         OPEN QUERY aonrq FOR EACH PLANNRTAB WHERE PLANNRTAB.OMRADE = varomr AND
         PLANNRTAB.PRISTYP = pris_typ AND 
         PLANNRTAB.PLANNRAVDATUM = 01/01/1991 AND PLANNRTAB.FASTAPLANNR = rad_fast AND 
         PLANNRTAB.ARTAL >= franar AND PLANNRTAB.ARTAL <= tillar
         USE-INDEX OMRADE NO-LOCK.
         GET FIRST aonrq NO-LOCK.
         DO WHILE AVAILABLE(PLANNRTAB):  
            RUN aonrkont_UI.            
            GET NEXT aonrq NO-LOCK.             
         END.
         CLOSE QUERY aonrq.               
      END.
      IF tog_avs = TRUE AND tog_ejav = FALSE THEN DO:
         /*ALLA AVSLUTADE*/
         OPEN QUERY aonrq FOR EACH PLANNRTAB WHERE PLANNRTAB.OMRADE = varomr AND 
         PLANNRTAB.PRISTYP = pris_typ AND 
         PLANNRTAB.PLANNRAVDATUM  >= bdatum AND PLANNRTAB.PLANNRAVDATUM <= avdatum AND 
         PLANNRTAB.FASTAPLANNR = rad_fast AND 
         PLANNRTAB.ARTAL >= franar AND PLANNRTAB.ARTAL <= tillar
         USE-INDEX OMRADE NO-LOCK.
         GET FIRST aonrq NO-LOCK.
         DO WHILE AVAILABLE(PLANNRTAB):    
            RUN aonrkont_UI.             
            GET NEXT aonrq NO-LOCK.             
         END.
         CLOSE QUERY aonrq.  
      END. 
      IF tog_avs = TRUE AND tog_ejav = TRUE THEN DO:
         /*ALLA AVSLUTADE*/
         OPEN QUERY aonrq FOR EACH PLANNRTAB WHERE PLANNRTAB.OMRADE = varomr AND
         PLANNRTAB.PRISTYP = pris_typ AND 
         ((PLANNRTAB.PLANNRAVDATUM >= bdatum AND PLANNRTAB.PLANNRAVDATUM <= avdatum) OR 
         PLANNRTAB.PLANNRAVDATUM = 01/01/1991) AND PLANNRTAB.FASTAPLANNR = rad_fast AND 
         PLANNRTAB.ARTAL >= franar AND PLANNRTAB.ARTAL <= tillar
         USE-INDEX OMRADE NO-LOCK.
         GET FIRST aonrq NO-LOCK.
         DO WHILE AVAILABLE(PLANNRTAB):  
            RUN aonrkont_UI.             
            GET NEXT aonrq NO-LOCK.             
         END.
         CLOSE QUERY aonrq.  
      END. 
   END.
END.
IF vart = 3 THEN RUN viarborg_UI.
/*vibesorg_UI*/
IF vart = 6 THEN DO:
   IF sel_omr NE "ALLA" THEN DO:
      FIND FIRST OMRADETAB WHERE OMRADETAB.NAMN = sel_omr AND OMRADETAB.ELVOMRKOD = 0 USE-INDEX OMRNAMN NO-LOCK NO-ERROR.
      IF AVAILABLE OMRADETAB THEN DO:
         varomr = OMRADETAB.OMRADE.
      END.                         
      ELSE varomr = "".
   END.
   DO TRANSACTION:                    
      CREATE tidut.                                  
      ASSIGN
      SUBSTRING(tidut.UT,50) = STRING(TODAY)
      SUBSTRING(tidut.UT,60) = STRING(TIME,"HH:MM:SS"). 
      CREATE tidut.                         
      ASSIGN
      SUBSTRING(tidut.UT,1) = CAPS(sel_list)
      SUBSTRING(tidut.UT,40) = sel_omr.
      CREATE tidut. 
      SUBSTRING(tidut.UT,1)= "MED " + CAPS(Guru.Konstanter:gbestk).
      FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = best_omr AND OMRADETAB.ELVOMRKOD = 0
      USE-INDEX OMR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE OMRADETAB THEN DO:
         FIND FIRST BESTTAB WHERE BESTTAB.BESTID = best_omr
         USE-INDEX BEST NO-LOCK NO-ERROR.
         SUBSTRING(tidut.UT,24)= BESTTAB.BESTNAMN.
      END.
      ELSE SUBSTRING(tidut.UT,24)= OMRADETAB.NAMN.           
      RUN avslut_UI. 
      RUN huvud_UI.
   END.   
   IF sel_omr = "ALLA" THEN DO:
      /*EJ AVSLUTAD*/  
      IF TOG_AVS = FALSE AND TOG_EJAV = TRUE THEN DO:
         OPEN QUERY aonrq FOR EACH PLANNRTAB WHERE PLANNRTAB.PLANNRAVDATUM = 01/01/1991 AND 
         PLANNRTAB.BESTID = best_omr AND
         PLANNRTAB.FASTAPLANNR = rad_fast AND 
         PLANNRTAB.ARTAL >= franar AND PLANNRTAB.ARTAL <= tillar
         USE-INDEX OMRADE NO-LOCK.
         GET FIRST aonrq NO-LOCK.
         DO WHILE AVAILABLE(PLANNRTAB):    
            RUN aonrkont_UI.                      
            GET NEXT aonrq NO-LOCK.             
         END.
         CLOSE QUERY aonrq.                
      END.
      IF TOG_AVS = TRUE AND TOG_EJAV = FALSE THEN DO:
         /*ALLA AVSLUTADE*/
         OPEN QUERY aonrq FOR EACH PLANNRTAB WHERE 
         PLANNRTAB.BESTID = best_omr AND
         PLANNRTAB.PLANNRAVDATUM  >= bdatum AND 
         PLANNRTAB.PLANNRAVDATUM <= avdatum AND PLANNRTAB.FASTAPLANNR = rad_fast AND 
         PLANNRTAB.ARTAL >= franar AND PLANNRTAB.ARTAL <= tillar
         USE-INDEX OMRADE NO-LOCK.
         GET FIRST aonrq NO-LOCK.
         DO WHILE AVAILABLE(PLANNRTAB):     
            RUN aonrkont_UI.            
            GET NEXT aonrq NO-LOCK.             
         END.
         CLOSE QUERY aonrq. 
      END. 
      IF TOG_AVS = TRUE AND TOG_EJAV = TRUE THEN DO:
         /*ALLA AVSLUTADE*/
         OPEN QUERY aonrq FOR EACH PLANNRTAB WHERE PLANNRTAB.BESTID = best_omr AND
         ((PLANNRTAB.PLANNRAVDATUM >= bdatum AND PLANNRTAB.PLANNRAVDATUM <= avdatum) OR 
         PLANNRTAB.PLANNRAVDATUM = 01/01/1991) AND PLANNRTAB.FASTAPLANNR = rad_fast AND 
         PLANNRTAB.ARTAL >= franar AND PLANNRTAB.ARTAL <= tillar
         USE-INDEX OMRADE NO-LOCK.
         GET FIRST aonrq NO-LOCK.
         DO WHILE AVAILABLE(PLANNRTAB):  
            RUN aonrkont_UI.             
            GET NEXT aonrq NO-LOCK.             
         END.
         CLOSE QUERY aonrq.  
      END. 
   END. 
   ELSE DO:
      /*EJ AVSLUTAD*/  
      IF TOG_AVS = FALSE AND TOG_EJAV = TRUE THEN DO:
         OPEN QUERY aonrq FOR EACH PLANNRTAB WHERE PLANNRTAB.OMRADE = varomr AND
         PLANNRTAB.BESTID = best_omr AND
         PLANNRTAB.PLANNRAVDATUM = 01/01/1991 AND PLANNRTAB.FASTAPLANNR = rad_fast AND 
         PLANNRTAB.ARTAL >= franar AND PLANNRTAB.ARTAL <= tillar
         USE-INDEX OMRADE NO-LOCK.
         GET FIRST aonrq NO-LOCK.
         DO WHILE AVAILABLE(PLANNRTAB):  
            RUN aonrkont_UI.            
            GET NEXT aonrq NO-LOCK.             
         END.
         CLOSE QUERY aonrq.               
      END.
      IF TOG_AVS = TRUE AND TOG_EJAV = FALSE THEN DO:
         /*ALLA AVSLUTADE*/
         OPEN QUERY aonrq FOR EACH PLANNRTAB WHERE PLANNRTAB.OMRADE = varomr AND 
         PLANNRTAB.BESTID = best_omr AND
         PLANNRTAB.PLANNRAVDATUM  >= bdatum AND PLANNRTAB.PLANNRAVDATUM <= avdatum AND 
         PLANNRTAB.FASTAPLANNR = rad_fast AND 
         PLANNRTAB.ARTAL >= franar AND PLANNRTAB.ARTAL <= tillar
         USE-INDEX OMRADE NO-LOCK.
         GET FIRST aonrq NO-LOCK.
         DO WHILE AVAILABLE(PLANNRTAB):    
            RUN aonrkont_UI.             
            GET NEXT aonrq NO-LOCK.             
         END.
         CLOSE QUERY aonrq. 
      END. 
      IF TOG_AVS = TRUE AND TOG_EJAV = TRUE THEN DO:
         /*ALLA AVSLUTADE*/
         OPEN QUERY aonrq FOR EACH PLANNRTAB WHERE PLANNRTAB.OMRADE = varomr AND
         PLANNRTAB.BESTID = best_omr AND
         ((PLANNRTAB.PLANNRAVDATUM >= bdatum AND PLANNRTAB.PLANNRAVDATUM <= avdatum) OR  
         PLANNRTAB.PLANNRAVDATUM = 01/01/1991) AND PLANNRTAB.FASTAPLANNR = rad_fast AND 
         PLANNRTAB.ARTAL >= franar AND PLANNRTAB.ARTAL <= tillar
         USE-INDEX OMRADE NO-LOCK.
         GET FIRST aonrq NO-LOCK.
         DO WHILE AVAILABLE(PLANNRTAB):  
            RUN aonrkont_UI.             
            GET NEXT aonrq NO-LOCK.             
         END.
         CLOSE QUERY aonrq.   
      END. 
   END.
END.

/*vianvorg_UI*/
IF vart = 8 THEN DO:
   IF SEL_OMR NE "ALLA" THEN DO:
      FIND FIRST OMRADETAB WHERE OMRADETAB.NAMN = SEL_OMR  AND OMRADETAB.ELVOMRKOD = 0 USE-INDEX OMRNAMN NO-LOCK NO-ERROR.
      IF AVAILABLE OMRADETAB THEN DO:
         varomr = OMRADETAB.OMRADE.
      END.                         
      ELSE varomr = "".
   END.
   DO TRANSACTION:           
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = in_arb NO-LOCK NO-ERROR.
      CREATE tidut.                         
      ASSIGN
      SUBSTRING(tidut.UT,50) = STRING(TODAY)
      SUBSTRING(tidut.UT,60) = STRING(TIME,"HH:MM:SS"). 
      CREATE tidut.                         
      ASSIGN
      SUBSTRING(tidut.UT,1) = CAPS(SEL_LIST)
      SUBSTRING(tidut.UT,37) = SEL_OMR. 
      CREATE tidut.                          
      ASSIGN
      SUBSTRING(tidut.UT,1)= "MED " + CAPS(Guru.Konstanter:garbak).
      IF AVAILABLE PERSONALTAB THEN DO:
         SUBSTRING(tidut.UT,20)= in_arb + " " + PERSONALTAB.FORNAMN + " " + PERSONALTAB.EFTERNAMN.
         Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
      END.   
      ELSE SUBSTRING(tidut.UT,20)= in_arb.
         
      RUN avslut_UI. 
      RUN huvud_UI.
   END.   
   IF SEL_OMR = "ALLA" THEN DO:
      /*EJ AVSLUTAD*/  
      IF TOG_AVS = FALSE AND TOG_EJAV = TRUE THEN DO:
         OPEN QUERY aonrq FOR EACH PLANNRTAB WHERE PLANNRTAB.PLANNRAVDATUM = 01/01/1991 AND 
         PLANNRTAB.ARBANSVARIG = in_arb AND
         PLANNRTAB.FASTAPLANNR = RAD_FAST AND 
         PLANNRTAB.ARTAL >= franar AND PLANNRTAB.ARTAL <= tillar
         USE-INDEX OMRADE NO-LOCK.
         GET FIRST aonrq NO-LOCK.
         DO WHILE AVAILABLE(PLANNRTAB):   
            RUN aonrkont_UI.                      
            GET NEXT aonrq NO-LOCK.             
         END.
         CLOSE QUERY aonrq.                  
      END.
      IF TOG_AVS = TRUE AND TOG_EJAV = FALSE THEN DO:
         /*ALLA AVSLUTADE*/
         OPEN QUERY aonrq FOR EACH PLANNRTAB WHERE 
         PLANNRTAB.ARBANSVARIG = in_arb AND
         PLANNRTAB.PLANNRAVDATUM  >= bdatum AND 
         PLANNRTAB.PLANNRAVDATUM <= avdatum AND PLANNRTAB.FASTAPLANNR = RAD_FAST AND 
         PLANNRTAB.ARTAL >= franar AND PLANNRTAB.ARTAL <= tillar
         USE-INDEX OMRADE NO-LOCK.
         GET FIRST aonrq NO-LOCK.
         DO WHILE AVAILABLE(PLANNRTAB):    
            RUN aonrkont_UI.            
            GET NEXT aonrq NO-LOCK.             
         END.
         CLOSE QUERY aonrq.    
      END. 
      IF TOG_AVS = TRUE AND TOG_EJAV = TRUE THEN DO:
         /*ALLA AVSLUTADE*/
         OPEN QUERY aonrq FOR EACH PLANNRTAB WHERE PLANNRTAB.ARBANSVARIG = in_arb AND
         ((PLANNRTAB.PLANNRAVDATUM >= bdatum AND PLANNRTAB.PLANNRAVDATUM <= avdatum) OR 
         PLANNRTAB.PLANNRAVDATUM = 01/01/1991) AND PLANNRTAB.FASTAPLANNR = RAD_FAST AND 
         PLANNRTAB.ARTAL >= franar AND PLANNRTAB.ARTAL <= tillar
         USE-INDEX OMRADE NO-LOCK.
         GET FIRST aonrq NO-LOCK.
         DO WHILE AVAILABLE(PLANNRTAB): 
            RUN aonrkont_UI.             
            GET NEXT aonrq NO-LOCK.             
         END.
         CLOSE QUERY aonrq.   
      END. 
   END. 
   ELSE DO:
      /*EJ AVSLUTAD*/  
      IF TOG_AVS = FALSE AND TOG_EJAV = TRUE THEN DO:
         OPEN QUERY aonrq FOR EACH PLANNRTAB WHERE PLANNRTAB.OMRADE = varomr AND
         PLANNRTAB.ARBANSVARIG = in_arb AND
         PLANNRTAB.PLANNRAVDATUM = 01/01/1991 AND PLANNRTAB.FASTAPLANNR = RAD_FAST AND 
         PLANNRTAB.ARTAL >= franar AND PLANNRTAB.ARTAL <= tillar
         USE-INDEX OMRADE NO-LOCK.
         GET FIRST aonrq NO-LOCK.
         DO WHILE AVAILABLE(PLANNRTAB):  
            RUN aonrkont_UI.            
            GET NEXT aonrq NO-LOCK.             
         END.
         CLOSE QUERY aonrq.                 
      END.
      IF TOG_AVS = TRUE AND TOG_EJAV = FALSE THEN DO:
         /*ALLA AVSLUTADE*/
         OPEN QUERY aonrq FOR EACH PLANNRTAB WHERE PLANNRTAB.OMRADE = varomr AND 
         PLANNRTAB.ARBANSVARIG = in_arb AND
         PLANNRTAB.PLANNRAVDATUM  >= bdatum AND PLANNRTAB.PLANNRAVDATUM <= avdatum AND 
         PLANNRTAB.FASTAPLANNR = RAD_FAST AND 
         PLANNRTAB.ARTAL >= franar AND PLANNRTAB.ARTAL <= tillar
         USE-INDEX OMRADE NO-LOCK.
         GET FIRST aonrq NO-LOCK.
         DO WHILE AVAILABLE(PLANNRTAB): 
            RUN aonrkont_UI.             
            GET NEXT aonrq NO-LOCK.             
         END.
         CLOSE QUERY aonrq.  
      END. 
      IF TOG_AVS = TRUE AND TOG_EJAV = TRUE THEN DO:
         /*ALLA AVSLUTADE*/
         OPEN QUERY aonrq FOR EACH PLANNRTAB WHERE PLANNRTAB.OMRADE = varomr AND
         PLANNRTAB.ARBANSVARIG = in_arb AND
         ((PLANNRTAB.PLANNRAVDATUM >= bdatum AND PLANNRTAB.PLANNRAVDATUM <= avdatum) OR  
         PLANNRTAB.PLANNRAVDATUM = 01/01/1991) AND PLANNRTAB.FASTAPLANNR = RAD_FAST AND 
         PLANNRTAB.ARTAL >= franar AND PLANNRTAB.ARTAL <= tillar
         USE-INDEX OMRADE NO-LOCK.
         GET FIRST aonrq NO-LOCK.
         DO WHILE AVAILABLE(PLANNRTAB): 
            RUN aonrkont_UI.             
            GET NEXT aonrq NO-LOCK.             
         END.
         CLOSE QUERY aonrq.  
      END. 
   END.
END.

IF vart = 19 THEN RUN viaorg_UI.
{GDPRLOGGCLIENT.I}  
PROCEDURE viaorg_UI :
   IF sel_omr NE "ALLA" THEN DO:
      FIND FIRST OMRADETAB WHERE OMRADETAB.NAMN = sel_omr AND OMRADETAB.ELVOMRKOD = 0 USE-INDEX OMRNAMN NO-LOCK NO-ERROR.
      IF AVAILABLE OMRADETAB THEN DO:
         varomr = OMRADETAB.OMRADE.
      END.                         
      ELSE varomr = "".
   END.
   DO TRANSACTION:  
      CREATE tidut.                        
      ASSIGN
      SUBSTRING(tidut.UT,50) = STRING(TODAY)
      SUBSTRING(tidut.UT,60) = STRING(TIME,"HH:MM:SS"). 
      CREATE tidut.                         
      ASSIGN
      SUBSTRING(tidut.UT,1) = CAPS(sel_list)
      SUBSTRING(tidut.UT,40) = sel_omr.     
      RUN avslut_UI.
      RUN huvud_UI.
   END.   
   IF sel_omr = "ALLA" THEN DO:
      /*EJ AVSLUTAD*/  
      IF tog_avs = FALSE AND tog_ejav = TRUE THEN DO:
         OPEN QUERY aonrq FOR EACH PLANNRTAB WHERE PLANNRTAB.PLANNRAVDATUM = 01/01/1991 AND 
         PLANNRTAB.FASTAPLANNR = rad_fast AND 
         PLANNRTAB.ARTAL >= franar AND PLANNRTAB.ARTAL <= tillar
         USE-INDEX OMRADE NO-LOCK.
         GET FIRST aonrq NO-LOCK.
         DO WHILE AVAILABLE(PLANNRTAB): 
            RUN aonrkont2_UI.
            GET NEXT aonrq NO-LOCK.             
         END.
         CLOSE QUERY aonrq.                
      END.
      IF tog_avs = TRUE AND tog_ejav = FALSE THEN DO:
         /*ALLA AVSLUTADE*/
         OPEN QUERY aonrq FOR EACH PLANNRTAB WHERE PLANNRTAB.PLANNRAVDATUM  >= bdatum AND 
         PLANNRTAB.PLANNRAVDATUM  <= avdatum AND PLANNRTAB.FASTAPLANNR = rad_fast AND 
         PLANNRTAB.ARTAL >= franar AND PLANNRTAB.ARTAL <= tillar
         USE-INDEX OMRADE NO-LOCK.
         GET FIRST aonrq NO-LOCK.
         DO WHILE AVAILABLE(PLANNRTAB): 
            RUN aonrkont2_UI. 
            GET NEXT aonrq NO-LOCK.             
         END.
         CLOSE QUERY aonrq.   
      END. 
      IF tog_avs = TRUE AND tog_ejav = TRUE THEN DO:
         /*ALLA AVSLUTADE*/
         OPEN QUERY aonrq FOR EACH PLANNRTAB WHERE
         ((PLANNRTAB.PLANNRAVDATUM >= bdatum AND PLANNRTAB.PLANNRAVDATUM <= avdatum) OR 
         PLANNRTAB.PLANNRAVDATUM = 01/01/1991) AND PLANNRTAB.FASTAPLANNR = rad_fast AND 
         PLANNRTAB.ARTAL >= franar AND PLANNRTAB.ARTAL <= tillar
         USE-INDEX OMRADE NO-LOCK.
         GET FIRST aonrq NO-LOCK.
         DO WHILE AVAILABLE(PLANNRTAB): 
            RUN aonrkont2_UI. 
            GET NEXT aonrq NO-LOCK.             
         END.
         CLOSE QUERY aonrq.  
      END. 
   END. 
   ELSE DO:
      /*EJ AVSLUTAD*/  
      IF tog_avs = FALSE AND tog_ejav = TRUE THEN DO:
         OPEN QUERY aonrq FOR EACH PLANNRTAB WHERE PLANNRTAB.OMRADE = varomr AND 
         PLANNRTAB.PLANNRAVDATUM = 01/01/1991 AND PLANNRTAB.FASTAPLANNR = rad_fast AND 
         PLANNRTAB.ARTAL >= franar AND PLANNRTAB.ARTAL <= tillar
         USE-INDEX OMRADE NO-LOCK.
         GET FIRST aonrq NO-LOCK.
         DO WHILE AVAILABLE(PLANNRTAB): 
            RUN aonrkont2_UI. 
            GET NEXT aonrq NO-LOCK.             
         END.
         CLOSE QUERY aonrq.               
      END.
      IF tog_avs = TRUE AND tog_ejav = FALSE THEN DO:
         /*ALLA AVSLUTADE*/
         OPEN QUERY aonrq FOR EACH PLANNRTAB WHERE PLANNRTAB.OMRADE = varomr AND  
         PLANNRTAB.PLANNRAVDATUM  >= bdatum AND PLANNRTAB.PLANNRAVDATUM <= avdatum AND 
         PLANNRTAB.FASTAPLANNR = rad_fast AND 
         PLANNRTAB.ARTAL >= franar AND PLANNRTAB.ARTAL <= tillar
         USE-INDEX OMRADE NO-LOCK.
         GET FIRST aonrq NO-LOCK.
         DO WHILE AVAILABLE(PLANNRTAB):   
            RUN aonrkont2_UI. 
            GET NEXT aonrq NO-LOCK.             
         END.
         CLOSE QUERY aonrq.     
      END. 
      IF tog_avs = TRUE AND tog_ejav = TRUE THEN DO:
         /*ALLA AVSLUTADE*/
         OPEN QUERY aonrq FOR EACH PLANNRTAB WHERE PLANNRTAB.OMRADE = varomr AND 
         ((PLANNRTAB.PLANNRAVDATUM >= bdatum AND PLANNRTAB.PLANNRAVDATUM <= avdatum) OR 
         PLANNRTAB.PLANNRAVDATUM = 01/01/1991) AND PLANNRTAB.FASTAPLANNR = rad_fast AND 
         PLANNRTAB.ARTAL >= franar AND PLANNRTAB.ARTAL <= tillar
         USE-INDEX OMRADE NO-LOCK.
         GET FIRST aonrq NO-LOCK.
         DO WHILE AVAILABLE(PLANNRTAB): 
            RUN aonrkont2_UI.  
            GET NEXT aonrq NO-LOCK.             
         END.
         CLOSE QUERY aonrq.    
      END. 
   END.
   IF vart = 19 THEN DO:       
       FOR EACH ARBETSART NO-LOCK,
       EACH tidut2 WHERE tidut2.ARBARTKOD = ARBETSART.ARBARTKOD:
          tidut2.BEN = ARBETSART.ARBBENAMNING.
       END.
       FOR EACH tidut2 :
           CREATE tidut.
           ASSIGN
           SUBSTRING(tidut.UT,1,79) = tidut2.UT.            
           IF tidut2.ARBARTKOD NE 0 THEN SUBSTRING(tidut.UT,vararbstart) = STRING(tidut2.ARBARTKOD) + " " + tidut2.BEN.
          /* IF TOG_KONTO = TRUE THEN SUBSTRING(tidut.UT,83,30) = SUBSTRING(tidut2.ORT,1,30).   */
       END.       
   END.
   ELSE DO:
      FOR EACH tidut2 :
         CREATE tidut.
         ASSIGN
         tidut.UT = tidut2.UT.
      END.
   END.
   EMPTY TEMP-TABLE tidut2 NO-ERROR.    

    
END PROCEDURE.
PROCEDURE viarborg_UI :
       /*VIARBORG*/
   IF SEL_OMR NE "ALLA" THEN DO:
      FIND FIRST OMRADETAB WHERE OMRADETAB.NAMN = SEL_OMR AND OMRADETAB.ELVOMRKOD = 0 NO-LOCK NO-ERROR.
      IF AVAILABLE OMRADETAB THEN DO:
         varomr = OMRADETAB.OMRADE.
      END.                         
      ELSE varomr = "".
   END.
   DO TRANSACTION:                    
      FIND FIRST ARBETSART WHERE  ARBETSART.ARBARTKOD = FILL-IN_ARBARTKOD NO-LOCK NO-ERROR.
      CREATE tidut.                         
      ASSIGN
      SUBSTRING(tidut.UT,50) = STRING(TODAY)
      SUBSTRING(tidut.UT,60) = STRING(TIME,"HH:MM:SS").        
      CREATE tidut.                         
      ASSIGN
      SUBSTRING(tidut.UT,1) = CAPS(SEL_LIST)
      SUBSTRING(tidut.UT,40) = SEL_OMR.
      CREATE tidut.                         
      ASSIGN
      SUBSTRING(tidut.UT,1)= "MED " + CAPS(Guru.Konstanter:gartk).
      IF AVAILABLE ARBETSART THEN SUBSTRING(tidut.UT,20)= STRING(FILL-IN_ARBARTKOD) + " " + ARBETSART.ARBBENAMNING.  
      ELSE SUBSTRING(tidut.UT,20)= STRING(FILL-IN_ARBARTKOD).   
      RUN avslut_UI. 
      RUN huvud_UI.
   END.   
   IF SEL_OMR = "ALLA" THEN DO:
      /*EJ AVSLUTAD*/  
      IF TOG_AVS = FALSE AND TOG_EJAV = TRUE THEN DO:
         OPEN QUERY aonrq FOR EACH PLANNRTAB WHERE PLANNRTAB.PLANNRAVDATUM = 01/01/91 AND 
         PLANNRTAB.ARBARTKOD = FILL-IN_ARBARTKOD AND
         PLANNRTAB.FASTAPLANNR = RAD_FAST AND 
         PLANNRTAB.ARTAL >= franar AND PLANNRTAB.ARTAL <= tillar
         USE-INDEX OMRADE NO-LOCK.
         GET FIRST aonrq NO-LOCK.
         DO WHILE AVAILABLE(PLANNRTAB):    
            RUN aonrkont_UI.                      
            GET NEXT aonrq NO-LOCK.             
         END.
         CLOSE QUERY aonrq.                  
      END.
      IF TOG_AVS = TRUE AND TOG_EJAV = FALSE THEN DO:
         /*ALLA AVSLUTADE*/
         OPEN QUERY aonrq FOR EACH PLANNRTAB WHERE 
         PLANNRTAB.ARBARTKOD = FILL-IN_ARBARTKOD AND
         PLANNRTAB.PLANNRAVDATUM  >= bdatum AND 
         PLANNRTAB.PLANNRAVDATUM <= avdatum AND PLANNRTAB.FASTAPLANNR = RAD_FAST AND 
         PLANNRTAB.ARTAL >= franar AND PLANNRTAB.ARTAL <= tillar
         USE-INDEX OMRADE NO-LOCK.
         GET FIRST aonrq NO-LOCK.
         DO WHILE AVAILABLE(PLANNRTAB):     
            RUN aonrkont_UI.            
            GET NEXT aonrq NO-LOCK.             
         END.
         CLOSE QUERY aonrq.    
      END. 
      IF TOG_AVS = TRUE AND TOG_EJAV = TRUE THEN DO:
         /*ALLA AVSLUTADE*/
         OPEN QUERY aonrq FOR EACH PLANNRTAB WHERE PLANNRTAB.ARBARTKOD = FILL-IN_ARBARTKOD AND
         ((PLANNRTAB.PLANNRAVDATUM >= bdatum AND PLANNRTAB.PLANNRAVDATUM <= avdatum) OR 
         PLANNRTAB.PLANNRAVDATUM = 01/01/91) AND PLANNRTAB.FASTAPLANNR = RAD_FAST AND 
         PLANNRTAB.ARTAL >= franar AND PLANNRTAB.ARTAL <= tillar
         USE-INDEX OMRADE NO-LOCK.
         GET FIRST aonrq NO-LOCK.
         DO WHILE AVAILABLE(PLANNRTAB):  
            RUN aonrkont_UI.             
            GET NEXT aonrq NO-LOCK.             
         END.
         CLOSE QUERY aonrq. 
      END. 
   END. 
   ELSE DO:
      /*EJ AVSLUTAD*/  
      IF TOG_AVS = FALSE AND TOG_EJAV = TRUE THEN DO:
         OPEN QUERY aonrq FOR EACH PLANNRTAB WHERE PLANNRTAB.OMRADE = varomr AND
         PLANNRTAB.ARBARTKOD = FILL-IN_ARBARTKOD AND
         PLANNRTAB.PLANNRAVDATUM = 01/01/91 AND PLANNRTAB.FASTAPLANNR = RAD_FAST AND 
         PLANNRTAB.ARTAL >= franar AND PLANNRTAB.ARTAL <= tillar
         USE-INDEX OMRADE NO-LOCK.
         GET FIRST aonrq NO-LOCK.
         DO WHILE AVAILABLE(PLANNRTAB):  
            RUN aonrkont_UI.            
            GET NEXT aonrq NO-LOCK.             
         END.
         CLOSE QUERY aonrq.               
      END.
      IF TOG_AVS = TRUE AND TOG_EJAV = FALSE THEN DO:
         /*ALLA AVSLUTADE*/
         OPEN QUERY aonrq FOR EACH PLANNRTAB WHERE PLANNRTAB.OMRADE = varomr AND 
         PLANNRTAB.ARBARTKOD = FILL-IN_ARBARTKOD AND
         PLANNRTAB.PLANNRAVDATUM  >= bdatum AND PLANNRTAB.PLANNRAVDATUM <= avdatum AND 
         PLANNRTAB.FASTAPLANNR = RAD_FAST AND 
         PLANNRTAB.ARTAL >= franar AND PLANNRTAB.ARTAL <= tillar
         USE-INDEX OMRADE NO-LOCK.
         GET FIRST aonrq NO-LOCK.
         DO WHILE AVAILABLE(PLANNRTAB):    
            RUN aonrkont_UI.             
            GET NEXT aonrq NO-LOCK.             
         END.
         CLOSE QUERY aonrq.   
      END. 
      IF TOG_AVS = TRUE AND TOG_EJAV = TRUE THEN DO:
         /*ALLA AVSLUTADE*/
         OPEN QUERY aonrq FOR EACH PLANNRTAB WHERE PLANNRTAB.OMRADE = varomr AND
         PLANNRTAB.ARBARTKOD = FILL-IN_ARBARTKOD AND 
         ((PLANNRTAB.PLANNRAVDATUM >= bdatum AND PLANNRTAB.PLANNRAVDATUM <= avdatum) OR 
         PLANNRTAB.PLANNRAVDATUM = 01/01/91) AND PLANNRTAB.FASTAPLANNR = RAD_FAST AND 
         PLANNRTAB.ARTAL >= franar AND PLANNRTAB.ARTAL <= tillar
         USE-INDEX OMRADE NO-LOCK.
         GET FIRST aonrq NO-LOCK.
         DO WHILE AVAILABLE(PLANNRTAB):  
            RUN aonrkont_UI.             
            GET NEXT aonrq NO-LOCK.             
         END.
         CLOSE QUERY aonrq.    
      END. 
   END.
   
END PROCEDURE.

/*AVSLUT*/
PROCEDURE avslut_UI.
   DO TRANSACTION:      
     IF tog_avs = TRUE AND tog_ejav = FALSE THEN DO:
         CREATE tidut. 
         IF rad_fast = FALSE THEN SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gtillk) + " " + CAPS(Guru.Konstanter:gplk).
         ELSE SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gplk).
         ASSIGN
         SUBSTRING(tidut.UT,21) = "AVSLUTADE"
         SUBSTRING(tidut.UT,31) = "MELLAN"
         SUBSTRING(tidut.UT,40) = STRING(bdatum)
         SUBSTRING(tidut.UT,50) = "OCH"
         SUBSTRING(tidut.UT,55) = STRING(avdatum).
      END.   
      ELSE DO:
         CREATE tidut. 
         IF rad_fast = FALSE THEN SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gtillk) + " " + CAPS(Guru.Konstanter:gplk).
         ELSE SUBSTRING(tidut.UT,1) = "FASTA " + CAPS(Guru.Konstanter:gplk).
         SUBSTRING(tidut.UT,21) = "EJ AVSLUTADE".
         IF tog_avs = TRUE AND tog_ejav = TRUE 
         THEN DO:                                 
            ASSIGN                    
            SUBSTRING(tidut.UT,21) = "EJ AVSLUTADE OCH AVSLUTADE"
            SUBSTRING(tidut.UT,48) = "MELLAN"
            SUBSTRING(tidut.UT,57) = STRING(bdatum)
            SUBSTRING(tidut.UT,67) = "OCH"
            SUBSTRING(tidut.UT,72) = STRING(avdatum).
         END.
      END.
   END.       
END PROCEDURE.

/*HUVUD*/
PROCEDURE huvud_UI :
   DO TRANSACTION:       
      IF tog_konto = TRUE THEN DO:
         FIND FIRST KBENAMNING USE-INDEX KBEN NO-LOCK NO-ERROR.
         CREATE tidut.                         
         ASSIGN
         SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gplk)
         SUBSTRING(tidut.UT,8) = "ÅRTAL"
         SUBSTRING(tidut.UT,14) = CAPS(Guru.Konstanter:gomrk)                                                                                
         SUBSTRING(tidut.UT,21) = "DEB. PRIS"
         SUBSTRING(tidut.UT,31) = KBENAMNING.K1
         SUBSTRING(tidut.UT,38) = KBENAMNING.K2
         SUBSTRING(tidut.UT,45) = KBENAMNING.K3
         SUBSTRING(tidut.UT,52) = KBENAMNING.K4
         SUBSTRING(tidut.UT,59) = KBENAMNING.K5
         SUBSTRING(tidut.UT,66) = " % ".
         IF tog_avs = FALSE AND tog_ejav = TRUE THEN DO:
            SUBSTRING(tidut.UT,70) = "KALKYL".
            IF vart = 19 THEN DO:
               vararbstart = 77.
               SUBSTRING(tidut.UT,vararbstart) = CAPS(Guru.Konstanter:gartk).
            END.
         END.                                
         ELSE DO:
            ASSIGN
            SUBSTRING(tidut.UT,70) = "AVSLUTAT"
            SUBSTRING(tidut.UT,79) = "KALKYL".
            IF vart = 19 THEN DO:
               vararbstart = 86.
               SUBSTRING(tidut.UT,vararbstart) = CAPS(Guru.Konstanter:gartk).
            END.
         END.
         CREATE tidut.
         IF tog_avs = FALSE AND tog_ejav = TRUE THEN DO:         
            tidut.UT =                                                            
            "======.=====.======.=========.======.======.======.======.======.===.======".
         END.
         ELSE DO:
            tidut.UT =                                                                 
            "======.=====.======.=========.======.======.======.======.======.===.========.======".    
         END.
         IF vart = 19 THEN DO:
            tidut.UT = tidut.UT + ".================================".
         END.
      END.
      ELSE DO:                                 
         CREATE tidut.                         
         ASSIGN
         SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gplk)
         SUBSTRING(tidut.UT,8) = "ÅRTAL"
         SUBSTRING(tidut.UT,14) = CAPS(Guru.Konstanter:gomrk)      
         SUBSTRING(tidut.UT,21) = CAPS(Guru.Konstanter:gaonamnk).
         IF tog_avs = FALSE AND tog_ejav = TRUE THEN DO:
            SUBSTRING(tidut.UT,70) = "KALKYL".
            IF vart = 19 THEN DO:
               vararbstart = 77.
               SUBSTRING(tidut.UT,vararbstart) = CAPS(Guru.Konstanter:gartk).
            END.
         END.      
         ELSE DO:
            ASSIGN
            SUBSTRING(tidut.UT,70) = "AVSLUTAT"
            SUBSTRING(tidut.UT,79) = "KALKYL".
            IF vart = 19 THEN DO:
               vararbstart = 86.
               SUBSTRING(tidut.UT,vararbstart) = CAPS(Guru.Konstanter:gartk).
            END.
         END.
         CREATE tidut.
         IF tog_avs = FALSE AND tog_ejav = TRUE THEN DO:         
            tidut.UT =                                                                
            "======.=====.======.================================================.======".
         END.
         ELSE DO:
            tidut.UT =                                                                 
            "======.=====.======.================================================.========.======".    
         END.
         IF vart = 19 THEN DO:
            tidut.UT = tidut.UT + ".================================".
         END.
      END.
   END.         
END PROCEDURE.

       /*AONRKONT*/
PROCEDURE aonrkont_UI :
   DO TRANSACTION:       
      CREATE tidut.     
      ASSIGN
      SUBSTRING(tidut.UT,1) = PLANNRTAB.PLANNR
      SUBSTRING(tidut.UT,8) = STRING(PLANNRTAB.ARTAL,"9999")                                                
      SUBSTRING(tidut.UT,14) = PLANNRTAB.OMRADE.  
      /*SUBSTRING(tidut.UT,21) = PLANNRTAB.PRISTYP.      */
      IF tog_avs = FALSE AND tog_ejav = TRUE THEN DO:
         SUBSTRING(tidut.UT,70) = STRING(PLANNRTAB.FASTKALK,"Ja/Nej").
      END.       
      ELSE DO:          
         IF PLANNRTAB.PLANNRAVDATUM NE ? THEN DO:    
            IF PLANNRTAB.PLANNRAVDATUM NE 01/01/91 THEN
            SUBSTRING(tidut.UT,70) = STRING(PLANNRTAB.PLANNRAVDATUM).
         END.            
         SUBSTRING(tidut.UT,79) = STRING(PLANNRTAB.FASTKALK,"Ja/Nej").
      END.     
      
      IF tog_konto = TRUE THEN DO:      
         musz = TRUE.
         OPEN QUERY aokontq FOR EACH PLANKONTO WHERE PLANKONTO.PLANNR = PLANNRTAB.PLANNR AND 
         PLANKONTO.ARTAL = PLANNRTAB.ARTAL 
         USE-INDEX PLANKONT NO-LOCK. 
         GET FIRST aokontq NO-LOCK.
         DO WHILE AVAILABLE(PLANKONTO):            
            SUBSTRING(tidut.UT,21) = PLANNRTAB.PRISTYP.
            IF musz = FALSE THEN CREATE tidut.
            ASSIGN               
            SUBSTRING(tidut.UT,21) = PLANNRTAB.PRISTYP
            SUBSTRING(tidut.UT,31) = PLANKONTO.K1
            SUBSTRING(tidut.UT,38) = PLANKONTO.K2
            SUBSTRING(tidut.UT,45) = PLANKONTO.K3
            SUBSTRING(tidut.UT,52) = PLANKONTO.K4
            SUBSTRING(tidut.UT,59) = PLANKONTO.K5
            SUBSTRING(tidut.UT,66) = STRING(PLANKONTO.SATS%).
            
            musz = FALSE.
            GET NEXT aokontq NO-LOCK.
         END. 
         CLOSE QUERY aokontq.       
         IF tog_ben = TRUE THEN DO:
            CREATE tidut.
            SUBSTRING(tidut.UT,1) = PLANNRTAB.ORT.                        
         END.
         musz = FALSE.
      END. 
      ELSE DO:                                                  
         SUBSTRING(tidut.UT,21) = SUBSTRING(PLANNRTAB.ORT,1,38).         
      END.     
   END.         
END PROCEDURE.

       /*AONRKONT*/
PROCEDURE aonrkont2_UI :
   DO TRANSACTION:       
      CREATE tidut2.     
      ASSIGN
      SUBSTRING(tidut2.UT,1) = PLANNRTAB.PLANNR
      SUBSTRING(tidut2.UT,8) = STRING(PLANNRTAB.ARTAL,"9999")                                                
      SUBSTRING(tidut2.UT,14) = PLANNRTAB.OMRADE.  
      /*SUBSTRING(tidut2.UT,21) = PLANNRTAB.PRISTYP.      */
      IF vart = 19 THEN DO:
         tidut2.ARBARTKOD = PLANNRTAB.ARBARTKOD.                                                        
      END.
      IF tog_avs = FALSE AND tog_ejav = TRUE THEN DO:
         SUBSTRING(tidut2.UT,70) = STRING(PLANNRTAB.FASTKALK,"Ja/Nej").
      END.       
      ELSE DO:          
         IF PLANNRTAB.PLANNRAVDATUM NE ? THEN DO:    
            IF PLANNRTAB.PLANNRAVDATUM NE 01/01/91 THEN
            SUBSTRING(tidut2.UT,70) = STRING(PLANNRTAB.PLANNRAVDATUM).
         END.            
         SUBSTRING(tidut2.UT,79) = STRING(PLANNRTAB.FASTKALK,"Ja/Nej").
      END.     
      
      IF tog_konto = TRUE THEN DO:      
         musz = TRUE.
         OPEN QUERY aokontq FOR EACH PLANKONTO WHERE PLANKONTO.PLANNR = PLANNRTAB.PLANNR AND 
         PLANKONTO.ARTAL = PLANNRTAB.ARTAL 
         USE-INDEX PLANKONT NO-LOCK. 
         GET FIRST aokontq NO-LOCK.
         DO WHILE AVAILABLE(PLANKONTO):            
            SUBSTRING(tidut2.UT,21) = PLANNRTAB.PRISTYP.
            IF musz = FALSE THEN CREATE tidut2.
            ASSIGN               
            SUBSTRING(tidut2.UT,21) = PLANNRTAB.PRISTYP
            SUBSTRING(tidut2.UT,31) = PLANKONTO.K1
            SUBSTRING(tidut2.UT,38) = PLANKONTO.K2
            SUBSTRING(tidut2.UT,45) = PLANKONTO.K3
            SUBSTRING(tidut2.UT,52) = PLANKONTO.K4
            SUBSTRING(tidut2.UT,59) = PLANKONTO.K5
            SUBSTRING(tidut2.UT,66) = STRING(PLANKONTO.SATS%).
            musz = FALSE.
            GET NEXT aokontq NO-LOCK.
         END. 
         CLOSE QUERY aokontq.       
         IF tog_ben = TRUE THEN DO:
            CREATE tidut2.
            SUBSTRING(tidut2.UT,1) = PLANNRTAB.ORT.                        
         END.
         musz = FALSE.
      END. 
      ELSE DO:                                                  
         IF vart = 19 THEN DO:
            tidut2.ARBARTKOD = PLANNRTAB.ARBARTKOD.                                         
         END.
         SUBSTRING(tidut2.UT,21) = SUBSTRING(PLANNRTAB.ORT,1,38).         
      END.     
   END.         
END PROCEDURE.
