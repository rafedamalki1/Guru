
/*------------------------------------------------------------------------
    File        : AOMBAPPSU45I.I
    Purpose     :  

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Jun 27 14:49:21 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

PROCEDURE fakt_UI :   
   GET FIRST fq NO-LOCK.
   DO WHILE AVAILABLE(FAKTINTAKTKONT):     
      RUN hitta_UI (INPUT valdaao.BESTID, INPUT valdaao.FAKTTYP).   
      IF FAKTINTAKTKONT.FAKTNR = valdaao.FAKTNR THEN 
         sumaotemp.FAKTKOST = sumaotemp.FAKTKOST + FAKTINTAKTKONT.BELOPP.
      ELSE DO:
         FIND FIRST FAKTPLAN WHERE FAKTPLAN.FAKTNR = FAKTINTAKTKONT.FAKTNR NO-LOCK NO-ERROR.
         RUN hitta_UI (INPUT FAKTPLAN.BESTID, INPUT FAKTPLAN.FAKTTYP).         
         sumaotemp.FAKTKOST = sumaotemp.FAKTKOST + FAKTINTAKTKONT.BELOPP.
      END.   
      
      GET NEXT fq NO-LOCK.      
   END.
END PROCEDURE.
PROCEDURE faktk_UI :   
   GET FIRST fkq NO-LOCK.
   DO WHILE AVAILABLE(FAKTINTAKTKONTKRED):     
      RUN hittak_UI (INPUT valdaao.BESTID, INPUT valdaao.FAKTTYP).   
      IF FAKTINTAKTKONTKRED.FAKTNR = valdaao.FAKTNR THEN sumaotemp.FAKTKOST = sumaotemp.FAKTKOST - FAKTINTAKTKONTKRED.BELOPP.
      ELSE DO:
         FIND FIRST FAKTPLAN WHERE FAKTPLAN.FAKTNR = FAKTINTAKTKONTKRED.FAKTNR NO-LOCK NO-ERROR.
         RUN hittak_UI (INPUT FAKTPLAN.BESTID, INPUT FAKTPLAN.FAKTTYP).         
         sumaotemp.FAKTKOST = sumaotemp.FAKTKOST - FAKTINTAKTKONTKRED.BELOPP.
      END.             
      GET NEXT fkq NO-LOCK.      
   END.
END PROCEDURE.
PROCEDURE hitta_UI :
   DEFINE INPUT PARAMETER varbes LIKE BESTTAB.BESTID NO-UNDO.
   DEFINE INPUT PARAMETER varftyp LIKE FAKTPLAN.FAKTTYP NO-UNDO.   
   FIND FIRST sumaotemp WHERE sumaotemp.AONR = valdaao.AONR AND sumaotemp.DELNR = valdaao.DELNR 
   NO-ERROR.
   IF NOT AVAILABLE sumaotemp THEN DO:
      CREATE sumaotemp.      
   END.
   ASSIGN      
   sumaotemp.AONR = valdaao.AONR
   sumaotemp.DELNR = valdaao.DELNR.            
END PROCEDURE.
PROCEDURE hittak_UI :
   DEFINE INPUT PARAMETER varbes LIKE BESTTAB.BESTID NO-UNDO.
   DEFINE INPUT PARAMETER varftyp LIKE FAKTPLAN.FAKTTYP NO-UNDO.   
   FIND FIRST sumaotemp WHERE sumaotemp.AONR = valdaao.AONR AND sumaotemp.DELNR = valdaao.DELNR 
   NO-ERROR.
   IF NOT AVAILABLE sumaotemp THEN DO:
      CREATE sumaotemp.
   END.
   ASSIGN      
   sumaotemp.AONR = valdaao.AONR
   sumaotemp.DELNR = valdaao.DELNR.            
END PROCEDURE.
PROCEDURE delnrsumma_UI:   
   
   FIND FIRST tidut WHERE tidut.UT = aonrvar NO-LOCK NO-ERROR.      
   tidut.UT = "".   
   /*FIND FIRST AONRTAB WHERE AONRTAB.AONR = aonrvar AND AONRTAB.DELNR = 0 NO-LOCK NO-ERROR.
   IF AVAILABLE AONRTAB  THEN DO:
      CREATE tidut.
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = Guru.Konstanter:garbal + " för "  + Guru.Konstanter:gaok + " " + aonrvar  + " " + AONRTAB.ARBANSVARIG.       
   END.*/   
   
   
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Summa " + Guru.Konstanter:gaok + " " + aonrvar .                              
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = STRING(p1sumsum.TIMMAR + p1sumsum.BTIMMAR,">>>>>9").                        
   kalktotvarp1 = p1sumsum.BELOPP + p1sumsum.MBELOPP + p1sumsum.MTRL + p1sumsum.OVRKR + p1sumsum.MASKGBELOPP + p1sumsum.BERBELOPP.
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(kalktotvarp1,">>>>>>>9").
  
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING( kalksumsum.TIMMAR + kalksumsum.BTIMMAR,">>>>>9").            
   kalktotvar = kalksumsum.BELOPP + kalksumsum.MBELOPP + kalksumsum.MTRL + kalksumsum.OVRKR + kalksumsum.MASKGBELOPP + kalksumsum.BERBELOPP.                  
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(kalktotvar,">>>>>>>9").
   IF delnrsummalager[1]  < 0                                          
   THEN SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(delnrsummalager[1],"->>>>9").
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(delnrsummalager[1],">>>>>9").
  
   IF (delnrsummalager[2] + delnrsummalager[3] + delnrsummalager[4] + delnrsummalager[5]  + delnrsummalager[6])  < 0 
   THEN SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING((delnrsummalager[2] + delnrsummalager[3] + delnrsummalager[4] + delnrsummalager[5]  + delnrsummalager[6]),"->>>>>>9").
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING((delnrsummalager[2] + delnrsummalager[3] + delnrsummalager[4] + delnrsummalager[5] + delnrsummalager[6]),">>>>>>>9").
   totsummauf = totsummauf + (delnrsummalager[2] + delnrsummalager[3] + delnrsummalager[4] + delnrsummalager[5] + delnrsummalager[6]).
   fsproc = 0.
   fsprocj = "".
   FOR EACH AONRTIDLAGE WHERE AONRTIDLAGE.AONR =  aonrvar AND AONRTIDLAGE.DELNR = 0 AND AONRTIDLAGE.IDTIDLAG BEGINS "Färdigställande Status" NO-LOCK:
      FIND FIRST TIDSLAGEN WHERE TIDSLAGEN.IDTIDLAG =  AONRTIDLAGE.IDTIDLAG NO-LOCK NO-ERROR.
      IF AVAILABLE TIDSLAGEN THEN DO:
         fsprocj = TIDSLAGEN.AKTIVITET1.
         fsprocj = REPLACE(fsprocj,"%","").
         IF  INTEGER(fsprocj) > fsproc THEN DO:
             fsproc = INTEGER(fsprocj).
             fsdat = AONRTIDLAGE.DATUM1.
         END.    
      END.
   END.
   /*MESSAGE "f" aonrvar   totsummaprog
   VIEW-AS ALERT-BOX.*/
   /*Om ingen FS-procent registrerad - räkna ut FS = UF kost/P2 kost*/  
   /*
   Om UF Kost > 0:
   Om FS % > 0                    Prognoskostnad = UF Kost(montör projektledare projektör dokumentation maskin materiel Övrigt/Entreprenad) / FS%
   Om FS% = 0 och P2 kost > 0      prognoskostnad =  P2 Kost
   Om FS% = 0 och P2 kost = 0 och P1 kost > 0  Prognoskostnad =  P1 Kost

  Om UF Kost = 0:
  Om FS % > 0 och och P2 kost > 0             Prognoskostnad = P2 Kost / FS%
  Om FS% > 0 och P2 kost = 0 och P1 kost > 0  Prognoskostnad =  P1 Kost /FS%
  Om FS% = 0 och P2 kost > 0                  Prognoskostnad =  P2 Kost
  Om FS% = 0 och P2 kost = 0 och P1 kost > 0  Prognoskostnad =  P1 Kost   
   */  
  

   IF (delnrsummalager[2] + delnrsummalager[3] + delnrsummalager[4] + delnrsummalager[5])  > 0 THEN DO:
      /*om det finns UF kostnader*/ 
      IF fsproc > 0 THEN DO:      
         SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(fsproc ,">>9").
         IF fsdat NE ? THEN  SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(fsdat ,"99/99/99").
         ELSE SUBSTRING(tidut.UT,utnr[nrcol[10]]) = "Ej dat".
         SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING((delnrsummalager[1]/(fsproc / 100)) ,">>>>>9").
         SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING( (delnrsummalager[2] + delnrsummalager[3] + delnrsummalager[4] + delnrsummalager[5]) / ( fsproc / 100 ) ,">>>>>>>9").
         totsummaprog = totsummaprog + ((delnrsummalager[2] + delnrsummalager[3] + delnrsummalager[4] + delnrsummalager[5]) / ( fsproc / 100 )).      
      END.
      ELSE DO:     
         IF  kalktotvar > 0 THEN DO:        
            IF kalktotvar NE 0 AND (delnrsummalager[2] + delnrsummalager[3] + delnrsummalager[4] + delnrsummalager[5]) NE 0 THEN
            SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING((( delnrsummalager[2] + delnrsummalager[3] + delnrsummalager[4] + delnrsummalager[5]) / kalktotvar ) * 100 ,">>>>9").         
            SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(( kalksumsum.TIMMAR + kalksumsum.BTIMMAR) ,">>>>>9").
            SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(kalktotvar ,">>>>>>>9").
            totsummaprog = totsummaprog + kalktotvar.
         END.
         ELSE DO:                    
            IF kalktotvarp1 NE 0 AND (delnrsummalager[2] + delnrsummalager[3] + delnrsummalager[4] + delnrsummalager[5]) NE 0 THEN
            SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING((( delnrsummalager[2] + delnrsummalager[3] + delnrsummalager[4] + delnrsummalager[5]) / kalktotvarp1 ) * 100 ,">>>>9").         
            SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(( p1sumsum.TIMMAR + p1sumsum.BTIMMAR) ,">>>>>9").
            SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(kalktotvarp1 ,">>>>>>>9").
            totsummaprog = totsummaprog + kalktotvarp1.
         END.
      END.           
   END.
   ELSE DO:  
      IF fsproc > 0 THEN DO:      
         SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(fsproc ,">>9").
         IF fsdat NE ? THEN  SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(fsdat ,"99/99/99").
         ELSE SUBSTRING(tidut.UT,utnr[nrcol[10]]) = "Ej dat".
         SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING((delnrsummalager[1]/(fsproc / 100)) ,">>>>>9").
         IF  kalktotvar > 0 THEN DO:
            SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING( kalktotvar / ( fsproc / 100 ) ,">>>>>>>9").
            totsummaprog = totsummaprog + (kalktotvar / ( fsproc / 100 )).
         END.
         ELSE DO:
            SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING( kalktotvarp1 / ( fsproc / 100 ) ,">>>>>>>9").
            totsummaprog = totsummaprog + (kalktotvarp1 / ( fsproc / 100 )).            
         END.            
      END.
      ELSE DO:     
         IF  kalktotvar > 0 THEN DO:                             
            SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(( kalksumsum.TIMMAR + kalksumsum.BTIMMAR) ,">>>>>9").
            SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(kalktotvar ,">>>>>>>9").
            totsummaprog = totsummaprog + kalktotvar.
         END.
         ELSE DO:                                         
            SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(( p1sumsum.TIMMAR + p1sumsum.BTIMMAR) ,">>>>>9").
            SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(kalktotvarp1 ,">>>>>>>9").
            totsummaprog = totsummaprog + kalktotvarp1.
         END.
      END.
   END.                           
   CREATE tidut.
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Tid/kostnad Maskin".
   IF p1sumsum.MASKGBELOPP > 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(p1sumsum.MASKGBELOPP,">>>>>>>9").
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(p1sumsum.MASKGBELOPP,"->>>>>>9").                              
   IF kalksumsum.MASKGBELOPP > 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(kalksumsum.MASKGBELOPP,">>>>>>>9").
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(kalksumsum.MASKGBELOPP,"->>>>>>9").                  
   IF delnrsummalager[3] > 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(delnrsummalager[3],">>>>>>>9").                                   
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(delnrsummalager[3],"->>>>>>9").
  
   CREATE tidut.
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Materiel".
   IF p1sumsum.MTRL > 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(p1sumsum.MTRL,">>>>>>>9").
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(p1sumsum.MTRL,"->>>>>>9").                                    
   IF kalksumsum.MTRL > 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(kalksumsum.MTRL,">>>>>>>9").
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(kalksumsum.MTRL,"->>>>>>9").                   
   IF delnrsummalager[4] > 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(delnrsummalager[4],">>>>>>>9").                                   
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(delnrsummalager[4],"->>>>>>9").
   
   CREATE tidut.
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Övrigt/Entreprenad".                                    
   IF p1sumsum.OVRKR > 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(p1sumsum.OVRKR,">>>>>>>9").
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(p1sumsum.OVRKR,"->>>>>>9").                        
   IF kalksumsum.OVRKR > 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(kalksumsum.OVRKR,">>>>>>>9").
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(kalksumsum.OVRKR,"->>>>>>9").            
   
   IF delnrsummalager[5] > 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(delnrsummalager[5],">>>>>>>9").                                   
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(delnrsummalager[5],"->>>>>>9").
   
   CREATE tidut.
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Övrigt".           /*tidigare Intäkt*/                         
        
   IF delnrsummalager[6] > 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(delnrsummalager[6],">>>>>>>9").                                   
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(delnrsummalager[6],"->>>>>>9").
   CREATE tidut.   
         
   i = 1.
   DO WHILE i <= breddantal:             
      ASSIGN
      delnrsummalagerK[i] = 0
      delnrsummalager[i] = 0.            
      i = i + 1.
   END.  

END PROCEDURE.


PROCEDURE huvud2_UI :
   CREATE tidut. 
   SUBSTRING(tidut.UT,60) = STRING(TODAY) + " " + STRING(TIME,"HH:MM").
   CREATE tidut.
   CREATE tidut.
   tidut.UT = uppvaltemp.VALDLISTA. 
   IF uppvaltemp.VISPERAR = TRUE THEN DO: 
      SUBSTRING(tidut.UT,64) = "ÅR " + STRING(YEAR(uppvaltemp.STARTDATUM),"9999").
   END.                        
   ELSE IF uppvaltemp.VISPERAR = FALSE THEN DO:
      SUBSTRING(tidut.UT,64) = "PERIOD " +  STRING(uppvaltemp.STARTDATUM) + 
      " - " + STRING(uppvaltemp.SLUTDATUM).     
   END.
   ELSE DO:
      SUBSTRING(tidut.UT,64) = "VISNING AV ALLT".
   END.
   /*CREATE tidut.
   {KUURV.I}*/
   FIND FIRST tempupp NO-ERROR.
   CREATE tidut.
   CREATE tidut.
   ASSIGN SUBSTRING(tidut.UT,1) = "Total Budget för år " + STRING(YEAR(uppvaltemp.STARTDATUM)) + " : " + STRING(tempupp.ototalt,">>>>>>>9") + " kr".
   CREATE tidut.
   ASSIGN SUBSTRING(tidut.UT,1) = "Utfall för år " + STRING(YEAR(uppvaltemp.STARTDATUM)) + " : " .
   CREATE tidut.
   ASSIGN SUBSTRING(tidut.UT,1) = "Prognos för år " + STRING(YEAR(uppvaltemp.STARTDATUM)) + " : ".      
   CREATE tidut.
   CREATE tidut.
   ASSIGN SUBSTRING(tidut.UT,1) = "FS = Färdigställande status. Om ingen FS är registrerad på projektet räknas FS ut 'UF kost / P2 kost'.".
   CREATE tidut.
   ASSIGN SUBSTRING(tidut.UT,1) = "                             Om FS är registrerad på projektet visas även FS-datum.".
   ASSIGN
   nrcol[1] = 1
   nrcol[2] = 2
   nrcol[3] = 3
   nrcol[4] = 4
   nrcol[5] = 5
   nrcol[6] = 6
   nrcol[7] = 7
   
   nrcol[8] = 8
   nrcol[9] = 9
   nrcol[10] = 10
   nrcol[11] = 11
   nrcol[12] = 12
     
      
   breddantal = 12    /*antal kolumner*/
   bredd[1] = 40
   bredd[2] = 6
   bredd[3] = 8
   bredd[4] = 6
   bredd[5] = 8
   bredd[6] = 6
   bredd[7] = 8
   bredd[8] = 5
   bredd[9] = 5
   bredd[10] = 8
   bredd[11] = 6.
   bredd[12] = 8.
         
   ASSIGN
   i = 2.     
   utnr[nrcol[1]] = 1.
   DO WHILE i <= breddantal:             
      utnr[i] = utnr[i - 1] + bredd[i - 1] + 1.            
      i = i + 1.
   END.   
   ASSIGN
   str = "".  
   i = 1.
   DO WHILE i <= utnr[breddantal] + bredd[breddantal] - 1:
      str = str + "=".     
      i = i + 1.
   END.   
   i = 2.      
   DO WHILE i <= breddantal:             
      SUBSTRING(str,(utnr[i] - 1),1) = ".".      
      i = i + 1.
   END.          
   FIND FIRST valdaao WHERE valdaao.DELNR = 0 NO-ERROR.
   
   CREATE tidut.  
   CREATE tidut.   
   CREATE tidut.             
   ASSIGN                                            
   /*SUBSTRING(tidut.UT,utnr[nrcol[1]]) = SUBSTRING(valdaao.ORT,1,bredd[1])         */                                                                     
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = "P1"
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "P1"         
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "P2"
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "P2"                                          
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "UF"
   SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "UF"
   SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "FS %"
   SUBSTRING(tidut.UT,utnr[nrcol[9]]) = "FS %"
   SUBSTRING(tidut.UT,utnr[nrcol[10]]) = "FS datum"
   SUBSTRING(tidut.UT,utnr[nrcol[11]]) = "Prognos".
   SUBSTRING(tidut.UT,utnr[nrcol[12]]) = "Prognos".
   
   CREATE tidut.             
   ASSIGN                                                                                                                            
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = "Tid"         
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "Kost"                                          
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "Tid"
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "Kost"
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "Tid".
   SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "Kost".
   SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "UF/KA".
   SUBSTRING(tidut.UT,utnr[nrcol[9]]) = "Man".
   SUBSTRING(tidut.UT,utnr[nrcol[11]]) = "Tid".
   SUBSTRING(tidut.UT,utnr[nrcol[12]]) = "Kostnad".
   str3 = tidut.UT.                
   CREATE tidut.      
   tidut.UT = str.
   /*IF summaforst = TRUE THEN DO:         
      CREATE tidut.
      tidut.UT = "UF".   
      CREATE tidut.      
   END.*/   
   CREATE sumsum.
   ASSIGN sumsum.PERSONALKOD = "UF".
   EMPTY TEMP-TABLE inextrakopptemp NO-ERROR. 
   EMPTY TEMP-TABLE extrakopptemp NO-ERROR.       
   CREATE inextrakopptemp.      
   FOR EACH valdaao: 
      FIND FIRST slutsum WHERE slutsum.AONR = valdaao.AONR AND
      slutsum.DELNR = valdaao.DELNR NO-ERROR.
      ASSIGN
      inextrakopptemp.PROGRAM = "FBDEB"      
      inextrakopptemp.KOPPLACHAR1 = ?  
      inextrakopptemp.KOPPLAINT1 =  ?      
      inextrakopptemp.KOPPLACHAR2 = valdaao.AONR           
      inextrakopptemp.KOPPLAINT2 =  valdaao.DELNR.
      RUN etabhamt_UI IN fbestapph (INPUT TABLE inextrakopptemp,OUTPUT TABLE extrakopptemp APPEND).
      ASSIGN
      inextrakopptemp.PROGRAM = "FBKRED"
      inextrakopptemp.KOPPLACHAR1 = ?  
      inextrakopptemp.KOPPLAINT1 =  ?      
      inextrakopptemp.KOPPLACHAR2 = valdaao.AONR           
      inextrakopptemp.KOPPLAINT2 =  valdaao.DELNR.         
      RUN etabhamt_UI IN fbestapph (INPUT TABLE inextrakopptemp,OUTPUT TABLE extrakopptemp APPEND).
      IF NOT AVAILABLE slutsum THEN DO:
         FIND FIRST kosttemp WHERE kosttemp.AONR = valdaao.AONR AND kosttemp.DELNR = valdaao.DELNR USE-INDEX AONR NO-ERROR.
         FIND FIRST kalksumsum WHERE kalksumsum.AONR = valdaao.AONR AND kalksumsum.DELNR = kalksumsum.DELNR USE-INDEX AONR NO-ERROR.
         IF AVAILABLE kosttemp THEN musz = musz.                  
         ELSE IF AVAILABLE kalksumsum THEN musz = musz.

         ELSE NEXT.
         CREATE slutsum.
         ASSIGN
         slutsum.AONR  = valdaao.AONR 
         slutsum.DELNR = valdaao.DELNR
         slutsum.ORT   = valdaao.ORT.  
      END.         
      ELSE slutsum.ORT = valdaao.ORT.
   END.
   IF uppvaltemp.VISPERAR = TRUE THEN DO:                  
      OPEN QUERY exq FOR EACH extrakopptemp WHERE 
      YEAR(extrakopptemp.SOKDATE[1]) = YEAR(uppvaltemp.STARTDATUM).
   END.
   ELSE IF uppvaltemp.VISPERAR = FALSE THEN DO:
      OPEN QUERY exq FOR EACH extrakopptemp WHERE 
      extrakopptemp.SOKDATE[1] >= uppvaltemp.STARTDATUM AND 
      extrakopptemp.SOKDATE[1] <= uppvaltemp.SLUTDATUM. 
   END.
   ELSE DO:                  
      OPEN QUERY exq FOR EACH extrakopptemp.
   END.
   GET FIRST exq.
   DO WHILE AVAILABLE(extrakopptemp):     
      FIND FIRST slutsum WHERE slutsum.AONR = extrakopptemp.KOPPLACHAR2 AND
      slutsum.DELNR = extrakopptemp.KOPPLAINT2 NO-ERROR.
      IF NOT AVAILABLE slutsum THEN DO:
         FIND FIRST valdaao WHERE valdaao.AONR = extrakopptemp.KOPPLACHAR2 AND
         valdaao.DELNR = extrakopptemp.KOPPLAINT2 NO-ERROR.
         IF AVAILABLE valdaao THEN DO:
            CREATE slutsum.
            ASSIGN
            slutsum.AONR  = valdaao.AONR 
            slutsum.DELNR = valdaao.DELNR
            slutsum.ORT   = valdaao.ORT.  
         END.        
      END.             
      IF AVAILABLE slutsum THEN DO:
         IF extrakopptemp.PROGRAM = "FBDEB" THEN slutsum.MBELOPP = slutsum.MBELOPP + extrakopptemp.SOKDEC[1].
         IF extrakopptemp.PROGRAM = "FBKRED" THEN slutsum.MBELOPP = slutsum.MBELOPP - extrakopptemp.SOKDEC[1].
      END.
      GET NEXT exq.
   END.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.      
   edataapph = ?.
   IF VALID-HANDLE(fbestapph) THEN DELETE PROCEDURE fbestapph.      
   fbestapph = ?.
   totsummauf = 0.
   totsummaprog = 0.           
   FIND FIRST slutsum NO-LOCK NO-ERROR.
   IF AVAILABLE slutsum THEN DO:      
      FOR EACH kalksumsum WHERE kalksumsum.KTYP = "PN1".
         CREATE p1sumsum.
         BUFFER-COPY kalksumsum  TO p1sumsum.
         DELETE kalksumsum.
      END.
      aonrvar = slutsum.AONR.
      ortvar = slutsum.ORT.
      
      /*Fördelningsnyckel sätts i Konotregistrering för alla projekt*/
      ASSIGN 
      montfak = 0.79
      pledfak = 0.09
      projektorfak = 0.07.
      dokfak = 0.05.  
      RUN procset_UI.          
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "FNYCKEL"                   
      inextradatatemp.HUVUDCH = "HUVUD"  
      inextradatatemp.HUVUDINT = 0.   
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp).
      FIND FIRST extradatatemp NO-LOCK NO-ERROR.
      IF AVAILABLE extradatatemp THEN DO:
         ASSIGN
         montfak = extradatatemp.SOKINT[1] / 100
         pledfak = extradatatemp.SOKINT[2] / 100
         projektorfak = extradatatemp.SOKINT[3] / 100
         dokfak = extradatatemp.SOKINT[4] / 100.         
      END.
         
      FOR EACH slutsum USE-INDEX AONR:      
         IF slutsum.AONR NE aonrvar THEN DO:
            RUN delnrsumma_UI.
            aonrvar = slutsum.AONR.
            ortvar = slutsum.ORT.        
         END.         
         FIND FIRST tidut WHERE tidut.UT = aonrvar NO-ERROR.
         IF NOT AVAILABLE tidut THEN DO:
            CREATE tidut.
            tidut.UT = Guru.Konstanter:gaok + ": " +  aonrvar + "  " + ortvar.   
            FIND FIRST AONRTAB WHERE AONRTAB.AONR = aonrvar AND AONRTAB.DELNR = 0 NO-LOCK NO-ERROR.
            IF AVAILABLE AONRTAB  THEN DO:
               FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = AONRTAB.ARBANSVARIG NO-LOCK NO-ERROR.
               IF AVAILABLE PERSONALTAB THEN DO:
                  CREATE tidut.
                  SUBSTRING(tidut.UT,utnr[nrcol[1]]) = Guru.Konstanter:garbal +  ": " + PERSONALTAB.FORNAMN + " " + PERSONALTAB.EFTERNAMN.
                  Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
               END.          
            END.
                     
            CREATE tidut.
            tidut.UT = aonrvar.   
         END.                       
         ASSIGN
         varin = 0
         varkalk = 0.
         FIND FIRST kalksumsum WHERE kalksumsum.AONR = slutsum.AONR AND
         kalksumsum.DELNR = 0 USE-INDEX AONR NO-ERROR.
         IF NOT AVAILABLE kalksumsum THEN DO:            
            CREATE kalksumsum.
            ASSIGN            
            kalksumsum.AONR = slutsum.AONR 
            kalksumsum.DELNR = 0
            kalksumsum.KTYP = "KL2".
         END.         
         varkalk = kalksumsum.BELOPP + kalksumsum.MBELOPP + 
         kalksumsum.MTRL + kalksumsum.OVRKR + kalksumsum.MASKGBELOPP  + kalksumsum.BERBELOPP.                  
         FIND FIRST p1sumsum WHERE p1sumsum.AONR = slutsum.AONR AND
         p1sumsum.DELNR = 0  USE-INDEX AONR NO-ERROR.
         IF NOT AVAILABLE p1sumsum THEN DO:
            CREATE p1sumsum.
            ASSIGN            
            p1sumsum.AONR = slutsum.AONR 
            p1sumsum.DELNR = 0
            p1sumsum.KTYP = "PN1".
         END.         
         varplan = p1sumsum.BELOPP + p1sumsum.MBELOPP + 
         p1sumsum.MTRL + p1sumsum.OVRKR + p1sumsum.MASKGBELOPP  + p1sumsum.BERBELOPP.
                  
        
         FIND FIRST kosttemp WHERE kosttemp.AONR = slutsum.AONR AND
         kosttemp.DELNR = slutsum.DELNR USE-INDEX AONR NO-ERROR.
         IF NOT AVAILABLE kosttemp THEN DO:
            CREATE kosttemp.
            ASSIGN
            kosttemp.AONR = slutsum.AONR 
            kosttemp.DELNR = slutsum.DELNR. 
         END.      
         FIND FIRST sumaotemp WHERE sumaotemp.AONR = slutsum.AONR AND sumaotemp.DELNR = slutsum.DELNR NO-ERROR.
         IF AVAILABLE sumaotemp THEN kosttemp.INKOMST = kosttemp.INKOMST .
         /* Enbart externa intäkter 20120925
         IF AVAILABLE sumaotemp THEN kosttemp.INKOMST = kosttemp.INKOMST + sumaotemp.FAKTKOST.*/ 
         varin = kosttemp.INKOMST.
         vartot = vartot + (varin - varkalk).
         
         /*
         /*Om avvikande fördelning för visst projekt*/  
         RUN procset_UI.          
         EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
         EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
         CREATE inextradatatemp.          
         ASSIGN
         inextradatatemp.PROGRAM = "FNYCKEL"                   
         inextradatatemp.HUVUDCH = slutsum.AONR 
         inextradatatemp.HUVUDINT = 0.   
         RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp).
         FIND FIRST extradatatemp NO-LOCK NO-ERROR.
         IF AVAILABLE extradatatemp THEN DO:
            ASSIGN
            montfak = extradatatemp.SOKINT[1] / 100
            pledfak = extradatatemp.SOKINT[2] / 100
            projektorfak = extradatatemp.SOKINT[3] / 100
            dokfak = extradatatemp.SOKINT[4] / 100.         
         END.*/     
             
         IF slutsum.DELNR = 0 THEN DO:
            CREATE tidut.                 
            ASSIGN
            /*SUBSTRING(tidut.UT,utnr[nrcol[1]]) = Guru.Konstanter:gaok + " " + slutsum.AONR + " delnr: " + STRING(slutsum.DELNR,Guru.Konstanter:varforetypchar[1]).*/
            SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Tid/kostnad delnr: " + STRING(slutsum.DELNR,Guru.Konstanter:varforetypchar[1]).                        
            ASSIGN
            SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(slutsum.TIMMAR,">>>>>9")                    
            SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(slutsum.BELOPP,">>>>>>>9").
            delnrsummalager[1] = delnrsummalager[1] + slutsum.TIMMAR.
            delnrsummalager[2] = delnrsummalager[2] + slutsum.BELOPP.
            delnrsummalager[3] = delnrsummalager[3] + slutsum.LONKOST + kosttemp.GRAVARE  + slutsum.MBELOPP.
            delnrsummalager[4] = delnrsummalager[4] + kosttemp.MTRL.
            delnrsummalager[5] = delnrsummalager[5]  +  kosttemp.MASKOVRIG.
            delnrsummalager[6] = delnrsummalager[6] + kosttemp.OVRKR.                /* + kosttemp.INKOMST.*/
            /*Ändrat 20121214
            delnrsummalager[1] = delnrsummalager[1] + slutsum.TIMMAR.
            delnrsummalager[2] = delnrsummalager[2] + slutsum.BELOPP.
            delnrsummalager[3] = delnrsummalager[3] + slutsum.LONKOST + kosttemp.GRAVARE +  kosttemp.MASKOVRIG + slutsum.MBELOPP.
            delnrsummalager[4] = delnrsummalager[4] + kosttemp.MTRL.
            delnrsummalager[5] = delnrsummalager[5] + kosttemp.OVRKR.
            delnrsummalager[6] = delnrsummalager[6] + kosttemp.INKOMST.*/                                    
            ASSIGN
            SUBSTRING(tidut.UT,utnr[nrcol[8]]) = ""
            SUBSTRING(tidut.UT,utnr[nrcol[9]]) = ""
            SUBSTRING(tidut.UT,utnr[nrcol[10]]) = "".
                       
         END.         
         ELSE IF slutsum.DELNR = 1 THEN DO:
            CREATE tidut.                             
            ASSIGN
            SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Tid/kostnad Montör/Entreprenör delnr: " + STRING(slutsum.DELNR,Guru.Konstanter:varforetypchar[1]).
            SUBSTRING(tidut.UT,utnr[nrcol[2]]) = STRING(montfak *( p1sumsum.TIMMAR ),">>>>>9").            
            SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(montfak * (p1sumsum.BELOPP),">>>>>>>9").                        
            SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(montfak * (kalksumsum.TIMMAR),">>>>>9").
            SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(montfak * (kalksumsum.BELOPP),">>>>>>>9").
            /*SUBSTRING(tidut.UT,utnr[nrcol[2]]) = STRING(montfak *(p1sumsum.MONTTIMMAR + p1sumsum.TIMMAR + p1sumsum.BTIMMAR),">>>>>9").            
            SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(montfak * (p1sumsum.BELOPP + p1sumsum.MONTBELOPP + p1sumsum.BERBELOPP),">>>>>>>9").                        
            SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(montfak * (kalksumsum.MONTTIMMAR + kalksumsum.TIMMAR + kalksumsum.BTIMMAR),">>>>>9").
            SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(montfak * (kalksumsum.BELOPP + kalksumsum.MONTBELOPP + kalksumsum.BERBELOPP),">>>>>>>9").*/            
            
                
            ASSIGN
            SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(slutsum.TIMMAR,">>>>>9")                    
            SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(slutsum.BELOPP,">>>>>>>9")
            SUBSTRING(tidut.UT,utnr[nrcol[8]]) = ""
            SUBSTRING(tidut.UT,utnr[nrcol[9]]) = ""
            SUBSTRING(tidut.UT,utnr[nrcol[10]]) = "".            
            delnrsummalager[1] = delnrsummalager[1] + slutsum.TIMMAR.
            delnrsummalager[2] = delnrsummalager[2] + slutsum.BELOPP.
            delnrsummalager[3] = delnrsummalager[3] + slutsum.LONKOST + kosttemp.GRAVARE  + slutsum.MBELOPP.
            delnrsummalager[4] = delnrsummalager[4] + kosttemp.MTRL.
            delnrsummalager[5] = delnrsummalager[5] + kosttemp.MASKOVRIG.
            delnrsummalager[6] = delnrsummalager[6] + kosttemp.OVRKR.  /*+ kosttemp.INKOMST.*/
         END.
         ELSE IF slutsum.DELNR = 2 THEN DO:
            CREATE tidut.                 
            ASSIGN
            SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Tid/kostnad Projektledare delnr: " + STRING(slutsum.DELNR,Guru.Konstanter:varforetypchar[1]).
            SUBSTRING(tidut.UT,utnr[nrcol[2]]) = STRING(pledfak *( p1sumsum.TIMMAR ),">>>>>9").            
            SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(pledfak * (p1sumsum.BELOPP ),">>>>>>>9").                        
            SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(pledfak * (kalksumsum.TIMMAR),">>>>>9").
            SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(pledfak * (kalksumsum.BELOPP),">>>>>>>9").                                          
            /*SUBSTRING(tidut.UT,utnr[nrcol[2]]) = STRING(pledfak *( p1sumsum.TIMMAR + p1sumsum.BTIMMAR),">>>>>9").            
            SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(pledfak * (p1sumsum.BELOPP  + p1sumsum.BERBELOPP),">>>>>>>9").                        
            SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(pledfak * (kalksumsum.TIMMAR + kalksumsum.BTIMMAR),">>>>>9").
            SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(pledfak * (kalksumsum.BELOPP + kalksumsum.BERBELOPP),">>>>>>>9").*/
            ASSIGN 
            SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(slutsum.TIMMAR,">>>>>9")                    
            SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(slutsum.BELOPP,">>>>>>>9")
            SUBSTRING(tidut.UT,utnr[nrcol[8]]) = ""
            SUBSTRING(tidut.UT,utnr[nrcol[9]]) = ""
            SUBSTRING(tidut.UT,utnr[nrcol[10]]) = "".            
            delnrsummalager[1] = delnrsummalager[1] + slutsum.TIMMAR.
            delnrsummalager[2] = delnrsummalager[2] + slutsum.BELOPP.
            delnrsummalager[3] = delnrsummalager[3] + slutsum.LONKOST + kosttemp.GRAVARE  + slutsum.MBELOPP.
            delnrsummalager[4] = delnrsummalager[4] + kosttemp.MTRL.
            delnrsummalager[5] = delnrsummalager[5] +  kosttemp.MASKOVRIG.
            delnrsummalager[6] = delnrsummalager[6] + kosttemp.OVRKR.  /* + kosttemp.INKOMST.*/
         END.
         ELSE IF slutsum.DELNR = 3 THEN DO:
            CREATE tidut.                 
            ASSIGN            
            SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Tid/kostnad Projektör delnr: " + STRING(slutsum.DELNR,Guru.Konstanter:varforetypchar[1]).                                    
            SUBSTRING(tidut.UT,utnr[nrcol[2]]) = STRING(projektorfak *( p1sumsum.BTIMMAR),">>>>>9").            
            SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(projektorfak * (p1sumsum.BERBELOPP),">>>>>>>9").                        
            SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(projektorfak * (kalksumsum.BTIMMAR),">>>>>9").
            SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(projektorfak * (kalksumsum.BERBELOPP),">>>>>>>9").
            ASSIGN                 
            SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(slutsum.TIMMAR,">>>>>9")                    
            SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(slutsum.BELOPP,">>>>>>>9")
            SUBSTRING(tidut.UT,utnr[nrcol[8]]) = ""
            SUBSTRING(tidut.UT,utnr[nrcol[9]]) = ""
            SUBSTRING(tidut.UT,utnr[nrcol[10]]) = "".
            delnrsummalager[1] = delnrsummalager[1] + slutsum.TIMMAR.
            delnrsummalager[2] = delnrsummalager[2] + slutsum.BELOPP.
            delnrsummalager[3] = delnrsummalager[3] + slutsum.LONKOST + kosttemp.GRAVARE  + slutsum.MBELOPP.
            delnrsummalager[4] = delnrsummalager[4] + kosttemp.MTRL.
            delnrsummalager[5] = delnrsummalager[5] + kosttemp.MASKOVRIG.
            delnrsummalager[6] = delnrsummalager[6] + kosttemp.OVRKR.  /* + kosttemp.INKOMST.*/
         END.
         ELSE IF slutsum.DELNR = 4 THEN DO:
            CREATE tidut.                 
            ASSIGN
            SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Tid/kostnad Dokumentation delnr: " + STRING(slutsum.DELNR,Guru.Konstanter:varforetypchar[1]).                        
            SUBSTRING(tidut.UT,utnr[nrcol[2]]) = STRING(dokfak *( p1sumsum.BTIMMAR),">>>>>9").            
            SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(dokfak * (p1sumsum.BERBELOPP),">>>>>>>9").                        
            SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(dokfak * (kalksumsum.BTIMMAR),">>>>>9").
            SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(dokfak * (kalksumsum.BERBELOPP),">>>>>>>9").
            ASSIGN                 
            SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(slutsum.TIMMAR,">>>>>9")                    
            SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(slutsum.BELOPP,">>>>>>>9")
            SUBSTRING(tidut.UT,utnr[nrcol[8]]) = ""
            SUBSTRING(tidut.UT,utnr[nrcol[9]]) = ""
            SUBSTRING(tidut.UT,utnr[nrcol[10]]) = "".            
            delnrsummalager[1] = delnrsummalager[1] + slutsum.TIMMAR.
            delnrsummalager[2] = delnrsummalager[2] + slutsum.BELOPP.
            delnrsummalager[3] = delnrsummalager[3] + slutsum.LONKOST + kosttemp.GRAVARE  + slutsum.MBELOPP.
            delnrsummalager[4] = delnrsummalager[4] + kosttemp.MTRL.
            delnrsummalager[5] = delnrsummalager[5] + kosttemp.MASKOVRIG.
            delnrsummalager[6] = delnrsummalager[6] + kosttemp.OVRKR.  /*+ kosttemp.INKOMST.*/
         END.
   
         ELSE IF slutsum.DELNR = 99 THEN DO:
            /*ändrat 20130521 efter samtal med Ingrid intäkt skall med men ej summeras 99 skall ha tidstopp*/
            CREATE tidut.                 
            ASSIGN
            SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Intäkt - ej summerad  delnr: " + STRING(slutsum.DELNR,Guru.Konstanter:varforetypchar[1]).
            ASSIGN                 
            SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(slutsum.TIMMAR,">>>>>9")                    
            SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING((slutsum.BELOPP - kosttemp.INKOMST) ,"->>>>>>9")
            SUBSTRING(tidut.UT,utnr[nrcol[8]]) = ""
            SUBSTRING(tidut.UT,utnr[nrcol[9]]) = ""
            SUBSTRING(tidut.UT,utnr[nrcol[10]]) = "".
                                    
                        
            delnrsummalager[1] = delnrsummalager[1] + slutsum.TIMMAR.
            delnrsummalager[2] = delnrsummalager[2] + slutsum.BELOPP.
            delnrsummalager[3] = delnrsummalager[3] + slutsum.LONKOST + kosttemp.GRAVARE  + slutsum.MBELOPP.
            delnrsummalager[4] = delnrsummalager[4] + kosttemp.MTRL.
            delnrsummalager[5] = delnrsummalager[5] + kosttemp.MASKOVRIG.
            delnrsummalager[6] = delnrsummalager[6] + kosttemp.OVRKR.  /*+ kosttemp.INKOMST.*/
         END.   
            
         ELSE DO:            
            FIND FIRST AONRTAB WHERE AONRTAB.AONR = slutsum.AONR AND AONRTAB.DELNR = slutsum.DELNR 
            NO-LOCK NO-ERROR.
            IF AVAILABLE AONRTAB THEN DO:               
               IF AONRTAB.ARBUPPG[1] NE "" THEN delrubrik =  AONRTAB.ARBUPPG[1].
               ELSE delrubrik = AONRTAB.ORT.     
                                         
            END.            
            CREATE tidut.                            
            ASSIGN
            SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Tid/kostnad " + delrubrik + " delnr: " + STRING(slutsum.DELNR,Guru.Konstanter:varforetypchar[1]).            
            ASSIGN                 
            SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(slutsum.TIMMAR,">>>>>9")                    
            SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(slutsum.BELOPP,">>>>>>>9")
            SUBSTRING(tidut.UT,utnr[nrcol[8]]) = ""
            SUBSTRING(tidut.UT,utnr[nrcol[9]]) = ""
            SUBSTRING(tidut.UT,utnr[nrcol[10]]) = "".
            
            delnrsummalager[1] = delnrsummalager[1] + slutsum.TIMMAR.
            delnrsummalager[2] = delnrsummalager[2] + slutsum.BELOPP.
            delnrsummalager[3] = delnrsummalager[3] + slutsum.LONKOST + kosttemp.GRAVARE  + slutsum.MBELOPP.
            delnrsummalager[4] = delnrsummalager[4] + kosttemp.MTRL.
            delnrsummalager[5] = delnrsummalager[5] +  kosttemp.MASKOVRIG.
            delnrsummalager[6] = delnrsummalager[6] + kosttemp.OVRKR.  /* + kosttemp.INKOMST.*/
         END.   
                  
         FIND FIRST sumsum WHERE sumsum.PERSONALKOD = "UF" NO-ERROR.
         ASSIGN
         sumsum.TIMMAR =  sumsum.TIMMAR  + slutsum.TIMMAR                                                      
         sumsum.BELOPP = sumsum.BELOPP + slutsum.BELOPP + kosttemp.BELOPP
         sumsum.SKOSTNAD = sumsum.SKOSTNAD + slutsum.BELOPP + kosttemp.MTRL + 
         slutsum.LONKOST + kosttemp.GRAVARE + kosttemp.MASKOVRIG + slutsum.MBELOPP +
         kosttemp.OVRKR
         sumsum.SLONKOST = sumsum.SLONKOST + slutsum.LONKOST + kosttemp.GRAVARE 
         sumsum.TINKOMST = sumsum.TINKOMST + (kosttemp.INKOMST - 
         (slutsum.BELOPP + kosttemp.MTRL + slutsum.LONKOST + kosttemp.GRAVARE + kosttemp.MASKOVRIG + 
          slutsum.MBELOPP + kosttemp.OVRKR))
         sumsum.GRAVARE = sumsum.GRAVARE + kosttemp.GRAVARE + slutsum.LONKOST
         sumsum.MASKOVRIG = sumsum.MASKOVRIG + kosttemp.MASKOVRIG + slutsum.MBELOPP         
         sumsum.MTRL = sumsum.MTRL + kosttemp.MTRL
         sumsum.OVRKR = sumsum.OVRKR + kosttemp.OVRKR                                                     
         sumsum.INKOMST = sumsum.INKOMST + kosttemp.INKOMST.                                   
      END.
      
   END.
   FIND LAST slutsum NO-LOCK NO-ERROR.
   IF AVAILABLE slutsum THEN DO:
      RUN delnrsumma_UI.       
   END.
   FIND FIRST tidut WHERE  SUBSTRING(tidut.UT,1,13) = "Utfall för år" NO-ERROR.
   IF AVAILABLE tidut THEN DO:
      ASSIGN SUBSTRING(tidut.UT,28) = STRING(totsummauf,">>>>>>>9") + " kr". 
   END.
   FIND FIRST tidut WHERE  SUBSTRING(tidut.UT,1,14) = "Prognos för år" NO-ERROR.
   IF AVAILABLE tidut THEN DO:
      ASSIGN SUBSTRING(tidut.UT,28) = STRING(totsummaprog,">>>>>>>9") + " kr". 
   END.   
   
   
   
   
         
   /*FIND FIRST sumsum WHERE sumsum.PERSONALKOD = "UF" NO-ERROR.
   IF summaforst = TRUE THEN DO:
      FIND FIRST tidut WHERE tidut.UT = "UF" NO-LOCK NO-ERROR.
      tidut.UT = "".
   END.
   ELSE CREATE tidut.
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "SUMMA".
   ASSIGN       
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "UF"                     
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(sumsum.TIMMAR,">>>>>9")    
   SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(sumsum.TINKOMST,"->>>>>>9").
   IF vartot > 0 THEN
   SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(vartot,"->>>>>>>9").
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(vartot,"->>>>>>>9").
   IF sumsum.BELOPP < 0                                            
   THEN SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(sumsum.BELOPP,"->>>>>>9").
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(sumsum.BELOPP,">>>>>>>9").
   IF sumsum.GRAVARE < 0 
   THEN SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(sumsum.GRAVARE,"->>>>>9").
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(sumsum.GRAVARE,">>>>>>9").
   IF sumsum.MASKOVRIG < 0 
   THEN SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(sumsum.MASKOVRIG,"->>>>>9").
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(sumsum.MASKOVRIG,">>>>>>9").
   IF sumsum.MTRL < 0 
   THEN SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(sumsum.MTRL,"->>>>>>9").
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(sumsum.MTRL,">>>>>>>9").
   ASSIGN                                                
   SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(sumsum.OVRKR,"->>>>>9")                                                                    
   SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(sumsum.SKOSTNAD,"->>>>>>9").
   IF sumsum.INKOMST > 0 THEN
   SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(sumsum.INKOMST,">>>>>>>9").
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(sumsum.INKOMST,"->>>>>>9").      
   FIND FIRST sumsum WHERE sumsum.PERSONALKOD = "KA" NO-ERROR.
   IF summaforst = TRUE THEN DO:
      FIND FIRST tidut WHERE tidut.UT = "KA" NO-LOCK NO-ERROR.
      tidut.UT = "".
   END.
   ELSE CREATE tidut.
   /*ANTAL MINUS*/
   ASSIGN                 
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "KA".
   
   IF sumsum.TIMMAR >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(sumsum.TIMMAR,">>>>>9").
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(sumsum.TIMMAR,"->>>>9").
   IF sumsum.BELOPP >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(sumsum.BELOPP,">>>>>>>9").                                                                       
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(sumsum.BELOPP,"->>>>>>9").                                                                       
   IF sumsum.MASKGBELOPP >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(sumsum.MASKGBELOPP,">>>>>>9").
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(sumsum.MASKGBELOPP,"->>>>>9").
   IF sumsum.MBELOPP >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(sumsum.MBELOPP,">>>>>>9").
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(sumsum.MBELOPP,"->>>>>9").
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(sumsum.OVRKR,"->>>>>9")                                                                     
   SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(sumsum.SKOSTNAD,"->>>>>>9").
   IF sumsum.MTRL > 0 THEN                                  
   SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(sumsum.MTRL,">>>>>>>9").
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(sumsum.MTRL,"->>>>>>9").
   IF sumsum.INKOMST > 0 THEN
   SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(sumsum.INKOMST,">>>>>>>9").         
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(sumsum.INKOMST,"->>>>>>9").*/                    
   
      
END PROCEDURE.

PROCEDURE tidenut_UI :
   DEFINE VARIABLE forsta AS LOGICAL NO-UNDO.
   forsta = TRUE. 
   FOR EACH valdaao NO-LOCK:
      ASSIGN
      anummer = valdaao.AONR
      dnummer = valdaao.DELNR.
      IF forsta = TRUE THEN DO:
         forsta = FALSE.                       
         /*SÖKER FÖRSTA MED TID*/
         RUN aodhuvud_UI (INPUT anummer,INPUT dnummer,INPUT uppvaltemp.STARTDATUM,
                                         INPUT uppvaltemp.SLUTDATUM,INPUT 2,INPUT FALSE ).
      END.                                         
      RUN aodsumma_UI  (INPUT anummer,INPUT dnummer, INPUT 2,INPUT FALSE ,
                                  INPUT uppvaltemp.STARTDATUM,INPUT uppvaltemp.SLUTDATUM).                                            
                    
   END.
   
END PROCEDURE.



PROCEDURE kostut_UI :
/*Smedman vill ha med kostnadsregistreringar 20170321 Lena*/
   /*IF ingakostver = FALSE THEN  DO:     */
      CREATE tidut. 
      CREATE tidut.      
      CREATE tidut. 
      CREATE tidut.
      ASSIGN tidut.UT = "VERIFIKAT FRÅN KOSTNADSREGISTRERING".              
      CREATE tidut.         
      CREATE tidut.
      ASSIGN              
      SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gaok)         
      SUBSTRING(tidut.UT,12) = "KONTO"                           
      SUBSTRING(tidut.UT,18) = "VER-NR"         
      SUBSTRING(tidut.UT,34) = "DATUM"  
      SUBSTRING(tidut.UT,43) = "BENÄMNING"
      SUBSTRING(tidut.UT,86) = "BELOPP".  
      CREATE tidut.             
      ASSIGN  
      tidut.UT =                                    
"==========.=====.===============.========.==========================================.==========".        
      FOR EACH kosttemp2 NO-LOCK:         
         CREATE tidut.
         ASSIGN                   
         SUBSTRING(tidut.UT,1) = kosttemp2.AONR + STRING(kosttemp2.DELNR,Guru.Konstanter:varforetypchar[1])
         SUBSTRING(tidut.UT,12,5) = SUBSTRING(kosttemp2.BOKKONTO,1,5) 
         SUBSTRING(tidut.UT,18) = kosttemp2.FAKTNR         
         SUBSTRING(tidut.UT,34) = STRING(kosttemp2.REGDATUM)
         SUBSTRING(tidut.UT,43) = kosttemp2.BENAMNING
         SUBSTRING(tidut.UT,86) = STRING((kosttemp2.MTRL + kosttemp2.OVRKR + kosttemp2.GRAVARE +
         kosttemp2.MASKOVRIG + 
         kosttemp2.PERSKOST + kosttemp2.PERSKOST2 + kosttemp2.TRAKTKOST) - kosttemp2.INKOMST,"->>>>>>>>9").                  
         IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO: 
             ASSIGN
             SUBSTRING(tidut.UT,7,3) = STRING(kosttemp2.DELNR,Guru.Konstanter:varforetypchar[1])
             SUBSTRING(tidut.UT,10,1) = " "
             SUBSTRING(tidut.UT,11,6) = SUBSTRING(kosttemp2.BOKKONTO,1,6). 
         END.
      END.
   /*END.*/
END PROCEDURE.   

PROCEDURE kostreg_UI : 
   
   GET FIRST kq NO-LOCK.
   DO WHILE AVAILABLE(KOSTREG):  
      CREATE kosttemp2.
      ASSIGN            
      kosttemp2.ANVANDARE = SUBSTRING(KOSTREG.ANVANDARE,1,12) 
      kosttemp2.AONR = KOSTREG.AONR 
      kosttemp2.DELNR = valdaao.DELNR
      kosttemp2.BENAMNING = KOSTREG.BENAMNING 
      kosttemp2.BETDATUM = KOSTREG.BETDATUM 
      kosttemp2.BOKKONTO = KOSTREG.BOKKONTO 
      kosttemp2.FAKBES = KOSTREG.FAKBES 
      kosttemp2.FAKTNR = KOSTREG.FAKTNR 
     /* kosttemp2.FAKTURERAD = KOSTREG.FAKTURERAD */
      kosttemp2.INKOMST = KOSTREG.INKOMST  
     /* kosttemp2.KOSTAUTO = KOSTREG.KOSTAUTO */
      kosttemp2.LEVKOD = KOSTREG.LEVKOD  
      kosttemp2.MOMS = KOSTREG.MOMS 
      kosttemp2.RADNR = KOSTREG.RADNR 
      kosttemp2.REGDATUM = KOSTREG.REGDATUM 
      kosttemp2.TRAKTKOST = KOSTREG.TRAKTKOST.

      IF KOSTREG.BOKKONTO = "471600" THEN DO:      
         kosttemp2.GRAVARE = KOSTREG.MASKKOST + KOSTREG.MTRL + KOSTREG.OVRKR +  KOSTREG.PERSKOST. 
      END.     
      ELSE IF KOSTREG.BOKKONTO = "471500" THEN DO:      
         kosttemp2.GRAVARE = KOSTREG.MASKKOST + KOSTREG.MTRL + KOSTREG.OVRKR +  KOSTREG.PERSKOST. 
      END.
      ELSE IF KOSTREG.BOKKONTO BEGINS "47" THEN DO:
         kosttemp2.MASKOVRIG = KOSTREG.MASKKOST + KOSTREG.MTRL + KOSTREG.OVRKR +  KOSTREG.PERSKOST.          
      END.      
      ELSE IF KOSTREG.BOKKONTO >= "450000" AND KOSTREG.BOKKONTO <= "459999" THEN DO:
         kosttemp2.MTRL = KOSTREG.MASKKOST + KOSTREG.MTRL + KOSTREG.OVRKR +  KOSTREG.PERSKOST.          
      END.
      ELSE IF KOSTREG.BOKKONTO >= "500000" AND KOSTREG.BOKKONTO <= "999999" THEN DO:
         kosttemp2.OVRKR = KOSTREG.MASKKOST + KOSTREG.MTRL + KOSTREG.OVRKR +  KOSTREG.PERSKOST.          
      END.
      IF KOSTREG.BOKKONTO = "681000" THEN DO:      
         DELETE kosttemp2.
      END.
      
      ELSE IF KOSTREG.BOKKONTO = "682000" THEN DO:      
         DELETE kosttemp2.
      END.
      ELSE IF KOSTREG.BOKKONTO = "683000" THEN DO:      
         DELETE kosttemp2.
      END.
      ELSE IF KOSTREG.BOKKONTO = "369999" THEN DO:      
         DELETE kosttemp2.
      END.
      GET NEXT kq NO-LOCK.      
   END.
END PROCEDURE.
PROCEDURE kontokoll_UI:
   DEFINE INPUT PARAMETER varkont LIKE KOSTREG.BOKKONTO NO-UNDO.
   DEFINE OUTPUT PARAMETER varvart AS CHARACTER NO-UNDO.
   RUN SUPER (INPUT varkont,OUTPUT varvart).
END PROCEDURE.


PROCEDURE skapadagdag_UI :
   GET FIRST stq NO-LOCK.
   DO WHILE AVAILABLE(SUMTIDDAG):  
      IF SUMTIDDAG.PERSMASK = FALSE THEN musz = musz.
      ELSE IF SUMTIDDAG.PRISTYP = "RESTID..." THEN DO: 
      END.
      ELSE DO:
         CREATE dagtemp.
         ASSIGN          
         dagtemp.AONR = SUMTIDDAG.AONR
         dagtemp.DELNR = valdaao.DELNR 
         dagtemp.ORT = SUMTIDDAG.ORT
         dagtemp.PERSMASK = SUMTIDDAG.PERSMASK
         dagtemp.LONKOST = SUMTIDDAG.LONKOST
         dagtemp.BELOPP = SUMTIDDAG.BELOPP + SUMTIDDAG.OBELOPP.
         dagtemp.TIMMAR = SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.         
      END.
      GET NEXT stq NO-LOCK.      
   END.
END PROCEDURE.
PROCEDURE summa_UI.
   /*PERSONER*/
   FOR EACH dagtemp WHERE dagtemp.PERSMASK = TRUE NO-LOCK 
   BREAK BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK: 
      ACCUMULATE dagtemp.BELOPP (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.TIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.LONKOST (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      IF LAST-OF(dagtemp.PERSMASK) THEN DO:
         CREATE slutsum.
         ASSIGN 
         slutsum.AONR = dagtemp.AONR
         slutsum.DELNR = dagtemp.DELNR
         slutsum.ORT = dagtemp.ORT
         slutsum.BELOPP = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.BELOPP)                      
         slutsum.TIMMAR = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.TIMMAR) 
         slutsum.LONKOST = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.LONKOST).         
      END.     
   END.
   FOR EACH kosttemp2 BREAK BY kosttemp2.AONR BY kosttemp2.DELNR:     
      ACCUMULATE kosttemp2.GRAVARE (TOTAL BY kosttemp2.AONR BY kosttemp2.DELNR). 
      ACCUMULATE kosttemp2.MASKOVRIG (TOTAL BY kosttemp2.AONR BY kosttemp2.DELNR). 
      ACCUMULATE kosttemp2.TRAKTKOST (TOTAL BY kosttemp2.AONR BY kosttemp2.DELNR). 
      ACCUMULATE kosttemp2.MTRL (TOTAL BY kosttemp2.AONR BY kosttemp2.DELNR).  
      ACCUMULATE kosttemp2.OVRKR (TOTAL BY kosttemp2.AONR BY kosttemp2.DELNR).  
      ACCUMULATE kosttemp2.INKOMST (TOTAL BY kosttemp2.AONR BY kosttemp2.DELNR). 
      IF LAST-OF(kosttemp2.DELNR) THEN DO:
         CREATE kosttemp.
         ASSIGN 
         kosttemp.AONR = kosttemp2.AONR
         kosttemp.DELNR = kosttemp2.DELNR 
         kosttemp.MASKOVRIG = (ACCUM TOTAL BY kosttemp2.DELNR kosttemp2.MASKOVRIG) 
         kosttemp.GRAVARE = (ACCUM TOTAL BY kosttemp2.DELNR kosttemp2.GRAVARE) 
         kosttemp.TBELOPP = (ACCUM TOTAL BY kosttemp2.DELNR kosttemp2.TRAKTKOST)
         kosttemp.MTRL = (ACCUM TOTAL BY kosttemp2.DELNR kosttemp2.MTRL)    
         kosttemp.OVRKR = (ACCUM TOTAL BY kosttemp2.DELNR kosttemp2.OVRKR)
         kosttemp.INKOMST = (ACCUM TOTAL BY kosttemp2.DELNR kosttemp2.INKOMST).                          
      END.       
   END. 
   FOR EACH kosttemp2 WHERE kosttemp2.INKOMST NE 0 
      BREAK BY kosttemp2.AONR BY kosttemp2.DELNR BY kosttemp2.BOKKONTO:     
      ACCUMULATE kosttemp2.INKOMST (TOTAL BY kosttemp2.AONR BY kosttemp2.DELNR BY kosttemp2.BOKKONTO). 
      IF LAST-OF(kosttemp2.BOKKONTO) THEN DO:
         CREATE inkomsttemp.
         ASSIGN 
         inkomsttemp.AONR = kosttemp2.AONR
         inkomsttemp.DELNR = kosttemp2.DELNR 
         inkomsttemp.BOKKONTO = kosttemp2.BOKKONTO
         inkomsttemp.INKOMST = (ACCUM TOTAL BY kosttemp2.BOKKONTO kosttemp2.INKOMST).                  
      END.       
   END.
   
END PROCEDURE.

PROCEDURE sidfot_UI.    
   ASSIGN
   tempupp.ototea = tempupp.ototea + tempupp.totea
   tempupp.ototarb = tempupp.ototarb + tempupp.totarb
   tempupp.ototmask = tempupp.ototmask + tempupp.totmask
   tempupp.ototmtrl = tempupp.ototmtrl + tempupp.totmtrl
   tempupp.ototovr = tempupp.ototovr + tempupp.totovr
   tempupp.ototalt = tempupp.ototalt + tempupp.totalt
   tempupp.oarbtim = tempupp.oarbtim + tempupp.arbtim
   tempupp.obertim = tempupp.obertim + tempupp.bertim
   tempupp.omsktim = tempupp.omsktim + tempupp.msktim
   tempupp.ototutr = tempupp.ototutr + tempupp.totutr
   tempupp.ototutrtim = tempupp.ototutrtim + tempupp.totutrtim.            
END PROCEDURE.

PROCEDURE procset_UI:  
  IF NOT VALID-HANDLE(edataapph) THEN RUN EXTRADATAHMT.P PERSISTENT SET edataapph.            
END PROCEDURE .

