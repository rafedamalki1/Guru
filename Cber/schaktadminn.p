/*
               KSV Editor
    Copyright: (C) 2000-2003 Serguey Klimoff (bulkl0DD)
     Filename: SCHAKTADMINN.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 2008.07.01 15:06 ELPAO   
     Modified: 2009.10.22 19:35 ELPAO    
     Modified: 2009.10.26 16:49 ELPAO    
     Modified: 2010.02.25 13:40 ELPAO    
     Modified:  
*/

&Scoped-define NEW
{SCHAKTWID.I}
{ALLDEF.I}
&Scoped-define SHARED SHARED
{SCADMIN.I}
{GLOBVAR2DEL1.I}
/*KALK*/
{PTEMP.I}
{LOPTEMP.I}
{FASTKALKTEMP.I}
&Scoped-define SHARED
&Scoped-define NEW
{EXTRADATA.I}
DEFINE VARIABLE KalkClasserStart AS HANDLE NO-UNDO.  
DEFINE VARIABLE schakadmapph AS HANDLE NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
DEFINE VARIABLE brwsortvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempkonsth AS HANDLE NO-UNDO.
DEFINE VARIABLE tempkabelh AS HANDLE NO-UNDO.
DEFINE VARIABLE tempyth AS HANDLE NO-UNDO.
DEFINE VARIABLE tempsamh AS HANDLE NO-UNDO.
DEFINE VARIABLE tempforh AS HANDLE NO-UNDO.
DEFINE VARIABLE temphandkoph AS HANDLE NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE VARIABLE ytvarde AS CHARACTER NO-UNDO.  
DEFINE VARIABLE samkod AS CHARACTER NO-UNDO.
DEFINE VARIABLE samkodold AS CHARACTER NO-UNDO.
DEFINE VARIABLE forkod AS CHARACTER NO-UNDO.
DEFINE VARIABLE forkodold AS CHARACTER NO-UNDO.
DEFINE VARIABLE kabkod AS INTEGER NO-UNDO.
DEFINE VARIABLE typ_val AS INTEGER NO-UNDO.
DEFINE VARIABLE btnnovit AS HANDLE NO-UNDO.
DEFINE VARIABLE exdatahmth AS HANDLE NO-UNDO.
DEFINE VARIABLE fbestapph AS HANDLE NO-UNDO.
DEFINE VARIABLE htyp AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE dokalkproch AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE NEW SHARED VARIABLE entrykabdia AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE forid AS INTEGER NO-UNDO.  
framesizeh = framesizeextrah.
ON 'CHOOSE' OF BTN_HANDEL PERSISTENT RUN btnhandel_UI IN THIS-PROCEDURE.
ON 'CHOOSE' OF BTN_KKOPP PERSISTENT RUN btnkkopp_UI IN THIS-PROCEDURE.
ON 'CHOOSE' OF BTN_FORLSATT PERSISTENT RUN btnfor_UI IN THIS-PROCEDURE.
ON 'CHOOSE' OF BTN_YT PERSISTENT RUN btnyt_UI IN THIS-PROCEDURE.
ON 'CHOOSE' OF BTN_SAM PERSISTENT RUN btnsam_UI IN THIS-PROCEDURE.
ON 'CHOOSE' OF BTN_KAB PERSISTENT RUN btnkab_UI IN THIS-PROCEDURE.
ON 'CHOOSE' OF AddKabelButton PERSISTENT RUN btnaddkab_UI IN THIS-PROCEDURE.
ON 'CHOOSE' OF BTN_NY PERSISTENT RUN btnny_UI IN THIS-PROCEDURE.
ON 'CHOOSE' OF BTN_BORT PERSISTENT RUN btnbort_UI IN THIS-PROCEDURE.
ON 'CHOOSE' OF BTN_AND PERSISTENT RUN btnand_UI IN THIS-PROCEDURE.
ON 'CHOOSE' OF BTN_BORTKAB PERSISTENT RUN btnbortkab_UI IN THIS-PROCEDURE.
ON 'CHOOSE' OF BTN_AVSLUTA PERSISTENT RUN btnavsluta_UI IN THIS-PROCEDURE.
ON 'CHOOSE' OF BTN_VALKOD PERSISTENT RUN valkod_UI IN THIS-PROCEDURE.
ON 'CHOOSE' OF BTN_OVALKOD PERSISTENT RUN ovalkod_UI IN THIS-PROCEDURE.
ON 'CHOOSE' OF BTN_KOPPLA PERSISTENT RUN koppla_UI IN THIS-PROCEDURE.
ON 'CHOOSE' OF BTN_OKOPPLA PERSISTENT RUN okoppla_UI IN THIS-PROCEDURE.
ON 'CHOOSE' OF BTN_MORDUPP PERSISTENT RUN btnupp_UI IN THIS-PROCEDURE.
ON 'CHOOSE' OF BTN_MORDNER PERSISTENT RUN btnner_UI IN THIS-PROCEDURE.        
ON 'ENTRY' OF listkabdia PERSISTENT RUN entrylma_UI IN THIS-PROCEDURE.
ON 'LEAVE' OF listkabdia PERSISTENT RUN leavekabel_UI IN THIS-PROCEDURE.
ON 'MOUSE-SELECT-CLICK' OF listkabdia PERSISTENT RUN mlma_UI IN THIS-PROCEDURE.
ON 'VALUE-CHANGED' OF CMB_VAL2 PERSISTENT RUN cmbval2_UI IN THIS-PROCEDURE.
ON 'VALUE-CHANGED' OF BRW_KON PERSISTENT RUN brwkon_UI IN THIS-PROCEDURE. 
ON 'VALUE-CHANGED' OF RAD_TYP PERSISTENT RUN changearb_UI IN THIS-PROCEDURE.
ON 'VALUE-CHANGED' OF RAD_HTYP PERSISTENT RUN sorteraHYF_UI IN THIS-PROCEDURE.
ON 'VALUE-CHANGED' OF BRW_P PERSISTENT RUN brwp_UI IN THIS-PROCEDURE. 
ON 'VALUE-CHANGED' OF BRW_LOP PERSISTENT RUN brwl_UI IN THIS-PROCEDURE. 
ON 'VALUE-CHANGED' OF BRW_HAND PERSISTENT RUN brwhand_UI IN THIS-PROCEDURE. 
ON 'VALUE-CHANGED' OF BRW_VALKOD PERSISTENT RUN brwval_UI IN THIS-PROCEDURE. 
ON 'VALUE-CHANGED' OF BRW_KKOPP PERSISTENT RUN brwkkopp_UI IN THIS-PROCEDURE.             
ON 'ROW-LEAVE' OF BRW_KKOPP PERSISTENT RUN rowlkopp_UI IN THIS-PROCEDURE.             
ON 'ENTRY' OF kkantal PERSISTENT RUN entrykkantal_UI IN THIS-PROCEDURE.
ON 'LEAVE' OF kkantal PERSISTENT RUN leavekkantal_UI IN THIS-PROCEDURE.
ON ANY-KEY OF FILL-IN-VKOD PERSISTENT RUN fillkod_UI IN THIS-PROCEDURE.
ON ANY-KEY OF FILL-IN-VLOP PERSISTENT RUN fillop_UI IN THIS-PROCEDURE.
RUN main_UI.

PROCEDURE rowlkopp_UI :
   RUN sparakopp_UI IN schakadmapph (INPUT TABLE tempkkopp).
END PROCEDURE.
PROCEDURE leavekkantal_UI :   
   IF AVAILABLE tempkkopp THEN DO:
      RUN setcellvalue_UI IN brwproc[11] (INPUT kkantal:NAME,INPUT kkantal:SCREEN-VALUE).      
   END.
END PROCEDURE.
PROCEDURE entrykkantal_UI :
   DEFINE VARIABLE brwrowid AS ROWID NO-UNDO.
   RUN selectrowid_UI IN brwproc[11] (OUTPUT brwrowid).
   FIND FIRST tempkkopp WHERE ROWID(tempkkopp) = brwrowid NO-LOCK NO-ERROR.
END PROCEDURE.
PROCEDURE main_UI :
   {ALLSTARTDYN.I}
   IF Guru.Konstanter:appcon THEN DO:
      RUN DOKALKAPP.P PERSISTENT SET dokalkproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
   END.
   ELSE DO:      
      RUN DOKALKAPP.P PERSISTENT SET dokalkproch.      
   END.                         
   FIND FIRST konstgrptemp WHERE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE konstgrptemp THEN DO:
      tthandle = TEMP-TABLE konstgrptemp:HANDLE.
      IF Guru.Konstanter:appcon THEN DO:
         RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
            (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "KONSTGRUPP", INPUT "").
      END.
      ELSE DO:
         RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
            (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "KONSTGRUPP", INPUT "").
      END.
      CMB_VAL2:LIST-ITEMS = "".
      FOR EACH konstgrptemp USE-INDEX ORD NO-LOCK:
         status-ok = CMB_VAL2:ADD-LAST(konstgrptemp.BENAMNING). 
         IF konstgrptemp.KONSKOD = kongrkod THEN CMB_VAL2:SCREEN-VALUE = konstgrptemp.BENAMNING.
      END.
   END.
   tthandle = TEMP-TABLE tempkonst:HANDLE.   
   tthandle = TEMP-TABLE tempytbelagg:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "YTBELAGG", INPUT "").
   tthandle = TEMP-TABLE tempforlagg:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "FORLAGG", INPUT "").
   tthandle = TEMP-TABLE temphandelse:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "HDHANDELSE", INPUT "").   
   RUN laddaHYF_UI.
   /*fixa radiobuttons oc sätt default till TYP 2*/
   RAD_TYP:DELETE("").
   RAD_TYP:ADD-LAST("Typ 2",2).
   RAD_TYP:ADD-LAST("Typ 3",3).
   RAD_TYP:SCREEN-VALUE = "2".
   RUN addmenuitem_UI IN brwproc[8] (INPUT BRW_LOP,INPUT "Kommentarer",INPUT "kommentar_UI").
   RUN addmenuitem_UI IN brwproc[8] (INPUT BRW_LOP,INPUT "Frekvens",INPUT "frekvens_UI").
   /*fylla i när typ ändras*/
   RUN changearb_UI.
   /* EXTRA DATA */
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
   END.
   RUN extrakabel_UI.      
   RUN cmbval_UI. /* så det inte fylls med skräp*/
   RUN ovrigatabeller_UI.
   RUN btnkab_UI.
   APPLY "VALUE-CHANGED" TO RAD_HTYP.   
   APPLY "VALUE-CHANGED" TO RAD_TYP.
   FILL-IN-VLOP:SCREEN-VALUE = ?.
   RUN addfillin_UI IN brwproc[7] (INPUT FILL-IN-BEN:HANDLE, INPUT "BENAMNING").
   /*den väljer förläggningssätt annars... */
   RUN btnsam_UI.
   APPLY "CHOOSE" TO BTN_FORLSATT.   
END PROCEDURE.



PROCEDURE fillkod_UI :
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   RUN ngnkey_UI.
   
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "ENTRY" TO FILL-IN-VKOD.
       RETURN NO-APPLY.
   END.
END PROCEDURE.

PROCEDURE fillop_UI :
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   RUN ngnkey_UI.
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "ENTRY" TO FILL-IN-VKOD.
      RETURN NO-APPLY.
   END.
END PROCEDURE.

PROCEDURE ngnkey_UI :
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      FIND FIRST loptemp WHERE loptemp.TYP = typ_val AND TRIM(loptemp.ARBKOD) = TRIM(STRING(FILL-IN-VKOD:SCREEN-VALUE)) AND loptemp.LOPNR = INTEGER(FILL-IN-VLOP:SCREEN-VALUE) NO-LOCK NO-ERROR.         
      IF AVAILABLE loptemp THEN DO:
         FIND FIRST fastkalktemp WHERE fastkalktemp.ARBKOD = STRING(FILL-IN-VKOD) AND fastkalktemp.LOPNR = INTEGER(FILL-IN-VLOP) NO-ERROR.
         IF NOT AVAILABLE fastkalktemp THEN DO:
            DO TRANSACTION:
               CREATE fastkalktemp.
               ASSIGN
               fastkalktemp.ARBKOD = loptemp.ARBKOD
               fastkalktemp.LOPNR = INTEGER(loptemp.LOPNR)
               fastkalktemp.BENAMNING = loptemp.BENAMNING.
            END.         
            RUN setlastrowid_UI IN brwproc[9] (INPUT ROWID(fastkalktemp)).        
            RUN openbdynspec_UI IN brwproc[9].
            RUN lastselectdyn_UI IN brwproc[9]. 
         END.
      END.
      ELSE DO:
        MESSAGE "Arbetskod eller löpnr saknas." VIEW-AS ALERT-BOX TITLE "Meddelande".
        RETURN.
      END.
   END.
END PROCEDURE.


PROCEDURE frekvens_UI :
   DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
   antal_raknare = 1.
   DO WHILE antal_raknare LE BRW_LOP:NUM-SELECTED-ROWS :
      BRW_LOP:FETCH-SELECTED-ROW(antal_raknare).
      EMPTY TEMP-TABLE extraloptemp NO-ERROR. 
      CREATE extraloptemp.
      BUFFER-COPY loptemp TO extraloptemp.      
      RUN KALKFREK2.W.      
      antal_raknare = antal_raknare + 1.
   END.   
END PROCEDURE.

PROCEDURE kommentar_UI :
   DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.  
   antal_raknare = 1.
   DO WHILE antal_raknare LE BRW_LOP:NUM-SELECTED-ROWS :
      BRW_LOP:FETCH-SELECTED-ROW(antal_raknare).
      EMPTY TEMP-TABLE extraloptemp NO-ERROR. 
      CREATE extraloptemp.
      BUFFER-COPY loptemp TO extraloptemp.      
      RUN KALKKOM2.W.      
      antal_raknare = antal_raknare + 1.
   END.
END PROCEDURE.


PROCEDURE changearb_UI :
   FIND FIRST ptemp WHERE ptemp.TYP = INTEGER(RAD_TYP:SCREEN-VALUE) NO-LOCK NO-ERROR.   
   IF NOT AVAILABLE ptemp THEN DO:
      RUN Ptemp_UI IN KalkClasserStart (INPUT INTEGER(RAD_TYP:SCREEN-VALUE), OUTPUT TABLE ptemp).         
   END.
   FIND FIRST loptemp WHERE loptemp.TYP = INTEGER(RAD_TYP:SCREEN-VALUE) NO-LOCK NO-ERROR.
   IF NOT AVAILABLE loptemp THEN DO:
      RUN Loptempn_UI IN KalkClasserStart (INPUT INTEGER(RAD_TYP:SCREEN-VALUE), OUTPUT TABLE loptemp).      
   END.
   RUN openbdynspec_UI IN brwproc[7].
   RUN openbdynspec_UI IN brwproc[8].
   APPLY "VALUE-CHANGED" TO BRW_P.
   APPLY "VALUE-CHANGED" TO BRW_LOP.
   typ_val = INTEGER(RAD_TYP:SCREEN-VALUE).
END PROCEDURE.

/*hmm*/
PROCEDURE ovrigatabeller_UI :
   FIND FIRST tempytbelagg USE-INDEX ORDNING NO-LOCK NO-ERROR.
   IF AVAILABLE tempytbelagg THEN DO:
         RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(tempytbelagg)).
         RUN lastselectdyn_UI IN brwproc[4].
      END.
   APPLY "VALUE-CHANGED" TO BRW_YT.
   RUN openbdynspec_UI IN brwproc[4].
   RUN openbdynspec_UI IN brwproc[3].
   RUN openbdynspec_UI IN brwproc[2].
   RUN openbdynspec_UI IN brwproc[12].   
END PROCEDURE.

PROCEDURE extrakabel_UI :
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
   EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "KABLAR"                   
   inextradatatemp.HUVUDCH = ?.
   inextradatatemp.HUVUDINT = ?.
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp,OUTPUT TABLE extradatatemp).   
   EMPTY TEMP-TABLE tempkabel NO-ERROR. 
   FOR EACH extradatatemp:
      CREATE tempkabel.
      ASSIGN 
      tempkabel.KABEL = extradatatemp.HUVUDCH.
      tempkabel.DIAMETER = extradatatemp.SOKINT[1].
      tempkabel.KABID = extradatatemp.HUVUDINT.
      tempkabel.BORTTAGEN = extradatatemp.SOKLOG[1].
   END.
   EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
   RUN openbdynspec_UI IN brwproc[6].   
END PROCEDURE.

PROCEDURE allstartbrw_UI :
   RUN DYNBRW.P PERSISTENT SET brwproc[2] (INPUT BRW_FOR).
   RUN DYNBRW.P PERSISTENT SET brwproc[3] (INPUT BRW_SAM).
   RUN DYNBRW.P PERSISTENT SET brwproc[4] (INPUT BRW_YT).
   RUN DYNBRW.P PERSISTENT SET brwproc[5] (INPUT BRW_KON).  
   RUN DYNBRW.P PERSISTENT SET brwproc[6] (INPUT BRW_KAB).
   RUN DYNBRW.P PERSISTENT SET brwproc[7] (INPUT BRW_P).
   RUN DYNBRW.P PERSISTENT SET brwproc[8] (INPUT BRW_LOP).   
   RUN DYNBRW.P PERSISTENT SET brwproc[9] (INPUT BRW_VALKOD).
   RUN DYNBRW.P PERSISTENT SET brwproc[10] (INPUT BRW_HAND).
   RUN DYNBRW.P PERSISTENT SET brwproc[11] (INPUT BRW_KKOPP).
   RUN DYNBRW.P PERSISTENT SET brwproc[12] (INPUT BRW_HANDELSE).
   RUN setcolindex_UI IN brwproc[2] (INPUT "ORDNING").
   RUN setcolindex_UI IN brwproc[4] (INPUT "ORDNING").
   RUN setcolindex_UI IN brwproc[10] (INPUT "ORDNING").
   RUN setcolindex_UI IN brwproc[12] (INPUT "ORDNING").
   brwsortvar = " WHERE temphandelse.BORT = FALSE".      
   RUN setcolsortvar_UI IN brwproc[12] (INPUT brwsortvar).
   RUN setdefaultcolbyname_UI IN brwproc[5] (INPUT "ORDNING").
   BTN_KAB:HIDDEN = TRUE.
   BTN_SAM:HIDDEN = TRUE.       
   Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = BTN_FORLSATT.
   RUN buttcolm_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).  
   Guru.GlobalaVariabler:colrighth = BTN_YT.
   RUN buttcolm_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).  
   Guru.GlobalaVariabler:colrighth = BTN_HANDEL.
   RUN buttcolm_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = BTN_KKOPP.
   RUN buttcolm_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).  
   BTN_KKOPP:LOAD-IMAGE("BILDER\xbtn_kkopp.gif") NO-ERROR.
   BTN_FORLSATT:LOAD-IMAGE("BILDER\xbtn_forlsatt.gif") NO-ERROR.
   BTN_YT:LOAD-IMAGE("BILDER\xbtn_ytbelagg.gif") NO-ERROR.
   BTN_SAM:LOAD-IMAGE("BILDER\xbtn_samforlagg.gif") NO-ERROR.
   BTN_KAB:LOAD-IMAGE("BILDER\xbtn_kablar.gif") NO-ERROR.
   BTN_HANDEL:LOAD-IMAGE("BILDER\xbtn_handel.gif") NO-ERROR.       
   BTN_VALKOD:LOAD-IMAGE("BILDER\xbtn_over.gif") NO-ERROR.
   BTN_OVALKOD:LOAD-IMAGE("BILDER\xbtn_back.gif") NO-ERROR.
   BTN_KOPPLA:LOAD-IMAGE("BILDER\xbtn_alldown.gif") NO-ERROR.
   BTN_OKOPPLA:LOAD-IMAGE("BILDER\xbtn_uparrow.gif") NO-ERROR.
   brwsortvar = " WHERE tempkabel.BORTTAGEN = FALSE ".      
   RUN setcolsortvar_UI IN brwproc[6] (INPUT brwsortvar).
   RUN setcolindex_UI IN brwproc[6] (INPUT "KABID").
   IF NOT VALID-HANDLE(KalkClasserStart) THEN RUN Modules\Kalkyl\KalkClasserStart.P PERSISTENT SET KalkClasserStart.
   IF Guru.Konstanter:appcon THEN DO:
      RUN SCHAKADMAPP.P PERSISTENT SET schakadmapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN SCHAKADMAPP.P PERSISTENT SET schakadmapph.
   END.   
END PROCEDURE.

PROCEDURE btnupp_UI :
   DEFINE VARIABLE frannr AS INTEGER NO-UNDO.
   DEFINE VARIABLE tillnr AS INTEGER NO-UNDO.
   DEFINE VARIABLE bytid AS INTEGER NO-UNDO.
   DEFINE VARIABLE till_rowid  AS ROWID NO-UNDO.
   IF BRW_YT:HIDDEN = FALSE THEN DO:
      RUN selectfocyt_UI.
      IF AVAILABLE tempytbelagg THEN DO:
         IF tempytbelagg.ORDNING = 1 THEN DO:
            MESSAGE "Du är på den första posten!" VIEW-AS ALERT-BOX.
            RETURN.
         END.
         frannr = tempytbelagg.ORDNING.
         till_rowid = ROWID(tempytbelagg).
         RUN pselectfoc_UI (INPUT 4).
         IF status-ok = TRUE THEN DO:
            ASSIGN
            bytid  = tempytbelagg.ID
            tillnr = tempytbelagg.ORDNING
            tempytbelagg.ORDNING = frannr.   
            FIND tempytbelagg WHERE ROWID(tempytbelagg) = till_rowid NO-LOCK NO-ERROR.
            tempytbelagg.ORDNING = tillnr.
            RUN orduppdat_UI IN schakadmapph (INPUT 1,INPUT tempytbelagg.ID,INPUT tempytbelagg.ORDNING,INPUT bytid,INPUT frannr).
            IF AVAILABLE tempytbelagg THEN DO:
               RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(tempytbelagg)).
               RUN openbdynspec_UI IN brwproc[4].
               RUN lastselectdyn_UI IN brwproc[4].
            END.
         END.   
      END.
   END.
   ELSE IF BRW_FOR:HIDDEN = FALSE THEN DO:
      RUN selectfocfor_UI.
      IF AVAILABLE tempforlagg THEN DO:
         IF tempforlagg.ORDNING = 1 THEN DO:
            MESSAGE "Du är på den första posten!" VIEW-AS ALERT-BOX.
            RETURN.
         END.
         frannr = tempforlagg.ORDNING.
         till_rowid = ROWID(tempforlagg).
         RUN pselectfoc_UI (INPUT 2).
         IF status-ok = TRUE THEN DO:
            ASSIGN
            bytid  = tempforlagg.ID
            tillnr = tempforlagg.ORDNING
            tempforlagg.ORDNING = frannr.   
            FIND tempforlagg WHERE ROWID(tempforlagg) = till_rowid NO-LOCK NO-ERROR.
            tempforlagg.ORDNING = tillnr.
            RUN orduppdat_UI IN schakadmapph (INPUT 2,INPUT tempforlagg.ID,INPUT tempforlagg.ORDNING,INPUT bytid,INPUT frannr).
            IF AVAILABLE tempforlagg THEN DO:
               RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(tempforlagg)).
               RUN openbdynspec_UI IN brwproc[2].
               RUN lastselectdyn_UI IN brwproc[2].
            END.
         END.          
      END.
   END.
   ELSE IF BRW_HANDELSE:HIDDEN = FALSE THEN DO:
      RUN selectfochand_UI.
      IF AVAILABLE temphandelse THEN DO:
         IF temphandelse.ORDNING = 1 THEN DO:
            MESSAGE "Du är på den första posten!" VIEW-AS ALERT-BOX.
            RETURN.
         END.
         frannr = temphandelse.ORDNING.
         till_rowid = ROWID(temphandelse).
         RUN pselectfoc_UI (INPUT 12).
         IF status-ok = TRUE THEN DO:
            ASSIGN
            bytid  = temphandelse.ID
            tillnr = temphandelse.ORDNING
            temphandelse.ORDNING = frannr.   
            FIND temphandelse WHERE ROWID(temphandelse) = till_rowid NO-LOCK NO-ERROR.
            temphandelse.ORDNING = tillnr.
            RUN orduppdat_UI IN schakadmapph (INPUT 3,INPUT temphandelse.ID,INPUT temphandelse.ORDNING,INPUT bytid,INPUT frannr).
            IF AVAILABLE temphandelse THEN DO:
               RUN setlastrowid_UI IN brwproc[12] (INPUT ROWID(temphandelse)).
               RUN openbdynspec_UI IN brwproc[12].
               RUN lastselectdyn_UI IN brwproc[12].
            END.
         END.          
      END.
   END.
END PROCEDURE.
PROCEDURE btnner_UI :
   DEFINE VARIABLE frannr AS INTEGER NO-UNDO.
   DEFINE VARIABLE tillnr AS INTEGER NO-UNDO.
   DEFINE VARIABLE bytid AS INTEGER NO-UNDO.
   DEFINE VARIABLE till_rowid  AS ROWID NO-UNDO.
   IF BRW_YT:HIDDEN = FALSE THEN DO:
      RUN selectfocyt_UI.
      IF AVAILABLE tempytbelagg THEN DO:
         frannr = tempytbelagg.ORDNING.
         till_rowid = ROWID(tempytbelagg).
         RUN nselectfoc_UI (INPUT 4).
         IF status-ok = TRUE THEN DO:
            ASSIGN
            bytid  = tempytbelagg.ID
            tillnr = tempytbelagg.ORDNING
            tempytbelagg.ORDNING = frannr.   
            FIND tempytbelagg WHERE ROWID(tempytbelagg) = till_rowid NO-LOCK NO-ERROR.
            tempytbelagg.ORDNING = tillnr.
            RUN orduppdat_UI IN schakadmapph (INPUT 1,INPUT tempytbelagg.ID,INPUT tempytbelagg.ORDNING,INPUT bytid,INPUT frannr).
            IF AVAILABLE tempytbelagg THEN DO:
               RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(tempytbelagg)).
               RUN openbdynspec_UI IN brwproc[4].
               RUN lastselectdyn_UI IN brwproc[4].
            END.
         END.   
         ELSE DO:
            MESSAGE "Du är på den sista posten!" VIEW-AS ALERT-BOX.
            RETURN.            
         END.
      END.
   END.
   ELSE IF BRW_FOR:HIDDEN = FALSE THEN DO:
      RUN selectfocfor_UI.
      IF AVAILABLE tempforlagg THEN DO:
         frannr = tempforlagg.ORDNING.
         till_rowid = ROWID(tempforlagg).
         RUN nselectfoc_UI (INPUT 2).
         IF status-ok = TRUE THEN DO:
            ASSIGN
            bytid  = tempforlagg.ID
            tillnr = tempforlagg.ORDNING
            tempforlagg.ORDNING = frannr.   
            FIND tempforlagg WHERE ROWID(tempforlagg) = till_rowid NO-LOCK NO-ERROR.
            tempforlagg.ORDNING = tillnr.
            RUN orduppdat_UI IN schakadmapph (INPUT 2,INPUT tempforlagg.ID,INPUT tempforlagg.ORDNING,INPUT bytid,INPUT frannr).
            IF AVAILABLE tempforlagg THEN DO:
               RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(tempforlagg)).
               RUN openbdynspec_UI IN brwproc[2].
               RUN lastselectdyn_UI IN brwproc[2].
            END.
         END.          
         ELSE DO:
            MESSAGE "Du är på den sista posten!" VIEW-AS ALERT-BOX.
            RETURN.            
         END.
      END.
   END.
   ELSE IF BRW_HANDELSE:HIDDEN = FALSE THEN DO:
      RUN selectfochand_UI.
      IF AVAILABLE temphandelse THEN DO:
         frannr = temphandelse.ORDNING.
         till_rowid = ROWID(temphandelse).
         RUN nselectfoc_UI (INPUT 12).
         IF status-ok = TRUE THEN DO:
            ASSIGN
            bytid  = temphandelse.ID
            tillnr = temphandelse.ORDNING
            temphandelse.ORDNING = frannr.   
            FIND temphandelse WHERE ROWID(temphandelse) = till_rowid NO-LOCK NO-ERROR.
            temphandelse.ORDNING = tillnr.
            RUN orduppdat_UI IN schakadmapph (INPUT 3,INPUT temphandelse.ID,INPUT temphandelse.ORDNING,INPUT bytid,INPUT frannr).
            IF AVAILABLE temphandelse THEN DO:
               RUN setlastrowid_UI IN brwproc[12] (INPUT ROWID(temphandelse)).
               RUN openbdynspec_UI IN brwproc[12].
               RUN lastselectdyn_UI IN brwproc[12].
            END.
         END.          
         ELSE DO:
            MESSAGE "Du är på den sista posten!" VIEW-AS ALERT-BOX.
            RETURN.            
         END.
      END.
   END.
END PROCEDURE.
PROCEDURE btnny_UI :
   RUN ny_UI.
END PROCEDURE.

PROCEDURE btnbort_UI :
   RUN bort_UI.
END PROCEDURE.

PROCEDURE btnand_UI :
   RUN andr_UI.
END PROCEDURE.


PROCEDURE btnbortkab_UI :
   RUN kabelbort_UI.
END PROCEDURE.


PROCEDURE btnavsluta_UI :
   RUN avsluta_UI IN huvprogh.
END PROCEDURE.

PROCEDURE btnkkopp_UI :
   RUN btnvit_UI (INPUT BTN_KKOPP).
   RUN kkopp_UI.
END PROCEDURE.


PROCEDURE btnhandel_UI :
   RUN btnvit_UI (INPUT BTN_HANDEL).
   RUN handel_UI.
END PROCEDURE.

PROCEDURE btnfor_UI :
   RUN btnvit_UI (INPUT BTN_FORLSATT).
   RUN forl_UI.
END PROCEDURE.

PROCEDURE btnyt_UI :
   RUN btnvit_UI (INPUT BTN_YT).
   RUN yt_UI.
END PROCEDURE.

PROCEDURE btnsam_UI :   
   RUN btnvit_UI (INPUT BTN_SAM).
   RUN sam_UI.
END PROCEDURE.

PROCEDURE btnkab_UI :   
   RUN btnvit_UI (INPUT BTN_KAB).
   RUN kabel_UI.
   FRAME-KABLAR:HIDDEN = FALSE.
   FRAME-KKOPP:HIDDEN = TRUE.
   FRAME-FORLSATT:HIDDEN = TRUE.
END PROCEDURE.

PROCEDURE btnaddkab_UI :   
   RUN addkabel_UI.
   FRAME-KABLAR:HIDDEN = FALSE.
END PROCEDURE.

PROCEDURE cmbval2_UI :   
   {muswait.i} 
   RUN cmbval_UI.   
   {musarrow.i}     
END PROCEDURE.

PROCEDURE btnvit_UI :
   DEFINE INPUT PARAMETER btnh AS HANDLE NO-UNDO.
   DEFINE VARIABLE btnlabel AS CHARACTER NO-UNDO.
   IF btnnovit NE ? THEN DO:
      btnlabel = btnnovit:IMAGE.
      btnlabel = REPLACE(btnlabel,"_vit.gif",".gif"). 
      btnnovit:LOAD-IMAGE (btnlabel) NO-ERROR.
   END.
   btnlabel = btnh:IMAGE.
   btnlabel = REPLACE(btnlabel,".gif","_vit.gif"). 
   btnh:LOAD-IMAGE (btnlabel) NO-ERROR.
   btnnovit = btnh.   
END PROCEDURE.

/* Ändra knappen */
PROCEDURE andr_UI :
   DEFINE VARIABLE idvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE benvar AS CHARACTER NO-UNDO.
   IF BRW_YT:HIDDEN = FALSE THEN DO:
      RUN selectfocyt_UI.
      IF AVAILABLE tempytbelagg THEN DO:
         ASSIGN
         idvar = tempytbelagg.ID.         
         RUN NYYTU.W (INPUT idvar,OUTPUT TABLE eytbelaggtemp).      
         IF musz = FALSE THEN DO:
            FIND FIRST eytbelaggtemp WHERE NO-LOCK NO-ERROR.
            FIND FIRST tempytbelagg WHERE tempytbelagg.ID = eytbelaggtemp.ID NO-LOCK NO-ERROR.
            BUFFER-COPY eytbelaggtemp EXCEPT ORDNING TO tempytbelagg.
            RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(tempytbelagg)).
            RUN openbdynspec_UI IN brwproc[4]. 
            RUN lastselectdyn_UI IN brwproc[4].         
         END.
         EMPTY TEMP-TABLE eytbelaggtemp NO-ERROR. 
         ASSIGN
         musz = FALSE.         
      END.
      ELSE DO:
         MESSAGE "Det finns ingen ytbeläggning att ändra."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
      END.
   END.
   ELSE IF BRW_FOR:HIDDEN = FALSE THEN DO:
      RUN selectfocfor_UI.
      IF AVAILABLE tempforlagg THEN DO:
         ASSIGN
         idvar = tempforlagg.ID
         forkod = tempforlagg.FORLAGG
         forkodold = forkod.
         RUN FORLANYU.W (INPUT idvar,OUTPUT TABLE eforlaggtemp).
         IF musz = FALSE THEN DO:
            FIND FIRST eforlaggtemp WHERE NO-LOCK NO-ERROR.
            FIND FIRST tempforlagg WHERE tempforlagg.ID = eforlaggtemp.ID NO-LOCK NO-ERROR.
            BUFFER-COPY eforlaggtemp TO tempforlagg.
            RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(tempforlagg)).
            RUN openbdynspec_UI IN brwproc[2]. 
            RUN lastselectdyn_UI IN brwproc[2].
         END.
         EMPTY TEMP-TABLE eforlaggtemp NO-ERROR. 
         ASSIGN
         musz = FALSE.         
      END.
   END.
   ELSE IF BRW_HANDELSE:HIDDEN = FALSE THEN DO:
      RUN selectfochand_UI.      
      IF AVAILABLE temphandelse THEN DO:
         idvar = temphandelse.ID.
         RUN NYHAND.W (INPUT-OUTPUT idvar).
         RUN laddahandelse_UI (INPUT idvar).
         
      END.
   END.   
END PROCEDURE.

/* Ta Bort knappen */
PROCEDURE bort_UI :
   DEFINE VARIABLE idnr AS INTEGER NO-UNDO.
   DEFINE VARIABLE benvar AS CHARACTER NO-UNDO.
   IF BRW_YT:HIDDEN = FALSE THEN DO:
      FIND FIRST tempytbelagg NO-LOCK NO-ERROR.
      IF AVAILABLE tempytbelagg THEN DO:       
         RUN selectfocyt_UI.
         ytvarde = tempytbelagg.YTBELAGG.
         MESSAGE "Vill du ta bort " + tempytbelagg.YTBELAGG + "?"
         VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO TITLE "Ta bort?" UPDATE svar.         
         IF svar THEN DO:  
            RUN bortytbelagg_UI IN schakadmapph (INPUT tempytbelagg.ID). /*RUN bortytbelagg_UI IN schakadmapph (INPUT forid). schakadmapp.p */
            FIND FIRST tempytbelagg WHERE tempytbelagg.YTBELAGG = ytvarde.
            DELETE tempytbelagg.
            RUN selnextprevrow_UI IN brwproc[4].
            RUN openbdynspec_UI IN brwproc[4].            
            RUN lastselectdyn_UI IN brwproc[4].            
         END.   
      END.
      ELSE DO:
         MESSAGE "Det finns ingen ytbeläggning att ta bort."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
      END.
   END.   
   ELSE IF BRW_FOR:HIDDEN = FALSE THEN DO:
      FIND FIRST tempforlagg NO-LOCK NO-ERROR.
      IF AVAILABLE tempforlagg THEN DO:
         RUN selectfocfor_UI.
         forkod  = tempforlagg.FORLAGG.
         MESSAGE "Vill du ta bort " + tempforlagg.FORLAGG + "?"
         VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO TITLE "Ta bort?" UPDATE svar.
         IF svar THEN DO:
            RUN bortforlagg_UI IN schakadmapph (INPUT tempforlagg.ID).
            FIND FIRST tempforlagg WHERE tempforlagg.FORLAGG = forkod.
            DELETE tempforlagg.
            RUN selnextprevrow_UI IN brwproc[2].
            RUN openbdynspec_UI IN brwproc[2].            
            RUN lastselectdyn_UI IN brwproc[2].            
         END.
      END.
      ELSE DO:
         MESSAGE "Det finns inget förläggningssätt att ta bort."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
      END.
   END.     
   ELSE IF BRW_HANDELSE:HIDDEN = FALSE THEN DO:
      RUN selectfochand_UI.
      IF AVAILABLE temphandelse THEN DO:
         MESSAGE "Vill du ta bort " + temphandelse.BENAMNING + "?"
         VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO TITLE "Ta bort?" UPDATE svar.         
         IF svar THEN DO:  
            RUN borthandelse_UI IN schakadmapph (INPUT temphandelse.ID).
            tthandle = TEMP-TABLE temphandelse:HANDLE.
            RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "HDHANDELSE", INPUT "").
            RUN selnextprevrow_UI IN brwproc[12].
            RUN openbdynspec_UI IN brwproc[12].
            RUN lastselectdyn_UI IN brwproc[12].
         END.
      END.      
   END.     
END PROCEDURE.

/* Ny Knappen */
PROCEDURE ny_UI :
   DEFINE VARIABLE idvar AS INTEGER NO-UNDO.
   IF BRW_YT:HIDDEN = FALSE THEN DO:
      RUN NYYTU.W (INPUT 0,OUTPUT TABLE eytbelaggtemp).      
      IF musz = FALSE THEN DO:
         FIND FIRST eytbelaggtemp NO-LOCK NO-ERROR.
         FIND FIRST tempytbelagg WHERE tempytbelagg.ID = eytbelaggtemp.ID NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tempytbelagg THEN CREATE tempytbelagg.
         BUFFER-COPY eytbelaggtemp TO tempytbelagg.
         RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(tempytbelagg)).
         RUN openbdynspec_UI IN brwproc[4]. 
         RUN lastselectdyn_UI IN brwproc[4].         
      END.     
      musz = FALSE.
   END.   
   ELSE IF BRW_FOR:HIDDEN = FALSE THEN DO:
      RUN FORLANYU.W (INPUT 0,OUTPUT TABLE eforlaggtemp).
      IF musz = FALSE THEN DO:
         FIND FIRST eforlaggtemp WHERE NO-LOCK NO-ERROR.
         FIND FIRST tempforlagg WHERE tempforlagg.ID = eforlaggtemp.ID NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tempforlagg THEN CREATE tempforlagg.
         BUFFER-COPY eforlaggtemp TO tempforlagg.
         RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(tempforlagg)).
         RUN openbdynspec_UI IN brwproc[2]. 
         RUN lastselectdyn_UI IN brwproc[2].
      END.
      EMPTY TEMP-TABLE eforlaggtemp NO-ERROR. 
      musz = FALSE.
   END.      
   ELSE IF BRW_SAM:HIDDEN = FALSE THEN DO:
      
   END. /* slut samförläggning */
   ELSE IF BRW_HANDELSE:HIDDEN = FALSE THEN DO:
      idvar = 0.
      RUN NYHAND.W (INPUT-OUTPUT idvar).
      RUN laddahandelse_UI (INPUT idvar).
   END.   
END PROCEDURE.

PROCEDURE kkopp_UI :
   FRAME-KKOPP:HIDDEN = FALSE.
   FRAME-FORLSATT:HIDDEN = TRUE.
   FRAME-KABLAR:HIDDEN = TRUE.
   BRW_YT:HIDDEN = TRUE.
   BRW_FOR:HIDDEN = TRUE.
   BRW_SAM:HIDDEN = TRUE.
END PROCEDURE.


PROCEDURE handel_UI :
   FRAME-FORLSATT:HIDDEN = FALSE.
   FRAME-KKOPP:HIDDEN = TRUE.
   FRAME-KABLAR:HIDDEN = TRUE.
   BRW_YT:HIDDEN = TRUE.
   BRW_FOR:HIDDEN = TRUE.
   BRW_SAM:HIDDEN = TRUE.
   BRW_HANDELSE:HIDDEN = FALSE.

END PROCEDURE.

PROCEDURE yt_UI :
   FRAME-FORLSATT:HIDDEN = FALSE.
   FRAME-KKOPP:HIDDEN = TRUE.
   FRAME-KABLAR:HIDDEN = TRUE.
   BRW_YT:HIDDEN = FALSE.
   BRW_FOR:HIDDEN = TRUE.
   BRW_SAM:HIDDEN = TRUE.
   BRW_HANDELSE:HIDDEN = TRUE.
END.

PROCEDURE sam_UI :
   FRAME-FORLSATT:HIDDEN = FALSE.
   FRAME-KKOPP:HIDDEN = TRUE.
   FRAME-KABLAR:HIDDEN = TRUE.
   BRW_YT:HIDDEN = TRUE.
   BRW_FOR:HIDDEN = TRUE.
   BRW_SAM:HIDDEN = FALSE.
   BRW_HANDELSE:HIDDEN = TRUE.
END.

PROCEDURE forl_UI :
   FRAME-FORLSATT:HIDDEN = FALSE.
   FRAME-KKOPP:HIDDEN = TRUE.
   FRAME-KABLAR:HIDDEN = TRUE.
   BRW_YT:HIDDEN = TRUE.
   BRW_FOR:HIDDEN = FALSE.
   BRW_SAM:HIDDEN = TRUE.
   BRW_HANDELSE:HIDDEN = TRUE.
END.

PROCEDURE addkabel_UI :
   DEFINE VARIABLE kabidnr AS INTEGER NO-UNDO.
   DEFINE VARIABLE kabelfinns AS LOGICAL NO-UNDO.
   RUN selectfoc_UI.
   
   RUN hittakab_UI IN schakadmapph (INPUT tempkonst.KTYPKOD, OUTPUT kabelfinns).   
   IF kabelfinns = TRUE THEN DO:    
      RUN bortkabel_UI IN schakadmapph (INPUT tempkonst.KTYPKOD, INPUT ?, INPUT FALSE). /*om kabeln redan fanns med borttag = true*/      
      RUN hamten_UI IN schakadmapph (INPUT tempkonst.KTYPKOD, OUTPUT TABLE tempkabel APPEND).
      FIND FIRST tempkabel WHERE tempkabel.KABEL = tempkonst.KTYPKOD NO-LOCK NO-ERROR.
      RUN setlastrowid_UI IN brwproc[6] (INPUT ROWID(tempkabel)).        
      RUN openbdynspec_UI IN brwproc[6].
      RUN lastselectdyn_UI IN brwproc[6].
   END.
   ELSE DO: /* helt ny kabel */
      FIND LAST tempkabel NO-ERROR.
      IF NOT AVAILABLE tempkabel THEN DO:
         kabidnr = 1.
      END.
      ELSE DO:
         kabidnr = tempkabel.KABID + 1.
      END.
      CREATE tempkabel.
      ASSIGN
      tempkabel.kabel = tempkonst.KTYPKOD
      tempkabel.kabid = kabidnr.
      RUN setlastrowid_UI IN brwproc[6] (INPUT ROWID(tempkabel)).        
      RUN openbdynspec_UI IN brwproc[6].
      RUN lastselectdyn_UI IN brwproc[6].
   END.
END PROCEDURE.

PROCEDURE selectfoc_UI :
   DEFINE VARIABLE brwrowid AS ROWID NO-UNDO.
   DEFINE VARIABLE brwrecid AS RECID NO-UNDO.
   IF NOT VALID-HANDLE(tempkonsth) THEN DO:
      tempkonsth = TEMP-TABLE tempkonst:DEFAULT-BUFFER-HANDLE.
   END.
   RUN selectrowid_UI IN brwproc[5] (OUTPUT brwrowid).   
   IF brwrowid NE ? THEN
   FIND FIRST tempkonst WHERE ROWID(tempkonst) = brwrowid NO-LOCK NO-ERROR.  
END PROCEDURE.

PROCEDURE selectfockab_UI :
   DEFINE VARIABLE brwrowid AS ROWID NO-UNDO.
   DEFINE VARIABLE brwrecid AS RECID NO-UNDO.
   IF NOT VALID-HANDLE(tempkabelh) THEN DO:
      tempkabelh = TEMP-TABLE tempkabel:DEFAULT-BUFFER-HANDLE.
   END.
   RUN selectrowid_UI IN brwproc[6] (OUTPUT brwrowid).   
   IF brwrowid NE ? THEN
   FIND FIRST tempkabel WHERE ROWID(tempkabel) = brwrowid NO-LOCK NO-ERROR.      
END PROCEDURE.

                          
PROCEDURE selectkhand_UI :
   DEFINE VARIABLE brwrowid AS ROWID NO-UNDO.
   DEFINE VARIABLE brwrecid AS RECID NO-UNDO.
   IF NOT VALID-HANDLE(temphandkoph) THEN DO:
      temphandkoph = TEMP-TABLE temphandkopp:DEFAULT-BUFFER-HANDLE.
   END.
   RUN selectrowid_UI IN brwproc[10] (OUTPUT brwrowid).
   RUN selectbyrowid_UI IN brwproc[10](INPUT brwrowid). 
   IF brwrowid NE ? THEN FIND FIRST temphandkopp WHERE temphandkopp.TYP = htyp AND ROWID(temphandkopp) = brwrowid NO-LOCK NO-ERROR.
   ELSE RELEASE temphandkopp NO-ERROR.
   
END PROCEDURE.   

PROCEDURE selectfocyt_UI :
   DEFINE VARIABLE brwrowid AS ROWID NO-UNDO.
   DEFINE VARIABLE brwrecid AS RECID NO-UNDO.
   IF NOT VALID-HANDLE(tempyth) THEN DO:
      tempyth = TEMP-TABLE tempytbelagg:DEFAULT-BUFFER-HANDLE.
   END.
   RUN selectrowid_UI IN brwproc[4] (OUTPUT brwrowid).
   RUN selectbyrowid_UI IN brwproc[4](INPUT brwrowid). 
   IF brwrowid NE ? THEN FIND FIRST tempytbelagg WHERE ROWID(tempytbelagg) = brwrowid NO-LOCK NO-ERROR.   
END PROCEDURE.   


PROCEDURE selectfocsam_UI :
   DEFINE VARIABLE brwrowid AS ROWID NO-UNDO.
   DEFINE VARIABLE brwrecid AS RECID NO-UNDO.
   IF NOT VALID-HANDLE(tempsamh) THEN DO:
      tempsamh = TEMP-TABLE tempsamforlagg:DEFAULT-BUFFER-HANDLE.
   END.
   RUN selectrowid_UI IN brwproc[3] (OUTPUT brwrowid).
   RUN selectbyrowid_UI IN brwproc[3](INPUT brwrowid). 
   IF brwrowid NE ? THEN
   FIND FIRST tempsamforlagg WHERE ROWID(tempsamforlagg) = brwrowid NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE selectfocfor_UI :
   DEFINE VARIABLE brwrowid AS ROWID NO-UNDO.
   DEFINE VARIABLE brwrecid AS RECID NO-UNDO.
   IF NOT VALID-HANDLE(tempforh) THEN DO:
      tempforh = TEMP-TABLE tempforlagg:DEFAULT-BUFFER-HANDLE.
   END.
   RUN selectrowid_UI IN brwproc[2] (OUTPUT brwrowid).
   RUN selectbyrowid_UI IN brwproc[2](INPUT brwrowid).
   IF brwrowid NE ? THEN
   FIND FIRST tempforlagg WHERE ROWID(tempforlagg) = brwrowid NO-LOCK NO-ERROR.
END PROCEDURE.


PROCEDURE selectfochand_UI :
   DEFINE VARIABLE brwrowid AS ROWID NO-UNDO.
   DEFINE VARIABLE brwrecid AS RECID NO-UNDO.
   IF NOT VALID-HANDLE(tempforh) THEN DO:
      tempforh = TEMP-TABLE temphandelse:DEFAULT-BUFFER-HANDLE.
   END.
   RUN selectrowid_UI IN brwproc[12] (OUTPUT brwrowid).
   RUN selectbyrowid_UI IN brwproc[12](INPUT brwrowid).
   IF brwrowid NE ? THEN
   FIND FIRST temphandelse WHERE ROWID(temphandelse) = brwrowid NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE pselectfoc_UI :
   DEFINE INPUT PARAMETER brwvald AS INTEGER NO-UNDO.
   DEFINE VARIABLE brwrowid AS ROWID NO-UNDO.
   DEFINE VARIABLE brwrecid AS RECID NO-UNDO.
   RUN pselectrowid_UI IN brwproc[brwvald] (OUTPUT brwrowid).
   RUN ff_UI (INPUT brwvald,INPUT brwrowid).  
END PROCEDURE.
PROCEDURE nselectfoc_UI :
   DEFINE INPUT PARAMETER brwvald AS INTEGER NO-UNDO.
   DEFINE VARIABLE brwrowid AS ROWID NO-UNDO.
   DEFINE VARIABLE brwrecid AS RECID NO-UNDO.
   RUN nselectrowid_UI IN brwproc[brwvald] (OUTPUT brwrowid).
   RUN ff_UI (INPUT brwvald,INPUT brwrowid).    
END PROCEDURE.

PROCEDURE ff_UI :
   DEFINE INPUT PARAMETER brwvald AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER brwrowid AS ROWID NO-UNDO.
   IF brwvald = 4 THEN FIND FIRST tempytbelagg WHERE ROWID(tempytbelagg) = brwrowid NO-LOCK NO-ERROR.         
   IF brwvald = 2 THEN FIND FIRST tempforlagg WHERE ROWID(tempforlagg) = brwrowid NO-LOCK NO-ERROR.         
   IF brwvald = 12 THEN FIND FIRST temphandelse WHERE ROWID(temphandelse) = brwrowid NO-LOCK NO-ERROR.         
   
END PROCEDURE.

PROCEDURE kabel_UI :
   RUN openbdynspec_UI IN brwproc[5].
   FIND FIRST tempkonst WHERE tempkonst.KONSKOD = kongrkod NO-LOCK NO-ERROR.
   IF AVAILABLE tempkonst THEN DO:
      RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(tempkonst)).
      RUN lastselectdyn_UI IN brwproc[5].    
   END.
END PROCEDURE.

PROCEDURE entrylma_UI : 
   IF listkabdia:READ-ONLY = FALSE THEN entrykabdia = TRUE.
   RUN selectfockab_UI.     
END PROCEDURE.

/* ladda tabell för händelse-admin */
PROCEDURE laddahandelse_UI :
   DEFINE INPUT PARAMETER idvar AS INTEGER NO-UNDO.
   tthandle = TEMP-TABLE temphandelse:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "HDHANDELSE", INPUT "").
   FIND FIRST temphandelse WHERE temphandelse.ID = idvar NO-LOCK NO-ERROR.
   IF AVAILABLE temphandelse THEN RUN setlastrowid_UI IN brwproc[12] (INPUT ROWID(temphandelse)).
   RUN openbdynspec_UI IN brwproc[12].
   RUN lastselectdyn_UI IN brwproc[12]. 

END PROCEDURE.


/*ladda Händelse / Ytbelägg / Forlagg för koppling beroende på RAD_HTYP (händelsetyp) */
PROCEDURE laddaHYF_UI :
   RUN ytforhand_UI IN schakadmapph (OUTPUT TABLE temphandkopp). 
END PROCEDURE.

/*sortera Händelse / Ytbelägg / Forlagg för koppling beroende på RAD_HTYP (händelsetyp) */
PROCEDURE sorteraHYF_UI :
   DEFINE VARIABLE htyp2 AS CHARACTER NO-UNDO.

   RUN typ_UI.
   
   htyp2 = htyp.
   BRW_KKOPP:VISIBLE = TRUE.    
   RUN brwhand_UI IN huvprogh (INPUT RAD_HTYP:SCREEN-VALUE).   
   brwsortvar = " WHERE temphandkopp.BORT = FALSE AND temphandkopp.TYP = '" + htyp2 + "'".
   RUN setcolsortvar_UI IN brwproc[10] (INPUT brwsortvar).
   RUN setcolindex_UI IN brwproc[10] (INPUT "ORDNING").  
   RUN openbdynspec_UI IN brwproc[10].   
   RUN brwhand_UI.     
END PROCEDURE.

/*lämna fältet bredd i brw för KKOPP*/


PROCEDURE leavekabel_UI :
   IF entrykabdia = TRUE THEN DO:   
      IF AVAILABLE tempkabel THEN DO:         
         tempkabel.DIAMETER = DECIMAL(listkabdia:SCREEN-VALUE).                  
         RUN nykabel_UI IN schakadmapph (INPUT tempkabel.KABID, INPUT tempkabel.KABEL, INPUT tempkabel.DIAMETER).         
      END.
   END.
   entrykabdia = FALSE.   
END PROCEDURE.


PROCEDURE kabelbort_UI :

   FIND FIRST tempkabel NO-LOCK NO-ERROR.
      IF AVAILABLE tempkabel THEN DO:
         RUN selectfockab_UI.
         kabkod  = tempkabel.KABID.
         MESSAGE "Vill du ta bort " + tempkabel.KABEL + "?"
         VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO TITLE "Ta bort?" UPDATE svar.
         IF svar THEN DO:
            RUN bortkabel_UI IN schakadmapph (INPUT ?, INPUT tempkabel.KABID, INPUT TRUE).
            FIND FIRST tempkabel WHERE tempkabel.KABID = kabkod.
            DELETE tempkabel.
            RUN selnextprevrow_UI IN brwproc[6].
            RUN openbdynspec_UI IN brwproc[6] (INPUT "").            
            RUN lastselectdyn_UI IN brwproc[6].            
         END.
      END.
      ELSE DO:
         MESSAGE "Det finns ingen kabel att ta bort."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
      END.
END PROCEDURE.



PROCEDURE mlma_UI :
   APPLY "ENTRY" TO  listkabdia.
END PROCEDURE.

PROCEDURE cmbval_UI :
   FIND FIRST konstgrptemp WHERE konstgrptemp.BENAMNING = CMB_VAL2:SCREEN-VALUE NO-LOCK NO-ERROR. 
   IF NOT AVAILABLE konstgrptemp THEN FIND FIRST konstgrptemp NO-LOCK NO-ERROR.  /* ?? */
   IF AVAILABLE konstgrptemp THEN DO:
      kongrkod = konstgrptemp.KONSKOD.      
      ASSIGN C-Win:TITLE = "Schaktadministration".     
      RUN setcolsortvar_UI IN brwproc[5] (INPUT " WHERE KONSKOD = '" + STRING(kongrkod) + "' ").      
      RUN openbdynspec_UI IN brwproc[5].            
      FIND FIRST tempkonst WHERE tempkonst.KONSKOD = kongrkod AND tempkonst.KTYPKOD = konstvalvar USE-INDEX ORD NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tempkonst THEN DO:
         FIND FIRST tempkonst WHERE tempkonst.KONSKOD = kongrkod USE-INDEX ORD NO-LOCK NO-ERROR.
      END.      
      IF AVAILABLE tempkonst THEN DO:         
         RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(tempkonst)).
         RUN lastselectdyn_UI IN brwproc[5].
      END.
      APPLY "VALUE-CHANGED" TO BRW_KON.
   END.
END PROCEDURE.

PROCEDURE brwkon_UI :  
  
END PROCEDURE.

   /*när man markerar arbkod*/
PROCEDURE brwp_UI :
   DEFINE VARIABLE brwrowid AS ROWID NO-UNDO.
   RUN selectrowid_UI IN brwproc[7] (OUTPUT brwrowid).
   FIND FIRST ptemp WHERE ROWID(ptemp) = brwrowid NO-LOCK NO-ERROR.
   RUN setcolsortvar_UI IN brwproc[8] (INPUT "TYP = " + STRING(RAD_TYP:SCREEN-VALUE) + " AND ARBKOD = '" + ptemp.ARBKOD + "'").
   RUN openbdynspec_UI IN brwproc[8].   
END PROCEDURE.

   /*när man markerar löpnr*/
PROCEDURE brwl_UI :
   DEFINE VARIABLE brwrowid AS ROWID NO-UNDO.
   RUN selectrowid_UI IN brwproc[8] (OUTPUT brwrowid).
   FIND FIRST loptemp WHERE ROWID(loptemp) = brwrowid NO-LOCK NO-ERROR.
END PROCEDURE.


PROCEDURE brwkkopp_UI :
  DEFINE VARIABLE brwrowid AS ROWID NO-UNDO.
  RUN typ_UI.
  RUN selectrowid_UI IN brwproc[11] (OUTPUT brwrowid).
  FIND FIRST tempkkopp WHERE ROWID(tempkkopp) = brwrowid NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE brwval_UI :
  DEFINE VARIABLE brwrowid AS ROWID NO-UNDO.
  RUN selectrowid_UI IN brwproc[9] (OUTPUT brwrowid).
  FIND FIRST fastkalktemp WHERE ROWID(fastkalktemp) = brwrowid NO-LOCK NO-ERROR.
END PROCEDURE.


PROCEDURE typ_UI :
   
   IF RAD_HTYP:SCREEN-VALUE = "1" THEN htyp = "ph".
   ELSE if RAD_HTYP:SCREEN-VALUE = "5" THEN htyp = "fh".
   ELSE if RAD_HTYP:SCREEN-VALUE = "2" THEN htyp = "y".
   ELSE IF RAD_HTYP:SCREEN-VALUE = "3" THEN htyp = "f".   
END PROCEDURE.

/*Skapa vallista från urval av koder*/

PROCEDURE valkod_UI :
    DEFINE VARIABLE arbeitskod AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lopnummer AS INTEGER NO-UNDO.
    DEFINE VARIABLE benamn AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE typ AS CHARACTER NO-UNDO.
    RUN cellvalue_UI IN brwproc[7] (INPUT "ARBKOD",OUTPUT arbeitskod).
    RUN cellvalue_UI IN brwproc[8] (INPUT "LOPNR",OUTPUT lopnummer).
    RUN cellvalue_UI IN brwproc[8] (INPUT "BENAMNING",OUTPUT benamn).    
    FIND FIRST fastkalktemp WHERE fastkalktemp.ARBKOD = arbeitskod AND fastkalktemp.LOPNR = lopnummer NO-ERROR.    
    IF NOT AVAILABLE fastkalktemp THEN DO:
        DO TRANSACTION:
          CREATE fastkalktemp.
          ASSIGN
          fastkalktemp.ARBKOD = arbeitskod
          fastkalktemp.LOPNR = INTEGER(lopnummer)
          fastkalktemp.BENAMNING = benamn.
       END.       
      RUN setlastrowid_UI IN brwproc[9] (INPUT ROWID(fastkalktemp)).        
      RUN openbdynspec_UI IN brwproc[9].
      RUN lastselectdyn_UI IN brwproc[9].
    END.     
END PROCEDURE.

PROCEDURE ovalkod_UI :
   DEFINE VARIABLE arbeitskod AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE lopnummer AS INTEGER  NO-UNDO.
   DEFINE VARIABLE finnas AS ROWID NO-UNDO.
   RUN hittapost_UI (INPUT 9, OUTPUT finnas).
   IF finnas NE ? THEN DO:      
      RUN selfocusrow_UI IN brwproc[9].
      IF AVAILABLE fastkalktemp THEN DELETE fastkalktemp.
      RUN selnextprevrow_UI IN brwproc[9].
      RUN openbdynspec_UI IN brwproc[9].            
      RUN lastselectdyn_UI IN brwproc[9].      
   END.
END PROCEDURE.

PROCEDURE hittapost_UI :
    DEFINE INPUT PARAMETER procnr AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER id AS ROWID NO-UNDO.
    RUN getfirst_UI IN brwproc[procnr].    
    RUN finns_UI IN brwproc[procnr] (OUTPUT id).
END PROCEDURE.

PROCEDURE okoppla_UI :
   DEFINE VARIABLE typvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE idvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE kkidvar AS INTEGER NO-UNDO.   
   DEFINE VARIABLE benamn AS CHARACTER NO-UNDO.
   DEFINE VARIABLE arbeitskod AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lopnummer AS INTEGER NO-UNDO.
   DEFINE VARIABLE finnas AS ROWID NO-UNDO.
   DEFINE VARIABLE brwrowid AS ROWID NO-UNDO.
   RUN typ_UI.
   RUN selectrowid_UI IN brwproc[11] (OUTPUT brwrowid).
   FIND FIRST tempkkopp WHERE ROWID(tempkkopp) = brwrowid NO-LOCK NO-ERROR.
   /*först kontroll om det finns något att tabort*/
   IF AVAILABLE tempkkopp AND tempkkopp.TYP NE ? THEN DO:      
      RUN cellvalue_UI IN brwproc[11] (INPUT "TYP",OUTPUT typvar).
      RUN cellvalue_UI IN brwproc[11] (INPUT "ID",OUTPUT idvar).
      RUN cellvalue_UI IN brwproc[11] (INPUT "KKID",OUTPUT kkidvar).      
      FIND FIRST fastkalktemp WHERE fastkalktemp.ARBKOD = tempkkopp.ARBKOD AND fastkalktemp.LOPNR = tempkkopp.LOPNR NO-LOCK NO-ERROR.
      /*.. sedan kontroll om posten som valts finns i "valda koder" sedan tidigare*/
      IF NOT AVAILABLE fastkalktemp THEN DO:
          /*skapa valkod igen*/
         RUN cellvalue_UI IN brwproc[11] (INPUT "BENAMNING",OUTPUT benamn).
         RUN cellvalue_UI IN brwproc[11] (INPUT "ARBKOD",OUTPUT arbeitskod).
         RUN cellvalue_UI IN brwproc[11] (INPUT "LOPNR",OUTPUT lopnummer).
         IF arbeitskod NE ? AND lopnummer NE ? THEN DO:
              FIND FIRST fastkalktemp WHERE fastkalktemp.ARBKOD = arbeitskod AND fastkalktemp.LOPNR = lopnummer NO-LOCK NO-ERROR.
              IF NOT AVAILABLE fastkalktemp THEN DO:
                  DO TRANSACTION:
                     CREATE fastkalktemp.
                     ASSIGN
                     fastkalktemp.ARBKOD = arbeitskod
                     fastkalktemp.LOPNR = INTEGER(lopnummer)
                     fastkalktemp.BENAMNING = benamn.
                  END.
              END.
          END.    
          RUN setlastrowid_UI IN brwproc[9] (INPUT ROWID(fastkalktemp)).        
          RUN openbdynspec_UI IN brwproc[9].
          RUN lastselectdyn_UI IN brwproc[9].   
          /*slut skapa valkod*/
      END.
      RUN kkidbort_UI IN schakadmapph (INPUT typvar, INPUT idvar, INPUT kkidvar).     
      brwsortvar = " WHERE tempkkopp.TYP = '" + htyp + "' AND tempkkopp.ID = '" + string(idvar) + "' ".            
      RUN selfocusrow_UI IN brwproc[11].
      IF AVAILABLE tempkkopp THEN DELETE tempkkopp.
      RUN selnextprevrow_UI IN brwproc[11].
      RUN setcolsortvar_UI IN brwproc[11] (INPUT brwsortvar).
      RUN openbdynspec_UI IN brwproc[11].            
      RUN lastselectdyn_UI IN brwproc[11].         
   END.
END PROCEDURE.

PROCEDURE koppla_UI :
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   DEFINE VARIABLE idvar AS INTEGER NO-UNDO.
   
   i = 1.

   RUN typ_UI.
   RUN cellvalue_UI IN brwproc[10] (INPUT "ID",OUTPUT idvar).   
   /*hittamax*/
   RUN hittamaxkkid_UI IN schakadmapph (INPUT htyp, INPUT idvar, OUTPUT i).
   i = i + 1.
   FOR EACH fastkalktemp NO-LOCK:
      FIND FIRST tempkkopp WHERE tempkkopp.ARBKOD = STRING(fastkalktemp.ARBKOD) AND tempkkopp.LOPNR = fastkalktemp.LOPNR AND
      tempkkopp.ID = idvar AND tempkkopp.TYP = htyp  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tempkkopp THEN DO:
         CREATE tempkkopp.
         ASSIGN
         tempkkopp.ARBKOD = STRING(fastkalktemp.ARBKOD)
         tempkkopp.LOPNR = fastkalktemp.LOPNR
         tempkkopp.ID = idvar
         tempkkopp.TYP = htyp
         tempkkopp.BENAMNING = fastkalktemp.BENAMNING
         tempkkopp.KKID = i
         tempkkopp.ANTAL = 1.
         i = i + 1.
      END.
   END.
   RUN sparakopp_UI IN schakadmapph (INPUT TABLE tempkkopp).
  APPLY "VALUE-CHANGED" TO BRW_HAND.
END PROCEDURE.

/*när man byter händelse*/
PROCEDURE brwhand_UI :
   DEFINE VARIABLE idvar AS INTEGER NO-UNDO.
   RUN selectkhand_UI.
   IF AVAILABLE temphandkopp THEN DO:
      RUN cellvalue_UI IN brwproc[10] (INPUT "ID",OUTPUT idvar).      
      RUN typ_UI.      
      RUN laddakkoppTYP_UI IN schakadmapph (INPUT htyp,INPUT idvar, OUTPUT TABLE tempkkopp).
      brwsortvar = " WHERE tempkkopp.TYP = '" + htyp + "' AND tempkkopp.ID = '" + STRING(idvar) + "' ".      
      RUN setcolsortvar_UI IN brwproc[11] (INPUT brwsortvar).
      RUN setlastrowid_UI IN brwproc[11] (INPUT ROWID(tempkkopp)).        
      RUN openbdynspec_UI IN brwproc[11].
      RUN lastselectdyn_UI IN brwproc[11].
   END.  
END PROCEDURE.

PROCEDURE borthand_UI :
   {BORTBRWPROC.I}           
   IF VALID-HANDLE(schakadmapph) THEN DELETE PROCEDURE schakadmapph.
   IF VALID-HANDLE(KalkClasserStart) THEN DELETE PROCEDURE KalkClasserStart NO-ERROR. 
   IF VALID-HANDLE(laddaproch) THEN DELETE PROCEDURE laddaproch NO-ERROR.   
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
END PROCEDURE.
