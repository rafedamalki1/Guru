/*AOKUPPD.P*/
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{GLOBVAR2DEL1.I}
{REGVAR.I}

{AVTPLANTEMP.I}
FIND FIRST FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
RUN STYRFORE.P (INPUT Guru.Konstanter:globforetag).
DEFINE TEMP-TABLE aokont
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD K1 AS CHARACTER
   FIELD K2 AS CHARACTER
   FIELD K3 AS CHARACTER
   FIELD K4 AS CHARACTER 
   FIELD K5 AS CHARACTER 
   FIELD SATS% AS INTEGER
   FIELD RECTIDVIS AS RECID.   
DEFINE BUFFER aonrbuff FOR AONRTAB.
DEFINE INPUT PARAMETER vartvar AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER vadgora AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ktorec AS RECID NO-UNDO.
DEFINE INPUT PARAMETER omrvar AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER benvar AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER aonrvar AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER delnrvar AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER plannrvar AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER artalvar AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR plankonttemp.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR aokont.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
DEFINE BUFFER aonrkontbuff FOR AONRKONTKOD.
{EXTRATAB.I} 
/*OBS ! 1-4 KÖRS ALDRIG 2004-01-14*/
/*Hämta benämning under btn_fran*/
IF vadgora = 1 THEN DO: 
   IF vartvar = 1 THEN DO:
      FIND FIRST aokont WHERE aokont.AONR = aonrvar AND 
      aokont.DELNR = delnrvar NO-LOCK NO-ERROR.
      FIND FIRST KONTOSTRANG WHERE KONTOSTRANG.OMRADE = omrvar AND 
      KONTOSTRANG.K1 = aokont.K1 AND
      KONTOSTRANG.K2 = aokont.K2 AND
      KONTOSTRANG.K3 = aokont.K3 AND
      KONTOSTRANG.K4 = aokont.K4 AND
      KONTOSTRANG.K5 = aokont.K5 NO-LOCK NO-ERROR.
      IF AVAILABLE KONTOSTRANG THEN ASSIGN benvar = KONTOSTRANG.BENAMNING.  
   END.
   IF vartvar = 2 THEN DO:
      FIND FIRST plankonttemp WHERE plankonttemp.PLANNR = plannrvar AND 
      plankonttemp.ARTAL = artalvar NO-LOCK NO-ERROR.
      FIND FIRST KONTOSTRANG WHERE KONTOSTRANG.OMRADE = omrvar AND 
      KONTOSTRANG.K1 = plankonttemp.K1 AND
      KONTOSTRANG.K2 = plankonttemp.K2 AND
      KONTOSTRANG.K3 = plankonttemp.K3 AND
      KONTOSTRANG.K4 = plankonttemp.K4 AND
      KONTOSTRANG.K5 = plankonttemp.K5 NO-LOCK NO-ERROR.
      IF AVAILABLE KONTOSTRANG THEN ASSIGN benvar = KONTOSTRANG.BENAMNING. 
   END.
END.

/*fran*/
ELSE IF vadgora = 3 THEN DO:
   IF vartvar = 1 THEN DO:
      DO TRANSACTION:   
         FIND FIRST AONRKONTKOD WHERE RECID(AONRKONTKOD) = ktorec EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE AONRKONTKOD THEN DO:      
           DELETE AONRKONTKOD.
         END.  
      END.
   END.
   IF vartvar = 2 THEN DO:
      DO TRANSACTION:
         FIND FIRST PLANKONTO WHERE RECID(PLANKONTO) = ktorec EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE PLANKONTO THEN DO:      
            DELETE PLANKONTO.
         END.
      END.
   END.
END.

/*ok*/
ELSE IF vadgora = 4 THEN DO:
   
   IF vartvar = 1 THEN DO:
      FOR EACH aokont:
         DO TRANSACTION:      
            FIND FIRST AONRKONTKOD WHERE RECID(AONRKONTKOD) = aokont.RECTIDVIS EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE AONRKONTKOD THEN DO:
               ASSIGN
               AONRKONTKOD.AONR = aokont.AONR
               AONRKONTKOD.DELNR = aokont.DELNR 
               AONRKONTKOD.K1 = aokont.K1
               AONRKONTKOD.K2 = aokont.K2         
               AONRKONTKOD.K3 = aokont.K3
               AONRKONTKOD.K4 = aokont.K4
               AONRKONTKOD.K5 = aokont.K5      
               AONRKONTKOD.SATS% = aokont.SATS%.     
            END.
            ELSE DO:
               CREATE AONRKONTKOD.            
               ASSIGN
               AONRKONTKOD.AONR = aokont.AONR
               AONRKONTKOD.DELNR = aokont.DELNR 
               AONRKONTKOD.K1 = aokont.K1
               AONRKONTKOD.K2 = aokont.K2         
               AONRKONTKOD.K3 = aokont.K3
               AONRKONTKOD.K4 = aokont.K4
               AONRKONTKOD.K5 = aokont.K5          
               AONRKONTKOD.SATS% = aokont.SATS%.
            END.
         END.
      END.
   END.
   IF vartvar = 2 THEN DO:
      FOR EACH plankonttemp:
         DO TRANSACTION:
            FIND FIRST PLANKONTO WHERE RECID(PLANKONTO) = plankonttemp.RECTIDVIS EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE PLANKONTO THEN DO:
               ASSIGN
               PLANKONTO.PLANNR = plankonttemp.PLANNR
               PLANKONTO.ARTAL = plankonttemp.ARTAL
               PLANKONTO.K1 = plankonttemp.K1
               PLANKONTO.K2 = plankonttemp.K2
               PLANKONTO.K3 = plankonttemp.K3
               PLANKONTO.K4 = plankonttemp.K4
               PLANKONTO.K5 = plankonttemp.K5
               PLANKONTO.SATS% = plankonttemp.SATS%.
            END.
            ELSE DO:
               CREATE PLANKONTO.
               ASSIGN
               PLANKONTO.PLANNR = plankonttemp.PLANNR
               PLANKONTO.ARTAL = plankonttemp.ARTAL
               PLANKONTO.K1 = plankonttemp.K1
               PLANKONTO.K2 = plankonttemp.K2
               PLANKONTO.K3 = plankonttemp.K3
               PLANKONTO.K4 = plankonttemp.K4
               PLANKONTO.K5 = plankonttemp.K5
               PLANKONTO.SATS% = plankonttemp.SATS%.
            END.
         END.
      END.
   END.

END.

/*ok*/
IF vadgora = 5 THEN DO:
   IF vartvar = 1 THEN DO:
      FIND FIRST AONRTAB WHERE AONRTAB.AONR = aonrvar AND
      AONRTAB.DELNR = delnrvar NO-LOCK NO-ERROR.
      DO TRANSACTION:
         FIND CURRENT AONRTAB EXCLUSIVE-LOCK NO-ERROR.
         AONRTAB.AUTOREG = TRUE.         
      END.
      OPEN QUERY akkq FOR EACH AONRKONTKOD WHERE AONRKONTKOD.AONR = AONRTAB.AONR AND 
      AONRKONTKOD.DELNR = AONRTAB.DELNR NO-LOCK.
      GET FIRST akkq NO-LOCK.
      DO WHILE AVAILABLE(AONRKONTKOD):               
         DO TRANSACTION:
            GET CURRENT akkq EXCLUSIVE-LOCK.
            DELETE AONRKONTKOD.
         END.              
         GET NEXT akkq NO-LOCK.
      END.
      FOR EACH aokont:
         DO TRANSACTION:
            CREATE AONRKONTKOD.
            BUFFER-COPY aokont TO AONRKONTKOD.               
         END.            
      END.
      
      /*BYT KONTO PÅ UNDER NUMMER*/
      IF Guru.Konstanter:varforetypval[12] = 0 THEN DO:
         IF AONRTAB.DELNR = 0 THEN DO:
            OPEN QUERY akkq FOR EACH AONRKONTKOD WHERE AONRKONTKOD.AONR = AONRTAB.AONR NO-LOCK.
            GET FIRST akkq NO-LOCK.
            DO WHILE AVAILABLE(AONRKONTKOD):
               IF AONRKONTKOD.DELNR NE 0 THEN DO TRANSACTION:
                  GET CURRENT akkq EXCLUSIVE-LOCK.
                  DELETE AONRKONTKOD.
               END.
               GET NEXT akkq NO-LOCK.
            END.
            OPEN QUERY aoq FOR EACH aonrbuff WHERE aonrbuff.AONR = AONRTAB.AONR NO-LOCK.
            GET FIRST aoq NO-LOCK.
            DO WHILE AVAILABLE(aonrbuff):
               IF aonrbuff.DELNR NE 0 THEN DO TRANSACTION:  
                  GET CURRENT aoq EXCLUSIVE-LOCK.
                  aonrbuff.AUTOREG = TRUE.         
                  FOR EACH aokont NO-LOCK:
                     CREATE AONRKONTKOD.
                     BUFFER-COPY aokont TO AONRKONTKOD.
                     AONRKONTKOD.DELNR = aonrbuff.DELNR. 
                  END.                 
               END.
               GET NEXT aoq NO-LOCK.
            END.
            RELEASE AONRKONTKOD NO-ERROR.           
         END.                          
      END.
      RUN extrakopp_UI.
   END.
   IF vartvar = 2 THEN DO:
      FIND PLANKONTO WHERE PLANKONTO.PLANNR = plannrvar AND
      PLANKONTO.ARTAL = artalvar NO-LOCK NO-ERROR.
      FOR EACH PLANKONTO WHERE PLANKONTO.PLANNR = plannrvar AND
      PLANKONTO.ARTAL = artalvar EXCLUSIVE-LOCK:
         DELETE PLANKONTO.
      END.              
      FOR EACH plankonttemp:
         DO TRANSACTION:
            CREATE PLANKONTO.
            BUFFER-COPY plankonttemp TO PLANKONTO.               
         END.            
      END.
      RELEASE PLANKONTO NO-ERROR.
   END.
END.




PROCEDURE extrakopp_UI :
   DEFINE VARIABLE nyomrade AS CHARACTER NO-UNDO.
   DEFINE VARIABLE andrasif AS CHARACTER NO-UNDO.
   DEFINE VARIABLE fakthmth AS HANDLE NO-UNDO.
   DEFINE VARIABLE bestidvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE berprojvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE nyserie  AS CHARACTER NO-UNDO.
   /*aonrsund*/
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      IF NOT VALID-HANDLE(edataapph) THEN RUN EXTRATABHMT.P PERSISTENT SET edataapph.        
      EMPTY TEMP-TABLE inextrakopptemp NO-ERROR. 
      CREATE inextrakopptemp.          
      ASSIGN
      inextrakopptemp.PROGRAM = "FBAONR"                   
      inextrakopptemp.KOPPLACHAR1 = aonrvar       
      inextrakopptemp.KOPPLAINT1 =  delnrvar      
      inextrakopptemp.KOPPLACHAR2 = ?            
      inextrakopptemp.KOPPLAINT2 =  ?.
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextrakopptemp, OUTPUT TABLE extrakopptemp).
      EMPTY TEMP-TABLE inextrakopptemp NO-ERROR. 
      FIND FIRST extrakopptemp NO-LOCK NO-ERROR.
      IF NOT AVAILABLE extrakopptemp THEN DO:
         IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.
         RETURN.
      END.
      /*se även nyttaoapp.p FBAONR skapar beställares ao*/
      FIND FIRST AONRTAB WHERE AONRTAB.AONR = aonrvar AND AONRTAB.DELNR = delnrvar NO-LOCK NO-ERROR.
      IF AONRTAB.AONR BEGINS "63" AND AONRTAB.OMRADE = "1611"  THEN DO:
         ASSIGN 
         bestidvar = "ao elnät"
         berprojvar = "SEBAS"
         nyserie    = "34"
         nyomrade   = "1380"
         andrasif   = "3".
      END.
      ELSE DO:
         EMPTY TEMP-TABLE extrakopptemp NO-ERROR. 
         IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.
         RETURN.
      END.
      /* Stäng enligt Ingrid 20091222 Lena
      ELSE IF AONRTAB.AONR BEGINS "65" AND AONRTAB.OMRADE = "1611"  THEN DO:
         ASSIGN 
         bestidvar = "520"
         berprojvar = "SEGHD"
         nyserie    = "38"
         nyomrade   = "1511"
         andrasif   = "5".
      END.*/
      
      FOR EACH aonrkontbuff WHERE aonrkontbuff.AONR = extrakopptemp.KOPPLACHAR2 AND
      aonrkontbuff.DELNR = extrakopptemp.KOPPLAINT2 EXCLUSIVE-LOCK:
         DELETE aonrkontbuff.
      END.
      FOR EACH AONRKONTKOD WHERE AONRKONTKOD.AONR = aonrvar AND    
      AONRKONTKOD.DELNR = delnrvar:      
          DO TRANSACTION:
            CREATE aonrkontbuff.
            BUFFER-COPY AONRKONTKOD TO aonrkontbuff.
            ASSIGN
            aonrkontbuff.AONR  = extrakopptemp.KOPPLACHAR2
            aonrkontbuff.DELNR = extrakopptemp.KOPPLAINT2
            SUBSTRING(aonrkontbuff.K1,2,1) = andrasif.
         END.
         RELEASE aonrkontbuff NO-ERROR.
      END.     
      DO TRANSACTION:
         FIND FIRST AONRTAB WHERE AONRTAB.AONR = extrakopptemp.KOPPLACHAR2 AND
         AONRTAB.DELNR = extrakopptemp.KOPPLAINT2 EXCLUSIVE-LOCK NO-ERROR.
         AONRTAB.AUTOREG = TRUE.         
      END.      
   END.
END PROCEDURE.
