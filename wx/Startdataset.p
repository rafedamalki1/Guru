
/*------------------------------------------------------------------------
    File        : Startdataset.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed Oct 22 10:10:36 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
 {KALKYLKAT.I}
  
   {KALKYLPRODATA.i}
 DEFINE VARIABLE AppServerHandle AS HANDLE NO-UNDO. 
 for each kalknum:

delete kalknum.
end.
for each kalknumsub:

delete  kalknumsub.
end.  
DEFINE VARIABLE kalknr AS INTEGER NO-UNDO.
DEFINE VARIABLE omr AS CHARACTER NO-UNDO.
kalknr =  100000.
OMR = "skek".
RUN STARTDATASETAPPDS.p PERSISTENT SET AppServerHandle.
 RUN kphmt IN AppServerHandle (INPUT kalknr, INPUT omr,OUTPUT TABLE kalkylprisertt, OUTPUT TABLE kalkvisningtt).   
RUN LaddaKalkyl IN AppServerHandle (INPUT kalknr,INPUT omr,OUTPUT DATASET KalkylDS).
 RUN KalkTracking(TRUE).  
FIND FIRST kalkhuvtt NO-LOCK NO-ERROR.
RUN CreateKoderRows.
FIND FIRST kalknumtt  WHERE NO-LOCK NO-ERROR.
UPDATE  kalknumtt.antal.
DISPLAY kalknumtt.TOTKOST.
RUN RaknaEnKod(FALSE).
RUN KalkSpara.
FIND FIRST kalknumtt  WHERE NO-LOCK NO-ERROR.
UPDATE  kalknumtt.antal.
DISPLAY kalknumtt.TOTKOST.
RUN RaknaEnKod(FALSE).
RUN KalkSpara.


PROCEDURE CreateKoderRows :
   CREATE kalknumtt.
   ASSIGN 
   kalknumtt.ARBKOD = "110"
   kalknumtt.LOPNR = 13.
     FOR EACH kalknumtt WHERE kalknumtt.MATRIS = 0 NO-LOCK: 
        FIND LAST kalknumttbuf USE-INDEX NUM NO-ERROR.
        IF AVAILABLE kalknumttbuf THEN kalknumtt.NUM = kalknumttbuf.NUM + 1.
        ELSE kalknumtt.NUM = 1.
        ASSIGN
         
        kalknumtt.KALKNR  = kalkhuvtt.KALKNR
        kalknumtt.OMRADE  = kalkhuvtt.OMRADE
        kalknumtt.TYPKALK = kalkhuvtt.TYPKALK /* asdasd*/
        kalknumtt.MARKNING = "test1"
        kalknumtt.MARKSUB = "test2"
        kalknumtt.KLOGSUBID = 13
        kalknumtt.MATRIS  = 1
        kalknumtt.ANTAL   = 1 .
        EMPTY TEMP-TABLE ekalknumsubtt NO-ERROR.  
        /*problem ccc NY KOD*/  
        RUN skapanumsub_UI IN AppServerHandle (INPUT kalknumtt.KLOGSUBID,INPUT kalknumtt.ARBKOD,INPUT kalknumtt.LOPNR,OUTPUT TABLE ekalknumsubtt).
        FOR EACH ekalknumsubtt WHERE NO-LOCK:
           FIND LAST kalknumsubtt WHERE kalknumsubtt.NUM = kalknumtt.NUM USE-INDEX NUM NO-LOCK NO-ERROR.
           IF AVAILABLE kalknumsubtt THEN ekalknumsubtt.NUMSUBID =  kalknumsubtt.NUMSUBID + 1.
           ELSE  ekalknumsubtt.NUMSUBID = 1.
           ASSIGN 
           ekalknumsubtt.KALKNR = kalknumtt.KALKNR
           ekalknumsubtt.OMRADE = kalknumtt.OMRADE
           ekalknumsubtt.NUM = kalknumtt.NUM.
           CREATE kalknumsubtt.
           BUFFER-COPY ekalknumsubtt TO kalknumsubtt.
           kalknumsubtt.TTRECID = RECID(kalknumsubtt).
           DELETE ekalknumsubtt.                      
        END.
        
        EMPTY TEMP-TABLE ekalknumsubtt NO-ERROR.
        
        RUN RaknaEnKod(FALSE).
        DISPLAY  kalknumtt.antal kalknumtt.TOTKOST.                    
     END.
     
     
     RUN KalkSpara.
 
     /*problem ccc NY KOD*/ 
     RUN LaddaKalkyl IN AppServerHandle (INPUT kalknr,INPUT omr,OUTPUT DATASET KalkylDS).
     FIND FIRST kalkhuvtt NO-LOCK NO-ERROR.
    
 
 
END PROCEDURE.



PROCEDURE KalkSpara :
      DEFINE VARIABLE hDSChanges AS HANDLE NO-UNDO.
      RUN KalkTracking(FALSE).
     
      CREATE DATASET hDSChanges.
      hDSChanges:CREATE-LIKE (DATASET KalkylDS:HANDLE).
      hDSChanges:GET-CHANGES (DATASET KalkylDS:HANDLE).
      
      DEFINE VARIABLE SPARAXML AS CHARACTER NO-UNDO.
      SPARAXML = "C:\CTest" + STRING(TIME) + ".xml". 
      hDSChanges:WRITE-XML("FILE", SPARAXML). 
      
      RUN SparaProDataSetKalkylDS IN AppServerHandle(INPUT DATASET-HANDLE hDSChanges).
      hDSChanges:MERGE-CHANGES(DATASET KalkylDS:HANDLE).
      
      RUN KalkTracking(TRUE).              
      FIND FIRST kalkhuvtt  WHERE NO-LOCK NO-ERROR.
   
END PROCEDURE.   
PROCEDURE KalkTracking :
   DEFINE INPUT  PARAMETER onoff AS LOGICAL NO-UNDO.
      
      TEMP-TABLE kalknumtt:TRACKING-CHANGES = onoff.
      TEMP-TABLE kalknumsubtt:TRACKING-CHANGES = onoff.
      TEMP-TABLE kalkhuvtt:TRACKING-CHANGES = onoff.
      TEMP-TABLE kalkaonrTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE kalkfaktorertt:TRACKING-CHANGES = onoff.
      TEMP-TABLE kalkegnaprisertt:TRACKING-CHANGES = onoff.
      TEMP-TABLE kalktmtrlTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE kalkttidlageTT:TRACKING-CHANGES = onoff.
                
       
END PROCEDURE.
PROCEDURE RaknaEnKod :
   DEFINE INPUT  PARAMETER allakoder AS LOGICAL NO-UNDO.
   DEFINE VARIABLE fritotkostvar AS DECIMAL NO-UNDO.
   DEFINE VARIABLE totkostvar AS DECIMAL NO-UNDO.
      fritotkostvar = 0.
      totkostvar = 0.
      EMPTY TEMP-TABLE kalkantal NO-ERROR. 
      EMPTY TEMP-TABLE kalkkostnad NO-ERROR.     
      IF allakoder = FALSE THEN DO:
         IF NOT AVAILABLE kalknumtt THEN DO:
            FIND FIRST kalknumtt WHERE kalknumtt.NUM = kalknumsubtt.NUM NO-ERROR. 
         END.
         OPEN QUERY ksq FOR EACH kalknumsubtt WHERE kalknumsubtt.NUM = kalknumtt.NUM NO-LOCK.
         GET FIRST ksq NO-LOCK.     
      END.
      ELSE DO:
         OPEN QUERY ksq FOR EACH kalknumsubtt NO-LOCK.
         GET FIRST ksq NO-LOCK.
      END.    
      DO WHILE AVAILABLE(kalknumsubtt):
         IF allakoder = TRUE THEN DO:
            FIND FIRST kalknumtt WHERE kalknumtt.NUM = kalknumsubtt.NUM NO-ERROR.  
         END.
                 
         CREATE kalkantal.
         /*kalkberin*/
         BUFFER-COPY kalknumtt TO kalkantal.
         CREATE kalkkostnad.
         BUFFER-COPY kalknumtt TO kalkkostnad.
         ASSIGN
         kalkkostnad.FRITOTKOST = 0
         kalkkostnad.TOTKOST = 0.
         FIND FIRST kalkylprisertt WHERE kalkylprisertt.KLOGSUBID = kalknumtt.KLOGSUBID AND 
         kalkylprisertt.KPID = kalknumsubtt.KPID NO-LOCK NO-ERROR.
         FIND FIRST kalkvisningtt WHERE kalkvisningtt.KVID = kalkylprisertt.KVID NO-LOCK NO-ERROR.
         IF AVAILABLE kalkvisningtt THEN DO:    
            ASSIGN 
            kalkantal.TIMTYP = kalkvisningtt.TIMTYP
            kalkantal.KPID = kalkylprisertt.KPID
            kalkantal.KVID = kalkylprisertt.KVID.   
            IF allakoder = TRUE THEN DO:           
               kalkantal.SUMMA = kalkantal.SUMMA + kalknumsubtt.TIMMAR * kalknumtt.ANTAL.
               kalkantal.FRISUMMA = kalkantal.FRISUMMA + kalknumsubtt.FRITIMMAR * kalknumtt.ANTAL.
            END.
            ELSE DO:
               kalkantal.SUMMA = kalkantal.SUMMA + kalknumsubtt.TIMMAR * kalknumtt.ANTAL.
               kalkantal.FRISUMMA = kalkantal.FRISUMMA + kalknumsubtt.FRITIMMAR * kalknumtt.ANTAL.
            END.      
            ASSIGN 
            kalkkostnad.KOSTTYP = kalkvisningtt.KOSTTYP
            kalkkostnad.KPID = kalkylprisertt.KPID
            kalkkostnad.KVID = kalkylprisertt.KVID. 
                  kalkkostnad.TOTKOST = kalkkostnad.TOTKOST + (kalknumsubtt.TIMMAR * kalknumtt.ANTAL * kalkylprisertt.PRIS) 
                  + (kalknumtt.ANTAL * kalknumsubtt.KOSTNAD) + kalknumsubtt.AVRUND.
                  kalkkostnad.FRITOTKOST = kalkkostnad.FRITOTKOST + (kalknumsubtt.FRITIMMAR * kalknumtt.ANTAL * kalknumsubtt.FRIPRIS) 
                  + (kalknumtt.ANTAL * kalknumsubtt.FRIKOSTNAD) + kalknumsubtt.FRIAVRUND.
                
            
            totkostvar = totkostvar + kalkkostnad.TOTKOST.
           
           
            fritotkostvar = fritotkostvar + kalkkostnad.FRITOTKOST.
         END.
         FIND FIRST kalkylprisertt WHERE kalkylprisertt.KLOGSUBID = kalknumtt.KLOGSUBID AND kalkylprisertt.KVID = 0 AND kalkylprisertt.SOKBENAMNING = "RÖRLIGKOSTNAD EA"  NO-LOCK NO-ERROR.
         IF AVAILABLE kalkylprisertt THEN DO:            
            IF kalkantal.KVID = 2 THEN DO:
               ASSIGN
               kalkkostnad.FRIEAMANGD = kalkantal.FRISUMMA
               kalkkostnad.EAMANGD = kalkantal.SUMMA.
            END.
            IF kalkantal.KVID = 3 OR kalkantal.KVID = 6 OR kalkantal.KVID = 7 THEN DO:
               ASSIGN
               kalkkostnad.FRIEAMANGD = kalkkostnad.FRITOTKOST / kalkylprisertt.PRIS
               kalkkostnad.EAMANGD = kalkkostnad.TOTKOST / kalkylprisertt.PRIS.
            END.                                
         END.
         ELSE DO:
            ASSIGN
            kalkkostnad.EAMANGD  = 0
            kalkkostnad.FRIEAMANGD = 0.        
         END.         
         GET NEXT ksq NO-LOCK.
      END.            
      IF allakoder = FALSE THEN DO:
          
         /*problem ccc*/
         ASSIGN
         kalknumtt.FRITOTKOST = fritotkostvar
         kalknumtt.TOTKOST = totkostvar.
         
         EMPTY TEMP-TABLE kalkantal NO-ERROR.
         EMPTY TEMP-TABLE kalkkostnad NO-ERROR.       
      END.  
      
  
END PROCEDURE.


