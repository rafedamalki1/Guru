 
 /*------------------------------------------------------------------------
    File        : ProDataTest
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Tue May 29 15:15:02 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/
DEFINE VARIABLE AppServerHandle AS HANDLE NO-UNDO.
{KALKYLKAT.I}
{KALKYLPRODATABORT.I}	
	RUN KALKBORT.p PERSISTENT SET AppServerHandle.
   RUN LaddaKalkylBORT IN AppServerHandle ( INPUT 100339,INPUT "0910",OUTPUT DATASET KalkylBortDS).
   TEMP-TABLE kalknumtt:TRACKING-CHANGES = TRUE.
   TEMP-TABLE kalknumsubtt:TRACKING-CHANGES = TRUE.
   TEMP-TABLE kalkhuvtt:TRACKING-CHANGES = TRUE.
   TEMP-TABLE kalkhuvtt:TRACKING-CHANGES = TRUE.
   TEMP-TABLE kalknumtt:TRACKING-CHANGES = TRUE.
   TEMP-TABLE kalknumsubtt:TRACKING-CHANGES = TRUE.
   TEMP-TABLE kalkaonrTT:TRACKING-CHANGES = TRUE.
   TEMP-TABLE kalkfaktorertt:TRACKING-CHANGES = TRUE.
   TEMP-TABLE kalkegnaprisertt:TRACKING-CHANGES = TRUE.
   TEMP-TABLE kalktmtrlTT:TRACKING-CHANGES = TRUE.
   FOR EACH kalknumtt WHERE NO-LOCK:
      DELETE kalknumtt.
   END.
   RUN SparaKalkyl.
   PROCEDURE SparaKalkyl:
      DEFINE VARIABLE hDSChanges AS HANDLE NO-UNDO.
      TEMP-TABLE kalknumtt:TRACKING-CHANGES = FALSE.
      TEMP-TABLE kalknumsubtt:TRACKING-CHANGES = FALSE.
      TEMP-TABLE kalkhuvtt:TRACKING-CHANGES = FALSE.
      TEMP-TABLE kalkhuvtt:TRACKING-CHANGES = FALSE.
      TEMP-TABLE kalknumtt:TRACKING-CHANGES = FALSE.
      TEMP-TABLE kalknumsubtt:TRACKING-CHANGES = FALSE.
      TEMP-TABLE kalkaonrTT:TRACKING-CHANGES = FALSE.
      TEMP-TABLE kalkfaktorertt:TRACKING-CHANGES = FALSE.
      TEMP-TABLE kalkegnaprisertt:TRACKING-CHANGES = FALSE.
      TEMP-TABLE kalktmtrlTT:TRACKING-CHANGES = FALSE.
      CREATE DATASET hDSChanges.
      hDSChanges:CREATE-LIKE (DATASET KalkylBortDS:HANDLE).
      hDSChanges:GET-CHANGES (DATASET KalkylBortDS:HANDLE).
      hDSChanges:WRITE-XML("FILE", "C:\CTest.xml").
      RUN SparaProDataSetKalkylBortDS IN AppServerHandle(INPUT DATASET-HANDLE hDSChanges).
      hDSChanges:MERGE-CHANGES(DATASET KalkylBortDS:HANDLE).
   END PROCEDURE .
   /*

{C:\DELAD\pro9\guru\2Guru\Tester\prodatakalk.i}   
   RUN C:\DELAD\pro9\guru\2Guru\Tester\ProDataApp.p PERSISTENT SET AppServerHandle.
   RUN LaddaKalkyl IN AppServerHandle ( OUTPUT DATASET KalkylDS, INPUT 100284).
	run upp.
	run upp.
	run upp.
  PROCEDURE upp:
      TEMP-TABLE kalknumtt:TRACKING-CHANGES = TRUE.
   TEMP-TABLE kalknumsubtt:TRACKING-CHANGES = TRUE.
   TEMP-TABLE kalkhuvtt:TRACKING-CHANGES = TRUE. 
      find first kalknumtt.
       update kalknumtt.markning.
       run sparaKalkyl.
      FOR EACH kalknum WHERE kalknum.kalknr = 100343 NO-LOCK:
     
      display kalknum.arbkod kalknum.LOPNR kalknum.NUM kalknum.MARKNING .
         
         
         
      end.

   END PROCEDURE .
    
  PROCEDURE SparaKalkyl:
      DEFINE VARIABLE hDSChanges AS HANDLE NO-UNDO.
      tEMP-TABLE kalknumtt:TRACKING-CHANGES = FALSE.
   TEMP-TABLE kalknumsubtt:TRACKING-CHANGES = FALSE.
   TEMP-TABLE kalkhuvtt:TRACKING-CHANGES = FALSE.
      CREATE DATASET hDSChanges.
      hDSChanges:CREATE-LIKE (DATASET KalkylDS:HANDLE).
      hDSChanges:GET-CHANGES (DATASET KalkylDS:HANDLE).
      hDSChanges:WRITE-XML("FILE", "C:\CTest.xml").
    
      RUN SparaProDataSetkalkylds IN AppServerHandle(INPUT DATASET-HANDLE hDSChanges).
      hDSChanges:MERGE-CHANGES(DATASET KalkylDS:HANDLE).
   END PROCEDURE .
*/