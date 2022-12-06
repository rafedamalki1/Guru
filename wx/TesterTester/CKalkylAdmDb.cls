 
 /*------------------------------------------------------------------------
    File        : KalkylAdmDb
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Mon Jan 07 14:13:22 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS Modules.Kalkyl.CKalkylAdmDb: 
   DEFINE PUBLIC VARIABLE ArbetskoderTTh        AS HANDLE NO-UNDO.
   DEFINE TEMP-TABLE kalkylarbkodertt NO-UNDO
   BEFORE-TABLE kalkylarbkoderttbef
   FIELD KLOGSUBID AS INTEGER
   FIELD TYPKALK AS INTEGER
   FIELD ARBKOD AS CHARACTER 
   FIELD BENAMNING AS CHARACTER
   FIELD REGION AS LOGICAL
   FIELD OPTO AS LOGICAL
   FIELD MARKNING AS CHARACTER
   FIELD MARKSUB AS CHARACTER
   FIELD TTRECID AS RECID
   INDEX KLOGSUBID KLOGSUBID
   INDEX TYPKALK IS PRIMARY KLOGSUBID TYPKALK ARBKOD
   INDEX ARBKOD  KLOGSUBID ARBKOD 
   INDEX ARBKODID ARBKOD KLOGSUBID.
   
  
   DEFINE PUBLIC  PROPERTY AppServerHandle AS HANDLE NO-UNDO
   PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC  PROPERTY KalkylKodTot AS DECIMAL NO-UNDO
   PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC VARIABLE Control               AS Modules.Kalkyl.CKalkylAdm NO-UNDO.
   DEFINE PUBLIC VARIABLE Root                  AS Guru.Root NO-UNDO.
   
   CONSTRUCTOR PUBLIC CKalkylAdmDb(INPUT r AS Guru.Root):
      THIS-OBJECT:Root = r.
   END CONSTRUCTOR.
  
   METHOD PUBLIC VOID InitializeTTs():
     THIS-OBJECT:ArbetskoderTTh = TEMP-TABLE kalkylarbkodertt:HANDLE:DEFAULT-BUFFER-HANDLE.    
   END METHOD.
   METHOD PUBLIC LOGICAL CFetchKoder():
      CREATE kalkylarbkodertt.
      ASSIGN
      kalkylarbkodertt.ARBKOD = "A"
      kalkylarbkodertt.KLOGSUBID = 1
      kalkylarbkodertt.TYPKALK = 2
      kalkylarbkodertt.BENAMNING  = " TYP A1"
      kalkylarbkodertt.TTRECID = RECID(kalkylarbkodertt).
      CREATE kalkylarbkodertt.
      ASSIGN
      kalkylarbkodertt.ARBKOD = "B"
      kalkylarbkodertt.KLOGSUBID = 1
      kalkylarbkodertt.TYPKALK = 2
      kalkylarbkodertt.BENAMNING  = " TYP B".
      kalkylarbkodertt.TTRECID = RECID(kalkylarbkodertt).
      CREATE kalkylarbkodertt.
      ASSIGN
      kalkylarbkodertt.ARBKOD = "C"
      kalkylarbkodertt.KLOGSUBID = 1
      kalkylarbkodertt.TYPKALK = 2
      kalkylarbkodertt.BENAMNING  = " TYP C".
      kalkylarbkodertt.TTRECID = RECID(kalkylarbkodertt).
      CREATE kalkylarbkodertt.
      ASSIGN
      kalkylarbkodertt.ARBKOD = "F"
      kalkylarbkodertt.KLOGSUBID = 1
      kalkylarbkodertt.TYPKALK = 2
      kalkylarbkodertt.BENAMNING  = " TYP F".
      kalkylarbkodertt.TTRECID = RECID(kalkylarbkodertt).
      CREATE kalkylarbkodertt.
      ASSIGN
      kalkylarbkodertt.ARBKOD = "D"
      kalkylarbkodertt.KLOGSUBID = 1
      kalkylarbkodertt.TYPKALK = 2
      kalkylarbkodertt.BENAMNING  = " TYP D".
      kalkylarbkodertt.TTRECID = RECID(kalkylarbkodertt).
      CREATE kalkylarbkodertt.
      ASSIGN
      kalkylarbkodertt.ARBKOD = "E"
      kalkylarbkodertt.KLOGSUBID = 1
      kalkylarbkodertt.TYPKALK = 2
      kalkylarbkodertt.BENAMNING  = " TYP E".
      kalkylarbkodertt.TTRECID = RECID(kalkylarbkodertt).
      RETURN TRUE.
   END METHOD.
   /*KOLL OM ARBKOD REDAN FINNS*/
   METHOD PUBLIC LOGICAL ArbKoderKoll(INPUT subkatnr AS INTEGER, INPUT arbkodvar AS CHARACTER, INPUT inttrec AS RECID):
      RETURN TRUE.
   END METHOD.
   
   DESTRUCTOR CKalkylAdmDb():
      IF VALID-HANDLE(THIS-OBJECT:AppServerHandle) THEN DELETE PROCEDURE THIS-OBJECT:AppServerHandle.
   END DESTRUCTOR.
END CLASS.