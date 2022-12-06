
/*------------------------------------------------------------------------
    File        : IFSidAnst.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Oct 23 10:01:24 CEST 2020
    Notes       :
  ----------------------------------------------------------------------*/
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{GLOBVAR2DEL1.I}



   

{EXECLIN2.I}
startc = "A".
slutc = "D".
DEFINE VARIABLE excelkommando AS CHARACTER NO-UNDO.
excelkommando = "d:\elpool\IFSID.xlsx".
CREATE "Excel.Application" chExcelApplication.
   ASSIGN 
   chWorkbook = chExcelApplication:Workbooks:OPEN(excelkommando) NO-ERROR.
   ASSIGN 
   chWorkSheet = chExcelApplication:Sheets:ITEM(1) NO-ERROR.
   ASSIGN 
   iRadslut = chWorksheet:Cells:SpecialCells(11):ROW NO-ERROR.
   MESSAGE iRadslut
   VIEW-AS ALERT-BOX.
   DO WHILE iRad <= iRadslut:
      iRad = iRad + 1.
      RUN readexcel_UI.
         FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = allacolumtext[1] NO-LOCK NO-ERROR.               
         IF AVAILABLE ANVANDARE THEN DO TRANSACTION:
            FIND FIRST PERSONALTAB   WHERE PERSONALTAB.PERSONALKOD = ANVANDARE.PERSONALKOD EXCLUSIVE-LOCK NO-ERROR. 
            IF AVAILABLE PERSONALTAB THEN DO:
               PERSONALTAB.ANSTNR = allacolumtext[3].
            END.   
            ELSE MESSAGE "P"  allacolumtext[1]  allacolumtext[3]
              VIEW-AS ALERT-BOX.
         END.
         ELSE MESSAGE "A"  allacolumtext[1]  allacolumtext[3]
              VIEW-AS ALERT-BOX.                              
         
   END.   
   RUN slutreadexcel_UI.
   RELEASE PERSONALTAB NO-ERROR.
  
   /*
   RUN ALLMANBYT.P (INPUT "PERSONALKOD", INPUT "ANSTNR",INPUT "PERSONALTAB",TRUE).
   */
   MESSAGE "BYT KLART"
   VIEW-AS ALERT-BOX.