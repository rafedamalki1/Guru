 /*
   ActiveSheet.Unprotect
    ActiveSheet.Protect DrawingObjects:=False, Contents:=True, Scenarios:= True
    */
   DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
   
kommando = "c:\protemp9\elpao\skydd.xls".
/*EXECLIN.I*/

DEFINE VARIABLE varforetypchar AS CHARACTER NO-UNDO EXTENT 20.
DEFINE VARIABLE plusaonr AS CHARACTER NO-UNDO.
DEFINE VARIABLE plusdnr AS INTEGER NO-UNDO.
DEFINE new  SHARED VARIABLE dlcvar AS CHARACTER NO-UNDO.
DEFINE new  SHARED VARIABLE guruvar AS CHARACTER NO-UNDO.
 
DEFINE VARIABLE vvv AS HANDLE NO-UNDO.
{EXECLIN2.I}
    CREATE "Excel.Application" chExcelApplication.
   chExcelApplication:Visible = TRUE.
   chWorkbook = chExcelApplication:Workbooks:OPEN(kommando).  
   chWorkSheet = chExcelApplication:Sheets:Item(1).   
   chWorkSheet:EnableAutoFilter = True.
   chWorkSheet:Unprotect("123"). 
   chWorkSheet:Protect("421",YES,YES,YES,yes).
   /*
   chWorkSheet:Unprotect("123").  
   */  
   /*
   vvv = chWorkSheet:Unprotect.
   vvv:Password="123". 
   */
   /*
   chWorkSheet:EnableAutoFilter = True. 
    chWorkSheet:Protect:Password="123". 
   chWorkSheet:UserInterFaceOnly=True.
   */
                     /*
'sh.Protect Password:="123", Contents:=True, Scenarios:=True 
'ActiveSheet.Protect DrawingObjects:=False, Contents:=True, Scenarios:=False 
Next 
End Sub 
     
     chWorkSheet:EnableAutoFilter = True. 
     chWorkSheet:Protect:Password="123", UserInterFaceOnly=True.     
     */

/*
Sub unlockw() 
For Each sh In ThisWorkbook.Worksheets 
   If sh.name="Sheet1" Then 
      sh.Unprotect Password:="123" 
   End If 
Next sh 

End Sub 
 */
   

   /*
   chWorkSheet:Protect:(DrawingObjects =False),( Contents=True),( Scenarios= True). 
   */
  RUN slutexcel_UI.
