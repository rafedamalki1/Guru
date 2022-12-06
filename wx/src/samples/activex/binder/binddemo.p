/*
 * This sample demonstrates the use of the Microsoft Office 95/97 Binder as an  
 * ActiveX Automation Server.  It opens the Binder window and a Progress 
 * dialog-box which allows the user to combine Excel, Word and Powerpoint files
 * into the binder.
 */

DEF BUTTON bExit LABEL "Done" SIZE 20 BY 1.25 AUTO-GO.
DEF BUTTON bExcel LABEL "Excel:Sheet" SIZE 20 BY 1.25.
DEF BUTTON bPowerPoint LABEL "PPoint:Slide" SIZE 20 BY 1.25.
DEF BUTTON bWord LABEL "Word:Document" SIZE 20 BY 1.25.

DEF VAR objBinder AS COM-HANDLE.
DEF VAR objSection1 AS COM-HANDLE.
DEF VAR objSection2 AS COM-HANDLE.
DEF VAR objSection3 AS COM-HANDLE.
DEF VAR ver_flag AS INTEGER INIT 8.

FORM  Skip(1) bExcel SPACE bWord 
      Skip(1) bPowerPoint SPACE bExit 
      WITH FRAME a VIEW-AS DIALOG-BOX THREE-D FONT 6.
      
FRAME a:TITLE = "Binder OLE Server Demo".

/*
 * Checks registry to find out the version of the MS Binder Server 
 */
 
LOAD "Office.Binder.8" BASE-KEY "HKEY_CLASSES_ROOT" NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        ver_flag = 7.
    ELSE
        UNLOAD "Office.Binder.8". /* Close registry key */
        
CREATE "Office.Binder" objBinder.

IF VALID-HANDLE(objBinder) THEN
    objBinder:Visible = TRUE.


/*
 * TRIGGERS 
 */

/*
 * Add Excel:Sheet object to Binder:Section 
 */

ON CHOOSE OF bExcel 
DO:
    IF ver_flag <> 8 THEN   
        objSection1 = objBinder:Sections:Add("Excel.Sheet").
    ELSE 
        objSection1 = objBinder:Sections:Add("Excel.Sheet",,,).
      
    objSection1:Name = "ExcelSheet Object".
    objSection1:Update().
    IF ver_flag <> 8 THEN
        objSection1:Object:Range("A1:A3"):Value = "Hello Excel!!!".
    ELSE    
        objSection1:Object:Parent:Range("A1:A3"):Value = "Hello Excel!!!".    
    RELEASE OBJECT objSection1. 
END.

/*
 * Add PowerPoint:Slide object to Binder:Section 
 */

ON CHOOSE OF bPowerPoint  
DO:
    IF ver_flag <> 8 THEN DO: 
      objSection2 = objBinder:Sections:Add("PowerPoint.Slide").
      objSection2:Name = "PowerPoint Object".
      objSection2:Update().
      objSection2:Object:Objects:Item(1):Text = "New Slide".
      objSection2:Object:Objects:Item(2):Text = "New text". 
    END.   
    ELSE DO:
      objSection2 = objBinder:Sections:Add("PowerPoint.Show",,,).
      objSection2:Name = "PowerPoint Object".
      objSection2:Update().
      objSection2:Object:Slides:Item(1):Shapes:Item(1):TextFrame:TextRange:Text = "New Slide".
      objSection2:Object:Slides:Item(1):Shapes:Item(2):TextFrame:TextRange:Text = "New text".
      objSection2:Delete(). /*-- Just to display how to delete the existed section --*/
    END.
   
   RELEASE OBJECT objSection2. 
END.

/*
 * Add Word:Document object to Binder:Section 
 */

ON CHOOSE OF bWord
DO:
   DEF VAR x as COM-HANDLE.
   IF ver_flag <> 8 THEN 
      objSection3 = objBinder:Sections:Add("Word.Document").
   ELSE  
      objSection3 = objBinder:Sections:Add("Word.Document",,,).

    objSection3:Name = "Word Object".
    objSection3:Update().
    IF ver_flag = 8 THEN
        objSection3:Object:Range(0,0):InsertAfter("Hello Word !!!").
    RELEASE OBJECT objSection3. 
END.

ON CHOOSE OF bExit 
DO:
    IF VALID-HANDLE(objBinder) THEN DO:
       objBinder:Visible = FALSE.
       objBinder:SaveAs("MSDN.OBD",2). /*-- Overwrites file if it exists --*/
       
       RELEASE OBJECT objBinder.
    END.
END.


ENABLE ALL WITH FRAME a.
WAIT-FOR GO OF FRAME a.
