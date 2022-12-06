
/*------------------------------------------------------------------------
    File        : prinos.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Apr 02 12:39:35 CEST 2020
    Notes       :
  ----------------------------------------------------------------------*/

 
 
 
/* Sample Code */
DEFINE VARIABLE result AS INTEGER.
DEFINE VARIABLE oldPrinter AS CHARACTER.
 
DEFINE VARIABLE whand         AS HANDLE    NO-UNDO.
DEFINE VARIABLE file-to-print AS CHARACTER NO-UNDO INITIAL "version".
DEFINE VARIABLE p_FontNumber  AS INTEGER   NO-UNDO INITIAL 5.
DEFINE VARIABLE p_UseDialog   AS INTEGER   NO-UNDO INITIAL 0.
DEFINE VARIABLE p_PageSize    AS INTEGER   NO-UNDO INITIAL 20.
DEFINE VARIABLE p_PageCount   AS INTEGER   NO-UNDO INITIAL 0.
DEFINE VARIABLE p_Printed     AS LOGICAL   NO-UNDO.
 
ASSIGN whand = CURRENT-WINDOW.    
 
/* Get original printer settings */
RUN getKey ("Windows", "Device", ?, OUTPUT oldPrinter).
 
/* Modify this statement */
 RUN setKey ("Windows", "Device", "Scancloud VP,WinSpool,Ne01:").
 
SESSION:PRINTER-CONTROL-HANDLE = 0.
file-to-print = 'D:\elpool\faktE\skickade\Kraftringen4924.pdf'.
RUN adecomm\_osprint.p (INPUT  whand,
                        INPUT  file-to-print,
                        INPUT  p_FontNumber,                             
                        INPUT  p_UseDialog,
                        INPUT  p_PageSize,                            
                        INPUT  p_PageCount,                            
                        OUTPUT p_Printed). 
 
/* Reset to original printer settings */
RUN setKey ("Windows", "Device", oldPrinter).
 
PROCEDURE GetKey:
DEFINE INPUT PARAMETER pSection AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pEntry AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pDefault AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER pString AS CHAR NO-UNDO.
DEFINE VAR result AS INT NO-UNDO.
DEFINE VAR wbuf AS MEMPTR NO-UNDO.
 
SET-SIZE(wbuf) = 255.
RUN GetProfileStringA(pSection,pEntry,pDefault,wbuf,254,OUTPUT result).
IF result = 0 THEN
   pString = ?.
ELSE
   pString = GET-STRING(wbuf,1).
 
SET-SIZE(wbuf) = 0.
END PROCEDURE.
 
PROCEDURE SetKey:
DEFINE INPUT PARAMETER pSection AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pEntry AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pString AS CHAR NO-UNDO.
DEFINE VAR result AS INT NO-UNDO.
RUN WriteProfileStringA(pSection,pEntry,pString, OUTPUT result).
END PROCEDURE.
 
PROCEDURE GetProfileStringA EXTERNAL "KERNEL32.DLL":
DEFINE INPUT PARAMETER lpszSection AS CHAR. /* address of section    */
DEFINE INPUT PARAMETER lpszEntry AS CHAR.    /* address of entry    */
DEFINE INPUT PARAMETER lpszDefault AS CHAR.
DEFINE INPUT PARAMETER lpszReturnBuffer AS MEMPTR.
DEFINE INPUT PARAMETER cbReturnBuffer AS LONG.
DEFINE RETURN PARAMETER result AS LONG.
END.
 
PROCEDURE WriteProfileStringA EXTERNAL "KERNEL32.DLL":
DEFINE INPUT PARAMETER lpszSection AS CHAR.
DEFINE INPUT PARAMETER lpszEntry AS CHAR.   
DEFINE INPUT PARAMETER lpszString AS CHAR.
DEFINE RETURN PARAMETER result AS LONG.
END.
