/**************************************************************************
    Procedure: acapi.i

    Purpose  : Actuate API definitions

    Notes    :
    Author   : Gerry Seidl
    Date     : 6/30/97
    
    Modified :
      01/13/97 - changed dll reference to a preprocessor
      07/31/97 - added variables and calls to LoadLibrary/FreeLibrary.
    
**************************************************************************/
{samples/actuate/acpproc.i} /* Actuate API preprocessor values */

/* Requester API DLLs
 * NOTE: The name of the requester dll in v3 is different than the
 *       one in v2. This version of acapi.i uses the dll from v2 by
 *       default. If you are running with Actuate v3 then comment out 
 *       the v2 dll and uncomment the one for v3.
 */
/* Version 2 requester DLL */
&GLOBAL-DEFINE ReqDLL "reqst32.dll"

/* Version 3 requester DLL */
/*&GLOBAL-DEFINE ReqDLL "acrq3041.dll"*/

/* Variables needed for API calls */
DEFINE VARIABLE hAPIDll  AS INTEGER NO-UNDO. /* Module handle of loaded reqst32.dll */
DEFINE VARIABLE hSession AS INTEGER NO-UNDO. /* Session ID for Actuate API */

/***************************/
/* Actuate API definitions */
/***************************/
PROCEDURE AcReqInitialize EXTERNAL {&ReqDLL}:U :
  DEFINE RETURN PARAMETER hSession AS LONG.
END.

PROCEDURE AcReqUnInitialize EXTERNAL {&ReqDLL}:U :
  DEFINE INPUT  PARAMETER hSession AS LONG.
  DEFINE RETURN PARAMETER rc       AS UNSIGNED-SHORT.
END.

PROCEDURE AcReqSetEUDTPath EXTERNAL {&ReqDLL}:U :
  DEFINE INPUT PARAMETER pathName AS CHARACTER.
END.

PROCEDURE AcReqPrintReport EXTERNAL {&ReqDLL}:U :
  DEFINE INPUT  PARAMETER filename AS CHARACTER.
  DEFINE INPUT  PARAMETER options  AS LONG.
  DEFINE RETURN PARAMETER rc       AS LONG.
END.

PROCEDURE AcReqSetPrinterName EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT PARAMETER printerName AS CHARACTER.
END.

PROCEDURE AcReqSetPrinterNumberCopies EXTERNAL {&ReqDLL}:U :
  DEFINE INPUT PARAMETER numCopies AS LONG.
END.

PROCEDURE AcReqSetPrinterOrientation EXTERNAL {&ReqDLL}:U :
  DEFINE INPUT PARAMETER orientation AS LONG. /* 1=portait, 2=landscape */
END.

PROCEDURE AcReqSetPrinterScale EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT PARAMETER scale AS LONG. /* percentage of orig size at which to print */
END.

PROCEDURE AcReqSetPrinterColor EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT PARAMETER printerColor AS LONG. /* boolean - 1=yes, 0=no */
END.

PROCEDURE AcReqSetPrinterDuplex EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT PARAMETER duplexMode AS LONG. /* 1=simplex mode, 2=horiz, 3=vert */
END.

PROCEDURE AcReqSetPrinterCollate EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT  PARAMETER collate AS LONG. /* boolean - 1=yes, 0=no */
END.

PROCEDURE AcReqSetPrinterFormName EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT PARAMETER formName AS CHARACTER.
END.

PROCEDURE AcReqSetPrinterPaperSize EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT PARAMETER size   AS LONG. /* value 1 to 68 */
  DEFINE INPUT PARAMETER length AS LONG. /* length of paper in 10ths of a mm (e.g. 2794) */
  DEFINE INPUT PARAMETER width  AS LONG. /* width of paper in 10ths of a mm (e.g. 2159) */
END.

PROCEDURE AcReqSetDefaultPrinter EXTERNAL {&ReqDLL}:U : 
  /* No parameters */
END.

PROCEDURE AcReqReadFile EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT  PARAMETER fileName   AS CHARACTER.
  DEFINE RETURN PARAMETER fileNumber AS LONG.
END.

PROCEDURE AcReqCloseFile EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT PARAMETER fileNumber AS LONG.
END.

PROCEDURE AcReqGetFirstGroup EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT  PARAMETER fileNumber AS LONG.
  DEFINE RETURN PARAMETER name       AS MEMPTR.
END.

PROCEDURE AcReqGetNextGroup EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT  PARAMETER fileNumber AS LONG.
  DEFINE RETURN PARAMETER name       AS MEMPTR.
END.

PROCEDURE AcReqGetFirstParameter EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT  PARAMETER fileNumber AS LONG.
  DEFINE INPUT  PARAMETER parmGroup  AS CHARACTER.
  DEFINE RETURN PARAMETER parmname   AS MEMPTR.
END.

PROCEDURE AcReqGetNextParameter EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT  PARAMETER fileNumber AS LONG.
  DEFINE INPUT  PARAMETER parmGroup  AS CHARACTER.
  DEFINE RETURN PARAMETER parmname   AS MEMPTR.
END.

PROCEDURE AcReqGetAlias EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT  PARAMETER fileNumber AS LONG.
  DEFINE INPUT  PARAMETER parmName   AS CHARACTER.
  DEFINE RETURN PARAMETER val        AS MEMPTR.
END.

PROCEDURE AcReqGetRequired EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT  PARAMETER fileNumber AS LONG.
  DEFINE INPUT  PARAMETER parmName   AS CHARACTER.
  DEFINE RETURN PARAMETER val        AS SHORT.
END.

PROCEDURE AcReqGetHidden EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT  PARAMETER fileNumber AS LONG.
  DEFINE INPUT  PARAMETER parmName   AS CHARACTER.
  DEFINE RETURN PARAMETER val        AS SHORT.
END.

PROCEDURE AcReqGetHideText EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT  PARAMETER fileNumber AS LONG.
  DEFINE INPUT  PARAMETER parmName   AS CHARACTER.
  DEFINE RETURN PARAMETER val        AS SHORT.
END.

PROCEDURE AcReqGetAdhoc EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT  PARAMETER fileNumber AS LONG.
  DEFINE INPUT  PARAMETER parmName   AS CHARACTER.
  DEFINE RETURN PARAMETER val        AS SHORT.
END.

PROCEDURE AcReqGetParmType EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT  PARAMETER fileNumber AS LONG.
  DEFINE INPUT  PARAMETER parmName   AS CHARACTER.
  DEFINE RETURN PARAMETER val        AS MEMPTR.
END.

PROCEDURE AcReqGetValueString EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT  PARAMETER fileNumber AS LONG.
  DEFINE INPUT  PARAMETER parmName   AS CHARACTER.
  DEFINE RETURN PARAMETER val        AS MEMPTR.
END.

PROCEDURE AcReqGetValueInteger EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT  PARAMETER fileNumber AS LONG.
  DEFINE INPUT  PARAMETER parmName   AS CHARACTER.
  DEFINE RETURN PARAMETER val        AS LONG.
END.

PROCEDURE AcReqGetValueCurrency EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT  PARAMETER fileNumber AS LONG.
  DEFINE INPUT  PARAMETER parmName   AS CHARACTER.
  DEFINE RETURN PARAMETER val        AS MEMPTR.
END.

PROCEDURE AcReqGetValueDate EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT  PARAMETER fileNumber AS LONG.
  DEFINE INPUT  PARAMETER parmName   AS CHARACTER.
  DEFINE RETURN PARAMETER val        AS MEMPTR.
END.

PROCEDURE AcReqGetValueDouble EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT  PARAMETER fileNumber AS LONG.
  DEFINE INPUT  PARAMETER parmName   AS CHARACTER.
  DEFINE RETURN PARAMETER val        AS DOUBLE.
END.

PROCEDURE AcReqSetValueString EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT  PARAMETER fileNumber AS LONG.
  DEFINE INPUT  PARAMETER parmName   AS CHARACTER.
  DEFINE INPUT  PARAMETER val        AS CHARACTER.
END.

PROCEDURE AcReqSetValueInteger EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT  PARAMETER fileNumber AS LONG.
  DEFINE INPUT  PARAMETER parmName   AS CHARACTER.
  DEFINE INPUT  PARAMETER val        AS LONG.
END.

PROCEDURE AcReqSetValueCurrency EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT  PARAMETER fileNumber AS LONG.
  DEFINE INPUT  PARAMETER parmName   AS CHARACTER.
  DEFINE INPUT  PARAMETER val        AS CHARACTER.
END.

PROCEDURE AcReqSetValueDate EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT  PARAMETER fileNumber AS LONG.
  DEFINE INPUT  PARAMETER parmName   AS CHARACTER.
  DEFINE INPUT  PARAMETER val        AS CHARACTER.
END.

PROCEDURE AcReqSetValueDouble EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT  PARAMETER fileNumber AS LONG.
  DEFINE INPUT  PARAMETER parmName   AS CHARACTER.
  DEFINE INPUT  PARAMETER val        AS DOUBLE.
END.

PROCEDURE AcReqWriteFile EXTERNAL {&ReqDLL}:U :  
  DEFINE INPUT  PARAMETER FileNumber AS LONG.
  DEFINE INPUT  PARAMETER FileName   AS CHARACTER.
END.

PROCEDURE AcReqGenerateReport EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT PARAMETER filename   AS CHARACTER.
  DEFINE INPUT PARAMETER options    AS LONG.
  DEFINE INPUT PARAMETER fileNumber AS LONG.
  DEFINE RETURN PARAMETER rc        AS LONG.
END.

PROCEDURE AcReqGetValueStr EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT  PARAMETER fileNumber AS LONG.
  DEFINE INPUT  PARAMETER parmName   AS CHARACTER.
  DEFINE RETURN  PARAMETER val       AS MEMPTR.
END.

PROCEDURE AcReqGetError EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT  PARAMETER filenumber AS LONG.
  DEFINE RETURN PARAMETER val        AS LONG.
END.

PROCEDURE AcReqGetErrorString EXTERNAL {&ReqDLL}:U : 
  DEFINE INPUT  PARAMETER filenumber AS LONG.
  DEFINE RETURN PARAMETER msg        AS MEMPTR.
END.

/***************************/
/* Windows API definitions */
/***************************/
PROCEDURE LoadLibraryA EXTERNAL "KERNEL32.DLL":U :
    DEFINE INPUT  PARAMETER dllname AS CHARACTER.
    DEFINE RETURN PARAMETER hdll    AS LONG.
END.

PROCEDURE FreeLibrary EXTERNAL "KERNEL32.DLL":U :
    DEFINE INPUT PARAMETER hdll AS LONG.
END.    

/*********************************************/
/* Helper routines for calling into the APIs */
/*********************************************/

/* Routine to check for an Actuate API error and return it */
PROCEDURE CheckForAPIError:
  DEFINE INPUT  PARAMETER filenumber   AS INTEGER   NO-UNDO.
  DEFINE OUTPUT PARAMETER ErrorNumber  AS INTEGER   NO-UNDO.
  DEFINE OUTPUT PARAMETER ErrorMessage AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE pErrorMsg   AS MEMPTR    NO-UNDO.
  
  RUN AcReqGetError(INPUT filenumber, OUTPUT ErrorNumber).
  IF ErrorNumber > 0 THEN DO:
     SET-SIZE(pErrorMsg) = 128.
     RUN AcReqGetErrorString(INPUT filenumber, OUTPUT pErrorMsg).
     ErrorMessage = GET-STRING(pErrorMsg,1). 
     SET-SIZE(pErrorMsg) = 0.
  END.
END.

/* 
 * Function to wrap Actuate's initialize routine.
 * NOTE: We use LoadLibrary/FreeLibrary instead of the
 *       PERSISTENT options so that we have control of 
 *       when it gets released from memory.
 */
FUNCTION InitializeAPI RETURNS INTEGER:
  DEFINE VARIABLE dllname AS CHARACTER NO-UNDO.

  /* Find the location of the Actuate Developer Workbench in the Registry */
  LOAD "software\actuate\developer workbench":U BASE-KEY "HKEY_LOCAL_MACHINE":U.
  USE "software\actuate\developer workbench":U.
  GET-KEY-VALUE SECTION "File Path":U KEY DEFAULT VALUE dllname.
  UNLOAD "software\actuate\developer workbench":U.
  USE "":U.

  /* Construct the name either from the Registry or from a default */
  IF dllname NE ? THEN 
    dllname = REPLACE(dllname,"designer.exe":U,{&ReqDLL}:U).
  ELSE
    dllname = "C:\Actuate\Devwb\BIN\":U + {&ReqDLL}:U.
    
  /* Load the DLL into Windows */
  RUN LoadLibraryA(INPUT dllname, OUTPUT hAPIDll).
  IF hAPIDll > -1 AND hAPIDll < 31 THEN DO:
    MESSAGE "Error loading " + dllname VIEW-AS ALERT-BOX ERROR TITLE "Actuate API":U.
    RETURN -1.
  END.
  /*
   * Initializes the Actuate API and prepares the API for
   * use in your procedures
   */
  RUN AcReqInitialize(OUTPUT hSession).
  RETURN hSession.
END.

/* Function to wrap Actuate's uninitialize routine. */
FUNCTION UnInitializeAPI RETURNS LOGICAL (hSession AS INTEGER):
  DEFINE VARIABLE rc      AS INTEGER NO-UNDO.
  
  /* 
   * Run Actuate API call to close the Actuate API library
   * and free its resources 
   */
  RUN AcReqUnInitialize(INPUT hSession, OUTPUT rc).
  
  /* Remove Actuate DLL from memory */
  RUN FreeLibrary(INPUT hAPIDll).

  IF rc EQ 0 THEN DO:
    MESSAGE "An error occurred while removing the Actuate API from memory" 
      VIEW-AS ALERT-BOX ERROR TITLE "Actuate API":U.
    RETURN YES.
  END.
  ELSE RETURN NO.
END.

