/**************************************************************************
    Procedure: acprtrpt.p

    Purpose  : Calls the Actuate API to print a report (.ROI)

    Notes    :
    Author   : Gerry Seidl
    Date     : 6/30/97
    
    Modified on 7/31/97.
    
**************************************************************************/
/* INCLUDE FILES */
{samples/actuate/acapi.i} /* Actuate API definitions */

/* INPUT PARAMETERS */
DEFINE INPUT  PARAMETER pc_filename       AS CHARACTER NO-UNDO. /* filename of report (ROI) to print */
DEFINE INPUT  PARAMETER pi_options        AS INTEGER   NO-UNDO. /* options */
DEFINE INPUT  PARAMETER pc_lptname        AS CHARACTER NO-UNDO. /* printer to use */
DEFINE INPUT  PARAMETER pi_lptnumcopies   AS INTEGER   NO-UNDO. /* number of copies to print */
DEFINE INPUT  PARAMETER pi_lptorientation AS INTEGER   NO-UNDO. /* page orientation */
DEFINE INPUT  PARAMETER pi_lptscale       AS INTEGER   NO-UNDO. /* size of image to print */
DEFINE INPUT  PARAMETER pl_lptcolor       AS LOGICAL   NO-UNDO. /* turn color printing on or off */
DEFINE INPUT  PARAMETER pi_lptduplex      AS INTEGER   NO-UNDO. /* turn duplexing on or off */
DEFINE INPUT  PARAMETER pl_lptcollate     AS LOGICAL   NO-UNDO. /* turn collating on or off */
DEFINE INPUT  PARAMETER pc_lptFormName    AS CHARACTER NO-UNDO. /* paper type */
DEFINE INPUT  PARAMETER pi_lptPaperSize   AS INTEGER   NO-UNDO. /* paper size */
DEFINE INPUT  PARAMETER pi_lptPaperLength AS INTEGER   NO-UNDO. /* paper length */
DEFINE INPUT  PARAMETER pi_lptPaperWidth  AS INTEGER   NO-UNDO. /* paper width */
DEFINE INPUT  PARAMETER pl_defaultlpt     AS LOGICAL   NO-UNDO. /* revert to the system's default printer */

/* OUTPUT PARAMETERS */
DEFINE OUTPUT PARAMETER pi_Status         AS INTEGER   NO-UNDO. /* Error code */

/* LOCAL VARIABLE DEFINTIONS */
DEFINE VARIABLE rc                        AS INTEGER   NO-UNDO.

/* Check for a report file to print */
IF pc_filename EQ ? OR pc_filename EQ "" THEN RETURN "ERROR":U.

/* Initialize Actuate API */
hSession = InitializeAPI().
IF hSession = -1 THEN DO:
  pi_Status = -1.
  RETURN "ERROR":U. /* Could not load API, exit procedure */
END.
       
/* Set printer parameters */
IF pc_lptname NE ? AND pc_lptname NE ""
  THEN RUN AcReqSetPrinterName(INPUT pc_lptname).

IF pi_lptnumcopies NE ? THEN RUN AcReqSetPrinterNumberCopies(INPUT pi_lptnumcopies).

IF pi_lptorientation NE ? THEN RUN AcReqSetPrinterOrientation(INPUT pi_lptorientation).

IF pi_lptscale NE ? THEN RUN AcReqSetPrinterScale(INPUT pi_lptscale).

IF pl_lptcolor NE ? THEN RUN AcReqSetPrinterColor(INPUT 1).
ELSE RUN AcReqSetPrinterColor(INPUT 0).

IF pi_lptduplex NE ? THEN RUN AcReqSetPrinterDuplex(INPUT pi_lptduplex).

IF pl_lptcollate THEN RUN AcReqSetPrinterCollate(INPUT 1).
ELSE RUN AcReqSetPrinterCollate(INPUT 0).

IF pc_lptFormName NE ? AND pc_lptFormName NE ""
  THEN RUN AcReqSetPrinterFormName(INPUT pc_lptFormName).

IF pi_lptPaperSize NE 0 THEN RUN AcReqSetPrinterPaperSize(INPUT pi_lptPaperSize, INPUT 0, INPUT 0).
ELSE RUN AcReqSetPrinterPaperSize(INPUT 0, INPUT pi_lptPaperlength, INPUT pi_lptPaperWidth).

IF pl_defaultlpt THEN RUN AcReqSetDefaultPrinter.
         
/* Print the report */
RUN AcReqPrintReport(INPUT pc_filename, INPUT pi_options, OUTPUT pi_Status).

/* Shutdown the Actuate API */            
UnInitializeAPI(hSession).
RETURN.

