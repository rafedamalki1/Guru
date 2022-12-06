
/*------------------------------------------------------------------------
    File        : ExcelDS.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Jan 03 11:28:39 CET 2014
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE fakturaexcelTT NO-UNDO
   FIELD FAKTNR AS INTEGER
   FIELD FAKTNUMMER AS CHARACTER
   FIELD FAKTDELNUMMER AS CHARACTER
   FIELD FAKTNAMN AS CHARACTER
   FIELD OMRADE AS CHARACTER
   FIELD FAKTADRESS AS CHARACTER /*ev*/
   FIELD FAKTPOSTNR AS CHARACTER /*ev*/
   FIELD FAKTORT AS CHARACTER /*ev*/
   FIELD FORFALLDATUM AS CHARACTER /*ev*/
   FIELD AVDELNINGSNAMN AS CHARACTER /*ev*/
   FIELD BESTALLARE AS CHARACTER /*ev*/
   FIELD BESTNAMN AS CHARACTER /*ev*/
   FIELD FAKTDATUM AS CHARACTER
   FIELD VARREF AS CHARACTER
   FIELD ERREF AS CHARACTER
   FIELD MOMS AS CHARACTER
   FIELD SUMMA AS CHARACTER /*nettosumma*/
   FIELD SLUTSUMMA AS CHARACTER /*bruttosumma*/
   FIELD VFAKTNR AS INTEGER
   FIELD BOKDATUM AS DATE
   
   FIELD EFORFALLDATUM AS DATE
   FIELD EFAKTDATUM AS DATE
   
   INDEX FAKTNUMMER FAKTNUMMER.
   
DEFINE TEMP-TABLE faktposterexcelTT NO-UNDO
   FIELD BESKIVNING AS CHARACTER
   FIELD ANTAL AS CHARACTER
   FIELD PRIS AS CHARACTER
   FIELD SUMMA AS CHARACTER /*netto*/
   INDEX BESKIVNING BESKIVNING.
   
DEFINE DATASET FaktExcelDS FOR fakturaexcelTT,faktposterexcelTT.