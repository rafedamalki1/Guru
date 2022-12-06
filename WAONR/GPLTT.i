
/*------------------------------------------------------------------------
    File        : GPLTT.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Jul 05 09:38:13 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/
   DEFINE TEMP-TABLE gplrattelsett NO-UNDO
   FIELD PLAID AS INTEGER INITIAL "0"
   FIELD LOGGTID AS DATETIME-TZ
   FIELD ANVANDARE AS CHARACTER
   FIELD SKAPADAV AS CHARACTER
   FIELD SKAPADI AS CHARACTER
   FIELD STARTTID AS DATETIME-TZ
   FIELD SLUTTID AS DATETIME-TZ
   FIELD RATTELSE AS LOGICAL INITIAL "no"
   FIELD RATTELSETYP AS CHARACTER
   FIELD RATTELSEUTFORARE AS CHARACTER
   FIELD RPLAID AS CHARACTER
   FIELD PLID AS INTEGER INITIAL "0"
   FIELD KOMMENTAR AS CHARACTER
   FIELD PERSONALKOD AS CHARACTER
   FIELD TYP AS CHARACTER.
   