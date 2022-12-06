
/*------------------------------------------------------------------------
    File        : KALKYLEXTRATAB.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : elpao
    Created     : Thu Jan 17 12:24:56 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE TEMP-TABLE typtt NO-UNDO
      FIELD ATAT    AS CHARACTER
      FIELD TYPKALK AS INTEGER
      FIELD TYPC    AS CHARACTER LABEL "Kalkyltyp".
DEFINE TEMP-TABLE konvtyptt NO-UNDO
   FIELD ATAT    AS CHARACTER
   FIELD TYPKALK AS INTEGER
   FIELD TYPC    AS CHARACTER LABEL "Kalkyltyp".
   
DEFINE TEMP-TABLE ufpris NO-UNDO
   FIELD UTYP AS INTEGER
   FIELD TYPC AS CHARACTER LABEL "Pris".  

    