
/*------------------------------------------------------------------------
    File        : BEREDTT.i    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : elpao
    Created     : Tue Jan 08 09:09:04 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/
   DEFINE TEMP-TABLE konsttempTT NO-UNDO LIKE konsttemp
      FIELD TTRECID AS RECID.
   DEFINE TEMP-TABLE bbenamntempTT NO-UNDO LIKE bbenamntemp
      FIELD TTRECID AS RECID. 
   DEFINE TEMP-TABLE konstvaltempTT NO-UNDO LIKE konstvaltemp
      FIELD TTRECID AS RECID.    
   DEFINE TEMP-TABLE andkonstvaltemp2TT NO-UNDO LIKE konstvaltempTT.  
   DEFINE TEMP-TABLE andkonstvaltemp3TT NO-UNDO LIKE konstvaltempTT.
   DEFINE TEMP-TABLE andkonstvaltemp4TT NO-UNDO LIKE konstvaltempTT.
   DEFINE TEMP-TABLE andkonstvaltemp5TT NO-UNDO LIKE konstvaltempTT.
   DEFINE TEMP-TABLE andkonstvaltemp6TT NO-UNDO LIKE konstvaltempTT.     
   DEFINE TEMP-TABLE upp_mtrlTT NO-UNDO
      FIELD KTYPKOD AS CHARACTER
      FIELD ENR AS CHARACTER
      FIELD MODUL AS INTEGER 
      FIELD TYPBER AS LOGICAL
      FIELD BENAMNING AS CHARACTER
      FIELD LEVKOD AS CHARACTER
      FIELD ANTAL AS INTEGER
      FIELD F1 AS CHARACTER
      FIELD F2 AS CHARACTER
      FIELD F3 AS CHARACTER
      FIELD F4 AS CHARACTER
      FIELD F5 AS CHARACTER
      FIELD KUND AS LOGICAL
      FIELD ENHET AS CHARACTER
      FIELD PRIS AS DECIMAL
      FIELD TTRECID AS RECID
      FIELD LISTAKUNDLEV AS LOGICAL. 
   DEFINE TEMP-TABLE ber_mtrlTT NO-UNDO
      FIELD NUM AS INTEGER
      FIELD SKAPNUM AS INTEGER
      FIELD ENR AS CHARACTER
      FIELD LEVKOD AS CHARACTER.   
      