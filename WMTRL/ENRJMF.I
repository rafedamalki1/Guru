
/*------------------------------------------------------------------------
    File        : ENRJMF.I
    Purpose     : Guru.GlobalaVariabler:jmfPrisEnrTTh

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Feb 25 12:48:10 CET 2020
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE {&STATIC} TEMP-TABLE enrjmf NO-UNDO
   FIELD ORDNING AS INTEGER
   FIELD ENR AS CHARACTER
   FIELD LEVKOD AS CHARACTER
   FIELD ANTAL AS INTEGER
   FIELD OutputLEVKOD AS CHARACTER
   FIELD InputLEVKOD AS CHARACTER
   FIELD InputBENAMNING AS CHARACTER
   FIELD RETUR AS LOGICAL
   INDEX ENR ORDNING ENR.
