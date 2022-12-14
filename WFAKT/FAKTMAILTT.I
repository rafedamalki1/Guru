
/*------------------------------------------------------------------------
    File        : FAKTMAILTT.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Aug 11 17:00:37 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE TEMP-TABLE faktmail NO-UNDO
  FIELD FAKTNR AS INTEGER
  FIELD EPOST AS CHARACTER
  FIELD BESTNAMN AS CHARACTER
  FIELD BESKRIVNING AS CHARACTER
  FIELD EGETNAMN AS CHARACTER
  FIELD DATUM AS DATE
  FIELD VFAKTNR AS INTEGER
  FIELD VKREDIT AS INTEGER
  INDEX FAKTNR IS PRIMARY FAKTNR
  INDEX VFAKTNR VFAKTNR. 
DEFINE TEMP-TABLE faktkuvert NO-UNDO
  FIELD FAKTNR AS INTEGER
  FIELD EPOST AS CHARACTER
  FIELD BESTNAMN AS CHARACTER
  FIELD BESKRIVNING AS CHARACTER
  FIELD EGETNAMN AS CHARACTER
  FIELD DATUM AS DATE
  FIELD VFAKTNR AS INTEGER
  FIELD VKREDIT AS INTEGER
  INDEX FAKTNR IS PRIMARY FAKTNR
  INDEX VFAKTNR VFAKTNR.
DEFINE TEMP-TABLE Epeppol NO-UNDO
  FIELD FAKTNR AS INTEGER
  FIELD EPOST AS CHARACTER
  FIELD BESTNAMN AS CHARACTER
  FIELD BESKRIVNING AS CHARACTER
  FIELD EGETNAMN AS CHARACTER
  FIELD DATUM AS DATE
  FIELD VFAKTNR AS INTEGER
  FIELD VKREDIT AS INTEGER
  FIELD ORGNR AS CHARACTER
  FIELD GLN AS CHARACTER
  FIELD VAT AS CHARACTER
  FIELD BUYREF AS CHARACTER
  FIELD PEPPOL AS CHARACTER
  INDEX FAKTNR IS PRIMARY FAKTNR
  INDEX VFAKTNR VFAKTNR.  
DEFINE TEMP-TABLE Epeppolin NO-UNDO LIKE Epeppol.
  
  
