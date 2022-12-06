
/*------------------------------------------------------------------------
    File        : GPLEXPORT.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed May 25 12:54:05 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/


DEFINE TEMP-TABLE Metadata NO-UNDO
   FIELD Verksamhetstyp AS CHARACTER.
   
DEFINE TEMP-TABLE Skapad NO-UNDO
   /*NAMESPACE-URI "se/skatteverket/ke/pl/1.1" */
   FIELD TidpunktSkapad AS DATETIME
   FIELD SkapadAvMjukvara AS CHARACTER 
   FIELD Mjukvaruversion AS CHARACTER 
      XML-DATA-TYPE "float" 
   FIELD Metadata_id AS RECID 
      XML-NODE-TYPE "HIDDEN" .
      
DEFINE TEMP-TABLE FilterBygg NO-UNDO
   /*NAMESPACE-URI "se/skatteverket/ke/pl/1.1" */
   FIELD Metadata_id AS RECID 
      XML-NODE-TYPE "HIDDEN" .
      
DEFINE TEMP-TABLE Period NO-UNDO
   /*NAMESPACE-URI "se/skatteverket/ke/pl/1.1" */
   FIELD Fromm AS DATETIME XML-NODE-NAME "From"
   FIELD Tom AS DATETIME
   FIELD FilterBygg_id AS RECID 
      XML-NODE-TYPE "HIDDEN" .

DEFINE TEMP-TABLE ArbetsplatsBygg NO-UNDO
   /*NAMESPACE-URI "se/skatteverket/ke/pl/1.1" */
   FIELD Identifikationsnummer AS CHARACTER INITIAL "000"
      XML-NODE-TYPE "ATTRIBUTE" .

DEFINE TEMP-TABLE Byggherre NO-UNDO
   /*NAMESPACE-URI "se/skatteverket/ke/pl/1.1" */
   FIELD Namn AS CHARACTER 
   FIELD Organisationsnummer AS CHARACTER 
   FIELD ArbetsplatsBygg_id AS RECID 
      XML-NODE-TYPE "HIDDEN" .

DEFINE TEMP-TABLE Aktivitetslogg NO-UNDO
   /*NAMESPACE-URI "se/skatteverket/ke/pl/1.1" */
   FIELD ArbetsplatsBygg_id AS RECID 
      XML-NODE-TYPE "HIDDEN" .

DEFINE TEMP-TABLE Aktivitet NO-UNDO
   /*NAMESPACE-URI "se/skatteverket/ke/pl/1.1" */
   FIELD AktivitetsID AS INTEGER 
      XML-DATA-TYPE "byte" XML-NODE-TYPE "ATTRIBUTE" 
   FIELD Loggtidpunkt AS DATETIME
   FIELD Kommentar AS CHARACTER 
   FIELD Aktivitetslogg_id AS RECID 
      XML-NODE-TYPE "HIDDEN" .

DEFINE TEMP-TABLE Rattelse NO-UNDO
   /*NAMESPACE-URI "se/skatteverket/ke/pl/1.1" */
   FIELD RattarAktivitet AS INTEGER 
      XML-DATA-TYPE "byte" 
   FIELD Rattelsetyp AS CHARACTER 
   FIELD Aktivitet_id AS RECID 
      XML-NODE-TYPE "HIDDEN" .

DEFINE TEMP-TABLE UtfordAv NO-UNDO
   /*NAMESPACE-URI "se/skatteverket/ke/pl/1.1" */
   FIELD Namn AS CHARACTER 
   FIELD Identitetsnummer AS CHARACTER INITIAL "000"
   FIELD Rattelse_id AS RECID 
      XML-NODE-TYPE "HIDDEN" .

DEFINE TEMP-TABLE Byggverksamhetsbedrivare NO-UNDO
   /*NAMESPACE-URI "se/skatteverket/ke/pl/1.1" */
   FIELD Namn AS CHARACTER 
   FIELD Organisationsnummer AS CHARACTER 
   FIELD FilterBygg_id AS RECID 
      XML-NODE-TYPE "HIDDEN" 
   FIELD Aktivitet_id AS RECID 
      XML-NODE-TYPE "HIDDEN" .

DEFINE TEMP-TABLE VerksamPerson NO-UNDO
   /*NAMESPACE-URI "se/skatteverket/ke/pl/1.1" */
   FIELD Namn AS CHARACTER 
   FIELD Identitetsnummer AS CHARACTER INITIAL "000"
   FIELD FilterBygg_id AS RECID 
      XML-NODE-TYPE "HIDDEN" 
   FIELD Aktivitet_id AS RECID 
      XML-NODE-TYPE "HIDDEN" .

DEFINE TEMP-TABLE Arbetspass NO-UNDO
   /*NAMESPACE-URI "se/skatteverket/ke/pl/1.1" */
   FIELD Sluttidpunkt AS DATETIME
   FIELD Starttidpunkt AS DATETIME
   FIELD Aktivitet_id AS RECID 
      XML-NODE-TYPE "HIDDEN" .

DEFINE DATASET Personalliggare NAMESPACE-URI "se/skatteverket/ke/pl/1.1" NAMESPACE-PREFIX "pl"
   FOR Metadata, Skapad, FilterBygg, Period, ArbetsplatsBygg, Byggherre, Aktivitetslogg, Aktivitet, Rattelse, UtfordAv, Byggverksamhetsbedrivare, VerksamPerson, Arbetspass
   PARENT-ID-RELATION RELATION1 FOR Metadata, Skapad
      PARENT-ID-FIELD Metadata_id
      PARENT-FIELDS-BEFORE (Verksamhetstyp)
   PARENT-ID-RELATION RELATION10 FOR Metadata, FilterBygg
      PARENT-ID-FIELD Metadata_id   
   PARENT-ID-RELATION RELATION11 FOR FilterBygg, Period
      PARENT-ID-FIELD FilterBygg_id
   PARENT-ID-RELATION RELATION12 FOR FilterBygg, VerksamPerson
      PARENT-ID-FIELD FilterBygg_id
   PARENT-ID-RELATION RELATION13 FOR FilterBygg, Byggverksamhetsbedrivare
      PARENT-ID-FIELD FilterBygg_id
   PARENT-ID-RELATION RELATION2 FOR ArbetsplatsBygg, Byggherre
      PARENT-ID-FIELD ArbetsplatsBygg_id
   PARENT-ID-RELATION RELATION3 FOR Rattelse, UtfordAv
      PARENT-ID-FIELD Rattelse_id
   PARENT-ID-RELATION RELATION4 FOR Aktivitet, Rattelse
      PARENT-ID-FIELD Aktivitet_id   
      PARENT-FIELDS-BEFORE (AktivitetsID, Loggtidpunkt)
   PARENT-ID-RELATION RELATION5 FOR Aktivitet, Byggverksamhetsbedrivare
      PARENT-ID-FIELD Aktivitet_id
   PARENT-ID-RELATION RELATION6 FOR Aktivitet, VerksamPerson
      PARENT-ID-FIELD Aktivitet_id
   PARENT-ID-RELATION RELATION7 FOR Aktivitet, Arbetspass
      PARENT-ID-FIELD Aktivitet_id 
      PARENT-FIELDS-AFTER (Kommentar)
   PARENT-ID-RELATION RELATION8 FOR Aktivitetslogg, Aktivitet
      PARENT-ID-FIELD Aktivitetslogg_id
   PARENT-ID-RELATION RELATION9 FOR ArbetsplatsBygg, Aktivitetslogg
      PARENT-ID-FIELD ArbetsplatsBygg_id.
