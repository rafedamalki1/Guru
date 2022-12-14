/*
               KSV Editor
    Copyright: (C) 2000-2003 Serguey Klimoff (bulkl0DD)
     Filename: LEVERANT.I
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 2009.02.18 16:14 ELPAO   
     Modified: 2009.11.26 14:55 ELPAO    
     Modified: 2010.07.02 15:43 ELPAO    
     Modified: 
*/

 DEFINE TEMP-TABLE templeverant NO-UNDO
   /*FIELD huvudlev AS CHARACTER*/
   FIELD levkod AS CHARACTER
   FIELD levnamn AS CHARACTER
   INDEX levnamn levnamn.


 DEFINE  {&NEW} {&SHARED} TEMP-TABLE tempbeststat NO-UNDO
   FIELD AONR      AS CHARACTER
   FIELD DELNR     AS INTEGER
   FIELD OMRADE AS CHARACTER
   FIELD LEVKOD AS CHARACTER 
   FIELD BESTDATUM AS DATE
   FIELD BESTALLD AS CHARACTER 
   FIELD BERNR AS INTEGER
   FIELD ANVANDARE AS CHARACTER
   FIELD BESTNR AS INTEGER
   FIELD BESTTID AS INTEGER /* tillagt */
   FIELD BERDATUM AS DATE
   FIELD DEP-NR AS INTEGER
   FIELD INKBESTID AS INTEGER
   FIELD KOMMENTAR AS CHARACTER
   FIELD KUNDNR AS INTEGER
   
   FIELD LEVKODINK AS CHARACTER
   FIELD LEVNAMN       AS CHARACTER
   FIELD KLOCKAN       AS CHARACTER
   FIELD LEVERANSDATUM AS DATE         /*?NSKAT LEVDATUM*/ 
   FIELD SKICKATSTATUS AS CHARACTER    
   FIELD ORDERNUMMER    AS CHARACTER      /*levErans nummer*/
   FIELD INKANMNUMMER   AS CHARACTER      /*IA nummer*/
   FIELD INKORDERNUMMER AS CHARACTER      /*IO nummer*/
   FIELD LEVDATUM AS DATE
   FIELD PACKNING AS CHARACTER
   FIELD TOTALTPRIS AS DECIMAL.
    
    /*BESTSTAT.BESTNR*/
DEFINE TEMP-TABLE tempbeststatall NO-UNDO LIKE tempbeststat.


 DEFINE TEMP-TABLE tempbermtrl NO-UNDO
   FIELD ANTAL     AS INTEGER
   FIELD AONR      AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   FIELD BERLEV    AS CHARACTER
   FIELD BESTANT   AS INTEGER
   FIELD DATUM     AS DATE
   FIELD DBEST     AS CHARACTER
   FIELD DELNR     AS INTEGER
   FIELD DIAMETER  AS INTEGER
   FIELD ENHET     AS CHARACTER
   FIELD ENR       AS CHARACTER
   FIELD INKOP     AS LOGICAL
   FIELD KLAR      AS LOGICAL
   FIELD LEVKOD    AS CHARACTER
   FIELD LINKAB    AS LOGICAL
   FIELD MODUL     AS INTEGER
   FIELD MTRLTEXT  AS CHARACTER
   FIELD NUM       AS INTEGER
   FIELD OFFERT    AS LOGICAL
   FIELD OMRADE    AS CHARACTER
   FIELD OPRIS     AS DECIMAL
   FIELD PAR       AS INTEGER
   FIELD PRIS      AS DECIMAL FORMAT "->>>>9.99"
   FIELD SATS      AS LOGICAL
   FIELD SKAPMODUL AS INTEGER
   FIELD SKAPMTRL  AS LOGICAL
   FIELD SKAPNUM   AS INTEGER
   FIELD RABATT    AS LOGICAL
   FIELD TYPBER    AS LOGICAL
   FIELD PLPADRESS AS CHARACTER
   FIELD PROJLED AS CHARACTER
   FIELD INKBESTID AS INTEGER
   FIELD DEPNR AS INTEGER
   FIELD BESTNR AS INTEGER
   FIELD POSTNR AS CHARACTER
   FIELD LEVDATUM     AS DATE
   FIELD FARG AS CHARACTER
   FIELD PACKNING AS CHARACTER
   FIELD SUMPRIS AS DECIMAL
   INDEX ENR IS PRIMARY ENR 
   INDEX AONR AONR OMRADE ENR.
   
 DEFINE TEMP-TABLE tempbermtrl2 NO-UNDO LIKE tempbermtrl.
 DEFINE TEMP-TABLE tempbermtrls NO-UNDO LIKE tempbermtrl.
 DEFINE TEMP-TABLE tempbermtrlink NO-UNDO LIKE tempbermtrl.

 DEFINE TEMP-TABLE depbesttemp NO-UNDO
    FIELD DEPNR AS INTEGER
    FIELD bestnr AS INTEGER
    FIELD bestdatum AS DATE
    FIELD bestallare AS CHARACTER
    FIELD AONR AS CHARACTER
    FIELD DELNR AS INTEGER
    INDEX NR bestnr DESCENDING.    
