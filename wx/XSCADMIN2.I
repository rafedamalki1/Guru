/*
               KSV Editor
    Copyright: (C) 2000-2003 Serguey Klimoff (bulkl0DD)
     Filename: XSCADMIN2.I
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 2008.07.01 14:55 ELPAO   
     Modified: 
*/
{HDTEMP.I}
   /*
DEFINE {&NEW} {&SHARED} TEMP-TABLE hdpunkttemp NO-UNDO
   FIELD ZKORD AS DECIMAL 
   FIELD XKORD AS DECIMAL
   FIELD YKORD AS DECIMAL
   FIELD UPPLAG AS INTEGER
   FIELD PID AS INTEGER
   FIELD PFRITEXT AS CHARACTER
   FIELD ORDNING AS INTEGER
   FIELD OMRADE AS CHARACTER
   FIELD NUM AS INTEGER
   FIELD BERNR AS INTEGER.

DEFINE {&NEW} {&SHARED} TEMP-TABLE hdforlkabtemp NO-UNDO
   FIELD BERNR AS INTEGER
   FIELD FID AS INTEGER
   FIELD KLID AS INTEGER
   FIELD OMRADE AS CHARACTER 
   FIELD SID AS INTEGER.
                                         
DEFINE {&NEW} {&SHARED} TEMP-TABLE hdforlsamtemp NO-UNDO
   FIELD SID AS INTEGER
   FIELD OMRADE AS CHARACTER
   FIELD KOD AS CHARACTER
   FIELD FID AS INTEGER
   FIELD BERNR AS INTEGER
   FIELD ANTAL AS INTEGER.

DEFINE {&NEW} {&SHARED} TEMP-TABLE hdkabellinjetemp NO-UNDO
   FIELD START AS LOGICAL
   FIELD SLUT AS LOGICAL 
   FIELD PID AS INTEGER 
   FIELD ORDNING AS INTEGER 
   FIELD OMRADE AS CHARACTER
   FIELD MATARPUNKT AS LOGICAL
   FIELD LEVERANSPUNKT AS LOGICAL
   FIELD KLID AS INTEGER
   FIELD KABID AS INTEGER
   FIELD BERNR AS INTEGER
   FIELD BENAMNING AS CHARACTER 
   FIELD AVSLUTAD AS LOGICAL
   FIELD SID AS INTEGER
   INDEX KLID KLID.

DEFINE {&NEW} {&SHARED} TEMP-TABLE hdschaktfortemp NO-UNDO
   FIELD YTBELAGG AS CHARACTER 
   FIELD SID AS INTEGER 
   FIELD PID2 AS INTEGER
   FIELD PID1 AS INTEGER
   FIELD OMRADE AS CHARACTER 
   FIELD LANGD AS DECIMAL
   FIELD LAGID AS CHARACTER
   FIELD FID AS INTEGER
   FIELD DJUP AS INTEGER 
   FIELD BREDD AS INTEGER
   FIELD BERNR AS INTEGER.
     */
/*administration och s?nt*/
DEFINE {&NEW} {&SHARED} TEMP-TABLE tempkabel NO-UNDO
   FIELD DIAMETER AS INTEGER
   FIELD KABEL AS CHARACTER
   FIELD KABID AS INTEGER
   FIELD BORTTAGEN AS LOGICAL
   INDEX KABID KABID.

DEFINE {&NEW} {&SHARED} TEMP-TABLE tempytbelagg NO-UNDO
   FIELD YTBELAGG AS CHARACTER
   FIELD YTBELROW AS ROWID
   INDEX YTB IS PRIMARY YTBELAGG.

DEFINE {&NEW} {&SHARED} TEMP-TABLE tempsamforlagg NO-UNDO
   FIELD BENAMNING AS CHARACTER
   FIELD DIAMETER AS INTEGER
   FIELD KOD AS CHARACTER
   FIELD SAMFORROW AS ROWID
   INDEX KOD IS PRIMARY KOD.

DEFINE {&NEW} {&SHARED} TEMP-TABLE tempforlagg NO-UNDO
   FIELD BENAMNING AS CHARACTER
   FIELD DJUP AS INTEGER
   FIELD FORLAGG AS CHARACTER
   FIELD MARK AS LOGICAL
   FIELD ROR AS LOGICAL
   FIELD SAM AS LOGICAL
   FIELD TILLAGG AS INTEGER
   FIELD FAKTOR AS DECIMAL
   FIELD FORLAGGROW AS ROWID
   INDEX KOD IS PRIMARY FORLAGG.

DEFINE {&NEW} {&SHARED} TEMP-TABLE tempkonst NO-UNDO
   FIELD BENAMNING AS CHARACTER
   FIELD KONSKOD AS INTEGER
   FIELD KTYPKOD AS CHARACTER
   FIELD ORDNING AS INTEGER
   INDEX ORD ORDNING.

DEFINE {&NEW} {&SHARED} TEMP-TABLE eforlaggtemp NO-UNDO LIKE tempforlagg.
DEFINE {&NEW} {&SHARED} TEMP-TABLE esamforlaggtemp NO-UNDO LIKE tempsamforlagg.
DEFINE {&NEW} {&SHARED} TEMP-TABLE eytbelaggtemp NO-UNDO LIKE tempytbelagg.

DEFINE {&NEW} {&SHARED} TEMP-TABLE okltemp NO-UNDO LIKE hdkabellinjetemp.
DEFINE {&NEW} {&SHARED} TEMP-TABLE pkltemp NO-UNDO LIKE hdkabellinjetemp.
DEFINE {&NEW} {&SHARED} TEMP-TABLE mkltemp NO-UNDO LIKE hdkabellinjetemp.
DEFINE {&NEW} {&SHARED} TEMP-TABLE akltemp NO-UNDO LIKE hdkabellinjetemp.

