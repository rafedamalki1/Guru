/*OMRALLT.I*/
DEFINE {&NEW} SHARED TEMP-TABLE juromravd NO-UNDO
   FIELD ID AS CHARACTER 
   FIELD SID AS CHARACTER 
   FIELD AVDELNINGNR AS INTEGER 
   FIELD NAMN AS CHARACTER
   FIELD TYP AS CHARACTER
   INDEX PERSONALKOD IS PRIMARY TYP ID
   INDEX AVDELNINGNR TYP AVDELNINGNR 
   INDEX NAMN TYP NAMN
   INDEX SID TYP SID.
   
DEFINE TEMP-TABLE omrallt NO-UNDO
   FIELD AVDELNINGNAMN AS CHARACTER FORMAT "X(16)" LABEL "Avdelning"
   FIELD AVDELNINGNR   AS INTEGER FORMAT ">9"
   FIELD ELVOMRKOD     AS INTEGER FORMAT "->>>>>>9"
   FIELD GATUADR       AS CHARACTER FORMAT "X(25)"   LABEL "Gatuadress"
   FIELD KALKYLINT1    AS INTEGER FORMAT "->>>>>>9"
   FIELD KALKYLINT2    AS INTEGER FORMAT "->>>>>>9"
   FIELD KALKYLSIST    AS INTEGER FORMAT ">>>>>>9"
   FIELD KONTOOB       AS LOGICAL FORMAT "Ja/Nej"    LABEL "Oblig.Konto"
   FIELD KUNDPRISF     AS DECIMAL FORMAT ">>9.99" INITIAL 1  LABEL "Kundprisfaktor"  
   FIELD NAMN         AS CHARACTER FORMAT "X(16)"    LABEL "Benämning"
   FIELD OMKMTR       AS DECIMAL FORMAT ">9.99"      LABEL "Materielomkostnad"
   FIELD OMKOSTP      AS DECIMAL FORMAT ">>9.99"    INITIAL 1  LABEL "Omkostnadspåslag"
   FIELD OMRADE       AS CHARACTER FORMAT "X(6)"     LABEL "Område"
   FIELD ORGIDNUM     AS CHARACTER FORMAT "X(10)"     LABEL "Numerisk.org"
   FIELD PLANINT1     AS CHARACTER FORMAT "X(6)"
   FIELD PLANINT2     AS CHARACTER FORMAT "X(6)"
   FIELD POSTANST     AS CHARACTER FORMAT "X(20)"    LABEL "Postanstalt"
   FIELD POSTNR       AS CHARACTER FORMAT "XXX XX"  LABEL "Postnummer"
   FIELD PROJINT1     AS CHARACTER FORMAT "X(6)"
   FIELD PROJINT2     AS CHARACTER FORMAT "X(6)"
   FIELD SISTPLAN     AS CHARACTER FORMAT "X(6)"
   FIELD SISTPROJ     AS CHARACTER FORMAT "X(6)"
   FIELD TELJOUR      AS CHARACTER FORMAT "X(11)"    LABEL "Journummer"
   FIELD TELVXL       AS CHARACTER FORMAT "X(11)"    LABEL "Telefonnr"
   FIELD TIMKOST      AS INTEGER FORMAT ">>>>"
   FIELD TIMKOSTMASK  AS INTEGER FORMAT ">>>>"
   FIELD VARDINT1    AS INTEGER FORMAT "->>>>>>9"
   FIELD VARDINT2    AS INTEGER FORMAT "->>>>>>9"
   FIELD VARDSIST    AS INTEGER FORMAT ">>>>>>9"
   INDEX OMRADE IS PRIMARY OMRADE.
   
