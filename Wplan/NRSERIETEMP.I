/*NRSERIETEMP.I*/
/* NRSERIETEMPn.I innehåller judid och vijudid i nrprefistemp*/
{NRSERIETEMPN.I}
/*
DEFINE TEMP-TABLE nrserietemp NO-UNDO
   FIELD NRSERIE AS INTEGER
   FIELD PROJINT1 AS CHARACTER
   FIELD PROJINT2 AS CHARACTER 
   FIELD SISTPROJ AS CHARACTER
   INDEX NRSERIE IS PRIMARY NRSERIE.

DEFINE TEMP-TABLE nrseromrtemp NO-UNDO
   FIELD NRSERIE AS INTEGER
   FIELD NRTYP AS CHARACTER
   FIELD OMRADE AS CHARACTER
   INDEX NRTYPOMR IS PRIMARY NRTYP OMRADE. 

DEFINE TEMP-TABLE nrstyrtemp NO-UNDO
   FIELD PREFIX AS CHARACTER FORMAT "x(8)"
   FIELD NRTEXT AS CHARACTER FORMAT "x(8)"
   FIELD IDNRS  AS INTEGER FORMAT "->>>>>>9" INITIAL 0
   INDEX PREFIX IS PRIMARY PREFIX
   INDEX IDNRS IDNRS.

DEFINE TEMP-TABLE nrprefistemp NO-UNDO
   FIELD PREFIX AS CHARACTER FORMAT "x(8)"                 
   FIELD NRTEXT AS CHARACTER FORMAT "x(8)"                 
   FIELD IDNRP  AS INTEGER FORMAT "->>>>>>9" INITIAL 0   
   INDEX PREFIX IS PRIMARY PREFIX
   INDEX IDNRP IDNRP.
*/



   
