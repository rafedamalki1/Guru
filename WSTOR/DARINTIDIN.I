/*DARINTIDIN.I*/

DEFINE TEMP-TABLE tidin3
   FIELD B1                 AS INTEGER /*företag*/
   FIELD B2                 AS CHARACTER /*distrikt*/
   FIELD B3                 AS INTEGER /*årtal*/
   FIELD B4                 AS CHARACTER /*spänningsnivå*/
   FIELD B7                 AS INTEGER /*luftledning*/
   FIELD B8                 AS INTEGER /*kabel*/
   FIELD B9                 AS INTEGER /*bland*/
   FIELD B10                AS INTEGER. /*totalt*/

DEFINE TEMP-TABLE tidin4
   FIELD B1                 AS INTEGER 
   FIELD B2                 AS CHARACTER
   FIELD B3                 AS INTEGER 
   FIELD B4                 AS CHARACTER
   FIELD B5                 AS CHARACTER 
   FIELD B6                 AS CHARACTER
   FIELD B7                 AS INTEGER 
   FIELD B8                 AS INTEGER
   FIELD B9                 AS INTEGER 
   FIELD B10                AS INTEGER.
DEFINE TEMP-TABLE tidin5   
   FIELD FORETAG            AS INTEGER 
   FIELD DISTRIKT           AS CHARACTER
   FIELD BDAT               AS DATE FORMAT "9999/99/99"
   FIELD BTID               AS INTEGER FORMAT "9999"
   FIELD C1                 AS INTEGER   
   FIELD D1                 AS CHARACTER
   FIELD D2                 AS CHARACTER 
   FIELD E1                 AS INTEGER
   FIELD E2                 AS CHARACTER
   FIELD F1                 AS INTEGER
   FIELD F2                 AS CHARACTER
   FIELD G1                 AS INTEGER
   FIELD G2                 AS INTEGER
   FIELD G3                 AS INTEGER
   FIELD H                  AS CHARACTER
   FIELD J1DAT              AS DATE FORMAT "9999/99/99"
   FIELD J1KLOCK            AS INTEGER FORMAT "9999"
   FIELD J2DAT              AS DATE FORMAT "9999/99/99"
   FIELD J2KLOCK            AS INTEGER FORMAT "9999"
   FIELD J3                 AS CHARACTER
   FIELD K1                 AS INTEGER
   FIELD K2                 AS INTEGER
   FIELD L1                 AS INTEGER
   FIELD L2                 AS INTEGER
   FIELD M1                 AS INTEGER
   FIELD M2                 AS INTEGER
   FIELD N1                 AS INTEGER
   FIELD N2                 AS CHARACTER
   FIELD P1                 AS INTEGER
   FIELD P2                 AS INTEGER
   FIELD Q                  AS INTEGER
   FIELD R                  AS INTEGER
   FIELD S                  AS INTEGER
   FIELD T1                 AS INTEGER
   FIELD T2                 AS INTEGER
   FIELD T3                 AS INTEGER   
   FIELD FEL                AS CHARACTER
   FIELD FEL2               AS LOGICAL INITIAL FALSE
   FIELD OKVAR              AS LOGICAL INITIAL FALSE
   INDEX ORD FORETAG DISTRIKT BDAT BTID FEL FEL2 ASCENDING.
