/*KALKTEMPE.I*/
DEFINE VARIABLE totea       AS DECIMAL NO-UNDO.
DEFINE VARIABLE totarb      AS DECIMAL NO-UNDO.
DEFINE VARIABLE totmask     AS DECIMAL NO-UNDO.
DEFINE VARIABLE totmasktim  AS DECIMAL NO-UNDO.
DEFINE VARIABLE totmtrl     AS DECIMAL NO-UNDO.
DEFINE VARIABLE totovr      AS DECIMAL NO-UNDO.
DEFINE VARIABLE totutr      AS DECIMAL NO-UNDO.
DEFINE VARIABLE totutrtim   AS DECIMAL NO-UNDO.
DEFINE VARIABLE arbtim      AS DECIMAL NO-UNDO.
DEFINE VARIABLE msktim      AS DECIMAL NO-UNDO.
DEFINE VARIABLE bertim      AS DECIMAL NO-UNDO. 
DEFINE VARIABLE totalt      AS DECIMAL NO-UNDO. 

DEFINE VARIABLE eavar   AS DECIMAL NO-UNDO.  
DEFINE VARIABLE arbvar  AS DECIMAL NO-UNDO.  
DEFINE VARIABLE mskvar  AS DECIMAL NO-UNDO.  
DEFINE VARIABLE faktvar AS DECIMAL NO-UNDO.  
DEFINE VARIABLE prisvar AS DECIMAL NO-UNDO.
DEFINE VARIABLE delningsvar AS INTEGER NO-UNDO. /*s?tts till 1 om F9 v?rde = 0 och 100 om F9 v?rde = 1*/
DEFINE VARIABLE fakdelningvar AS INTEGER NO-UNDO. /*s?tts till 1 om F9 v?rde = 1 och 100 om F9 v?rde = 2*/

DEFINE NEW SHARED VARIABLE kontroll AS LOGICAL NO-UNDO.

{KALKTEMPDEF.I}
