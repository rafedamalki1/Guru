/*DEFVALUESNETTT.I
 */

DEFINE {&STATIC} TEMP-TABLE GuruDefaultsTT NO-UNDO
   BEFORE-TABLE GuruDefaultsTTBef
   FIELD PROGRAM AS CHARACTER       /* Vilket program värdet tillhör */
   FIELD ANVANDARE AS CHARACTER
   FIELD HUVUDINT AS INTEGER        /* Identifierande int-variabel */
   FIELD HUVUDCHAR AS CHARACTER     /* Identifierande char-variabel */
   FIELD EXTRADATA AS CHARACTER     /* Extraparametrar för att söka/lagra */
   FIELD TOOLGROUP AS CHARACTER     /* Vilken grupp den tillhör */
   FIELD TOOLKEY AS CHARACTER       /* Key/Benämning */
   FIELD TOOLTYPE AS CHARACTER      /* Vilken typ värdet tillhör, för att sätta/hämta lättare via klasser */
   FIELD TOOLVALUE AS CHARACTER     /* Vilket värde */
   FIELD VALIDTOOLVALUE AS CHARACTER     /* Vilket värde NU */
   FIELD TTRECID AS RECID
   INDEX PROGRAM PROGRAM ANVANDARE HUVUDINT HUVUDCHAR                       /* Standard-index, söker program, int, char*/
   INDEX PROKEY PROGRAM ANVANDARE TOOLKEY 
   INDEX TOOLKEY TOOLTYPE TOOLGROUP TOOLKEY.                                  /* Standard-index för specifierad key */
   
   
DEFINE {&STATIC} TEMP-TABLE GuruDefaultValuesTT NO-UNDO LIKE GuruDefaultsTT.