/* DYNBRWSTART.I */
DEFINE VARIABLE cellbrwrow AS ROWID NO-UNDO.
DEFINE VARIABLE ejtomabrw AS LOGICAL NO-UNDO.
DEFINE VARIABLE varjoin AS CHARACTER NO-UNDO.


{GLOBVAR2DEL1.I}

{SOKDEF.I}
{EXECLIN2.I}
DEFINE SHARED VARIABLE tth AS HANDLE NO-UNDO.
DEFINE VARIABLE dynenrh AS HANDLE NO-UNDO.
DEFINE VARIABLE brwradnr AS INTEGER NO-UNDO.
DEFINE VARIABLE deselvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE fontvar AS INTEGER NO-UNDO.
DEFINE VARIABLE bgcolvar AS INTEGER NO-UNDO.
DEFINE VARIABLE labelvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE aocolh AS HANDLE NO-UNDO.
DEFINE VARIABLE brwcolh AS HANDLE NO-UNDO EXTENT 15.
DEFINE VARIABLE delnrcolh AS HANDLE NO-UNDO.
DEFINE VARIABLE kalklopnrh AS HANDLE NO-UNDO.
DEFINE VARIABLE brwrow AS ROWID NO-UNDO.
DEFINE VARIABLE brwsok AS CHARACTER NO-UNDO.
DEFINE VARIABLE colfinns AS LOGICAL NO-UNDO.
DEFINE VARIABLE colformat AS CHARACTER NO-UNDO.
DEFINE VARIABLE sattindexvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE colindex AS CHARACTER NO-UNDO INITIAL ?.
DEFINE VARIABLE colindexnr AS INTEGER NO-UNDO INITIAL ?.
DEFINE VARIABLE colselnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE colsortby AS CHARACTER NO-UNDO INITIAL "".
DEFINE VARIABLE colsortcharinput AS CHARACTER NO-UNDO INITIAL "".
DEFINE VARIABLE defaultcol AS INTEGER NO-UNDO INITIAL 1.
DEFINE VARIABLE descvarcol AS LOGICAL NO-UNDO.
DEFINE VARIABLE dynbrwh AS HANDLE NO-UNDO.
DEFINE VARIABLE dynbuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE dynbuffhexten AS HANDLE NO-UNDO.
DEFINE VARIABLE dyncurcolh AS HANDLE NO-UNDO.
DEFINE VARIABLE dynfalth AS HANDLE NO-UNDO.
DEFINE VARIABLE dynlastcolh AS HANDLE NO-UNDO.
DEFINE VARIABLE dynok AS LOGICAL NO-UNDO.
DEFINE VARIABLE dynqueh AS HANDLE NO-UNDO.
DEFINE VARIABLE faltvalue AS CHARACTER NO-UNDO.
DEFINE VARIABLE frameh AS HANDLE NO-UNDO.
DEFINE VARIABLE kommandosortquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE lastbrwrow AS ROWID NO-UNDO.
DEFINE VARIABLE logdate AS CHARACTER NO-UNDO.
DEFINE VARIABLE logfalse AS CHARACTER NO-UNDO.
DEFINE VARIABLE logtrue AS CHARACTER NO-UNDO.
DEFINE VARIABLE mselclick AS LOGICAL NO-UNDO INITIAL TRUE.
DEFINE VARIABLE multi AS LOGICAL NO-UNDO.
DEFINE VARIABLE multibrwsok AS CHARACTER NO-UNDO.
DEFINE VARIABLE multitid AS INTEGER NO-UNDO.
DEFINE VARIABLE retvalue AS LOGICAL NO-UNDO.
DEFINE VARIABLE saverowid AS ROWID NO-UNDO.
DEFINE VARIABLE slproc AS HANDLE NO-UNDO. 
DEFINE VARIABLE sokord AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempchar AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempcolh AS HANDLE NO-UNDO.
DEFINE VARIABLE tempcounter AS INTEGER NO-UNDO.
DEFINE VARIABLE tempenrh AS HANDLE NO-UNDO.
DEFINE VARIABLE tempexh AS HANDLE NO-UNDO.
DEFINE VARIABLE temph AS HANDLE NO-UNDO.
DEFINE VARIABLE tempfieldh AS HANDLE NO-UNDO.
DEFINE VARIABLE temptableh AS HANDLE NO-UNDO.
DEFINE VARIABLE titlelog AS LOGICAL NO-UNDO.
DEFINE VARIABLE notitlelog AS LOGICAL NO-UNDO.
DEFINE VARIABLE trimnr AS INTEGER NO-UNDO.
DEFINE VARIABLE ttqh AS HANDLE NO-UNDO.
DEFINE VARIABLE selfh AS HANDLE NO-UNDO.
DEFINE VARIABLE otherh AS HANDLE NO-UNDO.
DEFINE VARIABLE sfalth AS HANDLE NO-UNDO.
DEFINE VARIABLE ofalth AS HANDLE NO-UNDO.
DEFINE VARIABLE ssok AS CHARACTER NO-UNDO.
DEFINE VARIABLE osok AS CHARACTER NO-UNDO.
DEFINE VARIABLE sfaltvalue AS CHARACTER NO-UNDO.
DEFINE VARIABLE ofaltvalue AS CHARACTER NO-UNDO.
DEFINE VARIABLE orgtitle AS CHARACTER NO-UNDO.
DEFINE VARIABLE multvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE chfillinproc AS CHARACTER NO-UNDO INITIAL "".
DEFINE VARIABLE hfillinproc AS HANDLE NO-UNDO.
DEFINE VARIABLE colrowshift AS INTEGER NO-UNDO INITIAL 0. 
DEFINE VARIABLE colshiftlog AS LOGICAL NO-UNDO.
DEFINE VARIABLE colvalnumber AS INTEGER NO-UNDO.
DEFINE VARIABLE colproclog AS LOGICAL NO-UNDO INITIAL FALSE.
DEFINE VARIABLE disbrwcol AS INTEGER NO-UNDO.
DEFINE VARIABLE rowdispextravar AS LOGICAL NO-UNDO.
DEFINE VARIABLE dynprogextrah AS HANDLE NO-UNDO.
DEFINE VARIABLE procexrowdisvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lastcolh AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE menuitemtemp
   FIELD BRWH AS HANDLE
   FIELD MENY AS CHARACTER
   FIELD MENYH AS HANDLE.
   