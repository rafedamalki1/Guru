/*AONR.I*/
DEFINE VARIABLE framesizeh AS HANDLE NO-UNDO.


{FRAMSIZETEMP.I}
/* DEFINE VARIABLE framesizedh AS HANDLE NO-UNDO. */

DEFINE VARIABLE hcur AS INTEGER NO-UNDO.
DEFINE VARIABLE retval AS INTEGER NO-UNDO.
DEFINE VARIABLE brwproc AS HANDLE EXTENT 25 NO-UNDO.
DEFINE VARIABLE handproc AS HANDLE EXTENT 25 NO-UNDO.
DEFINE VARIABLE apphandass AS HANDLE NO-UNDO.
DEFINE VARIABLE apphandklar AS HANDLE NO-UNDO.
DEFINE VARIABLE appprogok AS LOGICAL NO-UNDO.
&GLOBAL-DEFINE LEFT-BROWSE 1
&GLOBAL-DEFINE RIGHT-BROWSE 2
&GLOBAL-DEFINE ARROWS 3
DEFINE VARIABLE tthandle AS HANDLE NO-UNDO.
DEFINE VARIABLE ReturnValue AS INTEGER.
{SLVALDBRW.I}
{BERIN.I}
{EXTRATAB.I}  
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{DIRDEF.I}
DEFINE {&NEW} {&SHARED} TEMP-TABLE valdaaotemp NO-UNDO  LIKE utsokaonr.
{AOTIDKOPP.I}
{AVDTEMP.I}


&Scoped-define NEW NEW
&Scoped-define SHARED SHARED


&Scoped-define NEW NEW
/*
DEFINE VARIABLE labelvar1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE labelvar2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE valtrecid AS RECID NO-UNDO.
DEFINE VARIABLE valdarec AS RECID NO-UNDO.
DEFINE VARIABLE valdarec2 AS RECID NO-UNDO.
DEFINE VARIABLE brwakt AS LOGICAL NO-UNDO.

DEFINE VARIABLE valnr AS CHARACTER NO-UNDO.

DEFINE VARIABLE tillbakaaonr AS CHARACTER NO-UNDO. 
DEFINE VARIABLE tillbakadelnr AS INTEGER NO-UNDO.
DEFINE VARIABLE skick AS LOGICAL NO-UNDO.
DEFINE VARIABLE efel AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE uppar AS INTEGER NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(8)" NO-UNDO. 
DEFINE VARIABLE ortssok AS CHARACTER NO-UNDO.

DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO. 
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.  
DEFINE VARIABLE hjdelvar AS INTEGER NO-UNDO.
DEFINE VARIABLE wh AS WIDGET-HANDLE.
DEFINE VARIABLE kalknrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE kalktypvar AS INTEGER NO-UNDO.
DEFINE VARIABLE vardnrvar AS INTEGER NO-UNDO.

DEFINE VARIABLE projvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE bortvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE aoavslut AS DATE NO-UNDO.
DEFINE VARIABLE bortaoapph AS HANDLE NO-UNDO.
DEFINE VARIABLE fastdelnr AS INTEGER NO-UNDO.
DEFINE VARIABLE nyttaoapph2 AS HANDLE NO-UNDO.                      /*NYTTAOAPP.P*/
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.
*/
