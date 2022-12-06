/*
               KSV Editor
    Copyright: (C) 2000-2001 Serguey Klimoff (bulkl0DD)
     Filename: XOEADRESS.I
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 2005.12.02 14:40 ELPAO   
     Modified: 
*/
DEFINE VARIABLE houtlook AS Com-handle NO-UNDO.
DEFINE VARIABLE hnamespace AS Com-handle NO-UNDO.
DEFINE VARIABLE hfolder AS Com-handle NO-UNDO.
DEFINE VARIABLE hitem AS Com-handle NO-UNDO.
DEFINE VARIABLE x AS integer NO-UNDO.

CREATE "outlook.application" houtlook.

hnamespace = houtlook:getnamespace("MAPI").
hfolder = hnamespace:addresslists:ITEM(1).

DO X=1 TO hfolder:addressentries:COUNT():
hitem = hfolder:addressentries:ITEM(X).
MESSAGE hitem:NAME + " " hitem:address VIEW-AS ALERT-BOX.
END.

RELEASE OBJECT hitem.
RELEASE OBJECT hfolder.
RELEASE OBJECT hnamespace.
RELEASE OBJECT houtlook.

