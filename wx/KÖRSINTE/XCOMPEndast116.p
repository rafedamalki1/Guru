/*XCOMPEndast116.p*/
{XCOMP116.I}


                              
                              
DEFINE NEW SHARED VARIABLE orgpropath AS CHARACTER NO-UNDO.
SESSION:DEBUG-ALERT = YES.

IF PROVERSION BEGINS "11.6" THEN.
ELSE DO:
   MESSAGE "fel PROVERSION" PROVERSION
   VIEW-AS ALERT-BOX.
   QUIT. 
END.
MESSAGE "Nu startar 116 kompilering"
VIEW-AS ALERT-BOX.
{PROVAG.I}
DEFINE VARIABLE webbdir AS CHARACTER NO-UNDO.
DEFINE VARIABLE savedir AS CHARACTER NO-UNDO.
DEFINE VARIABLE ekgwebbdir AS CHARACTER NO-UNDO.
DEFINE VARIABLE dirlist AS CHARACTER NO-UNDO.
DEFINE VARIABLE filnamnR AS CHARACTER NO-UNDO.
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE utfil AS CHARACTER NO-UNDO.
DEFINE VARIABLE curdir AS CHARACTER NO-UNDO.
DEFINE VARIABLE C116var AS INTEGER NO-UNDO.


SESSION:DEBUG-ALERT = YES.
ASSIGN
utfil = SESSION:TEMP-DIR + "cmplog116.doc" /*Logfil för kompilering.*/.
OUTPUT TO VALUE(utfil).
PUT TODAY .
OUTPUT CLOSE.



savedir = "\\PC112\WebGuru\\Komp11\".   /*Sökväg till de kompilerade filerna.*/.
webbdir = "\\PC112\WebGuru\GuruOnWeb\".   /*Sökväg till de kompilerade filerna.*/
ekgwebbdir = "\\PC112\WebGuru\GuruOnWeb\".

/*savedir = "\\PC112\delad\PRO11\guru\komp11\".   /*Sökväg till de kompilerade filerna.*/.
webbdir = "\\PC112\DELAD\PRO11\GURUWEB\".   /*Sökväg till de kompilerade filerna.*/
ekgwebbdir = "\\PC112\DELAD\PRO11\EKGWEB\".
*/
RUN C116_UI.



RUN OPENDOC.P (utfil,"","",NO).
MESSAGE C116var "Kompilerades KOLLA ATT DESSA INTE FINNS I KOMP11!"
VIEW-AS ALERT-BOX.
QUIT.

PROCEDURE comp_UI :
   /*körs från C116_UI*/
   DEFINE INPUT  PARAMETER filnamncomp AS CHARACTER NO-UNDO.
   filnamn = SEARCH(filnamncomp).
   IF filnamn = ? THEN RETURN.
   OUTPUT TO VALUE(utfil) APPEND. 
   COMPILE VALUE(filnamn) SAVE INTO VALUE(savedir).
   OUTPUT CLOSE.
   filnamnR = REPLACE(filnamn,".P",".r").
   filnamnR = REPLACE(filnamnR,".w",".r").
   filnamncomp = REPLACE(filnamncomp,".P",".r").
   filnamncomp = REPLACE(filnamncomp,".w",".r").
   C116var = C116var + 1.
   
   filnamnR = REPLACE(filnamnR,Guru.Konstanter:guruvar,webbdir).
   OS-COPY VALUE(savedir + filnamncomp) VALUE(filnamnR).
   /*ekg kör från samma
   filnamnR = REPLACE(filnamnR,webbdir,ekgwebbdir).
   OS-COPY VALUE(savedir + filnamncomp) VALUE(filnamnR).
   */
   OS-DELETE VALUE(savedir + filnamncomp).
      
   
END PROCEDURE.                     