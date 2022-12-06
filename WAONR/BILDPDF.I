/*BILDPDF.I*/

DEFINE VARIABLE link AS CHARACTER NO-UNDO.

link = ?.
/*FOREBILDER*/
{LOGGOR.I}
logga = REPLACE(link,".gif",".jpg").
logga = REPLACE(logga,".png",".jpg").
logga = SEARCH(logga).
