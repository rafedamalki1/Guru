There are different kinds of solutions for using Email from Progress. 
For example, there are several ActiveX components, there is a 
Progress 7+8 product with source made by Ketil Parow 
(you can download a trial version), 
you can use the MAPI OLE automation server 
(there is a document about that on the ActiveX pages on www.dotr.com) or 
you can use the MAPI through its native API. 
On this site we'll only discuss the API kind of solutions. 

The next sourcecode example was posted by Jeff Ledbetter and was earlier found 
in Profiles. I ported it to 32-bit.
The example uses the 'Simple MAPI' interface. 
This interface is not by default available on any machine. 
The easiest way to ensure if Simple MAPI is available is to check 
the registry: the "HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows Messaging Subsystem" 
variable "MAPI" should have value "1". 

 
RUN SendMail (INPUT "yourself@home.com",
              INPUT "this is the subject line",
              INPUT "This is the body text",
              OUTPUT Okay).
 
 
PROCEDURE SendMail :
DEFINE INPUT PARAMETER send-to-name AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER send-subject AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER send-text    AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER Okay        AS LOGICAL NO-UNDO INITIAL NO.
 
DEFINE VARIABLE pnames   AS MEMPTR.
DEFINE VARIABLE psendto  AS MEMPTR.
DEFINE VARIABLE psubj    AS MEMPTR.
DEFINE VARIABLE ptext    AS MEMPTR.
DEFINE VARIABLE pmessage AS MEMPTR.
 
DEFINE VARIABLE wans AS INT .
 
SET-SIZE(pnames)  = 24.
SET-SIZE(psendto) = 16.
 
 
PUT-LONG(pnames,1)  = 0. /* Reserved */
PUT-LONG(pnames,5)  = 1. /* Recip Class MAPI_TO */
PUT-LONG(pnames,9)  = GET-POINTER-VALUE(psendto). /* Names */
PUT-LONG(pnames,17) = 0. /* EID Size */
 
SET-SIZE(psubj)    = 100.
SET-SIZE(ptext)    = 8000.
SET-SIZE(pmessage) = 48.
 
PUT-STRING(psubj,1)   = send-subject.
PUT-STRING(ptext,1)   = send-text.
PUT-STRING(psendto,1) = send-to-name.
 
PUT-LONG(pmessage,1)  = 0. /* Reserved */
PUT-LONG(pmessage,5)  = GET-POINTER-VALUE(psubj). /* Subject */
PUT-LONG(pmessage,9)  = GET-POINTER-VALUE(ptext). /* Text */
PUT-LONG(pmessage,25) = 0. /* Flags */
PUT-LONG(pmessage,33) = 1. /* RecipCount */
PUT-LONG(pmessage,37) = GET-POINTER-VALUE(pnames).
PUT-LONG(pmessage,41) = 0.
 
RUN MAPISendMail IN Guru.Konstanter:hpApi(INPUT 0,      /* mapi session handle */
                          INPUT 0,      /* parent window handle */
                          INPUT GET-POINTER-VALUE(pmessage),
                          INPUT 0,      /* flags */
                          INPUT 0,      /* reserved, must be 0 */
                          OUTPUT Wans). /* error status */
IF Wans<>0 THEN
   MESSAGE "Mail not sent, error code=" Wans
           VIEW-AS ALERT-BOX.
ELSE
   Okay = YES.
 
/* dealloc memory */
SET-SIZE(pnames)   = 0.
SET-SIZE(psendto)  = 0.
SET-SIZE(psubj)    = 0.
SET-SIZE(ptext)    = 0.
SET-SIZE(pmessage) = 0.
 
END PROCEDURE.
 

 

Comments on MAPISendMail:
The first parameter is a Mapi-session handle. 
If 0, MAPI will try to find a shared session or create a temporary session for 
the duration of the call and it might have to show a login dialog. 
So if you need to send several mails it might be better to 
create a session up front and reuse its handle.
The second parameter is a parent window handle. 
If MapiSendMail has to show a dialog (for example a login dialog), 
it will be an application-modal dialog parented to this window handle.
The value 0 is valid but will not block input in your Progress application.
The third parameter is a fairly complicated message structure. 
This example only creates a simple mail but MAPI also allows to add one or 
more attachements as shown in the source on the next page.
If you do not specify a recipient (send-to-name) 
you must allow MAPI to ask for one. 
That is, you must use the MAPI_DIALOG (=8) flag in the 4th parameter of 
MapiSendMail.
