/*
  Detta program öppnar outlook och skapar mail med
  bifogade filer vilkas namn skickas med i temptable.
  
  Inget subject
  Ingen adress vald
  
  Användande:
  Skicka in temptable med fullständig sökväg i temptable definierad i MAIL.I, 
  hur många attachments som helst.
  
  ex:
  CREATE tempattach.
      tempattach.fil = SESSION:TEMP-DIRECTORY + "exempel.txt".
  
  
  Exempel på flera bifogade filer:
  xmailtest.p
  
*/


&Scoped-define SHARED
&Scoped-define NEW
{MAIL.I}


DEFINE INPUT PARAMETER TABLE FOR tempattach.

DEFINE VARIABLE attach-name  AS CHARACTER    NO-UNDO.
DEFINE VARIABLE Folder       AS COM-HANDLE   NO-UNDO.
DEFINE VARIABLE MailItem     AS COM-HANDLE   NO-UNDO.
DEFINE VARIABLE message-text AS CHARACTER    NO-UNDO.
DEFINE VARIABLE NameSpace    AS COM-HANDLE   NO-UNDO.
DEFINE VARIABLE Outlook      AS COM-HANDLE   NO-UNDO.
DEFINE VARIABLE priority     AS CHARACTER    NO-UNDO.
DEFINE VARIABLE SafeItem     AS COM-HANDLE   NO-UNDO.


   
DO:

   /*CREATE "Outlook.Application" Outlook.*/
   CREATE "Outlook.Application" Outlook.
        
    ASSIGN 
        NameSpace   = Outlook:GetNameSpace("MAPI":U)
        Folder      = NameSpace:GetDefaultFolder(6).
        
    
    ASSIGN 
        MailItem            = Folder:Items:Add()
        MailItem:To         = "" /*scr-To:SCREEN-VALUE*/
        MailItem:Subject    = "" /*scr-Subject:SCREEN-VALUE*/
        MailItem:Body       = "" /*TRIM(ed-Mess:SCREEN-VALUE)*/
        MailItem:Importance = 0.

   /* Lägga till attachment för varje rad i medskickat temp-table */
    FOR EACH tempattach WHERE NO-LOCK:
       attach-name = tempattach.fil.
       MAilItem:Attachments:ADD(attach-name).
    END.

    /* Visar mailet i outlook */
    MailItem:DISPLAY.
    /*MailItem:SEND.*/
    
    
    /* Redemption addition */
    /*CREATE "Redemption.SafeMailItem" SafeItem.
        
    SafeItem:item = MailItem.
    SafeItem:SEND().                          */
    /* addition ended */
    
    

    RELEASE OBJECT MailItem  NO-ERROR.
    RELEASE OBJECT Folder    NO-ERROR.
    RELEASE OBJECT NameSpace NO-ERROR.
    RELEASE OBJECT Outlook   NO-ERROR.
    RELEASE OBJECT SafeItem  NO-ERROR.
    
END.
