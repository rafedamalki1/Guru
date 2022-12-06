/*
     Filename: XMAPICOM.P
      Created: 03.04.0014 12:04ELPAO     
     Modified: 
*/

/*  mapi.p   
Progress version 8.2+
04/13/1999 Dmitri Levin
*/
define variable chSession   as com-handle no-undo.
define variable chMessage   as com-handle no-undo.
define variable chRecipient as com-handle no-undo.
define variable chAttach    as com-handle no-undo.

FUNCTION FuncRegVal RETURNS CHARACTER ( pBaseKey as char, pKeyName as char, pSecName as char, pItem as char ) forward.

create "MAPI.session" chSession.
chSession:logon("Default Exchange Profile", No, Yes, 0).
/* chSession:logon( FuncRegVal("HKEY_CURRENT_USER",                     */
/*                    "Software\Microsoft\Windows Messaging Subsystem", */
/*                    "Profiles",                                       */
/*                    "DefaultProfile")                                 */
/*                 ).                                                   */

chMessage = chSession:outbox:messages:add.

chMessage:Type    = "IPM.Note".
chMessage:Subject = "Message Subject".
chMessage:Text    = "Message Text" + CHR(10).

chAttach      = chMessage:Attachments:Add.
chAttach:Name = "config.sys".
chAttach:ReadFromFile("c:\config.sys").
chAttach:Type = 1.
/*********************************
    AttachmentType
      1  ByValue
      4  ByReference
      5  EmbeddedItem
      6  OLE
***********************************/
release object chAttach.

chRecipient      = chMessage:Recipients:Add("mikael.eriksson.78@home.se"). /* change e-mail address */
chRecipient:Type = 1.
chRecipient:resolve.
/*
chRecipient      = chMessage:Recipients:Add("other address").
chRecipient:Type = 1.
chRecipient:resolve.
chRecipient      = chMessage:Recipients:Add("copy to address").
chRecipient:Type = 2. /* 1 = To, 2 = Cc, 3 = Bcc */
chRecipient:resolve.
*/
chMessage:Update.
chMessage:send(Yes, No, 0).

release object chRecipient.
release object chMessage.
release object chSession.

FUNCTION FuncRegVal RETURNS CHARACTER
  ( pBaseKey as char,   /* i.e. "HKEY_..."  */
    pKeyName as char,   /* main key, i.e. "software\ACME..." */
    pSecName as char,   /* section */
    pItem    as char    /* item identifier, "" = return list, ? = default */
   ) :

   def var iValue as char no-undo.

   load pKeyName base-key pBaseKey no-error.
   MESSAGE pKeyName pBaseKey VIEW-AS ALERT-BOX.
   if not error-status:error then
   do:
      use pKeyName.
      if pItem = ? then
         get-key-value section pSecName
                       key     default
                       value   iValue.
      else
         get-key-value section pSecName
                       key     pItem
                       value   iValue.
      if iValue = ? then 
         iValue = "".
      unload pKeyName no-error.
   end. /* if no error*/
   MESSAGE iValue pItem pSecName VIEW-AS ALERT-BOX.
   return iValue.

END FUNCTION.

