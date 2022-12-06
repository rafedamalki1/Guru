 
 /*------------------------------------------------------------------------
    File        : OutMail
    Purpose     : Attacha filer till mail i outlook
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Fri Mar 24 11:03:36 CET 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Microsoft.Office.Interop.Outlook.*.
USING Progress.Windows.UserControl.


CLASS Controls.OutMail INHERITS UserControl: 
   DEFINE PRIVATE  VARIABLE OutlookAppHandle     AS Application                               NO-UNDO.
   
   CONSTRUCTOR PUBLIC OutMail():
      THIS-OBJECT:OutlookAppHandle = NEW Microsoft.Office.Interop.Outlook.ApplicationClass().
   END.
   
   METHOD PUBLIC VOID SkapaMail(pcRecipient AS CHARACTER, pcCc AS CHARACTER, pcBcc AS CHARACTER, pcSubject AS CHARACTER, pcMailTxt AS MEMPTR, pcAttachName AS CHARACTER, pcAttach AS CHARACTER, pcSender AS CHARACTER):
      DEFINE VARIABLE mail     AS Microsoft.Office.Interop.Outlook.MailItem                               NO-UNDO.
      mail = CAST(THIS-OBJECT:OutlookAppHandle:CreateItem(Microsoft.Office.Interop.Outlook.OlItemType:olMailItem), Microsoft.Office.Interop.Outlook.MailItem).
      mail:To = pcRecipient. /*pcRecipient*/
      mail:Subject = pcSubject. /*pcSubject*/
      mail:CC = pcCC. /*pcCc*/
      mail:BCC = pcBcc. /*pcBcc*/
      mail:Body = STRING(pcMailTxt).
      IF pcAttachName EQ "" OR pcAttachName EQ "" THEN.
      ELSE mail:Attachments:Add(pcAttach,Microsoft.Office.Interop.Outlook.OlAttachmentType:olByValue,1,pcAttachName).
      mail:Display(TRUE).
   END METHOD.
   
   
   
END CLASS.