/*
 * This sample demonstrates the use of OLE Windows Messaging Server as an 
 * ActiveX Automation server. The following code sends an e-mail message to the 
 * user whose e-mail address is assigned to the Name property.
 */   

DEFINE VAR objSession AS COM-HANDLE.
DEFINE VAR objMessage AS COM-HANDLE.
DEFINE VAR objRecip AS COM-HANDLE.
DEFINE VAR one AS LOGICAL INIT YES.

CREATE "MAPI.SESSION" objSession.

objSession:Logon().
objMessage = objSession:OutBox:Messages:Add().
objMessage:Subject = "4GL Automation Test".
objMessage:Text = "Hi, this is a test message using the MAPI Server".
objRecip = objMessage:Recipients:Add().

/* TODO: PLEASE type in a valid email address inside the quotes */
objRecip:Name = "<put address here>".

objRecip:Type = 1.
objRecip:Resolve.
objMessage:Update(TRUE, TRUE).
objMessage:Send(TRUE, FALSE).
objSession:Logoff.
	
RELEASE OBJECT objRecip.
RELEASE OBJECT objMessage.
RELEASE OBJECT objSession.

MESSAGE "Completed" VIEW-AS ALERT-BOX.
