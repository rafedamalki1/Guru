/*XMAPI.P*/
DEFINE VAR objSession AS COM-HANDLE.
DEFINE VAR objMessage AS COM-HANDLE.
DEFINE VAR objFile AS COM-HANDLE.
DEFINE VAR objRecip AS COM-HANDLE.
DEFINE VAR one AS LOGICAL INIT YES.
DEFINE VAR vem AS CHARACTER.
DEFINE VAR varifran AS CHARACTER.
DEFINE VAR vadfil AS CHARACTER FORMAT "X(30)".
DEFINE VARIABLE vad AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 53 BY 9
     BGCOLOR 8  NO-UNDO.

update vad vem.
vem = "erik.olsson@mbox306.swipnet.se".
vem = "elpool.ume@elpool.se".


varifran = "c:\protemp8\elpao.txt".
vadfil = "elpao.txt".


CREATE "MAPI.SESSION" objSession.

/*objSession:Logon().
objSession:Logon("Anders Olsson", "", false).*/
objSession:Logon("Guru-profil").
objMessage = objSession:OutBox:Messages:Add().
objMessage:Subject = "4GL Automation Test".
objMessage:Text = vad.

objRecip = objMessage:Recipients:Add().
objRecip:Name = vem.
/*objRecip:Name = "anders olsson".
objRecip:Address = vem.
*/
objRecip:Type = 1.
objRecip:Resolve.
IF vadfil NE "" THEN DO:
   objFile = objMessage:Attachments:Add().
   objFile:Source = varifran.
   objFile:Name = vadfil.
  /* objFile:ReadFromFile = varifran.*/
END.
objMessage:Update(TRUE, TRUE).
objMessage:Send(TRUE, FALSE).
objSession:Logoff.
	
RELEASE OBJECT objRecip.
RELEASE OBJECT objMessage.
RELEASE OBJECT objSession.
IF vadfil NE "" THEN DO:
   RELEASE OBJECT objFile.
END.
MESSAGE "Completed" VIEW-AS ALERT-BOX.
