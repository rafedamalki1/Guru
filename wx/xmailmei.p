DEFINE INPUT  PARAMETER Profile  AS CHARACTER.      /* Profile name for sender */
DEFINE INPUT  PARAMETER Address  AS CHARACTER.      /* Email address of recipient */
DEFINE INPUT  PARAMETER Subject  AS CHARACTER.      /* Subject of email */
DEFINE INPUT  PARAMETER Body     AS CHARACTER.      /* Body text */
DEFINE INPUT  PARAMETER FileName AS CHARACTER.      /* Name of file to attach */
DEFINE OUTPUT PARAMETER Result   AS LOGICAL.        /* Result of procedure */

DEFINE VAR hSession     AS COM-HANDLE.
DEFINE VAR hMessage     AS COM-HANDLE.
DEFINE VAR hRecip       AS COM-HANDLE.
DEFINE VAR hAttach      AS COM-HANDLE.

    /* Attempt to create a MAPI session */
    CREATE "MAPI.SESSION" hSession.

    /* Logon to the mail client */
    hSession:Logon().
    
    /* Create a new message */
    hMessage = hSession:OutBox:Messages:Add().
    
    /* Fill message */
    hMessage:Subject = Subject.
    hMessage:Text = Body.
    
    /* Add recipient */
    hRecip = hMessage:Recipients:Add().
    hRecip:Name = Address.
    
    /* Add attachment */
    hAttach = hMessage:Attachments:Add().
    hAttach:Source = FileName.

    /* Set final parameters */
    hRecip:Type = 1.
    hRecip:Resolve.
    hMessage:Update(TRUE, TRUE).
    
    /* Send message */
    hMessage:Send(TRUE, FALSE).

    /* Cleanup */
    hSession:Logoff.
    RELEASE OBJECT hRecip.
    RELEASE OBJECT hAttach.
    RELEASE OBJECT hMessage.
    RELEASE OBJECT hSession.
    
    Result = TRUE.
