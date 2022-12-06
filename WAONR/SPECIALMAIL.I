
/*------------------------------------------------------------------------
    File        : SPECIALMAIL.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Mar 24 12:31:13 CET 2017
    Notes       :
  ----------------------------------------------------------------------*/

   IF PROVERSION BEGINS "11.2" THEN.
   
   ELSE DO:
   RUN SPECIALMAPI116.P (pcRecipient, /* To */
                pcCC, /* CC */
                pcBCC, /* BCC */
                pcSubject, /* Subject */
                pcMailTxt, /* Body */
                pcAttachName, /* AttachName */
                pcAttach, /* Attachment */
                pcSender /* From */
                ).
    RETURN.
END.
