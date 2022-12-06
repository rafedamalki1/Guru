
/*------------------------------------------------------------------------
    File        : CardIONHelpStart.p
    Purpose     :  GCK CARDSKIPPER IDROTTONLINE
GIMONÄS CK
    Syntax      :run C:\delad\pro116\GuruAnders\wx\GCK\CardIONHelpStart.p

    Description : 

    Author(s)   : 
    Created     : Tue Sep 15 15:39:55 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/
SESSION:DEBUG-ALERT = YES.
RUN CardION_UI.
PROCEDURE CardION_UI :
   
   DEFINE VARIABLE CardION   AS wx.GCK.CardION NO-UNDO.
   CardION  = NEW wx.GCK.CardION().
   WAIT-FOR CardION:ShowDialog().
   
   DELETE OBJECT CardION NO-ERROR.
   CardION = ?.
    
END PROCEDURE.

