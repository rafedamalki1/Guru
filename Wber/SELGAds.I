
/*------------------------------------------------------------------------
    File        : SELGAds.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Apr 20 16:42:54 CEST 2017
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE DATASET SelgaDs  XML-NODE-TYPE  "HIDDEN"
   FOR SELGAOrder, tbl_OrderHead, tbl_OrderLine
   PARENT-ID-RELATION RELATION1 FOR SELGAOrder, tbl_OrderHead
      PARENT-ID-FIELD RID
   PARENT-ID-RELATION RELATION10 FOR tbl_OrderHead, tbl_OrderLine
      PARENT-ID-FIELD RID.   
   