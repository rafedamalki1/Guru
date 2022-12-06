
/*------------------------------------------------------------------------
    File        : ORD_TEMP.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Oct 02 09:58:45 CEST 2018
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE {&NEW} {&SHARED} TEMP-TABLE ord_temp
   /*Anders Olsson Elpool i Umeå AB  2 okt 2018 10:42:01 
   för dataset
   BEFORE-TABLE ord_tembef
   */
   
   FIELD AONR AS CHARACTER
   FIELD OMRADE AS CHARACTER 
   FIELD NUM AS INTEGER
   FIELD ORD AS INTEGER
   FIELD TTRECID AS RECID
   INDEX NUM IS PRIMARY NUM ASCENDING. 
