
/*------------------------------------------------------------------------
    File        : DEPMTR.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Feb 20 11:36:03 CET 2020
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE TEMP-TABLE depa_mtrl    
    FIELD ENR AS CHARACTER
    FIELD ANTAL AS INTEGER    
    FIELD DEP-NR AS INTEGER
    INDEX ENR IS PRIMARY DEP-NR ENR ASCENDING.
    /*snatdepå*/
