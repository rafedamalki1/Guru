/* Include file for Language Tutorial Chapter 11 Exercises */

DEFINE SUB-MENU sm-Open
    MENU-ITEM mi-Cust        LABEL "&Customer"
    MENU-ITEM mi-Order       LABEL "&Order".   
    
DEFINE SUB-MENU sm-Table
    SUB-MENU sm-Open         LABEL "O&pen"
    MENU-ITEM mi-Exit        LABEL "E&xit".  
     
DEFINE SUB-MENU sm-Reports
    MENU-ITEM mi-Cust        LABEL "&Monthly Summary"
    MENU-ITEM mi-Labels      LABEL "Mailing &Labels"
    RULE
    MENU-ITEM mi-Balances    LABEL "Order Tot&als" DISABLED
    MENU-ITEM mi-Today       LABEL "Order &Items"  DISABLED
    RULE
    MENU-ITEM mi-Print       LABEL "&Output to Printer" TOGGLE-BOX.
    
DEFINE SUB-MENU sm-Help
    MENU-ITEM mi-Help        LABEL "H&elp". 
    
DEFINE MENU mbar MENUBAR
    SUB-MENU  sm-Table       LABEL "&Tables"
    SUB-MENU  sm-Reports     LABEL "&Reports"
    SUB-MENU  sm-Help        LABEL "&Help".  
    
ASSIGN DEFAULT-WINDOW:MENUBAR = MENU mbar:HANDLE. 
