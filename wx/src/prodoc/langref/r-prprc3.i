/* r-prprc3.i */

MESSAGE "My-Name argument in r-prprc3.i is" "{&My-Name}" + "." 
    VIEW-AS ALERT-BOX.
&SCOPED-DEFINE My-Name "Donald"
MESSAGE "My-Name preprocessed in r-prprc3.i is" {&My-Name} + "." 
    VIEW-AS ALERT-BOX.
