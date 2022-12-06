/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*--------------------------------------------------------------------

File: prodict/gate/gat_pul2.i

Description:
    calculates formats and decimals of fields
    
interface:
    l_char-types    -> character, decimals = length
    l_chrw-types    -> character, decimals = ? (raw-data-types)
    l_date-types    -> date
    l_dcml-types    -> fixed point decimal
    l_floa-types    -> floating point decimal
    l_logi-types    -> logical
    l_time-types    -> time
    l_tmst-types    ->  time-stamp
    
Included in:            
    prodict/odb/odb_pul.i
    prodict/ora/_ora_prc.p
    prodict/gate/gat_pul.i
    
History:
    hutegger    95/03   combined from odb, ora and syb4

--------------------------------------------------------------------*/

if      CAN-DO(l_char-types,s_ttb_fld.ds_type)    /**** CHARACTERS ****/
 then assign
         l_dcml = s_ttb_fld.ds_lngth
         l_frmt    = ( if ENTRY(dtyp,user_env[17],"|") <> "c"
                      then ENTRY(dtyp,user_env[17],"|")
                      else "x(" + STRING(min(320,max(1,l_dcml))) + ")" 
                   ).

else if CAN-DO(l_chrw-types,s_ttb_fld.ds_type)          /**** RAWS ****/
     then assign
         l_dcml  = (if s_ttb_fld.ds_lngth = 0
                     then 8
                     else s_ttb_fld.ds_lngth
                   )
         l_frmt    = ( if ENTRY(dtyp,user_env[17],"|") <> "c"
                      then ENTRY(dtyp,user_env[17],"|")
                      else "x(" + STRING(min(320,max(1,l_dcml))) + ")" 
                   )
         l_dcml = ?.

else if CAN-DO(l_date-types,s_ttb_fld.ds_type)         /**** DATES ****/
     then assign
      l_dcml = ?
      l_frmt   = ( if ENTRY(dtyp,user_env[17],"|") = "d"
                  then "99/99/99"
                  else ENTRY(dtyp,user_env[17],"|")
               ).

else if CAN-DO(l_dcml-types,s_ttb_fld.ds_type)      /**** DECIMALS ****/
     then DO:
       assign
         l_prec  = ( if s_ttb_fld.ds_radix = 2
                       then 0.30103
                       else 1
                   )
         l_dcml  = min(10,integer(s_ttb_fld.ds_scale * l_prec))
         l_prec  = integer(s_ttb_fld.ds_prec * l_prec)
         l_frmt  = ( if      ENTRY(dtyp,user_env[17],"|") = "i"
                      then "->>>>>>>>9"
                      else ENTRY(dtyp,user_env[17],"|")
                   )
         l_dcml  = ( if      ENTRY(dtyp,user_env[17],"|") = "#"
                      then l_dcml
                     else if ENTRY(dtyp,user_env[17],"|") = "i"
                      or     INDEX(l_frmt,".")            =  0
                      then 0
                      else LENGTH(l_frmt, "character") - INDEX(l_frmt,".")
                   ).
       /* description of l_prec and l_dcml; (l_prec: sign does, decimal 
        *   point doesn't count). Example:   l_prec = 18; l_dcml = 8
        *          l_prec
        *   |---------x-------|
        *   ->>>>>>>>9.99999999          [->(8)9.9(8)
        *              |------| 
        *               l_dcml
        * Maximum length of format is 30 characters (dec-point counts)
        */
       if ENTRY(dtyp,user_env[17],"|") = "#"
        then do:  /* format to generate out of ora-info */
         if l_dcml = ? then assign l_dcml = 0.
         if  l_prec = ?
          then assign    /*  ?,?  =>  ->>>>>>>>9  */
           ntyp    = ( if user_dbtype = "ORACLE"
                        then "integer"
                        else "decimal"
                     )
           l_dcml  = 0
           l_frmt  = "-"
                   + FILL(">",MINIMUM(8,s_ttb_fld.ds_lngth - 2))
                   + "9".
         else if l_prec = 1
          and    l_dcml = 0
          then assign   /* 1,0 =>  yes/no  */
           ntyp    = ( if user_dbtype = "ORACLE"
                        then "logical"
                        else "decimal"
                     )
           l_dcml  = 0
           l_frmt  = ( if user_dbtype = "ORACLE"
                        then "yes/no"
                        else "9"
                     ).
         else if l_prec <= 9
          and    l_dcml <= 0
          then assign   /* 6,-2 =>  ->>999 [->(2)9(3)] */
           i       = 1 - l_dcml
           ntyp    = ( if user_dbtype = "ORACLE"
                        then "integer"
                        else "decimal"
                     )
           l_dcml  = 0
           l_frmt  = "-"
                   + FILL(">",l_prec - i - 1)
                   + FILL("9",i).
         else if l_dcml <= 0
          then assign    /* 15,-2 =>  ->>>>>>>>>>>999 [->(11)9(3)] */ 
           i       = MINIMUM(29,1 - l_dcml)
           l_dcml  = 0
           l_frmt  = "-"
                   + FILL(">",MINIMUM(29,l_prec) - i - 1)
                   + FILL("9",i).
          else assign    /* 15,2  =>  ->>>>>>>>>>>9.99 [->(11)9.9(2)] */
           l_frmt = "-"
                  + FILL(">",MINIMUM(29,l_prec) - l_dcml - 2)
                  + "9." 
                  + FILL("9",l_dcml).
         end.     /* format to generate out of ora-info */
       end.

else if CAN-DO(l_floa-types,s_ttb_fld.ds_type)        /**** FLOATS ****/
     then DO:
       assign
        l_prec = ( if s_ttb_fld.ds_radix = 2
                    then INTEGER(s_ttb_fld.ds_prec * 0.30103)
                    else s_ttb_fld.ds_prec
                 ).
       if      ENTRY(dtyp,user_env[17],"|")  = "i"
        then assign /* new data-type and format for INTEGER */
             l_frmt    = "->>>>>>>>9"
             l_dcml = 0.
       else if ENTRY(dtyp,user_env[17],"|") <> "#"
        then assign /* new data-type and format from _ora_typ.p */
         l_frmt    = ENTRY(dtyp,user_env[17],"|")
         l_dcml = ( if INDEX(l_frmt,".") = 0
                      then 0
                      else LENGTH(l_frmt, "character") - INDEX(l_frmt,".")
                   ).
       else if l_prec = ?
        then assign    /* ? (:=30) =>  ->>>>>>>>>>>>>>>>>>9.9<<<<<<<<< */
         l_dcml = 10
         l_frmt    = "-"
                 + FILL(">",17)
                 + "9.9" 
                 + FILL("<",l_dcml - 1).
       else if l_prec <= 22
        then assign    /* 12    =>  ->>>>>9.9<<<< */
         l_dcml = 1 + integer(l_prec / 2 - 1.6)
         l_frmt    = "-"
                 + FILL(">",l_prec - l_dcml - 2)
                 + "9.9" 
                 + FILL("<",l_dcml - 1).
        else assign    /* 26    =>  ->>>>>>>>>>>>>9.9<<<<<<<<<< */
         l_dcml = 1 + MINIMUM(9,integer(l_prec / 2 - 1.6))
         l_frmt    = "-"
                 + FILL(">",MINIMUM(17,l_prec - l_dcml - 2))
                 + "9.9" 
                 + FILL("<",l_dcml - 1).
       end.

else if CAN-DO(l_i#dl-types,s_ttb_fld.ds_type)      /**** INTEGERS ****/
     then assign
      l_frmt   = ( if    ENTRY(dtyp,user_env[17],"|") = "i"
                    then "->>>>>>>>9"
                   else if ENTRY(dtyp,user_env[17],"|") = "l"
                    then "yes/no"
                   else if ENTRY(dtyp,user_env[17],"|") = "#"
                    then "->>>>>>>>>>>>>>9"  /* ->(14)9 */
                    else ENTRY(dtyp,user_env[17],"|")
                 ).

else if CAN-DO(l_i##d-types,s_ttb_fld.ds_type)      /**** INTEGERS ****/
     then assign
      l_frmt   = ( if    ENTRY(dtyp,user_env[17],"|") = "i"
                    then "->>>>>>>>9"        /* ->(8)9 */
                   else if ENTRY(dtyp,user_env[17],"|") = "#"
                    then "->>>>>>>>>>>>>>9"  /* ->(14)9 */
                    else ENTRY(dtyp,user_env[17],"|")
                 ).

else if CAN-DO(l_i###-types,s_ttb_fld.ds_type)      /**** INTEGERS ****/
     then assign
      l_frmt   = ( if      ENTRY(dtyp,user_env[17],"|") = "i"
                    then "->>>>>>>>9"        /* ->(8)9 */
                    else ENTRY(dtyp,user_env[17],"|")
                 ).

else if CAN-DO(l_logi-types,s_ttb_fld.ds_type)      /**** LOGICALS ****/
     then assign
      l_frmt   = ( if ENTRY(dtyp,user_env[17],"|") = "l"
                    then "yes/no"
                    else ENTRY(dtyp,user_env[17],"|")
                 ).

/*
else if CAN-DO(l_time-types,s_ttb_fld.ds_type)    /**** TIME-PARTS ****/
     then assign
      l_frmt   = ( if      ENTRY(dtyp,user_env[17],"|") = "i"
                    then "->>>>>>>>9"        /* ->(8)9 */
                    else ENTRY(dtyp,user_env[17],"|")
                 ).
*/

else if CAN-DO(l_tmst-types,s_ttb_fld.ds_type)   /**** TIME-STAMPS ****/
     then assign
         l_dcml = ?
         l_frmt    = ( if       ENTRY(dtyp,user_env[17],"|") = "c"
                         then "x(26)"
                       else if  ENTRY(dtyp,user_env[17],"|") = "d"
                         then "99/99/99"
                         else ENTRY(dtyp,user_env[17],"|")
                     ).

     else do:                                    /**** OTHERWISE ****/
      run error_handling
        ( 2, 
          s_ttb_fld.ds_type, 
          s_ttb_tbl.ds_name + " Field#: " + s_ttb_fld.ds_name
        ).
      run error_handling ( 3, "", "" ).
/*
       message "Data-type " s_ttb_fld.ds_type " is not supported."  skip
               "skipping this field ..."
               view-as alert-box error buttons ok.
*/
       UNDO, {&undo}.  /* check */        
       end.


/*------------------------------------------------------------------*/

assign
  s_ttb_fld.pro_type    = ntyp
  s_ttb_fld.pro_dcml    = l_dcml
  s_ttb_fld.pro_case    = (ntyp = "character") /* changes if shadow exists*/
  s_ttb_fld.ds_stdtype  = INTEGER(ENTRY(dtyp,user_env[14]))
  s_ttb_fld.pro_frmt    = ( if l_frmt = "?"
                             then s_ttb_fld.pro_frmt
                             else l_frmt
                          )
  s_ttb_fld.pro_init    = ( if (l_init = "?" or l_init = ?)
                             then ?
                            else if ntyp = "logical"
                             then string(l_init = "0")
                             else l_init
                          ).

/*------------------------------------------------------------------*/
