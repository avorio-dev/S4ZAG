@EndUserText.label: 'Table Function'
define table function ZAG_TF_01
with parameters @Environment.systemField: #CLIENT 
                p_clnt: abap.clnt,
                p_bukrs : bukrs
returns {
  key mandt : abap.clnt;
  key ebeln : ebeln;
  bukrs : bukrs;
  bstyp : bstyp;
  bsart : bsart;
  loekz : loekz;
  aedat : aedat;
  ernam : ernam;
}
implemented by method zag_academy_amdp=>get_zag_tf_01;