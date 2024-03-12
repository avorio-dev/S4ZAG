*&---------------------------------------------------------------------*
*& Report zag_amdp_test
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zag_amdp_test.

TABLES: mara.

DATA: gt_mara TYPE zag_amdp_demo_001=>tt_result.

PARAMETERS: p_matnr TYPE mara-matnr DEFAULT '000000000000000210'.

SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS: s_matnr FOR mara-matnr,
                s_aenam FOR mara-aenam.

START-OF-SELECTION.

  "Check IF System supports AMDP
  IF NOT cl_abap_dbfeatures=>use_features(
           requested_features = VALUE #(
                                     ( cl_abap_dbfeatures=>call_amdp_method )
                                     ( cl_abap_dbfeatures=>amdp_table_function )
                                       )
         ).

    cl_demo_output=>display( 'System does not suport AMDP' ).
    STOP.
  ENDIF.

  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  zag_amdp_demo_001=>get_data_with_params(
    EXPORTING
      xp_client = sy-mandt
      xp_matnr  = p_matnr
      xp_spras  = sy-langu
    IMPORTING
      yt_mara   = gt_mara[]
  ).

  "cl_demo_output=>display_data( EXPORTING value = gt_mara ).


  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  DATA(lv_where) = cl_shdb_seltab=>combine_seltabs(
                     it_named_seltabs = VALUE #(
                                              ( name = 'MATNR' dref = REF #( s_matnr[] ) )
                                              ( name = 'AENAM' dref = REF #( s_aenam[] ) )
                                        )
                     iv_client_field  = 'MANDT'
                   ).

  WRITE: lv_where.

  zag_amdp_demo_001=>get_data_with_selopt(
    EXPORTING
      xp_client     = sy-mandt
      xp_where_cond = lv_where
    IMPORTING
      yt_mara       = gt_mara[]
  ).

  cl_demo_output=>display_data( EXPORTING value = gt_mara ).

  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""