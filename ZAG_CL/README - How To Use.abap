**********************************************************************
"ZAG_CL_CSV_XLSX - EXAMPLE
**********************************************************************
  SELECT * FROM SFLIGHT UP TO 10 ROWS INTO TABLE @DATA(lt_sflight).

  zag_cl_csv_xlsx=>download(
    EXPORTING
      x_filename              = '/tmp/test.csv'
      x_header                = 'X'              " Presenza riga testata ( 'X' = True )
      xt_sap_data             = lt_sflight
      x_source                = 'S'   " 'SERV' / 'LOCL'
    EXCEPTIONS
      not_supported_file      = 1
      unable_open_path        = 2
      unable_define_structure = 3
      OTHERS                  = 4
  ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


  REFRESH lt_sflight.
  zag_cl_csv_xlsx=>upload(
    EXPORTING
      x_filename              = '/tmp/test.csv'
      x_header                = 'X'              " Presenza riga testata ( 'X' = True )
      x_source                = 'S'   " 'LOCL'/'SERV'
    IMPORTING
      yt_sap_data             = lt_sflight
    EXCEPTIONS
      not_supported_file      = 1
      unable_open_path        = 2
      unable_define_structure = 3
      OTHERS                  = 4
  ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  DATA(lv_filename) = |{ zag_cl_csv_xlsx=>get_desktop_directory( ) }/test.xlsx|.
  zag_cl_csv_xlsx=>download(
    EXPORTING
      x_filename              = lv_filename
      x_header                = 'X'              " Presenza riga testata ( 'X' = True )
      xt_sap_data             = lt_sflight
      x_source                = 'L'              " 'S' / 'L'
    EXCEPTIONS
      not_supported_file      = 1
      unable_open_path        = 2
      unable_define_structure = 3
      OTHERS                  = 4
  ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  REFRESH lt_sflight.
  zag_cl_csv_xlsx=>upload(
    EXPORTING
      x_filename              = lv_filename
      x_header                = 'X'              " Presenza riga testata ( 'X' = True )
      x_source                = 'L'              " 'L'/'S'
    IMPORTING
      yt_sap_data             = lt_sflight
    EXCEPTIONS
      not_supported_file      = 1
      unable_open_path        = 2
      unable_define_structure = 3
      OTHERS                  = 4
  ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


**********************************************************************
"ZAG_CL_SALV - EXAMPLE
**********************************************************************
  TYPES: BEGIN OF ty_alv,
           icon  TYPE icon_d,
           matnr TYPE mara-matnr,
           ersda TYPE mara-ersda,
           ernam TYPE mara-ernam,
           laeda TYPE mara-laeda,
           aenam TYPE mara-aenam,
           t_col TYPE lvc_t_scol,
         END OF ty_alv.

  DATA: gt_alv       TYPE TABLE OF ty_alv,
        lv_fieldname TYPE fieldname.

  DATA(lo_salv) = NEW zag_cl_salv( xt_table = gt_alv[] ).


  SELECT * FROM mara UP TO 10 ROWS INTO TABLE @DATA(lt_mara).
  LOOP AT lt_mara ASSIGNING FIELD-SYMBOL(<mara>).

    "Set Data
    "-------------------------------------------------
    APPEND INITIAL LINE TO gt_alv ASSIGNING FIELD-SYMBOL(<alv>).
    MOVE-CORRESPONDING <mara> TO <alv>.


    "Set Icon
    "-------------------------------------------------
    DATA(lv_diff) = sy-datum - <mara>-ersda.
    IF lv_diff MOD 2 EQ 0.
      <alv>-icon = zag_cl_salv=>c_icon_green.
    ELSE.
      <alv>-icon = zag_cl_salv=>c_icon_red.
    ENDIF.



    lv_diff = sy-datum - <mara>-ersda.
    IF lv_diff MOD 2 EQ 0.

      "Set Cell Color
      "-------------------------------------------------
      lv_fieldname = 'ERSDA'.
      lo_salv->set_color_cell(
        EXPORTING
          x_color             = zag_cl_salv=>c_cell_col_green
          x_fieldname         = lv_fieldname
        CHANGING
          y_row               = <alv>
        EXCEPTIONS
          col_tab_not_found   = 1
          fieldname_not_found = 2
          OTHERS              = 3
      ).

    ELSE.

      "Set Row Color
      "-------------------------------------------------
      lo_salv->set_color_row(
        EXPORTING
          x_color           = zag_cl_salv=>c_cell_col_red
        CHANGING
          y_row             = <alv>
        EXCEPTIONS
          col_tab_not_found = 1
          OTHERS            = 2
      ).

    ENDIF.
  ENDLOOP.


  DATA: lt_col_settings TYPE zag_cl_salv=>tt_col_settings.

  lt_col_settings = VALUE #(
    ( fieldname = 'MATNR'
      label     = 'My Material'
      hotspot   = ' ' )

    ( fieldname = 'ERSDA'
      label     = 'Data Creation'
      hotspot   = ' ' )

    ( fieldname = 'aenam'
      label     = 'Creator'
      hotspot   = 'X' )
   ).

  lo_salv->display_generic_alv(
  EXPORTING
*      x_popup         = abap_true
    xt_col_settings = lt_col_settings
    xt_output       = gt_alv[]
).


