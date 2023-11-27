**********************************************************************
"ZAG_CL_SEND_MAIL_BCS - EXAMPLE
**********************************************************************
  DATA: lt_recipients TYPE zag_cl_send_mail_bcs=>tt_recipients,
        lt_attch      TYPE zag_cl_send_mail_bcs=>tt_bcs_attch.

  SELECT * FROM sflight UP TO 10 ROWS INTO TABLE @DATA(lt_sflight).

  zag_cl_csv_xlsx=>get_compdescr_from_data(
    EXPORTING
      xt_sap_table            = lt_sflight
    IMPORTING
      yo_structdescr          = DATA(lo_structdescr)
    EXCEPTIONS
      unable_define_structure = 1
      OTHERS                  = 2
  ).

  APPEND INITIAL LINE TO lt_attch ASSIGNING FIELD-SYMBOL(<attch>).
  <attch>-subject = 'your_file_name'.
  
  APPEND INITIAL LINE TO <attch>-data_csv ASSIGNING FIELD-SYMBOL(<csv_line>).

  zag_cl_csv_xlsx=>get_header_from_data(
    EXPORTING
      xt_sap_table  = lt_sflight
    CHANGING
      y_str_header  = <csv_line>
  ).

  LOOP AT lt_sflight ASSIGNING FIELD-SYMBOL(<sflight>).

    APPEND INITIAL LINE TO <attch>-data_csv ASSIGNING <csv_line>.

    zag_cl_csv_xlsx=>conv_sap_to_string(
      EXPORTING
        xo_structdescr = lo_structdescr
        x_sap_data     = <sflight>
      IMPORTING
        y_str_data     = <csv_line>
    ).

  ENDLOOP.

  APPEND INITIAL LINE TO lt_recipients ASSIGNING FIELD-SYMBOL(<recipient>).
  <recipient>-smtp_addr = 'yourmail@domain.com'.

  zag_cl_send_mail_bcs=>send_mail_bcs(
    EXPORTING
      x_sender         = sy-uname
      xt_recipients    = lt_recipients
      x_mail_obj       = 'ZAG Mail'
      x_mail_body_str  = 'This is a mail generated with ZAG Library'
*      x_mail_body_so10 =
      xt_attch         = lt_attch
    IMPORTING
      y_error_msg      = DATA(lv_error_msg)
    EXCEPTIONS
      request_error    = 1
      sender_error     = 2
      recipient_error  = 3
      body_error       = 4
      attachment_error = 5
      OTHERS           = 6
  ).
  IF sy-subrc <> 0.
    WRITE lv_error_msg.
  ENDIF.


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
           zeinr TYPE mara-zeinr,

           t_col TYPE lvc_t_scol, "Use this if you want colors in output
         END OF ty_alv.

  DATA: gt_alv       TYPE TABLE OF ty_alv,
        lv_fieldname TYPE fieldname.

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

      "Set Single Cell Color
      "-------------------------------------------------
      lv_fieldname = 'ERSDA'.
      zag_cl_salv=>set_color_cell(
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
      IF sy-subrc <> 0.
        "Handle exceptions
      ENDIF.

    ELSE.

      "Set Row Color
      "-------------------------------------------------
      zag_cl_salv=>set_color_row(
        EXPORTING
          x_color           = zag_cl_salv=>c_cell_col_red
        CHANGING
          y_row             = <alv>
        EXCEPTIONS
          col_tab_not_found = 1
          OTHERS            = 2
      ).
      IF sy-subrc <> 0.
        "Handle exceptions
      ENDIF.

    ENDIF.
  ENDLOOP.


  DATA: lt_col_settings TYPE zag_cl_salv=>tt_col_settings.

  lt_col_settings = VALUE #(
    ( fieldname = 'MATNR'
      label     = 'My Material'
      no_out    = '' )

    ( fieldname = 'ERSDA'
      label     = 'Data Creation' )

    ( fieldname = 'ZEINR'
      no_out    = 'X' )

   ).

  zag_cl_salv=>display_generic_alv(
    EXPORTING
      x_popup         = abap_true
      xt_col_settings = lt_col_settings[]
      xt_output       = gt_alv[]
    EXCEPTIONS
      general_fault   = 1
      OTHERS          = 2
  ).
  IF sy-subrc <> 0.
    "Handle exceptions
  ENDIF.
