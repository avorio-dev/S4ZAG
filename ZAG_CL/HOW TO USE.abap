**********************************************************************
" Below there are examples on how to use the library classes

" EXAMPLE - ZAG_CL_SALV
" EXAMPLE - ZAG_CL_CSV_XLSX
" EXAMPLE - ZAG_CL_SEND_MAIL_BCS
**********************************************************************


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
  ).

**********************************************************************
"ZAG_CL_CSV_XLSX - EXAMPLE
**********************************************************************
  " WHY USE IT?

  " - DOWNLOAD
  "     It allows to Download CSV and XLSX on Local or Server destination

  " - UPLOAD
  "     It allows to Upload CSV from Local or Server
  "     It allows to Upload XLSX from Local

  " - It provieds utility Methods like:
  "   - CONV_SAP_TO_STRING / CONV_STRING_TO_SAP
  "         allows to convert any sap line into a csv string and vice versa.
  "         Also, it automatically apply conversion for Date / Time and Numbers
  "
  "   - F4_HELP_DIR_INPUT
  "       provide a matchcode for directory in input or output
  "
  "   - GET_FIELDCAT_FROM_ITAB
  "       provide the fieldcat of any table
  "
  "   - REMOVE_SPECIAL_CHAR
  "       remove special characters from a string, and it can be expanded with new charset

**********************************************************************

    SELECT * FROM sflight UP TO 10 ROWS INTO TABLE @DATA(lt_sflight).

  DATA(lv_source) = ''.
  DATA(lv_filename) = zag_cl_csv_xlsx=>get_desktop_directory( ).

  "Example 1 -> Download CSV on Server / Local
  "-------------------------------------------------
  lv_source   = 'S'. "S - Server / L - Local
  lv_filename = |/tmp/test.csv|.
  zag_cl_csv_xlsx=>download(
    EXPORTING
      x_filename                = lv_filename
      x_source                  = lv_source " 'S' / 'L'
    CHANGING
      xt_sap_data               = lt_sflight
    EXCEPTIONS
      not_supported_file        = 1
      unable_open_path          = 2
      unable_define_structdescr = 3
      OTHERS                    = 4
  ).
  IF sy-subrc <> 0.
    "Handle Exception
  ENDIF.

  "Example 2 -> Download XLSX on Server / Local
  "-------------------------------------------------
  lv_filename = zag_cl_csv_xlsx=>get_desktop_directory( ).
  lv_source   = 'L'. "S - Server / L - Local
  lv_filename = |{ lv_filename }/test.xlsx|.
  zag_cl_csv_xlsx=>download(
    EXPORTING
      x_filename                = lv_filename
      x_source                  = lv_source  " 'S' / 'L'
    CHANGING
      xt_sap_data               = lt_sflight
    EXCEPTIONS
      not_supported_file        = 1
      unable_open_path          = 2
      unable_define_structdescr = 3
      OTHERS                    = 4
  ).
  IF sy-subrc <> 0.
    "Handle Exception
  ENDIF.


  REFRESH lt_sflight.

  "Example 3 - Upload XLSX from Local
  "-------------------------------------------------
  DATA lt_conversion_errors TYPE zag_cl_csv_xlsx=>tt_conversions_errors.
  zag_cl_csv_xlsx=>upload(
    EXPORTING
      x_filename                = lv_filename
      x_header                  = 'X'
      x_source                  = 'L'              " 'L'/'S'
    IMPORTING
      yt_sap_data               = lt_sflight
      yt_conversions_errors     = lt_conversion_errors
    EXCEPTIONS
      input_error               = 1
      not_supported_file        = 2
      unable_open_path          = 3
      unable_define_structdescr = 4
      empty_file                = 5
      conversion_error          = 6
      OTHERS                    = 7
  ).
  IF sy-subrc <> 0.
    "Handle Excepion
  ENDIF.


  "SHOW RESULTS
  "-------------------------------------------------
  IF lines( lt_conversion_errors ) GT 0.

    zag_cl_salv=>display_generic_alv(
      EXPORTING
        x_popup             = abap_true
        xt_output           = lt_conversion_errors
      EXCEPTIONS
        salv_creation_error = 1
        OTHERS              = 2
    ).
    IF sy-subrc <> 0.
      "Handle Exception
    ENDIF.

  ELSE.

    zag_cl_salv=>display_generic_alv(
      EXPORTING
        x_popup             = abap_false
        xt_output           = lt_sflight
      EXCEPTIONS
        salv_creation_error = 1
        OTHERS              = 2
    ).
    IF sy-subrc <> 0.
      "Handle Exception
    ENDIF.

  ENDIF.


**********************************************************************
"ZAG_CL_SEND_MAIL_BCS - EXAMPLE
**********************************************************************
  DATA: lt_recipients TYPE zag_cl_send_mail_bcs=>tt_recipients,
        lt_attch      TYPE zag_cl_send_mail_bcs=>tt_bcs_attch.

  SELECT * FROM sflight UP TO 10 ROWS INTO TABLE @DATA(lt_data).

  zag_cl_csv_xlsx=>get_compdescr_from_data(
    EXPORTING
      xt_sap_table            = lt_data
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
      xt_sap_table  = lt_data
    CHANGING
      y_str_header  = <csv_line>
  ).

  LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<sflight>).

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
      x_sender                  = sy-uname
      xt_recipients             = lt_recipients
      x_mail_obj                = 'ZAG Mail'
      x_mail_body_str           = 'This is a mail generated with ZAG Library'
*      x_mail_body_standard_text =
      xt_attachments            = lt_attch
    IMPORTING
      y_mail_sent               = DATA(lv_mail_sent)
      y_error_msg               = DATA(lv_error_msg)
    EXCEPTIONS
      request_error             = 1
      sender_error              = 2
      recipient_error           = 3
      body_error                = 4
      attachment_error          = 5
      OTHERS                    = 6
  ).
  IF sy-subrc <> 0.
    WRITE lv_error_msg.
  ENDIF.




