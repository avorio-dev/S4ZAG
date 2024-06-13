# Classes Example List
## 1. [ZAG_CL_SALV](#zag_cl_salv)
## 2. [ZAG_CL_CSV_XLSX](#zag_cl_csv_xlsx)
## 3. [ZAG_CL_SEND_MAIL](#zag_cl_send_mail)
## 4. [ZAG_CL_SALV_IDA](#zag_cl_salv_ida)

```abap
" Below there are examples on how to use the library classes.
" The target of this classes is to USE THEM, not to adapt! 
" In this way, no time will be wasted!!!
" So, if you have suggestions, please contact me :)

" If you want to use them, it will be enough to copy the code of the whole class
" and paste it into an include if you are in a report, or in the code-based builder of SE24.
" NB If you are in a report, 
" you will need to comment the following line code at the beginning of the class declaration.
"   final
"   create public .
```

## 1. ZAG_CL_SALV <a name="zag_cl_salv"></a>

```abap
**********************************************************************
  "ZAG_CL_SALV - EXAMPLE
**********************************************************************
  " - DISPLAY_GENERIC_ALV
  "     It allows to Display whatever table you want, providing only the table itself
  "     You can also pass additional parameters like
  "       - Display in popup
  "       - Column Settings in which you can give provide your labels or hide fields
  "
  " - GET_FIELDCAT
  "     It allows to extract Fieldcat from both table or simple row which you provide
  "
  " - SET_COLOR_CELL / SET_COLOR_ROW
  "     It allows to set Color tab which will be printed
  "     The only constraint is that you will need to have a component in your types
  "     named T_COL TYPE lvc_t_scol

**********************************************************************

  "Example 1 -> Display a generic ALV
  "-------------------------------------------------

  SELECT * FROM mara UP TO 10 ROWS INTO TABLE @DATA(lt_mara).

  zag_cl_salv=>display_generic_alv( lt_mara ).


  "Example 2 -> Set colors for cells and / or rows
  "          -> Set Labels / Hide fields
  "-------------------------------------------------

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

  DATA: gt_alv       TYPE TABLE OF ty_alv.


  LOOP AT lt_mara ASSIGNING FIELD-SYMBOL(<mara>).

    "Set Data
    APPEND INITIAL LINE TO gt_alv ASSIGNING FIELD-SYMBOL(<alv>).
    MOVE-CORRESPONDING <mara> TO <alv>.


    "Set Icon
    DATA(lv_diff) = sy-datum - <mara>-ersda.
    <alv>-icon = COND #(
      WHEN lv_diff MOD 2 EQ 0 THEN zag_cl_salv=>cc_icon-green
      ELSE zag_cl_salv=>cc_icon-red
    ).


    lv_diff = sy-datum - <mara>-ersda.
    IF lv_diff MOD 2 EQ 0.

      "Set Single Cell Color
      zag_cl_salv=>set_color_cell(
        EXPORTING
          xs_color            = VALUE #( col = zag_cl_salv=>cc_cell_col-green
                                         int = 1
                                         inv = 1 )
          xt_fieldname        = VALUE #( ( 'MATNR' ) )
        CHANGING
          y_row               = <alv>
        EXCEPTIONS
          col_tab_not_found   = 1
          fieldname_not_found = 2
          OTHERS              = 3
      ).

    ELSE.

      "Set Row Color
      zag_cl_salv=>set_color_row(
        EXPORTING
          xs_color          = VALUE #( col = zag_cl_salv=>cc_cell_col-red
                                       int = 0
                                       inv = 0 )
        CHANGING
          ys_row            = <alv>
        EXCEPTIONS
          col_tab_not_found = 1
          fcat_not_found    = 2
          OTHERS            = 3
      ).

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
      xv_popup            = abap_true
      xt_col_settings     = lt_col_settings
      xt_output           = gt_alv
    EXCEPTIONS
      salv_creation_error = 1
      others              = 2
  ).
```


## 2. ZAG_CL_CSV_XLSX <a name="zag_cl_csv_xlsx"></a>

```abap
**********************************************************************
  "ZAG_CL_CSV_XLSX - EXAMPLE
**********************************************************************
  SELECT * UP TO 10 ROWS FROM but000 INTO TABLE @DATA(lt_but000).

  DATA(lo_csv_xlsx) = NEW zag_cl_csv_xlsx(
    xt_sap_table = lt_but000
  ).

  "DOWNLOAD
  "-------------------------------------------------

  "Download Example - LOCAL
  lo_csv_xlsx->file_download(
    EXPORTING
      x_filename         = |{ zag_cl_csv_xlsx=>get_desktop_directory( ) }/my_zag_file.csv|
      x_source           = zag_cl_csv_xlsx=>cc_file_source-local
    EXCEPTIONS
      not_supported_file = 1
      unable_open_path   = 2
      OTHERS             = 3
  ).
  IF sy-subrc <> 0.
  ENDIF.


  "Download Example - SERVER
  lo_csv_xlsx->file_download(
    EXPORTING
      x_filename         = '/tmp/my_zag_file.csv'
      x_source           = zag_cl_csv_xlsx=>cc_file_source-server
    EXCEPTIONS
      not_supported_file = 1
      unable_open_path   = 2
      OTHERS             = 3
  ).
  IF sy-subrc <> 0.
  ENDIF.



  "UPLOAD
  "-------------------------------------------------

  DATA lt_conversion_errors TYPE zag_cl_csv_xlsx=>tt_conversions_errors.

  "Upload Example - LOCAL
  lo_csv_xlsx->file_upload(
    EXPORTING
      x_filename            = |{ zag_cl_csv_xlsx=>get_desktop_directory( ) }/my_zag_file.csv|
      x_header              = 'X'
      x_source              = zag_cl_csv_xlsx=>cc_file_source-local
    IMPORTING
      yt_sap_data           = lt_but000
      yt_conversions_errors = lt_conversion_errors
    EXCEPTIONS
      input_error           = 1
      not_supported_file    = 2
      unable_open_path      = 3
      empty_file            = 4
      conversion_error      = 5
      OTHERS                = 6
  ).
  IF sy-subrc <> 0.
  ENDIF.

  "Upload Example - SERVER
  lo_csv_xlsx->file_upload(
    EXPORTING
      x_filename            = 'tmp/my_zag_file.csv'
      x_header              = 'X'
      x_source              = zag_cl_csv_xlsx=>cc_file_source-server
    IMPORTING
      yt_sap_data           = lt_but000
      yt_conversions_errors = lt_conversion_errors
    EXCEPTIONS
      input_error           = 1
      not_supported_file    = 2
      unable_open_path      = 3
      empty_file            = 4
      conversion_error      = 5
      OTHERS                = 6
  ).
  IF sy-subrc <> 0.
  ENDIF.
```

## 3. ZAG_CL_SEND_MAIL <a name="zag_cl_send_mail"></a>


```abap
**********************************************************************
  "ZAG_CL_SEND_MAIL_BCS - EXAMPLE
**********************************************************************
  " WHY USE IT?

  " - SEND_MAIL_BCS
  "     In addition to the classic parameters such as Recipient / Email Subject / Email Body,
  "     it allows attaching files like CSV, XLSX, and PDF.
  "     Furthermore, you will be able to use a Standard Text created by Trx SO10 as the Email Body.
  "     It will be sufficient to provide the name of the Standard Text and, if necessary,
  "     you will be able to specify a variable name in the Standard Text
  "     (for example &LIFNR&) and the corresponding value. The class will automatically
  "     perform the substitutions.

  "    NB: If you not provide any mail body, the class will automatically put the subject mail as body
  "        because it is a mandatory parameter.

**********************************************************************


  DATA: lt_recipients TYPE zag_cl_send_mail=>tt_recipients.

  SELECT * FROM sflight UP TO 10 ROWS INTO TABLE @DATA(lt_data).


  "Example 1 -> Simple mail
  "-------------------------------------------------

  lt_recipients = VALUE #(
    ( smtp_addr = 'yourmail@domain.com'
      copy      = space                 )
  ).

  zag_cl_send_mail=>send_mail(
    EXPORTING
      x_sender                  = sy-uname
      xt_recipients             = lt_recipients
      x_mail_obj                = 'Mail Object'
      x_mail_body_str           = 'Mail Body'
*      x_mail_body_standard_text =
*      xt_attachments            =
      x_commit                  = abap_true
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



  "Example 2 -> Mail with attach. and Standard Text as Mail Body
  "-------------------------------------------------

  DATA: ls_mail_body_stdtxt TYPE zag_cl_send_mail=>ty_standard_text,
        lt_attch            TYPE zag_cl_send_mail=>tt_bcs_attch.


  lt_recipients = VALUE #(
      ( smtp_addr = 'yourmail@domain.com'
        copy      = space                 )
    ).

  APPEND INITIAL LINE TO lt_attch ASSIGNING FIELD-SYMBOL(<attch>).
  <attch>-subject = 'your_file_name'.


  DATA(lo_csv_xlsx) = NEW zag_cl_csv_xlsx( xt_sap_table = lt_data ).
  lo_csv_xlsx->get_fieldcat_from_data(
    EXPORTING
      xt_sap_table              = lt_data
    IMPORTING
      yt_fcat                   = DATA(lt_fcat)
      yo_structdescr            = DATA(lo_structdescr)
    EXCEPTIONS
      unable_define_structdescr = 1
      OTHERS                    = 2
  ).


  APPEND INITIAL LINE TO <attch>-data_csv ASSIGNING FIELD-SYMBOL(<csv_line>).
  <csv_line> = lo_csv_xlsx->get_header_from_data( ).

  LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<sflight>).

    APPEND INITIAL LINE TO <attch>-data_csv ASSIGNING <csv_line>.

    lo_csv_xlsx->conv_sap_to_string(
      EXPORTING
        xt_fcat        = lt_fcat
        xo_structdescr = lo_structdescr
        x_sap_data     = <sflight>
      IMPORTING
        y_str_data     = <csv_line>
    ).

  ENDLOOP.


  ls_mail_body_stdtxt-std_txt_name = 'ZAG_SEND_MAIL_BCS_STDTXT'.
  ls_mail_body_stdtxt-substitutions = VALUE #(
    ( varname  = '&SUBSTITUTION&'
      value    = sy-datum         )
  ).

  zag_cl_send_mail=>send_mail(
    EXPORTING
      x_sender                  = sy-uname
      xt_recipients             = lt_recipients
      x_mail_obj                = 'Mail Object'
      x_mail_body_str           = 'Mail Body'
      x_mail_body_standard_text = ls_mail_body_stdtxt
      xt_attachments            = lt_attch
      x_commit                  = abap_true
    IMPORTING
      y_mail_sent               = lv_mail_sent
      y_error_msg               = lv_error_msg
    EXCEPTIONS
      request_error             = 1
      sender_error              = 2
      recipient_error           = 3
      body_error                = 4
      attachment_error          = 5
      OTHERS                    = 6
  ).
```

## 4. ZAG_CL_ALV_IDA <a name="zag_cl_salv_ida"></a>

```abap
**********************************************************************
  "ZAG_CL_ALV_IDA - EXAMPLE
**********************************************************************
  " - DISPLAY

 "Example 1 -> Display a generic ALV
 "-------------------------------------------------
  DATA: lo_ida TYPE REF TO zag_cl_alv_ida.
  
  lv_tabname = 'EKPO'.
  
  CREATE OBJECT lo_ida
    EXPORTING
      xv_ddic_tabname     = CONV #( lv_tabname )
    EXCEPTIONS
      table_not_supported = 1
      OTHERS              = 2.
  IF sy-subrc <> 0.
  ENDIF.
  
  lo_ida->display( ).


 "Example 1 -> Display ALV with settings and handler double click
 "-------------------------------------------------
  DATA: lv_tabname      TYPE string,
        lt_selopt       TYPE zag_cl_alv_ida=>tt_selopt,
        lt_field_list   TYPE zag_cl_alv_ida=>tt_field_list,
        lt_field_label  TYPE zag_cl_alv_ida=>tt_field_label,
        lt_sort_group   TYPE zag_cl_alv_ida=>tt_sort_group,
        ls_double_click TYPE zag_cl_alv_ida=>ts_double_click.


  "DDIC Tabname or CDS Name
  "-------------------------------------------------
  lv_tabname = 'EKPO'.


  "Select Options
  "-------------------------------------------------
  lt_selopt = VALUE #(
     ( fieldname = 'BUKRS'
       selopt_t  = VALUE #(
        ( sign = 'I' option = 'EQ'
          low  = 'ITA1'
          high = 'ITA1' )
       )
     )

     ( fieldname = 'MATNR'
       selopt_t  = VALUE #(
         ( sign = 'I' option = 'NE'
           low  = ''
           high = '' )
        )
     )
  ).


  "List of fields to extract
  "-------------------------------------------------
  lt_field_list = VALUE zag_cl_alv_ida=>tt_field_list(
    ( CONV string('EBELN') )
    ( CONV string('EBELP') )
    ( CONV string('AEDAT') )
    ( CONV string('MATNR') )
    ( CONV string('NETWR') )
  ).


  "Field Labels
  "-------------------------------------------------
  lt_field_label = VALUE #(
    ( fieldname = 'EBELN' label = 'PO Order' )
    ( fieldname = 'EBELP' label = 'PO Item' )
    ( fieldname = 'MATNR' label = 'Mat.' )
    ( fieldname = 'NETWR' label = 'Amount' )
  ).


  "Sort and Grouping
  "-------------------------------------------------
  lt_sort_group = VALUE #(
    ( field_name = 'MATNR'
      descending = abap_false
      is_grouped = abap_true )

    ( field_name = 'AEDAT'
      descending = abap_true
      is_grouped = abap_false )

    ( field_name = 'EBELN'
      descending = abap_false
      is_grouped = abap_false )

    ( field_name = 'EBELP'
      descending = abap_false
      is_grouped = abap_false )
  ).


  "Double Click Handler
  "-------------------------------------------------
  ls_double_click-repid   = sy-repid.
  ls_double_click-perform = 'DOUBLE_CLICK'.


  "ALV
  "-------------------------------------------------
  DATA: lo_ida TYPE REF TO zag_cl_alv_ida.

  CREATE OBJECT lo_ida
    EXPORTING
      xv_ddic_tabname     = CONV #( lv_tabname )
      xt_selopt           = lt_selopt[]
      xt_field_list       = lt_field_list[]
      xt_field_label      = lt_field_label[]
      xt_sort_group       = lt_sort_group[]
      xs_double_click     = ls_double_click
    EXCEPTIONS
      table_not_supported = 1
      OTHERS              = 2.
  IF sy-subrc <> 0.
  ENDIF.

  lo_ida->display( ).

*&---------------------------------------------------------------------*
*& Form DOUBLE_CLICK
*&---------------------------------------------------------------------*
FORM double_click USING xs_selected_row TYPE REF TO data.

  DATA: ls_ekpo        TYPE ekpo.

  ASSIGN xs_selected_row->* TO FIELD-SYMBOL(<ekpo>).
  MOVE-CORRESPONDING <ekpo> TO ls_ekpo.

  DATA(lo_ida_item) = NEW zag_cl_alv_ida(
    xv_ddic_tabname = 'EKPO'
    xt_selopt       = VALUE #(
     ( fieldname = 'EBELN'
       selopt_t  = VALUE #(
        ( sign = 'I' option = 'EQ'
          low  = ls_ekpo-ebeln
          high = ls_ekpo-ebeln )
       )
     ) )
  ).
  IF lo_ida_item IS BOUND.
    lo_ida_item->display( ).
  ENDIF.

ENDFORM.
```

