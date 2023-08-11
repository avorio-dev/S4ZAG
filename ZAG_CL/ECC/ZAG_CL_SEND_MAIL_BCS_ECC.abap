*----------------------------------------------------------------------*
*       CLASS zag_cl_send_mail_bcs_ecc DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zag_cl_send_mail_bcs_ecc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_bcs_attch,
          subject   TYPE string,
          data_csv  TYPE string_table,
          data_xlsx TYPE xstring,
          data_pdf  TYPE xstring,
        END OF ty_bcs_attch .
    TYPES:
      tt_bcs_attch TYPE TABLE OF ty_bcs_attch .
    TYPES:
      BEGIN OF ty_coltxt,
          fieldname TYPE lvc_s_fcat-fieldname,
          coltext   TYPE string,
        END OF ty_coltxt .
    TYPES:
      BEGIN OF ty_stdtxt_subs,
          varname TYPE string,
          value   TYPE string,
        END OF ty_stdtxt_subs .
    TYPES:
      tt_hrrange    TYPE TABLE OF hrrange .
    TYPES:
      tt_smtp_addr  TYPE TABLE OF adr6-smtp_addr .
    TYPES:
      tt_coltxt  TYPE TABLE OF ty_coltxt .
    TYPES:
      tt_stdtxt_subs TYPE STANDARD TABLE OF ty_stdtxt_subs WITH NON-UNIQUE KEY varname .
    TYPES:
      tt_string TYPE TABLE OF string .
    TYPES:
      BEGIN OF ty_stdtxt,
               name          TYPE thead-tdname,
               substitutions TYPE tt_stdtxt_subs,
             END OF ty_stdtxt .

    CONSTANTS c_local TYPE char4 VALUE 'LOCL' ##no_text.
    CONSTANTS c_server TYPE char4 VALUE 'SERV' ##no_text.
    CONSTANTS c_attch_csv TYPE soodk-objtp VALUE 'CSV' ##no_text.
    CONSTANTS c_attch_raw TYPE soodk-objtp VALUE 'RAW' ##no_text.
    CONSTANTS c_attch_pdf TYPE soodk-objtp VALUE 'PDF' ##no_text.
    CONSTANTS c_attch_bin TYPE soodk-objtp VALUE 'BIN' ##no_text.
    CONSTANTS c_attch_xlsx TYPE soodk-objtp VALUE 'XLSX' ##no_text.

    CLASS-METHODS send_mail_bcs
      IMPORTING
        !x_sender TYPE syuname DEFAULT sy-uname
        !xt_recipient TYPE tt_smtp_addr
        !x_mail_obj TYPE so_obj_des
        !x_mail_body_str TYPE string OPTIONAL
        !x_mail_body_so10 TYPE ty_stdtxt OPTIONAL
        !xt_attch TYPE tt_bcs_attch OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS fill_from_so10
      IMPORTING
        !x_stdtxt_name TYPE thead-tdname
        !xt_stdtxt_subs TYPE tt_stdtxt_subs OPTIONAL
      EXPORTING
        !yt_lines TYPE bcsy_text
      EXCEPTIONS
        so10_reading_fault .
    CLASS-METHODS bcs_fill_attachment
      IMPORTING
        !xt_attch TYPE tt_bcs_attch OPTIONAL
      CHANGING
        !yo_send_request TYPE REF TO cl_bcs
        !yo_document TYPE REF TO cl_document_bcs .
    CLASS-METHODS bcs_fill_body
      IMPORTING
        !x_mail_obj TYPE so_obj_des
        !x_mail_body_str TYPE string OPTIONAL
        !x_mail_body_so10 TYPE ty_stdtxt OPTIONAL
      CHANGING
        !yo_send_request TYPE REF TO cl_bcs
        !yo_document TYPE REF TO cl_document_bcs .
    CLASS-METHODS bcs_set_recipient
      IMPORTING
        !xt_recipient TYPE tt_smtp_addr
      CHANGING
        !yo_send_request TYPE REF TO cl_bcs .
    CLASS-METHODS bcs_set_sender
      IMPORTING
        !x_sender TYPE sy-uname
      CHANGING
        !yo_send_request TYPE REF TO cl_bcs .
ENDCLASS.



CLASS ZAG_CL_SEND_MAIL_BCS_ECC IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_SEND_MAIL_BCS_ECC=>BCS_FILL_ATTACHMENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] XT_ATTCH                       TYPE        TT_BCS_ATTCH(optional)
* | [<-->] YO_SEND_REQUEST                TYPE REF TO CL_BCS
* | [<-->] YO_DOCUMENT                    TYPE REF TO CL_DOCUMENT_BCS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD bcs_fill_attachment.

    DATA: lt_soli_tab      TYPE soli_tab,
          lt_solix_tab     TYPE solix_tab,
          lv_attch_subject TYPE sood-objdes,
          lv_attch_size    TYPE sood-objlen,
          lv_csv_string    TYPE string.

    DATA: lv_except_msg    TYPE string,
          lx_document_bcs  TYPE REF TO cx_document_bcs.

    FIELD-SYMBOLS: <attch> LIKE LINE OF xt_attch,
                   <csv>   TYPE string.

    "-------------------------------------------------

    LOOP AT xt_attch ASSIGNING <attch>.

      lv_attch_subject = <attch>-subject.

      "CSV Attachment
      "-------------------------------------------------
      IF <attch>-data_csv[] IS NOT INITIAL.

        lv_csv_string = ''.
        LOOP AT <attch>-data_csv ASSIGNING <csv>.
          IF lv_csv_string IS INITIAL.
            lv_csv_string = <csv>.
          ELSE.
            lv_csv_string = |{ lv_csv_string }{ cl_abap_char_utilities=>cr_lf }{ <csv> }|.
          ENDIF.
        ENDLOOP.

        REFRESH lt_soli_tab[].
        lt_soli_tab[] = cl_bcs_convert=>string_to_soli( iv_string = lv_csv_string ).

        lv_attch_subject = |{ lv_attch_subject }.{ c_attch_csv }|.

        TRY.
            CALL METHOD yo_document->add_attachment
              EXPORTING
                i_attachment_type    = c_attch_csv   "For CSV file Extension
                i_attachment_subject = lv_attch_subject
                i_att_content_text   = lt_soli_tab[].

          CATCH cx_document_bcs INTO lx_document_bcs.
            lv_except_msg = lx_document_bcs->get_text( ).
        ENDTRY.
      ENDIF.


      "EXCEL Attachment
      "-------------------------------------------------
      IF <attch>-data_xlsx IS NOT INITIAL.

        REFRESH lt_solix_tab[].
        lt_solix_tab = cl_bcs_convert=>xstring_to_solix( iv_xstring = <attch>-data_pdf ).

        lv_attch_subject = |{ lv_attch_subject }.{ c_attch_xlsx }|.

        TRY.
            lv_attch_size = xstrlen( <attch>-data_xlsx ).
            CALL METHOD yo_document->add_attachment
              EXPORTING
                i_attachment_type    = c_attch_bin   "For CSV file Extension
                i_attachment_subject = lv_attch_subject
                i_attachment_size    = lv_attch_size
                i_att_content_hex    = lt_solix_tab[].

          CATCH cx_document_bcs INTO lx_document_bcs.
            lv_except_msg = lx_document_bcs->get_text( ).
        ENDTRY.

      ENDIF.


      "PDF Attachment
      "-------------------------------------------------
      IF <attch>-data_pdf IS NOT INITIAL.

        REFRESH lt_solix_tab[].
        lt_solix_tab = cl_bcs_convert=>xstring_to_solix( iv_xstring = <attch>-data_pdf ).

        lv_attch_subject = |{ lv_attch_subject }.{ c_attch_pdf }|.

        TRY.
            CALL METHOD yo_document->add_attachment
              EXPORTING
                i_attachment_type    = c_attch_raw   "For CSV file Extension
                i_attachment_subject = lv_attch_subject
                i_att_content_hex    = lt_solix_tab[].

          CATCH cx_document_bcs INTO lx_document_bcs.
            lv_except_msg = lx_document_bcs->get_text( ).
        ENDTRY.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.                    "bcs_fill_attachment


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_SEND_MAIL_BCS_ECC=>BCS_FILL_BODY
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_MAIL_OBJ                     TYPE        SO_OBJ_DES
* | [--->] X_MAIL_BODY_STR                TYPE        STRING(optional)
* | [--->] X_MAIL_BODY_SO10               TYPE        TY_STDTXT(optional)
* | [<-->] YO_SEND_REQUEST                TYPE REF TO CL_BCS
* | [<-->] YO_DOCUMENT                    TYPE REF TO CL_DOCUMENT_BCS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD bcs_fill_body.

    CONSTANTS: c_mail_type_raw TYPE char03 VALUE 'RAW',
               c_mail_type_htm TYPE char03 VALUE 'HTM'.

    DATA: lv_subject TYPE so_obj_des,
          lt_lines   TYPE bcsy_text.

    DATA: lv_except_msg   TYPE string,
          lx_document_bcs TYPE REF TO cx_document_bcs.

    FIELD-SYMBOLS: <line> LIKE LINE OF lt_lines.

    "-------------------------------------------------

    REFRESH lt_lines[].


    "Set body with input string if provided
    "-------------------------------------------------
    IF x_mail_body_str IS NOT INITIAL.

      lt_lines[] = cl_bcs_convert=>string_to_soli( iv_string = x_mail_body_str ).

    ENDIF.


    "Set body with standard SO10 text with substitution if provided
    "-------------------------------------------------
    IF x_mail_body_so10 IS NOT INITIAL.

      fill_from_so10(
        EXPORTING
          x_stdtxt_name      = x_mail_body_so10-name
          xt_stdtxt_subs     = x_mail_body_so10-substitutions
        IMPORTING
          yt_lines           = lt_lines[]
        EXCEPTIONS
          so10_reading_fault = 1
          OTHERS             = 2
      ).

    ENDIF.


    "Set as body the mail object if nothing is provided
    "-------------------------------------------------
    IF lt_lines[] IS INITIAL.
      APPEND INITIAL LINE TO lt_lines ASSIGNING <line>.
      <line>-line = x_mail_obj.
    ENDIF.



    TRY.
        yo_document = cl_document_bcs=>create_document( i_type    = c_mail_type_htm
                                                        i_text    = lt_lines[]
                                                        i_subject = x_mail_obj ).
      CATCH cx_document_bcs INTO lx_document_bcs.
        lv_except_msg = lx_document_bcs->get_text( ).
    ENDTRY.

  ENDMETHOD.                    "bcs_fill_body


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_SEND_MAIL_BCS_ECC=>BCS_SET_RECIPIENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] XT_RECIPIENT                   TYPE        TT_SMTP_ADDR
* | [<-->] YO_SEND_REQUEST                TYPE REF TO CL_BCS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD bcs_set_recipient.

    DATA: lo_recipient TYPE REF TO cl_cam_address_bcs.

    DATA: lv_except_msg   TYPE string,
          lx_send_req_bcs TYPE REF TO cx_send_req_bcs.

    FIELD-SYMBOLS: <recipient> LIKE LINE OF xt_recipient.

    "-------------------------------------------------

    LOOP AT xt_recipient ASSIGNING <recipient>.

      lo_recipient = cl_cam_address_bcs=>create_internet_address( <recipient> ).

      TRY.
          yo_send_request->add_recipient( i_recipient = lo_recipient
                                          i_express   = 'X' ).

        CATCH cx_send_req_bcs INTO lx_send_req_bcs.
          lv_except_msg = lx_send_req_bcs->get_text( ).
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.                    "bcs_set_recipient


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_SEND_MAIL_BCS_ECC=>BCS_SET_SENDER
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_SENDER                       TYPE        SY-UNAME
* | [<-->] YO_SEND_REQUEST                TYPE REF TO CL_BCS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD bcs_set_sender.

    DATA: lo_sender TYPE REF TO cl_sapuser_bcs.

    DATA: lv_except_msg  TYPE string,
          lx_address_bcs TYPE REF TO cx_address_bcs.

    "-------------------------------------------------

    lo_sender = cl_sapuser_bcs=>create( x_sender ).

    TRY.
        yo_send_request->set_sender( i_sender = lo_sender ).

      CATCH cx_address_bcs INTO lx_address_bcs.
        lv_except_msg = lx_address_bcs->get_text( ).
    ENDTRY.

  ENDMETHOD.                    "bcs_set_sender


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_SEND_MAIL_BCS_ECC=>FILL_FROM_SO10
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_STDTXT_NAME                  TYPE        THEAD-TDNAME
* | [--->] XT_STDTXT_SUBS                 TYPE        TT_STDTXT_SUBS(optional)
* | [<---] YT_LINES                       TYPE        BCSY_TEXT
* | [EXC!] SO10_READING_FAULT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD fill_from_so10.

    DATA: lt_lines         TYPE TABLE OF tline.

    FIELD-SYMBOLS: <lines> LIKE LINE OF lt_lines,
                   <subs>  LIKE LINE OF xt_stdtxt_subs.

    "-------------------------------------------------

    CLEAR yt_lines[].

    CLEAR: lt_lines[].
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'ST'
        language                = sy-langu
        name                    = x_stdtxt_name
        object                  = 'TEXT'
      TABLES
        lines                   = lt_lines[]
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF sy-subrc <> 0.

      DATA: lv_except_msg TYPE string.
      CALL FUNCTION 'FORMAT_MESSAGE'
        EXPORTING
          id        = sy-msgid
          lang      = '-D'
          no        = sy-msgno
          v1        = sy-msgv1
          v2        = sy-msgv2
          v3        = sy-msgv3
          v4        = sy-msgv4
        IMPORTING
          msg       = lv_except_msg
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.

      RAISE so10_reading_fault.

    ENDIF.

    LOOP AT lt_lines ASSIGNING <lines>.

      LOOP AT xt_stdtxt_subs ASSIGNING <subs>.
        REPLACE ALL OCCURRENCES OF <subs>-varname IN <lines>-tdline WITH <subs>-value.
      ENDLOOP.

      APPEND <lines>-tdline TO yt_lines.
    ENDLOOP.

  ENDMETHOD.                    "fill_from_so10


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_SEND_MAIL_BCS_ECC=>SEND_MAIL_BCS
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_SENDER                       TYPE        SYUNAME (default =SY-UNAME)
* | [--->] XT_RECIPIENT                   TYPE        TT_SMTP_ADDR
* | [--->] X_MAIL_OBJ                     TYPE        SO_OBJ_DES
* | [--->] X_MAIL_BODY_STR                TYPE        STRING(optional)
* | [--->] X_MAIL_BODY_SO10               TYPE        TY_STDTXT(optional)
* | [--->] XT_ATTCH                       TYPE        TT_BCS_ATTCH(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD send_mail_bcs.

    DATA: lo_send_request TYPE REF TO cl_bcs,
          lo_document     TYPE REF TO cl_document_bcs,
          lv_sent_to_all  TYPE os_boolean.

    DATA: lv_except_msg   TYPE string,
          lx_send_req_bcs TYPE REF TO cx_send_req_bcs,
          lx_bcs          TYPE REF TO cx_bcs.

    "-------------------------------------------------

    TRY.

        "Create send request
        "-------------------------------------------------
        TRY.
            lo_send_request = cl_bcs=>create_persistent( ).
          CATCH cx_send_req_bcs INTO lx_send_req_bcs.
            lv_except_msg = lx_send_req_bcs->get_text( ).
        ENDTRY.


        "Set Sender of email
        "-------------------------------------------------
        bcs_set_sender(
          EXPORTING
            x_sender        = x_sender
          CHANGING
            yo_send_request = lo_send_request
        ).


        "Set list of recipients
        "-------------------------------------------------
        bcs_set_recipient(
          EXPORTING
            xt_recipient    = xt_recipient[]
          CHANGING
            yo_send_request = lo_send_request
        ).


        "Set body with an input string or an SO10 standard text
        "-------------------------------------------------
        bcs_fill_body(
          EXPORTING
            x_mail_obj       = x_mail_obj
            x_mail_body_str  = x_mail_body_str
            x_mail_body_so10 = x_mail_body_so10
          CHANGING
            yo_send_request  = lo_send_request
            yo_document      = lo_document
        ).


        "Set attachments CSV or PDF
        "-------------------------------------------------
        bcs_fill_attachment(
          EXPORTING
            xt_attch        = xt_attch[]
          CHANGING
            yo_send_request = lo_send_request
            yo_document     = lo_document
        ).


        "Add document to send request
        "-------------------------------------------------
        TRY.
            lo_send_request->set_document( lo_document ).
          CATCH cx_send_req_bcs INTO lx_send_req_bcs.
            lv_except_msg = lx_send_req_bcs->get_text( ).
        ENDTRY.


        "Send email
        "-------------------------------------------------
        lv_sent_to_all = lo_send_request->send( i_with_error_screen = 'X' ).
        IF lv_sent_to_all = 'X'.
          MESSAGE s646(db) WITH 'Email Sent'.
        ENDIF.
        COMMIT WORK.


        "Exception handling
      CATCH cx_bcs INTO lx_bcs.
        lv_except_msg = lx_bcs->get_text( ).
    ENDTRY.

  ENDMETHOD.                    "send_mail_bcs
ENDCLASS.
