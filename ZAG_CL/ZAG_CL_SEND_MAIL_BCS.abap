CLASS zag_cl_send_mail_bcs DEFINITION
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
      BEGIN OF ty_recipients,
        smtp_addr TYPE adr6-smtp_addr,
        copy      TYPE flag,
      END OF ty_recipients .
    TYPES:
      tt_hrrange    TYPE TABLE OF hrrange .
    TYPES:
      tt_recipients  TYPE TABLE OF ty_recipients .
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

    CONSTANTS c_local TYPE char4 VALUE 'LOCL' ##NO_TEXT.
    CONSTANTS c_server TYPE char4 VALUE 'SERV' ##NO_TEXT.
    CONSTANTS c_attch_csv TYPE soodk-objtp VALUE 'CSV' ##NO_TEXT.
    CONSTANTS c_attch_raw TYPE soodk-objtp VALUE 'RAW' ##NO_TEXT.
    CONSTANTS c_attch_pdf TYPE char3 VALUE 'PDF' ##NO_TEXT.
    CONSTANTS c_attch_bin TYPE soodk-objtp VALUE 'BIN' ##NO_TEXT.
    CONSTANTS c_attch_xlsx TYPE char4 VALUE 'XLSX' ##NO_TEXT.

    CLASS-METHODS send_mail_bcs
      IMPORTING
        !x_sender         TYPE syst_uname DEFAULT sy-uname
        !xt_recipients    TYPE tt_recipients
        !x_mail_obj       TYPE so_obj_des
        !x_mail_body_str  TYPE string OPTIONAL
        !x_mail_body_so10 TYPE ty_stdtxt OPTIONAL
        !xt_attch         TYPE tt_bcs_attch OPTIONAL
      EXPORTING
        !y_error_msg      TYPE string
      EXCEPTIONS
        request_error
        sender_error
        recipient_error
        body_error
        attachment_error .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS fill_from_so10
      IMPORTING
        !x_stdtxt_name  TYPE thead-tdname
        !xt_stdtxt_subs TYPE tt_stdtxt_subs OPTIONAL
      EXPORTING
        !yt_lines       TYPE bcsy_text
        !y_error_msg    TYPE string
      EXCEPTIONS
        so10_reading_fault .
    CLASS-METHODS bcs_fill_attachment
      IMPORTING
        !xt_attch        TYPE tt_bcs_attch OPTIONAL
      EXPORTING
        !y_error_msg     TYPE string
      CHANGING
        !yo_send_request TYPE REF TO cl_bcs
        !yo_document     TYPE REF TO cl_document_bcs
      EXCEPTIONS
        attachment_error .
    CLASS-METHODS bcs_fill_body
      IMPORTING
        !x_mail_obj       TYPE so_obj_des
        !x_mail_body_str  TYPE string OPTIONAL
        !x_mail_body_so10 TYPE ty_stdtxt OPTIONAL
      EXPORTING
        !y_error_msg      TYPE string
      CHANGING
        !yo_send_request  TYPE REF TO cl_bcs
        !yo_document      TYPE REF TO cl_document_bcs
      EXCEPTIONS
        body_error .
    CLASS-METHODS bcs_set_recipient
      IMPORTING
        !xt_recipients   TYPE tt_recipients
      EXPORTING
        !y_error_msg     TYPE string
      CHANGING
        !yo_send_request TYPE REF TO cl_bcs
      EXCEPTIONS
        recipient_error .
    CLASS-METHODS bcs_set_sender
      IMPORTING
        !x_sender        TYPE sy-uname
      EXPORTING
        !y_error_msg     TYPE string
      CHANGING
        !yo_send_request TYPE REF TO cl_bcs
      EXCEPTIONS
        sender_error .
ENDCLASS.



CLASS ZAG_CL_SEND_MAIL_BCS IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_SEND_MAIL_BCS=>BCS_FILL_ATTACHMENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] XT_ATTCH                       TYPE        TT_BCS_ATTCH(optional)
* | [<---] Y_ERROR_MSG                    TYPE        STRING
* | [<-->] YO_SEND_REQUEST                TYPE REF TO CL_BCS
* | [<-->] YO_DOCUMENT                    TYPE REF TO CL_DOCUMENT_BCS
* | [EXC!] ATTACHMENT_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD bcs_fill_attachment.

    DATA: lt_soli_tab      TYPE soli_tab,
          lt_solix_tab     TYPE solix_tab,
          lv_attch_subject TYPE sood-objdes,
          lv_csv_string    TYPE string.

    DATA: lx_document_bcs  TYPE REF TO cx_document_bcs.

    "-------------------------------------------------

    LOOP AT xt_attch ASSIGNING FIELD-SYMBOL(<attch>).

      lv_attch_subject = <attch>-subject.

      "CSV Attachment
      "-------------------------------------------------
      IF <attch>-data_csv[] IS NOT INITIAL.

        lv_csv_string = ''.
        LOOP AT <attch>-data_csv ASSIGNING FIELD-SYMBOL(<csv>).
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
            y_error_msg = lx_document_bcs->get_text( ).
            RAISE attachment_error.
        ENDTRY.
      ENDIF.


      "EXCEL Attachment
      "-------------------------------------------------
      IF <attch>-data_xlsx IS NOT INITIAL.

        REFRESH lt_solix_tab[].
        lt_solix_tab = cl_bcs_convert=>xstring_to_solix( iv_xstring = <attch>-data_pdf ).

        lv_attch_subject = |{ lv_attch_subject }.{ c_attch_xlsx }|.

        TRY.
            CALL METHOD yo_document->add_attachment
              EXPORTING
                i_attachment_type    = c_attch_bin   "For CSV file Extension
                i_attachment_subject = lv_attch_subject
                i_attachment_size    = CONV sood-objlen( xstrlen( <attch>-data_xlsx ) )
                i_att_content_hex    = lt_solix_tab[].

          CATCH cx_document_bcs INTO lx_document_bcs.
            y_error_msg = lx_document_bcs->get_text( ).
            RAISE attachment_error.
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
            y_error_msg = lx_document_bcs->get_text( ).
            RAISE attachment_error.
        ENDTRY.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_SEND_MAIL_BCS=>BCS_FILL_BODY
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_MAIL_OBJ                     TYPE        SO_OBJ_DES
* | [--->] X_MAIL_BODY_STR                TYPE        STRING(optional)
* | [--->] X_MAIL_BODY_SO10               TYPE        TY_STDTXT(optional)
* | [<---] Y_ERROR_MSG                    TYPE        STRING
* | [<-->] YO_SEND_REQUEST                TYPE REF TO CL_BCS
* | [<-->] YO_DOCUMENT                    TYPE REF TO CL_DOCUMENT_BCS
* | [EXC!] BODY_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD bcs_fill_body.

    CONSTANTS: c_mail_type_raw TYPE char03 VALUE 'RAW',
               c_mail_type_htm TYPE char03 VALUE 'HTM'.

    DATA: lv_subject TYPE so_obj_des,
          lt_lines   TYPE bcsy_text.

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
          y_error_msg        = y_error_msg
        EXCEPTIONS
          so10_reading_fault = 1
          OTHERS             = 2
      ).
      IF sy-subrc <> 0.
        RAISE body_error.
      ENDIF.

    ENDIF.


    "Set as body the mail object if nothing is provided
    "-------------------------------------------------
    IF lt_lines[] IS INITIAL.
      APPEND INITIAL LINE TO lt_lines ASSIGNING FIELD-SYMBOL(<line>).
      <line>-line = x_mail_obj.
    ENDIF.

    TRY.
        yo_document = cl_document_bcs=>create_document( i_type    = c_mail_type_htm
                                                        i_text    = lt_lines[]
                                                        i_subject = x_mail_obj ).
      CATCH cx_document_bcs INTO DATA(lx_document_bcs).
        y_error_msg = lx_document_bcs->get_text( ).
        RAISE body_error.
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_SEND_MAIL_BCS=>BCS_SET_RECIPIENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] XT_RECIPIENTS                  TYPE        TT_RECIPIENTS
* | [<---] Y_ERROR_MSG                    TYPE        STRING
* | [<-->] YO_SEND_REQUEST                TYPE REF TO CL_BCS
* | [EXC!] RECIPIENT_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD bcs_set_recipient.

    LOOP AT xt_recipients ASSIGNING FIELD-SYMBOL(<recipient>).

      DATA(lo_recipient) = cl_cam_address_bcs=>create_internet_address( <recipient>-smtp_addr ).

      TRY.
          yo_send_request->add_recipient( i_recipient = lo_recipient
                                          i_express   = 'X'
                                          i_copy      = <recipient>-copy ).

        CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs).
          y_error_msg = lx_send_req_bcs->get_text( ).
          RAISE recipient_error.
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_SEND_MAIL_BCS=>BCS_SET_SENDER
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_SENDER                       TYPE        SY-UNAME
* | [<---] Y_ERROR_MSG                    TYPE        STRING
* | [<-->] YO_SEND_REQUEST                TYPE REF TO CL_BCS
* | [EXC!] SENDER_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD bcs_set_sender.

    DATA(lo_sender) = cl_sapuser_bcs=>create( x_sender ).

    TRY.
        yo_send_request->set_sender( i_sender = lo_sender ).

      CATCH cx_address_bcs INTO DATA(lx_address_bcs).
        y_error_msg = lx_address_bcs->get_text( ).
        RAISE sender_error.
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_SEND_MAIL_BCS=>FILL_FROM_SO10
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_STDTXT_NAME                  TYPE        THEAD-TDNAME
* | [--->] XT_STDTXT_SUBS                 TYPE        TT_STDTXT_SUBS(optional)
* | [<---] YT_LINES                       TYPE        BCSY_TEXT
* | [<---] Y_ERROR_MSG                    TYPE        STRING
* | [EXC!] SO10_READING_FAULT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD fill_from_so10.

    DATA: lt_lines         TYPE TABLE OF tline.

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
          msg       = y_error_msg
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.

      RAISE so10_reading_fault.

    ENDIF.

    LOOP AT lt_lines ASSIGNING FIELD-SYMBOL(<lines>).

      LOOP AT xt_stdtxt_subs ASSIGNING FIELD-SYMBOL(<subs>).
        REPLACE ALL OCCURRENCES OF <subs>-varname IN <lines>-tdline WITH <subs>-value.
      ENDLOOP.

      APPEND <lines>-tdline TO yt_lines.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_SEND_MAIL_BCS=>SEND_MAIL_BCS
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_SENDER                       TYPE        SYST_UNAME (default =SY-UNAME)
* | [--->] XT_RECIPIENTS                  TYPE        TT_RECIPIENTS
* | [--->] X_MAIL_OBJ                     TYPE        SO_OBJ_DES
* | [--->] X_MAIL_BODY_STR                TYPE        STRING(optional)
* | [--->] X_MAIL_BODY_SO10               TYPE        TY_STDTXT(optional)
* | [--->] XT_ATTCH                       TYPE        TT_BCS_ATTCH(optional)
* | [<---] Y_ERROR_MSG                    TYPE        STRING
* | [EXC!] REQUEST_ERROR
* | [EXC!] SENDER_ERROR
* | [EXC!] RECIPIENT_ERROR
* | [EXC!] BODY_ERROR
* | [EXC!] ATTACHMENT_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD send_mail_bcs.

    DATA: lo_send_request TYPE REF TO cl_bcs,
          lo_document     TYPE REF TO cl_document_bcs,
          lv_sent_to_all  TYPE os_boolean.

    "-------------------------------------------------

    y_error_msg = ''.

    TRY.

        "Create send request
        "-------------------------------------------------
        TRY.
            lo_send_request = cl_bcs=>create_persistent( ).
          CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs).
            y_error_msg = lx_send_req_bcs->get_text( ).
            RAISE request_error.
        ENDTRY.


        "Set Sender of email
        "-------------------------------------------------
        bcs_set_sender(
          EXPORTING
            x_sender        = x_sender
          IMPORTING
            y_error_msg     = y_error_msg
          CHANGING
            yo_send_request = lo_send_request
          EXCEPTIONS
            sender_error    = 1
            OTHERS          = 2
        ).
        IF sy-subrc <> 0.
          RAISE sender_error.
        ENDIF.


        "Set list of recipients
        "-------------------------------------------------
        bcs_set_recipient(
          EXPORTING
            xt_recipients   = xt_recipients[]
          IMPORTING
            y_error_msg     = y_error_msg
          CHANGING
            yo_send_request = lo_send_request
          EXCEPTIONS
            recipient_error = 1
            OTHERS          = 2
        ).
        IF sy-subrc <> 0.
          RAISE recipient_error.
        ENDIF.


        "Set body with an input string or an SO10 standard text
        "-------------------------------------------------
        bcs_fill_body(
          EXPORTING
            x_mail_obj       = x_mail_obj
            x_mail_body_str  = x_mail_body_str
            x_mail_body_so10 = x_mail_body_so10
          IMPORTING
            y_error_msg      = y_error_msg
          CHANGING
            yo_send_request  = lo_send_request
            yo_document      = lo_document
          EXCEPTIONS
            body_error         = 1
            OTHERS             = 3
        ).
        IF sy-subrc <> 0.
          RAISE body_error.
        ENDIF.


        "Set attachments CSV or PDF
        "-------------------------------------------------
        bcs_fill_attachment(
          EXPORTING
            xt_attch        = xt_attch
          IMPORTING
            y_error_msg     = y_error_msg
          CHANGING
            yo_send_request = lo_send_request
            yo_document     = lo_document
          EXCEPTIONS
            attachment_error  = 1
            OTHERS            = 2
        ).
        IF sy-subrc <> 0.
          RAISE attachment_error.
        ENDIF.


        "Add document to send request
        "-------------------------------------------------
        TRY.
            lo_send_request->set_document( lo_document ).
          CATCH cx_send_req_bcs INTO lx_send_req_bcs.
            y_error_msg = lx_send_req_bcs->get_text( ).
            RAISE request_error.
        ENDTRY.


        "Send email
        "-------------------------------------------------
        lv_sent_to_all = lo_send_request->send( i_with_error_screen = 'X' ).
        IF lv_sent_to_all = 'X'.
          MESSAGE s646(db) WITH 'Email Sent'.
        ENDIF.
        COMMIT WORK.


        "Exception handling
      CATCH cx_bcs INTO DATA(lx_bcs).
        y_error_msg = lx_bcs->get_text( ).
        RAISE request_error.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
