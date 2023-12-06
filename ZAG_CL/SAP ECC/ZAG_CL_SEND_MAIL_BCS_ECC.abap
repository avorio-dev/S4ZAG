class ZAG_CL_SEND_MAIL_BCS_ECC definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_bcs_attch,
        subject   TYPE string,
        data_csv  TYPE string_table,
        data_xlsx TYPE xstring,
        data_pdf  TYPE xstring,
      END OF ty_bcs_attch .
  types:
    tt_bcs_attch TYPE TABLE OF ty_bcs_attch .
  types:
    BEGIN OF ty_coltxt,
        fieldname TYPE lvc_s_fcat-fieldname,
        coltext   TYPE string,
      END OF ty_coltxt .
  types:
    BEGIN OF ty_stdtxt_subs,
        varname TYPE string,
        value   TYPE string,
      END OF ty_stdtxt_subs .
  types:
    BEGIN OF ty_recipients,
        smtp_addr TYPE adr6-smtp_addr,
        copy      TYPE flag,
      END OF ty_recipients .
  types:
    tt_hrrange    TYPE TABLE OF hrrange .
  types:
    tt_recipients  TYPE TABLE OF ty_recipients .
  types:
    tt_coltxt  TYPE TABLE OF ty_coltxt .
  types:
    tt_stdtxt_subs TYPE STANDARD TABLE OF ty_stdtxt_subs WITH NON-UNIQUE KEY varname .
  types:
    tt_string TYPE TABLE OF string .
  types:
    BEGIN OF ty_standard_text,
        std_txt_name  TYPE thead-tdname,
        substitutions TYPE tt_stdtxt_subs,
      END OF ty_standard_text .

  constants C_LOCAL type CHAR4 value 'LOCL' ##NO_TEXT.
  constants C_SERVER type CHAR4 value 'SERV' ##NO_TEXT.
  constants C_ATTCH_CSV type SOODK-OBJTP value 'CSV' ##NO_TEXT.
  constants C_ATTCH_RAW type SOODK-OBJTP value 'RAW' ##NO_TEXT.
  constants C_ATTCH_PDF type CHAR3 value 'PDF' ##NO_TEXT.
  constants C_ATTCH_BIN type SOODK-OBJTP value 'BIN' ##NO_TEXT.
  constants C_ATTCH_XLSX type CHAR4 value 'XLSX' ##NO_TEXT.
  constants C_MAIL_TYPE_HTM type CHAR3 value 'HTM' ##NO_TEXT.
  constants C_MAIL_TYPE_RAW type CHAR3 value 'RAW' ##NO_TEXT.

  class-methods SEND_MAIL_BCS
    importing
      !X_SENDER type SY-UNAME default SY-UNAME
      !XT_RECIPIENTS type TT_RECIPIENTS
      !X_MAIL_OBJ type SO_OBJ_DES
      !X_MAIL_BODY_STR type STRING optional
      !X_MAIL_BODY_STANDARD_TEXT type TY_STANDARD_TEXT optional
      !XT_ATTACHMENTS type TT_BCS_ATTCH optional
    exporting
      !Y_MAIL_SENT type OS_BOOLEAN
      !Y_ERROR_MSG type STRING
    exceptions
      REQUEST_ERROR
      SENDER_ERROR
      RECIPIENT_ERROR
      BODY_ERROR
      ATTACHMENT_ERROR .
  PROTECTED SECTION.
private section.

  class-methods FILL_FROM_SO10
    importing
      !X_STDTXT_NAME type THEAD-TDNAME
      !XT_STDTXT_SUBS type TT_STDTXT_SUBS optional
    exporting
      !YT_LINES type BCSY_TEXT
      !Y_ERROR_MSG type STRING
    exceptions
      SO10_READING_FAULT .
  class-methods BCS_FILL_ATTACHMENT
    importing
      !XT_ATTACHMENTS type TT_BCS_ATTCH optional
    exporting
      !Y_ERROR_MSG type STRING
    changing
      !YO_SEND_REQUEST type ref to CL_BCS
      !YO_DOCUMENT type ref to CL_DOCUMENT_BCS
    exceptions
      ATTACHMENT_ERROR .
  class-methods BCS_FILL_BODY
    importing
      !X_MAIL_OBJ type SO_OBJ_DES
      !X_MAIL_BODY_STR type STRING optional
      !X_MAIL_BODY_STANDARD_TEXT type TY_STANDARD_TEXT optional
    exporting
      !Y_ERROR_MSG type STRING
    changing
      !YO_SEND_REQUEST type ref to CL_BCS
      !YO_DOCUMENT type ref to CL_DOCUMENT_BCS
    exceptions
      BODY_ERROR .
  class-methods BCS_SET_RECIPIENT
    importing
      !XT_RECIPIENTS type TT_RECIPIENTS
    exporting
      !Y_ERROR_MSG type STRING
    changing
      !YO_SEND_REQUEST type ref to CL_BCS
    exceptions
      RECIPIENT_ERROR .
  class-methods BCS_SET_SENDER
    importing
      !X_SENDER type SY-UNAME
    exporting
      !Y_ERROR_MSG type STRING
    changing
      !YO_SEND_REQUEST type ref to CL_BCS
    exceptions
      SENDER_ERROR .
ENDCLASS.



CLASS ZAG_CL_SEND_MAIL_BCS_ECC IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_SEND_MAIL_BCS_ECC=>BCS_FILL_ATTACHMENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] XT_ATTACHMENTS                 TYPE        TT_BCS_ATTCH(optional)
* | [<---] Y_ERROR_MSG                    TYPE        STRING
* | [<-->] YO_SEND_REQUEST                TYPE REF TO CL_BCS
* | [<-->] YO_DOCUMENT                    TYPE REF TO CL_DOCUMENT_BCS
* | [EXC!] ATTACHMENT_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD bcs_fill_attachment.

    DATA: lt_soli_tab      TYPE soli_tab,
          lt_solix_tab     TYPE solix_tab,
          lv_csv_string    TYPE string,

          lv_attch_subject TYPE sood-objdes,
          lv_attch_size    TYPE sood-objlen.

    DATA: lx_document_bcs  TYPE REF TO cx_document_bcs.

    FIELD-SYMBOLS: <attch> LIKE LINE OF xt_attachments,
                   <csv>   TYPE string.

    "-------------------------------------------------

    LOOP AT xt_attachments ASSIGNING <attch>.

      lv_attch_subject = <attch>-subject.

      "CSV Attachment
      "-------------------------------------------------
      IF <attch>-data_csv[] IS NOT INITIAL.

        "CSV need to be included in one single string,
        "Conversion will be applied, separating each orignal row by CR_LF
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
            y_error_msg = lx_document_bcs->get_text( ).
            RAISE attachment_error.
        ENDTRY.
      ENDIF.


      "EXCEL Attachment
      "-------------------------------------------------
      IF <attch>-data_xlsx IS NOT INITIAL.

        REFRESH lt_solix_tab[].
        lt_solix_tab = cl_bcs_convert=>xstring_to_solix( iv_xstring = <attch>-data_xlsx ).

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
* | Static Private Method ZAG_CL_SEND_MAIL_BCS_ECC=>BCS_FILL_BODY
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_MAIL_OBJ                     TYPE        SO_OBJ_DES
* | [--->] X_MAIL_BODY_STR                TYPE        STRING(optional)
* | [--->] X_MAIL_BODY_STANDARD_TEXT      TYPE        TY_STANDARD_TEXT(optional)
* | [<---] Y_ERROR_MSG                    TYPE        STRING
* | [<-->] YO_SEND_REQUEST                TYPE REF TO CL_BCS
* | [<-->] YO_DOCUMENT                    TYPE REF TO CL_DOCUMENT_BCS
* | [EXC!] BODY_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD bcs_fill_body.

    DATA: lv_subject TYPE so_obj_des,
          lt_lines   TYPE bcsy_text.

    DATA: lx_document_bcs TYPE REF TO cx_document_bcs.

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
    IF x_mail_body_standard_text IS NOT INITIAL.

      fill_from_so10(
        EXPORTING
          x_stdtxt_name      = x_mail_body_standard_text-std_txt_name
          xt_stdtxt_subs     = x_mail_body_standard_text-substitutions
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
      APPEND INITIAL LINE TO lt_lines ASSIGNING <line>.
      <line>-line = x_mail_obj.
    ENDIF.

    TRY.
        yo_document = cl_document_bcs=>create_document( i_type    = c_mail_type_htm
                                                        i_text    = lt_lines[]
                                                        i_subject = x_mail_obj ).
      CATCH cx_document_bcs INTO lx_document_bcs.
        y_error_msg = lx_document_bcs->get_text( ).
        RAISE body_error.
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_SEND_MAIL_BCS_ECC=>BCS_SET_RECIPIENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] XT_RECIPIENTS                  TYPE        TT_RECIPIENTS
* | [<---] Y_ERROR_MSG                    TYPE        STRING
* | [<-->] YO_SEND_REQUEST                TYPE REF TO CL_BCS
* | [EXC!] RECIPIENT_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD bcs_set_recipient.

    DATA: lo_recipient TYPE REF TO cl_cam_address_bcs.

    DATA: lx_send_req_bcs TYPE REF TO cx_send_req_bcs,
          lx_address_bcs  TYPE REF TO cx_address_bcs.

    FIELD-SYMBOLS: <recipient> LIKE LINE OF xt_recipients.

    "-------------------------------------------------

    LOOP AT xt_recipients ASSIGNING <recipient>.

      TRY.
          lo_recipient = cl_cam_address_bcs=>create_internet_address( <recipient>-smtp_addr ).

          yo_send_request->add_recipient( i_recipient = lo_recipient
                                          i_express   = 'X'
                                          i_copy      = <recipient>-copy ).

        CATCH cx_send_req_bcs INTO lx_send_req_bcs.
          y_error_msg = lx_send_req_bcs->get_text( ).
          RAISE recipient_error.
        CATCH cx_address_bcs INTO lx_address_bcs.
          y_error_msg = lx_address_bcs->get_text( ).
          RAISE recipient_error.
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_SEND_MAIL_BCS_ECC=>BCS_SET_SENDER
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_SENDER                       TYPE        SY-UNAME
* | [<---] Y_ERROR_MSG                    TYPE        STRING
* | [<-->] YO_SEND_REQUEST                TYPE REF TO CL_BCS
* | [EXC!] SENDER_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD bcs_set_sender.

    DATA: lo_sender TYPE REF TO cl_sapuser_bcs.

    DATA: lx_address_bcs  TYPE REF TO cx_address_bcs,
          lx_send_req_bcs TYPE REF TO cx_send_req_bcs.

    "-------------------------------------------------

    TRY.
        lo_sender = cl_sapuser_bcs=>create( x_sender ).

        yo_send_request->set_sender( i_sender = lo_sender ).

      CATCH cx_address_bcs INTO lx_address_bcs.
        y_error_msg = lx_address_bcs->get_text( ).
        RAISE sender_error.
      CATCH cx_send_req_bcs INTO lx_send_req_bcs.
        y_error_msg = lx_send_req_bcs->get_text( ).
        RAISE sender_error.
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_SEND_MAIL_BCS_ECC=>FILL_FROM_SO10
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_STDTXT_NAME                  TYPE        THEAD-TDNAME
* | [--->] XT_STDTXT_SUBS                 TYPE        TT_STDTXT_SUBS(optional)
* | [<---] YT_LINES                       TYPE        BCSY_TEXT
* | [<---] Y_ERROR_MSG                    TYPE        STRING
* | [EXC!] SO10_READING_FAULT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD fill_from_so10.

    DATA: lt_lines TYPE TABLE OF tline.

    FIELD-SYMBOLS: <lines> LIKE LINE OF lt_lines,
                   <subs>  LIKE LINE OF xt_stdtxt_subs.

    "-------------------------------------------------

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
          id   = sy-msgid
          lang = '-D'
          no   = sy-msgno
          v1   = sy-msgv1
          v2   = sy-msgv2
          v3   = sy-msgv3
          v4   = sy-msgv4
        IMPORTING
          msg  = y_error_msg.

      RAISE so10_reading_fault.

    ENDIF.

    LOOP AT lt_lines ASSIGNING <lines>.

      LOOP AT xt_stdtxt_subs ASSIGNING <subs>.
        REPLACE ALL OCCURRENCES OF <subs>-varname IN <lines>-tdline WITH <subs>-value.
      ENDLOOP.

      APPEND <lines>-tdline TO yt_lines.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZAG_CL_SEND_MAIL_BCS_ECC->SEND_MAIL_BCS
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD send_mail_bcs.

    DATA: lo_send_request TYPE REF TO cl_bcs,
          lo_document     TYPE REF TO cl_document_bcs.

    DATA: lx_send_req_bcs TYPE REF TO cx_send_req_bcs,
          lx_bcs          TYPE REF TO cx_bcs.

    "-------------------------------------------------

    y_mail_sent = abap_false.
    y_error_msg = ''.

    TRY.

        "Create send request
        "-------------------------------------------------
        TRY.
            lo_send_request = cl_bcs=>create_persistent( ).
          CATCH cx_send_req_bcs INTO lx_send_req_bcs.
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
            x_mail_obj                = x_mail_obj
            x_mail_body_str           = x_mail_body_str
            x_mail_body_standard_text = x_mail_body_standard_text
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
            xt_attachments  = xt_attachments
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
        y_mail_sent = lo_send_request->send( i_with_error_screen = 'X' ).
        IF y_mail_sent EQ abap_true.
          COMMIT WORK.
        ENDIF.


        "Exception handling
      CATCH cx_bcs INTO lx_bcs.
        y_error_msg = lx_bcs->get_text( ).
        RAISE request_error.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
