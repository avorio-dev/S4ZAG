class ZAG_CL_SEND_MAIL definition
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
      !X_SENDER type SYST_UNAME default SY-UNAME
      !XT_RECIPIENTS type TT_RECIPIENTS
      !X_MAIL_OBJ type SO_OBJ_DES
      !X_MAIL_BODY_STR type STRING optional
      !X_MAIL_BODY_STANDARD_TEXT type TY_STANDARD_TEXT optional
      !XT_ATTACHMENTS type TT_BCS_ATTCH optional
      !X_COMMIT type OS_BOOLEAN default ABAP_TRUE
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



CLASS ZAG_CL_SEND_MAIL IMPLEMENTATION.


  METHOD BCS_FILL_ATTACHMENT.

    DATA: lt_soli_tab      TYPE soli_tab,
          lt_solix_tab     TYPE solix_tab,
          lv_csv_string    TYPE string.

    DATA: lx_document_bcs  TYPE REF TO cx_document_bcs.

    "-------------------------------------------------

    LOOP AT xt_attachments ASSIGNING FIELD-SYMBOL(<attch>).

      DATA(lv_attch_subject) = CONV sood-objdes( <attch>-subject ).

      "CSV Attachment
      "-------------------------------------------------
      IF <attch>-data_csv[] IS NOT INITIAL.

        "CSV need to be included in one single string,
        "Conversion will be applied, separating each orignal row by CR_LF
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
        lt_solix_tab = cl_bcs_convert=>xstring_to_solix( iv_xstring = <attch>-data_xlsx ).

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


  METHOD BCS_FILL_BODY.

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


  METHOD BCS_SET_RECIPIENT.

    LOOP AT xt_recipients ASSIGNING FIELD-SYMBOL(<recipient>).

      TRY.
          DATA(lo_recipient) = cl_cam_address_bcs=>create_internet_address( <recipient>-smtp_addr ).

          yo_send_request->add_recipient( i_recipient = lo_recipient
                                          i_express   = 'X'
                                          i_copy      = <recipient>-copy ).

        CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs).
          y_error_msg = lx_send_req_bcs->get_text( ).
          RAISE recipient_error.
        CATCH cx_address_bcs INTO DATA(lx_address_bcs).
          y_error_msg = lx_address_bcs->get_text( ).
          RAISE recipient_error.
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD BCS_SET_SENDER.

    TRY.
        DATA(lo_sender) = cl_sapuser_bcs=>create( x_sender ).

        yo_send_request->set_sender( i_sender = lo_sender ).

      CATCH cx_address_bcs INTO DATA(lx_address_bcs).
        y_error_msg = lx_address_bcs->get_text( ).
        RAISE sender_error.
      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs).
        y_error_msg = lx_send_req_bcs->get_text( ).
        RAISE sender_error.
    ENDTRY.

  ENDMETHOD.


  METHOD FILL_FROM_SO10.

    DATA: lt_lines TYPE TABLE OF tline.

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
          id        = sy-msgid
          lang      = '-D'
          no        = sy-msgno
          v1        = sy-msgv1
          v2        = sy-msgv2
          v3        = sy-msgv3
          v4        = sy-msgv4
        IMPORTING
          msg       = y_error_msg.

      RAISE so10_reading_fault.

    ENDIF.

    LOOP AT lt_lines ASSIGNING FIELD-SYMBOL(<lines>).

      LOOP AT xt_stdtxt_subs ASSIGNING FIELD-SYMBOL(<subs>).
        REPLACE ALL OCCURRENCES OF <subs>-varname IN <lines>-tdline WITH <subs>-value.
      ENDLOOP.

      APPEND <lines>-tdline TO yt_lines.
    ENDLOOP.

  ENDMETHOD.


  METHOD SEND_MAIL_BCS.

    DATA: lo_send_request TYPE REF TO cl_bcs,
          lo_document     TYPE REF TO cl_document_bcs.

    "-------------------------------------------------

    y_mail_sent = abap_false.
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
        lo_send_request->set_send_immediately( i_send_immediately = 'X' ).
        y_mail_sent = lo_send_request->send( i_with_error_screen = 'X' ).
        IF y_mail_sent EQ abap_true.
          IF x_commit EQ abap_true.
            COMMIT WORK.
          ENDIF.
        ENDIF.


        "Exception handling
      CATCH cx_bcs INTO DATA(lx_bcs).
        y_error_msg = lx_bcs->get_text( ).
        RAISE request_error.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
