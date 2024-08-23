CLASS zag_cl_send_mail DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    " Types
    "-------------------------------------------------
    TYPES:
      BEGIN OF ts_recipients,
        smtp_addr TYPE adr6-smtp_addr,
        copy      TYPE flag,
      END OF ts_recipients,

      BEGIN OF ts_attachments,
        subject   TYPE string,
        data_csv  TYPE string_table,
        data_xlsx TYPE xstring,
        data_pdf  TYPE xstring,
      END OF ts_attachments,

      BEGIN OF ts_stdtxt_replace,
        varname TYPE string,
        value   TYPE string,
      END OF ts_stdtxt_replace,

      BEGIN OF ts_standard_text,
        std_txt_name TYPE thead-tdname,
        replacement  TYPE TABLE OF ts_stdtxt_replace WITH NON-UNIQUE KEY varname,
      END OF ts_standard_text .

    TYPES:
      tt_recipients  TYPE TABLE OF ts_recipients     WITH NON-UNIQUE KEY smtp_addr copy,
      tt_attachments TYPE TABLE OF ts_attachments    WITH DEFAULT KEY,
      tt_stdtxt_subs TYPE TABLE OF ts_stdtxt_replace WITH NON-UNIQUE KEY varname.

    TYPES:
      BEGIN OF ts_mail_params,
        sender      TYPE syst_uname,
        recipients  TYPE tt_recipients,
        object      TYPE so_obj_des,
        body        TYPE string,
        body_stdtxt TYPE ts_standard_text,
        attachments TYPE tt_attachments,
      END OF ts_mail_params.


    " Constants
    "-------------------------------------------------
    CONSTANTS:
      BEGIN OF tc_attch_type,
        csv  TYPE soodk-objtp VALUE 'CSV' ##NO_TEXT,
        raw  TYPE soodk-objtp VALUE 'RAW' ##NO_TEXT,
        pdf  TYPE char3       VALUE 'PDF' ##NO_TEXT,
        bin  TYPE soodk-objtp VALUE 'BIN' ##NO_TEXT,
        xlsx TYPE char4       VALUE 'XLSX' ##NO_TEXT,
      END OF tc_attch_type,

      BEGIN OF tc_mail_type,
        htm TYPE char3 VALUE 'HTM' ##NO_TEXT,
        raw TYPE char3 VALUE 'RAW' ##NO_TEXT,
      END OF tc_mail_type.


    " Methods
    "-------------------------------------------------
    METHODS:
      send_mail
        IMPORTING
          !xs_mail_params    TYPE ts_mail_params
          !xv_commit         TYPE os_boolean DEFAULT abap_true
        RETURNING
          VALUE(y_mail_sent) TYPE os_boolean
        RAISING
          cx_ai_system_fault.


  PROTECTED SECTION.

    "Constants
    "-------------------------------------------------
    CONSTANTS:
      BEGIN OF tc_exception_msg,
        missing_recipients TYPE string VALUE 'Recipient list must be filled'                   ##NO_TEXT,
        missing_mail_obj   TYPE string VALUE 'Mail object must be empty filled'                ##NO_TEXT,
      END OF tc_exception_msg.


    "Data
    "---------------------------------------------------------------
    DATA:
      gs_mail_params  TYPE ts_mail_params,
      go_send_request TYPE REF TO cl_bcs,
      go_document     TYPE REF TO cl_document_bcs.


    "Methods
    "---------------------------------------------------------------
    METHODS:
      check_mandatory_fields
        RAISING
          cx_ai_system_fault,

      fill_sender
        RAISING
          cx_ai_system_fault,

      fill_recipient
        RAISING
          cx_ai_system_fault,

      fill_body
        RAISING
          cx_ai_system_fault,

      fill_attachment
        RAISING
          cx_ai_system_fault,

      get_standard_text_lines
        RETURNING
          VALUE(yt_lines) TYPE bcsy_text
        RAISING
          cx_ai_system_fault.

ENDCLASS.



CLASS zag_cl_send_mail IMPLEMENTATION.


  METHOD send_mail.

    y_mail_sent = abap_false.

    CLEAR me->gs_mail_params.
    me->gs_mail_params = xs_mail_params.


    TRY.
        "Check if input is correct
        "-------------------------------------------------
        me->check_mandatory_fields( ).


        "Create send request
        "-------------------------------------------------
        me->go_send_request = cl_bcs=>create_persistent( ).

        me->fill_sender( ).
        me->fill_recipient( ).
        me->fill_body( ).
        me->fill_attachment( ).


        "Send Email request
        "-------------------------------------------------
        me->go_send_request->set_document( me->go_document ).
        me->go_send_request->set_send_immediately( i_send_immediately = 'X' ).

        y_mail_sent = me->go_send_request->send( i_with_error_screen = 'X' ).

        IF y_mail_sent EQ abap_true.
          IF xv_commit EQ abap_true.
            COMMIT WORK.
          ENDIF.
        ENDIF.

      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs).
        RAISE EXCEPTION TYPE cx_ai_system_fault
          EXPORTING
            errortext = lx_send_req_bcs->get_text( ).

      CATCH cx_ai_system_fault INTO DATA(lx_ai_system_fault).
        RAISE EXCEPTION lx_ai_system_fault.

    ENDTRY.


  ENDMETHOD.


  METHOD check_mandatory_fields.

    IF me->gs_mail_params-recipients[] IS INITIAL.
      RAISE EXCEPTION TYPE cx_ai_system_fault
        EXPORTING
          errortext = tc_exception_msg-missing_recipients.
    ENDIF.

    IF me->gs_mail_params-object IS INITIAL.
      RAISE EXCEPTION TYPE cx_ai_system_fault
        EXPORTING
          errortext = tc_exception_msg-missing_mail_obj.
    ENDIF.

  ENDMETHOD.


  METHOD fill_sender.

    TRY.
        IF me->gs_mail_params-sender IS INITIAL.
          me->gs_mail_params-sender = sy-uname.
        ENDIF.

        DATA(lo_sender) = cl_sapuser_bcs=>create( me->gs_mail_params-sender ).
        me->go_send_request->set_sender( lo_sender ).


      CATCH cx_address_bcs INTO DATA(lx_address_bcs).

        RAISE EXCEPTION TYPE cx_ai_system_fault
          EXPORTING
            errortext = lx_address_bcs->get_text( ).

      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs).

        RAISE EXCEPTION TYPE cx_ai_system_fault
          EXPORTING
            errortext = lx_send_req_bcs->get_text( ).

    ENDTRY.

  ENDMETHOD.


  METHOD fill_recipient.

    TRY.
        LOOP AT me->gs_mail_params-recipients ASSIGNING FIELD-SYMBOL(<recipient>).

          DATA(lo_recipient) = cl_cam_address_bcs=>create_internet_address( <recipient>-smtp_addr ).

          me->go_send_request->add_recipient( i_recipient = lo_recipient
                                              i_express   = 'X'
                                              i_copy      = <recipient>-copy
          ).

        ENDLOOP.


      CATCH cx_address_bcs INTO DATA(lx_address_bcs).
        RAISE EXCEPTION TYPE cx_ai_system_fault
          EXPORTING
            errortext = lx_address_bcs->get_text( ).

      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs).
        RAISE EXCEPTION TYPE cx_ai_system_fault
          EXPORTING
            errortext = lx_send_req_bcs->get_text( ).

    ENDTRY.

  ENDMETHOD.


  METHOD fill_body.

    DATA:
      lt_lines     TYPE bcsy_text,
      lt_lines_tmp TYPE bcsy_text.


    TRY.
        CLEAR lt_lines[].

        "Set body with input string if provided
        "-------------------------------------------------
        IF me->gs_mail_params-body IS NOT INITIAL.

          lt_lines_tmp[] = cl_bcs_convert=>string_to_soli( me->gs_mail_params-body ).
          APPEND LINES OF lt_lines_tmp[] TO lt_lines[].

        ENDIF.


        "Set body with standard SO10 text with substitution if provided
        "-------------------------------------------------
        IF me->gs_mail_params-body_stdtxt IS NOT INITIAL.

          lt_lines_tmp[] = get_standard_text_lines( ).
          APPEND LINES OF lt_lines_tmp[] TO lt_lines[].

        ENDIF.


        "Set as body the mail object if nothing is provided
        "-------------------------------------------------
        IF lt_lines[] IS INITIAL.

          lt_lines_tmp = VALUE #( ( line = me->gs_mail_params-object ) ).
          APPEND LINES OF lt_lines_tmp[] TO lt_lines[].

        ENDIF.


        "Link body with BCS Request
        "-------------------------------------------------
        me->go_document = cl_document_bcs=>create_document( i_type    = tc_mail_type-htm
                                                            i_text    = lt_lines[]
                                                            i_subject = me->gs_mail_params-object
        ).


      CATCH cx_document_bcs INTO DATA(lx_document_bcs).
        RAISE EXCEPTION TYPE cx_ai_system_fault
          EXPORTING
            errortext = lx_document_bcs->get_text( ).

    ENDTRY.

  ENDMETHOD.


  METHOD fill_attachment.

    DATA:
      lt_soli_tab   TYPE soli_tab,
      lt_solix_tab  TYPE solix_tab,
      lv_csv_string TYPE string.


    TRY.
        LOOP AT me->gs_mail_params-attachments ASSIGNING FIELD-SYMBOL(<attch>).

          DATA(lv_attch_subject) = CONV sood-objdes( <attch>-subject ).


          "CSV Attachment
          "-------------------------------------------------
          IF <attch>-data_csv[] IS NOT INITIAL.

            "CSV need to be included in one single string,
            "Conversion will be applied, separating each original row by CR_LF
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

            lv_attch_subject = |{ lv_attch_subject }.{ tc_attch_type-csv }|.

            me->go_document->add_attachment( i_attachment_type     = tc_attch_type-csv
                                             i_attachment_subject  = lv_attch_subject
                                             i_att_content_text    = lt_soli_tab[]
            ).

          ENDIF.


          "EXCEL Attachment
          "-------------------------------------------------
          IF <attch>-data_xlsx IS NOT INITIAL.

            REFRESH lt_solix_tab[].
            lt_solix_tab = cl_bcs_convert=>xstring_to_solix( iv_xstring = <attch>-data_xlsx ).

            lv_attch_subject = |{ lv_attch_subject }.{ tc_attch_type-xlsx }|.

            me->go_document->add_attachment( i_attachment_type     = tc_attch_type-bin
                                             i_attachment_subject  = lv_attch_subject
                                             i_attachment_size    = CONV sood-objlen( xstrlen( <attch>-data_xlsx ) )
                                             i_att_content_hex    = lt_solix_tab[]
            ).

          ENDIF.


          "PDF Attachment
          "-------------------------------------------------
          IF <attch>-data_pdf IS NOT INITIAL.

            REFRESH lt_solix_tab[].
            lt_solix_tab = cl_bcs_convert=>xstring_to_solix( iv_xstring = <attch>-data_pdf ).

            lv_attch_subject = |{ lv_attch_subject }.{ tc_attch_type-pdf }|.

            me->go_document->add_attachment( i_attachment_type    = tc_attch_type-raw
                                             i_attachment_subject = lv_attch_subject
                                             i_att_content_hex    = lt_solix_tab[]
            ).

          ENDIF.

        ENDLOOP.


      CATCH cx_document_bcs INTO DATA(lx_document_bcs).
        RAISE EXCEPTION TYPE cx_ai_system_fault
          EXPORTING
            errortext = lx_document_bcs->get_text( ).

    ENDTRY.

  ENDMETHOD.


  METHOD get_standard_text_lines.

    DATA:
      lt_lines     TYPE TABLE OF tline,
      lv_error_msg TYPE string.


    CLEAR: lt_lines[].
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'ST'
        language                = sy-langu
        name                    = me->gs_mail_params-body_stdtxt-std_txt_name
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
          msg  = lv_error_msg.

      RAISE EXCEPTION TYPE cx_ai_system_fault
        EXPORTING
          errortext = lv_error_msg.

    ENDIF.


    LOOP AT lt_lines ASSIGNING FIELD-SYMBOL(<lines>).

      LOOP AT me->gs_mail_params-body_stdtxt-replacement ASSIGNING FIELD-SYMBOL(<replace>).
        REPLACE ALL OCCURRENCES OF <replace>-varname IN <lines>-tdline WITH <replace>-value.
      ENDLOOP.

      APPEND <lines>-tdline TO yt_lines.

    ENDLOOP.

  ENDMETHOD.


ENDCLASS.