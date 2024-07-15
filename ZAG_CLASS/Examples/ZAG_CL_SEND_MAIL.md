# ZAG_CL_SEND_MAIL <a name="zag_cl_send_mail"></a>
- SEND_MAIL
    - In addition to the classic parameters such as Recipient / Email Subject / Email Body,
    it allows attaching files like CSV, XLSX, and PDF.

    - Furthermore, you will be able to use a Standard Text created by Trx SO10 as the Email Body.
    It will be enough to provide the name of the Standard Text and, if necessary,
    you will be able to provide a variable name in the Standard Text
    (for example &LIFNR&) which will be replaced automatically by corresponding value provided.
    
>NB: If you not provide any mail body, the class will automatically put the subject mail as body because it is a mandatory parameter.

---

```abap
  "Example 1 -> Simple mail
  "-------------------------------------------------

  SELECT * FROM bseg UP TO 10 ROWS INTO TABLE @DATA(lt_data).
  
  DATA: lt_recipients TYPE zag_cl_send_mail=>ts_mail_params-recipients.

  lt_recipients = VALUE #(
    (
      smtp_addr = 'yourmail@domain.com'
      copy      = space
    )
    (
      smtp_addr = 'yourmailcopy@domain.com'
      copy      = 'X'
    )
  ).

  DATA(lo_send_mail) = NEW zag_cl_send_mail( ).
  lo_send_mail->send_mail(
    EXPORTING
      xs_mail_params   = VALUE #(
                            sender     = sy-uname
                            recipients = lt_recipients[]
                            object    = 'Mail Object'
                            body      = 'Mail Body'
                         )
      xv_commit        = abap_true
    IMPORTING
      y_mail_sent      = DATA(lv_mail_sent)
      y_error_msg      = DATA(lv_error_msg)
    EXCEPTIONS
      missing_param    = 1
      request_error    = 2
      sender_error     = 3
      recipient_error  = 4
      body_error       = 5
      attachment_error = 6
      OTHERS           = 7
  ).
  IF sy-subrc <> 0
    OR lv_mail_sent NE abap_true.
    WRITE lv_error_msg.
  ENDIF.
```

---

```abap
  "Example 2 -> Mail with attach. and Standard Text as Mail Body
  "-------------------------------------------------

  DATA:
    lt_recipients TYPE zag_cl_send_mail=>ts_mail_params-recipients,
    ls_mail_body_stdtxt TYPE zag_cl_send_mail=>ts_mail_params-body_stdtxt,
    lt_attch            TYPE zag_cl_send_mail=>ts_mail_params-attachments.


  SELECT * FROM bseg UP TO 10 ROWS INTO TABLE @DATA(lt_data).

  lt_recipients = VALUE #(
    (
      smtp_addr = 'yourmail@domain.com'
      copy      = space
    )
    (
      smtp_addr = 'yourmailcopy@domain.com'
      copy      = 'X'
    )
  ).


  APPEND INITIAL LINE TO lt_attch ASSIGNING FIELD-SYMBOL(<attch>).
  <attch>-subject = 'your_file_name'.

  TRY.
    zag_cl_csv_xlsx=>get_fieldcat_from_data(
        EXPORTING
        xt_sap_table              = lt_data
        IMPORTING
        yt_fcat                   = DATA(lt_fcat)
        yo_structdescr            = DATA(lo_structdescr)
    ).

    CATCH cx_ai_system_fault INTO DATA(lx_ai_system_fault).
  ENDTRY.


  APPEND INITIAL LINE TO <attch>-data_csv ASSIGNING FIELD-SYMBOL(<csv_line>).
  <csv_line> = lo_csv_xlsx->get_header_from_data( xt_fcat = lt_fcat ).

  LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<sflight>).

    APPEND INITIAL LINE TO <attch>-data_csv ASSIGNING <csv_line>.

    zag_cl_csv_xlsx=>conv_sap_to_string(
      EXPORTING
        xs_sap_data    = <sflight>
        xt_fcat        = lt_fcat
        xo_structdescr = lo_structdescr
      IMPORTING
        y_str_data     = <csv_line>
    ).

  ENDLOOP.


  ls_mail_body_stdtxt-std_txt_name = 'ZAG_SEND_MAIL_BCS_STDTXT'.
  ls_mail_body_stdtxt-replacement  = VALUE #(
    (
      varname = '&REPLACE_VAR&'
      value   = sy-datum
    )
  ).


  CLEAR lo_send_mail.
  lo_send_mail = NEW zag_cl_send_mail( ).
  lo_send_mail->send_mail(
    EXPORTING
      xs_mail_params   = VALUE #(
                            sender      = sy-uname
                            recipients  = lt_recipients[]
                            object      = 'Mail Object'
                            body        = 'Mail Body'
                            body_stdtxt = ls_mail_body_stdtxt
                            attachments = lt_attch[]
                         )
      xv_commit        = abap_true
    IMPORTING
      y_mail_sent      = lv_mail_sent
      y_error_msg      = lv_error_msg
    EXCEPTIONS
      missing_param    = 1
      request_error    = 2
      sender_error     = 3
      recipient_error  = 4
      body_error       = 5
      attachment_error = 6
      OTHERS           = 7
  ).
  IF sy-subrc <> 0
    OR lv_mail_sent NE abap_true.
    WRITE lv_error_msg.
  ENDIF.
```