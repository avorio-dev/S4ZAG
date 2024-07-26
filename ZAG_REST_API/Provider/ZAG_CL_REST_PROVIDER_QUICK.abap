CLASS zag_cl_rest_provider_quick DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    " Interfaces
    "-------------------------------------------------
    INTERFACES:
      if_http_extension.


    "Aliases
    "---------------------------------------------------------------
    ALIASES:
      handle_request FOR if_http_extension~handle_request.


    " Constants
    "-------------------------------------------------
    CONSTANTS:
      c_http_499 TYPE i VALUE 499 ##NO_TEXT.


    " Methods
    "-------------------------------------------------
    CLASS-METHODS:
      format_syst_message
        RETURNING VALUE(y_msg) TYPE string.


  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zag_cl_rest_provider_quick IMPLEMENTATION.


  METHOD handle_request.

    DATA: lt_data_req  TYPE TABLE OF lfa1,
          lt_data_resp TYPE TABLE OF lfb1.

    "---------------------------------------------------------------

    CLEAR: lt_data_req[],
           lt_data_resp[].

    " Import JSON into SAP Structure Request
    "-------------------------------------------------
    DATA(lv_json_string) = server->request->get_cdata( ).

    /ui2/cl_json=>deserialize(
      EXPORTING
        json     = lv_json_string
      CHANGING
        data     = lt_data_req
    ).


    " Custom Logic
    "---------------------------------------------------------------
    IF lt_data_req IS NOT INITIAL.

      SELECT lifnr, bukrs
        FROM lfb1
        INTO TABLE @DATA(lt_lfb1)
        FOR ALL ENTRIES IN @lt_data_req
        WHERE lifnr EQ @lt_data_req-lifnr.
      IF sy-subrc EQ 0.

        lt_data_resp[] = CORRESPONDING #( lt_lfb1[] ).

      ENDIF.

    ENDIF.


    " Export SAP Response Structure into JSON
    "-------------------------------------------------
    lv_json_string = /ui2/cl_json=>serialize( data = lt_data_resp ).

    server->response->set_cdata(
        data = lv_json_string
    ).

  ENDMETHOD.


  METHOD format_syst_message.

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
        msg       = y_msg
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
    ENDIF.

  ENDMETHOD.

ENDCLASS.