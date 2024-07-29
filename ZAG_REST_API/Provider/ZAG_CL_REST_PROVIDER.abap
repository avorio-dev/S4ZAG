CLASS zag_cl_rest_provider DEFINITION
  INHERITING FROM cl_rest_resource
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    " Methods
    "-------------------------------------------------
    METHODS:
      if_rest_resource~get REDEFINITION,
      if_rest_resource~post REDEFINITION.


  PROTECTED SECTION.
  PRIVATE SECTION.


ENDCLASS.



CLASS ZAG_CL_REST_PROVIDER IMPLEMENTATION.


  METHOD if_rest_resource~get.

    TYPES:
      BEGIN OF ts_lfa1,
        lifnr TYPE lfa1-lifnr,
      END OF ts_lfa1.

    DATA:
      lt_lfa1  TYPE TABLE OF ts_lfa1,
      lr_lifnr TYPE RANGE OF lfa1-lifnr,
      lt_lfb1  TYPE TABLE OF lfb1.


    DATA(lv_url)          = CONV string( mo_request->get_uri( ) ).

    DATA(lv_json_request) = mo_request->get_entity( )->get_string_data( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json    = lv_json_request
      CHANGING
        data    = lt_lfa1
    ).

    CLEAR lr_lifnr[].
    lr_lifnr = VALUE #( FOR <lfa1> IN lt_lfa1
      sign = 'I' option = 'EQ'
      ( low = |{ <lfa1>-lifnr ALPHA = IN }| )
    ).

    CLEAR lt_lfb1[].
    IF lr_lifnr[] IS NOT INITIAL.

      SELECT *
        FROM lfb1
        INTO TABLE @lt_lfb1
        WHERE lifnr IN @lr_lifnr[].

    ELSE.

      SELECT * UP TO 50 ROWS
        FROM lfb1
        INTO TABLE @lt_lfb1.

    ENDIF.

    DATA(lv_json_response) = /ui2/cl_json=>serialize( data = lt_lfb1[] ).

    mo_response->create_entity( )->set_string_data( iv_data = lv_json_response ).


  ENDMETHOD.


  METHOD if_rest_resource~post.

  ENDMETHOD.
ENDCLASS.