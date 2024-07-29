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



CLASS zag_cl_rest_provider IMPLEMENTATION.

  METHOD if_rest_resource~get.

    DATA(lv_url) = CONV string( mo_request->get_uri( ) ).

    SPLIT lv_url AT '?=' INTO
      DATA(lv_sx)
      DATA(lv_key_value).


    lv_key_value = |{ lv_key_value ALPHA = IN }|.
    SELECT SINGLE *
      FROM lfa1
      INTO @DATA(ls_lfa1)
      WHERE lifnr EQ @lv_key_value.

    DATA(lv_json) = /ui2/cl_json=>serialize( data = ls_lfa1 ).

    mo_response->create_entity( )->set_string_data( iv_data = lv_json ).


  ENDMETHOD.

  METHOD if_rest_resource~post.

  ENDMETHOD.

ENDCLASS.