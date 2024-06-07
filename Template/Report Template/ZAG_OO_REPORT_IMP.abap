*&---------------------------------------------------------------------*
*& Include          ZAG_OO_REPORT_IMP
*&---------------------------------------------------------------------*
CLASS lcx_generic IMPLEMENTATION.

  METHOD constructor.

    super->constructor( previous = previous ).

    CLEAR me->textid.
    IF textid IS INITIAL AND me->if_t100_message~t100key IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

    me->gv_attr1 = xv_attr1.
    me->gv_attr2 = xv_attr2.
    me->gv_attr3 = xv_attr3.
    me->gv_attr4 = xv_attr4.

  ENDMETHOD.

ENDCLASS.

**********************************************************************

CLASS lcx_selscreen IMPLEMENTATION.

  METHOD constructor.

    super->constructor( previous = previous ).

    CLEAR me->textid.
    IF textid IS INITIAL AND me->if_t100_message~t100key IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

    me->gv_attr1 = xv_attr1.
    me->gv_attr2 = xv_attr2.
    me->gv_attr3 = xv_attr3.
    me->gv_attr4 = xv_attr4.

  ENDMETHOD.

ENDCLASS.

**********************************************************************

CLASS lcx_processor IMPLEMENTATION.

  METHOD constructor.

    super->constructor( previous = previous ).

    CLEAR me->textid.
    IF textid IS INITIAL AND me->if_t100_message~t100key IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

    me->gv_attr1 = xv_attr1.
    me->gv_attr2 = xv_attr2.
    me->gv_attr3 = xv_attr3.
    me->gv_attr4 = xv_attr4.

  ENDMETHOD.

ENDCLASS.

**********************************************************************

CLASS lcl_selection_screen IMPLEMENTATION.
  METHOD set_params.

    "Set Select Options / Parameters
    me->gr_partner = xr_partner[].
    me->gr_bukrs   = xr_bukrs[].

  ENDMETHOD.

  METHOD get_params.

    "Get Select Options / Parameters
    yr_partner = me->gr_partner[].
    yr_bukrs   = me->gr_bukrs[].

  ENDMETHOD.

  METHOD check_params.

    " Check input parameters
    IF 1 = 2.
      RAISE EXCEPTION TYPE lcx_selscreen
        EXPORTING
          textid   = lcx_selscreen=>missing_param
          xv_attr1 = 'Partner'.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

**********************************************************************

CLASS lcl_data_processor IMPLEMENTATION.

  METHOD main.

    "Get Selection Screen Parameters
    xo_selection_screen->get_params(
      IMPORTING
        yr_partner = me->gr_partner[]
        yr_bukrs   = me->gr_bukrs[]
    ).

    "Data extraction
    me->select_data( ).

    "Build Output Table
    DATA(lt_output) = me->build_output( ).

    " Display data
    DATA(lo_display) = NEW lcl_display( xt_output = lt_output ).
    lo_display->display_data( ).

  ENDMETHOD.

  METHOD select_data.

    SELECT *
      FROM but000
      INTO TABLE @me->gt_partner
      WHERE partner IN @me->gr_partner[].

    "Raise exception if no data found
    IF me->gt_partner IS INITIAL.

      RAISE EXCEPTION TYPE lcx_processor
        EXPORTING
          textid = lcx_processor=>no_rec_found.

    ENDIF.
  ENDMETHOD.

  METHOD build_output.

    me->gt_output = CORRESPONDING #( gt_partner ).

    yt_output = me->gt_output[].

  ENDMETHOD.
ENDCLASS.

**********************************************************************

CLASS lcl_display IMPLEMENTATION.

  METHOD constructor.

    me->gt_output = xt_output[].

  ENDMETHOD.

  METHOD display_data.

    DATA: lo_alv TYPE REF TO cl_salv_table.

    TRY.

        " Create ALV instance
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_alv
          CHANGING
            t_table      = me->gt_output ).

        " Display ALV
        lo_alv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_salv_msg).

        RAISE EXCEPTION TYPE lcx_generic
          EXPORTING
            textid = lcx_generic=>generic_fault.

    ENDTRY.


  ENDMETHOD.
ENDCLASS.
