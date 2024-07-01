*&---------------------------------------------------------------------*
*& Include          ZAG_OO_REPORT_IMP
*&---------------------------------------------------------------------*
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

  METHOD get_params.

    "Get Parameters / Select Options
    ys_params = me->gs_params.
    ys_selopt = me->gs_selopt.

  ENDMETHOD.

  METHOD set_params.

    "Set Parameters / Select Options
    me->gs_params = xs_params.
    me->gs_selopt = xs_selopt.

    TRY.
        me->check_params( ).

      CATCH lcx_selscreen INTO DATA(lx_selscreen).
        RAISE EXCEPTION lx_selscreen.

    ENDTRY.

  ENDMETHOD.

  METHOD check_params.

    " Check input parameters
    IF 1 = 2.
      RAISE EXCEPTION TYPE lcx_selscreen
        EXPORTING
          textid   = lcx_selscreen=>missing_param
          xv_attr1 = 'Vendor'.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

**********************************************************************

CLASS lcl_data_processor IMPLEMENTATION.

  METHOD main.

    "Get Selection Screen Parameters
    xo_selection_screen->get_params(
      IMPORTING
        ys_params = me->gs_params
        ys_selopt = me->gs_selopt
    ).


    TRY.
        "Data extraction
        me->select_data( ).

        "Build Output Table
        me->build_output( ).

        " Display data
        DATA(lo_display) = NEW lcl_display( me->get_output(  ) ).
        lo_display->display_data( ).

      CATCH lcx_processor INTO DATA(lx_processor).
        RAISE EXCEPTION lx_processor.

    ENDTRY.

  ENDMETHOD.

  METHOD select_data.

    CLEAR me->gt_lfa1[].
    SELECT * UP TO 10 ROWS
      FROM lfa1
      INTO TABLE @me->gt_lfa1
      WHERE lifnr IN @me->gs_selopt-lifnr[].
    IF sy-subrc <> 0.

      "Raise exception if no data found
      IF me->gt_lfa1 IS INITIAL.

        RAISE EXCEPTION TYPE lcx_processor
          EXPORTING
            textid = lcx_processor=>no_rec_found.

      ENDIF.

    ENDIF.

    CLEAR me->gt_lfb1[].
    SELECT * UP TO 10 ROWS
      FROM lfb1
      INTO TABLE @me->gt_lfb1
      FOR ALL ENTRIES IN @me->gt_lfa1
      WHERE lifnr EQ @me->gt_lfa1-lifnr
        AND bukrs IN @me->gs_selopt-bukrs.


  ENDMETHOD.

  METHOD build_output.

    CLEAR me->gt_output[].
    LOOP AT me->gt_lfa1 ASSIGNING FIELD-SYMBOL(<lfa1>).

      LOOP AT me->gt_lfb1 ASSIGNING FIELD-SYMBOL(<lfb1>)
          FROM line_index( me->gt_lfb1[ lifnr = <lfa1>-lifnr ] ).
        IF <lfb1>-lifnr NE <lfa1>-lifnr.
          EXIT.
        ENDIF.

        DATA(ls_output) = CORRESPONDING ts_output( <lfa1> ).
        ls_output       = CORRESPONDING #( BASE ( ls_output ) <lfb1> ).

        APPEND ls_output TO me->gt_output.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_output.

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

        RAISE EXCEPTION TYPE lcx_processor
          EXPORTING
            textid = lcx_processor=>generic_fault.

    ENDTRY.

  ENDMETHOD.

ENDCLASS.
