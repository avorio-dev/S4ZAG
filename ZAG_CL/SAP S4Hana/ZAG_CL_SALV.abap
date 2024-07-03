CLASS zag_cl_salv DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    " Types
    "---------------------------------------------------------------
    TYPES:
      BEGIN OF ts_col_settings,
        fieldname TYPE lvc_s_fcat-fieldname,
        label     TYPE string,
        no_out    TYPE flag,
        hotspot   TYPE flag,
      END OF ts_col_settings .

    TYPES:
      tt_col_settings TYPE TABLE OF ts_col_settings WITH DEFAULT KEY,
      tt_fieldname    TYPE TABLE OF lvc_s_fcat-fieldname WITH DEFAULT KEY.


    " Constants
    "---------------------------------------------------------------
    CONSTANTS:
      BEGIN OF cc_icon,
        green TYPE icon_d VALUE '@5B@' ##NO_TEXT,
        red   TYPE icon_d VALUE '@5C@' ##NO_TEXT,
        yell  TYPE icon_d VALUE '@5D@' ##NO_TEXT,
        info  TYPE icon_d VALUE '@0S@' ##NO_TEXT,
        miss  TYPE icon_d VALUE '@D7@' ##NO_TEXT,
        exec  TYPE icon_d VALUE '@15@' ##NO_TEXT,
        refr  TYPE icon_d VALUE '@42@' ##NO_TEXT,
        save  TYPE icon_d VALUE '@2L@' ##NO_TEXT,
      END OF cc_icon,

      BEGIN OF cc_cell_col,
        cyan  TYPE lvc_col VALUE '1' ##NO_TEXT,
        grey  TYPE lvc_col VALUE '2' ##NO_TEXT,
        yell  TYPE lvc_col VALUE '3' ##NO_TEXT,
        blue  TYPE lvc_col VALUE '4' ##NO_TEXT,
        green TYPE lvc_col VALUE '5' ##NO_TEXT,
        red   TYPE lvc_col VALUE '6' ##NO_TEXT,
        oran  TYPE lvc_col VALUE '7' ##NO_TEXT,
      END OF cc_cell_col .

    CONSTANTS:
      c_col_fieldname TYPE lvc_fname VALUE 'T_COL' ##NO_TEXT.


    " Methods
    "---------------------------------------------------------------
    CLASS-METHODS:
      set_color_cell
        IMPORTING
          !xs_color     TYPE lvc_s_colo
          !xt_fieldname TYPE tt_fieldname
        CHANGING
          !y_row        TYPE any
        EXCEPTIONS
          col_tab_not_found
          fieldname_not_found,

      set_color_row
        IMPORTING
          !xs_color TYPE lvc_s_colo
        CHANGING
          !ys_row   TYPE any
        EXCEPTIONS
          col_tab_not_found
          unable_define_structdescr,

      get_fieldcat_from_data
        IMPORTING
          !xs_sap_line    TYPE any OPTIONAL
          !xt_sap_table   TYPE table OPTIONAL
        EXPORTING
          !yo_structdescr TYPE REF TO cl_abap_structdescr
          !yt_fcat        TYPE lvc_t_fcat
        EXCEPTIONS
          unable_define_structdescr .

    METHODS:
      display_generic_alv
        IMPORTING
          !xt_output        TYPE STANDARD TABLE
          !xt_col_settings  TYPE tt_col_settings OPTIONAL
          !xo_event_handler TYPE REF TO object OPTIONAL
          !xv_popup         TYPE boolean DEFAULT abap_false
        RAISING
          cx_salv_msg,

      display_transposed_row
        IMPORTING
          !xv_popup      TYPE flag DEFAULT abap_true
          !xs_row        TYPE any
        EXPORTING
          !yt_transposed TYPE STANDARD TABLE
        RAISING
          cx_salv_msg,

      on_link_click FOR EVENT link_click OF cl_salv_events_table
        IMPORTING
          row
          column,

      on_double_click FOR EVENT double_click OF cl_salv_events_table
        IMPORTING
          row
          column.


  PRIVATE SECTION.

    " Types
    "---------------------------------------------------------------
    TYPES:
      tt_sorted_col_settings TYPE SORTED TABLE OF ts_col_settings WITH NON-UNIQUE KEY fieldname.


    " Data
    "---------------------------------------------------------------
    DATA:
      go_salv          TYPE REF TO cl_salv_table,
      gref_output      TYPE REF TO data,
      gt_fcat          TYPE lvc_t_fcat,
      go_structdescr   TYPE REF TO cl_abap_structdescr,
      gt_col_settings  TYPE tt_sorted_col_settings,
      go_event_handler TYPE REF TO object.


    " Methods
    "---------------------------------------------------------------
    METHODS:
      set_display_settings,
      set_layout_settings,
      set_column_settings,
      set_handler_settings.

ENDCLASS.



CLASS zag_cl_salv IMPLEMENTATION.


  METHOD display_generic_alv.

    DATA:
      lv_excep_msg TYPE string VALUE IS INITIAL,
      lx_root      TYPE REF TO cx_root.

    FIELD-SYMBOLS:
      <t_output> TYPE STANDARD TABLE.


    "Init. SALV Object
    "-------------------------------------------------
    TRY.
        CLEAR me->gref_output.
        CREATE DATA me->gref_output LIKE xt_output.
        ASSIGN me->gref_output->* TO <t_output>.

        <t_output> = xt_output.

        FREE me->go_salv.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = me->go_salv
          CHANGING
            t_table      = <t_output>[] ).


        get_fieldcat_from_data(
          EXPORTING
            xt_sap_table              = <t_output>
          IMPORTING
            yo_structdescr            = me->go_structdescr
            yt_fcat                   = me->gt_fcat[]
          EXCEPTIONS
            unable_define_structdescr = 1
            OTHERS                    = 2
        ).
        IF sy-subrc <> 0.
        ENDIF.


        "Set Display settings
        "-------------------------------------------------
        set_display_settings( ).


        "Set layout settings
        "-------------------------------------------------
        set_layout_settings( ).


        "Set Columns Settings
        "-------------------------------------------------
        me->gt_col_settings = xt_col_settings[].
        set_column_settings( ).


        "Set handler and functions based on sy-tcode
        "-------------------------------------------------
        me->go_event_handler = xo_event_handler.
        set_handler_settings( ).


        "Print as PopUp
        "-------------------------------------------------
        IF xv_popup EQ abap_true.

          me->go_salv->set_screen_popup(
            start_column = 1
            end_column   = 100
            start_line   = 1
            end_line     = 15
          ).

        ENDIF.


        "PRINT ALV
        "-------------------------------------------------
        me->go_salv->display( ).


      CATCH cx_salv_msg INTO DATA(lx_salv_msg).
        RAISE EXCEPTION lx_salv_msg.

    ENDTRY.


  ENDMETHOD.


  METHOD display_transposed_row.

    DATA:
      lref_t_row     TYPE REF TO data,
      lt_fcat        TYPE lvc_t_fcat,
      lref_transp    TYPE REF TO data,

      lo_data_transp TYPE  REF TO data,
      lt_fcat_transp TYPE  lvc_t_fcat.

    FIELD-SYMBOLS:
      <t_table>        TYPE STANDARD TABLE,
      <fcat>           TYPE lvc_s_fcat,
      <lt_transp_data> TYPE table.


    "-------------------------------------------------

    CLEAR: lo_data_transp, lt_fcat_transp[].

    CREATE DATA lref_t_row LIKE TABLE OF xs_row.
    ASSIGN lref_t_row->* TO <t_table>.


    "-> Creazione tabella 'DESCR_CAMPO' + 'VALORE'
    "-> con relativo riferimento <fs>
    "------------------------------------------------
    APPEND INITIAL LINE TO lt_fcat ASSIGNING <fcat>.
    <fcat>-fieldname  = 'COLUMNTEXT'.
    <fcat>-ref_table  = 'LVC_S_DETA'.

    APPEND INITIAL LINE TO lt_fcat ASSIGNING <fcat>.
    <fcat>-fieldname  = 'VALUE'.
    <fcat>-ref_field  = 'VALUE'.
    <fcat>-ref_table  = 'LVC_S_DETA'.

    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog = lt_fcat
      IMPORTING
        ep_table        = lref_transp.

    ASSIGN lref_transp->* TO <lt_transp_data>.


    "-> Estrazione lista campi tabella originale
    "------------------------------------------------
    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = DATA(lo_salv)
          CHANGING
            t_table      = <t_table>[] ).


        DATA(lr_columns)    = lo_salv->get_columns( ).
        DATA(lt_column_ref) = lr_columns->get( ).


        "-> Trasposizione campi da NxM a MxN
        "------------------------------------------------

        LOOP AT lt_column_ref ASSIGNING FIELD-SYMBOL(<column_ref>).

          CHECK <column_ref>-columnname NE 'MANDT'.

          ASSIGN COMPONENT <column_ref>-columnname OF STRUCTURE xs_row TO FIELD-SYMBOL(<original_value>).
          CHECK sy-subrc EQ 0.

          APPEND INITIAL LINE TO <lt_transp_data> ASSIGNING FIELD-SYMBOL(<transp_row>).

          ASSIGN COMPONENT 'COLUMNTEXT' OF STRUCTURE <transp_row> TO FIELD-SYMBOL(<transp_coltxt>).
          ASSIGN COMPONENT 'VALUE'      OF STRUCTURE <transp_row> TO FIELD-SYMBOL(<transp_value>).

          <transp_coltxt> = <column_ref>-r_column->get_long_text( ).
          <transp_value>  = <original_value>.

        ENDLOOP.


        "-> Esportazione tabella trasposta
        "------------------------------------------------
        yt_transposed = <lt_transp_data>.


        "-> Stampa tabella
        "-------------------------------------------------

        display_generic_alv( <lt_transp_data>[] ).

      CATCH cx_salv_msg INTO DATA(lx_salv_msg).
        DATA(lv_except_msg) = lx_salv_msg->get_text( ).
        RAISE EXCEPTION lx_salv_msg.

    ENDTRY.

  ENDMETHOD.


  METHOD get_fieldcat_from_data.

    DATA:
      lv_except_msg  TYPE string,
      lref_sap_data  TYPE REF TO data,
      lref_sap_table TYPE REF TO data.

    FIELD-SYMBOLS:
      <sap_line>  TYPE any,
      <sap_table> TYPE STANDARD TABLE.

    "-------------------------------------------------

    FREE yo_structdescr.
    CLEAR yt_fcat[].

    IF xs_sap_line IS SUPPLIED.
      CREATE DATA lref_sap_data LIKE xs_sap_line.
      ASSIGN lref_sap_data->* TO <sap_line>.

      CREATE DATA lref_sap_table LIKE TABLE OF xs_sap_line.
      ASSIGN lref_sap_table->* TO <sap_table>.

    ELSEIF xt_sap_table IS SUPPLIED.
      CREATE DATA lref_sap_data LIKE LINE OF xt_sap_table.
      ASSIGN lref_sap_data->* TO <sap_line>.

      CREATE DATA lref_sap_table LIKE xt_sap_table.
      ASSIGN lref_sap_table->* TO <sap_table>.

    ELSE.
      RAISE unable_define_structdescr.

    ENDIF.

    "-------------------------------------------------

    yo_structdescr ?= cl_abap_typedescr=>describe_by_data( <sap_line> ).


    TRY.
        cl_salv_table=>factory(
            IMPORTING
              r_salv_table   = DATA(lt_salv_table)
            CHANGING
              t_table        = <sap_table>
        ).

        yt_fcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
            r_columns      = lt_salv_table->get_columns( ) " ALV Filter
            r_aggregations = lt_salv_table->get_aggregations( ) " ALV Aggregations
        ) .


      CATCH cx_ai_system_fault INTO DATA(lx_ai_system_fault).
        lv_except_msg = lx_ai_system_fault->get_text( ).
        RAISE unable_define_structdescr.

      CATCH cx_salv_msg  INTO DATA(lx_salv_msg).
        lv_except_msg = lx_salv_msg->get_text( ).
        RAISE unable_define_structdescr.

    ENDTRY.

  ENDMETHOD.


  METHOD set_color_cell.

    DATA:
      ls_scol TYPE lvc_s_scol.

    FIELD-SYMBOLS:
      <t_col> TYPE lvc_t_scol.

    "-------------------------------------------------

    ASSIGN COMPONENT c_col_fieldname OF STRUCTURE y_row TO <t_col>.
    IF sy-subrc <> 0.
      RAISE col_tab_not_found.
    ENDIF.

    LOOP AT xt_fieldname ASSIGNING FIELD-SYMBOL(<fieldname>).
      ASSIGN COMPONENT <fieldname> OF STRUCTURE y_row TO FIELD-SYMBOL(<field_check>).
      IF sy-subrc <> 0.
        RAISE fieldname_not_found.
      ENDIF.

      DELETE <t_col> WHERE fname EQ <fieldname>.

      INSERT VALUE #(
        fname     = <fieldname>
        color-col = xs_color-col
        color-int = xs_color-int
        color-inv = xs_color-inv
      ) INTO TABLE <t_col>.

    ENDLOOP.

  ENDMETHOD.


  METHOD set_color_row.

    DATA:
      ls_scol TYPE lvc_s_scol.

    FIELD-SYMBOLS:
      <t_col> TYPE lvc_t_scol.

    "-------------------------------------------------

    ASSIGN COMPONENT c_col_fieldname OF STRUCTURE ys_row TO <t_col>.
    IF sy-subrc <> 0.
      RAISE col_tab_not_found.
    ENDIF.

    get_fieldcat_from_data(
      EXPORTING
        xs_sap_line = ys_row
      IMPORTING
        yt_fcat     = DATA(lt_fcat)
      EXCEPTIONS
        unable_define_structdescr = 1
        OTHERS                    = 2
    ).
    IF sy-subrc <> 0.
      RAISE unable_define_structdescr.
    ENDIF.

    LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<fcat>).

      DELETE <t_col> WHERE fname EQ <fcat>-fieldname.

      INSERT VALUE #(
        fname     = <fcat>-fieldname
        color-col = xs_color-col
        color-int = xs_color-int
        color-inv = xs_color-inv
      ) INTO TABLE <t_col>.

    ENDLOOP.

  ENDMETHOD.


  METHOD set_display_settings.

    FIELD-SYMBOLS:
      <t_output> TYPE STANDARD TABLE.


    ASSIGN me->gref_output->* TO <t_output>.

    DATA(lv_lines)   = lines( <t_output>[] ).
    DATA(lv_title)   = CONV lvc_title( |{ sy-title } ( { lv_lines } Record )| ) ##NO_TEXT.

    DATA(lr_display) = me->go_salv->get_display_settings( ).
    lr_display->set_list_header( lv_title ).
    lr_display->set_striped_pattern( cl_salv_display_settings=>true ).

  ENDMETHOD.


  METHOD set_layout_settings.

    DATA:
      ls_key TYPE salv_s_layout_key.


    ls_key-report   = sy-repid.
    DATA(lr_layout) = me->go_salv->get_layout( ).
    lr_layout->set_key( ls_key ).
    lr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

    DATA(lr_selections) = me->go_salv->get_selections( ).
    lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

  ENDMETHOD.


  METHOD set_column_settings.

    DATA:
      lr_column TYPE REF TO cl_salv_column_table.

    FIELD-SYMBOLS:
      <t_output> TYPE STANDARD TABLE.


    "--> Optimize all columns
    "---------------------------------------------------------------
    DATA(lr_columns) = me->go_salv->get_columns( ).
    lr_columns->set_optimize( 'X' ).


    "If T_COL column exists in your table, it will be set as color column reference
    "---------------------------------------------------------------
    TRY.
        lr_columns->get_column( c_col_fieldname  ).
        lr_columns->set_color_column( c_col_fieldname ).

      CATCH cx_salv_not_found  INTO DATA(lx_salv_not_found).
      CATCH cx_salv_data_error INTO DATA(lx_salv_data_err).
    ENDTRY.



    " Set Fieldcat settings
    "---------------------------------------------------------------
    LOOP AT lr_columns->get( ) ASSIGNING FIELD-SYMBOL(<column>).
      lr_column ?= <column>-r_column.

      "Default settings "-------------------------------------------------
      DATA(lv_rollname) = <column>-r_column->get_ddic_rollname( ).
      CASE lv_rollname.
        WHEN 'MANDT'.
          lr_column->set_visible( if_salv_c_bool_sap=>false ).

        WHEN 'ICON_D'.
          lr_column->set_icon( if_salv_c_bool_sap=>true ).
          lr_column->set_alignment( if_salv_c_alignment=>centered ).

      ENDCASE.

      ASSIGN me->gt_fcat[ fieldname = <column>-columnname ] TO FIELD-SYMBOL(<fcat>).
      IF sy-subrc EQ 0.
        lr_column->set_ddic_reference(
          VALUE #(
            table =  <fcat>-ref_table
            field =  <fcat>-ref_field
          )
        ).
      ENDIF.


      "User settings
      "---------------------------------------------------------------
      ASSIGN me->gt_col_settings[ fieldname = <column>-columnname ] TO FIELD-SYMBOL(<col_settings>).
      IF sy-subrc EQ 0.

        "Labels
        IF <col_settings>-label IS NOT INITIAL.

          lr_column->set_long_text( CONV #( <col_settings>-label ) ).
          lr_column->set_medium_text( CONV #( <col_settings>-label ) ).
          lr_column->set_short_text( CONV #( <col_settings>-label ) ).

        ENDIF.


        "Hide columns
        IF <col_settings>-no_out EQ 'X'.

          lr_column->set_visible( if_salv_c_bool_sap=>false ).

        ENDIF.


        "Set Hotspot
        IF <col_settings>-hotspot EQ 'X'.

          lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).

        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD set_handler_settings.

    DATA:
        lo_event_handler TYPE REF TO object.


    DATA(lr_functions) = me->go_salv->get_functions( ).
    lr_functions->set_all( 'X' ).
    DATA(lo_events) = me->go_salv->get_event( ).


    CHECK me->go_event_handler IS NOT INITIAL.
    SET HANDLER on_link_click   FOR lo_events.
    SET HANDLER on_double_click FOR lo_events.


  ENDMETHOD.

  METHOD on_link_click.

    TRY.
        CALL METHOD me->go_event_handler->('ON_LINK_CLICK')
          EXPORTING
            xv_row    = row
            xv_column = column.

      CATCH cx_sy_dyn_call_illegal_method INTO DATA(lx_illegal_method).
        MESSAGE lx_illegal_method->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD on_double_click.

    TRY.
        CALL METHOD me->go_event_handler->('ON_DOUBLE_CLICK')
          EXPORTING
            xv_row    = row
            xv_column = column.

      CATCH cx_sy_dyn_call_illegal_method INTO DATA(lx_illegal_method).
        MESSAGE lx_illegal_method->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
