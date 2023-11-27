class ZAG_CL_SALV definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_col_settings,
        fieldname TYPE lvc_s_fcat-fieldname,
        label     TYPE string,
        no_out    TYPE flag,
        hotspot   TYPE flag,
      END OF ty_col_settings .
  types:
    tt_col_settings TYPE TABLE OF ty_col_settings .

  constants C_ICON_GREEN type ICON_D value '@5B@' ##NO_TEXT.
  constants C_ICON_RED type ICON_D value '@5C@' ##NO_TEXT.
  constants C_ICON_YELL type ICON_D value '@5D@' ##NO_TEXT.
  constants C_ICON_INFO type ICON_D value '@0S@' ##NO_TEXT.
  constants C_ICON_MISS type ICON_D value '@D7@' ##NO_TEXT.
  constants C_ICON_EXEC type ICON_D value '@15@' ##NO_TEXT.
  constants C_ICON_REFR type ICON_D value '@42@' ##NO_TEXT.
  constants C_ICON_SAVE type ICON_D value '@2L@' ##NO_TEXT.
  constants C_CELL_COL_GREEN type LVC_COL value '5' ##NO_TEXT.
  constants C_CELL_COL_YELL type LVC_COL value '3' ##NO_TEXT.
  constants C_CELL_COL_RED type LVC_COL value '6' ##NO_TEXT.
  constants C_CELL_COL_ORAN type LVC_COL value '7' ##NO_TEXT.
  constants C_CELL_COL_NULL type LVC_COL value '2' ##NO_TEXT.
  constants C_COL_FIELDNAME type LVC_FNAME value 'T_COL' ##NO_TEXT.

  class-methods DISPLAY_GENERIC_ALV
    importing
      !X_POPUP type BOOLEAN default ABAP_FALSE
      !XT_COL_SETTINGS type TT_COL_SETTINGS optional
      !XT_OUTPUT type STANDARD TABLE
    exceptions
      GENERAL_FAULT .
  class-methods DISPLAY_TRANSPOSED_ROW
    importing
      !X_POPUP type FLAG default ABAP_TRUE
      !X_ROW type ANY
    exporting
      !YT_TRANSPOSED type STANDARD TABLE .
  class-methods SET_COLOR_CELL
    importing
      !X_COLOR type LVC_COL
      !X_FIELDNAME type FIELDNAME
    changing
      !Y_ROW type ANY
    exceptions
      COL_TAB_NOT_FOUND
      FIELDNAME_NOT_FOUND .
  class-methods SET_COLOR_ROW
    importing
      !X_COLOR type LVC_COL
    changing
      !Y_ROW type ANY
    exceptions
      COL_TAB_NOT_FOUND
      FCAT_NOT_FOUND .
  class-methods GET_FIELDCAT
    importing
      !XS_ROW type ANY optional
      !XT_TABLE type STANDARD TABLE optional
    returning
      value(YT_FCAT) type LVC_T_FCAT
    exceptions
      FCAT_NOT_FOUND .
private section.

  data GT_FCAT type LVC_T_FCAT .

  class-methods SET_SALV_TEXT_COLUMN
    importing
      !X_FIELDNAME type FIELDNAME
      !X_LABEL type STRING
    changing
      !YO_COLUMN type ref to CL_SALV_COLUMN_TABLE
    exceptions
      GENERAL_FAULT .
ENDCLASS.



CLASS ZAG_CL_SALV IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_SALV=>DISPLAY_GENERIC_ALV
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_POPUP                        TYPE        BOOLEAN (default =ABAP_FALSE)
* | [--->] XT_COL_SETTINGS                TYPE        TT_COL_SETTINGS(optional)
* | [--->] XT_OUTPUT                      TYPE        STANDARD TABLE
* | [EXC!] GENERAL_FAULT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD display_generic_alv.

    DATA: lref_output     TYPE REF TO data,
          lr_column       TYPE REF TO cl_salv_column_table,
          ls_key          TYPE salv_s_layout_key,
          lt_col_settings TYPE tt_col_settings.

    DATA: lv_excep_msg    TYPE string VALUE IS INITIAL,
          lx_root         TYPE REF TO cx_root.

    FIELD-SYMBOLS: <t_output> TYPE STANDARD TABLE.

    "-------------------------------------------------

    CREATE DATA lref_output LIKE xt_output.

    ASSIGN lref_output->* TO <t_output>.
    <t_output> = xt_output.

    "-------------------------------------------------

*    "TEMPLATE
*    APPEND INITIAL LINE TO lt_col_settings ASSIGNING FIELD-SYMBOL(<col_sett>).
*    <col_sett>-fieldname = 'FIELD'.
*    <col_sett>-label     = 'Column Label'.
*    <col_sett>-no_out    = 'X'.
*    <col_sett>-hotspot   = 'X'.

    lt_col_settings = xt_col_settings.
    SORT lt_col_settings BY fieldname.



    "Init Alv Object
    "-------------------------------------------------
    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = DATA(lcl_alv)
          CHANGING
            t_table      = <t_output>[] ).

      CATCH cx_salv_msg INTO DATA(lx_salv_msg).
        lv_excep_msg = lx_salv_msg->get_text( ).
        RAISE general_fault.
    ENDTRY.


    "Set Display settings
    "-------------------------------------------------
    DATA(lv_lines)   = lines( <t_output>[] ).
    DATA(lv_title)   = CONV lvc_title( |{ sy-title } ( { lv_lines } Record )| ) ##NO_TEXT.
    DATA(lr_display) = lcl_alv->get_display_settings( ).
    lr_display->set_list_header( lv_title ).
    lr_display->set_striped_pattern( cl_salv_display_settings=>true ).


    "Set layout settings
    "-------------------------------------------------
    ls_key-report   = sy-repid.
    DATA(lr_layout) = lcl_alv->get_layout( ).
    lr_layout->set_key( ls_key ).
    lr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

    DATA(lr_selections) = lcl_alv->get_selections( ).
    lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).


    "Set Columns Settings
    "-------------------------------------------------
    DATA(lr_columns) = lcl_alv->get_columns( ).
    lr_columns->set_optimize( 'X' ). "--> Optimise all columns

    TRY. "If T_COL column exists in your table, it will be set as color column reference
        lr_columns->set_color_column( c_col_fieldname ).
      CATCH cx_salv_data_error INTO DATA(lx_salv_data_err).
        lv_excep_msg = lx_salv_data_err->get_text( ).
    ENDTRY.


    LOOP AT lr_columns->get( ) ASSIGNING FIELD-SYMBOL(<column>).
      lr_column ?= <column>-r_column.

      "Default settings "-------------------------------------------------
      DATA(lv_rollname) = <column>-r_column->get_ddic_rollname( ).
      CASE lv_rollname.
        WHEN 'MANDT'.
          lr_column->set_visible( value  = if_salv_c_bool_sap=>false ).

        WHEN 'ICON_D'.
          lr_column->set_icon( value = if_salv_c_bool_sap=>true ).

      ENDCASE.


      "User settings "-------------------------------------------------
      ASSIGN lt_col_settings[ line_index( lt_col_settings[
        fieldname = <column>-columnname ] ) ] TO FIELD-SYMBOL(<col_settings>).
      IF sy-subrc EQ 0.

        "Labels
        IF <col_settings>-label IS NOT INITIAL.
          TRY.
              set_salv_text_column( EXPORTING
                                      x_fieldname = <col_settings>-fieldname
                                      x_label     = <col_settings>-label
                                    CHANGING
                                      yo_column   = lr_column ).

            CATCH cx_root INTO lx_root.
              lv_excep_msg = lx_root->get_text( ).
              RAISE general_fault.
          ENDTRY.
        ENDIF.

        "Hide columns
        IF <col_settings>-no_out EQ 'X'.
          lr_column->set_visible(
              value = if_salv_c_bool_sap=>false
          ).
        ENDIF.

        "Set Hotspot
        IF <col_settings>-hotspot EQ 'X'.
          lr_column->set_cell_type(
              value = if_salv_c_cell_type=>hotspot
          ).

          "TODO create class for event handling
        ENDIF.

      ENDIF.

    ENDLOOP.


    "Set handler and functions based on sy-tcode
    "-------------------------------------------------
    DATA(lr_functions) = lcl_alv->get_functions( ).
    lr_functions->set_all( 'X' ).

*    TRY.
*        DATA(lo_events) = lcl_alv->get_event( ).
*
*        CREATE OBJECT gr_event_handler. "type ref to lcl_events.
*        SET HANDLER gr_event_handler->on_link_click FOR lo_events.
*
*      CATCH cx_salv_not_found INTO DATA(lx_salv_not_found).
*        lv_excep_msg = lx_salv_not_found->get_text( ).
*        RAISE general_fault.
*    ENDTRY.


    "Print as popup
    "-------------------------------------------------
    IF x_popup EQ abap_true.

      lcl_alv->set_screen_popup(
        start_column = 1
        end_column  = 100
        start_line  = 1
        end_line    = 15 ).

    ENDIF.


    "PRINT ALV
    "-------------------------------------------------
    lcl_alv->display( ).


*"Class
*"-------------------------------------------------
**----------------------------------------------------------------------*
** SALV Event Handler Definition                                        *
**----------------------------------------------------------------------*
*CLASS lcl_events DEFINITION.
*  PUBLIC SECTION.
*
*    METHODS:
*          on_link_click FOR EVENT link_click OF cl_salv_events_table
*                          IMPORTING row column.
*
*ENDCLASS.                    "lcl_events DEFINITION
**----------------------------------------------------------------------*
**       CLASS lcl_events IMPLEMENTATION
**----------------------------------------------------------------------*
**  SAL Event Handler Methods                                           *
**----------------------------------------------------------------------*
*CLASS lcl_events IMPLEMENTATION.
*
*  METHOD on_link_click.
*
*    FIELD-SYMBOLS: <alv_0100> LIKE LINE OF gt_alv_0100.
*
*    READ TABLE gt_alv_0100 ASSIGNING <alv_0100> INDEX row.
*    CHECK sy-subrc EQ 0.
*
*    CASE column.
*      WHEN 'ANLAGE'.
*        SET PARAMETER ID 'ANL' FIELD <alv_0100>-anlage.
*        CALL TRANSACTION 'ES32' AND SKIP FIRST SCREEN.
*
*      WHEN OTHERS.
*    ENDCASE.
*
*  ENDMETHOD.                    "on_link_click
*
*ENDCLASS.                    "lcl_events IMPLEMENTATION
*DATA: gr_event_handler TYPE REF TO lcl_events.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_SALV=>DISPLAY_TRANSPOSED_ROW
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_POPUP                        TYPE        FLAG (default =ABAP_TRUE)
* | [--->] X_ROW                          TYPE        ANY
* | [<---] YT_TRANSPOSED                  TYPE        STANDARD TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD display_transposed_row.

    DATA: lref_t_row      TYPE REF TO data.

    DATA: lt_fcat        TYPE lvc_t_fcat,
          lref_transp    TYPE REF TO data,

          lo_data_transp TYPE  REF TO data,
          lt_fcat_transp TYPE  lvc_t_fcat.

    FIELD-SYMBOLS: <t_table>        TYPE STANDARD TABLE,
                   <fcat>           TYPE lvc_s_fcat,
                   <lt_transp_data> TYPE table.


    "-------------------------------------------------

    CLEAR: lo_data_transp, lt_fcat_transp[].

    CREATE DATA lref_t_row LIKE TABLE OF x_row.
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

      CATCH cx_salv_msg INTO DATA(lx_salv_msg).
        DATA(lv_except_msg) = lx_salv_msg->get_text( ).
    ENDTRY.

    DATA(lr_columns)    = lo_salv->get_columns( ).
    DATA(lt_column_ref) = lr_columns->get( ).


    "-> Trasposizione campi da NxM a MxN
    "------------------------------------------------

    LOOP AT lt_column_ref ASSIGNING FIELD-SYMBOL(<column_ref>).

      CHECK <column_ref>-columnname NE 'MANDT'.

      ASSIGN COMPONENT <column_ref>-columnname OF STRUCTURE x_row TO FIELD-SYMBOL(<original_value>).
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
    TRY.
        display_generic_alv(
          EXPORTING
*        x_popup         = abap_false
*        xt_col_settings =
            xt_output       = <lt_transp_data>
        ).
      CATCH cx_salv_msg.        " ALV: General Error Class with Message
      CATCH cx_salv_data_error. " ALV: General Error Class (Checked During Syntax Check)
      CATCH cx_salv_not_found.  " ALV: General Error Class (Checked During Syntax Check)
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_SALV=>GET_FIELDCAT
* +-------------------------------------------------------------------------------------------------+
* | [--->] XS_ROW                         TYPE        ANY(optional)
* | [--->] XT_TABLE                       TYPE        STANDARD TABLE(optional)
* | [<-()] YT_FCAT                        TYPE        LVC_T_FCAT
* | [EXC!] FCAT_NOT_FOUND
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_fieldcat.

    DATA: lref_table TYPE REF TO data.

    DATA: lv_except_msg TYPE string.

    "-------------------------------------------------

    IF xs_row IS SUPPLIED.
      CREATE DATA lref_table LIKE TABLE OF xs_row.
    ELSEIF xt_table IS SUPPLIED.
      CREATE DATA lref_table LIKE xt_table.
    ELSE.
      EXIT.
    ENDIF.


    ASSIGN lref_table->* TO FIELD-SYMBOL(<table>).
    TRY.
        cl_salv_table=>factory( IMPORTING
                                  r_salv_table   = DATA(lt_salv_table)
                                CHANGING
                                  t_table        = <table>  ).

        yt_fcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns = lt_salv_table->get_columns( ) " ALV Filter
                                                                     r_aggregations = lt_salv_table->get_aggregations( ) " ALV Aggregations
                                                                     ).

        DELETE yt_fcat WHERE fieldname EQ 'MANDT'.

      CATCH cx_ai_system_fault INTO DATA(lx_ai_system_fault).
        lv_except_msg = lx_ai_system_fault->get_text( ).
        RAISE fcat_not_found.
      CATCH cx_salv_msg INTO DATA(lx_salv_msg).
        lv_except_msg = lx_salv_msg->get_text( ).
        RAISE fcat_not_found.
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_SALV=>SET_COLOR_CELL
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_COLOR                        TYPE        LVC_COL
* | [--->] X_FIELDNAME                    TYPE        FIELDNAME
* | [<-->] Y_ROW                          TYPE        ANY
* | [EXC!] COL_TAB_NOT_FOUND
* | [EXC!] FIELDNAME_NOT_FOUND
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_color_cell.

    DATA: ls_scol TYPE lvc_s_scol.

    FIELD-SYMBOLS: <t_col> TYPE lvc_t_scol.

    "-------------------------------------------------

    ASSIGN COMPONENT c_col_fieldname OF STRUCTURE y_row TO <t_col>.
    IF sy-subrc <> 0.
      RAISE col_tab_not_found.
    ENDIF.

    ASSIGN COMPONENT x_fieldname     OF STRUCTURE y_row TO FIELD-SYMBOL(<fieldname>).
    IF sy-subrc <> 0.
      RAISE fieldname_not_found.
    ENDIF.

    DELETE <t_col> WHERE fname EQ x_fieldname.

    CLEAR ls_scol.
    ls_scol-fname     = x_fieldname.
    ls_scol-color-col = x_color.
    INSERT ls_scol INTO TABLE <t_col> .

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZAG_CL_SALV=>SET_COLOR_ROW
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_COLOR                        TYPE        LVC_COL
* | [<-->] Y_ROW                          TYPE        ANY
* | [EXC!] COL_TAB_NOT_FOUND
* | [EXC!] FCAT_NOT_FOUND
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_color_row.

    DATA: ls_scol TYPE lvc_s_scol.

    FIELD-SYMBOLS: <t_col> TYPE lvc_t_scol.

    "-------------------------------------------------

    ASSIGN COMPONENT c_col_fieldname OF STRUCTURE y_row TO <t_col>.
    IF sy-subrc <> 0.
      RAISE col_tab_not_found.
    ENDIF.

    TRY.
        DATA(lt_fcat) = get_fieldcat( xs_row = y_row ).

        LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<fcat>).

          DELETE <t_col> WHERE fname EQ <fcat>-fieldname.

          CLEAR ls_scol.
          ls_scol-fname     = <fcat>-fieldname.
          ls_scol-color-col = x_color.
          INSERT ls_scol INTO TABLE <t_col> .

        ENDLOOP.

      CATCH cx_root INTO DATA(lx_root).
        DATA(lv_excep_msg) = lx_root->get_text( ).
        RAISE fcat_not_found.
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZAG_CL_SALV=>SET_SALV_TEXT_COLUMN
* +-------------------------------------------------------------------------------------------------+
* | [--->] X_FIELDNAME                    TYPE        FIELDNAME
* | [--->] X_LABEL                        TYPE        STRING
* | [<-->] YO_COLUMN                      TYPE REF TO CL_SALV_COLUMN_TABLE
* | [EXC!] GENERAL_FAULT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_salv_text_column.

    DATA: lv_outlen    TYPE lvc_outlen,
          lv_scrtext_l TYPE scrtext_l,
          lv_scrtext_m TYPE scrtext_m,
          lv_scrtext_s TYPE scrtext_s,
          lv_text_col  TYPE lvc_fname.

    DATA: lv_excep_msg TYPE string.

    "-------------------------------------------------

    lv_scrtext_s = ''.
    lv_scrtext_m = ''.
    lv_scrtext_l = x_label.
    lv_text_col  = x_fieldname.

    lv_outlen = strlen( x_label ).
    TRY.
        yo_column->set_long_text( lv_scrtext_l ).
        yo_column->set_medium_text( lv_scrtext_m ).
        yo_column->set_short_text( lv_scrtext_s ).
        yo_column->set_text_column( lv_text_col ).
        yo_column->set_output_length( lv_outlen ).

      CATCH cx_salv_not_found INTO DATA(lx_salv_not_found).
        lv_excep_msg = lx_salv_not_found->get_text( ).
        RAISE general_fault.
      CATCH cx_salv_data_error INTO DATA(lx_salv_data_error).
        lv_excep_msg = lx_salv_data_error->get_text( ).
        RAISE general_fault.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
