*&---------------------------------------------------------------------*
*& Report ZEXIT_FINDER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zag_enh_spot_finder NO STANDARD PAGE HEADING..

"Declarations
"-------------------------------------------------
TABLES: tstc.

TYPES: BEGIN OF ty_tfdir,
         pname    TYPE tfdir-pname,
         funcname TYPE tfdir-funcname,
       END OF ty_tfdir.

TYPES: BEGIN OF ty_enlfdir,
         funcname      TYPE enlfdir-funcname,
         area          TYPE enlfdir-area,
         area_obj_name TYPE tadir-obj_name,
       END OF ty_enlfdir.

TYPES: BEGIN OF ty_modsap.
         INCLUDE TYPE modsap.
         TYPES: modtext TYPE modsapt-modtext.
TYPES: END OF ty_modsap.

TYPES: BEGIN OF ty_smod.
         INCLUDE TYPE tadir.
         TYPES: modsap_name TYPE modsap-name.
TYPES: END OF ty_smod.

TYPES: BEGIN OF ty_sxsd.
         INCLUDE TYPE tadir.
         TYPES: exit_name TYPE sxs_attr-exit_name,
                badi_name TYPE badi_impl-badi_name.
TYPES: END OF ty_sxsd.

TYPES: BEGIN OF ty_sxs_attr.
         INCLUDE TYPE sxs_attr.
         TYPES: text TYPE sxs_attrt-text.
TYPES: END OF ty_sxs_attr.

TYPES: BEGIN OF ty_smod_out,
         tcode   TYPE tstc-tcode,
         name    TYPE modsap-name,
         typ     TYPE modsap-typ,
         member  TYPE modsap-member,
         modtext TYPE modsapt-modtext,
       END OF ty_smod_out.

TYPES: BEGIN OF ty_sxsd_out,
         tcode    TYPE tstc-tcode,
         name     TYPE sxs_attr-exit_name,
         text     TYPE sxs_attrt-text,
         mltp_use TYPE sxs_attr-mltp_use,
       END OF ty_sxsd_out.

DATA: t_tstc      TYPE TABLE OF tstc,
      t_tadir     TYPE TABLE OF tadir,
      t_tfdir     TYPE TABLE OF ty_tfdir,
      t_enlfdir   TYPE TABLE OF ty_enlfdir,
      t_smod      TYPE TABLE OF ty_smod,
      t_sxsd      TYPE TABLE OF ty_sxsd,
      t_sxsd_attr TYPE TABLE OF ty_sxs_attr,
      t_badi_impl TYPE TABLE OF badi_impl,
      t_modsap    TYPE TABLE OF ty_modsap.

DATA: t_smod_out TYPE TABLE OF ty_smod_out,
      t_sxsd_out TYPE TABLE OF ty_sxsd_out.


"Selection Screen
"-------------------------------------------------
SELECT-OPTIONS: s_tcode FOR tstc-tcode OBLIGATORY NO INTERVALS.

PARAMETERS: rb_exit RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND ucom,
            rb_badi RADIOBUTTON GROUP g1.

"Class
"-------------------------------------------------
*----------------------------------------------------------------------*
* SALV Event Handler Definition                                        *
*----------------------------------------------------------------------*
CLASS lcl_events DEFINITION.
  PUBLIC SECTION.

    METHODS:
      on_link_click FOR EVENT link_click OF cl_salv_events_table
        IMPORTING row column.

ENDCLASS.                    "lcl_events DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_events IMPLEMENTATION
*----------------------------------------------------------------------*
*  SAL Event Handler Methods                                           *
*----------------------------------------------------------------------*
CLASS lcl_events IMPLEMENTATION.

  METHOD on_link_click.

    READ TABLE t_smod_out ASSIGNING FIELD-SYMBOL(<smod>) INDEX row.
    CHECK sy-subrc EQ 0.

    CASE column.
      WHEN 'NAME'.
        SET PARAMETER ID 'MON' FIELD <smod>-name.
        CALL TRANSACTION 'SMOD' AND SKIP FIRST   SCREEN.

      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.                    "on_link_click
*
ENDCLASS.                    "lcl_events IMPLEMENTATION
DATA: gr_event_handler TYPE REF TO lcl_events.


"Main
"-------------------------------------------------

START-OF-SELECTION.

  PERFORM init_globals.

  "Table with SAP Tcode
  SELECT * FROM tstc INTO TABLE @t_tstc WHERE tcode IN @s_tcode[].
  IF sy-subrc EQ 0.

    "Donwload main programs list
    PERFORM download_prog_catalog.

    "Download function group programs list
    PERFORM download_fugr_catalog.

    CASE 'X'.
      WHEN rb_exit.

        "Download list of SAP enhancement
        PERFORM download_smod_catalog.

        "Build Output List with User Exit
        PERFORM build_exit_list.

        "Print Output
        PERFORM print_list TABLES t_smod_out[].

      WHEN rb_badi.

        "Download list of SAP Badi
        PERFORM download_sxsd_catalog.

        "Build Output List with Badi
        PERFORM build_badi_list.

        "Print Output
        PERFORM print_list TABLES t_sxsd_out[].

      WHEN OTHERS.
    ENDCASE.

  ENDIF.


  "Form Implementation
  "-------------------------------------------------
*&---------------------------------------------------------------------*
*& Form PRINT_LIST
*&---------------------------------------------------------------------*
FORM print_list  TABLES xt_table.

  DATA: go_alv   TYPE REF TO cl_salv_table,
        lv_title TYPE lvc_title,
        lv_lines TYPE i.

  "-------------------------------------------------

  lv_lines = lines( xt_table[] ).
  lv_title = sy-title && ' ( ' && lv_lines && ' Record' && ' ) '.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = go_alv
        CHANGING
          t_table      = xt_table[] ).

    CATCH cx_salv_msg.
  ENDTRY.

  DATA lr_columns TYPE REF TO cl_salv_columns_table.
  lr_columns = go_alv->get_columns( ).
  lr_columns->set_optimize( 'X' ). "--> Optimise all columns

  DATA lr_column TYPE REF TO cl_salv_column_table.
  TRY.
      lr_column ?= lr_columns->get_column( 'MANDT' ).
      lr_column->set_visible( value  = if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found.
  ENDTRY.

  CASE 'X'.
    WHEN rb_exit.

      TRY.
          lr_column ?= lr_columns->get_column( 'NAME' ).
          lr_column->set_cell_type( value = if_salv_c_cell_type=>hotspot ).

          DATA: lo_events TYPE REF TO cl_salv_events_table.
          lo_events = go_alv->get_event( ).

          CREATE OBJECT gr_event_handler. "type ref to lcl_events.
          SET HANDLER gr_event_handler->on_link_click FOR lo_events.

        CATCH cx_salv_not_found.
      ENDTRY.

    WHEN rb_badi.



  ENDCASE.

  DATA: lr_functions TYPE REF TO cl_salv_functions_list.
  lr_functions = go_alv->get_functions( ).
  lr_functions->set_all( 'X' ).

  DATA: lr_display   TYPE REF TO cl_salv_display_settings.
  lr_display = go_alv->get_display_settings( ).
  lr_display->set_striped_pattern( cl_salv_display_settings=>true ).
  lr_display->set_list_header( lv_title ).

  DATA: lr_selections TYPE REF TO cl_salv_selections.
  lr_selections = go_alv->get_selections( ).
  lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

*  IF x_popup EQ abap_true.
*
*    go_alv->set_screen_popup(
*      start_column = 1
*      end_column  = 100
*      start_line  = 1
*      end_line    = 15 ).
*
*  ENDIF.

  go_alv->display( ).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form INIT_GLOBALS
*&---------------------------------------------------------------------*
FORM init_globals .

  REFRESH: t_tstc     ,
           t_tadir    ,
           t_tfdir    ,
           t_enlfdir  ,
           t_smod     ,
           t_sxsd     ,
           t_sxsd_attr,
           t_badi_impl,
           t_modsap   .

  REFRESH: t_smod_out,
           t_sxsd_out.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DOWNLOAD_PROG_CATALOG
*&---------------------------------------------------------------------*
FORM download_prog_catalog .

  "Table with objects catalog repository
  SELECT *
    FROM tadir
    APPENDING TABLE @t_tadir
    FOR ALL ENTRIES IN @t_tstc
      WHERE obj_name EQ @t_tstc-pgmna
        AND pgmid    EQ 'R3TR'
        AND object   EQ 'PROG'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DOWNLOAD_FUGR_CATALOG
*&---------------------------------------------------------------------*
FORM download_fugr_catalog .

  "Table with Function Module Name
  SELECT pname, funcname
     FROM tfdir
     INTO TABLE @t_tfdir
     FOR ALL ENTRIES IN @t_tstc
  WHERE pname EQ @t_tstc-pgmna.
  CHECK sy-subrc EQ 0.

  SELECT funcname, area
    FROM enlfdir
    INTO TABLE @t_enlfdir
    FOR ALL ENTRIES IN @t_tfdir
  WHERE funcname EQ @t_tfdir-funcname.
  CHECK sy-subrc EQ 0.

  LOOP AT t_enlfdir ASSIGNING FIELD-SYMBOL(<enlfdir>).
    <enlfdir>-area_obj_name = <enlfdir>-area.
  ENDLOOP.

  SELECT *
    FROM tadir
    APPENDING TABLE @t_tadir
    FOR ALL ENTRIES IN @t_enlfdir
    WHERE obj_name EQ @t_enlfdir-area_obj_name
      AND pgmid    EQ 'R3TR'
      AND object   EQ 'FUGR'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DOWNLOAD_SMOD_CATALOG
*&---------------------------------------------------------------------*
FORM download_smod_catalog .

  DATA(lt_devclass) = t_tadir.

  SORT lt_devclass BY devclass.
  DELETE ADJACENT DUPLICATES FROM lt_devclass COMPARING devclass.

  CHECK lt_devclass[] IS NOT INITIAL.

  SELECT *
    FROM tadir
    INTO TABLE @t_smod
    FOR ALL ENTRIES IN @lt_devclass
    WHERE devclass EQ @lt_devclass-devclass
      AND pgmid    EQ 'R3TR'
      AND object   EQ 'SMOD' .
  CHECK sy-subrc EQ 0.

  LOOP AT t_smod ASSIGNING FIELD-SYMBOL(<smod>).
    <smod>-modsap_name = <smod>-obj_name.
  ENDLOOP.

  SELECT modsap~*, modsapt~modtext
    FROM modsap AS modsap
    INNER JOIN modsapt AS modsapt ON modsapt~name EQ modsap~name
    INTO TABLE @t_modsap
    FOR ALL ENTRIES IN @t_smod
    WHERE modsap~name   EQ @t_smod-modsap_name
  AND modsapt~sprsl EQ @sy-langu.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_EXIT_LIST
*&---------------------------------------------------------------------*
FORM build_exit_list .

  SORT t_tstc    BY tcode.

  SORT t_tfdir   BY pname.
  SORT t_enlfdir BY funcname.

  SORT t_tadir   BY obj_name
                    pgmid
                    object   .

  SORT t_smod    BY devclass
                    pgmid
                    object   .

  SORT t_modsap  BY name.

  "-------------------------------------------------

  LOOP AT t_tstc ASSIGNING FIELD-SYMBOL(<tstc>).


    READ TABLE t_tadir ASSIGNING FIELD-SYMBOL(<tadir>)
      WITH KEY obj_name = <tstc>-pgmna
               pgmid    = 'R3TR'
               object   = 'PROG'
    BINARY SEARCH.
    IF sy-subrc <> 0.

      READ TABLE t_tfdir ASSIGNING FIELD-SYMBOL(<tfdir>)
        WITH KEY pname = <tstc>-pgmna
        BINARY SEARCH.
      CHECK sy-subrc EQ 0.

      READ TABLE t_enlfdir ASSIGNING FIELD-SYMBOL(<enlfdir>)
        WITH KEY funcname = <tfdir>-funcname
        BINARY SEARCH.
      CHECK sy-subrc EQ 0.

      READ TABLE t_tadir ASSIGNING <tadir>
        WITH KEY obj_name = <enlfdir>-area_obj_name
                 pgmid    = 'R3TR'
                 object   = 'FUGR'
      BINARY SEARCH.
      CHECK sy-subrc EQ 0.

    ENDIF.

    "-------------------------------------------------

    READ TABLE t_smod BINARY SEARCH TRANSPORTING NO FIELDS
      WITH KEY devclass = <tadir>-devclass
               pgmid    = 'R3TR'
               object   = 'SMOD'.

    CHECK sy-subrc EQ 0.

    LOOP AT t_smod ASSIGNING FIELD-SYMBOL(<smod>) FROM sy-tabix.
      IF <smod>-devclass NE <tadir>-devclass.
        EXIT.
      ENDIF.

      READ TABLE t_modsap ASSIGNING FIELD-SYMBOL(<modsap>)
        WITH KEY name = <smod>-modsap_name
        BINARY SEARCH.
      CHECK sy-subrc EQ 0.

      APPEND INITIAL LINE TO t_smod_out ASSIGNING FIELD-SYMBOL(<smod_out>).
      <smod_out>-tcode   = <tstc>-tcode.
      <smod_out>-name    = <modsap>-name.
      <smod_out>-typ     = <modsap>-typ.
      <smod_out>-member  = <modsap>-member.
      <smod_out>-modtext = <modsap>-modtext.

    ENDLOOP.

  ENDLOOP.


  SORT t_smod_out BY tcode name.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DOWNLOAD_SXSD_CATALOG
*&---------------------------------------------------------------------*
FORM download_sxsd_catalog .

  DATA(lt_devclass) = t_tadir.

  SORT lt_devclass BY devclass.
  DELETE ADJACENT DUPLICATES FROM lt_devclass COMPARING devclass.

  CHECK lt_devclass[] IS NOT INITIAL.

  SELECT *
    FROM tadir
    INTO TABLE @t_sxsd
    FOR ALL ENTRIES IN @lt_devclass
    WHERE devclass EQ @lt_devclass-devclass
      AND pgmid    EQ 'R3TR'
      AND object   EQ 'SXSD' .
  CHECK sy-subrc EQ 0.

  LOOP AT t_sxsd ASSIGNING FIELD-SYMBOL(<sxsd>).
    <sxsd>-exit_name = <sxsd>-obj_name.
    <sxsd>-badi_name = <sxsd>-obj_name.
  ENDLOOP.

  SELECT sxs_attr~*, sxs_attrt~text
    FROM sxs_attr AS sxs_attr
    LEFT OUTER JOIN sxs_attrt AS sxs_attrt ON sxs_attrt~exit_name EQ sxs_attr~exit_name
    INTO TABLE @t_sxsd_attr
    FOR ALL ENTRIES IN @t_sxsd
    WHERE sxs_attr~exit_name EQ @t_sxsd-exit_name
      AND sxs_attr~internal  EQ @space
      AND sxs_attrt~sprsl    EQ @sy-langu.

  SELECT *
    FROM badi_impl
    INTO TABLE @t_badi_impl
    FOR ALL ENTRIES IN @t_sxsd
    WHERE badi_name EQ @t_sxsd-badi_name.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_BADI_LIST
*&---------------------------------------------------------------------*
FORM build_badi_list .

  SORT t_tstc      BY tcode.

  SORT t_tfdir     BY pname.

  SORT t_enlfdir   BY funcname.

  SORT t_tadir     BY obj_name
                      pgmid
                      object   .

  SORT t_sxsd      BY devclass .

  SORT t_sxsd_attr BY exit_name.

  SORT t_badi_impl BY badi_name pos.

  "-------------------------------------------------

  LOOP AT t_tstc ASSIGNING FIELD-SYMBOL(<tstc>).


    READ TABLE t_tadir ASSIGNING FIELD-SYMBOL(<tadir>)
      WITH KEY obj_name = <tstc>-pgmna
               pgmid    = 'R3TR'
               object   = 'PROG'
    BINARY SEARCH.
    IF sy-subrc <> 0.

      READ TABLE t_tfdir ASSIGNING FIELD-SYMBOL(<tfdir>)
        WITH KEY pname = <tstc>-pgmna
        BINARY SEARCH.
      CHECK sy-subrc EQ 0.

      READ TABLE t_enlfdir ASSIGNING FIELD-SYMBOL(<enlfdir>)
        WITH KEY funcname = <tfdir>-funcname
        BINARY SEARCH.
      CHECK sy-subrc EQ 0.

      READ TABLE t_tadir ASSIGNING <tadir>
        WITH KEY obj_name = <enlfdir>-area_obj_name
                 pgmid    = 'R3TR'
                 object   = 'FUGR'
      BINARY SEARCH.
      CHECK sy-subrc EQ 0.

    ENDIF.

    "-------------------------------------------------

    READ TABLE t_sxsd BINARY SEARCH TRANSPORTING NO FIELDS
      WITH KEY devclass = <tadir>-devclass
               pgmid    = 'R3TR'
               object   = 'SXSD'.

    CHECK sy-subrc EQ 0.

    LOOP AT t_sxsd ASSIGNING FIELD-SYMBOL(<sxsd>) FROM sy-tabix.
      IF <sxsd>-devclass NE <tadir>-devclass.
        EXIT.
      ENDIF.

      READ TABLE t_sxsd_attr ASSIGNING FIELD-SYMBOL(<sxsd_attr>)
        WITH KEY exit_name = <sxsd>-exit_name
        BINARY SEARCH.
      CHECK sy-subrc EQ 0.

      APPEND INITIAL LINE TO t_sxsd_out ASSIGNING FIELD-SYMBOL(<sxsd_out>).
      <sxsd_out>-tcode    = <tstc>-tcode.
      <sxsd_out>-name     = <sxsd>-exit_name.
      <sxsd_out>-text     = <sxsd_attr>-text.
      <sxsd_out>-mltp_use = <sxsd_attr>-mltp_use.

      "TODO
*      READ TABLE t_badi_impl ASSIGNING FIELD-SYMBOL(<badi_impl>)
*        WITH KEY badi_name = <sxsd>-badi_name
*        BINARY SEARCH.
*      IF sy-subrc <> 0.
*
*        CONTINUE.
*      ENDIF.



    ENDLOOP.


  ENDLOOP.

ENDFORM.
