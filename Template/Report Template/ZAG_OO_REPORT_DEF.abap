*&---------------------------------------------------------------------*
*& Include          ZAG_OO_REPORT_DEF
*&---------------------------------------------------------------------*
INTERFACE lif_common_types.

  " Types
  "---------------------------------------------------------------
  TYPES: "Structures
    BEGIN OF ts_params,
      budat TYPE sy-datum,
    END OF ts_params,

    BEGIN OF ts_selopt,
      lifnr TYPE RANGE OF lfa1-lifnr,
      bukrs TYPE RANGE OF t001-bukrs,
    END OF ts_selopt,

    BEGIN OF ts_output,
      lifnr TYPE lfa1-lifnr,
      name1 TYPE lfa1-name1,
      name2 TYPE lfa1-name2,
      name3 TYPE lfa1-name3,
      name4 TYPE lfa1-name4,
      bukrs TYPE lfb1-bukrs,
      loevm TYPE lfb1-loevm ,
    END OF ts_output.


  TYPES: "Table Types
    tt_output TYPE TABLE OF ts_output WITH DEFAULT KEY.


ENDINTERFACE.

**********************************************************************

CLASS lcx_selscreen DEFINITION INHERITING FROM cx_static_check FINAL.

  PUBLIC SECTION.

    " Interfaces
    "-------------------------------------------------
    INTERFACES:
      if_t100_dyn_msg,
      if_t100_message.


    " Constants
    "-------------------------------------------------
    CONSTANTS:
      BEGIN OF missing_param,
        msgid TYPE symsgid      VALUE 'DB',
        msgno TYPE symsgno      VALUE '646',
        attr1 TYPE scx_attrname VALUE 'GV_MISSING_PARAM',
        attr2 TYPE scx_attrname VALUE 'GV_ATTR1',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF missing_param .


    " Data
    "-------------------------------------------------
    DATA:
      gv_attr1         TYPE string,
      gv_attr2         TYPE string,
      gv_attr3         TYPE string,
      gv_attr4         TYPE string,

      gv_missing_param TYPE string VALUE 'Missing Parameter: ' ##NO_TEXT.


    " Methods
    "-------------------------------------------------
    METHODS:
      constructor
        IMPORTING
          !textid   LIKE if_t100_message=>t100key OPTIONAL
          !xv_attr1 TYPE string OPTIONAL
          !xv_attr2 TYPE string OPTIONAL
          !xv_attr3 TYPE string OPTIONAL
          !xv_attr4 TYPE string OPTIONAL.

ENDCLASS.

**********************************************************************

CLASS lcx_processor DEFINITION INHERITING FROM cx_static_check FINAL.

  PUBLIC SECTION.

    " Interfaces
    "-------------------------------------------------
    INTERFACES:
      if_t100_dyn_msg,
      if_t100_message.


    " Constants
    "-------------------------------------------------
    CONSTANTS:
      BEGIN OF generic_fault,
        msgid TYPE symsgid      VALUE 'DB',
        msgno TYPE symsgno      VALUE '646',
        attr1 TYPE scx_attrname VALUE 'GV_ATTR1',
        attr2 TYPE scx_attrname VALUE 'GV_ATTR2',
        attr3 TYPE scx_attrname VALUE 'GV_ATTR3',
        attr4 TYPE scx_attrname VALUE 'GV_ATTR4',
      END OF generic_fault,

      BEGIN OF no_rec_found,                        "#EC "#EC CI_NAMING
        msgid TYPE symsgid      VALUE 'DB',
        msgno TYPE symsgno      VALUE '646',
        attr1 TYPE scx_attrname VALUE 'GV_NO_REC_FOUND',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_rec_found.


    " Data
    "-------------------------------------------------
    DATA:
      gv_attr1        TYPE string,
      gv_attr2        TYPE string,
      gv_attr3        TYPE string,
      gv_attr4        TYPE string,

      gv_no_rec_found TYPE string VALUE 'No Records Found' ##NO_TEXT.


    " Methods
    "-------------------------------------------------
    METHODS:
      constructor
        IMPORTING
          !textid   LIKE if_t100_message=>t100key OPTIONAL
          !xv_attr1 TYPE string OPTIONAL
          !xv_attr2 TYPE string OPTIONAL
          !xv_attr3 TYPE string OPTIONAL
          !xv_attr4 TYPE string OPTIONAL.           "#EC "#EC CI_NAMING

ENDCLASS.

**********************************************************************

CLASS lcl_selection_screen DEFINITION.

  PUBLIC SECTION.

    " Interfaces
    "-------------------------------------------------
    INTERFACES:
      lif_common_types.


    " Aliases
    "-------------------------------------------------
    ALIASES:
      ts_params FOR lif_common_types~ts_params,
      ts_selopt FOR lif_common_types~ts_selopt.


    " Methods
    "-------------------------------------------------
    METHODS:
      get_params
        EXPORTING
          !ys_params TYPE ts_params
          !ys_selopt TYPE ts_selopt,

      set_params
        IMPORTING
                  !xs_params TYPE ts_params OPTIONAL
                  !xs_selopt TYPE ts_selopt OPTIONAL
        RAISING   lcx_selscreen,

      check_params
        RAISING lcx_selscreen.


  PRIVATE SECTION.

    " Data
    "---------------------------------------------------------------
    DATA:
      gs_params TYPE ts_params,
      gs_selopt TYPE ts_selopt.

ENDCLASS.

**********************************************************************

CLASS lcl_display DEFINITION.

  PUBLIC SECTION.

    " Interfaces
    "-------------------------------------------------
    INTERFACES:
      lif_common_types.


    " Aliases
    "-------------------------------------------------
    ALIASES:
      ts_output FOR lif_common_types~ts_output,
      tt_output FOR lif_common_types~tt_output.


    " Methods
    "-------------------------------------------------
    METHODS:
      constructor
        IMPORTING
          !xt_output TYPE tt_output,

      display_data
        RAISING lcx_processor.

  PRIVATE SECTION.

    " Data
    "-------------------------------------------------
    DATA:
      gt_output TYPE tt_output.

ENDCLASS.

**********************************************************************

CLASS lcl_data_processor DEFINITION.

  PUBLIC SECTION.

    " Interfaces
    "-------------------------------------------------
    INTERFACES:
      lif_common_types.


    " Aliases
    "-------------------------------------------------
    ALIASES:
        ts_params FOR lif_common_types~ts_params,
        ts_selopt FOR lif_common_types~ts_selopt,

        ts_output FOR lif_common_types~ts_output,
        tt_output FOR lif_common_types~tt_output.


    " Methods
    "-------------------------------------------------
    METHODS:
      main
        IMPORTING xo_selection_screen TYPE REF TO lcl_selection_screen
        RAISING   lcx_processor,

      select_data
        RAISING lcx_processor,

      build_output
        RAISING lcx_processor,

      get_output
        RETURNING VALUE(yt_output) TYPE tt_output.


  PRIVATE SECTION.

    " Data
    "-------------------------------------------------
    DATA: gs_params TYPE ts_params,
          gs_selopt TYPE ts_selopt,

          gt_output TYPE tt_output,

          gt_lfa1   TYPE SORTED TABLE OF lfa1 WITH NON-UNIQUE KEY lifnr,
          gt_lfb1   TYPE SORTED TABLE OF lfb1 WITH NON-UNIQUE KEY lifnr bukrs.

ENDCLASS.
