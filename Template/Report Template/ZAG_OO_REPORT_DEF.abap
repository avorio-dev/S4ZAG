*&---------------------------------------------------------------------*
*& Include          ZAG_OO_REPORT_DEF
*&---------------------------------------------------------------------*
INTERFACE lif_common_types.

  TYPES: BEGIN OF ts_output,
           partner TYPE but000-partner,
           type    TYPE but000-type,
           bpkind  TYPE but000-bpkind,
         END OF ts_output.

  TYPES: tt_output TYPE TABLE OF ts_output WITH DEFAULT KEY.

  TYPES: BEGIN OF ts_selopt,
           bukrs   TYPE RANGE OF t001-bukrs,
           partner TYPE RANGE OF but000-partner,
         END OF ts_selopt.

ENDINTERFACE.

**********************************************************************

CLASS lcx_generic DEFINITION INHERITING FROM cx_static_check FINAL.

  PUBLIC SECTION.
    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .

    CONSTANTS:
      BEGIN OF generic_fault,
        msgid TYPE symsgid      VALUE 'DB',
        msgno TYPE symsgno      VALUE '646',
        attr1 TYPE scx_attrname VALUE 'GV_ATTR1',
        attr2 TYPE scx_attrname VALUE 'GV_ATTR2',
        attr3 TYPE scx_attrname VALUE 'GV_ATTR3',
        attr4 TYPE scx_attrname VALUE 'GV_ATTR4',
      END OF generic_fault.

    DATA: gv_attr1 TYPE string,
          gv_attr2 TYPE string,
          gv_attr3 TYPE string,
          gv_attr4 TYPE string.

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !xv_attr1 TYPE string OPTIONAL
        !xv_attr2 TYPE string OPTIONAL
        !xv_attr3 TYPE string OPTIONAL
        !xv_attr4 TYPE string OPTIONAL.

ENDCLASS.

**********************************************************************

CLASS lcx_selscreen DEFINITION INHERITING FROM cx_static_check FINAL.

  PUBLIC SECTION.
    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .

    CONSTANTS:
      BEGIN OF missing_param,
        msgid TYPE symsgid      VALUE 'DB',
        msgno TYPE symsgno      VALUE '646',
        attr1 TYPE scx_attrname VALUE 'GV_MISSING_PARAM',
        attr2 TYPE scx_attrname VALUE 'GV_ATTR1',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF missing_param .

    DATA gv_missing_param TYPE string VALUE 'Missing Parameter: ' ##NO_TEXT.


    DATA: gv_attr1 TYPE string,
          gv_attr2 TYPE string,
          gv_attr3 TYPE string,
          gv_attr4 TYPE string.

    METHODS constructor
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
    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .

    CONSTANTS:
      BEGIN OF no_rec_found,                        "#EC "#EC CI_NAMING
        msgid TYPE symsgid      VALUE 'DB',
        msgno TYPE symsgno      VALUE '646',
        attr1 TYPE scx_attrname VALUE 'GV_NO_REC_FOUND',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_rec_found.

    DATA gv_no_rec_found TYPE string VALUE 'No Records Found' ##NO_TEXT.


    DATA: gv_attr1 TYPE string,
          gv_attr2 TYPE string,
          gv_attr3 TYPE string,
          gv_attr4 TYPE string.

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !xv_attr1 TYPE string OPTIONAL
        !xv_attr2 TYPE string OPTIONAL
        !xv_attr3 TYPE string OPTIONAL
        !xv_attr4 TYPE string OPTIONAL.             "#EC "#EC CI_NAMING

ENDCLASS.

**********************************************************************

CLASS lcl_selection_screen DEFINITION.
  PUBLIC SECTION.
    INTERFACES:
      lif_common_types.

    ALIASES:
      ts_selopt FOR lif_common_types~ts_selopt.

    METHODS:
      set_params
        IMPORTING
          !xr_partner TYPE ts_selopt-partner OPTIONAL
          !xr_bukrs   TYPE ts_selopt-bukrs   OPTIONAL,

      get_params
        EXPORTING
          !yr_partner TYPE ts_selopt-partner
          !yr_bukrs   TYPE ts_selopt-bukrs,

      check_params
        RAISING lcx_selscreen.

  PRIVATE SECTION.
    DATA: gr_partner TYPE ts_selopt-partner,
          gr_bukrs   TYPE ts_selopt-bukrs.

ENDCLASS.

**********************************************************************

CLASS lcl_data_processor DEFINITION.
  PUBLIC SECTION.
    INTERFACES:
      lif_common_types.

    ALIASES: ts_output FOR lif_common_types~ts_output,
             tt_output FOR lif_common_types~tt_output,
             ts_selopt FOR lif_common_types~ts_selopt.

    METHODS:
      main
        IMPORTING xo_selection_screen TYPE REF TO lcl_selection_screen
        RAISING   lcx_generic
                  lcx_processor,

      select_data
        RAISING lcx_processor,

      build_output
        RETURNING VALUE(yt_output) TYPE tt_output
        RAISING   lcx_processor.


  PRIVATE SECTION.

    DATA: gr_partner TYPE ts_selopt-partner,
          gr_bukrs   TYPE ts_selopt-bukrs,

          gt_partner TYPE TABLE OF but000,
          gt_output  TYPE tt_output.

ENDCLASS.

**********************************************************************

CLASS lcl_display DEFINITION.
  PUBLIC SECTION.
    INTERFACES:
      lif_common_types.

    ALIASES: ts_output FOR lif_common_types~ts_output,
             tt_output FOR lif_common_types~tt_output.

    METHODS:
      constructor
        IMPORTING
          !xt_output TYPE tt_output,

      display_data
        RAISING lcx_generic.

  PRIVATE SECTION.
    DATA: gt_output TYPE tt_output.

ENDCLASS.
