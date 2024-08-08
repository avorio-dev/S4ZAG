# ZAG_CL_CONVERTER <a name="zag_cl_converter"></a>
- CONV_TSAP_TO_EXT
    - It allows to create a string table from your SAP Table, performing autaomatically standard conversion exit and conversion from SAP Format to Excel Format

- CONV_TSAP_TO_INT
    - It allows to create a SAP table from your String Table, performing autaomatically standard conversion exit and conversion from Excel Format to SAP Format

- PRE/POST _ STRUCT_TO_ EXT/INT
    - Redefine this method to apply your custom logic

---

```abap

"Example 1 -> Convert SAP Table to String Table
"-------------------------------------------------

SELECT * UP TO 10 ROWS FROM caufv INTO TABLE @DATA(lt_caufv).

DATA(lo_conv) = NEW zag_cl_converter( ).

DATA(lt_tsap_ext) = lo_conv->conv_tsap_to_ext(
    xt_tsap_int  = lt_caufv
).

"Do something with LT_TSAP_EXT Like using it as mail attachment

```

---

```abap

"Example 2 -> Convert String Table to SAP Table
"-------------------------------------------------

DATA:
  lt_caufv    TYPE TABLE OF caufv,
  lt_tsap_ext TYPE string_table,
  lt_conv_err TYPE zag_cl_converter=>tt_conversions_errors.

DATA(lo_conv) = NEW zag_cl_converter( ).
lo_conv->conv_tsap_to_int(
  EXPORTING
    xt_tsap_ext           = lt_tsap_ext
  CHANGING
    yt_tsap_int           = lt_caufv[]
    yt_conversions_errors = lt_conv_err[]
).

DATA(lo_salv) = NEW zag_cl_salv( ).

IF lt_conv_err[] IS NOT INITIAL.
  lo_salv->display_generic_alv( lt_conv_err[] ).

ELSE.
  lo_salv->display_generic_alv( lt_caufv[] ).

ENDIF.

```
---

```abap

"Example 3 -> Conversion with custom logic
"-------------------------------------------------

CLASS lcl_custom_converter DEFINITION
  INHERITING FROM zag_cl_converter.

  PROTECTED SECTION.
    METHODS:
      pre_struct_to_ext REDEFINITION,
      post_struct_to_ext REDEFINITION.

ENDCLASS.

CLASS lcl_custom_converter IMPLEMENTATION.

  METHOD pre_struct_to_ext.

*      pre_struct_to_ext
*        IMPORTING
*          xs_fcat            TYPE lvc_s_fcat
*          xs_sap_data        TYPE any
*          xv_value_pre_conv  TYPE any
*        CHANGING
*          yv_value_post_conv TYPE string

  ENDMETHOD.

  METHOD post_struct_to_ext.

*      post_struct_to_ext
*        IMPORTING
*          xs_fcat            TYPE lvc_s_fcat
*          xs_sap_data        TYPE any
*          xv_value_pre_conv  TYPE any
*        CHANGING
*          yv_value_post_conv TYPE string

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  SELECT * UP TO 10 ROWS FROM caufv INTO TABLE @DATA(lt_caufv).

  DATA(lo_conv) = NEW lcl_custom_converter( ).

  DATA(lt_tsap_ext) = lo_conv->conv_tsap_to_ext(
      xt_tsap_int  = lt_caufv
  ).

  "Do something with LT_TSAP_EXT Like using it as mail attachment

```