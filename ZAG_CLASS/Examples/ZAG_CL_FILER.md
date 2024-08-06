# ZAG_CL_FILER <a name="zag_cl_filer"></a>
 - FILE_DOWNLOAD
    - You will be able to download .csv or .xlsx both on your local system or application server

 - FILE_UPLOAD
    - You will be able to upload .csv from your local system or application server
    - You will be able to upload .xlsx from your local system
    - The system will provide you a table with conversion errors
---

```abap
  "Example 1 -> Download .CSV
  "-------------------------------------------------

  SELECT * UP TO 10 ROWS FROM caufv INTO TABLE @DATA(lt_caufv).

  DATA(lv_filename) = |{ zag_cl_filer=>get_desktop_directory( ) }/zag_file.csv|.
  DATA(lo_filer)    = NEW zag_cl_filer( ).

  lo_filer->file_download(
    EXPORTING
      xv_filename               = lv_filename
      xt_sap_table              = lt_caufv
      xv_source                 = zag_cl_filer=>tc_file_source-local
      xv_header                 = abap_true
    EXCEPTIONS
      not_supported_file        = 1
      unable_define_structdescr = 2
      unable_open_path          = 3
      OTHERS                    = 4
  ).

```

---

```abap
  "Example 2 -> Upload .CSV
  "-------------------------------------------------

  SELECT * UP TO 10 ROWS FROM caufv INTO TABLE @DATA(lt_caufv).

  DATA(lv_filename)     = |{ zag_cl_filer=>get_desktop_directory( ) }/zag_file.csv|.
  DATA(lo_filer)     = NEW zag_cl_filer( ).

  lo_filer->file_upload(
    EXPORTING
      xv_filename               = lv_filename
      xv_source                 = zag_cl_filer=>tc_file_source-local
      xv_header                 = abap_true
    IMPORTING
      yt_conversions_errors     = DATA(lt_conv_error)
    CHANGING
      yt_sap_data               = lt_caufv
    EXCEPTIONS
      not_supported_file        = 1
      unable_define_structdescr = 2
      input_error               = 3
      unable_open_path          = 4
      empty_file                = 5
      OTHERS                    = 6
  ).

```

---

```abap
"Example 3 -> Download / Upload .CSV
"             with User exit conversion methods
"-------------------------------------------------

CLASS lcl_custom_filer DEFINITION
  INHERITING FROM zag_cl_filer.

  PROTECTED SECTION.
    METHODS:
      pre_struct_to_ext REDEFINITION,
      post_struct_to_ext REDEFINITION,
      pre_struct_to_int REDEFINITION,
      post_struct_to_int REDEFINITION.


ENDCLASS.

CLASS lcl_custom_filer IMPLEMENTATION.

  METHOD pre_struct_to_ext.

*        IMPORTING
*          xs_fcat            TYPE lvc_s_fcat
*          xs_sap_data        TYPE any
*          xv_value_pre_conv  TYPE any
*        CHANGING
*          yv_value_post_conv TYPE string

  ENDMETHOD.

  METHOD post_struct_to_ext.

*        IMPORTING
*          xs_fcat            TYPE lvc_s_fcat
*          xs_sap_data        TYPE any
*          xv_value_pre_conv  TYPE any
*        CHANGING
*          yv_value_post_conv TYPE string

  ENDMETHOD.

  METHOD pre_struct_to_int.

*        IMPORTING
*          xs_fcat           TYPE lvc_s_fcat
*        CHANGING
*          yv_value_pre_conv TYPE string

  ENDMETHOD.

  METHOD post_struct_to_int.

*        IMPORTING
*          xs_fcat            TYPE lvc_s_fcat
*          xv_value_pre_conv  TYPE string
*        CHANGING
*          yv_value_post_conv TYPE string

  ENDMETHOD.

ENDCLASS.


START-OF-SELECTION.

  SELECT * UP TO 10 ROWS FROM caufv INTO TABLE @DATA(lt_caufv).

  DATA(lv_filename) = |{ zag_cl_filer=>get_desktop_directory( ) }/zag_file.csv|.
  DATA(lo_filer)    = NEW lcl_custom_filer( ).


  lo_filer->file_download(
    EXPORTING
      xv_filename               = lv_filename
      xt_sap_table              = lt_caufv
      xv_source                 = zag_cl_filer=>tc_file_source-local
      xv_header                 = abap_true
    EXCEPTIONS
      not_supported_file        = 1
      unable_define_structdescr = 2
      unable_open_path          = 3
      OTHERS                    = 4
  ).


  CLEAR lt_caufv[].
  lo_filer->file_upload(
    EXPORTING
      xv_filename               = lv_filename
      xv_source                 = zag_cl_filer=>tc_file_source-local
      xv_header                 = abap_true
    IMPORTING
      yt_conversions_errors     = DATA(lt_conv_error)
    CHANGING
      yt_sap_data               = lt_caufv
    EXCEPTIONS
      not_supported_file        = 1
      unable_define_structdescr = 2
      input_error               = 3
      unable_open_path          = 4
      empty_file                = 5
      OTHERS                    = 6
  ).
```