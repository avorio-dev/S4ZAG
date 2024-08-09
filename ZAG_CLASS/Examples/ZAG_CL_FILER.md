# ZAG_CL_FILER <a name="zag_cl_filer"></a>
 - FILE_DOWNLOAD
    - You will be able to download .csv, .txt or .xlsx both on your local system or application server
    - You will be able to download your files into a zip archive

 - FILE_UPLOAD
    - You will be able to upload .csv from your local system or application server
    - You will be able to upload .xlsx from your local system
    - The system will provide you a table with conversion errors
---

```abap
  "Example 1 -> Download .CSV
  "-------------------------------------------------

  SELECT * UP TO 10 ROWS FROM caufv INTO TABLE @DATA(lt_caufv).
  SELECT * UP TO 10 ROWS FROM bseg  INTO TABLE @DATA(lt_bseg).

  DATA(lo_filer)    = NEW zag_cl_filer( ).

  TRY.

      DATA(lt_files) = VALUE zag_cl_filer=>tt_files(
        ( filename = 'caufv.csv'
          sap_content = REF #( lt_caufv )
        )

        ( filename = 'bseg.csv'
          sap_content = REF #( lt_bseg )
        )
      ).

      lo_filer->file_download(
        EXPORTING
          xv_directory = zag_cl_filer=>get_desktop_directory( )
          xv_source    = zag_cl_filer=>tc_file_source-local
        CHANGING
          yt_files     = lt_files[]
      ).
    CATCH cx_ai_system_fault INTO DATA(lx_ai_system_fault). " Application Integration: Technical Error
      WRITE lx_ai_system_fault->get_text( ).
  ENDTRY.

```

---

```abap
  "Example 2 -> Upload .CSV
  "-------------------------------------------------

  DATA: lt_caufv TYPE TABLE OF caufv,
        lt_bseg  TYPE TABLE OF bseg.

  DATA(lo_filer) = NEW zag_cl_filer( ).

  TRY.

      DATA(lt_files) = VALUE zag_cl_filer=>tt_files(
        ( filename = 'caufv.csv'
          sap_content = REF #( lt_caufv )
        )

        ( filename = 'bseg.csv'
          sap_content = REF #( lt_bseg )
        )
      ).

      lo_filer->file_upload(
        EXPORTING
          xv_directory = zag_cl_filer=>get_desktop_directory( )
          xv_source    = zag_cl_filer=>tc_file_source-local
        CHANGING
          yt_files     = lt_files[]
      ).

    CATCH cx_ai_system_fault INTO DATA(lx_ai_system_fault). " Application Integration: Technical Error
      WRITE lx_ai_system_fault->get_text( ).
  ENDTRY.

```

---

```abap

  "Example 3 -> Download .CSV into a ZIP File
  "-------------------------------------------------

  SELECT * UP TO 10 ROWS FROM caufv INTO TABLE @DATA(lt_caufv).
  SELECT * UP TO 10 ROWS FROM bseg  INTO TABLE @DATA(lt_bseg).

  DATA(lo_filer)    = NEW zag_cl_filer( ).

  TRY.

      DATA(lt_files) = VALUE zag_cl_filer=>tt_files(
        ( filename = 'caufv.csv'
          sap_content = REF #( lt_caufv )
        )

        ( filename = 'bseg.csv'
          sap_content = REF #( lt_bseg )
        )
      ).

      lo_filer->file_download(
        EXPORTING
          xv_directory  = zag_cl_filer=>get_desktop_directory( )
          xv_source     = zag_cl_filer=>tc_file_source-local
          xv_create_zip = abap_true
          xv_zip_name   = 'MyZAGZIP.zip'
        CHANGING
          yt_files      = lt_files[]
      ).
    CATCH cx_ai_system_fault INTO DATA(lx_ai_system_fault). " Application Integration: Technical Error
      WRITE lx_ai_system_fault->get_text( ).
  ENDTRY.
```

---

```abap

  "Example 4 -> Upload .CSV from a ZIP File
  "-------------------------------------------------

  DATA: lt_caufv TYPE TABLE OF caufv,
        lt_bseg  TYPE TABLE OF bseg.

  DATA(lo_filer) = NEW zag_cl_filer( ).

  TRY.

      DATA(lt_files) = VALUE zag_cl_filer=>tt_files(
        ( filename = 'caufv.csv'
          sap_content = REF #( lt_caufv )
        )

        ( filename = 'bseg.csv'
          sap_content = REF #( lt_bseg )
        )
      ).

      lo_filer->file_upload(
        EXPORTING
          xv_directory = zag_cl_filer=>get_desktop_directory( )
          xv_source    = zag_cl_filer=>tc_file_source-local
          xv_load_zip  = 'X'
          xv_zip_name  = 'MyZAGZIP.zip'
        CHANGING
          yt_files     = lt_files[]
      ).

    CATCH cx_ai_system_fault INTO DATA(lx_ai_system_fault). " Application Integration: Technical Error
      WRITE lx_ai_system_fault->get_text( ).
  ENDTRY.
```

---


```abap
"Example 5 -> Download / Upload .CSV
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
  SELECT * UP TO 10 ROWS FROM bseg  INTO TABLE @DATA(lt_bseg).

  DATA(lo_filer) = NEW zag_cl_filer( ).

  TRY.

      DATA(lt_files) = VALUE zag_cl_filer=>tt_files(
        ( filename = 'caufv.csv'
          sap_content = REF #( lt_caufv )
        )

        ( filename = 'bseg.csv'
          sap_content = REF #( lt_bseg )
        )
      ).

      lo_filer->file_download(
        EXPORTING
          xv_directory = zag_cl_filer=>get_desktop_directory( )
          xv_source    = zag_cl_filer=>tc_file_source-local
        CHANGING
          yt_files     = lt_files[]
      ).

      lo_filer->file_upload(
        EXPORTING
          xv_directory = zag_cl_filer=>get_desktop_directory( )
          xv_source    = zag_cl_filer=>tc_file_source-local
        CHANGING
          yt_files     = lt_files[]
      ).

    CATCH cx_ai_system_fault INTO DATA(lx_ai_system_fault). " Application Integration: Technical Error
      WRITE lx_ai_system_fault->get_text( ).
  ENDTRY.
```