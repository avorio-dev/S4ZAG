CLASS zag_amdp_demo_001 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "Declare the following interface to use AMDP Method
    INTERFACES:
      if_amdp_marker_hdb,
      if_oo_adt_classrun.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    TYPES: BEGIN OF ty_result,
             matnr TYPE mara-matnr,
             matkl TYPE mara-matkl,
             maktx TYPE char100,
             spras TYPE spras,
           END OF ty_result,
           tt_result TYPE TABLE OF ty_result.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    CLASS-METHODS:
      get_data_with_params
        IMPORTING
          VALUE(xp_client) TYPE sy-mandt
          VALUE(xp_matnr)  TYPE mara-matnr
          VALUE(xp_spras)  TYPE makt-spras DEFAULT 'E'
        EXPORTING
          VALUE(yt_mara)   TYPE tt_result,

      get_data_with_selopt
        IMPORTING
          VALUE(xp_client)     TYPE sy-mandt
          VALUE(xp_where_cond) TYPE string
        EXPORTING
          VALUE(yt_mara)       TYPE tt_result.


  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zag_amdp_demo_001 IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

  ENDMETHOD.

  METHOD get_data_with_params
    BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT
    OPTIONS READ-ONLY
    USING mara makt.

    -- 1. BY DATABASE PROCEDURE to implement a database procedure. In this option,
    --  your AMDP method will automatically create a procedure in hana systerm
    -- 2. FOR HDB to indicate hana database
    -- 3. LANGUAGE SQLSCRIPT to indicate the database-specific language in which AMDP is implemented
    -- 4. OPTION READ-ONLY indicates we can only read in the database procedure
    -- 5. USING <name of table/view>: If you use tables in this procedure, please give name of table in this option

    --""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


    -- inside AMDP method, we will fetch data from mara and makt (declare previously at USING statement)
    -- with the specific conditions which passed by parameter in class definition before, nchar( 32 ) same as a space in ascii characters.
    -- I want to concat material Code and material description and seperate between two value by space, so I will use nchar( 32 )

    -- because AMDP cannot handle client, so we need pass value mandt at where clause to get exactly value in this system

    -- If you want create a variable inside AMDP method, you can use declare to create. inside AMDP, you can create both SQL Type and Type in SAP.
    -- With using built-in function LTRIM, I will remove leading zero before material Code (if have any)

    declare lv_test nvarchar( 5 );
    lv_test := 'TEST_';

    yt_mara =
        SELECT mara.matnr,
               mara.matkl,
               ( :lv_test || ltrim(mara.matnr, '0') || nchar( 32 ) || makt.maktx ) AS maktx,
               makt.spras
            FROM mara AS mara
            INNER JOIN makt AS makt ON makt.mandt = mara.mandt
                                    AND makt.matnr = mara.matnr
            WHERE mara.matnr = :xp_matnr
              AND makt.spras = :xp_spras;

  ENDMETHOD.

  METHOD get_data_with_selopt
    BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT
    OPTIONS READ-ONLY
    USING mara makt.

    declare lv_test nvarchar( 5 );
    lv_test := 'TEST_';


    lt_mara_filtered = apply_filter ( mara, :xp_where_cond );

    yt_mara =
        SELECT mara.matnr,
               mara.matkl,
               ( :lv_test || ltrim(mara.matnr, '0') || nchar( 32 ) || makt.maktx ) AS maktx,
               makt.spras
          from :lt_mara_filtered as mara
            inner join makt as makt on makt.mandt = mara.mandt
                                     AND makt.matnr = mara.matnr;

  ENDMETHOD.

ENDCLASS.