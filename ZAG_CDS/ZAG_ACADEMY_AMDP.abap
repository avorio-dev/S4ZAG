CLASS zag_academy_amdp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "Declare the following interface to use AMDP Method
    INTERFACES:
      if_amdp_marker_hdb,
      if_oo_adt_classrun.

    CLASS-METHODS: get_zag_tf_01 FOR TABLE FUNCTION zag_tf_01.
    CLASS-METHODS: get_zag_tf_02 FOR TABLE FUNCTION zag_tf_02.


  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zag_academy_amdp IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

  ENDMETHOD.

  METHOD get_zag_tf_01 BY DATABASE FUNCTION FOR HDB LANGUAGE SQLSCRIPT
      OPTIONS READ-ONLY
      USING ekko.

    RETURN SELECT mandt,
                  ebeln,
                  bukrs,
                  bstyp,
                  bsart,
                  loekz,
                  aedat,
                  ernam
        FROM ekko
        WHERE bukrs = p_bukrs
        ORDER BY ebeln;

  endmethod.

  METHOD get_zag_tf_02 BY DATABASE FUNCTION FOR HDB LANGUAGE SQLSCRIPT
      OPTIONS READ-ONLY
      USING ekko.

    RETURN SELECT mandt,
                  ebeln,
                  bukrs,
                  bstyp,
                  bsart,
                  loekz,
                  aedat,
                  ernam
        FROM ekko
        WHERE bukrs = p_bukrs
        ORDER BY ebeln;

  endmethod.

ENDCLASS.