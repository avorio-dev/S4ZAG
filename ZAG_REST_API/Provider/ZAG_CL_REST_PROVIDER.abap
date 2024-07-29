CLASS zag_cl_rest_provider DEFINITION
  INHERITING FROM cl_rest_resource
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      if_rest_resource~get REDEFINITION,
      if_rest_resource~post REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zag_cl_rest_provider IMPLEMENTATION.

  METHOD if_rest_resource~get.

  ENDMETHOD.

  METHOD if_rest_resource~post.

  ENDMETHOD.

ENDCLASS.