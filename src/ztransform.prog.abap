*&---------------------------------------------------------------------*
*& Report ZTRANSFORM
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztransform.

FIELD-SYMBOLS: <gft_output> TYPE STANDARD TABLE.
FIELD-SYMBOLS: <gfs_output> TYPE any.
DATA: gv_tabix TYPE sy-tabix.

SELECTION-SCREEN BEGIN OF BLOCK b01.
  PARAMETERS: docnum TYPE edidc-docnum DEFAULT '1559006'.
SELECTION-SCREEN END OF BLOCK b01.

CLASS zcl_transform DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS main.
    CLASS-METHODS class_constructor.
  PRIVATE SECTION.
    CLASS-DATA gt_tab TYPE STANDARD TABLE OF ztransform.
    CLASS-METHODS handle_type_tab
      IMPORTING iv_path  TYPE char100 OPTIONAL
                iv_name  TYPE char30 OPTIONAL
                iv_hier  TYPE i OPTIONAL
                iv_field TYPE ANY TABLE OPTIONAL
      CHANGING  cs_struc TYPE any OPTIONAL.
    CLASS-METHODS handle_type_struc
      IMPORTING iv_path  TYPE char100 OPTIONAL
                iv_name  TYPE char30 OPTIONAL
                iv_hier  TYPE i OPTIONAL
                iv_field TYPE any OPTIONAL
      CHANGING  cs_struc TYPE any OPTIONAL.
    CLASS-METHODS handle_type_field
      IMPORTING iv_path  TYPE char100 OPTIONAL
                iv_name  TYPE char30 OPTIONAL
                iv_hier  TYPE i OPTIONAL
                iv_field TYPE any OPTIONAL
      CHANGING  cs_struc TYPE any OPTIONAL.
ENDCLASS.

CLASS zcl_transform IMPLEMENTATION.

  METHOD class_constructor.

    DATA: lr_table TYPE REF TO data,
          lr_struc TYPE REF TO data.

    "Select data from custom table
    SELECT * FROM ztransform INTO TABLE @gt_tab.

    "Create dynamic output table
    DATA(lt_components) = VALUE cl_abap_structdescr=>component_table( FOR ls_tab IN gt_tab ( name = ls_tab-filename type = CAST cl_abap_datadescr( cl_abap_elemdescr=>describe_by_name( ls_tab-rollname ) ) ) ).
    DATA(lo_struc) = cl_abap_structdescr=>create( lt_components ).
    DATA(lo_new_tab) = cl_abap_tabledescr=>create( p_line_type  = lo_struc
                                                   p_table_kind = cl_abap_tabledescr=>tablekind_std
                                                   p_unique     = abap_false ).
    CREATE DATA lr_table TYPE HANDLE lo_new_tab.
    CREATE DATA lr_struc TYPE HANDLE lo_struc.

    ASSIGN lr_table->* TO <gft_output>.

  ENDMETHOD.

  METHOD main.

    DATA:
      ls_edidc       TYPE edidc,
      lt_edidd       TYPE STANDARD TABLE OF edidd,
      lv_guid32      TYPE guid_32,
      lr_source_data TYPE REF TO data,
      lr_inf_det     TYPE REF TO /aif/cl_inf_det_engine_idoc,
      ls_finf        TYPE /aif/t_finf,
      lv_typename    TYPE strname,
      ls_input       TYPE /aif/idoc_control_data_rec.

    FIELD-SYMBOLS:
      <lfs_field> TYPE any.

    "Read IDOC Data
    CALL FUNCTION 'IDOC_READ_COMPLETELY'
      EXPORTING
        document_number         = docnum
      IMPORTING
        idoc_control            = ls_edidc
      TABLES
        int_edidd               = lt_edidd
      EXCEPTIONS
        document_not_exist      = 1
        document_number_invalid = 2
        OTHERS                  = 3.
    IF sy-subrc EQ 0.

      "Transfer to AIF
      ls_edidc-status = '30'.
      lv_guid32 = ls_edidc-docnum.
      /aif/cl_enabler_idoc=>transfer_to_aif( is_idoc_control_rec = ls_edidc iv_do_commit = '' ).

      "Interface Determination
      ls_input-idoc_data =   lt_edidd.
      ls_input-idoc_contrl = ls_edidc.
      TRY.
          lr_inf_det ?= /aif/cl_aif_engine_factory=>get_inf_det_engine(
                  iv_type           = '001'
                  iv_cust_ns        = ''
                  iv_cust_type      = '' ).
        CATCH /aif/cx_aif_engine_not_found .
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDTRY.

      TRY.
          lr_inf_det->/aif/if_inf_det_engine~determine_inf(
                EXPORTING
                  iv_name1      = ls_edidc-idoctp
                  iv_name2      = ls_edidc-mestyp
                  iv_input      = ls_input
                  iv_msgguid    = lv_guid32
                IMPORTING
                  es_finf       = ls_finf
                  ).
        CATCH /aif/cx_error_handling_general .
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        CATCH /aif/cx_inf_det_base .
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        CATCH /aif/cx_aif_engine_base .
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDTRY.

      "Transform data to AIF structure
      IF ls_finf IS NOT INITIAL.
        IF ls_edidc-direct = 1.
          lv_typename = ls_finf-ddicstructure.
        ELSEIF ls_edidc-direct = 2.
          lv_typename = ls_finf-ddicstructureraw.
        ENDIF.
        CALL METHOD /aif/cl_idoc_utilities=>convert_idoc_to_aif_structure
          EXPORTING
            is_control_rec    = ls_edidc
            it_edid4          = lt_edidd
            iv_typename       = lv_typename
            iv_keep_segments  = ls_finf-keep_segments
          CHANGING
            cr_aif_structure  = lr_source_data
          EXCEPTIONS
            aiftype_not_found = 1
            OTHERS            = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ENDIF.

      "Parse IDOC
      APPEND INITIAL LINE TO <gft_output> ASSIGNING <gfs_output>.
      ASSIGN lr_source_data->* TO <lfs_field>.
      CALL METHOD handle_type_struc
        EXPORTING
          iv_field = <lfs_field>
          iv_hier  = 0
          iv_name  = lv_typename
        CHANGING
          cs_struc = <gfs_output>.

    ENDIF.

  ENDMETHOD.

  METHOD handle_type_field.
    "READ TABLE gt_tab WITH KEY field = iv_path INTO DATA(ls_tab).
    "IF sy-subrc EQ 0 AND iv_field IS NOT INITIAL.
    "ASSIGN COMPONENT ls_tab-filename OF STRUCTURE <gfs_output> TO FIELD-SYMBOL(<lfs_field>).
    "<lfs_field> = iv_field.
    IF iv_field IS NOT INITIAL.
      WRITE: iv_path(30),':',iv_hier,':', iv_field,/.
    ENDIF.
  ENDMETHOD.

  METHOD handle_type_struc.

    DATA:
      lr_descr  TYPE REF TO cl_abap_structdescr,
      wa_comp   TYPE abap_compdescr,
      lv_method TYPE char20.

    lr_descr ?= cl_abap_typedescr=>describe_by_data( iv_field ).
    LOOP AT lr_descr->components INTO wa_comp.

      ASSIGN COMPONENT wa_comp-name OF STRUCTURE iv_field TO FIELD-SYMBOL(<lfs_field>).
      IF sy-subrc EQ 0.
        lv_method = |HANDLE_TYPE_{ to_upper( COND char05( WHEN wa_comp-type_kind EQ 'h' THEN 'TAB'
                                                     WHEN wa_comp-type_kind EQ 'u' THEN 'STRUC'
                                                     ELSE 'FIELD'  ) ) }|.
        CALL METHOD (lv_method)
          EXPORTING
            iv_path  = COND char100( WHEN iv_path IS INITIAL THEN wa_comp-name ELSE |{ iv_path }-{ wa_comp-name }| )
            iv_field = <lfs_field>
            iv_hier  = iv_hier + 1
            iv_name  = wa_comp-name.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD handle_type_tab.

    DATA:
      lr_descr  TYPE REF TO cl_abap_structdescr,
      wa_comp   TYPE abap_compdescr,
      lv_method TYPE char20,
      lv_tabix  TYPE sy-tabix,
      lv_index  TYPE sy-index,
      lv_path   TYPE char100.

    LOOP AT iv_field ASSIGNING FIELD-SYMBOL(<lfs_wa>).

      lv_path = iv_path.
      lr_descr ?= cl_abap_typedescr=>describe_by_data( <lfs_wa> ).
      IF lr_descr->components[ 1 ]-name = 'QUALF'
      OR lr_descr->components[ 1 ]-name = 'IDDAT'
      OR lr_descr->components[ 1 ]-name = 'PARVW'.
        ASSIGN COMPONENT 1 OF STRUCTURE <lfs_wa> TO FIELD-SYMBOL(<lfs_field>).
        IF sy-subrc EQ 0.
          lv_path = |{ lv_path }-{ <lfs_field> }|.
          lv_index = 1.
        ENDIF.
      ENDIF.

      LOOP AT lr_descr->components INTO wa_comp FROM lv_index + 1.
        ASSIGN COMPONENT wa_comp-name OF STRUCTURE <lfs_wa> TO <lfs_field>.
        IF sy-subrc EQ 0.
          lv_method = |HANDLE_TYPE_{ to_upper( COND char05( WHEN wa_comp-type_kind EQ 'h' THEN 'TAB'
                                                       WHEN wa_comp-type_kind EQ 'u' THEN 'STRUC'
                                                       ELSE 'FIELD'  ) ) }|.
          CALL METHOD (lv_method)
            EXPORTING
              iv_path  = COND char100( WHEN lv_path IS INITIAL THEN wa_comp-name ELSE |{ lv_path }-{ wa_comp-name }| )
              iv_field = <lfs_field>
              iv_hier  = iv_hier + 1
              iv_name  = wa_comp-name.
        ENDIF.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  zcl_transform=>main( ).
