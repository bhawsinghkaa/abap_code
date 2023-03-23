class ZCL_ZFT_P_FORMS_DPC_EXT definition
  public
  inheriting from ZCL_ZFT_P_FORMS_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_STREAM
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_DEEP_ENTITY
    redefinition .
protected section.

  methods FORM_LANGUAGESET_GET_ENTITY
    redefinition .
  methods FORM_LANGUAGESET_GET_ENTITYSET
    redefinition .
  methods FORM_LANGUAGESET_UPDATE_ENTITY
    redefinition .
  methods FORM_NAME_SET_GET_ENTITY
    redefinition .
  methods FORM_NAME_SET_GET_ENTITYSET
    redefinition .
  methods TRANSLATIONSET_GET_ENTITY
    redefinition .
  methods TRANSLATIONSET_GET_ENTITYSET
    redefinition .
  methods TRANSLATIONSET_UPDATE_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZFT_P_FORMS_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~create_deep_entity.

    DATA:
      lv_name          TYPE fpname,
      lv_lang          TYPE laiso,
      lv_spras         TYPE spras,
      lv_guid          TYPE char255,
      lv_object        TYPE lxeobjname,
      lv_source_lang   TYPE spras,
      lv_source_string TYPE xstring,
      lv_target_string TYPE xstring,
      lv_update_string TYPE xstring,
      lv_final_status  TYPE lxestatprc,
      lt_source_text   TYPE STANDARD TABLE OF smum_xmltb,
      lt_target_text   TYPE STANDARD TABLE OF smum_xmltb,
      lt_ret           TYPE STANDARD TABLE OF bapiret2,
      lr_deep_entity   TYPE zcl_zft_p_forms_mpc=>ts_deep_entity.

    "Read the data
    CALL METHOD io_data_provider->read_entry_data( IMPORTING es_data = lr_deep_entity ).

    lv_name = lr_deep_entity-name.
    lv_lang = lr_deep_entity-language.
    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
      EXPORTING
        input  = lv_lang
      IMPORTING
        output = lv_spras.

    "Check if and value is not present
    IF lv_name IS NOT INITIAL
   AND lv_spras IS NOT INITIAL
   AND lr_deep_entity-languagetotranslation IS NOT INITIAL.

      lv_object = lv_name.
      SELECT SINGLE masterlang FROM tadir INTO @lv_source_lang WHERE object = 'SFPF' AND obj_name = @lv_object.

      "Read the source text in English
      CALL FUNCTION 'LXE_OBJ_REMO_FP_READ_XDP_IN'
        EXPORTING
          objtype    = 'PDFB'
          objname    = lv_object
          i_language = lv_source_lang
          sysysid    = sy-sysid
          local_call = 'X'
        IMPORTING
          e_xdp      = lv_source_string.
      IF lv_source_string IS INITIAL.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception.
      ENDIF.

      "Parse the source text into translations
      CALL FUNCTION 'SMUM_XML_PARSE'
        EXPORTING
          xml_input = lv_source_string
        TABLES
          xml_table = lt_source_text
          return    = lt_ret.
      LOOP AT lt_ret INTO DATA(ls_ret) WHERE type = 'E' OR type = 'A'.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception.
      ENDLOOP.

      "Read the source text in Target language
      CALL FUNCTION 'LXE_OBJ_REMO_FP_READ_XDP_IN'
        EXPORTING
          objtype    = 'PDFB'
          objname    = lv_object
          i_language = lv_spras
          sysysid    = sy-sysid
          local_call = 'X'
        IMPORTING
          e_xdp      = lv_target_string.
      IF lv_target_string IS INITIAL.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception.
      ENDIF.

      "Parse the source text into translations
      CALL FUNCTION 'SMUM_XML_PARSE'
        EXPORTING
          xml_input = lv_target_string
        TABLES
          xml_table = lt_target_text
          return    = lt_ret.
      LOOP AT lt_ret INTO ls_ret WHERE type = 'E' OR type = 'A'.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception.
      ENDLOOP.

      "Cumulate it all in output
      LOOP AT lr_deep_entity-languagetotranslation INTO DATA(ls_translation).
        lv_guid = ls_translation-guid.
        READ TABLE lt_source_text INTO DATA(ls_source_text_uid) WITH KEY cvalue = lv_guid.
        IF sy-subrc EQ 0.
          READ TABLE lt_source_text INTO DATA(ls_source_text) INDEX sy-tabix - 1.
          READ TABLE lt_target_text INTO DATA(ls_target_text_uid) WITH KEY cvalue = lv_guid.
          IF sy-subrc EQ 0 AND ls_source_text-cvalue = ls_translation-source.
            READ TABLE lt_target_text ASSIGNING FIELD-SYMBOL(<lfs_target_text>) INDEX sy-tabix - 1.
            <lfs_target_text>-cvalue = ls_translation-target.
          ENDIF.
        ENDIF.
      ENDLOOP.

      "Create XSTRING from XML
      CALL FUNCTION 'SMUM_XML_CREATE_X'
        IMPORTING
          xml_output = lv_update_string
        TABLES
          xml_table  = lt_target_text.

      "Check for XSTRING validity
      CALL FUNCTION 'FP_CHECK_S2Y'
        EXPORTING
          i_s2y          = lv_update_string
        EXCEPTIONS
          wrong_s2y_file = 1
          OTHERS         = 2.
      IF sy-subrc EQ 0.
        CALL FUNCTION 'LXE_OBJ_REMO_FP_WRITE_XDP_IN'
          EXPORTING
            objtype    = 'PDFB'
            objname    = lv_object
            i_language = lv_spras
            sysysid    = sy-sysid
            i_xdp      = lv_update_string
          IMPORTING
            pstatus    = lv_final_status.
        IF lv_final_status EQ 'S'.
          copy_data_to_ref( EXPORTING is_data = lr_deep_entity
                  CHANGING  cr_data = er_deep_entity ).
        ENDIF.
      ENDIF.

    ELSE.

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception.

    ENDIF.

  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_stream.

    DATA:
      lt_keys     TYPE /iwbep/t_mgw_tech_pairs,
      ls_key      TYPE /iwbep/s_mgw_tech_pair,
      lv_customer TYPE s_customer,
      lv_xstring  TYPE xstring,
      ls_stream   TYPE ty_s_media_resource.

    DATA :
      lv_fm_name            TYPE rs38l_fnam,
      ls_output_options     TYPE ssfcompop,
      lv_language           TYPE tdspras,
      ls_control_parameters TYPE ssfctrlop,
      ls_output_data        TYPE ssfcrescl,
      lv_pdf_len            TYPE i,
      lv_pdf_xstring        TYPE xstring,
      lt_lines              TYPE TABLE OF tline,
      lv_devtype            TYPE rspoptype,
      lv_app_type           TYPE string,
      lv_guid               TYPE guid_32,
      lo_cached_response    TYPE REF TO if_http_response,
      ls_customer           TYPE  scustom,
      lt_bookings           TYPE  ty_bookings,
      lt_connections        TYPE  ty_connections,
      lt_tstotf             TYPE tsfotf.

    lt_keys = io_tech_request_context->get_keys( ).
    READ TABLE lt_keys WITH KEY name = 'document' INTO ls_key.
    lv_customer = ls_key-value.

    lv_language = sy-langu.
    TRANSLATE lv_language TO UPPER CASE.
    CONDENSE lv_language.
    ls_control_parameters-langu = lv_language.
    ls_control_parameters-no_dialog = abap_true.
    ls_control_parameters-getotf   = abap_true.
    ls_control_parameters-preview = abap_true.

    CALL FUNCTION 'SSF_GET_DEVICE_TYPE'
      EXPORTING
        i_language             = lv_language
      IMPORTING
        e_devtype              = lv_devtype
      EXCEPTIONS
        no_language            = 1
        language_not_installed = 2
        no_devtype_found       = 3
        system_error           = 4
        OTHERS                 = 5.


    ls_output_options-tdprinter = lv_devtype.
    ls_output_options-tdnewid = abap_true.
    ls_output_options-tddelete = space.

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname           = 'SF_EXAMPLE_03'
      IMPORTING
        fm_name            = lv_fm_name
      EXCEPTIONS
        no_form            = 1
        no_function_module = 2
        OTHERS             = 3.

* Data retrieval and supplying it to Samrtform FM
    SELECT SINGLE * FROM scustom INTO ls_customer WHERE id = lv_customer.
    SELECT * FROM sbook INTO TABLE lt_bookings   WHERE customid = lv_customer.
    SELECT * FROM spfli INTO TABLE lt_connections UP TO 10 ROWS.

* Call Smartform generated FM
    CALL FUNCTION lv_fm_name
      EXPORTING
        control_parameters = ls_control_parameters
        output_options     = ls_output_options
        user_settings      = space
        customer           = ls_customer
        bookings           = lt_bookings
        connections        = lt_connections
      IMPORTING
        job_output_info    = ls_output_data
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.
    APPEND LINES OF ls_output_data-otfdata[] TO lt_tstotf[].

* Convert to OTF
    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        format                = 'PDF'
      IMPORTING
        bin_filesize          = lv_pdf_len
        bin_file              = lv_pdf_xstring
      TABLES
        otf                   = lt_tstotf
        lines                 = lt_lines
      EXCEPTIONS
        err_max_linewidth     = 1
        err_format            = 2
        err_conv_not_possible = 3
        err_bad_otf           = 4
        OTHERS                = 5.
    IF sy-subrc = 0.
      lv_xstring = lv_pdf_xstring.
    ENDIF.

    ls_stream-value = lv_xstring.
    ls_stream-mime_type = 'application/pdf'.
    copy_data_to_ref( EXPORTING is_data = ls_stream
                      CHANGING  cr_data = er_stream ).

    DATA:  ls_header  TYPE ihttpnvp.
    ls_header-name = 'content-disposition'.
    ls_header-value = 'inline; filename=entity.pdf'.
    /iwbep/if_mgw_conv_srv_runtime~set_header( ls_header ).

  ENDMETHOD.


  METHOD form_languageset_get_entity.

    DATA: lv_name  TYPE fpname,
          lv_lang  TYPE laiso,
          lv_spras TYPE spras.

    CASE iv_source_name.
      WHEN 'form_name'.
        READ TABLE it_navigation_path INTO DATA(ls_navigation_path) WITH KEY nav_prop = 'nametolanguage'.
        IF sy-subrc EQ 0.
          DATA(lt_key_tab) = ls_navigation_path-key_tab.
        ENDIF.
      WHEN 'form_language'.
        lt_key_tab = it_key_tab.
    ENDCASE.

    READ TABLE lt_key_tab INTO DATA(ls_key_tab) WITH KEY name = 'name'.
    IF sy-subrc EQ 0 AND ls_key_tab-value IS NOT INITIAL.
      READ TABLE lt_key_tab INTO DATA(ls_key_tab2) WITH KEY name = 'language'.
      IF sy-subrc EQ 0 AND ls_key_tab2-value IS NOT INITIAL.
        lv_name = ls_key_tab-value.
        lv_lang = ls_key_tab2-value.
        CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
          EXPORTING
            input  = lv_lang
          IMPORTING
            output = lv_spras.
      ENDIF.
    ENDIF.

    IF lv_name IS NOT INITIAL AND lv_spras IS NOT INITIAL.

      SELECT
      SINGLE fplayoutt~name,
             fplayoutt~language,
             t002t~sptxt
        FROM fplayoutt
  LEFT OUTER JOIN t002t ON t002t~spras EQ @sy-langu AND fplayoutt~language EQ t002t~sprsl
       WHERE name = @lv_name
         AND state = 'A'
         AND language EQ @lv_spras
        INTO @er_entity.
      IF sy-subrc EQ 0.
        CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
          EXPORTING
            input  = er_entity-language
          IMPORTING
            output = er_entity-language.
      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD form_languageset_get_entityset.

    READ TABLE it_key_tab INTO DATA(ls_key_tab) WITH KEY name = 'name'.
    IF sy-subrc EQ 0 AND ls_key_tab-value IS NOT INITIAL.

      SELECT fplayoutt~name,
             fplayoutt~language,
             t002t~sptxt
        FROM fplayoutt
  LEFT OUTER JOIN t002t ON t002t~spras EQ @sy-langu AND fplayoutt~language EQ t002t~sprsl
       WHERE name = @ls_key_tab-value
         AND state = 'A'
         AND language NE ( SELECT masterlang FROM tadir WHERE object = 'SFPF' AND obj_name = @ls_key_tab-value )
    ORDER BY language
        INTO TABLE @et_entityset.
      IF sy-subrc EQ 0.
        LOOP AT et_entityset ASSIGNING FIELD-SYMBOL(<lfs_entityset>).
          CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
            EXPORTING
              input  = <lfs_entityset>-language
            IMPORTING
              output = <lfs_entityset>-language.
        ENDLOOP.
      ENDIF.

      "The function module for Filter conditons
      CALL METHOD /iwbep/cl_mgw_data_util=>filtering
        EXPORTING
          it_select_options = it_filter_select_options
        CHANGING
          ct_data           = et_entityset.

      "Paging
      CALL METHOD /iwbep/cl_mgw_data_util=>paging
        EXPORTING
          is_paging = is_paging
        CHANGING
          ct_data   = et_entityset.

      "Order By
      CALL METHOD /iwbep/cl_mgw_data_util=>orderby
        EXPORTING
          it_order = it_order
        CHANGING
          ct_data  = et_entityset.

      "$inlinecount query option for all count entries
      IF io_tech_request_context->has_inlinecount( ) = abap_true.
        DESCRIBE TABLE et_entityset LINES es_response_context-inlinecount.
      ELSE.
        CLEAR es_response_context-inlinecount.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  method FORM_LANGUAGESET_UPDATE_ENTITY.
**TRY.
*CALL METHOD SUPER->FORM_LANGUAGESET_UPDATE_ENTITY
*  EXPORTING
*    IV_ENTITY_NAME          =
*    IV_ENTITY_SET_NAME      =
*    IV_SOURCE_NAME          =
*    IT_KEY_TAB              =
**    io_tech_request_context =
*    IT_NAVIGATION_PATH      =
**    io_data_provider        =
**  IMPORTING
**    er_entity               =
*    .
**  CATCH /iwbep/cx_mgw_busi_exception.
**  CATCH /iwbep/cx_mgw_tech_exception.
**ENDTRY.
  endmethod.


  METHOD form_name_set_get_entity.

    READ TABLE it_key_tab INTO DATA(ls_key_tab) WITH KEY name = 'name'.
    IF sy-subrc EQ 0 AND ls_key_tab-value IS NOT INITIAL.

      SELECT
      SINGLE fpcontext~name,
             fpcontextt~text
        FROM fpcontext
       INNER JOIN fpcontextt ON fpcontext~name = fpcontextt~name AND fpcontext~state = fpcontextt~state AND fpcontextt~language = @sy-langu AND fpcontextt~id = ''
       WHERE fpcontext~name = @ls_key_tab-value
         AND fpcontext~state = 'A'
         AND fpcontext~type = @space
        INTO @er_entity.

    ENDIF.

  ENDMETHOD.


  METHOD form_name_set_get_entityset.

    DATA:
      lt_otab  TYPE abap_sortorder_tab,
      ls_oline TYPE abap_sortorder.

    CONSTANTS:
      BEGIN OF lcs_sorting_order,
        descending TYPE string VALUE 'desc',
        ascending  TYPE string VALUE 'asc',
      END OF   lcs_sorting_order.

    SELECT fpcontext~name,
           fpcontextt~text
      FROM fpcontext
     INNER JOIN fpcontextt ON fpcontext~name = fpcontextt~name AND fpcontext~state = fpcontextt~state AND fpcontextt~language = @sy-langu AND fpcontextt~id EQ ''
     WHERE fpcontext~state = 'A'
       AND fpcontext~type = @space
     ORDER BY fpcontext~name
      INTO TABLE @et_entityset.

    "The function module for Filter conditons
    CALL METHOD /iwbep/cl_mgw_data_util=>filtering
      EXPORTING
        it_select_options = it_filter_select_options
      CHANGING
        ct_data           = et_entityset.

    "Paging
    CALL METHOD /iwbep/cl_mgw_data_util=>paging
      EXPORTING
        is_paging = is_paging
      CHANGING
        ct_data   = et_entityset.

    "Order By
    CALL METHOD /iwbep/cl_mgw_data_util=>orderby
      EXPORTING
        it_order = it_order
      CHANGING
        ct_data  = et_entityset.

    "$inlinecount query option for all count entries
    IF io_tech_request_context->has_inlinecount( ) = abap_true.
      DESCRIBE TABLE et_entityset LINES es_response_context-inlinecount.
    ELSE.
      CLEAR es_response_context-inlinecount.
    ENDIF.

  ENDMETHOD.


  METHOD translationset_get_entity.

    DATA:
      lv_name          TYPE fpname,
      lv_lang          TYPE laiso,
      lv_spras         TYPE spras,
      lv_guid          TYPE char255,
      lv_object        TYPE lxeobjname,
      lv_source_lang   TYPE spras,
      lv_source_string TYPE xstring,
      lv_target_string TYPE xstring,
      lt_source_text   TYPE STANDARD TABLE OF smum_xmltb,
      lt_target_text   TYPE STANDARD TABLE OF smum_xmltb,
      lt_ret           TYPE STANDARD TABLE OF bapiret2.

    CASE iv_source_name.
      WHEN 'form_name' OR 'form_language'.
        READ TABLE it_navigation_path INTO DATA(ls_navigation_path) WITH KEY nav_prop = 'languagetotranslation'.
        IF sy-subrc EQ 0.
          DATA(lt_key_tab) = ls_navigation_path-key_tab.
        ENDIF.
      WHEN 'translation'.
        lt_key_tab = it_key_tab.
    ENDCASE.

    READ TABLE lt_key_tab INTO DATA(ls_key_tab) WITH KEY name = 'name'.
    IF sy-subrc EQ 0 AND ls_key_tab-value IS NOT INITIAL.
      READ TABLE lt_key_tab INTO DATA(ls_key_tab2) WITH KEY name = 'language'.
      IF sy-subrc EQ 0 AND ls_key_tab2-value IS NOT INITIAL.
        READ TABLE lt_key_tab INTO DATA(ls_key_tab3) WITH KEY name = 'guid'.
        IF sy-subrc EQ 0 AND ls_key_tab3-value IS NOT INITIAL.
          lv_name = ls_key_tab-value.
          lv_lang = ls_key_tab2-value.
          lv_guid = ls_key_tab3-value.
          CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
            EXPORTING
              input  = lv_lang
            IMPORTING
              output = lv_spras.
        ENDIF.
      ENDIF.
    ENDIF.

    IF lv_name IS NOT INITIAL AND lv_spras IS NOT INITIAL AND lv_guid IS NOT INITIAL.

      lv_object = lv_name.
      SELECT SINGLE masterlang FROM tadir INTO @lv_source_lang WHERE object = 'SFPF' AND obj_name = @lv_object.

      "Read the source text in English
      CALL FUNCTION 'LXE_OBJ_REMO_FP_READ_XDP_IN'
        EXPORTING
          objtype    = 'PDFB'
          objname    = lv_object
          i_language = lv_source_lang
          sysysid    = sy-sysid
          local_call = 'X'
        IMPORTING
          e_xdp      = lv_source_string.
      IF lv_source_string IS INITIAL.
        RETURN.
      ENDIF.

      "Parse the source text into translations
      CALL FUNCTION 'SMUM_XML_PARSE'
        EXPORTING
          xml_input = lv_source_string
        TABLES
          xml_table = lt_source_text
          return    = lt_ret.
      LOOP AT lt_ret INTO DATA(ls_ret) WHERE type = 'E' OR type = 'A'.
        RETURN.
      ENDLOOP.

      "Read the source text in Target language
      CALL FUNCTION 'LXE_OBJ_REMO_FP_READ_XDP_IN'
        EXPORTING
          objtype    = 'PDFB'
          objname    = lv_object
          i_language = lv_spras
          sysysid    = sy-sysid
          local_call = 'X'
        IMPORTING
          e_xdp      = lv_target_string.
      IF lv_target_string IS INITIAL.
        RETURN.
      ENDIF.

      "Parse the source text into translations
      CALL FUNCTION 'SMUM_XML_PARSE'
        EXPORTING
          xml_input = lv_target_string
        TABLES
          xml_table = lt_target_text
          return    = lt_ret.
      LOOP AT lt_ret INTO ls_ret WHERE type = 'E' OR type = 'A'.
        RETURN.
      ENDLOOP.

      "Cumulate it all in output
      READ TABLE lt_source_text INTO DATA(ls_source_text_uid) WITH KEY cvalue = lv_guid.
      IF sy-subrc EQ 0.
        READ TABLE lt_source_text INTO DATA(ls_source_text) INDEX sy-tabix - 1.
        READ TABLE lt_target_text INTO DATA(ls_target_text_uid) WITH KEY cvalue = lv_guid.
        IF sy-subrc EQ 0.
          READ TABLE lt_target_text INTO DATA(ls_target_text) INDEX sy-tabix - 1.
          er_entity-name = lv_name.
          er_entity-language = lv_lang.
          er_entity-guid = lv_guid.
          er_entity-source = ls_source_text-cvalue.
          er_entity-target = ls_target_text-cvalue.
        ENDIF.
      ENDIF.

    ENDIF.



  ENDMETHOD.


  METHOD translationset_get_entityset.

    DATA:
      lv_name          TYPE fpname,
      lv_lang          TYPE laiso,
      lv_spras         TYPE spras,
      lv_source_lang   TYPE spras,
      lv_object        TYPE lxeobjname,
      lv_source_string TYPE xstring,
      lv_target_string TYPE xstring,
      lt_source_text   TYPE STANDARD TABLE OF smum_xmltb,
      lt_target_text   TYPE STANDARD TABLE OF smum_xmltb,
      lt_ret           TYPE STANDARD TABLE OF bapiret2.

    CASE iv_source_name.
      WHEN 'form_name'.
        READ TABLE it_navigation_path INTO DATA(ls_navigation_path) WITH KEY nav_prop = 'nametolanguage'.
        IF sy-subrc EQ 0.
          DATA(lt_key_tab) = ls_navigation_path-key_tab.
        ENDIF.
      WHEN 'form_language'.
        lt_key_tab = it_key_tab.
    ENDCASE.

    READ TABLE lt_key_tab INTO DATA(ls_key_tab) WITH KEY name = 'name'.
    IF sy-subrc EQ 0 AND ls_key_tab-value IS NOT INITIAL.
      READ TABLE lt_key_tab INTO DATA(ls_key_tab2) WITH KEY name = 'language'.
      IF sy-subrc EQ 0 AND ls_key_tab2-value IS NOT INITIAL.
        lv_name = ls_key_tab-value.
        lv_lang = ls_key_tab2-value.
        CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
          EXPORTING
            input  = lv_lang
          IMPORTING
            output = lv_spras.
      ENDIF.
    ENDIF.

    IF lv_name IS NOT INITIAL AND lv_spras IS NOT INITIAL.

      lv_object = lv_name.

      SELECT SINGLE masterlang FROM tadir INTO @lv_source_lang WHERE object = 'SFPF' AND obj_name = @lv_object.

      "Read the source text in English
      CALL FUNCTION 'LXE_OBJ_REMO_FP_READ_XDP_IN'
        EXPORTING
          objtype    = 'PDFB'
          objname    = lv_object
          i_language = lv_source_lang
          sysysid    = sy-sysid
          local_call = 'X'
        IMPORTING
          e_xdp      = lv_source_string.
      IF lv_source_string IS INITIAL.
        RETURN.
      ENDIF.

      "Parse the source text into translations
      CALL FUNCTION 'SMUM_XML_PARSE'
        EXPORTING
          xml_input = lv_source_string
        TABLES
          xml_table = lt_source_text
          return    = lt_ret.
      LOOP AT lt_ret INTO DATA(ls_ret) WHERE type = 'E' OR type = 'A'.
        RETURN.
      ENDLOOP.

      "Read the source text in Target language
      CALL FUNCTION 'LXE_OBJ_REMO_FP_READ_XDP_IN'
        EXPORTING
          objtype    = 'PDFB'
          objname    = lv_object
          i_language = lv_spras
          sysysid    = sy-sysid
          local_call = 'X'
        IMPORTING
          e_xdp      = lv_target_string.
      IF lv_target_string IS INITIAL.
        RETURN.
      ENDIF.

      "Parse the source text into translations
      CALL FUNCTION 'SMUM_XML_PARSE'
        EXPORTING
          xml_input = lv_target_string
        TABLES
          xml_table = lt_target_text
          return    = lt_ret.
      LOOP AT lt_ret INTO ls_ret WHERE type = 'E' OR type = 'A'.
        RETURN.
      ENDLOOP.

      "Cumulate it all in a table
      LOOP AT lt_source_text INTO DATA(ls_source_text) WHERE type = 'V' AND cname = 'trans-unit'.
        READ TABLE lt_source_text INTO DATA(ls_source_text_uid) INDEX sy-tabix + 1.
        READ TABLE lt_target_text INTO DATA(ls_target_text_uid) WITH KEY hier   = ls_source_text_uid-hier
                                                                         type   = ls_source_text_uid-type
                                                                         cname  = ls_source_text_uid-cname
                                                                         cvalue = ls_source_text_uid-cvalue.
        READ TABLE lt_target_text INTO DATA(ls_target_text) INDEX sy-tabix - 1.
        APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<lfs_entityset>).
        <lfs_entityset>-name = lv_name.
        <lfs_entityset>-language = lv_lang.
        <lfs_entityset>-guid = ls_source_text_uid-cvalue.
        <lfs_entityset>-source = ls_source_text-cvalue.
        <lfs_entityset>-target = ls_target_text-cvalue.
      ENDLOOP.

    ENDIF.

    "The function module for Filter conditons
    CALL METHOD /iwbep/cl_mgw_data_util=>filtering
      EXPORTING
        it_select_options = it_filter_select_options
      CHANGING
        ct_data           = et_entityset.

    "Paging
    CALL METHOD /iwbep/cl_mgw_data_util=>paging
      EXPORTING
        is_paging = is_paging
      CHANGING
        ct_data   = et_entityset.

    "Order By
    CALL METHOD /iwbep/cl_mgw_data_util=>orderby
      EXPORTING
        it_order = it_order
      CHANGING
        ct_data  = et_entityset.

    "$inlinecount query option for all count entries
    IF io_tech_request_context->has_inlinecount( ) = abap_true.
      DESCRIBE TABLE et_entityset LINES es_response_context-inlinecount.
    ELSE.
      CLEAR es_response_context-inlinecount.
    ENDIF.

  ENDMETHOD.


  METHOD translationset_update_entity.

    DATA:
      lv_name          TYPE fpname,
      lv_lang          TYPE laiso,
      lv_spras         TYPE spras,
      lv_guid          TYPE char255,
      lv_object        TYPE lxeobjname,
      lv_source_lang   TYPE spras,
      lv_source_string TYPE xstring,
      lv_target_string TYPE xstring,
      lv_update_string TYPE xstring,
      lr_entity        LIKE er_entity,
      lv_final_status  TYPE lxestatprc,
      lt_source_text   TYPE STANDARD TABLE OF smum_xmltb,
      lt_target_text   TYPE STANDARD TABLE OF smum_xmltb,
      lt_ret           TYPE STANDARD TABLE OF bapiret2.

    DATA(lt_key_tab) = it_key_tab.

    "Read the key
    READ TABLE lt_key_tab INTO DATA(ls_key_tab) WITH KEY name = 'name'.
    IF sy-subrc EQ 0 AND ls_key_tab-value IS NOT INITIAL.
      READ TABLE lt_key_tab INTO DATA(ls_key_tab2) WITH KEY name = 'language'.
      IF sy-subrc EQ 0 AND ls_key_tab2-value IS NOT INITIAL.
        READ TABLE lt_key_tab INTO DATA(ls_key_tab3) WITH KEY name = 'guid'.
        IF sy-subrc EQ 0 AND ls_key_tab3-value IS NOT INITIAL.
          lv_name = ls_key_tab-value.
          lv_lang = ls_key_tab2-value.
          lv_guid = ls_key_tab3-value.
          CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
            EXPORTING
              input  = lv_lang
            IMPORTING
              output = lv_spras.
        ENDIF.
      ENDIF.
    ENDIF.

    "Read the data
    CALL METHOD io_data_provider->read_entry_data( IMPORTING es_data = lr_entity ).

    "Check if and value is not present
    IF lv_name IS NOT INITIAL
   AND lv_spras IS NOT INITIAL
   AND lv_guid IS NOT INITIAL
   AND lr_entity-source IS NOT INITIAL
   AND lr_entity-target IS NOT INITIAL.

      lv_object = lv_name.
      SELECT SINGLE masterlang FROM tadir INTO @lv_source_lang WHERE object = 'SFPF' AND obj_name = @lv_object.

      "Read the source text in English
      CALL FUNCTION 'LXE_OBJ_REMO_FP_READ_XDP_IN'
        EXPORTING
          objtype    = 'PDFB'
          objname    = lv_object
          i_language = lv_source_lang
          sysysid    = sy-sysid
          local_call = 'X'
        IMPORTING
          e_xdp      = lv_source_string.
      IF lv_source_string IS INITIAL.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception.
      ENDIF.

      "Parse the source text into translations
      CALL FUNCTION 'SMUM_XML_PARSE'
        EXPORTING
          xml_input = lv_source_string
        TABLES
          xml_table = lt_source_text
          return    = lt_ret.
      LOOP AT lt_ret INTO DATA(ls_ret) WHERE type = 'E' OR type = 'A'.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception.
      ENDLOOP.

      "Read the source text in Target language
      CALL FUNCTION 'LXE_OBJ_REMO_FP_READ_XDP_IN'
        EXPORTING
          objtype    = 'PDFB'
          objname    = lv_object
          i_language = lv_spras
          sysysid    = sy-sysid
          local_call = 'X'
        IMPORTING
          e_xdp      = lv_target_string.
      IF lv_target_string IS INITIAL.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception.
      ENDIF.

      "Parse the source text into translations
      CALL FUNCTION 'SMUM_XML_PARSE'
        EXPORTING
          xml_input = lv_target_string
        TABLES
          xml_table = lt_target_text
          return    = lt_ret.
      LOOP AT lt_ret INTO ls_ret WHERE type = 'E' OR type = 'A'.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception.
      ENDLOOP.

      "Cumulate it all in output
      READ TABLE lt_source_text INTO DATA(ls_source_text_uid) WITH KEY cvalue = lv_guid.
      IF sy-subrc EQ 0.
        READ TABLE lt_source_text INTO DATA(ls_source_text) INDEX sy-tabix - 1.
        READ TABLE lt_target_text INTO DATA(ls_target_text_uid) WITH KEY cvalue = lv_guid.
        IF sy-subrc EQ 0 AND ls_source_text-cvalue = er_entity-source.
          READ TABLE lt_target_text ASSIGNING FIELD-SYMBOL(<lfs_target_text>) INDEX sy-tabix - 1.
          <lfs_target_text>-cvalue = er_entity-target.

          "Create XSTRING from XML
          CALL FUNCTION 'SMUM_XML_CREATE_X'
            IMPORTING
              xml_output = lv_update_string
            TABLES
              xml_table  = lt_target_text.

          "Check for XSTRING validity
          CALL FUNCTION 'FP_CHECK_S2Y'
            EXPORTING
              i_s2y          = lv_update_string
            EXCEPTIONS
              wrong_s2y_file = 1
              OTHERS         = 2.
          IF sy-subrc EQ 0.
            CALL FUNCTION 'LXE_OBJ_REMO_FP_WRITE_XDP_IN'
              EXPORTING
                objtype    = 'PDFB'
                objname    = lv_object
                i_language = lv_spras
                sysysid    = sy-sysid
                i_xdp      = lv_update_string
              IMPORTING
                pstatus    = lv_final_status.
            IF lv_final_status EQ 'S'.
              er_entity-name = lv_name.
              er_entity-language = lv_lang.
              er_entity-guid = lv_guid.
              er_entity-source = ls_source_text-cvalue.
              er_entity-target = <lfs_target_text>-cvalue.
            ENDIF.
          ENDIF.

        ENDIF.
      ENDIF.

    ELSE.

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception.

    ENDIF.



  ENDMETHOD.
ENDCLASS.
