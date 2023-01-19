*&---------------------------------------------------------------------*
*& Report ZTRANSLATE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztranslate.

"Global Types
TYPES:
  BEGIN OF lty_excel,
    fieldname(20),
  END OF lty_excel,

  BEGIN OF lty_output,
    uid    TYPE char255,
    source TYPE char255,
    target TYPE char255,
  END OF lty_output.

"Data Initialization
DATA:
  lv_filename      TYPE string,
  lt_filetable     TYPE filetable,
  lv_rc            TYPE i,
  lv_answer        TYPE char1,
  lv_source_string TYPE xstring,
  lv_target_string TYPE xstring,
  lv_object        TYPE lxeobjname,
  ls_output        TYPE lty_output,
  ls_excel         TYPE lty_excel,
  lv_final_status  TYPE lxestatprc,
  lt_output        TYPE STANDARD TABLE OF lty_output,
  lt_input_file    TYPE STANDARD TABLE OF lty_output,
  lt_excel         TYPE STANDARD TABLE OF lty_excel,
  lt_source_text   TYPE STANDARD TABLE OF smum_xmltb,
  lt_ret           TYPE STANDARD TABLE OF bapiret2,
  lo_alv           TYPE REF TO cl_salv_table,
  lo_columns       TYPE REF TO cl_salv_columns_table.

"Selection Screen
SELECTION-SCREEN BEGIN OF BLOCK a.
  PARAMETERS:
    r_dwn   RADIOBUTTON GROUP rad DEFAULT 'X' USER-COMMAND rad,
    r_upl   RADIOBUTTON GROUP rad,
    p_form  TYPE fpname OBLIGATORY,
    p_fname TYPE string OBLIGATORY,
    p_src   TYPE spras DEFAULT 'EN' OBLIGATORY,
    p_trg   TYPE spras.
SELECTION-SCREEN END OF BLOCK a.

"Show thw user the window dialog to choose the download location

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  IF r_dwn IS NOT INITIAL.
    CALL METHOD cl_gui_frontend_services=>directory_browse
      CHANGING
        selected_folder = lv_filename.
    p_fname = lv_filename.
  ELSE.
    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      CHANGING
        file_table = lt_filetable
        rc         = lv_rc.
    IF lv_rc NE 1.
      MESSAGE 'Please select only 1 file' TYPE 'I'.
    ELSE.
      p_fname = lt_filetable[ 1 ]-filename.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF r_dwn IS NOT INITIAL.
      IF screen-name CS 'P_TRG'.
        screen-active = 0.
        screen-required = 0.
        MODIFY SCREEN.
      ENDIF.
    ELSE.
      IF screen-name CS 'P_TRG'.
        screen-active = 1.
        screen-required = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

START-OF-SELECTION.

  lv_object = p_form.
  IF r_dwn IS NOT INITIAL.

    "Read the source text
    CALL FUNCTION 'LXE_OBJ_REMO_FP_READ_XDP_IN'
      EXPORTING
        objtype    = 'PDFB'
        objname    = lv_object
        i_language = p_src
        sysysid    = sy-sysid
        local_call = 'X'
      IMPORTING
        e_xdp      = lv_source_string.
    IF lv_source_string IS INITIAL.
      MESSAGE TEXT-001 TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    DATA(lo_util) = NEW cl_aps_om_form_trans_util( iv_form = CONV fpname( lv_object ) ).
    lo_util->deserialize_xstring( EXPORTING iv_xml = lv_source_string iv_include_path = ''
                                                  IMPORTING et_translations = DATA(lt_new_translation) ).
    DATA(lt_master_translation) = lo_util->get_translation(
        iv_language =  p_src
        iv_include_path = '' ).

    "Parse the source text into translations
    CALL FUNCTION 'SMUM_XML_PARSE'
      EXPORTING
        xml_input = lv_source_string
      TABLES
        xml_table = lt_source_text
        return    = lt_ret.
    LOOP AT lt_ret INTO DATA(ls_ret) WHERE type = 'E' OR type = 'A'.
      MESSAGE TEXT-001 TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ENDLOOP.

    "Cumulate it all in a table
    LOOP AT lt_source_text INTO DATA(ls_source_text) WHERE type = 'V' AND cname = 'trans-unit'.
      READ TABLE lt_source_text INTO DATA(ls_source_text_next) INDEX sy-tabix + 1.
      ls_output-source = ls_source_text-cvalue.
      ls_output-uid = ls_source_text_next-cvalue.
      APPEND ls_output TO lt_output.
    ENDLOOP.

    "Prepare the header
    ls_excel-fieldname = 'GUID'.
    APPEND ls_excel TO lt_excel.

    ls_excel-fieldname = 'Source'.
    APPEND ls_excel TO lt_excel.

    ls_excel-fieldname = 'Target'.
    APPEND ls_excel TO lt_excel.

    lv_filename = p_fname && '\' && p_form && '_' && p_src && '.xls'.
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = lv_filename
        filetype                = 'ASC'
        write_field_separator   = 'X'
      TABLES
        data_tab                = lt_output
        fieldnames              = lt_excel
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.
    IF sy-subrc <> 0.
      MESSAGE TEXT-002 TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ENDIF.

  ELSE.

    "Check if the form is already translated and user wants to overwrite
    CALL FUNCTION 'LXE_OBJ_REMO_FP_READ_XDP_IN'
      EXPORTING
        objtype    = 'PDFB'
        objname    = lv_object
        i_language = p_trg
        sysysid    = sy-sysid
        local_call = 'X'
      IMPORTING
        e_xdp      = lv_target_string.
    IF lv_target_string IS NOT INITIAL.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Text already maintained in target language'
          text_question         = 'Do you want to Overwrite'
          text_button_1         = 'Yes'
          text_button_2         = 'No'
          default_button        = '2'
          display_cancel_button = 'X'
        IMPORTING
          answer                = lv_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.
      IF lv_answer NE '1'.
        RETURN.
      ENDIF.
    ENDIF.
    CLEAR lv_target_string.

    lv_filename = p_fname.
    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = lv_filename
        has_field_separator     = 'X'
      TABLES
        data_tab                = lt_input_file
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        OTHERS                  = 17.
    IF sy-subrc <> 0.
      MESSAGE 'Failed to upload file' TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    "Read the source text
    CALL FUNCTION 'LXE_OBJ_REMO_FP_READ_XDP_IN'
      EXPORTING
        objtype    = 'PDFB'
        objname    = lv_object
        i_language = p_src
        sysysid    = sy-sysid
        local_call = 'X'
      IMPORTING
        e_xdp      = lv_source_string.
    IF lv_source_string IS INITIAL.
      MESSAGE TEXT-001 TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    "Parse the source text into translations
    CALL FUNCTION 'SMUM_XML_PARSE'
      EXPORTING
        xml_input = lv_source_string
      TABLES
        xml_table = lt_source_text
        return    = lt_ret.
    LOOP AT lt_ret INTO ls_ret WHERE type = 'E' OR type = 'A'.
      MESSAGE TEXT-001 TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ENDLOOP.

    "Update the target texts to display
    LOOP AT lt_source_text ASSIGNING FIELD-SYMBOL(<lfs_source>) WHERE type = 'V' AND cname = 'trans-unit'.
      READ TABLE lt_source_text INTO ls_source_text_next INDEX sy-tabix + 1.
      ls_output-source = <lfs_source>-cvalue.
      ls_output-uid = ls_source_text_next-cvalue.
      READ TABLE lt_input_file INTO DATA(ls_input_file) WITH KEY uid = ls_output-uid
                                                                 source = ls_output-source.
      IF sy-subrc EQ 0 AND ls_input_file-target IS NOT INITIAL.
        <lfs_source>-cvalue = ls_input_file-target.
        ls_output-target = ls_input_file-target.
        APPEND ls_output TO lt_output.
      ELSE.
        ls_output-target = <lfs_source>-cvalue.
        APPEND ls_output TO lt_output.
      ENDIF.
    ENDLOOP.

    "Display ALV
    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_alv
          CHANGING
            t_table      = lt_output ).
        IF lo_alv IS BOUND.
          lo_columns = lo_alv->get_columns( ).
          lo_columns->set_optimize( abap_true ).
          lo_alv->set_screen_popup(
            start_column = 1
            end_column = 100
            start_line  = 1
            end_line = 10 ).
          lo_alv->display( ).
        ENDIF.
      CATCH cx_salv_msg.
    ENDTRY.

    "Create XSTRING from XML
    CALL FUNCTION 'SMUM_XML_CREATE_X'
      IMPORTING
        xml_output = lv_target_string
      TABLES
        xml_table  = lt_source_text.

    DATA: lv_subrc TYPE sy-subrc.
    PERFORM handle_crlf_container CHANGING lv_target_string
    lv_subrc.

    "Check for XSTRING validity
    CALL FUNCTION 'FP_CHECK_S2Y'
      EXPORTING
        i_s2y          = lv_target_string
      EXCEPTIONS
        wrong_s2y_file = 1
        OTHERS         = 2.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'LXE_OBJ_REMO_FP_WRITE_XDP_IN'
        EXPORTING
          objtype    = 'PDFB'
          objname    = lv_object
          i_language = p_trg
          sysysid    = sy-sysid
          i_xdp      = lv_target_string
        IMPORTING
          pstatus    = lv_final_status.
      IF lv_final_status EQ 'S'.
        MESSAGE 'Translations uploaded successfully' TYPE 'S'.
      ELSE.
        MESSAGE 'Translations upload failed' TYPE 'I'.
      ENDIF.
    ENDIF.

  ENDIF.

FORM handle_crlf_container CHANGING p_xstring TYPE lxe_xstring
p_subrc TYPE sysubrc.

  DATA: lv_xstring TYPE xstring
, lv_type_attr TYPE lxetypeatt
, lv_exit TYPE lxebool
, lv_xsearch TYPE xstring
, lv_xreplace TYPE xstring
, lv_moffset TYPE i.
  CONSTANTS: lc_cl_tag TYPE xstring VALUE '3E' ">
  , lc_op_tag TYPE xstring VALUE '3C' "<
  , lc_crlf TYPE xstring VALUE '0D0A' "CRLF
  , lc_para_utf8 TYPE xstring VALUE 'E280A9'.



*
  CLEAR p_subrc.
  lv_xstring = p_xstring.



** don't insert CRLF's for Y-texts - they are not xml like.
*  CALL FUNCTION 'LXE_ATTOB_ATTS_OBJECT_READ'
*    EXPORTING
*      obj_type  = gs_wrkob-objtype
*    IMPORTING
*      type_attr = lv_type_attr.
*
*
*
*  IF lv_type_attr = 'Y'.
*    RETURN.
*  ENDIF.



* replace CRLF at tag positions
  lv_xsearch = lc_cl_tag && lc_crlf.
  CLEAR lv_exit.
  WHILE lv_exit IS INITIAL.



    CLEAR lv_moffset.
    FIND FIRST OCCURRENCE OF lv_xsearch IN lv_xstring MATCH OFFSET lv_moffset IN BYTE MODE.



    IF sy-subrc = 0.
      lv_exit = abap_false.
      REPLACE lv_xsearch WITH lc_cl_tag INTO lv_xstring IN BYTE MODE.
      IF sy-subrc <> 0.
        lv_exit = abap_true.
        p_subrc = 4.
        RETURN.
      ENDIF.
    ELSE.
      lv_exit = abap_true.
    ENDIF.



  ENDWHILE.



  lv_xsearch = lc_crlf && lc_op_tag .
  CLEAR lv_exit.
  WHILE lv_exit IS INITIAL.



    CLEAR lv_moffset.
    FIND FIRST OCCURRENCE OF lv_xsearch IN lv_xstring MATCH OFFSET lv_moffset IN BYTE MODE.



    IF sy-subrc = 0.
      lv_exit = abap_false.
      REPLACE lv_xsearch WITH lc_op_tag INTO lv_xstring IN BYTE MODE.
      IF sy-subrc <> 0.
        lv_exit = abap_true.
        p_subrc = 4.
        RETURN.
      ENDIF.
    ELSE.
      lv_exit = abap_true.
    ENDIF.



  ENDWHILE.



* replace remaining CRLF by utf-8 paragraph
  IF cl_abap_char_utilities=>charsize > 1. "only for unicode systems
    lv_xsearch = lc_crlf.
    CLEAR lv_exit.
    WHILE lv_exit IS INITIAL.



      CLEAR lv_moffset.
      FIND FIRST OCCURRENCE OF lv_xsearch IN lv_xstring MATCH OFFSET lv_moffset IN BYTE MODE.



      IF sy-subrc = 0.
        lv_exit = abap_false.
        REPLACE lv_xsearch WITH lc_para_utf8 INTO lv_xstring IN BYTE MODE.
        IF sy-subrc <> 0.
          lv_exit = abap_true.
          p_subrc = 4.
          RETURN.
        ENDIF.
      ELSE.
        lv_exit = abap_true.
      ENDIF.



    ENDWHILE.
  ENDIF.



  IF p_subrc = 0.
    p_xstring = lv_xstring.
  ENDIF.
ENDFORM.
