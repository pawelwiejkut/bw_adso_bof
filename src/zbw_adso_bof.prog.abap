REPORT zbw_ADSO_BOF.
TYPES: BEGIN OF t_msg,
         msgty TYPE symsgty,
         msgid TYPE symsgid,
         msgno TYPE symsgno,
         msgv1 TYPE symsgv,
         msgv2 TYPE symsgv,
         msgv3 TYPE symsgv,
         msgv4 TYPE symsgv.
TYPES: END OF t_msg.

TYPES: t_ty_msg TYPE STANDARD TABLE OF t_msg WITH DEFAULT KEY.

DATA: lt_file_table     TYPE filetable,
      lv_rc             TYPE i,
      lv_escape_char    TYPE char1,
      lv_separator_char TYPE char1,
      lt_output         TYPE table_of_strings,
      lt_key            TYPE cl_rso_adso_api=>tn_t_key,
      lv_hex            TYPE xstring,
      lt_dimension      TYPE cl_rso_adso_api=>tn_t_dimension,
      lv_prev_fieldname TYPE string.

SELECTION-SCREEN BEGIN OF BLOCK part1 WITH FRAME TITLE TEXT-b01.
  PARAMETERS: pa_path TYPE string LOWER CASE OBLIGATORY.
SELECTION-SCREEN END OF BLOCK part1.

SELECTION-SCREEN BEGIN OF BLOCK part2 WITH FRAME TITLE TEXT-b02.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(11) TEXT-001 FOR FIELD pa_esc.
    SELECTION-SCREEN POSITION 15.
    PARAMETERS pa_esc  TYPE char4.

    SELECTION-SCREEN COMMENT 25(4) TEXT-002 FOR FIELD pa_ehx.
    SELECTION-SCREEN POSITION 23.
    PARAMETERS pa_ehx  AS CHECKBOX.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(14) TEXT-003 FOR FIELD pa_sep.
    SELECTION-SCREEN POSITION 15.
    PARAMETERS pa_sep  TYPE char4.

    SELECTION-SCREEN COMMENT 25(4) TEXT-004 FOR FIELD pa_shx.
    SELECTION-SCREEN POSITION 23.
    PARAMETERS pa_shx  AS CHECKBOX.
  SELECTION-SCREEN END OF LINE.

  PARAMETERS: pa_tse  TYPE rsthousand,
              pa_dse  TYPE rsdecimal,
              pa_head AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK part2.

SELECTION-SCREEN BEGIN OF BLOCK part3 WITH FRAME TITLE TEXT-b03.
  PARAMETERS: pa_ane  TYPE char10 OBLIGATORY,
              pa_lod  AS CHECKBOX,
              pa_area TYPE rsinfoarea.
SELECTION-SCREEN END OF BLOCK part3.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_path.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      file_filter             = '*.CSV'
    CHANGING
      file_table              = lt_file_table
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF lt_file_table IS NOT INITIAL.
    pa_path = lt_file_table[ 1 ].
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_esc.
  PERFORM f4_hex CHANGING pa_esc.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_sep.
  PERFORM f4_hex CHANGING pa_sep.

END-OF-SELECTION.

  cl_gui_frontend_services=>gui_upload(
   EXPORTING
     filename                = pa_path
   CHANGING
     data_tab                = lt_output
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
     not_supported_by_gui    = 17
     error_no_gui            = 18
     OTHERS                  = 19 ).

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT lt_output REFERENCE INTO DATA(lr_output).
    IF sy-tabix > 100000.
      DELETE lt_output.
    ENDIF.
  ENDLOOP.

  IF pa_ehx = abap_true.
    lv_hex = pa_esc+2(2).
    TRY.
        lv_escape_char = cl_abap_codepage=>convert_from(
                             source      = lv_hex ).
      CATCH cx_parameter_invalid_range.
      CATCH cx_sy_codepage_converter_init.
      CATCH cx_sy_conversion_codepage.
      CATCH cx_parameter_invalid_type.
    ENDTRY.
  ELSE.
    lv_escape_char = pa_esc.
  ENDIF.

  IF pa_shx = abap_true.
    lv_hex = pa_sep+2(2).
    TRY.
        lv_separator_char = cl_abap_codepage=>convert_from(
                                 source      = lv_hex ).
      CATCH cx_parameter_invalid_range.
      CATCH cx_sy_codepage_converter_init.
      CATCH cx_sy_conversion_codepage.
      CATCH cx_parameter_invalid_type.
    ENDTRY.

  ELSE.
    lv_separator_char = pa_sep.
  ENDIF.

  DATA(lobj_adso_bof) = NEW zcl_bw_adso_bof(
    iv_data_sep     = lv_separator_char
    iv_esc_sign     = lv_escape_char
    iv_thousand_sep = pa_tse
    iv_decimal_sep  = pa_dse
    iv_header       = pa_head ).

  lobj_adso_bof->convert_into_columns(
    EXPORTING
      it_data         = lt_output
    IMPORTING
      et_prop_fields  = DATA(lt_prop_fields)
      et_fields       = DATA(lt_fields) ).

  lobj_adso_bof->post_process(
    EXPORTING
      it_prop_fields = lt_prop_fields
      it_fields      = lt_fields
    IMPORTING
      et_adso_fields = DATA(lt_adso_fields) ).

  LOOP AT lt_adso_fields REFERENCE INTO DATA(lr_adso_fields) WHERE datatp = 'QUAN'.
    lr_adso_fields->datatp = 'CHAR'.
  ENDLOOP.

  lobj_adso_bof->check_fields( EXPORTING it_adso_fields    = lt_adso_fields
                               IMPORTING et_adso_corrected = lt_adso_fields ).

  DATA(lt_fieldcat) =  VALUE slis_t_fieldcat_alv(
                   ( fieldname  = 'FIELDNAME' seltext_s = 'FIELD NAME'  edit = abap_true )
                   ( fieldname  = 'LENGTH'    seltext_s = 'LENGTH'      edit = abap_true )
                   ( fieldname  = 'DATATP'    seltext_s = 'DATA TYPE'   edit = abap_true )
                   ( fieldname  = 'DECIMALS'  seltext_s = 'DECIMALS'    edit = abap_true )
                   ( fieldname  = 'KEY'       seltext_s = 'KEY'         edit = abap_true  checkbox = abap_true ) ).

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = sy-repid
      i_callback_user_command = 'USER_COMMAND'
      it_fieldcat             = lt_fieldcat
    TABLES
      t_outtab                = lt_adso_fields
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

FORM user_command USING rcomm TYPE sy-ucomm sel TYPE slis_selfield.

  CASE rcomm.
    WHEN '&DATA_SAVE'.

      LOOP AT lt_adso_fields INTO DATA(ls_adso_fields) WHERE key = abap_true.
        APPEND ls_adso_fields-fieldname TO lt_key.
      ENDLOOP.

      DATA(ls_flags) = VALUE cl_rso_adso_api=>tn_s_adsoflags( direct_update = abap_true  ).

      lobj_adso_bof->check_fields( EXPORTING it_adso_fields    = lt_adso_fields
                                   IMPORTING et_adso_corrected = lt_adso_fields ).

      TRY.
          cl_rso_adso_api=>create(
            EXPORTING
              i_adsonm                      = CONV #( pa_ane )
              i_infoarea                    = pa_area
              i_s_adsoflags                 = ls_flags
              i_t_object                    = CORRESPONDING #( lt_adso_fields )
              i_t_dimension                 = lt_dimension
              i_t_key                       = lt_key
          IMPORTING
            e_t_msg                       =  DATA(lt_msg)
          ).

        CATCH cx_rs_all_msg INTO DATA(lr_msg).

          cl_demo_output=>display(
            EXPORTING
              data = lr_msg->get_longtext( )
              name = 'Error'  ).
      ENDTRY.

      IF pa_lod = abap_true.
        lobj_adso_bof->load_data( EXPORTING it_data             = lt_output
                                            iv_adso_name        = CONV #( pa_ane )
                                            it_adso_fields      = lt_adso_fields
                                   IMPORTING et_msg = DATA(lt_request_msg) ).
      ENDIF.

      IF lt_msg IS NOT INITIAL.
        DATA(lt_ready_msg) = CORRESPONDING t_ty_msg( lt_msg ).
        cl_demo_output=>display(
          EXPORTING
            name = 'ADSO Create'
            data = lt_ready_msg ).
      ENDIF.

      IF lt_request_msg IS NOT INITIAL.
        DATA(lt_ready_request_msg) = CORRESPONDING t_ty_msg( lt_request_msg ).
        cl_demo_output=>display(
         EXPORTING
           name = 'ADSO Request'
           data = lt_ready_request_msg ).
      ENDIF.

  ENDCASE.

ENDFORM.

FORM f4_hex CHANGING cv_param TYPE char4.

  DATA: lt_dd07v  TYPE dd07v_tab,
        ls_dd07v  TYPE dd07v,
        ls_ret    TYPE slis_selfield,
        lt_fields TYPE slis_t_fieldcat_alv.
  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname        = 'RSASCII'
      text           = 'X'
      langu          = sy-langu
    TABLES
      dd07v_tab      = lt_dd07v
    EXCEPTIONS
      wrong_textflag = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            DISPLAY LIKE 'E'.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'DD07V'
    CHANGING
      ct_fieldcat            = lt_fields
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            DISPLAY LIKE 'E'.
  ENDIF.
  DELETE lt_fields WHERE fieldname <> 'DOMVALUE_L'
                    AND   fieldname <> 'DDTEXT'.

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_title       = TEXT-044
      i_tabname     = 'DD07V'
      it_fieldcat   = lt_fields
    IMPORTING
      es_selfield   = ls_ret
    TABLES
      t_outtab      = lt_dd07v
    EXCEPTIONS
      program_error = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            DISPLAY LIKE 'E'.
  ENDIF.

  IF NOT ls_ret-tabindex IS INITIAL.
    READ TABLE lt_dd07v INTO ls_dd07v INDEX ls_ret-tabindex.
    cv_param = ls_dd07v-domvalue_l.

  ENDIF.
ENDFORM.
