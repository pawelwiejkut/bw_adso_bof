REPORT zbw_adso_bof_standalone.
CLASS zcl_bw_adso_bof DEFINITION DEFERRED.
CLASS zcl_bw_adso_bof DEFINITION
     FINAL
  CREATE PUBLIC.
  PUBLIC SECTION.

    TYPES: BEGIN OF t_alv,
             key TYPE abap_bool.
             INCLUDE TYPE cl_rso_adso_api=>tn_s_object.
           TYPES: END OF t_alv.

    TYPES t_ty_alv TYPE STANDARD TABLE OF t_alv.

    METHODS constructor
      IMPORTING iv_data_sep     TYPE char1
                iv_esc_sign     TYPE char1
                iv_thousand_sep TYPE char1
                iv_decimal_sep  TYPE char1
                iv_header       TYPE abap_bool.

    METHODS convert_into_columns
      IMPORTING it_data        TYPE table_of_strings
      EXPORTING et_prop_fields TYPE rsds_t_fields
                et_fields      TYPE rsds_t_fields.

    METHODS post_process
      IMPORTING it_prop_fields TYPE rsds_t_fields
                it_fields      TYPE rsds_t_fields
      EXPORTING et_adso_fields TYPE t_ty_alv.

    METHODS load_data
      IMPORTING it_data        TYPE table_of_strings
                iv_adso_name   TYPE rsoadsonm
                it_adso_fields TYPE t_ty_alv
      EXPORTING et_msg         TYPE rs_t_msg .

    METHODS check_fields
      IMPORTING it_adso_fields    TYPE t_ty_alv
      EXPORTING et_adso_corrected TYPE t_ty_alv.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: gv_data_sep     TYPE char1,
          gv_esc_sign     TYPE char1,
          gv_thousand_sep TYPE char1,
          gv_decimal_sep  TYPE char1,
          gv_header       TYPE abap_bool.

ENDCLASS.
CLASS zcl_bw_adso_bof IMPLEMENTATION.

  METHOD convert_into_columns.

    DATA: lv_ctr         TYPE i,
          lt_string      TYPE table_of_strings,
          lv_field_count TYPE i,
          ls_fields      TYPE rsds_s_fields,
          lt_fields      TYPE rsds_t_fields,
          lv_fieldnm     TYPE rsfieldnm,
          lt_prop_fields TYPE rsds_t_fields,
          lv_typenum     TYPE i.

    LOOP AT it_data INTO DATA(ls_data).
      lv_ctr = lv_ctr + 1.
      CALL FUNCTION 'RSDS_CONVERT_CSV'
        EXPORTING
          i_data_sep       = gv_data_sep
          i_esc_char       = gv_esc_sign
          i_record         = ls_data
          i_field_count    = 999
        IMPORTING
          e_t_data         = lt_string
        EXCEPTIONS
          escape_no_close  = 1
          escape_improper  = 2
          conversion_error = 3
          OTHERS           = 4.

      IF sy-subrc <> 0.
        IF sy-msgno IS NOT INITIAL.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ELSE.
          MESSAGE |Error during file processing, line { lv_ctr } Data:{ ls_data(50) }| TYPE 'E'.
        ENDIF.
      ENDIF.

      IF gv_header = abap_true.
        DATA(lv_ignorelines) = 1.
      ENDIF.

      IF lv_ignorelines > 0 AND lv_ctr = lv_ignorelines.
        LOOP AT lt_string INTO DATA(ls_string).
          lv_field_count = lv_field_count + 1.
          IF ls_string IS NOT INITIAL.
            lv_fieldnm = to_upper( ls_string ).

            CALL FUNCTION 'RSDS_PROPOSE_FIELDNM_CHECK'
              CHANGING
                c_fieldname = lv_fieldnm.

            IF lv_fieldnm IS NOT INITIAL.
              ls_fields-fieldnm = lv_fieldnm.
              ls_fields-txtlg   = ls_string.
            ELSE.
              ls_fields-fieldnm = sy-tabix.
            ENDIF.

          ELSE.
            ls_fields-fieldnm = sy-tabix.

          ENDIF.
          ls_fields-posit = lv_field_count.
          APPEND ls_fields TO lt_fields.

        ENDLOOP.

      ELSEIF lv_ctr > lv_ignorelines.
        DATA(lv_index) = 0.
        LOOP AT lt_string INTO ls_string.

          SHIFT ls_string LEFT DELETING LEADING space.
          lv_index = lv_index + 1.
          READ TABLE lt_prop_fields INTO DATA(ls_prop_fields) INDEX lv_index.

          IF sy-subrc <> 0.
            CLEAR ls_prop_fields.
            APPEND ls_prop_fields TO lt_prop_fields.
          ENDIF.
          IF ls_string IS INITIAL.
            CONTINUE.
          ENDIF.

          TRY.
              CALL FUNCTION 'RS_GET_TYPE_FROM_DATA'
                EXPORTING
                  i_data       = ls_string
                  i_char1000   = gv_thousand_sep
                  i_dezichar   = gv_decimal_sep
                CHANGING
                  e_datatype   = ls_prop_fields-datatype
                  e_typenum    = lv_typenum
                  e_convexit   = ls_prop_fields-convexit
                  e_convtype   = ls_prop_fields-convtype
                  e_leng       = ls_prop_fields-leng
                  e_decimal    = ls_prop_fields-decimals
                  e_lowercase  = ls_prop_fields-lowercase
                  e_unifieldnm = ls_prop_fields-unifieldnm.

              DATA(ls_old_str) = lt_prop_fields[ lv_index ].

              "Avoid changing from other data types to tims or dats.
              IF ls_old_str IS NOT INITIAL.

                IF ls_old_str-datatype <> 'TIMS' AND ls_prop_fields-datatype = 'TIMS' .
                  EXIT.
                ENDIF.

                IF ls_old_str-datatype <> 'DATS' AND ls_prop_fields-datatype = 'DATS' .
                  EXIT.
                ENDIF.

              ENDIF.

              "Convert to adso datatype
              IF ls_prop_fields-datatype+2(2) = '34' OR ls_prop_fields-datatype+2(2) = '16'.
                ls_prop_fields-datatype = |D{ ls_prop_fields-datatype+2(2) }D|.
              ENDIF.

              "Don't shorten lenghts
              IF ls_old_str-leng < ls_prop_fields-leng.
                DATA(lv_leng) = 'LENG'.
              ELSE.
                lv_leng = ''.
              ENDIF.

              "Don't shorten decimals
              IF ls_old_str-decimals < ls_prop_fields-decimals.
                DATA(lv_decimals) = 'DECIMALS'.
              ELSE.
                lv_decimals = ''.
              ENDIF.

              "If once you are discovered as CHAR - be a char til the end
              IF ls_old_str-datatype <> 'CHAR'.
                DATA(lv_datatype) = 'DATATYPE'.
              ELSE.
                lv_datatype = ''.
              ENDIF.

              "If you had comma once, you can'r be a intiger anymore
              IF ( ls_old_str-datatype = 'DEC'
                     OR ls_old_str-datatype = 'QUAN'
                     OR ls_old_str-datatype = 'CURR'
                     OR ls_old_str-datatype = 'FLTP' ) AND
                  ls_prop_fields-datatype CP 'INT*'.
                lv_datatype = ''.
              ELSEIF ls_prop_fields-datatype CP 'INT*'.
                DATA(lv_int_length_new) = ls_prop_fields-datatype+3(1).
                DATA(lv_int_length_old) = ls_old_str-datatype+3(1).
                IF lv_int_length_new > lv_int_length_old.
                  lv_datatype = 'DATATYPE'.
                ELSE.
                  lv_datatype = ''.
                ENDIF.
              ENDIF.

              "Wrong date discovery fix
              IF strlen( ls_string ) > 8 AND ls_prop_fields-datatype = 'DATS'.
                ls_prop_fields-datatype = 'CHAR'.
                ls_prop_fields-leng = strlen( ls_string ).
              ENDIF.

              "Change only required fields
              MODIFY lt_prop_fields FROM ls_prop_fields INDEX lv_index
              TRANSPORTING  ('CONVEXIT') ('CONVTYPE') ('LOWERCASE') ('UNIFIELDNM')
              (lv_datatype) (lv_leng) (lv_decimals).

              "Catch exceptions from RS_GET_TYPE_FROM_DATA
            CATCH cx_root.
*            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

        ENDLOOP.
      ENDIF.

      cl_progress_indicator=>progress_indicate(
                           i_text = |Discovering types in progress|
                           i_processed = lv_ctr
                           i_total = lines( it_data ) ).

    ENDLOOP.
    et_prop_fields = lt_prop_fields.
    et_fields = lt_fields.
  ENDMETHOD.

  METHOD constructor.

    gv_data_sep = iv_data_sep.
    gv_esc_sign = iv_esc_sign.
    gv_thousand_sep = iv_thousand_sep.
    gv_decimal_sep = iv_decimal_sep.
    gv_header = iv_header.

  ENDMETHOD.

  METHOD post_process.

    DATA: lv_index        TYPE i,
          lt_string       TYPE table_of_strings,
          lv_field_count  TYPE i,
          ls_fields       TYPE rsds_s_fields,
          lt_fields       TYPE rsds_t_fields,
          lt_prop_fields  TYPE rsds_t_fields,
          ls_segfd        TYPE rsdssegfd,
          lv_header_count TYPE i,
          lv_line_count   TYPE i.

    lt_prop_fields = it_prop_fields.
    lt_fields = it_fields.
    LOOP AT lt_prop_fields INTO DATA(ls_prop_fields).
      CLEAR ls_fields.
      lv_index = lv_index + 1.

      MOVE-CORRESPONDING ls_prop_fields TO ls_fields.
      ls_fields-outputlen = ls_prop_fields-leng.

      IF ls_fields-datatype IS INITIAL.
        ls_fields-datatype = 'CHAR'.
        ls_fields-leng     = 1.
        ls_fields-transfer = space.
      ELSE.
        ls_fields-transfer = 'X'.
      ENDIF.

      MODIFY lt_fields FROM ls_fields INDEX lv_index TRANSPORTING
          datatype leng convexit convtype decimals outputlen lowercase transfer unifieldnm.

      IF sy-subrc <> 0.
        ls_fields-fieldnm = lv_index.
        ls_fields-txtlg   = lv_index.
        ls_fields-posit = lv_index.
        APPEND ls_fields TO lt_fields.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'RSDS_CUKY_UNIT_CORRECT'
      CHANGING
        c_t_fields = lt_fields.

    LOOP AT lt_fields INTO ls_fields.
      lv_index = sy-tabix.
      CALL FUNCTION 'RSDS_TYPELENGHT_CORRECT'
        EXPORTING
          i_datatype = ls_fields-datatype
          i_leng     = ls_fields-leng
          i_decimals = ls_fields-decimals
        IMPORTING
          e_leng     = ls_fields-leng
          e_decimals = ls_fields-decimals.
      MOVE-CORRESPONDING ls_fields TO ls_segfd.
      CALL FUNCTION 'RSDS_FIELD_GET_CHARLENGTH'
        EXPORTING
          i_s_segfd = ls_segfd
        IMPORTING
          e_length  = ls_fields-outputlen
        EXCEPTIONS
          OTHERS    = 0.

      MODIFY lt_fields FROM ls_fields INDEX lv_index.
    ENDLOOP.

    lv_header_count = lines( lt_fields ).
    lv_line_count = lines( lt_prop_fields ).

    IF lv_header_count > lv_line_count.
      lv_line_count = lv_line_count + 1.
      DELETE lt_fields FROM lv_line_count TO lv_header_count.
    ENDIF.

    et_adso_fields = CORRESPONDING #( lt_fields
            MAPPING fieldname  = fieldnm
                      length   = leng
                      datatp   = datatype
                      decimals = decimals ).

  ENDMETHOD.

  METHOD load_data.

    DATA: lr_adso_table TYPE REF TO data,
          lv_ctr        TYPE i,
          lt_msg        TYPE rs_t_msg,
          lv_cnt        TYPE i,
          lt_output     TYPE table_of_strings.

    FIELD-SYMBOLS: <lt_adso> TYPE STANDARD TABLE.

    TRY.
        DATA(lt_tabname) = cl_rso_adso=>get_tablnm(
          EXPORTING
            i_adsonm        = CONV #( iv_adso_name ) ).
      CATCH cx_rs_not_found.
    ENDTRY.

    TRY.
        DATA(lv_table) = lt_tabname[ dsotabtype = 'AT' ]-name.
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    CREATE DATA lr_adso_table TYPE STANDARD TABLE OF (lv_table).
    ASSIGN lr_adso_table->* TO <lt_adso>.

    LOOP AT it_data INTO DATA(ls_data).
      lv_ctr = lv_ctr + 1.
      CALL FUNCTION 'RSDS_CONVERT_CSV'
        EXPORTING
          i_data_sep       = gv_data_sep
          i_esc_char       = gv_esc_sign
          i_record         = ls_data
          i_field_count    = 999
        IMPORTING
          e_t_data         = lt_output
        EXCEPTIONS
          escape_no_close  = 1
          escape_improper  = 2
          conversion_error = 3
          OTHERS           = 4.

      IF sy-subrc <> 0.
        IF sy-msgno IS NOT INITIAL.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ELSE.
          MESSAGE |Error during file processing, line { lv_ctr } Data:{ ls_data(50) }| TYPE 'E'.
        ENDIF.
      ENDIF.

      IF gv_header = abap_true AND lv_ctr = 1.
        CONTINUE.
      ENDIF.

      cl_progress_indicator=>progress_indicate(
                         i_text = 'Loading data to DSO in progress'
                         i_output_immediately = abap_true ).

      APPEND INITIAL LINE TO <lt_adso> ASSIGNING FIELD-SYMBOL(<ls_adso>).
      DATA(lobj_ref) = CAST cl_abap_structdescr(
                            cl_abap_typedescr=>describe_by_data( p_data = <ls_adso>  ) ).
      DATA(lv_lengt) = lines( it_adso_fields ).

      DO lv_lengt TIMES.

        lv_cnt = lv_cnt + 1.
        DATA(ls_adso_fields) =  it_adso_fields[ lv_cnt ] .

        ASSIGN COMPONENT ls_adso_fields-fieldname OF STRUCTURE <ls_adso> TO FIELD-SYMBOL(<lv_adso_field>).

        DATA(lv_kind) = lobj_ref->components[ name = ls_adso_fields-fieldname ]-type_kind.

        TRY.
            DATA(lv_output) = lt_output[ lv_cnt ].

            IF lv_kind = 'P' OR lv_kind = 'F' .

              IF gv_decimal_sep IS NOT INITIAL.
                REPLACE ALL OCCURRENCES OF gv_decimal_sep IN lv_output WITH '.'.
              ENDIF.

              IF gv_thousand_sep IS NOT INITIAL.
                REPLACE ALL OCCURRENCES OF gv_thousand_sep IN lv_output WITH ''.
              ENDIF.
              CONDENSE lv_output.

            ENDIF.

            <lv_adso_field> = lv_output.

          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

      ENDDO.
      CLEAR lv_cnt.
    ENDLOOP.

    CALL FUNCTION 'RSDSO_DU_WRITE_API'
      EXPORTING
        i_adsonm            = iv_adso_name
        it_data             = <lt_adso>
      IMPORTING
        et_msg              = lt_msg
      EXCEPTIONS
        write_failed        = 1
        datastore_not_found = 2
        OTHERS              = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    et_msg = lt_msg.

  ENDMETHOD.

  METHOD check_fields.

    DATA: lv_prev_fieldname TYPE string.
    DATA(lt_fields) = it_adso_fields.
    DATA(lt_adso_fields_result) = it_adso_fields.

    SORT lt_fields BY fieldname.

    LOOP AT lt_fields REFERENCE INTO DATA(lr_field).

      IF lr_field->fieldname = lv_prev_fieldname.

        READ TABLE lt_adso_fields_result WITH KEY fieldname = lr_field->fieldname
        REFERENCE INTO DATA(lr_adso_field).

        DATA(lv_char_trim) = strlen( lr_adso_field->fieldname ) - 1.

        lr_adso_field->fieldname = |{ lr_adso_field->fieldname(lv_char_trim) }|.

        lv_prev_fieldname = lr_field->fieldname.

      ENDIF.

      lv_prev_fieldname = lr_field->fieldname.

    ENDLOOP.
    SELECT *
    FROM trese
    INTO TABLE @DATA(lt_trese)
    FOR ALL ENTRIES IN @it_adso_fields
    WHERE name = @it_adso_fields-fieldname.

    LOOP AT lt_trese REFERENCE INTO DATA(lr_trese).

      READ TABLE lt_adso_fields_result WITH KEY fieldname = lr_trese->name
      REFERENCE INTO DATA(lr_asdo_fields_result).

      lv_char_trim = strlen( lr_asdo_fields_result->fieldname ) - 1.

      lr_asdo_fields_result->fieldname = |{ lr_asdo_fields_result->fieldname(lv_char_trim) }|.

    ENDLOOP.

    et_adso_corrected = lt_adso_fields_result.

  ENDMETHOD.

ENDCLASS.

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
      lv_prev_fieldname TYPE string,
      lt_adso_fields    TYPE zcl_bw_adso_bof=>t_ty_alv.

SELECTION-SCREEN BEGIN OF BLOCK part1 WITH FRAME TITLE TEXT-b01.
PARAMETERS: pa_path TYPE string LOWER CASE OBLIGATORY,
            pa_line TYPE i OBLIGATORY.
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

    cl_progress_indicator=>progress_indicate(
                         i_text = 'Uploading in progress'
                         i_output_immediately = abap_true ).

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

  IF pa_line = 1 AND pa_head = abap_true.

    MESSAGE 'If header is selected, please provide at least 2 lines'
    TYPE 'S' DISPLAY LIKE 'W'.
    EXIT.

  ENDIF.

  LOOP AT lt_output REFERENCE INTO DATA(lr_output).
    IF sy-tabix > pa_line.
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
        et_adso_fields = lt_adso_fields ).

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

      IF lines( lt_key ) = 0 .

        MESSAGE 'This report creates Direct Update ADSO, so at least one key is required'
        TYPE 'S' DISPLAY LIKE 'W'.
        EXIT.

      ENDIF.

      DATA(ls_flags) = VALUE cl_rso_adso_api=>tn_s_adsoflags( direct_update = abap_true  ).

      lobj_adso_bof->check_fields( EXPORTING it_adso_fields    = lt_adso_fields
                                   IMPORTING et_adso_corrected = lt_adso_fields ).

      DATA(lv_adso_exists) = cl_rso_adso_api=>exist( i_adsonm = CONV #( pa_ane ) ).

      IF lv_adso_exists = abap_true.

        MESSAGE 'ADSO with this name already exist, please go back and change the name'
        TYPE 'S' DISPLAY LIKE 'W'.
        EXIT.

      ENDIF.

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

****************************************************
INTERFACE lif_abapmerge_marker.
* abapmerge 0.14.7 - 2022-07-04T15:45:47.722Z
ENDINTERFACE.
****************************************************