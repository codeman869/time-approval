*&---------------------------------------------------------------------*
*& Report  ZMSS_TIME_APPROVAL_REPORT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ymss_time_approval_report.

INCLUDE ymss_time_approval_report01.
DATA cl_receiver TYPE REF TO lcl_event_receiver.
"Pay period
SELECTION-SCREEN BEGIN OF BLOCK var4 WITH FRAME TITLE text-t04.
SELECTION-SCREEN BEGIN OF LINE .
SELECTION-SCREEN COMMENT 1(28) text-t05 FOR FIELD s_f_prp.
SELECT-OPTIONS : s_f_prp FOR frm_pabrp NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN COMMENT 31(1) text-t06 FOR FIELD s_f_prj.
SELECT-OPTIONS : s_f_prj FOR frm_pabrj NO-EXTENSION NO INTERVALS.

SELECTION-SCREEN COMMENT 52(2) text-t07 FOR FIELD s_t_prp.
SELECT-OPTIONS : s_t_prp FOR to_pabrp NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN COMMENT 55(2) text-t10 FOR FIELD s_t_prj.
SELECT-OPTIONS : s_t_prj FOR to_pabrj NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN END OF LINE .
SELECTION-SCREEN END OF BLOCK var4.

"By Manager
SELECTION-SCREEN BEGIN OF BLOCK vari WITH FRAME TITLE text-t02.
*PARAMETERS   : p_plans TYPE plans MATCHCODE OBJECT plom.
SELECT-OPTIONS : s_m_pern FOR lv_m_pernr MATCHCODE OBJECT prem.

SELECT-OPTIONS : s_m_pers FOR lv_m_persa MATCHCODE OBJECT hrpad_persa.
SELECT-OPTIONS : s_m_plan FOR lv_m_plans NO INTERVALS.
*PARAMETERS   : p_persa TYPE persa MATCHCODE OBJECT hrpad_persa.
SELECTION-SCREEN END OF BLOCK vari.

"By Employee
SELECTION-SCREEN BEGIN OF BLOCK var2 WITH FRAME TITLE text-t01.
SELECT-OPTIONS : s_e_pern FOR lv_e_pernr MATCHCODE OBJECT prem.

SELECT-OPTIONS : s_e_pers FOR lv_e_persa MATCHCODE OBJECT hrpad_persa.
SELECT-OPTIONS : s_e_plan FOR lv_e_plans NO INTERVALS.
SELECTION-SCREEN END OF BLOCK var2.

"By Time Keeper
SELECTION-SCREEN BEGIN OF BLOCK var3 WITH FRAME TITLE text-t03.
SELECT-OPTIONS : s_t_pern FOR lv_e_pernr MATCHCODE OBJECT prem.

SELECT-OPTIONS : s_t_pers FOR lv_e_persa MATCHCODE OBJECT hrpad_persa.
*SELECT-OPTIONS : s_t_plan FOR lv_t_plans NO INTERVALS.

SELECTION-SCREEN END OF BLOCK var3.
*
*SELECTION-SCREEN BEGIN OF BLOCK var8 WITH FRAME TITLE text-t08.
*  SELECTION-SCREEN COMMENT 1(28) for field s_file.
*SELECT-OPTIONS : s_file FOR lv_filename NO-EXTENSION NO INTERVALS.
*
*
*
*SELECTION-SCREEN END OF BLOCK var8.

INITIALIZATION.

*  LV_CUT_OFF = P_DATE - 90. " ~ Sx Pay periods from today
*
** If the cutover date is less than Jun 06 2013, then set it to that
*date
*  IF LV_CUT_OFF LT '20130606'.
*    LV_CUT_OFF = '20130606'.
*  ENDIF.


  CALL FUNCTION 'HR_MX_GET_PAYROLL_PERIOD'
    EXPORTING
      payroll_area   = 'Z1'
*     DATE           = SY-DATUM
    IMPORTING
      payroll_year   = wa_period-pabrj
      payroll_period = wa_period-pabrp
      period_begin   = wa_period-begda
      period_end     = wa_period-endda
    EXCEPTIONS
      t549a_error    = 1
      t549q_error    = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

*==============================================
*
*Search Helps
*
*==============================================

*
  "AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_T_PLAN-LOW .
  "  PERFORM F4_HELP_OBJ USING 'Timekeeper'.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_T_PLAN-HIGH .
*  PERFORM F4_HELP_OBJ USING 'Timekeeper'.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_m_plan-low .
  PERFORM f4_help_obj USING 'Manager'.

*  AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_M_PLAN-HIGH .
*  PERFORM F4_HELP_OBJ USING 'Manager'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_e_plan-low .
  PERFORM f4_help_obj USING 'Employee'.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_E_PLAN-HIGH .
*  PERFORM F4_HELP_OBJ USING 'Employee'.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_file-low.
*  PERFORM get_filename.


AT SELECTION-SCREEN.
*Determine valid payroll periods & order

*Ensure a payroll period and year are specified

  IF s_t_prp IS INITIAL AND s_t_prj IS INITIAL.
  ELSEIF s_t_prp IS NOT INITIAL AND s_t_prj IS NOT INITIAL.
  ELSE.
    MESSAGE e070(zhr_msg).
  ENDIF.


  IF s_f_prp IS INITIAL AND s_f_prj IS INITIAL.
  ELSEIF s_f_prp IS NOT INITIAL AND s_f_prj IS NOT INITIAL.
  ELSE.
    MESSAGE e070(zhr_msg).

  ENDIF.

*Determines that payroll period is between 1 & 26
  IF s_f_prp IS NOT INITIAL AND
    ( s_f_prp-low < 1 OR s_f_prp-low > 26 ).

    MESSAGE e070(zhr_msg).
  ENDIF.

  IF s_t_prp IS NOT INITIAL AND
    ( s_t_prp-low < 1 OR s_t_prp-low > 26 ).

    MESSAGE e070(zhr_msg).
  ENDIF.

*If there is a second payroll period, make sure it happens after the
*first payroll period

  IF s_t_prp IS NOT INITIAL.

    "Payroll periods fall in different years

    IF s_f_prj-low > s_t_prj-low.
      MESSAGE e071(zhr_msg).
    ENDIF.

    "Payroll periods fall in the same year

    IF s_f_prj-low = s_t_prj-low AND s_f_prp-low > s_t_prp-low.
      MESSAGE e071(zhr_msg).
    ENDIF.

  ENDIF.




START-OF-SELECTION.



*SET PF-STATUS 'STATUS'.

*  APP_DATA-MGR_POS = P_PLANS.

  PERFORM collect_pernrs.
*  PERFORM SET_POSITION.
  PERFORM set_employees.
  PERFORM fill_time_data.

END-OF-SELECTION.

  PERFORM display_output.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_output .

  DATA: wa_cellcolor TYPE lvc_s_scol.


  LOOP AT it_emp_data INTO wa_emp_data.

    MOVE-CORRESPONDING wa_emp_data TO wa_out.

    CASE wa_out-emp_cert.
      WHEN 'Certified'.
        wa_out-ee_cert_stat = icon_green_light.
      WHEN 'Pending'.
        wa_out-ee_cert_stat = icon_yellow_light.
      WHEN OTHERS.
        wa_out-ee_cert_stat = icon_red_light.
    ENDCASE.

    CASE wa_out-mgr_cert.
      WHEN 'Approved'.
        wa_out-mgr_cert_stat = icon_green_light.
      WHEN 'Pending'.
        wa_out-mgr_cert_stat = icon_yellow_light.
      WHEN OTHERS.
        wa_out-mgr_cert_stat = icon_red_light.
    ENDCASE.

    IF wa_emp_data-emp_type_col <> 0.
      PERFORM change_cell_color USING 7 'PERNR' CHANGING wa_out.
    ENDIF.

*    wa_out-sel = 'X'.

    APPEND wa_cellcolor TO wa_out-cel_color.

    APPEND wa_out TO it_out.
    CLEAR wa_out.
  ENDLOOP.

  DATA: "fcat TYPE LVC_T_FCAT,
          g_repid LIKE sy-repid ",
*          s_layout              TYPE LVC_S_LAYO.
        .
**  s_layout-detail_popup = 'X'.
***  S_LAYOUT-ZEBRA = 'X'.
**  s_layout-colwidth_optimize = 'X'.
**  s_layout-colwidth_optimize = 'X'.
**  s_layout-group_buttons = 'X'.
*  s_layout-coltab_fieldname = 'CEL_COLOR'.
  s_layout-ctab_fname = 'CEL_COLOR'.
  s_layout-cwidth_opt = 'X'.
*  *s_layout-BOX_FNAME = 'SEL'.
  s_layout-sel_mode = 'C'.
  FIELD-SYMBOLS <fs> LIKE LINE OF fcat.
  g_repid = sy-repid.

*  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
*   EXPORTING
*     i_program_name               = g_repid
*     i_internal_tabname           = 'ALV_OUT4'
**     I_STRUCTURE_NAME             =
**     I_CLIENT_NEVER_DISPLAY       = 'X'
*     i_inclname                   = g_repid
**     I_BYPASSING_BUFFER           =
**     I_BUFFER_ACTIVE              =
*    CHANGING
*      ct_fieldcat                  = fcat
**   EXCEPTIONS
**     INCONSISTENT_INTERFACE       = 1
**     PROGRAM_ERROR                = 2
**     OTHERS                       = 3
*            .
*  IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
   EXPORTING
*   I_BUFFER_ACTIVE              =
     i_structure_name             = 'ZHR_TM_APPR_REPORT_ALV'
*   I_CLIENT_NEVER_DISPLAY       = 'X'
*   I_BYPASSING_BUFFER           =
*   I_INTERNAL_TABNAME           =
    CHANGING
      ct_fieldcat                  = fcat
   EXCEPTIONS
     inconsistent_interface       = 1
     program_error                = 2
     OTHERS                       = 3
            .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.



  IF fcat IS NOT INITIAL.

    LOOP AT fcat ASSIGNING <fs>.
      CASE <fs>-fieldname.
        WHEN 'BEGDA'.
          <fs>-scrtext_s = 'Begda'.
          <fs>-scrtext_l = 'Beginning Date'.
          <fs>-scrtext_m = 'Beg Date'.
        WHEN 'ENDDA'.
          <fs>-scrtext_s = 'Endda'.
          <fs>-scrtext_l = 'Ending Date'.
          <fs>-scrtext_m = 'End Date'.
        WHEN 'PLAN_HRS'.
          <fs>-scrtext_s = 'Plan Hrs'.
          <fs>-scrtext_l = 'Planned hours'.
          <fs>-scrtext_m = 'Plan hours'.
        WHEN 'ACT_HRS'.
          <fs>-scrtext_s = 'Act Hrs'.
          <fs>-scrtext_l = 'Actual hours'.
          <fs>-scrtext_m = 'Act hours'.
        WHEN 'ATT_HRS'.
          <fs>-scrtext_s = 'Att Hrs'.
          <fs>-scrtext_l = 'Attendance hours'.
          <fs>-scrtext_m = 'Att hours'.
        WHEN 'ABS_HRS'.
          <fs>-scrtext_s = 'Abs Hrs'.
          <fs>-scrtext_l = 'Absence hours'.
          <fs>-scrtext_m = 'Abs hours'.
        WHEN 'EE_CERT_STAT'.

          <fs>-scrtext_s = 'EE Cert'.
          <fs>-scrtext_l = 'Employee Cert Status'.
          <fs>-scrtext_m = 'EE Cert Status'.

        WHEN 'MGR_CERT_STAT'.
          <fs>-scrtext_s =  'MGR Cert'.
          <fs>-scrtext_l = 'Manager Cert Status'.
          <fs>-scrtext_m = 'MGR Cert Status'.
        WHEN 'PA'.
          <fs>-scrtext_s = 'Pers. Area'.
          <fs>-scrtext_l = 'Personnel Area'.
          <fs>-scrtext_m = 'Personnel A'.
          <fs>-no_out = 'X'.

        WHEN 'PA_TXT'.
          <fs>-scrtext_s = 'Pers. Area'.
          <fs>-scrtext_l = 'Personnel Area'.
          <fs>-scrtext_m = 'Personnel A'.
        WHEN 'PERSG'.
          <fs>-seltext = 'Emp. Grp'.
          <fs>-no_out = 'X'.

        WHEN 'PGTXT'.
          <fs>-seltext = 'Emp. Grp'.

*        WHEN 'ORG'.
*          <FS>-SELTEXT_L = <FS>-SELTEXT_M = <FS>-REPTEXT_DDIC
*          = <FS>-SELTEXT_S = 'Org. Unit'.
        WHEN 'PP'.

          <fs>-scrtext_s = 'Pay period'.
          <fs>-scrtext_l = 'Pay period'.
          <fs>-scrtext_m = 'Pay period'.
          <fs>-key = ' '.
        WHEN 'PERNR'.
          <fs>-scrtext_s = 'PERNR'.
          <fs>-scrtext_l = 'Personnel Number'.
          <fs>-scrtext_m = 'Personnel N'.
        WHEN 'EMP_CERT'.
          <fs>-scrtext_s = 'Emp. Cert.'.
          <fs>-scrtext_l = 'Employee Cert'.
          <fs>-scrtext_m = 'Emp. Cert.'.
        WHEN 'MGR_CERT'.
          <fs>-scrtext_s = 'Mgr. Appr.'.
          <fs>-scrtext_l = 'Manager Approval'.
          <fs>-scrtext_m = 'Mgr Approval'.
          <fs>-just = 'C'.

        WHEN 'NO_TIME'.
          <fs>-seltext = 'No Time'.
          <fs>-just = 'C'.

        WHEN 'PROFL'.
          <fs>-seltext = 'PD Profile'.
          <fs>-key = ' '.

        WHEN 'PORTAL'.
          <fs>-seltext = 'Portal Role'.
          <fs>-just = 'C'.

        WHEN 'MSS'.
          <fs>-seltext = 'MSS Backend Role'.
          <fs>-just = 'C'.

        WHEN 'MSS_X'.
          <fs>-seltext = 'MSS Portal Role'.
          <fs>-just = 'C'.

        WHEN 'TIME_X'.
          <fs>-seltext = 'Time Entry (New)'.
          <fs>-just = 'C'.

        WHEN 'NO_TIME_X'.
          <fs>-seltext = 'No Time (New)'.
          <fs>-just = 'C'.

        WHEN 'PROFL_X'.
          <fs>-seltext = 'PD Profile (New)'.
          <fs>-key = ' '.

        WHEN 'PORTAL_X'.
          <fs>-seltext = 'Portal Role (New)'.
          <fs>-just = 'C'.

        WHEN 'MGR_NAME'.
          <fs>-scrtext_s = 'Manager Name'.
          <fs>-scrtext_l = 'Manager Name'.
          <fs>-scrtext_m = 'Manager Name'.

        WHEN 'EMP_NAME'.

          <fs>-scrtext_s = 'Employee Name'.
          <fs>-scrtext_l = 'Employee Name'.
          <fs>-scrtext_m = 'Employee Name'.
*        WHEN 'BEGDA'.
*          <FS>-SELTEXT_L = <FS>-SELTEXT_M = <FS>-REPTEXT_DDIC
*          = <FS>-SELTEXT_S = 'Valid from'.
*        WHEN 'ENDDA'.
*          <FS>-SELTEXT_L = <FS>-SELTEXT_M = <FS>-REPTEXT_DDIC
*          = <FS>-SELTEXT_S = 'Valid to'.
*        WHEN 'STEXT'.
*          <FS>-SELTEXT_L = <FS>-SELTEXT_M = <FS>-REPTEXT_DDIC
*           = <FS>-SELTEXT_S = 'Desc'.
*        WHEN 'ACTION'.
*          <FS>-SELTEXT_L = <FS>-SELTEXT_M = <FS>-REPTEXT_DDIC
*          = <FS>-SELTEXT_S = 'Action'.
*
*
*        WHEN 'SYSTEM'.
*          <FS>-SELTEXT_L = <FS>-SELTEXT_M = <FS>-REPTEXT_DDIC
*          = <FS>-SELTEXT_S = 'System'.
*
*        WHEN 'R_BEGDA'.
*          <FS>-SELTEXT_L = <FS>-SELTEXT_M = <FS>-REPTEXT_DDIC
*          = <FS>-SELTEXT_S = 'Valid from'.
*        WHEN 'R_ENDDA'.
*          <FS>-SELTEXT_L = <FS>-SELTEXT_M = <FS>-REPTEXT_DDIC
*          = <FS>-SELTEXT_S = 'Valid to'.
*        WHEN 'SOBID'.
*          <FS>-SELTEXT_L = <FS>-SELTEXT_M = <FS>-REPTEXT_DDIC
*          = <FS>-SELTEXT_S = 'Role'.
*        WHEN 'ROLE_TXT'.
*          <FS>-SELTEXT_L = <FS>-SELTEXT_M = <FS>-REPTEXT_DDIC
*          = <FS>-SELTEXT_S = 'Desc'.

        WHEN 'COMMENT'.
          <fs>-seltext = 'Comments'.
          <fs>-just = 'C'.

        WHEN 'UPD'.
          <fs>-seltext = 'Update'.
          <fs>-just = 'C'.

      ENDCASE.

      <fs>-col_opt = 'X'.
    ENDLOOP.

  ENDIF.

*  IF P_TEST = ' '.
*    DELETE IT_USERS WHERE STATUS NE 'Updated'.
*  ENDIF.


* Sort ALV Output

  wa_sort-spos = 1.
  wa_sort-fieldname = 'OBJID'.
  wa_sort-up = 'X'.
  APPEND wa_sort TO it_sort.

  wa_sort-spos = 2.
  wa_sort-fieldname = 'STEXT'.
  wa_sort-up = 'X'.
  APPEND wa_sort TO it_sort.

  wa_sort-spos = 3.
  wa_sort-fieldname = 'SYSTEM'.
  wa_sort-up = 'X'.
  APPEND wa_sort TO it_sort.

  wa_sort-spos = 4.
  wa_sort-fieldname = 'PA'.
  wa_sort-up = 'X'.
  APPEND wa_sort TO it_sort.

  wa_sort-spos = 5.
  wa_sort-fieldname = 'ORG'.
  wa_sort-up = 'X'.
  APPEND wa_sort TO it_sort.

  wa_sort-spos = 6.
  wa_sort-fieldname = 'PERNR'.
  wa_sort-up = 'X'.
  APPEND wa_sort TO it_sort.

  wa_sort-spos = 7.
  wa_sort-fieldname = 'USRID'.
  wa_sort-up = 'X'.
  APPEND wa_sort TO it_sort.

  wa_sort-spos = 8.
  wa_sort-fieldname = 'ENAME'.
  wa_sort-up = 'X'.
  APPEND wa_sort TO it_sort.
  IF it_out IS NOT INITIAL.
*
*    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*     EXPORTING
*
*       i_callback_program                = g_repid
**   I_CALLBACK_PF_STATUS_SET          =  'STATUS'
**   I_CALLBACK_USER_COMMAND           = ' '
**   I_CALLBACK_TOP_OF_PAGE            = ' '
**   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
**   I_CALLBACK_HTML_END_OF_LIST       = ' '
**   I_STRUCTURE_NAME                  =
**   I_BACKGROUND_ID                   = ' '
**   I_GRID_TITLE                      =
**   I_GRID_SETTINGS                   =
*     is_layout                         = s_layout
*       it_fieldcat                       = fcat
**       IT_SORT                           = IT_SORT " Commented for now
*      TABLES
*        t_outtab                          = it_out
** EXCEPTIONS
**   PROGRAM_ERROR                     = 1
**   OTHERS                            = 2
*              .
*    IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*
    IF custom_container IS INITIAL.

      PERFORM setup_objects.

    ENDIF.


*  CALL METHOD alv_grid->set_table_for_first_display
*      EXPORTING
**        i_buffer_active               =
**        i_bypassing_buffer            =
**        i_consistency_check           =
**        i_structure_name              =
**        is_variant                    =
**        i_save                        =
**        i_default                     = 'X'
*        is_layout                     = S_LAYOUT
*        is_print                      = print_params
**        it_special_groups             =
**        it_toolbar_excluding          =
**        it_hyperlink                  =
**        it_alv_graphics               =
**        it_except_qinfo               =
**        ir_salv_adapter               =
*      CHANGING
*        it_outtab                     = IT_OUT[]
*        it_fieldcatalog               = FCAT[]
**        it_sort                       =
**        it_filter                     =
*      EXCEPTIONS
*        invalid_parameter_combination = 1
*        program_error                 = 2
*        too_many_lines                = 3
*        others                        = 4
*            .
*    IF sy-subrc <> 0.
**     Implement suitable error handling here
*    ENDIF.



    CALL METHOD alv_grid->set_table_for_first_display
      EXPORTING
*        i_buffer_active               =
*        i_bypassing_buffer            =
*        i_consistency_check           =
*        i_structure_name              =
*        is_variant                    =
*        i_save                        =
*        i_default                     = 'X'
        is_layout                     = s_layout
*        is_print                      =
*        it_special_groups             =
*        it_toolbar_excluding          =
*        it_hyperlink                  =
*        it_alv_graphics               =
*        it_except_qinfo               =
*        ir_salv_adapter               =
      CHANGING
        it_outtab                     = it_out[]
        it_fieldcatalog               = fcat[]
*        it_sort                       =
*        it_filter                     =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4
            .
    IF sy-subrc <> 0.
*     Implement suitable error handling here
    ENDIF.

    CALL SCREEN 100.

  ENDIF.
ENDFORM.                    " DISPLAY_OUTPUT

*&---------------------------------------------------------------------*
*&      Form  CHANGE_CELL_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WA_OUT_CEL_COLOR  text
*----------------------------------------------------------------------*
FORM change_cell_color  USING p_color
                              p_field
                        CHANGING p_out TYPE ty_out.
  DATA: v_color      TYPE lvc_s_scol.  "For cell colors
  v_color-fname = p_field.
  v_color-color-col = p_color.   "Color cell red.
  v_color-color-int = 1.
  APPEND v_color TO p_out-cel_color.


ENDFORM.                    " CHANGE_CELL_COLOR
*&---------------------------------------------------------------------*
*&      Form  SET_POSITION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_position .

  DATA v_usr TYPE sysid.
  DATA: wa_1008 TYPE hrp1008.
  DATA: it_disp TYPE TABLE OF tree_objec,
        wa_disp TYPE tree_objec.

  CALL FUNCTION 'RH_OM_GET_HOLDER_OF_POSITION'
    EXPORTING
      plvar           = '01'
      otype           = 'S'
      objid           = app_data-mgr_pos
*     REFRESH         = 'X'
    TABLES
      disp_tab        = it_disp
*     RELAT_TAB       =
    EXCEPTIONS
      no_active_plvar = 1
      OTHERS          = 2.

  IF it_disp[] IS NOT INITIAL.

    LOOP AT it_disp INTO wa_disp WHERE
        plvar = '01'
     AND otype = 'P'
     AND begda LE sy-datum
     AND endda GE sy-datum
     AND istat EQ 1.

      EXIT.

    ENDLOOP.
  ENDIF.

  IF wa_disp IS NOT INITIAL. " IF a valid holder exists

    MOVE: wa_disp-objid TO app_data-mgr_id,
          wa_disp-stext  TO app_data-mgr_name.

    CALL FUNCTION 'CATSXT_GET_USER_ID_OF_PERNR'
      EXPORTING
        im_personnel_number = app_data-mgr_id
      IMPORTING
        ex_user_id          = app_data-mgr_user_name
      EXCEPTIONS
        failed              = 1
        OTHERS              = 2.
  ELSE.
    app_data-mgr_name = 'Vacant Position'.
  ENDIF.




  CALL FUNCTION 'HR_READ_FOREIGN_OBJECT_TEXT'
    EXPORTING
      otype                   = 'S'
      objid                   = app_data-mgr_pos
      begda                   = sy-datum
      endda                   = sy-datum
    IMPORTING
      object_text             = app_data-mgr_pos_text
    EXCEPTIONS
      nothing_found           = 1
      wrong_objecttype        = 2
      missing_costcenter_data = 3
      missing_object_id       = 4
      OTHERS                  = 5.



  SELECT SINGLE * FROM hrp1008 INTO wa_1008 WHERE
    plvar = '01' AND
  otype = 'S' AND
  objid = app_data-mgr_pos AND
  istat = 1 AND
  begda LE sy-datum AND
  endda GE sy-datum.

  app_data-mgr_persa = wa_1008-persa.



ENDFORM.                    " SET_POSITION
*&---------------------------------------------------------------------*
*&      Form  SET_EMPLOYEES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_employees .

  DATA: it_list TYPE wdy_key_value_list.
  DATA: wa_list TYPE wdy_key_value.
  DATA: v_persk TYPE persk.
  DATA: lv_pernr TYPE pernr_d.


*  LOOP AT LT_SUBSTITUTIONS INTO LS_SUBSTITUTIONS.

*  CALL METHOD ZCL_HRPAOM_UTILITIES=>GET_EMPLOYEES_MANGPOS
*    EXPORTING
*      IM_POS       = APP_DATA-MGR_POS
*      IM_BEGDA     = SY-DATUM " LS_PP_DATA-PERIOD_BEGIN
*      IM_ENDDA     = SY-DATUM " LS_PP_DATA-PERIOD_END
*    IMPORTING
*      EX_EMPLOYEES = IT_LIST.

*  LOOP AT IT_LIST INTO WA_LIST.
*
*    MOVE: WA_LIST-VALUE TO WA_EMP_DATA-EMP_NAME,
*        APP_DATA-MGR_NAME TO WA_EMP_DATA-MGR_NAME,
*        WA_LIST-KEY TO WA_EMP_DATA-PERNR.


  LOOP AT it_pernrs INTO lv_pernr.

    MOVE: "WA_LIST-VALUE TO WA_EMP_DATA-EMP_NAME,
         "APP_DATA-MGR_NAME TO WA_EMP_DATA-MGR_NAME,
        wa_list-key TO wa_emp_data-pernr.
    wa_emp_data-pernr = lv_pernr.

**Set employee type ( Positive or Negative employee )
*    SELECT SINGLE persk  FROM pa0001 INTO v_persk WHERE
*                pernr = wa_emp_data-pernr
*           AND  endda GE sy-datum
*           AND begda LE sy-datum.
*
*    CASE v_persk.
*      WHEN '02'.
*        wa_emp_data-emp_type = 'Positive Pay Employee'.
*        wa_emp_data-emp_type_col = 03.
*      WHEN '06'.
*        wa_emp_data-emp_type = 'Positive Pay Employee'.
*        wa_emp_data-emp_type_col = 03.
*      WHEN '10'.
*        wa_emp_data-emp_type = 'Positive Pay Employee'.
*        wa_emp_data-emp_type_col = 03.
*      WHEN '18'.
*        wa_emp_data-emp_type = 'Positive Pay Employee'.
*        wa_emp_data-emp_type_col = 03.
*      WHEN '19'.
*        wa_emp_data-emp_type = 'Positive Pay Employee'.
*        wa_emp_data-emp_type_col = 03.
*      WHEN '20'.
*        wa_emp_data-emp_type = 'Positive Pay Employee'.
*        wa_emp_data-emp_type_col = 03.
*      WHEN '21'.
*        wa_emp_data-emp_type = 'Positive Pay Employee'.
*        wa_emp_data-emp_type_col = 03.
*      WHEN '22'.
*        wa_emp_data-emp_type = 'Positive Pay Employee'.
*        wa_emp_data-emp_type_col = 03.
*      WHEN OTHERS.
*        wa_emp_data-emp_type = 'Negative Pay Employee'.
*        wa_emp_data-emp_type_col = 00.
*    ENDCASE.

    APPEND wa_emp_data TO it_emp_data.
  ENDLOOP.


  DATA: it_9004 TYPE TABLE OF pa9004,
        wa_9004 TYPE pa9004.

  IF it_emp_data[] IS NOT INITIAL.

    it_emp_datax[] = it_emp_data[].

    SELECT * FROM pa9004 INTO TABLE it_9004
                         FOR ALL ENTRIES IN it_emp_data
                         WHERE pernr = it_emp_data-pernr.

*============================================
*
*
*Delete pay periods based on selection screen
*
*
*============================================
*BREAK CDECKARD.
    IF s_f_prp IS NOT INITIAL AND s_f_prj IS NOT INITIAL.
      IF s_t_prp IS NOT INITIAL AND s_t_prj IS NOT INITIAL.
        DELETE it_9004 WHERE pabrj LT s_f_prj-low.
        "Remove years less than from selection
        DELETE it_9004 WHERE pabrj GT s_t_prj-low.
        "Remove entries with years greater than to selection
       DELETE it_9004 WHERE pabrj = s_f_prj-low AND pabrp < s_f_prp-low.
    "Remove pay periods less than from selection, where year is the same
       DELETE it_9004 WHERE pabrj = s_t_prj-low AND pabrp > s_t_prp-low.
   "Remove pay periods greater than to selection, where year is the same
      ELSE. "Otherwise, it's a single selection
        DELETE it_9004 WHERE pabrj <> s_f_prj-low.
        DELETE it_9004 WHERE pabrp <> s_f_prp-low.
      ENDIF.
    ENDIF.

    TYPES: BEGIN OF ty_catsdata,
      catshours TYPE catsdb-catshours,
      art01 TYPE t554s-art01,
      awart TYPE catsdb-awart,

    END OF ty_catsdata.

    DATA: lv_begda TYPE datum,
                  lv_endda TYPE datum,
                  it_hrs TYPE TABLE OF cats_hours_per_day,
                  wa_hrs TYPE cats_hours_per_day,
                  lv_hrs TYPE catssum,
                  lv_abs TYPE catssum,
                  lv_att TYPE catssum,
                  lv_moabw TYPE t554s-moabw,
                  it_cats TYPE TABLE OF catsdb,
                  wa_cats TYPE catsdb,
                  it_t554s TYPE TABLE OF t554s,
                  wa_t554s TYPE t554s.

    DATA: lt_org TYPE TABLE OF objec,
          wa_org TYPE objec.

    SELECT * FROM t554s INTO TABLE it_t554s
        WHERE begda <= sy-datum AND endda >= sy-datum.

    LOOP AT it_t554s INTO wa_t554s WHERE subty = '0HOL'.
        wa_t554s-art01 = 'A'.
        modify it_t554s FROM wa_t554s.
    ENDLOOP.


    SORT it_emp_datax BY pernr ASCENDING.
    DELETE ADJACENT DUPLICATES FROM it_emp_datax COMPARING pernr.

    LOOP AT it_emp_datax INTO wa_emp_data.

      LOOP AT it_9004 INTO wa_9004 WHERE pernr = wa_emp_data-pernr.
        MOVE-CORRESPONDING wa_9004 TO wa_emp_data.
        CONCATENATE wa_9004-pabrp '/' wa_9004-pabrj INTO wa_emp_data-pp.

        CASE wa_9004-emp_cert.
          WHEN '00'. wa_emp_data-emp_cert = 'Pending'.
          WHEN '01'. wa_emp_data-emp_cert = 'Certified'.
          WHEN '02'. wa_emp_data-emp_cert = 'Cancelled'.
          WHEN '03'. wa_emp_data-emp_cert = 'Approved'.
          WHEN '04'. wa_emp_data-emp_cert = 'Rejected'.
        ENDCASE.

        CASE wa_9004-mgr_cert.
          WHEN '00'. wa_emp_data-mgr_cert = 'Pending'.
          WHEN '01'. wa_emp_data-mgr_cert = 'Certified'.
          WHEN '02'. wa_emp_data-mgr_cert = 'Cancelled'.
          WHEN '03'. wa_emp_data-mgr_cert = 'Approved'.
          WHEN '04'. wa_emp_data-mgr_cert = 'Rejected'.
        ENDCASE.

*================= Get Current Org Unit =================



        CALL FUNCTION 'RH_STRUC_GET'
          EXPORTING
            act_otype              = 'P'
            act_objid              = wa_emp_data-pernr
            act_wegid              = 'WF_ORGUN'
*   ACT_INT_FLAG           =
*   ACT_PLVAR              = ' '
*   ACT_BEGDA              = SY-DATUM
*   ACT_ENDDA              = SY-DATUM
*   ACT_TDEPTH             = 0
*   ACT_TFLAG              = 'X'
*   ACT_VFLAG              = 'X'
           authority_check        = ' '
*   TEXT_BUFFER_FILL       =
*   BUFFER_MODE            =
* IMPORTING
*   ACT_PLVAR              =
         TABLES
*   RESULT_TAB             =
           result_objec           = lt_org
*   RESULT_STRUC           =
         EXCEPTIONS
           no_plvar_found         = 1
           no_entry_found         = 2
           OTHERS                 = 3
                  .
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.

        LOOP AT lt_org INTO wa_org WHERE otype = 'O'.

          wa_emp_data-orgeh = wa_org-objid.
          wa_emp_data-orgeh_2nd_text = wa_org-stext.

        ENDLOOP.



*================== Positive / Neg Pay ==================
        CLEAR:lv_begda, lv_endda, it_hrs, lv_hrs, wa_hrs,
        lv_abs, lv_moabw, lv_att.
        CALL FUNCTION 'HR_GB_PERIOD_DATES'
          EXPORTING
           abkrs                  = 'Z1'
           permo                  = 4
            pabrj                  = wa_9004-pabrj
            pabrp                  = wa_9004-pabrp
         IMPORTING
           begda                  = lv_begda
           endda                  = lv_endda
         EXCEPTIONS
           period_not_found       = 1
           OTHERS                 = 2
                  .
        IF sy-subrc <> 0.
* Implement suitable error handling here
          CONTINUE.
        ENDIF.

        SELECT SINGLE persk INTO @v_persk FROM pa0001
          WHERE pernr = @wa_emp_data-pernr AND begda <= @lv_endda AND
          endda >= @lv_endda.

        CASE v_persk.
          WHEN '02'.
            wa_emp_data-emp_type = 'Positive Pay Employee'.
            wa_emp_data-emp_type_col = 03.
          WHEN '06'.
            wa_emp_data-emp_type = 'Positive Pay Employee'.
            wa_emp_data-emp_type_col = 03.
          WHEN '06'.
            wa_emp_data-emp_type = 'Positive Pay Employee'.
            wa_emp_data-emp_type_col = 03.
          WHEN '10'.
            wa_emp_data-emp_type = 'Positive Pay Employee'.
            wa_emp_data-emp_type_col = 03.
          WHEN '18'.
            wa_emp_data-emp_type = 'Positive Pay Employee'.
            wa_emp_data-emp_type_col = 03.
          WHEN '19'.
            wa_emp_data-emp_type = 'Positive Pay Employee'.
            wa_emp_data-emp_type_col = 03.
          WHEN '20'.
            wa_emp_data-emp_type = 'Positive Pay Employee'.
            wa_emp_data-emp_type_col = 03.
          WHEN '21'.
            wa_emp_data-emp_type = 'Positive Pay Employee'.
            wa_emp_data-emp_type_col = 03.
          WHEN '22'.
            wa_emp_data-emp_type = 'Positive Pay Employee'.
            wa_emp_data-emp_type_col = 03.
          WHEN OTHERS.
            wa_emp_data-emp_type = 'Negative Pay Employee'.
            wa_emp_data-emp_type_col = 00.
        ENDCASE.

*================== PLANNED HOURS ==================
        CALL FUNCTION 'CATS_GET_TARGET_HOURS'
          EXPORTING
            pernr                          = wa_9004-pernr
            begda                          = lv_begda
            endda                          = lv_endda
*           TIMETYPE                       = '    '
*           SUBHRTIMES                     = ' '
*           ADDOVERTIME                    = ' '
*         IMPORTING
*           SUBRC                          =
          TABLES
            target_hours                   = it_hrs
         EXCEPTIONS
           pernr_not_found                = 1
           too_many_days                  = 2
           error_in_sap_enhancement       = 3
           OTHERS                         = 4
                  .
        IF sy-subrc = 0.
* Implement suitable error handling here
          LOOP AT it_hrs INTO wa_hrs.
            lv_hrs = lv_hrs + wa_hrs-stdaz.
          ENDLOOP.

          wa_emp_data-plan_hrs = lv_hrs.

        ENDIF.
*================== ACTUAL HOURS ==================
        CALL FUNCTION 'HR_DE_GET_GROUPING_FOR_PERNR'
          EXPORTING
            iv_pernr             = wa_9004-pernr
           iv_stichtag          = lv_begda
*           IT_P0001             =
         IMPORTING
*           EV_MOPGK             =
*           EV_MOZKO             =
           ev_moabw             = lv_moabw
         EXCEPTIONS
           error_occurred       = 1
           OTHERS               = 2
                  .
        IF sy-subrc <> 0.
* Implement suitable error handling here
          CONTINUE.
        ENDIF.

        SELECT * FROM CATSDB INTO TABLE @it_cats
          WHERE pernr = @wa_9004-pernr AND workdate BETWEEN
          @lv_begda AND @lv_endda AND status = '30'.

        LOOP AT it_cats INTO wa_cats.
          READ TABLE it_t554s INTO wa_t554s
          with key subty = wa_cats-awart.

          IF sy-subrc = 0.
            CASE wa_t554s-art01.
              WHEN 'P'.
                lv_att = lv_att + wa_cats-catshours.
              WHEN 'A'.
                lv_abs = lv_abs + wa_cats-catshours.
            ENDCASE.
          ENDIF.
        ENDLOOP.


*        SELECT a~catshours, b~art01, a~awart FROM catsdb AS a INNER
*JOIN
*          t554s AS b ON a~awart = b~subty
*          INTO CORRESPONDING FIELDS OF TABLE @it_cats WHERE
*          a~pernr = @wa_9004-pernr
*          AND a~workdate BETWEEN @lv_begda AND @lv_endda
*          AND a~status = 30 AND b~begda <= @lv_begda
*          AND b~endda >= @lv_endda AND b~moabw = @lv_moabw.
*
*        IF sy-subrc = 0.
*          LOOP AT it_cats INTO wa_cats.
*            CASE wa_cats-art01.
*              WHEN 'A'.
*                lv_abs = lv_abs + wa_cats-catshours.
*              WHEN 'P'.
*                IF wa_cats-awart = '0HOL'.
*                  lv_abs = lv_abs + wa_cats-catshours.
*                ELSE.
*                  lv_att = lv_att + wa_cats-catshours.
*                ENDIF.
*
*              WHEN OTHERS.
*            ENDCASE.
*            CLEAR wa_cats.
*          ENDLOOP.
          wa_emp_data-att_hrs = lv_att.
          wa_emp_data-abs_hrs = lv_abs.
          wa_emp_data-act_hrs = lv_att + lv_abs.


*        ENDIF.

        wa_emp_data-begda = lv_begda.
        wa_emp_data-endda = lv_endda.

        APPEND wa_emp_data TO it_emp_data.
      ENDLOOP.

    ENDLOOP.
  ENDIF.

*  SORT IT_EMP_DATA BY EMP_NAME ASCENDING.
  DELETE it_emp_data WHERE pp IS INITIAL.
  CLEAR: it_list[], it_emp_datax[].
ENDFORM.                    " SET_EMPLOYEES
*&---------------------------------------------------------------------*
*&      Form  FILL_TIME_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_time_data .

  DATA: lv_obj TYPE  realo,
        lv_leader_id TYPE  realo,
        lv_name TYPE string,
        lv_phone TYPE string,
        lv_email TYPE string,
        lv_date TYPE datum.

  lv_date = sy-datum.
  LOOP AT it_emp_data INTO wa_emp_data.

    SELECT SINGLE * FROM pa0002 INTO wa_pa9002
                 WHERE pernr = wa_emp_data-pernr
             AND begda LE sy-datum AND endda LE sy-datum.
    IF sy-subrc NE 0.
      SELECT SINGLE * FROM pa0002 INTO wa_pa9002
        WHERE pernr = wa_emp_data-pernr.
    ENDIF.

    IF wa_emp_data-emp_cert_date IS NOT INITIAL.
      lv_date = wa_emp_data-emp_cert_date.
    ENDIF.

    IF wa_emp_data-mgr_cert_date IS NOT INITIAL.
      lv_date = wa_emp_data-mgr_cert_date.
    ENDIF.

    IF wa_pa9002-rufnm IS NOT INITIAL.
   CONCATENATE wa_pa9002-rufnm wa_pa9002-nachn INTO wa_emp_data-emp_name
   SEPARATED BY ' '.
    ELSE.
      CONCATENATE wa_pa9002-vorna wa_pa9002-nachn INTO
  wa_emp_data-emp_name SEPARATED BY ' '.
    ENDIF.


    CLEAR: wa_pa9002.
    MOVE wa_emp_data-pernr TO lv_obj.
    CALL FUNCTION 'RH_GET_LEADER'
      EXPORTING
        plvar                     = '01'
        keydate                   = lv_date
        otype                     = 'P'
        objid                     = lv_obj
      IMPORTING
        leader_id                 = lv_leader_id
      EXCEPTIONS
        no_leader_found           = 1
        no_leading_position_found = 2
        OTHERS                    = 3.


    SELECT SINGLE * FROM pa0002 INTO wa_pa9002
                 WHERE pernr = lv_leader_id
             AND begda LE sy-datum AND endda LE sy-datum.
    IF sy-subrc NE 0.
      SELECT SINGLE * FROM pa0002 INTO wa_pa9002
        WHERE pernr = lv_leader_id.
    ENDIF.

    IF wa_pa9002-rufnm IS NOT INITIAL.
   CONCATENATE wa_pa9002-rufnm wa_pa9002-nachn INTO wa_emp_data-mgr_name
   SEPARATED BY ' '.
    ELSE.
      CONCATENATE wa_pa9002-vorna wa_pa9002-nachn INTO
  wa_emp_data-mgr_name SEPARATED BY ' '.
    ENDIF.

    MODIFY it_emp_data FROM wa_emp_data.
    CLEAR: wa_emp_data, lv_obj, lv_leader_id.
  ENDLOOP.
ENDFORM.                    " FILL_TIME_DATA
*&---------------------------------------------------------------------*
*&      Form  COLLECT_PERNRS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM collect_pernrs .

*  BREAK TBASETTY.

  DATA: it_pa0001 TYPE TABLE OF pernr_d.
  DATA: lv_pernr TYPE pernr_d.
  DATA: it_hrp1001 TYPE TABLE OF hrp1001.
  DATA: wa_hrp1001 TYPE hrp1001.
  DATA: it_emps TYPE wdy_key_value_list.
  DATA: wa_emps TYPE wdy_key_value.
  DATA: lt_pernrs TYPE TABLE OF pernr_d,
        it_tmks TYPE wdy_key_value_list.



*** Employee Selection
  IF s_e_pern[] IS NOT INITIAL.
    SELECT pernr FROM pa0001 INTO TABLE it_pa0001 WHERE pernr IN
    s_e_pern.
  ENDIF.
  APPEND LINES OF it_pa0001 TO it_pernrs.

  IF s_e_plan[] IS NOT INITIAL.
    SELECT pernr FROM pa0001 INTO TABLE it_pa0001 WHERE plans IN
    s_e_plan.
  ENDIF.
  APPEND LINES OF it_pa0001 TO it_pernrs.

  IF s_e_pers[] IS NOT INITIAL.
    SELECT pernr FROM pa0001 INTO TABLE it_pa0001 WHERE werks IN
    s_e_pers.
  ENDIF.
  APPEND LINES OF it_pa0001 TO it_pernrs.

*  SORT IT_PERNRS ASCENDING.
*  DELETE ADJACENT DUPLICATES FROM IT_PERNRS.

* By Manager




*  SELECT-OPTIONS : S_M_PERS FOR LV_M_PERSA.
*  SELECT-OPTIONS : S_M_PERN FOR LV_M_PERNR.
  IF s_m_pern[] IS NOT INITIAL.
    SELECT pernr FROM pa0001 INTO TABLE it_pa0001 WHERE pernr IN
    s_m_pern.

    LOOP AT it_pa0001 INTO lv_pernr.

      CALL METHOD zcl_hrpaom_utilities=>get_dirrep_for_mang
        EXPORTING  " Get Direct reports
          im_pernr     = lv_pernr
          im_uname     = ''
        IMPORTING
          ex_employees = it_emps.

      LOOP AT it_emps INTO wa_emps. " Collect PERNRs of the Manager
        lv_pernr = wa_emps-key.
        APPEND lv_pernr TO it_pernrs.
      ENDLOOP.

    ENDLOOP.

  ENDIF.

*  SELECT-OPTIONS : S_M_PLAN FOR LV_M_PLANS.
  IF s_m_plan IS NOT INITIAL.
    SELECT * FROM hrp1001 INTO TABLE it_hrp1001 WHERE
                              objid IN s_m_plan
                          AND plvar EQ '01'
                          AND rsign EQ 'A'
                          AND relat EQ '008'
                          AND istat EQ '1'
                          AND begda LE sy-datum
                          AND endda GE sy-datum.

    LOOP AT it_hrp1001 INTO wa_hrp1001. "Loop through all Managers
      lv_pernr = wa_hrp1001-sobid.
      CALL METHOD zcl_hrpaom_utilities=>get_dirrep_for_mang
        EXPORTING  " Get Direct reports
          im_pernr     = lv_pernr
          im_uname     = ''
        IMPORTING
          ex_employees = it_emps.

      LOOP AT it_emps INTO wa_emps. " Collect PERNRs of the Manager
        lv_pernr = wa_emps-key.
        APPEND lv_pernr TO it_pernrs.
      ENDLOOP.
    ENDLOOP.

  ENDIF.


  IF s_m_pers[] IS NOT INITIAL.
    SELECT pernr FROM pa0001 INTO TABLE it_pa0001 WHERE
       werks IN s_m_pers.

    LOOP AT it_pa0001 INTO lv_pernr.

      CALL METHOD zcl_hrpaom_utilities=>get_dirrep_for_mang
        EXPORTING  " Get Direct reports
          im_pernr     = lv_pernr
          im_uname     = ''
        IMPORTING
          ex_employees = it_emps.

      LOOP AT it_emps INTO wa_emps. " Collect PERNRs of the Manager
        lv_pernr = wa_emps-key.
        APPEND lv_pernr TO it_pernrs.
      ENDLOOP.

    ENDLOOP.

  ENDIF.
*===========================================
*
* Select PERNRs by Timekeeper
*
*===========================================


  IF s_t_pern[] IS NOT INITIAL.


    SELECT pernr FROM pa0001 INTO TABLE lt_pernrs WHERE
      begda <= sy-datum AND
      endda >= sy-datum AND
      pernr IN s_t_pern.

*IF SY-SUBRC = 0.
*  PERFORM COLLECT_TIMEKEEPERS USING LT_PERNRS.
*ENDIF.

    IF sy-subrc <> 0.
*ERROR HANDLING
    ENDIF.

    CLEAR: wa_emps, it_emps.

    LOOP AT lt_pernrs INTO lv_pernr.
      wa_emps-key = lv_pernr.
      APPEND wa_emps TO it_tmks.
    ENDLOOP.

    CLEAR wa_emps.

    CALL METHOD zcl_hrpaom_utilities=>get_emp_by_timekeeper
      EXPORTING
*    IM_PERNR      =
        im_pernrs     = it_tmks
      IMPORTING
        ex_emps       = it_emps
      EXCEPTIONS
        not_qualified = 1
            .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    LOOP AT it_emps INTO wa_emps.
      lv_pernr = wa_emps-key.
      APPEND lv_pernr TO it_pernrs.
    ENDLOOP.


  ENDIF.



*IF S_T_PLAN[] IS NOT INITIAL.
*
*SELECT PERNR FROM PA0001 INTO TABLE LT_PERNRS WHERE
*  BEGDA <= SY-DATUM AND
*  ENDDA >= SY-DATUM AND
*  PLANS IN S_T_PLAN.
*
*IF SY-SUBRC = 0.
*  PERFORM COLLECT_TIMEKEEPERS USING LT_PERNRS.
*ENDIF.
*
*
*
*ENDIF.

*IF S_T_PERS[] IS NOT INITIAL.
*
*SELECT PERNR FROM PA0001 INTO TABLE LT_PERNRS WHERE
*  BEGDA <= SY-DATUM AND
*  ENDDA >= SY-DATUM AND
*  WERKS IN S_T_PERS.
*
*IF SY-SUBRC = 0.
*  PERFORM COLLECT_TIMEKEEPERS USING LT_PERNRS.
*ENDIF.
*
*
*
*
*ENDIF.


  SORT it_pernrs ASCENDING.
  DELETE ADJACENT DUPLICATES FROM it_pernrs.

ENDFORM.                    " COLLECT_PERNRS


*FORM COLLECT_TIMEKEEPERS USING lt_pernr LIKE it_pernrs.
*BREAK cdeckard.
*   TYPES: BEGIN OF ty_timekpr,
*    pernr LIKE P0001-PERNR,
*    SACHZ LIKE p0001-SACHZ,
*    WERKS LIKE P0001-WERKS,
*    TELNR LIKE T526-TELNR,
*
*    END OF ty_timekpr.
*
*
*  TYPES: BEGIN OF TY_TMK01,
*   PERNR LIKE P0001-PERNR,
*   SACHZ LIKE P0001-SACHZ,
*   END OF TY_TMK01.
*
*   DATA: IT_TMK01 TYPE TABLE OF TY_TMK01.
*
*
*  DATA: IT_tmkpr TYPE TABLE OF TY_TIMEKPR.
*
*  FIELD-SYMBOLS <FS_TMKPR> TYPE TY_TIMEKPR.
*
*  DATA: LT_T526 TYPE TABLE OF T526.
*   DATA: IT_PA0001 TYPE TABLE OF PERNR_D.
*   DATA: LV_PERNR TYPE PERNR_D.
*
**  SELECT * FROM pa0001 INTO CORRESPONDING FIELDS OF TABLE
**    IT_tmkpr WHERE
**    PERNR IN S_T_PERN[].
*
*   MOVE LT_PERNR TO IT_TMKPR.
*
*
**   SORT IT_TMKPR BY PERNR ASCENDING.
**   DELETE ADJACENT DUPLICATES FROM IT_TMKPR.
*
*    LOOP AT IT_tmkpr ASSIGNING <FS_TMKPR>.
*      <FS_TMKPR>-TELNR = <FS_TMKPR>-PERNR.
*      SHIFT <FS_TMKPR>-TELNR LEFT DELETING LEADING '0'.
*      ENDLOOP.
*
*SELECT * FROM T526 INTO TABLE LT_T526 FOR ALL ENTRIES IN IT_tmkpr
*    WHERE TELNR = IT_tmkpr-TELNR.
*
*  IF SY-SUBRC <> 0.
*    EXIT.
*  ENDIF.
*
*
*SELECT PERNR FROM PA0001 INTO TABLE @IT_PA0001 FOR ALL ENTRIES IN
*  @LT_T526 WHERE
*  SACHZ = @LT_T526-SACHX AND
*  begda <= @sy-datum and
*  endda >= @sy-datum.
*
*
*
*LOOP AT IT_PA0001 INTO LV_PERNR.
*  APPEND LV_PERNR TO IT_PERNRS.
*
*ENDLOOP.
*
*ENDFORM. " COLLECT_TIMEKEEPERS.


FORM f4_help_obj USING p_fname TYPE dynfnam.
*      CHANGING P_S_OBJ TYPE SEARK.

  CALL FUNCTION 'RH_TYPE_STRUC_HELP'
    EXPORTING
      act_search_otype               = 'S'
*     ACT_SEARCH_WEGID               =
     act_search_svect               = '1'
     set_mode                       = 'X'
*     ACT_ROOT_OT                    =
*     ACT_ROOT_ID                    =
     act_plvar                      = '01'
*     ACT_SEARCH_BEGDA               = SY-DATUM
*     ACT_SEARCH_ENDDA               = SY-DATUM
     no_seark                       = 'X'
*     ACT_LIST_TYPE                  =
*     ACT_INT_WEGID                  =
     selected_obj_append            = 'X'
*     CHANGE_SEARCH_TYPE             =
*     RESTRICT_CALLBACK              =
*   IMPORTING
*     SELECTED_PLVAR                 =
*     SELECTED_OTYPE                 =
*     SELECTED_OBJID                 =
*     CHANGED_FLAG                   =
*     LAST_OK_CODE                   =
   TABLES
     selected_objects               = chosen_objects
   EXCEPTIONS
     no_active_plvar                = 1
     no_object_selected             = 2
     no_struc_search_possible       = 3
     OTHERS                         = 4
            .
  IF sy-subrc <> 0.
* Implement suitable error handling here

  ELSE.

    CASE p_fname.
*      WHEN 'Timekeeper'.
*        CLEAR S_T_PLAN[].
*
*        LOOP AT CHOSEN_OBJECTS.
*          S_T_PLAN-LOW = CHOSEN_OBJECTS-SOBID.
*          S_T_PLAN-OPTION = 'EQ'.
*          S_T_PLAN-SIGN = 'I'.
*          APPEND S_T_PLAN.
*        ENDLOOP.

      WHEN 'Manager'.
        CLEAR s_m_plan[].

        LOOP AT chosen_objects.
          s_m_plan-low = chosen_objects-sobid.
          s_m_plan-option = 'EQ'.
          s_m_plan-sign = 'I'.
          APPEND s_m_plan.
        ENDLOOP.

      WHEN 'Employee' .
        CLEAR s_e_plan[].

        LOOP AT chosen_objects.
          s_e_plan-low = chosen_objects-sobid.
          s_e_plan-option = 'EQ'.
          s_e_plan-sign = 'I'.
          APPEND s_e_plan.
        ENDLOOP.


      WHEN OTHERS.
    ENDCASE.



  ENDIF.





ENDFORM.

"INCLUDE YMSS_TIME_APPROVAL_REPORT01.
*&---------------------------------------------------------------------*
*&      Form  setup_objects
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM setup_objects .
  CREATE OBJECT cl_receiver.

  IF cl_gui_alv_grid=>offline( ) IS INITIAL.


    CREATE OBJECT custom_container
    EXPORTING
*          parent                      =
      container_name              = container_name
*          style                       =
*          lifetime                    = lifetime_default
*          repid                       =
*          dynnr                       =
*          no_autodef_progid_dynnr     =
*        EXCEPTIONS
*          cntl_error                  = 1
*          cntl_system_error           = 2
*          create_error                = 3
*          lifetime_error              = 4
*          lifetime_dynpro_dynpro_link = 5
*          others                      = 6
      .
    IF sy-subrc <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.







    CREATE OBJECT alv_grid
      EXPORTING
*          i_shellstyle      = 0
*          i_lifetime        =
        i_parent          = custom_container
*          i_appl_events     = space
*          i_parentdbg       =
*          i_applogparent    =
*          i_graphicsparent  =
*          i_name            =
*          i_fcat_complete   = SPACE
*        EXCEPTIONS
*          error_cntl_create = 1
*          error_cntl_init   = 2
*          error_cntl_link   = 3
*          error_dp_create   = 4
*          others            = 5
        .
    IF sy-subrc <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ELSE.
    CREATE OBJECT alv_grid
      EXPORTING
*          i_shellstyle      = 0
*          i_lifetime        =
        i_parent          = gro_doc_container
*          i_appl_events     = space
*          i_parentdbg       =
*          i_applogparent    =
*          i_graphicsparent  =
*          i_name            =
*          i_fcat_complete   = SPACE
*        EXCEPTIONS
*          error_cntl_create = 1
*          error_cntl_init   = 2
*          error_cntl_link   = 3
*          error_dp_create   = 4
*          others            = 5
        .
    IF sy-subrc <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.

*IF s_file-low is not INITIAL.
*  alv_grid->offline( ).
*ENDIF.



*      set HANDLER cl_receiver->top_of_page FOR alv_grid.
  SET HANDLER cl_receiver->handle_user_command FOR alv_grid.
  SET HANDLER cl_receiver->handle_toolbar FOR alv_grid.

*      CALL METHOD alv_grid->list_processing_events
*        EXPORTING
*          i_event_name      = 'TOP_OF_PAGE'
*          i_dyndoc_id       = O_DYNDOC_ID
**          is_subtottxt_info =
**          ip_subtot_line    =
**          i_table_index     =
**        CHANGING
**          c_subtottxt       =
*          .
*
*

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  send_email
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_email USING wa_tab TYPE ty_out
                       e_ucomm LIKE sy-ucomm.
  DATA: subject TYPE so_obj_des,
        senderemail TYPE ad_smtpadr,
        sender TYPE ad_smtpadr,
        lt_content TYPE TABLE OF soli,
        wa_content TYPE soli,
        receiver TYPE ad_smtpadr,
        return TYPE bapiret2,
        msg TYPE string
        .
  CONSTANTS: c_mail_start(255) VALUE
  '<HTML><HEAD><meta http-equiv="Content-type" content="text/html;' &
  'charset=UTF-16"></HEAD><BODY bg' &
  'color="#FFFFFF" style="font-family:Calibri;color:black;font-size:' &
  '14px;" >',
  c_space(6) VALUE '&nbsp;',
  c_mail_end(255) VALUE '</body></html>'.

  IF NOT ( e_ucomm = 'EMAIL' OR e_ucomm = 'MMAIL' ).
    EXIT.
  ENDIF.
  CLEAR wa_tracker.
  senderemail = 'noreply@example.com'.
  sender = 'SAP Time Approval'.
  wa_content-line = c_mail_start.

  APPEND wa_content TO lt_content.

  CLEAR wa_content.

  CASE e_ucomm.
    WHEN 'EMAIL'.


      SELECT SINGLE usrid_long FROM pa0105 INTO receiver
        WHERE pernr = wa_tab-pernr AND begda <= sy-datum
        AND endda >= sy-datum AND subty = '0010'.

    IF sy-subrc <> 0.
      MESSAGE 'No Email address maintained for Employee.' TYPE 'S'.
      exit.
    ENDIF.

    TRANSLATE receiver to LOWER CASE.

    READ TABLE it_tracker WITH key
    recipient = receiver pp = wa_tab-pp TRANSPORTING NO FIELDS.

    IF sy-subrc = 0.
      CONCATENATE 'Email reminder already sent to' receiver into msg
      separated by space.

      MESSAGE msg TYPE 'S'.

      exit.
    ENDIF.

        wa_tracker-recipient = receiver.
        wa_tracker-pp = wa_tab-pp.

      CONCATENATE wa_tab-emp_name ',' '<p>' INTO wa_content-line.

      APPEND wa_content TO lt_content.

      CLEAR wa_content.

 wa_content-line = 'Please log into ESS and certify your timesheet for'.

      APPEND wa_content TO lt_content.

      CLEAR wa_content.

      CONCATENATE ' Pay Period:' wa_tab-pp INTO wa_content-line.

      APPEND wa_content TO lt_content.

      CLEAR wa_content.

      CONCATENATE ' (' wa_tab-begda+4(2) '/' wa_tab-begda+6(2) '/'
      wa_tab-begda(4) ' to ' INTO wa_content.

      APPEND wa_content TO lt_content.

      CLEAR wa_content.
      CONCATENATE wa_tab-endda+4(2) '/' wa_tab-endda+6(2) '/'
      wa_tab-endda(4) ').<P>' INTO wa_content-line.
      APPEND wa_content TO lt_content.

      CLEAR wa_content.
      wa_content-line = '<a href="http://www.portlandonline.com/ep">' &
      'Click here</a> to go to the timesheet application.<P>'.


      APPEND wa_content TO lt_content.

      CLEAR wa_content.

     CONCATENATE 'Certify your timesheet for Pay Period:' wa_tab-pp INTO
     subject SEPARATED BY space.

    WHEN 'MMAIL'.
      DATA: lv_leader_id TYPE  realo,
            lv_name TYPE string,
            lv_pernr TYPE realo.


      WRITE wa_tab-pernr TO lv_pernr.

      CALL FUNCTION 'RH_GET_LEADER'
        EXPORTING
          plvar                           = '01'
          keydate                         = sy-datum
         otype                           = 'P'
          objid                           = lv_pernr
*     GET_LEADER_TAB                  = ' '
*     CONSIDER_VAC_POS                = ' '
       IMPORTING
*     LEADER_TYPE                     =
         leader_id                       = lv_leader_id
*     MULTIPLE                        =
*   TABLES
*     LEADER_TAB                      =
       EXCEPTIONS
         no_leader_found                 = 1
         no_leading_position_found       = 2
         OTHERS                          = 3
                .
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      SELECT SINGLE usrid_long FROM pa0105 INTO receiver
        WHERE pernr = lv_leader_id AND begda <= sy-datum
        AND endda >= sy-datum AND subty = '0010'.

        IF sy-subrc <> 0.
          MESSAGE 'Email address not maintained for manager' TYPE 'S'.
          exit.
        ENDIF.

        TRANSLATE receiver TO LOWER CASE.


        READ TABLE it_tracker WITH key recipient = receiver
        pp = wa_tab-pp manager = 'X' TRANSPORTING NO FIELDS.

        IF sy-subrc = 0.
          MESSAGE 'Email reminder already sent to manager' TYPE 'S'.
          exit.
        ENDIF.

      wa_tracker-recipient = receiver.
      wa_tracker-pp = wa_tab-pp.
      wa_tracker-manager = 'X'.

      SELECT SINGLE ename FROM pa0001
        INTO lv_name
        WHERE pernr = lv_leader_id
        AND begda <= sy-datum AND endda >= sy-datum.

      CONCATENATE lv_name ',<P>' INTO wa_content-line.

      APPEND wa_content TO lt_content.

      CLEAR wa_content.

      CONCATENATE 'Please log into MSS and approve time for'
     wa_tab-emp_name 'for pay period:' INTO wa_content-line SEPARATED BY
     space.

      APPEND wa_content TO lt_content.

      CLEAR wa_content.

      CONCATENATE c_space wa_tab-pp ' (' wa_tab-begda+4(2) '/'
        wa_tab-begda+6(2) '/' wa_tab-begda(4) ' to ' c_space
        wa_tab-endda+4(2) '/' wa_tab-endda+6(2) '/' wa_tab-endda(4)
        ').<P>'INTO wa_content-line.

      APPEND wa_content TO lt_content.

      CLEAR wa_content.

      wa_content-line = '<a href="http://www.example.com/ep">' &
      'Click here</a> to go to the time approval application.<P>'.


      APPEND wa_content TO lt_content.

      CLEAR wa_content.

      CONCATENATE 'Please Approve Time for Pay Period:' wa_tab-pp INTO
      subject SEPARATED BY space.

  ENDCASE.

  wa_content-line = '<span style="color:blue">' &
  'NOTE: This is an automatically generated message.'.

  APPEND wa_content TO lt_content.

  CLEAR wa_content.

  wa_content-line = 'Please do not respond to this sender.</span>'.

  APPEND wa_content TO lt_content.

  CLEAR wa_content.


*==================== Don't send live emails ==================*

  IF sy-sysid(3) <> 'PRP'.
    receiver = 'example@example.com'.
    CONCATENATE '<P><P><span style="color:blue;font-family:'
    'Calibri;font-size:14px;">NOTE: This is a test '
    ' email sent from ' c_space sy-sysid(3) ' please ignore.</span>'
    INTO wa_content-line.
    APPEND wa_content TO lt_content.
    CLEAR wa_content.
  ENDIF.

  wa_content-line = c_mail_end.

  APPEND wa_content TO lt_content.
  CLEAR wa_content.

  CALL FUNCTION 'ZUTIL_SEND_EMAIL_NOTIFICATION'
   EXPORTING
     i_subject               = subject
     i_body                  = lt_content
     i_type                  = 'HTM'
     i_imp                   = '9'
*     I_EMAIL_FROM_USER       = SY-UNAME
*     I_EMAIL_TO_USER         =
*     I_EMAIL_CC_USER         =
     i_email_from_id         = senderemail
     i_email_to_id           = receiver
*     I_EMAIL_CC_ID           =
*     I_EMAIL_CC2_ID          =
*     I_EMAIL_CC3_ID          =
*     I_EMAIL_CC4_ID          =
     i_email_from_name       = sender
   IMPORTING
     return                  = return
            .

  IF return IS INITIAL.


    APPEND wa_tracker to it_tracker.

    CONCATENATE 'Your message was successfully sent to ' receiver INTO
    msg SEPARATED BY space.

    MESSAGE msg TYPE 'S'.
  ELSE.
    MESSAGE 'Error during send' TYPE 'S'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  get_filename
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM get_filename .
*  DATA: l_filename TYPE string,
*        l_path TYPE string,
*        l_fullpath TYPE string.
*
*  CALL METHOD cl_gui_frontend_services=>file_save_dialog
*    EXPORTING
*      window_title              = 'Save Time Approval PDF'
*      default_extension         = 'pdf'
*      default_file_name         = 'Time Approval'
**    with_encoding             =
**    file_filter               =
**    initial_directory         =
*      prompt_on_overwrite       = 'X'
*    CHANGING
*      filename                  = l_filename
*      path                      = l_path
*      fullpath                  = l_fullpath
**    user_action               =
**    file_encoding             =
**  EXCEPTIONS
**    cntl_error                = 1
**    error_no_gui              = 2
**    not_supported_by_gui      = 3
**    invalid_default_file_name = 4
**    others                    = 5
*          .
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*  ENDIF.
*
*  WRITE l_fullpath TO s_file-low.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  save_pdf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM save_pdf .
*
*  DATA: l_printp TYPE pri_params,
*        v_flag TYPE c,
*        w_pdf LIKE TABLE OF tline,
*        lv_fname TYPE string,
*        lo_print TYPE REF TO cl_salv_print,
*        print_params TYPE lvc_s_prnt,
*        pparams TYPE slis_print_alv
*        .
*
*
*
**  lo_print = alv_grid->get_print( ).
*
*
*  CALL FUNCTION 'GET_PRINT_PARAMETERS'
*   EXPORTING
**     ARCHIVE_ID                     = C_CHAR_UNKNOWN
**     ARCHIVE_INFO                   = C_CHAR_UNKNOWN
**     ARCHIVE_MODE                   = C_CHAR_UNKNOWN
**     ARCHIVE_TEXT                   = C_CHAR_UNKNOWN
**     AR_OBJECT                      = C_CHAR_UNKNOWN
**     ARCHIVE_REPORT                 = C_CHAR_UNKNOWN
**     AUTHORITY                      = C_CHAR_UNKNOWN
**     COPIES                         = C_NUM3_UNKNOWN
**     COVER_PAGE                     = C_CHAR_UNKNOWN
**     DATA_SET                       = C_CHAR_UNKNOWN
**     DEPARTMENT                     = C_CHAR_UNKNOWN
**     DESTINATION                    = C_CHAR_UNKNOWN
**     EXPIRATION                     = C_NUM1_UNKNOWN
**     IMMEDIATELY                    = 'X'
**     IN_ARCHIVE_PARAMETERS          = ' '
**     IN_PARAMETERS                  = ' '
**     LAYOUT                         = C_CHAR_UNKNOWN
**     LINE_COUNT                     = C_INT_UNKNOWN
**     LINE_SIZE                      = C_INT_UNKNOWN
**     LIST_NAME                      = C_CHAR_UNKNOWN
**     LIST_TEXT                      = C_CHAR_UNKNOWN
**     MODE                           = ' '
**     NEW_LIST_ID                    = C_CHAR_UNKNOWN
**     PROTECT_LIST                   = C_CHAR_UNKNOWN
*     no_dialog                      = 'X'
**     RECEIVER                       = C_CHAR_UNKNOWN
**     RELEASE                        = C_CHAR_UNKNOWN
**     REPORT                         = C_CHAR_UNKNOWN
**     SAP_COVER_PAGE                 = C_CHAR_UNKNOWN
**     HOST_COVER_PAGE                = C_CHAR_UNKNOWN
**     PRIORITY                       = C_NUM1_UNKNOWN
**     SAP_OBJECT                     = C_CHAR_UNKNOWN
**     TYPE                           = C_CHAR_UNKNOWN
*     user                           = sy-uname
**     USE_OLD_LAYOUT                 = ' '
**     UC_DISPLAY_MODE                = C_CHAR_UNKNOWN
**     DRAFT                          = C_CHAR_UNKNOWN
**     ABAP_LIST                      = ' '
**     USE_ARCHIVENAME_DEF            = ' '
**     DEFAULT_SPOOL_SIZE             = C_CHAR_UNKNOWN
**     WITH_STRUCTURE                 = C_CHAR_UNKNOWN
**     SUPPRESS_SHADING               = C_CHAR_UNKNOWN
**     PO_FAX_STORE                   = ' '
**     NO_FRAMES                      = C_CHAR_UNKNOWN
*   IMPORTING
**     OUT_ARCHIVE_PARAMETERS         =
*     out_parameters                 = l_printp
*     valid                          = v_flag
**     VALID_FOR_SPOOL_CREATION       =
*   EXCEPTIONS
*     archive_info_not_found         = 1
*     invalid_print_params           = 2
*     invalid_archive_params         = 3
*     OTHERS                         = 4
*            .
*  IF v_flag = 'X' AND sy-subrc EQ 0.
*    print_params-print = 'X'.
*    print_params-prnt_info = ' '.
*    print_params-prntlstinf = 'X'.
*    print_params-prntselinf = 'X'.
*    "print_params-no_coverpage = 'X'.
*    print_params-reservelns = 1.
*    l_printp-pdest = 'LPDF'.
*    l_printp-primm = 'X'.
*    print_params-print_ctrl-pri_params = l_printp.
*
*    CALL METHOD alv_grid->set_table_for_first_display
*      EXPORTING
**        i_buffer_active               =
**        i_bypassing_buffer            =
**        i_consistency_check           =
**        i_structure_name              =
**        is_variant                    =
**        i_save                        =
**        i_default                     = 'X'
*        is_layout                     = s_layout
*        is_print                      = print_params
**        it_special_groups             =
**        it_toolbar_excluding          =
**        it_hyperlink                  =
**        it_alv_graphics               =
**        it_except_qinfo               =
**        ir_salv_adapter               =
*      CHANGING
*        it_outtab                     = it_out[]
*        it_fieldcatalog               = fcat[]
**        it_sort                       =
**        it_filter                     =
*      EXCEPTIONS
*        invalid_parameter_combination = 1
*        program_error                 = 2
*        too_many_lines                = 3
*        OTHERS                        = 4
*            .
*    IF sy-subrc <> 0.
**     Implement suitable error handling here
*    ENDIF.
*
*
*    w_spono = sy-spono.
*
*
*  ENDIF.
*
*
*
*
*  IF w_spono IS NOT INITIAL.
*
*
*
*    CALL FUNCTION 'CONVERT_ABAPSPOOLJOB_2_PDF'
*      EXPORTING
*        src_spoolid                    = w_spono
**     NO_DIALOG                      =
**     DST_DEVICE                     =
**     PDF_DESTINATION                =
**     NO_BACKGROUND                  =
**     GET_SIZE_FROM_FORMAT           = 'U'
**     USE_CASCADING                  = ' '
**   IMPORTING
**     PDF_BYTECOUNT                  =
**     PDF_SPOOLID                    =
**     LIST_PAGECOUNT                 =
**     BTC_JOBNAME                    =
**     BTC_JOBCOUNT                   =
**     BIN_FILE                       =
*     TABLES
*       pdf                            = w_pdf[]
*     EXCEPTIONS
*       err_no_abap_spooljob           = 1
*       err_no_spooljob                = 2
*       err_no_permission              = 3
*       err_conv_not_possible          = 4
*       err_bad_destdevice             = 5
*       user_cancelled                 = 6
*       err_spoolerror                 = 7
*       err_temseerror                 = 8
*       err_btcjob_open_failed         = 9
*       err_btcjob_submit_failed       = 10
*       err_btcjob_close_failed        = 11
*       OTHERS                         = 12
*              .
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.
*
*    lv_fname = s_file-low.
*
*    CALL FUNCTION 'GUI_DOWNLOAD'
*      EXPORTING
**     BIN_FILESIZE                    =
*        filename                        = lv_fname
*       filetype                        = 'BIN'
**     APPEND                          = ' '
**     WRITE_FIELD_SEPARATOR           = ' '
**     HEADER                          = '00'
**     TRUNC_TRAILING_BLANKS           = ' '
**     WRITE_LF                        = 'X'
**     COL_SELECT                      = ' '
**     COL_SELECT_MASK                 = ' '
**     DAT_MODE                        = ' '
**     CONFIRM_OVERWRITE               = ' '
**     NO_AUTH_CHECK                   = ' '
**     CODEPAGE                        = ' '
**     IGNORE_CERR                     = ABAP_TRUE
**     REPLACEMENT                     = '#'
**     WRITE_BOM                       = ' '
**     TRUNC_TRAILING_BLANKS_EOL       = 'X'
**     WK1_N_FORMAT                    = ' '
**     WK1_N_SIZE                      = ' '
**     WK1_T_FORMAT                    = ' '
**     WK1_T_SIZE                      = ' '
**     WRITE_LF_AFTER_LAST_LINE        = ABAP_TRUE
**     SHOW_TRANSFER_STATUS            = ABAP_TRUE
**     VIRUS_SCAN_PROFILE              = '/SCET/GUI_DOWNLOAD'
**   IMPORTING
**     FILELENGTH                      =
*      TABLES
*        data_tab                        = w_pdf[]
**     FIELDNAMES                      =
*     EXCEPTIONS
*       file_write_error                = 1
*       no_batch                        = 2
*       gui_refuse_filetransfer         = 3
*       invalid_type                    = 4
*       no_authority                    = 5
*       unknown_error                   = 6
*       header_not_allowed              = 7
*       separator_not_allowed           = 8
*       filesize_not_allowed            = 9
*       header_too_long                 = 10
*       dp_error_create                 = 11
*       dp_error_send                   = 12
*       dp_error_write                  = 13
*       unknown_dp_error                = 14
*       access_denied                   = 15
*       dp_out_of_memory                = 16
*       disk_full                       = 17
*       dp_timeout                      = 18
*       file_not_found                  = 19
*       dataprovider_exception          = 20
*       control_flush_error             = 21
*       OTHERS                          = 22
*              .
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.
*
*  ENDIF.
*ENDFORM.