*&---------------------------------------------------------------------*
*& Report  ZFII_UP_MD_EXRATEDIFF
*& Lalu Raynaldi Pratama Putra	
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZFII_UP_MD_EXRATEDIFF.
*       NO STANDARD PAGE HEADING LINE-SIZE 255.

*include bdcrecx1.
INCLUDE ZABAPALV.
DATA : IT_BDCDATA    LIKE BDCDATA OCCURS 0 WITH HEADER LINE,
       IT_BDCMSGCOLL LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       WA_BDCMSGCOLL LIKE BDCMSGCOLL,
       FNAME TYPE STRING,
       OPT LIKE CTU_PARAMS.

DATA : BEGIN OF IT_DATA OCCURS 0,
        KTOPL LIKE T030H-KTOPL, "Chart of Account
        HKONT LIKE T030H-HKONT, "GL Account
        LKORR LIKE T030H-LKORR, "Bal Sheet Adj 1
        LSREA LIKE T030H-LSREA, "Loss
        LHREA LIKE T030H-LHREA, "Gain
        LSBEW LIKE T030H-LSBEW, "Val Loss 1
        LHBEW LIKE T030H-LHBEW, "Val Gain 1
        TRKORR LIKE E070-TRKORR, "CR Number
     END OF IT_DATA.


DATA: BEGIN OF IT_DISPLAY OCCURS 0,
      VICON(4) TYPE C,
      HKONT LIKE T030H-HKONT, "GL Account
      VSTATUS(10) TYPE C,
      VTEXT(132) TYPE C,
    END OF IT_DISPLAY.

SELECTION-SCREEN BEGIN OF BLOCK B1001 WITH FRAME TITLE TEXT-100.
PARAMETERS: FILENAME LIKE RLGRAP-FILENAME
                     LOWER CASE OBLIGATORY
                     DEFAULT 'C:/'.
SELECTION-SCREEN END OF BLOCK B1001.

SELECTION-SCREEN BEGIN OF BLOCK B1002 WITH FRAME TITLE TEXT-150.
PARAMETERS: MODE LIKE CTU_PARAMS-DISMODE DEFAULT 'N'.
SELECTION-SCREEN END OF BLOCK B1002.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR FILENAME.
  PERFORM F4_OPEN_FILE.


START-OF-SELECTION.
  PERFORM GET_DATA.
  PERFORM PROCESS_DATA.
  PERFORM DISPLAY_DATA.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  F4_OPEN_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F4_OPEN_FILE .
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      PROGRAM_NAME  = SY-CPROG
      DYNPRO_NUMBER = SY-DYNNR
      FIELD_NAME    = ' '
    IMPORTING
      FILE_NAME     = FILENAME.

  IF SY-SUBRC <> 0 OR FILENAME EQ ''.
* Implement suitable error handling here
  ENDIF.
ENDFORM.                    "F4_OPEN_FILE

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_DATA.
  CLEAR : IT_DATA, IT_DISPLAY.
  REFRESH : IT_DATA, IT_DISPLAY.

  MOVE FILENAME TO FNAME.
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      FILENAME                = FNAME
      HAS_FIELD_SEPARATOR     = 'X' "FILL : READ BY TAB, NO FILL : READ BY SPACE
      READ_BY_LINE            = 'X' "FILL : READ  PER ROW, NO FILL : READ PER COLUMN"
    TABLES
      DATA_TAB                = IT_DATA
    EXCEPTIONS
      FILE_OPEN_ERROR         = 1
      FILE_READ_ERROR         = 2
      NO_BATCH                = 3
      GUI_REFUSE_FILETRANSFER = 4
      INVALID_TYPE            = 5
      NO_AUTHORITY            = 6
      UNKNOWN_ERROR           = 7
      BAD_DATA_FORMAT         = 8
      HEADER_NOT_ALLOWED      = 9
      SEPARATOR_NOT_ALLOWED   = 10
      HEADER_TOO_LONG         = 11
      UNKNOWN_DP_ERROR        = 12
      ACCESS_DENIED           = 13
      DP_OUT_OF_MEMORY        = 14
      DISK_FULL               = 15
      DP_TIMEOUT              = 16
      OTHERS                  = 17.

  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.



ENDFORM.                    "GET_DATA


*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PROCESS_DATA.
  CLEAR : IT_BDCDATA, IT_BDCMSGCOLL, IT_DISPLAY.
  REFRESH : IT_BDCDATA, IT_BDCMSGCOLL, IT_DISPLAY.

  LOOP AT IT_DATA.
    PERFORM CEK_CR USING IT_DATA-TRKORR.
  ENDLOOP.
*  OPT-DISMODE  = 'Q'. " Q / H / D
*  OPT-UPDMODE  = 'S'.
*  OPT-RACOMMIT = 'X'.
*  OPT-DEFSIZE  = 'X'.

  OPT-DISMODE = MODE. "A: show all dynpros "E: show dynpro on error only "N: do not display dynpro
  OPT-UPDMODE = 'A'. "S: synchronously "A: asynchronously "L: local
  OPT-DEFSIZE = 'X'.

  LOOP AT IT_DATA.
    CLEAR : IT_BDCDATA, IT_BDCMSGCOLL.
    REFRESH : IT_BDCDATA, IT_BDCMSGCOLL.

    PERFORM BDC_DYNPRO      USING 'SAPDFKB1' '0120'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'T030W-LTEXT(03)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=PICK'.
    PERFORM BDC_DYNPRO      USING 'SAPDFKB1' '1006'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'T004-KTOPL'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=ENTR'.
    PERFORM BDC_FIELD       USING 'T004-KTOPL'
                                  IT_DATA-KTOPL. "Chart of Account
    PERFORM BDC_DYNPRO      USING 'SAPL0F11' '0040'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'V_T030H-KTOPL'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=NEWL'.
    PERFORM BDC_DYNPRO      USING 'SAPL0F11' '0041'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'V_T030H-LHBEW'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=ANZG'.
    PERFORM BDC_FIELD       USING 'V_T030H-HKONT'
                                  IT_DATA-HKONT. "GL Account
    PERFORM BDC_FIELD       USING 'V_T030H-LSREA'
                                  IT_DATA-LSREA.
    PERFORM BDC_FIELD       USING 'V_T030H-LHREA'
                                  IT_DATA-LHREA.
    PERFORM BDC_FIELD       USING 'V_T030H-LSBEW'
                                  IT_DATA-LSBEW.
    PERFORM BDC_FIELD       USING 'V_T030H-LHBEW'
                                  IT_DATA-LHBEW.
    PERFORM BDC_FIELD       USING 'V_T030H-LKORR'
                                  IT_DATA-LKORR.
    PERFORM BDC_DYNPRO      USING 'SAPLSPO1' '0100'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=YES'.
    PERFORM BDC_DYNPRO      USING 'SAPLSTRD' '0300'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'KO008-TRKORR'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=LOCK'.
    PERFORM BDC_FIELD       USING 'KO008-TRKORR'
                                  IT_DATA-TRKORR.
    PERFORM BDC_DYNPRO      USING 'SAPL0F11' '0040'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'V_T030H-HKONT(01)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=BACK'.
    PERFORM BDC_DYNPRO      USING 'SAPDFKB1' '0120'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'T030W-LTEXT(03)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=BACK'.

    CALL TRANSACTION 'OBA1' USING IT_BDCDATA
*              MODE MODE
              OPTIONS FROM OPT
              MESSAGES INTO IT_BDCMSGCOLL.

    IF SY-SUBRC = 0.
      IT_DISPLAY-VICON = ICON_GREEN_LIGHT.
      IT_DISPLAY-HKONT = IT_DATA-HKONT.
      IT_DISPLAY-VSTATUS = 'Sukses'.
    ELSE.
      READ TABLE IT_BDCMSGCOLL INTO WA_BDCMSGCOLL INDEX 1.
      IF SY-SUBRC = 0.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            MSGID               = WA_BDCMSGCOLL-MSGID
            MSGNR               = WA_BDCMSGCOLL-MSGNR
            MSGV1               = WA_BDCMSGCOLL-MSGV1
            MSGV2               = WA_BDCMSGCOLL-MSGV2
            MSGV3               = WA_BDCMSGCOLL-MSGV3
            MSGV4               = WA_BDCMSGCOLL-MSGV4
          IMPORTING
            MESSAGE_TEXT_OUTPUT = IT_DISPLAY-VTEXT.
        IT_DISPLAY-VICON = ICON_RED_LIGHT.
        IT_DISPLAY-HKONT = IT_DATA-HKONT.
        IT_DISPLAY-VSTATUS = 'Gagal'.
      ELSE.
        IT_DISPLAY-VICON = ICON_RED_LIGHT.
        IT_DISPLAY-HKONT = IT_DATA-HKONT.
        IT_DISPLAY-VSTATUS = 'Gagal'.
      ENDIF.
    ENDIF.
    APPEND IT_DISPLAY.
    CLEAR : IT_DISPLAY, IT_BDCMSGCOLL, WA_BDCMSGCOLL.
    REFRESH : IT_BDCMSGCOLL.

  ENDLOOP.
ENDFORM.                    "PROCESS_DATA


*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PROG       text
*      -->SCR        text
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING PROG SCR.
  CLEAR IT_BDCDATA.
  IT_BDCDATA-PROGRAM = PROG.
  IT_BDCDATA-DYNPRO  = SCR.
  IT_BDCDATA-DYNBEGIN = 'X'.
  APPEND IT_BDCDATA.
ENDFORM.                    "BDC_DYNPRO


*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FNAM       text
*      -->FVAL       text
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
  CLEAR IT_BDCDATA.
  IT_BDCDATA-FNAM = FNAM.
  IT_BDCDATA-FVAL  = FVAL.
  APPEND IT_BDCDATA.
ENDFORM.                    "BDC_FIELD

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DISPLAY_DATA.
  PERFORM F_FIELD_CATALOG.
*  PERFORM F_LAYOUT.
  PERFORM F_LIST_DETAIL.
ENDFORM.                    "DISPLAY_DATA


*&---------------------------------------------------------------------*
*&      Form  F_FIELD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_FIELD_CATALOG .
  REFRESH : T_FIELDCAT.
  PERFORM F_ALV_FIELDCATG_ICON USING 'IT_DISPLAY' :
    'VICON' '' ''  '' ''  'Icon' '' ' ' '' '',
    'HKONT' '' ''  '' ''  'GL Account' '' ' ' '' '',
    'VSTATUS' '' ''  '' ''  'Status' '' ' ' '' '',
    'VTEXT' '' ''  '' ''  'Text' '' ' ' '' ''.
ENDFORM.                    " F_FIELD_CATALOG


*&---------------------------------------------------------------------*
*&      Form  F_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_LAYOUT .
  CLEAR WA_LAYOUT.
*  WA_LAYOUT-ZEBRA = 'X'.
*  WA_LAYOUT-CONFIRMATION_PROMPT = 'X'.
*  WA_LAYOUT-SUBTOTALS_TEXT = 'Sub Total'.
  WA_LAYOUT-TOTALS_TEXT = 'Total'.
  WA_LAYOUT-CELL_MERGE = 'X'.
  WA_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
ENDFORM.                    " F_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  F_LIST_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_LIST_DETAIL .
  D_REPID = SY-REPID.
  T_PRINT-NO_PRINT_LISTINFOS = 'X'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = D_REPID
*     I_CALLBACK_USER_COMMAND = ''
      IS_LAYOUT               = WA_LAYOUT
      IT_FIELDCAT             = T_FIELDCAT[]
      IT_EVENTS               = T_EVENTS[]
      IT_EVENT_EXIT           = T_EVENT_EXIT[]
*     I_DEFAULT               = 'X'
*     I_SAVE                  = 'A'
      IS_VARIANT              = WA_VARIANTE
      IS_PRINT                = T_PRINT
      IT_SORT                 = T_SORT[]
      IT_EXCLUDING            = T_EXCLUDING[]
      I_BYPASSING_BUFFER      = 'X'
    TABLES
      T_OUTTAB                = IT_DISPLAY
    EXCEPTIONS
      PROGRAM_ERROR           = 1
      OTHERS                  = 2.
ENDFORM.                    " F_LIST_DETAIL

*&---------------------------------------------------------------------*
*&      Form  CEK_CR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CEK_CR USING CR.
  DATA: RESULT(10) TYPE C.
  SELECT SINGLE TRKORR FROM E070 INTO RESULT WHERE TRKORR EQ CR AND TRSTATUS EQ 'D'.
  IF SY-SUBRC <> 0.
    MESSAGE 'CR TIDAK ADA..!!' TYPE 'E'.
  ENDIF.
ENDFORM.                    "CEK_CR