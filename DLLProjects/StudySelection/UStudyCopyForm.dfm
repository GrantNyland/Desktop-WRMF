object frmStudyCopyForm: TfrmStudyCopyForm
  Left = 38
  Top = 166
  BorderIcons = [biSystemMenu, biHelp]
  Caption = 'Study Copy'
  ClientHeight = 373
  ClientWidth = 756
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object pnlButtons: TPanel
    Left = 0
    Top = 335
    Width = 756
    Height = 38
    Align = alBottom
    BevelInner = bvLowered
    TabOrder = 0
    ExplicitTop = 331
    object btnCancel: TBitBtn
      Left = 388
      Top = 5
      Width = 100
      Height = 28
      Cancel = True
      Caption = 'Cancel'
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        333333333333333333333333000033338833333333333333333F333333333333
        0000333911833333983333333388F333333F3333000033391118333911833333
        38F38F333F88F33300003339111183911118333338F338F3F8338F3300003333
        911118111118333338F3338F833338F3000033333911111111833333338F3338
        3333F8330000333333911111183333333338F333333F83330000333333311111
        8333333333338F3333383333000033333339111183333333333338F333833333
        00003333339111118333333333333833338F3333000033333911181118333333
        33338333338F333300003333911183911183333333383338F338F33300003333
        9118333911183333338F33838F338F33000033333913333391113333338FF833
        38F338F300003333333333333919333333388333338FFF830000333333333333
        3333333333333333333888330000333333333333333333333333333333333333
        0000}
      ModalResult = 2
      NumGlyphs = 2
      TabOrder = 0
    end
    object btnOk: TBitBtn
      Left = 250
      Top = 5
      Width = 100
      Height = 28
      Caption = 'Ok'
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333330000333333333333333333333333F33333333333
        00003333344333333333333333388F3333333333000033334224333333333333
        338338F3333333330000333422224333333333333833338F3333333300003342
        222224333333333383333338F3333333000034222A22224333333338F338F333
        8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
        33333338F83338F338F33333000033A33333A222433333338333338F338F3333
        0000333333333A222433333333333338F338F33300003333333333A222433333
        333333338F338F33000033333333333A222433333333333338F338F300003333
        33333333A222433333333333338F338F00003333333333333A22433333333333
        3338F38F000033333333333333A223333333333333338F830000333333333333
        333A333333333333333338330000333333333333333333333333333333333333
        0000}
      ModalResult = 1
      NumGlyphs = 2
      TabOrder = 1
    end
  end
  object pnlStudy: TPanel
    Left = 0
    Top = 0
    Width = 756
    Height = 133
    Align = alTop
    BevelInner = bvLowered
    TabOrder = 1
    object lblStudyCaption: TLabel
      Left = 48
      Top = 4
      Width = 30
      Height = 13
      Alignment = taRightJustify
      Caption = 'Study:'
    end
    object lblStudyLabelCaption: TLabel
      Left = 17
      Top = 39
      Width = 61
      Height = 13
      Alignment = taRightJustify
      Caption = 'Study Name:'
    end
    object lblStudyClientCaption: TLabel
      Left = 49
      Top = 75
      Width = 29
      Height = 13
      Alignment = taRightJustify
      Caption = 'Client:'
    end
    object lblStudyConsultantCaption: TLabel
      Left = 25
      Top = 111
      Width = 53
      Height = 13
      Alignment = taRightJustify
      Caption = 'Consultant:'
    end
    object lblStudyNumberCaption: TLabel
      Left = 374
      Top = 8
      Width = 70
      Height = 13
      Alignment = taRightJustify
      Caption = 'Study Number:'
    end
    object lblStudyDateCaption: TLabel
      Left = 388
      Top = 39
      Width = 56
      Height = 13
      Alignment = taRightJustify
      Caption = 'Study Date:'
    end
    object lblStudyDescrCaption: TLabel
      Left = 388
      Top = 94
      Width = 56
      Height = 13
      Alignment = taRightJustify
      Caption = 'Description:'
    end
    object edtStudy: TEdit
      Left = 81
      Top = 2
      Width = 250
      Height = 21
      TabOrder = 0
      OnExit = edtStudyExit
      OnKeyPress = FormKeyPress
    end
    object edtStudyLabel: TEdit
      Left = 81
      Top = 37
      Width = 250
      Height = 21
      TabOrder = 1
      OnExit = edtStudyExit
    end
    object edtStudyClient: TEdit
      Left = 81
      Top = 73
      Width = 250
      Height = 21
      TabOrder = 2
      OnExit = edtStudyExit
    end
    object edtStudyConsultant: TEdit
      Left = 81
      Top = 109
      Width = 250
      Height = 21
      TabOrder = 3
      OnExit = edtStudyExit
    end
    object edtStudyNumber: TEdit
      Left = 447
      Top = 6
      Width = 270
      Height = 21
      TabOrder = 4
      Text = '0'
      OnExit = edtStudyExit
    end
    object dtpStudyDate: TDateTimePicker
      Left = 447
      Top = 37
      Width = 270
      Height = 21
      Date = 37414.422419050900000000
      Time = 37414.422419050900000000
      TabOrder = 5
    end
    object memoStudyDescr: TMemo
      Left = 448
      Top = 68
      Width = 271
      Height = 63
      TabOrder = 6
      OnExit = edtStudyExit
    end
  end
  object pnlSubArea: TPanel
    Left = 0
    Top = 133
    Width = 756
    Height = 79
    Align = alClient
    BevelInner = bvLowered
    TabOrder = 2
    ExplicitHeight = 75
    object lblSubAreaCaption: TLabel
      Left = 34
      Top = 10
      Width = 44
      Height = 13
      Alignment = taRightJustify
      Caption = 'SubArea:'
    end
    object lblSubAreaLabelCaption: TLabel
      Left = 3
      Top = 45
      Width = 75
      Height = 13
      Alignment = taRightJustify
      Caption = 'SubArea Name:'
    end
    object lblSubAreDescr: TLabel
      Left = 388
      Top = 40
      Width = 56
      Height = 13
      Alignment = taRightJustify
      Caption = 'Description:'
    end
    object edtSubArea: TEdit
      Left = 81
      Top = 8
      Width = 250
      Height = 21
      TabOrder = 0
      OnExit = edtSubAreaExit
      OnKeyPress = FormKeyPress
    end
    object edtSubAreaLabel: TEdit
      Left = 81
      Top = 43
      Width = 250
      Height = 21
      TabOrder = 1
      OnExit = edtSubAreaExit
    end
    object memoSubAreaDescr: TMemo
      Left = 448
      Top = 9
      Width = 271
      Height = 63
      TabOrder = 2
      OnExit = edtSubAreaExit
    end
  end
  object pnlScenario: TPanel
    Left = 0
    Top = 212
    Width = 756
    Height = 123
    Align = alBottom
    BevelInner = bvLowered
    TabOrder = 3
    ExplicitTop = 208
    object lblScenarioCaption: TLabel
      Left = 33
      Top = 10
      Width = 45
      Height = 13
      Alignment = taRightJustify
      Caption = 'Scenario:'
    end
    object lblScenarioLabelCaption: TLabel
      Left = 2
      Top = 45
      Width = 76
      Height = 13
      Alignment = taRightJustify
      Caption = 'Scenario Name:'
    end
    object lblScenarioDescr: TLabel
      Left = 388
      Top = 41
      Width = 56
      Height = 13
      Alignment = taRightJustify
      Caption = 'Description:'
    end
    object lblVersion: TLabel
      Left = 41
      Top = 76
      Width = 38
      Height = 13
      Alignment = taRightJustify
      Caption = 'Version:'
    end
    object lblMaxChars: TLabel
      Left = 336
      Top = 12
      Width = 68
      Height = 13
      Caption = '(*Max 3 chars)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Visible = False
    end
    object edtScenario: TEdit
      Left = 81
      Top = 8
      Width = 250
      Height = 21
      TabOrder = 0
      OnExit = edtScenarioExit
      OnKeyPress = FormKeyPress
    end
    object edtScenarioLabel: TEdit
      Left = 81
      Top = 43
      Width = 250
      Height = 21
      TabOrder = 1
      OnExit = edtScenarioExit
    end
    object memoScenarioDescr: TMemo
      Left = 448
      Top = 8
      Width = 271
      Height = 63
      TabOrder = 2
      OnExit = edtScenarioExit
    end
    object edtVersion: TEdit
      Left = 82
      Top = 74
      Width = 250
      Height = 21
      TabOrder = 3
      Text = '6'
      OnExit = edtScenarioExit
    end
  end
  object dlgWYRMFileSelector: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 294
    Top = 4
  end
end
