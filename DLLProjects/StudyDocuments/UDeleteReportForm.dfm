object frmDeleteReportForm: TfrmDeleteReportForm
  Left = 98
  Top = 135
  BorderIcons = [biSystemMenu, biHelp]
  Caption = 'Delete Report'
  ClientHeight = 231
  ClientWidth = 547
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pnlButtons: TPanel
    Left = 0
    Top = 193
    Width = 547
    Height = 38
    Align = alBottom
    BevelInner = bvLowered
    TabOrder = 0
    object btnCancel: TBitBtn
      Left = 314
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
      Left = 176
      Top = 5
      Width = 100
      Height = 28
      Caption = 'Ok'
      Enabled = False
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
    Width = 547
    Height = 193
    Align = alClient
    BevelInner = bvLowered
    TabOrder = 1
    object lblDocCategoryCaption: TLabel
      Left = 13
      Top = 18
      Width = 97
      Height = 13
      Alignment = taRightJustify
      Caption = 'Document Category:'
    end
    object lblDocNameCaption: TLabel
      Left = 39
      Top = 87
      Width = 71
      Height = 13
      Alignment = taRightJustify
      Caption = 'Document File:'
    end
    object lblMenuCaption: TLabel
      Left = 41
      Top = 121
      Width = 69
      Height = 13
      Alignment = taRightJustify
      Caption = 'Menu Caption:'
    end
    object lblID: TLabel
      Left = 34
      Top = 155
      Width = 78
      Height = 13
      Alignment = taRightJustify
      Caption = 'Report Identifier:'
    end
    object lblReportType: TLabel
      Left = 49
      Top = 51
      Width = 62
      Height = 13
      Alignment = taRightJustify
      Caption = 'Report Type:'
    end
    object cmbDocCategory: TComboBox
      Left = 115
      Top = 15
      Width = 400
      Height = 21
      Sorted = True
      TabOrder = 0
      OnChange = cmbDocCategoryChange
    end
    object edtMenuCaption: TEdit
      Left = 115
      Top = 118
      Width = 400
      Height = 21
      ReadOnly = True
      TabOrder = 1
    end
    object edtID: TEdit
      Left = 115
      Top = 152
      Width = 400
      Height = 21
      ReadOnly = True
      TabOrder = 2
    end
    object cmbReportType: TComboBox
      Left = 115
      Top = 48
      Width = 400
      Height = 21
      Sorted = True
      TabOrder = 3
      OnChange = cmbReportTypeChange
    end
    object cmbFileName: TComboBox
      Left = 115
      Top = 82
      Width = 400
      Height = 21
      Sorted = True
      TabOrder = 4
      OnChange = cmbFileNameChange
    end
  end
  object dlgFileSelector: TOpenDialog
    Filter = 'Acrobat Documents|*.pdf|Word Documents|*.doc'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofNoTestFileCreate, ofEnableIncludeNotify]
    Left = 398
    Top = 220
  end
end
