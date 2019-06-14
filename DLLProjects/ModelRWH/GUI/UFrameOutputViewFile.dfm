object frmOutputViewFile: TfrmOutputViewFile
  Left = 0
  Top = 0
  Width = 1054
  Height = 335
  Align = alClient
  TabOrder = 0
  object redtFile: TRichEdit
    Left = 0
    Top = 51
    Width = 1054
    Height = 243
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    PlainText = True
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1054
    Height = 51
    Align = alTop
    BevelInner = bvLowered
    TabOrder = 1
    object rgViewFileText: TRadioGroup
      Left = 2
      Top = 6
      Width = 319
      Height = 40
      Caption = 'Select File To View as Text'
      Columns = 3
      Items.Strings = (
        'Daily'
        'Monthly'
        'Annual')
      TabOrder = 0
      OnClick = rgViewFileTextClick
    end
    object rgViewFileExcel: TRadioGroup
      Left = 362
      Top = 6
      Width = 319
      Height = 40
      Caption = 'Select File To View in Excel'
      Columns = 3
      Items.Strings = (
        'Daily'
        'Monthly'
        'Annual')
      TabOrder = 1
      OnClick = rgViewFileExcelClick
    end
  end
  object pnlFilename: TPanel
    Left = 0
    Top = 294
    Width = 1054
    Height = 41
    Align = alBottom
    TabOrder = 2
  end
end
