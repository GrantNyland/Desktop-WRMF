object frmGaugeSelection: TfrmGaugeSelection
  Left = 0
  Top = 0
  Width = 451
  Height = 305
  Align = alClient
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object gbSelection: TGroupBox
    Left = 0
    Top = 0
    Width = 451
    Height = 79
    Align = alTop
    Caption = 'Selection'
    TabOrder = 0
    object lblType: TLabel
      Left = 30
      Top = 21
      Width = 28
      Height = 13
      Alignment = taRightJustify
      Caption = 'Type:'
    end
    object lblName: TLabel
      Left = 28
      Top = 52
      Width = 31
      Height = 13
      Alignment = taRightJustify
      Caption = 'Name:'
    end
    object lblStationNumber: TLabel
      Left = 265
      Top = 21
      Width = 78
      Height = 13
      Alignment = taRightJustify
      Caption = 'Station Number:'
    end
    object lblStationType: TLabel
      Left = 278
      Top = 51
      Width = 65
      Height = 13
      Alignment = taRightJustify
      Caption = 'Station Type:'
    end
    object edtType: TEdit
      Left = 60
      Top = 17
      Width = 174
      Height = 21
      TabOrder = 0
    end
    object edtName: TEdit
      Left = 60
      Top = 47
      Width = 174
      Height = 21
      TabOrder = 1
    end
    object edtStationNumber: TEdit
      Left = 344
      Top = 17
      Width = 137
      Height = 21
      TabOrder = 2
    end
    object edtStationType: TEdit
      Left = 345
      Top = 47
      Width = 137
      Height = 21
      TabOrder = 3
    end
    object rdgDailyDataSource: TRadioGroup
      Left = 520
      Top = 17
      Width = 305
      Height = 46
      Caption = 'Daily Data Source'
      Columns = 2
      Items.Strings = (
        'DWA/SWAS'
        'WRC Patch')
      TabOrder = 4
      OnClick = rdgDailyDataSourceClick
    end
  end
  object pnlLayerSection: TPanel
    Left = 261
    Top = 79
    Width = 190
    Height = 226
    Align = alRight
    TabOrder = 1
    ExplicitHeight = 225
    object rgLayerSelection: TRadioGroup
      Left = 16
      Top = 6
      Width = 177
      Height = 283
      Caption = 'Select Station By'
      ItemIndex = 3
      Items.Strings = (
        'Province'
        'Water Management Area'
        'District'
        'Rainfall Station'
        'Quaternary'
        'Local Municipality'
        'District Municipality')
      TabOrder = 0
      OnClick = rgLayerSelectionClick
    end
    object importRawDailyData: TButton
      Left = 15
      Top = 295
      Width = 119
      Height = 25
      Caption = 'Import Daily Data'
      TabOrder = 1
      OnClick = importRawDailyDataClick
    end
  end
  object pmGisSelection: TPopupMenu
    Left = 168
    Top = 168
    object ClearAll1: TMenuItem
      Caption = 'Clear All'
      OnClick = ClearAll1Click
    end
    object DeleteStation1: TMenuItem
      Caption = 'Delete Station'
      OnClick = DeleteStation1Click
    end
  end
end
