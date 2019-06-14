object frmOutputSummary: TfrmOutputSummary
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  ExplicitWidth = 641
  ExplicitHeight = 390
  object gboxSupply: TGroupBox
    Left = 0
    Top = 0
    Width = 641
    Height = 248
    Align = alTop
    Caption = 'Supply/Deficit'
    TabOrder = 0
    object strgrdSummary: TStringGrid
      Left = 2
      Top = 15
      Width = 637
      Height = 228
      Align = alTop
      ColCount = 7
      DefaultColWidth = 100
      DefaultRowHeight = 18
      RowCount = 11
      TabOrder = 0
    end
  end
  object gbox: TGroupBox
    Left = 0
    Top = 248
    Width = 641
    Height = 142
    Align = alClient
    Caption = 'Cost'
    TabOrder = 1
    object lblCost: TLabel
      Left = 112
      Top = 21
      Width = 41
      Height = 13
      Alignment = taRightJustify
      Caption = 'Cost per'
    end
    object edtCost: TEdit
      Left = 153
      Top = 18
      Width = 121
      Height = 21
      TabOrder = 0
      Text = '1'
    end
    object btnCalculate: TButton
      Left = 275
      Top = 18
      Width = 74
      Height = 21
      Caption = 'ReCalculate'
      TabOrder = 1
      OnClick = btnCalculateClick
    end
  end
end
