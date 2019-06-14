object frmRunConfiguration: TfrmRunConfiguration
  Left = 0
  Top = 0
  Width = 451
  Height = 305
  Align = alClient
  TabOrder = 0
  ExplicitHeight = 304
  object gbRoof: TGroupBox
    Left = 0
    Top = 285
    Width = 451
    Height = 77
    Align = alTop
    Caption = 'Roof:'
    TabOrder = 0
    object lblRoofArea: TLabel
      Left = 5
      Top = 18
      Width = 200
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Roof Area:'
    end
    object lblRunoffCoefficient: TLabel
      Left = 5
      Top = 47
      Width = 200
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Runoff Coefficient:'
    end
    object lbPercent: TLabel
      Left = 397
      Top = 47
      Width = 44
      Height = 13
      Caption = '(0..100%)'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object lblRoofAreaUnits: TLabel
      Left = 394
      Top = 18
      Width = 17
      Height = 13
      Caption = '(m'#178')'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object edtRoofArea: TEdit
      Left = 207
      Top = 14
      Width = 184
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnExit = edtStartingVolumeExit
    end
    object edtRunoffCoefficient: TEdit
      Left = 207
      Top = 43
      Width = 184
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnExit = edtStartingVolumeExit
    end
  end
  object gbRunType: TGroupBox
    Left = 0
    Top = 41
    Width = 451
    Height = 134
    Align = alTop
    Caption = 'Run Type'
    TabOrder = 1
    object lblStartingVolume: TLabel
      Left = 6
      Top = 47
      Width = 200
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Starting Volume:'
    end
    object lblUpperLevel: TLabel
      Left = 6
      Top = 77
      Width = 200
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Level to Start Using Water:'
    end
    object lblLowerLevel: TLabel
      Left = 6
      Top = 105
      Width = 200
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Level to Stop Using Water:'
    end
    object lblStartingVolumeUnits: TLabel
      Left = 395
      Top = 47
      Width = 36
      Height = 13
      Caption = '(Litre/s)'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object lblParentName: TLabel
      Left = 175
      Top = 18
      Width = 31
      Height = 13
      Alignment = taRightJustify
      Caption = 'Name:'
      Enabled = False
    end
    object edtStartingVolume: TEdit
      Left = 207
      Top = 43
      Width = 184
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnExit = edtStartingVolumeExit
    end
    object edtUpperLevel: TEdit
      Left = 207
      Top = 73
      Width = 184
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnExit = edtStartingVolumeExit
    end
    object edtLowerLevel: TEdit
      Left = 207
      Top = 100
      Width = 184
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      OnExit = edtStartingVolumeExit
    end
    object edtName: TEdit
      Left = 207
      Top = 15
      Width = 245
      Height = 21
      Color = clInactiveCaption
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
    end
  end
  object gbStorage: TGroupBox
    Left = 0
    Top = 362
    Width = 451
    Height = 85
    Align = alTop
    Caption = 'Storage Tank/s:'
    TabOrder = 2
    object lblTankCount: TLabel
      Left = 5
      Top = 20
      Width = 200
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Number of tanks to consider:'
    end
    object lblTankVolume: TLabel
      Left = 3
      Top = 43
      Width = 200
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Tanks Size:'
    end
    object lblTankCountMax: TLabel
      Left = 395
      Top = 19
      Width = 32
      Height = 13
      Caption = 'Max(5)'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object lblVolumeUnits: TLabel
      Left = 726
      Top = 51
      Width = 30
      Height = 13
      Caption = 'Litre/s'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object edtTankCount: TEdit
      Left = 207
      Top = 16
      Width = 184
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnExit = edtTankCountExit
    end
    object strgrdTankVolume: TStringGrid
      Left = 207
      Top = 43
      Width = 513
      Height = 26
      ColCount = 1
      DefaultColWidth = 100
      DefaultRowHeight = 21
      FixedCols = 0
      RowCount = 1
      FixedRows = 0
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goRowSizing, goEditing, goAlwaysShowEditor]
      ParentFont = False
      TabOrder = 1
      OnSelectCell = strgrdTankVolumeSelectCell
      OnSetEditText = strgrdTankVolumeSetEditText
    end
    object cmboxTankSizes: TComboBox
      Left = 536
      Top = 16
      Width = 100
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      Visible = False
      OnExit = cmboxTankSizesExit
      Items.Strings = (
        '1000'#9
        '1500'#9
        '2200'#9
        '2500'#9
        '4500'#9
        '5000'#9
        '5500'#9
        '10000'#9
        '15000'#9
        '20000'#9)
    end
  end
  object gbHousehold: TGroupBox
    Left = 0
    Top = 175
    Width = 451
    Height = 110
    Align = alTop
    Caption = 'Household:'
    TabOrder = 3
    object lblHouseholdCount: TLabel
      Left = 5
      Top = 81
      Width = 200
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Number of Households:'
    end
    object lblMembersPerHousehold: TLabel
      Left = 5
      Top = 24
      Width = 200
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Number of persons per Household:'
    end
    object lblPerCapitaDemand: TLabel
      Left = 5
      Top = 53
      Width = 200
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Water Demand Volume per Person:'
    end
    object lblPerCapitaDemandUnits: TLabel
      Left = 395
      Top = 54
      Width = 36
      Height = 13
      Caption = '(Litre/s)'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object edtHouseholdCount: TEdit
      Left = 207
      Top = 77
      Width = 184
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnExit = edtStartingVolumeExit
    end
    object edtMembersPerHousehold: TEdit
      Left = 207
      Top = 20
      Width = 184
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnExit = edtStartingVolumeExit
    end
    object edtPerCapitaDemand: TEdit
      Left = 207
      Top = 49
      Width = 184
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      OnExit = edtStartingVolumeExit
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 0
    Width = 451
    Height = 41
    Align = alTop
    BevelInner = bvLowered
    ParentColor = True
    TabOrder = 4
    object lblSelect: TLabel
      Left = 707
      Top = 13
      Width = 188
      Height = 13
      Caption = 'Configuration Used in Run: None'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
    end
  end
end
