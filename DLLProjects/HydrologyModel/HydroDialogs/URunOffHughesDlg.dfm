object RunOffHughesDlg: TRunOffHughesDlg
  Left = 300
  Top = 199
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Hughes'
  ClientHeight = 435
  ClientWidth = 755
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PnlBottom: TPanel
    Left = 0
    Top = 400
    Width = 755
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object BtnApply: TButton
      Left = 10
      Top = 3
      Width = 75
      Height = 25
      Caption = 'Apply'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = BtnApplyClick
    end
    object BtnReset: TButton
      Left = 90
      Top = 3
      Width = 75
      Height = 25
      Caption = 'Reset'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = BtnResetClick
    end
  end
  object ScrClient: TScrollBox
    Left = 0
    Top = 0
    Width = 755
    Height = 400
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    TabOrder = 0
    object LblInflowRouteNo: TLabel
      Left = 10
      Top = 10
      Width = 175
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Inflow route no from upstream :'
      Layout = tlCenter
    end
    object LblInfluenceROMNo: TLabel
      Left = 10
      Top = 35
      Width = 175
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'RunOff influencing groundwater :'
      Layout = tlCenter
    end
    object LblGroundWaterModel: TLabel
      Left = 10
      Top = 60
      Width = 175
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Groundwater model :'
      Layout = tlCenter
    end
    object LblDrainageDensity: TLabel
      Left = 10
      Top = 85
      Width = 175
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Drainage density :'
      Layout = tlCenter
    end
    object LblNumberOfReaches: TLabel
      Left = 10
      Top = 135
      Width = 175
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Number of reaches :'
      Layout = tlCenter
    end
    object LblRiparianAreaWidthPercentage: TLabel
      Left = 10
      Top = 160
      Width = 175
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Riparian area width percentage :'
      Layout = tlCenter
    end
    object LblRiparianStripFactor: TLabel
      Left = 10
      Top = 185
      Width = 175
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Riparian strip factor :'
      Layout = tlCenter
    end
    object LblRestWaterlevel: TLabel
      Left = 10
      Top = 210
      Width = 175
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Rest water level :'
      Layout = tlCenter
    end
    object LblTransmissivity: TLabel
      Left = 10
      Top = 234
      Width = 175
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Transmissivity :'
      Layout = tlCenter
    end
    object LblStorativity: TLabel
      Left = 10
      Top = 260
      Width = 175
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Storativity :'
      Layout = tlCenter
    end
    object LblGroundwaterSlope: TLabel
      Left = 10
      Top = 285
      Width = 175
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Initial groundwater slope :'
      Layout = tlCenter
    end
    object LblHughesHGSL: TLabel
      Left = 135
      Top = 310
      Width = 50
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'HGSL :'
      Layout = tlCenter
    end
    object LblHughesGPOW: TLabel
      Left = 135
      Top = 335
      Width = 50
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'GPOW :'
      Layout = tlCenter
    end
    object LblHughesTLGMax: TLabel
      Left = 300
      Top = 10
      Width = 50
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'TLGMax :'
      Layout = tlCenter
    end
    object LblHughesHGGW: TLabel
      Left = 300
      Top = 35
      Width = 50
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'HGGW :'
      Layout = tlCenter
    end
    object LblHughesPOW: TLabel
      Left = 300
      Top = 60
      Width = 50
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'POW :'
      Layout = tlCenter
    end
    object LblHughesSL: TLabel
      Left = 300
      Top = 85
      Width = 50
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'SL :'
      Layout = tlCenter
    end
    object LblHughesST: TLabel
      Left = 300
      Top = 110
      Width = 50
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'ST :'
      Layout = tlCenter
    end
    object LblHughesFT: TLabel
      Left = 300
      Top = 135
      Width = 50
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'FT :'
      Layout = tlCenter
    end
    object LblHughesGW: TLabel
      Left = 300
      Top = 160
      Width = 50
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'GW :'
      Layout = tlCenter
    end
    object LblHughesZMIN: TLabel
      Left = 300
      Top = 185
      Width = 50
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'ZMIN :'
      Layout = tlCenter
    end
    object LblHughesZMAX: TLabel
      Left = 300
      Top = 210
      Width = 50
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'ZMAX :'
      Layout = tlCenter
    end
    object LblHughesPI: TLabel
      Left = 300
      Top = 235
      Width = 50
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'PI :'
      Layout = tlCenter
    end
    object LblHughesTL: TLabel
      Left = 300
      Top = 260
      Width = 50
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'TL :'
      Layout = tlCenter
    end
    object LblHughesGL: TLabel
      Left = 300
      Top = 285
      Width = 50
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'GL :'
      Layout = tlCenter
    end
    object LblHughesR: TLabel
      Left = 300
      Top = 310
      Width = 50
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'R :'
      Layout = tlCenter
    end
    object LblHughesFF: TLabel
      Left = 300
      Top = 335
      Width = 50
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'FF :'
      Layout = tlCenter
    end
    object LblAnnualUpperZoneAbstraction: TLabel
      Left = 465
      Top = 10
      Width = 170
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Annual upper zone abstraction :'
      Layout = tlCenter
    end
    object LblAnnualRiparianZoneAbstraction: TLabel
      Left = 465
      Top = 34
      Width = 170
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Annual riparian zone abstraction :'
      Layout = tlCenter
    end
    object LblGrdHughesHeading: TLabel
      Left = 485
      Top = 60
      Width = 218
      Height = 13
      Caption = 'Monthly abstraction demand percentages (%) :'
    end
    object ChbUseNoOfReaches: TWRMFCheckBox
      Left = 10
      Top = 110
      Width = 285
      Height = 21
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Caption = 'Use number of reaches to calculate drainage density :'
      Color = clBtnFace
      TabOrder = 4
      OnExit = ControlExit
      Active = True
      PropertyName = 'UseNoOfReaches'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
      Checked = False
    end
    object EdtHughesGPOW: TWRMFEdit
      Left = 195
      Top = 335
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 13
      OnExit = ControlExit
      Active = True
      PropertyName = 'HughesGPOW'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtHughesHGSL: TWRMFEdit
      Left = 195
      Top = 310
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 12
      OnExit = ControlExit
      Active = True
      PropertyName = 'HughesHGSL'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtGroundwaterSlope: TWRMFEdit
      Left = 195
      Top = 285
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 11
      OnExit = ControlExit
      Active = True
      PropertyName = 'GroundwaterSlope'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtStorativity: TWRMFEdit
      Left = 195
      Top = 260
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 10
      OnExit = ControlExit
      Active = True
      PropertyName = 'Storativity'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtTransmissivity: TWRMFEdit
      Left = 195
      Top = 234
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 9
      OnExit = ControlExit
      Active = True
      PropertyName = 'Transmissivity'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtRestWaterlevel: TWRMFEdit
      Left = 195
      Top = 210
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 8
      OnExit = ControlExit
      Active = True
      PropertyName = 'RestWaterlevel'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtRiparianStripFactor: TWRMFEdit
      Left = 195
      Top = 185
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 7
      OnExit = ControlExit
      Active = True
      PropertyName = 'RiparianStripFactor'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtRiparianAreaWidthPercentage: TWRMFEdit
      Left = 195
      Top = 160
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 6
      OnExit = ControlExit
      Active = True
      PropertyName = 'RiparianAreaWidthPercentage'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtNumberOfReaches: TWRMFEdit
      Left = 195
      Top = 135
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 5
      OnExit = ControlExit
      Active = True
      PropertyName = 'NumberOfReaches'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtDrainageDensity: TWRMFEdit
      Left = 195
      Top = 85
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 3
      OnExit = ControlExit
      Active = True
      PropertyName = 'DrainageDensity'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object CbxGroundwaterModel: TWRMFComboBox
      Left = 195
      Top = 60
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 2
      OnExit = ControlExit
      Active = True
      PropertyName = 'GroundWaterModel'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
      ItemIndex = -1
    end
    object CbxInfluenceROMNo: TWRMFComboBox
      Left = 195
      Top = 35
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 1
      OnExit = ControlExit
      Active = True
      PropertyName = 'InfluenceROMNo'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
      ItemIndex = -1
    end
    object CbxInflowRouteNo: TWRMFComboBox
      Left = 195
      Top = 10
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 0
      OnExit = ControlExit
      Active = True
      PropertyName = 'InflowRouteNo'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
      ItemIndex = -1
    end
    object EdtHughesFF: TWRMFEdit
      Left = 360
      Top = 335
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 27
      OnExit = ControlExit
      Active = True
      PropertyName = 'HughesFF'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtHughesR: TWRMFEdit
      Left = 360
      Top = 310
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 26
      OnExit = ControlExit
      Active = True
      PropertyName = 'HughesR'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtHughesGL: TWRMFEdit
      Left = 360
      Top = 285
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 25
      OnExit = ControlExit
      Active = True
      PropertyName = 'HughesGL'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtHughesTL: TWRMFEdit
      Left = 360
      Top = 260
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 24
      OnExit = ControlExit
      Active = True
      PropertyName = 'HughesTL'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtHughesPI: TWRMFEdit
      Left = 360
      Top = 235
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 23
      OnExit = ControlExit
      Active = True
      PropertyName = 'HughesPI'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtHughesZMAX: TWRMFEdit
      Left = 360
      Top = 210
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 22
      OnExit = ControlExit
      Active = True
      PropertyName = 'HughesZMAX'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtHughesZMIN: TWRMFEdit
      Left = 360
      Top = 185
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 21
      OnExit = ControlExit
      Active = True
      PropertyName = 'HughesZMIN'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtHughesGW: TWRMFEdit
      Left = 360
      Top = 160
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 20
      OnExit = ControlExit
      Active = True
      PropertyName = 'HughesGW'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtHughesFT: TWRMFEdit
      Left = 360
      Top = 135
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 19
      OnExit = ControlExit
      Active = True
      PropertyName = 'HughesFT'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtHughesST: TWRMFEdit
      Left = 360
      Top = 110
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 18
      OnExit = ControlExit
      Active = True
      PropertyName = 'HughesST'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtHughesSL: TWRMFEdit
      Left = 360
      Top = 85
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 17
      OnExit = ControlExit
      Active = True
      PropertyName = 'HughesSL'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtHughesPOW: TWRMFEdit
      Left = 360
      Top = 60
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 16
      OnExit = ControlExit
      Active = True
      PropertyName = 'HughesPOW'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtHughesHGGW: TWRMFEdit
      Left = 360
      Top = 35
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 15
      OnExit = ControlExit
      Active = True
      PropertyName = 'HughesHGGW'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtHughesTLGMax: TWRMFEdit
      Left = 360
      Top = 10
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 14
      OnExit = ControlExit
      Active = True
      PropertyName = 'HughesTLGMax'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtAnnualRiparianZoneAbstraction: TWRMFEdit
      Left = 640
      Top = 34
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 29
      OnExit = ControlExit
      Active = True
      PropertyName = 'AnnualRiparianZoneAbstraction'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
    end
    object EdtAnnualUpperZoneAbstraction: TWRMFEdit
      Left = 640
      Top = 10
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 28
      OnExit = ControlExit
      Active = True
      PropertyName = 'AnnualUpperZoneAbstraction'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
    end
    object GrdHughes: TWRMFGrid
      Left = 485
      Top = 80
      Width = 248
      Height = 290
      ColCount = 3
      DefaultColWidth = 80
      DefaultRowHeight = 21
      RowCount = 13
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs]
      TabOrder = 30
      OnExit = GrdHughesExit
      DblClickColAutoSize = True
      ColAutoSizeIgnoreHeading = False
      AutoSizeFixedCols = False
      OnDataCellExit = GrdHughesDataCellExit
      CellsInfo.AssignOption = 1
      CellsInfo.NoOfHeadingRows = 1
      CellsInfo.NoOfHeadingCols = 1
      CellsInfo.DataListName = 'DataList'
      CellsInfo = <
        item
          PropertyName = 'Month'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 0
          Column = 0
          Active = True
        end
        item
          PropertyName = 'UpperZone'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 0
          Column = 1
          Active = True
        end
        item
          PropertyName = 'Month'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 1
          Column = 0
          Active = True
        end
        item
          PropertyName = 'UpperZone'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 1
          Column = 1
          Active = True
        end
        item
          PropertyName = 'RiparianZone'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 0
          Column = 2
          Active = True
        end
        item
          PropertyName = 'RiparianZone'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 1
          Column = 2
          Active = True
        end
        item
          PropertyName = 'Month'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 2
          Column = 0
          Active = True
        end
        item
          PropertyName = 'UpperZone'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 2
          Column = 1
          Active = True
        end
        item
          PropertyName = 'RiparianZone'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 2
          Column = 2
          Active = True
        end
        item
          PropertyName = 'Month'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 3
          Column = 0
          Active = True
        end
        item
          PropertyName = 'UpperZone'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 3
          Column = 1
          Active = True
        end
        item
          PropertyName = 'RiparianZone'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 3
          Column = 2
          Active = True
        end
        item
          PropertyName = 'Month'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 4
          Column = 0
          Active = True
        end
        item
          PropertyName = 'UpperZone'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 4
          Column = 1
          Active = True
        end
        item
          PropertyName = 'RiparianZone'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 4
          Column = 2
          Active = True
        end
        item
          PropertyName = 'Month'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 5
          Column = 0
          Active = True
        end
        item
          PropertyName = 'UpperZone'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 5
          Column = 1
          Active = True
        end
        item
          PropertyName = 'RiparianZone'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 5
          Column = 2
          Active = True
        end
        item
          PropertyName = 'Month'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 6
          Column = 0
          Active = True
        end
        item
          PropertyName = 'UpperZone'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 6
          Column = 1
          Active = True
        end
        item
          PropertyName = 'RiparianZone'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 6
          Column = 2
          Active = True
        end
        item
          PropertyName = 'Month'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 7
          Column = 0
          Active = True
        end
        item
          PropertyName = 'UpperZone'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 7
          Column = 1
          Active = True
        end
        item
          PropertyName = 'RiparianZone'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 7
          Column = 2
          Active = True
        end
        item
          PropertyName = 'Month'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 8
          Column = 0
          Active = True
        end
        item
          PropertyName = 'UpperZone'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 8
          Column = 1
          Active = True
        end
        item
          PropertyName = 'RiparianZone'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 8
          Column = 2
          Active = True
        end
        item
          PropertyName = 'Month'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 9
          Column = 0
          Active = True
        end
        item
          PropertyName = 'UpperZone'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 9
          Column = 1
          Active = True
        end
        item
          PropertyName = 'RiparianZone'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 9
          Column = 2
          Active = True
        end
        item
          PropertyName = 'Month'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 10
          Column = 0
          Active = True
        end
        item
          PropertyName = 'UpperZone'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 10
          Column = 1
          Active = True
        end
        item
          PropertyName = 'RiparianZone'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 10
          Column = 2
          Active = True
        end
        item
          PropertyName = 'Month'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 11
          Column = 0
          Active = True
        end
        item
          PropertyName = 'UpperZone'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 11
          Column = 1
          Active = True
        end
        item
          PropertyName = 'RiparianZone'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 11
          Column = 2
          Active = True
        end
        item
          PropertyName = 'Month'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 12
          Column = 0
          Active = True
        end
        item
          PropertyName = 'UpperZone'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 12
          Column = 1
          Active = True
        end
        item
          PropertyName = 'RiparianZone'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 12
          Column = 2
          Active = True
        end>
      WrapHeaderText = False
    end
  end
  object FXMLDocumentIn: TXMLDocument
    Left = 569
    Top = 464
    DOMVendorDesc = 'MSXML'
  end
  object FXMLDocumentOut: TXMLDocument
    Left = 601
    Top = 464
    DOMVendorDesc = 'MSXML'
  end
end
