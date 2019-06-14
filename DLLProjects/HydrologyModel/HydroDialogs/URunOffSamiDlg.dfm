object RunOffSamiDlg: TRunOffSamiDlg
  Left = 289
  Top = 103
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Sami'
  ClientHeight = 485
  ClientWidth = 693
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
    Top = 450
    Width = 693
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
    Width = 693
    Height = 450
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    TabOrder = 0
    object LblAquiferThickness: TLabel
      Left = 10
      Top = 10
      Width = 230
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Aquifer thickness :'
      Layout = tlCenter
    end
    object LblSamiStorativity: TLabel
      Left = 10
      Top = 35
      Width = 230
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Storativity :'
      Layout = tlCenter
    end
    object LblInitialAquiferStorage: TLabel
      Left = 10
      Top = 60
      Width = 230
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Initial aquifer storage :'
      Layout = tlCenter
    end
    object LblStaticWaterLevel: TLabel
      Left = 10
      Top = 85
      Width = 230
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Static water level :'
      Layout = tlCenter
    end
    object LblUnsaturatedStorage: TLabel
      Left = 10
      Top = 110
      Width = 230
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Unsaturated storage :'
      Layout = tlCenter
    end
    object LblInitialUnsaturatedZoneStorage: TLabel
      Left = 10
      Top = 135
      Width = 230
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Initial unsaturated zone storage :'
      Layout = tlCenter
    end
    object LblPerculationPower: TLabel
      Left = 10
      Top = 160
      Width = 230
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Perculation power :'
      Layout = tlCenter
    end
    object LblMaxDischarge: TLabel
      Left = 10
      Top = 185
      Width = 230
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Maximum discharge :'
      Layout = tlCenter
    end
    object LblInteractionCurvePower: TLabel
      Left = 10
      Top = 210
      Width = 230
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Surface/ground water interaction power :'
      Layout = tlCenter
    end
    object LblMaxHydrologicalGradient: TLabel
      Left = 10
      Top = 235
      Width = 230
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Max hydrological gradient :'
      Layout = tlCenter
    end
    object LblSamiTransmissivity: TLabel
      Left = 10
      Top = 260
      Width = 230
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Transmissivity :'
      Layout = tlCenter
    end
    object LblBoreholeDistanceToRiver: TLabel
      Left = 11
      Top = 285
      Width = 230
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Borehole distance to river :'
      Layout = tlCenter
    end
    object LblGroundWaterEvaporationArea: TLabel
      Left = 10
      Top = 310
      Width = 230
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Groundwater evaporation area :'
      Layout = tlCenter
    end
    object LblInterflowLag: TLabel
      Left = 10
      Top = 335
      Width = 230
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Interflow lag :'
      Layout = tlCenter
    end
    object LblRechargeAveragedNoMonths: TLabel
      Left = 5
      Top = 360
      Width = 235
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'No of months over which recharge are averaged :'
      Layout = tlCenter
    end
    object LblSamiGPOW: TLabel
      Left = 410
      Top = 11
      Width = 80
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'GPOW :'
      Layout = tlCenter
    end
    object LblSamiHGSL: TLabel
      Left = 410
      Top = 35
      Width = 80
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'HGSL :'
      Layout = tlCenter
    end
    object LblSamiHGGW: TLabel
      Left = 410
      Top = 60
      Width = 80
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'HGGW :'
      Layout = tlCenter
    end
    object LblK2: TLabel
      Left = 410
      Top = 85
      Width = 80
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'K2 :'
      Layout = tlCenter
    end
    object LblK3: TLabel
      Left = 410
      Top = 110
      Width = 80
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'K3 :'
      Layout = tlCenter
    end
    object LblSamiPOW: TLabel
      Left = 410
      Top = 135
      Width = 80
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'POW :'
      Layout = tlCenter
    end
    object LblSamiSL: TLabel
      Left = 410
      Top = 160
      Width = 80
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'SL :'
      Layout = tlCenter
    end
    object LblSamiST: TLabel
      Left = 410
      Top = 185
      Width = 80
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'ST :'
      Layout = tlCenter
    end
    object LblSamiFT: TLabel
      Left = 410
      Top = 210
      Width = 80
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'FT :'
      Layout = tlCenter
    end
    object LblSamiGW: TLabel
      Left = 410
      Top = 235
      Width = 80
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'GW :'
      Layout = tlCenter
    end
    object LblSamiZMIN: TLabel
      Left = 410
      Top = 260
      Width = 80
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'ZMIN :'
      Layout = tlCenter
    end
    object LblSamiZMAX: TLabel
      Left = 410
      Top = 285
      Width = 80
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'ZMAX :'
      Layout = tlCenter
    end
    object LblSamiPI: TLabel
      Left = 410
      Top = 310
      Width = 80
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'PI :'
      Layout = tlCenter
    end
    object LblSamiTL: TLabel
      Left = 410
      Top = 335
      Width = 80
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'TL :'
      Layout = tlCenter
    end
    object LblSamiGL: TLabel
      Left = 410
      Top = 360
      Width = 80
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'GL :'
      Layout = tlCenter
    end
    object LblSamiR: TLabel
      Left = 410
      Top = 385
      Width = 80
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'R :'
      Layout = tlCenter
    end
    object LblSamiFF: TLabel
      Left = 410
      Top = 410
      Width = 80
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'FF :'
      Layout = tlCenter
    end
    object EdtAquiferThickness: TWRMFEdit
      Left = 250
      Top = 10
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 0
      OnExit = ControlExit
      Active = True
      PropertyName = 'AquiferThickness'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtSamiStorativity: TWRMFEdit
      Left = 250
      Top = 35
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 1
      OnExit = ControlExit
      Active = True
      PropertyName = 'Storativity'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtInitialAquiferStorage: TWRMFEdit
      Left = 250
      Top = 60
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 2
      OnExit = ControlExit
      Active = True
      PropertyName = 'InitialAquiferStorage'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtStaticWaterLevel: TWRMFEdit
      Left = 250
      Top = 85
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 3
      OnExit = ControlExit
      Active = True
      PropertyName = 'StaticWaterLevel'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtUnsaturatedStorage: TWRMFEdit
      Left = 250
      Top = 110
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 4
      OnExit = ControlExit
      Active = True
      PropertyName = 'UnsaturatedStorage'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtInitialUnsaturatedZoneStorage: TWRMFEdit
      Left = 250
      Top = 135
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 5
      OnExit = ControlExit
      Active = True
      PropertyName = 'InitialUnsaturatedZoneStorage'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtPerculationPower: TWRMFEdit
      Left = 250
      Top = 160
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 6
      OnExit = ControlExit
      Active = True
      PropertyName = 'PerculationPower'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtMaxDischarge: TWRMFEdit
      Left = 250
      Top = 185
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 7
      OnExit = ControlExit
      Active = True
      PropertyName = 'MaxDischarge'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtInteractionCurvePower: TWRMFEdit
      Left = 250
      Top = 210
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 8
      OnExit = ControlExit
      Active = True
      PropertyName = 'InteractionCurvePower'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtMaxHydrologicalGradient: TWRMFEdit
      Left = 250
      Top = 235
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 9
      OnExit = ControlExit
      Active = True
      PropertyName = 'MaxHydrologicalGradient'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtSamiTransmissivity: TWRMFEdit
      Left = 250
      Top = 260
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 10
      OnExit = ControlExit
      Active = True
      PropertyName = 'Transmissivity'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtBoreholeDistanceToRiver: TWRMFEdit
      Left = 250
      Top = 285
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 11
      OnExit = ControlExit
      Active = True
      PropertyName = 'BoreholeDistanceToRiver'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtGroundWaterEvaporationArea: TWRMFEdit
      Left = 250
      Top = 310
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 12
      OnExit = ControlExit
      Active = True
      PropertyName = 'GroundWaterEvaporationArea'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtInterflowLag: TWRMFEdit
      Left = 250
      Top = 335
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 13
      OnExit = ControlExit
      Active = True
      PropertyName = 'InterflowLag'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtRechargeAveragedNoMonths: TWRMFEdit
      Left = 250
      Top = 360
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 14
      OnExit = ControlExit
      Active = True
      PropertyName = 'RechargeAveragedNoMonths'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object ChbUseAbstractions: TWRMFCheckBox
      Left = 92
      Top = 385
      Width = 180
      Height = 21
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Caption = 'Use abstractions in simulation :'
      Color = clBtnFace
      TabOrder = 15
      OnExit = ControlExit
      Active = True
      PropertyName = 'UseAbstractions'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
      Checked = False
    end
    object EdtSamiGPOW: TWRMFEdit
      Left = 495
      Top = 10
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 16
      OnExit = ControlExit
      Active = True
      PropertyName = 'SamiGPOW'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtSamiHGSL: TWRMFEdit
      Left = 495
      Top = 35
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 17
      OnExit = ControlExit
      Active = True
      PropertyName = 'SamiHGSL'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtSamiHGGW: TWRMFEdit
      Left = 495
      Top = 60
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 18
      OnExit = ControlExit
      Active = True
      PropertyName = 'SamiHGGW'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtK2: TWRMFEdit
      Left = 495
      Top = 85
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 19
      OnExit = ControlExit
      Active = True
      PropertyName = 'SamiK2'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtK3: TWRMFEdit
      Left = 495
      Top = 110
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 20
      OnExit = ControlExit
      Active = True
      PropertyName = 'SamiK3'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtSamiPOW: TWRMFEdit
      Left = 495
      Top = 135
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 21
      OnExit = ControlExit
      Active = True
      PropertyName = 'SamiPOW'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtSamiSL: TWRMFEdit
      Left = 495
      Top = 160
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 22
      OnExit = ControlExit
      Active = True
      PropertyName = 'SamiSL'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtSamiST: TWRMFEdit
      Left = 495
      Top = 185
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 23
      OnExit = ControlExit
      Active = True
      PropertyName = 'SamiST'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtSamiFT: TWRMFEdit
      Left = 495
      Top = 210
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 24
      OnExit = ControlExit
      Active = True
      PropertyName = 'SamiFT'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtSamiGW: TWRMFEdit
      Left = 495
      Top = 235
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 25
      OnExit = ControlExit
      Active = True
      PropertyName = 'SamiGW'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtSamiZMIN: TWRMFEdit
      Left = 495
      Top = 260
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 26
      OnExit = ControlExit
      Active = True
      PropertyName = 'SamiZMIN'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtSamiZMAX: TWRMFEdit
      Left = 495
      Top = 285
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 27
      OnExit = ControlExit
      Active = True
      PropertyName = 'SamiZMAX'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtSamiPI: TWRMFEdit
      Left = 495
      Top = 310
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 28
      OnExit = ControlExit
      Active = True
      PropertyName = 'SamiPI'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtSamiTL: TWRMFEdit
      Left = 495
      Top = 335
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 29
      OnExit = ControlExit
      Active = True
      PropertyName = 'SamiTL'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtSamiGL: TWRMFEdit
      Left = 495
      Top = 360
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 30
      OnExit = ControlExit
      Active = True
      PropertyName = 'SamiGL'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtSamiR: TWRMFEdit
      Left = 495
      Top = 385
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 31
      OnExit = ControlExit
      Active = True
      PropertyName = 'SamiR'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtSamiFF: TWRMFEdit
      Left = 495
      Top = 410
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 32
      OnExit = ControlExit
      Active = True
      PropertyName = 'SamiFF'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
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
