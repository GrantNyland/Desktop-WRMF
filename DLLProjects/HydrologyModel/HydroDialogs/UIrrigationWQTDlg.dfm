object IrrigationWQTDlg: TIrrigationWQTDlg
  Left = 349
  Top = 158
  BorderIcons = [biHelp]
  BorderStyle = bsNone
  Caption = 'WQT'
  ClientHeight = 405
  ClientWidth = 692
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
    Top = 370
    Width = 692
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
    Width = 692
    Height = 370
    Align = alClient
    TabOrder = 0
    object LblMaxWaterAllocation: TLabel
      Left = 10
      Top = 10
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Maximum water allocation (MCM/yr) :'
      Layout = tlCenter
    end
    object LblWaterAllocationInterpolationType: TLabel
      Left = 10
      Top = 35
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Water allocation interpolation type :'
      Layout = tlCenter
    end
    object LblRunOffModule: TLabel
      Left = 10
      Top = 60
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'RunOff module :'
      Layout = tlCenter
    end
    object LblTransferCanalSeepage: TLabel
      Left = 10
      Top = 85
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Transfer canal seepage :'
      Layout = tlCenter
    end
    object LblTransferCanalFlowLossProportion: TLabel
      Left = 10
      Top = 135
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Transfer canal flow loss proportion :'
      Layout = tlCenter
    end
    object LblTransferCanalSaltLossProportion: TLabel
      Left = 10
      Top = 160
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Transfer canal salt loss proportion :'
      Layout = tlCenter
    end
    object LblIrrigationEfficiencyFactor: TLabel
      Left = 10
      Top = 185
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Irrigation efficiency factor :'
      Layout = tlCenter
    end
    object LblReturnFlowFactor: TLabel
      Left = 10
      Top = 210
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Return flow factor :'
      Layout = tlCenter
    end
    object LblUpperZoneReturnFlowProportion: TLabel
      Left = 10
      Top = 235
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Upper zone return flow proportion :'
      Layout = tlCenter
    end
    object LblLowerZoneReturnFlowProportion: TLabel
      Left = 10
      Top = 260
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Lower zone return flow proportion :'
      Layout = tlCenter
    end
    object LblSaltConcentrationFactor: TLabel
      Left = 10
      Top = 285
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Salt concentration factor :'
      Layout = tlCenter
    end
    object LblLandSaltLossProportion: TLabel
      Left = 10
      Top = 310
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Land salt loss proportion :'
      Layout = tlCenter
    end
    object LblSaltLoad1: TLabel
      Left = 340
      Top = 10
      Width = 210
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Salt load applied to irrigated land 1 :'
      Layout = tlCenter
    end
    object LblSaltLoad2: TLabel
      Left = 340
      Top = 35
      Width = 210
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Salt load applied to irrigated land 2 :'
      Layout = tlCenter
    end
    object LblInitialSaltLoadUpperZone: TLabel
      Left = 340
      Top = 60
      Width = 210
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Initial salt load upper zone :'
      Layout = tlCenter
    end
    object LblInitialSaltLoadLowerZone: TLabel
      Left = 340
      Top = 85
      Width = 210
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Initial salt load lower zone :'
      Layout = tlCenter
    end
    object LblSoilMoistureStorageCapacityUpperZone: TLabel
      Left = 340
      Top = 110
      Width = 210
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Soil moisture storage capacity upper zone :'
      Layout = tlCenter
    end
    object LblSoilMoistureStorageCapacityLowerZone: TLabel
      Left = 340
      Top = 135
      Width = 210
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Soil moisture storage capacity lower zone :'
      Layout = tlCenter
    end
    object LblTargetSoilMoisture: TLabel
      Left = 340
      Top = 160
      Width = 210
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Target soil moisture :'
      Layout = tlCenter
    end
    object LblInitialSoilMoisture: TLabel
      Left = 340
      Top = 185
      Width = 210
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Initial soil moisture :'
      Layout = tlCenter
    end
    object LblEffectiveRainfallFactor1: TLabel
      Left = 340
      Top = 210
      Width = 210
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Effective rainfall factor 1 :'
      Layout = tlCenter
    end
    object LblEffectiveRainfallFactor2: TLabel
      Left = 340
      Top = 235
      Width = 210
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Effective rainfall factor 2 :'
      Layout = tlCenter
    end
  end
  object FXMLDocumentIn: TXMLDocument
    Left = 463
    Top = 427
    DOMVendorDesc = 'MSXML'
  end
  object FXMLDocumentOut: TXMLDocument
    Left = 495
    Top = 427
    DOMVendorDesc = 'MSXML'
  end
end
