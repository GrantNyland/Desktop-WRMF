object MineOpencastPitPropertiesDlg: TMineOpencastPitPropertiesDlg
  Left = 380
  Top = 179
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Opencast pit'
  ClientHeight = 340
  ClientWidth = 727
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
    Top = 305
    Width = 727
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object BtnApply: TButton
      Left = 10
      Top = 5
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
      Top = 5
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
    Width = 727
    Height = 305
    Align = alClient
    TabOrder = 0
    object LblOpencastPitName: TLabel
      Left = 10
      Top = 10
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Opencast pit name :'
      Layout = tlCenter
    end
    object LblCoalReserveArea: TLabel
      Left = 10
      Top = 85
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Coal reserve area :'
      Layout = tlCenter
    end
    object LblWorkingArea: TLabel
      Left = 10
      Top = 110
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Workings area :'
      Layout = tlCenter
    end
    object LblCommissionYear: TLabel
      Left = 10
      Top = 35
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Commissioning year :'
      Layout = tlCenter
    end
    object LblCommissionMonth: TLabel
      Left = 265
      Top = 35
      Width = 40
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'month :'
      Layout = tlCenter
    end
    object LblDeCommissionYear: TLabel
      Left = 10
      Top = 59
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Decommissioning year :'
      Layout = tlCenter
    end
    object LblDeCommissionMonth: TLabel
      Left = 265
      Top = 60
      Width = 40
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'month :'
      Layout = tlCenter
    end
    object LblDisturbedArea: TLabel
      Left = 10
      Top = 135
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Disturbed area :'
      Layout = tlCenter
    end
    object LblRehabilitatedArea: TLabel
      Left = 10
      Top = 160
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Rehabilitated area :'
      Layout = tlCenter
    end
    object LblEvaporationArea: TLabel
      Left = 10
      Top = 185
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Pit evaporation area :'
      Layout = tlCenter
    end
    object LblDisturbedAreaRunOffFactor: TLabel
      Left = 10
      Top = 210
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Disturbed area run-off factor :'
      Layout = tlCenter
    end
    object LblDisturbedWorkingAreaRunOffFactor: TLabel
      Left = 10
      Top = 235
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Disturbed workings area run-off factor :'
      Layout = tlCenter
    end
    object LblWashOffParameter: TLabel
      Left = 10
      Top = 260
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Wash-off parameter :'
      Layout = tlCenter
    end
    object LblSulphateBuildUpRate: TLabel
      Left = 410
      Top = 10
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Sulphate build-up rate :'
      Layout = tlCenter
    end
    object LblInitialSaltMass: TLabel
      Left = 410
      Top = 35
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Initial salt mass :'
      Layout = tlCenter
    end
    object LblInspoilsStorageSeepage: TLabel
      Left = 410
      Top = 85
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Inspoils storage where seepage occurs :'
      Layout = tlCenter
    end
    object LblInspoilsStorageDecant: TLabel
      Left = 410
      Top = 60
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Inspoils storage where decant occurs :'
      Layout = tlCenter
    end
    object LblInspoilsStorageInitialVolume: TLabel
      Left = 410
      Top = 110
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Initial inspoils storage volume :'
      Layout = tlCenter
    end
    object LblMaxSeepageRate: TLabel
      Left = 410
      Top = 135
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Maximum seepage rate :'
      Layout = tlCenter
    end
    object LblSeepageEquationExponent: TLabel
      Left = 410
      Top = 160
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Exponent of seepage equation :'
      Layout = tlCenter
    end
    object LblPCDFullSurfaceArea: TLabel
      Left = 410
      Top = 186
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'PCD full surface area :'
      Layout = tlCenter
    end
    object LblPCDCapacity: TLabel
      Left = 410
      Top = 211
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'PCD Capacity :'
      Layout = tlCenter
    end
    object LblPCDInitialVolume: TLabel
      Left = 410
      Top = 236
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'PCD initial volume :'
      Layout = tlCenter
    end
    object LblInspoilsDamConcentration: TLabel
      Left = 410
      Top = 261
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Inspoils dam concentration :'
      Layout = tlCenter
    end
  end
  object FXMLDocumentOut: TXMLDocument
    Left = 448
    Top = 495
    DOMVendorDesc = 'MSXML'
  end
  object FXMLDocumentIn: TXMLDocument
    Left = 416
    Top = 495
    DOMVendorDesc = 'MSXML'
  end
end
