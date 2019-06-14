object IrrigationPropertiesDlg: TIrrigationPropertiesDlg
  Left = 340
  Top = 161
  BorderIcons = [biHelp]
  BorderStyle = bsNone
  Caption = 'Irrigation Properties'
  ClientHeight = 457
  ClientWidth = 717
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
    Top = 422
    Width = 717
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
    Width = 717
    Height = 422
    Align = alClient
    TabOrder = 0
    object LblNetworkSequence: TLabel
      Left = 10
      Top = 10
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Network sequence :'
      Layout = tlCenter
    end
    object LblModuleNumber: TLabel
      Left = 10
      Top = 35
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Module number :'
      Layout = tlCenter
    end
    object LblLatitude: TLabel
      Left = 10
      Top = 85
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Latitude :'
      Layout = tlCenter
    end
    object LblLongitude: TLabel
      Left = 10
      Top = 110
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Longitude :'
      Layout = tlCenter
    end
    object LblIrrigationName: TLabel
      Left = 10
      Top = 135
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Irrigation name :'
      Layout = tlCenter
    end
    object LblVersionNo: TLabel
      Left = 10
      Top = 160
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Version no :'
      Layout = tlCenter
    end
    object LblModelType: TLabel
      Left = 10
      Top = 185
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Model type :'
      Layout = tlCenter
    end
    object LblLastUsedModelType: TLabel
      Left = 10
      Top = 210
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Last used model type'
      Layout = tlCenter
    end
    object LblMAP: TLabel
      Left = 10
      Top = 235
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'MAP :'
      Layout = tlCenter
    end
    object LblRainfallFileName: TLabel
      Left = 10
      Top = 260
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Rainfall file name :'
      Layout = tlCenter
    end
    object LblMaxAnnualIrrAllocation: TLabel
      Left = 10
      Top = 285
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Maximum annual irrigation allocation :'
      Layout = tlCenter
    end
    object LblAbstractionRoute: TLabel
      Left = 10
      Top = 310
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Abstraction route :'
      Layout = tlCenter
    end
    object LblReturnFlowRoute: TLabel
      Left = 10
      Top = 335
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Return flow route :'
      Layout = tlCenter
    end
    object LblReturnFlowPercentage: TLabel
      Left = 10
      Top = 360
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Return flow percentage :'
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
