object MinePropertiesDlg: TMinePropertiesDlg
  Left = 459
  Top = 167
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'MineProperties'
  ClientHeight = 380
  ClientWidth = 640
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
    Top = 345
    Width = 640
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      640
      35)
    object BtnReset: TButton
      Left = 90
      Top = 3
      Width = 75
      Height = 25
      Anchors = [akLeft]
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
    object BtnApply: TButton
      Left = 10
      Top = 3
      Width = 75
      Height = 25
      Anchors = [akLeft]
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
  end
  object ScrClient: TScrollBox
    Left = 0
    Top = 0
    Width = 640
    Height = 345
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    TabOrder = 0
    object LblNetworkSequence: TLabel
      Left = 10
      Top = 10
      Width = 100
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Network sequence :'
      Layout = tlCenter
    end
    object LblModuleNumber: TLabel
      Left = 10
      Top = 35
      Width = 100
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Module number :'
      Layout = tlCenter
    end
    object LblLatitude: TLabel
      Left = 10
      Top = 85
      Width = 100
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Latitude :'
      Layout = tlCenter
    end
    object LblLongitude: TLabel
      Left = 10
      Top = 110
      Width = 100
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Longitude :'
      Layout = tlCenter
    end
    object LblMineName: TLabel
      Left = 10
      Top = 135
      Width = 100
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Mine name :'
      Layout = tlCenter
    end
    object LblMAP: TLabel
      Left = 10
      Top = 185
      Width = 100
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'MAP :'
      Layout = tlCenter
    end
    object LblRainfallFileName: TLabel
      Left = 10
      Top = 210
      Width = 100
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Rainfall file name :'
      Layout = tlCenter
    end
    object LblRunOffModuleNo: TLabel
      Left = 340
      Top = 10
      Width = 140
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Run-off module :'
      Layout = tlCenter
    end
    object LblOutflowRouteNoToRiver: TLabel
      Left = 340
      Top = 35
      Width = 140
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Outflow route to river :'
      Layout = tlCenter
    end
    object LblPlantArea: TLabel
      Left = 340
      Top = 85
      Width = 140
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Plant area :'
      Layout = tlCenter
    end
    object LblVersionNo: TLabel
      Left = 10
      Top = 160
      Width = 100
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Version no :'
      Layout = tlCenter
    end
    object LblOutflowRouteNoToPCD: TLabel
      Left = 340
      Top = 60
      Width = 140
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Outflow route to central PCD :'
      Layout = tlCenter
    end
    object LblPlantAreaRunOffFactor: TLabel
      Left = 340
      Top = 110
      Width = 140
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Plant area run-off factor :'
      Layout = tlCenter
    end
    object LblSaltBuildUpRate: TLabel
      Left = 340
      Top = 135
      Width = 140
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Salt build-up rate :'
      Layout = tlCenter
    end
    object LblSaltWashOffFactor: TLabel
      Left = 340
      Top = 160
      Width = 140
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Salt wash-off factor :'
      Layout = tlCenter
    end
    object LblInitialSaltStore: TLabel
      Left = 340
      Top = 185
      Width = 140
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Initial salt store :'
      Layout = tlCenter
    end
  end
  object FXMLDocumentIn: TXMLDocument
    Left = 250
    Top = 300
    DOMVendorDesc = 'MSXML'
  end
  object FXMLDocumentOut: TXMLDocument
    Left = 282
    Top = 300
    DOMVendorDesc = 'MSXML'
  end
end
