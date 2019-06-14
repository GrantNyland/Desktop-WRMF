object ReservoirPropertiesDlg: TReservoirPropertiesDlg
  Left = 459
  Top = 167
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'ReservoirProperties'
  ClientHeight = 335
  ClientWidth = 500
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
    Top = 300
    Width = 500
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      500
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
    Width = 500
    Height = 300
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
    object LblReservoirName: TLabel
      Left = 10
      Top = 135
      Width = 100
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Reservoir name :'
      Layout = tlCenter
    end
    object LblMAP: TLabel
      Left = 10
      Top = 160
      Width = 100
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'MAP :'
      Layout = tlCenter
    end
    object LblRainfallFileName: TLabel
      Left = 10
      Top = 185
      Width = 100
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Rainfall file name :'
      Layout = tlCenter
    end
    object LblAreaPower: TLabel
      Left = 10
      Top = 210
      Width = 100
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Area power :'
      Layout = tlCenter
    end
    object LblSpillageRoute: TLabel
      Left = 10
      Top = 235
      Width = 100
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Spillage route :'
      Layout = tlCenter
    end
    object LblInitialStorage: TLabel
      Left = 10
      Top = 260
      Width = 100
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Initial storage :'
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
