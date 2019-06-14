object RunOffPropertiesDlg: TRunOffPropertiesDlg
  Left = 305
  Top = 159
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'RunOff Module'
  ClientHeight = 480
  ClientWidth = 753
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
    Top = 445
    Width = 753
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
    Width = 753
    Height = 445
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    TabOrder = 0
    object LblNetworkSequence: TLabel
      Left = 10
      Top = 10
      Width = 128
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Network sequence :'
      Layout = tlCenter
    end
    object LblModuleNumber: TLabel
      Left = 10
      Top = 35
      Width = 128
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Module number :'
      Layout = tlCenter
    end
    object LblLatitude: TLabel
      Left = 10
      Top = 85
      Width = 128
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Latitude :'
      Layout = tlCenter
    end
    object LblLongitude: TLabel
      Left = 10
      Top = 110
      Width = 128
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Longitude :'
      Layout = tlCenter
    end
    object LblRunOffName: TLabel
      Left = 10
      Top = 135
      Width = 128
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'RunOff name :'
      Layout = tlCenter
    end
    object LblVersionNo: TLabel
      Left = 10
      Top = 160
      Width = 128
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Version no :'
      Layout = tlCenter
    end
    object LblCatchmentArea: TLabel
      Left = 10
      Top = 185
      Width = 128
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Catchment area :'
      Layout = tlCenter
    end
    object LblCatchmentMAP: TLabel
      Left = 10
      Top = 210
      Width = 128
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Catchment MAP :'
      Layout = tlCenter
    end
    object LblRainfallFileName: TLabel
      Left = 10
      Top = 235
      Width = 128
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Rainfall file name :'
      Layout = tlCenter
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
