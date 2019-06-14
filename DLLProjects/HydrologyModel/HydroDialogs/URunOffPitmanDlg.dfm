object RunOffPitmanDlg: TRunOffPitmanDlg
  Left = 305
  Top = 159
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Pitman'
  ClientHeight = 499
  ClientWidth = 756
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
    Top = 464
    Width = 756
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
    Width = 756
    Height = 464
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    TabOrder = 0
    object LblPitmanPOW: TLabel
      Left = 10
      Top = 10
      Width = 80
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'POW :'
      Layout = tlCenter
    end
    object LblPitmanSL: TLabel
      Left = 10
      Top = 35
      Width = 80
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'SL :'
      Layout = tlCenter
    end
    object LblPitmanST: TLabel
      Left = 10
      Top = 60
      Width = 80
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'ST :'
      Layout = tlCenter
    end
    object LblPitmanFT: TLabel
      Left = 10
      Top = 85
      Width = 80
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'FT :'
      Layout = tlCenter
    end
    object LblPitmanGW: TLabel
      Left = 10
      Top = 110
      Width = 80
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'GW :'
      Layout = tlCenter
    end
    object LblPitmanZMIN: TLabel
      Left = 10
      Top = 135
      Width = 80
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'ZMIN :'
      Layout = tlCenter
    end
    object LblPitmanZMAX: TLabel
      Left = 10
      Top = 165
      Width = 80
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'ZMAX :'
      Layout = tlCenter
    end
    object LblPitmanPI: TLabel
      Left = 10
      Top = 190
      Width = 80
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'PI :'
      Layout = tlCenter
    end
    object LblPitmanTL: TLabel
      Left = 10
      Top = 215
      Width = 80
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'TL :'
      Layout = tlCenter
    end
    object LblPitmanGL: TLabel
      Left = 10
      Top = 240
      Width = 80
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'GL :'
      Layout = tlCenter
    end
    object LblPitmanR: TLabel
      Left = 10
      Top = 265
      Width = 80
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'R :'
      Layout = tlCenter
    end
    object LblPitmanFF: TLabel
      Left = 10
      Top = 290
      Width = 80
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'FF :'
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
