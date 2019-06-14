object InflowRoutesDlg: TInflowRoutesDlg
  Left = 459
  Top = 167
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Inflow Routes'
  ClientHeight = 396
  ClientWidth = 542
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
    Top = 361
    Width = 542
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
    Width = 542
    Height = 361
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    TabOrder = 0
  end
  object FXMLDocumentIn: TXMLDocument
    Left = 340
    Top = 31
    DOMVendorDesc = 'MSXML'
  end
  object FXMLDocumentOut: TXMLDocument
    Left = 372
    Top = 31
    DOMVendorDesc = 'MSXML'
  end
end
