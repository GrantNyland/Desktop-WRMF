object MineSectionsDlg: TMineSectionsDlg
  Left = 459
  Top = 167
  Caption = 'Add/Remove Sections'
  ClientHeight = 315
  ClientWidth = 293
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
    Top = 280
    Width = 293
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 287
    DesignSize = (
      293
      35)
    object BtnCancel: TButton
      Left = 90
      Top = 3
      Width = 75
      Height = 25
      Anchors = [akLeft]
      Caption = 'Cancel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = BtnCancelClick
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
    Width = 293
    Height = 280
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitHeight = 287
    object LblSection: TLabel
      Left = 10
      Top = 180
      Width = 92
      Height = 13
      Caption = 'Section to remove :'
      Visible = False
    end
    object RgpAction: TRadioGroup
      Left = 9
      Top = 10
      Width = 185
      Height = 60
      Caption = ' Action '
      Items.Strings = (
        'Add a new section'
        'Remove an existing section')
      TabOrder = 0
      OnClick = OnControlClick
    end
    object RgpType: TRadioGroup
      Left = 10
      Top = 80
      Width = 185
      Height = 90
      Caption = ' Section type '
      Items.Strings = (
        'Opencast'
        'Undeground'
        'Slurry dump')
      TabOrder = 1
      OnClick = OnControlClick
    end
    object CbxSection: TComboBox
      Left = 10
      Top = 200
      Width = 200
      Height = 21
      Style = csDropDownList
      TabOrder = 2
      Visible = False
    end
  end
  object FXMLDocumentIn: TXMLDocument
    Left = 494
    Top = 349
    DOMVendorDesc = 'MSXML'
  end
  object FXMLDocumentOut: TXMLDocument
    Left = 526
    Top = 349
    DOMVendorDesc = 'MSXML'
  end
end
