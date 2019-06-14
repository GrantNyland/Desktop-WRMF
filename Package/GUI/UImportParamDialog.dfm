object frmImportParam: TfrmImportParam
  Left = 277
  Top = 203
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Confirmation'
  ClientHeight = 115
  ClientWidth = 469
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblMessage: TLabel
    Left = 0
    Top = 0
    Width = 469
    Height = 74
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
    ExplicitWidth = 5
    ExplicitHeight = 13
  end
  object Panel1: TPanel
    Left = 0
    Top = 74
    Width = 469
    Height = 41
    Align = alBottom
    BevelInner = bvLowered
    TabOrder = 0
    object btnOK: TButton
      Left = 376
      Top = 6
      Width = 83
      Height = 29
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
    end
    object chkboxImportHyrology: TCheckBox
      Left = 8
      Top = 16
      Width = 169
      Height = 17
      Caption = 'Import all hydrology files'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      State = cbChecked
      TabOrder = 1
    end
  end
end
