object frmPenaltyAction: TfrmPenaltyAction
  Left = 147
  Top = 163
  Width = 358
  Height = 197
  ActiveControl = btnOk
  Caption = 'Copy Penalty'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblCopy: TLabel
    Left = 8
    Top = 16
    Width = 50
    Height = 13
    Caption = 'Copy From'
  end
  object lblInsert: TLabel
    Left = 8
    Top = 48
    Width = 52
    Height = 13
    Caption = 'Paste After'
  end
  object lblDelete: TLabel
    Left = 8
    Top = 80
    Width = 68
    Height = 13
    Caption = 'Select Penalty'
  end
  object btnOk: TButton
    Left = 141
    Top = 112
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 232
    Top = 112
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object CmbCopy: TComboBox
    Left = 96
    Top = 16
    Width = 209
    Height = 21
    ItemHeight = 13
    TabOrder = 2
  end
  object CmbInsert: TComboBox
    Left = 96
    Top = 48
    Width = 209
    Height = 21
    ItemHeight = 13
    TabOrder = 3
  end
  object CmbDelete: TComboBox
    Left = 96
    Top = 80
    Width = 209
    Height = 21
    ItemHeight = 13
    TabOrder = 4
  end
end
