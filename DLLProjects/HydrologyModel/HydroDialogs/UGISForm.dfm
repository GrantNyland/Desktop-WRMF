object GISForm: TGISForm
  Left = 405
  Top = 187
  Caption = 'GIS'
  ClientHeight = 441
  ClientWidth = 448
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object FPnlButtons: TPanel
    Left = 0
    Top = 406
    Width = 448
    Height = 35
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 413
    object FBtnOK: TButton
      Left = 5
      Top = 5
      Width = 75
      Height = 25
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
    end
    object FBtnCancel: TButton
      Left = 85
      Top = 5
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
