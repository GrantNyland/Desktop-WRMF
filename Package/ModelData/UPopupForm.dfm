object frmPopupForm: TfrmPopupForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  ClientHeight = 409
  ClientWidth = 819
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnlButton: TPanel
    Left = 0
    Top = 0
    Width = 819
    Height = 65
    Align = alTop
    TabOrder = 0
    object btnOK: TButton
      Left = 24
      Top = 21
      Width = 75
      Height = 25
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 136
      Top = 21
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
