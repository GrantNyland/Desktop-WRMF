object frmRenameDailyDiversion: TfrmRenameDailyDiversion
  Left = 482
  Top = 225
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Rename Daily Diversion'
  ClientHeight = 102
  ClientWidth = 441
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblTop: TLabel
    Left = 37
    Top = 13
    Width = 120
    Height = 13
    Alignment = taRightJustify
    BiDiMode = bdLeftToRight
    Caption = 'Old Daily Diversion Name'
    ParentBiDiMode = False
  end
  object lblBottom: TLabel
    Left = 32
    Top = 43
    Width = 126
    Height = 13
    Alignment = taRightJustify
    BiDiMode = bdLeftToRight
    Caption = 'New Daily Diversion Name'
    ParentBiDiMode = False
  end
  object edtTop: TEdit
    Left = 162
    Top = 9
    Width = 267
    Height = 21
    TabOrder = 0
  end
  object edtBottom: TEdit
    Left = 162
    Top = 39
    Width = 267
    Height = 21
    TabOrder = 1
  end
  object btnOk: TButton
    Left = 264
    Top = 74
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 354
    Top = 74
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
