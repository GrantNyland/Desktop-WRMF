object frmChartAction: TfrmChartAction
  Left = 535
  Top = 247
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'frmChartAction'
  ClientHeight = 102
  ClientWidth = 366
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
    Left = 26
    Top = 13
    Width = 59
    Height = 13
    Alignment = taRightJustify
    BiDiMode = bdLeftToRight
    Caption = 'Chart Name:'
    ParentBiDiMode = False
  end
  object lblBottom: TLabel
    Left = 29
    Top = 43
    Width = 57
    Height = 13
    Alignment = taRightJustify
    BiDiMode = bdLeftToRight
    Caption = 'View Name:'
    ParentBiDiMode = False
  end
  object edtTop: TEdit
    Left = 90
    Top = 9
    Width = 267
    Height = 21
    TabOrder = 0
    OnChange = edtTopChange
  end
  object edtBottom: TEdit
    Left = 90
    Top = 39
    Width = 267
    Height = 21
    TabOrder = 1
  end
  object btnOk: TButton
    Left = 192
    Top = 74
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 282
    Top = 74
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
