object Form1: TForm1
  Left = 207
  Top = 90
  Width = 696
  Height = 480
  Caption = 'Form1'
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
  object StringGrid1: TStringGrid
    Left = 0
    Top = 41
    Width = 688
    Height = 412
    Align = alClient
    ColCount = 4
    DefaultColWidth = 150
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 688
    Height = 41
    Align = alTop
    Caption = 'Panel1'
    TabOrder = 1
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Button1'
      TabOrder = 0
      OnClick = Button1Click
    end
    object RadioGroup1: TRadioGroup
      Left = 96
      Top = 0
      Width = 585
      Height = 33
      Columns = 8
      ItemIndex = 0
      Items.Strings = (
        'Null'
        'HorDet'
        'HorAve'
        'LinDet'
        'LinReg'
        'Quad'
        'Tri')
      TabOrder = 1
    end
  end
end
