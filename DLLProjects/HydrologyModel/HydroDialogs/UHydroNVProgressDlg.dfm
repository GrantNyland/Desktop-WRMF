object HydroNVProgressDlg: THydroNVProgressDlg
  Left = 442
  Top = 189
  BorderIcons = []
  Caption = 'Refreshing shapes ... Please wait!'
  ClientHeight = 63
  ClientWidth = 270
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object FNrOfShapesCaption: TLabel
    Left = 10
    Top = 40
    Width = 140
    Height = 13
    AutoSize = False
    Caption = 'Nr of shapes updated = '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object FNrOfShapesValue: TLabel
    Left = 150
    Top = 40
    Width = 30
    Height = 13
    AutoSize = False
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object FOfTotalCaption: TLabel
    Left = 185
    Top = 40
    Width = 70
    Height = 13
    AutoSize = False
    Caption = ' of '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object FProgressBar: TProgressBar
    Left = 10
    Top = 10
    Width = 250
    Height = 20
    TabOrder = 0
  end
end
