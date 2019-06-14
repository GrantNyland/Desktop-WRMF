object FrmClassRPatchR: TFrmClassRPatchR
  Left = 222
  Top = 184
  Caption = 'FrmClassRPatchR'
  ClientHeight = 381
  ClientWidth = 669
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object PnlTop: TPanel
    Left = 0
    Top = 0
    Width = 669
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
  end
  object ScrClient: TScrollBox
    Left = 0
    Top = 30
    Width = 669
    Height = 351
    Align = alClient
    TabOrder = 1
    ExplicitHeight = 347
    object MmoText: TMemo
      Left = 0
      Top = 0
      Width = 665
      Height = 343
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
    end
  end
end
