object frmRunModel: TfrmRunModel
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  object ProgressRichEdit: TRichEdit
    Left = 0
    Top = 41
    Width = 451
    Height = 246
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    PlainText = True
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object ActionProgressBar: TProgressBar
    Left = 0
    Top = 287
    Width = 451
    Height = 17
    Align = alBottom
    TabOrder = 1
    ExplicitTop = 513
    ExplicitWidth = 1175
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 0
    Width = 451
    Height = 41
    Align = alTop
    BevelInner = bvLowered
    ParentColor = True
    TabOrder = 2
    ExplicitWidth = 1175
    DesignSize = (
      451
      41)
    object lblSelect: TLabel
      Left = 515
      Top = 13
      Width = 188
      Height = 13
      Caption = 'Configuration Used in Run: None'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
    end
    object btnStart: TColourBitBtn
      Left = 103
      Top = 7
      Width = 80
      Height = 28
      Anchors = [akLeft]
      Caption = 'Start'
      Enabled = False
      TabOrder = 0
      WordWrap = True
      OnClick = btnStartClick
      PressColor = clBlue
      Color = clInfoBk
    end
    object btnStop: TColourBitBtn
      Left = 197
      Top = 7
      Width = 80
      Height = 28
      Anchors = [akLeft]
      Caption = 'Stop'
      Enabled = False
      TabOrder = 1
      WordWrap = True
      OnClick = btnStopClick
      PressColor = clBlue
      Color = clInfoBk
    end
    object btnPrint: TColourBitBtn
      Left = 311
      Top = 7
      Width = 80
      Height = 28
      Anchors = [akLeft]
      Caption = 'Print'
      Enabled = False
      TabOrder = 2
      WordWrap = True
      OnClick = btnPrintClick
      PressColor = clBlue
      Color = clInfoBk
    end
    object btnSave: TColourBitBtn
      Left = 405
      Top = 7
      Width = 80
      Height = 28
      Anchors = [akLeft]
      Caption = 'Save'
      Enabled = False
      TabOrder = 3
      WordWrap = True
      OnClick = btnSaveClick
      PressColor = clBlue
      Color = clInfoBk
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 192
    Top = 144
  end
end
