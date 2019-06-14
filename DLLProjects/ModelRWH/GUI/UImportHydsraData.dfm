object frmImportRawDailyData: TfrmImportRawDailyData
  Left = 0
  Top = 0
  Caption = 'Import Hydstra/SWAS Daily Ranfall Data'
  ClientHeight = 461
  ClientWidth = 465
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object spbtnLoadDir: TSpeedButton
    Left = 429
    Top = 88
    Width = 34
    Height = 22
    Caption = '...'
    OnClick = spbtnLoadDirClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 116
    Width = 455
    Height = 280
    TabOrder = 0
  end
  object edtFilePath: TLabeledEdit
    Left = 56
    Top = 88
    Width = 367
    Height = 21
    EditLabel.Width = 41
    EditLabel.Height = 13
    EditLabel.Caption = 'File Path'
    LabelPosition = lpLeft
    TabOrder = 1
  end
  object btnLoadFilesToDB: TBitBtn
    Left = 272
    Top = 402
    Width = 81
    Height = 25
    Caption = 'Load To DB'
    TabOrder = 2
    OnClick = btnLoadFilesToDBClick
  end
  object Animate1: TAnimate
    Left = 0
    Top = 0
    Width = 465
    Height = 82
    Align = alTop
    AutoSize = False
    StopFrame = 12
  end
  object btnClose: TButton
    Left = 388
    Top = 402
    Width = 75
    Height = 25
    Caption = 'Close'
    ModalResult = 8
    TabOrder = 4
    OnClick = btnCloseClick
  end
  object ProgressBar: TProgressBar
    Left = 8
    Top = 433
    Width = 455
    Height = 17
    TabOrder = 5
  end
end
