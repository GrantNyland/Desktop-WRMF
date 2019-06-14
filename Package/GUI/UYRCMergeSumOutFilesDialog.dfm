object YRCMergeSumOutFilesDialog: TYRCMergeSumOutFilesDialog
  Left = 269
  Top = 183
  Caption = 'Merge Sum.Out files'
  ClientHeight = 217
  ClientWidth = 439
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 177
    Width = 439
    Height = 3
    Cursor = crVSplit
    Align = alTop
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 439
    Height = 177
    Align = alTop
    BevelInner = bvLowered
    BevelOuter = bvNone
    TabOrder = 0
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 437
      Height = 24
      Align = alTop
      TabOrder = 0
      object Label1: TLabel
        Left = 32
        Top = 5
        Width = 159
        Height = 13
        Caption = 'Old (Select Target Draft to delete)'
      end
      object Label2: TLabel
        Left = 248
        Top = 5
        Width = 154
        Height = 13
        Caption = 'New (Select Target Draft to add)'
      end
    end
    object CheckListBox1: TCheckListBox
      Left = 1
      Top = 25
      Width = 216
      Height = 151
      OnClickCheck = CheckListBox1ClickCheck
      Align = alLeft
      ItemHeight = 13
      TabOrder = 1
    end
    object CheckListBox2: TCheckListBox
      Left = 217
      Top = 25
      Width = 221
      Height = 151
      OnClickCheck = CheckListBox1ClickCheck
      Align = alClient
      ItemHeight = 13
      TabOrder = 2
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 180
    Width = 439
    Height = 37
    Align = alBottom
    BevelInner = bvLowered
    BevelOuter = bvNone
    TabOrder = 1
    object Button1: TButton
      Left = 141
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      Enabled = False
      ModalResult = 1
      TabOrder = 0
    end
    object Button2: TButton
      Left = 232
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
