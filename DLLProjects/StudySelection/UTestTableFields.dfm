object Form1: TForm1
  Left = 191
  Top = 145
  Caption = 'Form1'
  ClientHeight = 371
  ClientWidth = 943
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 943
    Height = 41
    Align = alTop
    BevelInner = bvLowered
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 115
  end
  object Panel2: TPanel
    Left = 0
    Top = 271
    Width = 943
    Height = 100
    Align = alBottom
    BevelInner = bvLowered
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = -100
    ExplicitWidth = 115
    object Label1: TLabel
      Left = 0
      Top = 27
      Width = 32
      Height = 13
      Caption = 'Label1'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label2: TLabel
      Left = 272
      Top = 52
      Width = 105
      Height = 13
      Caption = 'Select model to delete'
    end
    object Label3: TLabel
      Left = 0
      Top = 72
      Width = 32
      Height = 13
      Caption = 'Label3'
    end
    object btnRefresh: TButton
      Left = 382
      Top = 6
      Width = 50
      Height = 33
      Caption = 'Refresh'
      TabOrder = 0
      OnClick = btnRefreshClick
    end
    object btnExit: TButton
      Left = 494
      Top = 6
      Width = 50
      Height = 33
      Caption = 'Close'
      TabOrder = 1
      OnClick = btnExitClick
    end
    object ProgressBar1: TProgressBar
      Left = 0
      Top = 8
      Width = 369
      Height = 17
      Max = 0
      Step = 1
      TabOrder = 2
    end
    object btnStop: TButton
      Left = 438
      Top = 6
      Width = 50
      Height = 33
      Caption = 'Stop'
      Enabled = False
      TabOrder = 3
      OnClick = btnStopClick
    end
    object btnDeleteModelData: TButton
      Left = 556
      Top = 46
      Width = 132
      Height = 25
      Caption = '&Delete model data'
      TabOrder = 4
      OnClick = btnDeleteModelDataClick
    end
    object cbxModel: TComboBox
      Left = 400
      Top = 48
      Width = 145
      Height = 21
      TabOrder = 5
      Items.Strings = (
        'Yield'
        'Planning'
        'Hydrology'
        'Rainfall'
        'All')
    end
    object ProgressBar2: TProgressBar
      Left = 0
      Top = 48
      Width = 249
      Height = 17
      Max = 0
      Step = 1
      TabOrder = 6
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 41
    Width = 943
    Height = 230
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 2
    ExplicitWidth = 115
    ExplicitHeight = 452
    object TabSheet1: TTabSheet
      Caption = 'Progress'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 107
      ExplicitHeight = 424
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 935
        Height = 202
        Align = alClient
        Lines.Strings = (
          'Memo1')
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Fields errors'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Memo4: TMemo
        Left = 0
        Top = 0
        Width = 935
        Height = 202
        Align = alClient
        Lines.Strings = (
          'Memo1')
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Index Errors'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Memo5: TMemo
        Left = 0
        Top = 0
        Width = 935
        Height = 202
        Align = alClient
        Lines.Strings = (
          'Memo1')
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Hint = 
        'Tables in database that have no entry in the table definition pa' +
        's file'
      Caption = 'Missing tables'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Memo2: TMemo
        Left = 0
        Top = 0
        Width = 935
        Height = 202
        Align = alClient
        Lines.Strings = (
          'Memo1')
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object TabSheet3: TTabSheet
      Hint = 
        'Tables in the table definition pas file that have no entry in da' +
        'tabase  '
      Caption = 'Tables that no longer Exist'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Memo3: TMemo
        Left = 0
        Top = 0
        Width = 935
        Height = 202
        Align = alClient
        Lines.Strings = (
          'Memo1')
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'Schema Errors'
      ImageIndex = 5
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Memo6: TMemo
        Left = 0
        Top = 0
        Width = 935
        Height = 202
        Align = alClient
        Lines.Strings = (
          'Memo1')
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object TabSheet7: TTabSheet
      Caption = 'Cleared table'
      ImageIndex = 6
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Memo7: TMemo
        Left = 0
        Top = 0
        Width = 691
        Height = 424
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
  object Database1: TDatabase
    AliasName = 'WRMFDatabase'
    DatabaseName = 'WRIMSDB'
    LoginPrompt = False
    SessionName = 'Default'
    Left = 168
    Top = 96
  end
  object Table1: TTable
    DatabaseName = 'WRIMSDB'
    Left = 316
    Top = 121
  end
  object Query1: TQuery
    DatabaseName = 'WRIMSDB'
    Left = 228
    Top = 97
  end
end
