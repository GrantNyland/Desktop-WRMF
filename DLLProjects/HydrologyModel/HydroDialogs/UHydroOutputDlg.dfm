object HydroOutputDlg: THydroOutputDlg
  Left = 337
  Top = 127
  Caption = 'Hydrology Output Results'
  ClientHeight = 450
  ClientWidth = 762
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PnlTop: TPanel
    Left = 0
    Top = 0
    Width = 762
    Height = 45
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object LblResultType: TLabel
      Left = 300
      Top = 3
      Width = 77
      Height = 21
      AutoSize = False
      Caption = 'Result Type :'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
    end
    object LblNoData: TLabel
      Left = 385
      Top = 30
      Width = 113
      Height = 13
      Caption = 'No data available...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
      Visible = False
    end
    object LblSectionType: TLabel
      Left = 5
      Top = 3
      Width = 60
      Height = 21
      AutoSize = False
      Caption = 'Section :'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
    end
    object LblUnits: TLabel
      Left = 603
      Top = 5
      Width = 100
      Height = 21
      AutoSize = False
      Caption = '....'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
    end
    object CbxSectionType: TComboBox
      Left = 65
      Top = 3
      Width = 210
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = CbxSectionTypeChange
    end
    object CbxResultType: TComboBox
      Left = 385
      Top = 3
      Width = 210
      Height = 21
      Style = csDropDownList
      TabOrder = 1
      OnChange = CbxResultTypeChange
    end
  end
  object PgcHydroOutput: TPageControl
    Left = 0
    Top = 45
    Width = 762
    Height = 405
    ActivePage = TbsTable
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    ExplicitHeight = 412
    object TbsTable: TTabSheet
      Caption = 'Table'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GrdHydroOutput: TStringGrid
        Left = 0
        Top = 0
        Width = 754
        Height = 384
        Align = alClient
        ColCount = 13
        DefaultColWidth = 55
        DefaultRowHeight = 21
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        ColWidths = (
          55
          55
          55
          55
          55
          55
          55
          55
          55
          55
          55
          55
          55)
        RowHeights = (
          21
          21
          21
          21
          21)
      end
    end
    object TbsGraph: TTabSheet
      Caption = 'Graph'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object CrtHydroOutput: TChart
        Left = 0
        Top = 0
        Width = 754
        Height = 394
        Legend.Alignment = laBottom
        Legend.LegendStyle = lsSeries
        Title.Font.Color = clBlack
        Title.Font.Height = -13
        Title.Font.Style = [fsBold]
        Title.Text.Strings = (
          'TChart')
        BottomAxis.ExactDateTime = False
        BottomAxis.LabelsAngle = 90
        View3D = False
        Align = alClient
        BevelOuter = bvLowered
        TabOrder = 0
        DefaultCanvas = 'TGDIPlusCanvas'
        ColorPaletteIndex = 0
        object LsrData: TLineSeries
          Brush.BackColor = clDefault
          ClickableLine = False
          LinePen.Width = 2
          Pointer.InflateMargins = True
          Pointer.Style = psRectangle
          XValues.Name = 'X'
          XValues.Order = loAscending
          YValues.Name = 'Y'
          YValues.Order = loNone
        end
        object LsrObserved: TLineSeries
          Active = False
          SeriesColor = clBlue
          Brush.BackColor = clDefault
          LinePen.Width = 2
          Pointer.InflateMargins = True
          Pointer.Style = psRectangle
          XValues.Name = 'X'
          XValues.Order = loAscending
          YValues.Name = 'Y'
          YValues.Order = loNone
        end
      end
    end
  end
  object FXMLDocumentIn: TXMLDocument
    Left = 674
    Top = 93
    DOMVendorDesc = 'MSXML'
  end
  object FXMLDocumentOut: TXMLDocument
    Left = 706
    Top = 93
    DOMVendorDesc = 'MSXML'
  end
end
