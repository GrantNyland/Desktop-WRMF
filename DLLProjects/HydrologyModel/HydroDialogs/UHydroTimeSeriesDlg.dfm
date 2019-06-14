object HydroTimeSeriesDlg: THydroTimeSeriesDlg
  Left = 337
  Top = 127
  BorderStyle = bsNone
  Caption = 'Hydrology Output Results'
  ClientHeight = 457
  ClientWidth = 762
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PgcHydroTimeSeries: TPageControl
    Left = 0
    Top = 0
    Width = 762
    Height = 457
    ActivePage = TbsTable
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object TbsTable: TTabSheet
      Caption = 'Table'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GrdHydroTimeSeries: TStringGrid
        Left = 0
        Top = 0
        Width = 754
        Height = 429
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
      object CrtHydroTimeSeries: TChart
        Left = 0
        Top = 0
        Width = 754
        Height = 429
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
