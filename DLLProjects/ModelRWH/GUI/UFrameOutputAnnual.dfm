object frmOutputAnnual: TfrmOutputAnnual
  Left = 0
  Top = 0
  Width = 451
  Height = 305
  Align = alClient
  TabOrder = 0
  OnResize = FrameResize
  ExplicitHeight = 304
  object Splitter1: TSplitter
    Left = 0
    Top = 264
    Width = 451
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitWidth = 738
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 451
    Height = 264
    Align = alTop
    BevelInner = bvLowered
    TabOrder = 0
    object strgrdData: TStringGrid
      Left = 2
      Top = 41
      Width = 447
      Height = 200
      Align = alClient
      Color = clInfoBk
      ColCount = 6
      DefaultColWidth = 100
      DefaultRowHeight = 16
      RowCount = 2
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSizing, goColSizing]
      TabOrder = 0
    end
    object strgrdTotals: TStringGrid
      Left = 2
      Top = 241
      Width = 447
      Height = 21
      Align = alBottom
      Color = clBtnFace
      ColCount = 6
      DefaultRowHeight = 16
      FixedCols = 0
      RowCount = 1
      FixedRows = 0
      TabOrder = 1
      Visible = False
    end
    object pnlOptions: TPanel
      Left = 2
      Top = 2
      Width = 447
      Height = 39
      Align = alTop
      BevelInner = bvLowered
      TabOrder = 2
      object lbTankVolume: TLabel
        Left = 76
        Top = 13
        Width = 45
        Height = 13
        Alignment = taRightJustify
        Caption = 'Tank Size'
      end
      object lblMinx: TLabel
        Left = 324
        Top = 13
        Width = 53
        Height = 13
        Alignment = taRightJustify
        Caption = 'Minimum X:'
      end
      object lblNote: TLabel
        Left = 602
        Top = 13
        Width = 224
        Height = 13
        Caption = 'Note:Double click chart for more chart options.'
      end
      object edtMinX: TEdit
        Left = 378
        Top = 10
        Width = 121
        Height = 21
        TabOrder = 0
        Text = '0'
        OnExit = edtMinXExit
      end
    end
  end
  object chartData: TChart
    Left = 0
    Top = 267
    Width = 451
    Height = 37
    BackWall.Brush.Gradient.EndColor = 11118482
    BackWall.Brush.Gradient.Visible = True
    BackWall.Transparent = False
    Border.Color = 14645801
    Border.Width = 7
    BottomWall.Brush.Gradient.EndColor = 16580349
    BottomWall.Brush.Gradient.StartColor = 3114493
    BottomWall.Brush.Gradient.Visible = True
    Gradient.Direction = gdDiagonalDown
    Gradient.EndColor = clSilver
    Gradient.Visible = True
    LeftWall.Brush.Gradient.EndColor = 2413052
    LeftWall.Brush.Gradient.StartColor = 900220
    LeftWall.Brush.Gradient.Visible = True
    Legend.Brush.Gradient.Direction = gdTopBottom
    Legend.Brush.Gradient.EndColor = clYellow
    Legend.Brush.Gradient.StartColor = clWhite
    Legend.Brush.Gradient.Visible = True
    Legend.LegendStyle = lsSeries
    Legend.Symbol.Pen.Visible = False
    MarginBottom = 0
    MarginLeft = 0
    MarginRight = 0
    MarginTop = 0
    PrintProportional = False
    Title.Text.Strings = (
      'Assurance of Supply'
      'Demand   : '
      'Roof Size : ')
    AxisBehind = False
    BottomAxis.Automatic = False
    BottomAxis.AutomaticMaximum = False
    BottomAxis.AutomaticMinimum = False
    BottomAxis.Maximum = 100.000000000000000000
    BottomAxis.Title.Caption = 'Reliability'
    LeftAxis.Title.Caption = 'Number of Days of Supply per Year'
    RightAxis.Title.Angle = 90
    Shadow.Color = clWhite
    Shadow.HorizSize = 10
    Shadow.VertSize = 10
    View3D = False
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    OnDblClick = chartDataDblClick
    DefaultCanvas = 'TGDIPlusCanvas'
    PrintMargins = (
      0
      0
      0
      0)
    ColorPaletteIndex = 0
    object Series1: TLineSeries
      Marks.Brush.Gradient.Visible = True
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
      Data = {0000000000}
    end
    object Series2: TLineSeries
      Marks.Brush.Gradient.Visible = True
      SeriesColor = clBlack
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
      Data = {0000000000}
    end
    object Series3: TLineSeries
      Marks.Brush.Gradient.Visible = True
      SeriesColor = 8421440
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
      Data = {0000000000}
    end
    object Series4: TLineSeries
      Marks.Brush.Gradient.Visible = True
      SeriesColor = 16744448
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
      Data = {0000000000}
    end
    object Series5: TLineSeries
      Marks.Brush.Gradient.Visible = True
      SeriesColor = 16777088
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
      Data = {0000000000}
    end
    object Series6: TLineSeries
      Marks.Brush.Gradient.Visible = True
      SeriesColor = clBlue
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
      Data = {0000000000}
    end
    object Series7: TLineSeries
      Marks.Brush.Gradient.Visible = True
      SeriesColor = 4227327
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
      Data = {0000000000}
    end
    object Series8: TLineSeries
      Marks.Brush.Gradient.Visible = True
      SeriesColor = 16711808
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
      Data = {0000000000}
    end
    object Series9: TLineSeries
      Marks.Brush.Gradient.Visible = True
      SeriesColor = clFuchsia
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
      Data = {0000000000}
    end
    object Series10: TLineSeries
      Marks.Brush.Gradient.Visible = True
      SeriesColor = 33023
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
      Data = {0000000000}
    end
  end
end
