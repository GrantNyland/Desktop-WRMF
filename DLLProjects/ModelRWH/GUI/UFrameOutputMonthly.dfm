object frmOutputMonthly: TfrmOutputMonthly
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  OnResize = FrameResize
  ExplicitWidth = 638
  ExplicitHeight = 475
  object Splitter1: TSplitter
    Left = 0
    Top = 264
    Width = 638
    Height = 3
    Cursor = crVSplit
    Align = alTop
  end
  object chartData: TChart
    Left = 0
    Top = 267
    Width = 638
    Height = 208
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
    Legend.Shadow.Transparency = 50
    Legend.Symbol.Pen.Visible = False
    MarginBottom = 0
    MarginLeft = 0
    MarginRight = 0
    MarginTop = 0
    PrintProportional = False
    Title.Text.Strings = (
      '')
    AxisBehind = False
    BottomAxis.Title.Caption = 'Date'
    LeftAxis.Title.Caption = 'Days Supplied'
    RightAxis.Title.Angle = 90
    Shadow.Color = clBlack
    Shadow.HorizSize = 10
    Shadow.VertSize = 10
    View3D = False
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    OnDblClick = chartDataDblClick
    PrintMargins = (
      0
      0
      0
      0)
    ColorPaletteIndex = 0
    object Series1: TLineSeries
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.Brush.Gradient.Visible = True
      Marks.ShapeStyle = fosRoundRectangle
      Marks.Visible = False
      Title = 'DaysSupplied'
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.DateTime = True
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series2: TLineSeries
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.ShapeStyle = fosRoundRectangle
      Marks.Visible = False
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series3: TLineSeries
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.ShapeStyle = fosRoundRectangle
      Marks.Visible = False
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series4: TLineSeries
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.ShapeStyle = fosRoundRectangle
      Marks.Visible = False
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series5: TLineSeries
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.ShapeStyle = fosRoundRectangle
      Marks.Visible = False
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series6: TLineSeries
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.ShapeStyle = fosRoundRectangle
      Marks.Visible = False
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series7: TLineSeries
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.ShapeStyle = fosRoundRectangle
      Marks.Visible = False
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series8: TLineSeries
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.ShapeStyle = fosRoundRectangle
      Marks.Visible = False
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series9: TLineSeries
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.ShapeStyle = fosRoundRectangle
      Marks.Visible = False
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series10: TLineSeries
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.ShapeStyle = fosRoundRectangle
      Marks.Visible = False
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series11: TLineSeries
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.ShapeStyle = fosRoundRectangle
      Marks.Visible = False
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series12: TLineSeries
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.ShapeStyle = fosRoundRectangle
      Marks.Visible = False
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series13: TLineSeries
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.ShapeStyle = fosRoundRectangle
      Marks.Visible = False
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 638
    Height = 264
    Align = alTop
    BevelInner = bvLowered
    TabOrder = 1
    object strgrdData: TStringGrid
      Left = 2
      Top = 45
      Width = 634
      Height = 196
      Align = alClient
      Color = clInfoBk
      ColCount = 13
      DefaultRowHeight = 16
      RowCount = 2
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSizing, goColSizing]
      TabOrder = 0
    end
    object strgrdTotals: TStringGrid
      Left = 2
      Top = 241
      Width = 634
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
      Width = 634
      Height = 43
      Align = alTop
      BevelInner = bvLowered
      TabOrder = 2
      DesignSize = (
        447
        43)
      object lbTankVolume: TLabel
        Left = 76
        Top = 15
        Width = 45
        Height = 13
        Alignment = taRightJustify
        Caption = 'Tank Size'
      end
      object lblNote: TLabel
        Left = 640
        Top = 14
        Width = 224
        Height = 13
        Caption = 'Note:Double click chart for more chart options.'
      end
      object btnPrintGraph: TColourBitBtn
        Left = 344
        Top = 9
        Width = 87
        Height = 28
        Anchors = [akLeft]
        Caption = 'Print Graph'
        TabOrder = 0
        WordWrap = True
        OnClick = btnPrintGraphClick
        PressColor = clBlue
        Color = clInfoBk
      end
      object cmbboxTankSize: TComboBox
        Left = 124
        Top = 13
        Width = 165
        Height = 21
        TabOrder = 1
        OnChange = cmbboxTankSizeChange
      end
      object rgGraphType: TRadioGroup
        Left = 451
        Top = -3
        Width = 185
        Height = 45
        ItemIndex = 0
        Items.Strings = (
          'Graph - Date'
          'Graph - Month ')
        TabOrder = 2
        OnClick = rgGraphTypeClick
      end
    end
  end
end
