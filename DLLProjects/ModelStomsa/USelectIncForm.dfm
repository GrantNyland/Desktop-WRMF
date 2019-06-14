object fmSelectInc: TfmSelectInc
  Left = 0
  Top = 0
  Width = 670
  Height = 519
  TabOrder = 0
  object fclIncFiles: TFileCollect
    Left = 0
    Top = 0
    Width = 670
    Height = 249
    Align = alTop
    BevelOuter = bvNone
    Directory = 'D:\_Software\Delphi\10\projects\Stomsa\20060706'
    OnFileAdd = fclIncFilesFileAdd
    OnFileRemove = fclIncFilesFileRemove
    FOnFileSelected = fclIncFilesFOnFileSelected
  end
  object stbIncStatus: TStatusBar
    Left = 0
    Top = 500
    Width = 670
    Height = 19
    Panels = <
      item
        Width = 70
      end
      item
        Width = 50
      end>
  end
  object chtIncFile: TChart
    Left = 0
    Top = 249
    Width = 601
    Height = 251
    Title.Text.Strings = (
      'TChart')
    View3D = False
    Align = alClient
    TabOrder = 2
    DefaultCanvas = 'TGDIPlusCanvas'
    ColorPaletteIndex = 0
    object Series1: TLineSeries
      Legend.Visible = False
      SeriesColor = 16744448
      ShowInLegend = False
      Title = 'IncMonthlySeries'
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.DateTime = True
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series3: TLineSeries
      Legend.Visible = False
      SeriesColor = 16744448
      ShowInLegend = False
      Title = 'IncAnnualSeries'
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.DateTime = True
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series2: TLineSeries
      Legend.Visible = False
      SeriesColor = 16744703
      ShowInLegend = False
      Title = 'MARSeries'
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.DateTime = True
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
  end
  object pnlChartOptions: TPanel
    Left = 601
    Top = 249
    Width = 69
    Height = 251
    Align = alRight
    TabOrder = 3
    object rgChartType: TRadioGroup
      Left = -8
      Top = 72
      Width = 185
      Height = 105
      Items.Strings = (
        'Monthly'
        'Annual')
      TabOrder = 0
      OnClick = rgChartTypeClick
    end
    object chkbxMAR: TCheckBox
      Left = 0
      Top = 176
      Width = 97
      Height = 17
      Caption = 'MAR'
      TabOrder = 1
      OnClick = chkbxMARClick
    end
  end
end
