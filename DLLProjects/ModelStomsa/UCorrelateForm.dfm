object fmCorrelate: TfmCorrelate
  Left = 0
  Top = 0
  Width = 696
  Height = 480
  TabOrder = 0
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 696
    Height = 480
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel2'
    TabOrder = 0
    object pnlCorrelateMenu: TPanel
      Left = 0
      Top = 0
      Width = 696
      Height = 34
      Align = alTop
      BevelOuter = bvLowered
      TabOrder = 0
      object Label3: TLabel
        Left = 64
        Top = 8
        Width = 46
        Height = 13
        Caption = 'No Action'
      end
      object Label4: TLabel
        Left = 144
        Top = 8
        Width = 106
        Height = 13
        Caption = 'Correlate In Next Run'
      end
      object Label5: TLabel
        Left = 280
        Top = 8
        Width = 51
        Height = 13
        Caption = 'Correlated'
      end
      object btnCorr: THotBtn
        Left = 1
        Top = 1
        Width = 30
        Height = 32
        Hint = 'Correlate Selected Gauges'
        Align = alLeft
        BevelOuter = bvNone
        Enabled = True
        ShowHint = True
        GlyphHot.Data = {
          96010000424D9601000000000000760000002800000018000000180000000100
          04000000000020010000CE0E0000C40E00001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFF999FFFFFFFFFFFFFFFF999FFF9FFFFFFFFF
          FFFFFFFFF9FFFF9FFFFF99999999FFFFF9FFFF9FFFFFFFFFFFFFFFFFF9FFFF9F
          FFFFFFFFFFFFFFFFF9FFFF9FFFFF99999999FFFFF9FFF99FFFFFFFFFFFFFFFFF
          99FFFF9FFFFFFFFFFFFFFFFFF9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        GlyphPlain.Data = {
          96010000424D9601000000000000760000002800000018000000180000000100
          04000000000020010000CE0E0000C40E00001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFF999FFFFFFFFFFFFFFFF999FFF9FFFFFFFFF
          FFFFFFFFF9FFFF9FFFFF99999999FFFFF9FFFF9FFFFFFFFFFFFFFFFFF9FFFF9F
          FFFFFFFFFFFFFFFFF9FFFF9FFFFF99999999FFFFF9FFF99FFFFFFFFFFFFFFFFF
          99FFFF9FFFFFFFFFFFFFFFFFF9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        GlyphDisabled.Data = {
          96010000424D9601000000000000760000002800000018000000180000000100
          04000000000020010000CE0E0000C40E00001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFF888FFFFFFFFFFFFFFFF888FFF8FFFFFFFFF
          FFFFFFFFF8FFFF8FFFFF88888888FFFFF8FFFF8FFFFFFFFFFFFFFFFFF8FFFF8F
          FFFFFFFFFFFFFFFFF8FFFF8FFFFF88888888FFFFF8FFF88FFFFFFFFFFFFFFFFF
          88FFFF8FFFFFFFFFFFFFFFFFF8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        Text = ''
        ImageAlign = alClient
        OnClick = btnCorrClick
      end
      object Panel5: TPanel
        Left = 40
        Top = 8
        Width = 17
        Height = 17
        BevelOuter = bvLowered
        Color = clWhite
        TabOrder = 1
      end
      object Panel6: TPanel
        Left = 120
        Top = 8
        Width = 17
        Height = 17
        BevelOuter = bvLowered
        Color = clBlack
        TabOrder = 2
      end
      object Panel7: TPanel
        Left = 256
        Top = 8
        Width = 17
        Height = 17
        BevelOuter = bvLowered
        Color = clNavy
        TabOrder = 3
      end
    end
    object grdCompare: TMarkStringGrid
      Left = 0
      Top = 34
      Width = 696
      Height = 427
      Align = alClient
      DefaultColWidth = 80
      DefaultDrawing = False
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goColSizing]
      TabOrder = 1
      OnMouseDown = grdCompareMouseDown
      RowHeights = (
        24
        24
        24
        24
        24)
    end
    object stbCorrelate: TStatusBar
      Left = 0
      Top = 461
      Width = 696
      Height = 19
      Panels = <
        item
          Width = 50
        end>
    end
  end
end