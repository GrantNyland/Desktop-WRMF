object HydroNVOutputSelectorDlg: THydroNVOutputSelectorDlg
  Left = 462
  Top = 175
  BorderIcons = [biSystemMenu]
  Caption = 'Output Selector (Hydro)'
  ClientHeight = 62
  ClientWidth = 216
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object FScrollBox: TScrollBox
    Left = 0
    Top = 0
    Width = 216
    Height = 62
    Align = alClient
    BevelInner = bvNone
    TabOrder = 0
    ExplicitHeight = 69
    object PnlInterval: TPanel
      Left = 5
      Top = 5
      Width = 202
      Height = 49
      BevelOuter = bvLowered
      TabOrder = 0
      object LblInterval: TLabel
        Left = 1
        Top = 1
        Width = 200
        Height = 25
        Align = alTop
        Alignment = taCenter
        AutoSize = False
        Caption = 'Interval'
        Color = clNavy
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object PnlIntervalBottom: TPanel
        Left = 1
        Top = 26
        Width = 200
        Height = 22
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object BtnFirstInterval: TSpeedButton
          Left = 0
          Top = 1
          Width = 25
          Height = 21
          Hint = 'First interval'
          Glyph.Data = {
            36010000424D3601000000000000760000002800000012000000100000000100
            040000000000C000000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
            8888880000008888888888888888880000008888888888888888880000008884
            4888888488888800000088844888884488888800000088844888842488888800
            0000888448884224888888000000888448842224888888000000888448422224
            8888880000008884488422248888880000008884488842248888880000008884
            4888842488888800000088844888884488888800000088844888888488888800
            0000888888888888888888000000888888888888888888000000}
          ParentShowHint = False
          ShowHint = True
          OnClick = BtnFirstIntervalClick
        end
        object BtnMinus12Intervals: TSpeedButton
          Left = 25
          Top = 1
          Width = 25
          Height = 21
          Hint = '- 12 Intervals'
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            0400000000000001000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
            8888888888888888888888888888888888888888888888888888888888888888
            8888888888888888888888484888888488888878788888878888884848888844
            8888887878888877888888484888842488888878788887878888884848884224
            8888887878887887888888484884222488888878788788878888884848422224
            8888887878788887888888484884222488888878788788878888884848884224
            8888887878887887888888484888842488888878788887878888884848888844
            8888887878888877888888484888888488888878788888878888888888888888
            8888888888888888888888888888888888888888888888888888}
          NumGlyphs = 2
          ParentShowHint = False
          ShowHint = True
          OnClick = BtnMinus12IntervalsClick
        end
        object BtnPreviousInterval: TSpeedButton
          Left = 50
          Top = 1
          Width = 25
          Height = 21
          Hint = 'Previous Interval'
          Glyph.Data = {
            36010000424D3601000000000000760000002800000012000000100000000100
            040000000000C000000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
            8888880000008888888888888888880000008888888888888888880000008888
            8888884888888800000088888888844888888800000088888888424888888800
            0000888888842248888888000000888888422248888888000000888884222248
            8888880000008888884222488888880000008888888422488888880000008888
            8888424888888800000088888888844888888800000088888888884888888800
            0000888888888888888888000000888888888888888888000000}
          ParentShowHint = False
          ShowHint = True
          OnClick = BtnPreviousIntervalClick
        end
        object BtnNextInterval: TSpeedButton
          Left = 125
          Top = 1
          Width = 25
          Height = 21
          Hint = 'Next Interval'
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            0400000000000001000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
            8888888888888888888888888888888888888888888888888888888888888888
            8888888888888888888888888488888888888888878888888888888884488888
            8888888887788888888888888424888888888888878788888888888884224888
            8888888887887888888888888422248888888888878887888888888884222248
            8888888887888878888888888422248888888888878887888888888884224888
            8888888887887888888888888424888888888888878788888888888884488888
            8888888887788888888888888488888888888888878888888888888888888888
            8888888888888888888888888888888888888888888888888888}
          NumGlyphs = 2
          ParentShowHint = False
          ShowHint = True
          OnClick = BtnNextIntervalClick
        end
        object BtnPlus12Intervals: TSpeedButton
          Left = 150
          Top = 1
          Width = 25
          Height = 21
          Hint = '+ 12 Intervals'
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            0400000000000001000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
            8888888888888888888888888888888888888888888888888888888888888888
            8888888888888888888888884888888484888888788888878788888844888884
            8488888877888887878888884248888484888888787888878788888842248884
            8488888878878887878888884222488484888888788878878788888842222484
            8488888878888787878888884222488484888888788878878788888842248884
            8488888878878887878888884248888484888888787888878788888844888884
            8488888877888887878888884888888484888888788888878788888888888888
            8888888888888888888888888888888888888888888888888888}
          NumGlyphs = 2
          ParentShowHint = False
          ShowHint = True
          OnClick = BtnPlus12IntervalsClick
        end
        object BtnLastInterval: TSpeedButton
          Left = 175
          Top = 1
          Width = 25
          Height = 21
          Hint = 'Last Interval'
          Glyph.Data = {
            36010000424D3601000000000000760000002800000012000000100000000100
            040000000000C000000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
            8888880000008888888888888888880000008888888888888888880000008888
            4888888448888800000088884488888448888800000088884248888448888800
            0000888842248884488888000000888842224884488888000000888842222484
            4888880000008888422248844888880000008888422488844888880000008888
            4248888448888800000088884488888448888800000088884888888448888800
            0000888888888888888888000000888888888888888888000000}
          ParentShowHint = False
          ShowHint = True
          OnClick = BtnLastIntervalClick
        end
        object EdtInterval: TEdit
          Left = 75
          Top = 1
          Width = 50
          Height = 21
          ReadOnly = True
          TabOrder = 0
        end
      end
    end
  end
end
