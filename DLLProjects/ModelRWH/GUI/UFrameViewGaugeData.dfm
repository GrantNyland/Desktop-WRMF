object frmViewGaugeData: TfrmViewGaugeData
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 451
    Height = 304
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Graph'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object pnlClient: TPanel
        Left = 0
        Top = 0
        Width = 443
        Height = 276
        Align = alClient
        TabOrder = 0
        object pnlChart: TPanel
          Left = 1
          Top = 1
          Width = 441
          Height = 274
          Align = alClient
          TabOrder = 0
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Grid'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object pnlGrid: TPanel
        Left = 0
        Top = 0
        Width = 443
        Height = 276
        Align = alClient
        TabOrder = 0
        object stgDailyData: TStringGrid
          Left = 1
          Top = 1
          Width = 441
          Height = 274
          Align = alClient
          FixedCols = 0
          TabOrder = 0
          ExplicitLeft = 40
          ExplicitTop = 8
          ExplicitWidth = 320
          ExplicitHeight = 120
        end
      end
    end
  end
end
