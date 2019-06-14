inherited YRCRegressionFunctionSeriesNodeEditor: TYRCRegressionFunctionSeriesNodeEditor
  ClientHeight = 128
  PixelsPerInch = 96
  TextHeight = 13
  inherited Panel1: TPanel
    Top = 94
  end
  inherited Panel3: TPanel
    Height = 94
    object Label7: TLabel [2]
      Left = 6
      Top = 69
      Width = 28
      Height = 13
      Caption = 'Count'
    end
    object mePointCount: TEdit
      Left = 49
      Top = 64
      Width = 110
      Height = 21
      Anchors = [akTop, akRight]
      MaxLength = 20
      TabOrder = 2
      OnEnter = mePointXEnter
      OnExit = mePointXExit
      OnKeyPress = mePointXKeyPress
    end
  end
end
