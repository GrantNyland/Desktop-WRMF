object YRCDeterministicFunctionSeriesNodeEditor: TYRCDeterministicFunctionSeriesNodeEditor
  Left = 629
  Top = 269
  ActiveControl = mePointX
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 99
  ClientWidth = 165
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
  object Panel1: TPanel
    Left = 0
    Top = 65
    Width = 165
    Height = 34
    Align = alBottom
    BevelInner = bvLowered
    TabOrder = 0
    DesignSize = (
      165
      34)
    object btnCancel: TButton
      Left = 7
      Top = 7
      Width = 61
      Height = 22
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      TabStop = False
    end
    object btnSave: TButton
      Left = 99
      Top = 7
      Width = 61
      Height = 22
      Anchors = [akTop, akRight, akBottom]
      Caption = 'Save'
      TabOrder = 0
      OnClick = btnSaveClick
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 165
    Height = 65
    Align = alClient
    Anchors = [akTop, akRight]
    BevelInner = bvLowered
    TabOrder = 1
    DesignSize = (
      165
      65)
    object Label5: TLabel
      Left = 6
      Top = 12
      Width = 37
      Height = 13
      Caption = 'X Value'
    end
    object Label6: TLabel
      Left = 6
      Top = 43
      Width = 37
      Height = 13
      Caption = 'Y Value'
    end
    object mePointX: TEdit
      Left = 49
      Top = 8
      Width = 110
      Height = 21
      Anchors = [akTop, akRight]
      BevelInner = bvLowered
      BevelOuter = bvRaised
      MaxLength = 20
      TabOrder = 0
      OnEnter = mePointXEnter
      OnExit = mePointXExit
      OnKeyPress = mePointXKeyPress
    end
    object mePointY: TEdit
      Left = 49
      Top = 36
      Width = 110
      Height = 21
      Anchors = [akTop, akRight]
      MaxLength = 20
      TabOrder = 1
      OnEnter = mePointXEnter
      OnExit = mePointXExit
      OnKeyPress = mePointXKeyPress
    end
  end
end
