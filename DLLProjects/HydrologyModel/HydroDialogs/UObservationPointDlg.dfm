object ObservationPointDlg: TObservationPointDlg
  Left = 462
  Top = 236
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Observation Point'
  ClientHeight = 234
  ClientWidth = 384
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
  object PnlBottom: TPanel
    Left = 0
    Top = 199
    Width = 384
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object BtnApply: TButton
      Left = 10
      Top = 5
      Width = 75
      Height = 25
      Caption = 'Apply'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = BtnApplyClick
    end
    object BtnReset: TButton
      Left = 90
      Top = 5
      Width = 75
      Height = 25
      Caption = 'Reset'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = BtnResetClick
    end
  end
  object ScrClient: TScrollBox
    Left = 0
    Top = 0
    Width = 384
    Height = 199
    Align = alClient
    BevelEdges = []
    BevelInner = bvNone
    BevelOuter = bvNone
    TabOrder = 0
    object LblRouteNo: TLabel
      Left = 10
      Top = 10
      Width = 120
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Route number :'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
    object LblName: TLabel
      Left = 10
      Top = 35
      Width = 120
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Name :'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
    object LblFlowdataFileName: TLabel
      Left = 10
      Top = 60
      Width = 120
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Flow data file name :'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
    object CbxRouteNo: TWRMFComboBox
      Left = 150
      Top = 11
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 0
      OnExit = ControlExit
      Active = True
      PropertyName = 'RouteNo'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
      ItemIndex = -1
    end
    object EdtFlowDataFileName: TWRMFEdit
      Left = 150
      Top = 60
      Width = 200
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 2
      TabStop = True
      OnExit = ControlExit
      Active = True
      PropertyName = 'FlowDataFileName'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
    object EdtName: TWRMFEdit
      Left = 150
      Top = 35
      Width = 200
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 1
      TabStop = True
      OnExit = ControlExit
      Active = True
      PropertyName = 'Name'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      OnParamChangeClick = ParamChangeIndicatorClicked
      OnMetaDataClick = MetaDataIndicatorClicked
    end
  end
  object FXMLDocumentOut: TXMLDocument
    Left = 283
    Top = 115
    DOMVendorDesc = 'MSXML'
  end
  object FXMLDocumentIn: TXMLDocument
    Left = 251
    Top = 115
    DOMVendorDesc = 'MSXML'
  end
end
