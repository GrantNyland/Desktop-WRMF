object MineUndergroundPropertiesDlg: TMineUndergroundPropertiesDlg
  Left = 302
  Top = 177
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Underground section'
  ClientHeight = 340
  ClientWidth = 727
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
    Top = 305
    Width = 727
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
    Width = 727
    Height = 305
    Align = alClient
    TabOrder = 0
    object LblSectionName: TLabel
      Left = 10
      Top = 10
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Section name :'
      Layout = tlCenter
    end
    object LblBoardAndPillarArea: TLabel
      Left = 10
      Top = 85
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Board and pillar area :'
      Layout = tlCenter
    end
    object LblHighExtractionArea: TLabel
      Left = 10
      Top = 110
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'High extraction area :'
      Layout = tlCenter
    end
    object LblUndergroundOutflowRoute: TLabel
      Left = 10
      Top = 35
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Underground outflow route :'
      Layout = tlCenter
    end
    object LblUpstreamCatchmentArea: TLabel
      Left = 10
      Top = 60
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Upstream catchment area :'
      Layout = tlCenter
    end
    object LblSurfaceRunOffFactor: TLabel
      Left = 10
      Top = 135
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Surface run-off factor :'
      Layout = tlCenter
    end
    object EdtUndergroundSectionName: TWRMFEdit
      Left = 210
      Top = 10
      Width = 200
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 0
      OnExit = ControlExit
      Active = True
      PropertyName = 'SectionName'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
    end
    object EdtBoardAndPillarArea: TWRMFEdit
      Left = 210
      Top = 85
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 3
      OnExit = ControlExit
      Active = True
      PropertyName = 'BoardAndPillarArea'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
    end
    object EdtHighExtractionArea: TWRMFEdit
      Left = 210
      Top = 110
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 4
      OnExit = ControlExit
      Active = True
      PropertyName = 'HighExtractionArea'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
    end
    object EdtUpstreamCatchmentArea: TWRMFEdit
      Left = 210
      Top = 60
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 2
      OnExit = ControlExit
      Active = True
      PropertyName = 'UpstreamCatchmentArea'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
    end
    object EdtSurfaceRunOffFactor: TWRMFEdit
      Left = 210
      Top = 135
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 5
      OnExit = ControlExit
      Active = True
      PropertyName = 'SurfaceRunOffFactor'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
    end
    object CbxUndergroundOutflowRouteNo: TWRMFComboBox
      Left = 210
      Top = 35
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 1
      OnExit = ControlExit
      Active = True
      PropertyName = 'UndergroundOutflowRouteNo'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
      ItemIndex = -1
    end
  end
  object FXMLDocumentOut: TXMLDocument
    Left = 448
    Top = 495
    DOMVendorDesc = 'MSXML'
  end
  object FXMLDocumentIn: TXMLDocument
    Left = 416
    Top = 495
    DOMVendorDesc = 'MSXML'
  end
end
