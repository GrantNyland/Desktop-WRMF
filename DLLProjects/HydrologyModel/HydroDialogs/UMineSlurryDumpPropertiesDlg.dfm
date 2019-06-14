object MineSlurryDumpPropertiesDlg: TMineSlurryDumpPropertiesDlg
  Left = 380
  Top = 172
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Slurry dump'
  ClientHeight = 340
  ClientWidth = 641
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
    Width = 641
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
    Width = 641
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
    object LblSlurrySeepProportion: TLabel
      Left = 10
      Top = 85
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Seep proportion to dam and river :'
      Layout = tlCenter
    end
    object LblSlurryPCDFullSupplyVolume: TLabel
      Left = 10
      Top = 110
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'PCD full supply volume :'
      Layout = tlCenter
    end
    object LblSlurryDumpArea: TLabel
      Left = 10
      Top = 35
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Slurry dump area :'
      Layout = tlCenter
    end
    object LblSlurryDumpRunOffFactor: TLabel
      Left = 10
      Top = 60
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Slurry dump run-off factor :'
      Layout = tlCenter
    end
    object LblSlurryPCDFullSupplyArea: TLabel
      Left = 10
      Top = 135
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'PCD full supply area :'
      Layout = tlCenter
    end
    object LblSlurryPCDInitialVolume: TLabel
      Left = 10
      Top = 160
      Width = 190
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'PCD initial volume :'
      Layout = tlCenter
    end
    object EdtSlurryDumpSectionName: TWRMFEdit
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
    object EdtSlurrySeepProportion: TWRMFEdit
      Left = 210
      Top = 85
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 3
      OnExit = ControlExit
      Active = True
      PropertyName = 'SlurrySeepProportion'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
    end
    object EdtSlurryPCDFullSupplyVolume: TWRMFEdit
      Left = 210
      Top = 110
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 4
      OnExit = ControlExit
      Active = True
      PropertyName = 'SlurryPCDFullSupplyVolume'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
    end
    object EdtSlurryDumpRunOffFactor: TWRMFEdit
      Left = 210
      Top = 60
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 2
      OnExit = ControlExit
      Active = True
      PropertyName = 'SlurryDumpRunOffFactor'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
    end
    object EdtSlurryPCDFullSupplyArea: TWRMFEdit
      Left = 210
      Top = 135
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 5
      OnExit = ControlExit
      Active = True
      PropertyName = 'SlurryPCDFullSupplyArea'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
    end
    object EdtSlurryDumpArea: TWRMFEdit
      Left = 210
      Top = 35
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 1
      OnExit = ControlExit
      Active = True
      PropertyName = 'SlurryDumpArea'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
    end
    object EdtSlurryPCDInitialVolume: TWRMFEdit
      Left = 210
      Top = 160
      Width = 100
      Height = 21
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 6
      OnExit = ControlExit
      Active = True
      PropertyName = 'SlurryPCDInitialVolume'
      HasParamChange = False
      HasMetaData = False
      IsValid = True
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
