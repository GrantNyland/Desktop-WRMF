object ChannelPropertiesDlg: TChannelPropertiesDlg
  Left = 433
  Top = 160
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Channel Reach Module'
  ClientHeight = 406
  ClientWidth = 758
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
    Top = 371
    Width = 758
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object BtnApply: TButton
      Left = 10
      Top = 3
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
      Top = 3
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
    Width = 758
    Height = 371
    Align = alClient
    TabOrder = 0
    object LblNetworkSequence: TLabel
      Left = 10
      Top = 10
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Network sequence :'
      Layout = tlCenter
    end
    object LblModuleNumber: TLabel
      Left = 10
      Top = 35
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Module number :'
      Layout = tlCenter
    end
    object LblLatitude: TLabel
      Left = 10
      Top = 85
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Latitude :'
      Layout = tlCenter
    end
    object LblLongitude: TLabel
      Left = 10
      Top = 110
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Longitude :'
      Layout = tlCenter
    end
    object LblChannelName: TLabel
      Left = 10
      Top = 135
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Channel reach name :'
      Layout = tlCenter
    end
    object LblVersionNo: TLabel
      Left = 10
      Top = 160
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Version no :'
      Layout = tlCenter
    end
    object LblWetlandMAP: TLabel
      Left = 10
      Top = 185
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Wetland MAP :'
      Layout = tlCenter
    end
    object LblRainfallFileName: TLabel
      Left = 10
      Top = 210
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Rainfall file name :'
      Layout = tlCenter
    end
    object LblMonthlyBedLoss: TLabel
      Left = 10
      Top = 235
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Monthly bed loss :'
      Layout = tlCenter
    end
    object LblWetlandStorage: TLabel
      Left = 10
      Top = 260
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Wetland/aquifer storage :'
      Layout = tlCenter
    end
    object LblWetlandArea: TLabel
      Left = 10
      Top = 285
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Wetland/aquifer area :'
      Layout = tlCenter
    end
    object LblWetlandRechargeCoefficient: TLabel
      Left = 10
      Top = 310
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Wetland/aquifer recharge coefficient :'
      Layout = tlCenter
    end
    object LblPrincipalOutflowRoute: TLabel
      Left = 10
      Top = 335
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Principal outflow route :'
      Layout = tlCenter
    end
    object LblWetlandsInflowRoute: TLabel
      Left = 410
      Top = 10
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Wetlands inflow route :'
      Layout = tlCenter
    end
    object LblWetlandsOutflowRoute: TLabel
      Left = 410
      Top = 35
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Wetlands outflow route :'
      Layout = tlCenter
    end
    object LblDiversionRoute: TLabel
      Left = 410
      Top = 60
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Diversion route :'
      Layout = tlCenter
    end
    object LblBankfillCapacity: TLabel
      Left = 410
      Top = 85
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Bankfill capacity of river channel :'
      Layout = tlCenter
    end
    object LblDiversionEfficiency: TLabel
      Left = 410
      Top = 110
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Diversion efficiency :'
      Layout = tlCenter
    end
    object LblMaxMonthlyDiversionCapacity: TLabel
      Left = 410
      Top = 135
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Maximum monthly diversion capacity :'
      Layout = tlCenter
    end
    object LblWetlandType: TLabel
      Left = 410
      Top = 185
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Wetland type :'
      Layout = tlCenter
    end
    object LblBankfillArea: TLabel
      Left = 410
      Top = 210
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Area of wetland at bankfill level :'
      Layout = tlCenter
    end
    object LblBankfillVolume: TLabel
      Left = 410
      Top = 235
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Volume of wetland at bankfill level :'
      Layout = tlCenter
    end
    object LblPowerOfAreaCapCurve: TLabel
      Left = 410
      Top = 260
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Power of area-volume relationship :'
      Layout = tlCenter
    end
    object LblBankfillCapacityComp: TLabel
      Left = 410
      Top = 285
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Bankfill capacity of river channel :'
      Layout = tlCenter
    end
    object LblWetlandInflowProportion: TLabel
      Left = 340
      Top = 310
      Width = 250
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Proportion of flow in excess of bankfill into wetland :'
      Layout = tlCenter
    end
    object LblChannelInflowProportion: TLabel
      Left = 320
      Top = 335
      Width = 270
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Proportion of wetland volume (over bankfill) into channel :'
      Layout = tlCenter
    end
    object LblQDIV: TLabel
      Left = 410
      Top = 160
      Width = 180
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'QDIV :'
      Layout = tlCenter
    end
  end
  object FXMLDocumentIn: TXMLDocument
    Left = 439
    Top = 369
    DOMVendorDesc = 'MSXML'
  end
  object FXMLDocumentOut: TXMLDocument
    Left = 471
    Top = 369
    DOMVendorDesc = 'MSXML'
  end
end
