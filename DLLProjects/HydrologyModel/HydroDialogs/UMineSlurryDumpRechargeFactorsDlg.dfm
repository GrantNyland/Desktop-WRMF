object MineSlurryDumpRechargeFactorsDlg: TMineSlurryDumpRechargeFactorsDlg
  Left = 459
  Top = 167
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Recharge factors'
  ClientHeight = 396
  ClientWidth = 526
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
    Top = 361
    Width = 526
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
    Width = 526
    Height = 361
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    TabOrder = 0
    object GrdRechargeFactors: TWRMFGrid
      Left = 10
      Top = 10
      Width = 183
      Height = 290
      ColCount = 2
      DefaultColWidth = 80
      DefaultRowHeight = 21
      RowCount = 2
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing, goTabs]
      TabOrder = 0
      OnExit = GrdRechargeFactorsExit
      DblClickColAutoSize = True
      ColAutoSizeIgnoreHeading = False
      AutoSizeFixedCols = False
      OnDataCellExit = GrdRechargeFactorsDataCellExit
      CellsInfo.AssignOption = 1
      CellsInfo.NoOfHeadingRows = 1
      CellsInfo.NoOfHeadingCols = 1
      CellsInfo.DataListName = 'DataList'
      CellsInfo = <
        item
          PropertyName = 'Month'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 0
          Column = 0
          Active = True
        end
        item
          PropertyName = 'SlurryRechargeFactor'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 0
          Column = 1
          Active = True
        end
        item
          PropertyName = 'Month'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 1
          Column = 0
          Active = True
        end
        item
          PropertyName = 'SlurryRechargeFactor'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 1
          Column = 1
          Active = True
        end>
      WrapHeaderText = False
      OnParamChangeClick = GridParamChangeIndicatorClicked
      OnMetaDataClick = GridMetaDataIndicatorClicked
    end
  end
  object FXMLDocumentIn: TXMLDocument
    Left = 374
    Top = 364
    DOMVendorDesc = 'MSXML'
  end
  object FXMLDocumentOut: TXMLDocument
    Left = 406
    Top = 364
    DOMVendorDesc = 'MSXML'
  end
end
