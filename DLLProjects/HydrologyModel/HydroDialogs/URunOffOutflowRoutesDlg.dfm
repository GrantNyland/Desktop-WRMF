object RunOffOutflowRoutesDlg: TRunOffOutflowRoutesDlg
  Left = 305
  Top = 159
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Outflow Routes'
  ClientHeight = 499
  ClientWidth = 756
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
    Top = 464
    Width = 756
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
    Width = 756
    Height = 464
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    TabOrder = 0
    object GrdOutflowRoutes: TWRMFGrid
      Left = 20
      Top = 20
      Width = 202
      Height = 290
      ColCount = 2
      DefaultColWidth = 80
      DefaultRowHeight = 21
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing, goTabs]
      TabOrder = 0
      OnExit = GrdOutflowRoutesExit
      DblClickColAutoSize = True
      ColAutoSizeIgnoreHeading = False
      AutoSizeFixedCols = False
      OnDataCellExit = GrdOutflowRoutesDataCellExit
      CellsInfo.AssignOption = 1
      CellsInfo.NoOfHeadingRows = 1
      CellsInfo.NoOfHeadingCols = 0
      CellsInfo.DataListName = 'DataList'
      CellsInfo = <
        item
          PropertyName = 'RouteNo'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 0
          Column = 0
          Active = True
        end
        item
          PropertyName = 'OutflowPercentage'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 0
          Column = 1
          Active = True
        end
        item
          PropertyName = 'RouteNo'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 1
          Column = 0
          Active = True
        end
        item
          PropertyName = 'OutflowPercentage'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 1
          Column = 1
          Active = True
        end
        item
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 2
          Column = 0
          Active = True
        end
        item
          PropertyName = 'OutflowPercentage'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 2
          Column = 1
          Active = True
        end
        item
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 3
          Column = 0
          Active = True
        end
        item
          PropertyName = 'OutflowPercentage'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 3
          Column = 1
          Active = True
        end
        item
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 4
          Column = 0
          Active = True
        end
        item
          PropertyName = 'OutflowPercentage'
          HasParamChange = False
          HasMetaData = False
          IsValid = True
          Row = 4
          Column = 1
          Active = True
        end>
      WrapHeaderText = False
      OnParamChangeClick = GridParamChangeIndicatorClicked
      OnMetaDataClick = GridMetaDataIndicatorClicked
    end
  end
  object FXMLDocumentIn: TXMLDocument
    Left = 569
    Top = 464
    DOMVendorDesc = 'MSXML'
  end
  object FXMLDocumentOut: TXMLDocument
    Left = 601
    Top = 464
    DOMVendorDesc = 'MSXML'
  end
end
