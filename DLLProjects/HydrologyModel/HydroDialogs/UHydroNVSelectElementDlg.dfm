object HydroNVSelectElementDlg: THydroNVSelectElementDlg
  Left = 442
  Top = 189
  Caption = 'Please select a Reservoir'
  ClientHeight = 193
  ClientWidth = 350
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object FScrollBox: TScrollBox
    Left = 0
    Top = 0
    Width = 350
    Height = 193
    Align = alClient
    BevelInner = bvNone
    TabOrder = 0
    ExplicitHeight = 200
    object FDuplicateLbl: TLabel
      Left = 10
      Top = 100
      Width = 250
      Height = 40
      AutoSize = False
      Caption = 'Empty'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Visible = False
      WordWrap = True
    end
    object FExistGrp: TGroupBox
      Left = 10
      Top = 5
      Width = 250
      Height = 30
      TabOrder = 0
      object FExistRdb: TRadioButton
        Left = 10
        Top = 10
        Width = 100
        Height = 17
        Caption = 'Use existing'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = OnControlClick
      end
      object FNewRdb: TRadioButton
        Left = 120
        Top = 10
        Width = 113
        Height = 17
        Caption = 'Create new'
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = OnControlClick
      end
    end
    object FShowDuplicatesChx: TCheckBox
      Left = 10
      Top = 40
      Width = 280
      Height = 17
      Caption = 'Include elements already on diagram'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = OnControlClick
    end
    object FNamesCbx: TComboBox
      Left = 10
      Top = 70
      Width = 250
      Height = 21
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
    object FCancelBtn: TButton
      Left = 10
      Top = 160
      Width = 60
      Height = 25
      Caption = 'Cancel'
      TabOrder = 3
      OnClick = OnCancelBtnClick
    end
    object FOKBtn: TButton
      Left = 75
      Top = 160
      Width = 60
      Height = 25
      Caption = 'OK'
      TabOrder = 4
      OnClick = OnOKBtnClick
    end
    object FYesBtn: TButton
      Left = 140
      Top = 160
      Width = 60
      Height = 25
      Caption = 'Yes'
      TabOrder = 5
      Visible = False
      OnClick = OnYesBtnClick
    end
    object FNoBtn: TButton
      Left = 205
      Top = 160
      Width = 60
      Height = 25
      Caption = 'No'
      TabOrder = 6
      Visible = False
      OnClick = OnCancelBtnClick
    end
  end
  object FXMLDocumentOut: TXMLDocument
    Left = 312
    Top = 161
    DOMVendorDesc = 'MSXML'
  end
  object FXMLDocumentIn: TXMLDocument
    Left = 312
    Top = 129
    DOMVendorDesc = 'MSXML'
  end
end
