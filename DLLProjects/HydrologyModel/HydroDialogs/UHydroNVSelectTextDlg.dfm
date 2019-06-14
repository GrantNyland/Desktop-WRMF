object HydroNVSelectTextDlg: THydroNVSelectTextDlg
  Left = 442
  Top = 189
  Caption = 'Add Text Label'
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
    object FTypeLbl: TLabel
      Left = 10
      Top = 110
      Width = 60
      Height = 21
      AutoSize = False
      Caption = 'Text Type :'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
    object FAssignParentChx: TCheckBox
      Left = 10
      Top = 10
      Width = 250
      Height = 17
      Caption = 'Assign text label to network element.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = OnControlClick
    end
    object FElementTypeCbx: TComboBox
      Left = 10
      Top = 35
      Width = 250
      Height = 21
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnSelect = FElementTypeCbxSelect
    end
    object FCancelBtn: TButton
      Left = 10
      Top = 160
      Width = 60
      Height = 25
      Caption = 'Cancel'
      TabOrder = 5
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
    object FNamesCbx: TComboBox
      Left = 10
      Top = 60
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
      OnSelect = FNamesCbxSelect
    end
    object FTextTypeCbx: TComboBox
      Left = 70
      Top = 110
      Width = 190
      Height = 21
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
    end
    object FSubTypesCbx: TComboBox
      Left = 10
      Top = 85
      Width = 150
      Height = 21
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
      Visible = False
      OnSelect = FSubTypesCbxSelect
    end
    object FSectionsCbx: TComboBox
      Left = 162
      Top = 85
      Width = 150
      Height = 21
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 7
      Visible = False
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
