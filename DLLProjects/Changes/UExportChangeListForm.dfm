object frmExportChangeListForm: TfrmExportChangeListForm
  Left = 202
  Top = 208
  Caption = 'Export Change List/s'
  ClientHeight = 433
  ClientWidth = 448
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 392
    Width = 448
    Height = 41
    Align = alBottom
    TabOrder = 0
    object btnOK: TButton
      Left = 278
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      Enabled = False
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 366
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object pnlClient: TPanel
    Left = 0
    Top = 0
    Width = 448
    Height = 392
    Align = alClient
    TabOrder = 1
    object cklboxChangeList: TCheckListBox
      Left = 1
      Top = 41
      Width = 446
      Height = 350
      OnClickCheck = cklboxChangeListClickCheck
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
    end
    object gbChangeList: TGroupBox
      Left = 1
      Top = 1
      Width = 446
      Height = 40
      Align = alTop
      TabOrder = 1
      object lblChangeGroup: TLabel
        Left = 16
        Top = 13
        Width = 72
        Height = 13
        Caption = 'Change Group:'
      end
      object cmbChangeGroup: TComboBox
        Left = 98
        Top = 11
        Width = 223
        Height = 21
        TabOrder = 0
        OnChange = cmbChangeGroupChange
      end
      object btnSelectAll: TBitBtn
        Left = 352
        Top = 10
        Width = 41
        Height = 25
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = btnSelectAllClick
      end
      object btnDeSelectAll: TBitBtn
        Left = 400
        Top = 10
        Width = 41
        Height = 25
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnClick = btnDeSelectAllClick
      end
    end
  end
end
