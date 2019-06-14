object MineOpencastPitEvaporationDlg: TMineOpencastPitEvaporationDlg
  Left = 459
  Top = 167
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Pit evaporation area growth'
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
    object LblEvaporationInterpolationType: TLabel
      Left = 10
      Top = 10
      Width = 90
      Height = 21
      AutoSize = False
      Caption = 'Interpolation type :'
      Layout = tlCenter
    end
    object BtnAddRow: TSpeedButton
      Left = 205
      Top = 40
      Width = 23
      Height = 22
      Hint = 'Insert row below selected row'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
        8888888888888888888888888800008888888888880EE08888888888880EE088
        88888888880EE08888888800000EE0000088880EEEEEEEEEE088880EEEEEEEEE
        E0888800000EE00000888888880EE08888888888880EE08888888888880EE088
        8888888888000088888888888888888888888888888888888888}
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddRowClick
    end
    object BtnDeleteRow: TSpeedButton
      Left = 205
      Top = 65
      Width = 23
      Height = 22
      Hint = 'Delete selected row'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        04000000000080000000C40E0000C40E00001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
        8888888888888888888888888888888888888888888888888888888888888888
        888888888888888888888800000000000088880EEEEEEEEEE088880EEEEEEEEE
        E088880000000000008888888888888888888888888888888888888888888888
        8888888888888888888888888888888888888888888888888888}
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnDeleteRowClick
    end
    object BtnRowUp: TSpeedButton
      Left = 205
      Top = 90
      Width = 23
      Height = 22
      Hint = 'Move selected row up'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
        88888888800000888888888880EEE0888888888880EEE0888888888880EEE088
        8888888880EEE0888888888880EEE0888888888880EEE0888888888880EEE088
        8888880000EEE00008888880EEEEEEE0888888880EEEEE088888888880EEE088
        88888888880E0888888888888880888888888888888888888888}
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnRowUpClick
    end
    object BtnRowDown: TSpeedButton
      Left = 205
      Top = 115
      Width = 23
      Height = 22
      Hint = 'Move selected row down'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
        888888888880888888888888880E08888888888880EEE088888888880EEEEE08
        88888880EEEEEEE08888880000EEE0000888888880EEE0888888888880EEE088
        8888888880EEE0888888888880EEE0888888888880EEE0888888888880EEE088
        8888888880EEE088888888888000008888888888888888888888}
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnRowDownClick
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
