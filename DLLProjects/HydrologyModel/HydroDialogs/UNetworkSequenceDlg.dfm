object NetworkSequenceDlg: TNetworkSequenceDlg
  Left = 466
  Top = 166
  BorderIcons = [biSystemMenu]
  Caption = 'Change Network Sequence'
  ClientHeight = 455
  ClientWidth = 203
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
    Top = 420
    Width = 203
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 427
    DesignSize = (
      203
      35)
    object BtnReset: TButton
      Left = 90
      Top = 3
      Width = 75
      Height = 25
      Anchors = [akLeft]
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
    object BtnApply: TButton
      Left = 10
      Top = 3
      Width = 75
      Height = 25
      Anchors = [akLeft]
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
  end
  object ScrClient: TScrollBox
    Left = 0
    Top = 0
    Width = 203
    Height = 420
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitHeight = 427
    object BtnRowUp: TSpeedButton
      Left = 165
      Top = 100
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
      Left = 165
      Top = 125
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
    object GrdModules: TStringGrid
      Left = 10
      Top = 10
      Width = 140
      Height = 400
      ColCount = 2
      DefaultColWidth = 60
      DefaultRowHeight = 21
      RowCount = 20
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
      TabOrder = 0
    end
  end
  object FXMLDocumentIn: TXMLDocument
    Left = 494
    Top = 349
    DOMVendorDesc = 'MSXML'
  end
  object FXMLDocumentOut: TXMLDocument
    Left = 526
    Top = 349
    DOMVendorDesc = 'MSXML'
  end
end
