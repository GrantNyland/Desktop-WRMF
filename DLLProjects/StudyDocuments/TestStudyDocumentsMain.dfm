object TestStudyDocumentsMainForm: TTestStudyDocumentsMainForm
  Left = 205
  Top = 109
  Width = 508
  Height = 169
  Caption = 'TestStudyDocumentsMainForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    500
    142)
  PixelsPerInch = 96
  TextHeight = 13
  object FileNameEdit: TEdit
    Left = 12
    Top = 8
    Width = 476
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'C:\Projects\VaalReportsPDF\ASSESSMENT OF DATA REQUIREMENTS.PDF'
  end
  object BookMarkEdit: TEdit
    Left = 12
    Top = 40
    Width = 476
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object PageNumberEdit: TEdit
    Left = 12
    Top = 72
    Width = 476
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    Text = '3'
  end
  object LaunchButton: TButton
    Left = 12
    Top = 104
    Width = 476
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Launch'
    TabOrder = 3
    OnClick = LaunchButtonClick
  end
end
