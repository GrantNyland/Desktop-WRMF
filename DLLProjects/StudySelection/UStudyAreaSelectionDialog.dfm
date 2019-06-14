object StudyAreaSelectionDialog: TStudyAreaSelectionDialog
  Left = 268
  Top = 151
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  Caption = 'Study Selection'
  ClientHeight = 426
  ClientWidth = 688
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = mnuMain
  OldCreateOrder = False
  OnCanResize = FormCanResize
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object FSplitter: TSplitter
    Left = 0
    Top = 0
    Width = 4
    Height = 426
    AutoSnap = False
    MinSize = 40
  end
  object FBackPanel: TPanel
    Left = 4
    Top = 0
    Width = 684
    Height = 426
    Align = alClient
    TabOrder = 0
  end
  object mnuMain: TMainMenu
    Left = 176
    Top = 56
    object mnuFile: TMenuItem
      Caption = '&File'
      object mnuImportStudy: TMenuItem
        Caption = '&Import Study'
        Enabled = False
      end
      object mnuImportAll: TMenuItem
        Caption = 'Import All'
        Enabled = False
      end
      object mnuExporStudy: TMenuItem
        Caption = '&Export Study'
        Enabled = False
      end
      object mnuExportAll: TMenuItem
        Caption = 'Export All'
        Enabled = False
      end
      object mnuBuildDatabase: TMenuItem
        Caption = '&Build Database'
        Enabled = False
      end
      object mnuExportSystemData: TMenuItem
        Caption = '&Export System Data'
        Enabled = False
      end
      object mnuLinkHydrologyStudies: TMenuItem
        Caption = 'Access Hydrology Study'
        Enabled = False
      end
      object mnuUnLinkHydrologyStudies: TMenuItem
        Caption = 'Discard Hydrology Study'
      end
      object mnuSeparator1: TMenuItem
        Caption = '-'
      end
      object mnuUnlockScenario: TMenuItem
        Caption = '&Unlock Scenario'
        Enabled = False
      end
      object mnuSeparator2: TMenuItem
        Caption = '-'
      end
      object mnuExit: TMenuItem
        Caption = '&Exit'
        OnClick = mnuExitClick
      end
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
      object mnuCopyGISToClibboard: TMenuItem
        Caption = 'Copy GIS To Clipboard'
      end
      object mnuExportGISToFile: TMenuItem
        Caption = 'Export GIS To File'
      end
      object mnuPrintGIS: TMenuItem
        Caption = 'Print GIS'
      end
    end
    object mnuEdit: TMenuItem
      Caption = '&Actions'
      object mnuNewStudy: TMenuItem
        Caption = 'New Study'
        Enabled = False
      end
      object mnuEditStudy: TMenuItem
        Caption = 'Edit Study'
        Enabled = False
      end
      object mnuCopyStudy: TMenuItem
        Caption = '&Copy Study'
        Enabled = False
      end
      object mnuDeleteStudy: TMenuItem
        Caption = '&Delete Study'
        Enabled = False
      end
      object mnuSelectStudy: TMenuItem
        Caption = 'Select Study'
        Enabled = False
      end
    end
    object mnuReports: TMenuItem
      Caption = 'Reports'
      object mnuViewStudyReports: TMenuItem
        Caption = 'View Study Reports'
        Enabled = False
      end
    end
  end
end
