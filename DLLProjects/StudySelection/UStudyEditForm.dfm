object frmStudyEditForm: TfrmStudyEditForm
  Left = 106
  Top = 190
  BorderIcons = [biSystemMenu, biHelp]
  Caption = 'Study Edit'
  ClientHeight = 517
  ClientWidth = 748
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCanResize = FormCanResize
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pcModel: TPageControl
    Left = 0
    Top = 38
    Width = 748
    Height = 479
    ActivePage = tbsDDTS
    Align = alClient
    TabOrder = 0
    object tbsAll: TTabSheet
      Caption = 'tbsAll'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object pnlModel: TPanel
        Left = 0
        Top = 146
        Width = 740
        Height = 35
        Align = alTop
        BevelInner = bvLowered
        TabOrder = 0
        object lblModelCaption: TLabel
          Left = 46
          Top = 10
          Width = 32
          Height = 13
          Alignment = taRightJustify
          Caption = 'Model:'
        end
        object cmbModel: TComboBox
          Left = 81
          Top = 8
          Width = 250
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          OnChange = cmbModelChange
          OnExit = cmbModelExit
        end
      end
      object pnlScenario: TPanel
        Left = 0
        Top = 281
        Width = 740
        Height = 170
        Align = alClient
        BevelInner = bvLowered
        TabOrder = 3
        object lblScenarioCaption: TLabel
          Left = 33
          Top = 10
          Width = 45
          Height = 13
          Alignment = taRightJustify
          Caption = 'Scenario:'
        end
        object lblScenarioLabelCaption: TLabel
          Left = 2
          Top = 45
          Width = 76
          Height = 13
          Alignment = taRightJustify
          Caption = 'Scenario Name:'
        end
        object lblScenarioDescr: TLabel
          Left = 388
          Top = 33
          Width = 56
          Height = 13
          Alignment = taRightJustify
          Caption = 'Description:'
        end
        object lblVersion: TLabel
          Left = 41
          Top = 76
          Width = 38
          Height = 13
          Alignment = taRightJustify
          Caption = 'Version:'
        end
        object lblVersionDescr: TLabel
          Left = 344
          Top = 80
          Width = 3
          Height = 13
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lblMaxChars: TLabel
          Left = 336
          Top = 12
          Width = 68
          Height = 13
          Caption = '(*Max 3 chars)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          Visible = False
        end
        object edtScenario: TEdit
          Left = 81
          Top = 8
          Width = 250
          Height = 21
          TabOrder = 0
          OnChange = edtScenarioChange
          OnKeyPress = FormKeyPress
        end
        object edtScenarioLabel: TEdit
          Left = 81
          Top = 43
          Width = 250
          Height = 21
          TabOrder = 1
          OnChange = edtScenarioChange
        end
        object memoScenarioDescr: TMemo
          Left = 448
          Top = 8
          Width = 271
          Height = 63
          TabOrder = 2
          OnChange = edtScenarioChange
        end
        object chkbFilesLoaded: TCheckBox
          Left = 81
          Top = 104
          Width = 81
          Height = 14
          BiDiMode = bdLeftToRight
          Caption = 'Files Loaded:'
          Enabled = False
          ParentBiDiMode = False
          TabOrder = 3
          OnClick = chkbFilesLoadedClick
        end
        object cmbVersion: TComboBox
          Left = 81
          Top = 74
          Width = 250
          Height = 21
          TabOrder = 4
          Text = '6'
          OnChange = cmbVersionChange
          Items.Strings = (
            '6'
            '6.1'
            '6.2'
            '7')
        end
        object chkbStudyDataLocked: TCheckBox
          Left = 81
          Top = 125
          Width = 111
          Height = 14
          BiDiMode = bdLeftToRight
          Caption = 'Study Data Locked:'
          ParentBiDiMode = False
          TabOrder = 5
          OnClick = chkbStudyDataLockedClick
        end
      end
      object pnlStudy: TPanel
        Left = 0
        Top = 0
        Width = 740
        Height = 146
        Align = alTop
        BevelInner = bvLowered
        TabOrder = 1
        object lblStudyCaption: TLabel
          Left = 48
          Top = 10
          Width = 30
          Height = 13
          Alignment = taRightJustify
          Caption = 'Study:'
        end
        object lblStudyLabelCaption: TLabel
          Left = 17
          Top = 37
          Width = 61
          Height = 13
          Alignment = taRightJustify
          Caption = 'Study Name:'
        end
        object lblStudyClientCaption: TLabel
          Left = 49
          Top = 65
          Width = 29
          Height = 13
          Alignment = taRightJustify
          Caption = 'Client:'
        end
        object lblStudyConsultantCaption: TLabel
          Left = 25
          Top = 93
          Width = 53
          Height = 13
          Alignment = taRightJustify
          Caption = 'Consultant:'
        end
        object lblStudyNumberCaption: TLabel
          Left = 374
          Top = 14
          Width = 70
          Height = 13
          Alignment = taRightJustify
          Caption = 'Study Number:'
        end
        object lblStudyDateCaption: TLabel
          Left = 388
          Top = 45
          Width = 56
          Height = 13
          Alignment = taRightJustify
          Caption = 'Study Date:'
        end
        object lblStudyDescrCaption: TLabel
          Left = 388
          Top = 101
          Width = 56
          Height = 13
          Alignment = taRightJustify
          Caption = 'Description:'
        end
        object Label2: TLabel
          Left = 27
          Top = 118
          Width = 50
          Height = 13
          Alignment = taRightJustify
          Caption = 'Shape File'
        end
        object edtStudy: TEdit
          Left = 81
          Top = 8
          Width = 250
          Height = 21
          TabOrder = 0
        end
        object edtStudyLabel: TEdit
          Left = 81
          Top = 35
          Width = 250
          Height = 21
          TabOrder = 1
        end
        object edtStudyClient: TEdit
          Left = 81
          Top = 63
          Width = 250
          Height = 21
          TabOrder = 2
        end
        object edtStudyConsultant: TEdit
          Left = 81
          Top = 91
          Width = 250
          Height = 21
          TabOrder = 3
        end
        object edtStudyNumber: TEdit
          Left = 447
          Top = 12
          Width = 270
          Height = 21
          TabOrder = 4
          Text = '0'
        end
        object dtpStudyDate: TDateTimePicker
          Left = 447
          Top = 43
          Width = 270
          Height = 21
          Date = 37414.422419050900000000
          Time = 37414.422419050900000000
          TabOrder = 5
        end
        object memoStudyDescr: TMemo
          Left = 448
          Top = 74
          Width = 271
          Height = 63
          TabOrder = 6
        end
        object edtStudyShapeFileName: TEdit
          Left = 80
          Top = 116
          Width = 250
          Height = 21
          TabOrder = 7
        end
        object btnLoadStudyShapeFiles: TButton
          Left = 333
          Top = 116
          Width = 23
          Height = 21
          Caption = '...'
          TabOrder = 8
          OnClick = btnLoadStudyShapeFilesClick
        end
      end
      object pnlSubArea: TPanel
        Left = 0
        Top = 181
        Width = 740
        Height = 100
        Align = alTop
        BevelInner = bvLowered
        TabOrder = 2
        object lblSubAreaCaption: TLabel
          Left = 34
          Top = 13
          Width = 44
          Height = 13
          Alignment = taRightJustify
          Caption = 'SubArea:'
        end
        object lblSubAreaLabelCaption: TLabel
          Left = 3
          Top = 41
          Width = 75
          Height = 13
          Alignment = taRightJustify
          Caption = 'SubArea Name:'
        end
        object lblSubAreDescr: TLabel
          Left = 388
          Top = 37
          Width = 56
          Height = 13
          Alignment = taRightJustify
          Caption = 'Description:'
        end
        object Label4: TLabel
          Left = 27
          Top = 74
          Width = 50
          Height = 13
          Alignment = taRightJustify
          Caption = 'Shape File'
        end
        object edtSubArea: TEdit
          Left = 81
          Top = 11
          Width = 250
          Height = 21
          TabOrder = 0
          OnKeyPress = FormKeyPress
        end
        object edtSubAreaLabel: TEdit
          Left = 81
          Top = 39
          Width = 250
          Height = 21
          TabOrder = 1
        end
        object memoSubAreaDescr: TMemo
          Left = 448
          Top = 9
          Width = 271
          Height = 63
          TabOrder = 2
        end
        object edtSubAreaShapeFileName: TEdit
          Left = 80
          Top = 68
          Width = 250
          Height = 21
          TabOrder = 3
        end
        object btnLoadSubAreaShapeFiles: TButton
          Left = 335
          Top = 72
          Width = 23
          Height = 17
          Caption = '...'
          TabOrder = 4
          OnClick = btnLoadStudyShapeFilesClick
        end
      end
    end
    object tbsYieldModel: TTabSheet
      Caption = 'tbsYieldModel'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object pnlYieldModel: TPanel
        Left = 0
        Top = 0
        Width = 740
        Height = 451
        Align = alClient
        TabOrder = 0
        object lblScenarioFilesPathCaption: TLabel
          Left = 11
          Top = 28
          Width = 67
          Height = 13
          Alignment = taRightJustify
          Caption = 'Wrym.dat File:'
        end
        object lblWrymFileContents: TLabel
          Left = 6
          Top = 92
          Width = 72
          Height = 13
          Alignment = taRightJustify
          Caption = 'Wrym File Data'
          Enabled = False
        end
        object lblCalenderMonth: TLabel
          Left = 10
          Top = 60
          Width = 55
          Height = 13
          Alignment = taRightJustify
          Caption = 'Start Month'
        end
        object memoWrymContents: TMemo
          Left = 82
          Top = 90
          Width = 247
          Height = 127
          Enabled = False
          TabOrder = 0
        end
        object edtScenarioFilesPath: TEdit
          Left = 82
          Top = 20
          Width = 245
          Height = 21
          TabOrder = 1
          OnExit = edtScenarioFilesPathExit
        end
        object btnScenarioPath: TButton
          Left = 328
          Top = 20
          Width = 23
          Height = 21
          Caption = '...'
          TabOrder = 2
          OnClick = btnScenarioPathClick
        end
        object cmbCalenderStartMonth: TComboBox
          Left = 83
          Top = 55
          Width = 246
          Height = 21
          Style = csDropDownList
          ItemIndex = 9
          TabOrder = 3
          Text = '10'
          OnChange = cmbCalenderStartMonthChange
          Items.Strings = (
            '01'
            '02'
            '03'
            '04'
            '05'
            '06'
            '07'
            '08'
            '09'
            '10'
            '11'
            '12')
        end
        object btnScenarioPathCreate: TButton
          Left = 355
          Top = 20
          Width = 51
          Height = 21
          Caption = 'Create'
          TabOrder = 4
          OnClick = btnScenarioPathCreateClick
        end
      end
    end
    object tbsHydrology: TTabSheet
      Caption = 'tbsHydrology'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lblHydrologyInputDir: TLabel
        Left = 45
        Top = 74
        Width = 72
        Height = 13
        Alignment = taRightJustify
        Caption = 'Input Directory:'
      end
      object lblHydrologyIOutputDir: TLabel
        Left = 37
        Top = 104
        Width = 80
        Height = 13
        Alignment = taRightJustify
        Caption = 'Output Directory:'
      end
      object lblHydrologyIDebugRequired: TLabel
        Left = 39
        Top = 247
        Width = 78
        Height = 13
        Alignment = taRightJustify
        Caption = 'Debug Required'
      end
      object lblHydrologyIDebugStart: TLabel
        Left = 24
        Top = 134
        Width = 93
        Height = 13
        Alignment = taRightJustify
        Caption = 'Debug Start Period:'
      end
      object lblHydrologyIDebugEnd: TLabel
        Left = 27
        Top = 164
        Width = 90
        Height = 13
        Alignment = taRightJustify
        Caption = 'Debug End Period:'
      end
      object lblHydrologyISumRequired: TLabel
        Left = 28
        Top = 270
        Width = 89
        Height = 13
        Alignment = taRightJustify
        Caption = 'Summary Required'
      end
      object lblHydrologyISimStart: TLabel
        Left = 15
        Top = 194
        Width = 101
        Height = 13
        Alignment = taRightJustify
        Caption = 'Simulation Start Year:'
      end
      object lblHydrologyISimEnd: TLabel
        Left = 19
        Top = 224
        Width = 98
        Height = 13
        Alignment = taRightJustify
        Caption = 'Simulation End Year:'
      end
      object btnHydrologyIInputDir: TSpeedButton
        Left = 370
        Top = 71
        Width = 23
        Height = 21
        Caption = '...'
        OnClick = btnHydrologyIInputDirClick
      end
      object btnHydrologyIOuputDir: TSpeedButton
        Left = 370
        Top = 100
        Width = 23
        Height = 21
        Caption = '...'
        OnClick = btnHydrologyIOuputDirClick
      end
      object lblNetworkID: TLabel
        Left = 64
        Top = 12
        Width = 54
        Height = 13
        Alignment = taRightJustify
        Caption = 'NetworkID:'
      end
      object lblNetworkCode: TLabel
        Left = 50
        Top = 44
        Width = 68
        Height = 13
        Alignment = taRightJustify
        Caption = 'NetworkCode:'
      end
      object edtHydrologyInputDir: TEdit
        Left = 120
        Top = 70
        Width = 250
        Height = 21
        TabOrder = 0
      end
      object edtHydrologyIOutputDir: TEdit
        Left = 120
        Top = 100
        Width = 250
        Height = 21
        TabOrder = 1
      end
      object chkHydrologyIDebugRequired: TCheckBox
        Left = 120
        Top = 245
        Width = 17
        Height = 17
        Checked = True
        State = cbChecked
        TabOrder = 2
      end
      object edtHydrologyIDebugStart: TEdit
        Left = 120
        Top = 130
        Width = 80
        Height = 21
        TabOrder = 3
      end
      object edtHydrologyIDebugEnd: TEdit
        Left = 120
        Top = 160
        Width = 80
        Height = 21
        TabOrder = 4
      end
      object chkHydrologyISumRequired: TCheckBox
        Left = 120
        Top = 268
        Width = 17
        Height = 17
        Checked = True
        State = cbChecked
        TabOrder = 5
      end
      object edtHydrologyISimStart: TEdit
        Left = 119
        Top = 190
        Width = 80
        Height = 21
        TabOrder = 6
      end
      object edtHydrologyISimEnd: TEdit
        Left = 119
        Top = 220
        Width = 80
        Height = 21
        TabOrder = 7
      end
      object edtNetworkID: TEdit
        Left = 121
        Top = 8
        Width = 80
        Height = 21
        Color = clSilver
        ReadOnly = True
        TabOrder = 8
      end
      object edtNetworkCode: TEdit
        Left = 121
        Top = 40
        Width = 80
        Height = 21
        Color = clSilver
        ReadOnly = True
        TabOrder = 9
      end
    end
    object tbsRainfall: TTabSheet
      Caption = 'tbsRainfall'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lblRainfalldir: TLabel
        Left = 6
        Top = 30
        Width = 160
        Height = 13
        Alignment = taRightJustify
        Caption = 'ClassR and PatchR File Directory:'
      end
      object edtRainfallDefaultDirectory: TEdit
        Left = 170
        Top = 28
        Width = 245
        Height = 21
        TabOrder = 0
      end
      object btnSetDirectory: TButton
        Left = 416
        Top = 28
        Width = 23
        Height = 21
        Caption = '...'
        TabOrder = 1
      end
    end
    object tbsStomsa: TTabSheet
      Caption = 'tbsStomsa'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lblStomsaHydrologypath: TLabel
        Left = 11
        Top = 40
        Width = 99
        Height = 13
        Alignment = taRightJustify
        Caption = 'Hydrology Files Path:'
      end
      object edtStomsaHydrologypath: TEdit
        Left = 114
        Top = 36
        Width = 375
        Height = 21
        TabOrder = 0
      end
      object btnStomsaHydrologypath: TButton
        Left = 492
        Top = 36
        Width = 23
        Height = 21
        Caption = '...'
        TabOrder = 1
        OnClick = btnStomsaHydrologypathClick
      end
    end
    object tbsDDTS: TTabSheet
      Caption = 'tbsDDTS'
      ImageIndex = 5
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label1: TLabel
        Left = 8
        Top = 28
        Width = 70
        Height = 13
        Alignment = taRightJustify
        Caption = 'DDTS.dat File:'
      end
      object Label3: TLabel
        Left = 3
        Top = 60
        Width = 75
        Height = 13
        Alignment = taRightJustify
        Caption = 'DDTS File Data'
        Enabled = False
      end
      object memoDDTSContents: TMemo
        Left = 84
        Top = 57
        Width = 247
        Height = 127
        Enabled = False
        TabOrder = 0
      end
      object Button1: TButton
        Left = 355
        Top = 20
        Width = 51
        Height = 21
        Caption = 'Create'
        TabOrder = 1
        OnClick = btnScenarioPathCreateClick
      end
      object Button2: TButton
        Left = 328
        Top = 20
        Width = 23
        Height = 21
        Caption = '...'
        TabOrder = 2
        OnClick = btnScenarioPathClick
      end
      object edtDDTSFilesPath: TEdit
        Left = 82
        Top = 20
        Width = 245
        Height = 21
        TabOrder = 3
        OnExit = edtScenarioFilesPathExit
      end
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 0
    Width = 748
    Height = 38
    Align = alTop
    BevelInner = bvLowered
    TabOrder = 1
    object btnReset: TBitBtn
      Left = 120
      Top = 5
      Width = 100
      Height = 28
      Caption = 'Reset'
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333444444
        33333333333F8888883F33330000324334222222443333388F3833333388F333
        000032244222222222433338F8833FFFFF338F3300003222222AAAAA22243338
        F333F88888F338F30000322222A33333A2224338F33F8333338F338F00003222
        223333333A224338F33833333338F38F00003222222333333A444338FFFF8F33
        3338888300003AAAAAAA33333333333888888833333333330000333333333333
        333333333333333333FFFFFF000033333333333344444433FFFF333333888888
        00003A444333333A22222438888F333338F3333800003A2243333333A2222438
        F38F333333833338000033A224333334422224338338FFFFF8833338000033A2
        22444442222224338F3388888333FF380000333A2222222222AA243338FF3333
        33FF88F800003333AA222222AA33A3333388FFFFFF8833830000333333AAAAAA
        3333333333338888883333330000333333333333333333333333333333333333
        0000}
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnResetClick
    end
    object btnCancel: TBitBtn
      Left = 230
      Top = 5
      Width = 100
      Height = 28
      Caption = 'Cancel'
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        333333333333333333333333000033338833333333333333333F333333333333
        0000333911833333983333333388F333333F3333000033391118333911833333
        38F38F333F88F33300003339111183911118333338F338F3F8338F3300003333
        911118111118333338F3338F833338F3000033333911111111833333338F3338
        3333F8330000333333911111183333333338F333333F83330000333333311111
        8333333333338F3333383333000033333339111183333333333338F333833333
        00003333339111118333333333333833338F3333000033333911181118333333
        33338333338F333300003333911183911183333333383338F338F33300003333
        9118333911183333338F33838F338F33000033333913333391113333338FF833
        38F338F300003333333333333919333333388333338FFF830000333333333333
        3333333333333333333888330000333333333333333333333333333333333333
        0000}
      ModalResult = 2
      NumGlyphs = 2
      TabOrder = 1
    end
    object btnSave: TBitBtn
      Left = 10
      Top = 5
      Width = 100
      Height = 28
      Caption = 'Save'
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333330000333333333333333333333333F33333333333
        00003333344333333333333333388F3333333333000033334224333333333333
        338338F3333333330000333422224333333333333833338F3333333300003342
        222224333333333383333338F3333333000034222A22224333333338F338F333
        8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
        33333338F83338F338F33333000033A33333A222433333338333338F338F3333
        0000333333333A222433333333333338F338F33300003333333333A222433333
        333333338F338F33000033333333333A222433333333333338F338F300003333
        33333333A222433333333333338F338F00003333333333333A22433333333333
        3338F38F000033333333333333A223333333333333338F830000333333333333
        333A333333333333333338330000333333333333333333333333333333333333
        0000}
      NumGlyphs = 2
      TabOrder = 2
    end
    object ProgressBar1: TProgressBar
      Left = 454
      Top = 7
      Width = 271
      Height = 24
      TabOrder = 3
    end
  end
  object dlgWYRMFileSelector: TOpenDialog
    Left = 478
    Top = 84
  end
end
