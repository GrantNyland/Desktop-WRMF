object frmHydroEditDialog: TfrmHydroEditDialog
  Left = 362
  Top = 167
  Caption = 'Hydrology Networks'
  ClientHeight = 416
  ClientWidth = 406
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 385
    Height = 369
    TabOrder = 0
    object PageControl1: TPageControl
      Left = 10
      Top = 5
      Width = 361
      Height = 361
      ActivePage = TabSheet3
      TabOrder = 0
      object TabSheet6: TTabSheet
        Caption = 'Insert'
        ImageIndex = 5
        OnShow = TabSheet6Show
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Label23: TLabel
          Left = 8
          Top = 0
          Width = 136
          Height = 13
          Caption = 'Networks Available to Insert'
        end
        object Label24: TLabel
          Left = 8
          Top = 168
          Width = 130
          Height = 13
          Caption = 'Select Study To Insert Into'
        end
        object InsertListBox: TListBox
          Left = 8
          Top = 16
          Width = 337
          Height = 145
          ItemHeight = 13
          Sorted = True
          TabOrder = 0
        end
        object InsertTreeView: TTreeView
          Left = 8
          Top = 184
          Width = 337
          Height = 113
          AutoExpand = True
          HideSelection = False
          Indent = 19
          RightClickSelect = True
          TabOrder = 1
        end
        object btnInsert: TButton
          Left = 272
          Top = 304
          Width = 75
          Height = 25
          Caption = '&Insert'
          TabOrder = 2
          OnClick = btnInsertClick
        end
      end
      object TabSheet3: TTabSheet
        Caption = 'New'
        ImageIndex = 2
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Label7: TLabel
          Left = 9
          Top = 15
          Width = 92
          Height = 13
          Alignment = taRightJustify
          Caption = 'New Network Code'
        end
        object Label8: TLabel
          Left = 26
          Top = 36
          Width = 75
          Height = 13
          Alignment = taRightJustify
          Caption = 'Version Number'
        end
        object Label9: TLabel
          Left = 28
          Top = 57
          Width = 73
          Height = 13
          Alignment = taRightJustify
          Caption = 'Input Directory'
        end
        object Label10: TLabel
          Left = 20
          Top = 78
          Width = 81
          Height = 13
          Alignment = taRightJustify
          Caption = 'Output Directory'
        end
        object Label11: TLabel
          Left = 24
          Top = 97
          Width = 77
          Height = 13
          Alignment = taRightJustify
          Caption = 'Debug Required'
        end
        object Label12: TLabel
          Left = 10
          Top = 117
          Width = 91
          Height = 13
          Alignment = taRightJustify
          Caption = 'Debug Start Period'
        end
        object Label13: TLabel
          Left = 16
          Top = 138
          Width = 85
          Height = 13
          Alignment = taRightJustify
          Caption = 'Debug End Period'
        end
        object Label14: TLabel
          Left = 187
          Top = 97
          Width = 90
          Height = 13
          Alignment = taRightJustify
          Caption = 'Summary Required'
        end
        object Label15: TLabel
          Left = 177
          Top = 117
          Width = 100
          Height = 13
          Alignment = taRightJustify
          Caption = 'Simulation Start Year'
        end
        object Label16: TLabel
          Left = 183
          Top = 138
          Width = 94
          Height = 13
          Alignment = taRightJustify
          Caption = 'Simulation End Year'
        end
        object Label17: TLabel
          Left = 51
          Top = 158
          Width = 50
          Height = 13
          Alignment = taRightJustify
          Caption = 'Read Only'
        end
        object btnInputDir: TSpeedButton
          Left = 320
          Top = 53
          Width = 23
          Height = 21
          Caption = '...'
          OnClick = btnInputDirClick
        end
        object btnOuputDir: TSpeedButton
          Left = 320
          Top = 74
          Width = 23
          Height = 21
          Caption = '...'
          OnClick = btnOuputDirClick
        end
        object edtNewNetCode: TEdit
          Left = 112
          Top = 11
          Width = 81
          Height = 21
          TabOrder = 0
        end
        object edtNewVersion: TEdit
          Left = 112
          Top = 32
          Width = 81
          Height = 21
          TabOrder = 1
        end
        object edtInputDir: TEdit
          Left = 112
          Top = 53
          Width = 207
          Height = 21
          TabOrder = 2
        end
        object edtOutputDir: TEdit
          Left = 112
          Top = 74
          Width = 207
          Height = 21
          TabOrder = 3
        end
        object chkNewDebugRequired: TCheckBox
          Left = 112
          Top = 95
          Width = 17
          Height = 17
          TabOrder = 4
        end
        object edtNewDebugStart: TEdit
          Left = 112
          Top = 113
          Width = 57
          Height = 21
          TabOrder = 6
        end
        object edtNewDebugEnd: TEdit
          Left = 112
          Top = 134
          Width = 57
          Height = 21
          TabOrder = 7
        end
        object chkNewSumRequired: TCheckBox
          Left = 288
          Top = 95
          Width = 17
          Height = 17
          TabOrder = 5
        end
        object edtNewSimStart: TEdit
          Left = 288
          Top = 113
          Width = 57
          Height = 21
          TabOrder = 8
        end
        object edtNewSimEnd: TEdit
          Left = 288
          Top = 134
          Width = 57
          Height = 21
          TabOrder = 9
        end
        object chkNewReadOnly: TCheckBox
          Left = 112
          Top = 156
          Width = 17
          Height = 17
          TabOrder = 10
        end
        object btnCreate: TButton
          Left = 272
          Top = 304
          Width = 75
          Height = 25
          Caption = '&Create'
          TabOrder = 11
          OnClick = btnCreateClick
        end
        object rdoNetwork: TRadioButton
          Left = 0
          Top = 176
          Width = 113
          Height = 17
          Caption = 'Only create network'
          Checked = True
          TabOrder = 12
          TabStop = True
          OnClick = rdoNetworkClick
        end
        object rdoNetworkAndStudy: TRadioButton
          Left = 0
          Top = 192
          Width = 241
          Height = 17
          Caption = 'Create Network and insert into existing study'
          TabOrder = 13
          OnClick = rdoNetworkAndStudyClick
        end
        object GroupBox4: TGroupBox
          Left = 0
          Top = 208
          Width = 273
          Height = 121
          Caption = 'Select Study To Insert Into'
          Enabled = False
          TabOrder = 14
          object NewTreeView: TTreeView
            Left = 2
            Top = 15
            Width = 269
            Height = 104
            Align = alClient
            Indent = 19
            TabOrder = 0
          end
        end
      end
      object TabSheet5: TTabSheet
        Caption = 'Copy'
        ImageIndex = 4
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Label6: TLabel
          Left = 8
          Top = 8
          Width = 129
          Height = 13
          Caption = 'Select the network to copy'
        end
        object RadioButton3: TRadioButton
          Left = 8
          Top = 136
          Width = 169
          Height = 17
          Caption = 'Copy into study'
          TabOrder = 0
          Visible = False
          OnClick = RadioButton3Click
        end
        object RadioButton4: TRadioButton
          Left = 8
          Top = 152
          Width = 169
          Height = 17
          Caption = 'Copy to new network'
          Checked = True
          TabOrder = 1
          TabStop = True
          Visible = False
          OnClick = RadioButton4Click
        end
        object CopyTreeView: TTreeView
          Left = 8
          Top = 24
          Width = 337
          Height = 113
          AutoExpand = True
          HideSelection = False
          Indent = 19
          RightClickSelect = True
          TabOrder = 2
        end
        object btnCopy: TButton
          Left = 272
          Top = 304
          Width = 75
          Height = 25
          Caption = '&Copy'
          Enabled = False
          TabOrder = 3
          OnClick = btnCopyClick
        end
        object GroupBox2: TGroupBox
          Left = 8
          Top = 176
          Width = 337
          Height = 121
          Caption = 'Study to copy into'
          TabOrder = 4
          Visible = False
          object TreeView3: TTreeView
            Left = 2
            Top = 15
            Width = 333
            Height = 104
            Align = alClient
            AutoExpand = True
            HideSelection = False
            Indent = 19
            RightClickSelect = True
            TabOrder = 0
          end
        end
        object GroupBox3: TGroupBox
          Left = 8
          Top = 176
          Width = 337
          Height = 121
          Caption = 'New Network'
          TabOrder = 5
          object Label19: TLabel
            Left = 39
            Top = 24
            Width = 92
            Height = 13
            Alignment = taRightJustify
            Caption = 'New Network Code'
          end
          object Label20: TLabel
            Left = 62
            Top = 48
            Width = 69
            Height = 13
            Alignment = taRightJustify
            Caption = 'Scenario Label'
          end
          object Label21: TLabel
            Left = 56
            Top = 96
            Width = 75
            Height = 13
            Alignment = taRightJustify
            Caption = 'Version Number'
          end
          object Label22: TLabel
            Left = 34
            Top = 72
            Width = 97
            Height = 13
            Alignment = taRightJustify
            Caption = 'Scenario Description'
          end
          object edtCopyNetworkCode: TEdit
            Left = 136
            Top = 20
            Width = 121
            Height = 21
            TabOrder = 0
            OnChange = edtCopyNetworkCodeChange
          end
          object edtCopyScenarioLabel: TEdit
            Left = 136
            Top = 44
            Width = 121
            Height = 21
            TabOrder = 1
            OnChange = edtCopyNetworkCodeChange
          end
          object edtCopyVersion: TEdit
            Left = 136
            Top = 92
            Width = 121
            Height = 21
            TabOrder = 3
            OnChange = edtCopyNetworkCodeChange
          end
          object edtCopyScenarioDescr: TEdit
            Left = 136
            Top = 68
            Width = 121
            Height = 21
            TabOrder = 2
            OnChange = edtCopyNetworkCodeChange
          end
        end
      end
      object TabSheet4: TTabSheet
        Caption = 'Delete'
        ImageIndex = 3
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Panel4: TPanel
          Left = 0
          Top = 16
          Width = 345
          Height = 313
          BevelOuter = bvNone
          TabOrder = 3
          object Label5: TLabel
            Left = 8
            Top = 8
            Width = 126
            Height = 13
            Caption = 'Select a network to delete'
          end
          object DeleteTreeView: TTreeView
            Left = 8
            Top = 24
            Width = 337
            Height = 241
            AutoExpand = True
            HideSelection = False
            Indent = 19
            RightClickSelect = True
            TabOrder = 0
          end
          object rdoDeleteFromStudy: TRadioButton
            Left = 8
            Top = 280
            Width = 185
            Height = 17
            Caption = 'Delete From Selected Study'
            Checked = True
            TabOrder = 1
            TabStop = True
          end
          object rdoDeleteAll: TRadioButton
            Left = 8
            Top = 296
            Width = 233
            Height = 17
            Caption = 'Delete All References To This Network'
            TabOrder = 2
          end
        end
        object btnDelete: TButton
          Left = 272
          Top = 304
          Width = 75
          Height = 25
          Caption = 'Delete'
          TabOrder = 0
          OnClick = btnDeleteClick
        end
        object rdoDeleteNetwork: TRadioButton
          Left = 16
          Top = 0
          Width = 113
          Height = 17
          Caption = 'Delete Network'
          TabOrder = 1
          OnClick = rdoDeleteNetworkClick
        end
        object rdoDeleteStudy: TRadioButton
          Left = 128
          Top = 0
          Width = 113
          Height = 17
          Caption = 'Delete From Study'
          Checked = True
          TabOrder = 2
          TabStop = True
          OnClick = rdoDeleteStudyClick
        end
        object Panel5: TPanel
          Left = 0
          Top = 24
          Width = 345
          Height = 273
          BevelOuter = bvNone
          TabOrder = 4
          Visible = False
          object Label25: TLabel
            Left = 8
            Top = 0
            Width = 126
            Height = 13
            Caption = 'Select a network to delete'
          end
          object DeleteListBox: TListBox
            Left = 8
            Top = 16
            Width = 337
            Height = 249
            ItemHeight = 13
            Sorted = True
            TabOrder = 0
          end
        end
      end
      object TabSheet1: TTabSheet
        Caption = 'Import'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object GroupBox1: TGroupBox
          Left = 0
          Top = 0
          Width = 353
          Height = 333
          Align = alLeft
          TabOrder = 0
          object ImportFileTreeView: TTreeView
            Left = 2
            Top = 73
            Width = 349
            Height = 227
            Align = alClient
            HideSelection = False
            Indent = 19
            MultiSelect = True
            TabOrder = 0
            OnClick = ImportFileTreeViewClick
            OnKeyDown = ImportFileTreeViewKeyDown
          end
          object Panel2: TPanel
            Left = 2
            Top = 300
            Width = 349
            Height = 31
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 1
            DesignSize = (
              349
              31)
            object Label18: TLabel
              Left = 8
              Top = 8
              Width = 92
              Height = 13
              Caption = 'New Network Code'
            end
            object btnImport: TButton
              Left = 270
              Top = 3
              Width = 75
              Height = 25
              Anchors = [akTop, akRight]
              Caption = '&Import'
              TabOrder = 0
              OnClick = btnImportClick
            end
            object edtNewNetworkCode: TEdit
              Left = 112
              Top = 4
              Width = 81
              Height = 21
              TabOrder = 1
            end
          end
          object Panel3: TPanel
            Left = 2
            Top = 15
            Width = 349
            Height = 58
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 2
            object Label3: TLabel
              Left = 6
              Top = 0
              Width = 132
              Height = 13
              Caption = 'Choose File To Import From'
            end
            object Label1: TLabel
              Left = 6
              Top = 40
              Width = 111
              Height = 13
              Caption = 'Network files in archive'
            end
            object edtImportHydrologyPath: TEdit
              Left = 6
              Top = 16
              Width = 265
              Height = 21
              TabOrder = 0
            end
            object btnHydrologyImportPath: TButton
              Left = 272
              Top = 16
              Width = 25
              Height = 21
              Caption = '...'
              TabOrder = 1
              OnClick = btnHydrologyImportPathClick
            end
          end
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Export'
        ImageIndex = 1
        OnShow = TabSheet2Show
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          353
          333)
        object Label2: TLabel
          Left = 8
          Top = 16
          Width = 145
          Height = 13
          Caption = 'Networks Available For Export'
        end
        object Label4: TLabel
          Left = 8
          Top = 200
          Width = 81
          Height = 13
          Caption = 'File To Export To'
        end
        object edtExportFile: TEdit
          Left = 6
          Top = 216
          Width = 265
          Height = 21
          TabOrder = 0
        end
        object Button2: TButton
          Left = 272
          Top = 216
          Width = 25
          Height = 21
          Caption = '...'
          TabOrder = 1
          OnClick = Button2Click
        end
        object btnExport: TButton
          Left = 278
          Top = 307
          Width = 75
          Height = 25
          Anchors = [akTop, akRight]
          Caption = '&Export'
          TabOrder = 2
          OnClick = btnExportClick
        end
        object NetworkListBox: TListBox
          Left = 8
          Top = 32
          Width = 337
          Height = 161
          ItemHeight = 13
          Sorted = True
          TabOrder = 3
        end
      end
    end
  end
  object OKBtn: TButton
    Left = 204
    Top = 376
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 289
    Top = 376
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    OnClick = CancelBtnClick
  end
  object dlgWYRMFileSelector: TOpenDialog
    Left = 302
    Top = 28
  end
  object ImageList1: TImageList
    Left = 304
    Bitmap = {
      494C010103000400080010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF0000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF0000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008080800040404000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF000000000000000000000000008080800040404000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF000000000000000000000000008080800040404000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008080800040404000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF000000000000000000000000008080800040404000FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF000000000000000000000000008080800040404000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008080800040404000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF000000000000000000000000008080800040404000FFFF
      FF00FFFFFF00000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF000000000000000000000000008080800040404000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008080800040404000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF000000000000000000000000008080800040404000FFFF
      FF000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF000000000000000000000000008080800040404000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008080800040404000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF000000000000000000000000008080800040404000FFFF
      FF000000000000000000FFFFFF00000000000000000000000000FFFFFF00FFFF
      FF0000000000FFFFFF000000000000000000000000008080800040404000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008080800040404000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF000000000000000000000000008080800040404000FFFF
      FF0000000000FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFF
      FF0000000000FFFFFF000000000000000000000000008080800040404000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008080800040404000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF000000000000000000000000008080800040404000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFF
      FF0000000000FFFFFF000000000000000000000000008080800040404000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008080800040404000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF000000000000000000000000008080800040404000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF
      FF0000000000FFFFFF000000000000000000000000008080800040404000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008080800040404000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF000000000000000000000000008080800040404000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF000000000000000000000000008080800040404000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000404040004040
      4000404040004040400040404000404040004040400040404000404040004040
      400000000000FFFFFF0000000000000000000000000080808000404040004040
      4000404040004040400040404000404040004040400040404000404040004040
      400000000000FFFFFF0000000000000000000000000080808000404040004040
      4000404040004040400040404000404040004040400040404000404040004040
      400000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000808080008080
      8000808080008080800080808000808080008080800080808000808080008080
      800080808000FFFFFF0000000000000000000000000080808000808080008080
      8000808080008080800080808000808080008080800080808000808080008080
      800080808000FFFFFF0000000000000000000000000080808000808080008080
      8000808080008080800080808000808080008080800080808000808080008080
      800080808000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFFFFFF0000FFFFFFFFFFFF0000
      8003800380030000BFFBBFFBBFFB0000800B800B800B0000800B800B800B0000
      800B800B800B0000800B800B800B0000800B800B800B0000800B800B800B0000
      800B800B800B0000800B800B800B0000800B800B800B0000800B800B800B0000
      8003800380030000FFFFFFFFFFFF000000000000000000000000000000000000
      000000000000}
  end
  object ExportFileDialog: TSaveDialog
    DefaultExt = 'zip'
    Left = 318
    Top = 237
  end
end
