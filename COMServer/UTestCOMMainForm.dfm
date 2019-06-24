object frmMain: TfrmMain
  Left = 234
  Top = 138
  ActiveControl = edtStudy
  Caption = 'Test COM Server'
  ClientHeight = 515
  ClientWidth = 951
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pgctrlMain: TPageControl
    Left = 0
    Top = 0
    Width = 951
    Height = 485
    ActivePage = tsSetup
    Align = alClient
    TabOrder = 0
    OnChange = pgctrlMainChange
    ExplicitWidth = 120
    ExplicitHeight = 514
    object tsSetup: TTabSheet
      Caption = 'Setup'
      object pnlTop: TPanel
        Left = 0
        Top = 0
        Width = 943
        Height = 129
        Align = alTop
        BevelInner = bvLowered
        TabOrder = 0
        object GroupBox1: TGroupBox
          Left = 2
          Top = 2
          Width = 295
          Height = 125
          Align = alLeft
          Caption = 'Study'
          TabOrder = 0
          object lblStudy: TLabel
            Left = 28
            Top = 15
            Width = 37
            Height = 13
            Alignment = taRightJustify
            Caption = 'Study:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object lblModel: TLabel
            Left = 26
            Top = 39
            Width = 39
            Height = 13
            Alignment = taRightJustify
            Caption = 'Model:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object lblSubArea: TLabel
            Left = 12
            Top = 63
            Width = 53
            Height = 13
            Alignment = taRightJustify
            Caption = 'SubArea:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object lblScenario: TLabel
            Left = 10
            Top = 88
            Width = 55
            Height = 13
            Alignment = taRightJustify
            Caption = 'Scenario:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object edtStudy: TEdit
            Left = 67
            Top = 16
            Width = 173
            Height = 21
            TabOrder = 0
            Text = 'Training'
          end
          object edtModel: TEdit
            Left = 67
            Top = 40
            Width = 173
            Height = 21
            TabOrder = 1
            Text = 'WRYM'
          end
          object edtSubArea: TEdit
            Left = 67
            Top = 64
            Width = 173
            Height = 21
            TabOrder = 2
            Text = 'Caledon-Modder'
          end
          object edtScenario: TEdit
            Left = 67
            Top = 89
            Width = 173
            Height = 21
            TabOrder = 3
            Text = 'DemandCentre'
          end
        end
        object GroupBox2: TGroupBox
          Left = 297
          Top = 2
          Width = 160
          Height = 125
          Align = alLeft
          Caption = 'User'
          TabOrder = 1
          object lblUserID: TLabel
            Left = 21
            Top = 26
            Width = 44
            Height = 13
            Alignment = taRightJustify
            Caption = 'UserID:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label2: TLabel
            Left = 6
            Top = 59
            Width = 59
            Height = 13
            Alignment = taRightJustify
            Caption = 'Password:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object edtUserID: TEdit
            Left = 67
            Top = 24
            Width = 80
            Height = 21
            TabOrder = 0
            Text = 'Administrator'
          end
          object edtPassword: TEdit
            Left = 67
            Top = 56
            Width = 80
            Height = 21
            TabOrder = 1
            Text = 'wrmf'
          end
        end
        object GroupBox3: TGroupBox
          Left = 457
          Top = 2
          Width = 112
          Height = 125
          Align = alLeft
          TabOrder = 2
          object btnLogon: TButton
            Left = 16
            Top = 16
            Width = 75
            Height = 25
            Caption = 'Logon User'
            TabOrder = 0
            OnClick = btnLogonClick
          end
          object btnSelectStudy: TButton
            Left = 16
            Top = 56
            Width = 75
            Height = 25
            Caption = 'Select Study'
            Enabled = False
            TabOrder = 1
            OnClick = btnSelectStudyClick
          end
        end
      end
    end
    object tsSFR: TTabSheet
      Caption = 'SFR'
      ImageIndex = 1
      object PnlClient: TPanel
        Left = 0
        Top = 0
        Width = 943
        Height = 357
        Align = alClient
        BevelInner = bvLowered
        TabOrder = 0
        object srtgrdFileData: TStringGrid
          Left = 2
          Top = 2
          Width = 939
          Height = 353
          Align = alClient
          ColCount = 14
          DefaultRowHeight = 18
          FixedCols = 0
          RowCount = 2
          TabOrder = 0
          ColWidths = (
            64
            64
            64
            64
            64
            64
            64
            64
            64
            64
            64
            64
            64
            64)
          RowHeights = (
            18
            18)
        end
      end
      object pnlBottom: TPanel
        Left = 0
        Top = 357
        Width = 943
        Height = 100
        Align = alBottom
        BevelInner = bvLowered
        TabOrder = 1
        object lblSFR: TLabel
          Left = 16
          Top = 11
          Width = 95
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Select SFR'
        end
        object lblRunOffFile: TLabel
          Left = 16
          Top = 44
          Width = 95
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Unit Runoff File'
        end
        object lblSoilMoisture: TLabel
          Left = 16
          Top = 69
          Width = 95
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Soil Moisture'
        end
        object btnViewRuoffFile: TButton
          Left = 475
          Top = 39
          Width = 50
          Height = 21
          Caption = 'View File'
          Enabled = False
          TabOrder = 0
          OnClick = btnViewRuoffFileClick
        end
        object btnViewSoilMoistureFile: TButton
          Left = 475
          Top = 67
          Width = 50
          Height = 21
          Caption = 'View File'
          Enabled = False
          TabOrder = 1
          OnClick = btnViewSoilMoistureFileClick
        end
        object cmbboxSFR: TComboBox
          Left = 112
          Top = 11
          Width = 145
          Height = 21
          Enabled = False
          TabOrder = 2
          OnChange = cmbboxSFRChange
        end
        object edtRunoff: TEdit
          Left = 112
          Top = 40
          Width = 361
          Height = 21
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 3
        end
        object edtSoilMoisture: TEdit
          Left = 112
          Top = 67
          Width = 361
          Height = 21
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 4
        end
      end
    end
    object tsDemandCenter: TTabSheet
      Caption = 'Demand Center'
      ImageIndex = 2
      ExplicitWidth = 112
      ExplicitHeight = 486
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 943
        Height = 41
        Align = alTop
        BevelInner = bvLowered
        TabOrder = 0
        ExplicitWidth = 112
        object Label1: TLabel
          Left = 24
          Top = 16
          Width = 129
          Height = 13
          Caption = 'Select demand centre:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object cmbDemandCentres: TComboBox
          Left = 156
          Top = 13
          Width = 250
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          OnChange = cmbDemandCentresChange
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 41
        Width = 943
        Height = 416
        Align = alClient
        BevelInner = bvLowered
        TabOrder = 1
        ExplicitWidth = 112
        ExplicitHeight = 445
        object GroupBox4: TGroupBox
          Left = 2
          Top = 2
          Width = 939
          Height = 79
          Align = alTop
          Caption = 'Demand Centre'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          ExplicitWidth = 108
          object Label3: TLabel
            Left = 16
            Top = 23
            Width = 100
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Node Number:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label4: TLabel
            Left = 14
            Top = 52
            Width = 100
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Name:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object edtDemandCentreNodeNumber: TEdit
            Left = 118
            Top = 15
            Width = 241
            Height = 24
            Color = clSilver
            TabOrder = 0
          end
          object edtDemandCentreName: TEdit
            Left = 118
            Top = 44
            Width = 241
            Height = 24
            Color = clSilver
            TabOrder = 1
          end
        end
        object GroupBox5: TGroupBox
          Left = 2
          Top = 81
          Width = 939
          Height = 80
          Align = alTop
          Caption = 'Consumptive Channel'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
          ExplicitWidth = 108
          object Label5: TLabel
            Left = 16
            Top = 20
            Width = 100
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Channel Number'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label6: TLabel
            Left = 14
            Top = 49
            Width = 100
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Channel Name:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label7: TLabel
            Left = 350
            Top = 18
            Width = 160
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Upstream Node Number:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label8: TLabel
            Left = 350
            Top = 47
            Width = 160
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Downstream Node Number:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object edtDCCCChannelNumber: TEdit
            Left = 118
            Top = 15
            Width = 200
            Height = 24
            Color = clSilver
            TabOrder = 0
          end
          object edtDCCCChannelName: TEdit
            Left = 118
            Top = 44
            Width = 200
            Height = 24
            Color = clSilver
            TabOrder = 1
          end
          object edtDCCCChannelUpstreamNodeNumber: TEdit
            Left = 512
            Top = 14
            Width = 200
            Height = 24
            Color = clSilver
            TabOrder = 2
          end
          object edtDCCCChannelDownstreamNodeNumber: TEdit
            Left = 512
            Top = 43
            Width = 200
            Height = 24
            Color = clSilver
            TabOrder = 3
          end
        end
        object GroupBox6: TGroupBox
          Left = 2
          Top = 161
          Width = 939
          Height = 112
          Align = alTop
          Caption = 'Reclaimation Channel'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 2
          ExplicitWidth = 108
          object Label9: TLabel
            Left = 16
            Top = 51
            Width = 100
            Height = 14
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Channel Number'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label10: TLabel
            Left = 14
            Top = 80
            Width = 100
            Height = 14
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Channel Name:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label11: TLabel
            Left = 350
            Top = 49
            Width = 160
            Height = 14
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Upstream Node Number:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label12: TLabel
            Left = 350
            Top = 78
            Width = 160
            Height = 14
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Downstream Node Number:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object edtDCRPChannelNumber: TEdit
            Left = 118
            Top = 46
            Width = 200
            Height = 24
            Color = clSilver
            TabOrder = 0
          end
          object edtDCRPChannelName: TEdit
            Left = 118
            Top = 75
            Width = 200
            Height = 24
            Color = clSilver
            TabOrder = 1
          end
          object edtDCRPChannelUpstreamNodeNumber: TEdit
            Left = 512
            Top = 45
            Width = 200
            Height = 24
            Color = clSilver
            TabOrder = 2
          end
          object edtDCRPChannelDownstreamNodeNumber: TEdit
            Left = 512
            Top = 74
            Width = 200
            Height = 24
            Color = clSilver
            TabOrder = 3
          end
          object chkboxDCRPChannelPlantExist: TCheckBox
            Left = 24
            Top = 25
            Width = 201
            Height = 18
            Alignment = taLeftJustify
            Caption = 'Reclamation Plant Exists'
            Color = clSilver
            ParentColor = False
            TabOrder = 4
          end
        end
        object GroupBox7: TGroupBox
          Left = 2
          Top = 273
          Width = 939
          Height = 141
          Align = alClient
          Caption = 'Return Flow Channels'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 3
          ExplicitWidth = 108
          ExplicitHeight = 170
          object strgrdReturnFlowChanells: TStringGrid
            Left = 2
            Top = 18
            Width = 935
            Height = 121
            Align = alClient
            ColCount = 6
            DefaultColWidth = 100
            DefaultRowHeight = 20
            RowCount = 2
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSizing, goColSizing]
            TabOrder = 0
            ExplicitWidth = 104
            ExplicitHeight = 150
            ColWidths = (
              100
              100
              100
              100
              100
              100)
            RowHeights = (
              20
              20)
          end
        end
      end
    end
  end
  object pnlMessg: TPanel
    Left = 0
    Top = 485
    Width = 951
    Height = 30
    Align = alBottom
    Caption = 'No user is currently logged on'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    ExplicitTop = -30
    ExplicitWidth = 120
  end
end
