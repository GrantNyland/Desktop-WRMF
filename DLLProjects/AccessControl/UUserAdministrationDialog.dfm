object UserAdministrationDialog: TUserAdministrationDialog
  Left = 200
  Top = 97
  Width = 525
  Height = 418
  Caption = 'User Administration'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCanResize = FormCanResize
  PixelsPerInch = 96
  TextHeight = 13
  object FPanel1: TPanel
    Left = 0
    Top = 0
    Width = 517
    Height = 384
    Align = alClient
    TabOrder = 0
    object FUserTreeView: TTreeView
      Left = 1
      Top = 129
      Width = 121
      Height = 226
      Align = alLeft
      HideSelection = False
      Indent = 19
      ReadOnly = True
      TabOrder = 0
      OnChange = FUserTreeViewChange
    end
    object FGroupBox1: TGroupBox
      Left = 1
      Top = 1
      Width = 515
      Height = 73
      Align = alTop
      TabOrder = 1
      object FUserIDLabel: TLabel
        Left = 16
        Top = 16
        Width = 33
        Height = 13
        Caption = 'UserID'
      end
      object FPasswordLabel: TLabel
        Left = 16
        Top = 48
        Width = 46
        Height = 13
        Caption = 'Password'
      end
      object EdtUserId: TEdit
        Left = 80
        Top = 16
        Width = 249
        Height = 21
        TabOrder = 0
        OnChange = OnEditChange
      end
      object EdtPassword: TEdit
        Left = 80
        Top = 40
        Width = 249
        Height = 21
        PasswordChar = '*'
        TabOrder = 1
        OnChange = OnEditChange
      end
      object btnValidate: TButton
        Left = 344
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Validate User'
        Default = True
        TabOrder = 2
      end
    end
    object TGroupBox
      Left = 1
      Top = 74
      Width = 515
      Height = 55
      Align = alTop
      TabOrder = 2
      object btnDelete: TSpeedButton
        Left = 96
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Delete'
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
          8888888888888888888888888888888888888888888888888888888888888888
          888888888888888888888800000000000088880BBBBBBBBBB088880BBBBBBBBB
          B088880000000000008888888888888888888888888888888888888888888888
          8888888888888888888888888888888888888888888888888888}
      end
      object btnAdd: TSpeedButton
        Left = 8
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Add'
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
          8888888888888888888888888800008888888888880BB08888888888880BB088
          88888888880BB08888888800000BB0000088880BBBBBBBBBB088880BBBBBBBBB
          B0888800000BB00000888888880BB08888888888880BB08888888888880BB088
          8888888888000088888888888888888888888888888888888888}
      end
      object GroupBox1: TGroupBox
        Left = 176
        Top = 7
        Width = 337
        Height = 38
        TabOrder = 0
        object chkStudy: TCheckBox
          Left = 128
          Top = 16
          Width = 97
          Height = 17
          Caption = 'Auto Study'
          TabOrder = 0
          OnClick = chkStudyClick
          OnKeyPress = DoOnKeypress
        end
        object chkLogOn: TCheckBox
          Left = 8
          Top = 16
          Width = 97
          Height = 17
          Caption = 'Auto Logon'
          TabOrder = 1
          OnClick = chkLogOnClick
          OnKeyPress = DoOnKeypress
        end
      end
    end
    object pnlMessage: TPanel
      Left = 1
      Top = 355
      Width = 515
      Height = 28
      Align = alBottom
      Alignment = taLeftJustify
      AutoSize = True
      BevelInner = bvLowered
      TabOrder = 3
      object btnSave: TSpeedButton
        Left = 360
        Top = 2
        Width = 66
        Height = 24
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
      end
      object btnClose: TSpeedButton
        Left = 440
        Top = 2
        Width = 66
        Height = 24
        Caption = 'Close'
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
        NumGlyphs = 2
        OnClick = btnCloseClick
      end
    end
    object TPanel
      Left = 122
      Top = 129
      Width = 394
      Height = 226
      Align = alClient
      BevelInner = bvLowered
      BevelOuter = bvLowered
      TabOrder = 4
      object FUSerlabel: TLabel
        Left = 4
        Top = 8
        Width = 44
        Height = 13
        Caption = 'User ID'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object FPassLabel: TLabel
        Left = 4
        Top = 32
        Width = 55
        Height = 13
        Caption = 'Password'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object FInitialsLabel: TLabel
        Left = 4
        Top = 56
        Width = 38
        Height = 13
        Caption = 'Initials'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object FFirstNameLabel: TLabel
        Left = 4
        Top = 136
        Width = 61
        Height = 13
        Caption = 'First Name'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object FSecondNameLabel: TLabel
        Left = 4
        Top = 108
        Width = 80
        Height = 13
        Caption = 'Second Name'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object FLastNameLabel: TLabel
        Left = 4
        Top = 84
        Width = 61
        Height = 13
        Caption = 'Last Name'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object FLanguageLabel: TLabel
        Left = 4
        Top = 156
        Width = 57
        Height = 13
        Caption = 'Language'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object FUserRightsLabel: TLabel
        Left = 4
        Top = 180
        Width = 63
        Height = 13
        Caption = 'UserRights'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object FUserTypeLabel: TLabel
        Left = 4
        Top = 204
        Width = 55
        Height = 13
        Caption = 'UserType'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object btnPasswordChange: TSpeedButton
        Left = 240
        Top = 32
        Width = 23
        Height = 22
        Hint = 'Change User Password'
        Glyph.Data = {
          4E010000424D4E01000000000000760000002800000012000000120000000100
          040000000000D8000000120B0000120B00001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          333333000000333333333333333333000000338000000000000033000000330E
          E000000880E033000000330EE000000880E033000000330EE000000F80E03300
          0000330EE000000000E033000000330EEEEEEEEEEEE033000000330EE0000000
          0EE033000000330E0888888880E033000000330E0888888880E033000000330E
          088F88F880E033000000330E0888888880E033000000330E0888888880003300
          0000330E08888888808033000000330000000000000033000000333333333333
          333333000000333333333333333333000000}
        ParentShowHint = False
        ShowHint = True
      end
      object EdtUser: TEdit
        Left = 94
        Top = 8
        Width = 145
        Height = 21
        TabOrder = 0
      end
      object EdtPass: TEdit
        Left = 94
        Top = 32
        Width = 145
        Height = 21
        PasswordChar = '*'
        TabOrder = 1
        OnChange = OnEditChange
      end
      object EdtInitials: TEdit
        Left = 94
        Top = 56
        Width = 145
        Height = 21
        TabOrder = 2
        OnChange = OnEditChange
        OnKeyPress = DoOnKeypress
      end
      object EdtFirstName: TEdit
        Left = 94
        Top = 130
        Width = 145
        Height = 21
        TabOrder = 5
        OnChange = OnEditChange
        OnKeyPress = DoOnKeypress
      end
      object EdtSecondName: TEdit
        Left = 94
        Top = 105
        Width = 145
        Height = 21
        TabOrder = 4
        OnChange = OnEditChange
        OnKeyPress = DoOnKeypress
      end
      object EdtLastName: TEdit
        Left = 94
        Top = 80
        Width = 145
        Height = 21
        TabOrder = 3
        OnChange = OnEditChange
        OnKeyPress = DoOnKeypress
      end
      object CbxLanguage: TComboBox
        Left = 94
        Top = 156
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 6
        Text = 'ENG'
        OnChange = OnEditChange
        OnKeyPress = DoOnKeypress
        Items.Strings = (
          'ENG'
          'AFR')
      end
      object CbxUserRights: TComboBox
        Left = 94
        Top = 180
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 8
        OnChange = OnEditChange
        OnKeyPress = DoOnKeypress
        Items.Strings = (
          '1'
          '2'
          '3'
          '100')
      end
      object CbxUserType: TComboBox
        Left = 94
        Top = 204
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 7
        OnChange = OnEditChange
        OnKeyPress = DoOnKeypress
      end
      object ChkModel: TCheckListBox
        Left = 268
        Top = 2
        Width = 124
        Height = 222
        Align = alRight
        ItemHeight = 13
        TabOrder = 9
        OnClick = ChkModelClick
      end
    end
  end
end
