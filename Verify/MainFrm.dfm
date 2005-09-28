object MainForm: TMainForm
  Left = 192
  Top = 114
  Width = 484
  Height = 84
  AutoSize = True
  BorderIcons = [biSystemMenu, biMinimize]
  BorderWidth = 4
  Caption = 'Verify signature'
  Color = clBtnFace
  Constraints.MinWidth = 420
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCanResize = FormCanResize
  OnCreate = FormCreate
  DesignSize = (
    468
    49)
  PixelsPerInch = 96
  TextHeight = 13
  object lblLocation: TLabel
    Left = 0
    Top = 3
    Width = 44
    Height = 13
    Caption = '&Location:'
    FocusControl = cbxLocation
  end
  object btnShortcut: TButton
    Left = 313
    Top = 24
    Width = 75
    Height = 25
    Hint = 'Create a shortcut to the entered program on the desktop.'
    Anchors = [akTop, akRight]
    Caption = '&Shortcut'
    Enabled = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = btnShortcutClick
  end
  object btnVerifyRun: TButton
    Left = 393
    Top = 24
    Width = 75
    Height = 25
    Hint = 'Verify and run the entered program.'
    Anchors = [akTop, akRight]
    Caption = '&Verify && run'
    Default = True
    Enabled = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnClick = btnVerifyRunClick
  end
  object btnTyr: TButton
    Left = 56
    Top = 24
    Width = 49
    Height = 25
    Caption = 'tyr'
    TabOrder = 3
    OnClick = btnTyrClick
  end
  object btnHTTP: TButton
    Left = 112
    Top = 24
    Width = 49
    Height = 25
    Caption = 'http://'
    TabOrder = 4
    OnClick = btnHTTPClick
  end
  object cbxLocation: TComboBox
    Left = 56
    Top = 0
    Width = 346
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 0
    OnChange = cbxLocationChange
  end
  object btnDelete: TButton
    Left = 409
    Top = 0
    Width = 27
    Height = 21
    Hint = 'Delete the selected item from the list.'
    Anchors = [akTop, akRight]
    Caption = '-'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
    TabStop = False
    OnClick = btnDeleteClick
  end
  object btnBrowse: TButton
    Left = 441
    Top = 0
    Width = 27
    Height = 21
    Hint = 'Browse for a program.'
    Anchors = [akTop, akRight]
    Caption = '...'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
    TabStop = False
    OnClick = btnBrowseClick
  end
  object XPM: TXPManifest
    Left = 168
    Top = 24
  end
  object OpenDialog: TOpenDialog
    Filter = 'Executables (*.exe)|*.exe|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 280
    Top = 24
  end
end
