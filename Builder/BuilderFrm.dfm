object BuilderForm: TBuilderForm
  Left = 425
  Top = 52
  Width = 511
  Height = 445
  BorderWidth = 4
  Caption = 'Create self extractor'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    495
    410)
  PixelsPerInch = 96
  TextHeight = 13
  object btnBuild: TButton
    Left = 420
    Top = 378
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Build'
    Default = True
    Enabled = False
    TabOrder = 1
    OnClick = btnBuildClick
  end
  object prgProgress: TProgressBar
    Left = 0
    Top = 378
    Width = 415
    Height = 12
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 2
  end
  object prgFiles: TProgressBar
    Left = 0
    Top = 391
    Width = 415
    Height = 12
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 3
  end
  object pcOptions: TPageControl
    Left = 0
    Top = 0
    Width = 495
    Height = 371
    ActivePage = tsProtection
    Anchors = [akLeft, akTop, akRight, akBottom]
    HotTrack = True
    TabOrder = 0
    object tsFiles: TTabSheet
      BorderWidth = 4
      Caption = '&Files'
      DesignSize = (
        479
        335)
      object TRadioGroup
        Left = 152
        Top = 112
        Width = 97
        Height = 33
        Items.Strings = (
          'h')
        TabOrder = 3
        Visible = False
      end
      object memFiles: TMemo
        Left = 0
        Top = 28
        Width = 479
        Height = 307
        Anchors = [akLeft, akTop, akRight, akBottom]
        ScrollBars = ssBoth
        TabOrder = 2
        WordWrap = False
        OnChange = edtExecutableChange
        OnKeyDown = memFilesKeyDown
      end
      object edtRootDirectory: TLabeledEdit
        Left = 32
        Top = 2
        Width = 367
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 27
        EditLabel.Height = 13
        EditLabel.Caption = '&Root:'
        LabelPosition = lpLeft
        TabOrder = 0
        OnChange = edtExecutableChange
      end
      object btnRecurse: TButton
        Left = 404
        Top = 0
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'R&ecurse. . .'
        TabOrder = 1
        OnClick = btnRecurseClick
      end
    end
    object tsOptions: TTabSheet
      BorderWidth = 4
      Caption = '&Options'
      ImageIndex = 2
      DesignSize = (
        479
        335)
      object lblCompression: TLabel
        Left = 0
        Top = 136
        Width = 90
        Height = 13
        Caption = 'Compression le&vel:'
        FocusControl = tbCompressionLevel
      end
      object edtCaption: TLabeledEdit
        Left = 0
        Top = 16
        Width = 479
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 41
        EditLabel.Height = 13
        EditLabel.Caption = '&Caption:'
        MaxLength = 255
        TabOrder = 0
        OnChange = edtExecutableChange
      end
      object edtExtractHere: TLabeledEdit
        Left = 0
        Top = 56
        Width = 479
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 189
        EditLabel.Height = 13
        EditLabel.Caption = 'E&xtract here if temporary directory full:'
        MaxLength = 255
        TabOrder = 1
        OnChange = edtExecutableChange
      end
      object tbCompressionLevel: TTrackBar
        Left = 0
        Top = 151
        Width = 479
        Height = 31
        Anchors = [akLeft, akTop, akRight]
        Max = 9
        Min = 1
        Position = 9
        TabOrder = 3
      end
      object rgCompressionType: TRadioGroup
        Left = 0
        Top = 84
        Width = 479
        Height = 49
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Co&mpression type'
        Columns = 2
        ItemIndex = 2
        Items.Strings = (
          '&None'
          '&BZ2'
          '&LZMA')
        TabOrder = 2
        OnClick = rgCompressionTypeClick
      end
    end
    object tsShell: TTabSheet
      BorderWidth = 4
      Caption = '&Shell'
      ImageIndex = 1
      DesignSize = (
        479
        335)
      object TRadioGroup
        Left = 136
        Top = 60
        Width = 41
        Height = 15
        Items.Strings = (
          'h')
        TabOrder = 2
        Visible = False
      end
      object edtExecutable: TLabeledEdit
        Left = 0
        Top = 16
        Width = 479
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 89
        EditLabel.Height = 13
        EditLabel.Caption = '&Executable to run:'
        MaxLength = 255
        TabOrder = 0
        OnChange = edtExecutableChange
      end
      object edtParams: TLabeledEdit
        Left = 0
        Top = 56
        Width = 479
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 59
        EditLabel.Height = 13
        EditLabel.Caption = 'Pa&ramaters:'
        MaxLength = 255
        TabOrder = 1
        OnChange = edtExecutableChange
      end
    end
    object tsProtection: TTabSheet
      BorderWidth = 4
      Caption = '&Protection'
      ImageIndex = 3
      DesignSize = (
        479
        335)
      object edtUNC: TLabeledEdit
        Left = 0
        Top = 16
        Width = 479
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 215
        EditLabel.Height = 13
        EditLabel.Caption = 'P&ermit execution from this path and file only:'
        MaxLength = 255
        TabOrder = 0
        OnChange = edtExecutableChange
      end
      object edtPassword: TLabeledEdit
        Left = 0
        Top = 56
        Width = 479
        Height = 20
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 50
        EditLabel.Height = 13
        EditLabel.Caption = 'Pass&word:'
        EditLabel.Transparent = True
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Wingdings'
        Font.Style = []
        ParentFont = False
        PasswordChar = 'l'
        TabOrder = 1
        OnChange = edtExecutableChange
      end
      object rgCipher: TRadioGroup
        Left = 0
        Top = 84
        Width = 479
        Height = 49
        Anchors = [akLeft, akTop, akRight]
        Caption = '&Cipher'
        Columns = 2
        Enabled = False
        ItemIndex = 1
        Items.Strings = (
          '&Blowfish'
          '&Twofish'
          'AES (R&ijndael)')
        TabOrder = 2
      end
      object edtHashProtection: TLabeledEdit
        Left = 0
        Top = 152
        Width = 479
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 105
        EditLabel.Height = 13
        EditLabel.Caption = '&Hash protection path:'
        EditLabel.Transparent = True
        TabOrder = 3
        OnChange = edtExecutableChange
      end
      object btnLoadKey: TButton
        Left = 0
        Top = 176
        Width = 479
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Lo&ad public key from file. . .'
        TabOrder = 4
        OnClick = btnLoadKeyClick
      end
      object edtP: TEdit
        Left = 0
        Top = 208
        Width = 479
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 5
        Text = 'P'
        Visible = False
      end
      object edtQ: TEdit
        Left = 0
        Top = 232
        Width = 479
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 6
        Text = 'Q'
        Visible = False
      end
      object edtG: TEdit
        Left = 0
        Top = 256
        Width = 479
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 7
        Text = 'G'
        Visible = False
      end
      object edtY: TEdit
        Left = 0
        Top = 280
        Width = 479
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 8
        Text = 'Y'
        Visible = False
      end
    end
    object tsLoadSave: TTabSheet
      BorderWidth = 4
      Caption = '&Load/save settings'
      ImageIndex = 4
      DesignSize = (
        479
        335)
      object TRadioGroup
        Left = 136
        Top = 60
        Width = 33
        Height = 17
        Items.Strings = (
          'h')
        TabOrder = 2
        Visible = False
      end
      object btnLoadOptions: TButton
        Left = 0
        Top = 16
        Width = 479
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Lo&ad settings from file. . .'
        TabOrder = 0
        OnClick = btnLoadOptionsClick
      end
      object btnSaveOptions: TButton
        Left = 0
        Top = 56
        Width = 479
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Sa&ve settings to file (saves password!). . .'
        TabOrder = 1
        OnClick = btnSaveOptionsClick
      end
    end
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'exe'
    Filter = 'Executables (*.exe)|*.exe|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 424
    Top = 216
  end
  object XP: TXPManifest
    Left = 192
    Top = 128
  end
  object OpenSettings: TOpenDialog
    Filter = 'Self extractor settings (*.ses)|*.ses|All files (*.*)'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 300
    Top = 64
  end
  object SaveSettings: TSaveDialog
    DefaultExt = 'ses'
    Filter = 'Self extractor settings (*.ses)|*.ses|All files (*.*)'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 332
    Top = 64
  end
  object OpenKeyDialog: TOpenDialog
    Filter = 'Self extractor keys (*.sak)|*.sak|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 460
    Top = 216
  end
end
