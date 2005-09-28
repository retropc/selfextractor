object MainForm: TMainForm
  Left = 124
  Top = 188
  AutoSize = True
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  BorderWidth = 4
  Caption = 'Key generator'
  ClientHeight = 49
  ClientWidth = 518
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object btnGenSave: TButton
    Left = 443
    Top = 24
    Width = 75
    Height = 25
    Caption = '&Gen && save'
    Default = True
    TabOrder = 1
    OnClick = btnGenSaveClick
  end
  object edtPP: TLabeledEdit
    Left = 62
    Top = 0
    Width = 456
    Height = 20
    EditLabel.Width = 59
    EditLabel.Height = 13
    EditLabel.Caption = '&Passphrase:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Wingdings'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    PasswordChar = 'l'
    TabOrder = 0
  end
  object SavePubDialog: TSaveDialog
    FileName = 'pubkey.sak'
    Filter = 'Self extractor ascii keys (*.sak)|*.sak|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Save public key'
    Left = 288
  end
  object SavePrivDialog: TSaveDialog
    FileName = 'privkey.sck'
    Filter = 'Self extractor crypted keys (*.sck)|*.sck|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Save private key'
    Left = 320
  end
  object XPM: TXPManifest
    Left = 352
  end
end
