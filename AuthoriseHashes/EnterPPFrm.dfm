object EnterPPForm: TEnterPPForm
  Left = 193
  Top = 108
  AutoSize = True
  BorderIcons = []
  BorderStyle = bsSingle
  BorderWidth = 4
  Caption = 'Enter private key passphrase'
  ClientHeight = 25
  ClientWidth = 467
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object edtPP: TEdit
    Left = 0
    Top = 2
    Width = 382
    Height = 21
    PasswordChar = '*'
    TabOrder = 0
  end
  object Button1: TButton
    Left = 392
    Top = 0
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
end
