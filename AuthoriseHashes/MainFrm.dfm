object MainForm: TMainForm
  Left = 219
  Top = 139
  Width = 636
  Height = 173
  BorderWidth = 4
  Caption = 'Authorise hashes'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lvHashes: TListView
    Left = 0
    Top = 0
    Width = 620
    Height = 138
    Align = alClient
    Checkboxes = True
    Columns = <
      item
        AutoSize = True
        Caption = 'File hash'
      end>
    GridLines = True
    HideSelection = False
    HotTrack = True
    ReadOnly = True
    RowSelect = True
    ParentShowHint = False
    PopupMenu = pmItems
    ShowHint = True
    TabOrder = 0
    ViewStyle = vsReport
    OnChange = lvHashesChange
  end
  object pmItems: TPopupMenu
    OnPopup = pmItemsPopup
    Left = 8
    Top = 24
    object mnuAdd: TMenuItem
      Caption = 'Add'
      OnClick = mnuAddClick
    end
    object mnuDelete: TMenuItem
      Caption = 'Delete'
      OnClick = mnuDeleteClick
    end
    object mnuSave: TMenuItem
      Caption = 'Save'
      OnClick = mnuSaveClick
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 'Executables (*.exe; *.com)|*.exe;*.com|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 40
    Top = 24
  end
  object XPM: TXPManifest
    Left = 72
    Top = 24
  end
  object OpenHash: TOpenDialog
    DefaultExt = 'shk'
    Filter = 'Hash lists (*.shk)|*.shk'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Open hash list'
    Left = 104
    Top = 24
  end
  object OpenPrivateKey: TOpenDialog
    Filter = 'Private keys (*.sck)|*.sck'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Open private key'
    Left = 136
    Top = 24
  end
end
