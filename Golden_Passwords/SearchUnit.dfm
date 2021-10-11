object SearchForm: TSearchForm
  Left = 198
  Top = 114
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Rechercher un mot de passe'
  ClientHeight = 156
  ClientWidth = 345
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object InfoLbl: TLabel
    Left = 8
    Top = 20
    Width = 127
    Height = 13
    Caption = 'Saisissez votre recherche :'
  end
  object CloseBtn: TButton
    Left = 262
    Top = 6
    Width = 75
    Height = 25
    Caption = 'Fermer'
    TabOrder = 0
    OnClick = CloseBtnClick
  end
  object GoToBtn: TButton
    Left = 176
    Top = 6
    Width = 81
    Height = 25
    Caption = 'Aller '#224' ...'
    Enabled = False
    TabOrder = 1
    OnClick = GoToBtnClick
  end
  object ResultBox: TListBox
    Left = 8
    Top = 60
    Width = 329
    Height = 89
    BevelKind = bkSoft
    BorderStyle = bsNone
    ItemHeight = 13
    TabOrder = 2
    OnClick = ResultBoxClick
  end
  object SearchEdit: TEdit
    Left = 8
    Top = 36
    Width = 329
    Height = 21
    BevelKind = bkSoft
    BorderStyle = bsNone
    ParentShowHint = False
    ShowHint = False
    TabOrder = 3
    OnChange = SearchEditChange
  end
end
