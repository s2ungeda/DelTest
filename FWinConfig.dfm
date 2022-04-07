object FrmWinConfig: TFrmWinConfig
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #54868#47732#49444#51221
  ClientHeight = 160
  ClientWidth = 425
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 193
    Height = 81
    Caption = #54868#47732' '#49444#51221
    TabOrder = 0
    object Label1: TLabel
      Left = 10
      Top = 24
      Width = 22
      Height = 13
      Caption = #54256#53944
    end
    object Label2: TLabel
      Left = 10
      Top = 51
      Width = 22
      Height = 13
      Caption = #53356#44592
    end
    object cbFont: TComboBox
      Left = 38
      Top = 19
      Width = 144
      Height = 21
      Style = csDropDownList
      TabOrder = 0
    end
    object edtSize: TEdit
      Left = 38
      Top = 46
      Width = 35
      Height = 21
      Alignment = taRightJustify
      NumbersOnly = True
      TabOrder = 1
      Text = '10'
    end
  end
  object ButtonCancel: TButton
    Left = 333
    Top = 128
    Width = 80
    Height = 24
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object ButtonOK: TButton
    Left = 234
    Top = 128
    Width = 80
    Height = 24
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
end
