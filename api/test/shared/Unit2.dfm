object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 164
  ClientWidth = 380
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Edit1: TEdit
    Left = 8
    Top = 8
    Width = 177
    Height = 21
    TabOrder = 0
    Text = 'Edit1'
  end
  object Button1: TButton
    Left = 191
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 1
    OnClick = Button1Click
  end
  object CheckBox1: TCheckBox
    Left = 40
    Top = 56
    Width = 97
    Height = 17
    Caption = 'CheckBox1'
    TabOrder = 2
    OnClick = CheckBox1Click
  end
  object Button2: TButton
    Left = 272
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 104
    Top = 88
  end
  object RESTClient1: TRESTClient
    BaseURL = 'https://api.binance.com'
    Params = <>
    Left = 312
    Top = 80
  end
  object req: TRESTRequest
    Client = RESTClient1
    Params = <>
    Response = res
    Left = 264
    Top = 72
  end
  object res: TRESTResponse
    Left = 224
    Top = 72
  end
end
