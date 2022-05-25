object Form10: TForm10
  Left = 0
  Top = 0
  Caption = 'Form10'
  ClientHeight = 786
  ClientWidth = 518
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = #49373#49457
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 112
    Top = 8
    Width = 75
    Height = 25
    Caption = #51333#47308
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 240
    Top = 101
    Width = 75
    Height = 25
    Caption = 'Button3'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Edit1: TEdit
    Left = 32
    Top = 79
    Width = 161
    Height = 21
    TabOrder = 3
    Text = 'https://fapi.binance.com'
  end
  object Edit2: TEdit
    Left = 32
    Top = 106
    Width = 194
    Height = 21
    TabOrder = 4
    Text = '/fapi/v1/ticker/bookTicker'
  end
  object Edit3: TEdit
    Left = 32
    Top = 133
    Width = 194
    Height = 21
    TabOrder = 5
    Text = '/fapi/v1/ticker/24hr'
  end
  object Button4: TButton
    Tag = 1
    Left = 240
    Top = 132
    Width = 75
    Height = 25
    Caption = 'Button4'
    TabOrder = 6
    OnClick = Button3Click
  end
  object Button5: TButton
    Left = 240
    Top = 70
    Width = 75
    Height = 25
    Caption = 'Button5'
    TabOrder = 7
    OnClick = Button5Click
  end
  object CheckBox1: TCheckBox
    Left = 256
    Top = 32
    Width = 97
    Height = 17
    Caption = 'CheckBox1'
    TabOrder = 8
    OnClick = CheckBox1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 184
    Width = 502
    Height = 594
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssBoth
    TabOrder = 9
  end
  object RESTClient1: TRESTClient
    Params = <>
    Left = 376
    Top = 112
  end
  object req: TRESTRequest
    Client = RESTClient1
    Params = <>
    Response = RESTResponse1
    Left = 360
    Top = 72
  end
  object RESTResponse1: TRESTResponse
    Left = 440
    Top = 80
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 500
    OnTimer = Timer1Timer
    Left = 320
    Top = 24
  end
end
