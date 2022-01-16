object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 465
  ClientWidth = 675
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
  object Memo1: TMemo
    Left = 8
    Top = 32
    Width = 659
    Height = 425
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object btnMaster: TButton
    Left = 8
    Top = 3
    Width = 75
    Height = 25
    Caption = #48148#51060#45240#49828
    TabOrder = 1
    OnClick = btnMasterClick
  end
  object Button2: TButton
    Left = 89
    Top = 3
    Width = 75
    Height = 25
    Caption = #48727#50040
    TabOrder = 2
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 280
    Top = 5
    Width = 209
    Height = 21
    TabOrder = 3
    Text = 'https://testnet.binance.vision'
  end
  object Button1: TButton
    Left = 170
    Top = 3
    Width = 75
    Height = 25
    Caption = #50629#48708#53944
    TabOrder = 4
    OnClick = Button1Click
  end
  object Button3: TButton
    Left = 504
    Top = 1
    Width = 75
    Height = 25
    Caption = 'Button3'
    TabOrder = 5
    OnClick = Button3Click
  end
  object conn: TRESTClient
    Params = <>
    Left = 536
    Top = 264
  end
  object req: TRESTRequest
    Client = conn
    Params = <>
    Response = res
    Left = 504
    Top = 264
  end
  object res: TRESTResponse
    Left = 464
    Top = 264
  end
end
