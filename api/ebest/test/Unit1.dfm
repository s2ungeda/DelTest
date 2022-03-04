object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 336
  ClientWidth = 569
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object xas: TXASession
    Left = 416
    Top = 168
    Width = 0
    Height = 0
    OnLogin = xasLogin
    OnLogout = xasLogout
    OnDisconnect = xasDisconnect
    ControlData = {000A0000D8130000D8130000}
  end
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 441
    Height = 320
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Button1: TButton
    Left = 464
    Top = 8
    Width = 75
    Height = 25
    Caption = #51217#49549
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 464
    Top = 303
    Width = 75
    Height = 25
    Caption = #51217#49549#51333#47308
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 464
    Top = 70
    Width = 75
    Height = 25
    Caption = #47196#44536#51064
    TabOrder = 4
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 464
    Top = 101
    Width = 75
    Height = 25
    Caption = #51312#54924
    TabOrder = 5
    OnClick = Button4Click
  end
  object xaq: TXAQuery
    Left = 504
    Top = 232
    Width = 0
    Height = 0
    OnReceiveData = xaqReceiveData
    OnReceiveMessage = xaqReceiveMessage
    OnReceiveSearchRealData = xaqReceiveSearchRealData
    ControlData = {
      000A0000D8130000D813000008001C0000005200650073005C00740033003500
      310038002E007200650073000000}
  end
  object xar: TXAReal
    Left = 512
    Top = 248
    Width = 0
    Height = 0
    OnReceiveRealData = xarReceiveRealData
    OnRecieveLinkData = xarRecieveLinkData
    ControlData = {
      000A0000D8130000D813000008001C0000005200650073005C00740033003500
      310038002E007200650073000000}
  end
end
