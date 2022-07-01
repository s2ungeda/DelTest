object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Form3'
  ClientHeight = 631
  ClientWidth = 892
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 632
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 8
    Top = 8
    Width = 489
    Height = 21
    TabOrder = 1
    Text = 'Edit1'
  end
  object Memo1: TMemo
    Left = 8
    Top = 184
    Width = 881
    Height = 439
    TabOrder = 2
    WantReturns = False
    WordWrap = False
  end
  object Button2: TButton
    Left = 528
    Top = 8
    Width = 75
    Height = 25
    Caption = #49884#44036
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 208
    Top = 35
    Width = 75
    Height = 25
    Caption = 'Button3'
    TabOrder = 4
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 658
    Top = 49
    Width = 75
    Height = 25
    Caption = 'binance'
    TabOrder = 5
    OnClick = Button4Click
  end
  object Edit2: TEdit
    Left = 8
    Top = 35
    Width = 121
    Height = 21
    TabOrder = 6
    Text = '1646117654536'
  end
  object Button5: TButton
    Left = 135
    Top = 35
    Width = 75
    Height = 25
    Caption = 'Button5'
    TabOrder = 7
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 496
    Top = 48
    Width = 75
    Height = 25
    Caption = 'uuid'
    TabOrder = 8
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 577
    Top = 49
    Width = 75
    Height = 25
    Caption = 'upbit'
    TabOrder = 9
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 280
    Top = 35
    Width = 75
    Height = 25
    Caption = 'bithumb'
    TabOrder = 10
    OnClick = Button8Click
  end
  object Button9: TButton
    Left = 415
    Top = 49
    Width = 75
    Height = 25
    Caption = 'up '#44228#51340#51312#54924
    TabOrder = 11
    OnClick = Button9Click
  end
  object Button10: TButton
    Left = 415
    Top = 79
    Width = 75
    Height = 25
    Caption = 'up '#51452#47928#44032#45733
    TabOrder = 12
    OnClick = Button10Click
  end
  object Button11: TButton
    Left = 280
    Top = 66
    Width = 75
    Height = 25
    Caption = #54924#50896#51221#48372#51312#54924
    TabOrder = 13
    OnClick = Button11Click
  end
  object Button12: TButton
    Left = 658
    Top = 80
    Width = 75
    Height = 25
    Caption = 'balance'
    TabOrder = 14
    OnClick = Button12Click
  end
  object Button13: TButton
    Left = 658
    Top = 111
    Width = 75
    Height = 25
    Caption = 'position'
    TabOrder = 15
    OnClick = Button13Click
  end
  object Button14: TButton
    Left = 415
    Top = 110
    Width = 82
    Height = 25
    Caption = 'up '#51452#47928#47532#49828#53944
    TabOrder = 16
    OnClick = Button14Click
  end
  object Button15: TButton
    Left = 415
    Top = 141
    Width = 98
    Height = 25
    Caption = 'up '#51452#47928#47532#49828#53944'2'
    TabOrder = 17
    OnClick = Button15Click
  end
  object Button16: TButton
    Left = 496
    Top = 80
    Width = 75
    Height = 25
    Caption = 'up '#52712#49548
    TabOrder = 18
    OnClick = Button16Click
  end
  object Button17: TButton
    Left = 503
    Top = 110
    Width = 68
    Height = 25
    Caption = 'up '#51452#47928
    TabOrder = 19
    OnClick = Button17Click
  end
  object edtUid: TEdit
    Left = 48
    Top = 157
    Width = 201
    Height = 21
    TabOrder = 20
  end
  object Button18: TButton
    Left = 280
    Top = 97
    Width = 75
    Height = 25
    Caption = #51452#47928#51312#54924
    TabOrder = 21
    OnClick = Button18Click
  end
  object Button19: TButton
    Left = 658
    Top = 142
    Width = 75
    Height = 25
    Caption = 'order'
    TabOrder = 22
    OnClick = Button19Click
  end
  object Button20: TButton
    Left = 739
    Top = 142
    Width = 75
    Height = 25
    Caption = #52712#49548
    TabOrder = 23
    OnClick = Button20Click
  end
  object Edit3: TEdit
    Left = 739
    Top = 115
    Width = 121
    Height = 21
    TabOrder = 24
    Text = 'Edit3'
  end
  object Edit4: TEdit
    Left = 24
    Top = 104
    Width = 121
    Height = 21
    TabOrder = 25
    OnKeyPress = Edit4KeyPress
  end
  object Button21: TButton
    Left = 151
    Top = 102
    Width = 75
    Height = 25
    Caption = #49548#49688'->'#51221#49688
    TabOrder = 26
    OnClick = Button21Click
  end
  object Button22: TButton
    Left = 280
    Top = 128
    Width = 75
    Height = 25
    Caption = #51452#47928
    TabOrder = 27
    OnClick = Button22Click
  end
  object Button23: TButton
    Left = 280
    Top = 159
    Width = 75
    Height = 25
    Caption = #52712#49548
    TabOrder = 28
    OnClick = Button23Click
  end
  object Button24: TButton
    Left = 809
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Button24'
    TabOrder = 29
    OnClick = Button24Click
  end
  object restClient: TRESTClient
    BaseURL = 'https://api.bithumb.com'
    Params = <>
    Left = 512
    Top = 280
  end
  object restReq: TRESTRequest
    AssignedValues = [rvAccept]
    Accept = '*/*'
    Client = restClient
    Params = <>
    Response = restRes
    OnAfterExecute = restReqAfterExecute
    OnHTTPProtocolError = restReqHTTPProtocolError
    Left = 392
    Top = 232
  end
  object restRes: TRESTResponse
    ContentType = 'application/json; charset=utf-8'
    Left = 456
    Top = 248
  end
end
