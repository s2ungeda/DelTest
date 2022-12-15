object FrmRestMain: TFrmRestMain
  Left = 0
  Top = 0
  Caption = 'REST'
  ClientHeight = 237
  ClientWidth = 808
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
  object m: TMemo
    Left = -136
    Top = 36
    Width = 784
    Height = 185
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object RESTClient1: TRESTClient
    Params = <>
    Left = 184
    Top = 48
  end
  object BinReq: TRESTRequest
    Client = RESTClient1
    Params = <>
    Response = RESTResponse1
    Left = 184
    Top = 96
  end
  object RESTResponse1: TRESTResponse
    Left = 184
    Top = 152
  end
  object RESTClient2: TRESTClient
    Params = <>
    Left = 248
    Top = 48
  end
  object UpbReq: TRESTRequest
    Client = RESTClient2
    Params = <>
    Response = RESTResponse2
    Left = 248
    Top = 104
  end
  object RESTResponse2: TRESTResponse
    Left = 248
    Top = 152
  end
  object RESTClient3: TRESTClient
    Params = <>
    Left = 312
    Top = 56
  end
  object BitReq: TRESTRequest
    Client = RESTClient3
    Params = <>
    Response = RESTResponse3
    Left = 312
    Top = 96
  end
  object RESTResponse3: TRESTResponse
    Left = 312
    Top = 152
  end
  object RESTClient4: TRESTClient
    Params = <>
    Left = 376
    Top = 56
  end
  object BinSpotReq: TRESTRequest
    Client = RESTClient4
    Params = <>
    Response = RESTResponse4
    Left = 376
    Top = 96
  end
  object RESTResponse4: TRESTResponse
    Left = 376
    Top = 152
  end
end
