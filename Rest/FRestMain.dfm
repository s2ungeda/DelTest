object FrmRestMain: TFrmRestMain
  Left = 0
  Top = 0
  Caption = 'REST'
  ClientHeight = 150
  ClientWidth = 173
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
    Left = 8
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
    Left = 392
    Top = 48
  end
  object UpbReq: TRESTRequest
    Client = RESTClient2
    Params = <>
    Response = RESTResponse2
    Left = 392
    Top = 104
  end
  object RESTResponse2: TRESTResponse
    Left = 392
    Top = 152
  end
  object RESTClient3: TRESTClient
    Params = <>
    Left = 688
    Top = 48
  end
  object BitReq: TRESTRequest
    Client = RESTClient3
    Params = <>
    Response = RESTResponse3
    Left = 688
    Top = 88
  end
  object RESTResponse3: TRESTResponse
    Left = 688
    Top = 144
  end
end
