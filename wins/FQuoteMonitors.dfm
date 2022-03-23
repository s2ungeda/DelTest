object FrmQuoteMonitors: TFrmQuoteMonitors
  Left = 0
  Top = 0
  Caption = #49884#49464#47784#45768#53552#47553
  ClientHeight = 481
  ClientWidth = 870
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
  object StatusBar1: TStatusBar
    Left = 0
    Top = 462
    Width = 870
    Height = 19
    Panels = <>
  end
  object plLeft: TPanel
    Left = 0
    Top = 0
    Width = 870
    Height = 462
    Align = alClient
    BevelOuter = bvNone
    Caption = 'plLeft'
    TabOrder = 1
    object plLeftTop: TPanel
      Left = 0
      Top = 0
      Width = 870
      Height = 51
      Align = alTop
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 0
      DesignSize = (
        870
        51)
      object cbOSEx: TComboBox
        Left = 80
        Top = 26
        Width = 79
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 0
        Text = 'Binance'
        Items.Strings = (
          'Binance')
      end
      object cbKREx1: TComboBox
        Left = 165
        Top = 26
        Width = 79
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 1
        Text = 'Upbit'
        Items.Strings = (
          'Upbit'
          'Bithumb')
      end
      object cbKREx2: TComboBox
        Left = 246
        Top = 26
        Width = 79
        Height = 21
        Style = csDropDownList
        ItemIndex = 1
        TabOrder = 2
        Text = 'Bithumb'
        Items.Strings = (
          'Upbit'
          'Bithumb')
      end
      object plExRate: TPanel
        Left = 6
        Top = 27
        Width = 70
        Height = 21
        Alignment = taRightJustify
        BevelKind = bkSoft
        BevelOuter = bvNone
        Caption = '1111'
        ParentBackground = False
        TabOrder = 3
      end
      object Panel2: TPanel
        Left = 6
        Top = 4
        Width = 70
        Height = 21
        BevelInner = bvRaised
        BevelOuter = bvNone
        Caption = 'USD'#54872#50984
        Color = clPurple
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentBackground = False
        ParentFont = False
        TabOrder = 4
      end
      object Panel1: TPanel
        Left = 80
        Top = 4
        Width = 79
        Height = 21
        Alignment = taLeftJustify
        BevelInner = bvRaised
        BevelOuter = bvNone
        Caption = #54644#50808
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentBackground = False
        ParentFont = False
        TabOrder = 5
      end
      object Panel3: TPanel
        Left = 165
        Top = 4
        Width = 160
        Height = 21
        Alignment = taLeftJustify
        BevelInner = bvRaised
        BevelOuter = bvNone
        Caption = #44397#45236
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentBackground = False
        ParentFont = False
        TabOrder = 6
      end
      object Panel4: TPanel
        Left = 681
        Top = 2
        Width = 177
        Height = 47
        Anchors = [akRight, akBottom]
        BevelOuter = bvNone
        TabOrder = 7
        object Label1: TLabel
          Left = 119
          Top = 28
          Width = 45
          Height = 13
          Caption = '('#45800#50948':'#50613')'
        end
        object Label2: TLabel
          Left = 77
          Top = 6
          Width = 80
          Height = 13
          Caption = #52572#49548' '#51068#44144#47000#45824#44552
        end
        object cbUB: TCheckBox
          Left = 32
          Top = 3
          Width = 36
          Height = 17
          Caption = 'UB'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
        object cbBT: TCheckBox
          Left = 32
          Top = 26
          Width = 36
          Height = 17
          Caption = 'BT'
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
        object edtAmt: TEdit
          Left = 66
          Top = 24
          Width = 50
          Height = 21
          Alignment = taRightJustify
          TabOrder = 2
          Text = '50'
        end
      end
      object Button1: TButton
        Left = 331
        Top = 26
        Width = 75
        Height = 21
        Anchors = [akRight, akBottom]
        Caption = 'refresh'
        TabOrder = 8
        OnClick = Button1Click
      end
    end
    object plLeftClient: TPanel
      Left = 0
      Top = 51
      Width = 870
      Height = 411
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object sgQuote: TStringGrid
        Left = 0
        Top = 0
        Width = 870
        Height = 411
        Align = alClient
        ColCount = 12
        DefaultRowHeight = 19
        DefaultDrawing = False
        FixedCols = 0
        RowCount = 25
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goFixedRowDefAlign]
        TabOrder = 0
        OnDrawCell = sgQuoteDrawCell
      end
    end
  end
  object Timer1: TTimer
    Interval = 3000
    OnTimer = Timer1Timer
    Left = 344
    Top = 195
  end
end
