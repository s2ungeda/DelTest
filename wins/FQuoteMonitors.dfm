object FrmQuoteMonitors: TFrmQuoteMonitors
  Left = 0
  Top = 0
  Caption = #49884#49464#47784#45768#53552#47553
  ClientHeight = 481
  ClientWidth = 775
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
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 462
    Width = 775
    Height = 19
    Panels = <>
    ExplicitWidth = 637
  end
  object plLeft: TPanel
    Left = 0
    Top = 0
    Width = 775
    Height = 462
    Align = alClient
    BevelOuter = bvNone
    Caption = 'plLeft'
    TabOrder = 1
    ExplicitWidth = 637
    object plLeftTop: TPanel
      Left = 0
      Top = 0
      Width = 775
      Height = 51
      Align = alTop
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 0
      ExplicitWidth = 637
      DesignSize = (
        775
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
        Left = 167
        Top = 26
        Width = 79
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 1
        Text = 'Upbit'
        OnChange = cbKREx1Change
        Items.Strings = (
          'Upbit'
          'Bithumb')
      end
      object cbKREx2: TComboBox
        Tag = 1
        Left = 246
        Top = 24
        Width = 79
        Height = 21
        Style = csDropDownList
        ItemIndex = 1
        TabOrder = 2
        Text = 'Bithumb'
        OnChange = cbKREx1Change
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
        Left = 586
        Top = 2
        Width = 177
        Height = 47
        Anchors = [akRight, akBottom]
        BevelOuter = bvNone
        TabOrder = 7
        ExplicitLeft = 448
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
          OnClick = cbUBClick
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
          OnClick = cbUBClick
        end
        object edtAmt: TEdit
          Left = 66
          Top = 24
          Width = 50
          Height = 21
          Alignment = taRightJustify
          NumbersOnly = True
          TabOrder = 2
          Text = '50'
          OnKeyDown = edtAmtKeyDown
        end
      end
      object Button1: TButton
        Left = 331
        Top = 26
        Width = 75
        Height = 21
        Caption = 'refresh'
        TabOrder = 8
        OnClick = Button1Click
      end
      object edtSec: TLabeledEdit
        Left = 380
        Top = 2
        Width = 25
        Height = 21
        Alignment = taRightJustify
        EditLabel.Width = 54
        EditLabel.Height = 13
        EditLabel.Caption = '('#45800#50948' : '#52488' )'
        LabelPosition = lpRight
        NumbersOnly = True
        TabOrder = 9
        Text = '3'
        OnKeyDown = edtSecKeyDown
      end
      object cbAuto: TCheckBox
        Left = 333
        Top = 6
        Width = 41
        Height = 17
        Caption = 'Auto'
        Checked = True
        State = cbChecked
        TabOrder = 10
        OnClick = cbAutoClick
      end
    end
    object plLeftClient: TPanel
      Left = 0
      Top = 51
      Width = 775
      Height = 411
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitWidth = 637
      object sgQuote: TStringGrid
        Left = 0
        Top = 0
        Width = 775
        Height = 411
        Align = alClient
        ColCount = 14
        DefaultRowHeight = 19
        DefaultDrawing = False
        FixedCols = 0
        RowCount = 25
        PopupMenu = PopupMenu1
        ScrollBars = ssVertical
        TabOrder = 0
        OnDrawCell = sgQuoteDrawCell
        OnMouseDown = sgQuoteMouseDown
        OnMouseWheelDown = sgQuoteMouseWheelDown
        OnMouseWheelUp = sgQuoteMouseWheelUp
        ExplicitWidth = 637
      end
      object SpinButton1: TSpinButton
        Left = 45
        Top = 2
        Width = 20
        Height = 22
        DownGlyph.Data = {
          0E010000424D0E01000000000000360000002800000009000000060000000100
          200000000000D800000000000000000000000000000000000000008080000080
          8000008080000080800000808000008080000080800000808000008080000080
          8000008080000080800000808000000000000080800000808000008080000080
          8000008080000080800000808000000000000000000000000000008080000080
          8000008080000080800000808000000000000000000000000000000000000000
          0000008080000080800000808000000000000000000000000000000000000000
          0000000000000000000000808000008080000080800000808000008080000080
          800000808000008080000080800000808000}
        TabOrder = 1
        UpGlyph.Data = {
          0E010000424D0E01000000000000360000002800000009000000060000000100
          200000000000D800000000000000000000000000000000000000008080000080
          8000008080000080800000808000008080000080800000808000008080000080
          8000000000000000000000000000000000000000000000000000000000000080
          8000008080000080800000000000000000000000000000000000000000000080
          8000008080000080800000808000008080000000000000000000000000000080
          8000008080000080800000808000008080000080800000808000000000000080
          8000008080000080800000808000008080000080800000808000008080000080
          800000808000008080000080800000808000}
        OnDownClick = SpinButton1DownClick
        OnUpClick = SpinButton1UpClick
      end
      object SpinButton2: TSpinButton
        Tag = 1
        Left = 141
        Top = 2
        Width = 20
        Height = 22
        DownGlyph.Data = {
          0E010000424D0E01000000000000360000002800000009000000060000000100
          200000000000D800000000000000000000000000000000000000008080000080
          8000008080000080800000808000008080000080800000808000008080000080
          8000008080000080800000808000000000000080800000808000008080000080
          8000008080000080800000808000000000000000000000000000008080000080
          8000008080000080800000808000000000000000000000000000000000000000
          0000008080000080800000808000000000000000000000000000000000000000
          0000000000000000000000808000008080000080800000808000008080000080
          800000808000008080000080800000808000}
        TabOrder = 2
        UpGlyph.Data = {
          0E010000424D0E01000000000000360000002800000009000000060000000100
          200000000000D800000000000000000000000000000000000000008080000080
          8000008080000080800000808000008080000080800000808000008080000080
          8000000000000000000000000000000000000000000000000000000000000080
          8000008080000080800000000000000000000000000000000000000000000080
          8000008080000080800000808000008080000000000000000000000000000080
          8000008080000080800000808000008080000080800000808000000000000080
          8000008080000080800000808000008080000080800000808000008080000080
          800000808000008080000080800000808000}
        OnDownClick = SpinButton1DownClick
        OnUpClick = SpinButton1UpClick
      end
      object SpinButton3: TSpinButton
        Tag = 2
        Left = 149
        Top = 2
        Width = 20
        Height = 22
        DownGlyph.Data = {
          0E010000424D0E01000000000000360000002800000009000000060000000100
          200000000000D800000000000000000000000000000000000000008080000080
          8000008080000080800000808000008080000080800000808000008080000080
          8000008080000080800000808000000000000080800000808000008080000080
          8000008080000080800000808000000000000000000000000000008080000080
          8000008080000080800000808000000000000000000000000000000000000000
          0000008080000080800000808000000000000000000000000000000000000000
          0000000000000000000000808000008080000080800000808000008080000080
          800000808000008080000080800000808000}
        TabOrder = 3
        UpGlyph.Data = {
          0E010000424D0E01000000000000360000002800000009000000060000000100
          200000000000D800000000000000000000000000000000000000008080000080
          8000008080000080800000808000008080000080800000808000008080000080
          8000000000000000000000000000000000000000000000000000000000000080
          8000008080000080800000000000000000000000000000000000000000000080
          8000008080000080800000808000008080000000000000000000000000000080
          8000008080000080800000808000008080000080800000808000000000000080
          8000008080000080800000808000008080000080800000808000008080000080
          800000808000008080000080800000808000}
        OnDownClick = SpinButton1DownClick
        OnUpClick = SpinButton1UpClick
      end
      object SpinButton4: TSpinButton
        Tag = 3
        Left = 157
        Top = 2
        Width = 20
        Height = 22
        DownGlyph.Data = {
          0E010000424D0E01000000000000360000002800000009000000060000000100
          200000000000D800000000000000000000000000000000000000008080000080
          8000008080000080800000808000008080000080800000808000008080000080
          8000008080000080800000808000000000000080800000808000008080000080
          8000008080000080800000808000000000000000000000000000008080000080
          8000008080000080800000808000000000000000000000000000000000000000
          0000008080000080800000808000000000000000000000000000000000000000
          0000000000000000000000808000008080000080800000808000008080000080
          800000808000008080000080800000808000}
        TabOrder = 4
        UpGlyph.Data = {
          0E010000424D0E01000000000000360000002800000009000000060000000100
          200000000000D800000000000000000000000000000000000000008080000080
          8000008080000080800000808000008080000080800000808000008080000080
          8000000000000000000000000000000000000000000000000000000000000080
          8000008080000080800000000000000000000000000000000000000000000080
          8000008080000080800000808000008080000000000000000000000000000080
          8000008080000080800000808000008080000080800000808000000000000080
          8000008080000080800000808000008080000080800000808000008080000080
          800000808000008080000080800000808000}
        OnDownClick = SpinButton1DownClick
        OnUpClick = SpinButton1UpClick
      end
      object SpinButton6: TSpinButton
        Tag = 5
        Left = 173
        Top = 2
        Width = 20
        Height = 22
        DownGlyph.Data = {
          0E010000424D0E01000000000000360000002800000009000000060000000100
          200000000000D800000000000000000000000000000000000000008080000080
          8000008080000080800000808000008080000080800000808000008080000080
          8000008080000080800000808000000000000080800000808000008080000080
          8000008080000080800000808000000000000000000000000000008080000080
          8000008080000080800000808000000000000000000000000000000000000000
          0000008080000080800000808000000000000000000000000000000000000000
          0000000000000000000000808000008080000080800000808000008080000080
          800000808000008080000080800000808000}
        TabOrder = 5
        UpGlyph.Data = {
          0E010000424D0E01000000000000360000002800000009000000060000000100
          200000000000D800000000000000000000000000000000000000008080000080
          8000008080000080800000808000008080000080800000808000008080000080
          8000000000000000000000000000000000000000000000000000000000000080
          8000008080000080800000000000000000000000000000000000000000000080
          8000008080000080800000808000008080000000000000000000000000000080
          8000008080000080800000808000008080000080800000808000000000000080
          8000008080000080800000808000008080000080800000808000008080000080
          800000808000008080000080800000808000}
        OnDownClick = SpinButton1DownClick
        OnUpClick = SpinButton1UpClick
      end
      object SpinButton7: TSpinButton
        Tag = 6
        Left = 181
        Top = 2
        Width = 20
        Height = 22
        DownGlyph.Data = {
          0E010000424D0E01000000000000360000002800000009000000060000000100
          200000000000D800000000000000000000000000000000000000008080000080
          8000008080000080800000808000008080000080800000808000008080000080
          8000008080000080800000808000000000000080800000808000008080000080
          8000008080000080800000808000000000000000000000000000008080000080
          8000008080000080800000808000000000000000000000000000000000000000
          0000008080000080800000808000000000000000000000000000000000000000
          0000000000000000000000808000008080000080800000808000008080000080
          800000808000008080000080800000808000}
        TabOrder = 6
        UpGlyph.Data = {
          0E010000424D0E01000000000000360000002800000009000000060000000100
          200000000000D800000000000000000000000000000000000000008080000080
          8000008080000080800000808000008080000080800000808000008080000080
          8000000000000000000000000000000000000000000000000000000000000080
          8000008080000080800000000000000000000000000000000000000000000080
          8000008080000080800000808000008080000000000000000000000000000080
          8000008080000080800000808000008080000080800000808000000000000080
          8000008080000080800000808000008080000080800000808000008080000080
          800000808000008080000080800000808000}
        OnDownClick = SpinButton1DownClick
        OnUpClick = SpinButton1UpClick
      end
      object SpinButton8: TSpinButton
        Tag = 7
        Left = 189
        Top = 2
        Width = 20
        Height = 22
        DownGlyph.Data = {
          0E010000424D0E01000000000000360000002800000009000000060000000100
          200000000000D800000000000000000000000000000000000000008080000080
          8000008080000080800000808000008080000080800000808000008080000080
          8000008080000080800000808000000000000080800000808000008080000080
          8000008080000080800000808000000000000000000000000000008080000080
          8000008080000080800000808000000000000000000000000000000000000000
          0000008080000080800000808000000000000000000000000000000000000000
          0000000000000000000000808000008080000080800000808000008080000080
          800000808000008080000080800000808000}
        TabOrder = 7
        UpGlyph.Data = {
          0E010000424D0E01000000000000360000002800000009000000060000000100
          200000000000D800000000000000000000000000000000000000008080000080
          8000008080000080800000808000008080000080800000808000008080000080
          8000000000000000000000000000000000000000000000000000000000000080
          8000008080000080800000000000000000000000000000000000000000000080
          8000008080000080800000808000008080000000000000000000000000000080
          8000008080000080800000808000008080000080800000808000000000000080
          8000008080000080800000808000008080000080800000808000008080000080
          800000808000008080000080800000808000}
        OnDownClick = SpinButton1DownClick
        OnUpClick = SpinButton1UpClick
      end
      object SpinButton5: TSpinButton
        Tag = 4
        Left = 82
        Top = 2
        Width = 20
        Height = 22
        DownGlyph.Data = {
          0E010000424D0E01000000000000360000002800000009000000060000000100
          200000000000D800000000000000000000000000000000000000008080000080
          8000008080000080800000808000008080000080800000808000008080000080
          8000008080000080800000808000000000000080800000808000008080000080
          8000008080000080800000808000000000000000000000000000008080000080
          8000008080000080800000808000000000000000000000000000000000000000
          0000008080000080800000808000000000000000000000000000000000000000
          0000000000000000000000808000008080000080800000808000008080000080
          800000808000008080000080800000808000}
        TabOrder = 8
        UpGlyph.Data = {
          0E010000424D0E01000000000000360000002800000009000000060000000100
          200000000000D800000000000000000000000000000000000000008080000080
          8000008080000080800000808000008080000080800000808000008080000080
          8000000000000000000000000000000000000000000000000000000000000080
          8000008080000080800000000000000000000000000000000000000000000080
          8000008080000080800000808000008080000000000000000000000000000080
          8000008080000080800000808000008080000080800000808000000000000080
          8000008080000080800000808000008080000080800000808000008080000080
          800000808000008080000080800000808000}
        OnDownClick = SpinButton1DownClick
        OnUpClick = SpinButton1UpClick
      end
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 3000
    OnTimer = Timer1Timer
    Left = 344
    Top = 195
  end
  object dlgFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 592
    Top = 179
  end
  object PopupMenu1: TPopupMenu
    Left = 704
    Top = 179
    object Font1: TMenuItem
      Caption = 'Font'
      OnClick = Font1Click
    end
  end
end
