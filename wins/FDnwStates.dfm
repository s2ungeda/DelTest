object FrmDnwStates: TFrmDnwStates
  Left = 0
  Top = 0
  Caption = #51077#52636#44552#54788#54889
  ClientHeight = 345
  ClientWidth = 936
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object plLeft: TPanel
    Left = 0
    Top = 0
    Width = 936
    Height = 317
    Align = alClient
    BevelOuter = bvNone
    Caption = 'plLeft'
    TabOrder = 0
    object plLeftTop: TPanel
      Left = 0
      Top = 0
      Width = 936
      Height = 32
      Align = alTop
      BevelOuter = bvLowered
      ParentBackground = False
      TabOrder = 0
      DesignSize = (
        936
        32)
      object Refresh: TButton
        Left = 847
        Top = 4
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Refresh'
        TabOrder = 0
        OnClick = RefreshClick
      end
      object cbAuto: TCheckBox
        Left = 697
        Top = 7
        Width = 41
        Height = 17
        Anchors = [akRight, akBottom]
        Caption = 'Auto'
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = cbAutoClick
      end
      object edtSec: TLabeledEdit
        Left = 744
        Top = 5
        Width = 39
        Height = 21
        Alignment = taRightJustify
        Anchors = [akRight, akBottom]
        EditLabel.Width = 54
        EditLabel.Height = 13
        EditLabel.Caption = '('#45800#50948' : '#52488' )'
        LabelPosition = lpRight
        NumbersOnly = True
        TabOrder = 2
        Text = '10'
      end
      object btnSort: TButton
        Left = 155
        Top = 4
        Width = 50
        Height = 25
        Caption = 'Reload'
        TabOrder = 3
        OnClick = btnSortClick
      end
      object cbAuto2: TCheckBox
        Left = 138
        Top = 7
        Width = 41
        Height = 17
        Anchors = [akRight, akBottom]
        Caption = 'Auto'
        Checked = True
        State = cbChecked
        TabOrder = 4
        OnClick = cbAuto2Click
      end
      object edtSec2: TLabeledEdit
        Left = 180
        Top = 5
        Width = 39
        Height = 21
        Alignment = taRightJustify
        Anchors = [akRight, akBottom]
        EditLabel.Width = 54
        EditLabel.Height = 13
        EditLabel.Caption = '('#45800#50948' : '#52488' )'
        LabelPosition = lpRight
        NumbersOnly = True
        TabOrder = 5
        Text = '10'
      end
      object Button1: TButton
        Left = 211
        Top = 3
        Width = 24
        Height = 25
        Caption = #9660
        TabOrder = 6
        OnClick = Button1Click
      end
    end
    object plLeftClient: TPanel
      Left = 0
      Top = 32
      Width = 936
      Height = 285
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object sgDnw: TStringGrid
        Left = 0
        Top = 0
        Width = 936
        Height = 285
        Align = alClient
        ColCount = 14
        DefaultRowHeight = 20
        DefaultDrawing = False
        FixedCols = 0
        RowCount = 25
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goFixedRowDefAlign]
        ParentFont = False
        TabOrder = 0
        OnDrawCell = sgDnwDrawCell
        OnMouseDown = sgDnwMouseDown
      end
    end
  end
  object plBottom: TPanel
    Left = 0
    Top = 317
    Width = 936
    Height = 28
    Align = alBottom
    Caption = 'plBottom'
    TabOrder = 1
    Visible = False
    object sgDetail: TStringGrid
      Left = 1
      Top = 1
      Width = 934
      Height = 26
      Align = alClient
      DefaultRowHeight = 20
      DefaultDrawing = False
      DoubleBuffered = True
      FixedCols = 0
      RowCount = 1
      FixedRows = 0
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goFixedRowDefAlign]
      ParentDoubleBuffered = False
      ParentFont = False
      TabOrder = 0
      OnDrawCell = sgDetailDrawCell
    end
  end
  object refreshTimer: TTimer
    OnTimer = refreshTimerTimer
    Left = 648
    Top = 216
  end
  object reLoadTimer: TTimer
    OnTimer = reLoadTimerTimer
    Left = 648
    Top = 160
  end
end
