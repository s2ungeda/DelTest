object FrmSPOrder: TFrmSPOrder
  Left = 0
  Top = 0
  Caption = 'SP'
  ClientHeight = 411
  ClientWidth = 520
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -21
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 25
  object StatusBar1: TStatusBar
    Left = 0
    Top = 392
    Width = 520
    Height = 19
    Panels = <>
    ExplicitLeft = 496
    ExplicitTop = 376
    ExplicitWidth = 0
  end
  object LabeledEdit1: TLabeledEdit
    Left = 40
    Top = 6
    Width = 73
    Height = 33
    EditLabel.Width = 32
    EditLabel.Height = 25
    EditLabel.Caption = #53076#51064
    LabelPosition = lpLeft
    TabOrder = 1
  end
  object LabeledEdit2: TLabeledEdit
    Left = 151
    Top = 6
    Width = 81
    Height = 33
    EditLabel.Width = 32
    EditLabel.Height = 25
    EditLabel.Caption = #49688#47049
    LabelPosition = lpLeft
    TabOrder = 2
  end
  object edtCount: TLabeledEdit
    Left = 277
    Top = 6
    Width = 52
    Height = 33
    Alignment = taRightJustify
    EditLabel.Width = 32
    EditLabel.Height = 25
    EditLabel.Caption = #48152#48373
    LabelPosition = lpLeft
    TabOrder = 3
    Text = '3'
  end
  object UpDown1: TUpDown
    Left = 329
    Top = 6
    Width = 16
    Height = 33
    Associate = edtCount
    Min = 1
    Max = 10
    Position = 3
    TabOrder = 4
  end
  object sgPos: TStringGrid
    Left = 3
    Top = 43
    Width = 518
    Height = 105
    ColCount = 7
    DefaultRowHeight = 33
    RowCount = 3
    TabOrder = 5
  end
  object StringGrid1: TStringGrid
    Left = 3
    Top = 154
    Width = 263
    Height = 105
    ColCount = 4
    DefaultRowHeight = 33
    FixedCols = 0
    RowCount = 3
    TabOrder = 6
    ColWidths = (
      65
      64
      64
      64)
  end
end
