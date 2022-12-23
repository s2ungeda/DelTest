object FrmNormalOrder: TFrmNormalOrder
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #51068#48152#51452#47928
  ClientHeight = 208
  ClientWidth = 529
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
  object Panel1: TPanel
    Left = 327
    Top = 0
    Width = 202
    Height = 208
    Align = alRight
    Caption = 'Panel1'
    TabOrder = 0
    object sgHoga: TStringGrid
      Left = 1
      Top = 1
      Width = 200
      Height = 206
      Align = alClient
      ColCount = 3
      DefaultRowHeight = 19
      DefaultDrawing = False
      DoubleBuffered = True
      FixedCols = 0
      RowCount = 10
      FixedRows = 0
      ParentDoubleBuffered = False
      ParentShowHint = False
      ScrollBars = ssNone
      ShowHint = False
      TabOrder = 0
      OnDrawCell = sgHogaDrawCell
      OnMouseDown = sgHogaMouseDown
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 329
    Height = 208
    Align = alLeft
    TabOrder = 1
    object lbDepth: TLabel
      Left = 157
      Top = 87
      Width = 3
      Height = 13
    end
    object rgPrice: TRadioGroup
      Left = 40
      Top = 71
      Width = 100
      Height = 36
      Columns = 2
      Ctl3D = True
      ItemIndex = 1
      Items.Strings = (
        #54840#44032
        #44032#44201)
      ParentCtl3D = False
      TabOrder = 12
    end
    object edtQty: TLabeledEdit
      Left = 46
      Top = 31
      Width = 65
      Height = 21
      EditLabel.Width = 22
      EditLabel.Height = 13
      EditLabel.Caption = #49688#47049
      LabelPosition = lpLeft
      TabOrder = 0
    end
    object edtPrice: TLabeledEdit
      Left = 46
      Top = 58
      Width = 94
      Height = 21
      EditLabel.Width = 22
      EditLabel.Height = 13
      EditLabel.Caption = #45800#44032
      LabelPosition = lpLeft
      TabOrder = 1
    end
    object cbExKind: TComboBox
      Left = 4
      Top = 4
      Width = 65
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 2
      Text = #48148#51060#45240#49828
      OnChange = cbExKindChange
      Items.Strings = (
        #48148#51060#45240#49828
        #50629#48708#53944
        #48727#50040)
    end
    object edtCode: TEdit
      Left = 134
      Top = 4
      Width = 76
      Height = 21
      TabOrder = 3
      OnKeyDown = edtCodeKeyDown
    end
    object rbBuy: TRadioButton
      Left = 164
      Top = 37
      Width = 46
      Height = 17
      Caption = #47588#49688
      Checked = True
      TabOrder = 4
      TabStop = True
      OnClick = rbBuyClick
    end
    object rbSell: TRadioButton
      Left = 216
      Top = 39
      Width = 46
      Height = 17
      Caption = #47588#46020
      TabOrder = 5
      OnClick = rbSellClick
    end
    object cbReduce: TCheckBox
      Left = 164
      Top = 60
      Width = 85
      Height = 17
      Caption = 'Reduct-Only'
      TabOrder = 6
    end
    object btnOrder: TButton
      Left = 216
      Top = 6
      Width = 109
      Height = 27
      Caption = #47588#49688' '#51452#47928
      TabOrder = 7
      OnClick = btnOrderClick
    end
    object sgBal: TStringGrid
      Left = 4
      Top = 135
      Width = 320
      Height = 63
      ColCount = 4
      DefaultRowHeight = 19
      FixedCols = 0
      RowCount = 3
      FixedRows = 0
      TabOrder = 8
      ColWidths = (
        59
        96
        57
        98)
    end
    object Button1: TButton
      Left = 4
      Top = 104
      Width = 75
      Height = 25
      Caption = #51092#44256#51312#54924
      TabOrder = 9
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 85
      Top = 106
      Width = 75
      Height = 25
      Caption = #51452#47928#51312#54924
      TabOrder = 10
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 166
      Top = 106
      Width = 75
      Height = 25
      Caption = '1'#51068#44144#47000#50529
      TabOrder = 11
      OnClick = Button3Click
    end
    object cbMarket: TComboBox
      Left = 75
      Top = 4
      Width = 54
      Height = 21
      Style = csDropDownList
      ItemIndex = 1
      TabOrder = 13
      Text = 'Fut'
      OnChange = cbMarketChange
      Items.Strings = (
        'Spot'
        'Fut')
    end
    object Button4: TButton
      Left = 247
      Top = 106
      Width = 75
      Height = 25
      Caption = #54252#51648#49496
      TabOrder = 14
      OnClick = Button4Click
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 272
    Top = 152
  end
end
