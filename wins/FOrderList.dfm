object FrmOrderList: TFrmOrderList
  Left = 0
  Top = 0
  Caption = #51452#47928#47532#49828#53944
  ClientHeight = 293
  ClientWidth = 643
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
    Left = 0
    Top = 0
    Width = 643
    Height = 25
    Align = alTop
    TabOrder = 0
    object ComboBox1: TComboBox
      Left = 5
      Top = 2
      Width = 68
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = #51204#52404
      OnChange = ComboBox1Change
      Items.Strings = (
        #51204#52404
        #48148#51060#45240#49828
        #50629#48708#53944
        #48727#50040)
    end
    object ComboBox2: TComboBox
      Left = 101
      Top = 2
      Width = 68
      Height = 21
      Style = csDropDownList
      ItemIndex = 1
      TabOrder = 1
      Text = #51217#49688
      OnChange = ComboBox2Change
      Items.Strings = (
        #51204#52404
        #51217#49688
        #52404#44208
        #52712#49548)
    end
  end
  object sgOrder: TStringGrid
    Left = 0
    Top = 25
    Width = 643
    Height = 268
    Align = alClient
    DefaultColAlignment = taCenter
    DefaultRowHeight = 19
    DefaultDrawing = False
    FixedCols = 0
    PopupMenu = PopupMenu1
    ScrollBars = ssVertical
    TabOrder = 1
    OnDrawCell = sgOrderDrawCell
  end
  object PopupMenu1: TPopupMenu
    Left = 512
    Top = 208
    object N1: TMenuItem
      Caption = #52712#49548#51452#47928
    end
  end
end
