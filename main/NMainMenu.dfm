object DataModule1: TDataModule1
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 172
  Width = 432
  object MainMenu1: TMainMenu
    Left = 112
    Top = 88
    object nFile: TMenuItem
      Caption = #54028#51068
      object N1: TMenuItem
        Caption = #51333#47308
      end
    end
    object nAccount: TMenuItem
      Caption = #44144#47000#49548
      object nExchange: TMenuItem
        Caption = #51077#52636#44552#54788#54889#54364
        OnClick = nExchangeClick
      end
    end
    object nOrder: TMenuItem
      Caption = #51452#47928
    end
    object nQuote: TMenuItem
      Caption = #49884#49464
      object Kimp1: TMenuItem
        Caption = 'Kimp '#54788#54889#54364
        OnClick = Kimp1Click
      end
      object N6: TMenuItem
        Tag = 1
        Caption = #49884#49464#47784#45768#53552#47553
        OnClick = Kimp1Click
      end
      object N2: TMenuItem
        Tag = 2
        Caption = #45824#54364' KIP'
        OnClick = Kimp1Click
      end
      object MainWDC1: TMenuItem
        Tag = 3
        Caption = #45824#54364' WDC'
        OnClick = Kimp1Click
      end
    end
  end
end
