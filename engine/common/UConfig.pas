unit UConfig;

interface

uses
  system.IniFiles, system.SysUtils
  ;

type

  TConfig = record
    LOG_DIR   : string;
    QUOTE_DIR : string;
    DATA_DIR   : string;
    LOG_LEVEL : integer;
    DATA_FILE : string;
    VERBOSE  : boolean;

    function LoadConfig : boolean;
  end;

implementation

{ TConfig }

function TConfig.LoadConfig: boolean;
var
  pIniFile : TIniFile;
  stDir : string;
begin
  result := true;

  try
    try
      stDir := ExtractFilePath( paramstr(0) )+'Config\';
      pIniFile := TIniFile.Create(stDir + 'Config.ini' );

      /////////////////////////////////////////////////////////////

      LOG_LEVEL := pIniFile.ReadInteger('Log', 'Level', 3);
      VERBOSE   := pIniFile.ReadInteger('Log', 'Verbose', 1) = 1;

      LOG_DIR   := pIniFile.ReadString('Dir', 'LogDir', 'Sauri');
      QUOTE_DIR := pIniFile.ReadString('Dir', 'QuoteDir', 'Sauri');
      DATA_DIR  := pIniFile.ReadString('Dir', 'DataDir', 'Sauri');

      DATA_FILE := pIniFile.ReadString('File', 'DataFile', 'Sauri');
      /////////////////////////////////////////////////////////////

    except
      result := false;
    end;
  finally
    pIniFile.Free;
  end;

end;

end.
