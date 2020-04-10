unit EspDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, fpjson, fpjsonrtti, jsonparser;

type
  TEspBoard = class(TPersistent)
  private
    FBoardTitle: string;
    FBoardName: string;
    FBoardVariant: string;
  published
    property BoardTitle: string read FBoardTitle write FBoardTitle;
    property BoardName: string read FBoardName write FBoardName;
    property BoardVariant: string read FBoardVariant write FBoardVariant;
  end;

  TEspData = class(TPersistent)
  private
    FProjectFile: string;
    FBoardName: string;
    FIPDeviceAddress: string;
    FOTAName: string;
    FOTAPassword: string;
    FComment: string;
  published
    property ProjectFile: string read FProjectFile write FProjectFile;
    property BoardName: string read FBoardName write FBoardName;
    property IPDeviceAddress: string read FIPDeviceAddress write FIPDeviceAddress;
    property OTAName: string read FOTAName write FOTAName;
    property OTAPassword: string read FOTAPassword write FOTAPassword;
    property Comment: string read FComment write FComment;
  end;

  TSettings = class(TPersistent)
  private
    FOtaScriptFile: string;
    FDefaultPassword: string;
  published
    property OtaScriptFile: string read FOtaScriptFile write FOtaScriptFile;
    property DefaultPassword: string read FDefaultPassword write FDefaultPassword;
  end;

  TEspDataManager = class
  private
    FBoardList: TList;
    FProjectList: TList;
    FIsModified: Boolean;
    FSettings: TSettings;
    procedure LoadBoards;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddData(const Data: TEspData);
    procedure DeleteData(const Data: TEspData);
    function GetBoardTitleByName(const Name: string): string;
    function GetBoardVariantByName(const Name: string): string;
    procedure ClearDatas;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    procedure LoadSettings;
    procedure SaveSettings;
    property ProjectList: TList read FProjectList;
    property BoardList: TList read FBoardList;
    property Settings: TSettings read FSettings;
    property IsModified: Boolean read FIsModified write FIsModified;
  end;

implementation

constructor TEspDataManager.Create;
begin
  FProjectList := TList.Create;
  FBoardList := TList.Create;
  FSettings := TSettings.Create;
  FIsModified := False;
  LoadBoards;
  LoadSettings;
end;

destructor TEspDataManager.Destroy;
begin
  FSettings.Free;
  FBoardList.Free;
  FProjectList.Free;
  inherited;
end;

procedure TEspDataManager.LoadBoards;
var
  i: Integer;
  JSONFile: TStringList;
  DeStreamer: TJSONDeStreamer;
  jData: TJSONData;
  jObj: TJSONObject;
  Board: TEspBoard;
begin
  JSONFile := TStringList.Create;
  try
    // File must be exists !
    JSONFile.LoadFromFile(ExtractFilePath(Application.ExeName) + 'boards.json');
    jData := GetJSON(JSONFile.Text);
    if Assigned(jData) then
    begin
      DeStreamer := TJSONDeStreamer.Create(nil);
      try
        for i := 0 to jData.Count - 1 do
        begin
          jObj := TJSONObject(jData.Items[i]);
          if Assigned(jObj) then
          begin
            Board := TEspBoard.Create;
            DeStreamer.JSONToObject(jObj, Board);
            FBoardList.Add(Board);
          end;
        end;
      finally
        DeStreamer.Free;
      end;
    end;
  finally
    JSONFile.Free;
  end;
end;

procedure TEspDataManager.LoadSettings;
var
  JSONFile: TStringList;
  DeStreamer: TJSONDeStreamer;
  jData: TJSONData;
  jObj: TJSONObject;
begin
  JSONFile := TStringList.Create;
  try
    if not FileExists(ExtractFilePath(Application.ExeName) + 'config.json') then
      JSONFile.SaveToFile(ExtractFilePath(Application.ExeName) + 'config.json');
    JSONFile.LoadFromFile(ExtractFilePath(Application.ExeName) + 'config.json');
    jData := GetJSON(JSONFile.Text);
    if Assigned(jData) then
    begin
      DeStreamer := TJSONDeStreamer.Create(nil);
      try
        jObj := TJSONObject(jData);
        if Assigned(jObj) then
          DeStreamer.JSONToObject(jObj, FSettings);
      finally
        DeStreamer.Free;
      end;
    end;
  finally
    JSONFile.Free;
  end;
end;

procedure TEspDataManager.SaveSettings;
var
  JSONFile: TStringList;
  Streamer: TJSONStreamer;
  jObj: TJSONObject;
begin
  JSONFile := TStringList.Create;
  try
    Streamer := TJSONStreamer.Create(nil);
    try
      jObj := TJSONObject.Create;
      jObj := Streamer.ObjectToJSON(FSettings);
    finally
      Streamer.Free;
    end;
    JSONFile.Add(jObj.FormatJSON);
    JSONFile.SaveToFile(ExtractFilePath(Application.ExeName) + 'config.json');
  finally
    JSONFile.Free;
  end;
end;

procedure TEspDataManager.AddData(const Data: TEspData);
begin
  FProjectList.Add(Data);
  FIsModified := True;
end;

procedure TEspDataManager.DeleteData(const Data: TEspData);
var
  i: Integer;
begin
  for i := 0 to FProjectList.Count - 1 do
  begin
    if (TEspData(FProjectList.Items[i]) = Data) then
    begin
      TEspData(FProjectList.Items[i]).Free;
      FProjectList.Delete(i);
      FIsModified := True;
      Break;
    end;
  end;
end;

function TEspDataManager.GetBoardTitleByName(const Name: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to FBoardList.Count - 1 do
  begin
    if (TEspBoard(FBoardList.Items[i]).BoardName = Name) then
    begin
      Result := TEspBoard(FBoardList.Items[i]).BoardTitle;
      Break;
    end;
  end;
end;

function TEspDataManager.GetBoardVariantByName(const Name: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to FBoardList.Count - 1 do
  begin
    if (TEspBoard(FBoardList.Items[i]).BoardName = Name) then
    begin
      Result := TEspBoard(FBoardList.Items[i]).BoardVariant;
      Break;
    end;
  end;
end;

procedure TEspDataManager.ClearDatas;
var
  i, j: Integer;
begin
  for i := FProjectList.Count - 1 downto 0 do
  begin
    TEspData(FProjectList.Items[i]).Free;
    FProjectList.Delete(i);
  end;
  FProjectList.Clear;
  for i := FBoardList.Count - 1 downto 0 do
  begin
    TEspBoard(FBoardList.Items[i]).Free;
    FBoardList.Delete(i);
  end;
  FBoardList.Clear;
end;

procedure TEspDataManager.LoadFromFile(const FileName: string);
var
  i: Integer;
  JSONFile: TStringList;
  DeStreamer: TJSONDeStreamer;
  ESPData: TEspData;
  jData: TJSONData;
  jObj: TJSONObject;
begin
  JSONFile := TStringList.Create;
  try
    JSONFile.LoadFromFile(FileName);
    jData := GetJSON(JSONFile.Text);
    if Assigned(jData) then
    begin
      DeStreamer := TJSONDeStreamer.Create(nil);
      try
        for i := 0 to jData.Count - 1 do
        begin
          jObj := TJSONObject(jData.Items[i]);
          if Assigned(jObj) then
          begin
            ESPData := TEspData.Create;
            DeStreamer.JSONToObject(jObj, ESPData);
            FProjectList.Add(ESPData);
          end;
        end;
      finally
        DeStreamer.Free;
      end;
    end;
  finally
    JSONFile.Free;
  end;
end;

procedure TEspDataManager.SaveToFile(const FileName: string);
var
  i: Integer;
  JSONFile: TStringList;
  Streamer: TJSONStreamer;
  Data: TEspData;
  jEntry, jData: TJSONObject;
begin
  JSONFile := TStringList.Create;
  try
    Streamer := TJSONStreamer.Create(nil);
    try
      jEntry := TJSONObject.Create;
      for i := 0 to FProjectList.Count - 1 do
      begin
        Data := TEspData(FProjectList.Items[i]);
        jData := Streamer.ObjectToJSON(Data);
        jEntry.Add('entry' + IntToStr(i), jData);
      end;
    finally
      Streamer.Free;
    end;
    JSONFile.Add(jEntry.FormatJSON);
    JSONFile.SaveToFile(FileName);
    FIsModified := False;
  finally
    JSONFile.Free;
  end;
end;

end.

