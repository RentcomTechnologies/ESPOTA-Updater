unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ActnList, ExtCtrls, StdCtrls, Process, EspDB;

type

  { TMainForm }

  TMainForm = class(TForm)
    SaveAction: TAction;
    HeadPanel: TPanel;
    CosmeticShape: TShape;
    Label1: TLabel;
    OutputMemo: TMemo;
    SaveButton: TToolButton;
    Splitter1: TSplitter;
    ToolButton2: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    SettingButton: TToolButton;
    ToolButton8: TToolButton;
    AboutButton: TToolButton;
    UploadAction: TAction;
    DeleteAction: TAction;
    EditAction: TAction;
    AddAction: TAction;
    ActionList: TActionList;
    DeleteButton: TToolButton;
    UploadButton: TToolButton;
    ToolImageList: TImageList;
    ManageView: TListView;
    MainToolBar: TToolBar;
    AddButton: TToolButton;
    EditButton: TToolButton;
    procedure AddActionExecute(Sender: TObject);
    procedure AddActionUpdate(Sender: TObject);
    procedure DeleteActionExecute(Sender: TObject);
    procedure DeleteActionUpdate(Sender: TObject);
    procedure EditActionExecute(Sender: TObject);
    procedure EditActionUpdate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure ManageViewDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AboutButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SaveActionExecute(Sender: TObject);
    procedure SaveActionUpdate(Sender: TObject);
    procedure SettingButtonClick(Sender: TObject);
    procedure UploadActionExecute(Sender: TObject);
    procedure UploadActionUpdate(Sender: TObject);
  private
    procedure AddManageViewEntry(const Data: TEspData);
    procedure EditManageViewEntry;
    procedure UpdateManageView(const Item: TListItem; const Data: TEspData);
  public
    ESPManager: TEspDataManager;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  AboutUnit, ProjectDetailUnit, SettingUnit;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  ESPManager := TEspDataManager.Create;
  if FileExists(ExtractFilePath(Application.ExeName) + 'espdb.json') then
  begin
    ESPManager.LoadFromFile(ExtractFilePath(Application.ExeName) + 'espdb.json');
    for i := 0 to ESPManager.ProjectList.Count - 1 do
      AddManageViewEntry(TEspData(ESPManager.ProjectList.Items[i]));
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ManageView.Items.Count -1 do
  begin
    if Assigned(ManageView.Items[i].Data) then
      ManageView.Items[i].Data := nil;
  end;
  ESPManager.ClearDatas;
  ESPManager.Free;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ESPManager.IsModified then
  begin
    case MessageDlg('Data has changed. Save on exit ?', mtWarning, [mbYes, mbNo, mbCancel], 0) of
      mrYes: ESPManager.SaveToFile(ExtractFilePath(Application.ExeName) + 'espdb.json');
      mrCancel: CanClose := False;
    end;
  end;
end;

procedure TMainForm.SaveActionExecute(Sender: TObject);
begin
  ManageView.ItemIndex := -1;
  ESPManager.SaveToFile(ExtractFilePath(Application.ExeName) + 'espdb.json');
end;

procedure TMainForm.SaveActionUpdate(Sender: TObject);
begin
  SaveAction.Enabled := ESPManager.IsModified;
end;

procedure TMainForm.SettingButtonClick(Sender: TObject);
begin
  with TSettingForm.Create(nil)do
  try
    ScriptFileEdit.Text := ESPManager.Settings.OtaScriptFile;
    OTADefaultPwdEdit.Text := ESPManager.Settings.DefaultPassword;
    if (ShowModal = mrOk) then
    begin
      ESPManager.Settings.OtaScriptFile := ScriptFileEdit.Text;
      ESPManager.Settings.DefaultPassword := OTADefaultPwdEdit.Text;
      ESPManager.SaveSettings;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.AboutButtonClick(Sender: TObject);
begin
  TAboutForm.Execute;
end;

procedure TMainForm.AddActionExecute(Sender: TObject);
var
  ESPData: TEspData;
begin
  with TProjectDetailForm.Create(nil)do
  try
    OTAPwdEdit.Text := ESPManager.Settings.DefaultPassword;
    if (ShowModal = mrOk) then
    begin
      ESPData := TEspData.Create;
      ESPData.ProjectFile := FileEdit.Text;
      if ((BoardBox.ItemIndex > -1) and Assigned(BoardBox.Items.Objects[BoardBox.ItemIndex])) then
        ESPData.BoardName := TEspBoard(BoardBox.Items.Objects[BoardBox.ItemIndex]).BoardName;
      ESPData.IPDeviceAddress := IPEdit.Text;
      ESPData.OTAName := OTANameEdit.Text;
      ESPData.OTAPassword := OTAPwdEdit.Text;
      ESPData.Comment := CommentEdit.Text;
      ESPManager.AddData(ESPData);
      AddManageViewEntry(ESPData);
    end;
  finally
    Free;
  end;
  ManageView.ItemIndex := -1;
end;

procedure TMainForm.AddActionUpdate(Sender: TObject);
begin
  AddAction.Enabled := True;
end;

procedure TMainForm.EditActionExecute(Sender: TObject);
begin
  EditManageViewEntry;
end;

procedure TMainForm.EditActionUpdate(Sender: TObject);
begin
  EditAction.Enabled := (ManageView.ItemIndex > -1) and Assigned(ManageView.Items.Item[ManageView.ItemIndex].Data);
end;

procedure TMainForm.ManageViewDblClick(Sender: TObject);
begin
  if ((ManageView.ItemIndex > -1) and Assigned(ManageView.Items.Item[ManageView.ItemIndex].Data)) then
    EditManageViewEntry;
end;

procedure TMainForm.DeleteActionExecute(Sender: TObject);
begin
  if (MessageDlg('Are you sure you want to delete this project ?', mtWarning, [mbYes, mbNo], 0) = mrYes) then
  begin
    ESPManager.DeleteData(TEspData(ManageView.Items.Item[ManageView.ItemIndex].Data));
    ManageView.Items.Item[ManageView.ItemIndex].Data := nil;
    ManageView.Items.Delete(ManageView.ItemIndex);
    ManageView.ItemIndex := -1;
  end;
end;

procedure TMainForm.DeleteActionUpdate(Sender: TObject);
begin
  DeleteAction.Enabled := (ManageView.ItemIndex > -1) and Assigned(ManageView.Items.Item[ManageView.ItemIndex].Data);
end;

procedure TMainForm.AddManageViewEntry(const Data: TEspData);
var
  Item: TListItem;
begin
  Item := ManageView.Items.Add;
  Item.Caption := ExtractFileName(Data.ProjectFile);
  Item.SubItems.Add(ESPManager.GetBoardTitleByName(Data.BoardName));
  Item.SubItems.Add(Data.IPDeviceAddress);
  Item.SubItems.Add(Data.OTAName);
  Item.SubItems.Add(Data.OTAPassword);
  Item.SubItems.Add(Data.Comment);
  Item.Data := Data;
end;

procedure TMainForm.UpdateManageView(const Item: TListItem; const Data: TEspData);
begin
  Item.Caption := ExtractFileName(Data.ProjectFile);
  Item.SubItems[0] := ESPManager.GetBoardTitleByName(Data.BoardName);
  Item.SubItems[1] := Data.IPDeviceAddress;
  Item.SubItems[2] := Data.OTAName;
  Item.SubItems[3] := Data.OTAPassword;
  Item.SubItems[4] := Data.Comment;
end;

procedure TMainForm.EditManageViewEntry;
var
  ESPData: TEspData;
  i: Integer;
begin
  with TProjectDetailForm.Create(nil)do
  try
    ESPData := TEspData(ManageView.Items.Item[ManageView.ItemIndex].Data);
    FileEdit.Text := ESPData.ProjectFile;
    for i := 0 to BoardBox.Items.Count - 1 do
    begin
      if Assigned(BoardBox.Items.Objects[i]) then
      begin
        if TEspBoard(BoardBox.Items.Objects[i]).BoardName = ESPData.BoardName then
        begin
          BoardBox.ItemIndex := i;
          Break;
        end;
      end;
    end;
    IPEdit.Text := ESPData.IPDeviceAddress;
    OTANameEdit.Text := ESPData.OTAName;
    OTAPwdEdit.Text := ESPData.OTAPassword;
    CommentEdit.Text := ESPData.Comment;
    if (ShowModal = mrOk) then
    begin
      ESPData.ProjectFile := FileEdit.Text;
      if ((BoardBox.ItemIndex > -1) and Assigned(BoardBox.Items.Objects[BoardBox.ItemIndex])) then
        ESPData.BoardName := TEspBoard(BoardBox.Items.Objects[BoardBox.ItemIndex]).BoardName;
      ESPData.IPDeviceAddress := IPEdit.Text;
      ESPData.OTAName := OTANameEdit.Text;
      ESPData.OTAPassword := OTAPwdEdit.Text;
      ESPData.Comment := CommentEdit.Text;
      UpdateManageView(ManageView.Items.Item[ManageView.ItemIndex], ESPData);
      ESPManager.IsModified := True;
      ManageView.ItemIndex := -1;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.UploadActionExecute(Sender: TObject);
var
  Python: TProcess;
  CharBuffer: array [0..511] of Char;
  ReadCount: Integer;
  ESPData: TEspData;
begin
  OutputMemo.Clear;
  ESPData := TEspData(ManageView.Items.Item[ManageView.ItemIndex].Data);
  if Assigned(ESPData) then
  begin
    if (MessageDlg('Are you sure you want to upload this new firmware for ' +
       ExtractFileName(ChangeFileExt(ESPData.ProjectFile, '')) + ' project ?', mtWarning, [mbYes, mbNo], 0) = mrYes) then
    begin
      if ((ESPData.IPDeviceAddress <> '') and (ESPData.BoardName <> '') and (ESPData.OTAPassword <> '') and (ESPData.ProjectFile <> '')) then
      begin
        if FileExists(ESPData.ProjectFile + '.' + ESPManager.GetBoardVariantByName(ESPData.BoardName) + '.bin') then
        begin
          Python := TProcess.Create(nil);
          try
            Python.CommandLine := 'python ' +
              ESPManager.Settings.OtaScriptFile + ' -d ' +
              ' -i ' + ESPData.IPDeviceAddress +
              ' -f ' + ESPData.ProjectFile + '.' + ESPManager.GetBoardVariantByName(ESPData.BoardName) + '.bin ' +
              ' --auth=' + ESPData.OTAPassword + ' -d -r';
            Python.Options := [poUsePipes, poStdErrToOutPut];
            Python.ShowWindow := swoHIDE;
            Python.Execute;
            while Python.Running do
            begin
              repeat
                ReadCount := Python.Output.Read(CharBuffer, 512);
                OutputMemo.Lines.Add(Copy(CharBuffer, 0, ReadCount));
              until (ReadCount < 512);
            end;
          finally
            Python.Free;
          end;
        end
        else
          ShowMessage('Firmware file (*.bin) was not found. Have you compiled the project ?'#13#10'Please use "Sketch>Export compiled Binary" from Arduino IDE');
      end
      else
        ShowMessage('Project file, Board name, OTA Password or IP Address fields not have been completed !'#13#10'Please fill in all fields.');
    end;
  end;
end;

procedure TMainForm.UploadActionUpdate(Sender: TObject);
begin
  UploadAction.Enabled := (ManageView.ItemIndex > -1) and Assigned(ManageView.Items.Item[ManageView.ItemIndex].Data);
end;

end.

