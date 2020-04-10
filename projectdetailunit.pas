unit ProjectDetailUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, AntOfficePanel, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls;

type

  { TProjectDetailForm }

  TProjectDetailForm = class(TForm)
    Bevel1: TBevel;
    OpenButton: TButton;
    CancelButton: TButton;
    BoardBox: TComboBox;
    FileEdit: TEdit;
    OTANameEdit: TEdit;
    OTAPwdEdit: TEdit;
    CommentEdit: TEdit;
    IPEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    OkButton: TButton;
    procedure OpenButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  ProjectDetailForm: TProjectDetailForm;

implementation

{$R *.lfm}

uses
  MainUnit, EspDB;

{ TProjectDetailForm }

procedure TProjectDetailForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  if Assigned(MainForm.ESPManager) then
  begin
    for i := 0 to MainForm.ESPManager.BoardList.Count - 1 do
      BoardBox.Items.AddObject(
        TEspBoard(MainForm.ESPManager.BoardList.Items[i]).BoardTitle,
        TEspBoard(MainForm.ESPManager.BoardList.Items[i])
        );
  end;
end;

procedure TProjectDetailForm.OpenButtonClick(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
  try
    Filter := 'Arduino Project File (*.ino)|*.ino';
    DefaultExt := 'ino';
    if Execute then
      FileEdit.Text := FileName;
  finally
    Free;
  end;
end;

end.

