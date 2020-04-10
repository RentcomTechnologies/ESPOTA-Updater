unit SettingUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TSettingForm }

  TSettingForm = class(TForm)
    Bevel1: TBevel;
    OpenButton: TButton;
    CancelButton: TButton;
    OTADefaultPwdEdit: TEdit;
    ScriptFileEdit: TEdit;
    Label1: TLabel;
    Label5: TLabel;
    OkButton: TButton;
    procedure OpenButtonClick(Sender: TObject);
  private

  public

  end;

var
  SettingForm: TSettingForm;

implementation

{$R *.lfm}

{ TSettingForm }

procedure TSettingForm.OpenButtonClick(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
  try
    Filter := 'Python Script File (*.py)|*.py';
    DefaultExt := 'py';
    if Execute then
      ScriptFileEdit.Text := FileName;
  finally
    Free;
  end;
end;

end.

