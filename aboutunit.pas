unit AboutUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    MessageLabel: TLabel;
    OkButton: TButton;
    Image: TImage;
  public
    class procedure Execute;
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

class procedure TAboutForm.Execute;
var
  AForm: TAboutForm;
begin
  AForm := TAboutForm.Create(nil);
  try
    AForm.ShowModal;
  finally
    AForm.Free;
  end;
end;

end.

