unit UController;

interface

uses GlobalData, MainForm;

type
  TController = class(TObject)
  private
  //
  protected
  //
  public
  //
  published
    procedure IncFilesLoaded;
  //
  end;

implementation

procedure TController.IncFilesLoaded;
const OPNAME = 'TController.IncFilesLoaded';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
