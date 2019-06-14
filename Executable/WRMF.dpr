program WRMF;

{$R *.RES}

{$R 'WRMFVersion.res' '..\WRMFVersion.rc'}

{$IFDEF MERGE_DLLS}
{$R 'WRMFGraphicsx.res' '..\DLLProjects\Graphics\WRMFGraphicsx.rc'}
{$R 'RainfallDataGraphics.res' '..\DLLProjects\Graphics\RainfallDataGraphics.rc'}
{$R 'StomsaSQL.res' '..\DLLProjects\ModelStomsa\StomsaSQL.rc'}
{$ENDIF}

uses
  ShareMem,
  Windows,
  Forms,
  SysUtils,
  UAppModulesConstructionMain;

var LCanProceed: boolean;
begin
  Application.Initialize;
  Application.Title := 'WRMF';
  GAppModules := TAppModulesConstructorMain.Create(LCanProceed);
  try
    if LCanProceed then
    begin
      Application.Icon.Handle := LoadIcon(HInstance, 'MAINICON');
      Application.HintPause := 2;
      Application.HintHidePause := 60000;
      Application.Run;
    end;
  finally
    GAppModules.Free;
  end;
end.
