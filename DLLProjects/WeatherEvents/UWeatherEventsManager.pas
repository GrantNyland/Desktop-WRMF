{******************************************************************************}
{*  UNIT      : Contains the class TWeatherEventsManager.                     *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/02/10                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UWeatherEventsManager;

interface

uses
  Classes,
  VCL.ComCtrls,
  UAbstractObject,
  UMenuItemManager,
  UWeatherEvents,
  UWeatherEventsTabSheet,
  UWeatherEventsValidator,
  VoaimsCom_TLB;

type
  TWeatherEventsManager = class(TAbstractWeatherEventsManager)
  protected
    FWeatherEventsTabSheet  : TWeatherEventsTabSheet;
    FWeatherEventsValidator : TWeatherEventsValidator;
    FData                   : TWeatherEvents;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged (AContext    : TChangeContext;
                                  AFieldName  : string;
                                  AOldValue   : string;
                                  ANewValue   : string): boolean; override;
    function ProcessMetaDataEvent : boolean; override;
    procedure TabHasChanged; override;
    procedure ShowWeatherEvents (AStartDateTime : TDateTime;
                                 AEndDateTime   : TDateTime;
                                 const AArea    : WideString); override;
    procedure AddTabsheetToPageControl; override;
    procedure RemoveTabsheetFromPageControl; override;
    function Data : IWeatherEvents; override;
  end;

implementation

uses
  VCL.Controls,
  SysUtils,
  UDataSetType,
  UConstants,
  UDBConstants,
  VCL.Dialogs,
  UAbstractComponent,
  UErrorHandlingOperations;

{ TWeatherEventsManager }

procedure TWeatherEventsManager.CreateMemberObjects;
const OPNAME = 'TWeatherEventsManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FData := TWeatherEvents.Create(FAppModules);
    FWeatherEventsTabSheet  := nil;
    FWeatherEventsValidator := nil;
    if (FAppModules.MainForm <> nil) and (FAppModules.MainForm.MainForm <> nil) then
    begin
      FWeatherEventsValidator := TWeatherEventsValidator.Create(nil,FAppModules);
      FWeatherEventsTabSheet  := TWeatherEventsTabSheet.Create(nil, FAppModules);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWeatherEventsManager.DestroyMemberObjects;
const OPNAME = 'TWeatherEventsManager.DestroyMemberObjects';
begin
  try
    FreeAndNil(FData);
    FreeAndNil(FWeatherEventsValidator);
    FreeAndNil(FWeatherEventsTabSheet);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWeatherEventsManager.Initialise: boolean;
const OPNAME = 'TWeatherEventsManager.Initialise';
begin
  Result := FALSE;
  try
    if (Assigned(FAppModules.MainForm()) AND Assigned(FAppModules.MainForm.PageControl)) then
    begin
      if Assigned(FWeatherEventsTabSheet) then
        FWeatherEventsTabSheet.Initialise;
      if Assigned(FWeatherEventsValidator) then
        FWeatherEventsValidator.Initialise;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWeatherEventsManager.AddTabsheetToPageControl;
const OPNAME = 'TWeatherEventsManager.AddTabsheetToPageControl';
begin
  try
    if (Assigned(FAppModules.MainForm()) AND
        Assigned(FAppModules.MainForm.PageControl) AND
        Assigned(FWeatherEventsTabSheet) AND
        (NOT Assigned(FWeatherEventsTabSheet.PageControl))) then
    begin
      FWeatherEventsTabSheet.PageControl := FAppModules.Mainform.PageControl;
      if Assigned(FWeatherEventsValidator) then
      begin
        FWeatherEventsValidator.Panel.Parent := FWeatherEventsTabSheet;
        FWeatherEventsValidator.Panel.Align  := alClient;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWeatherEventsManager.RemoveTabsheetFromPageControl;
const OPNAME = 'TWeatherEventsManager.RemoveTabsheetFromPageControl';
begin
  try
    if Assigned(FWeatherEventsTabSheet) then
      FWeatherEventsTabSheet.PageControl := nil;
    if Assigned(FWeatherEventsValidator) then
      FWeatherEventsValidator.Panel.Parent := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWeatherEventsManager.LanguageHasChanged: boolean;
const OPNAME = 'TWeatherEventsManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    if Assigned(FWeatherEventsTabSheet) then
      FWeatherEventsTabSheet.LanguageHasChanged;
    if Assigned(FWeatherEventsValidator) then
      FWeatherEventsValidator.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWeatherEventsManager.StudyDataHasChanged (AContext    : TChangeContext;
                                                    AFieldName  : string;
                                                    AOldValue   : string;
                                                    ANewValue   : string): boolean;
const OPNAME = 'TWeatherEventsManager.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWeatherEventsManager.StudyHasChanged: boolean;
const OPNAME = 'TWeatherEventsManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    if (Assigned(FWeatherEventsTabSheet) AND
        Assigned(FWeatherEventsTabSheet.PageControl) AND
        Assigned(FWeatherEventsValidator)) then
        FWeatherEventsValidator.StudyHasChanged;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWeatherEventsManager.TabHasChanged;
const OPNAME = 'TWeatherEventsManager.TabHasChanged';
begin
  try
//    if Assigned(FWeatherEventsValidator) then
//      FWeatherEventsValidator.PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWeatherEventsManager.ShowWeatherEvents (AStartDateTime : TDateTime;
                                                   AEndDateTime   : TDateTime;
                                                   const AArea    : WideString);
const OPNAME = 'TWeatherEventsManager.ShowWeatherEvents';
var
  lForm      : TAbstractForm;
  lValidator : TWeatherEventsValidator;
begin
  try
    lForm := TAbstractForm.CreateWithoutDFM(nil,FAppModules);
    try
      lForm.Initialise;
      lForm.LanguageHasChanged;
      lValidator := TWeatherEventsValidator.Create(LForm,FAppModules);
      try
        lValidator.Panel.Parent  := lForm;
        lValidator.Panel.Align   := alClient;
        lValidator.Panel.Visible := TRUE;
        lValidator.Initialise;
        lValidator.SetDateRange(AStartDateTime, AEndDateTime);
        lValidator.PopulateDataViewer;
        lValidator.LanguageHasChanged;
        lForm.ShowModal;
      finally
        lValidator.Free;
      end;
    finally
      LForm.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWeatherEventsManager.ProcessMetaDataEvent : boolean;
const OPNAME = 'TWeatherEventsManager.ProcessMetaDataEvent';
begin
  Result := FALSE;
  try
    if (FAppModules.MainForm.PageControl.ActivePage = FWeatherEventsTabSheet) then
      if Assigned(FWeatherEventsValidator) then
        Result := FWeatherEventsValidator.ProcessMetaDataEvent;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWeatherEventsManager.Data : IWeatherEvents;
const OPNAME = 'TWeatherEventsManager.Data';
begin
  try
    Result := FData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
