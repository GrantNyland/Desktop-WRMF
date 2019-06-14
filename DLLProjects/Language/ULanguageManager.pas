//
//
//  UNIT      : Contains TLanguageManager Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/22
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit ULanguageManager;

interface

uses
  ULanguageList,
  UAbstractObject,
  UAbstractModelObjects;

type
  TLanguageManager = class(TAbstractLanguageManager)
  protected
    FLanguageList: TLanguageList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function Initialise: boolean; override;
    function Language: TAbstractLanguage; override;
    function SetLanguage(ANewLanguage: string): boolean; override;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

procedure TLanguageManager.CreateMemberObjects;
const OPNAME = 'TLanguageManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FLanguageList := TLanguageList.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TLanguageManager.DestroyMemberObjects;
const OPNAME = 'TLanguageManager.DestroyMemberObjects';
begin
  try
    FreeAndNil(FLanguageList);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLanguageManager.Initialise: boolean;
const OPNAME = 'TLanguageManager.Initialise';
begin
  Result := False;
  try
    if FLanguageList.Initialise then
      if FLanguageList.SetCurrentLanguage('ENG') then
        Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLanguageManager.Language: TAbstractLanguage;
const OPNAME = 'TLanguageManager.Language';
begin
  Result := nil;
  try
    Result := FLanguageList.CurrentLanguage;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TLanguageManager.SetLanguage(ANewLanguage: string): boolean;
const OPNAME = 'TLanguageManager.SetLanguage';
begin
  Result := False;
  try
    Result := FLanguageList.SetCurrentLanguage(ANewLanguage);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
