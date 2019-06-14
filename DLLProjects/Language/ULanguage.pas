//
//  UNIT      : Contains TLanguage Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/02/07
//  COPYRIGHT : Copyright © 2002 DWAF
//
unit ULanguage;

interface

uses
  Classes,
  UAbstractObject;

type
  TLanguage = class(TAbstractLanguage)
  protected
    FLanguageID: string;
    FDescription: string;
    FIndex, FText: TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetLanguageID: string; override;
    function GetDescription: string; override;
  public
    procedure Clear;
    function Count: integer;
    procedure AddTextItem(AContext, AConstant, AText: string);
    function GetString(AStringConstant: string): string; override;
    property LanguageID: string read GetLanguageID write FLanguageID;
    property Description: string read GetDescription write FDescription;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

procedure TLanguage.CreateMemberObjects;
const OPNAME = 'TLanguage.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FLanguageID  := '';
    FDescription := '';
    FIndex := TStringList.Create;
    FText  := TStringList.Create;
    FIndex.Sorted := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLanguage.DestroyMemberObjects;
const OPNAME = 'TLanguage.DestroyMemberObjects';
begin
  try
    Clear;
    FreeAndNil(FIndex);
    FreeAndNil(FText);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLanguage.Clear;
const OPNAME = 'TLanguage.Clear';
begin
  try
    FIndex.Clear;
    FText.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLanguage.GetLanguageID: string;
const OPNAME = 'TLanguage.GetLanguageID';
begin
  Result := '';
  try
    Result := FLanguageID;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLanguage.GetDescription: string;
const OPNAME = 'TLanguage.GetDescription';
begin
  Result := '';
  try
    Result := FDescription;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLanguage.Count;
const OPNAME = 'TLanguage.Count';
begin
  Result := 0;
  try
    Result := FIndex.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLanguage.GetString(AStringConstant: string): string;
const OPNAME = 'TLanguage.GetString';
var LStringConstantIndex, LStringPosition: integer;
begin
  Result := AStringConstant + ' not found';
  try
    LStringConstantIndex := FIndex.IndexOf(UpperCase(Trim(AStringConstant)));
    if (LStringConstantIndex >= 0) then
    begin
      LStringPosition := integer(FIndex.Objects[LStringConstantIndex]);
      Result := FText[LStringPosition];
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLanguage.AddTextItem(AContext, AConstant, AText: string);
const OPNAME = 'TLanguage.AddTextItem';
var LIndexValue: string;
begin
  try

    // Clean the data up.
    AContext  := UpperCase(Trim(AContext));
    AConstant := UpperCase(Trim(AConstant));
    //AText     := Trim(AText);

    // Construct the item index.
    if (FLanguageID <> '') and (AContext <> '') then
    begin
      LIndexValue := AContext;
      if (AConstant <> '') then
        LIndexValue := LIndexValue + '.' + AConstant;

      // Add the item to the lists.
      if (FIndex.IndexOf(LIndexValue) < 0) then
      begin
        FIndex.AddObject(LIndexValue, TObject(FText.Count));
        FText.Add(AText);
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
