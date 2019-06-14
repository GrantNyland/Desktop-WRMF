//
//  UNIT      : Contains TConfiguration Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/16
//  COPYRIGHT : Copyright © 2002 DWAF
//
unit UConfiguration;

interface

uses
  Classes,
  IniFiles,
  UUtilities,
  UAbstractObject;

type
  TIniFileType = (itApplicationConfiguration, itViewSettings);
  TConfiguration = class(TAbstractConfiguration)
  protected
    FIniFileType: TIniFileType;
    FIniFile: TIniFile;
    FIniFilename: string;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    constructor Create(AIniFileType: TIniFileType);
    function FileName: string; override;
    function ReadSectionValues(const Section: string; Strings: TStrings):boolean; override;
    function ReadSection(const Section: string; Strings: TStrings):boolean; override;
    function ReadInteger(const ASection, AIdent: string; const ADefault: integer): integer; override;
    function ReadString(const ASection, AIdent, ADefault: string): string; override;
    procedure WriteInteger(const ASection, AIdent: string; const AValue: integer); override;
    procedure WriteString(const ASection, AIdent, AValue: string); override;
    procedure DeleteKey(const ASection, AIdent: String); override;
    procedure LoadFormView(AForm: TObject); override;
    procedure SaveFormView(AForm: TObject); override;
    procedure ResetToDefaults; override;
  end;

implementation

uses
  SysUtils,
  Windows,
  VCL.Forms,
  UErrorHandlingOperations;

constructor TConfiguration.Create(AIniFileType: TIniFileType);
const OPNAME = 'TConfiguration.Create';
begin
  try
    FIniFileType := AIniFileType;
    FIniFilename := GetAppDataLocalDir;// //ExtractFilePath(ApplicationExeName);
    case AIniFileType of
      itApplicationConfiguration :
        begin
          FIniFilename := ExtractFilePath(Application.ExeName) + ExtractFileName(Application.ExeName);
          FIniFilename := ChangeFileExt(FIniFilename, '.ini');
        end;
      itViewSettings :
        begin
          FIniFilename := ExtractFilePath(Application.ExeName) + 'Logs\ViewSettings.ini';     //ExtractFilePath(ApplicationExeName) + 'Logs\ViewSettings.ini';
        end;
    else
      raise Exception.CreateFmt('Unknown ini file type [%d].', [integer(AIniFileType)]);
    end;
    inherited Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TConfiguration.CreateMemberObjects;
const OPNAME = 'TConfiguration.CreateMemberObjects';
begin
  try
    FIniFile := TIniFile.Create(FIniFilename);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TConfiguration.DestroyMemberObjects;
const OPNAME = 'TConfiguration.DestroyMemberObjects';
begin
  try
    FreeAndNil(FIniFile)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TConfiguration.ResetToDefaults;
const OPNAME = 'TConfiguration.ResetToDefaults';
begin
  try
    case FIniFileType of
      itApplicationConfiguration : ; // Do nothing.
      itViewSettings :
        begin
          FreeAndNil(FIniFile);
          SysUtils.DeleteFile(FIniFilename);
          FIniFile := TIniFile.Create(FIniFilename);
        end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TConfiguration.FileName: string;
const OPNAME = 'TConfiguration.FileName';
begin
  Result := '';
  try
    Result := FIniFile.FileName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TConfiguration.ReadSection(const Section: string; Strings: TStrings): boolean;
const OPNAME = 'TConfiguration.ReadSection';
begin
  Result := False;
  try
    if Assigned(Strings) then
    begin
      FIniFile.ReadSection(Section,Strings);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TConfiguration.ReadSectionValues(const Section: string;
  Strings: TStrings): boolean;
const OPNAME = 'TConfiguration.ReadSectionValues';
begin
  Result := False;
  try
    if not Assigned(Strings) then
    Exit;
    FIniFile.ReadSectionValues(Section,Strings);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TConfiguration.ReadInteger(const ASection, AIdent: string;
  const ADefault: integer): integer;
const OPNAME = 'TConfiguration.ReadInteger';
begin
  Result := ADefault;
  try
    Result := FIniFile.ReadInteger(ASection, AIdent, ADefault);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TConfiguration.ReadString(const ASection, AIdent,
  ADefault: string): string;
const OPNAME = 'TConfiguration.ReadString';
begin
  try
    Result := FIniFile.ReadString(ASection, AIdent,ADefault)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TConfiguration.WriteInteger(const ASection, AIdent: string; const AValue: integer);
const OPNAME = 'TConfiguration.WriteInteger';
begin
  try
    FIniFile.WriteInteger(ASection, AIdent, AValue);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TConfiguration.WriteString(const ASection, AIdent, AValue: string);
const OPNAME = 'TConfiguration.WriteString';
begin
  try
    FIniFile.WriteString(ASection, AIdent, AValue);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TConfiguration.DeleteKey(const ASection, AIdent: String);
const OPNAME = 'TConfiguration.DeleteKey';
begin
  try
    FIniFile.DeleteKey(ASection, AIdent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TConfiguration.LoadFormView(AForm: TObject);
const
  OPNAME = 'TConfiguration.LoadFormView';
  C_IniSection_FormViews = 'FormViews';
var
  LViewName, LCommaText: string;
  LFields: TStringList;
begin
  try
    if Assigned(AForm) then
    begin

      // Construct the view name from the form's class name.
      LViewName := AForm.ClassName;

      // Read the data from the ini file.
      // Apply a standard size if the data is not found.
      LCommaText := ReadString(C_IniSection_FormViews, LViewName, '0,100,50,440,380');

      // Attempt to extract the pixel values. Abort on error.  V,L,T,R,B
      LFields := TStringList.Create;
      try
        LFields.CommaText := LCommaText;
        TForm(AForm).Left        := StrToInt(LFields[1]);
        TForm(AForm).Top         := StrToInt(LFields[2]);
        TForm(AForm).Width       := StrToInt(LFields[3]) - TForm(AForm).Left;
        TForm(AForm).Height      := StrToInt(LFields[4]) - TForm(AForm).Top;

        // Do the window state last.
        TForm(AForm).WindowState := TWindowState(StrToInt(LFields[0]));

      // Destroy string list.
      finally
        LFields.Free;
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TConfiguration.SaveFormView(AForm: TObject);
const
  OPNAME = 'TConfiguration.SaveFormView';
  C_IniSection_FormViews = 'FormViews';
var
  LViewName: string;
  LWindowPlacement: WINDOWPLACEMENT;
begin
  try
    if Assigned(AForm) then
    begin

      // Allocate enough memory for the data.
      LWindowPlacement.length := sizeof(WINDOWPLACEMENT);

      // Get the correct sizes to store.
      if GetWindowPlacement(TForm(AForm).Handle, @LWindowPlacement) then
      begin

        // Construct the view name from the form's class name.
        LViewName := AForm.ClassName;

        // V,L,T,R,B
        FIniFile.WriteString(C_IniSection_FormViews, LViewName,
          IntToStr(Integer(TForm(AForm).WindowState)) + ',' +
          IntToStr(LWindowPlacement.rcNormalPosition.Left) + ',' +
          IntToStr(LWindowPlacement.rcNormalPosition.Top) + ',' +
          IntToStr(LWindowPlacement.rcNormalPosition.Right) + ',' +
          IntToStr(LWindowPlacement.rcNormalPosition.Bottom));
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
