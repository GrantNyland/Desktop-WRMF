unit UUtilities;

interface
uses
  //  Delphi VCL
  Classes,VCL.Dialogs,sysutils, windows, VCL.Forms,

  //  DWAF VCL
  UConstants,
  UAbstractObject,
  VoaimsCom_TLB,
  UBasicObjects;

  function DeleteDirectory(const Directory: TFileName):boolean;
  procedure ExtractErrorsAndColumns(AErrorMessage: WideString; AErrors, AColumns: TStrings);
  procedure GetFileModifiedTime(strFileName:string; var T: FILETIME);
  function ExtractFirstSubstring(var AInputString : String):String;
  function GatLastSubstring(AInputString : String):String;
  function ExtractLastSubstring(AInputString : String):String;
  function ExtractLengthFirstSubstring(ALength:Integer;var AInputString : String):String;
  function ExtractDelemetedFirstSubstring(ADelimeter: char;var AInputString : String):String;
  function GetSubstring(AInputString : String;AStartPos,ALength:Integer):String;
  function CopyStringFromRight(AString: string; ANumberOfCharsToBeCopied: integer): string;
  function PadInt(ATheInt:TInteger):String;
  function PadString(ATheString: TString):String;
  function PadLeftString(AString: String; ALength : integer):String;
  function PadChar(ATheChar: TChar):String;
  function PadStringWhithChar(ATheString: TString; ATheChar: char): string;
  //function PadReal(ATheReal:TSingle):String;
  function PadDouble(ATheDouble:TDouble):String;
  function TrimChars(AValue: string; AChar: Char): string;
  function LeftTrimChars(AValue: string; AChar: Char): string;
  function RightTrimChars(AValue: string; AChar: Char): string;
  function SmartPadDouble(AValue:TDouble;AAlwaysRemoveTrailingZeros : boolean=False;ALeaveLeftBlank : boolean=False):String;
  function ReturnSubstringFromChar(ANameString: string;AChar: char): string;

  function SearchFiles(ASearchPath: string;AFileNamesContainer: TStrings):boolean;
  function DeleteMultipleFiles(ASearchPathAndCriteria: string):boolean;
  function SearchForDirectories(AParentDirectory: string;AFileNamesContainer: TStrings):boolean;
  function GetHydrologyFileType(AAppModules: TAppModules;AHydrologyFileName: string): Integer;
  function GetOutputFileType(AFileName: string): TOutputFileType;
  function CompareDouble(AValue,BValue: double; APrecesion: integer): boolean;
  function FilePathIsDosCompatible(AAppModules: TAppModules;AFileOrPathName: string): boolean;
  function GetTempDir: string;
  function GetAppDataLocalDir: string;
  function GetWindowsDir: TFileName;
  function LoggedOnUserName: string;
  function ExecAndWait(AExecuteFile, AParamString : string): boolean;
  function RegisterOCX(ADllFileName: string): boolean;
  function UnRegisterOCX(ADllFileName: string): boolean;
  function IsPintableCharacter(AChar: Char): boolean;
  function GetMinMaxYearFromHydrologyFiles(const AFileName: string; var AStartYear, AEndYear: integer): boolean;
  function CutErrorsAndColumns(AErrorMessage: WideString; AErrors, AColumns: TStrings): string;
  function SelectionComboBox(const ACaption, APrompt, ACommaTextItems: string; ADefault: integer): integer;
  function SelectionComboQuery(const ACaption, APrompt, ACommaTextItems: string; var Value: integer): Boolean;
  function SelectionRadio(const ACaption, APrompt, ACommaTextItems: string; ADefault: integer): integer;
  function SelectionRadioQuery(const ACaption, APrompt, ACommaTextItems: string; var Value: integer): Boolean;
  function GetGeneralFormCaption(AAppModules: TAppModules): string;
  function GetHydrologyFileNames(ASearchPathAndCriteria: string; AFileNames : TStrings): boolean;
  function GetAnalysisYearsFromParamDataFile(const AParamFileName: string ; var ANumberOfYears, AStartYearGregorian: integer) : boolean;
  function GetSpecialFolder(const FolderId: integer): string;
  function GetUserSettingsFolder: string;
  function GetReportFileName(AStudyName,ACategory, AFileName: string): string;
  function NetworkDiagramsPath : string;
  function StudyDocumentsPath : string;
  function DeploymentPath : string;
  function GISCoversDirectory:string;
  function ChopCharacters (AName : string) : string;
  function SmartFloatFormat(AValue : double; ALength,AMinDecimals : integer) : string;
  function SmartFloatFormatForFiles(AValue : double; ALength, AMaxDecimals: integer) : string;
  function ConvertXValueToPlottingBase(AXValue: double;APeriodLength,APlottingBase: integer): double;
  function Distance(Pt1 : TPoint; Pt2 : TPoint) : Double;
  function Angle(Pt1 : TPoint; Pt2 : TPoint) : Double;
  function UnCommaTextString(ACommaTextString: string): string;
  function CommaTextString(AString: string): string;
  function StringsItemsCount(AContainer: TStrings): integer; overload;
  function StringsItemsCount(ACommaTextData: String): integer;  overload;
  function UnCompressCommatextString(ACommaTextData : string) : string;
  function CompressCommatextString(ACommaTextData : string) : string;
  function UpdateCompressCommaTextString(AIndex:integer;AUpdateValue: string;var ACommaTextData : string) : boolean;
  function GetCompressedCommaTextIndexValue(AIndex:integer;ACommaTextData : string) : string;
  function ChangeSizeCommatextString(ANewSize:integer;AAddDefaultValue: string;var ACommaTextData : string) : boolean;
  function FileCreationDate(AFileName: string): TDateTime;
  function FileLastAccessDate(AFileName: string): TDateTime;
  function FileLastWriteDate(AFileName: string): TDateTime;
  function GetCommaDelimetedValue(var AInputStr: string): string;
  function UpdateGISShapeFilePath(AGLFFileName: string):boolean;
  function getRectange(ALeft, ATop, AWidth, AHeight : integer; AAArr,AyArr : array of double) : boolean;
  function ExpandStudyKeysToStrings(AStudyKeys: string; AContainer: TStrings): boolean;
  function ViewModelDataContextDataCommaText(AViewName: string; AModelElementID: integer;AModelElementName : string=''; ANodeIndex: Integer=-1): string;
  function WRMFMessageDialog(const Msg: string; DlgType: TMsgDlgType;Buttons: TMsgDlgButtons; Captions: array of string): Integer;
  function ErrorSeverity(var AErrorMsg: string): integer;
  function GetFileSize(const S: string): Int64;
  function NumberOfMonthsBetween(AStartDate, AEndDate: TDatetime): Integer;
  function GetElementDataTypeBySubNodesDatasetID(ASubNodesDatasetID : string) : TOutputDataType;
  function DoubleArrayToCommaText(AArray : array of double; ANumberOfDecimals : integer): string;
  function IntegerArrayToCommaText(AArray : array of integer; ALength : integer=0): string;
  procedure SortFloatArray(var AArray: TOneDimensionDoubleArray; AOrder:TSortOrder);
  procedure SortIntegerArray(var AArray: TOneDimensionIntegerArray; AOrder:TSortOrder);
  procedure QuickSortFloatArray(var AList: TOneDimensionDoubleArray; ALeft, ARight : integer);
  procedure QuickSortIntegerArray(var AList: TOneDimensionIntegerArray; ALeft, ARight : integer);
  procedure Save1DIntegerArrayToStrings(AContainer: TStrings; AArrayName :string; AArray: TOneDimensionIntegerArray;ALength1: integer);
  procedure Save2DIntegerArrayToStrings(AContainer: TStrings; AArrayName :string; AArray: TTwoDimensionIntegerArray;ALength1,ALength2: integer);
  procedure Save3DIntegerArrayToStrings(AContainer: TStrings; AArrayName :string; AArray: TThreeDimensionIntegerArray;ALength1,ALength2,ALength3: integer);
  procedure Save1DDoubleArrayToStrings(AContainer: TStrings; AArrayName :string;AArray: TOneDimensionDoubleArray;ALength1: integer;    ANumberOfDecimals : integer=3);
  procedure Save2DDoubleArrayToStrings(AContainer: TStrings; AArrayName :string;AArray: TTwoDimensionDoubleArray;ALength1,ALength2: integer;    ANumberOfDecimals : integer=3);
  procedure Save3DDoubleArrayToStrings(AContainer: TStrings; AArrayName :string;AArray: TThreeDimensionDoubleArray;ALength1,ALength2,ALength3: integer;  ANumberOfDecimals : integer=3);
  procedure RestoreFormProperties(AForm: TForm);
  procedure SaveFormProperties(AForm: TForm);
  function ExtractChannelNameFromComment(AComment:string):string;
  function RemoveTrailingMemoChars(ASource:string):string;
  function ExtractDBFileNameFromADOConnectionStr(AValue: string): string;
  function StringToStringList(AString:String; AStrings: TStrings;ADelimeter: char = ','):boolean;
  function FileCopy(const ASourceFileName, AdestinationFileName: string): boolean;
  function GetHydrologyMonth(const AMonth : integer):integer;
  function ConvertHydrologicalMonthToCalendarMonth(ACalendarStartMonth,AHydrologicalYear, AHydrologicalMonth: integer): integer;
  function ConvertHydrologicalYearToCalendarYear(ACalendarStartMonth,AHydrologicalYear, AHydrologicalMonth: integer): integer;
  function ConvertCalendarMonthToHydrologicalMonth(ACalendarStartMonth,ACalendarYear,ACalendarMonth: integer): integer;
  function ConvertCalendarYearToHydrologicalYear(ACalendarStartMonth,ACalendarYear,ACalendarMonth: integer): integer;
  function MWGCompareDouble(ADouble,BDouble : double):boolean;

  procedure DoNothing;
implementation

uses
  {$WARN UNIT_PLATFORM OFF}
  vcl.FileCtrl,
  {$WARN UNIT_PLATFORM ON}
  System.UITypes,
  Math,
  ShlObj,
  vcl.Consts,
  shellapi,
  StrUtils,
  vcl.StdCtrls,
  vcl.Graphics,
  vcl.Controls,
  vcl.ExtCtrls,
  System.DateUtils,
  //System.SysUtils,
  IniFiles,
  UModuleVersionOperations,
  UErrorHandlingOperations;

function GetCommaDelimetedValue(var AInputStr: string): string;
const OPNAME = 'UUtilities.GetCommaDelimetedValue';
begin
  Result := '';
  try
    if Pos(',',AInputStr) = 1 then
    begin
     Result := '';
     Delete(AInputStr,1,Pos(',',AInputStr));
     Exit;
    end;
    if Pos(',',AInputStr) = 0 then
    begin
      Result := AInputStr;
      Delete(AInputStr,1,Length(AInputStr));
    end
    else
    begin
      Result := Copy(AInputStr,1, Pos(',',AInputStr)-1);
      Delete(AInputStr,1,Pos(',',AInputStr));
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function GetGeneralFormCaption(AAppModules: TAppModules): string;
const  OPNAME = 'UUtilities.GetGeneralFormCaption';
var
  LModelName: string;
  ls_Module: array[0..100] of char; // Contains the name of the DLL.
  ls_Version : string; //File version
begin
  Result := 'WRMF';
  try
    // Get the name of the current DLL
    ls_Module[0] := #0;
    GetModuleFileName(HInstance, ls_Module, SizeOf(ls_Module));

    ls_Version := Trim(GetModuleVersion(ls_Module));
    if(ls_Version <> '') then
      Result := Result + ' Ver '+ ls_Version;

    LModelName := '';
    if Assigned(AAppModules) and Assigned(AAppModules.Model()) then
      LModelName := AAppModules.Language.GetString('FormCaption.' + Trim(AAppModules.Model.ModelName));

    if(LModelName <> '') then
      Result := Result +  ' ('+ LModelName + ')';

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function ExtractLastSubstring(AInputString : String):String;
const OPNAME = 'UUtilities.ExtractFirstSubstring';
Var
  LIndex: integer;
Begin
  Result := '';
  try
    AInputString := Trim(AInputString);
    if(AInputString <> '') then
    begin
      for LIndex := Length(AInputString) downto 1 do
      begin
        if(AInputString[LIndex] = ' ') then Exit;
        Result := AInputString[LIndex] + Result;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function GatLastSubstring(AInputString : String):String;
//this will cut a piece out of the input string if it exists
//it will trim it and pad it to a single space if it is empty
const OPNAME = 'UUtilities.ExtractFirstSubstring';
Var
  LPos: integer;
Begin
  Result := '';
  try
    AInputString := Trim(AInputString);
    if(AInputString <> '') then
    begin
      LPos := Length(AInputString);
      while (LPos > 0) do
      begin
        if(AInputString[LPos] = ' ') then
          Break;
        Result := AInputString[LPos]+Result;
        LPos := LPos -1;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function ExtractFirstSubstring(var AInputString : String):String;
//this will cut a piece out of the input string if it exists
//it will trim it and pad it to a single space if it is empty
const OPNAME = 'UUtilities.ExtractFirstSubstring';
Var
  LTempString : String;
  LPos: integer;
  LPosTab: integer;
Begin
  Result := '';
  try
    AInputString := Trim(AInputString);
    if(AInputString <> '') then
    begin
      if(AInputString[1] = '"') then
      begin
        Delete(AInputString,1,1);
        LPos := Pos('"',AInputString);
        if(LPos = 1) then
          Delete(AInputString,1,1)
        else
        if(LPos > 1) then
        begin
          Result := '"' + Copy(AInputString,1,LPos);
          Delete(AInputString,1,LPos);
        end
        else
        begin
          Result := '"' + AInputString + '"';
          AInputString := '';
        end;
      end
      else
      begin
        if(AInputString[1] = '''') then
        begin
          Delete(AInputString,1,1);
          LPos := Pos('''',AInputString);
          if(LPos = 1) then
            Delete(AInputString,1,1)
          else
          if(LPos > 1) then
          begin
            Result := '''' + Copy(AInputString,1,LPos);
            Delete(AInputString,1,LPos);
          end
          else
          begin
            Result := '''' + AInputString + '''';
            AInputString := '';
          end;
        end
        else
        begin
          LPosTab := Pos(Char(9),AInputString);
          LPos := Pos(' ',AInputString);
          if(LPos > 1) and (LPosTab > 1) then
            LPos := Min(LPos,LPosTab);
          if(LPos > 1) then
          begin
            LTempString := Copy(AInputString,1,LPos-1);
            AInputString := Copy(AInputString,LPos,Length(AInputString));
          end
          else
          begin
            LTempString := AInputString;
            AInputString := '';
          end;
          Result := LTempString;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function ExtractDelemetedFirstSubstring(ADelimeter: char;var AInputString : String):String;
//this will cut a piece out of the input string if it exists
//it will trim it and pad it to a single space if it is empty
const OPNAME = 'UUtilities.ExtractDelemetedFirstSubstring';
Var
  LTempString : String;
  LPos: integer;
Begin
  Result := '';
  try
    AInputString := Trim(AInputString);
    if(AInputString <> '') then
    begin
      if (AInputString[1] = ADelimeter) then
        Delete(AInputString,1,1);
      LTempString := '';
      LPos := Pos(ADelimeter,AInputString);
      if(LPos = 1) then
        Delete(AInputString,1,1)
      else
      if(LPos > 1) then
      begin
        LTempString := Copy(AInputString,1,LPos-1);
        AInputString := Copy(AInputString,LPos+1,Length(AInputString));
      end
      else
      begin
        LTempString := AInputString;
        AInputString := '';
      end;
      Result := LTempString;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function ExtractLengthFirstSubstring(ALength:Integer;var AInputString : String):String;
//this will cut a piece out of the input string if it exists
const OPNAME = 'UUtilities.ExtractLengthFirstSubstring';
Begin
  Result := '';
  try
    if(AInputString <> '')  and(ALength > 0)then
    begin
      Result := Copy(AInputString,1,ALength);
      AInputString := Copy(AInputString,ALength+1,Length(AInputString));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function GetSubstring(AInputString : String;AStartPos,ALength:Integer):String;
//this will cut a piece out of the input string if it exists
//it will trim it and pad it to a single space if it is empty
const OPNAME = 'UUtilities.GetSubstring';
Var
  LTempString : String;
Begin
  Result := '';
  try
    If AStartPos<=Length(AInputString) then
    begin
      If ((AStartPos + ALength) > Length(AInputString)) then
        ALength:=Length(AInputString) - AStartPos + 1;

      LTempString:=Copy(AInputString,AStartPos,ALength);
      //LTempString:=Trim(LTempString);

      //If LTempString = '' then
      //  LTempString := ' ';
      Result := LTempString;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function PadInt(ATheInt:TInteger):String;
const OPNAME = 'UUtilities.PadInt';
Var
  LTmpStr : ShortString;
Begin
  Result := '';
  try
    while (Length(Result) < ATheInt.FLength) do
      Result := Result + ' ';
    if not(ATheInt.FInitalised) then Exit;

    Str(ATheInt.FData:ATheInt.FLength,LTmpStr);
    if(Length(LTmpStr) > ATheInt.FLength) then
        raise Exception.Create('Integer('+string(LTmpStr)+') is too big. Its maximum length is '+IntToStr(ATheInt.FLength));
    Result := string(LTmpStr);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function PadString(ATheString: Tstring):String;
const OPNAME = 'UUtilities.PadString';
Var
  Lformat,
  LTmpStr : String;
Begin
  Result := '';
  try
    if (not ATheString.FInitalised) or (ATheString.FLength < 1) then Exit;

    if ATheString.FDefaultPadding then
      LFormat := '%-'+IntToStr(ATheString.FLength)+'s'
    else
      LFormat := '%'+IntToStr(ATheString.FLength)+'s';

    LTmpStr := Format(LFormat,[ATheString.FData]);
    if(Length(LTmpStr) > ATheString.FLength) then
        raise Exception.Create('String('+LTmpStr+') is too big. Its maximum length is '+IntToStr(ATheString.FLength));
    Result := LTmpStr;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function PadLeftString(AString: String; ALength : integer):String;
const OPNAME = 'UUtilities.PadLeftString';
Begin
  Result := AString;
  try
    while (Length(Result) < ALength) do
      Result := ' ' + Result;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function PadStringWhithChar(ATheString: TString; ATheChar: char): string;
const OPNAME = 'UUtilities.PadStringWhithChar';
var
  LIndex : integer;
Begin
  Result := '';
  try
    Result := PadString(ATheString);
    if(Length(Result) > 2) then
    begin
      Result := ATheChar + Result;
      Delete(Result,Length(Result),1);
      for LIndex := Length(Result) downto 2 do
      begin
        if(Result[LIndex] <> ' ') then
        begin
          Insert(ATheChar,Result,LIndex+1);
          Delete(Result,Length(Result),1);
          Break;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function PadChar(ATheChar: TChar):String;
const OPNAME = 'UUtilities.PadChar';
Var
  LCount: integer;
  LTmpStr : String;
Begin
  Result := '';
  try
    while (Length(Result) < ATheChar.FLength) do
      Result := Result + ' ';
    if not(ATheChar.FInitalised) then Exit;

    LTmpStr := ATheChar.FData;
    for LCount := 2 to ATheChar.FLength do
    begin
      if ATheChar.FDefaultPadding then
        LTmpStr := LTmpStr + ' '
      else
        LTmpStr := ' ' + LTmpStr;
    end;

    if(Length(LTmpStr) > ATheChar.FLength) then
        raise Exception.Create('String('+LTmpStr+') is too big. Its maximum length is '+IntToStr(ATheChar.FLength));
    Result := LTmpStr;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{function PadReal(ATheReal:TSingle):String;
const OPNAME = 'UUtilities.PadReal';
Var
  LTmpStr : String;
  LPos,
  LCount,
  LDec: integer;
Begin
  Result := '';
  try
    while (Length(Result) < ATheReal.FLength) do
      Result := Result + ' ';
    if not(ATheReal.FInitalised) then Exit;

    LDec := ATheReal.FDecimal;
    if((ATheReal.FDecimal = 0) and ATheReal.ShowDecimalPoint) then
      LDec := 1;
    Str(ATheReal.FData:ATheReal.FLength:LDec,LTmpStr);

    If ((ATheReal.FDecimal = 0) and ATheReal.ShowDecimalPoint) then
     LTmpStr:=' '+ Copy(LTmpStr,1,length(LTmpStr)-1);

    //Remove trailing zeros
    if(Length(LTmpStr) > ATheReal.FLength) then
    begin
      if(Pos('.',LTmpStr) > 0) then
      begin
        LPos := Length(LTmpStr);
        for LCount := Length(LTmpStr) downto 1 do
        begin
          LPos := LCount;
          if(LTmpStr[LCount] <> '0') then Break;
          if(LTmpStr[LCount] = '.')  then Break;
        end;
        LTmpStr := Copy(LTmpStr,1,LPos);
        while (Length(LTmpStr) < ATheReal.FLength) do
        begin
          if ATheReal.FDefaultPadding then
            LTmpStr := ' '+ LTmpStr
          else
            LTmpStr := LTmpStr + ' ';
        end;
      end;
    end;

    if(Length(LTmpStr) > ATheReal.FLength) then
        raise Exception.Create('Float('+LTmpStr+') is too big. Its maximum length is '+IntToStr(ATheReal.FLength));
    Result := LTmpStr;

    {if ARemoveRightZero then
    begin
      LCopyStr :=  LTmpStr;
      for LPos := Length(LTmpStr) downto 1 do
      begin
        if(LTmpStr[LPos] = '.') or (LTmpStr[LPos] <> '0') then
        Break;
        LCopyStr := ' ' + Copy(LCopyStr,1,Length(LCopyStr)-1);
        Break;
      end;
      LTmpStr :=  LCopyStr;
    end;

    if ARemoveLeftZeros then
    begin
      for LPos := 1 to Length(LTmpStr) do
      begin
        if(LTmpStr[LPos] <> '0') and (LTmpStr[LPos] <> ' ') and (LTmpStr[LPos] <> '.') then
        Break;
        if(LTmpStr[LPos] = '.') then
        begin
          for LPosDown := LPos-1 downto 1 do
            LTmpStr[LPosDown] := ' ';
          Break;
        end;
      end;
    end;//end comment
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

function SmartPadDouble(AValue:TDouble;AAlwaysRemoveTrailingZeros : boolean=False;ALeaveLeftBlank : boolean=False):String;
const OPNAME = 'UUtilities.SmartPadDouble';
Var
  LTmpStr : ShortString;
  LPos,
  LCount,
  LDec: integer;
Begin
  Result := '';
  try
    if ALeaveLeftBlank then AValue.FLength := AValue.FLength - 1;
    try
      while (Length(Result) < AValue.FLength) do
        Result := Result + ' ';
      if not(AValue.FInitalised) then Exit;

      LDec := AValue.FDecimal;
      if((AValue.FDecimal = 0) and AValue.ShowDecimalPoint) then
        LDec := 1;
      Str(AValue.FData:AValue.FLength:LDec,LTmpStr);

      If ((AValue.FDecimal = 0) and AValue.ShowDecimalPoint) then
       LTmpStr:=' '+ Copy(LTmpStr,1,length(LTmpStr)-1);

      if AAlwaysRemoveTrailingZeros then
      begin
        if(Pos('.',string(LTmpStr)) > 0) then
        begin
          LPos := Length(LTmpStr);
          for LCount := Length(LTmpStr) downto 1 do
          begin
            if(LTmpStr[LCount] <> '0') then Break;
            if(LTmpStr[LCount] = '.')  then Break;
            LPos := LCount;
          end;
          LTmpStr := Copy(LTmpStr,1,LPos);
          while (Length(LTmpStr) < AValue.FLength) do
          begin
            if AValue.FDefaultPadding then
              LTmpStr := ' '+ LTmpStr
            else
              LTmpStr := LTmpStr + ' ';
          end;
        end;
      end;

      //Remove trailing zeros
      if(Length(LTmpStr) > AValue.FLength)  then
      begin
        if(Pos('.',string(LTmpStr)) > 0) then
        begin
          LPos := Length(LTmpStr);
          for LCount := Length(LTmpStr) downto 1 do
          begin
            LPos := LCount;
            if(LTmpStr[LCount] <> '0') then Break;
            if(LTmpStr[LCount] = '.')  then
            begin
              LPos := LCount-1;
              Break;
            end;
            if(LCount <= AValue.FLength) then Break;
          end;
          LTmpStr := Copy(LTmpStr,1,LPos);
          while (Length(LTmpStr) < AValue.FLength) do
          begin
            if AValue.FDefaultPadding then
              LTmpStr := ' '+ LTmpStr
            else
              LTmpStr := LTmpStr + ' ';
          end;
        end;
      end;

      //Remove trailing decimals
      if(Length(LTmpStr) > AValue.FLength) then
      begin
        if(Pos('.',string(LTmpStr)) > 0) then
        begin
          LPos := Length(LTmpStr);
          for LCount := Length(LTmpStr) downto 1 do
          begin
            LPos := LCount;
            if(LTmpStr[LCount] = '.')  then
            begin
              LPos := LCount-1;
              Break;
            end;
            if(Length(LTmpStr) = AValue.FLength) then Break;
          end;
          LTmpStr := Copy(LTmpStr,1,LPos);
          while (Length(LTmpStr) < AValue.FLength) do
          begin
            if AValue.FDefaultPadding then
              LTmpStr := ' '+ LTmpStr
            else
              LTmpStr := LTmpStr + ' ';
          end;
        end;
      end;

      if(Length(LTmpStr) > AValue.FLength) then
          raise Exception.Create('Float('+string(LTmpStr)+') is too big. Its maximum length is '+IntToStr(AValue.FLength));
      Result := string(LTmpStr);
    finally
      if ALeaveLeftBlank then
      begin
        AValue.FLength := AValue.FLength + 1;
        Result := ' ' + Result;
      end;
      if(Result[Length(Result)] = '.') then
        Result := ' ' + Copy(Result,1,Length(Result)-1);

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function ReturnSubstringFromChar(ANameString: string;AChar: char): string;
const OPNAME = 'UUtilities.ExtractReservoirName';
var
  LPos : integer;
begin
  Result := '';
  try
    LPos := Pos(AChar,ANameString);
    if LPos > 0 then
      Result := Trim(copy(ANameString,LPos+1,length(ANameString)- LPos));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function PadDouble(ATheDouble:TDouble):String;
const OPNAME = 'UUtilities.PadDouble';
Var
  LNegStr : string;
  LTmpStr : ShortString;
  LPos,
  LCount,
  LDec: integer;
Begin
  Result := '';
  try
    while (Length(Result) < ATheDouble.FLength) do
      Result := Result + ' ';
    if not(ATheDouble.FInitalised) then Exit;

    LDec := ATheDouble.FDecimal;
    if((ATheDouble.FDecimal = 0) and ATheDouble.ShowDecimalPoint) then
      LDec := 1;
    Str(ATheDouble.FData:ATheDouble.FLength:LDec,LTmpStr);

    If ((ATheDouble.FDecimal = 0) and ATheDouble.ShowDecimalPoint) then
     LTmpStr:=' '+ Copy(LTmpStr,1,length(LTmpStr)-1);

    //Remove trailing zeros
    if(Length(LTmpStr) > ATheDouble.FLength) then
    begin
      if(Pos('.',String(LTmpStr)) > 0) then
      begin
        LPos := Length(LTmpStr);
        for LCount := Length(LTmpStr) downto 1 do
        begin
          LPos := LCount;
          if(LTmpStr[LCount] <> '0') then Break;
          if(LTmpStr[LCount] = '.')  then Break;
        end;
        LTmpStr := Copy(LTmpStr,1,LPos);
        while (Length(LTmpStr) < ATheDouble.FLength) do
        begin
          if ATheDouble.FDefaultPadding then
            LTmpStr := ' '+ LTmpStr
          else
            LTmpStr := LTmpStr + ' ';
        end;
      end;
    end;

    //remove . when string too big
    {if(Length(LTmpStr) > ATheDouble.FLength) then
      if(LTmpStr[Length(LTmpStr)] = '.') then
        LTmpStr := Copy(LTmpStr,1,Length(LTmpStr)-1);}

    if(Length(LTmpStr) > ATheDouble.FLength) then
        raise Exception.Create('Float('+String(LTmpStr)+') is too big. Its maximum length is '+IntToStr(ATheDouble.FLength));

    //remove negative sign for a very small negative number.
    Lpos := Pos('-',String(LTmpStr));
    if(Lpos > 0) then
    begin
      LNegStr := String(LTmpStr);
      LNegStr[Lpos] := ' ';
      LNegStr := Trim(LNegStr);
      if(StrToFloat(LNegStr) = 0.0) then
        LTmpStr[Lpos] := ' ';
    end;
    Result := String(LTmpStr);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function SearchFiles(ASearchPath: string;AFileNamesContainer: TStrings):boolean;
const OPNAME = 'UUtilities.SearchFiles';
Var
  LSearchRec: TSearchRec;
  LMoreFiles: boolean;
  LPath: string;
begin
  Result := False;
  try

    if(ASearchPath = '') then
      raise Exception.Create('The search path parameter is empty. Path should be a valid file path.');
    if not Assigned(AFileNamesContainer) then
        raise Exception.Create('File names container object parameter is not yet assigned.');

    LPath := ExtractFilePath(ASearchPath);
    if(Length(LPath) = 0) then
    begin
      LPath := GetCurrentDir;
      if(LPath[Length(LPath)] <> '\') then
        LPath := LPath + '\';
    end;

    AFileNamesContainer.Clear;
    LMoreFiles := ( FindFirst(ASearchPath, faAnyFile, LSearchRec) = 0);
    while LMoreFiles do
    begin
      if (LSearchRec.Name <> '.') and (LSearchRec.Name <> '..') then
        AFileNamesContainer.Add(LPath+LSearchRec.Name);
      LMoreFiles := (FindNext(LSearchRec)= 0);
    end;
    SysUtils.FindClose(LSearchRec);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function GetHydrologyFileNames(ASearchPathAndCriteria: string; AFileNames : TStrings): boolean;
const OPNAME = 'UUtilities.GetHydrologyFileNames';
var
  LContainer: TStringList;
  LIndex: integer;
  LFileExt : string;
begin
  Result := False;
  try
    AFileNames.Clear;
    LContainer := TStringList.Create;
    try
      if SearchFiles(ASearchPathAndCriteria,LContainer) then
      begin
        for LIndex := 0 to LContainer.Count -1 do
        begin
          LFileExt := ExtractFileExt(LContainer[LIndex]);
          if(UpperCase(LFileExt) = '.INC') or
           (UpperCase(LFileExt) = '.RNK') or
           (UpperCase(LFileExt) = '.RAN') or
           (UpperCase(LFileExt) = '.AFF') or
           (UpperCase(LFileExt) = '.URB') or
           (UpperCase(LFileExt) = '.IRR') then
          begin
           AFileNames.Add(LContainer[LIndex]);
          end;
        end;
      end;
    finally
      LContainer.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function DeleteMultipleFiles(ASearchPathAndCriteria: string):boolean;
const OPNAME = 'UUtilities.DeleteMultipleFiles';
var
  LContainer: TStringList;
begin
  Result := False;
  try
    LContainer := TStringList.Create;
    try
      if SearchFiles(ASearchPathAndCriteria,LContainer) then
      begin
        while LContainer.Count > 0 do
        begin
{$WARN SYMBOL_PLATFORM OFF}
        	FileSetAttr(LContainer.Strings[0],0);
{$WARN SYMBOL_PLATFORM ON}
          SysUtils.DeleteFile(LContainer.Strings[0]);
          LContainer.Delete(0);
        end;
        Result := True;
      end;
    finally
      LContainer.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function SearchForDirectories(AParentDirectory: string;AFileNamesContainer: TStrings):boolean;
const OPNAME = 'UUtilities.SearchForDirectories';
Var
  LSearchRec: TSearchRec;
  LMoreFiles: boolean;
  LPath: string;
begin
  Result := False;
  try

    if(AParentDirectory = '') then
      raise Exception.Create('The Parent Directory parameter is empty. Parent Directory should be a valid file directory.');
    if not Assigned(AFileNamesContainer) then
        raise Exception.Create('Directory names container object parameter is not yet assigned.');

    LPath := ExtractFilePath(AParentDirectory);
    if(Length(LPath) <> 0) then
    begin
      LPath := IncludeTrailingPathDelimiter(LPath)+ '*';
    end;

    AParentDirectory := IncludeTrailingPathDelimiter(AParentDirectory);
    AFileNamesContainer.Clear;
    LMoreFiles := ( FindFirst(LPath, faDirectory, LSearchRec) = 0);
    while LMoreFiles do
    begin
      if(LSearchRec.Name[1] <> '.') then
        AFileNamesContainer.Add(AParentDirectory+LSearchRec.Name);
      LMoreFiles := (FindNext(LSearchRec)= 0);
    end;
    SysUtils.FindClose(LSearchRec);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function GetHydrologyFileType(AAppModules: TAppModules;AHydrologyFileName: string): Integer;
const OPNAME = 'UUtilities.GetHydrologyFileType';
var
  LFileExt: string;
  Lmessage: string;
begin
  Result := -1;
  try
    if (Trim(AHydrologyFileName) = '') then
       MessageDlg(AAppModules.Language.GetString('UUtilities.strNoFileName'),mtWarning,[mbOK],0)
    else
    begin
      LFileExt := ExtractFileExt(AHydrologyFileName);
      if (Trim(AHydrologyFileName) = LFileExt) then
      begin
        Lmessage := Format(AAppModules.Language.GetString('UUtilities.strNoExtention'),[AHydrologyFileName]);
        MessageDlg(Lmessage,mtWarning,[mbOK],0);
      end
      else
      begin
        Result := AAppModules.HydrologyFileType.GetFileType(LFileExt);
        if(Result < 0) then
          Result := 100;
        {begin
          Lmessage := Format(AAppModules.Language.GetString('UUtilities.strExtentionNew'),[AHydrologyFileName]);
          MessageDlg(Lmessage,mtWarning,[mbOK],0);
        end;}
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function GetOutputFileType(AFileName: string): TOutputFileType;
const OPNAME = 'UUtilities.GetOutputFileType';
var
  LUpperFileName: string;
begin
  Result := oftNone;
  try
    if(Trim(AFileName) <> '') then
    begin
      LUpperFileName := UpperCase(ExtractFileName(AFileName));
      if(Pos('YLD.OUT',LUpperFileName) > 0) then
        Result := oftYield
      else if(Pos('SUM.OUT',LUpperFileName) > 0) then
        Result := oftSum
      else if(Pos('DAT.OUT',LUpperFileName) > 0) then
        Result := oftData
      else if(Pos('DBG.OUT',LUpperFileName) > 0) then
        Result := oftDebug
      else if(Pos('PLT.OUT',LUpperFileName) > 0) then
        Result := oftPlot
      else if(Pos('PMP.OUT',LUpperFileName) > 0) then
        Result := oftPmp
      else if(Pos('HYD.OUT',LUpperFileName) > 0) then
        Result := oftHydroPower
      else if(Pos('DEM.OUT',LUpperFileName) > 0) then
        Result := oftDemand;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function CopyStringFromRight(AString: string; ANumberOfCharsToBeCopied: integer): string;
const OPNAME = 'UUtilities.CopyStringFromRight';
var
  LPos: integer;
begin
  Result := '';
  try
    LPos := Length(AString);
    while (LPos > 0) do
    begin
      Result := AString[LPos] + Result;
      LPos := LPos -1;
      if(ANumberOfCharsToBeCopied <= Length(AString)) then
      begin
        if ((LPos - ANumberOfCharsToBeCopied) <= 0) then
          Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function CompareDouble(AValue,BValue: double; APrecesion: integer): boolean;
const OPNAME = 'UUtilities.CompareDouble';
var
  LDiff: extended;
  LRem: integer;
begin
  Result := False;
  try
    if(AValue = NullFloat) or (BValue = NullFloat) then
    begin
      Result := (AValue = BValue);
    end
    else
    begin
      LDiff := Abs(Abs(AValue) - Abs(BValue));
      LRem  := Trunc(LDiff*Power(10,APrecesion));
      Result := LRem = 0;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function FilePathIsDosCompatible(AAppModules: TAppModules;AFileOrPathName: string): boolean;
const OPNAME = 'UUtilities.FilePathIsDosCompatible';
var
  LFileName,
  LFilePath,
  LFileExt: string;
  LPos: integer;
begin
  Result := False;
  try
   if(Trim(AFileOrPathName) = '') then
     Result := True
   else if((AAppModules <> nil) and (AAppModules.StudyArea <> nil)) and
          ((AAppModules.StudyArea.ModelCode = CYield) or (AAppModules.StudyArea.ModelCode = CPlanning)) and
          (AAppModules.StudyArea.ModelVersion = '7') then
     Result := True
    else
    begin
      if(Length(AFileOrPathName) > 50) then
        Exit;
      LFileExt := ExtractFileExt(Trim(AFileOrPathName));
      if(Length(LFileExt) > 4) then
        Exit;

      LFileName := ExtractFileName(Trim(AFileOrPathName));
      if(Length(LFileName) > 12) then
        Exit;

      LFilePath := ExtractFilePath(Trim(AFileOrPathName));
      if(Length(LFilePath) <= 8) then
        Result := True
      else
      begin
        while(Length(LFilePath) > 8) do
        begin
          LPos := Pos('\',LFilePath);
          if(LPos = 0) then
            Break;
          if(LPos > 9) then
            Break;
          Delete(LFilePath,1,LPos);
        end;
        Result := (Length(LFilePath) <= 8);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function GetTempDir: string;
const OPNAME = 'UUtilities.GetTempDir';
var
  TmpDir: array [0..MAX_PATH-1] of char;
begin
  try
    try
      SetString(Result, TmpDir, GetTempPath(MAX_PATH, TmpDir));
      if not SysUtils.DirectoryExists(Result) then
        if not CreateDirectory(PChar(Result), nil) then begin
          Result := IncludeTrailingPathDelimiter(GetWindowsDir) + 'TEMP';
          if not SysUtils.DirectoryExists(Result) then
            if not CreateDirectory(Pointer(Result), nil) then begin
              Result := ExtractFileDrive(Result) + '\TEMP';
              if not SysUtils.DirectoryExists(Result) then
                if not CreateDirectory(Pointer(Result), nil) then begin
                  Result := ExtractFileDrive(Result) + '\TMP';
                  if not SysUtils.DirectoryExists(Result) then
                    if not CreateDirectory(Pointer(Result), nil) then begin
                      raise Exception.Create(SysErrorMessage(GetLastError));
                    end;
                end;
            end;
        end;
    except
      Result := '';
      raise;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function GetWindowsDir: TFileName;
const OPNAME = 'UUtilities.GetWindowsDir';
var
  WinDir: array [0..MAX_PATH-1] of char;
begin
  try
    SetString(Result, WinDir, GetWindowsDirectory(WinDir, MAX_PATH));
    if Result = '' then
      raise Exception.Create(SysErrorMessage(GetLastError));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function GetAppDataLocalDir: string;
const OPNAME = 'UUtilities.GetAppDataLocalDir';
//var
  //TmpDir: array [0..MAX_PATH-1] of char;
  //LPos : integer;
begin
  try
    try
      //SetString(Result, TmpDir, GetTempPath(MAX_PATH, TmpDir));
      Result := 'C:\Users\Default\AppData\Local\WRMF';
      {LPos := Pos('Temp',Result);
      if LPos = 0 then
        LPos := Pos('Tmp',Result);
       }
      //Result := Copy(Result,1,LPos-1)+'WRMF';
         //IncludeTrailingPathDelimiter(GetWindowsDir) + 'WRMF';
      if not SysUtils.DirectoryExists(Result) then
        if not CreateDirectory(PChar(Result), nil) then
         raise Exception.Create(SysErrorMessage(GetLastError));
    except
      Result := '';
      raise;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function ExecAndWait(AExecuteFile, AParamString : string): boolean;
const OPNAME = 'UUtilities.ExecAndWait';
var
  LSEInfo: TShellExecuteInfo;
  LExitCode: DWORD;
  LPos: integer;
begin
  Result := False;
  try
    FillChar(LSEInfo, SizeOf(LSEInfo), 0);
    LSEInfo.cbSize := SizeOf(TShellExecuteInfo);
    with LSEInfo do begin
      fMask := SEE_MASK_NOCLOSEPROCESS;
      Wnd := Application.Handle;
      lpFile := PChar(AExecuteFile);
      lpParameters := PChar(AParamString);
      if(Length(AParamString) >= 2) then
      begin
        LPos := Pos(':', AParamString);
        if (AParamString[LPos -2] = '"') then
          lpDirectory := PChar(Copy(AParamString,LPos -2, 255))
        else if (AParamString[LPos -1] = '"') then
          lpDirectory := PChar(Copy(AParamString,LPos -1, 255));
      end
      else
        lpDirectory := PChar(ExtractFilePath(AExecuteFile));
      nShow := SW_SHOWNORMAL;
    end;

    if ShellExecuteEx(@LSEInfo) then
    begin
      repeat
        Application.ProcessMessages;
        GetExitCodeProcess(LSEInfo.hProcess, LExitCode);
      until (LExitCode <> STILL_ACTIVE) or Application.Terminated;
      Result := True;
    end
    else Result := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function RegisterOCX(ADllFileName: string): boolean;
const OPNAME = 'UUtilities.RegisterOCX';
type
  TRegFunc = function : HResult; stdcall;
var
  LRegFunc : TRegFunc;
  LHandle  : THandle;
  LDllFileName,
  LExtractFileName: string;
begin
  Result := False;
  try
    try
      ChDir(ExtractFilePath(ADllFileName));
      LExtractFileName := ExtractFileName(ADllFileName);
      LHandle := LoadLibrary(PChar(LExtractFileName));
      if LHandle <> 0 then
      begin
        LRegFunc := GetProcAddress(LHandle,'DllRegisterServer');
        if Assigned(LRegFunc) then
        begin
          LDllFileName := '"' + ADllFileName + '"';
          Result := ExecAndWait('regsvr32', '/s ' + LDllFileName);
        end;
      end;
    except
      ShowMessage(Format('Unable to register %s', [ADllFileName]));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function UnRegisterOCX(ADllFileName: string): boolean;
const OPNAME = 'UUtilities.UnRegisterOCX';
type
  TRegFunc = function : HResult; stdcall;
var
  LRegFunc : TRegFunc;
  LHandle  : THandle;
  LDllFileName,
  LExtractFileName: string;
begin
  Result := False;
  try
    try
      ChDir(ExtractFilePath(ADllFileName));
      LExtractFileName := ExtractFileName(ADllFileName);
      LHandle := LoadLibrary(PChar(LExtractFileName));
      if LHandle <> 0 then
      begin
        LRegFunc := GetProcAddress(LHandle,'DllUnregisterServer');
        if Assigned(LRegFunc) then
        begin
          LDllFileName := '"' + ADllFileName + '"';
          Result := ExecAndWait('regsvr32', '/s/u ' + LDllFileName);
        end;
      end;
    except
      ShowMessage(Format('Unable to unregister %s', [ADllFileName]));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function IsPintableCharacter(AChar: Char): boolean;
const OPNAME = 'UUtilities.IsPintableCharacter';
begin
  Result := False;
  try
    Result := (AChar >= Char(32)) and (AChar <= Char(126));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function GetMinMaxYearFromHydrologyFiles(const AFileName: string; var AStartYear, AEndYear: integer): boolean;
const OPNAME = 'UUtilities.GetMinMaxYearFromHydrologyFiles';
var
  LFileContents : TStringList;
  LYear,LLine   : string;
  Lindex        : integer;
begin
  Result := False;
  try
    AStartYear := 0;
    AEndYear   := 0;
    if FileExists(AFileName) then
    begin
      LFileContents := TStringList.Create;
      try
        LFileContents.LoadFromFile(AFileName);
        if (LFileContents.Count > 0) then
        begin
          for Lindex := 0 to  LFileContents.Count -1 do
          begin
            if (Trim(LFileContents[Lindex]) <> '') then
            begin
              LLine := LFileContents[Lindex];
              LYear := ExtractFirstSubstring(LLine);
              AStartYear := StrToInt(Trim(LYear));
              Break;
            end;
          end;
          for Lindex := LFileContents.Count -1 downto 0  do
          begin
            if (Trim(LFileContents[Lindex]) <> '') then
            begin
              LLine := LFileContents[Lindex];
              LYear := ExtractFirstSubstring(LLine);
              AEndYear := StrToInt(Trim(LYear));
              Break;
            end;
          end;
          Result := True;
        end;
      finally
        LFileContents.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function LoggedOnUserName: string;
const OPNAME = 'UUtilities.LoggedOnUserName';
var
  ls_UserName: array[0..100] of char; // Contains the name of the DLL.
  LSize: cardinal;
begin
  Result := '';
  try
    ls_UserName[0] := #0;
    LSize := SizeOf(ls_UserName);
    GetUserName(ls_UserName,LSize);
    Result := String(ls_UserName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure ExtractErrorsAndColumns(AErrorMessage: WideString; AErrors, AColumns: TStrings);
const OPNAME = 'UUtilities.ExtractErrorsAndColumns';
var
  LErrorsString,
  LColumnString: WideString;
  LIndex: integer;
begin
  try
    AErrors.Clear;
    AColumns.Clear;
    LErrorsString := AErrorMessage;
    LColumnString := '';

    LIndex := Pos(CTStringsSeparator,PChar(AErrorMessage));
    if(LIndex >= 1) then
    begin
      AErrorMessage := Copy(AErrorMessage,LIndex + Length(CTStringsSeparator),Length(AErrorMessage));
      LErrorsString := AErrorMessage;
      LIndex := Pos(CTStringsSeparator,PChar(AErrorMessage));
      if(LIndex > 1) then
      begin
        LErrorsString := Copy(AErrorMessage,1,LIndex-1);
        AErrorMessage := Copy(AErrorMessage,LIndex + Length(CTStringsSeparator),Length(AErrorMessage));
        LIndex := Pos(CTStringsSeparator,PChar(AErrorMessage));
        if(LIndex > 1) then
        begin
          LColumnString := Copy(AErrorMessage,1,LIndex-1);
        end;
      end;
    end;
    AErrors.Text := LErrorsString;
    AColumns.Text := LColumnString;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function CutErrorsAndColumns(AErrorMessage: WideString; AErrors, AColumns: TStrings): string;
const OPNAME = 'UUtilities.CutErrorsAndColumns';
var
  LErrorsString: WideString;
  LIndex: integer;
  LIndex1: integer;
  LIndex2: integer;
  LIndex3: integer;
begin
  Result := '';
  try
    AErrors.Clear;
    AColumns.Clear;
    LIndex := Pos(CTStringsSeparator,PChar(AErrorMessage));
    if(LIndex < 1) then
    begin
      AErrors.Text := AErrorMessage;
      Result := '';
    end
    else
    if(LIndex > 1) then
    begin
      LErrorsString := Copy(AErrorMessage,1,LIndex-1);
      AErrors.Text := LErrorsString;
      Result := Copy(AErrorMessage,LIndex, Length(AErrorMessage));
    end
    else
    if(LIndex = 1) then
    begin
      LIndex1 := 1;
      LIndex3 := 0;
      LIndex2 := PosEx(CTStringsSeparator,AErrorMessage,LIndex1 + Length(CTStringsSeparator));
      if(LIndex2 > 0) then
      begin
        LIndex3 := PosEx(CTStringsSeparator,AErrorMessage,LIndex2 + Length(CTStringsSeparator));
      end;
      if(LIndex3 > 0) then
      begin
        LErrorsString := Copy(AErrorMessage,1,LIndex3 + Length(CTStringsSeparator) -1);
        Result := Copy(AErrorMessage,LIndex3 + Length(CTStringsSeparator),Length(AErrorMessage));
        ExtractErrorsAndColumns(LErrorsString,AErrors,AColumns);
      end
      else
      begin
        ExtractErrorsAndColumns(AErrorMessage,AErrors,AColumns);
        Result := '';
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function SelectionComboBox(const ACaption, APrompt, ACommaTextItems: string; ADefault: integer): integer;
const OPNAME = 'UUtilities.SelectionComboBox';
begin
  Result := ADefault;
  SelectionComboQuery(ACaption,APrompt ,ACommaTextItems , Result);
end;
function SelectionRadio(const ACaption, APrompt, ACommaTextItems: string; ADefault: integer): integer;
const OPNAME = 'UUtilities.SelectionRadio';
begin
  Result := ADefault;
  SelectionRadioQuery(ACaption,APrompt, ACommaTextItems, Result);
end;


function SelectionComboQuery(const ACaption, APrompt, ACommaTextItems: string;
  var Value: integer): Boolean;
const OPNAME = 'UUtilities.SelectionComboQuery';
function GetAveCharSize(Canvas: TCanvas): TPoint;
const OPNAME = 'UUtilities.GetAveCharSize';
var
  I: Integer;
  Buffer: array[0..51] of Char;
begin
  for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));
  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;
var
  Form: TForm;
  Prompt: TLabel;
  Edit: TComboBox;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
begin
  Result := False;
  Form := TForm.Create(Application);
  with Form do
    try
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := ACaption;
      ClientWidth := MulDiv(180, DialogUnits.X, 4);
      Position := poScreenCenter;
      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        Caption := APrompt;
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := MulDiv(8, DialogUnits.Y, 8);
        Constraints.MaxWidth := MulDiv(164, DialogUnits.X, 4);
        WordWrap := True;
      end;
      Edit := TComboBox.Create(Form);
      with Edit do
      begin
        Parent := Form;
        Left := Prompt.Left;
        Top := Prompt.Top + Prompt.Height + 5;
        Width := MulDiv(164, DialogUnits.X, 4);
        MaxLength := 255;
        Items.CommaText := ACommaTextItems;
        ItemIndex := Value;
      end;
      ButtonTop := Edit.Top + Edit.Height + 15;
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgOK;
        ModalResult := mrOk;
        Default := True;
        SetBounds(MulDiv(38, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgCancel;
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(MulDiv(92, DialogUnits.X, 4), Edit.Top + Edit.Height + 15,
          ButtonWidth, ButtonHeight);
        Form.ClientHeight := Top + Height + 13;
      end;
      if ShowModal = mrOk then
      begin
        Value := Edit.ItemIndex;
        Result := True;
      end;
    finally
      Form.Free;
    end;
end;
function SelectionRadioQuery(const ACaption, APrompt, ACommaTextItems: string; var Value: integer): Boolean;
const OPNAME = 'UUtilities.SelectionRadioQuery';
function GetAveCharSize(Canvas: TCanvas): TPoint;
const OPNAME = 'UUtilities.GetAveCharSize';
var
  I: Integer;
  Buffer: array[0..51] of Char;
begin
  for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));
  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;
var
  Form: TForm;
  Prompt: TLabel;
  Edit: TRadioGroup;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
begin
  Result := False;
  Form := TForm.Create(Application);
  with Form do
  begin
    try
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := ACaption;
      ClientWidth := MulDiv(180, DialogUnits.X, 4);
      Position := poScreenCenter;
      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        Caption := APrompt;
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := MulDiv(8, DialogUnits.Y, 8);
        Constraints.MaxWidth := MulDiv(164, DialogUnits.X, 4);
        WordWrap := True;
      end;
      Edit := TRadioGroup.Create(Form);
      with Edit do
      begin
        Parent := Form;
        Left := Prompt.Left;
        Top := Prompt.Top + Prompt.Height + 5;
        Width := MulDiv(164, DialogUnits.X, 4);
        Items.CommaText := ACommaTextItems;
        ItemIndex := Value;
      end;
      ButtonTop := Edit.Top + Edit.Height + 15;
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgOK;
        ModalResult := mrOk;
        Default := True;
        SetBounds(MulDiv(38, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgCancel;
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(MulDiv(92, DialogUnits.X, 4), Edit.Top + Edit.Height + 15,
          ButtonWidth, ButtonHeight);
        Form.ClientHeight := Top + Height + 13;
      end;
      if ShowModal = mrOk then
      begin
        Value := Edit.ItemIndex;
        Result := True;
      end;
    finally
      Form.Free;
    end;
  end;
end;
function GetAnalysisYearsFromParamDataFile(const AParamFileName: string ; var ANumberOfYears, AStartYearGregorian: integer) : boolean;
const OPNAME = 'UUtilities.GetAnalysisYearsFromParamDataFile';
var
  LFileContents : TStringList;
begin
  Result := FALSE;
  try
    if FileExists(AParamFileName) then
    begin
      LFileContents := TStringList.Create;
      try
      begin
        LFileContents.LoadFromFile(AParamFileName);
        if (LFileContents.Count > 5) then
        begin
          ANumberOfYears      := StrToInt(Trim(Copy(LFileContents[5], 1, 12)));
          AStartYearGregorian := StrToInt(Trim(Copy(LFileContents[5], 13, 12)));
          if(AStartYearGregorian < 1899) then
            AStartYearGregorian := 1900 + AStartYearGregorian;
          Result := TRUE;
        end;
      end;
      finally
        LFileContents.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure GetFileModifiedTime(strFileName:string; var T: FILETIME);
const OPNAME = 'UUtilities.GetFileModifiedTime';
var LHFile: THandle;
begin
	ZeroMemory(@T,sizeof(T));
	LHFile := CreateFile(PChar(strFileName),GENERIC_READ,FILE_SHARE_WRITE,nil,OPEN_EXISTING,0,0);
  if(LHFile <> 0)then
  begin
  	GetFileTime(LHFile,nil,nil,@T);
    CloseHandle(LHFile);
  end;
end;

function GetUserSettingsFolder: string;
const OPNAME = 'UUtilities.GetUserSettingsFolder';
begin
  Result := GetSpecialFolder(CSIDL_STARTMENU	);
  if(Result <> '') then
  begin
    Result := ExcludeTrailingPathDelimiter(Result);
    while (Length(Result) > 0) and (Result[Length(Result)] <> '\') do
       Delete(Result,Length(Result),1);
    Result := IncludeTrailingPathDelimiter(Result);
  end;
end;

{  Return the full path to the requested folder.  FolderId should be one of the
   ShlObj.pas CSIDL_ constants such as CSIDL_APPDATA.}
function GetSpecialFolder(const FolderId: integer): string;
const OPNAME = 'UUtilities.GetSpecialFolder';
var
  ppidl : PItemIdList;
begin
  Result := '';
  If SHGetSpecialFolderLocation(0,FolderId,ppidl) = NOERROR then
    begin
      SetLength(Result,MAX_PATH);

      If SHGetPathFromIDList(ppidl,PChar(Result)) then
        SetLength(Result,StrLen(PChar(Result)))
      else
        Result := '';

    end
  else
    Result := '';

end;

function GetReportFileName(AStudyName,ACategory,AFileName: string): string;
const OPNAME = 'UUtilities.GetReportFileName';
var
  LBinStr,
  LFileName,
  LPath: string;
begin
  Result := '';
  try
    LPath := ApplicationExeName;
    LPath := ExtractFilePath(LPath);
    if(Length(LPath) > 4) then
    begin
      LBinStr := Copy(LPath,Length(LPath)- 3,4);
      if(UpperCase(LBinStr) = 'BIN\') then
         LPath := Copy(LPath,1,Length(LPath)- 4);
    end;
    LPath := IncludeTrailingPathDelimiter(LPath);
    LPath := LPath + 'Reports\' ;
    if(Trim(AStudyName) <> '') then
      LPath := LPath + AStudyName + '\';
    if not SysUtils.DirectoryExists(LPath) then
       SysUtils.ForceDirectories(LPath);
    LPath := LPath + ACategory + '\' ;
    if not SysUtils.DirectoryExists(LPath) then
       SysUtils.ForceDirectories(LPath);
    LFileName := LPath + AFileName;
    Result := LFileName;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function NetworkDiagramsPath : string;
const OPNAME = 'UUtilities.NetworkDiagramsPath';
var
  lBinStr   : string;
  lPath     : string;
begin
  Result := '';
  try
    lPath := GetAppDataLocalDir;  //ApplicationExeName;
    //lPath := ExtractFilePath(lPath);
    if (Length(lPath) > 4) then
    begin
      lBinStr := Copy(lPath, Length(lPath)-3, 4);
      if (UpperCase(lBinStr) = 'BIN\') then
        lPath := Copy(lPath, 1, Length(lPath)-4);
    end;
    lPath := IncludeTrailingPathDelimiter(lPath);
    lPath := lPath + 'Network Diagrams\';
    Result := lPath;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function StudyDocumentsPath : string;
const OPNAME = 'UUtilities.StudyDocumentsPath';
var
  lBinStr   : string;
  lPath     : string;
begin
  Result := '';
  try
    lPath := ApplicationExeName;
    lPath := ExtractFilePath(lPath);
    if (Length(lPath) > 4) then
    begin
      lBinStr := Copy(lPath, Length(lPath)-3, 4);
      if (UpperCase(lBinStr) = 'BIN\') then
        lPath := Copy(lPath, 1, Length(lPath)-4);
    end;
    lPath := IncludeTrailingPathDelimiter(lPath);
    lPath := lPath + 'Reports\';
    Result := lPath;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function DeploymentPath : string;
const OPNAME = 'UUtilities.DeploymentPath';
var
  lBinStr   : string;
  lPath     : string;
begin
  Result := '';
  try
    lPath := ApplicationExeName;
    lPath := ExtractFilePath(lPath);
    if (Length(lPath) > 4) then
    begin
      lBinStr := Copy(lPath, Length(lPath)-3, 4);
      if (UpperCase(lBinStr) = 'BIN\') then
        lPath := Copy(lPath, 1, Length(lPath)-4);
    end;
    lPath := IncludeTrailingPathDelimiter(lPath);
    Result := lPath;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function ChopCharacters (AName : string) : string;
const OPNAME = 'UUtilities.ChopCharacters';
var
  lIndex : integer;
begin
  Result := AName;
  try
    lIndex := 1;
    while (lIndex <= Length(AName)) do
    begin
      if CharInSet(AName[lIndex],['A'..'Z','a'..'z', '0'..'9']) then
        lIndex := lIndex + 1
      else
        AName := Copy(AName, 1, lIndex-1) + Copy(AName, lIndex+1, Length(AName)-lIndex);
    end;
    Result := AName;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TrimChars(AValue: string; AChar: Char): string;
const OPNAME = 'UUtilities.TrimChars';
begin
  Result := '';
  try
    Result := LeftTrimChars(AValue,AChar);
    Result := RightTrimChars(Result,AChar);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function LeftTrimChars(AValue: string; AChar: Char): string;
const OPNAME = 'UUtilities.LeftTrimChars';
var
  LPos, LIndex: Integer;
begin
  Result := '';
  try
    LIndex := Length(AValue);
    LPos := 1;
    while (LPos <= LIndex) and (AValue[LPos] = AChar) do
      Inc(LPos);
    Result := Copy(AValue, LPos, Maxint);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function RightTrimChars(AValue: string; AChar: Char): string;
const OPNAME = 'UUtilities.RightTrimChars';
var
  LPos: Integer;
begin
  Result := '';
  try
  LPos := Length(AValue);
  while (LPos > 0) and (AValue[LPos] = AChar)  do
    Dec(LPos);
  Result := Copy(AValue, 1, LPos);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function SmartFloatFormat(AValue : double; ALength,AMinDecimals : integer) : string;
const OPNAME = 'UUtilities.SmartFloatFormat';
var
  LSpaceLeft: integer;
  LWholeInt: integer;
  LFraction: double;
  LWholeStr: string;
  LFractionStr: string;
  LFormat: string;
  LPos,
  LIndex: integer;
begin
  Result := '';
  try
    if(AValue > NullFloat) then
    begin
      LFractionStr := '';
      LWholeInt    := Trunc(AValue);
      LFraction    := AValue - LWholeInt;;
      LWholeStr    := IntToStr(LWholeInt);
      LSpaceLeft   := ALength - Length(LWholeStr)-1;
      if(LSpaceLeft > 0) then
      begin
        LFormat := '0.0';
        for LIndex := 1 to LSpaceLeft-1 do
        LFormat := LFormat + '0';
        LFractionStr := FormatFloat(LFormat,LFraction);
        Delete(LFractionStr,1,1);
        Result := LWholeStr + LFractionStr;
        Result := RightTrimChars(Result,'0');
      end
      else
        Result := LWholeStr;
      if(LSpaceLeft > 0) and (AMinDecimals > 0) then
      begin
        if(Length(Result) < ALength) then
        begin
          LPos := Pos('.',Result);
          if(LPos = 0) then
          begin
            Result := Result + '.';
            for LIndex := 1 to AMinDecimals do
            begin
              if(Length(Result) >= ALength)  then
                Break;
              Result := Result + '0';
            end;
          end
          else
          begin
            for LIndex := 1 to AMinDecimals do
            begin
              if(Length(Result) >= ALength) or ((Length(Result) - Pos('.',Result)) >= AMinDecimals) then
                Break;
              Result := Result + '0';
            end;
          end;
        end;
      end;
    end;
    while (Length(Result) < ALength) do
      Result := ' ' + Result;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function SmartFloatFormatForFiles(AValue : double; ALength,AMaxDecimals : integer) : string;
const OPNAME = 'UUtilities.SmartFloatFormatForFiles';
var
  LSpaceLeft: integer;
  LWholeInt: integer;
  LFraction: double;
  LWholeStr: string;
  LFractionStr: string;
  LFormat: string;
  LPos,
  LIndex: integer;
begin
  Result := '';
  try
    LFractionStr := '';
    LWholeInt    := Trunc(AValue);
    LFraction    := AValue - LWholeInt;;
    LWholeStr    := IntToStr(LWholeInt);
    LSpaceLeft   := ALength - Length(LWholeStr)-1;
    if(LSpaceLeft > 0) then
    begin
      LFormat := '0.0';
      for LIndex := 1 to LSpaceLeft-1 do
        LFormat := LFormat + '0';
      LFractionStr := FormatFloat(LFormat,LFraction);
      Delete(LFractionStr,1,1);
      Result := LWholeStr + LFractionStr;
      Result := RightTrimChars(Result,'0');
    end
    else
      Result := LWholeStr;

    if(LSpaceLeft > 0) and (AMaxDecimals > 0) then
    begin
      if(Length(Result) < ALength) then
      begin
        LPos := Pos('.',Result);
        if(LPos = 0) then
          Result := Result + '.';
        for LIndex := 1 to AMaxDecimals do
        begin
          if(Length(Result) >= ALength) or ((Length(Result) - Pos('.',Result)) >= AMaxDecimals) then
            Break;
          if(Result[Length(Result)] <> '0') then
            Result := Result + '0'
          else
            Result := ' ' + Result;
        end;
      end;
    end;
    while (Length(Result) < ALength) do
      Result := ' ' + Result;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function ConvertXValueToPlottingBase(AXValue: double;APeriodLength,APlottingBase: integer): double;
const OPNAME = 'UUtilities.ConvertXValueToPlottingBase';
var
  LExponent: double;
begin
  Result := AXValue;
  try
    if(APeriodLength > 0) and (AXValue > 0.0) then
    begin
      LExponent := APlottingBase / APeriodLength;
      Result    := Power((AXValue / 100.0), LExponent) * 100.0;
    end;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function CommaTextString(AString: string): string;
const OPNAME = 'UUtilities.CommaTextString';
var
  LPos: integer;
begin
  Result := '';
  try
    AString := Trim(AString);
    LPos := Pos('  ',AString);
    while (LPos > 0) do
    begin
      AString := StringReplace(AString,'  ',' ',[rfReplaceAll]);
      LPos := Pos('  ',AString);
    end;
    Result := StringReplace(AString,' ',',',[rfReplaceAll]);

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function UnCommaTextString(ACommaTextString: string): string;
const OPNAME = 'UUtilities.CommaTextString';
var
  LIndex: integer;
  LCommaText : TStringList;
begin
  Result := '';
  try
    LCommaText := TStringList.Create;
    try
      LCommaText.CommaText := ACommaTextString;
      if(LCommaText.Count > 0) then
      begin
        Result := LCommaText[0];
        for LIndex := 1 to LCommaText.Count-1 do
          Result := Result + ' ' + LCommaText[LIndex];
      end;
    finally
      LCommaText.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function StringsItemsCount(AContainer: TStrings): integer;
const OPNAME = 'UUtilities.StringsItemsCount';
var
  LCount,
  LIndex,
  LPos: integer;
  LTempStr: string;
begin
  Result := 0;
  try
    if Assigned(AContainer) then
    begin
      for LIndex := 0 to AContainer.Count -1 do
      begin
        LPos := pos('*',AContainer[LIndex]);
        if(LPos = 0) then
          Result := Result + 1
        else
        begin
          LTempStr := Copy(AContainer[LIndex],1,LPos-1);
          LCount   := StrToIntDef(LTempStr,1);
          Result   := Result + LCount;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function StringsItemsCount(ACommaTextData: String): integer;  overload;
const OPNAME = 'UUtilities.StringsItemsCount';
var
  LData: TStringList;
begin
  Result := 0;
  try
    LData := TStringList.Create;
    try
      LData.CommaText := ACommaTextData;
      Result := StringsItemsCount(LData);
    finally
      LData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

// if angle is outside of 0-360 then put it within 0-360
function fix_angle(AAngle:double):double;
const  OPNAME = 'UUtilities.fix_angle';
begin
  Result := 0.0;
  try
    if AAngle>360.0 then AAngle:=AAngle-360.0;
    if AAngle>360.0 then AAngle:=AAngle-360.0;
    if AAngle<0 then AAngle:=AAngle+360.0;
    if AAngle<0 then AAngle:=AAngle+360.0;
    result:=AAngle;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

// angle going from x1,y1 to x2,y2
function get_angle_degrees(x1,y1,x2,y2:double):double;
const  OPNAME = 'UUtilities.get_angle_degrees';
       RADIANS = 57.29577951;
var
 LPart1, LPart2:double;
 LAngle:double;
begin
  Result := 0.0;
  try
   if (x1=x2) and (y1=y2) then
    begin
     result:=0.0;
     exit;
    end;
   LPart1:=abs(y2-y1);
   if (LPart1=0) then begin LPart1:=0.0000001; y1:=y1+0.0000001; end;
   LPart2:=abs(x2-x1);
   if (LPart2=0) then begin LPart2:=0.0000001; x1:=x1+0.0000001; end;
   LAngle:=arctan(LPart1/LPart2)*RADIANS;
   if ((x1>x2) and (y1<y2)) then LAngle:=180-LAngle;
   if ((x1>x2) and (y1>y2)) then LAngle:=LAngle +180;
   if ((x1<x2) and (y1>y2)) then LAngle:=360-LAngle;
   LAngle:=fix_angle(LAngle);
   result:=LAngle;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function Angle(Pt1 : TPoint; Pt2 : TPoint) : Double;
const  OPNAME = 'UUtilities.Distance';
begin
  Result := 0.0;
  try
    Result := get_angle_degrees(Pt1.X,Pt1.Y,Pt2.X,Pt2.Y);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

funcTion Distance(Pt1,Pt2 : TPoint) : Double;
const  OPNAME = 'UUtilities.Distance';
var
  dx : LongInt;
  dy : LongInt;
begin
  Result := 0.0;
  try
   dx:= pt1.x - pt2.x;
   dy:= pt1.y - pt2.y;
   Result := Sqrt((Dx * Dx) + (Dy * Dy));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function UnCompressCommatextString(ACommaTextData: string): string;
const OPNAME = 'UUtilities.UnCompressCommatextString';
var
  LPos       : integer;
  lCount     : integer;
  lIndex     : integer;
  lIndex2    : integer;
  LCountStr  : string;
  lTempValue : string;
  LDataContainer   : TStringList;
begin
  Result := '';
  try
    if(Trim(ACommaTextData) = '') then  Exit;
    LDataContainer := TStringList.Create;
    try
      LDataContainer.CommaText := ACommaTextData;
      LIndex := 0;
      while (LIndex < LDataContainer.Count) do
      begin
        LPos := Pos('*', LDataContainer[LIndex]);
        if(LPos > 0) then
        begin
          LCountStr  := Copy(LDataContainer[LIndex],1,LPos-1);
          lTempValue := Copy(LDataContainer[LIndex], LPos + 1, Length(LDataContainer.CommaText) - lPos);
          LCount     := StrToIntDef(LCountStr,1); //+1;
          LDataContainer[LIndex] := lTempValue;
          for lIndex2 := 2 to LCount do
            LDataContainer.Insert(LIndex,lTempValue);
        end;
        LIndex := LIndex + 1;
      end;
      Result := LDataContainer.CommaText;
    finally
      LDataContainer.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function CompressCommatextString(ACommaTextData: string): string;
const OPNAME = 'UUtilities.CompressCommatextString';
var
  LPos,
  LIndex     : integer;
  LCount     : integer;
  LValue     : string;
  LDataContainer   : TStringList;
begin
  Result := '';
  try
    if(Trim(ACommaTextData) = '') then  Exit;
    LDataContainer := TStringList.Create;
    try
      LDataContainer.CommaText := ACommaTextData;
      LCount := 1;
      LPos   := 0;
      LValue := Trim(LDataContainer[LDataContainer.Count-1]);
      for LIndex := LDataContainer.Count-2 downto 0 do
      begin
        if(Trim(LDataContainer[LIndex]) <> LValue) then
        begin
          LPos   := LIndex + 1;
          Break;
        end;
        LCount := LCount + 1;
      end;

      if(LCount-1 > 1) then
      begin
        LDataContainer[LPos] := IntToStr(LCount) + '*' + LValue;
        LCount := LCount -1;
        while (LCount > 0) do
        begin
          LDataContainer.Delete(LPos + 1);
          LCount := LCount -1;
        end;
      end;
      Result := LDataContainer.CommaText;
    finally
      LDataContainer.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function GetCompressedCommaTextIndexValue(AIndex:integer;ACommaTextData : string) : string;
const OPNAME = 'UUtilities.GetCompressedCommaTextIndexValue';
var
  LDataContainer   : TStringList;
begin
  Result := '';
  try
    if(Trim(ACommaTextData) = '') then  Exit;
    LDataContainer := TStringList.Create;
    try
      LDataContainer.CommaText :=  UnCompressCommatextString(ACommaTextData);
      if(AIndex >= 0) and (AIndex < LDataContainer.Count) then
        Result := LDataContainer[AIndex];
    finally
      LDataContainer.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function UpdateCompressCommatextString(AIndex:integer;AUpdateValue: string;var ACommaTextData : string) : boolean;
const OPNAME = 'UUtilities.UpdateCompressCommatextString';
var
  LDataContainer   : TStringList;
begin
  Result := False;
  try
    if(Trim(ACommaTextData) = '') then  Exit;
    LDataContainer := TStringList.Create;
    try
      ACommaTextData :=  UnCompressCommatextString(ACommaTextData);
      LDataContainer.CommaText := ACommaTextData;
      if(AIndex >= 0) and (AIndex < LDataContainer.Count) then
      begin
        LDataContainer[AIndex] := AUpdateValue;
        ACommaTextData :=  CompressCommatextString(LDataContainer.CommaText);
        Result := True;
      end;
    finally
      LDataContainer.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function ChangeSizeCommatextString(ANewSize:integer;AAddDefaultValue: string;var ACommaTextData : string) : boolean;
const OPNAME = 'UUtilities.ChangeSizeCommatextString';
var
  LDataContainer   : TStringList;
begin
  Result := False;
  try
    LDataContainer := TStringList.Create;
    try
      ACommaTextData           :=  UnCompressCommatextString(ACommaTextData);
      LDataContainer.CommaText := ACommaTextData;
      if (ANewSize > LDataContainer.Count) then
      begin
         while(LDataContainer.Count < ANewSize) do
            LDataContainer.Add(AAddDefaultValue)
      end
      else
      begin
         while(LDataContainer.Count > ANewSize) do
            LDataContainer.Delete(LDataContainer.Count-1);
      end;
      ACommaTextData :=  CompressCommatextString(LDataContainer.CommaText);
      Result := True;
    finally
      LDataContainer.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function FileCreationDate(AFileName: string): TDateTime;
const OPNAME = 'UUtilities.FileCreationDate';
var
  LFindData   : TWin32FindData;
  LFileDate   : DWord;
  LFileTime   : TFileTime;
  LFileHandle : THandle;
begin
  Result := 0.0;
  try
    // get file information
    LFileHandle := Windows.FindFirstFile(PChar(AFileName), LFindData);
    if(INVALID_HANDLE_VALUE <> LFileHandle)then
    begin
      // we're looking for just one file, so close our "find"
      Windows.FindClose( LFileHandle );
      // convert the FILETIME to local FILETIME
      FileTimeToLocalFileTime(LFindData.ftCreationTime, LFileTime );
      // convert FILETIME to DOS time
      FileTimeToDosDateTime(LFileTime, LongRec(LFileDate).Hi, LongRec(LFileDate).Lo);
      // finally, convert DOS time to TDateTime for use in Delphi's native date/time functions
      Result := FileDateToDateTime(LFileDate);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function FileLastAccessDate(AFileName: string): TDateTime;
const OPNAME = 'UUtilities.FileLastAccessDate';
var
  LFindData   : TWin32FindData;
  LFileDate   : DWord;
  LFileTime   : TFileTime;
  LFileHandle : THandle;
begin
  Result := 0.0;
  try
    // get file information
    LFileHandle := Windows.FindFirstFile(PChar(AFileName), LFindData);
    if(INVALID_HANDLE_VALUE <> LFileHandle)then
    begin
      // we're looking for just one file, so close our "find"
      Windows.FindClose( LFileHandle );
      // convert the FILETIME to local FILETIME
      FileTimeToLocalFileTime(LFindData.ftLastAccessTime, LFileTime );
      // convert FILETIME to DOS time
      FileTimeToDosDateTime(LFileTime, LongRec(LFileDate).Hi, LongRec(LFileDate).Lo);
      // finally, convert DOS time to TDateTime for use in Delphi's native date/time functions
      Result := FileDateToDateTime(LFileDate);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function FileLastWriteDate(AFileName: string): TDateTime;
const OPNAME = 'UUtilities.FileLastWriteDate';
var
  LFindData   : TWin32FindData;
  LFileDate   : DWord;
  LFileTime   : TFileTime;
  LFileHandle : THandle;
begin
  Result := 0.0;
  try
    // get file information
    LFileHandle := Windows.FindFirstFile(PChar(AFileName), LFindData);
    if(INVALID_HANDLE_VALUE <> LFileHandle)then
    begin
      // we're looking for just one file, so close our "find"
      Windows.FindClose( LFileHandle );
      // convert the FILETIME to local FILETIME
      FileTimeToLocalFileTime(LFindData.ftLastWriteTime, LFileTime );
      // convert FILETIME to DOS time
      FileTimeToDosDateTime(LFileTime, LongRec(LFileDate).Hi, LongRec(LFileDate).Lo);
      // finally, convert DOS time to TDateTime for use in Delphi's native date/time functions
      Result := FileDateToDateTime(LFileDate);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{$WARN SYMBOL_PLATFORM OFF}
function GISCoversDirectory:string;
const OPNAME = 'UUtilities.GISCoversDirectory';
var
  LDWSIndex, LIndex : integer;
  LDrive : char;
  LPath,
  lDirPart,
  LFilePart : string;
begin
  Result :='';
  try
    LPath := ExtractFilePath(ApplicationExeName);
    ProcessPath(LPath,LDrive,lDirPart,LFilePart);
    if(UpperCase(LDrive) = 'R') then
    begin
      Result := 'R:\GISViewer\Covers\';
    end
    else
    begin
      LIndex := Pos('DWA',LPath);
      LDWSIndex := Pos('DWS',LPath);
      if (LDWSIndex > 0) then
      begin
        LPath := Copy(LPath,1,LDWSIndex+3);
        LPath := IncludeTrailingPathDelimiter(LPath);
        Result := LPath+'GISViewer\Covers\';
      end;
      if(LIndex > 0) then
      begin
        LPath := Copy(LPath,1,LIndex+3);
        LPath := IncludeTrailingPathDelimiter(LPath);
        Result := LPath+'GISViewer\Covers\';
      end;
      {LPath := IncludeTrailingPathDelimiter(LPath);
      LPath := Copy(LPath,1,Length(LPath)-5);
      Result := LPath+'GISViewer\Covers\';}
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function getRectange(ALeft, ATop, AWidth, AHeight : integer; AAArr,AyArr : array of double) : boolean;
const OPNAME = 'UUtilities.getRectange';
//var
  //LXx : array of double;
  //LYy : array of double;
begin
  Result := False;
  try
    //SetLength(AAArr,4);
    //SetLength(AyArr,4);
    AAArr[0] := ALeft;
    AAArr[1] := ALeft;
    AAArr[2] := ALeft + AWidth;
    AAArr[3] := ALeft + AWidth;

    AyArr[0] := ATop;
    AyArr[1] := ATop + AHeight;
    AyArr[2] := ATop + AHeight;
    AyArr[3] := ATop;
    //AAArr := LXx; AyArr = LYy;
    //Finalize(LYy);
    //Finalize(LYy);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function UpdateGISShapeFilePath(AGLFFileName: string):boolean;
const OPNAME = 'UUtilities.UpdateGISShapeFilePath';
var
  LFileAttr,
  LIndex : integer;
  LNewPath: string;
  LFileContents: TStringList;
begin
  Result :=False;
  try
    if FileExists(AGLFFileName) then
    begin
      LFileContents := TStringList.Create;
      try
        LFileContents.LoadFromFile(AGLFFileName);
        if(LFileContents.Count > 0) then
        begin
          if(Pos('object',LFileContents[0]) > 0) then
          begin
            LNewPath := GISCoversDirectory;
            for LIndex := 0 to LFileContents.Count -1 do
            begin
              LFileContents[LIndex] := StringReplace(LFileContents[LIndex],'R:\GISViewer\Covers\',LNewPath,[rfReplaceAll, rfIgnoreCase]);
              LFileContents[LIndex] := StringReplace(LFileContents[LIndex],'C:\Program Files\DWA\GISViewer\Covers\',LNewPath,[rfReplaceAll, rfIgnoreCase]);
              LFileContents[LIndex] := StringReplace(LFileContents[LIndex],'R:\WRIMS\Deployment\Covers\',LNewPath,[rfReplaceAll, rfIgnoreCase]);
            end;
            LFileAttr := FileGetAttr(AGLFFileName);
            if (LFileAttr and faReadOnly) = faReadOnly then
              FileSetAttr(AGLFFileName, LFileAttr and not faReadOnly);
            LFileContents.SaveToFile(AGLFFileName);
            Result := True;
          end;
        end;
      finally
        LFileContents.Free;
      end;

    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;
{$WARN SYMBOL_PLATFORM ON}

function ExpandStudyKeysToStrings(AStudyKeys: string; AContainer: TStrings): boolean;
const OPNAME = 'UUtilities.ExpandStudyKeysToStrings';
var
  LPos       : integer;
  LSearchStr,
  LRowData   : string;
begin
  Result := False;
  try
    AContainer.Clear;
    AStudyKeys := Trim(AStudyKeys);
    if(AStudyKeys <> '') then
    begin
      LSearchStr := QuotedStr(',');
      Delete(LSearchStr,3,1);
      while (AStudyKeys <> '') do
      begin
        LPos := Pos(LSearchStr,AStudyKeys);
        if (LPos = 0) then
        begin
          AContainer.Add(AStudyKeys);
          AStudyKeys := '';
        end
        else
        begin
          LRowData := Copy(AStudyKeys,1,LPos);
          AContainer.Add(LRowData);
          Delete(AStudyKeys, 1, LPos+1)
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function ViewModelDataContextDataCommaText(AViewName: string; AModelElementID: integer;AModelElementName : string=''; ANodeIndex: Integer=-1): string;
const OPNAME = 'UUtilities.ViewModelDataContextDataCommaText';
var
  LContextDataList : TStringList;
begin
  Result := '';
  try
    LContextDataList := TStringList.Create;
    try
      AViewName := UpperCase(AViewName);
      LContextDataList.Add('VIEWNAME='+AViewName);
      LContextDataList.Add(Format('MODELELEMENTID=%d',[AModelElementID]));
      if(AModelElementName <> '') then
        LContextDataList.Add('MODELELEMENTNAME='+QuotedStr(AModelElementName));
      LContextDataList.Add(Format('NODEINDEX=%d',[ANodeIndex]));
      Result := LContextDataList.CommaText;
    finally
      FreeAndNil(LContextDataList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function WRMFMessageDialog(const Msg: string; DlgType: TMsgDlgType;Buttons: TMsgDlgButtons; Captions: array of string): Integer;
const OPNAME = 'UUtilities.WRMFMessageDialog';
var
  LMsgDlg: TForm;
  LIndex: Integer;
  LdlgButton: TButton;
  LCaptionIndex: Integer;
begin
  { Create the Dialog }
  { Dialog erzeugen }

  LMsgDlg := CreateMessageDialog(Msg, DlgType, Buttons);
  LCaptionIndex := 0;
  { Loop through Objects in Dialog }
  { Über alle Objekte auf dem Dialog iterieren}
    for LIndex := 0 to LMsgDlg.ComponentCount - 1 do
    begin
    { If the object is of type TButton, then }
    { Wenn es ein Button ist, dann...}
      if (LMsgDlg.Components[LIndex] is TButton) then
      begin
        LdlgButton := TButton(LMsgDlg.Components[LIndex]);
        if LCaptionIndex > High(Captions) then Break;
        { Give a new caption from our Captions array}
        { Schreibe Beschriftung entsprechend Captions array}
        LdlgButton.Caption := Captions[LCaptionIndex];
        Inc(LCaptionIndex);
      end;
    end;
  Result := LMsgDlg.ShowModal;
end;

function ErrorSeverity(var AErrorMsg: string): integer;
//TFilesActionYieldManager.
const OPNAME = 'UUtilities.ErrorSeverity';
begin
  Result := 0;
  try
    if(Pos('WARNING:',AErrorMsg) = 1) then
    begin
      AErrorMsg := Copy(AErrorMsg,9,Length(AErrorMsg));
      Result    := 1;
    end
    else
    if(Pos('ERROR:',AErrorMsg) = 1) then
    begin
      AErrorMsg := Copy(AErrorMsg,7,Length(AErrorMsg));
      Result    := 2;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{$WARNINGS OFF}
function DeleteDirectory(const Directory: TFileName):boolean;
const OPNAME = 'UUtilities.DeleteDirectory';
var
  LDrivesPathsBuff: array[0..1024] of char;
  LDrivesPaths: string;
  LLength: longword;
  LShortPath: array[0..MAX_PATH] of char;
  LDirectory: TFileName;

  // Recursively deletes all files and directories
  // inside the directory passed as parameter.
  procedure DeleleteDirTree(const Directory: TFileName);
  const OPNAME = 'UUtilities.DeleleteDirTree';
  var
    LSearchRec: TSearchRec;
    LAttributes: LongWord;
    LShortName,
    LFullName: TFileName;
    LFileNamePointer: pchar;
  begin
    try
      if SysUtils.FindFirst(Directory + '*', faAnyFile and not faVolumeID, LSearchRec) = 0 then
      begin
        try
          repeat // Processes all files and directories
            if LSearchRec.FindData.cAlternateFileName[0] = #0 then
              LShortName := LSearchRec.Name
            else
              LShortName := LSearchRec.FindData.cAlternateFileName;
            LFullName := Directory + LShortName;

            if (LSearchRec.Attr and faDirectory) <> 0 then
            begin
              // It's a directory
              if (LShortName <> '.') and (LShortName <> '..') then
                DeleleteDirTree(LFullName + '\');
            end
            else
            begin
              // It's a file
              LFileNamePointer := PChar(LFullName);
              LAttributes := GetFileAttributes(LFileNamePointer);
              if LAttributes = $FFFFFFFF then
                raise EInOutError.Create(SysErrorMessage(GetLastError));
              if (LAttributes and FILE_ATTRIBUTE_READONLY) <> 0 then
                SetFileAttributes(LFileNamePointer, LAttributes and not
                  FILE_ATTRIBUTE_READONLY);
              if Windows.DeleteFile(LFileNamePointer) = False then
                raise EInOutError.Create(SysErrorMessage(GetLastError));
            end;
          until SysUtils.FindNext(LSearchRec) <> 0;
        except
          SysUtils.FindClose(LSearchRec);
          raise;
        end;
        SysUtils.FindClose(LSearchRec);
      end;

      if Pos(#0 + Directory + #0, LDrivesPaths) = 0 then
      begin
        // if not a root directory, remove it
        LFileNamePointer := PChar(Directory);
        LAttributes := GetFileAttributes(LFileNamePointer);
        if LAttributes = $FFFFFFFF then
          raise EInOutError.Create(SysErrorMessage(GetLastError));
        if (LAttributes and FILE_ATTRIBUTE_READONLY) <> 0 then
          SetFileAttributes(LFileNamePointer, LAttributes and not FILE_ATTRIBUTE_READONLY);
        if Windows.RemoveDirectory(LFileNamePointer) = False then begin
          raise EInOutError.Create(SysErrorMessage(GetLastError));
        end;
      end;
    except on E: Exception do HandleError(E, OPNAME) end;
  end;

begin
  Result := False;
  try
    LDrivesPathsBuff[0] := #0;
    LLength := GetLogicalDriveStrings(1022, @LDrivesPathsBuff[1]);
    if LLength = 0 then
      raise EInOutError.Create(SysErrorMessage(GetLastError));
    SetString(LDrivesPaths, LDrivesPathsBuff, LLength + 1);
    LDrivesPaths := Uppercase(LDrivesPaths);
    LLength := GetShortPathName(PChar(Directory), LShortPath, MAX_PATH);
    if LLength = 0 then
      raise EInOutError.Create(SysErrorMessage(GetLastError));
    SetString(LDirectory, LShortPath, LLength);
    LDirectory := Uppercase(LDirectory);
    DeleleteDirTree(IncludeTrailingBackslash(LDirectory));
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function GetFileSize(const S: string): Int64;
const OPNAME = 'UUtilities.GetFileSize';
var
  LFindData: TWin32FindData;
  LFileHandle: THandle;
begin
  Result := 0;
  try
    LFileHandle := FindFirstFile(PChar(S), LFindData);
    if LFileHandle = INVALID_HANDLE_VALUE then
      Result := 0
    else
      //try
        Result := LFindData.nFileSizeHigh;
        Result := Result shl 32;
        Result := Result + LFindData.nFileSizeLow;
      //finally
      //  CloseHandle(LFileHandle);
      //end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function NumberOfMonthsBetween(AStartDate, AEndDate: TDatetime): Integer;
const OPNAME = 'UUtilities.NumberOfMonthsBetween';
var
    LStartYear,
    LStartMonth,
    LStartDay: Word;
    LEndYear,
    LEndMonth,
    LEndDay: Word;
begin
  Result := 0;
  try
    DecodeDate(AStartDate,LStartYear, LStartMonth, LStartDay);
    DecodeDate(AEndDate,LEndYear, LEndMonth, LEndDay);
    if abs(LEndYear-LStartYear)=0 then
        result:=(abs(LEndMonth-LStartMonth));
    if abs(LEndYear-LStartYear)>1 then
        result:=-LStartMonth+LEndMonth+(abs(LEndYear-LStartYear)*12);
    if abs(LEndYear-LStartYear)=1 then
        result:=(12-LStartMonth)+LEndMonth;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function GetElementDataTypeBySubNodesDatasetID(ASubNodesDatasetID : string) : TOutputDataType;
const OPNAME = 'UUtilities.GetElementDataTypeBySubNodesDatasetID';
begin
  Result := btNone;
  try
    if Trim(ASubNodesDatasetID) = 'SumOutYearFileNumbers1' then
      Result := btMonthEndReservoirVolume;
    if Trim(ASubNodesDatasetID) = 'SumOutYearFileNumbers10' then
      Result := btAnualFirmEnergyDemands ;
    if Trim(ASubNodesDatasetID) = 'SumOutYearFileNumbers11' then
      Result := btMonthlyAverageChannelFlow;
    if Trim(ASubNodesDatasetID) = 'SumOutYearFileNumbers12' then
      Result := btMonthlyPumpingEnergy;
    if Trim(ASubNodesDatasetID) = 'SumOutYearFileNumbers2' then
      Result := btMonthEndReservoirElevation;
    if Trim(ASubNodesDatasetID) = 'SumOutYearFileNumbers3' then
      Result := btNetBasinRunoffIntoResArea;
    if Trim(ASubNodesDatasetID) = 'SumOutYearFileNumbers4' then
      Result := btRainfallOnReservoirSurface;
    if Trim(ASubNodesDatasetID) = 'SumOutYearFileNumbers5' then
      Result := btGrossEvaporationLossFromReservoir;
    if Trim(ASubNodesDatasetID) = 'SumOutYearFileNumbers6' then
      Result := btMonthlyAveragePowerFlow;
    if Trim(ASubNodesDatasetID) = 'SumOutYearFileNumbers7' then
      Result := btMonthlyAverageSpillFlow;
    if Trim(ASubNodesDatasetID) = 'SumOutYearFileNumbers8' then
      Result := btMonthlyAverageStackedCapacity;
    if Trim(ASubNodesDatasetID) = 'SumOutYearFileNumbers9' then
      Result := btMonthlyAverageStackedEnergy;
    if Trim(ASubNodesDatasetID) = 'SumOutYearFileNumbers9' then
      Result := btMonthlyAverageChannelFlow
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function DoubleArrayToCommaText(AArray : array of double; ANumberOfDecimals : integer): string;
const OPNAME = 'UUtilities.DoubleArrayToCommaText';
var
  LIndex : integer;
  LFormat : string;
begin
  Result := '';
  try
    LFormat := '##0.';
    for LIndex := 1 to ANumberOfDecimals do
      LFormat := LFormat + '0';
    for LIndex := Low(AArray) to High(AArray) do
      Result := Result + FormatFloat(LFormat,AArray[LIndex]) + ',';
    if(Result <> '') and (Result[Length(Result)] = ',') then
      Result := Copy(Result,1,Length(Result) -1);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function IntegerArrayToCommaText(AArray : array of integer; ALength : integer=0): string;
const OPNAME = 'UUtilities.IntegerArrayToCommaText';
var
  LIndex : integer;
  LFormat : string;
begin
  Result := '';
  try
    LFormat := '';
    if(ALength > 0) then
      LFormat := '%'+IntToStr(ALength)+'.'+IntToStr(ALength)+'d';
    for LIndex := Low(AArray) to High(AArray) do
    begin
      if(ALength > 0) then
        Result := Result + Format(LFormat,[AArray[LIndex]]) + ','
      else
        Result := Result + IntToStr(AArray[LIndex]) + ',';
    end;
    if(Result <> '') and (Result[Length(Result)] = ',') then
      Result := Copy(Result,1,Length(Result) -1);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure SortFloatArray(var AArray: TOneDimensionDoubleArray; AOrder:TSortOrder);
const OPNAME = 'UUtilities.SortFloatArray';
var
  LOrderArray: TOneDimensionDoubleArray;
  LIndex : integer;
  LIndex2 : integer;
begin
  try
    QuickSortFloatArray(AArray,Low(AArray),High(AArray));
    if(AOrder = soDescending) then
    begin
      SetLength(LOrderArray,Length(AArray));
      try
        for LIndex := Low(AArray) to High(AArray) do
          LOrderArray[LIndex] := AArray[LIndex];
        LIndex2 := High(LOrderArray);
        for LIndex := Low(LOrderArray) to High(LOrderArray) do
        begin
          AArray[LIndex2] := LOrderArray[LIndex];
          LIndex2 := LIndex2-1;
        end;
      finally
        Finalize(LOrderArray);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure SortIntegerArray(var AArray: TOneDimensionIntegerArray; AOrder:TSortOrder);
const OPNAME = 'UUtilities.SortIntegerArray';
var
  LOrderArray: TOneDimensionIntegerArray;
  LIndex : integer;
  LIndex2 : integer;
begin
  try
    QuickSortIntegerArray(AArray,Low(AArray),High(AArray));
    if(AOrder = soDescending) then
    begin
      SetLength(LOrderArray,Length(AArray));
      try
        for LIndex := Low(AArray) to High(AArray) do
          LOrderArray[LIndex] := AArray[LIndex];
        LIndex2 := High(LOrderArray);
        for LIndex := Low(LOrderArray) to High(LOrderArray) do
        begin
          AArray[LIndex2] := LOrderArray[LIndex];
          LIndex2 := LIndex2-1;
        end;
      finally
        Finalize(LOrderArray);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure Save1DDoubleArrayToStrings(AContainer: TStrings; AArrayName :string;
          AArray: TOneDimensionDoubleArray;ALength1: integer;  ANumberOfDecimals : integer=3);
const OPNAME = 'UUtilities.Save1DDoubleArrayToStrings';
var
  LLine  : string;
  LRowCount : integer;
begin
  try
    if(AContainer <> nil) then
    begin
      LRowCount := 0;
      LLine := Format('%s %s %2.2d',[AArrayName,'Row',LRowCount])+ '  ' + DoubleArrayToCommaText(AArray,ANumberOfDecimals);
      AContainer.Add(LLine);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure Save2DDoubleArrayToStrings(AContainer: TStrings; AArrayName :string;
          AArray: TTwoDimensionDoubleArray; ALength1,ALength2: integer;  ANumberOfDecimals : integer=3);
const OPNAME = 'UUtilities.Save2DDoubleArrayToStrings';
var
  LIndex1    : integer;
  LIndex2   : integer;
  LLine     : string;
  LArray    : array of double;
  LRowCount : integer;
begin
  try
    if(AContainer <> nil) then
    begin
      LRowCount := 0;
      SetLength(LArray,ALength2);
      for LIndex1 :=0  to ALength1-1 do
      begin
        for LIndex2 := 0  to ALength2-1 do
          LArray[LIndex2] := AArray[LIndex1,LIndex2];
        LLine := Format('%s %s %2.2d',[AArrayName,'Row',LRowCount])+ '  ' + DoubleArrayToCommaText(LArray,ANumberOfDecimals);
        AContainer.Add(LLine);
        LRowCount := LRowCount+1;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure Save3DDoubleArrayToStrings(AContainer: TStrings; AArrayName :string;
          AArray: TThreeDimensionDoubleArray; ALength1,ALength2,ALength3: integer; ANumberOfDecimals : integer=3);
const OPNAME = 'UUtilities.Save3DDoubleArrayToStrings';
var
  LIndex1    : integer;
  LIndex2   : integer;
  LIndex3   : integer;
  LLine     : string;
  LArray    : array of double;
  LRowCount : integer;
begin
  try
    if(AContainer <> nil) then
    begin
      LRowCount := 0;
      SetLength(LArray,ALength3);
      for LIndex1 := 0  to ALength1-1 do
      begin
        for LIndex2 := 0  to ALength2-1 do
        begin
          for LIndex3 := 0  to ALength3-1 do
            LArray[LIndex3] := AArray[LIndex1,LIndex2,LIndex3];
          LLine := Format('%s %s %2.2d',[AArrayName,'Row',LRowCount])+ '  ' + DoubleArrayToCommaText(LArray,ANumberOfDecimals);
          AContainer.Add(LLine);
          LRowCount := LRowCount+1;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure Save1DIntegerArrayToStrings(AContainer: TStrings; AArrayName :string; AArray: TOneDimensionIntegerArray;
          ALength1: integer);
const OPNAME = 'UUtilities.Save1DIntegerArrayToStrings';
var
  LLine  : string;
  LRowCount : integer;
begin
  try
    if(AContainer <> nil) then
    begin
      LRowCount := 0;
      LLine := Format('%s %s %2.2d',[AArrayName,'Row',LRowCount])+ '  ' + IntegerArrayToCommaText(AArray);
      AContainer.Add(LLine);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure Save2DIntegerArrayToStrings(AContainer: TStrings; AArrayName :string; AArray: TTwoDimensionIntegerArray;
          ALength1,ALength2: integer);
const OPNAME = 'UUtilities.Save2DIntegerArrayToStrings';
var
  LIndex1    : integer;
  LIndex2   : integer;
  LLine     : string;
  LArray    : array of integer;
  LRowCount : integer;
begin
  try
    if(AContainer <> nil) then
    begin
      LRowCount := 0;
      SetLength(LArray,ALength2);
      for LIndex1 := 0  to ALength1-1 do
      begin
        for LIndex2 :=0  to ALength2-1 do
          LArray[LIndex2] := AArray[LIndex1,LIndex2];
        LLine := Format('%s %s %2.2d',[AArrayName,'Row',LRowCount])+ '  ' + IntegerArrayToCommaText(LArray);
        AContainer.Add(LLine);
        LRowCount := LRowCount+1;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure Save3DIntegerArrayToStrings(AContainer: TStrings; AArrayName :string; AArray: TThreeDimensionIntegerArray;
          ALength1,ALength2,ALength3: integer);
const OPNAME = 'UUtilities.Save3DDoubleArrayToStrings';
var
  LIndex1    : integer;
  LIndex2   : integer;
  LIndex3   : integer;
  LLine     : string;
  LArray    : array of integer;
  LRowCount : integer;
begin
  try
    if(AContainer <> nil) then
    begin
      LRowCount := 0;
      SetLength(LArray,ALength3);
      for LIndex1 := 0  to ALength1-1 do
      begin
        for LIndex2 := 0  to ALength2-1 do
        begin
          for LIndex3 := 0  to ALength3-1 do
            LArray[LIndex3] := AArray[LIndex1,LIndex2,LIndex3];
          LLine := Format('%s %s %2.2d',[AArrayName,'Row',LRowCount])+ '  ' + IntegerArrayToCommaText(LArray);
          AContainer.Add(LLine);
          LRowCount := LRowCount+1;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{This code performs the sort in the ascending order}
procedure QuickSortFloatArray(var AList: TOneDimensionDoubleArray; ALeft, ARight : integer);
const OPNAME = 'UUtilities.QuickSortFloatArray';
var
  LPivot : integer;
  procedure SplitFloatArray(var AList: TOneDimensionDoubleArray; ALeft, ARight : integer);
  const OPNAME = 'UUtilities.SplitFloatArray';
  var
    x,t : double;
    i,j :Integer;
  begin
    try
      i:=ALeft;
      x:=AList[ALeft];
      for j:=ALeft+1 to ARight do { this is to ARight. not to N }
        if x>=AList[j] then
        begin
          inc(i);
          { exchange AList[i] and AList[j] }
          t:=AList[i];
          AList[i]:=AList[j];
          AList[j]:=t;
          { exchange end }
        end;
        { end if }
      {for end}
      { exchange AList[i] and AList[ALeft] }
      t:=AList[ALeft];
      AList[ALeft]:=AList[i];
      AList[i]:=t;
      { exchange end }
      LPivot:=i;
    except on E: Exception do HandleError(E, OPNAME); end;
  end;
  { end split }

begin
  try
    LPivot := NullInteger;
    if ALeft<ARight then
    begin
      SplitFloatArray(AList, ALeft, ARight);
      QuickSortFloatArray(AList, ALeft,LPivot-1);
      QuickSortFloatArray(AList,LPivot+1,ARight);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{This code performs the sort in the ascending order}
procedure QuickSortIntegerArray(var AList: TOneDimensionIntegerArray; ALeft, ARight : integer);
const OPNAME = 'UUtilities.QuickSortIntegerArray';
var
  LPivot : integer;
  procedure SplitIntegerArray(var AList: TOneDimensionIntegerArray; ALeft, ARight : integer);
  const OPNAME = 'UUtilities.SplitIntegerArray';
  var
    x,t : integer;
    i,j :Integer;
  begin
    try
      i:=ALeft;
      x:=AList[ALeft];
      for j:=ALeft+1 to ARight do { this is to ARight. not to N }
        if x>=AList[j] then
        begin
          inc(i);
          { exchange AList[i] and AList[j] }
          t:=AList[i];
          AList[i]:=AList[j];
          AList[j]:=t;
          { exchange end }
        end;
        { end if }
      {for end}
      { exchange AList[i] and AList[ALeft] }
      t:=AList[ALeft];
      AList[ALeft]:=AList[i];
      AList[i]:=t;
      { exchange end }
      LPivot:=i;
    except on E: Exception do HandleError(E, OPNAME); end;
  end;
  { end split }

begin
  try
    LPivot := NullInteger;
    if ALeft<ARight then
    begin
      SplitIntegerArray(AList, ALeft, ARight);
      QuickSortIntegerArray(AList, ALeft,LPivot-1);
      QuickSortIntegerArray(AList,LPivot+1,ARight);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure RestoreFormProperties(AForm: TForm);
const OPNAME = 'UUtilities.RestoreFormProperties';
      C_IniSection_FormViews = 'FormViews';
var
  LIniFile   : TIniFile;
  LFileName  : string;
  LViewName  : string;
  LCommaText : string;
  LFields    : TStringList;
begin
  try
    LFileName :=  ChangeFileExt(Application.ExeName,'View.ini');
    if FileExists(LFileName) then
    begin
      LIniFile   := TIniFile.Create(LFileName);
      LFields := TStringList.Create;
      try
        // Construct the view name from the form's class name.
        LViewName := AForm.ClassName;

        // Read the data from the ini file.
        // Apply a standard size if the data is not found.
        //LCommaText := LIniFile.ReadString(C_IniSection_FormViews, LViewName, '0,100,50,440,380');
        LCommaText := LIniFile.ReadString(C_IniSection_FormViews, LViewName, '');

        // Attempt to extract the pixel values. Abort on error.  V,L,T,R,B
        if(LCommaText <> '') then
        begin
          LFields.CommaText := LCommaText;
          AForm.Left        := StrToInt(LFields[1]);
          AForm.Top         := StrToInt(LFields[2]);
          AForm.Width       := StrToInt(LFields[3]) - AForm.Left;
          AForm.Height      := StrToInt(LFields[4]) - AForm.Top;

          // Do the window state last.
          AForm.WindowState := TWindowState(StrToInt(LFields[0]));
        end;
      // Destroy string list.
      finally
        LIniFile.Free;
        LFields.Free;
      end;
      //LoadFormGraphics(AForm);
      //RestoreGridsProperties(AForm);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure SaveFormProperties(AForm: TForm);
const OPNAME = 'UUtilities.SaveFormProperties';
      C_IniSection_FormViews = 'FormViews';
var
  LIniFile   : TIniFile;
  LFileName  : string;
  LViewName  : string;
  LWindowPlacement: WINDOWPLACEMENT;
begin
  try
    LFileName :=  ChangeFileExt(Application.ExeName,'View.ini');
    LIniFile   := TIniFile.Create(LFileName);
    try
      // Construct the view name from the form's class name.
      LViewName := AForm.ClassName;

      // Allocate enough memory for the data.
      LWindowPlacement.length := sizeof(WINDOWPLACEMENT);

      // Get the correct sizes to store.
      if GetWindowPlacement(AForm.Handle, @LWindowPlacement) then
      begin

        // Construct the view name from the form's class name.
        LViewName := AForm.ClassName;

        // V,L,T,R,B
        LIniFile.WriteString(C_IniSection_FormViews, LViewName,
          IntToStr(Integer(AForm.WindowState)) + ',' +
          IntToStr(LWindowPlacement.rcNormalPosition.Left) + ',' +
          IntToStr(LWindowPlacement.rcNormalPosition.Top) + ',' +
          IntToStr(LWindowPlacement.rcNormalPosition.Right) + ',' +
          IntToStr(LWindowPlacement.rcNormalPosition.Bottom));
      end;

    // Destroy string list.
    finally
      LIniFile.Free;
    end;
    //SaveGridsProperties(AForm);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function FileCopy(const ASourceFileName, AdestinationFileName: string): boolean;
const OPNAME = 'UUtilities.FileCopy';
var
  lSourceStream,
  lDestStream: TFileStream;
begin
  Result := False;
  try
    lSourceStream := TFileStream.Create(ASourceFileName, fmOpenRead);
    try
      lDestStream := TFileStream.Create(AdestinationFileName, fmCreate);
      try
        lDestStream.CopyFrom(lSourceStream, 0);//read the whole block
        Result := True;
      finally
        lDestStream.Free;
      end;
    finally
      lSourceStream.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function ExtractChannelNameFromComment(AComment:string):string;
const OPNAME = 'UUtilities.ExtractChannelNameFromComment';
var
  LChar : Char;
begin
  Result := Trim(AComment);
  try
    if(Result <> '') then
    begin
      while (Length(Result) > 0) do
      begin
        LChar := Result[1];
        if((LChar >= '0') and (LChar <= '9')) or ((LChar >= 'a') and (LChar <= 'z')) or ((LChar >= 'A') and (LChar <= 'Z')) then
          break
        else
        begin
          Delete(Result,1,1);
          Result := Trim(Result);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function RemoveTrailingMemoChars(ASource:string):string;
const OPNAME = 'UUtilities.RemoveTrailingMemoChars';
var
  LIndex : integer;
begin
  Result := ASource;
  try
    LIndex := Length(Result);
    if(LIndex >= 2) then
    begin
      if(Result[LIndex-1] = #13) and (Result[LIndex] = #10) then
        Result := Copy(Result,1,LIndex-2);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function ExtractDBFileNameFromADOConnectionStr(AValue: string): string;
const OPNAME = 'UUtilities.ExtractDBFileNameFromADOConnectionStr';
var
  LContainer : TStringList;
  LValues    : string;
begin
  Result := '';
  try
    LContainer := TStringList.Create;
    try
      LContainer.Add(AValue);
      LValues := LContainer.ValueFromIndex[0];
      if(LValues <> '') then
      begin
        StringToStringList(LValues,LContainer,';');
        Result := LContainer.Values['Data Source'];
      end;
    finally
      LContainer.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function StringToStringList(AString:String; AStrings: TStrings;ADelimeter: char = ','):boolean;
const OPNAME = 'UUtilities.StringToStringList';
var
  LIndex : integer;
begin
  Result := False;
  try
    AStrings.Clear;
    LIndex := Pos(ADelimeter,AString);
    while LIndex > 0 do
    begin
      AStrings.Add(Copy(AString,1,LIndex-1));
      AString := Copy(AString,LIndex+1,Length(AString));
      LIndex := Pos(ADelimeter,AString);
    end;
    if(AString <> '') then
      AStrings.Add(Trim(AString));
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function GetHydrologyMonth(const AMonth : integer):integer;
const OPNAME = 'UUtilities.GetHydrologyMonth';
begin
  Result := 0;
  try
  if (AMonth <= 3) then
    Result := AMonth + 9
  else
    Result := AMonth - 3;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function ConvertHydrologicalMonthToCalendarMonth(ACalendarStartMonth,AHydrologicalYear, AHydrologicalMonth: integer): integer;
const OPNAME = 'UUtilities.ConvertHydrologicalMonthToCalendarMonth';
begin
  if(AHydrologicalMonth < 1) or (AHydrologicalMonth > 12) then
     AHydrologicalMonth :=  1;

  Result := AHydrologicalMonth;
  try

    if(ACalendarStartMonth > 1) then
    begin
      Result := ACalendarStartMonth + AHydrologicalMonth -1;
      if(Result > 12) then
        Result := Result - 12;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function ConvertHydrologicalYearToCalendarYear(ACalendarStartMonth,AHydrologicalYear, AHydrologicalMonth: integer): integer;
const OPNAME = 'UUtilities.ConvertHydrologicalYearToCalendarYear';
begin
  if(AHydrologicalMonth < 1) or (AHydrologicalMonth > 12) then
     AHydrologicalMonth :=  1;

  Result := AHydrologicalYear;
  try
    if(ACalendarStartMonth > 1) then
    begin
      if((ACalendarStartMonth + AHydrologicalMonth -1) > 12) then
       Result := AHydrologicalYear+1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function ConvertCalendarMonthToHydrologicalMonth(ACalendarStartMonth,ACalendarYear,ACalendarMonth: integer): integer;
const OPNAME = 'UUtilities.ConvertCalendarMonthToHydrologicalMonth';
{var
  LStartDate,
  LEndDate      : TDateTime;
  LMonthsBetween : integer;
  LCalendarStartYear : integer;}
begin
  Result := ACalendarMonth;
  try
    if(ACalendarStartMonth > 1) then
    begin
      Result := (ACalendarMonth - ACalendarStartMonth) + 1;
      if(Result <= 0) then
        Result := 12 + Result;
{
      LCalendarStartYear := ACalendarYear;
      if(ACalendarMonth < ACalendarStartMonth) then
        LCalendarStartYear := LCalendarStartYear - 1;

      LStartDate      := EncodeDate(LCalendarStartYear,ACalendarStartMonth,01);
      LEndDate        := EncodeDate(ACalendarYear,ACalendarMonth,01);

      LMonthsBetween  := MonthsBetween(LEndDate,LStartDate);
      if((ACalendarStartMonth + LMonthsBetween) > 12) then
        LMonthsBetween := (ACalendarStartMonth + LMonthsBetween) - 12;

      Result := LMonthsBetween+1;}
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function ConvertCalendarYearToHydrologicalYear(ACalendarStartMonth,ACalendarYear,ACalendarMonth: integer): integer;
const OPNAME = 'UUtilities.ConvertCalendarYearToHydrologicalYear';
var
  //LStartDate,
  //LEndDate      : TDateTime;
  //LMonthsBetween : integer;
  LCalendarStartYear : integer;
begin
  Result := ACalendarYear;
  try
    if(ACalendarStartMonth > 1) then
    begin
      LCalendarStartYear := ACalendarYear;
      if(ACalendarMonth < ACalendarStartMonth) then
        LCalendarStartYear := LCalendarStartYear - 1;
        Result := LCalendarStartYear;


      {LStartDate      := EncodeDate(LCalendarStartYear,ACalendarStartMonth,01);
      LEndDate        := EncodeDate(ACalendarYear,ACalendarMonth,01);

      LMonthsBetween  := MonthsBetween(LEndDate,LStartDate);
      if((ACalendarStartMonth + LMonthsBetween) > 12) then
        Result := ACalendarYear -1;}
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


(* C# code
 /// <summary>
    /// True if a is within *EPSILON of b
    /// </summary>
    /// <param name="a"></param>
    /// <param name="b"></param>
    /// <returns></returns>
    private bool CompareDouble(double a, double b)
    {
        // check close to zero first
        if ( Math.Abs(a) < (EPSILON-1.0)   &&  Math.Abs(b) < (EPSILON-1.0) )
            return true;
        if (a==b)
            return true; // handles infinity

        if (a < 0.0)
        {   // at least one is negative - assume both are
            if (a * EPSILON > b)
                return false;
            if (b * EPSILON > a)
                return false;
        }
        else // at least one is positive
        {
            if (a * EPSILON < b)
                return false;
            if (b * EPSILON < a)
                return false;
            return true;
        }
        return false; // should not reach here
    }

*)

//Delphi Code

function MWGCompareDouble(ADouble,BDouble : double):boolean;
const OPNAME = 'UUtilities.MWGCompareDouble';
var
  EPSILON : double;
begin
  Result := False;
  try
    //EPSILON := 0.00000001;
    if SameValue(ADouble,BDouble,EPSILON) then
    begin
      Result :=  True;
      Exit;
    end;

    if (Abs(ADouble) < (EPSILON-1.0))   and
      (Abs(BDouble) < (EPSILON-1.0)) then
    begin
      Result :=  True;
      Exit;
    end;
    if (ADouble = BDouble) then
    begin
      Result :=  True; // handles infinity
      Exit;
    end;

    if (ADouble < 0.0) then
    begin  // at least one is negative - assume both are
      if (ADouble * EPSILON > BDouble) then
      begin
        Result := False;
        Exit
      end;
      if (BDouble * EPSILON > ADouble) then
      begin
        Result := False;
        Exit;
      end;
    end
    else // at least one is positive
    begin
      if (ADouble * EPSILON < BDouble) then
      begin
        Result := False;
        Exit
      end;
      if (BDouble * EPSILON < ADouble) then
      begin
        Result := False;
        Exit
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



procedure DoNothing;
const OPNAME = 'UUtilities.DoNothing';
begin
  try
 except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
