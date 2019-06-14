//
//  Contains THydroDBManager Class, the manager for the Hydrology model database
//  for study selection functionality like CreateNew, Delete, Copy, Export and Import.
//
unit UHydroDBManager;

interface

uses
  VCL.Forms,
  Classes,
  DB,
  UUtilities,
  UAbstractObject,
  HydrologyCom_TLB;

type
  THydroDBManager = class(TAbstractAppObject)
  protected
  private
    procedure GetNetworkModuleIDs (ANetworkID : Integer;
                                   AModuleIDs : TStringList);
    procedure PopulateNetworkTablesList (ANetworkTablesList : TStringList);
    procedure PopulateModuleTablesList (AModuleTablesList : TStringList);
    function IsNetworkReadOnly (ANetworkID : Integer) : Boolean;

    function CopyNetworkDB (AOldNetworkID   : Integer;
                            ANewNetworkID   : Integer;
                            AOldNetworkCode : String;
                            ANewNetworkCode : String): Boolean;
    function CopyNetworkModules (AOldNetworkID : Integer;
                                 ANewNetworkID : Integer;
                                 AOldModuleIDs : TStringList;
                                 ANewModuleIDs : TStringList): Boolean;
    function CopyModuleTable (ATableName    : String;
                              AOldModuleIDs : TStringList;
                              ANewModuleIDs : TStringList;
                              AModuleIDs    : TStringList) : Boolean;
    function CopyNetworkTable (ATableName    : String;
                               AOldNetworkID : Integer;
                               ANewNetworkID : Integer) : Boolean;
    function CopyNetworkRoutes (AOldNetworkID : Integer;
                                ANewNetworkID : Integer;
                                AOldModuleIDs : TStringList;
                                ANewModuleIDs : TStringList;
                                AOldRouteIDs  : TStringList;
                                ANewRouteIDs  : TStringList) : Boolean;
    function CopySimulationResults (AOldNetworkID : Integer;
                                    ANewNetworkID : Integer;
                                    AOldModuleIDs : TStringList;
                                    ANewModuleIDs : TStringList;
                                    AOldRouteIDs  : TStringList;
                                    ANewRouteIDs  : TStringList) : Boolean;
    function CopyHydroNVDiagrams (AOldNetworkCode : WideString;
                                  ANewNetworkCode : WideString;
                                  AOldNetworkID   : Integer;
                                  ANewNetworkID   : Integer;
                                  ADiagramDir     : String): WordBool;
    function ExportTableXML (ATableName  : String;
                             AQuery      : TDataSet;
                             AXMLFileDir : String) : Boolean;
    function ExportHydroNVDiagrams (AXMLFileDir : String;
                                    ANetworkID  : Integer;
                                    ADiagramDir : String) : Boolean;
    function CheckNetworkCode (AFileName        : String;
                               var ANetworkCode : WideString;
                               var AErrorMsg    : WideString) : Boolean;
    function ImportNetworkTableXML (ADirectory    : String;
                                    ATableName    : String;
                                    ANetworkID    : Integer) : Boolean;
    function ImportSimulationResultsXML (ADirectory    : String;
                                         ANetworkID    : Integer;
                                         AOldModuleIDs : TStringList;
                                         ANewModuleIDs : TStringList;
                                         AOldRouteIDs  : TStringList;
                                         ANewRouteIDs  : TStringList) : Boolean;
    function ImportModuleTableXML (ADirectory    : String;
                                   ATableName    : String;
                                   AMustExist    : Boolean;
                                   AOldModuleIDs : TStringList;
                                   ANewModuleIDs : TStringList) : Boolean;
    function ImportNetworkModulesXML (ADirectory    : String;
                                      ANetworkID    : Integer;
                                      AOldModuleIDs : TStringList;
                                      ANewModuleIDs : TStringList) : Boolean;
    function ImportNetworkRoutesXML (ADirectory    : String;
                                     ANetworkID    : Integer;
                                     AOldModuleIDs : TStringList;
                                     ANewModuleIDs : TStringList;
                                     AOldRouteIDs  : TStringList;
                                     ANewRouteIDs  : TStringList) : Boolean;
    function ImportHydroNVDiagramsXML (AXMLFileDir : String;
                                       ANetworkID  : Integer;
                                       ADiagramDir : String) : Boolean;
    function DeleteHydroNVDiagrams (ANetworkID  : Integer;
                                    ADiagramDir : String): WordBool;
  public
    function GetNetworkID (ANetworkCode : String) : Integer;
    function GetNetworkCode (ANetworkID : Integer) : String;
    function GetNetworkDiagramsDirectory(ANetworkCode : string): string;
    function CreateNewNetwork (ANetworkCode         : WideString;
                               AVersionNo           : Integer;
                               AInputDir            : WideString;
                               AOutputDir           : WideString;
                               ADebugRequired       : WideString;
                               ADebugStartPeriod    : Integer;
                               ADebudEndPeriod      : Integer;
                               ASummaryRequired     : WideString;
                               ASimulationStartYear : Integer;
                               ASimulationEndYear   : Integer;
                               AReadOnly            : Integer;
                               var ANetworkID       : Integer;
                               var AErrorMsg        : WideString) : WordBool;
    function DeleteNetwork (ANetworkID    : Integer;
                            var AErrorMsg : WideString): WordBool;
    function CopyNetwork (ANetworkID      : Integer;
                          ANewNetworkCode : WideString;
                          var AErrorMsg   : WideString): WordBool;
    function ExportNetwork (ANetworkID    : Integer;
                            ADirectory    : WideString;
                            var AErrorMsg : WideString): WordBool;
    function ImportNetwork (ADirectory    : WideString;
                            ANetworkID    : Integer;
                            var AErrorMsg : WideString): WordBool;
    function Get_AllNetworkIDsCommText : WideString;
    function Get_AllNetworkCodeIDsNameValuePairs : WideString;
    function Get_NetworkPropertiesCommText(ANetworkID : integer) : WideString;
    function Set_NetworkPropertiesCommText(ANetworkPropertiesCommText : WideString) : boolean;
    function DoesNetworkExist(ANetworkID : integer) : boolean; overload;
    function DoesNetworkExist(ANetworkCode : WideString) : boolean; overload;
  end;

implementation

uses
  SysUtils,
  Windows,
  VCL.ComCtrls,
  VCL.Controls,

  DBClient,
  Provider,
  VCL.Dialogs,
  XMLIntf,
  XMLDoc,
  xmldom,

  UHydroDBAgent,
  UErrorHandlingOperations, Math;

function THydroDBManager.CreateNewNetwork (ANetworkCode         : WideString;
                                           AVersionNo           : Integer;
                                           AInputDir            : WideString;
                                           AOutputDir           : WideString;
                                           ADebugRequired       : WideString;
                                           ADebugStartPeriod    : Integer;
                                           ADebudEndPeriod      : Integer;
                                           ASummaryRequired     : WideString;
                                           ASimulationStartYear : Integer;
                                           ASimulationEndYear   : Integer;
                                           AReadOnly            : Integer;
                                           var ANetworkID       : Integer;
                                           var AErrorMsg        : WideString): WordBool;
const OPNAME = 'THydroDBManager.CreateNewNetwork';
var
  LSQL       : String;
  LResult    : Boolean;
begin
  Result := FALSE;
  try
    ANetworkID := GetNetworkID(ANetworkCode);
    if (ANetworkID <> 0) then
    begin
      AErrorMsg := 'Network ' + ANetworkCode + ' already exists.';
    end
    else
    begin
      AInputDir  := IncludeTrailingPathDelimiter(AInputDir);
      AOutputDir := IncludeTrailingPathDelimiter(AOutputDir);
      ANetworkID := GHydroDBAgent.GetNextID('Network', 'NetworkID');
      LSQL := 'INSERT INTO Network (NetworkID, NetworkCode, VersionNo, InputDirectory, OutputDirectory, ' +
              'DebugRequired, DebugStartPeriod, DebugEndPeriod, SummaryRequired, SimulationStartYear, ' +
              'SimulationEndYear, IsReadOnly) VALUES (' +
              IntToStr(ANetworkID) + ', ' +
              QuotedStr(ANetworkCode) + ', ' +
              IntToStr(AVersionNo) + ', ' +
              QuotedStr(AInputDir) + ', ' +
              QuotedStr(AOutputDir) + ', ' +
              QuotedStr(ADebugRequired) + ', ' +
              IntToStr(ADebugStartPeriod) + ', ' +
              IntToStr(ADebudEndPeriod) + ', ' +
              QuotedStr(ASummaryRequired) + ', ' +
              IntToStr(ASimulationStartYear) + ', ' +
              IntToStr(ASimulationEndYear) + ', ' +
              IntToStr(AReadOnly) + ')';
      LResult := GHydroDBAgent.ExecuteSQL(LSQL, True);
      Result := LResult;
    end;  
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroDBManager.PopulateNetworkTablesList (ANetworkTablesList : TStringList);
const OPNAME = 'THydroDBManager.PopulateNetworkTablesList';
begin
  try
    ANetworkTablesList.Clear;
    ANetworkTablesList.Add('ObservationPoints');                             // [1]
    ANetworkTablesList.Add('NetworkModules');                                // [37]
    ANetworkTablesList.Add('HydroNVDrawings');                               // [39]
    ANetworkTablesList.Add('SimulationResults');                             // [40]
    ANetworkTablesList.Add('NetworkRoutes');                                 // [36]
    ANetworkTablesList.Add('Network');                                       // [41]
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroDBManager.PopulateModuleTablesList (AModuleTablesList : TStringList);
const OPNAME = 'THydroDBManager.PopulateModuleTablesList';
begin
  try
    AModuleTablesList.Clear;
    AModuleTablesList.Add('Modules');                                        // [38]
    AModuleTablesList.Add('OutflowRoutes');                                  // [2]
    AModuleTablesList.Add('InflowRoutes');                                   // [3]
    AModuleTablesList.Add('Pan');                                            // [4]
    AModuleTablesList.Add('CompWetlandParams');                              // [5]
    AModuleTablesList.Add('ChannelModules');                                 // [6]
    AModuleTablesList.Add('ReservoirVolumeArea');                            // [7]
    AModuleTablesList.Add('ReservoirModules');                               // [8]
    AModuleTablesList.Add('RunOffOutflowRoutes');                            // [9]
    AModuleTablesList.Add('RunOffAfforestationAreaData');                    // [10]
    AModuleTablesList.Add('RunOffAfforestationParams');                      // [11]
    AModuleTablesList.Add('RunOffAlienVegetationAreaData');                  // [12]
    AModuleTablesList.Add('RunOffAlienVegetationParams');                    // [13]
    AModuleTablesList.Add('RunOffGroundwaterAbstractionData');               // [14]
    AModuleTablesList.Add('RunOffPavedAreaData');                            // [15]
    AModuleTablesList.Add('RunOffModuleSlaves');                             // [16]
    AModuleTablesList.Add('RunOffHughesGWParams');                           // [17]
    AModuleTablesList.Add('RunOffSamiGWParams');                             // [18]
    AModuleTablesList.Add('RunOffModules');                                  // [19]
    AModuleTablesList.Add('IrrigationReturnFlow');                           // [20]
    AModuleTablesList.Add('IrrigationModulesMonthlyData');                   // [21]
    AModuleTablesList.Add('IrrigationEfficiency');                           // [22]
    AModuleTablesList.Add('IrrigationCrops');                                // [23]
    AModuleTablesList.Add('IrrigationArea');                                 // [24]
    AModuleTablesList.Add('IrrigationAllocationGrowth');                     // [25]
    AModuleTablesList.Add('IrrigationModules');                              // [26]
    AModuleTablesList.Add('MineModulesGrowthData');                          // [27]
    AModuleTablesList.Add('MineQvsSLDData');                                 // [28]
    AModuleTablesList.Add('MineOpencastMonthlyData');                        // [29]
    AModuleTablesList.Add('MineOpencastData');                               // [30]
    AModuleTablesList.Add('MineUndergroundMonthlyData');                     // [31]
    AModuleTablesList.Add('MineUndergroundData');                            // [32]
    AModuleTablesList.Add('MineSlurryMonthlyData');                          // [33]
    AModuleTablesList.Add('MineSlurryData');                                 // [34]
    AModuleTablesList.Add('MineModules');                                    // [35]
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroDBManager.DeleteNetwork (ANetworkID    : Integer;
                                        var AErrorMsg : WideString): WordBool;
const OPNAME = 'THydroDBManager.DeleteNetwork';
var
  LNetworkCode     : string;
  LSQL              : String;
  LModuleIDList     : TStringList;
  LResult           : Boolean;
  LModuleIDs        : String;
  LModuleTableList  : TStringList;
  LNetworkTableList : TStringList;
  LIndex            : Integer;
  LTableName        : String;
  LDiagramDir        : String;
begin
  Result := FALSE;
  try
    LNetworkCode := GetNetworkCode(ANetworkID);
    if (LNetworkCode = '') then
    begin
      AErrorMsg := 'Network ' + IntToStr(ANetworkID) + ' does not exist.';
    end
    else if (IsNetworkReadOnly(ANetworkID)) then
    begin
      AErrorMsg := 'Network ' + LNetworkCode + ' is ReadOnly.';
    end
    else
    begin
      LDiagramDir       := GetNetworkDiagramsDirectory(LNetworkCode);
      LModuleIDList     := TStringList.Create;
      LModuleTableList  := TStringList.Create;
      LNetworkTableList := TStringList.Create;
      try
        GetNetworkModuleIDs(ANetworkID, LModuleIDList);
        PopulateNetworkTablesList(LNetworkTableList);
        PopulateModuleTablesList(LModuleTableList);
        GHydroDBAgent.StartTransaction;
        LResult := TRUE;
        try
          if (LModuleIDList.Count > 0) then
          begin
            LModuleIDs := LModuleIDList.CommaText;

            LIndex := 0;
            while (LResult AND (LIndex < LModuleTableList.Count)) do
            begin
              LTableName := LModuleTableList.Strings[LIndex];
              LSQL := 'DELETE FROM ' + LTableName + ' WHERE ModuleID IN (' + LModuleIDs + ')';
              LResult := GHydroDBAgent.ExecuteSQL(LSQL, True);
              if (NOT LResult) then
                AErrorMsg := 'Could not delete ' + LNetworkCode + ' from table ' + LTableName + '.';
              LIndex := LIndex + 1;
            end;
          end;

          LIndex := 0;
          while (LResult AND (LIndex < LNetworkTableList.Count)) do
          begin
            LTableName := LNetworkTableList.Strings[LIndex];
            if (LTableName = 'HydroNVDrawings') then
              LResult := DeleteHydroNVDiagrams(ANetworkID, LDiagramDir)
            else
            begin
              LSQL := 'DELETE FROM ' + LTableName + ' WHERE NetworkID = ' + IntToStr(ANetworkID);
              LResult := GHydroDBAgent.ExecuteSQL(LSQL, True);
            end;
            if (NOT LResult) then
              AErrorMsg := 'Could not delete ' + LNetworkCode + ' from table ' + LTableName + '.';
            LIndex := LIndex + 1;
          end;

          if (LResult) then
          begin
            GHydroDBAgent.CommitTransaction;
            Result := LResult;
          end
          else
            GHydroDBAgent.RollbackTransaction;
        except
          GHydroDBAgent.RollbackTransaction;
        end;
      finally
        FreeAndNil(LModuleIDList);
        FreeAndNil(LModuleTableList);
        FreeAndNil(LNetworkTableList);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroDBManager.DeleteHydroNVDiagrams (ANetworkID  : Integer;
                                                ADiagramDir : String): WordBool;
const OPNAME = 'THydroDBManager.DeleteHydroNVDiagrams';
var
  LSQL      : String;
  LQuery    : TDataSet;
  LFileName : String;
  LReadOnly : Integer;
begin
  Result := FALSE;
  try
    LSQL := 'SELECT * FROM HydroNVDrawings WHERE NetworkID = ' + IntToStr(ANetworkID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        LReadOnly := LQuery.FieldByName('ReadOnly').AsInteger;
        if (LReadOnly = 0) then
        begin
          LFileName := ADiagramDir + Trim(LQuery.FieldByName('DrawingName').AsString) + '.VSD';
          if (FileExists(LFileName)) then
            SysUtils.DeleteFile(LFileName);
        end;
        LQuery.Next;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;

    LSQL := 'DELETE FROM HydroNVDrawings WHERE NetworkID = ' + IntToStr(ANetworkID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroDBManager.GetNetworkID (ANetworkCode : String) : Integer;
const OPNAME = 'THydroDBManager.GetNetworkID';
var
  LQuery      : TDataSet;
  LSQL        : String;
begin
  Result := 0;
  try
    LSQL := 'SELECT * FROM Network WHERE NetworkCode = ' + QuotedStr(ANetworkCode);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      if (NOT LQuery.Eof) then
        Result := LQuery.FieldByName('NetworkID').AsInteger;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroDBManager.GetNetworkCode(ANetworkID : Integer): String;
const OPNAME = 'THydroDBManager.GetNetworkCode';
var
  LQuery      : TDataSet;
  LSQL        : String;
begin
  Result := '';
  try
    LSQL := 'SELECT * FROM Network WHERE  NetworkID = ' + IntToStr(ANetworkID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      if (NOT LQuery.Eof) then
        Result := Trim(LQuery.FieldByName('NetworkCode').AsString);
      LQuery.Close;
    finally
      LQuery.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroDBManager.GetNetworkModuleIDs (ANetworkID : Integer;
                                               AModuleIDs : TStringList);
const OPNAME = 'THydroDBManager.GetNetworkModuleIDs';
var
  LQuery      : TDataSet;
  LSQL        : String;
  LModuleID   : Integer;
begin
  try
    AModuleIDs.Clear;
    LSQL := 'SELECT * FROM NetworkModules WHERE NetworkID = ' + IntToStr(ANetworkID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        LModuleID     := LQuery.FieldByName('ModuleID').AsInteger;
        AModuleIDs.Add(IntToStr(LModuleID));
        LQuery.Next;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroDBManager.ExportNetwork (ANetworkID    : Integer;
                                        ADirectory    : WideString;
                                        var AErrorMsg : WideString): WordBool;
const OPNAME = 'THydroDBManager.ExportNetwork';
var
  LNetworkCode      : String;
  LModuleIDList     : TStringList;
  LModuleTableList  : TStringList;
  LNetworkTableList : TStringList;
  LIndex            : Integer;
  LTableName        : String;
  LDiagramDir       : String;
  LQuery            : TDataSet;
  LSQL              : String;
begin
  Result := FALSE;
  try
    if (NOT DoesNetworkExist(ANetworkID)) then
    begin
      AErrorMsg := 'Network ' + IntToStr(ANetworkID) + ' does not exist.';
    end
    else
    begin
      Result            := TRUE;
      LNetworkCode      := GetNetworkCode(ANetworkID);
      ADirectory        := IncludeTrailingPathDelimiter(ADirectory);
      LDiagramDir       := GetNetworkDiagramsDirectory(LNetworkCode);
      LModuleIDList     := TStringList.Create;
      LModuleTableList  := TStringList.Create;
      LNetworkTableList := TStringList.Create;
      try
        GetNetworkModuleIDs(ANetworkID, LModuleIDList);
        PopulateNetworkTablesList(LNetworkTableList);
        PopulateModuleTablesList(LModuleTableList);

        LIndex := 0;
        while (Result AND (LIndex < LNetworkTableList.Count)) do
        begin
          LTableName := LNetworkTableList.Strings[LIndex];
          LSQL       := 'SELECT * FROM ' + LTableName + ' WHERE NetworkID = ' + IntToStr(ANetworkID);
          LQuery     := GHydroDBAgent.CreateQuery(LSQL);
          try
            Result := ExportTableXML(LTableName, LQuery, ADirectory);
            if (NOT Result) then
              AErrorMsg := 'Could not export ' + LTableName + ' table ' + ' for ' + LNetworkCode;
          finally
            LQuery.Free;
          end;
          LIndex := LIndex + 1;
        end;

        if (LModuleIDList.Count > 0) then
        begin
          LIndex := 0;
          while (Result AND (LIndex < LModuleTableList.Count)) do
          begin
            LTableName := LModuleTableList.Strings[LIndex];
            LSQL       := 'SELECT * FROM ' + LTableName + ' WHERE ModuleID IN (' + LModuleIDList.CommaText + ')';
            LQuery     := GHydroDBAgent.CreateQuery(LSQL);
            try
              Result := ExportTableXML(LTableName, LQuery, ADirectory);
              if (NOT Result) then
                AErrorMsg := 'Could not export ' + LTableName + ' table ' + ' for ' + LNetworkCode;
            finally
              LQuery.Free;
            end;
            LIndex := LIndex + 1;
          end;
        end;

        if (Result) then
          ExportHydroNVDiagrams(ADirectory, ANetworkID, LDiagramDir);

      finally
        FreeAndNil(LModuleIDList);
        FreeAndNil(LModuleTableList);
        FreeAndNil(LNetworkTableList);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroDBManager.ExportTableXML (ATableName  : String;
                                         AQuery      : TDataSet;
                                         AXMLFileDir : String) : Boolean;
const OPNAME = 'THydroDBManager.ExportTableXML';
var
  LIndex         : Integer;
  LXML           : TStringList;
  LFieldDef      : TFieldDef;
  LTemp          : String;
  LFileName      : String;
  LBlobStream    : TStream;
  LBufferSize    : Integer;
  LBytesRead     : integer;
  LBuffer        : PChar;
  LBufStr        : PChar;
begin
  Result := TRUE;
  try
    AQuery.Open;
    if ((AQuery.Eof) AND (AQuery.Bof)) then
      Result := TRUE
    else
    begin
      LXML := TStringList.Create;
      try
        LXML.Add('<?xml version="1.0" standalone="yes" ?> ');
        LXML.Add('<DATAPACKET Version="2.0">');
        LXML.Add('<METADATA>');
        LXML.Add('<FIELDS>');

        for LIndex := 0 to AQuery.FieldDefs.Count - 1 do
        begin
          LFieldDef := AQuery.FieldDefs.Items[LIndex];
          LTemp     := '<FIELD attrname="' + LFieldDef.Name + '" fieldtype=';
          case (LFieldDef.DataType) of
            ftWideString,
            ftString   : LTemp := LTemp + '"string.uni" WIDTH="' + IntToStr(LFieldDef.Size * 2) + '" />';
            ftInteger  : LTemp := LTemp + '"i4" />';
            ftWord,
            ftSmallint : LTemp := LTemp + '"i2" />';
            ftFloat    : LTemp := LTemp + '"r8" />';
            ftBlob     : LTemp := LTemp + '"bin.hex" SUBTYPE="Binary" />';
          else
            ShowMessage('Unknown data type ');
          end;
          LXML.Add(LTemp);
        end;

        LXML.Add('</FIELDS>');
        LXML.Add('<PARAMS />');
        LXML.Add('</METADATA>');
        LXML.Add('<ROWDATA>');

        while (NOT AQuery.Eof) do
        begin
          LTemp := '<ROW ';
          for LIndex := 0 to AQuery.FieldDefs.Count - 1 do
          begin
            LFieldDef := AQuery.FieldDefs.Items[LIndex];
            if (LFieldDef.DataType = ftBlob) then
            begin
              LBlobStream := AQuery.CreateBlobStream(AQuery.FieldByName(LFieldDef.Name), bmRead);
              try
                // Determine how much data is in the file.
                LBlobStream.Seek(0, soFromBeginning);
                LBufferSize := LBlobStream.Size;
                LBlobStream.Seek(0, soFromBeginning);
                // Create a buffer large enough to contain all the data.
                GetMem(LBuffer, LBufferSize);
                GetMem(LBufStr, 3*LBufferSize);
                try
                  ZeroMemory(LBuffer, LBufferSize);
                  ZeroMemory(LBufStr, 3*LBufferSize);
                  LBytesRead := LBlobStream.Read(LBuffer[0], LBufferSize);
                  Result := Result AND (LBytesRead = LBufferSize);
                  BinToHex(LBuffer, LBufStr, LBufferSize);
                  LTemp   := LTemp + LFieldDef.Name + '="' + LBufStr + '" ';
                finally
                  FreeMem(LBuffer);
                end;
              finally
                LBlobStream.Free
              end;
            end
            else
              LTemp := LTemp + LFieldDef.Name + '="' + Trim(AQuery.FieldByName(LFieldDef.Name).AsString) + '" ';
          end;
          LTemp := LTemp + '/>';
          LXML.Add(LTemp);

          AQuery.Next;
        end;

        LXML.Add('</ROWDATA>');
        LXML.Add('</DATAPACKET>');
        LFileName := Trim(AXMLFileDir) + 'LD_' + Trim(ATableName) + '.xml';
        LXML.SaveToFile(LFileName);
      finally
        LXML.Free;
      end;
    end;
    AQuery.Close;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroDBManager.ExportHydroNVDiagrams (AXMLFileDir : String;
                                                ANetworkID  : Integer;
                                                ADiagramDir : String) : Boolean;
const OPNAME = 'THydroDBManager.ExportHydroNVDiagrams';
var
  LSQL          : String;
  LQuery        : TDataSet;
  LFileName     : String;
  LExpFileName  : String;
begin
  Result := FALSE;
  try
    LSQL := 'SELECT * FROM HydroNVDrawings WHERE NetworkID = ' + IntToStr(ANetworkID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        LFileName    := Trim(LQuery.FieldByName('DrawingName').AsString);
        LExpFileName := AXMLFileDir + LFileName + '.VSD';
        LFileName    := ADiagramDir + LFileName + '.VSD';

        if (FileExists(LFileName)) then
          CopyFile(PChar(LFileName), PChar(LExpFileName), TRUE);
        LQuery.Next;
      end;
      Result := TRUE;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroDBManager.ImportNetworkTableXML (ADirectory    : String;
                                                ATableName    : String;
                                                ANetworkID    : Integer) : Boolean;
const OPNAME = 'THydroDBManager.ImportNetworkTableXML';
var
  LInsSQL           : String;
  LValSQL           : String;
  LIndex            : Integer;
  LFieldName        : String;
  LFileName         : String;
  LFieldNames       : TStringList;
  LFieldTypes       : TStringList;
  LBlobFieldNames   : TStringList;
  LXMLDocument      : TXMLDocument;
  LRootNode         : IXMLNode;
  LAllFieldsNode    : IXMLNode;
  LAllDataNode      : IXMLNode;
  LNode             : IXMLNode;
  LFieldType        : String;
  LFieldVal         : String;
  LCount            : Integer;
  LBufferSize       : Integer;
  LBuffer           : PChar;
begin
  Result := FALSE;
  try
    LFileName := ADirectory + 'LD_' + ATableName + '.xml';
    if (FileExists(LFileName)) then
    begin
      LXMLDocument := TXMLDocument.Create(Application);
      try
        LXMLDocument.DOMVendor    := GetDOMVendor('MSXML');
        LXMLDocument.ParseOptions := [];
        LXMLDocument.Active := FALSE;
        LXMLDocument.XML.Clear;
        LXMLDocument.LoadFromFile(LFileName);
        LXMLDocument.Active       := TRUE;
        LRootNode                 := LXMLDocument.DocumentElement;
        LAllFieldsNode            := LRootNode.ChildNodes['METADATA'];
        LAllFieldsNode            := LAllFieldsNode.ChildNodes['FIELDS'];
        LAllDataNode              := LRootNode.ChildNodes['ROWDATA'];

        LFieldNames     := TStringList.Create;
        LFieldTypes     := TStringList.Create;
        LBlobFieldNames := TStringList.Create;
        try
          for LIndex := 0 to LAllFieldsNode.ChildNodes.Count - 1 do
          begin
            LNode      := LAllFieldsNode.ChildNodes.Get(LIndex);
            LFieldName := LNode.Attributes['attrname'];
            LFieldType := LNode.Attributes['fieldtype'];
            if (LFieldType = 'bin.hex') then
              LBlobFieldNames.Add(LFieldName)
            else
            begin
              LFieldNames.Add(LFieldName);
              LFieldTypes.Add(LFieldType);
            end;
          end;
          LIndex := 0;
          Result := TRUE;
          while (Result AND (LIndex < LAllDataNode.ChildNodes.Count)) do
          begin
            LInsSQL      := '';
            LValSQL      := '';
            LNode        := LAllDataNode.ChildNodes.Get(LIndex);
            for LCount := 0 to LFieldNames.Count - 1 do
            begin
              LFieldName := LFieldNames.Strings[LCount];
              LFieldType := LFieldTypes.Strings[LCount];
              LFieldVal  := LNode.Attributes[LFieldName];
              if (LFieldName = 'NetworkID') then
              begin
                LInsSQL   := LInsSQL + ', ' + LFieldName;
                LValSQL   := LValSQL + ', ' + IntToStr(ANetworkID);
              end
              else
              begin
                if ((LFieldName = 'Year') OR (LFieldName = 'Month') OR (LFieldName = 'Index')) then
                  LInsSQL := LInsSQL + ', [' + LFieldName + ']'
                else
                  LInsSQL := LInsSQL + ', ' + LFieldName;
                if (LFieldType = 'string.uni') then
                  LValSQL := LValSQL + ', ' + QuotedStr(LFieldVal)
                else if (LFieldType = 'r8') then
                  LValSQL := LValSQL + ', ' + LFieldVal
                else
                  LValSQL := LValSQL + ', ' + LFieldVal
              end;
            end;
            
            if (LInsSQL <> '') then
            begin
              LInsSQL := Copy(LInsSQL, 2, Length(LInsSQL) - 1);
              LValSQL := Copy(LValSQL, 2, Length(LValSQL) - 1);
              LInsSQL := 'INSERT INTO ' + ATableName + ' (' + LInsSQL + ') VALUES (' + LValSQL + ')';
              Result := GHydroDBAgent.ExecuteSQL(LInsSQL, True);

              LCount := 0;
              while (Result AND (LCount < LBlobFieldNames.Count)) do
              begin
                LFieldName  := LBlobFieldNames.Strings[LCount];
                LFieldVal   := LNode.Attributes[LFieldName];
                LBufferSize := Length(LFieldVal);
                LBufferSize := Round(LBufferSize / 2);
                GetMem(LBuffer, LBufferSize);
                try
                  ZeroMemory(LBuffer, LBufferSize);
                  HexToBin(PChar(LFieldVal), LBuffer, LBufferSize);
                  Result := GHydroDBAgent.WriteBufferToBlobField(ATableName, 'NetworkID', LFieldName,
                                                                  ANetworkID, LBufferSize, LBuffer);
                finally
                  FreeMem(LBuffer);
                end;
                LCount := LCount + 1;
              end;
            end
            else
              Result := FALSE;

            LIndex := LIndex + 1;
          end;
        finally
          LFieldNames.Free;
          LFieldTypes.Free;
          LBlobFieldNames.Free;
        end;
      finally
        LXMLDocument.Free;
      end;
    end
    else
      Result := TRUE;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroDBManager.ImportModuleTableXML (ADirectory    : String;
                                               ATableName    : String;
                                               AMustExist    : Boolean;
                                               AOldModuleIDs : TStringList;
                                               ANewModuleIDs : TStringList) : Boolean;
const OPNAME = 'THydroDBManager.ImportModuleTableXML';
var
  LInsSQL           : String;
  LValSQL           : String;
  LIndex            : Integer;
  LCount            : Integer;
  LFieldNames       : TStringList;
  LFieldTypes       : TStringList;
  LFieldName        : String;
  LFileName         : String;
  LNewModuleID      : Integer;
  LOldModuleID      : Integer;
  LIDIndex          : Integer;
  LResult           : Boolean;
  LXMLDocument      : TXMLDocument;
  LRootNode         : IXMLNode;
  LAllFieldsNode    : IXMLNode;
  LAllDataNode      : IXMLNode;
  LNode             : IXMLNode;
  LFieldType        : String;
  LFieldVal         : String;
begin
  Result := FALSE;
  try
    LFileName := ADirectory + 'LD_' + ATableName + '.xml';
    if (NOT (FileExists(LFileName))) then
      Result := (NOT AMustExist)
    else
    begin
      LXMLDocument := TXMLDocument.Create(Application);
      try
        LResult := TRUE;

        LXMLDocument.DOMVendor    := GetDOMVendor('MSXML');
        LXMLDocument.ParseOptions := [];
        LXMLDocument.Active := FALSE;
        LXMLDocument.XML.Clear;
        LXMLDocument.LoadFromFile(LFileName);
        LXMLDocument.Active       := TRUE;
        LRootNode                 := LXMLDocument.DocumentElement;
        LAllFieldsNode            := LRootNode.ChildNodes['METADATA'];
        LAllFieldsNode            := LAllFieldsNode.ChildNodes['FIELDS'];
        LAllDataNode              := LRootNode.ChildNodes['ROWDATA'];

        LFieldNames := TStringList.Create;
        LFieldTypes := TStringList.Create;
        try
          for LIndex := 0 to LAllFieldsNode.ChildNodes.Count - 1 do
          begin
            LNode      := LAllFieldsNode.ChildNodes.Get(LIndex);
            LFieldName := LNode.Attributes['attrname'];
            LFieldType := LNode.Attributes['fieldtype'];
            LFieldNames.Add(LFieldName);
            LFieldTypes.Add(LFieldType);
          end;
          LIndex := 0;
          while (LResult AND (LIndex < LAllDataNode.ChildNodes.Count)) do
          begin
            LNewModuleID := 0;
            LInsSQL      := '';
            LValSQL      := '';
            LNode        := LAllDataNode.ChildNodes.Get(LIndex);
            for LCount := 0 to LFieldNames.Count - 1 do
            begin
              LFieldName := LFieldNames.Strings[LCount];
              LFieldType := LFieldTypes.Strings[LCount];
              LFieldVal  := LNode.Attributes[LFieldName];
              if (LFieldName = 'ModuleID') then
              begin
                LOldModuleID := LNode.Attributes[LFieldName];
                LIDIndex     := AOldModuleIDs.IndexOf(IntToStr(LOldModuleID));
                if (LIDIndex >= 0) then
                  LNewModuleID := StrToInt(ANewModuleIDs.Strings[LIDIndex]);
                LInsSQL      := LInsSQL + ', ' + LFieldName;
                LValSQL      := LValSQL + ', ' + IntToStr(LNewModuleID);
              end
              else
              begin
                if ((LFieldName = 'Year') OR (LFieldName = 'Month') OR (LFieldName = 'Index')) then
                  LInsSQL    := LInsSQL + ', [' + LFieldName + ']'
                else
                  LInsSQL    := LInsSQL + ', ' + LFieldName;
                if (LFieldType = 'string.uni') then
                  LValSQL := LValSQL + ', ' + QuotedStr(LFieldVal)
                else if (LFieldType = 'r8') then
                  LValSQL := LValSQL + ', ' + LFieldVal
                else
                  LValSQL := LValSQL + ', ' + LFieldVal
              end;
            end;

            if (LNewModuleID <> 0) then
            begin
              LInsSQL := Copy(LInsSQL, 2, Length(LInsSQL) - 1);
              LValSQL := Copy(LValSQL, 2, Length(LValSQL) - 1);
              LInsSQL := 'INSERT INTO ' + ATableName + ' (' + LInsSQL + ') VALUES (' + LValSQL + ')';
              LResult := GHydroDBAgent.ExecuteSQL(LInsSQL, True);
            end
            else
              LResult := FALSE;
            LIndex := LIndex + 1;
          end;
        finally
          LFieldNames.Free;
          LFieldTypes.Free;
        end;
      finally
        LXMLDocument.Free;
      end;
      Result := LResult;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroDBManager.ImportNetworkModulesXML (ADirectory    : String;
                                                  ANetworkID    : Integer;
                                                  AOldModuleIDs : TStringList;
                                                  ANewModuleIDs : TStringList) : Boolean;
const OPNAME = 'THydroDBManager.ImportNetworkModulesXML';
var
  LInsSQL           : String;
  LValSQL           : String;
  LIndex            : Integer;
  LFieldName        : String;
  LFileName         : String;
  LNewModuleID      : Integer;
  LOldModuleID      : Integer;
  LResult           : Boolean;
  LFieldNames       : TStringList;
  LFieldTypes       : TStringList;
  LXMLDocument      : TXMLDocument;
  LRootNode         : IXMLNode;
  LAllFieldsNode    : IXMLNode;
  LAllDataNode      : IXMLNode;
  LNode             : IXMLNode;
  LFieldType        : String;
  LFieldVal         : String;
  LCount            : Integer;
begin
  Result := FALSE;
  try
    LFileName := ADirectory + 'LD_NetworkModules.xml';

    LXMLDocument := TXMLDocument.Create(Application);
    try
      LResult := TRUE;

      LXMLDocument.DOMVendor    := GetDOMVendor('MSXML');
      LXMLDocument.ParseOptions := [];
      LXMLDocument.Active := FALSE;
      LXMLDocument.XML.Clear;
      LXMLDocument.LoadFromFile(LFileName);
      LXMLDocument.Active := TRUE;
      LRootNode           := LXMLDocument.DocumentElement;
      LAllFieldsNode      := LRootNode.ChildNodes['METADATA'];
      LAllFieldsNode      := LAllFieldsNode.ChildNodes['FIELDS'];
      LAllDataNode        := LRootNode.ChildNodes['ROWDATA'];

      LFieldNames := TStringList.Create;
      LFieldTypes := TStringList.Create;
      try
        for LIndex := 0 to LAllFieldsNode.ChildNodes.Count - 1 do
        begin
          LNode      := LAllFieldsNode.ChildNodes.Get(LIndex);
          LFieldName := LNode.Attributes['attrname'];
          LFieldType := LNode.Attributes['fieldtype'];
          LFieldNames.Add(LFieldName);
          LFieldTypes.Add(LFieldType);
        end;
        LIndex := 0;
        while (LResult AND (LIndex < LAllDataNode.ChildNodes.Count)) do
        begin
          LNewModuleID := GHydroDBAgent.GetNextID('NetworkModules', 'ModuleID');
          LInsSQL      := 'INSERT INTO NetworkModules (NetworkID';
          LValSQL      := '(' + IntToStr(ANetworkID);
          LNode        := LAllDataNode.ChildNodes.Get(LIndex);
          for LCount := 0 to LFieldNames.Count - 1 do
          begin
            LFieldName := LFieldNames.Strings[LCount];
            LFieldType := LFieldTypes.Strings[LCount];
            LFieldVal  := LNode.Attributes[LFieldName];
            if (LFieldName <> 'NetworkID') then
            begin
              if (LFieldName = 'ModuleID') then
              begin
                LOldModuleID := StrToInt(LFieldVal);
                LInsSQL      := LInsSQL + ', ' + LFieldName;
                LValSQL      := LValSQL + ', ' + IntToStr(LNewModuleID);
                AOldModuleIDs.Add(IntToStr(LOldModuleID));
                ANewModuleIDs.Add(IntToStr(LNewModuleID));
              end
              else
              begin
                if ((LFieldName = 'Year') OR (LFieldName = 'Month') OR (LFieldName = 'Index')) then
                  LInsSQL    := LInsSQL + ', [' + LFieldName + ']'
                else
                  LInsSQL    := LInsSQL + ', ' + LFieldName;
                if (LFieldType = 'string.uni') then
                  LValSQL := LValSQL + ', ' + QuotedStr(LFieldVal)
                else if (LFieldType = 'r8') then
                  LValSQL := LValSQL + ', ' + LFieldVal
                else
                  LValSQL := LValSQL + ', ' + LFieldVal
              end;
            end;
          end;
          LInsSQL := LInsSQL + ') VALUES ' + LValSQL + ')';
          LResult := GHydroDBAgent.ExecuteSQL(LInsSQL, True);
          LIndex := LIndex + 1;
        end;
      finally
        LFieldNames.Free;
        LFieldTypes.Free;
      end;
      Result := LResult;
    finally
      LXMLDocument.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroDBManager.ImportNetworkRoutesXML (ADirectory    : String;
                                                 ANetworkID    : Integer;
                                                 AOldModuleIDs : TStringList;
                                                 ANewModuleIDs : TStringList;
                                                 AOldRouteIDs  : TStringList;
                                                 ANewRouteIDs  : TStringList) : Boolean;
const OPNAME = 'THydroDBManager.ImportNetworkRoutesXML';
var
  LInsSQL           : String;
  LFileName         : String;
  LNewRouteID       : Integer;
  LOldRouteID       : String;
  LOldSourceID      : String;
  LNewSourceID      : String;
  LOldSinkID        : String;
  LNewSinkID        : String;
  LIDIndex          : Integer;
  LResult           : Boolean;
  LFieldNames       : TStringList;
  LFieldTypes       : TStringList;
  LXMLDocument      : TXMLDocument;
  LRootNode         : IXMLNode;
  LAllFieldsNode    : IXMLNode;
  LAllDataNode      : IXMLNode;
  LNode             : IXMLNode;
  LFieldName        : String;
  LFieldType        : String;
  LIndex            : Integer;
begin
  Result := FALSE;
  try
    LResult := FALSE;
    LFileName := ADirectory + 'LD_NetworkRoutes.xml';
    if (FileExists(LFileName)) then
    begin
      LXMLDocument := TXMLDocument.Create(Application);
      try
        LResult := TRUE;

        LXMLDocument.DOMVendor    := GetDOMVendor('MSXML');
        LXMLDocument.ParseOptions := [];
        LXMLDocument.Active := FALSE;
        LXMLDocument.XML.Clear;
        LXMLDocument.LoadFromFile(LFileName);
        LXMLDocument.Active       := TRUE;
        LRootNode                 := LXMLDocument.DocumentElement;
        LAllFieldsNode            := LRootNode.ChildNodes['METADATA'];
        LAllFieldsNode            := LAllFieldsNode.ChildNodes['FIELDS'];
        LAllDataNode              := LRootNode.ChildNodes['ROWDATA'];

        LFieldNames := TStringList.Create;
        LFieldTypes := TStringList.Create;
        try
          for LIndex := 0 to LAllFieldsNode.ChildNodes.Count - 1 do
          begin
            LNode      := LAllFieldsNode.ChildNodes.Get(LIndex);
            LFieldName := LNode.Attributes['attrname'];
            LFieldType := LNode.Attributes['fieldtype'];
            LFieldNames.Add(LFieldName);
            LFieldTypes.Add(LFieldType);
          end;
          LIndex := 0;
          while (LResult AND (LIndex < LAllDataNode.ChildNodes.Count)) do
          begin
            LNode         := LAllDataNode.ChildNodes.Get(LIndex);
            LNewSourceID  := '0';
            LNewSinkID    := '0';
            LOldRouteID   := LNode.Attributes['RouteID'];
            LNewRouteID   := GHydroDBAgent.GetNextID('NetworkRoutes', 'RouteID');
            AOldRouteIDs.Add(LOldRouteID);
            ANewRouteIDs.Add(IntToStr(LNewRouteID));
            LOldSourceID  := LNode.Attributes['SourceModuleID'];
            LIDIndex      := AOldModuleIDs.IndexOf(LOldSourceID);
            if (LIDIndex >= 0) then
              LNewSourceID := ANewModuleIDs.Strings[LIDIndex];
            LOldSinkID    := LNode.Attributes['SinkModuleID'];
            LIDIndex      := AOldModuleIDs.IndexOf(LOldSinkID);
            if (LIDIndex >= 0) then
              LNewSinkID := ANewModuleIDs.Strings[LIDIndex];

            LInsSQL := 'INSERT INTO NetworkRoutes (RouteID, NetworkID, RouteNo, ' +
                       'SourceModuleID, SinkModuleID, RouteCost) VALUES (' +
                       IntToStr(LNewRouteID)           + ', ' +
                       IntToStr(ANetworkID)            + ', ' +
                       LNode.Attributes['RouteNo']     + ', ' +
                       LNewSourceID                    + ', ' +
                       LNewSinkID                      + ', ' +
                       LNode.Attributes['RouteCost']   + ')';

            LResult := GHydroDBAgent.ExecuteSQL(LInsSQL, True);

            LIndex := LIndex + 1;
          end;
        finally
          LFieldNames.Free;
          LFieldTypes.Free;
        end;
      finally
        LXMLDocument.Free;
      end;
    end;
    Result := LResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroDBManager.ImportNetwork (ADirectory    : WideString;
                                        ANetworkID    : Integer;
                                        var AErrorMsg : WideString): WordBool;
const OPNAME = 'THydroDBManager.ImportNetwork';
var
  LNetworkID            : Integer;
  LResult               : Boolean;
  LOldModuleIDs         : TStringList;
  LNewModuleIDs         : TStringList;
  LOldRouteIDs          : TStringList;
  LNewRouteIDs          : TStringList;
  LModuleTableList      : TStringList;
  LNetworkTableList     : TStringList;
  LIndex                : Integer;
  LTableName            : String;
  LFileName             : String;
  LDiagramDir           : String;
  LNetworkCode          : WideString;
begin
  Result := FALSE;
  try
    LResult := TRUE;
    if (DoesNetworkExist(ANetworkID)) then
    begin
      Result := FALSE;
      AErrorMsg := 'Network ' + LNetworkCode + ' already exist.';
      Exit;
    end;
    
    LNetworkID := GHydroDBAgent.GetNextID('Network', 'NetworkID');

    GHydroDBAgent.StartTransaction;
    try
      LFileName := ADirectory + 'LD_Network.xml';
      if (NOT CheckNetworkCode(LFileName, LNetworkCode, AErrorMsg)) then
        LResult := FALSE
      else
        LResult := ImportNetworkTableXML(ADirectory, 'Network', LNetworkID);

      if (LResult) then
      begin
        LDiagramDir       := GetNetworkDiagramsDirectory(LNetworkCode);
        LOldModuleIDs     := TStringList.Create;
        LNewModuleIDs     := TStringList.Create;
        LOldRouteIDs      := TStringList.Create;
        LNewRouteIDs      := TStringList.Create;
        LModuleTableList  := TStringList.Create;
        LNetworkTableList := TStringList.Create;
        try
          PopulateModuleTablesList(LModuleTableList);
          PopulateNetworkTablesList(LNetworkTableList);

          LFileName := ADirectory + 'LD_NetworkModules.xml';
          if (NOT (FileExists(LFileName))) then
          begin
            LResult := FALSE;
            AErrorMsg := 'Could not find file ' + LFileName;
          end
          else
          begin
            LResult   := ImportNetworkModulesXML(ADirectory, LNetworkID, LOldModuleIDs, LNewModuleIDs);
            if (NOT LResult) then
              AErrorMsg := 'Could not import ' + LNetworkCode + ' into NetworkModules table.';
          end;

          LIndex := 0;
          while (LResult AND (LIndex < LModuleTableList.Count)) do
          begin
            LTableName := LModuleTableList.Strings[LIndex];
            if (LTableName = 'Modules') then
              LResult := ImportModuleTableXML(ADirectory, LTableName, TRUE, LOldModuleIDs, LNewModuleIDs)
            else
              LResult := ImportModuleTableXML(ADirectory, LTableName, FALSE, LOldModuleIDs, LNewModuleIDs);
            if (NOT LResult) then
              AErrorMsg := 'Could not import ' + LNetworkCode + ' into ' + LTableName + ' table.';
            LIndex := LIndex + 1;
          end;

          if (LResult) then
          begin
            LFileName := ADirectory + 'LD_NetworkRoutes.xml';
            if (NOT (FileExists(LFileName))) then
            begin
              LResult := FALSE;
              AErrorMsg := 'Could not find file ' + LFileName;
            end
            else
            begin
              LResult := ImportNetworkRoutesXML(ADirectory, LNetworkID, LOldModuleIDs, LNewModuleIDs, LOldRouteIDs, LNewRouteIDs);
              if (NOT LResult) then
                AErrorMsg := 'Could not import ' + LNetworkCode + ' into NetworkRoutes table.';
            end;
          end;

          LIndex := 0;
          while (LResult AND (LIndex < LNetworkTableList.Count)) do
          begin
            LTableName := LNetworkTableList.Strings[LIndex];
            if (LTableName = 'SimulationResults') then
              LResult := ImportSimulationResultsXML(ADirectory, LNetworkID, LOldModuleIDs, LNewModuleIDs, LOldRouteIDs, LNewRouteIDs)
            else if (LTableName = 'HydroNVDrawings') then
              LResult := ImportHydroNVDiagramsXML(ADirectory, LNetworkID, LDiagramDir)
            else if ((LTableName <> 'NetworkModules') AND (LTableName <> 'NetworkRoutes') AND (LTableName <> 'Network')) then
              LResult := ImportNetworkTableXML(ADirectory, LTableName, LNetworkID);
            if (NOT LResult) then
              AErrorMsg := 'Could not import ' + LNetworkCode + ' into ' + LTableName + ' table.';
            LIndex := LIndex + 1;
          end;

        finally
          LOldModuleIDs.Free;
          LNewModuleIDs.Free;
          LOldRouteIDs.Free;
          LNewRouteIDs.Free;
          LModuleTableList.Free;
          LNetworkTableList.Free;
        end;
        if (LResult) then
        begin
          GHydroDBAgent.CommitTransaction;
          Result := LResult;
        end
        else
          GHydroDBAgent.RollbackTransaction;
      end;
    except
      GHydroDBAgent.RollbackTransaction;
    end;
  Result := LResult;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroDBManager.ImportSimulationResultsXML (ADirectory    : String;
                                                     ANetworkID    : Integer;
                                                     AOldModuleIDs : TStringList;
                                                     ANewModuleIDs : TStringList;
                                                     AOldRouteIDs  : TStringList;
                                                     ANewRouteIDs  : TStringList) : Boolean;
const OPNAME = 'THydroDBManager.ImportSimulationResultsXML';
var
  LSQLInsert        : String;
  LIndex            : Integer;
  LFileName         : String;
  LResult           : Boolean;
  LXMLDocument      : TXMLDocument;
  LRootNode         : IXMLNode;
  LAllDataNode      : IXMLNode;
  LNode             : IXMLNode;
  LNewResultID      : Integer;
  LOldElementID     : Integer;
  LNewElementID     : Integer;
  LElementType      : String;
  LIDIndex          : Integer;
  LResultData       : String;
  LBufferSize       : Integer;
  LBuffer           : PChar;
begin
  Result := FALSE;
  try
    LFileName := ADirectory + 'LD_SimulationResults.xml';
    if (FileExists(LFileName)) then
    begin
      LXMLDocument := TXMLDocument.Create(Application);
      try
        LResult := TRUE;

        LXMLDocument.DOMVendor    := GetDOMVendor('MSXML');
        LXMLDocument.ParseOptions := [];
        LXMLDocument.Active       := FALSE;
        LXMLDocument.XML.Clear;
        LXMLDocument.LoadFromFile(LFileName);
        LXMLDocument.Active       := TRUE;
        LRootNode                 := LXMLDocument.DocumentElement;
        LAllDataNode              := LRootNode.ChildNodes['ROWDATA'];

        LIndex := 0;
        while (LResult AND (LIndex < LAllDataNode.ChildNodes.Count)) do
        begin
          LNode        := LAllDataNode.ChildNodes.Get(LIndex);

          LNewElementID  := 0;
          LOldElementID  := StrToInt(LNode.Attributes['ElementID']);
          LElementType   := LNode.Attributes['ElementType'];
          if (LElementType = 'RQ') then
          begin
            LIDIndex := AOldRouteIDs.IndexOf(IntToStr(LOldElementID));
            if (LIDIndex >= 0) then
              LNewElementID := StrToInt(ANewRouteIDs.Strings[LIDIndex]);
          end
          else
          begin
            LIDIndex := AOldModuleIDs.IndexOf(IntToStr(LOldElementID));
            if (LIDIndex >= 0) then
              LNewElementID := StrToInt(ANewModuleIDs.Strings[LIDIndex]);
          end;

          LNewResultID   := GHydroDBAgent.GetNextID('SimulationResults', 'ResultID');

          LSQLInsert := 'INSERT INTO SimulationResults (ResultID, NetworkID, ElementType, ' +
                        'ElementSubType, ElementID, SubElementID, ResultTypeID, AllZero) VALUES (' +
                         IntToStr(LNewResultID)                        + ', ' +
                         IntToStr(ANetworkID)                          + ', ' +
                         QuotedStr(LElementType)                       + ', ' +
                         QuotedStr(LNode.Attributes['ElementSubType']) + ', ' +
                         IntToStr(LNewElementID)                       + ', ' +
                         LNode.Attributes['SubElementID']              + ', ' +
                         LNode.Attributes['ResultTypeID']              + ', ' +
                         LNode.Attributes['AllZero']                   + ')';

          LResult := GHydroDBAgent.ExecuteSQL(LSQLInsert, True);

          if (LResult) then
          begin
            LResultData := LNode.Attributes['ResultData'];
            LBufferSize := Length(LResultData);
            LBufferSize := Round(LBufferSize / 2);
            GetMem(LBuffer, LBufferSize);
            try
              ZeroMemory(LBuffer, LBufferSize);
              HexToBin(PChar(LResultData), LBuffer, LBufferSize);

              LResult := GHydroDBAgent.WriteBufferToBlobField('SimulationResults', 'ResultID', 'ResultData',
                                                              LNewResultID, LBufferSize, LBuffer);
            finally
              FreeMem(LBuffer);
            end;
          end;
          LIndex := LIndex + 1;
        end;
      finally
        LXMLDocument.Free;
      end;
      Result := LResult;
    end
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroDBManager.ImportHydroNVDiagramsXML (AXMLFileDir : String;
                                                   ANetworkID  : Integer;
                                                   ADiagramDir : String) : Boolean;
const OPNAME = 'THydroDBManager.ImportHydroNVDiagramsXML';
var
  LInsSQL           : String;
  LIndex            : Integer;
  LFileName         : String;
  LResult           : Boolean;
  LXMLDocument      : TXMLDocument;
  LRootNode         : IXMLNode;
  LAllDataNode      : IXMLNode;
  LNode             : IXMLNode;
  LImpFileName      : String;
begin
  Result := FALSE;
  try
    LFileName := AXMLFileDir + 'LD_HydroNVDrawings.xml';
    if (FileExists(LFileName)) then
    begin
      LXMLDocument := TXMLDocument.Create(Application);
      try
        LResult := TRUE;

        LXMLDocument.DOMVendor    := GetDOMVendor('MSXML');
        LXMLDocument.ParseOptions := [];
        LXMLDocument.Active := FALSE;
        LXMLDocument.XML.Clear;
        LXMLDocument.LoadFromFile(LFileName);
        LXMLDocument.Active := TRUE;
        LRootNode           := LXMLDocument.DocumentElement;
        LAllDataNode        := LRootNode.ChildNodes['ROWDATA'];

        LIndex := 0;
        while (LResult AND (LIndex < LAllDataNode.ChildNodes.Count)) do
        begin
          LInsSQL       := '';
          LNode         := LAllDataNode.ChildNodes.Get(LIndex);
          LFileName     := LNode.Attributes['DrawingName'];

          LInsSQL := 'INSERT INTO HydroNVDrawings (NetworkID, DrawingName, GISDrawing, ReadOnly) VALUES ('  +
                     IntToStr(ANetworkID)           + ',' +
                     QuotedStr(LFileName)           + ', ' +
                     LNode.Attributes['GISDrawing'] + ', ' +
                     LNode.Attributes['ReadOnly']   + ')';

          LResult := GHydroDBAgent.ExecuteSQL(LInsSQL, True);

          if (LResult) then
          begin
            LImpFileName := AXMLFileDir + LFileName + '.VSD';
            LFileName    := ADiagramDir + LFileName + '.VSD';
            if (FileExists(LImpFileName)) then
            begin
              if (NOT DirectoryExists(ADiagramDir)) then
                CreateDir(ADiagramDir);
              if (FileExists(LFileName)) then
                SysUtils.DeleteFile(LFileName);
              CopyFile(PChar(LImpFileName), PChar(LFileName), TRUE);
            end;
          end;
          LIndex := LIndex + 1;
        end;
      finally
        LXMLDocument.Free;
      end;
      Result := LResult;
    end
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroDBManager.CopyNetworkDB (AOldNetworkID   : Integer;
                                        ANewNetworkID   : Integer;
                                        AOldNetworkCode : String;
                                        ANewNetworkCode : String): Boolean;
const OPNAME = 'THydroDBManager.CopyNetworkDB';
var
  LQuery      : TDataSet;
  LSQL        : String;
  LInputDir   : String;
  LOutputDir  : String;
begin
  Result := FALSE;
  try
    LSQL := 'SELECT * FROM Network WHERE NetworkID = ' + IntToStr(AOldNetworkID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      if (NOT LQuery.Eof) then
      begin
        LInputDir  := Trim(LQuery.FieldByName('InputDirectory').AsString);
        LInputDir  := StringReplace(LInputDir, AOldNetworkCode, ANewNetworkCode, [rfReplaceAll, rfIgnoreCase]);
        LOutputDir := Trim(LQuery.FieldByName('OutputDirectory').AsString);
        LOutputDir := StringReplace(LOutputDir, AOldNetworkCode, ANewNetworkCode, [rfReplaceAll, rfIgnoreCase]);

        LSQL := 'INSERT INTO Network (NetworkID, NetworkCode, VersionNo, InputDirectory, OutputDirectory, ' +
                'DebugRequired, DebugStartPeriod, DebugEndPeriod, SummaryRequired, SimulationStartYear, ' +
                'SimulationEndYear, IsReadOnly, MinLongitude, MaxLongitude, MinLatitude, MaxLatitude) VALUES (' +
                IntToStr(ANewNetworkID)                                       + ', ' +
                QuotedStr(ANewNetworkCode)                                    + ', ' +
                IntToStr(LQuery.FieldByName('VersionNo').AsInteger)           + ', ' +
                QuotedStr(LInputDir)                                          + ', ' +
                QuotedStr(LOutputDir)                                         + ', ' +
                QuotedStr(Trim(LQuery.FieldByName('DebugRequired').AsString))       + ', ' +
                IntToStr(LQuery.FieldByName('DebugStartPeriod').AsInteger)    + ', ' +
                IntToStr(LQuery.FieldByName('DebugEndPeriod').AsInteger)      + ', ' +
                QuotedStr(Trim(LQuery.FieldByName('SummaryRequired').AsString))     + ', ' +
                IntToStr(LQuery.FieldByName('SimulationStartYear').AsInteger) + ', ' +
                IntToStr(LQuery.FieldByName('SimulationEndYear').AsInteger)   + ', ' +
                IntToStr(0)                                                   + ', ' +
                FloatToStr(LQuery.FieldByName('MinLongitude').AsFloat)        + ', ' +
                FloatToStr(LQuery.FieldByName('MaxLongitude').AsFloat)        + ', ' +
                FloatToStr(LQuery.FieldByName('MinLatitude').AsFloat)         + ', ' +
                FloatToStr(LQuery.FieldByName('MaxLatitude').AsFloat)         + ')';

        Result := GHydroDBAgent.ExecuteSQL(LSQL, True);

        if (Result) then
          Result := GHydroDBAgent.CopyBlobField('Network', 'NetworkID', 'NetworkDiagram', AOldNetworkID, ANewNetworkID);

      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroDBManager.CopyNetworkModules (AOldNetworkID : Integer;
                                             ANewNetworkID : Integer;
                                             AOldModuleIDs : TStringList;
                                             ANewModuleIDs : TStringList): Boolean;
const OPNAME = 'THydroDBManager.CopyNetworkModules';
var
  LSQL         : String;
  LInsertSQL   : String;
  LQuery       : TDataSet;
  LResult      : Boolean;
  LOldModuleID : Integer;
  LNewModuleID : Integer;
begin
  Result := FALSE;
  try
    LSQL := 'SELECT * FROM NetworkModules WHERE NetworkID = ' + IntToStr(AOldNetworkID) +
            ' ORDER BY ModuleID';
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      LResult := TRUE;
      while (LResult AND (NOT LQuery.Eof)) do
      begin
        LOldModuleID := LQuery.FieldByName('ModuleID').AsInteger;
        LNewModuleID := GHydroDBAgent.GetNextID('NetworkModules', 'ModuleID');
        AOldModuleIDs.Add(IntToStr(LOldModuleID));
        ANewModuleIDs.Add(IntToStr(LNewModuleID));

        LInsertSQL := 'INSERT INTO NetworkModules (NetworkID, ModuleID, ModuleNumber, NetworkSequence, Active) VALUES (' +
                      IntToStr(ANewNetworkID)                                   + ', ' +
                      IntToStr(LNewModuleID)                                    + ', ' +
                      IntToStr(LQuery.FieldByName('ModuleNumber').AsInteger)    + ', ' +
                      IntToStr(LQuery.FieldByName('NetworkSequence').AsInteger) + ', ' +
                      QuotedStr(Trim(LQuery.FieldByName('Active').AsString))          + ')';

        LResult := GHydroDBAgent.ExecuteSQL(LInsertSQL, True);

        LQuery.Next;
      end;
      Result := LResult;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroDBManager.CopyModuleTable (ATableName    : String;
                                          AOldModuleIDs : TStringList;
                                          ANewModuleIDs : TStringList;
                                          AModuleIDs    : TStringList) : Boolean;
const OPNAME = 'THydroDBManager.CopyModuleTable';
var
  LInsSQL      : String;
  LValSQL      : String;
  LIndex       : Integer;
  LFieldName   : String;
  LIDIndex     : Integer;
  LFieldDef    : TFieldDef;
  LSQL         : String;
  LQuery       : TDataSet;
  LResult      : Boolean;
  LOldModuleID : Integer;
  LNewModuleID : Integer;
begin
  Result := FALSE;
  try
    LSQL    := 'SELECT * FROM ' + ATableName + ' WHERE ModuleID IN (' + AModuleIDs.CommaText + ')';
    LQuery  := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      if ((LQuery.Eof) AND (LQuery.Bof)) then
        Result := TRUE
      else
      begin
        LResult := TRUE;
        while (LResult AND (NOT(LQuery.Eof))) do
        begin
          LNewModuleID := 0;
          LInsSQL      := '';
          LValSQL      := '';
          for LIndex := 0 to LQuery.FieldDefs.Count - 1 do
          begin
            LFieldDef := LQuery.FieldDefs.Items[LIndex];
            LFieldName := LFieldDef.Name;
            if (LFieldName = 'ModuleID') then
            begin
              LOldModuleID := LQuery.FieldByName(LFieldName).AsInteger;
              LIDIndex     := AOldModuleIDs.IndexOf(IntToStr(LOldModuleID));
              if (LIDIndex >= 0) then
                LNewModuleID := StrToInt(ANewModuleIDs.Strings[LIDIndex]);
              LInsSQL      := LInsSQL + ', ' + LFieldName;
              LValSQL      := LValSQL + ', ' + IntToStr(LNewModuleID);
            end
            else
            begin
              if ((LFieldName = 'Year') OR (LFieldName = 'Month') OR (LFieldName = 'Index')) then
                LInsSQL    := LInsSQL + ', [' + LFieldName + ']'
              else
                LInsSQL    := LInsSQL + ', ' + LFieldName;
              case (LFieldDef.DataType) of
                ftWideString,
                ftString   : LValSQL := LValSQL + ', ' + QuotedStr(Trim(LQuery.FieldByName(LFieldName).AsString));
                ftFloat    : LValSQL := LValSQL + ', ' + FloatToStr(LQuery.FieldByName(LFieldName).AsFloat);
                ftInteger,
                ftWord,
                ftSmallint : LValSQL := LValSQL + ', ' + IntToStr(LQuery.FieldByName(LFieldName).AsInteger);
              else
                ShowMessage('Unknown data type ');
              end;
            end;
          end;

          if (LNewModuleID <> 0) then
          begin
            LInsSQL := Copy(LInsSQL, 2, Length(LInsSQL) - 1);
            LValSQL := Copy(LValSQL, 2, Length(LValSQL) - 1);
            LInsSQL := 'INSERT INTO ' + ATableName + ' (' + LInsSQL + ') VALUES (' + LValSQL + ')';
            LResult := GHydroDBAgent.ExecuteSQL(LInsSQL, True);
          end
          else
            LResult := FALSE;

          LQuery.Next;
        end;
        Result := LResult;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroDBManager.CopyNetworkTable (ATableName    : String;
                                           AOldNetworkID : Integer;
                                           ANewNetworkID : Integer) : Boolean;
const OPNAME = 'THydroDBManager.CopyNetworkTable';
var
  LInsSQL      : String;
  LValSQL      : String;
  LIndex       : Integer;
  LFieldName   : String;
  LFieldDef    : TFieldDef;
  LSQL         : String;
  LQuery       : TDataSet;
  LResult      : Boolean;
begin
  Result := FALSE;
  try
    LSQL    := 'SELECT * FROM ' + ATableName + ' WHERE NetworkID = ' + IntToStr(AOldNetworkID);
    LQuery  := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      if ((LQuery.Eof) AND (LQuery.Bof)) then
        Result := TRUE
      else
      begin
        LResult := TRUE;
        while (LResult AND (NOT(LQuery.Eof))) do
        begin
          LInsSQL      := '';
          LValSQL      := '';
          for LIndex := 0 to LQuery.FieldDefs.Count - 1 do
          begin
            LFieldDef := LQuery.FieldDefs.Items[LIndex];
            LFieldName := LFieldDef.Name;
            if (LFieldName = 'NetworkID') then
            begin
              LInsSQL      := LInsSQL + ', ' + LFieldName;
              LValSQL      := LValSQL + ', ' + IntToStr(ANewNetworkID);
            end
            else
            begin
              if ((LFieldName = 'Year') OR (LFieldName = 'Month') OR (LFieldName = 'Index')) then
                LInsSQL    := LInsSQL + ', [' + LFieldName + ']'
              else
                LInsSQL    := LInsSQL + ', ' + LFieldName;
              case (LFieldDef.DataType) of
                ftWideString,
                ftString   : LValSQL := LValSQL + ', ' + QuotedStr(Trim(LQuery.FieldByName(LFieldName).AsString));
                ftFloat    : LValSQL := LValSQL + ', ' + FloatToStr(LQuery.FieldByName(LFieldName).AsFloat);
                ftInteger,
                ftWord,
                ftSmallint : LValSQL := LValSQL + ', ' + IntToStr(LQuery.FieldByName(LFieldName).AsInteger);
              else
                ShowMessage('Unknown data type ');
              end;
            end;
          end;

          if (LInsSQL <> '') then
          begin
            LInsSQL := Copy(LInsSQL, 2, Length(LInsSQL) - 1);
            LValSQL := Copy(LValSQL, 2, Length(LValSQL) - 1);
            LInsSQL := 'INSERT INTO ' + ATableName + ' (' + LInsSQL + ') VALUES (' + LValSQL + ')';
            LResult := GHydroDBAgent.ExecuteSQL(LInsSQL, True);
          end
          else
            LResult := FALSE;

          LQuery.Next;
        end;
        Result := LResult;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroDBManager.CopyNetworkRoutes (AOldNetworkID : Integer;
                                            ANewNetworkID : Integer;
                                            AOldModuleIDs : TStringList;
                                            ANewModuleIDs : TStringList;
                                            AOldRouteIDs  : TStringList;
                                            ANewRouteIDs  : TStringList) : Boolean;
const OPNAME = 'THydroDBManager.CopyNetworkRoutes';
var
  LQuery        : TDataSet;
  LSQL          : String;
  LSQLInsert    : String;
  LOldRouteID   : Integer;
  LNewRouteID   : Integer;
  LOldSourceID  : Integer;
  LNewSourceID  : Integer;
  LOldSinkID    : Integer;
  LNewSinkID    : Integer;
  LResult       : Boolean;
  LIDIndex      : Integer;
begin
  Result := FALSE;
  try
    LSQL := 'SELECT * FROM NetworkRoutes WHERE NetworkID = ' + IntToStr(AOldNetworkID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      LResult := TRUE;
      while (LResult AND (NOT LQuery.Eof)) do
      begin
        LNewSourceID  := 0;
        LNewSinkID    := 0;
        LOldRouteID   := LQuery.FieldByName('RouteID').AsInteger;
        LNewRouteID   := GHydroDBAgent.GetNextID('NetworkRoutes', 'RouteID');
        AOldRouteIDs.Add(IntToStr(LOldRouteID));
        ANewRouteIDs.Add(IntToStr(LNewRouteID));
        
        LOldSourceID  := LQuery.FieldByName('SourceModuleID').AsInteger;
        LIDIndex      := AOldModuleIDs.IndexOf(IntToStr(LOldSourceID));
        if (LIDIndex >= 0) then
          LNewSourceID := StrToInt(ANewModuleIDs.Strings[LIDIndex]);
        LOldSinkID    := LQuery.FieldByName('SinkModuleID').AsInteger;
        LIDIndex      := AOldModuleIDs.IndexOf(IntToStr(LOldSinkID));
        if (LIDIndex >= 0) then
          LNewSinkID := StrToInt(ANewModuleIDs.Strings[LIDIndex]);

        LSQLInsert := 'INSERT INTO NetworkRoutes (RouteID, NetworkID, RouteNo, ' +
                      'SourceModuleID, SinkModuleID, RouteCost) VALUES (' +
                       IntToStr(LNewRouteID)                               + ', ' +
                       IntToStr(ANewNetworkID)                             + ', ' +
                       IntToStr(LQuery.FieldByName('RouteNo').AsInteger)   + ', ' +
                       IntToStr(LNewSourceID)                              + ', ' +
                       IntToStr(LNewSinkID)                                + ', ' +
                       IntToStr(LQuery.FieldByName('RouteCost').AsInteger) + ')';

        LResult := GHydroDBAgent.ExecuteSQL(LSQLInsert, True);

        LQuery.Next;
      end;
      Result := LResult;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroDBManager.CopySimulationResults (AOldNetworkID : Integer;
                                                ANewNetworkID : Integer;
                                                AOldModuleIDs : TStringList;
                                                ANewModuleIDs : TStringList;
                                                AOldRouteIDs  : TStringList;
                                                ANewRouteIDs  : TStringList) : Boolean;
const OPNAME = 'THydroDBManager.CopySimulationResults';
var
  LQuery        : TDataSet;
  LSQL          : String;
  LSQLInsert    : String;
  LOldElementID : Integer;
  LNewElementID : Integer;
  LElementType  : String;
  LResult       : Boolean;
  LIDIndex      : Integer;
  LOldResultID  : Integer;
  LNewResultID  : Integer;
begin
  Result := FALSE;
  try
    LSQL := 'SELECT * FROM SimulationResults WHERE NetworkID = ' + IntToStr(AOldNetworkID) +
            ' ORDER BY ResultID';
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      LResult := TRUE;
      while (LResult AND (NOT LQuery.Eof)) do
      begin
        LNewElementID  := 0;
        LOldResultID   := LQuery.FieldByName('ResultID').AsInteger;
        LOldElementID  := LQuery.FieldByName('ElementID').AsInteger;
        LElementType   := Trim(LQuery.FieldByName('ElementType').AsString);
        if (LElementType = 'RQ') then
        begin
          LIDIndex := AOldRouteIDs.IndexOf(IntToStr(LOldElementID));
          if (LIDIndex >= 0) then
            LNewElementID := StrToInt(ANewRouteIDs.Strings[LIDIndex]);
        end
        else
        begin
          LIDIndex := AOldModuleIDs.IndexOf(IntToStr(LOldElementID));
          if (LIDIndex >= 0) then
            LNewElementID := StrToInt(ANewModuleIDs.Strings[LIDIndex]);
        end;

        LNewResultID   := GHydroDBAgent.GetNextID('SimulationResults', 'ResultID');

        LSQLInsert := 'INSERT INTO SimulationResults (ResultID, NetworkID, ElementType, ' +
                      'ElementSubType, ElementID, SubElementID, ResultTypeID, AllZero) VALUES (' +
                       IntToStr(LNewResultID)                                   + ', ' +
                       IntToStr(ANewNetworkID)                                  + ', ' +
                       QuotedStr(Trim(LQuery.FieldByName('ElementType').AsString))    + ', ' +
                       QuotedStr(Trim(LQuery.FieldByName('ElementSubType').AsString)) + ', ' +
                       IntToStr(LNewElementID)                                  + ', ' +
                       IntToStr(LQuery.FieldByName('SubElementID').AsInteger)   + ', ' +
                       IntToStr(LQuery.FieldByName('ResultTypeID').AsInteger)   + ', ' +
                       IntToStr(LQuery.FieldByName('AllZero').AsInteger)        + ')';

        LResult := GHydroDBAgent.ExecuteSQL(LSQLInsert, True);

        if (LResult) then
          LResult := GHydroDBAgent.CopyBlobField('SimulationResults', 'ResultID', 'ResultData', LOldResultID, LNewResultID);

        LQuery.Next;
      end;
      Result := LResult;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroDBManager.CopyNetwork (ANetworkID      : Integer;
                                      ANewNetworkCode : WideString;
                                      var AErrorMsg   : WideString): WordBool;
const OPNAME = 'THydroDBManager.CopyNetwork';
var
  LNewNetworkID         : Integer;
  LResult               : Boolean;
  LOldModuleIDs         : TStringList;
  LNewModuleIDs         : TStringList;
  LOldRouteIDs          : TStringList;
  LNewRouteIDs          : TStringList;
  LModuleTableList      : TStringList;
  LNetworkTableList     : TStringList;
  LModuleIDsList        : TStringList;
  LIndex                : Integer;
  LTableName            : String;
  LDiagramDir           : string;
  LOldNetworkCode       : string;
begin
  Result := FALSE;
  try
    LOldNetworkCode := GetNetworkCode(ANetworkID);
    LNewNetworkID   := GetNetworkID(ANewNetworkCode);
    if (LOldNetworkCode = '') then
    begin
      AErrorMsg := 'Network ' + LOldNetworkCode + ' does NOT exist.';
    end
    else if (LNewNetworkID <> 0) then
    begin
      AErrorMsg := 'Network ' + ANewNetworkCode + ' already exists.';
    end
    else
    begin
      LDiagramDir := GetNetworkDiagramsDirectory(LOldNetworkCode);
      LNewNetworkID := GHydroDBAgent.GetNextID('Network', 'NetworkID');
      GHydroDBAgent.StartTransaction;
      try
        LResult := CopyNetworkDB(ANetworkID, LNewNetworkID, LOldNetworkCode, ANewNetworkCode);
        if (NOT LResult) then
          AErrorMsg := 'Could not copy ' + LOldNetworkCode + ' to ' + ANewNetworkCode + ' in Network table.';
        if (LResult) then
        begin
          LOldModuleIDs     := TStringList.Create;
          LNewModuleIDs     := TStringList.Create;
          LOldRouteIDs      := TStringList.Create;
          LNewRouteIDs      := TStringList.Create;
          LModuleIDsList    := TStringList.Create;
          LModuleTableList  := TStringList.Create;
          LNetworkTableList := TStringList.Create;
          try
            GetNetworkModuleIDs(ANetworkID, LModuleIDsList);
            PopulateModuleTablesList(LModuleTableList);
            PopulateNetworkTablesList(LNetworkTableList);

            // The following must be done here because it populates the OldModuleIDs and NewModuleIDs on
            // which the following code depends.
            LResult := CopyNetworkModules(ANetworkID, LNewNetworkID, LOldModuleIDs, LNewModuleIDs);
            if (NOT LResult) then
              AErrorMsg := 'Could not copy ' + LOldNetworkCode + ' to ' + ANewNetworkCode + ' in NetworkModules table.';

            LIndex := 0;
            while (LResult AND (LIndex < LModuleTableList.Count)) do
            begin
              LTableName := LModuleTableList.Strings[LIndex];
              LResult := CopyModuleTable(LTableName, LOldModuleIDs, LNewModuleIDs, LModuleIDsList);
              if (NOT LResult) then
                AErrorMsg := 'Could not copy ' + LOldNetworkCode + ' to ' + ANewNetworkCode + ' in ' + LTableName + ' table.';
              LIndex := LIndex + 1;
            end;

            // The following must be done here because it populates the OldRouteIDs and NewRouteIDs on
            // which the following code depends.
            if (LResult) then
            begin
              LResult := CopyNetworkRoutes(ANetworkID, LNewNetworkID, LOldModuleIDs, LNewModuleIDs, LOldRouteIDs, LNewRouteIDs);
              if (NOT LResult) then
                AErrorMsg := 'Could not copy ' + LOldNetworkCode + ' to ' + ANewNetworkCode + ' in NetworkRoutes table.';
            end;

            LIndex := 0;
            while (LResult AND (LIndex < LNetworkTableList.Count)) do
            begin
              LTableName := LNetworkTableList.Strings[LIndex];
              if (LTableName = 'SimulationResults') then
                LResult := CopySimulationResults(ANetworkID, LNewNetworkID, LOldModuleIDs, LNewModuleIDs, LOldRouteIDs, LNewRouteIDs)
              else if (LTableName = 'HydroNVDrawings') then
                LResult := CopyHydroNVDiagrams(LOldNetworkCode, ANewNetworkCode, ANetworkID, LNewNetworkID, LDiagramDir)
              else if ((LTableName <> 'NetworkModules') AND (LTableName <> 'NetworkRoutes') AND (LTableName <> 'Network')) then
                LResult := CopyNetworkTable(LTableName, ANetworkID, LNewNetworkID);
              if (NOT LResult) then
                AErrorMsg := 'Could not copy ' + LOldNetworkCode + ' to ' + ANewNetworkCode + ' in ' + LTableName + ' table.';
              LIndex := LIndex + 1;
            end;

          finally
            LOldModuleIDs.Free;
            LNewModuleIDs.Free;
            LOldRouteIDs.Free;
            LNewRouteIDs.Free;
            LModuleTableList.Free;
            LNetworkTableList.Free;
            LModuleIDsList.Free;
          end;
        end;
        if (LResult) then
        begin
          GHydroDBAgent.CommitTransaction;
          Result := LResult;
        end
        else
          GHydroDBAgent.RollbackTransaction;
      except
        GHydroDBAgent.RollbackTransaction;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroDBManager.CopyHydroNVDiagrams (AOldNetworkCode : WideString;
                                              ANewNetworkCode : WideString;
                                              AOldNetworkID   : Integer;
                                              ANewNetworkID   : Integer;
                                              ADiagramDir     : String): WordBool;
const OPNAME = 'THydroDBManager.CopyHydroNVDiagrams';
var
  LSQL          : String;
  LInsertSQL    : String;
  LQuery        : TDataSet;
  LOldFileName  : String;
  LNewFileName  : String;
  LNewDirectory : String;
  LResult       : Boolean;
begin
  Result := FALSE;
  try
    LSQL := 'SELECT * FROM HydroNVDrawings WHERE NetworkID = ' + IntToStr(AOldNetworkID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LResult := TRUE;
      LQuery.Open;
      while (LResult AND (NOT LQuery.Eof)) do
      begin
        LOldFileName := Trim(LQuery.FieldByName('DrawingName').AsString);
        LNewFileName := StringReplace(LOldFileName, AOldNetworkCode, ANewNetworkCode, [rfReplaceAll, rfIgnoreCase]);
        LInsertSQL   := 'INSERT INTO HydroNVDrawings (NetworkID, DrawingName, GISDrawing, ReadOnly) VALUES ('  +
                        IntToStr(ANewNetworkID) + ',' +
                        QuotedStr(LNewFileName) + ', ' +
                        IntToStr(LQuery.FieldByName('GISDrawing').AsInteger) + ', 0)';
        LResult := GHydroDBAgent.ExecuteSQL(LInsertSQL, True);

        LOldFileName  := ADiagramDir + LOldFileName + '.VSD';
        LNewDirectory := StringReplace(ADiagramDir, AOldNetworkCode, ANewNetworkCode, [rfReplaceAll, rfIgnoreCase]); //DSR Degerous in case the network code appears more than once in the path
        LNewFileName  := LNewDirectory + LNewFileName + '.VSD';
        if (FileExists(LOldFileName)) then
        begin
          if (NOT DirectoryExists(LNewDirectory)) then
            ForceDirectories(LNewDirectory);
          if (DirectoryExists(LNewDirectory)) then
          begin
            if (FileExists(LNewFileName)) then
              SysUtils.DeleteFile(LNewFileName);
            CopyFile(PChar(LOldFileName), PChar(LNewFileName), TRUE);
          end;
        end;
        LQuery.Next;
      end;
      Result := LResult;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroDBManager.IsNetworkReadOnly (ANetworkID : Integer) : Boolean;
const OPNAME = 'THydroDBManager.IsNetworkReadOnly';
var
  LQuery      : TDataSet;
  LSQL        : String;
  LReadOnly   : Integer;
begin
  Result := FALSE;
  try
    LSQL := 'SELECT * FROM Network WHERE NetworkID = ' + IntToStr(ANetworkID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      if (NOT LQuery.Eof) then
      begin
        LReadOnly := LQuery.FieldByName('IsReadOnly').AsInteger;
        Result    := LReadOnly <> 0;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroDBManager.Get_AllNetworkIDsCommText : WideString;
const OPNAME = 'THydroDBManager.Get_AllNetworkIDsCommText';
var
  LQuery      : TDataSet;
  LSQL        : String;
  LCodeList   : TStringList;
begin
  Result := '';
  try
    LCodeList := TStringList.Create;
    try
      LSQL := 'SELECT * FROM Network';
      LQuery := GHydroDBAgent.CreateQuery(LSQL);
      try
        LQuery.Open;
        while (NOT LQuery.Eof) do
        begin
          LCodeList.Add(Trim(LQuery.FieldByName('NetworkID').AsString));
          LQuery.Next;
        end;
        Result := LCodeList.CommaText;
      finally
        LQuery.Close;
        LQuery.Free;
      end;
    finally
      LCodeList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function THydroDBManager.Get_NetworkPropertiesCommText(ANetworkID : integer): WideString;
const OPNAME = 'THydroDBManager.Get_NetworkPropertiesCommText';
var
  LQuery      : TDataSet;
  LSQL        : String;
  LNetworkProperties   : TStringList;
begin
  Result := '';
  try
    LNetworkProperties := TStringList.Create;
    try
      LSQL := 'SELECT * FROM Network WHERE NetworkID = '+ IntToStr(ANetworkID);
      LQuery := GHydroDBAgent.CreateQuery(LSQL);
      try
        LQuery.Open;
        if (NOT LQuery.Eof) then
        begin
          LNetworkProperties.Add('NetworkID='+Trim(LQuery.FieldByName('NetworkID').AsString));
          LNetworkProperties.Add('NetworkCode='+Trim(LQuery.FieldByName('NetworkCode').AsString));
          LNetworkProperties.Add('VersionNo='+Trim(LQuery.FieldByName('VersionNo').AsString));
          LNetworkProperties.Add('InputDirectory='+Trim(LQuery.FieldByName('InputDirectory').AsString));
          LNetworkProperties.Add('OutputDirectory='+Trim(LQuery.FieldByName('OutputDirectory').AsString));
          LNetworkProperties.Add('DebugRequired='+Trim(LQuery.FieldByName('DebugRequired').AsString));
          LNetworkProperties.Add('DebugStartPeriod='+Trim(LQuery.FieldByName('DebugStartPeriod').AsString));
          LNetworkProperties.Add('DebugEndPeriod='+Trim(LQuery.FieldByName('DebugEndPeriod').AsString));
          LNetworkProperties.Add('SummaryRequired='+Trim(LQuery.FieldByName('SummaryRequired').AsString));
          LNetworkProperties.Add('SimulationStartYear='+Trim(LQuery.FieldByName('SimulationStartYear').AsString));
          LNetworkProperties.Add('SimulationEndYear='+Trim(LQuery.FieldByName('SimulationEndYear').AsString));
          LNetworkProperties.Add('IsReadOnly='+Trim(LQuery.FieldByName('IsReadOnly').AsString));
          LNetworkProperties.Add('MinLongitude='+Trim(LQuery.FieldByName('MinLongitude').AsString));
          LNetworkProperties.Add('MaxLongitude='+Trim(LQuery.FieldByName('MaxLongitude').AsString));
          LNetworkProperties.Add('MinLatitude='+Trim(LQuery.FieldByName('MinLatitude').AsString));
          LNetworkProperties.Add('MaxLatitude='+Trim(LQuery.FieldByName('MaxLatitude').AsString));
          Result := LNetworkProperties.CommaText;
        end;
        LQuery.Close;
      finally
        LQuery.Free;
      end;
    finally
      LNetworkProperties.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroDBManager.Set_NetworkPropertiesCommText(ANetworkPropertiesCommText : WideString): boolean;
const OPNAME = 'THydroDBManager.Get_NetworkPropertiesCommText';
var
  LTable      : TDataSet;
  LNetworkID  : integer;
  LNetworkProperties   : TStringList;
begin
  Result := False;
  try
    LNetworkProperties := TStringList.Create;
    try
      LNetworkProperties.CommaText := ANetworkPropertiesCommText;
      if(LNetworkProperties.Values['NetworkID'] = '') or (LNetworkProperties.Values['NetworkCode'] = '') then
        Exit;

      LTable := GHydroDBAgent.CreateTable('Network');
      try
        LNetworkID := StrToInt(LNetworkProperties.Values['NetworkID']);
        LTable.Open;
        if LTable.Locate('NetworkID',LNetworkID,[]) then
        begin
          LTable.Edit;
          if(LNetworkProperties.Values['VersionNo'] <> '') then
            LTable.FieldByName('VersionNo').AsString := LNetworkProperties.Values['VersionNo'];
          if(LNetworkProperties.Values['InputDirectory'] <> '') then
            LTable.FieldByName('InputDirectory').AsString := LNetworkProperties.Values['InputDirectory'];
          if(LNetworkProperties.Values['OutputDirectory'] <> '') then
            LTable.FieldByName('OutputDirectory').AsString := LNetworkProperties.Values['OutputDirectory'];
          if(LNetworkProperties.Values['DebugRequired'] <> '') then
            LTable.FieldByName('DebugRequired').AsString := LNetworkProperties.Values['DebugRequired'];
          if(LNetworkProperties.Values['DebugStartPeriod'] <> '') then
            LTable.FieldByName('DebugStartPeriod').AsString := LNetworkProperties.Values['DebugStartPeriod'];
          if(LNetworkProperties.Values['DebugEndPeriod'] <> '') then
            LTable.FieldByName('DebugEndPeriod').AsString := LNetworkProperties.Values['DebugEndPeriod'];
          if(LNetworkProperties.Values['SummaryRequired'] <> '') then
            LTable.FieldByName('SummaryRequired').AsString := LNetworkProperties.Values['SummaryRequired'];
          if(LNetworkProperties.Values['SimulationStartYear'] <> '') then
            LTable.FieldByName('SimulationStartYear').AsString := LNetworkProperties.Values['SimulationStartYear'];
          if(LNetworkProperties.Values['SimulationEndYear'] <> '') then
            LTable.FieldByName('SimulationEndYear').AsString := LNetworkProperties.Values['SimulationEndYear'];
          if(LNetworkProperties.Values['IsReadOnly'] <> '') then
            LTable.FieldByName('IsReadOnly').AsString := LNetworkProperties.Values['IsReadOnly'];
          if(LNetworkProperties.Values['MinLongitude'] <> '') then
            LTable.FieldByName('MinLongitude').AsString := LNetworkProperties.Values['MinLongitude'];
          if(LNetworkProperties.Values['MaxLongitude'] <> '') then
            LTable.FieldByName('MaxLongitude').AsString := LNetworkProperties.Values['MaxLongitude'];
          if(LNetworkProperties.Values['MinLatitude'] <> '') then
            LTable.FieldByName('MinLatitude').AsString := LNetworkProperties.Values['MinLatitude'];
          if(LNetworkProperties.Values['MaxLatitude'] <> '') then
            LTable.FieldByName('MaxLatitude').AsString := LNetworkProperties.Values['MaxLatitude'];
          LTable.Post;
          Result := True;
        end;
      finally
        LTable.Close;
        LTable.Free;
      end;
    finally
      LNetworkProperties.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroDBManager.GetNetworkDiagramsDirectory(ANetworkCode : string): string;
const OPNAME = 'THydroDBManager.GetNetworkDiagramsDirectory';
begin
  Result := '';
  try
    Result := GetAppDataLocalDir + '\Network Diagrams\'+    //ExtractFilePath(ApplicationExeName) + 'Network Diagrams\'+
              (FAppModules.StudyArea.StudyAreaCode) + '\' + ANetworkCode;
//              (FAppModules.StudyArea.ScenarioCode);
    Result :=  IncludeTrailingPathDelimiter(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroDBManager.DoesNetworkExist(ANetworkID: integer): boolean;
const OPNAME = 'THydroDBManager.DoesNetworkExist';
var
  LQuery      : TDataSet;
  LSQL        : String;
begin
  Result := False;
  try
    LSQL := 'SELECT * FROM Network WHERE NetworkID = ' + IntToStr(ANetworkID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      Result := NOT (LQuery.Eof and LQuery.Eof);
    finally
      LQuery.Close;
      LQuery.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroDBManager.DoesNetworkExist(ANetworkCode : WideString): boolean;
const OPNAME = 'THydroDBManager.DoesNetworkExist';
var
  LQuery      : TDataSet;
  LSQL        : String;
begin
  Result := False;
  try
    LSQL := 'SELECT * FROM Network WHERE NetworkCode = ' + QuotedStr(ANetworkCode);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      Result := NOT (LQuery.Eof and LQuery.Eof);
    finally
      LQuery.Close;
      LQuery.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroDBManager.Get_AllNetworkCodeIDsNameValuePairs : WideString;
const OPNAME = 'THydroDBManager.Get_AllNetworkCodeIDsNameValuePairs';
var
  LQuery      : TDataSet;
  LSQL        : String;
  LCodeList   : TStringList;
begin
  Result := '';
  try
    LCodeList := TStringList.Create;
    try
      LSQL := 'SELECT * FROM Network';
      LQuery := GHydroDBAgent.CreateQuery(LSQL);
      try
        LQuery.Open;
        while (NOT LQuery.Eof) do
        begin
          LCodeList.Add(Trim(LQuery.FieldByName('NetworkCode').AsString) + '=' + Trim(LQuery.FieldByName('NetworkID').AsString));
          LQuery.Next;
        end;
        Result := LCodeList.CommaText;
      finally
        LQuery.Close;
        LQuery.Free;
      end;
    finally
      LCodeList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroDBManager.CheckNetworkCode (AFileName        : String;
                                           var ANetworkCode : WideString;
                                           var AErrorMsg    : WideString) : Boolean;
const OPNAME = 'THydroDBManager.CheckNetworkCode';
var
  LXMLDocument : TXMLDocument;
  LRootNode    : IXMLNode;
  LAllDataNode : IXMLNode;
  LNode        : IXMLNode;
begin
  Result := TRUE;
  try
    ANetworkCode := '';
    if (NOT (FileExists(AFileName))) then
    begin
      Result := FALSE;
      AErrorMsg := 'Could not find file ' + AFileName;
    end
    else
    begin
      LXMLDocument := TXMLDocument.Create(Application);
      try
        LXMLDocument.DOMVendor    := GetDOMVendor('MSXML');
        LXMLDocument.ParseOptions := [];
        LXMLDocument.Active := FALSE;
        LXMLDocument.XML.Clear;
        LXMLDocument.LoadFromFile(AFileName);
        LXMLDocument.Active := TRUE;
        LRootNode           := LXMLDocument.DocumentElement;
        LAllDataNode        := LRootNode.ChildNodes['ROWDATA'];

        if (LAllDataNode.ChildNodes.Count > 0) then
        begin
          LNode := LAllDataNode.ChildNodes.Get(0);
          ANetworkCode := LNode.Attributes['NetworkCode'];
          if (ANetworkCode = '') then
          begin
            AErrorMsg := 'Network code may not be empty.';
            Result    := FALSE;
          end
          else if (DoesNetworkExist(ANetworkCode)) then
          begin
            AErrorMsg := 'Network ' + ANetworkCode + ' already exists.';
            Result    := FALSE;
          end;
        end;
      finally
        LXMLDocument.Free;
      end;
    end;  
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.

