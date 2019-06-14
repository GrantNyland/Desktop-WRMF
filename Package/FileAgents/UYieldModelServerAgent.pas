//
//  Contains : Class TYieldModelServerAgent
//
unit UYieldModelServerAgent;


interface


//
// Interface dependencies
//
uses
  VoaimsCom_TLB;


//
// A class the manages the in-process instance of the Voaims COM server for the Yield Model.
//
type
  TYieldModelServerAgent = class(TObject)
  protected
    FServerInitialised: boolean;
    FUserHasLoggedIn: boolean;
    FStudyHasBeenSelected: boolean;
  public

    // Public reference to the server object.
    Server: IVoaimsComObject;

    // Construction, destruction.
    constructor Create;
    destructor Destroy; override;

    // Initialisation.
    function ReInitialiseServer: boolean;
    procedure DestroyServer;
    function LogonUser(AUserName, APassword: string): boolean;
    function SelectStudy(AModel, AStudy, ASubArea, AScenario: string): boolean;
    function CheckServer: boolean;
  end;


implementation


//
// Implementation dependencies
//
uses

  // Delphi
  SysUtils,

  // Alborak
  UErrorHandlingOperations;


//
// Constructor.
//
constructor TYieldModelServerAgent.Create;
const OPNAME = 'TYieldModelServerAgent.Create';
begin
  try
    inherited Create;
    Server := nil;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Destructor.
//
destructor TYieldModelServerAgent.Destroy;
const OPNAME = 'TYieldModelServerAgent.Destroy';
begin
  try
    Server := nil;
    inherited Destroy;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Creates an instance of the server and call the initialisation routine.
//
function TYieldModelServerAgent.ReInitialiseServer: boolean;
const OPNAME = 'TYieldModelServerAgent.ReInitialiseServer';
begin
  Result := False;
  try

    // Destroy the server if it currently exists.
    DestroyServer;

    // Attempt to create a new instance of the server.
    Server := CoVoaimsComObject.Create;
    if (not Assigned(Server)) then
    begin
      raise Exception.Create('Could not create an instance of the VoaimsCOMServer object. ');
    end else begin
      if (not Server.Initialise) then
      begin
        DestroyServer;
        raise Exception.Create('Could not initialise the VoaimsCOMServer instance. ');
      end else begin
        FServerInitialised := True;
        Result := True;
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Destroys the current instance of the COM server.
//
procedure TYieldModelServerAgent.DestroyServer;
const OPNAME = 'TYieldModelServerAgent.DestroyServer';
begin
  try
    FServerInitialised := False;
    FUserHasLoggedIn := False;
    FStudyHasBeenSelected := False;
    Server := nil;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Logon user
//
function TYieldModelServerAgent.LogonUser(AUserName, APassword: string): boolean;
const OPNAME = 'TYieldModelServerAgent.LogonUser';
begin
  Result := False;
  FUserHasLoggedIn := False;
  try

    // Check that the server is initialised.
    if (not Assigned(Server)) and FServerInitialised then
    begin
      raise Exception.Create('The Logon function can not be called if the server has not been initialised. ');
    end else begin

      // Attempt to log the user on.
      if (not Server.Logon(AUserName, APassword)) then
      begin
        raise Exception.CreateFmt('The user [%s] could not be logged on. ', [AUserName]);
      end else begin
        FUserHasLoggedIn := True;
        Result := True;
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Selects the required study user.
//
function TYieldModelServerAgent.SelectStudy(AModel, AStudy, ASubArea, AScenario: string): boolean;
const OPNAME = 'TYieldModelServerAgent.SelectStudy';
begin
  Result := False;
  FStudyHasBeenSelected := False;
  try

    // Check that the server is initialised.
    if (not Assigned(Server)) and FServerInitialised then
    begin
      raise Exception.Create('The Logon function can not be called if the server has not been initialised. ');
    end else begin

      // Check that a user is logged on.
      if (not FServerInitialised) then
      begin
        raise Exception.Create('A study can not be selected until a user has been logged on. ');
      end else begin

        // Attempt to select the study.
        if (not Server.SelectStudy(AModel, AStudy, ASubArea, AScenario)) then
        begin
          raise Exception.CreateFmt('The study [%s, %s, %s, %s] could not be selected. ', [AModel, AStudy, ASubArea, AScenario]);
        end else begin
          FStudyHasBeenSelected := True;
          Result := True;
        end;
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Checks whether the server is ready to run the model.
//
function TYieldModelServerAgent.CheckServer: boolean;
const OPNAME = 'TYieldModelServerAgent.CheckServer';
begin
  Result := False;
  try
    if (not Assigned(Server)) then
    begin
      raise Exception.Create('An instance of the VoaimsCOMServer object has not beed assigned. ');
    end else begin

      // We need to add functions for these.
      FServerInitialised := True;
      FUserHasLoggedIn := True;
      FStudyHasBeenSelected := True;

      // Done.
      Result := True;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
