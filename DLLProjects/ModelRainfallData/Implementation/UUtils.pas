unit UUtils;

interface

uses windows, Classes;

function GetNextCSVItem(var AStringIn : string): string;
function PadStrLeft(ASource, APadChar : string; ALengthIn : integer):string; overload;
function PadStrLeft(ASource : string; ALengthIn : integer):string; overload;
function PadStr(ASource, APadChar : string; ALengthIn : integer):string; overload;
function PadStr(ASource : string; ALengthIn : integer):string; overload;
function IsValueBetweenEnds(AValue, AEnd1, AEnd2 : integer) : boolean;overload;
function IsValueBetweenEnds(AValue, AEnd1, AEnd2 : double) : boolean;overload;
function IsPointToRightOfLine(APoint, ALineEnd1, ALineEnd2 : TPoint) : boolean;
function DoLinesIntersect(AFirstLineEnd1, AFirstLineEnd2, ASecondLineEnd1, ASecondLineEnd2: TPoint) : boolean;
function ArePointsSame(APoint1, APoint2 : TPoint) : boolean;
//function DoPolygonSidesIntersect (APoints : array of TPoint) : boolean;
function WildCardCompare(ACriteria, AValue: string): boolean;
function CriteriaHasOnlyOneStarInTheMiddle(ACriteria: string) : boolean;
function WithoutSpaces(AValue : string) : string;


implementation

uses

  // Delphi
  SysUtils,
  System.Types,
  VCL.Dialogs,

  //DWAF
  UErrorHandlingOperations;

function GetNextCSVItem(var AStringIn : string): string;
const
  OPNAME = 'UUTils.GetNextCSVItem';
var
 LIndex : integer;
begin
  try
    LIndex := Pos(',',AStringIn);
    // VGN 17/01/2002 We might have to cater for quotes here that is ' or "
    if (LIndex = 0) then
    begin
      Result := AStringIn;
      AStringIn := '';
    end
    else
    begin
      Result := Copy(AStringIn,1 ,LIndex - 1);
      AStringIn := Copy(AStringIn, LIndex + 1, Length(AStringIn));
    end;
    if Result = '' then
      Result := '*';
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function PadStrLeft(ASource,APadChar : string; ALengthIn : integer):string;
const
  OPNAME = 'UUTils.PadStrLeft';
begin
  try
    Result := ASource;
    while Length(Result) < ALengthIn do
    begin
      Result := APadChar + Result;
    end;
    Result := Copy(Result,1,ALengthIn);
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function PadStrLeft(ASource : string; ALengthIn : integer):string;
const
  OPNAME = 'UUTils.PadStrLeft';
begin
  try
    Result := ASource;
    while Length(Result) < ALengthIn do
    begin
      Result := ' ' + Result;
    end;
    Result := Copy(Result,1,ALengthIn);
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function PadStr(ASource,APadChar : string; ALengthIn : integer):string;
const
  OPNAME = 'UUTils.PadStr';
begin
  try
    Result := ASource;
    while Length(Result) < ALengthIn do
    begin
      Result := Result + APadChar;
    end;
    Result := Copy(Result,1,ALengthIn);
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function PadStr(ASource : string; ALengthIn : integer):string;
const
  OPNAME = 'UUTils.PadStr';
begin
  try
    Result := ASource;
    while Length(Result) < ALengthIn do
    begin
      Result := Result + ' ';
    end;
    Result := Copy(Result,1,ALengthIn);
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function ArePointsSame(APoint1, APoint2 : TPoint) : boolean;
const
  OPNAME = 'UUTils.ArePointsSame';
begin
  //try
    Result := ((APoint1.x = APoint2.x) and (APoint1.y = APoint2.y));
  // except on E:Exception do HandleError(E,OPNAME) end;
end;

function IsValueBetweenEnds(AValue, AEnd1, AEnd2 : integer) : boolean;
const
  OPNAME = 'UUTils.IsValueBetweenEnds';
begin
  //try
    Result := ((AValue >= AEnd1) and (AValue <= AEnd2)) or ((AValue <= AEnd1) and (AValue >= AEnd2));
  // except on E:Exception do HandleError(E,OPNAME) end;
end;

function IsValueBetweenEnds(AValue, AEnd1, AEnd2 : Double) : boolean;
const
  OPNAME = 'UUTils.IsValueBetweenEnds';
begin
  //try
    Result := ((AValue >= AEnd1) and (AValue <= AEnd2)) or ((AValue <= AEnd1) and (AValue >= AEnd2));
  // except on E:Exception do HandleError(E,OPNAME) end;
end;


function IsPointToRightOfLine(APoint, ALineEnd1, ALineEnd2 : TPoint) : boolean;
const
  OPNAME = 'UUTils.IsPointToRightOfLine';
begin
  Result := false;
  try
    if (ALineEnd1.x < ALineEnd2.x) then
    begin
      Result := DoLinesIntersect(Point(ALineEnd1.x, APoint.y), APoint, ALineEnd1, ALineEnd2);
    end
    else
    begin
      Result := DoLinesIntersect(Point(ALineEnd2.x, APoint.y), APoint, ALineEnd1, ALineEnd2);
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function DoLinesIntersect(AFirstLineEnd1, AFirstLineEnd2, ASecondLineEnd1, ASecondLineEnd2: TPoint) : boolean;
const
  OPNAME = 'UUTils.DoLinesIntersect';
type
  TLineCalcType = (lctSecondLineVertical);
var
//  LCalcType: TLineCalcType;
  LMConst,LCConst : double;
  LXOnLine : integer;
begin
  Result := False;
  try
    // Check if the points co-incide.
    if (ArePointsSame(AFirstLineEnd2, ASecondLineEnd1) or
        ArePointsSame(AFirstLineEnd2, ASecondLineEnd2)) then
    begin
      Result := True;
    end
    else
    begin

      //  Check if point is between Y values.
      if IsValueBetweenEnds(AFirstLineEnd2.y, ASecondLineEnd1.y, ASecondLineEnd2.y) then
      begin

        // Is line vertical ?
        if (ASecondLineEnd1.x = ASecondLineEnd2.x) then
        begin

          // The line is vertical then the gradient is infinite.
          LXOnLine := ASecondLineEnd1.x;
  //        LCalcType := lctSecondLineVertical;
        end
        else
        begin

          // Attempt to calculate the gradient of the line.
          LMConst := (ASecondLineEnd1.y - ASecondLineEnd2.y)/(ASecondLineEnd1.x - ASecondLineEnd2.x);

          // Attempt to calculate the y intercept of the line.
          LCConst := ASecondLineEnd1.y - (LMConst * ASecondLineEnd1.x);

          // Calculate the X intercept.
          LXOnLine := Round((AFirstLineEnd2.y - LCConst) / LMConst);
        end;

        // The line crosses if the point is to the right of the line.
        Result := (LXOnLine <= AFirstLineEnd2.x);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;
{
function DoPolygonSidesIntersect (AFirstLineEnd2s : array of TPoint) : boolean;
const
  OPNAME = 'UUTils.DoPolygonSidesIntersect';
var
  LIndex,LInnerIndex : integer;
begin
  for LIndex  := 0 to length(APoints) - 2 do
  begin
    for LInnerIndex := 0 to length(APoints) - 2 do
    begin

    end;
  end;
end;
}
function CriteriaHasOnlyOneStarInTheMiddle(ACriteria : string) : boolean;
const
  OPNAME = 'UUTils.CriteriaHasOnlyOneStarInTheMiddle';
begin
    Result := ( Pos ( '*', Copy ( ACriteria, 2, Length ( ACriteria ) - 2 ) ) > 0 )
end;
function WithoutSpaces(AValue : string) : string;
const OPNAME = 'WithoutSpaces';
begin
  try
    if AValue <> '' then
    begin
      while (Pos(' ', AValue) > 0) do
        Delete(AValue, Pos(' ', AValue), 1);
    end;
    Result := AValue;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function WildCardCompare(ACriteria, AValue: string): boolean;
const
  OPNAME = 'UUTils.WildCardCompare';
var
  LCharacterIndex: integer;
  LCounter : integer;
  LValue : string;
  LStrValue : string;
  LSubCriteria : string;
  LCriteria : string;
  function CriteriaAtTheEndOfStr : boolean;
  const
    OPNAME = 'UUTils.CriteriaAtTheEndOfStr';
  begin
    Result := False;
    LStrValue := AValue;
    while CharInSet(LValue[Length(LValue)],[LStrValue[Length(LStrValue)]]) do
    begin
      Result := False;
      delete ( LStrValue, Length ( LStrValue ), 1 );
      delete ( LValue, Length ( LValue ), 1 );
      if LValue = '' then
      begin
        Result := True;
        Break;
      end;
    end;
  end;

  function CriteriaHasOneStarAtTheBeginningAndInTheMiddle : boolean;
  const
    OPNAME = 'UUTils.CriteriaHasOneStarAtTheBeginningAndInTheMiddle';
  begin
    Result := ( ACriteria [ 1 ] = '*' ) and
              CriteriaHasOnlyOneStarInTheMiddle(ACriteria);
  end;

  function CriteriaHasOneStarAtTheEndAndInTheMiddle : boolean;
  const
    OPNAME = 'UUTils.CriteriaHasOneStarAtTheEndAndInTheMiddle';
  begin
    Result := ( ACriteria [ Length ( Acriteria ) ] = '*' ) and
              ( ACriteria [ 1 ] <> '*' ) and
              CriteriaHasOnlyOneStarInTheMiddle(ACriteria);
  end;

  function CriteriaHasOneStarAtTheEndBeginningAndInTheMiddle : boolean;
  const
    OPNAME = 'UUTils.CriteriaHasOneStarAtTheEndBeginningAndInTheMiddle';
  begin
    Result := ( ACriteria [ Length ( Acriteria ) ] = '*' ) and
              ( ACriteria [ 1 ] = '*' ) and
              CriteriaHasOnlyOneStarInTheMiddle(ACriteria);
  end;

  function GetSubStrWithStarAtTheBeginning ( ACriteriaStr : string ) : string;
  const
    OPNAME = 'UUTils.GetSubStrWithStarAtTheBeginning';
  begin
    result := Copy ( ACriteriaStr, 2, Length ( ACriteriaStr ) );
  end;

  function GetSubStrWithoutStars ( ACriteriaStr : string ) : string;
  const
    OPNAME = 'UUTils.GetSubStrWithoutStars';
  var
    LCharCount : integer;
  begin
    if Pos ( '*', ACriteriaStr ) > 0 then
    begin
      if ( ACriteriaStr[1] = '*' ) and ( ACriteriaStr[Length(ACriteriaStr)] = '*' ) then
      begin
        LCharCount := Pos ( '*', Copy ( ACriteriaStr, 2, Length ( ACriteriaStr ) ) );
        Result := Copy ( ACriteriaStr, 2, LCharCount );
      end;

      if ( ACriteriaStr[1] <> '*' ) and ( ACriteriaStr[Length(ACriteriaStr)] = '*' ) then
        Result := Copy ( ACriteriaStr, 1, Pos ( '*', ACriteriaStr ) - 1 );

      if ( ACriteriaStr[1] <> '*' ) and ( ACriteriaStr[Length(ACriteriaStr)] <> '*' ) then
        Result := Copy ( ACriteriaStr, 1, Pos ( '*', ACriteriaStr ) );

      if ( ACriteriaStr[1] = '*' ) and ( ACriteriaStr[Length(ACriteriaStr)] <> '*' ) and ( LCounter > 1 ) then
        Result := Copy ( ACriteriaStr, 2, Length ( ACriteriaStr ) );

      if ( ACriteriaStr[1] = '*' ) and ( ACriteriaStr[Length(ACriteriaStr)] <> '*' ) and ( LCounter = 1 ) then
      begin
        LCharCount := Pos ( '*', Copy ( ACriteriaStr, 2, Length ( ACriteriaStr ) ) );
        Result := Copy ( ACriteriaStr, 2, LCharCount );
      end;

      while pos ( '*', Result ) > 0 do
        Delete ( Result,Pos ( '*', Result ), 1 );
     end;
  end;

  function SubCriteriaFound : boolean;
  const
    OPNAME = 'UUTils.SubCriteriaFound';
  begin
    Result := Pos( LSubCriteria, AValue ) > 0;
  end;

  function CriteriaHasMoreSubCreteria : boolean;
  const
    OPNAME = 'UUTils.CriteriaHasMoreSubCreteria';
  begin
    Result := CriteriaHasOnlyOneStarInTheMiddle(ACriteria) or
              CriteriaHasOneStarAtTheBeginningAndInTheMiddle or
              CriteriaHasOneStarAtTheEndAndInTheMiddle or
              CriteriaHasOneStarAtTheEndBeginningAndInTheMiddle;
  end;

  procedure CrearUsedSubCriteria;
  const
    OPNAME = 'UUTils.CrearUsedSubCriteria';
  begin
    if LCriteria <> '' then
      delete ( LCriteria, Pos ( LSubCriteria, LCriteria ), Length ( LSubCriteria ) );

    while ( ( LCriteria[1] = '*' ) and ( LCriteria[2] = '*' ) ) do
      delete ( LCriteria, 1, 1 );
  end;

  function CriteriaFound : boolean;
  const
    OPNAME = 'UUTils.CriteriaFound';
  begin
    Result := False;
    LCriteria := ACriteria;
    LCounter := 0;
    while Length ( LCriteria ) > 3 do
    begin
      inc ( LCounter );
      LSubCriteria := GetSubStrWithoutStars ( LCriteria );
      Result := SubCriteriaFound;
      CrearUsedSubCriteria;
    end;
  end;
begin
  Result := True;
  try
    for LCharacterIndex := 1 to Length(ACriteria) do
    begin
      LValue := ACriteria;
      while Pos ( '*', LValue ) > 0 do
        delete ( LValue, Pos ( '*', LValue ), 1 );

      if ( CriteriaHasMoreSubCreteria ) and ( CriteriaFound ) then
      begin
        Result := True;
        Exit;
      end
      else
      if ( ACriteria [ LCharacterIndex ] = '*') and
        ( Pos ( LValue, Avalue ) > 0 ) and
        ( LCharacterIndex = 1 ) and CriteriaAtTheEndOfStr then
      begin
        Result := True;
        Exit;
      end
      else
      if ( ACriteria [ LCharacterIndex ] = '*') and
        ( Pos ( LValue, Avalue ) > 0 ) and
        ( ACriteria [ Length ( ACriteria )] = '*') and
        ( LCharacterIndex = 1 ) then
      begin
        Result := True;
        Exit;
      end
      else
      if (ACriteria[LCharacterIndex] = '*') and ( LCharacterIndex > 1 ) then
      begin
        Result := True;
        Exit;
      end
      else
      begin
        if (LCharacterIndex > Length(AValue)) then
        begin
          Result := False;
          break;
        end
        else
        begin
          if (ACriteria[LCharacterIndex] <> '?') then
          begin
            if (ACriteria[LCharacterIndex] <> AValue[LCharacterIndex]) then
            begin
              Result := False;
              break;
            end;
          end;
        end;
      end;
    end;
    // If this point is reached there are no wild cards at the end of the criteria.
    if (Length(AValue) > Length(ACriteria)) then
      Result := False;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

end.
