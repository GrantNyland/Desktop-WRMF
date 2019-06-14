//
//  UNIT      : Contains field update SQL.
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/01/23
//  COPYRIGHT : Copyright © 2003 DWAF
//
unit UFieldUpdateSQL_NetworkVisualiser;

interface

type TFieldUpdateSQLStepItemAddFunction = procedure (
  AStepNo: integer; AFieldName, ATableName, AFieldInTable, AUpdateSQL, AGetValueSQL: string) of object;

procedure LoadFieldPropertyUpdateSQLSteps(AAdd: TFieldUpdateSQLStepItemAddFunction);

implementation

uses
  UFieldUpdateSQLCommonClauses;

const
  CNVDrawingPos =
    ' FROM                                        ' +
    '   NVDrawingPos A                            ' +
    CWhereScenario                       + '  AND ' +
    '   (A.ElementId     = :AElementId    )   AND ' +
    '   (A.ElementInstID = :AElementInstID)   AND ' +
    '   (A.DrawingID     = :ADrawingID    )   AND ' +
    '   (A.Owner         = :AOwner        )       ' ;
  CNVDrawingInstChannel =
    ' FROM                                        ' +
    '   NVDrawingInstChannel A                    ' +
    CWhereScenario                       + '  AND ' +
    '   (A.ElementId      = :AElementId     ) AND ' +
    '   (A.ElementInstID  = :AElementInstID ) AND ' +
    '   (A.DrawingID      = :ADrawingID     ) AND ' +
    '   (A.ChannelSegment = :AChannelSegment)     ' ;
  CNVDrawingInstCaption =
    ' FROM                                        ' +
    '   NVDrawingInstCaption A                    ' +
    CWhereScenario                       + '  AND ' +
    '   (A.ElementId      = :AElementId     ) AND ' +
    '   (A.ElementInstID  = :AElementInstID ) AND ' +
    '   (A.DrawingID      = :ADrawingID     ) AND ' +
    '   (A.CaptionIndex   = :ACaptionIndex)       ' ;
  CNVDrawingGroup =
    ' FROM                                        ' +
    '   NVDrawingGroup A                          ' +
    ' WHERE                                       ' +
    '   (A.Model          = :AModel         ) AND ' +
    '   (A.StudyAreaName  = :AStudyAreaName ) AND ' +
    '   (A.SubArea        = :ASubArea       ) AND ' +
    '   (A.Scenario       = :AScenario      ) AND ' +
    '   (A.DrawingGroupID = :ADrawingGroupID)     ' ;
  CNVDrawing =
    ' FROM                                        ' +
    '   NVDrawing A                               ' +
    ' WHERE                                       ' +
    '   (A.Model          = :AModel         ) AND ' +
    '   (A.StudyAreaName  = :AStudyAreaName ) AND ' +
    '   (A.SubArea        = :ASubArea       ) AND ' +
    '   (A.Scenario       = :AScenario      ) AND ' +
    '   (A.DrawingGroupID = :ADrawingGroupID) AND ' +
    '   (A.DrawingID      = :ADrawingID     )     ' ;

procedure LoadFieldPropertyUpdateSQLSteps(AAdd: TFieldUpdateSQLStepItemAddFunction);
const OPNAME = 'LoadFieldPropertyUpdateSQLSteps';
begin

  //
  // NVDrawingPos.
  //
  AAdd(0,'XPos',           'NVDrawingPos', 'XPos',          'SELECT *            ' + CNVDrawingPos, '');
  AAdd(0,'YPos',           'NVDrawingPos', 'YPos',          'SELECT *            ' + CNVDrawingPos, '');
  AAdd(0,'GISXPos',        'NVDrawingPos', 'GISXPos',       'SELECT *         ' + CNVDrawingPos, '');
  AAdd(0,'GISYPos',        'NVDrawingPos', 'GISYPos',       'SELECT *         ' + CNVDrawingPos, '');
  AAdd(0,'GISExtentTop',   'NVDrawingPos', 'GISExtentLeft', 'SELECT *    ' + CNVDrawingPos, '');
  AAdd(0,'GISExtentLeft',  'NVDrawingPos', 'GISExtentLeft', 'SELECT *   ' + CNVDrawingPos, '');
  AAdd(0,'GISExtentRight', 'NVDrawingPos', 'GISExtentLeft', 'SELECT *  ' + CNVDrawingPos, '');
  AAdd(0,'GISExtentBottom','NVDrawingPos', 'GISExtentLeft', 'SELECT * ' + CNVDrawingPos, '');

  //
  // NVDrawingInstChannel
  //
  AAdd(0,'ChannelSegment', 'NVDrawingInstChannel','ChannelSegment', ' SELECT * ' + CNVDrawingInstChannel, '');

  //
  // NVDrawingInstCaption
  //
  AAdd(0,'Caption',        'NVDrawingInstCaption', 'Caption',      ' SELECT * ' + CNVDrawingInstCaption, '');
  AAdd(0,'CaptionIndex',   'NVDrawingInstCaption', 'CaptionIndex', ' SELECT *, CaptionIndex AS CaptionNumber ' + CNVDrawingInstCaption, '');

  //
  // NVDrawingGroup
  //
  AAdd(0, 'DrawingGroupName', 'NVDrawingGroup', 'DrawingGroupName', 'SELECT * ' + CNVDrawingGroup, '');

  //
  // NVDrawing
  //
  AAdd(0,'LastZoom',      'NVDrawing', 'LastZoom',      'SELECT *      ' + CNVDrawing, '');
  AAdd(0,'LastLayout',    'NVDrawing', 'LastLayout',    'SELECT *    ' + CNVDrawing, '');
  AAdd(0,'DrawingWidth',  'NVDrawing', 'DrawingWidth',  'SELECT *  ' + CNVDrawing, '');
  AAdd(0,'DrawingHeight', 'NVDrawing', 'DrawingHeight', 'SELECT * ' + CNVDrawing, '');

end;

end.
