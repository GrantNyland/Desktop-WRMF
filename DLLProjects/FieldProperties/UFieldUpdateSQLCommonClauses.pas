//
//  UNIT      : Contains SQL statements.
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/01/23
//  COPYRIGHT : Copyright © 2003 DWAF
//
unit UFieldUpdateSQLCommonClauses;

interface

const
  CWhereScenario =
    ' WHERE                                     ' +
    '   (A.Model         = :AModelCode    ) AND ' +
    '   (A.StudyAreaName = :AStudyAreaCode) AND ' +
    '   (A.SubArea       = :ASubAreaCode  ) AND ' +
    '   (A.Scenario      = :AScenarioCode )     ' ;
  CWhereScenarioIdentifier =
    ' WHERE                                     ' +
    '   (A.Model         = :AModelCode    ) AND ' +
    '   (A.StudyAreaName = :AStudyAreaCode) AND ' +
    '   (A.SubArea       = :ASubAreaCode  ) AND ' +
    '   (A.Scenario      = :AScenarioCode ) AND ' +
    '   (A.Identifier    = :AIdentifier   )     ' ;

implementation

end.
