SELECT
  Sequence,
  FileName
FROM
  StomsaHydrologyFileLink
WHERE
  (Model = @aModel) AND
  (StudyAreaName = @aStudyAreaName) AND
  (SubArea = @aSubArea) AND
  (Scenario = @aScenario)
ORDER BY
  Sequence
