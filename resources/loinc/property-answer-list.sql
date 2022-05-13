SELECT LoincNumber id
     , json_group_array(
           json_object(
               'code', 'answer-list',
               'valueCoding', json_object(
                   'code', AnswerListId,
                   'system', 'http://loinc.org',
                   'display', AnswerListName)))
       AS property
FROM answerlistlink
GROUP BY LoincNumber
