let wrap x = x :: []
 
let test z cond = if cond
 then wrap z
 else wrap true
 
let rec check cond =
 test (if cond then false else check (not cond)) cond
//│ U max: 59, total: 154
//│ UERR 6 errors
//│ L: 1 [bool ~ [[[[[[[[[[[[[list['a106']]]]]]]]]]]]]], [[[[[bool]]]]] <: 'a106', ['a106' - list['a106'] ~ list['a101'] - 'a101', [[[[[[[list['a106']]]]]]]] <: α112', [[[[[[[list['a101']]]]]]]] <: α112'], [[[[[[[[[[[[[list['a106']]]]]]]]]]]]]] <: 'a101']
//│ L: 2 [list['a101'] ~ [[[[[bool]]]]], [[[[[[[[[[[[[list['a101']]]]]]]]]]]]]] <: 'a101', ['a101' - list['a101'] ~ list['a106'] - 'a106', [[[[list['a101']]]]] <: α105', [α99' - (α99' -> [α105']) ~ ([bool] -> α112') - [bool], [[(α99' -> [α105'])]] <: check98', check98' <: [[[([bool] -> α112')]]]], [[[[[[[list['a106']]]]]]]] <: α112'], [[[[[bool]]]]] <: 'a106']
//│ L: 2 [list['a106'] ~ [[[[[bool]]]]], [[[[[[[[[[[[[list['a106']]]]]]]]]]]]]] <: 'a101', ['a101' - list['a101'] ~ list['a106'] - 'a106', [[[[list['a101']]]]] <: α105', [α99' - (α99' -> [α105']) ~ ([bool] -> α112') - [bool], [[(α99' -> [α105'])]] <: check98', check98' <: [[[([bool] -> α112')]]]], [[[[[[[list['a106']]]]]]]] <: α112'], [[[[[bool]]]]] <: 'a106']
//│ L: 1 [bool ~ [[[[[[[[[[[[[list['a101']]]]]]]]]]]]]], [[[[[bool]]]]] <: 'a106', ['a106' - list['a106'] ~ list['a101'] - 'a101', [[[[[[[list['a106']]]]]]]] <: α112', [[[[[[[list['a101']]]]]]]] <: α112'], [[[[[[[[[[[[[list['a101']]]]]]]]]]]]]] <: 'a101']
//│ L: 0 [bool ~ [[[[[[[[list['a106']]]]]]]]], [[[bool]]] <: α109', [[[[[[[[list['a106']]]]]]]]] <: α109']
//│ L: 0 [bool ~ [[[[[[[[list['a101']]]]]]]]], [[[bool]]] <: α109', [[[[[[[[list['a101']]]]]]]]] <: α109']

