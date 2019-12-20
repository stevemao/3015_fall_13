data Position = CEO | Manager | Programmer | Intern  deriving (Eq,Show)

data Field = EmployeeId Int | Title Position | Name String | Salary Int | Shares Int deriving (Eq)

instance Show Field where
   show (EmployeeId k) = show k
   show (Title p) = show p
   show (Name s) = s
   show (Salary k) = "$" ++ show k
   show (Shares k) = show k ++" options"

type Column = Int
type Row = [Field]
type Table = [Row]

employees = 
   [[EmployeeId 1234, Name "Smith, John"],
    [EmployeeId 1235, Name "Jones, Mary"], 
    [EmployeeId 1236, Name "Brown, Edwin"], 
    [EmployeeId 1237, Name "Green, Marvin"],
    [EmployeeId 1238, Name "Edwards, Susan"]]

positions = 
   [[EmployeeId 1234, Title CEO],
    [EmployeeId 1235, Title Manager], 
    [EmployeeId 1236, Title Programmer], 
    [EmployeeId 1237, Title Programmer],
    [EmployeeId 1238, Title Intern]]

salary = 
    [[EmployeeId 1234, Salary 1000000],
     [EmployeeId 1235, Salary 100000], 
     [EmployeeId 1236, Salary 55000 ], 
     [EmployeeId 1237, Salary 78000],
     [EmployeeId 1238,Salary 0]]


stock_options = 
    [[EmployeeId 1234, Shares 1000000],
     [EmployeeId 1235, Shares 1000], 
     [EmployeeId 1236, Shares 5000 ], 
     [EmployeeId 1237, Shares 2500],
     [EmployeeId 1238, Shares 5]]



-- delete :: Column -> Row -> Row
-- join :: (Table,Column) -> (Table,Column) -> Table
-- project :: [Column] -> Table -> Table



