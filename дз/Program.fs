//функция ввода числа с клавиатуры
let rec readFloat() = // let привязывает имя к значению или функции
    match System.Double.TryParse(System.Console.ReadLine()) with // match позволяет управлятьь 
    | false, _ -> printfn "Требуется ввести число"; readFloat() // ветвлением на основе сравнения выражения с набором шаблонов
    | _, x -> x
 
//ввод с клавиатуры коэффициентов уравнения
let a = readFloat();
let b = readFloat();
let c = readFloat();
 
//зададим тип - результат решения квадратного уравнения
type ResultOfSolve=
    None
    |Line of float
    |Quadrat of float*float
 
//зададим формулу решения квадратого уравнения в общем виде
let solve = fun (a,b,c) ->
    let D = b*b-4.0*a*c
    if a=0.0 then
        if b=0.0 then None else Line(-c/b)
    else
        if D<0.0 then None else Quadrat(((-b+sqrt(D))/(2.0*a),(-b-sqrt(D))/(2.0*a)))
 
//решим квадратное уравнение
let res = solve(a,b,c);
 
//выведем результат
match res with
    None -> printf "Нет решений"
    |Line(x) -> printf "Линейное уравнение, корень %f" x
    |Quadrat(x1,x2) -> printf "Квадратное уравнение, корни %f %f " x1 x2
 
System.Console.Read();