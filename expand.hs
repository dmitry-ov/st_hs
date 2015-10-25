infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

--Исправьте определение функции expand так, чтобы она,
--используя дистрибутивность (а также, возможно, ассоциативность и коммутативность),
--возвращала значение, эквивалентное данному и являющееся суммой произведений числовых значений.



expand :: Expr -> Expr

expand ((e1 :+: e2) :*: e) = expand e1 :*: expand e :+: expand e2 :*: expand e
expand (e :*: (e1 :+: e2)) = expand e :*: expand e1 :+: expand e :*: expand e2

expand ((e1 :*: e2) :+: e) = expand e1 :*: expand e2 :+: expand e
expand (e :+: (e1 :*: e2)) = expand e :+: expand e1 :*: expand e2

expand (e :+: (e1 :+: e2)) = expand e :+: expand e1 :+: expand e2
expand ((e1 :+: e2) :+: e) = expand e1 :+: expand e2 :+: expand e

expand (e :*: (e1 :*: e2)) = expand e :*: expand e1 :*: expand e2
expand ((e1 :*: e2) :*: e) = expand e1 :*: expand e2 :*: expand e

expand (e1 :+: e2) = expand e1 :+: expand e2
expand (e2 :+: e1) = expand e2 :+: expand e1

expand (e1 :*: e2) = expand e1 :*: expand e2
expand (e2 :*: e1) = expand e2 :*: expand e1


expand e = e

