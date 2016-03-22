import Data.List
data InfNumber a = MinusInfinity
		| Number a
		| PlusInfinity
		deriving Show

infMax MinusInfinity x = x
infMax x MinusInfinity = x
infMax PlusInfinity x = PlusInfinity
infMax x PlusInfinity = PlusInfinity
infMax (Number x) (Number y) = Number $ max x y

--list and predicate
yxpartition f l = partition f l

yxfind f l = find f l

--dropWhile 这个函数并不是把list中不满足f的element给drop掉，而是把list中的element从左到右，输入f，当f值为True时，就把此element给drop，知道f为False，停止，并把剩下的list作为结果输出，**在剩下的list中是允许有使f为True的值的。
yxdropWhile f l = dropWhile f l

--takeWhile 就跟dropWhile相反，他会保留List中的element，知道f为True时，List中剩下的Element都会被drop掉；
yxtakeWhile f l = dropWhile f l

--span就是同时使用takeWhile和dropwhile，返回一个tuple，fst tuple是takeWhile的值， snd tuple是dropWhile的值
yxspan f l = span f l

--exists
yxany f l = any f l
--for all
yxall f l = all f l

