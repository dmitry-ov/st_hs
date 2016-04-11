import Data.Monoid (Sum(..))
import Control.Monad.Trans.Writer (Writer, execWriter, writer)


type Shopping = Writer (Sum Integer) ()

--shopping1 :: Shopping
--shopping1 = do
--  purchase "Jeans"   19200
--  purchase "Water"     180
--  purchase "Lettuce"   328


purchase :: String -> Integer -> Shopping
purchase item cost = writer ((), Sum cost)
--
total :: Shopping -> Integer
total = getSum . execWriter


--Последовательность приобретенных товаров записывается с помощью do-нотации. Для этого используется функция purchase, которую вам предстоит реализовать. Эта функция принимает наименование товара, а также его стоимость в исландских кронах (исландскую крону не принято делить на меньшие единицы, потому используется целочисленный тип Integer). Кроме того, вы должны реализовать функцию total:
--
--GHCi> total shopping1
--19708