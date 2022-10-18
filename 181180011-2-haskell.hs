-- Ufuk Bakan Gazi University  - 181180011 - 2020
--Noktalar kümesi :
noktalar = [0,1,2,3]
--Kenarlar kümesi :
--Kenar gösterimi : kaynak , hedef , ağırlık
kenarlar  = [[0,3,4] , [0,2,9] , [1,3,1] , [3,2,2], [2,1,8]]

checknext kenar ziyaretGrafi currentNextNode =
    if currentNextNode /= -1
    then currentNextNode
    else
      if ziyaretGrafi!!(kenar!!1) == 0
      then (kenar!!1)
      else (-1)

toInt :: Float -> Int
toInt x = round x

dijkstra noktalar kenarlar baslangic alinanYol uzaklikGrafi ziyaretGrafi i nextNode = do
  if(length uzaklikGrafi < 1)
    then do
    let bString = show baslangic
    putStrLn ("Baslangic noktasi "++bString++" olarak secildi")
    let tempUGrafi = take (baslangic) (repeat (1/0)) ++ [0] ++ take ((length noktalar) - baslangic - 1) (repeat(1/0))
    dijkstra noktalar kenarlar baslangic alinanYol tempUGrafi ziyaretGrafi i nextNode
    else do
    if (length ziyaretGrafi < 1)
      then do
      let tempZGrafi = take (length noktalar) (repeat 0)
      dijkstra noktalar kenarlar baslangic alinanYol uzaklikGrafi tempZGrafi i nextNode
      else do
      if (i < length kenarlar)
        then do
        if (kenarlar!!i!!0 == baslangic)
          then do
          let yeniNextNode = checknext (kenarlar!!i) ziyaretGrafi nextNode
          let yol = alinanYol + (kenarlar!!i!!2)
          if(fromIntegral yol < uzaklikGrafi!!(kenarlar!!i!!1) )
            then do
            let j = kenarlar!!i!!1
            let yeniUGrafi = take j uzaklikGrafi ++ [fromIntegral yol] ++ drop (j+1) uzaklikGrafi
            dijkstra noktalar kenarlar baslangic alinanYol yeniUGrafi ziyaretGrafi (i+1) yeniNextNode
            else do
            dijkstra noktalar kenarlar baslangic alinanYol uzaklikGrafi ziyaretGrafi (i+1) yeniNextNode
          else do
          dijkstra noktalar kenarlar baslangic alinanYol uzaklikGrafi ziyaretGrafi (i+1) nextNode
        else do
        let yeniZGrafi = take(baslangic)(ziyaretGrafi) ++ [1] ++ drop(baslangic+1)(ziyaretGrafi)
        if (nextNode /= (-1) )
          then do
          let shortestPath = toInt(uzaklikGrafi!!nextNode)
          dijkstra noktalar kenarlar nextNode shortestPath uzaklikGrafi yeniZGrafi 0 (-1)
          else do
          --Ziyaret edilecek nextNode yoksa ve i iterasyonu
          --tamamlanmışsa uzaklıkları ekrana yaz
          enYakinUzakliklarıYaz uzaklikGrafi 0

enYakinUzakliklarıYaz uzaklikGrafi n = do
  if(n < length uzaklikGrafi)
    then do
    let stringn = show n
    let stringu = show (uzaklikGrafi!!n)
    putStrLn (stringn ++ ". noktaya en kisa uzaklik : " ++ stringu)
    enYakinUzakliklarıYaz uzaklikGrafi (n+1)
    else do
    putStrLn "==================="

main = do
  putStrLn "Graf Yonludur !"
  putStrLn "Noktalar Kumesi :"
  print noktalar
  putStrLn "Kenarlar Kumesi :"
  print kenarlar
  dijkstra noktalar kenarlar 0 0 [] [] 0 (-1)
  dijkstra noktalar kenarlar 1 0 [] [] 0 (-1)
  dijkstra noktalar kenarlar 2 0 [] [] 0 (-1)
  dijkstra noktalar kenarlar 3 0 [] [] 0 (-1)