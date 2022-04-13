double x = x+x
square x=x*x
liste=[1..9]
liste2=[4,7,5,1,9,2,3,6,8]
--1 ile parametre olarak verilen n sayısı arasındaki tek sayıların toplamını hesaplayan fonksiyon
soru1 1 = 1
soru1 n = if even n then soru1 (n-1) else n+soru1 (n-2)
--Parametre olarak verilen listedeki elemanların toplamını döndüren fonksiyon
--soru2::[Float]->Float
--soru2 = sum
soru2 [] = 0
soru2 (b:k) = b+soru2 k
--Parametre olarak verilen listenin en küçük elemanını veren fonksiyon
soru3 [x] = x
soru3 (b:k) = if b < soru3 k then b else soru3 k
--Parametre olarak verilen listedeki tek sayıların bulunduğu listeyi döndüren fonksiyon
--soru4 = filter (\x->mod x 2==1)
soru4 [] = []
soru4 (b:k) = if mod b 2==1 then b:soru4 k else soru4 k
--Bir fonksiyon ve bir listeyi parametre olarak alan ve listeyi fonksiyona göre filtreleyen fonksiyon
soru5 _ [] = []
soru5 f (b:k)
    | f b = b:soru5 f k
    | otherwise = soru5 f k
--map fonksiyonunu kullanan ve parametre olarak verilen listedeki sayıların karesini alıp liste olarak döndüren fonksiyon
soru6 l = map (\y->y*y) l