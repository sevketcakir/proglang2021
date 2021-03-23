topla a b = a + b
--1 ile parametre olarak verilen n sayısı arasındaki tek sayıların toplamını hesaplayan fonksiyon
soru1 n = sum [x | x <- [1..n], odd x]
--Parametre olarak verilen listedeki elemanların toplamını döndüren fonksiyon
{-
Derste hata veren kısmı aşağıdaki iki satırla çözebiliriz
soru2::[Int]->Int
soru2 = sum
-}
soru2 [] = 0
soru2 (b:k) = b + soru2 k

--soru2 l=sum l
--Parametre olarak verilen listenin en küçük elemanını veren fonksiyon
soru3 [e] = e
soru3 (b:k) = if b < soru3 k then b else soru3 k
--Parametre olarak verilen listedeki tek sayıların bulunduğu listeyi döndüren fonksiyon
--soru4 l = [x| x<-l, odd x]
soru4 [] = []
soru4 (b:k) = if odd b then b:soru4 k else soru4 k
--Bir fonksiyon ve bir listeyi parametre olarak alan ve listeyi fonksiyona göre filtreleyen fonksiyon
soru5 _ [] = []
soru5 f (b:k)
    | f b = b:soru5 f k
    | otherwise = soru5 f k
--map fonksiyonunu kullanan ve parametre olarak verilen listedeki sayıların karesini alıp liste olarak döndüren fonksiyon
soru6 l = map (\x -> x*x) l

data BST a = Nil | Node a (BST a) (BST a) deriving (Show, Eq)

kar (Node v _ _) v2 = v<v2 -- karşılaştırma denemesi için
--İkili arama ağacında arama yapan fonksiyon
ornek_agac = Node 6 (Node 4 Nil Nil) (Node 14 Nil Nil)
ornek_agac2 = Node 8 (Node 4 (Node 2 Nil Nil) (Node 6 Nil Nil)) (Node 12 (Node 10 Nil Nil) (Node 14 Nil Nil))
ara Nil _ = False
ara (Node deger sol sag) aranacak
    | aranacak==deger = True
    | aranacak<deger = ara sol aranacak
    | otherwise = ara sag aranacak
--   K
--  / \
--Sol Sag
--İkili arama ağacına ekleme yapan fonksiyon
ekle Nil eklenecek = Node eklenecek Nil Nil
ekle (Node deger sol sag) eklenecek
    | eklenecek < deger = Node deger (ekle sol eklenecek) sag
    | eklenecek > deger = Node deger sol (ekle sag eklenecek)
    | otherwise = Node deger sol sag

--Parametre olarak verilen listedeki elemanları sırasıyla ağaca ekleyecek fonksiyonu yazın(ekleListe)
ekleListe agac [] = agac
ekleListe agac liste = ekle (ekleListe agac (init liste)) (last liste)

{-
    BST silme 
    Silme işlemi için bir dizi başka fonksiyona ihtiyaç vardır,
    bunlar aşağıdadır
-}
--Düğüm yaprak mı
yaprak (Node _ Nil Nil) = True
yaprak _ = False

--düğümün tek çocuğu mu var
tekCocuk (Node _ Nil (Node _ _ _)) = True
tekCocuk (Node _ (Node _ _ _) Nil) = True
tekCocuk _ = False
--düğümün çift çocuğu mu var
ciftCocuk (Node _ sol sag) = sol /= Nil && sag /= Nil
--Silinecek değerin bulunduğu düğümü bulan fonksiyon
dugum Nil _ = Nil
dugum agac@(Node deger sol sag) aranan
    | deger==aranan = agac
    | aranan<deger = dugum sol aranan
    | otherwise = dugum sag aranan

{-
    sil fonksiyonu parameter olarak verilen ağaçtan 
    yine parametre olarak alınan değeri siler. Önce silinecek 
    değerin bulunduğu düğüm bulunur ve silYrd yardımcı fonksiyonu
    ile silme işlemi gerçekleştirilir.
-}
sil Nil _ = Nil
sil agac@(Node deger sol sag) silinecek
    | deger == silinecek = silYrd agac silinecek
    | silinecek < deger = Node deger (sil sol silinecek) sag
    | otherwise = Node deger sol (sil sag silinecek)

--Ağacın sağ çocuğu var mı
solVar (Node _ (Node _ _ _) Nil) = True
solVar _ = False
--Ağacın sol çocuğu var mı
sagVar (Node _ Nil (Node _ _ _)) = True
sagVar _ = False
--Ardıl değerini bulan successor fonksiyonu
successor (Node deger _ sag) = enSol sag
--Ağacın en soldaki düğümünün değerini veren fonksiyon
enSol (Node deger Nil _) = deger
enSol (Node _ sol _) = enSol sol

--Sil fonksiyonu yardımcısı
silYrd agac@(Node deger sol sag) silinecek
    | yaprak agac = Nil -- Yapraktaysa sil
    | tekCocuk agac = if solVar agac then sol else sag --tek çocuk sol ise solu bağla aksi halde sağı
    | otherwise = Node (successor agac) sol (sil sag (successor agac))

--Ağacı girintilerle yazdırmak için kullanılan fonksiyon
--Kullanımı: putStrLn $ bst_str ornek_agac2 0
bst_str Nil _ = ""
bst_str (Node deger sol sag) seviye = (take seviye $ repeat ' ') ++ show deger ++ "\n" ++ bst_str sol (seviye+1) ++ bst_str sag (seviye+1)