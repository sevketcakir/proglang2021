fun double x = x+x;
fun square x = x*x;
(* 1 ile parametre olarak verilen n sayısı arasındaki tek sayıların toplamını hesaplayan fonksiyon *)
fun soru1 1 = 1|soru1 n = if n mod 2 = 0 then soru1(n-1) else n+soru1(n-2);
(* Parametre olarak verilen listedeki elemanların toplamını döndüren fonksiyon *)
fun soru2 [] = 0 | soru2 (b::k) = b + soru2 k;
(* Parametre olarak verilen listenin en küçük elemanını veren fonksiyon *)
fun soru3 [x] = x| soru3 (b::k) = if b<=soru3 k then b else soru3 k;
(* Parametre olarak verilen listedeki tek sayıların bulunduğu listeyi döndüren fonksiyon *)
fun soru4 [] = []|soru4 (b::k) = if b mod 2 = 1 then b::soru4 k else soru4 k;
(* Bir fonksiyon ve bir listeyi parametre olarak alan ve listeyi fonksiyona göre filtreleyen fonksiyon *)
fun soru5 _ [] = []|soru5 f (b::k)=if f b then b::soru5 f k else soru5 f k;
(* map fonksiyonunu kullanan ve parametre olarak verilen listedeki sayıların karesini alıp liste olarak döndüren fonksiyon *)
fun soru6 l = map (fn x=>x*x) l;