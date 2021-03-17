fun double x = x*x;
(* 1 ile parametre olarak verilen n sayısı arasındaki tek sayıların toplamını hesaplayan fonksiyon *)
fun soru1 1 = 1 | soru1 n = if n mod 2=0 then soru1 (n-1) else n+soru1 (n-2);
(* Parametre olarak verilen listedeki elemanların toplamını döndüren fonksiyon *)
fun soru2 [] = 0 | soru2 (h::t) = h + soru2 t;
(* Parametre olarak verilen listenin en küçük elemanını veren fonksiyon *)
fun soru3 (h::[]) = h | soru3 (h::t) = if h < soru3 t then h else soru3 t;
fun soru3_2 (h::[]) = h | soru3_2 l = if hd l < soru3_2 (tl l) then hd l else soru3_2 (tl l);
(* Parametre olarak verilen listedeki tek sayıların bulunduğu listeyi döndüren fonksiyon *)
fun soru4 [] = [] | soru4 (h::t) = if h mod 2=1 then h::soru4 t else soru4 t;
(* Bir fonksiyon ve bir listeyi parametre olarak alan ve listeyi fonksiyona göre filtreleyen fonksiyon *)
fun soru5 _ [] = [] | soru5 f (h::t) = if f h then h::soru5 f t else soru5 f t; 
(* map fonksiyonunu kullanan ve parametre olarak verilen listedeki sayıların karesini alıp liste olarak döndüren fonksiyon *)

datatype 'a BST = Nil | Node of 'a * 'a BST * 'a BST;
(* İkili arama ağacında arama yapan fonksiyon *)
fun soru6 Nil _ = false | soru6 (Node(d,sol,sag)) a = if d=a then true else if a<d then soru6 sol a else soru6 sag a;
val deneme=Node(4,Node(3,Nil,Nil),Node(8,Nil,Nil));
(* İkili arama ağacına ekleme yapan fonksiyon *)
fun soru7 Nil e=Node(e,Nil,Nil) | soru7 (Node(d,sol,sag)) e = if e<d then Node(d,soru7 sol e,sag) else Node(d,sol,soru7 sag e);

