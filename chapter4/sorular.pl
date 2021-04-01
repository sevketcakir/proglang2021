%Listenin ilk elemanını veren yüklem
ilk([B|K], B).
%Listenin son elemanını veren yüklem
son([X], X).
son([B|K], S) :- son(K, S).

son2(L, S) :- append(_,[S],L).
%Listenin sondan bir önceki elemanını veren yüklem
sondan_bir_once(L, S) :- append(_, [S,_], L).
%Listedeki en küçük sayıyı veren yüklem
enkucuk([X], X).
enkucuk([B|K], EK) :- enkucuk(K, EK), EK<B.
enkucuk([B|K], B) :- enkucuk(K, EK), EK>=B.

enkucuk2([X], X).
enkucuk2([B|K], EKK) :- enkucuk2(K, EK),
    ( EK<B -> EKK=EK;
        EKK=B
        ).
%Listedeki elemanların toplamını veren yüklem
toplam([],0).
toplam([B|K], T) :- toplam(K, T1), T is B+T1.
%Listedeki elemanların karesini veren yüklem
kare([], []).
kare([B|K], [BK|KK]) :- BK is B*B, kare(K, KK).
%Listedeki negatif sayıları filtreleyen yüklem
negatif([], []).
negatif([B|K], [B|KR]) :- B >= 0, negatif(K, KR).
negatif([B|K], KR) :- B < 0, negatif(K, KR).
%Listede ardışık tekrar eden elemanları eleyen/ayıklayan yüklem ele([1, 1, 2, 2, 2, 1, 1, 3], [1, 2, 1, 3]).
ele([], []).
ele([X],[X]).
ele([A,A|K], KE) :- ele([A|K], KE).
ele([A,B|K], [A|KE]) :- A \= B, ele([B|K], KE).

