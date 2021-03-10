#lang racket
;;;Sorular
;;; 1'den verilen n sayısına kadar olan tek sayıların toplamını bulan fonksiyon
(define soru1 (lambda (n) (cond
                            ((< n 1) 0)
                            ((= 0 (modulo n 2)) (soru1 (- n 1)))
                            (else (+ n (soru1 (- n 2))))
                            )))
;;; Listedeki en küçük değeri bulan fonksiyon
;;; car cdr cons null?
;;; -|-------
(define soru2 (lambda (l) (
                           cond
                            ((null? (cdr l)) (car l)) ;;; tek eleman varsa
                            (else (min (car l) (soru2 (cdr l))))
                            )))
;;; Listedeki elemanların toplamını hesaplayan fonksiyon
(define soru3 (lambda (l) (
                           cond
                            ((null? l) 0)
                            (else (+ (car l) (soru3 (cdr l))))
                            )))
;;; Listedeki çift sayıların kareleri toplamını hesaplayan fonksiyon
(define soru4 (lambda (l) (
                           cond
                            ((null? l) 0)
                            ((= 0 (modulo (car l) 2)) (+ (* (car l) (car l)) (soru4 (cdr l))))
                            (else (soru4 (cdr l)))
                            )))
;;; Listedeki tek sayıları filtreleyen(kabul eden) fonksiyon
(define soru5 (lambda (l) (
                           cond
                            ((null? l) '())
                            ((= 1 (modulo (car l) 2)) (cons (car l) (soru5 (cdr l))))
                            (else (soru5 (cdr l)))
                            )))
;;; Bir fonksiyon ve listeyi parametre olarak alan ve verilen fonksiyona göre listeyi filtreleyen fonksiyon
(define soru6 (lambda (f l) (
                             cond
                              ((null? l) '())
                              ((f (car l)) (cons (car l) (soru6 f (cdr l))))
                              (else (soru6 f (cdr l)))
                              )))
;;; Bir listedeki elemanların toplamını kuyruk özyinelemeli olarak hesaplayan fonksiyonu yazınız
(define soru7 (lambda (l sonuc) (
                                 cond
                                  ((null? l) sonuc)
                                  (else (soru7 (cdr l) (+ sonuc (car l))))
                                  )))
;;; Collatz sanısını kuyruk özyinelemeli olarak bulan fonksiyon(1'e ulaşmak için gereken adım sayısı)
;;; tek ise 3n+1, çift ise n/2
(define soru8 (lambda (n sonuc) (
                                 cond
                                  ((= n 1) sonuc)
                                  ((= 0 (modulo n 2)) (soru8 (/ n 2) (+ sonuc 1)))
                                  (else (soru8 (+ 1 (* 3 n)) (+ sonuc 1)))
                                  )))

;;; https://github.com/sevketcakir/proglang2021/