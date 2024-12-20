#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).
(define (lcp-helper w1 w2 prefix)
  (cond
    ((or (null? w1) (null? w2))
     (cons prefix (cons w1 (cons w2 '()))))
    ((and (pair? w1) (pair? w2) (equal? (car w1) (car w2)))
     (lcp-helper (cdr w1) (cdr w2) (append prefix (cons (car w1) '()))))
    (else
     (cons prefix (cons w1 (cons w2 '()))))))

(define (longest-common-prefix w1 w2)
  (lcp-helper w1 w2 '()))



; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection
(define (lcp-list-helper prefix remaining-words)
    (cond
      ((st-empty? remaining-words)
       prefix)
      (else
       (let* ((first-word (collection-first remaining-words))
              (lcp-result (longest-common-prefix prefix first-word)))
         (if (stream-empty? (collection-first(collection-rest lcp-result)))
             prefix
             (lcp-list-helper (car lcp-result) (collection-rest remaining-words)))))))

(define (longest-common-prefix-of-collection words)
  (if (st-empty? words)
      '()
      (lcp-list-helper (collection-first words) (collection-rest words))))


(define (mpwl-helper st pattern sol)
  (cond
    ((st-empty? st) (list #f sol))
    (else
     (let* ((label (car (collection-first st)))  ;eticheta
            (subtree (cdr (collection-first st)))  ;subarborele
            (cuvinte (longest-common-prefix label pattern)))  ;cautam prefixul comun si restul cuvintelor
       (cond
         ;nu gasim prefix comun
         ((null? (car cuvinte))
          (mpwl-helper (collection-rest st) pattern sol)) ;caut in continuare
         ;gasim exact pattern-ul
         ((equal? (car cuvinte) pattern) #t)
         ;gasim partial pattern-ul
         ((null? (cadr cuvinte))
          ;(cons label (cons (caddr cuvinte) subtree)))
          (list label (caddr cuvinte) subtree)) 
         (else (list #f (car cuvinte))))))))

(define (match-pattern-with-label st pattern)
  (mpwl-helper st pattern '()))


(define (st-has-pattern? st pattern)
  (let ((result (match-pattern-with-label st pattern)))
    (cond
      ((equal? result #t) #t)
      ((equal? (car result) #f) #f)
      (else (st-has-pattern? (caddr result) (cadr result))))))


(define (get-suffixes text)
  (if (not (null? text))
      (collection-cons text (get-suffixes (cdr text)))
      '()))


(define (get-ch-words words ch)
  (collection-filter (lambda (word)
            (and (pair? word)
                 (equal? (car word) ch)))
          words))

(define (ast-func suffixes)
  ;(if (and (pair? suffixes) (pair? (car suffixes)))
      (cons (list (car (collection-first suffixes)))
            (collection-map cdr suffixes)))
     ; '()))


(define (cst-func suffixes)
  (cons (longest-common-prefix-of-collection suffixes)
        (collection-map (lambda (suffix)
               (collection-first(collection-rest(longest-common-prefix suffix (longest-common-prefix-of-collection suffixes)))))
             suffixes)))


; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)
(define (suffixes->st labeling-func suffixes alphabet)
  (define (aux ch)
    (if (collection-empty? suffixes)
        empty-collection
  (let ((ch_suffixes (get-ch-words suffixes ch)))  ;sufixele care incep cu characterul "ch"
       (if (not (st-empty? ch_suffixes))
           (let* ((label (car (labeling-func ch_suffixes)))  ; eticheta unei ramuri
                  (new_suffixes (cdr (labeling-func ch_suffixes))))  ; subarborele
            (cons label (suffixes->st labeling-func new_suffixes alphabet)))
           '()))))
     (collection-filter (lambda (x) (not (null? x)))
             (collection-map aux alphabet)))


; nu uitați să convertiți alfabetul într-un flux
(define text->st
  (lambda (text)
    (lambda (labeling-func)
      (let ((suffixes (get-suffixes (append text (list #\$))))
             (alphabet (stream->list (sort (remove-duplicates (append text (list #\$))) char<?))))
        (suffixes->st labeling-func suffixes alphabet)))))


(define text->ast 
  (lambda (text) ((text->st text ) ast-func)))


(define text->cst
(lambda (text) ((text->st text ) cst-func)))


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (substring? text pattern)
  (let ((arb (text->ast text)))
    (st-has-pattern? arb pattern)))

; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (repeated-substring-of-given-length text len)
  (let search((st (text->cst text))
              (pattern '()))
    (cond
      ((null? st) #f)
      ((>= (length pattern) len)
       (take pattern len))
      (else
       (let* ((label (get-branch-label (first-branch st)))
              (match (get-ch-branch st (car label))))
         (or (and match
                  (search (get-branch-subtree match) 
                          (append pattern label)))
             (search (other-branches st) pattern)))))))