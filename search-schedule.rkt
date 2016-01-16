#lang racket

(require "../common/uw-api.rkt")

;; INTEGRITY STATEMENT (modify if neccessary)
;; I received help from the following sources:
;; None. I am the sole author of this work 

;; sign this statement by removing the line below and entering your name

;; Name:
;; login ID:

;;;;;;;;;;;;;;;;
;; INTERFACE: ;;
;;;;;;;;;;;;;;;;
(provide get-prof search req course subject)
;;(get-prof subject catalog) provides all the profs of a specific subject
;;described as subject+catalog
;;(get-prof Str Str -> (listof (list Str))


;;;;;;;;;;;;;;;;;;;;;
;; IMPLEMENTATION: ;;
;;;;;;;;;;;;;;;;;;;;;

;;A Dictionary is a (list Any Any)
;;An API-List is a (listof Dicitonary)

(struct req (starts finish course) #:transparent)
;;A Requirements is a (req Str Str Course )

(struct course (subjectlst) #:transparent)
;;A Course is a (couse (listof Subject))

(struct subject (subject catalog section professor hr day) #:transparent)
;;A subject is (subject Str Str Str Str (list Str Str) (listof Char))
(define test
  (req "09:30" "14:30"
       (course (list ;;(subject "MATH" "136" empty empty '(1030 1120) empty)
                     (subject "CS" "136" empty '("Kharal,Rosina") empty empty)
                     ;;(subject "BIOL" "239" empty empty empty empty)
                     (subject "EMLS" "102R" empty empty '(1300 1420) empty)
                     ;;(subject "MATH" "138" empty empty empty empty)
                     ))))

(struct state (subjects))
;;A State is a (state(listof Subjects))


;;helper functions:

;;(get-item lst targ) finds the value of a given targ inside lst
;;get-item: (listof (list Str Any)) Str -> Any
(define (get-item lst targ)
  (cond
    [(empty? lst) #f]
    [(string=? (car (car lst)) targ) (cadr (car lst))]
    [else (get-item (cdr lst) targ)]))

;;(char->num char) convert a char into an int
;;char->num: Char -> Int
(define (char->num char)
  (local [(struct char-bst (val left right))
          ;; a Char-BST is a 
          ;;(char-bst Node Node Node)
          ;; a Node is oneof:
          ;;* Char-BST
          ;;(list char num)
          ;; empty
          
          (define dic 
            (char-bst (list #\5 5)
                      (char-bst
                       (list #\3 3)
                       (char-bst
                        (list #\1 1)
                        (list #\0 0)
                        (list #\2 2))
                       (list #\4 4))
                      (char-bst
                       (list #\7 7)
                       (list #\6 6)
                       (char-bst 
                        (list #\8 8)
                        empty
                        (list #\9 9)))))
          ;;(get-num ch diction) gets the corresponding number
          ;;  of the ch from a diction
          ;;get-num: Char Char-BST -> Num
          (define (get-num ch diction)
            (cond
              [(empty? diction) #f]
              [(cons? diction) (cadr diction)]
              [(char=? ch (car (char-bst-val diction)))
               (cadr (char-bst-val diction))]
              [(char>? ch (car (char-bst-val diction)))
               (get-num ch (char-bst-right diction))]
              [else (get-num ch (char-bst-left diction))]))]
    (get-num char dic)))
;;(time-convert time) converts a time into digit
;;time-convert: Str -> Int
(define (time-convert time)
  (cond
    [(empty? time) 0]
    [else
     (local [(define timelst (string->list time))]
       (cond
         [(string=? time "null") #f]
         [else
          (+ (* 1000 (char->num (car timelst)))
             (* 100 (char->num (cadr timelst)))
             (* 10 (char->num (cadr (cdr (cdr timelst)))))
             (char->num (cadr (cdr (cdr (cdr timelst))))))]))]))
;;(get-days days acc store) produces the weekdays in days, acc is part
;;  of the final product, store stores part of acc
;;get-days: (listof Char) (listof Str) (listof Char) -> (listof Str)
(define (get-days days acc store)
  (cond
    [(empty? days) (append acc (list (list->string store)))]
    [(char=? #\n (car days)) #f]
    [(and (empty? store)
          (char-upper-case? (car days)))
     (get-days (cdr days) acc (list (car days)))]
    [(char-upper-case? (car days))
     (get-days (cdr days)
               (append acc
                       (list (list->string store))) (list (car days)))]
    [else (get-days (cdr days) acc (append store (list (car days))))]))
;;(get-subject api-lst) gets all subjects in api-lst
;;get-subject: API-List -> (listof subjects)
(define (get-subject api-lst)
  (local [;;(get-itemlst datalst itemlst) searches all elments in itemlst
          ;;  and produces them from datalst
          ;;get-itemlst: API-List (listof Str) ->  (listof Dictionary) 
          (define (get-itemlst datalst itemlst)
            (foldr (lambda (x y)
                     (cond
                       [(member (car x) y)
                        (cons  x (remove (car x) y))]
                       [else y]))
                   itemlst datalst))
          (define datalst
            (cond
              [api-lst
               (map (lambda (x)
                      (append
                       (append
                        (get-itemlst (cadr (car (car (get-item x "classes"))))
                                     (list "start_time" "end_time" "weekdays"))
                        (list (last (car (get-item x "classes")))))
                       (get-itemlst
                        x
                        (list "subject" "catalog_number" "section"))))
                    api-lst)]
              [else api-lst]))]
    (cond
      [datalst
       (filter
        (lambda (y)
          (and (string=?
                (substring (subject-section y) 0 3)
                "LEC")
               (subject-day y)
               (car (subject-hr y))))
        (map (lambda (x)
               (subject (get-item x "subject")
                        (get-item x "catalog_number")
                        (get-item x "section")
                        (get-item x "instructors")
                        (list
                         (time-convert (get-item x "start_time"))
                         (time-convert (get-item x "end_time")))
                        (get-days (string->list (get-item x "weekdays"))
                                  empty empty))) datalst))]
      [else datalst])))
#|
;;------------------------------------------------------
(define (get-itemlst datalst itemlst)
  (foldr (lambda (x y)
           (cond
             [(member (car x) y)
              (cons  x (remove (car x) y))]
             [else y]))
         itemlst datalst))
(define datalst (get-subject (uw-api "/courses/MATH/136/schedule")))
;;------------------------------------------------------
|#

;;(get-schedule subject catalog) produces the class schedule of
;;  a course described as subject+catalog
;;get-schedule: Str Str -> API-List

(define (get-schedule subject catalog)
  (uw-api (string-append
           "/courses/"
           subject "/"
           catalog "/schedule")))

;;(get-prof subject catalog) provides all the profs of a specific subject
;;described as subject+catalog
;;(get-prof Str Str -> (listof (list Str))
(define (get-prof subject catalog)
  (map subject-professor (get-subject (get-schedule subject catalog))))

;;(val-sbueject? reqsbj targsbj) determines whether targsbj meets
;;  the requirement of reqsbj
;;val-sbuject?: Subject Subject -> Bool
(define (val-subject? reqsbj targsbj)
  (and
   (or (equal? (subject-section reqsbj)
               (subject-section targsbj))
       (empty? (subject-section reqsbj)))
   (or (equal? (subject-professor reqsbj)
               (subject-professor targsbj))
       (empty? (subject-professor reqsbj)))
   (or (equal? (subject-hr reqsbj)
               (subject-hr targsbj))
       (empty? (subject-hr reqsbj)))
   (or (equal? (subject-day reqsbj)
               (subject-day targsbj))
       (empty? (subject-day reqsbj)))))

;;(initialstate req) produces the initial state
;;  according to req
;;initialstate: Req -> (listof Subject)
(define (initialstate req)
  (local [(define api-lst (get-schedule
                           (subject-subject
                            (car (course-subjectlst (req-course req))))
                           (subject-catalog
                            (car (course-subjectlst (req-course req))))))
          (define subjectlst (get-subject api-lst))
          ;;(makelst targlst paralst) builds a list with targlst as the
          ;; first element and a length with paralst
          (define (makelst targlst paralst)
            (cond
              [(not subjectlst)
               (raise-user-error "Please recheck the course name and catalog number")]
              [(empty? (cdr paralst)) targlst]
              [(cons? targlst)
               (makelst (append targlst (list empty)) (cdr paralst))]
              [else (makelst (append (list targlst) (list empty)) (cdr paralst))]))]
    (map (lambda (y)
           (makelst y (course-subjectlst (req-course req))))
         (filter (lambda (x)
                   (val-subject? (car (course-subjectlst (req-course req))) x))
                 subjectlst))))
;;(time-okay? s1 s2 req) determines if s1 and s2 has a time conflict
;;time-okay?: Subject Subject Req-> Bool
(define (time-okay? s1 s2 req)
  (cond
    [(empty? s2) #t]
    [(cons? s2) (foldr (lambda (a b)
                         (and a b)) #t
                                    (map (lambda (x)
                                           (time-okay? s1 x req))
                                         s2))]
    [else
     (and (or (> (car (subject-hr s1))
                 (time-convert (req-starts req)))
              (< (cadr (subject-hr s1))
                 (time-convert (req-finish req))))
          (or (empty? (filter (lambda (x)
                                (member x (subject-day s2)))
                              (subject-day s1)))
              (or (> (car (subject-hr s1))
                     (cadr (subject-hr s2)))
                  (< (cadr (subject-hr s1))
                     (car (subject-hr s2))))))]))
;;(val-neighbour? state req subject) determines wheth(er a subject is
;; a valid neighbour according to state and req
;;val-neighbour?: State Req Subject -> Bool
(define (val-neighbour? state req subject)
  (and (subject-day subject)
       (car (subject-hr subject))
       (foldr (lambda (c d)
                (and c d))
              #t
              (map (lambda (y)
                     (time-okay? subject y req))
                   state))
       (foldr (lambda (a b)
                (and a b))
              #t
              (map (lambda (x)
                     (val-subject? x subject))
                   (course-subjectlst (req-course req))))))

;;(fill-state state subject acc) fills a subject into a state,acc stores
;;  part of the product
;;fill-state: State Subject (listof Subject) -> State
(define (fill-state state subject acc)
  (cond
    [(empty? subject) state]
    [(empty? (car state))
     (fill-state (append acc (list subject) (cdr state)) empty empty)]
    [else (fill-state (cdr state) subject (append (list (car state)) acc))]))

;;(generate-neighbour rq state) generates all possible neighbours
;;  of a state, req stores the requirements
;;generate-neighbour: Req State -> State

(define (generate-neighbour req sta)
  (map (lambda (a)
         (fill-state sta a empty))
       (foldr (lambda (x y)
                (cond
                  [(val-neighbour? sta req x)
                   (cons x y)]
                  [else y]))
              empty
              (get-subject
               (get-schedule
                (subject-subject
                 (car (course-subjectlst (req-course req))))
                (subject-catalog
                 (car (course-subjectlst (req-course req)))))))))

;;(search req statelst) searches the valid courese based on given
;; req and statelst
;;search: Req (listof State) ->
(define (search reqmnt statelst)
  (cond
    [(empty? (course-subjectlst (req-course reqmnt)))
     statelst]
    [(empty? statelst)
     (search (req (req-starts reqmnt)
                  (req-finish reqmnt)
                  (course (cdr (course-subjectlst (req-course reqmnt)))))
             (initialstate reqmnt))]
    [else
     (search (req (req-starts reqmnt)
                  (req-finish reqmnt)
                  (course (cdr (course-subjectlst (req-course reqmnt)))))
             (foldr (lambda (x y)
                      (append (generate-neighbour reqmnt x) y))
                    empty statelst))]))

;;constants used for testing
(define t2 (req (req-starts test)
                (req-finish test)
                (course (cdr (course-subjectlst (req-course test))))))
(define sta (car (initialstate test)))
(define a (car (get-subject
                (get-schedule
                 (subject-subject
                  (car (course-subjectlst (req-course t2))))
                 (subject-catalog
                  (car (course-subjectlst (req-course t2))))))))
(define yong (car sta))
(define t (car (course-subjectlst (req-course t2))))






