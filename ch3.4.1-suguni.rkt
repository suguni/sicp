;; 3.4.1 병행성

;; ex 3.38
;; a.
;; 시작 100
;; peter +10
;; paul  -20
;; marry /2
;; peter > paul > marry = (100 + 10 - 20) / 2 = 45
;; peter > marry > paul = (100 + 10) / 2 - 20 = 35
;; paul > peter > marry = (100 - 20 + 10) / 2 = 45
;; paul > marry > peter = (100 - 20) / 2 + 10 = 50
;; marry > peter > paul = 100 / 2 + 10 - 20 =  40
;; marry > paul > peter = 100 / 2 - 20 + 10 = 40
;; 가능한 balance 값들: 35, 40, 45, 50

;; b.
;; 가능한 balance 값 : 90, 120, 50 !

