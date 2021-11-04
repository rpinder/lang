# lang
A simple language with lispish syntax. Running the stack project will result in a REPL where you can enter code or load files.

## Simple Examples
```
(+ 2 3) => 5
(fst 1 2) => 1
(snd 1 2) => 2
(if (= 1 1) 3 4) => 3
```

# Examples (examples.lang)
```
(def factorial (fn x
  (if (= x 1)
    1
    (* x (factorial (- x 1))))))

(print (factorial 5)) # 120

(def fib (fn n
  (if (= n 0) 0
    (if (= n 1) 1
      (+ (fib (- n 1)) (fib (- n 2)))))))

(print (fib 5)) # 5

(def ackermann (fn n
  (if (= (fst n) 0)
    (+ (snd n) 1)
    (if (= (snd n) 0)
      (ackermann (- (fst n) 1) 1)
      (ackermann (- (fst n) 1) (ackermann (fst n) (- (snd n) 1)))))))

(print (ackermann 3 3)) # 61

(def map (fn n
  (let f (fst n)
  (let list (snd n)
  (if list
    (cons (f (fst list)) (map f (snd list)))
    nil)))))

(def square (fn x (* x x)))
(print (map square [1 2 3 4 5])) # (1, (4, (9, (16, (25, nil)))))

(def filter (fn n
  (let f (fst n)
  (let list (snd n)
  (if list
    (if (f (fst list))
      (cons (fst list) (filter f (snd list)))
      (filter f (snd list)))
    nil)))))

(def istwo (fn x (= x 2)))
(print (filter istwo [1 2 3 4 5 4 3 2 1 2])) # (2, (2, (2, nil)))

(def reduce (fn x
  (let f (fst x)
  (let base (fst (snd x))
  (let list (snd (snd x))
  (if list
    (f (fst list) (reduce f base (snd list)))
    base))))))

(print (reduce + 0 [1 2 3 4 5])) # 15
       
nil
```
