(module tests mzscheme
  
  (provide test-list)
  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
      ; ==================== Vector test cases =========================

   (vector-detailed-test-1 "let a = newvector(2, -99) in
                              let p = proc (x)
                                  let v = read-vector(x, 1)
                                  in update-vector(x, 1, -(v, -1))
                       in begin update-vector(a, 1, 0); (p a); (p a); read-vector(a, 1) end"
                      2)

      (vector-detailed-test-2 "let a = newvector(3, 5) in
                              let p = proc (x)
                                   let v = read-vector(x, 1)
                                   in update-vector(x, 1, -(-2, v))
                              in let q = proc(x)
                                  let v1 = read-vector(x, 1) in 
                                  let v2 = read-vector(x, 2)    
                                  in update-vector(x, 1, -(v2, -(0, v1)))
                       in begin update-vector(a, 1, -5); (p a); (q a); read-vector(a, 1) end"
                      8)

      (vector-detailed-test-3 "let a = newvector(2, -99) in
                              let p = proc (x)
                                  let v = read-vector(x, 1)
                                  in update-vector(v, 1, -(read-vector(v, 2), -(-1, read-vector(v, 1))))
                       in begin update-vector(a, 1, newvector(3,4)); (p a); (p a); (p a); read-vector(read-vector(a, 1), 1) end"
                      19)

      (vector-detailed-test-4 "let a = newvector(5, 1) in
                                  begin
                                    update-vector(a, 0, 0); update-vector(a, 1, 1); update-vector(a, 2, 2); update-vector(a, 3, 3);
                                     swap-vector(a, 0, 3);
                                     -(read-vector(a, 0), read-vector(a,1))
                                  end"
                      2)

      (vector-detailed-test-5 "let a = newvector(101, 45) in
                                let b = newvector(56, 34) in
                                    -(length-vector(b), -(0, length-vector(a)))"
                      157)

      (vector-detailed-test-6 "let a = newvector(89, 78) in
                                let b = copy-vector(a) in
                                    length-vector(b)"
                      89)

      (vector-detailed-test-7 "let a = newvector(89, 78) in
                                let b = copy-vector(a) in
                                    begin
                                    update-vector(b, 0, 5); swap-vector(b, 0, 1); -(length-vector(a), -(0, read-vector(b, 1)))
                                    end"
                      94)


        ; ==================== Queue Test Cases =========================;
      (queue-test1 "let x = newqueue(5) in begin enqueue(x, 10); enqueue(x, 20); enqueue(x, 30); queue-size(x) end" 3)
      (queue-test2 "let x = newqueue(6) in begin enqueue(x, 10); enqueue(x, 20); dequeue(x); dequeue(x); enqueue(x, 30); peek-queue(x) end" 30)
      (queue-test3 "let x = newqueue(6) in begin enqueue(x, 10); enqueue(x, 20); enqueue(x,30); dequeue(x); dequeue(x); dequeue(x); queue-empty?(x) end" #t)
      (queue-test4 "let x = newqueue(6) in begin enqueue(x, 10); dequeue(x); enqueue(x, 40); enqueue(x, 20); dequeue(x); peek-queue(x) end" 20)

 ))
  )
