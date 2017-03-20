;Dominique Owens, Elle Zadina

;programming project: interpreter
;--------------------------------------
;fixed the state vars stuff so
;need to make an if and while filter for ifs whiles that can contain a throw in try catch stmnt
;    - this if will take the catch info and the final block info
;    - if a throw is encountered in this if, a 
;
;You need to use continuations to properly implement return, break, continue, and throw.
;      - im gonna use call\cc and letrec

;still need to test after all the 
;--------------------------------------
(load "simpleParser.scm")

;this method takes a file name and returns the evaluate file (int, true, false, or error)
(define interpret
  (lambda (fileName)
    (eval-prgm (parser fileName) '((true false) (#t #f)))
     ))

;this method takes the parse tree and a state
;calling eval-prgm recursively until hitting return stmt
(define eval-prgm
  (lambda (prgm state)
    (cond
      ((not (list? state)) (checkForBoolean state))
       ;if declaring a variable
       ((eq? (caar prgm) 'var) (eval-prgm (cdr prgm) (declareState (car prgm) state)))
       ;if assign
       ((eq? (caar prgm) '=) (eval-prgm (cdr prgm) (assignState (car prgm) state)))
       ;if code block
       ((eq? (caar prgm) 'begin) (eval-prgm (cdr prgm) (blockStmt (cdr (car prgm)) (addStateLayer state))))
       ;if if statment                                           
       ((eq? (caar prgm) 'if) (eval-prgm (cdr prgm) (ifFilter (car prgm) state)))
       ;if while                                                 
       ((eq? (caar prgm) 'while) (eval-prgm (cdr prgm) (whileStmt (cadr (car prgm)) (caddr (car prgm)) state)))
       ;if there is a try block
       ;if there is a try catch in the loop                       trybody      catchstmt     finallystmt 
       ;((eq? (car body) 'try) (whileStmt condition body (tryStmt (cadr body) (caddr body) (cadr (cadddr body)) state)))
       ((eq? (caar prgm) 'try) (eval-prgm (cdr prgm) (tryStmt (cadr (car prgm)) (caddr (car prgm)) (cadddr (car prgm)) state)))
       ;if return
       ((eq? (caar prgm) 'return) (returnVal (car prgm) state))
   (else (error "unknown or illegal syntax: " (car prgm)))
)))

;return takes an expression (car of program) and a state
;and returns either (int true false)
(define returnVal 
  (lambda (expr state)
    (cond
      ;if the expression is a throw
      ((eq? (car expr) 'throw) (makeState expr state))
      ;if the experssion is a break
      ((eq? (car expr) 'break) (makeState expr state))
      ;if the expresion is a list
      ((list? (cadr expr)) (checkForBoolean (expr_eval* (cadr expr) state)))
      ;if the expresion is a number
      ((number? (cadr expr)) (checkForBoolean (cadr expr)))
      ;if the expression is a variable- these include true and false
      ((member*? (cadr expr) (car state)) (checkForBoolean (getVarValue (cadr expr) (car state) (cadr state))))
      ;
      (else (error "invalid return value: " (cadr expr)))
      )))

;the method filters the return data to make sure that booleans are turned into 'true and 'false
(define checkForBoolean
  (lambda (value)
    (cond
      ;if value is a number
      ((number? value) value)
      ;if value is #t
      ((eq? value #t) 'true)
      ;if value is #f
      ((eq? value #f) 'false)
      ;else there is an error: there is no value 
      (else (error "invalid return value: " value))
      )))

;returns true if a is a member of list l
(define member*?
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((list? (car l)) (or (member*? a (car l)) (member*? a (cdr l))))
      ((eq? (car l) a) #t)
      (else (member*? a (cdr l)))
      )))

;declareState takes a expression and a state and updates the state for the initialization of a variable
;returns a state
(define declareState 
  (lambda (l state)
     (cond
       ;if the variable has already been declared 
       ((member*? (cadr l) (car state)) (error "this variable has already been declared: " (cadr l)))
       ;if the decalaration has two parts
       ((eq? (length l) 3)
        (cond
          ;if the declaration is a list
          ((list? (caddr l))  (updateState (cadr l) (expr_eval* (caddr l) state) state))
          ;if the declaration is a number
          ((number? (caddr l))  (updateState (cadr l) (caddr l) state))
          ;if the declaration is true
          ((eq? (caddr l) 'true) (updateState (cadr l) #t state))
          ;if the declaration is false
          ((eq? (caddr l) 'false)  (updateState (cadr l) #f state))
          ;else assume its a variable 
          (else (updateState (cadr l) (getVarValue (caddr l) (car state) (cadr state)) state))))
       ;else there is no value 
       (else (updateState (cadr l) 'noVal state))
      )))
      

;assign takes a variable declaration and adds it to the state
;returns a state
(define assignState
  (lambda (l state)
    (cond
      ;if the assinged var is not already a declared var
      ((not (member*? (cadr l) (car state))) (error "this variable has not been defined: " (cadr l)))
      ;if the assignment is to a list
      ((list? (caddr l))  
       (cond       
         ;list is another assinemnt         
         ((eq? (car (caddr l)) '=) (updateState (cadr l) (getVarValue (cadr (caddr l)) (car (assignState (caddr l) state)) (cadr (assignState (caddr l) state))) (assignState (caddr l) state) ))
         ;else the list is an expresion 
         (else (updateState (cadr l) (expr_eval* (caddr l) state) state))))
      ;if we are assigning a number to a var
      ((number? (caddr l)) (updateState (cadr l) (caddr l) state))
      ; if we are assigning a var to a var
      ((member*? (caddr l) (car state)) (updateState (cadr l) (getVarValue (caddr l) (car state) (cadr state)) state))
      ;else invalid assignment 
      (else (error "invalid assignment value: " (cdr l)))
      )))

;epression_eval take an expression and evaluates it to either a number or a boolean
(define expr_eval*
  (lambda (expr state)
      (cond
        ((eq? expr 'false) #f)
        ((eq? expr 'true) #t)
        ;if the expression only has an operator and a value/expression 
        ((eq? (length expr) 2)
         (cond
           ;if the second element is a list
           ((list? (cadr expr)) (singleValueEval (car expr) (expr_eval* (cadr expr) state)))
           ;if the second element is a number 
           ((number? (cadr expr))  (singleValueEval (car expr) (cadr expr)))
           ;else it must be a varaible 
           (else (singleValueEval (car expr) (getVarValue (cadr expr) (car state) (cadr state))))
           ))
        
        ;both lists
        ((and (list? (caddr expr)) (list? (cadr expr)))
         (evaluate (car expr) (expr_eval* (cadr expr) state)  (expr_eval* (caddr expr) state)))
        
        ;second list
        ((list? (caddr expr))
         (cond
            ;check for first being a number or a boolean 
           ((or (boolean? (cadr expr)) (number? (cadr expr))) (evaluate (car expr) (cadr expr) (expr_eval* (caddr expr) state)))

           (else
            (cond

              ;assign the value to get a new state, and then get the value of the asked about variable 
              ((eq? (car expr) '=) (getVarValue (cadr expr) (car (assignState expr state)) (cadr (assignState expr state))))
              ;
              (else (evaluate (car expr)  (getVarValue (cadr expr) (car state) (cadr state)) (expr_eval* (caddr expr) state)))))))
        
        ;first list
        ((list? (cadr expr))
         (cond
            ;check for second being a number or a boolean 
           ((or (boolean? (caddr expr)) (number? (caddr expr))) (evaluate (car expr) (expr_eval* (cadr expr) state) (caddr expr)))
            ;else it should be a var
           (else (evaluate (car expr) (expr_eval* (cadr expr) state) (getVarValue (caddr expr) (car state) (cadr state))))
           ))
        
        ;both are numbers or both are boolean 
        ((or (and (boolean? (cadr expr)) (boolean? (caddr expr))) (and (number? (cadr expr)) (number? (caddr expr))))
         (evaluate (car expr) (cadr expr) (caddr expr)))
        
        ;second is number; first is either variable or boolean 
        ((number? (caddr expr))
         (cond
           ;check for boolean
           ((boolean? (cadr expr)) (error "cannot compare numbers and booleans :" expr))
           ;else is var
           (else (evaluate (car expr)  (getVarValue (cadr expr) (car state) (cadr state)) (caddr expr)))
           ))
         
        ;first is number; second is either variable or boolean
        ((number? (cadr expr))
         (cond
           ;check for boolean
           ((boolean? (caddr expr)) (error "cannot compare numbers and booleans :" expr))
           ;else is var
           (else (evaluate (car expr)  (cadr expr) (getVarValue (caddr expr) (car state) (cadr state))))
           ))
        
        ;first is boolean; assume second is var
        ((boolean? (cadr expr))
         (cond
           ((boolean? (getVarValue (caddr expr) (car state) (cadr state))) (evaluate (car expr)  (cadr expr) (getVarValue (caddr expr) (car state) (cadr state)))) 
           (else (error "cannot compare numbers and booleans :" expr))))
         
        ;second is boolean; assume first is var
        ((boolean? (caddr expr))
         (cond
           ((boolean? (getVarValue (cadr expr) (car state) (cadr state))) (evaluate (car expr) (getVarValue (cadr expr) (car state) (cadr state))  (caddr expr) )) 
           (else (error "cannot compare numbers and booleans :" expr)))
         )
      ;else assume both are variables 
      (else (evaluate (car expr) (getVarValue (cadr expr) (car state) (cadr state))  (getVarValue (caddr expr) (car state) (cadr state))))
      )))


;takes an operator and a single expression - either ! or -
;returns a number or a boolean 
(define singleValueEval
  (lambda (operator value)
    (cond
      ((eq? operator '!) (not value))
      ((eq? operator '-) (* -1 value))
      (else(error " invalid operator:" operator ))
      )))


;takes an operator and two expressions
;returns a number or a boolean 
(define evaluate
  (lambda (operator value1 value2)
    (cond
      ((eq? operator '+) (+ value1 value2))
      ((eq? operator '*) (* value1 value2))
      ((eq? operator '-) (- value1 value2))
      ((eq? operator '/) (quotient value1 value2))
      ((eq? operator '%) (modulo value1 value2))
      ((eq? operator '==) (eq? value1 value2))
      ((eq? operator '!=) (not (eq? value1 value2)))
      ((eq? operator '>=) (>= value1 value2))
      ((eq? operator '<=) (<= value1 value2))
      ((eq? operator '>) (> value1 value2))
      ((eq? operator '<) (< value1 value2))
      ((eq? operator '&&) (and value1 value2))
      ((eq? operator '||) (or value1 value2))
      (else (error " invalid operator:" operator ))
      )))

;a function that filers a if stmnt so that it will send the proper format to my ifSmnt function
(define ifFilter
  (lambda (ifstmt state)
    (cond
     ;if the if list only has one statment
     ((eq? (length ifstmt) 3) (ifStmt (cadr ifstmt) (caddr ifstmt) '--- state))
     ;if the if statement has a return
     ;((member*? 'return ifstmt) (ifStmt (cadr (car prgm)) (caddr (car prgm)) (cadddr (car prgm)) state))
     ;((eq? (car (caddr (car prgm))) 'return) (ifStmt (cadr (car prgm)) (caddr (car prgm)) (cadddr (car prgm)) state))
     ;else return state to eval-prgm
     (else (ifStmt (cadr ifstmt) (caddr ifstmt) (cadddr ifstmt) state))
    )))

;takes a conditoon, two statments, and state
;returns either a state or a value
 (define ifStmt
  (lambda (condition stmt1 stmt2 state)
    (cond
      ;if condition is true          update state with stmt1
      ((expr_eval* condition state) (eval_ifStmt stmt1 state))
      ;else update state with stmt2
      (else (eval_ifStmt stmt2 state))
      )))

;this evaluates the if stmt and breaks if a return is encountered in the if
;;
(define ret (lambda (v) v))

;;
(define eval_ifStmt
  (lambda (stmt state)
    (call/cc (lambda (ret)
               (letrec ((helper1 (lambda (expr)
                                  (cond
                                    ;check for single statement char
                                    ((eq? expr '---) state)
                                    ;check for assign 
                                    ((eq? (car expr) '=) (assignState expr state))
                                    ;check for return or throw
                                    ((or (eq? (car expr) 'return ) (eq? (car expr) 'throw)) (ret (returnVal expr state)))
                                    ;check for if
                                    ((eq? (car expr) 'if) (ifFilter expr state))
;-----------------------------------check for thow
                                    ;for some reason the returnVal is not working!!!!!!!!!!
                                    ;((eq? (car expr) 'throw) (returnVal expr state))
                                    ;check for while 
                                    ((eq? (car expr) 'while) (whileStmt (cadr expr) (caddr expr) state))
                                    ;check for continue 
                                    ((eq? (car expr) 'continue) state)
                                    ;check for break
                                    ((eq? (car expr) 'break) (ret (returnVal expr state)))
                                    ;check for code block
                                    ((eq? (car expr) 'begin) (blockStmt (cdr expr) (addStateLayer state)))
                                    ;
                                    (else (error "invalid statment in if statement: " stmt)) ))))
                 
         (helper1 stmt))))))





;takes a condition, a body, and a state
;returns a state
;----------------------------------------------------------can contain trys!!!!!
(define whileStmt
  (lambda (condition body state)
    (cond
      ;
      ((number? state) state)
      ;
      ((eq? (caar state) 'break) (removeStateLayer (cadr state)))
      ;
      ;((eq? (caar state) 'throw) error "is this getting reached in while")
       ;if loop condition is true
      ((expr_eval* condition state)
       (cond
         ;if there was a throws encounterd somewhere in this while loop
        ((eq? (caar state) 'throw) state)
       
       ;if there is a if statment in the loop
       ((eq? (car body) 'if) (whileStmt condition body (ifFilter body state)))
       ;if there is a code block in the loop
       ((eq? (car body) 'begin) (whileStmt condition body (blockStmt (cdr body) (addStateLayer state))))
       ;if there is a try catch in the loop                       trybody      catchstmt     finallybody 
       ((eq? (car body) 'try) (whileStmt condition body (tryStmt (cadr body) (caddr body) (cadr (cadddr body)) state)))
       ;if there is an expression in the loop
       ((eq? (car body) '=) (whileStmt condition body (assignState body state)))
       ;else it must be an expression, so evaluate it 
       (else (error "invalid statment in loop: " body))
       ))
      ;else return the current state
      (else state)
      )))
;--------------------------------------------------------------------------------------------------------------------------
;returns an altered state
(define blockStmt
  (lambda (body state)
    (cond
      ;if the state is ever a number, it means that there was a nested return somewhere, and we must return a vlaue 
      ((number? state) state)
      ;if we have previously encountered a break 
      ((eq? (caar state) 'break) state)
      ;if we have previously encountered a throw
      ;((eq? (caar state) 'throw) state)
      ;if the body is null return the current state, but remove the cons'd on layer for the variable and the values 
      ((null? body) (removeStateLayer state)) 
       ;if declaring a variable
       ((eq? (caar body) 'var) (blockStmt (cdr body) (declareState (car body) state)))
       ;if assign
       ((eq? (caar body) '=) (blockStmt (cdr body) (assignState (car body) state)))
       ;()
       ;if if statment                                           
       ((eq? (caar body) 'if) (blockStmt (cdr body) (ifFilter (car body) state)))
       ;if continue 
       ((eq? (caar body) 'continue) state)
       ;if break
       ((eq? (caar body) 'break) (returnVal (car body) state))
       ;if try
       ((eq? (caar body) 'try) (tryStmt (cadr (car body)) (caddr (car body)) (cadddr (car body)) state))
       ;if while                                                 
       ((eq? (caar body) 'while) (blockStmt (cdr body) (whileStmt (cadr (car body)) (caddr (car body)) state)))
       ;if another block of code
       ((eq? (caar body) 'begin) (blockStmt (cdr body) (blockStmt (cadr (car body)) (addStateLayer state))))
       ;if return
       ((eq? (caar body) 'return) (returnVal (car body) state))
       (else (error "unknown or illegal syntax in block of code: " (car body)))
      )))

;
(define addStateLayer
  (lambda (state)
    (makeState (cons '() (car state)) (cons '() (cadr state)))
    ))

;
(define removeStateLayer
  (lambda (state)
    (makeState (cdr (car state)) (cdr (cadr state)))
      ))
;--------------------------------------------------------------------------------------------------------------------------
;((var x) (try ((= x 20) (if (< x 0) (throw 10)) (= x (+ x 5))) (catch (e) ((= x e))) (finally ((= x (+ x 100))))) (return x))
;try can do if, while,assign,declare, but no block code or return!!!- must have throw
(define tryStmt
  (lambda (tryBody catchStmt finStmt state); do this with let rect and call\cc to handle the catch 
    (call/cc (lambda (ret)
               (letrec ((helper (lambda (body cs fb st)
                                  (cond
                                    ;if the state is a number, it means that we have encounterd a
                                    ;throw somewhere in this try block 
                                    ((eq? (caar st) 'throw) (ret (catchFilter (caadr cs) (cadar st) (caddr cs) fb (cadr st) )) )
                                    ;if the try body is null, go to the finally block
                                    ((null? body) (finallyStmt fb st))
                                    ;if there is an if
                                    ((eq? (caar body) 'if) (helper (cdr body) cs fb (ifFilter (car body) st)))
                                    ;if there is a while 
                                    ((eq? (caar body) 'while) (tryStmt (cdr body) cs fb (whileStmt (cadr (car body)) (caddr (car body)) st)))
                                    ;if there is an assignment 
                                    ((eq? (caar body) '=) (tryStmt (cdr body) cs fb (assignState (car body) st)))
                                    ;if there is a variable declaration
                                    ((eq? (caar body) 'var) (tryStmt (cdr body) cs fb (declareState (car body) st)))
                                    ;if there is a throw state
                                    ((eq? (caar body) 'throw) (ret (catchFilter (caadr cs) (cadr (car body)) (caddr cs) fb st)))
                                    (else error "unknonw or illegal syntax in try body: " (car body)) ))))
                 (helper tryBody catchStmt finStmt state))))))


;a filter method that assingns whatever variable we want in the catch to the state with the value from throw
;then calls the catch stmt wiht the body and the new state
(define catchFilter
  (lambda (var value body finStmt state)
    (catchStmt var body finStmt (updateState var value state))))


;this method removes a vaiable and its value from a state
;it returns the state after the removal
(define removeVar; make this work on just the state 
  (lambda (var state)
    (cond
      ;if the state is empty
      ((null? (car state)) '(()()))
      ;if we encounter the variable we want to remove 
      ((eq? (caar state) var) (stateCdr state))
      ;else update state with the current first elemnst
      (else (updateState (caar state) (caadr state) (removeVar var (stateCdr state))))
      )))

;catch can have if, block, assigns - no declares and no thorws!!! - can be empty
(define catchStmt
  (lambda (var body finStmt state)
    (cond
      ((equal? body '(())) (finallyStmt finStmt (removeVar var state)))
      ((null? body) (finallyStmt finStmt (removeVar var state)))
      ((eq? (caar body) 'if) (catchStmt var (cdr body) finStmt (ifFilter (car body) state) ))
      ((eq? (caar body) 'while) (catchStmt var (cdr body) finStmt (whileStmt (cadr (car body)) (caddr (body)) state)))
      ((eq? (caar body) '=) (catchStmt var (cdr body) finStmt (assignState (car body) state)))
      ((eq? (caar body) 'throw) error "is this being reached?")
      (else error "unknonw or illegal syntax in catch body: " (car body))
      )))


;finally can have if, while, block, assigns - and can also be empty - anything else is an error
(define finallyStmt
  (lambda (stmt state)
    (cond
      ((null? stmt) state)
      (else (letrec((helper (lambda (body st)
                              (cond
                                ((null? body) st)
                                ((eq? (caar body) 'while) (helper (cdr body) (whileStmt (cadr (car body)) (caddr (body)) st)))
                                ((eq? (caar body) 'if) (helper (cdr body) (ifFilter (car body) st)))
                                ((eq? (caar body) '=) (helper (cdr body) (assignState (car body) st)))
                                (else error "unknonw or illegal syntax in finally statement: " ) ))))
              (helper (cadr stmt) state)))
      )))

;---------------------------------------------------------------------------------------------------------------------------
;takes the elements of one list and adds them to another list
(define myAppend
  (lambda (l1 l2)
    (cond
      ((null? l1) l2)
      (else (cons (car l1) (myAppend (cdr l1) l2)))
      )))

;this method either adds a variable and a value to the state
;or changes the value of a variable 
(define updateState
  (lambda (var value state)
    (cond
      ((null? (car state)) (makeState (cons var (car state)) (cons value (cadr state))))
      ;if this variable has already been defined - update its value
      ((member*? var (stateVar state)) (updateStateHelper var value state))
      ;if the first element of the state vars is a list, cons this new var onto that list
      ;since this will mean that we are updating a variable in a block of code
      ;cons a ((t e w) g y b) ---> ((a t e w) g y b)
      ((list? (caar state)) (makeState (cons (cons var (caar state)) (cdar state)) (cons (cons value (caadr state)) (cdadr state))))
      ;else cons this variable onto the end of the state 
      (else (makeState (cons var (car state)) (cons value (cadr state))))
      )))

(define stateVar (lambda (state) (car state)))

(define stateValue (lambda (state) (cadr state)))


(define stateCons
  (lambda (var value state)
    (makeState (cons var (stateVar state)) (cons value (stateValue state)))
      ))

(define stateCdr
  (lambda (state)
    (cond
      ((null?  state) '())
      (else (cons (cdr (car state)) (stateCdr (cdr state)))))))

(define hasLists?
  (lambda (l)
    (cond
      ((null? l) #f)
      ((list? (car l)) #t)
      (else (hasLists? (cdr l)))
      )))

(define updateStateHelper
  (lambda (var value state)
    (cond
      ;if the car of the state vars list is a list
      ((list? (car (stateVar state)))
       (cond
         ;if the variable we are looking for is not defined in this list, cons this list onto the result of updating this state on the cdr of the state  
         ((member*? var (cdr (stateVar state))) (stateCons (car (stateVar state)) (car (stateValue state)) (updateStateHelper var value (stateCdr state))) )
         ;else the variable must be in this list, so cons the result of findign the var in this list onto the cdr of the state
         (else (stateCons (car (stateVar state)) (changeValueInState var value (car (stateVar state)) (car (stateValue state))) (stateCdr state)))))
      ;if the state var list contain no other list, just search for and update value in this list ((not (hasLists? (stateVar state))) 
      (else (makeState (stateVar state) (changeValueInState var value (stateVar state) (stateValue state))))
      )))

;this method returns the updates stateValue list 
(define changeValueInState
  (lambda (var value stateVar stateValue)
    (cond
      ((eq? (car stateVar) var) (cons value (cdr stateValue)))
      (else (cons (car stateValue) (changeValueInState var value (cdr stateVar) (cdr stateValue))))
      )))

;this method returns the value for a given variable at a given state
;use ony for evaluatation
(define getVarValue 
  (lambda (var stateVar stateValues)
    (cond
      ;if the stateVar list is null, the desired var has not been declared - so there is an error 
      ((null? stateVar) (error "unknown variable: " var) )
      ;if there is a list in our state varaibles, this measn that we are currently in a code block
      ;we need to check to see if the var we are looking for has been defined in the code block
      ;or is defiend outside the code block
      ((list? (car stateVar))
       (cond
         ;if the variable is defiend in the code block 
         ((member*? var (car stateVar)) (getVarValue var (car stateVar) (car stateValues)))
         ;else the variable might be defiend else where 
         (else (getVarValue var (cdr stateVar) (cdr stateValues)))
         )
       )
      ;if we have found the desired var in the list of stateVars
      ((eq? var (car stateVar))
       (cond
         ;if the value of the var is noVal - ther is an error
         ((eq? (car stateValues) 'noVal) (error "this variable has no value: " (car stateVar) ))
         ;else return the value
         (else(car stateValues))))
      ;else recursivly search for the desired var
      (else (getVarValue var (cdr stateVar) (cdr stateValues)))
      )))

;this method makes a state given two lists    
(define makeState 
  (lambda (varList valueList)
    ( cons varList (cons valueList '()))
    ))
