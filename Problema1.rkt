#lang plai
#|
PROBLEMA 1
La funcion la hago en cinco pasos.
1- Defino los parentesis izquierdo y derecho a través de su caracter para no tener problemas
con los parentesis durante la creación de la funcion.
2- Paso el string dado a una lista
3- Creo una lista que solo contiene los parentesis izquierdos, usando cond y recursividad
4- Creo una lista que solo contiene los parentesis derechos, usando cond y recursividad
5- Comparo la longitud de ambas listas
Para los pasos 3 y 4, como es una funcion recursiva, primero se toma el caso base (lista
vacía), y después comparo si el primer elemento de la lista(con car) es igual a (.
Para el caso recursivo uso cons para tener la lista menos el primer elemento y uso la funcion
de nuevo sobre la lista menos el primer elemento usando cdr. Se hizo de manera análoga para
')'.
|#
;; parentesis-balanceados? : String -> Boolean

(define (parentesis-balanceados? s)
  (begin
    (define parentesis-izq (integer->char 40)) ;;Paso 1 para (
    (define parentesis-der (integer->char 41)) ;; Paso 1 para )
    (define (ls-elementos-s s)
      (string->list s)) ;;Paso 2
    (define (obtener-parentesis-izq ls-elementos-s)
      (cond
        [(null? ls-elementos-s) '()]
        [(eq? (car ls-elementos-s) parentesis-izq)
         (cons parentesis-izq (obtener-parentesis-izq (cdr ls-elementos-s)))]
        [else (obtener-parentesis-izq (cdr ls-elementos-s))])) ;;Paso 3
    (define (obtener-parentesis-der ls-elementos-s)
      (cond
        [(null? ls-elementos-s) '()]
        [(eq? (car ls-elementos-s) parentesis-der)
         (cons parentesis-der (obtener-parentesis-der (cdr ls-elementos-s)))]
        [else (obtener-parentesis-der (cdr ls-elementos-s))]))  ;; Paso 4
    (define (comparacion-izq-der obtener-parentesis-izq obtener-parentesis-der)
      (cond
        [(= (length obtener-parentesis-izq) (length obtener-parentesis-der)) #t]
        [else #f])) ;;Paso 5
    (comparacion-izq-der
     (obtener-parentesis-izq (ls-elementos-s s))
     (obtener-parentesis-der (ls-elementos-s s))))) ;;Hago la comparación

#|
PROBLEMA 2
Será algo parecido al problema anterior, pero esta vez quiero comparar la longitud
de la lista solo con las vocales y otra lista con todo el string. Lo haré por pasos
1- Paso el string a una lista
2- Creo una lista solo con las vocales de la lista anterior
3- Comparo ambas listas, pero tomo la mitad de la primera longitud para saber si
predominan las vocales o no.
|#
;; vocal-predominante? : String-> Boolean

(define (vocal-predominante? s)
  (begin
    (define (ls-elementos-s s)
      (string->list s)) ;;Paso 1, mismo paso 2 que el ejercicio anterior.
    (define (obtener-vocales ls-elementos-s)
      (cond
        [(null? ls-elementos-s) '()]
        [(eq? (car ls-elementos-s) #\a )
         (cons 'a (obtener-vocales (cdr ls-elementos-s)))] ;para a
        [(eq? (car ls-elementos-s) #\e )
         (cons 'e (obtener-vocales (cdr ls-elementos-s)))] ;;para e
        [(eq? (car ls-elementos-s) #\i )
         (cons 'i (obtener-vocales (cdr ls-elementos-s)))] ;;para i
        [(eq? (car ls-elementos-s) #\o )
         (cons 'o (obtener-vocales (cdr ls-elementos-s)))] ;;para o
        [(eq? (car ls-elementos-s) #\u )
         (cons 'u (obtener-vocales (cdr ls-elementos-s)))] ;;para u
        [else (obtener-vocales (cdr ls-elementos-s))])) ;;Paso 2, caso recursivo
    (define (comparacion-ls obtener-vocales ls-elementos-s)
      (cond
        [(> (length obtener-vocales) (/ (length ls-elementos-s) 2)) #t]
        [else #f])) ;;Paso 3
    (comparacion-ls
     (obtener-vocales (ls-elementos-s s))
     ( ls-elementos-s s)))) ;;Hago la comparación

#|
PROBLEMA 3
Esta vez no definimos una funcion para pasar de cadena a lista, ahora definimos
directamente dos listas que estan dadas por cada cadena.
Hacemos una relación uno a uno, creando una lista con dos elementos correspondientes
a los primeros, segundos, terceros, etc... elementos de cada string, guardándolos en una
lista, teniendo asi una lista de sublistas con dos elementos cada una.
La idea ahora es comparar el primer elemento de cada sublista con el siguiente primer
elemento de la siguiente sublista. Si son iguales, se compara el segundo elemento. Si vuelven a
ser iguales, se guarda #t en una nueva lista. En caso contrario, se guarda #f.
Seguimos así para la segunda sublista y asi sucesivamente
Después se hace lo mismo, pero cambiando el orden de los string para comparar ahora los
primeros elementos del segundo string. La lista con los booleanos obtenidos se añade a la lista
del caso anterior.
Al final se analiza esta lista, donde si tiene un solo booleano #f es suficiente para decir que
los strings no son isomorfos. En caso contrario, regresa #t.
|#
;; cadena-isomorfa? : String String -> Boolean
(define (cadena-isomorfa? s1 s2)
  (let* ((ls1-real (string->list s1))
         (ls2-real (string->list s2)))
    
    (define (relaciones ls1 ls2) ;;relaciono elementos correspondientes de cada string/lista
      (cond
        [(and (null? ls1) (null? ls2)) '()] ;; Caso base 1: ambas listas están vacías
        [(or (null? ls1) (null? ls2)) #f] ;; Caso base 2: una de las dos está vacía
        [else (cons (list (car ls1) (car ls2)) (relaciones (cdr ls1) (cdr ls2)))])) ;; Emparejar elementos de ambos strings
    (define relaciones-izq (relaciones ls1-real ls2-real))
    (define relaciones-der (relaciones ls2-real ls1-real))
    ;; Comparar relaciones
    (define (comparar-relaciones ls)

      (define (comparar-pares subls1 subls) ;;funcion para comparar el primer elemento de dos sublistas
        (cond
          [(null? subls) #t] ;; Caso base: no hay más sublistas, devuelve #t
          [(not (eq? (car subls1) (car (car subls)))) #t] ;; Si el primer elemento es diferente, devuelve #t
          [(and (eq? (car subls1) (car (car subls))) ;; Si el primer elemento es igual
                (not (eq? (cadr subls1) (cadr (car subls))))) ;; y el segundo es diferente, devuelve #f
           #f]
          [else (comparar-pares subls1 (cdr subls))])) ;; Continuar con las siguientes sublistas

      (define (comparar-subls ls) ;;caso recursivo
        (cond
          [(null? ls) empty] ;; Caso base: la lista está vacía
          [else (cons (comparar-pares (car ls) (cdr ls))
                      (comparar-subls (cdr ls)))])) ;; Continuar con el resto de la lista
      (define comparacion-izq (comparar-subls relaciones-izq))
      (define comparacion-der (comparar-subls relaciones-der))
      (define comparacion-total (append comparacion-izq comparacion-der))
      comparacion-total) ;; Devolver el resultado de la comparación
    (if (memq #f (comparar-relaciones relaciones-izq)) ;;comparación de relaciones, lista de booleanos
        #f
        #t)))

#|
PROBLEMA 4
|#
;; automorfico?: number-> boolean
(define (automorfico? n)
  (begin
        (define (cuadrado n) (* n n)) ;;defino la operación de elevar al cuadrado
        (define ultimo-digito (modulo (cuadrado n) 10)) ;;modulo devuelve el último digito
        (if (= n ultimo-digito) ;;comparo el numero n con el ultimo digito de n^2
            #t
            #f)))

#|
PROBLEMA 5
|#
;;clasificar-num : (listof number -> (listof String)
(define (clasificar-num ls)
  (cond
    [(null? ls) '()] ;;caso base
    [(= (car ls) 0)
     (cons "Cero" (clasificar-num (cdr ls)))] ;;caso si es = 0
    [(> (car ls) 0)
     (cons "Positivo" (clasificar-num (cdr ls)))] ;;caso positivo
    [else 
     (cons "Negativo" (clasificar-num (cdr ls)))])) ;;solo queda caso negativo

#|
PROBLEMA 6
Supongamos que se trata de un dado de 20 caras, con valores posibles del 1 al 20. Por lo que
n debe estar entre esos valores.
|#
;; simulador-dado : number -> (list of number)
(define (simulador-dado n)
  (define (tirar-dado-20-caras) (+ 1 (random 20))) ; Genera un número aleatorio entre 1 y 20
  (define (recursion tiradas) ;;para poder hacer la recursión más adelante
    (let ((tirada (tirar-dado-20-caras))) ;;defino tirada para usarlo en el if
      (if (= tirada n) ;;no se puede usar tirar-dado-20... porque es un proceso, no un numero
          (reverse (cons tirada tiradas)) ; Devuelve la lista de tiradas incluyendo la última
          (recursion (cons tirada tiradas))))) ; Sigue tirando el dado y guarda la tirada
  (recursion '())) ; Inicia la recursión con una lista vacía



    
  
       
    