# OOΛ - Object-Oriented LISP

## Membri gruppo del progetto

**Bellucci Edoardo** 899795

**Cattaneo Francesco** 900411

**Dongiovanni Federico** 865774


## Specifica del progetto
Il progetto consiste nella costruzione di un’estensione
“object oriented” di Common Lisp, chiamata OOΛ.

## Funzioni
### def-class
`def-class` è utilizzato per creare classi e prende come argomenti:
- **`class-name`** : il nome della classe
- **`parents`** : le classi genitore (nil se non sono presenti)
- **`parts`** : le parti, che possono essere dei field e/o dei metodi 
(opzionali)
    - *`fields`* : i campi sono definiti con: nome, valore, tipo 
    (opzionale)
    - *`methods`* : i metodi sono definiti con: nome, argomenti 
    (nome dell'istanza più altri opzionali), corpo del metodo

Esempio di utilizzo:  

```lisp
(def-class ’person nil
    ’(fields
        (name "undefined")
        (birthyear 0 integer))
    '(methods (talk (&optional (out *standard-output*))
        (format out "My name is ~A~%I was born in the year ~D~%"
            (field this ’name)
            (field this 'birthyear)))))
```

___

### make
`make` è utilizzato per creare istanze a partire da classi. 
Vengono ereditati sia i metodi che i field della classe 
da cui l'istanza è creata, e tutte le sue classi genitore.

Viene usata in congiunzione con `defparameter` 
per creare una variabile che rappresenta l'istanza.

Prende come argomenti:
- **`class-name`** : il nome della classe da cui viene creata
- **`[<field-name> <value>]`** : la definizione dei field
 (`field-name field-value`, opzionale)

Esempio di utilizzo:
```lisp
(defparameter matt 
    (make 'person 'name "Matt" 'birthyear 1954))
```
___

### is-class
is-class restituisce `T` se l'atomo passatogli è il nome di una classe.

Esempio di utilizzo:
```lisp
(is-class 'person)
; restituisce T
```
___

### is-instance
`is-instance` restituisce `T` se l'oggetto passatogli è l'istanza di una classe. 

Prende come argomenti:
- **`instance-name`** : il nome dell'istanza
- **`class-name`** : che può essere il nome della classe o T (opzionale)
    - se è il nome di una classe, `instance-name` deve essere 
    un'istanza di una classe che ha `class-name` come superclasse
    - se è `T`, allora basta che `instance-name` sia un'istanza qualunque

Esempio di utilizzo:
```lisp
(is-instance matt)
; restituisce T

(is-instance matt 'person)
; restituisce T

(is-instance 'akira-toriyama 'person)
; restituisce NIL

(is-instance matt 'a)
; restituisce NIL
```
___

### field
`field` estrae il valore di un campo da un'istanza.

Prende come argomenti:
- **`instance-name`** : il nome dell'istanza
- **`field-name`** : il nome del field da cui estrarre il valore

Esempio di utilizzo:
```lisp
(field matt 'name)
"Matt"

(field matt 'surname)
Error: Field not found.
```

___

### field*
`field*` estrae il valore da un'istanza percorrendo una serie di attributi. 

Prende come argomenti:
- **`instance-name`** : il nome dell'istanza
- **`field-series`** : uno o più fields

Esempio di utilizzo:
```lisp
;(field matt 'person) è un'istanza 
;e a seguire, (field (...) 'name) sarà a sua volta un'istanza
(field (field (field matt 'person) 'name) 'length)
; corrisponde a scrivere:
(field* matt 'person 'name 'length)
```

## Funzioni ausiliarie importanti

### process-method
`process-method` processa il metodo, in modo che sia possibile 
chiamarlo da terminale come una qualsiasi funzione.

Implementa anche l'utilizzo della keyword `this`, 
comune nei linguaggi ad oggetti. 

Viene usato nel `make`, passandogli come argomento 
la lista dei metodi dell'istanza non ancora processata. 
___

### aux-parents-fields, aux-parents-methods
`aux-parents-fields` e `aux-parents-methods` vengono usati per 
implementare l'ereditarietà, comune nei linguaggi ad oggetti. 

Vengono usati entrambi nel `make`, per andare automaticamente a riprendere 
i field/metodi nelle superclassi evitando ripetizioni. 
___

### make-parts-duo
`make-parts-duo` viene usato per creare la struttura dati delle parti, 
dopo aver processato ciascun field e method 
(N.B. : i metodi non vengono ancora definiti con `defun` in questo punto).