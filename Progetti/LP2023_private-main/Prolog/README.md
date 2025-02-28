# OOΠ - Object-Oriented Prolog

## Membri gruppo del progetto

**Bellucci Edoardo** 899795

**Cattaneo Francesco** 900411

**Dongiovanni Federico** 865774


## Specifica del progetto
Il progetto consiste nella costruzione di un’estensione
“object oriented” di Prolog, chiamata OOΠ.

## Funzioni
### def-class
`def-class` è utilizzato per creare classi e prende come argomenti:
- **`class-name`** : il nome della classe
- **`parents`** : le classi genitore, messe all'interno di una lista
 (`[]` se non sono presenti)
- **`parts`** : le parti, che possono essere dei field e/o dei metodi. 
(opzionali)
    - *`fields`* : i campi sono definiti con: nome, valore, tipo (opzionale)
    - *`methods`* : i metodi sono definiti con: nome, argomenti 
    (nome dell'istanza più altri opzionali), corpo del metodo

Esempio di utilizzo:  

```prolog
def_class(person, [],
    [field(name, _),
     field(birthyear, _),
     method(talk, [],
        (write('My name is ’),
        field(this, name, N),
        writeln(N),
        write(’I was born in the year ’)
        field(this, birthyear, A),
        writeln(A)))]).
```

___

### make
`make` è utilizzato per creare istanze a partire da classi. 
Vengono ereditati sia i metodi che i field della classe 
da cui l'istanza è creata, e tutte le sue classi genitore.

Dato lo stesso nome per un'istanza e una definizione dei field diversi, è 
possibile ridefinire più istanze con lo stesso nome. Le vecchie istanze
sono salvate nella Knowledge Base, ma una volta che sono state ridefinite, solo
l'ultima ridefinizione è accessibile dai predicati di OOP.

Dopo aver eseguito `make`, il predicato restituirà *true* la prima volta, e
fallirà nella seconda; questo comportamento è normale e previsto.

Prende come argomenti:
- **`instance-name`** : il nome dell'istanza
- **`class-name`** : il nome della classe da cui viene creata
- **`[<field-name> = <value>]`** : la definizione dei field 
(`field-name = field-value`, opzionale)

Esempio di utilizzo:
```prolog
make(matt, person, [name = 'Matt', birthyear = 1954]).
```
___

### is_class
is_class restituisce `true` se l'atomo passatogli è il nome di una classe.

Esempio di utilizzo:
```prolog
is_class(person).
; restituisce true
```
___

### is_instance
`is_instance` restituisce `true` se l'oggetto passatogli 
è l'istanza di una classe. 

Prende come argomenti:
- **`instance-name`** : il nome dell'istanza/istanza/riferimento ad istanza
- **`class-name`** : il nome della classe (opzionale)

Esempio di utilizzo:
```prolog
is_instance(matt).
% restituisce true

is_instance(matt, person).
% restituisce true

is_instance(akira, person).
% restituisce false

inst(matt, X), is_instance(X).
% restituisce true
```
___

### inst
`inst` recupera un'istanza dato il nome con cui è stata creata da make.

Prende come argomenti:
- **`instance-name`** : il nome dell'istanza (simbolo)
- **`instance`** : termine che rappresenta un'istanza/variabile logica

Esempio di utilizzo:
```prolog
inst(matt, X).
/* restitusce 
X = instance(matt, person, 
  [field(name, "Matt"), field(birthyear, 1954), method(talk, [],
                (write(’My name is ’),
                field(matt, name, N),
                writeln(N),
                write(’I was born in the year ’)
                field(matt, birthyear, A),
                writeln(A)))]). */
```
___

### field
`field` estrae il valore di un campo da un'istanza.

Prende come argomenti:
- **`instance`** : un'istanza
- **`field-name`** : il nome del field da cui estrarre il valore
- **`result`** : valore associato a field-name/variabile logica

Esempio di utilizzo:
```prolog
field(matt, name, X).
%restituisce X = "Matt".

field (matt, surname, X)
%restituisce false.
```

___

### fieldx
`fieldx` estrae il valore da un'istanza percorrendo una serie di attributi. 

Prende come argomenti:
- **`instance`** : un'istanza
- **`field-names`** : lista di fields
- **`result`** : valore associato all'ultimo elemento 
di field-names nell'ultima istanza


Esempio di utilizzo:
```prolog
V1 e V2 sono istanze usate come attributi.
field(matt, person, V1),
field(V1, namename, V2),
field(V2, length, R),
%corrisponde a scrivere:
fieldx(matt, [person, name, length], R).
```

## Predicati ausiliari importanti

### def_method
`def_method` viene usato per dividere il metodo in ciascuna delle sue parti,
sostituisce la keyword `this` all'interno degli argomenti 
e del corpo del metodo con il nome dell'istanza, 
e infine esegue l'`assert` di esso.

Per chiamare il metodo creato all'interno dell'istanza, si può passare come
argomento sia il nome dell'istanza da cui è stato creato, sia il riferimento
dell'istanza da cui è stata creato.

Esempio:
```prolog
> talk(matt).
% Risultato:
% My name is Matt
% I was born in the year 1954
% true.

> inst(matt, S), talk(S).
% Risultato:
% My name is Matt
% I was born in the year 1954
% *risultato della chiamata a inst/2*
% true.
```

___

### find_field, find_field_in_parents
`find_field` e `find_field_in_parents` vengono usati per 
trovare i field all'interno di una classe, 
e all'interno delle sue classi genitore.

Viene usato in `field`.

___

### create_instance_parts
`create_instance_parts` , a partire da un'istanza, 
processa i field e i metodi:
- assegna i valori dati come argomento ai vari field
- esegue l'`assert` di ciascun metodo, formattato correttamente 

Viene eseguito nel `make`, affinchè le istanze vengano create correttamente.