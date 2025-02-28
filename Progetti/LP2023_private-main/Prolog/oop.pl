%%% Bellucci Edoardo 899795
%%% Cattaneo Francesco 900411
%%% Dongiovanni Francesco 865774
%
%%% OOP - Object-Oriented Prolog
%
%%% -*- Mode: Prolog -*-
%%% oop.pl
%
%%% definizione dinamica di class - in modo da poterla modificare
:- dynamic class/3.

%%% definizione dinamica di method
:- dynamic method/3.

%%% definizione dinamica di field
:- dynamic field/2.
:- dynamic field/3.

%%% definizione dinamica di instance
:- dynamic instance/3.

%%% definizione dinamica di reference a instance
:- dynamic reference/2.

%%% aux - is_parts/1: verifica se parts � un field o un method
is_parts([]).
is_parts([X | Y]) :-
    (is_field(X); is_method(X)),
    is_parts(Y).

%%% aux - is_field/2, is_field/3: verifica se � un field
is_field(field(FieldName, _)) :-
    (atom(FieldName) -> true
    ; writeln('Error: fieldname must be an atom.'), fail).

is_field(field(FieldName, Value, Type)) :-
    (atom(FieldName) -> true
    ; writeln('Error: fieldname must be an atom.'), fail),
    check_type(Value, Type).

%%% aux - is_method/3: verifica se � un method
is_method(method(MethodName, ArgList, Form)) :-
    % controllo se nome � un atomo
    (atom(MethodName) -> true
    ; writeln('Error: methodname must be an atom.'), fail),
    % controllo se arglist � una lista
    (is_list(ArgList) -> true
    ; writeln('Error: argument list must be a list.'), fail),
    % controllo se form � un callable term
    (callable(Form) -> true
    ; writeln('Error: form must be a callable term.'), fail).

%%% aux - is_parent/1: verifica se parents � una lista di classi
is_parent([]).
is_parent([X | Y]) :-
    (atom(X), is_class(X), !
    ; writeln('Error: parent must be an atom.'), fail),
    is_parent(Y).

%%% aux - is_atom/1: verifica se X � un atomo
is_atom(X) :-
    (atom(X) -> true
    ; write('Error: name must be an atom.'), fail).

%%% aux - is_variable/1: verifica se X � una variabile
is_variable(X) :-
    (var(X) -> true
    ; writeln('Error: name must be a variable.'), fail).

%%% aux - append_parts/3: appende i parts di una classe
%%% a quelli di un'altra
append_parts([], Parts, Parts).
append_parts([Part | Rest], Parts, PartsNew) :-
    % se part � membro della lista parts, non lo aggiungo
    (member(Part, Parts) ->
	 append_parts(Rest, Parts, PartsNew)
    % altrimenti, lo aggiungo
    ; append_parts(Rest, [Part | Parts], PartsNew)).

%%% def_class/3 : crea una nuova classe
def_class(ClassName, Parents, Parts) :-
    % controllo se classname � un atomo
    (is_atom(ClassName), !),
    % controllo se parents � una lista
    (is_parent(Parents) -> true),
    % controllo se la class name esiste gi�.
    (is_parts(Parts) -> true
    ; writeln('Error: parts isn\'t/aren\'t fields or methods.'), fail),
    % se esiste gi� una classe con lo stesso nome: errore
    (class(ClassName, _, _)
    -> writeln('Error: class already exists.'), fail
    ; assertz((class(ClassName, Parents, Parts)))).

%%% def_class/2 : crea una nuova classe
def_class(ClassName, Parents) :- def_class(ClassName, Parents, []).

%%% is_class/1 : ritorna true se il nome passato � una classe o no
is_class(ClassName) :-
    % se esiste una classe con lo stesso nome: true
    (class(ClassName, _, _), !
    ; writeln('Error: class does not exist in knowledge base.'), fail).

%%% is_reference/1 : ritorna true se il termine passato � una reference
is_reference(Name) :-
    % controlla se il termine � una reference
    reference(Name, _),
    write(Name), writeln(' found as a redefined instance.'), true.

%%% is_instance/1 : ritorna true se l'istanza passata esiste o no
is_instance(Instance) :-
        % crea una lista di tutti i nomi delle istanze nella base di conoscenza
        bagof(N, C^F^instance(N, C, F), Names),
        % se il nome dell'istanza � membro di Names: true
        (member(Instance, Names),
        write(Instance),
        writeln(' instance exists in the knowledge base.'), !
        ; write('Warning: instance does not exist in knowledge base.'),
            writeln(' It can be a redefined instance.'),
            % qui chiamiamo is_reference/1 con l'istanza come argomento
            Instance = instance(Name, _, _),
            is_reference(Name)).

%%% is_instance/1 : ritorna true se l'istanza passata esiste o no
is_instance(InstanceName) :-
        % se esiste un'istanza con lo stesso nome: true
        (instance(InstanceName, _, _), !
        ; write('Warning: instance does not exist in knowledge base.'),
            writeln(' It can be a redefined instance.'),
            % qui chiamiamo is_reference/1 con l'istanza come argomento
            is_reference(InstanceName)).

%%% aux - instance_parts/3: trova le parti dell'istanza
instance_parts(ClassName, FieldValues, InstanceParts, InstanceName) :-
    % trova le parti all'interno di una classe
    class(ClassName, _, ClassParts),
    % assegna i valori dei field immessi a quelli dei field della classe
    assign_fields(FieldValues, AssignedFields, ClassName),
    % trova tutti i metodi della classe
    findall(Method, (member(Method, ClassParts), is_method(Method)), Methods),
    % aggiunge i metodi trovati dalle classi all'interno della KB
    append(AssignedFields, Methods, InstanceParts),
    % asserisce ogni metodo
    maplist(def_method(InstanceName), Methods).

%%% make/3: crea una nuova istanza o una reference
make(InstanceName, ClassName, FieldValues) :-
    % controllo se ClassName esiste
    is_class(ClassName), !,
    % se esiste gi� un'istanza, crea una reference
    (instance(InstanceName, _, _) ->
        % rimuove l'istanza esistente
        retract(instance(InstanceName, _, _)),
        % crea le parti di un'istanza con field e metodi
        instance_parts(ClassName, FieldValues, InstanceParts, InstanceName),
        % inserisce il nuovo predicato reference nella knowledge base
        assertz(reference(InstanceName,
             instance(InstanceName, ClassName, InstanceParts)))
    % se atomo: si crea nella base dati
    ; atom(InstanceName) ->
      % crea le parti di un'istanza con field e metodi
      instance_parts(ClassName, FieldValues, InstanceParts, InstanceName),
      % inserisce il nuovo predicato instance nella knowledge base
      assertz(instance(InstanceName, ClassName, InstanceParts))
    % se variabile: unificata con il termine della nuova istanza
    ; var(InstanceName) ->
      instance_parts(ClassName, FieldValues, InstanceParts, InstanceName),
      InstanceName = instance(_, ClassName, InstanceParts)
    % altrimenti, unifica con la nuova istanza appena creata
    ; instance_parts(ClassName, FieldValues, InstanceParts, InstanceName),
      InstanceName = instance(_, ClassName, InstanceParts)).

%%% make/2: crea una nuova istanza di una classe
make(InstanceName, ClassName) :- make(InstanceName, ClassName, []).

%%% aux - def_method/2: crea un nuovo predicato per il metodo
def_method(InstanceName, method(MethodName, ArgList, Form)) :-
    atom(InstanceName), !, 
    % crea un termine composto per il nuovo predicato
    Predicate =.. [MethodName, Instance | ArgList],
    % verifico se l'argomento passato � un nome o istanza
    Check = (compound(Instance) ->
		 Instance = instance(InstanceName, _, _);
	     Instance = InstanceName),
    replace_this(Form, _, NewForm, InstanceName),
    % aggiungi il predicato alla base di conoscenza
    assertz((Predicate :- (Check, NewForm))).

%%% aux - replace_this/3: sostituisce il termine this
%%% caso base: se Form � 'this', lo sostituisce con il nome dell'istanza
replace_this(this, _, InstanceName, InstanceName) :-
    nonvar(InstanceName), !.
replace_this(Form, This, NewForm, InstanceName) :-
    nonvar(Form), Form =.. [Functor | Args],
    replace_this_in_args(Args, This, NewArgs, InstanceName),
    NewForm =.. [Functor | NewArgs].
replace_this(Form, _, Form, _) :- var(Form), !.

%%% aux - replace_this_in_args/4: sostituisce il termine this
%%% con il nome dell'istanza in una lista
replace_this_in_args([], _, [], _).
replace_this_in_args([Arg | Args], This, [NewArg | NewArgs], InstanceName) :-
    (var(Arg) -> NewArg = Arg ;
     Arg == This -> NewArg = InstanceName ;
     replace_this(Arg, This, NewArg, InstanceName)),
    replace_this_in_args(Args, This, NewArgs, InstanceName).

%%% aux - parent_parts/3: eredita parts da classe parente
parent_parts([], Parts, Parts).
parent_parts([Parent | Rest], Parts, ParentParts) :-
    class(Parent, _, TempParentParts),
    append_parts(TempParentParts, Parts, TempParts),
    parent_parts(Rest, TempParts, ParentParts).

%%% aux - assign_fields/3 : assegna i valori ai field
%%% aux - assign_fields/3 : assegna i valori ai field
assign_fields([], [], _).
assign_fields([Field = Value | Rest], [AssignField | AssignRest], ClassName) :-
    % trova la classe di istanza
    class(ClassName, Parents, ClassFields),
    % trova i parts della classe parente
    parent_parts(Parents, ClassFields, ParentParts),
    % trova il field all'interno della classe di
    % istanza o della classe parente
    (member(field(Field, _, Type), ParentParts) ->
        % controlla che il tipo del field sia corretto
        (check_type(Value, Type), AssignField = field(Field, Value, Type)),
        % continua la ricerca nel resto dei field
        assign_fields(Rest, AssignRest, ClassName)
    % controllo del field se non possiede un attributo di tipo
    ; member(field(Field, _), ParentParts) ->
        % assegna il field direttamente senza verificare il tipo
        (AssignField = field(Field, Value)),
        % continua il controllo sul resto della lista
        assign_fields(Rest, AssignRest, ClassName)
    ;
        write('Error: Invalid field for class'), nl, fail).

%%% aux - check_type/2: controlla che il tipo del field sia corretto
check_type(Value, integer) :- integer(Value).
check_type(Value, float) :- float(Value).
check_type(Value, atom) :- atom(Value).
check_type(Value, string) :- string(Value).
check_type(Value, class) :- is_class(Value).
check_type(Value, instance) :- is_instance(Value).

%%% inst/2: recupera un'istanza o una referenza
%%% dato il nome con cui � stata creata da make
inst(InstanceName, Instance) :-
    % se InstanceName � un'istanza o una referenza gi� esistente
    (is_instance(InstanceName) ; reference(InstanceName, _)), !,
    % altrimenti, cerca prima tra le istanze
    (instance(InstanceName, ClassName, Parts) ->
         % e la si unifica con la variabile Instance
         Instance = instance(InstanceName, ClassName, Parts)
    % se non � un'istanza, cerca tra le referenze
    ; reference(InstanceName, Ref) ->
      Instance = Ref
    % se non n� un'istanza n� una referenza, restituisce un errore
    ; write('Error: Invalid instance or reference name'), nl, fail).

%%% aux - find_field/3: trova il field all'interno di una classe
find_field(ClassName, FieldName, Result) :-
    % trova i field all'interno di una data classe
    class(ClassName, _, Parts),
    % ottiene tutti i field di tutte le parti di una classe
    get_field_parts(Parts, FieldName, Field),
    % salva il field cercato come field/2 o field/3
    (Field = field(FieldName, Result);
     Field = field(FieldName, Result, _)), !.

%%% aux - find_field/3: trova il field all'interno di una classe
find_field(ClassName, FieldName, Result) :-
    % trova i field all'interno delle classi parent di una data classe
    parent_class(ClassName, ParentClassName),
    % cerca il field all'interno della classe parent
    find_field(ParentClassName, FieldName, Result).

%%% aux - find_field_in_parents/3: trova il field all'interno
%%% delle classi parent di una data classe
find_field_in_parents(ClassName, FieldName, Result) :-
    % cerca le parent class di una classe
    parent_class(ClassName, ParentClassName),
    % cerca il field all'interno della classe
    find_field(ParentClassName, FieldName, Result), !.

%%% aux - find_field_in_parents/3: trova il field all'interno
%%% delle classi parent di una data classe
find_field_in_parents(ClassName, FieldName, Result) :-
    % cerca le parent class di una classe
    parent_class(ClassName, ParentClassName),
    % chiamata ricorsiva
    find_field_in_parents(ParentClassName, FieldName, Result).

%%% aux - parent_class/2: trova la classe parent di una data classe
parent_class(ClassName, ParentClassName) :-
    % cerca la classe all'interno della knowledge base
    class(ClassName, Parents, _),
    % trova la classe parent
    member(ParentClassName, Parents).

%%% field/3: estrae il valore di un campo da un'istanza o da un riferimento
field(InstanceOrReference, FieldName, Result) :-
    % verifica se il nome del field � un atomo
    atom(FieldName),
    % risolve il riferimento, se necessario
    (atom(InstanceOrReference), instance(InstanceOrReference, _, _) ->
         instance(InstanceOrReference, ClassName, InstanceParts)
    ; atom(InstanceOrReference), reference(InstanceOrReference, _) ->
      reference(InstanceOrReference, instance(_, ClassName, InstanceParts))
    ; InstanceOrReference = instance(_, ClassName, InstanceParts)),
    % cerca il field all'interno dell'istanza
    field_aux(ClassName, InstanceParts, FieldName, Result).

%%% aux - field_aux/4: trova il field all'interno di una classe
field_aux(ClassName, InstanceParts, FieldName, Result) :-
    % trova il field all'interno di una classe
    (get_field_parts(InstanceParts, FieldName, Field),
     % se il field � di tipo field/2 o field/3, lo unifico con Field
     (Field = field(FieldName, FieldValue)
     ; Field = field(FieldName, FieldValue, _)),
     % se il field � una variabile, la unifico con il suo valore
     (var(FieldValue) -> Result = 'undefined', ! ; Result = FieldValue), !)
    ;
    % cerca il field all'interno della classe da cui deriva l'istanza
    (find_field(ClassName, FieldName, Result), !)
    ;
    % cerca il field all'interno delle classi parent di una data classe
    (parent_class(ClassName, ParentClassName),
     find_field(ParentClassName, FieldName, Result), !)
    ;
    % cerca il field all'interno delle classi parent di una data classe
    (find_field_in_parents(ClassName, FieldName, Result), !).

%%% aux - get_field_parts/3: ottiene tutti i field di
%%% tutte le parti di una classe
get_field_parts([X | Y], FieldName, Field) :-
    %  prendo il primo elemento della lista e verifico se sia un field
    (is_field(X),
     %  ottengo il nome del field e se � uguale a quello cercato
     get_field_name(X, FieldName),
     %  se il field � di tipo field/2 o field/3, lo unifico con Field
     (X = field(FieldName, _) ; X = field(FieldName, _, _)),
     Field = X)
    ; get_field_parts(Y, FieldName, Field).

%%% aux - extract_field_name/2: estrae il nome del field
extract_field_name(field(FieldName, _, _), FieldName).

%%% aux - get_field_name/2: verifica se il nome del field � uguale al
%%% nome del field cercato
get_field_name(_Fields, FieldName) :-
    extract_field_name(_Field, FieldName).

%%% fieldx/3: estrae il valore di un campo da una classe
fieldx(Instance, [Field], Result) :-
    % cerca il field all'interno dell'istanza
    field(Instance, Field, Result).

%%% fieldx/3: estrae il valore di un campo da una classe - ricorsivo
fieldx(Instance, [Field | Fields], Result) :-
    % cerca il field all'interno dell'istanza e salva il risultato intermedio
    field(Instance, Field, IntermediateResult),
    % continua la ricerca nel resto dei field
    fieldx(IntermediateResult, Fields, Result).

%%% End of file - oop.pl
