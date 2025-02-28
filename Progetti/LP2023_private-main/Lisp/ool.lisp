;;; Bellucci Edoardo 899795
;;; Cattaneo Francesco 900411
;;; Dongiovanni Federico 865774

;;; OOL - Object-Oriented Lisp

;;; -*- Mode: Common Lisp -*-

;;; ool.lisp

;;; definizione del registro delle classi
(defvar *class-registry* '())

;;; definizione del registro delle istanze
(defvar *instance-registry* '())

;;; definizione della struttura dati classe
(defstruct classe name parents parti)

;;; definizione della struttura dati parts
(defstruct parti fields methods)

;;; definizione della struttura dati field
(defstruct field name value type)

;;; definizione della struttura dati method
(defstruct metodo name arglist form)

;;; definizione della struttura dati istanza
(defstruct istanza classname fields methods)

;;; aux - is_parent: verifica che parents sia una lista di classi
(defun is-parent (parents)
  (and (listp parents)
					; e che ciascun elemento
					; della lista sia un simbolo
       (every #'symbolp parents)
       )
  )

;;; aux - is-field: verifica se un field passato è valido
(defun is-field (fields)
					; verifica che fields sia una lista
  (and (listp fields)
					; verifica che ogni field sia valido
       (every (lambda (field)
                (and (symbolp (field-name field))
                     (or (symbolp (field-type field))
                         (numberp (field-type field))
                         (typep (field-type field) 
				'(simple-array character (*)))
                         (stringp (field-type field))
                         (listp (field-type field))
                         (null (field-type field)))))
              fields)))

;;; aux - is-method: verifica se un method passato è valido
(defun is-method (method)
					; verifica che il nome sia un simbolo
  (and (symbolp (metodo-name method))
					; verifica che arglist sia una lista
       (listp (metodo-arglist method))
					; verifica che form sia una lista
       (listp (metodo-form method))
       )
  )
;;; aux - is-parts: verifica che le parti passate siano valide
(defun is-parts (parts)
					; verifica che le parti
					; passate siano una lista e
					; del tipo corretto
  (when (parti-p parts)
					; applica una funzione a
					; tutti gli elementi della lista
    (every (lambda (part)
					; ogni elemento deve essere
					; un field o un method
             (or (is-field part)
                 (is-method part))) (parti-fields parts))
    )
  )

;;; is-class: verifica che il nome passato sia una classe
(defun is-class (classname)
					; verifica che il nome sia un simbolo
  (and (symbolp classname)
					; se esiste almeno un singolo elemento
					; check- soddisfa la funzione
       (some (lambda (classe)
					; allora ritorno T, altrimenti nil
               (eq classname (classe-name classe))) *class-registry*)
       )
  )

;;; aux - find-classe: trova la classe associata a classname
(defun find-classe (classname)
					; dato il nome della classe,
					; cerca la classe nel registro
  (find classname *class-registry* :key #' classe-name)
  )

;;; aux - is-subclass-of: verifica se classname
;;; è una sottoclasse di superclass
(defun is-subclass-of (classname superclass)
  (let ((classe (find-classe classname)))
    (if classe
					; se la classe è stata trovata,
					; controlla se è una sottoclasse
        (or (eq classname superclass)
            (some (lambda (parent)
					; cerca ricorsivamente tra
					; i parent della classe
                    (is-subclass-of parent superclass))
                  (classe-parents classe))
            )
					; altrimenti ritorna nil
	NIL)
    )
  )

;;; is-instance: verifica che il nome passato sia una istanza
(defun is-instance (value &optional (class-name T))
					; cerca se esiste
					; un'istanza con il valore passato
  (let ((instance (find value *instance-registry* :test #'eq)))
					; se esiste
    (if instance
					; verifica che il nome della classe
					; sia uguale a class-name
        (if (eq class-name T)
            T
					; altrimenti verifica che la classe
					; sia una sottoclasse di class-name
            (is-subclass-of (istanza-classname instance) class-name)
            )
	NIL)
    )
  )


;;; aux - make-parts-duo: crea una struttura parts con sintassi diversa
(defun make-parts-duo (parts)
					; inizializza le liste
					;di fields e methods
  (let ((fields '())
        (methods '()))
					; itera su tutti gli elementi in parts
    (mapc (lambda (part)
            (let* ((type (first part))
                   (fields-or-methods (rest part)))
					; se il tipo è fields
					; aggiungi alla lista di fields
              (when (equal type 'fields)
                (push (mapcar
		       #'make-field-from-list fields-or-methods) fields))
					; se il tipo è methods
					; aggiungi alla lista di methods
              (when (equal type 'methods)
                (push (mapcar #'make-metodo-from-list fields-or-methods)
		      methods))))
          parts)
					; crea la struttura parti
					; con le liste di fields e methods
    (make-parti :fields fields :methods methods)))

;;; aux - make-field-from-list: crea un field a partire da una lista
(defun make-field-from-list (field-list)
					; creo variabili per
					; ciascuno delle parti di un field
  (let ((name (first field-list))
        (value (second field-list))
					; se il field passato ha lunghezza 3
					; allora ha un tipo, altrimenti nil
        (type (if (= (length field-list) 3) (third field-list) nil))
        )
					; verifico se il valore
					; corrisponde al tipo
    (if (or (not type) (typep value type))
					; se il valore corrisponde
					; al tipo, creo il field
        (make-field :name name :value value :type type)
					; altrimenti, segnalo un errore
	(error "The value ~S is not of type ~S~%" value type)
	)
    )
  )

;;; aux - make-metodo-from-list: crea un metodo
;;; a partire da una lista di metodi
(defun make-metodo-from-list (method-list)
					; divide la lista del
					; metodo in nome, argomento e body
  (let ((name (first method-list))
        (arglist (second method-list))
        (form (third method-list))
        )
					; inizializza la struttura dati
    (make-metodo :name name :arglist arglist :form form)
    )
  )

;;; def-class: definisce una nuova classe e la salva all'interno del registro
(defun def-class (name parents &rest parti)
					; verifica che il nome sia un simbolo
  (unless (symbolp name)
    (error "Invalid class name")
    )
					; verifica che la classe non esista già
  (if (is-class name)
      (error "Class already defined")
      )
					; verifica che i parent siano
					; delle classi valide
  (unless (every #'is-class parents)
    (error "Invalid parent classes")
    )
					; crea una nuova classe e la si
					; aggiunge al registro con let
  (let ((new-class
					; si usa make-parts-duo per poter
					; definire ciascun field o method
         (make-classe :name name
                      :parents parents
                      :parti (make-parts-duo parti))))
					; verifica che le parti
					; passate siano valide
    (cond ((is-parts (classe-parti new-class))
					; effettua un push nel
					; registro delle classi
           (progn (push new-class *class-registry*) name)
           )
          (t (error "Invalid parts."))
          )
    )
  )

;;; aux - find-field: trova un field all'interno di una lista di fields
(defun find-field (field-name fields)
  (find field-name fields :key #'field-name :test #'string=))

;;; aux - aux-parents-fields: Raggruppa in una lista
;;; tutti i fields della classe e dei suoi parents
(defun aux-parents-fields (classname auxfields)
					; cerco la classe
  (let* ((classe (find-classe classname)))
					; se non esiste la classe, ritorno
    (if (not classe) 
        auxfields
	(let* ((classe-fields (first (parti-fields (classe-parti classe))))
               (updated-auxfields
					; per ogni lezione della classe
					; chiama la funzione su ciascuno
					; e aggiunge i risultati
		(mapcan (lambda (parent)
                          (aux-parents-fields parent 
					      (apply #'append (list
							       auxfields))))
			(classe-parents classe))))
					; rimuovo i campi con lo stesso nome
					; da acc
          (let* ((new-auxfields 
                  (reduce (lambda (acc field)
                            (append (if (not (null field))
					(remove-if (lambda (f)
                                                     (and f 
							  (string= (field-name
								    f) 
								   (field-name
								    field))))
						   acc))
					; ritorno la lista di campi
                                    (list field)))
                          classe-fields
                          :initial-value updated-auxfields)))
            new-auxfields)))))

;;; aux - aux-parents-methods: Raggruppa in una lista tutti i methods
;;; della classe e dei suoi parents
(defun aux-parents-methods (classname auxmethods)
					; trova la classe corrente
  (let* ((classe (find-classe classname)))
					; se non trova la classe
					; ritorna auxmethods
    (if (not classe) auxmethods
	(let* ((classe-methods (first (parti-methods (classe-parti classe)))))
					; chiamata ricorsiva per ogni parent
          (dolist (parent (classe-parents classe))
            (setf auxmethods (aux-parents-methods parent auxmethods)))
					; aggiungi i methods della
					; classe corrente a auxmethods
          (reduce (lambda (auxmethods metodo)
					; se un metodo con lo stesso
					; nome è già presente, rimuovilo
                    (append (if (not (null metodo))
				(remove-if (lambda (f)
                                             (and f (string=
						     (metodo-name f)
						     (metodo-name metodo))))
                                           auxmethods))
                            (list metodo)))
                  classe-methods
                  :initial-value auxmethods)))))

;;; aux - get-field-type - ottieni il tipo di un field nella classe
(defun get-field-type (classname fieldname)
					; trovo il field all'interno delle
					; classi e dei parents
  (let ((fieldfound (find fieldname
                          (aux-parents-fields classname
                                              '()) :key #'field-name)
          ))
					; se il field è trovato,
					; ritorna il tipo
    (if fieldfound
        (field-type fieldfound)
	nil
	)
    )
  )

;;; aux - find-metodo: trova un metodo all'interno dell'istanza
(defun find-metodo (instance method-name)
  (some (lambda (method)
					; confronta "method-name"
					; con i metodi di "instance"
          (string= (if (symbolp method)
                       (symbol-name method)
                       (metodo-name method))
                   method-name)
          )
        (istanza-methods instance)
        )
  )

;;; process-method: processa il metodo
(defun process-method (method-name method-spec)
					; dividiamo il method-spec
					; in argomenti e body
  (let ((arg (first method-spec))
        (body (second method-spec))
        )
    (cond
					; se non ci sono argomenti:
					; imposta come primo argomento l'istanza
      ((null arg) (eval `(defun ,method-name (this)
					; imposta come corpo del metodo il body
					; del metodo non processato
					; preceduto date-string un
					; check della presenza
					; del metodo nell'istanza
                           (if (find-metodo this ',method-name)
                               (,@body)
                               (error "Method ~a not found in instance ~a" '
                                      ,method-name this)
                               )))
       )
					; se c'è un solo argomento:
					; imposta come argomenti l'istanza e arg
      ((symbolp arg) (eval `(defun ,method-name (this arg)
                              (if (find-metodo this ',method-name)
                                  (lambda (arg) (,@body))
				  (error "Method ~a not found in instance ~a" '
					 ,method-name this))))
       )
					; se c'è una lista di argomenti,
					;imposta istanza e arg come argomenti
      ((listp arg) (eval `(defun ,method-name (this . ,arg)
                            (if (find-metodo this ',method-name)
				(,@body)
				(error "Method ~a not found in instance ~a" '
                                       ,method-name this))))
       )
      )
    )
  )

;;; aux - process-method-aux - passo gli attributi del metodo a process-method
(defun process-method-aux (object)
  (process-method (metodo-name object)
                  (list (metodo-arglist object) (metodo-form object)))
  )

;;; make: crea un'istanza e la salva all'interno del registro
(defun make (classname &rest field-values)
  (unless (is-class classname)
    (error "Invalid class name"))
  (let* ((fields (loop for (field-name value) on field-values by #'cddr
                       collect (let* ((expected-type
				       (get-field-type classname field-name))
                                      (actual-type (type-of value)))
                                 (when (and expected-type 
                                            (integerp actual-type)
                                            (not (eq expected-type
						     actual-type)))
                                   (error "The value ~S is not of type ~S" 
                                          value expected-type))
                                 (make-field :name field-name
                                             :value value
                                             :type actual-type)))))
    (unless (is-field fields)
      (format t "Current fields: ~A~%" fields)
      (error "Invalid fields"))
    (let* ((new-fields (aux-parents-fields classname fields))
           (new-fields (reduce (lambda (new-fields field)
                                 (cons field
                                       (remove-if (lambda (f)
                                                    (string= (field-name f)
							     (field-name field)
							     )) new-fields)))
                               fields
                               :initial-value new-fields))
           (new-methods (aux-parents-methods classname '()))
           (new-istanza (make-istanza :classname classname
                                      :fields new-fields
                                      :methods new-methods)))
      (push new-istanza *instance-registry*)
      (mapcar #'process-method-aux new-methods)
      new-istanza)))

;;; field: estrae il valore di un campo da un'istanza
(defun field (instance fieldname)
					; verifica se l'istanza passata è valida
  (unless (is-instance instance)
    (error "Invalid instance name"))
					; verifica che il field
					; passato sia un nome
  (unless (symbolp fieldname)
    (error "Invalid field name"))
					; cerca il field all'interno
					; dei field dell'istanza
  (let* ((fields (istanza-fields instance))
					; se è trovato: ritorna il valore
         (field (find fieldname fields :key #'field-name)))
    (if field
        (field-value field)
	(error "Field not found"))))

;;; field*: estrae valore percorrendo una serie di attributi
(defun field* (instance &rest fieldlist)
					; verifica se l'istanza passata è valida
  (unless (is-instance instance)
    (error "Invalid instance name")
    )
					; verifica che fieldlist sia una lista
  (unless (listp fieldlist)
    (error "Invalid field list")
    )
					; caso base: se fieldlist è
					; vuota, ritorna l'istanza
  (if (null fieldlist)
      instance
					; caso ricorsivo: applica
					; field all'istanza
					; e al primo elemento di fieldlist,
					; poi chiama field* ricorsivamente
					; sul risultato
					; e sul restore-code-coverage-data
					; di fieldlist
      (apply #'field* (field instance (car fieldlist)) (cdr fieldlist))
      )
  )

;;; End of file - ool.lisp
