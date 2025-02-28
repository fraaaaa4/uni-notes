#ifndef SORTEDARRAY_HPP
#define SORTEDARRAY_HPP

#include <iterator>
#include <stdexcept>
#include <iostream>

/**
 @brief Classe generica che implementa un array dinamico ordinato di elementi generici di tipo `T`.
 Questa classe implementa un array dinamico che mantiene gli elementi ordinati in ordine crescente. L'array segue un ordinamento definito liberamente dall'utente.
 La dimensione dell'array è dinamica e può crescere o ridursi durante l'uso.
 @tparam T Tipo degli elementi contenuti nell'array dinamico ordinato.
 @tparam Comparator Funtore utilizzato per definire l'ordinamento degli elementi.
 */
template <typename T, typename Comparator>
class SortedArray {
private:
    T* array;           ///< Array dinamico per contenere gli elementi
    size_t arraySize;   ///< Dimensione attuale dell'array
    Comparator compare; ///< Funtore per l'ordinamento personalizzato
    
public:
    /**
     @brief Costruttore di default.
     Crea un array dinamico vuoto inizializzando il puntatore a `nullptr` e la dimensione a 0. Il comparatore viene inizializzato usando il costruttore di default di `Comparator`.
     */
    SortedArray(): array(nullptr), arraySize(0), compare(Comparator()) {}
    
    /**
     @brief Costruttore con comparatore personalizzato.
     Crea un array dinamico vuoto inizializzando il puntatore a `nullptr` e la dimensione a 0. Il comparatore viene inizializzato con il valore passato dall'utente.
     @param compare Funtore per il confronto tra elementi.
     */
    SortedArray(Comparator compare) : array(nullptr), arraySize(0), compare(compare) {
    }
    
    /**
     @brief Distruttore della classe.
     Rilascia la memoria allocata dinamicamente per gli elementi dell'arary.
     */
    ~SortedArray() {
        delete[] array;
    }
    
    /**
     @brief Costruttore di copia.
     Crea una copia profonda di un altro array ordinato, duplicando gli elementi senza influire sull'array originale.
     @param other Array da cui copiare gli elementi; deve essere un array ordinato di tipo `SortedArray<T>` con lo stesso tipo di dati e comparatore.
     @pre `other` deve essere un SortedArray valido è già ordinato.
     @throws std::bad_alloc se l'allocazione dinamica fallisce.
     */
    SortedArray(const SortedArray& other) : array(nullptr), arraySize(0), compare(other.compare) {
        // Se l'array da copiare è vuoto, ritorniamo immediatamente
        try {
            if (other.size() == 0) {
                return;
            }
            
            for (size_t i = 0; i < other.size(); ++i) {
                insert(other[i]);
            }
        } catch (const std::bad_alloc&) {
            clear(); throw;
        }
    }
    
    /**
     @brief Costruttore di copia con comparatore diverso.
     Crea una copia profonda di un altro array ordinato, duplicando gli elementi e ordinandoli secondo il comparatore del nuovo array (`Comparator`),
     anche se l'array di origine utilizza un comparatore diverso (`OtherComparator`).
     @param other Array da cui copiare gli elementi, con un comparatore diverso.
     @pre `other` deve essere un SortedArray valido e già ordinato.
     @throw std::bad_alloc se l'allocazione dinamica fallisce.
     */
    template <typename OtherComparator>
    SortedArray(const SortedArray<T, OtherComparator>& other) : array(nullptr), arraySize(0), compare(Comparator()) {
        try {
            // Se l'array da copiare è vuoto, ritorniamo immediatamente
            if (other.size() == 0) {
                return;
            }
            
            for (size_t i = 0; i < other.size(); ++i) {
                insert(other[i]);
            }
        } catch (const std::bad_alloc&) {
            clear(); throw;
        }
    }
    
    /**
     * @brief Costruttore di copia tra tipi e comparatori diversi.
     * Crea una copia profonda di un altro array ordinato con tipo di dato `U` e comparatore `OtherComparator`.
     * Gli elementi vengono convertiti nel tipo `T` e riordinati secondo `Comparator`.
     * @tparam U Tipo degli elementi nell'array sorgente.
     * @tparam OtherComparator Comparatore dell'array sorgente.
     * @param other Array da copiare, con tipo e/o comparatore diversi.
     * @pre `other` deve essere un `SortedArray<U, OtherComparator>` valido e già ordinato.
     * @throws std::bad_alloc se l'allocazione dinamica per la copia fallisce.
     */
    template <typename U, typename OtherComparator>
    SortedArray(const SortedArray<U, OtherComparator>& other) : compare(Comparator()) {
        try {
            if (other.size() == 0) {  // Se l'array è vuoto
                array = nullptr;
                arraySize = 0;
                return;
            }

            // Inizializza l'array con la dimensione dell'array sorgente
            arraySize = 0;
            array = nullptr;
            
            // Inserisci ogni elemento ordinato
            for (size_t i = 0; i < other.size(); ++i) {
                insert(static_cast<T>(other[i]));
            }
        } catch (const std::bad_alloc&) {
            delete[] array;
            array = nullptr;
            arraySize = 0;
            throw;
        }
    }
    
    /**
     @brief Costruttore di copia con conversione di tipo.
     Costruisce un `SortedArray<T, Comparator>` copiando gli elementi di un altro `SortedArray<Q, Comparator>`, convertendoli dal tipo `Q` al tipo `T` e mantenendo l'ordine originale.
     @tparam Q Tipo degli elementi nell'array sorgente.
     @param other Altro `SortedArray<Q, Comparator>` da copiare e convertire.
     @pre Deve essere possibile convertire `Q` in `T` tramite `static_cast<T>`.
     */
    template <typename Q>
    SortedArray(const SortedArray<Q, Comparator>& other) : array(nullptr), arraySize(0), compare(Comparator()) {
        try {
            for (size_t i = 0; i < other.size(); ++i) {
                insert(static_cast<T>(other[i]));
            }
        } catch (std::bad_alloc&) {
            clear(); throw;
        }
    }
    
    /**
     @brief Costruttore secondario con iteratori.
     Costruisce un array ordinato a partire da una sequenza di dati fornita da due iteratori generici. Gli elementi vengono copiati e ordinati secondo il comparatore specificato nella classe.
     @tparam Iter Tipo generico degli iteratori.
     @param begin Iteratore generico di inizio sequenza.
     @param end Iteratore generico di fine sequenza.
     @pre Gli iteratori devono definire un intervallo valido `[begin, end)`.
     @throws Lancia qualsiasi eccezione che possa essere generata durante l'inserimento degli elementi (ad esempio, fallimento nell'inserire un elemento, errore di memoria).
     */
    template <typename Iter>
    SortedArray(Iter begin, Iter end) : array(nullptr), arraySize(0), compare(Comparator()) {
        try {
            for (Iter it = begin; it != end; ++it) {
                insert(*it);
            }
        } catch (...) {
            clear();  // Libera la memoria in caso di errore
            throw;
        }
    }
    
    /**
     @brief Operatore di assegnamento.
     Assegna il contenuto di un altro array ordinato a questo.
     @param other Array ordinato da assegnare.
     @return Riferimento a questo array dopo l'assegnazione.
     */
    SortedArray<T, Comparator> &operator=(const SortedArray<T,
                                          Comparator> &other)
    {
        if (this != &other)
        {
            SortedArray temp(other);
            this->swap(temp);
        }
        return *this;
    }
    
    /**
     @brief Operatore di accesso in sola lettura.
     Restituisce l'elemento all'indice specificato dell'array ordinato. Il valore restituito è costante, quindi non può essere modificato.
     @param index Indice dell'elemento da restituire.
     @return Riferimento costante all'elemento all'indice specificato.
     @pre L'indice deve essere un valore valido (0 ≤ `index` < size()).
     @throws std::out_of_range se l'indice è fuori dai limiti dell'array.
     @throws std::runtime_error se l'array su cui si tenta di accedere non è stato inizializzato.
     */
    const T& operator[](size_t index) const {
        if (array == nullptr)
            throw std::runtime_error("Array is not initialized.");
        if (index >= arraySize)
            throw std::out_of_range("Index out of bounds in SortedArray.");
        return array[index];
    }

    /**
     @brief Restituisce il primo elemento dell'array ordinato, con riferimento costante.
     @return Riferimento costante al primo elemento dell'array.
     @pre L'array non deve essere vuoto.
     @throws std::runtime_error se l'array è vuoto.
     @throws std::runtime_error se l'array su cui si tenta di accedere non è stato inizializzato.
     */
    const T& top() const {
        if (array == nullptr)
            throw std::runtime_error("Array is not initialized.");
        if (arraySize == 0)
            throw std::runtime_error("Cannot access top element: SortedArray is empty.");
        return array[0];
    }
           
    /**
     @brief Restituisce l'ultimo elemento dell'array ordinato, con riferimento costante.
     @return Riferimento costante all'ultimo elemento dell'array.
     @pre L'array non deve essere vuoto.
     @throws std::runtime_error se l'array è vuoto, e quindi non esiste un ultimo elemento.
     @throws std::runtime_error se l'array su cui si tenta di accedere non è stato inizializzato.
     */
    const T& _end() const {
        if (array == nullptr)
            throw std::runtime_error("Array is not initialized.");
        if (arraySize == 0)
            throw std::runtime_error("Cannot access end element: SortedArray is empty.");
        return array[arraySize - 1];
    }
    
    /**
     @brief Restituisce la dimensione dell'array.
     @return Numero di elementi presenti nell'array ordinato.
     */
    size_t size() const {
        return arraySize;
    }
    
    /**
     @brief Verifica se l'array ordinato è vuoto.
     @return `true` se l'array ordinato è vuoto, altrimenti `false`.
     */
    // Versione generica per tipi non-puntatori
    bool isEmpty() const {
        return (arraySize == 0);
    }
    
    /**
     @brief Verifica se un elemento è presente nell'array ordinato.
     @param value Valore da cercare.
     @return `true` se l'elemento è presente, `false` altrimenti.
     @throws std::runtime_error se l'array a cui si tenta di accedere non è inizializzato.
     @throws std::exception se si verifica un errore durante il confronto degli elementi.
     */
    bool contains(const T& value) const {
        if (array == nullptr)
            throw std::runtime_error("Array is not initialized.");
        size_t left = 0;
        size_t right = arraySize - 1;

        while (left <= right) {
            size_t mid = left + (right - left) / 2;

            if (mid >= arraySize) return false;
            
            try {
                if (array[mid] == value) {
                    return true;  // Elemento trovato
                } else if (compare(array[mid], value)) {
                    left = mid + 1;  // Cerca nella metà destra
                } else {
                    right = mid - 1;  // Cerca nella metà sinistra
                }
            } catch (const std::exception& e) {
                std::cerr << "Error - " << e.what() << std::endl;
                return false;
            }
        }

        return false;  // Elemento non trovato
    }
    
    /**
     @brief Modifica tutti gli elementi che corrispondono al valore specificato, sostituendoli con un nuovo valore, mantenendo l'ordinamento dell'array.
     @param oldValue Valore da cercare nell'array.
     @param newValue Nuovo valore da inserire al posto di `oldValue`.
     @return `true` se almeno un elemento è stato sostituito, `false` altrimenti.
     @throws std::out_of_range se l'indice dell'array è fuori dai limiti.
     @throws std::runtime_error se si verifica un errore durante la rimozione o l'inserimento.
     */
    bool _edit(const T& oldValue, const T& newValue) {
        bool found = false;
        
        // trova tutte le occorrenze
        for (size_t i = 0; i < arraySize; ++i) {
            try {
                if (array[i] == oldValue) {
                    if (!removeOne(array[i])) {
                        throw std::runtime_error("Error during removal of old value.");
                    }
                    array[i] = newValue;
                    if (!insert(newValue)) {
                        throw std::runtime_error("Error during insertion of new value.");
                    }
                    found = true;
                }
            } catch (const std::exception& e) {
                std::cerr << "Error: " << e.what() << std::endl;
                throw;
            }
        }
        return found;
    }
    
    /**
     @brief Modifica un elemento all'indice specificato dell'array ordinato.
     L'elemento esistente all'indice dato viene rimosso e sostituito dal nuovo valore, mantenendo l'ordinamento dell'array.
     @param index Indice dell'elemento da modificare.
     @param newValue Nuovo valore da inserire nell'array.
     @return Il valore dopo la sostituzione.
     @pre L'indice deve essere un valore valido (0 ≤ `index` < size()).
     @throws std::out_of_range se l'indice è fuori dai limiti dell'array.
     @throws std::runtime_error se si verifica un errore durante l'operazione di rimozione o inserimento.
     */
    T edit(size_t index, const T& newValue) {
        if (index < 0 || index >= arraySize){
            throw std::out_of_range("Index out of range");
            return false;
        }

        T oldValue = array[index];
        try {
            removeOne(oldValue);
        } catch (const std::exception& e) {
            throw std::runtime_error("Error during removal: " + std::string(e.what()));
        }

        try {
            insert(newValue);
        } catch (const std::exception& e) {
            throw std::runtime_error("Error during insertion: " + std::string(e.what()));
        }
        
        return newValue;
    }
    
    /**
     * @brief Modifica un elemento all'indice specificato dell'array ordinato.
     * L'elemento esistente all'indice dato viene rimosso e sostituito dal nuovo valore, mantenendo l'ordinamento dell'array.
     * @param index Indice dell'elemento da modificare.
     * @param newValue Nuovo valore da inserire nell'array.
     * @return Il valore precedentemente presente nell'array, prima della sostituzione.
     * @pre L'indice deve essere un valore valido (0 ≤ `index` < size()).
     * @throws std::out_of_range se l'indice è fuori dai limiti dell'array.
     * @throws std::runtime_error se l'array non è stato inizializzato correttamente.
     * @throws std::bad_alloc se la memoria non può essere allocata durante l'inserimento.
     * @throws std::exception se si verifica un altro errore durante l'inserimento.
     */
    T editPre(size_t index, const T& newValue) {
        if (index >= arraySize) {
            throw std::out_of_range("Index out of range");
        }

        // Controlla se l'array è correttamente allocato
        if (array == nullptr) {
            throw std::runtime_error("Array is not initialized");
        }

        T oldValue = array[index];
        remove(index); // Rimuovi l'elemento all'indice dato

        try {
            insert(newValue);  // Inserisce il nuovo valore, mantenendo l'ordinamento
        } catch (const std::bad_alloc& e) {
            throw std::bad_alloc();  // Propaga l'eccezione di allocazione
        } catch (const std::exception& e) {
            throw std::runtime_error("Error during insertion: " + std::string(e.what()));  // Propaga altri errori
        } catch (...) {
            throw std::runtime_error("Unknown error occurred during insertion");  // Gestisce errori sconosciuti
        }

        return oldValue;
    }

    /**
     @brief Inserisce un elemento `insert` nell'array.
     Inserisce un elemento `insert` all'interno dell'array ordinato, mantenendolo ordinato secondo il comparatore specificato. L'array viene ridimensionato dinamicamente per inserire il nuovo elemento.
     @param insert Elemento da inserire.
     @return `true` se l'inserimento è avvenuto con successo, `false` altrimenti.
     @throws std::bad_alloc Se l'allocazione della memoria fallisce quando si crea un nuovo array.
     @throws Qualsiasi eccezione sollevata da `operator=` della classe T se la copia degli elementi fallisce.
     */
    bool insert(const T& insert) {
        T* newArray = nullptr;

        try {
            newArray = new T[arraySize + 1];
        } catch (const std::bad_alloc&) {
            return false;
        }

        size_t i = 0;

        try {
            while (i < arraySize && compare(array[i], insert)) {
                ++i;
            }

            for (size_t j = 0; j < i; ++j) {
                newArray[j] = array[j];
            }
            newArray[i] = insert;
            for (size_t j = i; j < arraySize; ++j) {
                newArray[j + 1] = array[j];
            }

        } catch (...) {
            delete[] newArray;
            return false;
        }

        delete[] array;
        array = newArray;
        ++arraySize;
        
        return true;
    }
    
    /**
     @brief Rimuove tutte le occorrenze di un elemento dall'array.
     Rimuove un elemento specificato e riduce la dimensione dell'array.
     @param remove Elemento da rimuovere.
     @return `true` Se almeno un elemento è stato rimosso, altrimenti `false`.
     @note La dimensione dell'array viene ridotta per quanti elementi sono stati rimossi.
     @throws std::bad_alloc Se l'allocazione di memoria fallisce durante la creazione del nuovo array.
     @throws Qualsiasi eccezione sollevata durante il riempimento del nuovo array.
     */
    bool remove(const T& remove) {
        size_t count = 0;

        // Conta quante occorrenze dell'elemento sono presenti nell'array
        for (size_t i = 0; i < arraySize; ++i) {
            if (array[i] == remove) {
                ++count;
            }
        }

        if (count == 0) {
            return false;  // Nessun elemento trovato da rimuovere
        }

        size_t newSize = arraySize - count;
        if (newSize == 0) {
            clear();
            return true;  // Se l'array è vuoto dopo la rimozione, cancella tutto
        }

        T* newArray = nullptr;

        try {
            newArray = new T[newSize];
        } catch (const std::bad_alloc& e) {
            std::cerr << "Memory error - " << e.what() << std::endl;
            return false;
        }

        size_t j = 0;

        try {
            for (size_t i = 0; i < arraySize; ++i) {
                if (array[i] != remove) {
                    newArray[j++] = array[i];
                }
            }
        } catch (const std::exception& e) {
            std::cerr << "Error - " << e.what() << std::endl;
            delete[] newArray;
            return false;
        } catch (...) {
            std::cerr << "Error." << std::endl;
            delete[] newArray;
            return false;
        }

        delete[] array;
        array = newArray;
        arraySize = newSize;
        return true;  // Rimozione riuscita
    }
    
    /**
     @brief Rimuove un elemento all'indice specificato.
     Rimuove l'elemento all'indice dato e riduce la dimensione dell'array.
     @param index Indice dell'elemento da rimuovere.
     @return `true` Se l'elemento è stato rimosso, altrimenti `false`.
     @note La dimensione dell'array viene ridotta di uno se un elemento viene rimosso.
     @note Questa funzione è implementata solo come funzione helper per le funzioni di modifica.
     */
private:
    bool _remove(size_t index) {
        if (index >= arraySize) {
            throw std::out_of_range("Index out of range in _remove.");
            return false;  // Indice fuori dai limiti
        }

        for (size_t i = index; i < arraySize - 1; ++i) {
            array[i] = array[i + 1];
        }

        --arraySize;

        if (arraySize == 0) {
            clear();
        }

        return true;
    }
    
    /**
     @brief Rimuove una sola occorrenza di un elemento nell'array.
     Rimuove la prima occorrenza di un elemento specificato e riduce la dimensione dell'array.
     @param remove Elemento da rimuovere.
     @return `true` Se almeno un elemento è stato rimosso, altrimenti `false`.
     @note La dimensione dell'array viene ridotta di uno se un elemento viene rimosso.
     @note Questa funzione è implementata solo come funzione helper per le funzioni di modifica.
     @throws std::runtime_error Se l'array non è stato inizializzato.
     */
private:
    bool removeOne(const T& remove) {
        if (array == nullptr) {
            throw std::runtime_error("Array is not initialized in removeOne.");
        }

        for (size_t i = 0; i < arraySize; ++i) {
            if (array[i] == remove) {
                for (size_t j = i; j < arraySize - 1; ++j) {
                    array[j] = array[j + 1];
                }
                --arraySize;
                if (arraySize == 0) {
                    clear();
                }
                return true; // Elemento trovato
            }
        }

        return false;  // Elemento non trovato
    }
    
public:
    
    /**
     @brief Svuota l'array.
     Rimuove tutti gli elementi dall'array e lo resetta alla sua dimensione minima.
     Se la dimensione dell'array diventa zero, l'array viene impostato su `nullptr`.
     @throws std::runtime_error Se si verifica un errore nell'allocazione della memoria durante la modifica dell'array.
     */
    void clear() {
        if (array) {
            try {
                delete[] array;  // Dealloca l'array
            } catch (const std::bad_alloc& e) {
                throw std::runtime_error("Memory allocation failed while clearing the array.");
            }
            array = nullptr;
        }
        arraySize = 0;
        assert(array == nullptr && arraySize == 0);
    }
    
    /**
     @brief Scambia i membri tra due array ordinati.
     Questa funzione scambia i dati (array, dimensione minima e dimensione dell'array) tra due oggetti `SortedArray`.
     Viene utilizzato il meccanismo di `std::swap` per ciascun membro coinvolto.
     @param first Il primo array ordinato da scambiare.
     @param second Il secondo array ordinato da scambiare.
     @post Dopo l'esecuzione della funzione, i membri dell'oggetto `first` e dell'oggetto `second` sono stati scambiati.
     @note Questa funzione è dichiarata come `friend` per poter accedere direttamente ai membri privati degli oggetti `SortedArray`.
     */
    friend void swap(SortedArray& first, SortedArray& second){
        std::swap(first.array, second.array);
        std::swap(first.arraySize, second.arraySize);
        std::swap(first.compare, second.compare);
    }
    
    /**
     @brief Scambia i membri tra due array ordinati.
     Questa funzione scambia i dati (array, dimensione minima e dimensione dell'array) tra due oggetti `SortedArray`.
     Viene utilizzato il meccanismo di `std::swap` per ciascun membro coinvolto.
     @param other Oggetto `SortedArray` da scambiare con l'oggetto corrente.
     @post Dopo l'esecuzione della funzione, i membri dell'oggetto corrente e di `other` sono stati scambiati.
     @note Questa funzione scambia direttamente i membri privati dell'oggetto.
     @throws std::runtime_error Se si verifica un errore durante lo scambio delle risorse.
     */
    void swap(SortedArray& other) {
        try {
            std::swap(array, other.array);
            std::swap(arraySize, other.arraySize);
            std::swap(compare, other.compare);
        } catch (const std::exception& e) {
            throw std::runtime_error("Error occurred during swap: " + std::string(e.what()));
        }
    }
    
    //annidata così questi iteratori possono essere usati soltanto all'interno del sortedArray
    
    /**
     * @brief Forward declarations per gli iteratori.
     *
     * Le classi `const_iterator` e `iterator` sono dichiarate in anticipo per consentire il loro utilizzo all'interno
     * della classe `SortedArray`.
     * - `const_iterator` permette di scorrere gli elementi dell'array senza modificarli.
     * - `iterator` permette di scorrere e modificare gli elementi dell'array.
     */
    friend class const_iterator;
    
    /**
     @brief Classe iteratori costanti per l'array ordinato.
     Classe di iteratori che permette di scorrere gli elementi dell'array senza modificarli.
     */
    class const_iterator {
    public:
        //traits
        /**
         @brief Categoria dell'iteratore.
         Indica che questo iteratore supporta l'accesso casuale agli elementi dell'array.
         */
        typedef std::random_access_iterator_tag iterator_category;
        typedef T value_type;                                       ///< Tipo di valore puntato
        typedef ptrdiff_t difference_type;                          ///< Tipo per rappresentare la differenza tra due iteratori
        /**
         @brief Puntatore al tipo degli elementi.
         Puntatore costante a `T`, in modo che non possa essere modificato tramite l'iteratore.
         */
        typedef const T* pointer;
        /**
         @brief Riferimento al tipo degli elementi.
         Riferimento costante a `T`, in modo che non possa essere modificato tramite l'iteratore.
         */
        typedef const T& reference;
        
        friend class SortedArray<T, Comparator>; ///< Forward definition di sortedArray
        
    private:
        const T* ptr;  ///< Puntatore all'elemento corrente nell'array.
        const SortedArray<T, Comparator>* array; ///< Puntatore all'array.
        
        /**
         @brief Costruttore privato.
         Costruttore privato per un iteratore costante.
         @param arr Array dell'iteratore.
         @param p Elemento dell'iteratore.
         @note Questo costruttore può essere soltanto usato all'interno della classe `const_iterator`.
         */
        explicit const_iterator(const SortedArray<T, Comparator>* arr, const T* p) : ptr(arr ? p : nullptr), array(arr) {
        }
        
    public:
        /**
         @brief Costruttore di default.
         Crea un nuovo iteratore costante che punta a `nullptr`.
         */
        const_iterator() : ptr(nullptr), array(nullptr) {}
        
        /**
         @brief Costruttore di copia.
         Crea un nuovo iteratore costante che punta allo stesso elemento a cui punta `other`.
         @param other Altro iteratore costante.
         @post Viene creato un nuovo iteratore costante, quello passato come parametro rimane intatto.
         */
        const_iterator(const const_iterator& other) : ptr(other.ptr), array(other.array) {
        }
        
        /**
         @brief Operatore di assegnamento.
         Assegna il contenuto di un altro iteratore a questo.
         @param other Iteratore costante da assegnare.
         @return Riferimento a questo iteratore dopo l'assegnazione.
         @note Viene verificato se non si sta assegnando lo stesso iteratore, evitando quindi l'auto-assegnamento.
         */
        const_iterator& operator=(const const_iterator& other) {
            if (this != &other) {
                ptr = other.ptr;
                array = other.array;
            }
            return *this;
        }
        
        /**
         @brief Operatore di dereferenziamento.
         Restituisce una referenza al valore puntato dall'iteratore; l'elemento a cui punta non può essere modificato.
         @return Una referenza al valore putnato dall'iteratore.
         @throws std::runtime_error se si tenta di dereferenziare un iteratore non valido.
         */
        reference operator*() const {
            if (!ptr) {
                throw std::runtime_error("Trying to dereference an invalid iterator (nullptr).");
            }
            return *ptr;
        }
        
        /**
         @brief Operatore di accesso.
         Permette l'accesso ai membri dell'oggetto puntato dall'iteratore, come se fosse un puntatore.
         @return Un puntatore costante al tipo degli elementi
         */
        pointer operator->() const {
            return ptr;
        }
        
        /**
         @brief Operatore di pre-incremento.
         Incrementa l'iteratore, facendo avanzare di una posizione nell'array.
         @return Una referenza all'iteratore dopo l'incremento.
         @throws std::out_of_range se si tenta di accedere a un indice invalido.
         */
        const_iterator& operator++() {
            if (!ptr)
                throw std::out_of_range("Trying to dereference an invalid iterator (nullptr).");
            ++ptr;
            return *this;
        }
        
        /**
         @brief Operatore di post-incremento.
         Incrementa l'iteratore, facendo avanzare di una posizione nell'array.
         @return L'iteratore prima dell'incremento.
         @throws std::out_of_range se si tenta di accedere a un indice invalido.
         */
        const_iterator operator++(int) {
            if (!ptr)
                throw std::out_of_range("Trying to increment an invalid iterator (nullptr).");
            const_iterator tmp = *this;
            ++ptr;
            return tmp;
        }
        
        /**
         @brief Operatore di pre-decremento.
         Decrementa l'iteratore, facendo retrocedere di una posizione nell'array.
         @return Una referenza all'iteratore dopo il decremento.
         @throws std::out_of_range se si tenta di retrocedere prima dell'inizio dell'array.
         */
        const_iterator& operator--(){
            if (ptr == array->array)
                throw std::out_of_range("Decrementing beyond the beginning of the array.");
            --ptr; return *this;
        }
        
        /**
         @brief Operatore di post-decremento.
         Decrementa l'iteratore, facendo retrocedere di una posizione nell'array.
         @return L'iteratore prima del decremento.
         @throws std::out_of_range se si tenta di accedere a un indice invalido.
         */
        const_iterator operator--(int){
            if (ptr == array->array)
                throw std::out_of_range("Decrementing beyond the beginning of the array.");
            const_iterator tmp = *this;
            --ptr;
            return tmp;
        }
        
        /**
         @brief Restituisce un iteratore che punta al primo elemento dell'array.
         @return Un iteratore costante che punta al primo elemento dell'array.
         */
        const_iterator begin() const{
            return const_iterator(array, array ? array->array : nullptr);
        }
        
        /**
         @brief Restituisce un iteratore che punta all'ultimo elemento dell'array.
         @return Un iteratore costante che punta all'ultimo elemento dell'array.
         */
        const_iterator end() const {
            return const_iterator(array,
                                  array ? array->array + array->size() : nullptr);
        }
        
        /**
         @brief Operatore di uguaglianza.
         Confronta due iteratori per determinare se puntano alla stessa posizione nell'array.
         @param other L'altro iteratore da confrontare.
         @return `true` se gli iteratori sono uguali, altrimenti `false`.
         @throws std::runtime_error se si tenta di confrontare due iteratori appartenenti a diversi array.
         */
        bool operator==(const const_iterator& other) const {
            if (array != other.array)
                throw std::runtime_error("Comparing iterators from different arrays.");
            return ptr == other.ptr;
        }
        
        /**
         @brief Operatore di disuguaglianza.
         Confronta due iteratori per determinare se non puntano alla stessa posizione.
         @param other L'altro iteratore da confrontare.
         @return `true` se gli iteratori sono diversi, altrimenti `false`.
         */
        bool operator!=(const const_iterator& other) const {
            return !(*this == other);
        }
        
        /**
         @brief Operatore di somma.
         Aggiunge una certa distanza all'iteratore.
         @param n La distanza da sommare all'iteratore.
         @return Un iteratore che punta alla posizione risultante.
         @throws std::out_of_range se si tenta di andare ad un indice non valido.
         */
        const_iterator operator+(difference_type n) const {
            if (ptr + n < array->array || ptr + n >= array->array + array->size())
                throw std::out_of_range("Iterator addition out of bounds.");
            return const_iterator(array, ptr + n);
        }
        
        /**
         @brief Operatore di sottrazione.
         Sottrae una certa distanza dall'iteratore.
         @param n La distanza da sottrarre all'iteratore.
         @return Un iteratore che punta alla posizione risultante.
         @throws std::out_of_range se si tenta di andare ad un indice non valido.
         */
        const_iterator operator-(difference_type n) const {
            if (ptr - n < array->array || ptr - n >= array->array + array->size())
                throw std::out_of_range("Iterator subtraction out of bounds.");
            
            // Restituisci un nuovo iteratore con il puntatore aggiornato
            return const_iterator(array, ptr - n);
        }
        
        /**
         @brief Operatore di sottrazione per il calcolo della distanza.
         Calcola la distanza, in termini di numero di elementi, tra due iteratori.
         @param other L'altro iteratore da cui calcolare la distanza.
         @return La distanza tra i due iteratori.
         @throw std::runtime_error se si tenta di calcolare la distanza usando due iteratori da due array diversi.
         */
        difference_type operator-(const const_iterator& other) const {
            if (array != other.array)
                throw std::runtime_error("Subtracting iterators from different arrays.");
            return ptr - other.ptr;
        }
        
        /**
         @brief Operatore di addizione assegnata.
         Aggiunge una certa distanza all'iteratore, avanzando di `n` posizioni.
         @param n La distanza da sommare all'iteratore. Può essere sia positiva che negativa.
         @return Una referenza all'iteratore stesso dopo l'operazione di addizione.
         @throws std::out_of_range se si tenta di accedere a un indice invalido.
         */
        const_iterator& operator+=(difference_type n) {
            if (ptr + n < array->array || ptr + n >= array->array + array->size())
                throw std::out_of_range("Iterator addition out of bounds.");
            ptr += n;
            return *this;
        }

        /**
         @brief Operatore di sottrazione assegnata.
         Aggiunge una certa distanza all'iteratore, retrocedendo di `n` posizioni.
         @param n La distanza da sottrarre all'iteratore. Può essere sia positiva che negativa.
         @return Una referenza all'iteratore stesso dopo l'operazione di sottrazione.
         @throws std::out_of_range se si tenta di accedere a un indice invalido.
         */
        const_iterator& operator-=(difference_type n) {
            if (ptr - n < array->array || ptr - n >= array->array + array->size())
                throw std::out_of_range("Iterator subtraction out of bounds.");
            ptr -= n;
            return *this;
        }
    };
    
    /**
     @brief Restituisce un iteratore costante all'inizio dell'array ordinato.
     @return `const_iterator` Iteratore costante alla prima posizione dell'array, oppure un iteratore nullo se l'array è vuoto.
     */
    const_iterator begin() const {
        return const_iterator(this, array ? array : nullptr);
    }

    /**
     @brief Restituisce un iteratore costante alla fine dell'array ordinato.
     L'iteratore restituito punta alla posizione successiva all'ultimo elemento valido dell'array.
     @return `const_iterator` Iteratore costante alla posizione di fine array, oppure un iteratore nullo se l'array è vuoto.
     */
    const_iterator end() const {
        return const_iterator(this, array ? array + size() : nullptr);
    }
};

/**
 @brief Filtra gli elementi di un array ordinato in base a un funtore.
 Filtra gli elementi di un array ordinato in base a una condizione definita da un funtore.
 @tparam F Tipo del funtore utilizzato per filtrare gli elementi. Il funtore deve essere un predicato che accetta un elemento di tipo T e restituisce un valore booleano.
 @tparam T Tipo degli elementi contenuti nell'array.
 @tparam Comparator Tipo del comparatore utilizzato per mantenere l'ordine degli elementi nell'array.
 @param array L'array ordinato su cui applicare il filtro.
 @param functor Il funtore che definisce la condizione di filtro.
 @return Un nuovo array ordinato contenente solo gli elementi che soddisfano la condizione definita dal funtore.
 @throws std::bad_alloc Se non è possibile allocare memoria per il nuovo array o per l'inserimento di nuovi elementi.
 @throws std::exception Se si verifica un errore generico durante l'inserimento di un elemento nell'array.
 @throws ... Per eventuali altre eccezioni non specificate che possono essere sollevate da `insert()`.

 */
template <typename F, typename T, typename Comparator>
SortedArray<T, Comparator> filter(const SortedArray<T, Comparator>& array,
                                  F functor) {
    SortedArray<T, Comparator> result;

    // Aggiungi solo gli elementi che soddisfano il predicato
    for (size_t i = 0; i < array.size(); ++i) {
        if (functor(array[i])) {
            try {
                result.insert(array[i]);
            } catch (const std::bad_alloc& e ) {
                std::cerr << "Memory error - " << e.what() << std::endl;
            } catch (const std::exception& e) {
                std::cerr << "Error - " << e.what() << std::endl;
            } catch (...) {
                std::cerr << "Error." << std::endl;
            }
        }
    }

    return result;
}

/**
 @brief Overloading dell'operatore di stampa.
 Permette di stampare un oggetto di tipo `SortedArray` su uno stream di output. Gli elementi dell'array vengono stampati in formato di lista, separati da virgole e racchiusi tra parentesi quadre.
 @param os Lo stream di output su cui stampare l'array.
 @param arr L'array ordinato da stampare.
 @return Lo stream di output dopo l'inserimento dell'array.
 */
template <typename T, typename Comparator>
std::ostream& operator<<(std::ostream& os,
                         const SortedArray<T, Comparator>& arr) {
    os << "[";
    for (size_t i = 0; i < arr.size(); ++i) {
        os << arr[i];
        if (i < arr.size() - 1) {
            os << ", ";
        }
    }
    os << "]";
    return os;
}
#endif
