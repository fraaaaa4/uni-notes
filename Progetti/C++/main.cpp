#include <iostream>
#include <cassert>
#include <stdexcept>
#include <vector>
#include "sortedArray.hpp"

using namespace std;

//funzioni di comparazione personalizzate - ordinamento decrescente ed ascendente
struct lessFunc {
    template <typename T>
    bool operator()(const T& lhs, const T& rhs) const {
        return lhs < rhs;
    }
};

struct greaterFunc {
    template <typename T>
    bool operator()(const T& lhs, const T& rhs) const {
        return lhs > rhs;
    }
};

//Struct di un custom type con gli operatori necessari
struct Point {
    int x;
    int y;
    
    // Operator <: sort first by x, then by y
    bool operator<(const Point& other) const {
        if (x == other.x)
            return y < other.y;
        return x < other.x;
    }
    
    // Operator >: OPZIONALE - necessario in questo test solo per greaterFunc
    bool operator>(const Point& other) const {
        if (x == other.x) return y > other.y;
        return x > other.x;
    }
    
    //costruttore di copia - OPZIONALE
    Point(const Point& other) : x(other.x), y(other.y) {}
    
    // Costruttore
        Point(int x = 0, int y = 0) : x(x), y(y) {}

        // Operatore di assegnazione
        Point& operator=(const Point& other) {
            if (this != &other) { // Controllo per evitare l'auto-assegnazione
                x = other.x;
                y = other.y;
            }
            return *this;
        }
    
    // Operator ==: equality comparison
    bool operator==(const Point& other) const {
        return (x == other.x) && (y == other.y);
    }
    
     // Operator !=: equality comparison
    bool operator!=(const Point& other) const {
        return !(*this == other);
    }
    
    // Operator<< : OPZIONALE - necessario in questo test solo per comodità
    friend std::ostream& operator<<(std::ostream& os, const Point& p) {
        os << "Point(" << p.x << ", " << p.y << ")";
        return os;
    }
    
    int getX() const{ return x; }
    int getY() const{ return y; }
};

struct IsEven {
    bool operator()(int x) const { return (x % 2) == 0; }
};

struct IsOdd {
    bool operator()(int x) const { return (x % 2) != 0; }
};

struct AlwaysTrue {
    bool operator()(int /*x*/) const { return true; }
};

struct AlwaysFalse {
    bool operator()(int /*x*/) const { return false; }
};

struct PointXGreaterThan {
    int threshold;
    PointXGreaterThan(int t) : threshold(t) {}
    bool operator()(const Point& p) const { return p.x > threshold; }
};

//funzione di comparazione "complessa"
//ordinamento crescente per i pari, decrescente per i dispari
struct intOrder {
    template <typename T>
    bool operator()(const T& a, const T& b) const {
        // Se entrambi sono pari
        if (a % 2 == 0 && b % 2 == 0) {
            return a < b; // Ordinamento crescente per i pari
        }
        // Se entrambi sono dispari
        if (a % 2 != 0 && b % 2 != 0) {
            return a > b; // Ordinamento decrescente per i dispari
        }
        return a % 2 == 0;
    }
};

//ordinamento decrescente per i pari, ordinamento crescente per i dispari
struct _intOrder {
    template <typename T>
    bool operator()(const T& a, const T& b) const {
        // Se entrambi sono pari
        if (a % 2 == 0 && b % 2 == 0) {
            return a > b; // Ordinamento decrescente per i pari
        }
        // Se entrambi sono dispari
        if (a % 2 != 0 && b % 2 != 0) {
            return a < b; // Ordinamento crescente per i dispari
        }
        return a % 2 != 0; // I numeri dispari vengono prima
    }
};

/**
 @brief Funzione per verificare che l'array sia ordinato in modo crescente.
 @note Piuttosto che usare un tipo di ritorno, usiamo `assert`. Questo serve soltanto per facilitare i test, visto che gli elementi che inseriamo e il comparatore che usiamo dovrebbe ordinare l'array in modo crescente.
 */
void verify_order(const SortedArray<int, lessFunc>& arr){
    for (size_t i = 0; i < arr.size() - 1; ++i)
        assert(arr[i] <= arr[i+1]);
}

/**
 @brief Funzione per verificare che l'array sia ordinato in ordine decrescente.
 @note Questa funzione serve soltanto per testare più facilmente l'uso del comparatore personalizzato `greaterFunc()`.
 */
void verify_order_desc(const SortedArray<int, greaterFunc>& arr){
    for (size_t i = 0; i < arr.size() - 1; ++i)
        assert(arr[i] >= arr[i+1]);
}

/**
 @brief Test del costruttore dell'array ordinato.
 Viene testato il costruttore di default, il costruttore di copia, il costruttore a partire da una sequenza di dati rappresentata da due iteratori generici, il costruttore a partire da un altro `SortedArray` definito su dati di tipo diverso.
 */
void test_constructor(){
    SortedArray<int, lessFunc> arr;
    
    cout << " [TEST CONSTRUCTOR] Verifica se un nuovo array è effettivamente vuoto" << endl;
    assert(arr.isEmpty());
    assert(arr.size() == 0);
    cout << "  Prova di stampa array vuoto: " << arr << " Expected: []." << endl;
    cout << "   Verifica nuovo array OK" << endl;
    
    SortedArray<int, lessFunc> arr1;
    arr1.insert(1); arr1.insert(3); arr1.insert(2);
    cout << " [END] Test costruttore di default OK." << endl;
    
    //copy constructor
    cout << endl << " [TEST CONSTRUCTOR] Test costruttore di copia" << endl;
    cout << "  >> Stesso tipo di dati, stesso comparatore." << endl;
    SortedArray<int, lessFunc> arr2 = arr1;
    cout << "   La dimensione tra i due array deve essere la stessa. arr2.size: " << arr2.size() << ", arr1.size: " << arr1.size() << " . Expected: 3." << endl;
    cout << "   Gli elementi nei due array devono essere gli stessi. arr2: " << arr2 << ", arr1: " << arr1 << ". Expected: [1, 2, 3]. " << endl;
    assert(arr2.size() == 3);
    assert(arr2[0] == 1); assert(arr2[1] == 2); assert(arr2[2] == 3);
    cout << "  >>> [END] Stesso tipo di dati, stesso comparatore OK." << endl;
    
    //copy constructor con tipo diverso, stesso comparatore
    cout << "  >> Diverso tipo di dati, stesso comparatore." << endl;
    SortedArray<double, lessFunc> arrD;
    arrD.insert(1.1);
    arrD.insert(2.2);
    
    SortedArray<int, lessFunc> arrI(arrD);
    cout << "   La dimensione tra i due array deve essere la stessa. arrD.size: " << arrD
        .size() << ", arrI.size: " << arrI.size() << " . Expected: 2" << endl;
    cout << "   Gli elementi tra i due array devono essere gli stessi. Convertiti da tipo double a tipo int. arrD: " 
    << arrD << ", arrI: " << arrI << ". Expected: arrD = [1.1, 2.2], arrI = [1, 2]." << endl;
    assert(arrI.size() == 2); assert(arrD.size() == 2);
    assert(arrI[0] == 1); assert(arrI[1] == 2);
    assert(arrD[0] == 1.1); assert(arrD[1] == 2.2);
    cout << "  >>> [END] Diverso tipo di dati, stesso comparatore OK." << endl;
    
    //copy constructor con stesso tipo, diverso comparatore
    cout << "  >> Stesso tipo di dati, diverso comparatore." << endl;
    //riuso l'array creato prima
    SortedArray<int, greaterFunc> _arrI(arrI);
    cout << "   La dimensione tra i due array deve essere la stessa. _arrI.size: " << _arrI.size() << ", arrI.size: " << arrI.size() << ". Expected: 2";
    cout << "   Gli elementi tra i due array devono essere gli stessi, ordinato in modo diverso. arrI: " << arrI << ", _arrI: " << _arrI << ". Expected arrI: [1, 2], _arrI = [2, 1]";
    assert(arrI.size() == 2); assert(_arrI.size() == 2);
    assert(arrI[0] == 1); assert(arrI[1] == 2);
    assert(_arrI[0] == 2); assert(_arrI[1] == 1);
    cout << "  >>> [END] Stesso tipo di dati, diverso comparatore OK." << endl;
    
    //copy constructor con tipo diverso, comparatore diverso
    cout << "  >> Diverso tipo di dati, diverso comparatore." << endl;
    SortedArray<double, greaterFunc> arrTest2;
    arrTest2.insert(3.14); arrTest2.insert(1.17); arrTest2.insert(8.18);
    SortedArray<int, lessFunc> arrTest3(arrTest2);
    cout << "   La dimensione tra i due array deve essere la stessa. arrTest2 size: " << arrTest2.size() << " , arrTest3 size: " << arrTest3.size() << ". Expected: 3." << endl;
    cout << "   Gli elementi tra i due arrai devono essere gli stessi, ordinamento diverso. Convertiti da tipo double a tipo int. arrTest2: "
    << arrTest2 << ", arrTest3: " << arrTest3 << ". Expected: arrTest2 = [8.18, 3.14, 1.17], arrTest3 = [1, 3, 8]." << endl;
    assert(arrTest2.size() == 3); assert(arrTest3.size() == 3);
    assert(arrTest2[0] == 8.18); assert(arrTest2[1] == 3.14); assert(arrTest2[2] == 1.17);
    assert(arrTest3[0] == 1); assert(arrTest3[1] == 3); assert(arrTest3[2] == 8);
    cout << "  >>> [END] Diverso tipo di dati, diverso comparatore OK." << endl;
    
    //copy constructor con funzione di comparazione complessa e diversa
    cout << "  >> Stesso tipo di dati, diverso comparatore con funzione di comparazione complessa." << endl;
    SortedArray<int, intOrder> compArr;
    compArr.insert(47); compArr.insert(83); compArr.insert(72); compArr.insert(12); compArr.insert(11); compArr.insert(95); compArr.insert(60); compArr.insert(19);
    compArr.insert(9); compArr.insert(35);
    SortedArray<int, _intOrder> _compArr(compArr);
    cout << "   La dimensione tra i due array deve essere la stessa. compArr size: " << compArr.size() << " , _compArr size: " << _compArr.size() << ". Expected: 10." << endl;
    cout << "   Gli elementi tra i due array devono essere gli stessi, ordinamento diverso. Primo array: ordine crescente per i pari, decrescente per i dispari." <<
    " Secondo array: ordine decrescente per i pari, crescente per i dispari." << endl
    << "    compArr: " << compArr << ", _compArr: " << _compArr << endl;
    assert(compArr.size() == 10); assert(_compArr.size() == 10);
    
    std::vector<int> expectedValues = {12, 60, 72, 95, 83, 47, 35, 19, 11, 9};
    for (size_t i = 0; i < expectedValues.size(); ++i)
        assert(compArr[i] == expectedValues[i]);
    
    expectedValues = {9, 11, 19, 35, 47, 83, 95, 72, 60, 12};
    for (size_t i = 0; i < expectedValues.size(); ++i)
        assert(_compArr[i] == expectedValues[i]);
    
    cout << "  >>> [END] Copy constructor con funzione di comparazione complessa e diversa OK." << endl;
    
    //copy constructor con tipi complessi
    cout << "  >> Stesso tipo complesso di dati, diverso comparatore." << endl;
    SortedArray<std::string, lessFunc> arrStr;
    arrStr.insert("abcd"); arrStr.insert("pkg"); arrStr.insert("qwerty");
    SortedArray<std::string, greaterFunc> _arrStr(arrStr);
    cout << "   La dimensione tra i due array deve essere la stessa. arrStr size: " << arrStr.size() << " , _arrStr size: " << arrStr.size() << ". Expected: 3." << endl;
    cout << "   Gli elementi tra i due array devono essere gli stessi, ordinamento diverso. Primo array ascendente, secondo array discendente. arrStr: " << arrStr << ", _arrStr: " << _arrStr << "." << endl;
    assert(arrStr.size() == 3); assert(_arrStr.size() == 3);
    assert(arrStr[0] == "abcd"); assert(arrStr[1] == "pkg"); assert(arrStr[2] == "qwerty");
    assert(_arrStr[0] == "qwerty"); assert(_arrStr[1] == "pkg"); assert(_arrStr[2] == "abcd");
    cout << "  >>> [END] Copy constructor con tipo complesso di dati, diverso comparatore OK." << endl;
    
    //copy constructor con struct
    cout << "  >> Stesso tipo di dati a partire da struct, diverso comparatore." << endl;
    SortedArray<Point, lessFunc> pArr1;
    pArr1.insert(Point(2,3)); pArr1.insert(Point(4,9)); pArr1.insert(Point(1,0));
    SortedArray<Point, greaterFunc> pArr2(pArr1);
    cout << "   La dimensione tra i due array deve essere la stessa. pArr1 size: " << pArr1.size() << " , pArr2 size: " << pArr2.size() << ". Expected: 3." << endl;
    cout << "   Gli elementi tra i due array devono essere gli stessi, ordinamento diverso. pArr1: " << "[" << pArr1[0].x << "," << pArr1[0].y << "], [" << pArr1[1].x << "," << pArr1[1].y << "], [" << pArr1[2].x << "," << pArr1[2].y << "]" << " , pArr2: " << "[" << pArr2[0].x << "," << pArr2[0].y << "], [" << pArr2[1].x << "," << pArr2[1].y << "], [" << pArr2[2].x << "," << pArr2[2].y << "]" << endl;
    Point p1(2,3); Point p2(4,9); Point p3(1,0);
    assert(pArr1.size() == 3); assert(pArr2.size() == 3);
    assert(pArr1[0] == p3); assert(pArr1[1] == p1); assert(pArr1[2] == p2);
    assert(pArr2[0] == p2); assert(pArr2[1] == p1); assert(pArr2[2] == p3);
    cout << "  >>> [END] Copy constructor con tipo struct, diverso comparatore OK." << endl;
    
    cout << endl;
    
    //iterator constructor
    cout << " [TEST CONSTRUCTOR] Test costruttore a partire da sequenza di dati con iteratori" << endl;
    vector<int> vec = {4,3,1,2};
    SortedArray<int, lessFunc> arr3(vec.begin(), vec.end());
    cout << "  La dimensione deve essere uguale alla dimensione del vettore. vec.size: " << vec.size() << " , arr3.size: " << arr3.size() << ". Expected: 4" << endl;
    cout << "  Gli elementi tra il vector e l'array devono essere gli stessi, ma ordinati ascendente nell'array. vec: " << "[" << vec[0] << ", " << vec[1] << ", " << vec[2] << ", " << vec[3] << "]" << " , arr3: " << arr3 << endl;
    assert(arr3.size() == 4);
    assert(arr3[0] == 1); assert(arr3[1] == 2);
    assert(arr3[2] == 3); assert(arr3[3] == 4);
    
    cout << " [END] Test costruttore a partire da sequenza di dati con iteratori OK." << endl;
    
    cout << "[TEST END] Test costruttori OK." << endl;

}

/**
 @brief Test dell'operatore di assegnazione.
 Vengono testati vari assegnamenti tra due array, in base al tipo di dati e comparatori. Viene testato poi l'autoassegnamento.
 */
void test_assign(){
    //assegnamento tra due array con stesso tipo e comparatore
    cout << "  >> Assegnamento tra due array con stesso tipo e comparatore." << endl;
    SortedArray<int, lessFunc> arr1;
    arr1.insert(1); arr1.insert(3); arr1.insert(2);
    
    SortedArray<int, lessFunc> arr2;
    arr2 = arr1;
    cout << "   La dimensione tra i due array deve essere la stessa. arr1.size: " << arr1.size() << ", arr2.size: " << arr2.size() << ". Expected: 3." << endl;
    cout << "   Gli elementi nei due array devono essere gli stessi. arr1: " << arr1 << ", arr2: " << arr2 << " . Expected: [1, 2, 3]." << endl;
    assert(arr1.size() == 3); assert(arr2.size() == 3);
    std::vector<int> expected = {1,2,3};
    for (size_t i = 0; i < arr1.size(); ++i){
        assert(arr1[i] == arr2[i]);
        assert(arr1[i] == expected[i]);
        assert(arr2[i] == expected[i]);
    }
    cout << "  >>> [END] Assegnamento tra due array con stesso tipo e comparatore OK." << endl;
    
    //assegnamento tra due array con stesso tipo e comparatore diverso
    cout << "  >> Assegnamento tra due array con stesso tipo e comparatore diverso." << endl;
    SortedArray<int, greaterFunc> arr3;
    arr3 = arr1;
    cout << "   La dimensione tra i due array deve essere la stessa. arr1.size: " << arr1.size() << ", arr3.size: " << arr3.size() << ". Expected: 3." << endl;
    cout << "   Gli elementi nei due array devono essere gli stessi, ma ordinati decrescente in arr3, ascendente in arr1. arr1: " << arr1 << ", arr3: " << arr3 << "." << endl;
    assert(arr1.size() == 3); assert(arr3.size() == 3);
    std::vector<int> _expected = {3,2,1};
    for (size_t i = 0; i < arr1.size(); ++i){
        assert(arr1[i] == expected[i]);
        assert(arr3[i] == _expected[i]);
    }
    cout << "  >>> [END] Assegnamento tra due array con stesso tipo e comparatore diverso OK." << endl;
    
    //assegnamento tra due array con tipo diverso e stesso comparatore
    cout << "  >> Assegnamento tra due array con tipo diverso e stesso comparatore." << endl;
    SortedArray<double, lessFunc> arr4;
    arr4.insert(9.19); arr4.insert(6.21); arr4.insert(3.13);
    arr1 = arr4;
    cout << "   La dimensione tra i due array deve essere la stessa. arr1.size: " << arr1.size() << ", arr4.size: " << arr4.size() << ". Expected: 3." << endl;
    cout << "   Gli elementi all'interno di arr1 devono essere gli elementi di arr4, trasformati da double ad int, in ordine ascendente. arr4: " << arr4 << ", arr1: " << arr1 << "." << endl;
    assert(arr1.size() == arr4.size());
    expected = {3, 6, 9};
    std::vector<double> dexpected = {3.13, 6.21, 9.19};
    for (size_t i = 0; i < arr1.size(); ++i){
        assert(arr1[i] == expected[i]);
        assert(arr4[i] == dexpected[i]);
    }
    cout << "  >>> [END] Assegnamento tra due array con tipo diverso e stesso comparatore OK." << endl;
    
    //assegnamento tra due array con tipo diverso e comparatore
    cout << "  >> Assegnamento tra due array con tipo diverso e comparatore." << endl;
    SortedArray<int, greaterFunc> arr5;
    arr5 = arr4;
    cout << "   La dimensione tra i due array deve essere la stessa. arr4.size: " << arr4.size() << ", arr5.size: " << arr5.size() << ". Expected: 3." << endl;
    cout << "   Gli elementi all'interno di arr5 devono essere gli elementi di arr4, trasformati da double ad int, in ordine decrescente. arr4: " << arr4 << ", arr5: " << arr5 << "." << endl;
    assert(arr5.size() == arr4.size());
    expected = {9, 6, 3};
    for (size_t i = 0; i < arr5.size(); ++i){
        assert(arr5[i] == expected[i]);
        assert(arr4[i] == dexpected[i]);
    }
    cout << "  >>> [END] Assegnamento tra due array con tipo diverso e diverso comparatore OK." << endl;
    
    //assegnamento tra due array con tipi composti e diversi comparatori
    cout << "  >> Assegnamento tra due array con tipi composti e diversi comparatori" << endl;
    SortedArray<string, lessFunc> strArr;
    strArr.insert("wow"); strArr.insert("qwerty"); strArr.insert("edm");
    SortedArray<string, greaterFunc> _strArr;
    _strArr = strArr;
    cout << "   La dimensione tra i due array deve essere la stessa. strArr size: " << strArr.size() << " , _strArr size: " << _strArr.size() << ". Expected 3." << endl;
    cout << "   Gli elemento all'interno di _strArr devono essere gli elementi di arr4, ma ordinati in modo diverso. strArr: " << strArr << ", _strArr: " << _strArr << "." << endl;
    assert(strArr.size() == _strArr.size());
    assert(strArr[0] == "edm"); assert(strArr[1] == "qwerty"); assert(strArr[2] == "wow");
    assert(_strArr[0] == "wow"); assert(_strArr[1] == "qwerty"); assert(_strArr[2] == "edm");
    cout << "  >>> [END] Assegnamento tra due array con tipi composti e diverso comparatore OK." << endl;
    
    //autoassegnamento
    cout << "  >> Autoassegnamento" << endl;
    arr5 = arr5;
    cout << "  La dimensione di arr5 dovrebbe essere rimasta invariata. arr5 size: " << arr5.size() << ". Expected 3." << endl;
    cout << "  Gli elementi all'interno di arr5 dovrebbero essere rimasti invariati. arr5: " << arr5 << ". Expected [9,6,3]." << endl;
    assert(arr5.size() == 3);
    for (size_t i = 0; i < arr5.size(); ++i)
        assert(arr5[i] == expected[i]);
    cout << "  >>> [END] Autoassegnamento OK." << endl;
    
    cout << "[TEST END] Test operatore di assegnamento OK." << endl;
}

/**
 @brief Test di inserimento e rimozione per i tipi base.
 Vengono testati 4 array con i seguenti tipi di dati: int, double, char, boor. Viene testato l'inserimento, la rimozione, l'accesso, la ricerca, e la modifica.
 */
void test_base(){
    cout << " [TEST INSERT] Test inserimento di tipi base" << endl;
    SortedArray<int, lessFunc> arrInt; SortedArray<double, lessFunc> arrDouble; SortedArray<char, lessFunc> arrChar; SortedArray<bool, lessFunc> arrBool;
    
    //popolazione arrInt
    //due degli elementi inseriti devono essere uguali per provare a rimuovere elementi che sono uguali
    arrInt.insert(97); arrInt.insert(47); arrInt.insert(35); arrInt.insert(97);
    arrDouble.insert(84.42); arrDouble.insert(28.30); arrDouble.insert(66.17); arrDouble.insert(84.42);
    arrChar.insert('K'); arrChar.insert('h'); arrChar.insert('H'); arrChar.insert('K');
    arrBool.insert(false); arrBool.insert(true); arrBool.insert(false); arrBool.insert(false);
    
    cout << "   [NOTA] Per arrBool, '0' corrisponde a 'false', '1' corrisponde a 'true'." << endl;
    cout << "   arrInt: " << arrInt << ", arrDouble: " << arrDouble << ", arrChar: " << arrChar << ", arrBool: " << arrBool << "." << endl;
    cout << "   Gli array dovrebbero essere tutti ordinati in ordine ascendente (i char seguono l'ordine alfabetico con prima le maiuscole, per i bool prima i false, poi i true)." << endl;
    cout << "   La dimensione degli array dovrebbe essere 4. arrInt.size: " << arrInt.size() << " , arrDouble.size: " << arrDouble.size() << " , arrChar.size: " << arrChar.size() << " , arrBool.size: " << arrBool.size() << endl;
    assert(arrInt.size() == 4); assert(arrDouble.size() == 4); assert(arrChar.size() == 4); assert(arrBool.size() == 4);
    assert(arrInt[0] == 35); assert(arrInt[1] == 47); assert(arrInt[2] == 97); assert(arrInt[3] == 97);
    assert(arrDouble[0] == 28.3); assert(arrDouble[1] == 66.17); assert(arrDouble[2] == 84.42); assert(arrDouble[2] == 84.42);
    assert(arrChar[0] == 'H'); assert(arrChar[1] == 'K'); assert(arrChar[2] == 'K'); assert(arrChar[3] == 'h');
    assert(arrBool[0] == 0); assert(arrBool[1] == 0); assert(arrBool[2] == 0); assert(arrBool[3] == 1);
    cout << " [END] Test inserimento di tipi base all'interno di un array OK." << endl << endl;
    
    //Accesso con operator[] ad elementi dell'array
    cout << " [TEST ACCESS] Test operatore di accesso con tipi base" << endl;
    cout << "  >> Accesso al terzo elemento di ciascun arary. arrInt: " << arrInt[1] << ", arrDouble: " << arrDouble[1] << ", arrChar:" << arrChar[1] << ", arrBool: " << arrBool[1] << ". Expected: 47 , 66.17 , K , 0" << endl;
    assert(arrInt[1] == 47); assert(arrDouble[1] == 66.17); assert(arrChar[1] == 'K'); assert(arrBool[1] == 0);
    cout << "  >>> [END] Accesso al secondo elemento di ciascun array OK." << endl;
    
    //accesso al di fuori della dimensione dell'array - corretto se lancia un'eccezione
    try {
        cout << "  >> Accesso a un elemento dell'array al di fuori degli indici consentiti. arrInt: " << arrInt[5] << "." << endl;
    } catch (std::out_of_range& e){
        cout << endl <<  "   [EXCEPTION] Expected exception out_of_range catturata, lanciata dalla funzione. " << e.what() << endl;
    }
    cout << "  >>> [END] Accesso a un elemento dell'array al di fuori degli indici consentiti OK." << endl << endl;
    
    //accesso con funzioni top e _end per tipi base
    cout << " [TEST ACCESS] Test funzioni array (top ed end) con tipi base" << endl;
    cout << "  >> Accesso al primo elemento di ciascun array." << endl << "Primo elemento di arrInt: " << arrInt.top() << ", arrDouble: " << arrDouble.top() << ", arrChar: " << arrChar.top() << ", arrBool: " << arrBool.top() << ". Expected: 35, 28.30, H, 0." << endl;
    assert(arrInt.top() == 35); assert(arrDouble.top() == 28.30); assert(arrChar.top() == 'H'); assert(arrBool.top() == 0);
    cout << "  >>> [END] Accesso al primo elemento di ciascun array OK." << endl;
    
    cout << "  >> Accesso all'ultimo elemento di ciascun array. " << endl << "Ultimo elemento di arrInt: " << arrInt._end() << ", arrDouble: " << arrDouble._end() << ", arrChar: " << arrChar._end() << ", arrBool: " << arrBool._end() << ". Expected: 97, 84.42, h, 1. " << endl;
    assert(arrInt._end() == 97); assert(arrDouble._end() == 84.42); assert(arrChar._end() == 'h'); assert(arrBool._end() == 1);
    cout << "  >>> [END] Accesso all'ultimo elemento di ciascun array OK." << endl << endl;
    
    //ricerca di elementi all'interno dell'array
    cout << " [TEST ACCESS] Test ricerca di elementi all'interno dell'array" << endl;
    cout << "   [NOTA] 1 per true, 0 per false." << endl;
    cout << "   Verifica se certi elementi sono presenti all'interno dell'array." << endl;
    cout << "   [35] è presente in arrInt? " << arrInt.contains(35) << ". [12] è presente in arrInt? " << arrInt.contains(12) << ".";
    cout << "   [84.42] è presente in arrDouble? " << arrDouble.contains(84.42) << ". [2.02] è presente in arrDouble? " << arrDouble.contains(2.02) << ".";
    cout << "   ['H'] è presente in arrChar? " << arrChar.contains('H') << ". ['Q'] è presente in arrChar? " << arrInt.contains('Q') << "." << endl;
    assert(arrInt.contains(35) == true); assert(arrInt.contains(12) == false);
    assert(arrDouble.contains(84.42) == true); assert(arrDouble.contains(2.02) == false);
    assert(arrChar.contains('H') == true); assert(arrChar.contains('Q') == false);
    cout << "  >>> [END] Test ricerca di elementi all'interno dell'array OK." << endl << endl;
    
    //rimozione degli elementi
    cout << " [TEST REMOVE] Test rimozione di tipi base" << endl;
    //rimozione degli elementi di cui ce ne è uno solo nell'array
    arrInt.remove(47); arrDouble.remove(28.30); arrChar.remove('H'); arrBool.remove(true);
    cout << "  >> Rimozione elementi di cui ce ne è soltanto uno nell'array. Per ogni array ci deve essere quindi solo un elemento in meno." << endl;
    cout << "   La dimensione degli array dovrebbe essere 3. arrInt size: " << arrInt.size() << " , arrDouble size: " << arrDouble.size() << " , arrChar size: " << arrChar.size() << " , arrBool size: " << arrBool.size() << endl;
    cout << "   Gli elementi all'interno dell'array dovrebbero essere gli stessi, senza '47', '28.30', 'H' e 'true'. arrInt: " << arrInt << ", arrDouble: " << arrDouble << ", arrChar: " << arrChar << ", arrBool: " << arrBool << "." << endl;
    assert(arrInt.size() == 3); assert(arrDouble.size() == 3); assert(arrChar.size() == 3); assert(arrBool.size() == 3);
    assert(arrInt[0] == 35); assert(arrInt[1] == 97); assert(arrInt[2] == 97);
    assert(arrDouble[0] == 66.17); assert(arrDouble[1] == 84.42); assert(arrDouble[2] == 84.42);
    assert(arrChar[0] == 'K'); assert(arrChar[1] == 'K'); assert(arrChar[2] == 'h');
    assert(arrBool[0] == 0); assert(arrBool[1] == 0); assert(arrBool[2] == 0);
    cout << "  >>> [END] Rimozione elementi di cui ce ne è soltanto uno nell'array OK." << endl;
    
    
    //rimozione degli elementi di cui ce ne sono più di uno nell'array
    arrInt.remove(97); arrDouble.remove(84.42); arrChar.remove('K'); arrBool.remove(0);
    cout << "  >> Rimozione elementi di cui ce ne sono più di uno nell'array." << endl;
    cout << "   La dimensione degli array dovrebbe essere 1. arrInt size: " << arrInt.size() << ", arrDouble size: " << arrDouble.size() << ", arrChar size: " << arrChar.size() << ", arrBool size: " << arrBool.size() << endl;
    cout << "   Elementi all'interno dell'array - arrInt: " << arrInt << ", arrDouble: " << arrDouble << ", arrChar: " << arrChar << ", arrBool: " << arrBool << " - arrBool è vuoto: " << arrBool.isEmpty() << ", expected true. Expected elements: 35, 66.17, h, []." << endl;
    assert(arrInt.size() == 1); assert(arrDouble.size() == 1); assert(arrChar.size() == 1); assert(arrBool.size() == 0);
    assert(arrInt[0] == 35); assert(arrDouble[0] == 66.17); assert(arrChar[0] == 'h'); assert(arrBool.isEmpty());
    cout << "  >>> [END] Rimozione elementi di cui ce ne sono più di uno nell'array OK." << endl;
    
    //rimozione di elementi non esistenti nell'array
    cout << "  >> Rimozione elementi non presenti nell'array." << endl;
    cout << "   Risultato booleano di rimozione elementi non presenti - arrInt (prova con 500): " << arrInt.remove(500) << " , arrDouble (prova con 1.00):" << arrDouble.remove(1.00) << " , arrChar (prova con 'Z'): " << arrDouble.remove('Z') << " , arrBool (prova con 'false'): " << arrBool.remove(0) << ". Expected 0, 0, 0, 0." << endl;
    assert(arrInt.remove(500) == false); assert(arrDouble.remove(1.00) == false); assert(arrChar.remove('Z') == false); assert(arrBool.remove(0) == false);
    cout << "  >>> [END] Rimozione elementi non presenti nell'array OK." << endl;
    
    //test funzione clear
    cout << "  >> Funzione clear/isEmpty per gli array." << endl;
    arrInt.clear(); arrDouble.clear(); arrChar.clear(); arrBool.clear();
    cout << "   La dimensione di ciascun array dovrebbe essere 0. arrInt size: " << arrInt.size() << " , arrDouble size: " << arrDouble.size() << " , arrChar size: " << arrChar.size() << " , arrBool size:" << arrBool.size() << "." << endl;
    cout << "   Ogni array dovrebbe essere vuoto. arrInt: " << arrInt << " , arrDouble: " << arrDouble << " , arrChar: " << arrChar << " , arrBool: " << arrBool << ". Expected [], [], [], []." << endl;
    cout << "   Ogni array dovrebbe essere vuoto, con funzione isEmpty. arrInt: " << arrInt.isEmpty() << " , arrDouble: " << arrDouble.isEmpty() << " , arrChar: " << arrChar.isEmpty() << " , arrBool: " << arrBool.isEmpty() << ". Expected 1, 1, 1, 1." << endl;
    assert(arrInt.size() == 0); assert(arrDouble.size() == 0); assert(arrChar.size() == 0); assert(arrBool.size() == 0);
    assert(arrInt.isEmpty() == 1); assert(arrDouble.isEmpty() == 1); assert(arrChar.isEmpty() == 1); assert(arrBool.isEmpty() == 1);
    cout << "  >>> [END] Funzione clear/isEmpty per gli array OK." << endl << endl;
    
    //test top/_end su array vuoti
    cout << " [TEST ACCESS] Test funzioni array (top ed end) su array vuoti." << endl;
    try {
        cout << "  >> Funzione top su array vuoti." << endl;
        cout << "   Funzione top su arrInt: " << arrInt.top() << ", arrDouble: " << arrDouble.top() << ", arrChar: " << arrChar.top() << ", arrBool: " << arrBool.top() << "." << endl;
    } catch (std::runtime_error& e){
        cout << endl <<  "   [EXCEPTION] Expected exception runtime_error catturata, lanciata dalla funzione. " << e.what() << endl;
    }
    cout << "  >>> [END] Funzione top su array vuoti OK." << endl;
    
    try {
        cout << "  >> Funzione end su array vuoti." << endl;
        cout << "   Funzione end su arrInt: " << arrInt._end() << ", arrDouble: " << arrDouble._end() << ", arrChar: " << arrChar._end() << ", arrBool: " << arrBool._end() << "." << endl;
    } catch (std::runtime_error& e){
        cout << endl << "   [EXCEPTION] Expceted exception runtime_error catturata, lanciata dalla funzione. " << e.what() << endl;
    }
    cout << "  >>> [END] Funzione end su array vuoti OK. " << endl << endl;
    
    //modifica di un elemento all'interno dell'array
    cout << " [TEST ACCESS] Modifica di un elemento dell'array con edit." << endl;
    arrInt.insert(2); arrInt.insert(3);  arrDouble.insert(2.1); arrDouble.insert(3.1);
    arrChar.insert('B'); arrChar.insert('b');
    arrBool.insert(true); arrBool.insert(false);
    
    cout << "  >> Modifica di un elemento dell'array dato l'indice all'interno dell'array. Restituito valore sostituito." << endl;
    cout << "   $ Array prima della modifica - arrInt: " << arrInt << " , arrDouble: " << arrDouble << " , arrChar: " << arrChar << " , arrBool: " << arrBool << "." << endl;
    cout << "   Viene modificato il secondo elemento di ciascun array - stampa del valore sostituito - arrInt: " << arrInt.edit(1, 4) << " , arrDouble: " << arrDouble.edit(1, 4.1) << " , arrChar: " << arrChar.edit(1, 'q') << " , arrBool: " << arrBool.edit(1, 0) << endl;
    cout << "   $ Array dopo la modifica - arrInt: " << arrInt << " , arrDouble: " << arrDouble << " , arrChar: " << arrChar << " , arrBool: " << arrBool << "." << endl;
    assert(arrInt[0] == 2); assert(arrInt[1] == 4); assert(arrDouble[0] == 2.1); assert(arrDouble[1] == 4.1);
    assert(arrChar[0] == 'B'); assert(arrChar[1] == 'q'); assert(arrBool[0] == 0); assert(arrBool[1] == 0);
    cout << "  >>> [END] Modifica di un elemento dell'array dato l'indice - restituito sostituito OK." << endl;
    
    cout << "  >> Modifica di un elemento dell'array dato l'indice all'interno dell'array. Restituito valore prima della sostituzione." << endl;
    cout << "   $ Array prima della modifica - arrInt: " << arrInt << " , arrDouble: " << arrDouble << " , arrChar: " << arrChar << " , arrBool: " << arrBool << "." << endl;
    cout << "   Viene modificato il primo elemento di ciascun array - stampa del valore da sostituire - arrInt: " << arrInt.editPre(1, 3) << " , arrDouble: " << arrDouble.editPre(1, 5.1) << " , arrChar: " << arrChar.editPre(1, 'A') << " , arrBool: " << arrBool.editPre(1, false) << "." << endl;
    cout << "   $ Array dopo la modifica - arrInt: " << arrInt << " , arrDouble: " << arrDouble << " , arrChar: " << arrChar << " , arrBool: " << arrBool << "." << endl;
    assert(arrInt[0] == 2); assert(arrInt[1] == 3); assert(arrDouble[0] == 2.1); assert(arrDouble[1] == 4.1);
    assert(arrChar[0] == 'A'); assert(arrChar[1] == 'B'); assert(arrBool[0] == 0); assert(arrBool[1] == 0);
    cout << "  >>> [END] Modifica di un elemento dell'array dato l'indice - restituito da sostituire OK." << endl;
    
    //vengono inseriti nuovi elementi negli array per testare la funzione di edit basata sull'elemento piuttosto che sull'indice
    arrInt.insert(2); arrDouble.insert(2.1); arrChar.insert('A');
    cout << "  >> Modifica di più elementi dell'array dato il valore da sostituire." << endl;
    cout << "   $ Array prima della modifica - arrInt: " << arrInt << " , arrDouble: " << arrDouble << " , arrChar: " << arrChar << " , arrBool: " << arrBool << "." << endl;
    cout << "   Viene modificato : arrInt (2 -> 5), arrDouble (2.1 -> 7.2), arrChar (A -> C), arrBool (false -> true)." << endl;
    cout << "   Modifica - 1 (true) se almeno un elemento viene sostituito, altrimenti 0 (false) - arrInt: " << arrInt._edit(2, 5) << " , arrDouble: " << arrDouble._edit(2.1, 7.2) << " , arrChar: " << arrChar._edit('A', 'C') << " , arrBool: " << arrBool._edit(0, 1) << "." << endl;
    cout << "   $ Array dopo la modifica - arrInt: " << arrInt << " , arrDouble: " << arrDouble << " , arrChar: " << arrChar << " , arrBool: " << arrBool << "." << endl;
    assert(arrInt[0] == 5); assert(arrInt[1] == 5); assert(arrInt[2] == 3);
    assert(arrDouble[0] == 7.2); assert(arrDouble[1] == 7.2); assert(arrDouble[2] == 4.1);
    assert(arrChar[0] == 'C'); assert(arrChar[1] == 'C'); assert(arrChar[2] == 'B');
    assert(arrBool[0] == 1); assert(arrBool[1] == 1);
    cout << "  >>> [END] Modifica di più elementi dell'array dato il valore da sostituire OK." << endl;
    
    cout << "  >> Modifica di un elemento al di fuori degli indici." << endl;
    try {
        cout << "   Il test viene provato su un solo array. Prova di modifica di arrInt in indice 9 con edit: " << arrInt.edit(9, 7) << ", editPre: " << arrInt.editPre(9, 7) << endl;
    } catch (std::out_of_range& e){
        cout << endl << "   >>> [EXCEPTION] Expected exception out_of_range catturata, lanciata dalla funzione. " << e.what() << endl;
    }
    cout << "  >>> [END] Modifica di un elemento al di fuori degli indici OK." << endl;
    
    cout << "  >> Modifica di un elemento non presente nell'array." << endl;
    cout << "   Il test viene provato su un solo array. Prova di modifica del numero 12 in arrInt: " << arrInt._edit(12, 14) << "." << endl;
     cout << "  >>> [END] Modifica di un elemento non presente nell'array OK." << endl;
    
    cout << "[TEST END] Test tipi base OK." << endl;
    
}

/**
 @brief Test con tipi complessi.
 Viene testato l'inserimento, la rimozione, l'accesso, la ricerca, e la modifica su un array con un tipo complesso, in questo caso un array di Point.
 */
void test_complexTypes(){
    cout << " [TEST INSERT] Test inserimento di tipi complessi" << endl;
    SortedArray<Point, lessFunc> arrP;
    Point p(1, 3); Point p2(2, 4); arrP.insert(p); arrP.insert(p2);
    
    cout << "   arrP: " << arrP << "." << endl;
    cout << "   La dimensione di arrP dovrebbe essere 2. arrP size: " << arrP.size() << "." << endl;
    assert(arrP.size() == 2); assert(arrP[0].getX() == 1); assert(arrP[0].getY() == 3); assert(arrP[1].getX() == 2); assert(arrP[1].getY() == 4);
    arrP.insert(p);
    cout << "   Viene inserito un altro Point [1, 3] - arrP: " << arrP << "." << endl;
    cout << "   La dimensione di arrP dovrebbe essere 3. arrP size: " << arrP.size() << "." << endl;
    assert(arrP.size() == 3); assert(arrP[0].getX() == 1); assert(arrP[0].getY() == 3); assert(arrP[1].getX() == 1); assert(arrP[1].getY() == 3); assert(arrP[2].getX() == 2); assert(arrP[2].getY() == 4);
    cout << " [END] Test inserimento di tipi complessi OK." << endl << endl;
    
    cout << " [TEST ACCESS] Test operatore di accesso con tipi complessi" << endl;
    cout << "  >> Accesso al secondo elemento dell'array: " << arrP[1] << ". Expected [1, 3]." << endl;
    assert(arrP[1].getX() == 1); assert(arrP[1].getY() == 3);
    cout << "  >>> [END] Accesso al secondo elemento dell'array OK." << endl;
    
    try {
        cout << "  >> Accesso a un elemento dell'array al di fuori degli indici consentiti. arrP: " << arrP[5] << "." << endl;
    } catch (std::out_of_range& e){
        cout << endl << "   [EXCEPTION] Expected exception out_of_range catturata, lanciata dalla funzione. " << e.what() << endl;
    }
    cout << "  >>> [END] Accesso a un elemento dell'array al di fuori degli indici consentiti OK." << endl << endl;
    
    cout << " [TEST ACCESS] Test funzioni array (top ed end) con tipi complessi" << endl;
    cout << "  >> Accesso al primo elemento dell'array. arrP: " << arrP.top() << ". Expected [1,3]." << endl;
    assert(arrP.top().getX() == 1); assert(arrP.top().getY() == 3);
    cout << "  >>> [END] Accesso al primo elemento dell'array OK." << endl;
    
    cout << "  >> Accesso all'ultimo elemento dell'array. arrP: " << arrP._end() << ". Expected [2, 4]." << endl;
    assert(arrP._end().getX() == 2); assert(arrP._end().getY() == 4);
    cout << "  >>> [END] Accesso all'ultimo elemento dell'array OK." << endl;
    
    cout << " [TEST ACCESS] Test ricerca di elementi all'interno dell'array" << endl;
    cout << "   [NOTA] 1 per true, 0 per false." << endl;
    cout << "   Verifico se un certo punto è presente all'interno dell'array. Punto [1,3] è presente in arrP? " << arrP.contains(Point(1,3)) << ". Expected 1." << endl;
    assert(arrP.contains(Point(1,3)) == true);
    cout << "  >>> [END] Test ricerca di elementi all'interno dell'array OK." << endl << endl;
    
    cout << " [TEST REMOVE] Test rimozione di tipi composti" << endl;
    cout << "  >> Rimozione elementi di cui ce ne è soltanto uno nell'array." << endl;
    arrP.remove(Point(2,4));
    cout << "   Viene rimosso Point [2, 4]. La dimensione dell'array dovrebbe essere 2: " << arrP.size() << ", e arrP dovrebbe non contenere più [2, 4]. arrP: " << arrP << ". Expected 2, [1,3], [1,3]." << endl;
    assert(arrP.size() == 2); assert(arrP[0].getX() == 1); assert(arrP[0].getY() == 3); assert(arrP[1].getX() == 1); assert(arrP[1].getY() == 3);
    cout << "  >> [END] Rimozione elementi di cui ce ne è soltanto uno OK." << endl;
    
    cout << "  >> Rimozione elementi di cui ce ne sono più di uno nell'array." << endl;
    arrP.remove(Point(1,3));
    cout << "   Viene rimosso Point [1, 3], di cui ce ne sono due. L'array dovrebbe essere vuoto. arrP: " << arrP << ", size: " << arrP.size() << ". isEmpty? " << (arrP.isEmpty() ? string("TRUE") : string("FALSE")) << ".Expected [], 0.";
    assert(arrP.size() == 0); assert(arrP.isEmpty());
    cout << "  >>> [END] Rimozione elementi di cui ce ne sono più di uno nell'array OK." << endl;
    
    cout << "  >> Rimozione elementi non presenti nell'array." << endl;
    cout << "   arrP è adesso vuoto, quindi proviamo a rimuoverci Point [1,3], non più presente - remove: " << (arrP.remove(Point(1,3)) ? string("TRUE") : string("FALSE")) << ". Expected FALSE.";
    assert(arrP.remove(Point(1,3)) == false);
    cout << "  >>> [END] Rimozione elementi non presenti nell'array." << endl;
    
    arrP.insert(Point(1,3)); arrP.insert(Point(2,4));
    cout << "  >> Funzione clear/isEmpty per gli array." << endl;
    cout << "   Sono stati reinseriti due punti nell'array per provare efficacemente le funzioni. arrP: " << arrP << "." << endl;
    arrP.clear();
    cout << "   La dimensione dell'array dovrebbe essere 0. arrP size: " << arrP.size() << "." << endl;
    cout << "   L'array dovrebbe essere vuoto. arrP: " << arrP << ". Expected []." << endl;
    cout << "   L'array dovrebbe essere vuoto, con funzione isEmpty: " << (arrP.isEmpty() ? string("TRUE") : string("FALSE")) << ". Expected TRUE." << endl;
    assert(arrP.size() == 0); assert(arrP.isEmpty() == 1);
    cout << "  >>> [END] Funzione clear/isEmpty per gli array OK." << endl << endl;
    
    cout << " [TEST ACCESS] Test funzioni array (top ed end) su array vuoti." << endl;
    try {
        cout << "  >> Funzione top su array vuoto." << endl;
        cout << "   Funzione top su arrP: " << arrP.top() << "." << endl;
    } catch (std::runtime_error& e){
        cout << endl << "   [EXCEPTION] Expected exception runtime_error catturata, lanciata dalla funzione. " << e.what() << endl;
    }
    cout << "  >>> [END] Funzione top su array vuoto OK." << endl;
    
    try {
        cout << "  >> Funzione end su array vuoto." << endl;
        cout << "   Funzione end su ArrInt: " << arrP._end() << "." << endl;
    } catch (std::runtime_error& e){
        cout << endl << "   [EXCEPTION] Expected exception runtime_error catturata, lanciata dalla funzione. " << e.what() << endl;
    }
    cout << "  >>> [END] Funzione end su array vuoto OK." << endl << endl;
    
    cout << " [TEST ACCESS] Modifica di un elemento dell'array con edit." << endl;
    arrP.insert(Point(1,3)); Point p3(2,5);
    cout << "  E' stato inserito un nuovo punto per provare la funzione. arrP: " << arrP << "." << endl;
    cout << "  >> Modifica di un elemento dell'array dato l'indice all'interno dell'array. Restituito valore sostituito." << endl;
    cout << "   $ Array prima della modifica: " << arrP << "." << endl;
    cout << "   Viene modificato il primo elemento - stampa del valore sostituito - arrP: " << arrP.edit(0, p2) << "." << endl;
    cout << "   $ Array dopo la modifica - arrP: " << arrP << "." << endl;
    assert(arrP[0].getX() == 2); assert(arrP[0].getY() == 4);
    cout << "  >>> [END] Modifica di un elemento dato l'indice - restituito sostituito OK." << endl;
    
    cout << "  >> Modifica di un elemento dell'array dato l'indice. Restituito valore prima della sostituzione." << endl;
    cout << "   $ Array prima della modifica - arrP: " << arrP << "." << endl;
    cout << "   Viene modificato il primo elemento - stampa del valore da sostituire: " << arrP.editPre(0, Point(3,5)) << "." << endl;
    cout << "   $ Array dopo la modifica - arrP: " << arrP << "." << endl;
    assert(arrP[0].getX() == 2); assert(arrP[0].getY() == 4);  assert(arrP[1].getX() == 3); assert(arrP[1].getY() == 5);
    cout << "  >>> [END] Modifica di un elemento dato l'indice - restituito valore prima della sostituzione OK." << endl;
    
    arrP.insert(Point(1,3));
    cout << "  >> Modifica di più elementi dell'array dato il valore da sostituire." << endl;
    cout << "   $ Array prima della modifica - arrP: " << arrP << "." << endl;
    cout << "   Viene modificato Point [1, 3] : " << arrP._edit(Point(1,3), Point(2,5)) << "." << endl;
    cout << "   $ Array dopo la modifica - arrP: " << arrP << "." << endl;
    assert(arrP[0].getX() == 2); assert(arrP[0].getY() == 5);
    assert(arrP[1].getX() == 2); assert(arrP[1].getY() == 5);
    
    cout << "  >> Modifica di un elemento al di fuori degli indici." << endl;
    try {
        cout << "   Il test viene provato su un solo array. Prova di modifica di arrInt in indice 9 con edit: " << arrP.edit(9, Point(3,7)) << ", editPre: " << arrP.editPre(9, Point(3,7)) << endl;
    } catch (std::out_of_range& e){
        cout << endl << "   >>> [EXCEPTION] Expected exception out_of_range catturata, lanciata dalla funzione. " << e.what() << endl;
    }
    cout << "  >>> [END] Modifica di un elemento al di fuori degli indici OK." << endl;
    
    cout << "  >> Modifica di un elemento non presente nell'array." << endl;
    cout << "   Il test viene provato su un solo array. Prova di modifica del numero 12 in arrInt: " << arrP._edit(Point(5,4), Point(5,7)) << "." << endl;
        cout << "  >>> [END] Modifica di un elemento non presente nell'array OK." << endl;
        cout << "[TEST END] Test tipi composti OK." << endl;
    
}

/**
 * @brief Test della funzione di filtro (filter) su SortedArray utilizzando IsEven.
 */
void test_filterFunction() {
    SortedArray<int, lessFunc> sa;
    sa.insert(1); sa.insert(2); sa.insert(3); sa.insert(4); sa.insert(5);

    IsEven isEven;
    SortedArray<int, lessFunc> filtered = filter(sa, isEven);

    cout << "  >> Verifica se il filtro seleziona correttamente i numeri pari" << endl;
    // La SortedArray filtrata dovrebbe contenere solo i numeri pari
    cout << "   Array di partenza - sa: " << sa << "." << endl;
    cout << "   La SortedArray filtrata deve contenere solo 2 e 4. arrFiltered: " << filtered << ". Expected: [2, 4]." << endl;
    assert(filtered.size() == 2); assert(filtered[0] == 2); assert(filtered[1] == 4);
    cout << "  >>> [END] Verifica filtro numeri pari OK." << endl;

    AlwaysTrue alwaysTrue;
    SortedArray<int, lessFunc> filteredTrue = filter(sa, alwaysTrue);

    cout << "  >> Verifica se il filtro con AlwaysTrue seleziona tutti i numeri" << endl;
    // La SortedArray filtrata dovrebbe contenere tutti gli elementi
    cout << "   La SortedArray filtrata deve contenere tutti i numeri. sa size: " << sa.size() << ", arrFilteredTrue size: " << filteredTrue.size() << "; sa: " << sa << ", arrFilteredTrue: " << filteredTrue << ". Expected: [1, 2, 3, 4, 5] per entrambi." << endl;
    assert(filteredTrue.size() == 5); assert(filteredTrue[0] == 1); assert(filteredTrue[1] == 2);
    assert(filteredTrue[2] == 3); assert(filteredTrue[3] == 4); assert(filteredTrue[4] == 5);
    cout << "  >>> [END] Verifica filtro AlwaysTrue OK." << endl;

    AlwaysFalse alwaysFalse;
    SortedArray<int, lessFunc> filteredFalse = filter(sa, alwaysFalse);

    cout << "  >> Verifica se il filtro con AlwaysFalse restituisce array vuoto" << endl;
    // La SortedArray filtrata dovrebbe essere vuota
    
    cout << "   La SortedArray filtrata deve essere vuota. arrFilteredFalse: " << filteredFalse << ". filteredFile size: " << filteredFalse.size() << ". filteredFile isEmpty: " << filteredFalse.isEmpty() << "Expected: [], 0, 1." << endl;
    assert(filteredFalse.size() == 0); assert(filteredFalse.isEmpty() == true);
    cout << "  >>> [END] Verifica filtro AlwaysFalse OK." << endl;

    cout << "[TEST END] Test della funzione filter OK." << endl;
}

// TEST ITERATORI

/**
 @brief Test dell'incremento pre-iteratore.
 Vengono testati tutte le operazioni che un iteratore dovrebbe supportare, quali pre/post-incremento, pre/post-decremento, operatori di accesso, spostamento all'interno dell'array, misura della distanza.
 */
void test_constIterator(){
    SortedArray<int, lessFunc> sa;
    sa.insert(40); sa.insert(20); sa.insert(30); sa.insert(10);
    
    SortedArray<double, greaterFunc> sa2;
    sa2.insert(9.11); sa2.insert(9.99); sa2.insert(3.02);
    
    cout << "   Array di partenza: " << sa << "." << endl;
    cout << " [TEST CONSTRUCTOR] Test costruttori di const_iterator" << endl;
    cout << "  >> Verifica costruttore di default." << endl;
    SortedArray<int, lessFunc>::const_iterator itDefault;
    try {
        cout << "   itDefault punta a nullptr. itDefault: " << *itDefault << endl;
        assert(false);
    } catch (std::runtime_error& e){
        cout << endl << "   [EXCEPTION] Expected exception runtime_error catturata, lanciata dalla funzione. " << e.what() << endl;
    }
    cout << "  >>> [END] Costruttore di default OK." << endl;
    
    cout << "  >> Verifica costruttore di copia." << endl;
    itDefault = sa.begin();
    SortedArray<int, lessFunc>::const_iterator itCopy(itDefault);
    cout << "   itDefault dovrebbe puntare al primo elemento di sa: " << *itDefault << ", itCopy dovrebbe puntare allo stesso elemento: " << *itCopy << ". Expected 10, 10." << endl;
    assert(*itDefault == *itCopy);
    cout << "  >>> [END] Verifica costruttore di copia OK." << endl << endl;
    
    //operatore di assegnamento
    cout << " [TEST ASSIGN] Assegnamento di un altro iteratore a questo." << endl;
    itDefault = sa.begin(); itCopy = sa.end(); --itCopy;
    cout << "   itDefault fa riferimento all'inizio dell'array: " << *(itDefault) << ", itCopy fa riferimento all'ultimo elemento dell'array: " << *(itCopy) << "." << endl;
    itDefault = itCopy;
    cout << "   Assegnazione itCopy a itDefault - itDefault fa riferimento all'ultimo elemento dell'array: " << *(itDefault) << ". Expected 40." << endl;
    assert(*itDefault == 40); assert(*itCopy == 40);
    cout << "  >>> [END] Assegnamento di un altro iteratore a questo OK." << endl << endl;
    
    //funzioni di accesso
    cout << " [TEST ACCESS] Accesso al primo elemento dell'array." << endl;
    cout << "   Il primo elemento dell'array sa è: " << *(sa.begin()) << ". Expected 10." << endl;
    assert(*(sa.begin()) == 10);
    cout << "  >>> [END] Accesso al primo elemento dell'array OK." << endl;
    
    cout << " [TEST ACCESS] Funzione end() dell'array." << endl;
    cout << "   Una funzione end() negli iteratori punta alla fine dell'array, non all'ultimo elemento dell'array. "
         << "Fine dell'array (iteratore non dereferenziabile - OK se punta alla fine dell'array): " << (sa.end() == sa.end() ? string("OK") : string("ERROR"))
         << " , ultimo valore dell'array: " << *(--sa.end()) // Ora corretto!
         << ". Expected OK, 40." << endl;
    assert(*(--sa.end()) == 40);
    cout << "  >>> [END] Funzione end dell'array OK." << endl << endl;
    
    cout << " [TEST ACCESS] Operatore di accesso ->." << endl;
    SortedArray<Point, lessFunc> sap;
    Point p(3,2); sap.insert(p);
    SortedArray<Point, lessFunc>::const_iterator itp = sap.begin();
    cout << "   Array di partenza - tipo Point: " << "[" << sap[0].getX() << "," << sap[0].getY() << "]. Point possiede getX() come membro, da chiamare con ->. getX: " << itp->getX() << ". Expected 3." << endl;
    assert(itp->getX() == 3);
    cout << "  >>> [END] Operatore di accesso ->." << endl << endl;
    
    
    //valore di default per questo test: sa.begin()
    SortedArray<int,lessFunc>::const_iterator it = sa.begin();
    cout << " [TEST OPERATORS] Operatori di incremento" << endl;
    cout << "  >> Operatore di pre-incremento (++it)." << endl;
    cout << "   Il valore prima del pre-incremento è " << *it << ", dopo il pre-incremento è " << *(++it) << ". Expected 10, 20." << endl;
    //reset di it al valore di default per poter fare gli assert
    it = sa.begin(); assert(*it == 10); assert(*(++it) == 20);
    cout << "  >>> [END] Opeatore di pre-incremento OK." << endl;
    
    //reset di it al valore di default
    it = sa.begin();
    cout << "  >> Operatore di post-incremento (it++)" << endl;
    cout << "   Il valore prima del post-incremento è " << *it << ", dopo il post-incremento è " << *(it++) << ", avanzamento dell'iteratore a " << *it << ". Expected 10, 10, 20." << endl;
    it = sa.begin(); assert(*it == 10); assert(*(it++) == 10); assert(*it == 20);
    cout << "  >>> [END] Operatore di post-incremento OK." << endl << endl;
    
    //valore di default per questo test: sa.end()
    it = sa.end();
    cout << " [TEST OPERATORS] Operatori di decremento" << endl;
    cout << "  >> Operatore di pre-decremento (--it)." << endl;
    cout << "   Il valore prima del pre-decremento è " << *it << ", dopo il pre-decremento è " << *(--it) << ". Expected [invalid value], 30." << endl;
    cout  << "   [NOTA] E' giusto che *it punti oltre l'ultimo elemento dell'array" << endl;
    it = sa.end(); assert(*(--it) == 40);
    cout << "  >>> [END] Operatore di pre-decremento OK." << endl;
    
    //reset di it al valore di default
    it = sa.end(); it--;
    //eseguito prima un decremento in modo che l'iteratore punti all'ultimo elemento dell'array
    cout << "  >> Operatore di post-decremento (it++)." << endl;
    cout << "   Il valore prima del post-decremento è " << *it << ", dopo il post-decremento è " << *(it--) << ", retrocessione dell'iteratore a " << *it << ". Expected 40, 40, 30." << endl;
    it = sa.end(); it--; assert(*it == 40); assert(*(it--) == 40); assert(*it == 30);
    cout << "  >>> [END] Operatore di post-decremento OK." << endl << endl;
    
    //operatore di uguaglianza
    //reset al valore di default
    it = sa.begin(); itCopy = sa.begin();
    cout << " [TEST BOOL] Operatore di uguaglianza." << endl;
    cout << "  >> it e itCopy fanno riferimento allo stesso elemento nell'array. it: " << *it << ", itCopy: " << *itCopy << ".\n Fanno riferimento allo stesso elemento? " << (it == itCopy ? string("TRUE.") : string("FALSE.")) << " Expected true." << endl;
    assert((it == itCopy) == true);
    cout << "  >>> [END] it e itCopy fanno riferimento allo stesso elemento OK." << endl;
    
    itCopy = --(sa.end());
    cout << "  >> it e itCopy fanno riferimento a diversi elementi nell'array. it: " << *it << ", itCopy: " << *itCopy << ".\n Fanno riferimento allo stesso elemento? " << (it == itCopy ? string("TRUE.") : string("FALSE.")) << " Expected false." << endl;
    assert((it == itCopy) == false);
    cout << "  >>> [END] it e itCopy fanno riferimento a diversi elementi OK." << endl << endl;
    
    //reset ai valori di default
    it = sa.begin(); itCopy = --(sa.end());
    cout << " [TEST BOOL] Operatore di disuguaglianza." << endl;
    cout << "  >> it e itCopy fanno riferimento a elementi diversi nell'array. it: " << *it << ", itCopy: " << *itCopy << ".\n Fanno riferimento a elementi diversi? " << (it != itCopy ? string("TRUE.") : string("FALSE.")) << " Expected true." << endl;
    assert((it != itCopy) == true);
    cout << "  >>> [END] it e itCopy fanno riferimento a elementi diversi OK.\n";
    
    itCopy = sa.begin();
    cout << "  >> it e itCopy fanno riferimento allo stesso elemento nell'array. it: " << *it << ", itCopy: " << *itCopy << ".\n Fanno riferimento a elementi diversi? " << (it != itCopy ? string("TRUE.") : string("FALSE.")) << " Expected false." << endl;
    assert((it != itCopy) == false);
    cout << "  >>> [END] it e itCopy fanno riferimento allo stesso elemento OK." << endl << endl;
    
    //operatore di somma
    it = sa.begin();
    cout << " [TEST ACCESS] Spostamento all'interno dell'array in base a una certa distanza" << endl;
    cout << "  >> Operatore di somma per lo spostamento." << endl;
    cout << "   [NOTA] operator+ crea un nuovo iteratore senza aggiornare l'iteratore esistente." << endl;
    cout << "   it punta all'inizio dell'array: " << *(it) << ", aggiungendoci 3 posizioni abbiamo un iteratore che punta all'ultimo elemento dell'array: " << *(it + 3) << ". Expected 40." << endl;
    assert(*(it) == 10); assert(*(it+3) == 40);
    cout << "  >>> [END] Operatore di somma per lo spostamento OK." << endl;
    
    it = --(sa.end());
    cout << "  >> Operatore di sottrazione per lo spostamento." << endl;
    cout << "   [NOTA] operator- crea un nuovo iteratore senza aggiornare l'iteratore esistente." << endl;
    cout << "   it punta all'ultimo elemento dell'array: " << *(it) << ", sottraendoci 3 posizioni abbiamo un iteratore che punta al primo elemento dell'array: " << *(it - 3) << ". Expected 10." << endl;
    assert(*(it) == 40); assert(*(it-3) == 10);
    cout << "  >>> [END] Operatore di sottrazione per lo spostamento OK." << endl;
    
    it = sa.begin();
    cout << "  >> Operatore di somma per lo spostamento." << endl;
    cout << "   [NOTA] operator+= modifica l'iteratore su cui è stato invocato, senza creare un nuovo iteratore." << endl;
    cout << "   it punta all'inizio dell'array: " << *(it) << ", aggiungendoci 3 posizioni punta all'ultimo elemento dell'array: " << *(it += 3) << ". Expected 40." << endl;
    it -= 3; assert(*it == 10); assert(*(it+=3) == 40);
    cout << "  >>> [END] Operatore di somma per lo spostamento OK." << endl;
    
    it = --(sa.end());
    cout << "  >> Operatore di sottrazione per lo spostamento." << endl;
    cout << "   [NOTA] operator-= modifica l'iteratore su cui è stato invocato, senza creare un nuovo iteratore." << endl;
    cout << "   it punta all'ultimo elemento dell'array: " << *(it) << ", sottraendoci 3 posizioni punta al primo elemento dell'array: " << *(it -= 3) << ". Expected 10." << endl;
    it += 3; assert(*it == 40); assert(*(it-=3) == 10);
    cout << "  >>> [END] Operatore di sottrazione per lo spostamento OK." << endl;
    
    it = sa.begin(); itCopy = (sa.end());
    ptrdiff_t distance = itCopy - it;
    cout << " [TEST DIFF] Calcola la distanza tra due iteratori" << endl;
    cout << "   it punta al primo elemento dell'array: " << *it << ", itCopy punta alla fine dell'array. La distanza tra i due iteratori è " << distance << ". Expected 4." << endl;
    assert(distance == 4);
    cout << "   >>> [END] Calcola la distanza tra due iteratori OK." << endl;
}


int main() {
    cout << "[NOTA] Per ogni test viene stampato a schermo i valori che verifica, e successivamente usa 'assert' per verificare a runtime." << endl << endl;
    cout << "[TEST] Test costruttori" << endl;
    test_constructor();
    
    cout << endl << endl << "[TEST] Test operatore di assegnamento" << endl;
    test_assign();
    
    cout << endl << endl << "[TEST] Test tipi base" << endl;
    test_base();
    
    cout << endl << endl << "[TEST] Test tipi composti" << endl;
    test_complexTypes();
    
    cout << endl << endl << "[TEST] Test funzione di filtro" << endl;
    test_filterFunction();
    
    cout << endl << endl << "[TEST] Test iteratore costante" << endl;
    test_constIterator();
    
    cout << endl << endl << "[TEST] All tests OK." << endl;

    
    return 0;
}
