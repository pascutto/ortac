  $ ortac report ../suite/arrays/lib.mli
  Lib
  File "../suite/arrays/lib.mli", line 1, characters 0-327:
  the value create:
   - Pure: false
   - Preconditions:
    + File "../suite/arrays/lib.mli", line 3, characters 13-19:
      ` ensur' has  been translated
  - Postconditions:
    + File "../suite/arrays/lib.mli", line 4, characters 12-32:
      `all (fun x -> x = v)' has  been translated
    + File "../suite/arrays/lib.mli", line 5, characters 12-46:
      `res forall i. 0 <= i < n -> arr.(i' has  been translated
    + File "../suite/arrays/lib.mli", line 6, characters 12-47:
      `forall i:integer. (0:integer <= i:integer):prop /\ (i:integer < (integer_of_int 
  n:int):integer):prop -> ((get 
  arr:'a array i:integer):'a = v:'a):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    n is not consumed and is not modified
    + Invariants involved:
      
    v is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    arr
    + Invariants involved:
      
  File "../suite/arrays/lib.mli", line 8, characters 0-238:
  the value bad_create:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/arrays/lib.mli", line 10, characters 12-47:
      `forall i_1:integer. (0:integer <= i_1:integer):prop /\ (i_1:integer < (integer_of_int 
  n_1:int):integer):prop -> ((get 
  arr_1:int array i_1:integer):int = v_1:int):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    n_1 is not consumed and is not modified
    + Invariants involved:
      
    v_1 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    arr_1
    + Invariants involved:
      
  File "../suite/arrays/lib.mli", line 12, characters 0-214:
  the value get:
   - Pure: false
   - Preconditions:
    + File "../suite/arrays/lib.mli", line 14, characters 13-38:
      `(0:integer <= (integer_of_int  i_2:int):integer):prop /\ ((integer_of_int 
  i_2:int):integer < (length 
  arr_2:'a array):integer):prop' has  been translated
  - Postconditions:
    + File "../suite/arrays/lib.mli", line 15, characters 12-23:
      `(o:'a = (get 
  arr_2:'a array (integer_of_int  i_2:int):integer):'a):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    arr_2 is not consumed and is not modified
    + Invariants involved:
      
    i_2 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    o
    + Invariants involved:
      
  File "../suite/arrays/lib.mli", line 17, characters 0-213:
  the Gospel axiom a has been translatedFile "../suite/arrays/lib.mli", line 21, characters 0-222:
  the value bad_get:
   - Pure: false
   - Preconditions:
    + File "../suite/arrays/lib.mli", line 23, characters 13-38:
      `(0:integer <= (integer_of_int  i_4:int):integer):prop /\ ((integer_of_int 
  i_4:int):integer < (length 
  arr_4:'a array):integer):prop' has  been translated
  - Postconditions:
    + File "../suite/arrays/lib.mli", line 24, characters 12-23:
      `(o_1:'a = (get 
  arr_4:'a array (integer_of_int  i_4:int):integer):'a):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    arr_4 is not consumed and is not modified
    + Invariants involved:
      
    i_4 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    o_1
    + Invariants involved:
      
  File "../suite/arrays/lib.mli", line 26, characters 0-322:
  the value set:
   - Pure: false
   - Preconditions:
    + File "../suite/arrays/lib.mli", line 29, characters 13-38:
      `i) = v
      (*
       ensure' has  been translated
  - Postconditions:
    + File "../suite/arrays/lib.mli", line 30, characters 13-24:
      ` <= j < Arr' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    arr_5 is not consumed and is modified
    + Invariants involved:
      
    i_5 is not consumed and is not modified
    + Invariants involved:
      
    v_2 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    
  File "../suite/arrays/lib.mli", line 36, characters 0-338:
  the value fill:
   - Pure: false
   - Preconditions:
    + File "../suite/arrays/lib.mli", line 39, characters 13-54:
      `  ensures  forall j. ofs <= j < ofs + len' has  been translated
  - Postconditions:
    + File "../suite/arrays/lib.mli", line 40, characters 13-58:
      `forall j:integer. ((integer_of_int 
  ofs:int):integer <= j:integer):prop /\ (j:integer < ((integer_of_int 
  ofs:int):integer + (integer_of_int  len:int):integer):integer):prop -> ((get 
  arr_6:'a array j:integer):'a = v_3:'a):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    arr_6 is not consumed and is modified
    + Invariants involved:
      
    ofs is not consumed and is not modified
    + Invariants involved:
      
    len is not consumed and is not modified
    + Invariants involved:
      
    v_3 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    
  File "../suite/arrays/lib.mli", line 42, characters 0-194:
  the value length:
   - Pure: true
   - Preconditions:
    
  - Postconditions:
    + File "../suite/arrays/lib.mli", line 45, characters 12-30:
      `((integer_of_int  i_6:int):integer = (length 
  a_1:'a array):integer):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    a_1 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    i_6
    + Invariants involved:
      
  File "../suite/arrays/lib.mli", line 47, characters 0-296:
  the value map:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/arrays/lib.mli", line 49, characters 12-33:
      `0 <= i < length a -> ' has  been translated
    + File "../suite/arrays/lib.mli", line 50, characters 12-60:
      `forall i_7:integer. (0:integer <= i_7:integer):prop /\ (i_7:integer < (integer_of_int 
  (length_1  a_2:'a array):int):integer):prop -> ((get 
  arr_7:'b array i_7:integer):'b = (apply 
  f:'a -> 'b (get  a_2:'a array i_7:integer):'a):'b):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    f is not consumed and is not modified
    + Invariants involved:
      
    a_2 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    arr_7
    + Invariants involved:
      
  File "../suite/arrays/lib.mli", line 52, characters 0-318:
  the value bad_map_length:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/arrays/lib.mli", line 54, characters 12-33:
      `0 <= i < length a -> ' has  been translated
    + File "../suite/arrays/lib.mli", line 55, characters 12-60:
      `forall i_8:integer. (0:integer <= i_8:integer):prop /\ (i_8:integer < (integer_of_int 
  (length_1  a_3:'a array):int):integer):prop -> ((get 
  arr_8:'b array i_8:integer):'b = (apply 
  f_1:'a -> 'b (get  a_3:'a array i_8:integer):'a):'b):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    f_1 is not consumed and is not modified
    + Invariants involved:
      
    a_3 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    arr_8
    + Invariants involved:
      
  File "../suite/arrays/lib.mli", line 57, characters 0-316:
  the value bad_map_fun:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/arrays/lib.mli", line 59, characters 12-33:
      `0 <= i < length a -> ' has  been translated
    + File "../suite/arrays/lib.mli", line 60, characters 12-60:
      `forall i_9:integer. (0:integer <= i_9:integer):prop /\ (i_9:integer < (integer_of_int 
  (length_1  a_4:int array):int):integer):prop -> ((get 
  arr_9:int array i_9:integer):int = (apply 
  f_2:int -> int (get  a_4:int array i_9:integer):int):int):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    f_2 is not consumed and is not modified
    + Invariants involved:
      
    a_4 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    arr_9
    + Invariants involved:
      
  File "../suite/arrays/lib.mli", line 62, characters 0-286:
  the value sort:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/arrays/lib.mli", line 65, characters 12-123:
      `forall i_10:integer. (0:integer <= i_10:integer):prop /\ (i_10:integer < (length 
  a_5:int array):integer):prop -> forall j_1:integer. (i_10:integer < j_1:
                                                                      integer):prop /\ (
  j_1:integer < (length  a_5:int array):integer):prop -> ((integer_of_int 
  (get  a_5:int array i_10:integer):int):integer <= (integer_of_int 
  (get  a_5:int array j_1:integer):int):integer):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    a_5 is not consumed and is modified
    + Invariants involved:
      
  - Return:
    
  File "../suite/arrays/lib.mli", line 69, characters 0-594:
  the value copy_sort:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/arrays/lib.mli", line 71, characters 12-43:
      `forall i. 0 <= i < Array.length' has  been translated
    + File "../suite/arrays/lib.mli", line 72, characters 12-123:
      `  -> forall j. i < j < Array.length r
              -> r.(i) <= r.(j)
      ensures forall i. 0 <= i < Array.length' has  been translated
    + File "../suite/arrays/lib.mli", line 75, characters 12-111:
      `  -> exists j. 0 <= j < Array.length a /\ r.(i) = a.(j)
      ensures forall i. 0 <= i < Array.length' has  been translated
    + File "../suite/arrays/lib.mli", line 77, characters 12-111:
      `forall i_13:integer. (0:integer <= i_13:integer):prop /\ (i_13:integer < (length 
  a_6:int array):integer):prop -> exists j_4:integer. (0:integer <= j_4:integer):prop /\ (
  j_4:integer < (length  r:int array):integer):prop /\ ((get 
  a_6:int array i_13:integer):int = (get 
  r:int array j_4:integer):int):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    a_6 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    r
    + Invariants involved:
      
  File "../suite/arrays/lib.mli", line 80, characters 0-288:
  the value bad_sort:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/arrays/lib.mli", line 82, characters 12-123:
      `forall i_14:integer. (0:integer <= i_14:integer):prop /\ (i_14:integer < (length 
  r_1:int array):integer):prop -> forall j_5:integer. (i_14:integer < j_5:
                                                                      integer):prop /\ (
  j_5:integer < (length  r_1:int array):integer):prop -> ((integer_of_int 
  (get  r_1:int array i_14:integer):int):integer <= (integer_of_int 
  (get  r_1:int array j_5:integer):int):integer):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    a_7 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    r_1
    + Invariants involved:
      
  File "../suite/arrays/lib.mli", line 86, characters 0-602:
  the value constant_sort:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/arrays/lib.mli", line 88, characters 12-43:
      `forall i. 0 <= i < Array.length' has  been translated
    + File "../suite/arrays/lib.mli", line 89, characters 12-123:
      `  -> forall j. i < j < Array.length r
              -> r.(i) <= r.(j)
      ensures forall i. 0 <= i < Array.length' has  been translated
    + File "../suite/arrays/lib.mli", line 92, characters 12-111:
      `  -> exists j. 0 <= j < Array.length a /\ r.(i) = a.(j)
      ensures forall i. 0 <= i < Array.length' has  been translated
    + File "../suite/arrays/lib.mli", line 94, characters 12-111:
      `forall i_17:integer. (0:integer <= i_17:integer):prop /\ (i_17:integer < (length 
  a_8:int array):integer):prop -> exists j_8:integer. (0:integer <= j_8:integer):prop /\ (
  j_8:integer < (length  r_2:int array):integer):prop /\ ((get 
  a_8:int array i_17:integer):int = (get 
  r_2:int array j_8:integer):int):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    a_8 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    r_2
    + Invariants involved:
      
  $ ortac report ../suite/terms/lib.mli
  Lib
  File "../suite/terms/lib.mli", line 3, characters 0-224:
  the value lazy_bool:
   - Pure: false
   - Preconditions:
    + File "../suite/terms/lib.mli", line 5, characters 13-29:
      `(x:int = x:int):prop || ((1:integer / 0:integer):integer = 2:integer):prop' has  been translated
    + File "../suite/terms/lib.mli", line 6, characters 13-36:
      `not not (x:int = x:int):prop && ((1:integer / 0:integer):integer = 2:
  integer):prop' has  been translated
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    x is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 8, characters 0-185:
  the value not_lazy_or:
   - Pure: false
   - Preconditions:
    + File "../suite/terms/lib.mli", line 10, characters 13-29:
      `(x_1:int = x_1:int):prop \/ ((1:integer / 0:integer):integer = 2:integer):prop' has  been translated
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    x_1 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_1
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 12, characters 0-202:
  the value not_lazy_and:
   - Pure: false
   - Preconditions:
    + File "../suite/terms/lib.mli", line 14, characters 13-36:
      `not not (x_2:int = x_2:int):prop /\ ((1:integer / 0:integer):integer = 2:
  integer):prop' has  been translated
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    x_2 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_2
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 18, characters 0-209:
  the value scope1:
   - Pure: false
   - Preconditions:
    + File "../suite/terms/lib.mli", line 20, characters 13-30:
      `let x_4:bool = (True ):bool in (x_4:bool = (True ):bool):prop' has  been translated
  - Postconditions:
    + File "../suite/terms/lib.mli", line 21, characters 13-30:
      `let y_4:bool = (True ):bool in (y_4:bool = (True ):bool):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    x_3 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_3
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 25, characters 0-264:
  the value if_forall:
   - Pure: false
   - Preconditions:
    + File "../suite/terms/lib.mli", line 27, characters 13-70:
      `if forall i:integer. (0:integer <= i:integer):prop /\ (i:integer < 10:
  integer):prop -> not ((integer_of_int 
  x_5:int):integer = i:integer):prop then ((integer_of_int 
  x_5:int):integer = 10:integer):prop else ((integer_of_int 
  x_5:int):integer = 3:integer):prop' has  been translated
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    x_5 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_5
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 29, characters 0-180:
  the value equiv:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 31, characters 12-31:
      `(1:integer = 2:integer):prop <-> (2:integer = 3:integer):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    () is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_6
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 33, characters 0-206:
  the value exists_:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 35, characters 12-42:
      `exists x_6:integer. (0:integer <= x_6:integer):prop /\ (x_6:integer < 10:
  integer):prop /\ (x_6:integer = 3:integer):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    () is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_7
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 39, characters 0-24:
  the type t:
  - Invariants:
    
  - Equality: has not been derived
  - Comparison: has not been derived
  - Copy: has not been derived
  File "../suite/terms/lib.mli", line 41, characters 0-219:
  the value a:
   - Pure: false
   - Preconditions:
    + File "../suite/terms/lib.mli", line 43, characters 13-76:
      `(match x_7:t with
  | A -> (True ):bool
  | B s:string -> (False ):bool
  end::bool = (True ):bool):prop' has  been translated
    + File "../suite/terms/lib.mli", line 46, characters 13-18:
      `(x_7:t = (A ):t):prop' has  been translated
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    x_7 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_8
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 48, characters 0-207:
  the value b:
   - Pure: false
   - Preconditions:
    + File "../suite/terms/lib.mli", line 50, characters 13-76:
      `(match x_8:t with
  | A -> (False ):bool
  | B _ -> (True ):bool
  end::bool = (True ):bool):prop' has  been translated
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    x_8 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_9
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 54, characters 0-27:
  the type peano:
  - Invariants:
    
  - Equality: has not been derived
  - Comparison: has not been derived
  - Copy: has not been derived
  File "../suite/terms/lib.mli", line 56, characters 0-160:
  the value succ:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 58, characters 14-21:
      `(y_10:peano = (S 
  x_9:peano):peano):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    x_9 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_10
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 60, characters 0-224:
  the value add:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 62, characters 16-32:
      `not (x_10:peano = (O ):peano):prop -> not (z:peano = (O ):peano):prop' has  been translated
    + File "../suite/terms/lib.mli", line 63, characters 16-32:
      `not (y_11:peano = (O ):peano):prop -> not (z:peano = (O ):peano):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    x_10 is not consumed and is not modified
    + Invariants involved:
      
    y_11 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    z
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 65, characters 0-226:
  the value bad_add:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 67, characters 14-30:
      `not (x_11:peano = (O ):peano):prop -> not (z_1:peano = (O ):peano):prop' has  been translated
    + File "../suite/terms/lib.mli", line 68, characters 14-30:
      `not (y_12:peano = (O ):peano):prop -> not (z_1:peano = (O ):peano):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    x_11 is not consumed and is not modified
    + Invariants involved:
      
    y_12 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    z_1
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 70, characters 0-38:
  the type tree:
  - Invariants:
    
  - Equality: has not been derived
  - Comparison: has not been derived
  - Copy: has not been derived
  File "../suite/terms/lib.mli", line 72, characters 4-105:
  the Gospel function __logical_size__017_ has been translatedFile "../suite/terms/lib.mli", line 75, characters 0-204:
  the value size:
   - Pure: true
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 78, characters 14-29:
      `not (t_2:tree = (E ):tree):prop -> ((integer_of_int 
  s_1:int):integer > 0:integer):prop' has  been translated
    + File "../suite/terms/lib.mli", line 79, characters 14-24:
      `((integer_of_int  s_1:int):integer = (size 
  t_2:tree):integer):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    t_2 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    s_1
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 81, characters 0-183:
  the value size_wrong_spec:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 83, characters 12-23:
      `not (s_2:int = (size_1 
  t_3:tree):int):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    t_3 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    s_2
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 85, characters 0-285:
  the value test_tree:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 87, characters 14-110:
      `(b:bool = (True ):bool):prop <-> (match t_4:tree with
  | E -> (True ):bool
  | N (l_1:tree, x_12:int, t_5:tree) -> if (l_1:tree = t_5:tree):prop && ((integer_of_int 
                                        x_12:int):integer = 0:integer):prop then (True ):
                                        bool else (False ):bool
  end::bool = (True ):bool):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    t_4 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    b
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 91, characters 0-203:
  the value make_tree:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 93, characters 14-29:
      `(t_6:tree = (N 
  l_2:tree x_13:int r_1:tree):tree):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    l_2 is not consumed and is not modified
    + Invariants involved:
      
    x_13 is not consumed and is not modified
    + Invariants involved:
      
    r_1 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    t_6
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 95, characters 0-282:
  the value fill:
   - Pure: false
   - Preconditions:
    + File "../suite/terms/lib.mli", line 97, characters 15-43:
      ` start <= stop <= Array.leng' has  been translated
  - Postconditions:
    + File "../suite/terms/lib.mli", line 98, characters 15-46:
      `((integer_of_int  start:int):integer <= (integer_of_int 
  stop:int):integer):prop /\ ((integer_of_int  stop:int):integer <= (length 
  a_1:int array):integer):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    t_7 is not consumed and is not modified
    + Invariants involved:
      
    a_1 is not consumed and is not modified
    + Invariants involved:
      
    start is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    stop
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 100, characters 0-58:
  the type alt_tree:
  - Invariants:
    
  - Equality: has not been derived
  - Comparison: has not been derived
  - Copy: has not been derived
  File "../suite/terms/lib.mli", line 102, characters 0-257:
  the value make_alt_tree:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 104, characters 14-45:
      `let c:alt_tree * int * alt_tree = (tuple3 
  l_3:alt_tree x_14:int r_2:alt_tree):alt_tree * int * alt_tree in (t_8:
                                                                    alt_tree = (Nalt 
  c:alt_tree * int * alt_tree):alt_tree):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    l_3 is not consumed and is not modified
    + Invariants involved:
      
    x_14 is not consumed and is not modified
    + Invariants involved:
      
    r_2 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    t_8
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 106, characters 0-166:
  the value ref_access:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 108, characters 12-18:
      `(y_13:'a = (prefix ! 
  x_15:'a ref):'a):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    x_15 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_13
    + Invariants involved:
      
  $ ortac report ../suite/arith/lib.mli
  Lib
  File "../suite/arith/lib.mli", line 1, characters 0-219:
  the value test_forall:
   - Pure: false
   - Preconditions:
    + File "../suite/arith/lib.mli", line 3, characters 13-42:
      `forall x:integer. ((integer_of_int 
  i:int):integer <= x:integer):prop /\ (x:integer < (integer_of_int 
  j:int):integer):prop -> (x:integer > 0:integer):prop' has  been translated
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    i is not consumed and is not modified
    + Invariants involved:
      
    j is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    r
    + Invariants involved:
      
  File "../suite/arith/lib.mli", line 5, characters 0-281:
  the value double_forall:
   - Pure: false
   - Preconditions:
    + File "../suite/arith/lib.mli", line 7, characters 13-70:
      `forall i_1:integer. ((integer_of_int 
  lo:int):integer <= i_1:integer):prop /\ (i_1:integer < (integer_of_int 
  hi:int):integer):prop -> forall j_1:integer. (i_1:integer <= j_1:integer):prop /\ (
  j_1:integer < (integer_of_int 
  hi:int):integer):prop -> (i_1:integer <= j_1:integer):prop' has  been translated
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    lo is not consumed and is not modified
    + Invariants involved:
      
    hi is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y
    + Invariants involved:
      
  File "../suite/arith/lib.mli", line 9, characters 0-192:
  the value power:
   - Pure: false
   - Preconditions:
    + File "../suite/arith/lib.mli", line 11, characters 13-18:
      `((integer_of_int 
  n:int):integer >= 0:integer):prop' has  been translated
  - Postconditions:
    + File "../suite/arith/lib.mli", line 12, characters 13-24:
      `((integer_of_int  r_1:int):integer = (pow 
  (integer_of_int  x_1:int):integer (integer_of_int  n:int):integer):integer):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    x_1 is not consumed and is not modified
    + Invariants involved:
      
    n is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    r_1
    + Invariants involved:
      
  $ ortac report ../suite/types/lib.mli
  Lib
  File "../suite/types/lib.mli", line 1, characters 0-175:
  the type t:
  - Invariants:
    + File "../suite/types/lib.mli", line 3, characters 14-25:
      `((t_1:t).y = (False ):bool):prop' has been translated
  - Equality: has not been derived
  - Comparison: has not been derived
  - Copy: has not been derived
  File "../suite/types/lib.mli", line 5, characters 0-172:
  the value v:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/types/lib.mli", line 7, characters 12-19:
      `(x:int = (t_2:t).x_1):prop' has  been translated
    + File "../suite/types/lib.mli", line 8, characters 12-19:
      `(y_1:bool = (t_2:t).y):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    x is not consumed and is not modified
    + Invariants involved:
      
    y_1 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    t_2
    + Invariants involved:
      + File "../suite/types/lib.mli", line 3, characters 14-25:
        `((t_1:t).y = (False ):bool):prop' has been translated
  File "../suite/types/lib.mli", line 10, characters 0-127:
  the constant value e:
  - Checks:
    + File "../suite/types/lib.mli", line 11, characters 12-20:
      `((integer_of_int 
  (e:t).x_1):integer >= 0:integer):prop' has  been translated
  - Invariants:
    + File "../suite/types/lib.mli", line 3, characters 14-25:
      `((t_1:t).y = (False ):bool):prop' has been translatedFile "../suite/types/lib.mli", line 13, characters 0-152:
  the value get_x:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/types/lib.mli", line 15, characters 12-19:
      `(r:int = (t_3:t).x_1):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    t_3 is not consumed and is not modified
    + Invariants involved:
      + File "../suite/types/lib.mli", line 3, characters 14-25:
        `((t_1:t).y = (False ):bool):prop' has been translated
  - Return:
    r
    + Invariants involved:
      
  $ ortac report ../suite/exceptions/lib.mli
  Lib
  File "../suite/exceptions/lib.mli", line 1, characters 0-150:
  the value raise_oom:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    i is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    o
    + Invariants involved:
      
  File "../suite/exceptions/lib.mli", line 4, characters 0-180:
  the value raise_stackoverflow:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    i_1 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    o_1
    + Invariants involved:
      
  File "../suite/exceptions/lib.mli", line 7, characters 0-198:
  the value undeclared_raise_notfound:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    i_2 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    o_2
    + Invariants involved:
      
  File "../suite/exceptions/lib.mli", line 10, characters 0-210:
  the value bad_raise_notfound:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    
  - Exceptional postconditions:
    + the clauses concerning the exception Not_found have been translated
  - Arguments:
    i_3 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    o_3
    + Invariants involved:
      
  File "../suite/exceptions/lib.mli", line 14, characters 0-200:
  the value raise_notfound:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    
  - Exceptional postconditions:
    + the clauses concerning the exception Not_found have been translated
  - Arguments:
    i_4 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    o_4
    + Invariants involved:
      
  File "../suite/exceptions/lib.mli", line 18, characters 0-323:
  the value raise_invalidarg:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    
  - Exceptional postconditions:
    + the clauses concerning the exception Not_found have been translated
    + the clauses concerning the exception Invalid_argument have been translated
    + the clauses concerning the exception Invalid_argument have been translated
  - Arguments:
    i_5 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    o_5
    + Invariants involved:
      
  File "../suite/exceptions/lib.mli", line 24, characters 0-166:
  the value check:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    x is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y
    + Invariants involved:
      
  File "../suite/exceptions/lib.mli", line 28, characters 0-174:
  the value bad_check:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    x_1 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_1
    + Invariants involved:
      
  File "../suite/exceptions/lib.mli", line 32, characters 0-176:
  the value bad_check2:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    x_2 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_2
    + Invariants involved:
      

  $ ortac report ../suite/arith/lib.mli
  Lib
  File "../suite/arith/lib.mli", line 1, characters 0-219:
  the value test_forall:
   - Pure: false
   - Preconditions:
    + File "../suite/arith/lib.mli", line 3, characters 13-42:
      `forall x:integer. ((integer_of_int 
  i:int):integer <= x:integer):prop /\ (x:integer < (integer_of_int 
  j:int):integer):prop -> (x:integer > 0:integer):prop' has  been translated
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    i is not consumed and is not modified
    + Invariants involved:
      
    j is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    r
    + Invariants involved:
      
  File "../suite/arith/lib.mli", line 5, characters 0-281:
  the value double_forall:
   - Pure: false
   - Preconditions:
    + File "../suite/arith/lib.mli", line 7, characters 13-70:
      `forall i_1:integer. ((integer_of_int 
  lo:int):integer <= i_1:integer):prop /\ (i_1:integer < (integer_of_int 
  hi:int):integer):prop -> forall j_1:integer. (i_1:integer <= j_1:integer):prop /\ (
  j_1:integer < (integer_of_int 
  hi:int):integer):prop -> (i_1:integer <= j_1:integer):prop' has  been translated
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    lo is not consumed and is not modified
    + Invariants involved:
      
    hi is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y
    + Invariants involved:
      
  File "../suite/arith/lib.mli", line 9, characters 0-192:
  the value power:
   - Pure: false
   - Preconditions:
    + File "../suite/arith/lib.mli", line 11, characters 13-18:
      `((integer_of_int 
  n:int):integer >= 0:integer):prop' has  been translated
  - Postconditions:
    + File "../suite/arith/lib.mli", line 12, characters 13-24:
      `((integer_of_int  r_1:int):integer = (pow 
  (integer_of_int  x_1:int):integer (integer_of_int  n:int):integer):integer):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    x_1 is not consumed and is not modified
    + Invariants involved:
      
    n is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    r_1
    + Invariants involved:
      

  $ ortac report ../suite/terms/lib.mli
  Lib
  File "../suite/terms/lib.mli", line 3, characters 0-224:
  the value lazy_bool:
   - Pure: false
   - Preconditions:
    + File "../suite/terms/lib.mli", line 5, characters 13-29:
      `(x:int = x:int):prop || ((1:integer / 0:integer):integer = 2:integer):prop' has  been translated
    + File "../suite/terms/lib.mli", line 6, characters 13-36:
      `not not (x:int = x:int):prop && ((1:integer / 0:integer):integer = 2:
  integer):prop' has  been translated
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    x is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 8, characters 0-185:
  the value not_lazy_or:
   - Pure: false
   - Preconditions:
    + File "../suite/terms/lib.mli", line 10, characters 13-29:
      `(x_1:int = x_1:int):prop \/ ((1:integer / 0:integer):integer = 2:integer):prop' has  been translated
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    x_1 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_1
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 12, characters 0-202:
  the value not_lazy_and:
   - Pure: false
   - Preconditions:
    + File "../suite/terms/lib.mli", line 14, characters 13-36:
      `not not (x_2:int = x_2:int):prop /\ ((1:integer / 0:integer):integer = 2:
  integer):prop' has  been translated
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    x_2 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_2
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 18, characters 0-209:
  the value scope1:
   - Pure: false
   - Preconditions:
    + File "../suite/terms/lib.mli", line 20, characters 13-30:
      `let x_4:bool = (True ):bool in (x_4:bool = (True ):bool):prop' has  been translated
  - Postconditions:
    + File "../suite/terms/lib.mli", line 21, characters 13-30:
      `let y_4:bool = (True ):bool in (y_4:bool = (True ):bool):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    x_3 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_3
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 25, characters 0-264:
  the value if_forall:
   - Pure: false
   - Preconditions:
    + File "../suite/terms/lib.mli", line 27, characters 13-70:
      `if forall i:integer. (0:integer <= i:integer):prop /\ (i:integer < 10:
  integer):prop -> not ((integer_of_int 
  x_5:int):integer = i:integer):prop then ((integer_of_int 
  x_5:int):integer = 10:integer):prop else ((integer_of_int 
  x_5:int):integer = 3:integer):prop' has  been translated
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    x_5 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_5
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 29, characters 0-180:
  the value equiv:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 31, characters 12-31:
      `(1:integer = 2:integer):prop <-> (2:integer = 3:integer):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    () is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_6
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 33, characters 0-206:
  the value exists_:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 35, characters 12-42:
      `exists x_6:integer. (0:integer <= x_6:integer):prop /\ (x_6:integer < 10:
  integer):prop /\ (x_6:integer = 3:integer):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    () is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_7
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 39, characters 0-24:
  the type t:
  - Invariants:
    
  - Equality: has not been derived
  - Comparison: has not been derived
  - Copy: has not been derived
  File "../suite/terms/lib.mli", line 41, characters 0-219:
  the value a:
   - Pure: false
   - Preconditions:
    + File "../suite/terms/lib.mli", line 43, characters 13-76:
      `(match x_7:t with
  | A -> (True ):bool
  | B s:string -> (False ):bool
  end::bool = (True ):bool):prop' has  been translated
    + File "../suite/terms/lib.mli", line 46, characters 13-18:
      `(x_7:t = (A ):t):prop' has  been translated
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    x_7 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_8
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 48, characters 0-207:
  the value b:
   - Pure: false
   - Preconditions:
    + File "../suite/terms/lib.mli", line 50, characters 13-76:
      `(match x_8:t with
  | A -> (False ):bool
  | B _ -> (True ):bool
  end::bool = (True ):bool):prop' has  been translated
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    x_8 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_9
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 54, characters 0-27:
  the type peano:
  - Invariants:
    
  - Equality: has not been derived
  - Comparison: has not been derived
  - Copy: has not been derived
  File "../suite/terms/lib.mli", line 56, characters 0-160:
  the value succ:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 58, characters 14-21:
      `(y_10:peano = (S 
  x_9:peano):peano):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    x_9 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_10
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 60, characters 0-224:
  the value add:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 62, characters 16-32:
      `not (x_10:peano = (O ):peano):prop -> not (z:peano = (O ):peano):prop' has  been translated
    + File "../suite/terms/lib.mli", line 63, characters 16-32:
      `not (y_11:peano = (O ):peano):prop -> not (z:peano = (O ):peano):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    x_10 is not consumed and is not modified
    + Invariants involved:
      
    y_11 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    z
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 65, characters 0-226:
  the value bad_add:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 67, characters 14-30:
      `not (x_11:peano = (O ):peano):prop -> not (z_1:peano = (O ):peano):prop' has  been translated
    + File "../suite/terms/lib.mli", line 68, characters 14-30:
      `not (y_12:peano = (O ):peano):prop -> not (z_1:peano = (O ):peano):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    x_11 is not consumed and is not modified
    + Invariants involved:
      
    y_12 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    z_1
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 70, characters 0-38:
  the type tree:
  - Invariants:
    
  - Equality: has not been derived
  - Comparison: has not been derived
  - Copy: has not been derived
  File "../suite/terms/lib.mli", line 72, characters 4-105:
  the Gospel function __logical_size__017_ has been translatedFile "../suite/terms/lib.mli", line 75, characters 0-204:
  the value size:
   - Pure: true
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 78, characters 14-29:
      `not (t_2:tree = (E ):tree):prop -> ((integer_of_int 
  s_1:int):integer > 0:integer):prop' has  been translated
    + File "../suite/terms/lib.mli", line 79, characters 14-24:
      `((integer_of_int  s_1:int):integer = (size 
  t_2:tree):integer):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    t_2 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    s_1
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 81, characters 0-183:
  the value size_wrong_spec:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 83, characters 12-23:
      `not (s_2:int = (size_1 
  t_3:tree):int):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    t_3 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    s_2
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 85, characters 0-285:
  the value test_tree:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 87, characters 14-110:
      `(b:bool = (True ):bool):prop <-> (match t_4:tree with
  | E -> (True ):bool
  | N (l_1:tree, x_12:int, t_5:tree) -> if (l_1:tree = t_5:tree):prop && ((integer_of_int 
                                        x_12:int):integer = 0:integer):prop then (True ):
                                        bool else (False ):bool
  end::bool = (True ):bool):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    t_4 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    b
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 91, characters 0-203:
  the value make_tree:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 93, characters 14-29:
      `(t_6:tree = (N 
  l_2:tree x_13:int r_1:tree):tree):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    l_2 is not consumed and is not modified
    + Invariants involved:
      
    x_13 is not consumed and is not modified
    + Invariants involved:
      
    r_1 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    t_6
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 95, characters 0-282:
  the value fill:
   - Pure: false
   - Preconditions:
    + File "../suite/terms/lib.mli", line 97, characters 15-43:
      ` start <= stop <= Array.leng' has  been translated
  - Postconditions:
    + File "../suite/terms/lib.mli", line 98, characters 15-46:
      `((integer_of_int  start:int):integer <= (integer_of_int 
  stop:int):integer):prop /\ ((integer_of_int  stop:int):integer <= (length 
  a_1:int array):integer):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    t_7 is not consumed and is not modified
    + Invariants involved:
      
    a_1 is not consumed and is not modified
    + Invariants involved:
      
    start is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    stop
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 100, characters 0-58:
  the type alt_tree:
  - Invariants:
    
  - Equality: has not been derived
  - Comparison: has not been derived
  - Copy: has not been derived
  File "../suite/terms/lib.mli", line 102, characters 0-257:
  the value make_alt_tree:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 104, characters 14-45:
      `let c:alt_tree * int * alt_tree = (tuple3 
  l_3:alt_tree x_14:int r_2:alt_tree):alt_tree * int * alt_tree in (t_8:
                                                                    alt_tree = (Nalt 
  c:alt_tree * int * alt_tree):alt_tree):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    l_3 is not consumed and is not modified
    + Invariants involved:
      
    x_14 is not consumed and is not modified
    + Invariants involved:
      
    r_2 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    t_8
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 106, characters 0-166:
  the value ref_access:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 108, characters 12-18:
      `(y_13:'a = (prefix ! 
  x_15:'a ref):'a):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    x_15 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_13
    + Invariants involved:
      

  $ ortac report ../suite/exceptions/lib.mli
  Lib
  File "../suite/exceptions/lib.mli", line 1, characters 0-150:
  the value raise_oom:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    i is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    o
    + Invariants involved:
      
  File "../suite/exceptions/lib.mli", line 4, characters 0-180:
  the value raise_stackoverflow:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    i_1 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    o_1
    + Invariants involved:
      
  File "../suite/exceptions/lib.mli", line 7, characters 0-198:
  the value undeclared_raise_notfound:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    i_2 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    o_2
    + Invariants involved:
      
  File "../suite/exceptions/lib.mli", line 10, characters 0-210:
  the value bad_raise_notfound:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    
  - Exceptional postconditions:
    + the clauses concerning the exception Not_found have been translated
  - Arguments:
    i_3 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    o_3
    + Invariants involved:
      
  File "../suite/exceptions/lib.mli", line 14, characters 0-200:
  the value raise_notfound:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    
  - Exceptional postconditions:
    + the clauses concerning the exception Not_found have been translated
  - Arguments:
    i_4 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    o_4
    + Invariants involved:
      
  File "../suite/exceptions/lib.mli", line 18, characters 0-323:
  the value raise_invalidarg:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    
  - Exceptional postconditions:
    + the clauses concerning the exception Not_found have been translated
    + the clauses concerning the exception Invalid_argument have been translated
    + the clauses concerning the exception Invalid_argument have been translated
  - Arguments:
    i_5 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    o_5
    + Invariants involved:
      
  File "../suite/exceptions/lib.mli", line 24, characters 0-166:
  the value check:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    x is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y
    + Invariants involved:
      
  File "../suite/exceptions/lib.mli", line 28, characters 0-174:
  the value bad_check:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    x_1 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_1
    + Invariants involved:
      
  File "../suite/exceptions/lib.mli", line 32, characters 0-176:
  the value bad_check2:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    x_2 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_2
    + Invariants involved:
      

  $ ortac report ../suite/types/lib.mli
  Lib
  File "../suite/types/lib.mli", line 1, characters 0-175:
  the type t:
  - Invariants:
    + File "../suite/types/lib.mli", line 3, characters 14-25:
      `((t_1:t).y = (False ):bool):prop' has been translated
  - Equality: has not been derived
  - Comparison: has not been derived
  - Copy: has not been derived
  File "../suite/types/lib.mli", line 5, characters 0-172:
  the value v:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/types/lib.mli", line 7, characters 12-19:
      `(x:int = (t_2:t).x_1):prop' has  been translated
    + File "../suite/types/lib.mli", line 8, characters 12-19:
      `(y_1:bool = (t_2:t).y):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    x is not consumed and is not modified
    + Invariants involved:
      
    y_1 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    t_2
    + Invariants involved:
      + File "../suite/types/lib.mli", line 3, characters 14-25:
        `((t_1:t).y = (False ):bool):prop' has been translated
  File "../suite/types/lib.mli", line 10, characters 0-127:
  the constant value e:
  - Checks:
    + File "../suite/types/lib.mli", line 11, characters 12-20:
      `((integer_of_int 
  (e:t).x_1):integer >= 0:integer):prop' has  been translated
  - Invariants:
    + File "../suite/types/lib.mli", line 3, characters 14-25:
      `((t_1:t).y = (False ):bool):prop' has been translatedFile "../suite/types/lib.mli", line 13, characters 0-152:
  the value get_x:
   - Pure: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/types/lib.mli", line 15, characters 12-19:
      `(r:int = (t_3:t).x_1):prop' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    t_3 is not consumed and is not modified
    + Invariants involved:
      + File "../suite/types/lib.mli", line 3, characters 14-25:
        `((t_1:t).y = (False ):bool):prop' has been translated
  - Return:
    r
    + Invariants involved:
      
