Система типов
=============

Если вы читали прошлые главы, то, скорее всего, обратили внимание, что мы нигде не указывали типы переменных и возвращаемых функциями данных. Так же мы не указывали тип данных при сопоставлении с образцом. Например, кортеж `{A, B}` может принимать значение `{someAtom, 100}`, `{100.0, "Some string"}` и т.д.

Если данные будет невозможно сопоставить с указанным образцом, будет сгенерированна ошибка. Это произойдет непосредственно в момент выполнения кода. Это происходит потому что Erlang - **динамически типизированный** язык. Компилятор не может отловить все ошибки на этапе компиляции. Поэтому многие ошибки генерируются во время выполнения программы.

Почему Erlang не имеет статической типизации? Немало копий было сломано в спорах о том, какая типизация лучше: статическая или динамическая. У каждой есть свои плюсы и минусы. Один из главных минусов динамической типизации - невозможность поиска ошибок, связанных с типами, на этапе компиляции. Из-за этого эти ошибки возникают в рантайме и программа "падает", что снижает общую надежность программного продукта. Но Erlang решает эту проблему другим, более эффективным способом.

Идеалогия Erlang: "Если оно сломалось - пусть ломается". Язык обладает инструментами, позволяющими изолировать и корректно обрабатывать все ошибки, которые могут возникнуть. Это делает программы на Erlang очень живучими. Классический пример живучести программ на Erlang, который приводят - ПО комутаторов Ericsson AXD 301 ATM. Этот продукт имеет больше миллиона строк кода и аптайм 99.9999999%. Впечатляет.

В общем, Erlang не стремиться избегать ошибок. Он предлагает правильно их обрабатывать.

Так же Erlang относится к языкам с **сильной типизацией**. Это значит, что приведения типов нужно производить явно. Если мы будет использовать в одном выражении несовместимые типы данных, то получим ошибку:
```erlang
1> 10 - "5".
** exception error: bad argument in an arithmetic expression
    in operator  -/2
        called as 10 - "5"
```

Преобразование типов
--------------------

Для преобразования типов в Erlang используются функции стандартной библиотеки (находятся в модуле `Erlang`). Имена этих функций формируются по следующей схеме: `<исходный_тип>_to_<небходимый_тип>`. Вот полный перечень таких функций:
- [`atom_to_binary/2`](http://www.erlang.org/doc/man/erlang.html#atom_to_binary-2)
- [`atom_to_list/1`](http://www.erlang.org/doc/man/erlang.html#atom_to_list-1)
- [`binary_to_atom/2`](http://www.erlang.org/doc/man/erlang.html#binary_to_atom-2)
- [`binary_to_existing_atom/2`](http://www.erlang.org/doc/man/erlang.html#binary_to_existing_atom-2)
- [`binary_to_list/1`](http://www.erlang.org/doc/man/erlang.html#binary_to_list-1)
- [`bitstring_to_list/1`](http://www.erlang.org/doc/man/erlang.html#bitstring_to_list-1)
- [`binary_to_term/1`](http://www.erlang.org/doc/man/erlang.html#binary_to_term-1)
- [`float_to_list/1`](http://www.erlang.org/doc/man/erlang.html#float_to_list-1)
- [`fun_to_list/1`](http://www.erlang.org/doc/man/erlang.html#fun_to_list-1)
- [`integer_to_list/1`](http://www.erlang.org/doc/man/erlang.html#integer_to_list-1)
- [`integer_to_list/2`](http://www.erlang.org/doc/man/erlang.html#integer_to_list-2)
- [`iolist_to_binary/1`](http://www.erlang.org/doc/man/erlang.html#iolist_to_binary-1)
- [`iolist_to_atom/1`](http://www.erlang.org/doc/man/erlang.html#iolist_to_atom-1)
- [`list_to_atom/1`](http://www.erlang.org/doc/man/erlang.html#list_to_atom-1)
- [`list_to_binary/1`](http://www.erlang.org/doc/man/erlang.html#list_to_binary-1)
- [`list_to_bitstring/1`](http://www.erlang.org/doc/man/erlang.html#list_to_bitstring-1)
- [`list_to_existing_atom/1`](http://www.erlang.org/doc/man/erlang.html#list_to_existing_atom-1)
- [`list_to_float/1`](http://www.erlang.org/doc/man/erlang.html#list_to_float-1)
- [`list_to_integer/2`](http://www.erlang.org/doc/man/erlang.html#list_to_integer-2)
- [`list_to_pid/1`](http://www.erlang.org/doc/man/erlang.html#list_to_pid-1)
- [`list_to_tuple/1`](http://www.erlang.org/doc/man/erlang.html#list_to_tuple-1)
- [`pid_to_list/1`](http://www.erlang.org/doc/man/erlang.html#pid_to_list-1)
- [`port_to_list/1`](http://www.erlang.org/doc/man/erlang.html#port_to_list-1)
- [`ref_to_list/1`](ref_to_list-1)
- [`term_to_binary/1`](term_to_binary-1)
- [`term_to_binary/2`](term_to_binary-2)
- [`tuple_to_list/1`](tuple_to_list-1)

И пример их использования:
```erlang
1> erlang:integer_to_list(123).
"123"
2> erlang:atom_to_list(false).
"false"
3> erlang:iolist_to_atom(123).
** exception error: bad argument
    in function  iolist_to_atom/1
        called as iolist_to_atom(123)
```

Проверка типов
--------------

Большинство типов данных в Erlang можно отличить визуально. Cписки заключаются в квадратные скобки, кортежи в фигурные, а строки в двойные кавычки и т.д. Благодаря этому обеспечивается простейшая проверка типов. Например, паттерн для сопастовления `[x|xs]` может быть сопоставлен только со списком. В противном случае операзия закончится неудачей.

Но таких элементарных проверок недостаточно. Зачастую нужно проверить, к какому типу относится та или иная переменная. Для этого в Erlang есть ряд функций, которые принимают один аргумент (иногда больше) и если его тип соответствует ожидаемому, возвращают `true`, иначе `false`. Эти функции можно использовать в охранных выражениях и проверяемый ими тип понятен из их названия.
- [`is_atom/1`](http://www.erlang.org/doc/man/erlang.html#is_atom-1)
- [`is_binary/1`](http://www.erlang.org/doc/man/erlang.html#is_binary-1)
- [`is_bitstring/1`](http://www.erlang.org/doc/man/erlang.html#is_bitstring-1)
- [`is_boolean/1`](http://www.erlang.org/doc/man/erlang.html#is_boolean-1)
- [`is_builtin/3`](http://www.erlang.org/doc/man/erlang.html#is_builtin-3)
- [`is_float/1`](http://www.erlang.org/doc/man/erlang.html#is_float-1) 
- [`is_function/1`](http://www.erlang.org/doc/man/erlang.html#is_function-1)
- [`is_function/2`](http://www.erlang.org/doc/man/erlang.html#is_function-2)
- [`is_integer/1`](http://www.erlang.org/doc/man/erlang.html#is_integer-1)
- [`is_list/1`](http://www.erlang.org/doc/man/erlang.html#is_list-1)
- [`is_number/1`](http://www.erlang.org/doc/man/erlang.html#is_number-1)
- [`is_pid/1`](http://www.erlang.org/doc/man/erlang.html#is_pid-1)
- [`is_port/1`](http://www.erlang.org/doc/man/erlang.html#is_port-1)
- [`is_record/2`](http://www.erlang.org/doc/man/erlang.html#is_record-2)
- [`is_record/3`](http://www.erlang.org/doc/man/erlang.html#is_record-3)
- [`is_reference/1`](http://www.erlang.org/doc/man/erlang.html#is_reference-1)
- [`is_tuple/1`](http://www.erlang.org/doc/man/erlang.html#is_tuple-1)

Интересный момент: в Erlang нет функции, которая возвращала бы тип переданной переменной (на подобии `type_of()`). И на первый взгляд это может показаться странным, медь одна универсальная функция удобнее, чем куча узконаправленных. Ответ на этот вопрос кроется в идеалогии языка: *программа должна делать только то, что описанно явно. Все остальное должно привести к ошибке*. Более подробно данный подход будет рассматриваться в главе, посвященной обработке ошибок.

Эти типы данных можно комбинировать между собой при помощи списокв и кортежей для создания более гибких и универсальных типов.
