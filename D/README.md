# D. Полнота исчисления высказываний

```
Имя входного файла:                 стандартный ввод
Имя выходного файла:                стандартный вывод
Ограничение по времени:             15 секунд
Ограничение по памяти:              512 мегабайт
```

На вход вашей программе дается утверждение α в грамматике из предыдущих заданий.
От вас требуется найти:

- Набор гипотез Γ1 со следующими свойствами:

    - Γ1 состоит только из переменных
    - Γ1 ⊢ α

    В этом случае вам нужно вывести доказательство Γ1 ⊢ α.

- Если такого набора гипотез не нашлось, то нужно найти наименьший набор гипотез Γ2 :
    - Γ2 состоит только из отрицаний переменных
    - Γ2 ⊢ ¬α:

    В этом случае вам нужно вывести доказательство Γ2 ⊢ ¬α.

- Если и такого набора гипотез не нашлось, то выведите «:(».

Если среди предыдущих случаев существует несколько подходящих наборов гипотез (а если
такие наборы есть, то их всегда бесконечно много), то требуется вывести любой подходящий набор
наименьшего размера.

## Формат входных данных

Во входном файле задано утверждение α. Размер входного файла не превышает 50 байт. Количество 
различных переменных, входящих в α, не превосходит 3.

## Формат выходных данных

Если требуемого набора гипотез не существует, в единственной строке выведите «:(». Иначе
выведите требуемое в условии доказательство, используя грамматику из предыдущих заданий.

## Примеры

```
стандартный ввод
!A
стандартный вывод
:(
```
```
стандартный ввод
A -> A & B
стандартный вывод
B |- A -> A & B
B
B -> A -> B
A -> B
A -> B -> A & B
(A -> B) -> (A -> B -> A & B) -> A -> A & B
(A -> B -> A & B) -> A -> A & B
A -> A & B
```