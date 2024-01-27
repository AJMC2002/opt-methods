#set page(
    paper: "a4",
    margin: (x: 1in, y: 1in),
)

#set text(size: 12pt)

#set align(center)

#image("fefu_logo.jpg", width: 4%)

#text(
    size: 10pt,
    [
    МИНИСТЕРСТВО НАУКИ И ВЫСШЕГО ОБРАЗОВАНИЯ И НАУКИ РОССИЙСКОЙ ФЕДЕРАЦИИ

    Федеральное государственное автономное образовательное учреждение высшего образования

    *«Дальневосточный федеральный университет»*

    (ДВФУ)
])

#v(0.5fr)

#line(length: 100%, stroke: 2pt)

#v(0.5fr)

*ИНСТИТУТ МАТЕМАТИКИ И КОМПЬЮТЕРНЫХ ТЕХНОЛОГИЙ*

#v(1fr)

*Департамент математического и компьютерного моделирования*

#v(1fr)

*ЛАБОРАТОРНАЯ РАБОТА №4*

По основной образовательной программе подготовки бакалавров
направлению 01.03.02 Прикладная математика и информатика
профиль «Системное программирование»

#v(1fr)

#grid(
    columns: (1fr,1fr),
    [],
    [
        #set align(left)
        Студент группы Б9121-02.03.01сцт

        Москера Креспо Адриан Хосуэ

        «26» января 2024 г.

        #v(24pt)

        Преподаватель кандидат физико-математических наук

        #underline("                                                        ") #text(size: 10pt, "(подпись)")

        Яковлев Анатолий Александрович

        «#underline("      ")» #underline("                         ") 2023 г.
    ]
)

#v(1fr)

г. Владивосток

2023

#v(1fr)

#pagebreak()

#set align(left)

#set par(
  first-line-indent: 0em,
  justify: true,
)

= Постановка задачи
Пусть дана матричная игра, заданная матрицей А размерности $6 times 8$. Необходимо найти верхнюю и нижнюю цену игры и равновесное решение в смешанных стратегиях.

$ 
mat(
-10.46, 14.80, -21.25, 30.84, -4.49, 24.20, 47.45, 5.68;
-24.87, 33.86, 45.66, -22.12, -16.24, 45.51, 35.63, -11.31;
-33.39, 19.95, 5.01, -10.35, 14.18, 40.46, 35.13, -26.32;
16.94, -16.97, -4.94, -40.60, -11.14, -41.60, 15.08, -22.84;
-4.66, 14.45, -26.48, 29.28, 38.81, 20.88, 24.52, 35.68;
-43.57, -1.90, -19.70, 4.80, 12.11, -3.41, 29.07, -22.78
)
$

Нижняя цена игры:
$ underline(A) = op("max", limits:#true)_i op("min", limits:#true)_j a_(i j) = -21.254878614230552 $

Верхняя цена игры:
$ overline(A) = op("min", limits:#true)_j op("max", limits:#true)_i a_(i j) = 16.937688915945937 $

Искать равновесное решение в смешанных стратегиях будем с помощью симплекс-метода. Для этого необходимо сделать матрицу $А$ неотрицательной, поэтому к каждому элементу матрицы $А$ добавим модуль минимального элемента $beta$ матрицы $А$.

$ beta = min_(i j) a_(i j) = -43.574988621921335 $

Получается неотрицательная матрица $hat(A)$.

$ hat(A) = mat(
33.12, 58.38, 22.32, 74.42, 39.09, 67.78, 91.02, 49.25;
18.70, 77.43, 89.23, 21.45, 27.34, 89.09, 79.21, 32.26;
10.18, 63.53, 48.59, 33.22, 57.76, 84.04, 78.71, 17.26;
60.51, 26.61, 38.64, 2.98, 32.44, 1.97, 58.65, 20.74;
38.91, 58.02, 17.09, 72.86, 82.38, 64.45, 68.09, 79.25;
0.00, 41.68, 23.87, 48.38, 55.68, 40.17, 72.65, 20.80
) $

Необходимо решить следующие задачи:

#grid(
  columns: (1fr, 1fr),
  [ 1. $ cases(
    y dot e arrow "max",
    hat(A) y <= e^T,
    y >= 0
  ) $
  ],
  [ 2. $ cases(
    e dot x arrow "min",
    hat(A)^T x >= e,
    x >= 0
  ) $
  ]
)

В этом случае оптимальная стратегия первого игрока будет найдена по формуле:

$ p^* = x/(||x||) $.

А оптимальная стратегия второго игрока будет найдена по формуле:

$ q^* = y/(||y||) $.

Цена игры будет равна:

$ phi = 1/alpha - |beta| $

где $alpha$ – значение целевой функции, полученной в результате решения задач линейной оптимизации.

$q^*$ – находится прямой задачей;
$p^*$ – находится двойственной задачей.

= Прямая задача

Задача приводится к каноническому виду. За начальную угловую точку берём $y_0=(0,e)$.

$
mat(
-1.00, -1.00, -1.00, -1.00, -1.00, -1.00, -1.00, -1.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00;
33.12, 58.38, 22.32, 74.42, 39.09, 67.78, 91.02, 49.25, 1.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1.00;
18.70, 77.43, 89.23, 21.45, 27.34, 89.09, 79.21, 32.26, 0.00, 1.00, 0.00, 0.00, 0.00, 0.00, 1.00;
10.18, 63.53, 48.59, 33.22, 57.76, 84.04, 78.71, 17.26, 0.00, 0.00, 1.00, 0.00, 0.00, 0.00, 1.00;
60.51, 26.61, 38.64, 2.98, 32.44, 1.97, 58.65, 20.74, 0.00, 0.00, 0.00, 1.00, 0.00, 0.00, 1.00;
38.91, 58.02, 17.09, 72.86, 82.38, 64.45, 68.09, 79.25, 0.00, 0.00, 0.00, 0.00, 1.00, 0.00, 1.00;
0.00, 41.68, 23.87, 48.38, 55.68, 40.17, 72.65, 20.80, 0.00, 0.00, 0.00, 0.00, 0.00, 1.00, 1.00
)\
$
$
"разрешающая строка" = "№"5\
"разрешающий столбец" = "№"1\
"разрешающий элемент" = 60.512677537867276\
$
$
mat(
0.00, -0.56, -0.36, -0.95, -0.46, -0.97, -0.03, -0.66, 0.00, 0.00, 0.00, 0.02, 0.00, 0.00, 0.02;
0.00, 43.81, 1.17, 72.79, 21.33, 66.70, 58.92, 37.90, 1.00, 0.00, 0.00, -0.55, 0.00, 0.00, 0.45;
0.00, 69.21, 77.29, 20.53, 17.31, 88.48, 61.08, 25.85, 0.00, 1.00, 0.00, -0.31, 0.00, 0.00, 0.69;
0.00, 59.05, 42.09, 32.72, 52.30, 83.71, 68.84, 13.77, 0.00, 0.00, 1.00, -0.17, 0.00, 0.00, 0.83;
1.00, 0.44, 0.64, 0.05, 0.54, 0.03, 0.97, 0.34, 0.00, 0.00, 0.00, 0.02, 0.00, 0.00, 0.02;
0.00, 40.91, -7.76, 70.94, 61.52, 63.18, 30.37, 65.92, 0.00, 0.00, 0.00, -0.64, 1.00, 0.00, 0.36;
0.00, 41.68, 23.87, 48.38, 55.68, 40.17, 72.65, 20.80, 0.00, 0.00, 0.00, 0.00, 0.00, 1.00, 1.00
)\
$
$
"разрешающая строка" = "№"6\
"разрешающий столбец" = "№"6\
"разрешающий элемент" = 63.18152294907165\
$
$
mat(
0.00, 0.07, -0.48, 0.14, 0.48, 0.00, 0.43, 0.35, 0.00, 0.00, 0.00, 0.01, 0.02, 0.00, 0.02;
0.00, 0.62, 9.36, -2.11, -43.61, 0.00, 26.86, -31.68, 1.00, 0.00, 0.00, 0.13, -1.06, 0.00, 0.08;
0.00, 11.91, 88.15, -78.82, -68.84, 0.00, 18.55, -66.45, 0.00, 1.00, 0.00, 0.59, -1.40, 0.00, 0.19;
0.00, 4.85, 52.36, -61.27, -29.21, 0.00, 28.60, -73.56, 0.00, 0.00, 1.00, 0.68, -1.32, 0.00, 0.36;
1.00, 0.42, 0.64, 0.01, 0.50, 0.00, 0.95, 0.31, 0.00, 0.00, 0.00, 0.02, -0.00, 0.00, 0.02;
0.00, 0.65, -0.12, 1.12, 0.97, 1.00, 0.48, 1.04, 0.00, 0.00, 0.00, -0.01, 0.02, 0.00, 0.01;
0.00, 15.67, 28.81, 3.27, 16.57, 0.00, 53.34, -21.11, 0.00, 0.00, 0.00, 0.41, -0.64, 1.00, 0.77
)\
$
$
"разрешающая строка" = "№"3\
"разрешающий столбец" = "№"3\
"разрешающий элемент" = 88.15166460660836\
$
$
mat(
0.00, 0.13, 0.00, -0.29, 0.10, 0.00, 0.54, -0.01, 0.00, 0.01, 0.00, 0.01, 0.01, 0.00, 0.02;
0.00, -0.64, 0.00, 6.26, -36.30, 0.00, 24.89, -24.62, 1.00, -0.11, 0.00, 0.07, -0.91, 0.00, 0.06;
0.00, 0.14, 1.00, -0.89, -0.78, 0.00, 0.21, -0.75, 0.00, 0.01, 0.00, 0.01, -0.02, 0.00, 0.00;
0.00, -2.23, 0.00, -14.45, 11.69, 0.00, 17.58, -34.08, 0.00, -0.59, 1.00, 0.33, -0.49, 0.00, 0.25;
1.00, 0.33, 0.00, 0.59, 1.01, 0.00, 0.82, 0.79, 0.00, -0.01, 0.00, 0.01, 0.01, 0.00, 0.01;
0.00, 0.66, 0.00, 1.01, 0.88, 1.00, 0.51, 0.95, 0.00, 0.00, 0.00, -0.01, 0.01, 0.00, 0.01;
0.00, 11.78, 0.00, 29.03, 39.07, 0.00, 47.28, 0.61, 0.00, -0.33, 0.00, 0.22, -0.18, 1.00, 0.71
)\
$
$
"разрешающая строка" = "№"6\
"разрешающий столбец" = "№"4\
"разрешающий элемент" = 1.0131157642947446\
$
$
mat(
0.00, 0.32, 0.00, 0.00, 0.36, 0.29, 0.68, 0.27, 0.00, 0.01, 0.00, 0.01, 0.01, 0.00, 0.02;
0.00, -4.75, 0.00, 0.00, -41.73, -6.18, 21.76, -30.50, 1.00, -0.11, 0.00, 0.13, -0.99, 0.00, 0.02;
0.00, 0.72, 1.00, 0.00, -0.01, 0.88, 0.66, 0.09, 0.00, 0.01, 0.00, -0.00, -0.00, 0.00, 0.01;
0.00, 7.24, 0.00, 0.00, 24.21, 14.27, 24.81, -20.52, 0.00, -0.57, 1.00, 0.20, -0.30, 0.00, 0.33;
1.00, -0.05, 0.00, 0.00, 0.50, -0.58, 0.52, 0.24, 0.00, -0.01, 0.00, 0.02, 0.00, 0.00, 0.01;
0.00, 0.66, 0.00, 1.00, 0.87, 0.99, 0.50, 0.94, 0.00, 0.00, 0.00, -0.01, 0.01, 0.00, 0.01;
0.00, -7.25, 0.00, 0.00, 13.91, -28.65, 32.76, -26.63, 0.00, -0.37, 0.00, 0.48, -0.58, 1.00, 0.54
)
$

$ underline("Результат") $

#grid(
  columns: (1fr, 1fr, 1fr),
  [ $ y = vec( 0.01, 0.00, 0.01, 0.01, 0.00, 0.00, 0.00, 0.00 ) $ ],
  [ #v(0.35fr) \ $ alpha = 2.4747741869746445e-2 $ \ #v(1fr) ],
  [ $ q^* = y/(||y||) = vec( 0.77, 0.00, 0.50, 0.39, 0.00, 0.00, 0.00, 0.00 ) $ ]
)

= Двойственная задача

Двойственная задача приводится к каноническому виду, далее ищется начальная угловая точка, решая вспомогательную задачу.

== Решение вспомогательной задачи

$
script(mat(
0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 0.00;
33.12, 18.70, 10.18, 60.51, 38.91, 0.00, -1.00, -0.00, -0.00, -0.00, -0.00, -0.00, -0.00, -0.00, 1.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1.00;
58.38, 77.43, 63.53, 26.61, 58.02, 41.68, -0.00, -1.00, -0.00, -0.00, -0.00, -0.00, -0.00, -0.00, 0.00, 1.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1.00;
22.32, 89.23, 48.59, 38.64, 17.09, 23.87, -0.00, -0.00, -1.00, -0.00, -0.00, -0.00, -0.00, -0.00, 0.00, 0.00, 1.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1.00;
74.42, 21.45, 33.22, 2.98, 72.86, 48.38, -0.00, -0.00, -0.00, -1.00, -0.00, -0.00, -0.00, -0.00, 0.00, 0.00, 0.00, 1.00, 0.00, 0.00, 0.00, 0.00, 1.00;
39.09, 27.34, 57.76, 32.44, 82.38, 55.68, -0.00, -0.00, -0.00, -0.00, -1.00, -0.00, -0.00, -0.00, 0.00, 0.00, 0.00, 0.00, 1.00, 0.00, 0.00, 0.00, 1.00;
67.78, 89.09, 84.04, 1.97, 64.45, 40.17, -0.00, -0.00, -0.00, -0.00, -0.00, -1.00, -0.00, -0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1.00, 0.00, 0.00, 1.00;
91.02, 79.21, 78.71, 58.65, 68.09, 72.65, -0.00, -0.00, -0.00, -0.00, -0.00, -0.00, -1.00, -0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1.00, 0.00, 1.00;
49.25, 32.26, 17.26, 20.74, 79.25, 20.80, -0.00, -0.00, -0.00, -0.00, -0.00, -0.00, -0.00, -1.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1.00, 1.00
))
$

Базисные столбцы выделяются с помощью элементарных преобразований строк. К первой строке добавляются остальные строки, умноженные на -1. Получается:

$
script(mat(
-435.37, -434.72, -393.28, -242.54, -481.06, -303.22, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, -8.00;
33.12, 18.70, 10.18, 60.51, 38.91, 0.00, -1.00, -0.00, -0.00, -0.00, -0.00, -0.00, -0.00, -0.00, 1.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1.00;
58.38, 77.43, 63.53, 26.61, 58.02, 41.68, -0.00, -1.00, -0.00, -0.00, -0.00, -0.00, -0.00, -0.00, 0.00, 1.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1.00;
22.32, 89.23, 48.59, 38.64, 17.09, 23.87, -0.00, -0.00, -1.00, -0.00, -0.00, -0.00, -0.00, -0.00, 0.00, 0.00, 1.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1.00;
74.42, 21.45, 33.22, 2.98, 72.86, 48.38, -0.00, -0.00, -0.00, -1.00, -0.00, -0.00, -0.00, -0.00, 0.00, 0.00, 0.00, 1.00, 0.00, 0.00, 0.00, 0.00, 1.00;
39.09, 27.34, 57.76, 32.44, 82.38, 55.68, -0.00, -0.00, -0.00, -0.00, -1.00, -0.00, -0.00, -0.00, 0.00, 0.00, 0.00, 0.00, 1.00, 0.00, 0.00, 0.00, 1.00;
67.78, 89.09, 84.04, 1.97, 64.45, 40.17, -0.00, -0.00, -0.00, -0.00, -0.00, -1.00, -0.00, -0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1.00, 0.00, 0.00, 1.00;
91.02, 79.21, 78.71, 58.65, 68.09, 72.65, -0.00, -0.00, -0.00, -0.00, -0.00, -0.00, -1.00, -0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1.00, 0.00, 1.00;
49.25, 32.26, 17.26, 20.74, 79.25, 20.80, -0.00, -0.00, -0.00, -0.00, -0.00, -0.00, -0.00, -1.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1.00, 1.00
))
$
$
"разрешающая строка" = "№"6\
"разрешающий столбец" = "№"5\
"разрешающий элемент" = 82.38116756630728\
$
$
script(mat(
-207.12, -275.09, -56.01, -53.11, 0.00, 21.93, 1.00, 1.00, 1.00, 1.00, -4.84, 1.00, 1.00, 1.00, 0.00, 0.00, 0.00, 0.00, 5.84, 0.00, 0.00, 0.00, -2.16;
14.65, 5.79, -17.10, 45.19, 0.00, -26.30, -1.00, 0.00, 0.00, 0.00, 0.47, 0.00, 0.00, 0.00, 1.00, 0.00, 0.00, 0.00, -0.47, 0.00, 0.00, 0.00, 0.53;
30.85, 58.18, 22.85, 3.76, 0.00, 2.46, 0.00, -1.00, 0.00, 0.00, 0.70, 0.00, 0.00, 0.00, 0.00, 1.00, 0.00, 0.00, -0.70, 0.00, 0.00, 0.00, 0.30;
14.21, 83.56, 36.60, 31.91, 0.00, 12.32, 0.00, 0.00, -1.00, 0.00, 0.21, 0.00, 0.00, 0.00, 0.00, 0.00, 1.00, 0.00, -0.21, 0.00, 0.00, 0.00, 0.79;
39.85, -2.72, -17.86, -25.71, 0.00, -0.87, 0.00, 0.00, 0.00, -1.00, 0.88, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1.00, -0.88, 0.00, 0.00, 0.00, 0.12;
0.47, 0.33, 0.70, 0.39, 1.00, 0.68, -0.00, -0.00, -0.00, -0.00, -0.01, -0.00, -0.00, -0.00, 0.00, 0.00, 0.00, 0.00, 0.01, 0.00, 0.00, 0.00, 0.01;
37.20, 67.70, 38.85, -23.41, 0.00, -3.40, 0.00, 0.00, 0.00, 0.00, 0.78, -1.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, -0.78, 1.00, 0.00, 0.00, 0.22;
58.71, 56.61, 30.97, 31.84, 0.00, 26.62, 0.00, 0.00, 0.00, 0.00, 0.83, 0.00, -1.00, 0.00, 0.00, 0.00, 0.00, 0.00, -0.83, 0.00, 1.00, 0.00, 0.17;
11.65, 5.97, -38.30, -10.47, 0.00, -32.77, 0.00, 0.00, 0.00, 0.00, 0.96, 0.00, 0.00, -1.00, 0.00, 0.00, 0.00, 0.00, -0.96, 0.00, 0.00, 1.00, 0.04
))\
$
$
"разрешающая строка" = "№"8\
"разрешающий столбец" = "№"2\
"разрешающий элемент" = 56.614991151878556\
$
$
script(mat(
78.17, 0.00, 94.47, 101.60, 0.00, 151.30, 1.00, 1.00, 1.00, 1.00, -0.82, 1.00, -3.86, 1.00, 0.00, 0.00, 0.00, 0.00, 1.82, 0.00, 4.86, 0.00, -1.32;
8.65, 0.00, -20.27, 41.93, 0.00, -29.02, -1.00, 0.00, 0.00, 0.00, 0.39, 0.00, 0.10, 0.00, 1.00, 0.00, 0.00, 0.00, -0.39, 0.00, -0.10, 0.00, 0.51;
-29.49, 0.00, -8.98, -28.96, 0.00, -24.90, 0.00, -1.00, 0.00, 0.00, -0.15, 0.00, 1.03, 0.00, 0.00, 1.00, 0.00, 0.00, 0.15, 0.00, -1.03, 0.00, 0.12;
-72.45, 0.00, -9.11, -15.09, 0.00, -26.97, 0.00, 0.00, -1.00, 0.00, -1.01, 0.00, 1.48, 0.00, 0.00, 0.00, 1.00, 0.00, 1.01, 0.00, -1.48, 0.00, 0.54;
42.67, 0.00, -16.37, -24.18, 0.00, 0.41, 0.00, 0.00, 0.00, -1.00, 0.92, 0.00, -0.05, 0.00, 0.00, 0.00, 0.00, 1.00, -0.92, 0.00, 0.05, 0.00, 0.12;
0.13, 0.00, 0.52, 0.21, 1.00, 0.52, -0.00, -0.00, -0.00, -0.00, -0.02, -0.00, 0.01, -0.00, 0.00, 0.00, 0.00, 0.00, 0.02, 0.00, -0.01, 0.00, 0.01;
-33.01, 0.00, 1.82, -61.48, 0.00, -35.23, 0.00, 0.00, 0.00, 0.00, -0.21, -1.00, 1.20, 0.00, 0.00, 0.00, 0.00, 0.00, 0.21, 1.00, -1.20, 0.00, 0.01;
1.04, 1.00, 0.55, 0.56, 0.00, 0.47, 0.00, 0.00, 0.00, 0.00, 0.01, 0.00, -0.02, 0.00, 0.00, 0.00, 0.00, 0.00, -0.01, 0.00, 0.02, 0.00, 0.00;
5.46, 0.00, -41.57, -13.82, 0.00, -35.57, 0.00, 0.00, 0.00, 0.00, 0.87, 0.00, 0.11, -1.00, 0.00, 0.00, 0.00, 0.00, -0.87, 0.00, -0.11, 1.00, 0.02
))\
$
$
"разрешающая строка" = "№"7\
"разрешающий столбец" = "№"13\
"разрешающий элемент" = 1.195840862534468\
$
$
script(mat(
-28.36, 0.00, 100.33, -96.80, 0.00, 37.59, 1.00, 1.00, 1.00, 1.00, -1.49, -2.23, 0.00, 1.00, 0.00, 0.00, 0.00, 0.00, 2.49, 3.23, 1.00, 0.00, -1.28;
11.47, 0.00, -20.42, 47.19, 0.00, -26.01, -1.00, 0.00, 0.00, 0.00, 0.41, 0.09, 0.00, 0.00, 1.00, 0.00, 0.00, 0.00, -0.41, -0.09, 0.00, 0.00, 0.51;
-1.12, 0.00, -10.54, 23.87, 0.00, 5.38, 0.00, -1.00, 0.00, 0.00, 0.03, 0.86, 0.00, 0.00, 0.00, 1.00, 0.00, 0.00, -0.03, -0.86, 0.00, 0.00, 0.11;
-31.70, 0.00, -11.35, 60.80, 0.00, 16.51, 0.00, 0.00, -1.00, 0.00, -0.76, 1.23, 0.00, 0.00, 0.00, 0.00, 1.00, 0.00, 0.76, -1.23, 0.00, 0.00, 0.52;
41.34, 0.00, -16.30, -26.65, 0.00, -1.01, 0.00, 0.00, 0.00, -1.00, 0.92, -0.04, 0.00, 0.00, 0.00, 0.00, 0.00, 1.00, -0.92, 0.04, 0.00, 0.00, 0.12;
0.29, 0.00, 0.51, 0.51, 1.00, 0.69, -0.00, -0.00, -0.00, -0.00, -0.02, 0.00, 0.00, -0.00, 0.00, 0.00, 0.00, 0.00, 0.02, -0.00, 0.00, 0.00, 0.01;
-27.61, 0.00, 1.52, -51.41, 0.00, -29.46, 0.00, 0.00, 0.00, 0.00, -0.17, -0.84, 1.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.17, 0.84, -1.00, 0.00, 0.01;
0.55, 1.00, 0.57, -0.35, 0.00, -0.05, 0.00, 0.00, 0.00, 0.00, 0.01, -0.01, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, -0.01, 0.01, 0.00, 0.00, 0.00;
8.37, 0.00, -41.73, -8.41, 0.00, -32.47, 0.00, 0.00, 0.00, 0.00, 0.89, 0.09, 0.00, -1.00, 0.00, 0.00, 0.00, 0.00, -0.89, -0.09, 0.00, 1.00, 0.02
))\
$
$
"разрешающая строка" = "№"3\
"разрешающий столбец" = "№"4\
"разрешающий элемент" = 23.872612592452022\
$
$
script(mat(
-32.91, 0.00, 57.60, 0.00, 0.00, 59.40, 1.00, -3.05, 1.00, 1.00, -1.36, 1.26, 0.00, 1.00, 0.00, 4.05, 0.00, 0.00, 2.36, -0.26, 1.00, 0.00, -0.84;
13.69, 0.00, 0.41, 0.00, 0.00, -36.64, -1.00, 1.98, 0.00, 0.00, 0.34, -1.61, 0.00, 0.00, 1.00, -1.98, 0.00, 0.00, -0.34, 1.61, 0.00, 0.00, 0.29;
-0.05, 0.00, -0.44, 1.00, 0.00, 0.23, 0.00, -0.04, 0.00, 0.00, 0.00, 0.04, 0.00, 0.00, 0.00, 0.04, 0.00, 0.00, -0.00, -0.04, 0.00, 0.00, 0.00;
-28.85, 0.00, 15.49, 0.00, 0.00, 2.82, 0.00, 2.55, -1.00, 0.00, -0.84, -0.95, 0.00, 0.00, 0.00, -2.55, 1.00, 0.00, 0.84, 0.95, 0.00, 0.00, 0.25;
40.09, 0.00, -28.06, 0.00, 0.00, 5.00, 0.00, -1.12, 0.00, -1.00, 0.95, 0.92, 0.00, 0.00, 0.00, 1.12, 0.00, 1.00, -0.95, -0.92, 0.00, 0.00, 0.25;
0.32, 0.00, 0.74, 0.00, 1.00, 0.58, -0.00, 0.02, -0.00, -0.00, -0.02, -0.01, 0.00, -0.00, 0.00, -0.02, 0.00, 0.00, 0.02, 0.01, 0.00, 0.00, 0.01;
-30.02, 0.00, -21.18, 0.00, 0.00, -17.88, 0.00, -2.15, 0.00, 0.00, -0.10, 1.01, 1.00, 0.00, 0.00, 2.15, 0.00, 0.00, 0.10, -1.01, -1.00, 0.00, 0.24;
0.53, 1.00, 0.42, 0.00, 0.00, 0.03, 0.00, -0.01, 0.00, 0.00, 0.01, -0.00, 0.00, 0.00, 0.00, 0.01, 0.00, 0.00, -0.01, 0.00, 0.00, 0.00, 0.00;
7.98, 0.00, -45.44, 0.00, 0.00, -30.58, 0.00, -0.35, 0.00, 0.00, 0.90, 0.39, 0.00, -1.00, 0.00, 0.35, 0.00, 0.00, -0.90, -0.39, 0.00, 1.00, 0.06
))\
$
$
"разрешающая строка" = "№"5\
"разрешающий столбец" = "№"1\
"разрешающий элемент" = 40.08978861384429\
$
$
script(mat(
0.00, 0.00, 34.56, 0.00, 0.00, 63.51, 1.00, -3.97, 1.00, 0.18, -0.58, 2.01, 0.00, 1.00, 0.00, 4.97, 0.00, 0.82, 1.58, -1.01, 1.00, 0.00, -0.64;
0.00, 0.00, 9.99, 0.00, 0.00, -38.35, -1.00, 2.36, 0.00, 0.34, 0.02, -1.93, 0.00, 0.00, 1.00, -2.36, 0.00, -0.34, -0.02, 1.93, 0.00, 0.00, 0.21;
0.00, 0.00, -0.47, 1.00, 0.00, 0.23, 0.00, -0.04, 0.00, -0.00, 0.00, 0.04, 0.00, 0.00, 0.00, 0.04, 0.00, 0.00, -0.00, -0.04, 0.00, 0.00, 0.00;
0.00, 0.00, -4.70, 0.00, 0.00, 6.41, 0.00, 1.74, -1.00, -0.72, -0.16, -0.29, 0.00, 0.00, 0.00, -1.74, 1.00, 0.72, 0.16, 0.29, 0.00, 0.00, 0.42;
1.00, 0.00, -0.70, 0.00, 0.00, 0.12, 0.00, -0.03, 0.00, -0.02, 0.02, 0.02, 0.00, 0.00, 0.00, 0.03, 0.00, 0.02, -0.02, -0.02, 0.00, 0.00, 0.01;
0.00, 0.00, 0.96, 0.00, 1.00, 0.54, -0.00, 0.03, -0.00, 0.01, -0.02, -0.02, 0.00, -0.00, 0.00, -0.03, 0.00, -0.01, 0.02, 0.02, 0.00, 0.00, 0.01;
0.00, 0.00, -42.20, 0.00, 0.00, -14.14, 0.00, -2.99, 0.00, -0.75, 0.61, 1.70, 1.00, 0.00, 0.00, 2.99, 0.00, 0.75, -0.61, -1.70, -1.00, 0.00, 0.43;
0.00, 1.00, 0.79, 0.00, 0.00, -0.04, 0.00, 0.00, 0.00, 0.01, -0.00, -0.01, 0.00, 0.00, 0.00, -0.00, 0.00, -0.01, 0.00, 0.01, 0.00, 0.00, 0.00;
0.00, 0.00, -39.85, 0.00, 0.00, -31.57, 0.00, -0.13, 0.00, 0.20, 0.71, 0.21, 0.00, -1.00, 0.00, 0.13, 0.00, -0.20, -0.71, -0.21, 0.00, 1.00, 0.01
))\
$
$
"разрешающая строка" = "№"2\
"разрешающий столбец" = "№"8\
"разрешающий элемент" = 2.3580598067948677\
$
$
script(mat(
0.00, 0.00, 51.39, 0.00, 0.00, -1.08, -0.68, 0.00, 1.00, 0.75, -0.55, -1.23, 0.00, 1.00, 1.68, 1.00, 0.00, 0.25, 1.55, 2.23, 1.00, 0.00, -0.29;
0.00, 0.00, 4.24, 0.00, 0.00, -16.26, -0.42, 1.00, 0.00, 0.14, 0.01, -0.82, 0.00, 0.00, 0.42, -1.00, 0.00, -0.14, -0.01, 0.82, 0.00, 0.00, 0.09;
0.00, 0.00, -0.29, 1.00, 0.00, -0.47, -0.02, 0.00, 0.00, 0.01, 0.00, 0.00, 0.00, 0.00, 0.02, 0.00, 0.00, -0.01, -0.00, -0.00, 0.00, 0.00, 0.01;
0.00, 0.00, -12.09, 0.00, 0.00, 34.77, 0.74, 0.00, -1.00, -0.97, -0.17, 1.13, 0.00, 0.00, -0.74, 0.00, 1.00, 0.97, 0.17, -1.13, 0.00, 0.00, 0.27;
1.00, 0.00, -0.58, 0.00, 0.00, -0.33, -0.01, 0.00, 0.00, -0.02, 0.02, 0.00, 0.00, 0.00, 0.01, -0.00, 0.00, 0.02, -0.02, -0.00, 0.00, 0.00, 0.01;
0.00, 0.00, 0.83, 0.00, 1.00, 1.03, 0.01, 0.00, -0.00, 0.00, -0.02, 0.00, 0.00, -0.00, -0.01, -0.00, 0.00, -0.00, 0.02, -0.00, 0.00, 0.00, 0.00;
0.00, 0.00, -29.52, 0.00, 0.00, -62.76, -1.27, 0.00, 0.00, -0.32, 0.63, -0.74, 1.00, 0.00, 1.27, 0.00, 0.00, 0.32, -0.63, 0.74, -1.00, 0.00, 0.69;
0.00, 1.00, 0.79, 0.00, 0.00, -0.03, 0.00, 0.00, 0.00, 0.01, -0.00, -0.01, 0.00, 0.00, -0.00, 0.00, 0.00, -0.01, 0.00, 0.01, 0.00, 0.00, 0.00;
0.00, 0.00, -39.30, 0.00, 0.00, -33.69, -0.06, 0.00, 0.00, 0.22, 0.72, 0.10, 0.00, -1.00, 0.06, 0.00, 0.00, -0.22, -0.72, -0.10, 0.00, 1.00, 0.02
))\
$
$
"разрешающая строка" = "№"9\
"разрешающий столбец" = "№"12\
"разрешающий элемент" = 0.10158876203453247\
$
$
script(mat(
0.00, 0.00, -425.82, 0.00, 0.00, -410.10, -1.35, 0.00, 1.00, 3.40, 8.14, 0.00, 0.00, -11.14, 2.35, 1.00, 0.00, -2.40, -7.14, 1.00, 1.00, 12.14, -0.05;
0.00, 0.00, -311.93, 0.00, 0.00, -287.25, -0.87, 1.00, 0.00, 1.90, 5.77, 0.00, 0.00, -8.04, 0.87, -1.00, 0.00, -1.90, -5.77, 0.00, 0.00, 8.04, 0.25;
0.00, 0.00, 0.39, 1.00, 0.00, 0.12, -0.02, 0.00, 0.00, 0.00, -0.01, 0.00, 0.00, 0.02, 0.02, 0.00, 0.00, -0.00, 0.01, 0.00, 0.00, -0.02, 0.01;
0.00, 0.00, 425.82, 0.00, 0.00, 410.10, 1.35, 0.00, -1.00, -3.40, -8.14, 0.00, 0.00, 11.14, -1.35, 0.00, 1.00, 3.40, 8.14, 0.00, 0.00, -11.14, 0.05;
1.00, 0.00, -0.52, 0.00, 0.00, -0.27, -0.01, 0.00, 0.00, -0.02, 0.02, 0.00, 0.00, 0.00, 0.01, -0.00, 0.00, 0.02, -0.02, 0.00, 0.00, -0.00, 0.01;
0.00, 0.00, 2.36, 0.00, 1.00, 2.34, 0.01, 0.00, -0.00, -0.00, -0.05, 0.00, 0.00, 0.04, -0.01, -0.00, 0.00, 0.00, 0.05, 0.00, 0.00, -0.04, 0.00;
0.00, 0.00, -316.00, 0.00, 0.00, -308.30, -1.67, 0.00, 0.00, 1.27, 5.85, 0.00, 1.00, -7.29, 1.67, 0.00, 0.00, -1.27, -5.85, -0.00, -1.00, 7.29, 0.84;
0.00, 1.00, -4.72, 0.00, 0.00, -4.76, -0.01, 0.00, 0.00, 0.04, 0.10, 0.00, 0.00, -0.14, 0.01, 0.00, 0.00, -0.04, -0.10, 0.00, 0.00, 0.14, 0.00;
0.00, 0.00, -386.87, 0.00, 0.00, -331.58, -0.54, 0.00, 0.00, 2.14, 7.05, 1.00, 0.00, -9.84, 0.54, 0.00, 0.00, -2.14, -7.05, -1.00, 0.00, 9.84, 0.19
))\
$
$
"разрешающая строка" = "№"4\
"разрешающий столбец" = "№"3\
"разрешающий элемент" = 425.8221274455778\
$
$
script(mat(
0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, -0.00, 0.00, -0.00, 0.00, 0.00, -0.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 0.00;
0.00, 0.00, 0.00, 0.00, 0.00, 13.16, 0.12, 1.00, -0.73, -0.59, -0.20, 0.00, 0.00, 0.12, -0.12, -1.00, 0.73, 0.59, 0.20, 0.00, 0.00, -0.12, 0.28;
0.00, 0.00, 0.00, 1.00, 0.00, -0.26, -0.02, 0.00, 0.00, 0.00, -0.00, 0.00, 0.00, 0.01, 0.02, -0.00, -0.00, -0.00, 0.00, 0.00, 0.00, -0.01, 0.01;
0.00, 0.00, 1.00, 0.00, 0.00, 0.96, 0.00, 0.00, -0.00, -0.01, -0.02, 0.00, 0.00, 0.03, -0.00, 0.00, 0.00, 0.01, 0.02, 0.00, 0.00, -0.03, 0.00;
1.00, 0.00, 0.00, 0.00, 0.00, 0.23, -0.01, 0.00, -0.00, -0.03, 0.01, 0.00, 0.00, 0.02, 0.01, -0.00, 0.00, 0.03, -0.01, 0.00, 0.00, -0.02, 0.01;
0.00, 0.00, 0.00, 0.00, 1.00, 0.07, 0.01, 0.00, 0.01, 0.01, -0.01, 0.00, 0.00, -0.02, -0.01, -0.00, -0.01, -0.01, 0.01, 0.00, 0.00, 0.02, 0.00;
0.00, 0.00, 0.00, 0.00, 0.00, -3.97, -0.67, 0.00, -0.74, -1.25, -0.19, 0.00, 1.00, 0.98, 0.67, 0.00, 0.74, 1.25, 0.19, -0.00, -1.00, -0.98, 0.87;
0.00, 1.00, 0.00, 0.00, 0.00, -0.21, 0.01, 0.00, -0.01, 0.01, 0.01, 0.00, 0.00, -0.02, -0.01, 0.00, 0.01, -0.01, -0.01, 0.00, 0.00, 0.02, 0.00;
0.00, 0.00, 0.00, 0.00, 0.00, 41.00, 0.69, 0.00, -0.91, -0.94, -0.35, 1.00, 0.00, 0.28, -0.69, 0.00, 0.91, 0.94, 0.35, -1.00, 0.00, -0.28, 0.24
))\
$
$
"разрешающая строка" = "№"8\
"разрешающий столбец" = "№"11\
"разрешающий элемент" = 9.504034650591187e-3\
$
$
script(mat(
0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, -0.00, 0.00, 0.00, 0.00, 0.00, -0.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 0.00;
0.00, 21.01, 0.00, 0.00, 0.00, 8.71, 0.28, 1.00, -0.97, -0.46, 0.00, 0.00, 0.00, -0.23, -0.28, -1.00, 0.97, 0.46, 0.00, 0.00, 0.00, 0.23, 0.38;
0.00, 0.23, 0.00, 1.00, 0.00, -0.31, -0.02, 0.00, -0.00, 0.01, 0.00, 0.00, 0.00, 0.00, 0.02, 0.00, 0.00, -0.01, 0.00, 0.00, 0.00, -0.00, 0.01;
0.00, 2.01, 1.00, 0.00, 0.00, 0.54, 0.02, 0.00, -0.02, 0.00, 0.00, 0.00, 0.00, -0.01, -0.02, 0.00, 0.02, -0.00, 0.00, 0.00, 0.00, 0.01, 0.01;
1.00, -1.35, 0.00, 0.00, 0.00, 0.51, -0.02, 0.00, 0.01, -0.03, 0.00, 0.00, 0.00, 0.04, 0.02, -0.00, -0.01, 0.03, 0.00, 0.00, 0.00, -0.04, 0.00;
0.00, 0.75, 0.00, 0.00, 1.00, -0.09, 0.01, 0.00, -0.00, 0.02, 0.00, 0.00, 0.00, -0.04, -0.01, -0.00, 0.00, -0.02, 0.00, 0.00, 0.00, 0.04, 0.01;
0.00, 20.47, 0.00, 0.00, 0.00, -8.31, -0.51, 0.00, -0.97, -1.13, 0.00, 0.00, 1.00, 0.64, 0.51, 0.00, 0.97, 1.13, 0.00, -0.00, -1.00, -0.64, 0.97;
0.00, 105.22, 0.00, 0.00, 0.00, -22.32, 0.78, 0.00, -1.17, 0.64, 1.00, 0.00, 0.00, -1.76, -0.78, 0.00, 1.17, -0.64, -1.00, 0.00, 0.00, 1.76, 0.50;
0.00, 37.08, 0.00, 0.00, 0.00, 33.13, 0.96, 0.00, -1.32, -0.72, 0.00, 1.00, 0.00, -0.34, -0.96, 0.00, 1.32, 0.72, 0.00, -1.00, 0.00, 0.34, 0.42
))\
$
$
"разрешающая строка" = "№"5\
"разрешающий столбец" = "№"14\
"разрешающий элемент" = 3.7881709862985787e-2\
$
$
script(mat(
0.00, 0.00, 0.00, 0.00, 0.00, 0.00, -0.00, 0.00, -0.00, -0.00, 0.00, 0.00, 0.00, 0.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 0.00;
6.20, 12.62, 0.00, 0.00, 0.00, 11.88, 0.16, 1.00, -0.88, -0.67, 0.00, 0.00, 0.00, 0.00, -0.16, -1.00, 0.88, 0.67, 0.00, 0.00, 0.00, 0.00, 0.40;
-0.09, 0.35, 0.00, 1.00, 0.00, -0.36, -0.02, 0.00, -0.00, 0.01, 0.00, 0.00, 0.00, 0.00, 0.02, 0.00, 0.00, -0.01, 0.00, 0.00, 0.00, 0.00, 0.01;
0.20, 1.74, 1.00, 0.00, 0.00, 0.64, 0.01, 0.00, -0.02, -0.00, 0.00, 0.00, 0.00, 0.00, -0.01, 0.00, 0.02, 0.00, 0.00, 0.00, 0.00, 0.00, 0.01;
26.40, -35.74, 0.00, 0.00, 0.00, 13.53, -0.53, 0.00, 0.36, -0.89, 0.00, 0.00, 0.00, 1.00, 0.53, -0.00, -0.36, 0.89, 0.00, 0.00, 0.00, -1.00, 0.06;
0.93, -0.51, 0.00, 0.00, 1.00, 0.39, -0.01, 0.00, 0.01, -0.01, 0.00, 0.00, 0.00, 0.00, 0.01, -0.00, -0.01, 0.01, 0.00, 0.00, 0.00, 0.00, 0.01;
-16.80, 43.21, 0.00, 0.00, 0.00, -16.92, -0.18, 0.00, -1.20, -0.56, 0.00, 0.00, 1.00, 0.00, 0.18, 0.00, 1.20, 0.56, 0.00, -0.00, -1.00, 0.00, 0.94;
46.57, 42.17, 0.00, 0.00, 0.00, 1.55, -0.16, 0.00, -0.52, -0.92, 1.00, 0.00, 0.00, 0.00, 0.16, -0.00, 0.52, 0.92, -1.00, 0.00, 0.00, 0.00, 0.60;
9.03, 24.85, 0.00, 0.00, 0.00, 37.76, 0.78, 0.00, -1.19, -1.02, 0.00, 1.00, 0.00, 0.00, -0.78, 0.00, 1.19, 1.02, 0.00, -1.00, 0.00, 0.00, 0.44
))\
$
$
"разрешающая строка" = "№"3\
"разрешающий столбец" = "№"10\
"разрешающий элемент" = 8.757095192450067e-3\
$
$
script(mat(
0.00, 0.00, 0.00, 0.00, 0.00, -0.00, -0.00, 0.00, -0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 0.00;
-0.47, 39.24, 0.00, 76.84, 0.00, -15.51, -1.01, 1.00, -1.10, 0.00, 0.00, 0.00, 0.00, 0.00, 1.01, -1.00, 1.10, 0.00, 0.00, 0.00, 0.00, 0.00, 1.11;
-9.91, 39.57, 0.00, 114.19, 0.00, -40.71, -1.73, 0.00, -0.32, 1.00, 0.00, 0.00, 0.00, 0.00, 1.73, 0.00, 0.32, -1.00, 0.00, 0.00, 0.00, 0.00, 1.05;
0.18, 1.84, 1.00, 0.27, 0.00, 0.54, 0.01, 0.00, -0.02, 0.00, 0.00, 0.00, 0.00, 0.00, -0.01, 0.00, 0.02, 0.00, 0.00, 0.00, 0.00, 0.00, 0.01;
17.58, -0.55, 0.00, 101.55, 0.00, -22.68, -2.07, 0.00, 0.08, 0.00, 0.00, 0.00, 0.00, 1.00, 2.07, -0.00, -0.08, 0.00, 0.00, 0.00, 0.00, -1.00, 0.99;
0.80, 0.00, 0.00, 1.48, 1.00, -0.14, -0.03, 0.00, 0.01, 0.00, 0.00, 0.00, 0.00, 0.00, 0.03, -0.00, -0.01, 0.00, 0.00, 0.00, 0.00, 0.00, 0.02;
-22.34, 65.35, 0.00, 63.88, 0.00, -39.69, -1.14, 0.00, -1.38, 0.00, 0.00, 0.00, 1.00, 0.00, 1.14, 0.00, 1.38, 0.00, 0.00, -0.00, -1.00, 0.00, 1.52;
37.40, 78.75, 0.00, 105.56, 0.00, -36.09, -1.76, 0.00, -0.82, 0.00, 1.00, 0.00, 0.00, 0.00, 1.76, 0.00, 0.82, -0.00, -1.00, 0.00, 0.00, 0.00, 1.58;
-1.09, 65.26, 0.00, 116.62, 0.00, -3.81, -0.99, 0.00, -1.52, 0.00, 0.00, 1.00, 0.00, 0.00, 0.99, 0.00, 1.52, 0.00, 0.00, -1.00, 0.00, 0.00, 1.51
))\
$
$
"разрешающая строка" = "№"4\
"разрешающий столбец" = "№"6\
"разрешающий элемент" = 0.5411911390555306\
$
$
script(mat(
0.00, 0.00, 0.00, 0.00, 0.00, 0.00, -0.00, 0.00, -0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 0.00;
4.58, 91.89, 28.67, 84.68, 0.00, 0.00, -0.72, 1.00, -1.75, 0.00, 0.00, 0.00, 0.00, 0.00, 0.72, -1.00, 1.75, 0.00, 0.00, 0.00, 0.00, 0.00, 1.47;
3.35, 177.72, 75.23, 134.76, 0.00, 0.00, -0.98, 0.00, -2.03, 1.00, 0.00, 0.00, 0.00, 0.00, 0.98, 0.00, 2.03, -1.00, 0.00, 0.00, 0.00, 0.00, 2.01;
0.33, 3.39, 1.85, 0.51, 0.00, 1.00, 0.02, 0.00, -0.04, 0.00, 0.00, 0.00, 0.00, 0.00, -0.02, 0.00, 0.04, 0.00, 0.00, 0.00, 0.00, 0.00, 0.02;
24.97, 76.40, 41.90, 113.01, 0.00, 0.00, -1.65, 0.00, -0.87, 0.00, 0.00, 0.00, 0.00, 1.00, 1.65, 0.00, 0.87, 0.00, 0.00, 0.00, 0.00, -1.00, 1.53;
0.85, 0.48, 0.26, 1.56, 1.00, 0.00, -0.03, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.03, -0.00, -0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.03;
-9.41, 200.04, 73.34, 83.93, 0.00, 0.00, -0.41, 0.00, -3.04, 0.00, 0.00, 0.00, 1.00, 0.00, 0.41, 0.00, 3.04, 0.00, 0.00, -0.00, -1.00, 0.00, 2.46;
49.16, 201.21, 66.68, 123.79, 0.00, 0.00, -1.09, 0.00, -2.33, 0.00, 1.00, 0.00, 0.00, 0.00, 1.09, 0.00, 2.33, -0.00, -1.00, 0.00, 0.00, 0.00, 2.42;
0.15, 78.19, 7.04, 118.54, 0.00, 0.00, -0.92, 0.00, -1.68, 0.00, 0.00, 1.00, 0.00, 0.00, 0.92, 0.00, 1.68, 0.00, 0.00, -1.00, 0.00, 0.00, 1.60
))\
$
$
"разрешающая строка" = "№"4\
"разрешающий столбец" = "№"7\
"разрешающий элемент" = 1.839587384277209e-2\
$
$
script(mat(
0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, -0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 0.00;
17.40, 225.52, 101.43, 104.57, 0.00, 39.38, 0.00, 1.00, -3.40, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, -1.00, 3.40, 0.00, 0.00, 0.00, 0.00, 0.00, 2.40;
20.74, 358.95, 173.91, 161.74, 0.00, 53.40, 0.00, 0.00, -4.26, 1.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 4.26, -1.00, 0.00, 0.00, 0.00, 0.00, 3.26;
17.70, 184.47, 100.45, 27.46, 0.00, 54.36, 1.00, 0.00, -2.28, 0.00, 0.00, 0.00, 0.00, 0.00, -1.00, 0.00, 2.28, 0.00, 0.00, 0.00, 0.00, 0.00, 1.28;
54.25, 381.51, 208.04, 158.43, 0.00, 89.91, 0.00, 0.00, -4.64, 0.00, 0.00, 0.00, 0.00, 1.00, 0.00, 0.00, 4.64, 0.00, 0.00, 0.00, 0.00, -1.00, 3.64;
1.31, 5.22, 2.84, 2.26, 1.00, 1.40, 0.00, 0.00, -0.06, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.06, 0.00, 0.00, 0.00, 0.00, 0.00, 0.06;
-2.10, 276.29, 114.86, 95.28, 0.00, 22.47, 0.00, 0.00, -3.98, 0.00, 0.00, 0.00, 1.00, 0.00, 0.00, 0.00, 3.98, 0.00, 0.00, -0.00, -1.00, 0.00, 2.98;
68.50, 402.78, 176.44, 153.80, 0.00, 59.40, 0.00, 0.00, -4.82, 0.00, 1.00, 0.00, 0.00, 0.00, 0.00, 0.00, 4.82, -0.00, -1.00, 0.00, 0.00, 0.00, 3.82;
16.39, 247.41, 99.19, 143.73, 0.00, 49.87, 0.00, 0.00, -3.77, 0.00, 0.00, 1.00, 0.00, 0.00, 0.00, 0.00, 3.77, 0.00, 0.00, -1.00, 0.00, 0.00, 2.77
))
$

$ underline("Оптимальное решение") \ vec( 0.00, 0.00, 0.00, 0.00, 0.06, 0.00 ) $

В первой строке не осталось отрицательных элементов (не считая значение целевой функции) и $u=0$, значит найдено оптимальное решение для вспомогательной задачи.

== Решение двойственной задачи

Для нахождения решения двойственной задачи продолжим с найденной угловой точки. Исключим из таблицы столбцы, соответствующие элементам $u$ и заменим первую строку на $(e,0)$.

$ mat(
1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00;
17.40, 225.52, 101.43, 104.57, 0.00, 39.38, 0.00, 1.00, -3.40, 0.00, 0.00, 0.00, 0.00, 0.00, 2.40;
20.74, 358.95, 173.91, 161.74, 0.00, 53.40, 0.00, 0.00, -4.26, 1.00, 0.00, 0.00, 0.00, 0.00, 3.26;
17.70, 184.47, 100.45, 27.46, 0.00, 54.36, 1.00, 0.00, -2.28, 0.00, 0.00, 0.00, 0.00, 0.00, 1.28;
54.25, 381.51, 208.04, 158.43, 0.00, 89.91, 0.00, 0.00, -4.64, 0.00, 0.00, 0.00, 0.00, 1.00, 3.64;
1.31, 5.22, 2.84, 2.26, 1.00, 1.40, 0.00, 0.00, -0.06, 0.00, 0.00, 0.00, 0.00, 0.00, 0.06;
-2.10, 276.29, 114.86, 95.28, 0.00, 22.47, 0.00, 0.00, -3.98, 0.00, 0.00, 0.00, 1.00, 0.00, 2.98;
68.50, 402.78, 176.44, 153.80, 0.00, 59.40, 0.00, 0.00, -4.82, 0.00, 1.00, 0.00, 0.00, 0.00, 3.82;
16.39, 247.41, 99.19, 143.73, 0.00, 49.87, 0.00, 0.00, -3.77, 0.00, 0.00, 1.00, 0.00, 0.00, 2.77
) $

Выделяем базисные столбцы с помощью элементарных преобразований строк матрицы. К первой строке добавляем 6 строку, умноженая на -1.

$
mat(
-0.31, -4.22, -1.84, -1.26, 0.00, -0.40, 0.00, 0.00, 0.06, 0.00, 0.00, 0.00, 0.00, 0.00, -0.06;
17.40, 225.52, 101.43, 104.57, 0.00, 39.38, 0.00, 1.00, -3.40, 0.00, 0.00, 0.00, 0.00, 0.00, 2.40;
20.74, 358.95, 173.91, 161.74, 0.00, 53.40, 0.00, 0.00, -4.26, 1.00, 0.00, 0.00, 0.00, 0.00, 3.26;
17.70, 184.47, 100.45, 27.46, 0.00, 54.36, 1.00, 0.00, -2.28, 0.00, 0.00, 0.00, 0.00, 0.00, 1.28;
54.25, 381.51, 208.04, 158.43, 0.00, 89.91, 0.00, 0.00, -4.64, 0.00, 0.00, 0.00, 0.00, 1.00, 3.64;
1.31, 5.22, 2.84, 2.26, 1.00, 1.40, 0.00, 0.00, -0.06, 0.00, 0.00, 0.00, 0.00, 0.00, 0.06;
-2.10, 276.29, 114.86, 95.28, 0.00, 22.47, 0.00, 0.00, -3.98, 0.00, 0.00, 0.00, 1.00, 0.00, 2.98;
68.50, 402.78, 176.44, 153.80, 0.00, 59.40, 0.00, 0.00, -4.82, 0.00, 1.00, 0.00, 0.00, 0.00, 3.82;
16.39, 247.41, 99.19, 143.73, 0.00, 49.87, 0.00, 0.00, -3.77, 0.00, 0.00, 1.00, 0.00, 0.00, 2.77
)
$
$
"разрешающая строка" = "№"4\
"разрешающий столбец" = "№"2\
"разрешающий элемент" = 184.4687252986854\
$
$
mat(
0.10, 0.00, 0.46, -0.63, 0.00, 0.85, 0.02, 0.00, 0.01, 0.00, 0.00, 0.00, 0.00, 0.00, -0.03;
-4.24, 0.00, -21.37, 71.00, 0.00, -27.08, -1.22, 1.00, -0.61, 0.00, 0.00, 0.00, 0.00, 0.00, 0.83;
-13.71, 0.00, -21.55, 108.30, 0.00, -52.37, -1.95, 0.00, 0.17, 1.00, 0.00, 0.00, 0.00, 0.00, 0.78;
0.10, 1.00, 0.54, 0.15, 0.00, 0.29, 0.01, 0.00, -0.01, 0.00, 0.00, 0.00, 0.00, 0.00, 0.01;
17.64, 0.00, 0.30, 101.63, 0.00, -22.51, -2.07, 0.00, 0.07, 0.00, 0.00, 0.00, 0.00, 1.00, 1.00;
0.80, 0.00, -0.00, 1.48, 1.00, -0.14, -0.03, 0.00, 0.01, 0.00, 0.00, 0.00, 0.00, 0.00, 0.02;
-28.61, 0.00, -35.58, 54.15, 0.00, -58.95, -1.50, 0.00, -0.57, 0.00, 0.00, 0.00, 1.00, 0.00, 1.07;
29.85, 0.00, -42.88, 93.84, 0.00, -59.29, -2.18, 0.00, 0.15, 0.00, 1.00, 0.00, 0.00, 0.00, 1.03;
-7.35, 0.00, -35.53, 106.90, 0.00, -23.04, -1.34, 0.00, -0.72, 0.00, 0.00, 1.00, 0.00, 0.00, 1.06
)
$
$
"разрешающая строка" = "№"3\
"разрешающий столбец" = "№"4\
"разрешающий элемент" = 108.30271580704607\
$
$
mat(
0.02, 0.00, 0.33, 0.00, 0.00, 0.54, 0.01, 0.00, 0.01, 0.01, 0.00, 0.00, 0.00, 0.00, -0.02;
4.75, 0.00, -7.24, 0.00, 0.00, 7.25, 0.05, 1.00, -0.72, -0.66, 0.00, 0.00, 0.00, 0.00, 0.32;
-0.13, 0.00, -0.20, 1.00, 0.00, -0.48, -0.02, 0.00, 0.00, 0.01, 0.00, 0.00, 0.00, 0.00, 0.01;
0.11, 1.00, 0.57, 0.00, 0.00, 0.37, 0.01, 0.00, -0.01, -0.00, 0.00, 0.00, 0.00, 0.00, 0.01;
30.50, 0.00, 20.52, 0.00, 0.00, 26.63, -0.24, 0.00, -0.09, -0.94, 0.00, 0.00, 0.00, 1.00, 0.27;
0.99, 0.00, 0.30, 0.00, 1.00, 0.58, -0.00, 0.00, 0.00, -0.01, 0.00, 0.00, 0.00, 0.00, 0.01;
-21.76, 0.00, -24.81, 0.00, 0.00, -32.76, -0.52, 0.00, -0.66, -0.50, 0.00, 0.00, 1.00, 0.00, 0.68;
41.73, 0.00, -24.21, 0.00, 0.00, -13.91, -0.50, 0.00, 0.01, -0.87, 1.00, 0.00, 0.00, 0.00, 0.36;
6.18, 0.00, -14.27, 0.00, 0.00, 28.65, 0.58, 0.00, -0.88, -0.99, 0.00, 1.00, 0.00, 0.00, 0.29
)
$

$ underline("Результат") $

#grid(
  columns: (1fr, 1fr, 1fr),
  [ $ x = vec( 0.00, 0.01, 0.00, 0.01, 0.01, 0.00 ) $ ],
  [ #v(0.04fr) \ $ alpha = 2.4747741869746438e-2 $ \ #v(1fr) ],
  [ $ p^* = x/(||x||) = vec( 0.00, 0.39, 0.00, 0.48, 0.78, 0.00 ) $ ]
)

= Ответ

Нижняя цена игры: $underline(A) = -21.254878614230552
$\
Верхняя цена игры: $overline(A) = 16.937688915945937$\
Оптимальная стратегия первого игрока: $p^* = vec( 0.00, 0.39, 0.00, 0.48, 0.78, 0.00 )$\
Оптимальная стратегия игрока: $q^* = vec( 0.77, 0.00, 0.50, 0.39, 0.00, 0.00, 0.00, 0.00 )$\
Цена игры: -3.1672615144038048

= Приложение (Я.П.: Haskell)

Весь исходный код этого приложения можно найти по адресу https://github.com/AJMC2002/opt-methods/tree/main.

== Библиотека

```haskell
-- lib/MatrixGame.hs
module MatrixGame where

import Data.Massiv.Array as A
import Prelude as P

lowStrategy :: Matrix P Double -> Double
lowStrategy a = maximum $ P.map (minimum' . (a !>)) [0 .. (i - 1)] where Sz2 i _ = size a

highStrategy :: Matrix P Double -> Double
highStrategy a = minimum $ P.map (maximum' . (a <!)) [0 .. (j - 1)] where Sz2 _ j = size a
```

== Бинарный

```haskell
-- exe/Main.hs
module Main where

import Data.Massiv.Array as A
import Formst
import MatrixGame (highStrategy, lowStrategy)
import Optimization.DirectTask (solveDirect)
import Optimization.DualTask (solveDual)
import Prelude as P
import System.Random qualified as R

main :: IO ()
main = do
    let 
        -- Initial values
        salt = 19092002
        gen = R.mkStdGen salt
        rng = (-50 :: Double, 50)
        comp = ParN 0
        a = computeP $ uniformRangeArray gen rng comp (Sz2 6 8) :: Matrix P Double
        b = A.replicate (ParN 0) 6 1
        c = A.replicate (ParN 0) 8 1
        beta = minimum' a
        a' = a .+ abs beta

    putStrLn $ showMat a 2
    putStrLn $ "underline(A) = " ++ show (lowStrategy a)
    putStrLn $ "overline(A) = " ++ show (highStrategy a)
    putStrLn $ "beta = " ++ show beta
    putStrLn $ showMat a' 2

    (y, alfa1) <- solveDirect a' b c
    putStrLn $ "y = " ++ showVec y 2
    putStrLn $ "alfa = " ++ show alfa1
    putStrLn $ "q^* = y/||y|| = " ++ showVec (y ./ normL2 y) 2

    (x, alfa2) <- solveDual a' b c
    putStrLn $ "x = " ++ showVec x 2
    putStrLn $ "alfa = " ++ show alfa2
    putStrLn $ "p^* = x/||x|| = " ++ showVec (x ./ normL2 x) 2

    putStrLn $ "underline(A) = " ++ show (lowStrategy a)
    putStrLn $ "overline(A) = " ++ show (highStrategy a)
    putStrLn $ "phi = " ++ show (1 / alfa1 - abs beta)
```
