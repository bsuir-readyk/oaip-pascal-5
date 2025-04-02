# Lab 5

ЧИСЛЕННОЕ ИНТЕГРИРОВАНИЕ

При вычислении определенного интеграла функции f(x) на промежутке от a до b, где f(x) — непрерывная на данном отрезке функция, можно воспользоваться формулой Ньютона-Лейбница:

\[
\int_{a}^{b}f(x)dx = F(b) - F(a)
\]

где F(x) — первообразная интегрируемой функции. Однако на практике не всегда удается найти первообразную в аналитической форме, но значение определенного интеграла всегда можно довести до численного значения. Таким образом, одной из задач численного интегрирования понимают приближенное вычисление значений интеграла при условии непрерывности подынтегральной функции. В этом случае для вычисления определенного интеграла следует помнить геометрический смысл вычисления определенного интеграла: значение определенного интеграла равно площади фигуры, ограниченной графиком функции, осью OX и прямыми x=a и x=b. Для приближенного интегрирования существует много численных методов. Рассмотрим некоторые из них.

МЕТОД ЛЕВЫХ ПРЯМОУГОЛЬНИКОВ

Разобьем отрезок интегрирования на n равных частей с шагом \( h = \frac{b-a}{n} \). Для вычисления интеграла по формуле левых прямоугольников получаем следующую формулу:

\[
I = h \sum_{i=0}^{n-1}f(x_i), \quad x_i=a+i\cdot h, \quad i=0, 1, \ldots, n-1
\]

\[
\text{Рис. 1. Иллюстрация к методу левых прямоугольников}
\]

ВЫЧИСЛЕНИЕ ИНТЕГРАЛА С ЗАДАННОЙ ТОЧНОСТЬЮ

Вычисление интеграла с заданной точностью t предполагает, что для различного количества \( n \), например n=5, 10, 15, 20, вычисляются значения J, по одному из выше описанных численных методов, т.е. имеются ряд значений интеграла:

\[ I_0, I_1, I_2, \ldots, I_n, I_{n+1}, \ldots \]

Тогда в качестве результата расчета можно полагать \( I_k \), если после (k+1) расчётной итерации выполняется следующее условие:

\[ |I_k - I_{k-1}| \leq t \]

В качестве начального приближения интеграла, если \( x \in [a, b] \), можно взять значение

\[ I_0 = f(a)\cdot (b-a) \]

для метода левых интегралов, рассчитывающее по следующей формуле I, где r=1. Для других методов несложно получить аналогичные формулы прямоугольников для r = 1.

ЗАДАНИЕ

ПОДПРОГРАММЫ С ПАРАМЕТРАМИ ПРОЦЕДУРНОГО ТИПА

В соответствии с заданными преподавателем двумя методами численных вычислений и вариантом индивидуального задания разработать программу для вычисления интеграла.

Для вычисления подынтегральных функций и вычисления непосредственно интеграла использовать функции с процедурными параметрами. Интегралы вычислять с точностью: \(\varepsilon = 10^{-2}, 10^{-5}\). Результаты расчетов свести в таблицу вида:

\[
\begin{array}{|c|c|c|}
\hline
\text{1-ый метод} & \text{2-ой метод} \\
\hline
\varepsilon=10^{-2} & \varepsilon=10^{-5} & \varepsilon=10^{-2} & \varepsilon=10^{-5} \\
\text{значение} & N & \text{значение} & N \\
\hline
\text{1-ый интеграл} & & & \\
\text{2-ой интеграл} & & & \\
\text{3-ий интеграл} & & & \\
\text{4-ый интеграл} & & & \\
\hline
\end{array}
\]

ЧИСЛЕННОЕ ИНТЕГРИРОВАНИЕ

При вычислении определенного интеграла функции \( f(x) \) на промежутке от \( a \) до \( b \), где \( f(x) \) — непрерывная на данном отрезке функция, можно воспользоваться формулой Ньютона-Лейбница:

\[
\int_{a}^{b}f(x)dx = F(b) - F(a)
\]

где \( F(x) \) — первообразная интегрируемой функции. Однако на практике не всегда удается найти первообразную в аналитической форме, но значение определенного интеграла всегда можно довести до числового ответа. Таким образом, одной из задач численного интегрирования понимают: приближенное вычисление значений интеграла при условии, что известны значения подынтегральной функции. При вычислении определенного интеграла следует помнить геометрический смысл вычисления определенного интеграла: значение определенного интеграла равно площади фигуры, ограниченной графиком функции, осью \( OX \) и прямыми \( x=a \) и \( x=b \). Для приближенного интегрирования существует много численных методов. Рассмотрим некоторые из них.

МЕТОД ТРАПЕЦИЙ

Разобьем отрезок интегрирования на n равных частей с шагом \( h = \frac{b-a}{n} \). Для вычисления интеграла по формуле трапеций получаем следующую формулу:

\[
I = h\left(\frac{y_0 + y_1}{2} + y_2 + \ldots + \frac{y_{n-1} + y_n}{2}\right) = h \sum_{i=1}^{n} f(x_i), \quad x_i=a+i\cdot h, \quad i=0, n 
\]

Рис. 4. Иллюстрация к методу трапеций

ВЫЧИСЛЕНИЕ ИНТЕГРАЛА С ЗАДАННОЙ ТОЧНОСТЬЮ

Вычисление интеграла с заданной точностью ε предполагает, что для различного количества n — интервалов разбиения, например, n=5, 10, 15, 20, вычисляется площадь по одному из выше описанных численных методов, т.е. имеется ряд значений интеграла \( I_0, I_1, I_2, \ldots, I_n, I_{n+1}, \ldots \). Тогда в качестве результата расчета можно полагать \( I_{k+1} \), если после (k+1) расчетной итерации выполняется следующее условие:

\[ |I_k - I_{k-1}| \leq \varepsilon \]

В качестве начального приближения интеграла, т.е. \( I_0 \), можно взять значение интеграла, рассчитанное по следующей формуле:

\[ I_0 = f(a)\cdot (b-a) \]

для метода левых интегралов, рассчитанное по следующей формуле I, где n=1. Для других методов несложно получить аналогичные формулы прямоугольников для n=1.

\[
\int_{0.6}^{1.4} \frac{\sqrt{x^2+5}}{2x+\sqrt{x^2+0.5}} \, dx
\quad
\int_{0.2}^{0.8} \frac{\sin(2x+0.5)}{2+\cos(x^2+1)} \, dx
\]

\[
\int_{0.8}^{1.6} \frac{1}{\sqrt{2x^2+1}} \, dx
\quad
\int_{1.2}^{2} \frac{\ln(x+2)}{x} \, dx
\]



|              | 1ый метод | 1ый метод | 1ый метод | 1ый метод | 2ой метод | 2ой метод | 2ой метод | 2ой метод |
|--------------|-----------|-----------|-----------|-----------|-----------|-----------|-----------|-----------|
|              | eps=10^-2 | eps=10^-2 | eps=10^-3 | eps=10^-3 | eps=10^-2 | eps=10^-2 | eps=10^-3 | eps=10^-3 |
|              | значение  | N         | значение  | N         | значение  | N         | значение  | N         |
| 1ый интеграл |           |           |           |           |           |           |           |           |
| 2ой интеграл |           |           |           |           |           |           |           |           |
| 3ий интеграл |           |           |           |           |           |           |           |           |
| 4ый интеграл |           |           |           |           |           |           |           |           |