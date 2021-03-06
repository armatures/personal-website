---
title: Aggregating in Pandas
teaser: "The other day I caught myself writing one too many `df.groupby()` in
Pandas, and thought to myself:"
toc: true
---

# 1. Introduction

The other day I caught myself writing one too many `df.groupby()` in Pandas,
and thought to myself: Isn't there a way I can simplify and reduce the amount
of `df.groupby()` calls? What I group by hardly changes and only the operation
that follows a group by varies from IPython cell to IPython cell. Aggregates
shall come to the rescue.

Pandas DataFrames allow many useful calculations to be performed on them. One
of them is the so called `.aggregate()` method. Using `.aggregate()`, a user
can perform many calculations on a group by object or resampler at once. This
can be quite handy in many situations and performs much faster than calculating
all required aggregate values in separate steps. As a matter of fact, I was
quite surprised while writing this article when I found out how much
flexibility exactly a user can get while using `aggregate()`.

# 2. Setup

First things first and we do business as usual. We import `pandas` in order to
create the DataFrames we use in our examples.


```python
import pandas as pd
```

We want to define a DataFrame with a variety of illustrative data types.

The example that we come up with here is a list of fruits that our fictitious
friends _Franz_, _Hans_ and _Gerhard_ have eaten in the last week. Furthermore,
we note down whether our friends have actually liked the fruit or not.

So the columns are:

- `index`: Name of person
- `Fruit`: Fruit consumed
- `Satisfaction`: Satisfaction with consumed fruit
- `Weight`: Weight of the consumed fruit in grams


```python
df = pd.DataFrame(
    [
        ['Apple', 'full', 100],
        ['Orange', 'none', 200],
        ['Pear', 'full', 300],
        ['Pear', 'partial', 100],
        ['Banana', 'full', 400],
        ['Banana', 'full', 300],
    ],
    columns=[
        'Fruit',
        'Satisfaction',
        'Weight'
    ],
    index=[
        'Franz',
        'Gerhard',
        'Gerhard',
        'Hans',
        'Hans',
        'Hans',
    ],
)
df.Satisfaction = df.Satisfaction.astype('category')
df
```

__Output:__

<section class="table-wrapper">
 <table>
  <thead>
   <tr>
    <th>
    </th>
    <th>
     Fruit
    </th>
    <th>
     Satisfaction
    </th>
    <th>
     Weight
    </th>
   </tr>
  </thead>
  <tbody>
   <tr>
    <th>
     Franz
    </th>
    <td>
     Apple
    </td>
    <td>
     full
    </td>
    <td>
     100
    </td>
   </tr>
   <tr>
    <th>
     Gerhard
    </th>
    <td>
     Orange
    </td>
    <td>
     none
    </td>
    <td>
     200
    </td>
   </tr>
   <tr>
    <th>
     Gerhard
    </th>
    <td>
     Pear
    </td>
    <td>
     full
    </td>
    <td>
     300
    </td>
   </tr>
   <tr>
    <th>
     Hans
    </th>
    <td>
     Pear
    </td>
    <td>
     partial
    </td>
    <td>
     100
    </td>
   </tr>
   <tr>
    <th>
     Hans
    </th>
    <td>
     Banana
    </td>
    <td>
     full
    </td>
    <td>
     400
    </td>
   </tr>
   <tr>
    <th>
     Hans
    </th>
    <td>
     Banana
    </td>
    <td>
     full
    </td>
    <td>
     300
    </td>
   </tr>
  </tbody>
 </table>
</section>

Are you as excited as I am to learn the first few magic incantations of
`.aggregate()`? Let's move on and start working on the DataFrame.


# 3. Aggregate Heaven

## 3.1. `count`

Using the `count` aggregation, we can retrieve the amount of rows that are
counted in a group by expression. This is not particularly exciting, but will
make the following steps a little bit clearer.

First we turn towards our Pandas DataFrame and try to count the number of
fruits that each person has consumed.

For this, we have to group the DataFrame by its index as the index contains the
person names. In order to group by the DataFrame's index, we can simply group
by using `.groupby(level=0)`. We use `level=0` to indicate that we want to
group by the DataFrames index. We could use `.groupby(df.index)` instead, but
this way we can leave it implicit and save us a bit of typing. [See the docs](h
ttps://pandas.pydata.org/pandas-docs/stable/generated/pandas.DataFrame.groupby.
html) for some more information on how `groupby()` can be invoked.


```python
df.groupby(level=0)
```

__Output:__

```
<pandas.core.groupby.DataFrameGroupBy object at 0x10b88ada0>
```

Calling `.groupby()` by itself does not do much. We need to perform an
aggregation on it in order to get a meaningful result. Let's do the actual
calculation now and see what we get. We call `.aggregate('count')` on the
`DataFrameGroupBy` object.


```python
df.groupby(level=0).aggregate('count')
```

__Output:__

<section class="table-wrapper">
 <table>
  <thead>
   <tr>
    <th>
    </th>
    <th>
     Fruit
    </th>
    <th>
     Satisfaction
    </th>
    <th>
     Weight
    </th>
   </tr>
  </thead>
  <tbody>
   <tr>
    <th>
     Franz
    </th>
    <td>
     1
    </td>
    <td>
     1
    </td>
    <td>
     1
    </td>
   </tr>
   <tr>
    <th>
     Gerhard
    </th>
    <td>
     2
    </td>
    <td>
     2
    </td>
    <td>
     2
    </td>
   </tr>
   <tr>
    <th>
     Hans
    </th>
    <td>
     3
    </td>
    <td>
     3
    </td>
    <td>
     3
    </td>
   </tr>
  </tbody>
 </table>
</section>

Our informed reader will surely let us know, that `count()` can also be invoked
more directly and will give us the same result:


```python
df.groupby(level=0).count()
```

__Output:__

<section class="table-wrapper">
 <table>
  <thead>
   <tr>
    <th>
    </th>
    <th>
     Fruit
    </th>
    <th>
     Satisfaction
    </th>
    <th>
     Weight
    </th>
   </tr>
  </thead>
  <tbody>
   <tr>
    <th>
     Franz
    </th>
    <td>
     1
    </td>
    <td>
     1
    </td>
    <td>
     1
    </td>
   </tr>
   <tr>
    <th>
     Gerhard
    </th>
    <td>
     2
    </td>
    <td>
     2
    </td>
    <td>
     2
    </td>
   </tr>
   <tr>
    <th>
     Hans
    </th>
    <td>
     3
    </td>
    <td>
     3
    </td>
    <td>
     3
    </td>
   </tr>
  </tbody>
 </table>
</section>

That is absolutely true, and in simple situations just using `.count()` on a
group by object will be much easier to understand. On the other hand,
`aggregate()` allows us to do one nifty thing that wouldn't be possible easily
any other way. It lets us perform several calculations at once and neatly
formats the results using nested columns. As a matter of fact, Pandas gives us
a lot of freedom in how exactly we want the aggregates to look like. Let's get
to a slightly more complicated example to illustrate the true flexibility right
away.

## 3.2. Aggregator Functions

If we would like to find out what the most frequent value in a column is, we
need to use a custom aggregator. Pandas does not include a method for this out
of the box, so we can either define a function or a lambda to give us the
desired result.

In our case we would like to simply define a lambda to facilitate this
calculation:


```python
most_frequent = lambda s: s.value_counts().idxmax()
```

We can try calling `most_frequent` on the whole DataFrame and check the result.
We use `.apply()` in this case to apply a function to every column in a
DataFrame.


```python
df.apply(most_frequent).to_frame()
```

__Output:__

<section class="table-wrapper">
 <table>
  <thead>
   <tr>
    <th>
    </th>
    <th>
     0
    </th>
   </tr>
  </thead>
  <tbody>
   <tr>
    <th>
     Fruit
    </th>
    <td>
     Banana
    </td>
   </tr>
   <tr>
    <th>
     Satisfaction
    </th>
    <td>
     full
    </td>
   </tr>
   <tr>
    <th>
     Weight
    </th>
    <td>
     100
    </td>
   </tr>
  </tbody>
 </table>
</section>

Let's put everything together and calculate the aggregate.


```python
df.groupby(level=0).aggregate(lambda s: s.value_counts().idxmax())
```

__Output:__

<section class="table-wrapper">
 <table>
  <thead>
   <tr>
    <th>
    </th>
    <th>
     Fruit
    </th>
    <th>
     Satisfaction
    </th>
    <th>
     Weight
    </th>
   </tr>
  </thead>
  <tbody>
   <tr>
    <th>
     Franz
    </th>
    <td>
     Apple
    </td>
    <td>
     full
    </td>
    <td>
     100
    </td>
   </tr>
   <tr>
    <th>
     Gerhard
    </th>
    <td>
     Pear
    </td>
    <td>
     none
    </td>
    <td>
     300
    </td>
   </tr>
   <tr>
    <th>
     Hans
    </th>
    <td>
     Banana
    </td>
    <td>
     full
    </td>
    <td>
     400
    </td>
   </tr>
  </tbody>
 </table>
</section>

## 3.3. Combining Aggregates

Let us find out, what

- the first and last fruit that each person has eaten is,
- while also counting the total amount of fruits consumed, and
- what the most frequently given satisfaction rating for each person is.

In order to do this, we first have to define an aggregate dictionary. It
contains instructions on which calculations to perform on which column. It even
allows defining custom aggregators using Python functions or lambdas.

The aggregate dictionary contains two entries corresponding to the two columns
in the DataFrame. For the fruit column, we add 3 desired aggregates in the form
of a list:

```python
['first', 'last', 'count']
```

And for the satisfaction column, we add a named aggregator function by
specifying a list containing one tuple with the `most_frequent` lambda that we
have defined before:

```python
[('most_frequent', most_frequent)]
```

Attaching a name to the aggregator is useful in our case, since it will tell
Pandas what to name the result column after calculating the aggregates.
Otherwise, using a lambda will lead to the resulting column to also be called
`lambda`.

The full dictionary is defined as follows:


```python
aggregate = {
    'Fruit': [
        'first',
        'last',
        'count',
    ],
    'Satisfaction': [
        ('most_frequent', most_frequent),
    ]
}
```

Let's run the actual calculation then and see what the result looks like.


```python
df.groupby(level=0).aggregate(aggregate)
```

__Output:__

<section class="table-wrapper">
 <table>
  <thead>
   <thead>
    <tr>
     <th>
     </th>
     <th colspan="3" halign="left">
      Fruit
     </th>
     <th>
      Satisfaction
     </th>
    </tr>
    <tr>
     <th>
     </th>
     <th>
      first
     </th>
     <th>
      last
     </th>
     <th>
      count
     </th>
     <th>
      most_frequent
     </th>
    </tr>
   </thead>
  </thead>
  <tbody>
   <tr>
    <th>
     Franz
    </th>
    <td>
     Apple
    </td>
    <td>
     Apple
    </td>
    <td>
     1
    </td>
    <td>
     full
    </td>
   </tr>
   <tr>
    <th>
     Gerhard
    </th>
    <td>
     Orange
    </td>
    <td>
     Pear
    </td>
    <td>
     2
    </td>
    <td>
     none
    </td>
   </tr>
   <tr>
    <th>
     Hans
    </th>
    <td>
     Pear
    </td>
    <td>
     Banana
    </td>
    <td>
     3
    </td>
    <td>
     full
    </td>
   </tr>
  </tbody>
 </table>
</section>

We can see that the DataFrame's columns are formatted and calculated exactly as
we have defined in the dictionary used for our `.aggregate()` call.

# 4. Side Note: Data Types

## 4.1. Pandas dtypes

Note that when running aggregates, the result type of an aggregated column can
be different from the source column. To get back to our `count()` example,
observe the following data types for the source DataFrame:


```python
df.dtypes.to_frame()
```

__Output:__

<section class="table-wrapper">
 <table>
  <thead>
   <tr>
    <th>
    </th>
    <th>
     0
    </th>
   </tr>
  </thead>
  <tbody>
   <tr>
    <th>
     Fruit
    </th>
    <td>
     object
    </td>
   </tr>
   <tr>
    <th>
     Satisfaction
    </th>
    <td>
     category
    </td>
   </tr>
   <tr>
    <th>
     Weight
    </th>
    <td>
     int64
    </td>
   </tr>
  </tbody>
 </table>
</section>

- The `Fruit` column contains data of the type `object`, which is the way
Pandas stores Python `str` (string) data in columns,
- `Satisfaction` contains category data, as indicated previously, and
- `Weight` contains `int64` data.

Now, observe one more time what happens when the count aggregate is retrieved
on the same DataFrame.


```python
df.groupby(level=0).aggregate('count')
```

__Output:__

<section class="table-wrapper">
 <table>
  <thead>
   <tr>
    <th>
    </th>
    <th>
     Fruit
    </th>
    <th>
     Satisfaction
    </th>
    <th>
     Weight
    </th>
   </tr>
  </thead>
  <tbody>
   <tr>
    <th>
     Franz
    </th>
    <td>
     1
    </td>
    <td>
     1
    </td>
    <td>
     1
    </td>
   </tr>
   <tr>
    <th>
     Gerhard
    </th>
    <td>
     2
    </td>
    <td>
     2
    </td>
    <td>
     2
    </td>
   </tr>
   <tr>
    <th>
     Hans
    </th>
    <td>
     3
    </td>
    <td>
     3
    </td>
    <td>
     3
    </td>
   </tr>
  </tbody>
 </table>
</section>

We look at the `.dtypes` attribute of our aggregate.


```python
df.groupby(level=0).aggregate('count').dtypes.to_frame()
```

__Output:__

<section class="table-wrapper">
 <table>
  <thead>
   <tr>
    <th>
    </th>
    <th>
     0
    </th>
   </tr>
  </thead>
  <tbody>
   <tr>
    <th>
     Fruit
    </th>
    <td>
     int64
    </td>
   </tr>
   <tr>
    <th>
     Satisfaction
    </th>
    <td>
     int64
    </td>
   </tr>
   <tr>
    <th>
     Weight
    </th>
    <td>
     int64
    </td>
   </tr>
  </tbody>
 </table>
</section>

This reveals that `count` will be stored as `int64` unlike the original
columns, which had the data types `object` and `category`. Pandas returns a
different data type after performing aggregates, depending on what the result
of a calculation is.

In the case of `count` data, integers such as `int64` (a 64 bit signed integer)
are the sensible choice for storing them. NumPy `ndarray` is the internal
storage format used for almost all data in Pandas, except for indices. More
info on `ndarray` [can be found here](https://docs.scipy.org/doc/numpy-1.13.0/r
eference/generated/numpy.ndarray.html). The reason why Pandas uses `ndarray`
objects is that it is a space and time efficient way of storing fixed length
lists of numbers.

We can easily take a look at the underlying data type of a Pandas column by
accessing the `.values` attribute.


```python
type(df.Fruit.values)
```

__Output:__

```
numpy.ndarray
```

And simply evaluating `df.Fruit.values` reveals that it has the data type
`object`, as is indicated by the `dtype` value.


```python
df.Fruit.values
```

__Output:__

```
array(['Apple', 'Orange', 'Pear', 'Pear', 'Banana', 'Banana'], dtype=object)
```

`object` is used to store `str` (string) data in NumPy and Pandas. Unlike
fixed-width integers, such as `int64`, string data cannot be stored efficiently
inside a continuous NumPy `ndarray`. The `ndarray` instance in this case only
contains a collection of pointers to objects, quite similarly to how a regular
Python `list` works. The space efficiency characteristics of a `ndarray`
instance with data type set to `object` are slightly better than just using
`list`. We will prove this in the next subsection.

## 4.2. Memory Profiling

Our claim that NumPy ndarrays store strings more efficiently than Python lists
can be verified using a memory profiler. Luckily, a PyPi package called
`pympler` allows us to quickly measured memory usage by Python objects. We
furthermore import NumPy to directly create NumPy arrays without requiring
Pandas.


```python
from pympler import asizeof
import numpy as np
```

We decide to create an `ndarray` containing `10**6` strings "hello".


```python
numpy_hello = np.zeros(10 ** 6, dtype=object)
numpy_hello.fill('hello')
numpy_hello
```

__Output:__

```
array(['hello', 'hello', 'hello', ..., 'hello', 'hello', 'hello'], dtype=object)
```

We create an equivalent Python `list` that contains "hello" `10**6` times.


```python
python_hello = ["hello" for _ in range(10 ** 6)]
# Print the first 3 items
python_hello[:3]
```

__Output:__

```
['hello', 'hello', 'hello']
```

Now for the evaluation: We print the size of the NumPy `ndarray` in bytes using
Pympler's `asizeof` method.


```python
asizeof.asizeof(numpy_hello)
```

__Output:__

```
8000096
```

To compare, we print the size of the Python `list`.


```python
asizeof.asizeof(python_hello)
```

__Output:__

```
8697520
```

```python
print("NumPy array size in relation to Python list size: {:2.2%}".format(
    asizeof.asizeof(numpy_hello) /
    asizeof.asizeof(python_hello)
))
```

__Output:__

```
NumPy array size in relation to Python list size: 91.98%
```

The NumPy array only takes 91.98% of the space required by the Python list,
even though the data that they store is the same.

__About Pympler__

Note that Pympler prints accurate sizes by traversing the object and summing up
attribute sizes for all descendant attributes. This is unlike `sys.getsizeof`,
which does not perform a deep traversal of an object and its attributes,
especially for user defined classes.

We can easily see the difference:


```python
from sys import getsizeof

getsizeof(python_hello)
```

__Output:__

```
8697464
```

Now here the difference is minimal, but as soon as we nest objects even
further, the difference of `sys.getsizeof` and `asizeof` becomes quite obvious:


```python
getsizeof([[[]]])
```

__Output:__

```
72
```

```python
asizeof.asizeof([[[]]])
```

__Output:__

```
208
```

As a bonus, we compare the previous two objects `numpy_hello` and
`python_hello` to using a `tuple()` instead.


```python
tuple_hello = tuple("hello" for _ in range(10 ** 6))
# Print the first 3 items
tuple_hello[:3]
```

__Output:__

```
('hello', 'hello', 'hello')
```

`tuple_hello` is smaller than the list object `python_hello`, but still bigger
than `numpy_hello`:


```python
asizeof.asizeof(tuple_hello)
```

__Output:__

```
8000104
```

From this we can safely conclude that Numpy `ndarray` is the most efficient way
of storing fixed-size array data.

This concludes our short excursion on Python and NumPy memory usage.


# 5. More About Aggregates

## 5.1. `nunique`

In Pandas, `nunique` counts the number of unique values in a column. We can
apply this to the whole DataFrame and get a count of the unique fruit and
satisfaction values:


```python
df.nunique().to_frame()
```

__Output:__

<section class="table-wrapper">
 <table>
  <thead>
   <tr>
    <th>
    </th>
    <th>
     0
    </th>
   </tr>
  </thead>
  <tbody>
   <tr>
    <th>
     Fruit
    </th>
    <td>
     4
    </td>
   </tr>
   <tr>
    <th>
     Satisfaction
    </th>
    <td>
     3
    </td>
   </tr>
   <tr>
    <th>
     Weight
    </th>
    <td>
     4
    </td>
   </tr>
  </tbody>
 </table>
</section>

Furthermore, the method can also be applied on a group by object to retrieve
the unique number of values per group. We see below the number of unique fruits
and satisfactions that have been assigned to each person.


```python
df.groupby(level=0).nunique()
```

__Output:__

<section class="table-wrapper">
 <table>
  <thead>
   <tr>
    <th>
    </th>
    <th>
     Fruit
    </th>
    <th>
     Satisfaction
    </th>
    <th>
     Weight
    </th>
   </tr>
  </thead>
  <tbody>
   <tr>
    <th>
     Franz
    </th>
    <td>
     1
    </td>
    <td>
     1
    </td>
    <td>
     1
    </td>
   </tr>
   <tr>
    <th>
     Gerhard
    </th>
    <td>
     2
    </td>
    <td>
     2
    </td>
    <td>
     2
    </td>
   </tr>
   <tr>
    <th>
     Hans
    </th>
    <td>
     2
    </td>
    <td>
     2
    </td>
    <td>
     3
    </td>
   </tr>
  </tbody>
 </table>
</section>

Now, `nunique` is also available in aggregates. The reason why we would use
`nunique` in aggregates is if we want to retrieve multiple results for one
group by expression at the same time. This can not only save us some typing,
but can potentially also save us some computational time, as a group only needs
to be created once and each operation can then be applied to it one after
another.


```python
df.groupby(level=0).agg('nunique')
```

__Output:__

<section class="table-wrapper">
 <table>
  <thead>
   <tr>
    <th>
    </th>
    <th>
     Fruit
    </th>
    <th>
     Satisfaction
    </th>
    <th>
     Weight
    </th>
   </tr>
  </thead>
  <tbody>
   <tr>
    <th>
     Franz
    </th>
    <td>
     1
    </td>
    <td>
     1
    </td>
    <td>
     1
    </td>
   </tr>
   <tr>
    <th>
     Gerhard
    </th>
    <td>
     2
    </td>
    <td>
     2
    </td>
    <td>
     2
    </td>
   </tr>
   <tr>
    <th>
     Hans
    </th>
    <td>
     2
    </td>
    <td>
     2
    </td>
    <td>
     3
    </td>
   </tr>
  </tbody>
 </table>
</section>

## 5.2. Using Aggregate Results

Having run this, we now know that Franz has only consumed one type of fruit.
Hans is the champion of trying out many types of fruits. We can then use this
value to compare it to the total count of fruits consumed. This allows us to
calculate a variety score for each person. We define a variety of 100 % as a
fruit consumption pattern in which a new fruit is tried every time.


```python
fruit_counts = df.groupby(level=0).Fruit.aggregate(['nunique', 'count'])
fruit_counts
```

__Output:__

<section class="table-wrapper">
 <table>
  <thead>
   <tr>
    <th>
    </th>
    <th>
     nunique
    </th>
    <th>
     count
    </th>
   </tr>
  </thead>
  <tbody>
   <tr>
    <th>
     Franz
    </th>
    <td>
     1
    </td>
    <td>
     1
    </td>
   </tr>
   <tr>
    <th>
     Gerhard
    </th>
    <td>
     2
    </td>
    <td>
     2
    </td>
   </tr>
   <tr>
    <th>
     Hans
    </th>
    <td>
     2
    </td>
    <td>
     3
    </td>
   </tr>
  </tbody>
 </table>
</section>

We decide to neatly display the variety counts with a quick `.apply` call in
which we format the resulting floats using a Python format string.


```python
(
    fruit_counts['nunique'] / fruit_counts['count']
).apply(
    lambda v: "{:.2%}".format(v)
).to_frame('Variety')
```

__Output:__

<section class="table-wrapper">
 <table>
  <thead>
   <tr>
    <th>
    </th>
    <th>
     Variety
    </th>
   </tr>
  </thead>
  <tbody>
   <tr>
    <th>
     Franz
    </th>
    <td>
     100.00%
    </td>
   </tr>
   <tr>
    <th>
     Gerhard
    </th>
    <td>
     100.00%
    </td>
   </tr>
   <tr>
    <th>
     Hans
    </th>
    <td>
     66.67%
    </td>
   </tr>
  </tbody>
 </table>
</section>

## 5.3. All Built-In Aggregates

What follows now is a list of all the built-in aggregates that I could find in
Pandas.

These are the aggregates that work with __all__ data types. To save some space
we limit ourselves to the Fruit column.


```python
df.groupby(level=0).Fruit.aggregate([
     'count',
     'min',
     'max',
     'first',
     'last',
     'nunique',
]).applymap(
    lambda v: v if isinstance(v, str) else "{:d}".format(v)
)
```

__Output:__

<section class="table-wrapper">
 <table>
  <thead>
   <tr>
    <th>
    </th>
    <th>
     count
    </th>
    <th>
     min
    </th>
    <th>
     max
    </th>
    <th>
     first
    </th>
    <th>
     last
    </th>
    <th>
     nunique
    </th>
   </tr>
  </thead>
  <tbody>
   <tr>
    <th>
     Franz
    </th>
    <td>
     1
    </td>
    <td>
     Apple
    </td>
    <td>
     Apple
    </td>
    <td>
     Apple
    </td>
    <td>
     Apple
    </td>
    <td>
     1
    </td>
   </tr>
   <tr>
    <th>
     Gerhard
    </th>
    <td>
     2
    </td>
    <td>
     Orange
    </td>
    <td>
     Pear
    </td>
    <td>
     Orange
    </td>
    <td>
     Pear
    </td>
    <td>
     2
    </td>
   </tr>
   <tr>
    <th>
     Hans
    </th>
    <td>
     3
    </td>
    <td>
     Banana
    </td>
    <td>
     Pear
    </td>
    <td>
     Pear
    </td>
    <td>
     Banana
    </td>
    <td>
     2
    </td>
   </tr>
  </tbody>
 </table>
</section>

Here are the data types that work with __numerical__ data types.


```python
df.groupby(level=0).aggregate([
    'mean',
    'std',
    'var',
    'median',
    'prod',
    'sum',
    'mad',
    'sem',
    'skew',
    'quantile',  # 50 % quantile
]).applymap(
    lambda v: v if isinstance(v, str) else "{:.2f}".format(v)
)
```

__Output:__

<section class="table-wrapper">
 <table>
  <thead>
   <thead>
    <tr>
     <th>
     </th>
     <th colspan="10" halign="left">
      Weight
     </th>
    </tr>
    <tr>
     <th>
     </th>
     <th>
      mean
     </th>
     <th>
      std
     </th>
     <th>
      var
     </th>
     <th>
      median
     </th>
     <th>
      prod
     </th>
     <th>
      sum
     </th>
     <th>
      mad
     </th>
     <th>
      sem
     </th>
     <th>
      skew
     </th>
     <th>
      quantile
     </th>
    </tr>
   </thead>
  </thead>
  <tbody>
   <tr>
    <th>
     Franz
    </th>
    <td>
     100.00
    </td>
    <td>
     nan
    </td>
    <td>
     nan
    </td>
    <td>
     100.00
    </td>
    <td>
     100.00
    </td>
    <td>
     100.00
    </td>
    <td>
     0.00
    </td>
    <td>
     nan
    </td>
    <td>
     nan
    </td>
    <td>
     100.00
    </td>
   </tr>
   <tr>
    <th>
     Gerhard
    </th>
    <td>
     250.00
    </td>
    <td>
     70.71
    </td>
    <td>
     5000.00
    </td>
    <td>
     250.00
    </td>
    <td>
     60000.00
    </td>
    <td>
     500.00
    </td>
    <td>
     50.00
    </td>
    <td>
     50.00
    </td>
    <td>
     nan
    </td>
    <td>
     250.00
    </td>
   </tr>
   <tr>
    <th>
     Hans
    </th>
    <td>
     266.67
    </td>
    <td>
     152.75
    </td>
    <td>
     23333.33
    </td>
    <td>
     300.00
    </td>
    <td>
     12000000.00
    </td>
    <td>
     800.00
    </td>
    <td>
     111.11
    </td>
    <td>
     88.19
    </td>
    <td>
     -0.94
    </td>
    <td>
     300.00
    </td>
   </tr>
  </tbody>
 </table>
</section>

# 6. Aggregate performance

Let's put the claim to the test that `.aggregate()` calls are faster than
aggregating directly on a group by, and find out in which circumstances this
statement holds. We would first like to find out which is faster: Multiple
aggregates in one `.aggregate()` call, or separate aggregates applied to a new
group by object each time? We choose to aggregate `min` and `max` on our
DataFrame.


```python
%%timeit
df.groupby(df.index).min()
df.groupby(df.index).max()
```

__Output:__

```
26.1 ms ± 819 µs per loop (mean ± std. dev. of 7 runs, 10 loops each)
```

```python
%%timeit
df.groupby(df.index).aggregate(['min', 'max'])
```

__Output:__

```
9.02 ms ± 75.6 µs per loop (mean ± std. dev. of 7 runs, 100 loops each)
```

We can clearly see that calling `.aggregate()` performs much faster when
multiple aggregate values are needed compared to aggregating twice on a
`.groupby()`. Furthermore, we can observe in the next two cells that there is
hardly any difference when only one aggregate value is required. In this case,
the shorter amount of code should win the contest, since it simply requires
less typing.


```python
%%timeit
df.groupby(df.index).min()
```

__Output:__

```
13.6 ms ± 945 µs per loop (mean ± std. dev. of 7 runs, 100 loops each)
```

```python
%%timeit
df.groupby(df.index).aggregate('min')
```

__Output:__

```
16.5 ms ± 4.14 ms per loop (mean ± std. dev. of 7 runs, 100 loops each)
```

# 7. Conclusion

I hope I was able to demonstrate a few use cases for Pandas aggregates. It is
certainly nice to be able to save some typing and have better performance when
dealing with multiple aggregate calculations, especially in a group by setting.

I certainly could not do without `.aggregate()`, as it saves me a lot of time
when typing out IPython notebooks.

