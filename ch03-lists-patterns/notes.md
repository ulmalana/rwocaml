# Chapter 3 - Lists and Patterns

## List

* List is an immutable, finite sequence of elements of the same type.
    * `[1;2;3]`
    * `1 :: (2 :: (3 :: []))`
    * `1 :: 2 :: 3 :: []`
* The empty list `[]` is polymorphic with type `'a list` and can be used to
  terminate a list of various type.

## Extract data from a list with patterns
* ```
    let rec sum l = 
        match l with
        | [] -> 0
        | hd :: tl -> hd + sum tl
  ```
## Limitations and Blessing of Pattern Matching

### Performance
* Pattern matching with `match` is considerably faster than with `if`.

### Detecting errors
* Pattern matching with `match` can detect **case redundancy** and check for
  its exhaustiveness to ensure that all possible cases will be handled.

## Using the List module effectively

`List` module provides many reusable functions that can be used for computing
with list, instead of building our own function using pattern matching and
recursion.

* `concat` is better than `^` for large data.
* `List.reduce` is a specialized version of `List.fold` that doesnt require
  explicit starting value.
* `List.filter_map` is used to both transform and filter some lists. 
* `List.partition_tf` is used for splitting a list based on some boolean
  functions
* `List.append` or `@` are used to concatenate lists
    * `List.append [1;2;3] [4;5;6]` -> `[1;2;3;4;5;6]`
    * `[1;2;3] @ [4;5;6]` -> `[1;2;3;4;5;6]`

## Tail recursion

Doing recursion is convenient but it can blows the stack with large amount of
recursion. Example: computing the length of a list recursively:

```
    let make_list = List.init n ~f:(fun x -> x)
    let rec length = function
        | [] -> 0
        | _ :: tl -> 1 + length tl
```

With **tail-call optimization**, we dont need to allocate more stack frame when
calling a function recursively. The following is the optimized version of
`length`

```
    let rec length_plus_n l n = 
        match l with
        | [] -> n
        | _ :: tl -> length_plus_n tl (n+1)

    let length' l = length_plus_n l 0
```

## Polymorphic comparison

By default, `Base` equality operator is specialize to integers. To use
polymorphic equality and comparison operators, open module `Base.Poly`. Using
polymorphic operator might produce **unpredictable behaviour**, so `Base`
discourages the use of polymorphic operator by hiding it by default.
