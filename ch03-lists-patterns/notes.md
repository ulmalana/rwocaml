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
