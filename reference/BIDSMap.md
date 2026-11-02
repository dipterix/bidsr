# Low-level nested map to store key-value data with inherited structure

Low-level nested map to store key-value data with inherited structure

## Usage

``` r
BIDSMap(parent = NULL, search_depth = BIDS_MAP_MAX_DEPTH())
```

## Arguments

- parent:

  `NULL` if the map is at the top level, or another map to inherit

- search_depth:

  integer maximum search depths; default is `29`; set options
  `'bidsr.map.search_depth'` or environment variable
  `'BIDS_MAP_MAX_DEPTH'` to change the default depth

## Value

A `'BIDSMap'` object.

## Author

Zhengjia Wang

## Examples

``` r


root_map <- BIDSMap()
root_map$key1 <- 1
root_map$key2 <- 2
names(root_map)
#> [1] "key2" "key1"

child_map <- BIDSMap(parent = root_map)
child_map$key3 <- 3
names(child_map)
#> [1] "key1" "key2" "key3"
child_map$key1
#> [1] 1
child_map$key2
#> [1] 2

# mask key2
child_map$key2 <- "a"
child_map
#> <bidsr::map acda0a31-d7ff-47cf-92d2-009249f702c7>
#>   (parent: 38fb550a-6ca5-49f0-ba43-633528a65eb2)
#> [key2, key3]
#>   (Inherited) [key1]

root_map$key2
#> [1] 2
child_map$key2
#> [1] "a"

# nested maps
grand_child <- BIDSMap(parent = child_map)

# value comes from child map
grand_child$key2
#> [1] "a"

# remove key2 from child map
child_map@impl$remove("key2")

# key2 is from root map now
grand_child$key2
#> [1] 2

```
