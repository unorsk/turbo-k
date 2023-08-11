### Variable declarations and assignment

```
a: 123
q)b:1+a:55
q)b
56
q)a
55
```

## Data types
int   - 64-bit signed integer
float - 8-byte value conforming to the IEEE floating-point specification
date - yyyy.mm.dd - is a signed integer, the number of days since the millenium (2000.01.01 / this is actually 0)
division always returns a float

## Comments

/ This is a comment
it requires at least one space to the left of it


### built-ins

- show
```
q)show show
k){1 .Q.s x;}
```