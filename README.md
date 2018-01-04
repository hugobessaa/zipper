Zipper
---

Zipper is a data structure that encodes pointing to an element into its type.

This repository has Zipper implementations for some data structures and enables
you to create your own Zippers based on any data structure you might want to
use for the before and after elements.

A `ListZipper` has a pointer to the current element, a `List` of elements before it
, and a `List` of elements after it. `ArrayZipper` has a pointer to the current
element, an `Array` of elements before it, and an `Array` of elements after it.

The goal of this repo is to be able to create an useful generic data structure
of type:

```elm
type Zipper seq a
    = Zipper (seq a) a (seq a)
```

And possibly:


```elm
type Zipper seq a
    = Zipper (seq a) Maybe a (seq a)
```

So that we can have a Zipper that _might_ have a current pointer or not.
