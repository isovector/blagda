```agda
open import Cat.Univalent
open import Cat.Prelude

module Cat.Diagram.Zero {o h} (C : Precategory o h) where

open import Cat.Diagram.Initial C
open import Cat.Diagram.Terminal C
```

<!--
```agda
open import Cat.Reasoning C
```
-->

# Zero Objects

In some categories, `Initial`{.Agda} and `Terminal`{.Agda} objects
coincide. When this occurs, we call the object a **zero object**.

```agda
record isZero (ob : Ob) : Type (o ⊔ h) where
  field
    is-initial  : isInitial ob
    is-terminal : isTerminal ob

record Zero : Type (o ⊔ h) where
  field
    ∅       : Ob
    is-zero : isZero ∅

  open isZero is-zero public

  terminal : Terminal
  terminal = record { top = ∅ ; has⊤ = is-terminal }

  initial : Initial
  initial = record { bot = ∅ ; has⊥ = is-initial }

  open Terminal terminal public hiding (top)
  open Initial initial public hiding (bot)
```

A curious fact about zero objects is that their existence implies that
every hom set is inhabited!

```agda
  zero→ : ∀ {x y} → Hom x y
  zero→ = ¡ ∘ !

  zero-∘ˡ : ∀ {x y z} → (f : Hom y z) → f ∘ zero→ {x} {y} ≡ zero→
  zero-∘ˡ f = pulll (sym (¡-unique (f ∘ ¡)))

  zero-∘ʳ : ∀ {x y z} → (f : Hom x y) → zero→ {y} {z} ∘ f ≡ zero→
  zero-∘ʳ f = pullr (sym (!-unique (! ∘ f)))
```

## Intuition

<!-- [TODO: Reed M, 15/02/2022]  Link to the category of groups -->

Most categories that have zero objects have enough structure to rule out
*totally* trivial structures like the empty set, but not enough
structure to cause the initial and terminal objects to "separate". The
canonical example here is the category of groups: the unit rules out a
completely trivial group, yet there's nothing else that would require
the initial object to have any more structure.

Another point of interest is that any category with zero objects is
canonically enriched in pointed sets: the `zero→`{.Agda} morphism from
earlier acts as the designated basepoint for each of the hom sets.
