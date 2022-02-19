```agda
open import Cat.Functor.Adjoint
open import Cat.Prelude
open import Cat.Monad

open Functor
open Monad
open _=>_
```

# The Monad from an Adjunction

```
module
  Cat.Functor.Adjoint.Monad
  {o₁ h₁ o₂ h₂ : _}
  {C : Precategory o₁ h₁}
  {D : Precategory o₂ h₂}
  {L : Functor C D} {R : Functor D C}
  (L⊣R : L ⊣ R)
  where

private
  module C = Precategory C
  module D = Precategory D
  module L = Functor L
  module R = Functor R
  module adj = _⊣_ L⊣R
```

Every adjunction $L \dashv R$ gives rise to a monad, where the
underlying functor is $R \circ L$.

```
Adjunction→Monad : Monad
Adjunction→Monad .M = R F∘ L
```

The unit of the monad is just adjunction monad, and the multiplication
comes from the counit.

```
Adjunction→Monad .unit = adj.unit
Adjunction→Monad .mult = NT (λ x → R.₁ (η adj.counit (L.₀ x))) λ x y f →
  R.₁ (adj.counit.ε (L.₀ y)) C.∘ R.₁ (L.₁ (R.₁ (L.₁ f))) ≡⟨ sym (R.F-∘ _ _) ⟩
  R.₁ (adj.counit.ε (L.₀ y) D.∘ L.₁ (R.₁ (L.₁ f)))       ≡⟨ ap R.₁ (adj.counit.is-natural _ _ _) ⟩
  R.₁ (L.₁ f D.∘ adj.counit.ε (L.₀ x))                   ≡⟨ R.F-∘ _ _ ⟩
  _                                                      ∎
```

The monad laws follow from the zig-zag identities. In fact, the
`right-ident`{.Agda}ity law is exactly the `zag`{.Agda ident="adj.zag"}
identity.

```
Adjunction→Monad .right-ident {x} = adj.zag
```

The others are slightly more involved.

```
Adjunction→Monad .left-ident =
  R.₁ (adj.counit.ε _) C.∘ R.₁ (L.₁ (adj.unit.η _)) ≡⟨ sym (R.F-∘ _ _) ⟩
  R.₁ (adj.counit.ε _ D.∘ L.₁ (adj.unit.η _))       ≡⟨ ap R.₁ adj.zig ⟩
  R.₁ D.id                                          ≡⟨ R.F-id ⟩
  C.id                                              ∎

Adjunction→Monad .mult-assoc =
  R.₁ (adj.counit.ε _) C.∘ R.₁ (L.₁ (R.₁ (adj.counit.ε _)))   ≡⟨ sym (R.F-∘ _ _) ⟩
  R.₁ (adj.counit.ε _ D.∘ L.₁ (R.₁ (adj.counit.ε _)))         ≡⟨ ap R.₁ (adj.counit.is-natural _ _ _) ⟩
  R.₁ (adj.counit.ε _ D.∘ adj.counit.ε _)                     ≡⟨ R.F-∘ _ _ ⟩
  R.₁ (adj.counit.ε _) C.∘ R.₁ (adj.counit.ε _)               ∎
```
