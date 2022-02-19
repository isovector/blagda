```agda
open import 1Lab.HLevel.Retracts
open import 1Lab.Type.Dec
open import 1Lab.HLevel
open import 1Lab.Equiv
open import 1Lab.Path
open import 1Lab.Type

module Data.Sum where
```

# Sum Types

Sum types are one of the fundamental ingredients of type theory.
They play a dual role to the [product type](agda://1Lab.Type#_×_);
if products allow us to state that we have elements of two types simultaneously,
sum types allow us to state that we have an element of _one_ of two types.

We use the notation `A ⊎ B` to hint at this type's set-theoretic analog: 
the disjoint union.

```agda
infixr 1 _⊎_

data _⊎_ {a b} (A : Type a) (B : Type b) : Type (a ⊔ b) where
  inl : A → A ⊎ B
  inr : B → A ⊎ B
```
<!--
```agda
private variable
  a b c d : Level
  A B C D : Type a
```
-->

As warmup, we have that both constructors are embeddings:

```agda
inl-inj : {B : Type b} {x y : A} → inl {B = B} x ≡ inl y → x ≡ y
inl-inj {A = A} {x = x} path = ap f path where
  f : A ⊎ B → A
  f (inl x) = x
  f (inr _) = x

inr-inj : {A : Type b} {x y : B} → inr {A = A} x ≡ inr y → x ≡ y
inr-inj {B = B} {x = x} path = ap f path where
  f : A ⊎ B → B
  f (inl _) = x
  f (inr x) = x

⊎-disjoint : {A : Type a} {B : Type b} {x : A} {y : B} → inl x ≡ inr y → ⊥
⊎-disjoint path = subst (λ { (inl x) → ⊤ ; (inr x) → ⊥ }) path tt
```

## Universal Properties

One of the most important things about sum types is the following property:
given two functions `A → C` and `B → C`, we can construct a function
`A ⊎ B → C`.

```agda
[_,_] : (A → C) → (B → C) → (A ⊎ B) → C
[ f , g ] (inl x) = f x
[ f , g ] (inr x) = g x
```

Furthermore, this function is "universal" in the following sense: if we
have some function `h : A ⊎ B → C` that behaves like `f` when provided
an `inl a`, and like `g` when provided `inr b`, then `h` _must_ be
identical to `[ f , g ]`.

```agda
[]-unique : ∀ {f : A → C} {g : B → C} {h} → f ≡ h ∘ inl → g ≡ h ∘ inr → [ f , g ] ≡ h
[]-unique p q = funext λ { (inl x) i → p i x ; (inr x) i → q i x }
```

We also have the following **eta law**. In general, eta laws relate the
_introduction_ forms with the _elimination_ forms. The most familiar eta
law is the one for functions: `λ x → (f x)` is the same as `f`. In agda,
the eta law for functions requires no proof, it holds by definition.
However, the same cannot be said for sum types, so we prove it here.

```agda
[]-η : ∀ (x : A ⊎ B) → [ inl , inr ] x ≡ x
[]-η (inl x) = refl
[]-η (inr x) = refl
```

This universal property can be strengthened to characterising the space
of _dependent functions_ out of the disjoint union: A dependent function
`(x : A ⊎ B) → P x` is the product of functions covering the left and
right cases.

```agda
⊎-universal : ∀ {A : Type a} {B : Type b} {C : A ⊎ B → Type c}
            → ((x : A ⊎ B) → C x)
            ≃ ( ((x : A) → C (inl x))
              × ((y : B) → C (inr y)))
⊎-universal {A = A} {B} {P} = Iso→Equiv the-iso where
  the-iso : Iso _ _
```

For "splitting" a dependent function from the coproduct, we can compose
it with either of the constructors to restrict to a function on that
factor:

```agda
  the-iso .fst f = (λ x → f (inl x)) , (λ x → f (inr x))
```

Similarly, given a pair of functions, we can do a case split on the
coproduct to decide which function to apply:

```agda
  the-iso .snd .isIso.inv (f , g) (inl x) = f x
  the-iso .snd .isIso.inv (f , g) (inr x) = g x

  the-iso .snd .isIso.rinv x = refl
  the-iso .snd .isIso.linv f i (inl x) = f (inl x)
  the-iso .snd .isIso.linv f i (inr x) = f (inr x)
```

## Transformations

Let's move away from the abstract nonsense of universal properties for a bit,
and cleanse our pallate with some small helper functions for mapping between sum
types.

```agda
⊎-map : (A → C) → (B → D) → A ⊎ B → C ⊎ D
⊎-map f g (inl a) = inl (f a)
⊎-map f g (inr b) = inr (g b)

⊎-mapl : (A → C) → A ⊎ B → C ⊎ B
⊎-mapl f = ⊎-map f id

⊎-mapr : (B → C) → A ⊎ B → A ⊎ C
⊎-mapr f = ⊎-map id f
```

## Decidablity

This type has a very similar structure to [Dec](agda://1Lab.Type.Dec#Dec), so
we provide some helpers to convert between the two.

```agda
from-dec : Dec A → A ⊎ (A → ⊥)
from-dec (yes a) = inl a
from-dec (no ¬a) = inr ¬a

to-dec : A ⊎ (A → ⊥) → Dec A
to-dec (inl  a) = yes a
to-dec (inr ¬a) = no ¬a
```

The proof that these functions are inverses is automatic by computation,
and thus it can be shown they are equivalences:

```agda
isEquiv-from-dec : {A : Type a} → isEquiv (from-dec {A = A})
isEquiv-from-dec = isIso→isEquiv (iso to-dec p q) where
  p : _
  p (inl x) = refl
  p (inr x) = refl

  q : _
  q (yes x) = refl
  q (no x) = refl
```

## Closure under h-levels

If $A$ and $B$ are $n$-types, for $n \ge 2$, then so is their coproduct.
The way we prove this is by characterising the entire path space of `A ⊎
B` in terms of the path spaces for `A` and `B`, using a recursive
definition:

```agda
module ⊎Path where
  Code : A ⊎ B → A ⊎ B → Type (level-of A ⊔ level-of B)
  Code {B = B} (inl x) (inl y) = Lift (level-of B) (x ≡ y)
  Code (inl x) (inr y) = Lift _ ⊥
  Code (inr x) (inl y) = Lift _ ⊥
  Code {A = A} (inr x) (inr y) = Lift (level-of A) (x ≡ y)
```

Given a `Code`{.Agda} for a path in `A ⊎ B`, we can turn it into a
legitimate path. Agda automatically lets us ignore the cases where
the `Code`{.Agda} computes to `the empty type`{.Agda ident=⊥}.

```agda
  decode : {x y : A ⊎ B} → Code x y → x ≡ y
  decode {x = inl x} {y = inl x₁} code = ap inl (Lift.lower code)
  decode {x = inr x} {y = inr x₁} code = ap inr (Lift.lower code)
```

In the inverse direction, we have a procedure for turning paths into
codes:

```agda
  encode : {x y : A ⊎ B} → x ≡ y → Code x y
  encode {x = inl x} {y = inl y} path = lift (inl-inj path)
  encode {x = inl x} {y = inr y} path = absurd (⊎-disjoint path)
  encode {x = inr x} {y = inl y} path = absurd (⊎-disjoint (sym path))
  encode {x = inr x} {y = inr y} path = lift (inr-inj path)
```

Now we must establish that `encode`{.Agda} and `decode`{.Agda} are
inverses. In the one direction, we can use path induction:

```agda
  decode-encode : {x y : A ⊎ B} (p : x ≡ y) → decode (encode p) ≡ p
  decode-encode = J (λ _ p → decode (encode p) ≡ p) d-e-refl where
    d-e-refl : {x : A ⊎ B} → decode (encode (λ i → x)) ≡ (λ i → x)
    d-e-refl {x = inl x} = refl
    d-e-refl {x = inr x} = refl
```

In the other direction, the proof is by case analysis, and everything
computes wonderfully to make the right-hand sides fillable by
`refl`{.Agda}:

```agda
  encode-decode : {x y : A ⊎ B} (p : Code x y) → encode (decode p) ≡ p
  encode-decode {x = inl x} {y = inl y} p = refl
  encode-decode {x = inr x} {y = inr y} p = refl
```

Thus, we have an equivalence between _codes for_ paths in `A ⊎ B` and
_actual_ paths `A ⊎ B`. Since `Code`{.Agda} has a nice computational
structure, we can establish its h-level by induction:

```agda
  Code≃Path : {x y : A ⊎ B} → Code x y ≃ (x ≡ y)
  Code≃Path = Iso→Equiv (decode , iso encode decode-encode encode-decode)
```

```agda
open ⊎Path

isHLevel-Code : {x y : A ⊎ B} {n : Nat}
            → isHLevel A (suc (suc n))
            → isHLevel B (suc (suc n))
            → isHLevel (Code x y) (suc n)
isHLevel-Code {x = inl x} {inl y} {n} ahl bhl =
  isHLevel-Lift (suc n) (ahl x y)
isHLevel-Code {x = inr x} {inr y} {n} ahl bhl =
  isHLevel-Lift (suc n) (bhl x y)
```

In the two cases where `x` and `y` match, we can use the fact that `Lift
preserves h-levels`{.Agda ident=isHLevel-Lift} and the assumption that
`A` (or `B`) have the given h-level.

```agda
isHLevel-Code {x = inl x} {inr y} {n} ahl bhl =
  isHLevel-Lift (suc n) (isProp→isHLevel-suc λ x → absurd x)
isHLevel-Code {x = inr x} {inl y} {n} ahl bhl = 
  isHLevel-Lift (suc n) (isProp→isHLevel-suc λ x → absurd x)
```

In the mismatched cases, we use the fact that `propositions have any
successor h-level`{.Agda ident=isProp→isHLevel-suc} to prove that `⊥` is
also at the same h-level as `A` and `B`. Thus, we have:

```agda
isHLevel-⊎ : (n : Nat)
           → isHLevel A (2 + n)
           → isHLevel B (2 + n)
           → isHLevel (A ⊎ B) (2 + n)
isHLevel-⊎ n ahl bhl x y = isHLevel≃ (1 + n) Code≃Path (isHLevel-Code ahl bhl)
```

Note that, in general, [being a proposition] and [being contractible]
are not preserved under coproducts. Consider the case where `(A, a)` and
`(B, b)` are both contractible (this generalises to propositions): Then
their coproduct has two distinct points, `in­l a` and `inr b`. However,
the coproduct of _disjoint_ propositions is a proposition:

[being a proposition]: agda://1Lab.HLevel#isProp
[being contractible]: agda://1Lab.HLevel#isContr

```agda
isProp-disjoint-⊎ : isProp A → isProp B → (A × B → ⊥)
                  → isProp (A ⊎ B)
isProp-disjoint-⊎ Ap Bp notab (inl x) (inl y) = ap inl (Ap x y)
isProp-disjoint-⊎ Ap Bp notab (inl x) (inr y) = absurd (notab (x , y))
isProp-disjoint-⊎ Ap Bp notab (inr x) (inl y) = absurd (notab (y , x))
isProp-disjoint-⊎ Ap Bp notab (inr x) (inr y) = ap inr (Bp x y)
```

## Closure under equivalences

[Univalence] automatically implies that all type formers respect
equivalences. However, the proof using univalence is restricted to types
of the same universe level. Thus, `⊎-ap`{.Agda}: Coproducts respect
equivalences in both arguments, across levels.

[Univalence]: 1Lab.Univalence.html#the-axiom

```agda
⊎-ap : A ≃ B → C ≃ D → (A ⊎ C) ≃ (B ⊎ D)
⊎-ap (f , f-eqv) (g , g-eqv) = Iso→Equiv cong where
  f-iso = isEquiv→isIso f-eqv
  g-iso = isEquiv→isIso g-eqv

  cong : Iso _ _
  cong .fst (inl x) = inl (f x)
  cong .fst (inr x) = inr (g x)

  cong .snd .isIso.inv (inl x) = inl (f-iso .isIso.inv x)
  cong .snd .isIso.inv (inr x) = inr (g-iso .isIso.inv x)

  cong .snd .isIso.rinv (inl x) = ap inl (f-iso .isIso.rinv x)
  cong .snd .isIso.rinv (inr x) = ap inr (g-iso .isIso.rinv x)

  cong .snd .isIso.linv (inl x) = ap inl (f-iso .isIso.linv x)
  cong .snd .isIso.linv (inr x) = ap inr (g-iso .isIso.linv x)

⊎-apˡ : A ≃ B → (A ⊎ C) ≃ (B ⊎ C)
⊎-apˡ f = ⊎-ap f (id , idEquiv)

⊎-apʳ : B ≃ C → (A ⊎ B) ≃ (A ⊎ C)
⊎-apʳ f = ⊎-ap (id , idEquiv) f
```

## Algebraic properties

Considered as an algebraic operator on _types_, the coproduct satisfies
many of the same properties of addition. Specifically, when restricted
to finite types, the coproduct is exactly the same as addition.

```agda
⊎-comm : (A ⊎ B) ≃ (B ⊎ A)
⊎-comm = Iso→Equiv i where
  i : Iso _ _
  i .fst (inl x) = inr x
  i .fst (inr x) = inl x

  i .snd .isIso.inv (inl x) = inr x
  i .snd .isIso.inv (inr x) = inl x

  i .snd .isIso.rinv (inl x) = refl
  i .snd .isIso.rinv (inr x) = refl
  i .snd .isIso.linv (inl x) = refl
  i .snd .isIso.linv (inr x) = refl

⊎-assoc : ((A ⊎ B) ⊎ C) ≃ (A ⊎ (B ⊎ C))
⊎-assoc = Iso→Equiv i where
  i : Iso _ _
  i .fst (inl (inl x)) = inl x
  i .fst (inl (inr x)) = inr (inl x)
  i .fst (inr x)       = inr (inr x)

  i .snd .isIso.inv (inl x)       = inl (inl x)
  i .snd .isIso.inv (inr (inl x)) = inl (inr x)
  i .snd .isIso.inv (inr (inr x)) = inr x

  i .snd .isIso.rinv (inl x) = refl
  i .snd .isIso.rinv (inr (inl x)) = refl
  i .snd .isIso.rinv (inr (inr x)) = refl

  i .snd .isIso.linv (inl (inl x)) = refl
  i .snd .isIso.linv (inl (inr x)) = refl
  i .snd .isIso.linv (inr x) = refl

⊎-zeroʳ : (A ⊎ ⊥) ≃ A
⊎-zeroʳ .fst (inl x) = x
⊎-zeroʳ .snd .isEqv y .centre = inl y , refl
⊎-zeroʳ .snd .isEqv y .paths (inl x , p) i = inl (p (~ i)) , λ j → p (~ i ∨ j)

⊎-zeroˡ : (⊥ ⊎ A) ≃ A
⊎-zeroˡ .fst (inr x) = x
⊎-zeroˡ .snd .isEqv y .centre = inr y , refl
⊎-zeroˡ .snd .isEqv y .paths (inr x , p) i = inr (p (~ i)) , λ j → p (~ i ∨ j)
```
