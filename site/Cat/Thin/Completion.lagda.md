```agda
open import Cat.Instances.StrictCat
open import Cat.Instances.Functor
open import Cat.Functor.Adjoint
open import Cat.Functor.Base
open import Cat.Prelude
open import Cat.Thin

open import Data.Set.Coequaliser

module Cat.Thin.Completion where
```

<!--
```agda
open Functor
open Proset using (underlying)
open Poset using (underlying)
```
-->

# Poset completion

We construct a universal completion of a `proset`{.Agda} to a
`poset`{.Agda}. Initially, recall the terms. A proset (which is how we
refer to strict, thin [categories]) is a set equipped with a relation $-
\le -$ which is both reflexive and transitive, but not necessarily
antisymmetric. A poset augments this with the requirement that $\le$ is
antisymmetric: It's a [_univalent_] thin category.

[categories]: Cat.Base.html
[_univalent_]: Cat.Univalent.html

The construction is _conceptually_ straightforward: The poset completion
of $\ca{C}$, written $\widehat{\ca{C}}$, will have an underlying set of
objects $\widehat{\ca{C}}_0$ given by a quotient of $\ca{C}_0$ by the
relation $(x \le y) \land (y \le x)$. Essentially, we will forcibly
"throw in" all of the missing antisymmetries.

[quotient]: Data.Set.Coequaliser.html#quotients

```agda
module Poset-completion {o h} (C : Proset o h) where
  module C = Proset C

  private
    _~_ : C.Ob → C.Ob → Type _
    x ~ y = C.Hom x y × C.Hom y x

  Ob′ : Type (o ⊔ h)
  Ob′ = C.Ob / _~_
```

However, showing that we can lift $\le$ from the proset $\ca{C}$ to its
completion $\widehat{\ca{C}}$ is much harder than it should be. We start
by showing that, assuming that $x \le y$ and $y \le x$, we have
equalities $(x \le a) = (y \le a)$ and $(x \le a) = (y \le a)$. These
are given by propositional extensionality and pre/post composition with
the assumed inequalities:

```agda
  private abstract
    p1 : (a : C.Ob) ((x , y , r) : Σ[ x ∈ C.Ob ] Σ[ y ∈ C.Ob ] x ~ y) 
      → Path (Prop h) (C.Hom x a , C.isThin x a) (C.Hom y a , C.isThin y a)
    p1 a (x , y , f , g) = 
      Σ≡Prop (λ _ → isProp-isProp) 
        (ua (propExt (C.isThin _ _) (C.isThin _ _) (λ h → h C.∘ g) (λ h → h C.∘ f)))

    p2 : (a : C.Ob) ((x , y , r) : Σ[ x ∈ C.Ob ] Σ[ y ∈ C.Ob ] x ~ y) 
      → Path (Prop h) (C.Hom a x , C.isThin a x) (C.Hom a y , C.isThin a y)
    p2 a (x , y , f , g) = 
      Σ≡Prop (λ _ → isProp-isProp) 
        (ua (propExt (C.isThin _ _) (C.isThin _ _) (λ h → f C.∘ h) (λ h → g C.∘ h)))
```

We can then eliminate from our quotient to the `type of
propositions`{.Agda}. This is because we're trying to define a type
which is a proposition, but we can't directly eliminate into
`Type`{.Agda}, since set-quotients only let you eliminate into sets. By
the equalities above, the map $x, y \mapsto (x \le y)$ respects the
quotient, hence `Hom′`{.Agda} below exists:

```agda
  Hom′ : Ob′ → Ob′ → Prop _
  Hom′ = Coeq-rec₂ (isHLevel-nType 1) (λ x y → C.Hom x y , C.isThin x y) p1 p2

  Hom′-prop : ∀ (x y : Ob′) (f g : Hom′ x y .fst) → f ≡ g
  Hom′-prop x y f g = Hom′ x y .snd f g
```

We can now prove that `Hom′`{.Agda} is `reflexive`{.Agda ident=id′},
`transitive`{.Agda ident=trans′} and `antisymmetric`{.Agda
ident=antisym′}. We get these by elimination on the domains/codomains of
the map:

```agda
  id′ : ∀ x → Hom′ x x .fst
  id′ = Coeq-elimProp (λ x → Hom′ x x .snd) (λ _ → C.id)

  trans′ : ∀ x y z → Hom′ x y .fst → Hom′ y z .fst → Hom′ x z .fst
  trans′ = Coeq-elimProp₃ 
    (λ x _ z → isHLevel→ 1 (isHLevel→ 1 (Hom′ x z .snd))) 
    (λ _ _ _ f g → g C.∘ f)

  antisym′ : ∀ x y → Hom′ x y .fst → Hom′ y x .fst → x ≡ y
  antisym′ = Coeq-elimProp₂ 
    (λ x y → isHLevel→ 1 (isHLevel→ 1 (squash _ _))) 
    (λ x y f g → quot (f , g))
```

The data above cleanly defines a `Poset`{.Agda}, so we're done!

```agda
  completed : Poset (o ⊔ h) h
  completed = makePoset {A = Ob′} {R = λ x y → Hom′ x y .fst} 
      (λ {x} → id′ x) 
      (λ {x} {y} {z} → trans′ x y z) 
      (λ {x} {y} → antisym′ x y) 
      (λ {x} {y} → Hom′ x y .snd)

open Poset-completion 
  renaming (completed to Poset-completion)
  hiding (Ob′ ; Hom′ ; Hom′-prop ; trans′ ; antisym′ ; id′)
```

## Embedding

There is a functor between the underlying category of a proset $\ca{C}$
and the underlying category of its completion $\widehat{\ca{C}}$, with
object part given by the quotient map `inc`{.Agda}.

```agda
Complete : ∀ {o h} {X : Proset o h} 
         → Functor (X .underlying) (Poset-completion X .underlying)
Complete .F₀ = inc
Complete .F₁ x = x
Complete .F-id = refl
Complete .F-∘ f g = refl
```

This functor has morphism part given by the identity function, so it's
fully faithful. It exhibits $\ca{C}$ as a full subproset of
$\widehat{\ca{C}}$.

```agda
Complete-ff : ∀ {o h} {X : Proset o h} → isFf (Complete {X = X})
Complete-ff = idEquiv
```

## Lifting functors

We prove that any functor $F : \ca{X} \to \ca{Y}$ lifts to a functor
$\widehat{F} : \widehat{\ca{X}} \to \widehat{\ca{Y}}$ between the
respective poset completions. The hardest part of the construction is
showing that $F_0$, i.e. the action of $F$ on the objects of $\ca{X}$,
respects the quotient which defines $\widehat{\ca{X}}$.

```agda
module _ 
  {o h} (X Y : Proset o h) 
  (F : Functor (X .underlying) (Y .underlying)) 
  where

  private
    module X′ = Poset (Poset-completion X)
    module Y′ = Poset (Poset-completion Y)
```

Fortunately, even this is not very hard: It suffices to show that if $x
< y$ and $y < x$, then $f(x) < f(y)$ and $f(y) < f(x)$. But this is
immediate by monotonicity of $F$.

```agda
    F′₀ : X′.Ob → Y′.Ob
    F′₀ = Coeq-rec Y′.isStrict 
      (λ x → inc (F₀ F x)) 
      (λ (_ , _ , f , g) → quot (F₁ F f , F₁ F g))
```

The rest of the data of a functor is immediate by induction on
quotients. It's given by lifting the functor data from $F$ to the
quotient, but it is quite annoying to convince Agda that this is a legal
move.

```agda
    F′₁ : (a b : X′.Ob) → X′.Hom a b → Y′.Hom (F′₀ a) (F′₀ b)
    F′₁ = Coeq-elimProp₂ 
      (λ a b → isHLevel→ 1 (Y′.isThin (F′₀ a) (F′₀ b))) 
      (λ _ _ → F₁ F)

    abstract
      F′₁-id : ∀ (a : X′.Ob) → F′₁ a a (X′.id {a}) ≡ Y′.id {F′₀ a}
      F′₁-id = Coeq-elimProp 
        (λ a → Y′.Hom-set (F′₀ a) (F′₀ a) _ _) 
        (λ a → F-id F)

      F′₁-∘ : ∀ (x y z : X′.Ob) (f : X′.Hom y z) (g : X′.Hom x y)
            → F′₁ x z (X′._∘_ {x} {y} {z} f g) 
            ≡ Y′._∘_ {F′₀ x} {F′₀ y} {F′₀ z} (F′₁ y z f) (F′₁ x y g)
      F′₁-∘ = 
        Coeq-elimProp₃ 
          (λ x y z → 
            isHLevelΠ 1 λ f → 
            isHLevelΠ 1 λ g → 
            Y′.Hom-set (F′₀ x) (F′₀ z) _ _) 
          λ x y z f g → F-∘ F f g
```

This defines a map between the completions of $\ca{X}$ and $\ca{Y}$:

```agda
  liftToCompletion : Functor X′.underlying Y′.underlying
  liftToCompletion .F₀               = F′₀
  liftToCompletion .F₁   {x} {y}     = F′₁ x y
  liftToCompletion .F-id {x}         = F′₁-id x
  liftToCompletion .F-∘  {x} {y} {z} = F′₁-∘ x y z
```
