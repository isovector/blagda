```agda
open import 1Lab.Type.Sigma
open import 1Lab.Univalence
open import 1Lab.HLevel
open import 1Lab.Equiv
open import 1Lab.Path
open import 1Lab.Type

module 1Lab.Equiv.Embedding where
```

<!--
```
private variable
  ℓ ℓ₁ : Level
  A B : Type ℓ
  w x : A
```
-->

# Embeddings

In HoTT, the notion of _injective map_ is not well-behaved for types
that are not sets. Thus, we strengthen the notion: Rather than using
`injective`{.Agda}, we use `isEmbedding`{.Agda}.

```agda
injective : (A → B) → Type _
injective f = {x y : _} → f x ≡ f y → x ≡ y

isEmbedding : (A → B) → Type _
isEmbedding f = {x y : _} → isEquiv (λ (p : x ≡ y) → ap f p)

_↪_ : Type ℓ → Type ℓ₁ → Type (ℓ ⊔ ℓ₁)
A ↪ B = Σ[ f ∈ (A → B) ] isEmbedding f
```

One of the canonical sources of embeddings are the _subtype inclusions_.
A subtype of `A` is given by a predicate `B : A → Type`, such that `B x`
is always a proposition.  When this is the case, we have that the `first
projection`{.Agda ident=fst} is an embedding:

```agda
Subset-proj-embedding
  : ∀ {a b} {A : Type a} {B : A → Type b}
  → ((x : A) → isProp (B x))
  → isEmbedding (fst {A = A} {B = B})
Subset-proj-embedding bp =
  isIso→isEquiv (isIso.inverse (isEquiv→isIso (isEquiv-Σ≡Prop bp)))
```

It can be seen that embeddings are a generalisation of injective
functions (hence, of subset inclusions) by considering how embeddings
behave when applied to sets:

```agda
injective-sets→embedding : isSet A → isSet B → (f : A → B)
                         → injective f
                         → isEmbedding f
```

In this case, we have that both `f x ≡ f y` and `x ≡ y` are mere
propositions, so biimplication becomes equivalence:

```agda
injective-sets→embedding Aset Bset f injective =
  isIso→isEquiv (iso injective (λ _ → Bset _ _ _ _) (λ _ → Aset _ _ _ _))
```

An equivalent characterisation of the embeddings is that they are the
types with `propositional`{.Agda ident=isProp} `fibres`{.Agda
ident=fibre}.

<!--
```
private
  fibAp→PathP
    : {f : A → B}
    → (p : f w ≡ f x)
    → (fi : fibre (ap f) p)
    → PathP (λ i → fibre f (p i)) (w , refl) (x , refl)
  fibAp→PathP p (q , r) i = q i , λ j → r j i

  PathP→fibAp
    : {f : A → B}
    → (p : f w ≡ f x)
    → (pp : PathP (λ i → fibre f (p i)) (w , refl) (x , refl))
    → fibre (ap f) p
  PathP→fibAp p pp = (λ i → fst (pp i)) , (λ j i → snd (pp i) j)

PathP≡fibAp
  : {f : A → B}
  → (p : f w ≡ f x)
  → PathP (λ i → fibre f (p i)) (w , refl) (x , refl) ≡ fibre (ap f) p
PathP≡fibAp p
  = Iso→path (PathP→fibAp p , iso (fibAp→PathP p) (λ _ → refl) (λ _ → refl))
```
-->

```agda
hasPropFibres→isEmbedding : (f : A → B) → ((x : B) → isProp (fibre f x))
                          → isEmbedding f
hasPropFibres→isEmbedding f prop-fib {w} {x} .isEqv y =
  subst isContr (PathP≡fibAp y)
    (isProp→isContrPathP (λ i → prop-fib (y i)) (w , refl) (x , refl))
```
