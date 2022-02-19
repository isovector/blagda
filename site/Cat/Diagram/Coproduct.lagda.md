```agda
open import Cat.Prelude

module Cat.Diagram.Coproduct {o h} (C : Precategory o h) where
```

<!--
```agda
open import Cat.Reasoning C
private variable
  A B : Ob
```
-->

# Coproducts

The **coproduct** $P$ of two objects $A$ and $B$ (if it exists), is the
smallest object equipped with "injection" maps $A \to P$, $B \to P$.
It is dual to the [product].

[product]: Cat.Diagram.Terminal.html

We witness this notion of "smallest object" with a universal property;
Given any other $Q$ that also admits injection maps from $A$ and $B$,
we must have a *unique* map $P \to Q$ that factors the injections into 
$Q$. This is best explained by a commutative diagram:

~~~{.quiver}
\[\begin{tikzcd}
  A & P & B \\
  & Q \\
  & {}
  \arrow[from=1-1, to=1-2]
  \arrow[from=1-3, to=1-2]
  \arrow[from=1-1, to=2-2]
  \arrow[from=1-3, to=2-2]
  \arrow["{\exists!}"', dashed, from=1-2, to=2-2]
\end{tikzcd}\]
~~~

```agda
record IsCoproduct {A B P} (in₀ : Hom A P) (in₁ : Hom B P) : Type (o ⊔ h) where
  field
    [_,_]      : ∀ {Q} (inj0 : Hom A Q) (inj1 : Hom B Q) → Hom P Q
    in₀∘factor : ∀ {Q} {inj0 : Hom A Q} {inj1} → [ inj0 , inj1 ] ∘ in₀ ≡ inj0
    in₁∘factor : ∀ {Q} {inj0 : Hom A Q} {inj1} → [ inj0 , inj1 ] ∘ in₁ ≡ inj1

    unique : ∀ {Q} {inj0 : Hom A Q} {inj1}
           → (other : Hom P Q)
           → other ∘ in₀ ≡ inj0
           → other ∘ in₁ ≡ inj1
           → other ≡ [ inj0 , inj1 ]

  unique₂ : ∀ {Q} {inj0 : Hom A Q} {inj1}
          → ∀ o1 (p1 : o1 ∘ in₀  ≡ inj0) (q1 : o1 ∘ in₁ ≡ inj1)
          → ∀ o2 (p2 : o2 ∘ in₀  ≡ inj0) (q2 : o2 ∘ in₁ ≡ inj1)
          → o1 ≡ o2
  unique₂ o1 p1 q1 o2 p2 q2 = unique o1 p1 q1 ∙ sym (unique o2 p2 q2)
```

A coproduct of $A$ and $B$ is an explicit choice of coproduct diagram:

```agda
record Coproduct (A B : Ob) : Type (o ⊔ h) where
  field
    coapex : Ob
    in₀ : Hom A coapex
    in₁ : Hom A coapex
    hasIsCoproduct : IsCoproduct in₀ in₁

  open IsCoproduct hasIsCoproduct public

open Coproduct
```

## Uniqueness

The uniqueness argument presented here is dual to the argument
for the [Product](agda://Cat.Diagram.Product#×-Unique).

```agda
+-Unique : (c1 c2 : Coproduct A B) → coapex c1 ≅ coapex c2
+-Unique c1 c2 = makeIso c1→c2 c2→c1 c1→c2→c1 c2→c1→c2
  where
    module c1 = Coproduct c1
    module c2 = Coproduct c2

    c1→c2 : Hom (coapex c1) (coapex c2)
    c1→c2 = c1.[ c2.in₀ , c2.in₁ ]

    c2→c1 : Hom (coapex c2) (coapex c1)
    c2→c1 = c2.[ c1.in₀ , c1.in₁ ]
```

```agda
    c1→c2→c1 : c1→c2 ∘ c2→c1 ≡ id
    c1→c2→c1 =
      c2.unique₂ _
        (pullr c2.in₀∘factor ∙ c1.in₀∘factor)
        (pullr c2.in₁∘factor ∙ c1.in₁∘factor)
        id (idl _) (idl _)

    c2→c1→c2 : c2→c1 ∘ c1→c2 ≡ id
    c2→c1→c2 =
      c1.unique₂ _
        (pullr c1.in₀∘factor ∙ c2.in₀∘factor)
        (pullr c1.in₁∘factor ∙ c2.in₁∘factor)
        id (idl _) (idl _)
```

