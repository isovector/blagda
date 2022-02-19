```agda
open import Cat.Prelude

module Cat.Diagram.Product {o h} (C : Precategory o h) where
```

<!--
```agda
open import Cat.Morphism C
private variable
  A B : Ob
```
-->

# Products

The **product** $P$ of two objects $A$ and $B$, if it exists, is the
smallest object equipped with "projection" maps $P \to A$ and $P \to B$.
This situation can be visualised by putting the data of a product into a
commutative diagram, as the one below: To express that $P$ is the
_smallest_ object with projections to $A$ and $B$, we ask that any other
object $Q$ with projections through $A$ and $B$ factors uniquely through
$P$:

~~~{.quiver}
\[\begin{tikzcd}
  & Q \\
  A & P & B
  \arrow[from=2-2, to=2-1]
  \arrow[from=2-2, to=2-3]
  \arrow[from=1-2, to=2-1]
  \arrow["{\exists!}"', dashed, from=1-2, to=2-2]
  \arrow[from=1-2, to=2-3]
\end{tikzcd}\]
~~~

In the sense that (univalent) categories generalise posets, the product
of $A$ and $B$ --- if it exists --- generalises the binary meet 
$A \wedge B$. Since products are [unique](#uniqueness) when they exist,
we may safely denote any product of $A$ and $B$ by $A \times B$.

For a diagram $A \ot A \times B \to B$ to be a product diagram, it must
be able to cough up an arrow $Q \to P$ given the data of another span $A
\ot Q \to B$, which must not only fit into the diagram above but be
unique among the arrows that do so. 

This factoring is called the **pairing** of the arrows $f : Q \to A$ and
$g : Q \to B$, since in the special case where $Q$ is the [terminal
object] (hence the two arrows are global elements of $A$ resp. $B$), the
pairing $\langle f, g \rangle$ is a global element of the product $A
\times B$.

[terminal object]: Cat.Diagram.Terminal.html

```agda
record IsProduct {A B P} (π₁ : Hom P A) (π₂ : Hom P B) : Type (o ⊔ h) where
  field
    ⟨_,_⟩     : ∀ {Q} (p1 : Hom Q A) (p2 : Hom Q B) → Hom Q P
    π₁∘factor : ∀ {Q} {p1 : Hom Q _} {p2} → π₁ ∘ ⟨ p1 , p2 ⟩ ≡ p1
    π₂∘factor : ∀ {Q} {p1 : Hom Q _} {p2} → π₂ ∘ ⟨ p1 , p2 ⟩ ≡ p2

    unique : ∀ {Q} {p1 : Hom Q A} {p2}
           → (other : Hom Q P)
           → π₁ ∘ other ≡ p1
           → π₂ ∘ other ≡ p2
           → other ≡ ⟨ p1 , p2 ⟩

  unique₂ : ∀ {Q} {pr1 : Hom Q A} {pr2}
          → ∀ o1 (p1 : π₁ ∘ o1 ≡ pr1) (q1 : π₂ ∘ o1 ≡ pr2)
          → ∀ o2 (p2 : π₁ ∘ o2 ≡ pr1) (q2 : π₂ ∘ o2 ≡ pr2)
          → o1 ≡ o2
  unique₂ o1 p1 q1 o2 p2 q2 = unique o1 p1 q1 ∙ sym (unique o2 p2 q2)
```

A product of $A$ and $B$ is an explicit choice of product diagram:

```agda
record Product (A B : Ob) : Type (o ⊔ h) where
  field
    apex : Ob
    π₁ : Hom apex A
    π₂ : Hom apex B
    hasIsProduct : IsProduct π₁ π₂

  open IsProduct hasIsProduct public

open Product
```

## Uniqueness

Products, when they exist, are unique. It's easiest to see this with a
diagrammatic argument: If we have product diagrams $A \ot P \to B$ and
$A \ot P' \to B$, we can fit them into a "commutative diamond" like the
one below:

~~~{.quiver .tall-1}
\[\begin{tikzcd}
  & P \\
  A && B \\
  & {P'}
  \arrow[from=3-2, to=2-3]
  \arrow[from=1-2, to=2-3]
  \arrow[from=1-2, to=2-1]
  \arrow[from=3-2, to=2-1]
\end{tikzcd}\]
~~~

Since both $P$ and $P'$ are products, we know that the dashed arrows in
the diagram below exist, so the overall diagram commutes: hence we have
an isomorphism $P \cong P'$.

~~~{.quiver .tall-1}
\[\begin{tikzcd}
  & P \\
  A && B \\
  & {P'}
  \arrow[from=3-2, to=2-3]
  \arrow[from=1-2, to=2-3]
  \arrow[from=1-2, to=2-1]
  \arrow[from=3-2, to=2-1]
  \arrow["{\exists!}"', shift right=1, dashed, from=1-2, to=3-2]
  \arrow["{\exists!}"', shift right=1, dashed, from=3-2, to=1-2]
\end{tikzcd}\]
~~~

We construct the map $P \to P'$ as the pairing of the projections from
$P$, and symmetrically for $P' \to P$.

```agda
×-Unique : (p1 p2 : Product A B) → apex p1 ≅ apex p2
×-Unique p1 p2 = makeIso p1→p2 p2→p1 p1→p2→p1 p2→p1→p2
  where
    module p1 = Product p1
    module p2 = Product p2

    p1→p2 : Hom (apex p1) (apex p2)
    p1→p2 = p2.⟨ p1.π₁ , p1.π₂ ⟩

    p2→p1 : Hom (apex p2) (apex p1)
    p2→p1 = p1.⟨ p2.π₁ , p2.π₂ ⟩
```

These are unique because they are maps into products which commute with
the projections.

```agda
    p1→p2→p1 : p1→p2 ∘ p2→p1 ≡ id
    p1→p2→p1 = 
      p2.unique₂ _ 
        (assoc _ _ _ ·· ap (_∘ _) p2.π₁∘factor ·· p1.π₁∘factor) 
        (assoc _ _ _ ·· ap (_∘ _) p2.π₂∘factor ·· p1.π₂∘factor) 
        id (idr _) (idr _)

    p2→p1→p2 : p2→p1 ∘ p1→p2 ≡ id
    p2→p1→p2 = 
      p1.unique₂ _ 
        (assoc _ _ _ ·· ap (_∘ _) p1.π₁∘factor ·· p2.π₁∘factor) 
        (assoc _ _ _ ·· ap (_∘ _) p1.π₂∘factor ·· p2.π₂∘factor) 
        id (idr _) (idr _)
```
