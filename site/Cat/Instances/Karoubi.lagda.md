```agda
open import Cat.Functor.Base
open import Cat.Prelude

import Cat.Diagram.Idempotent as CI

module Cat.Instances.Karoubi {o h} (C : Precategory o h) where
```

<!--
```agda
open CI C
import Cat.Reasoning C as C
open Precategory
open Functor
```
-->

# Karoubi envelopes

We give a construction of the **Karoubi envelope** $\~\ca{C}$ of a
precategory $\ca{C}$, a formal construction which adds a choice of
splittings for every [idempotent] in $\ca{C}$. Furthermore, the Karoubi
envelope is the _smallest_ idempotent-complete category which admits a
map from $\ca{C}$, in the sense that any $F : \ca{C} \to \ca{D}$ into an
idempotent-complete category $\ca{D}$ factors through $\~\ca{C}$:

$$
\ca{C} \mono \~\ca{C} \to \ca{D}
$$

Furthermore, the `embedding functor`{.Agda ident=Embed} $\ca{C} \to
\~\ca{C}$ is [fully faithful].

[fully faithful]: Cat.Functor.Base.html#ff-functors
[idempotent]: Cat.Diagram.Idempotent.html

The `objects` in $\~\ca{C}$ are given by pairs of an object $c : \ca{C}$
and an idempotent $f : c \to c$. A map between $(c,f)$ and $(d,g)$ is
given by a map $\phi : c \to d$ which absorbs $f$ from the left and $g$
from the right: $\phi \circ f = \phi = g \circ \phi$.

```agda
private
  KOb : Type (o ⊔ h)
  KOb = Σ[ c ∈ C.Ob ] Σ[ f ∈ C.Hom c c ] (isIdempotent f)

  KHom : KOb → KOb → Type h
  KHom (c , f , _) (d , g , _) = Σ[ φ ∈ C.Hom c d ] ((φ C.∘ f ≡ φ) × (g C.∘ φ ≡ φ))
  
  KH≡ : ∀ {a b : C.Ob} {af : C.Hom a a} {bf : C.Hom b b}
          {ai : isIdempotent af} {bi : isIdempotent bf}
          {f g : KHom (a , af , ai) (b , bf , bi)} → fst f ≡ fst g → f ≡ g
  KH≡ = Σ≡Prop (λ _ → (isHLevel× 1 (C.Hom-set _ _ _ _) (C.Hom-set _ _ _ _)))
```

We can see that these data assemble into a precategory. However, note
that the identity on $(c,e)$ in $\~\ca{C}$ _isn't_ the identity in $C$,
it's the chosen idempotent $e$!

```agda
Karoubi : Precategory (o ⊔ h) h
Karoubi .Ob = KOb
Karoubi .Hom = KHom
Karoubi .Hom-set _ _ = 
  isHLevelΣ 2 (C.Hom-set _ _) λ _ → 
    isProp→isSet (isHLevel× 1 (C.Hom-set _ _ _ _) (C.Hom-set _ _ _ _))

Karoubi .id {x = c , e , i} = e , i , i
Karoubi ._∘_ (f , fp , fp') (g , gp , gp') = f C.∘ g , C.pullr gp , C.pulll fp'

Karoubi .idr {x = _ , _ , i} {_ , _ , j} (f , p , q) = KH≡ {ai = i} {bi = j} p
Karoubi .idl {x = _ , _ , i} {_ , _ , j} (f , p , q) = KH≡ {ai = i} {bi = j} q
Karoubi .assoc {w = _ , _ , i} {z = _ , _ , j} _ _ _ = 
  KH≡ {ai = i} {bi = j} (C.assoc _ _ _)
```

We can now define the embedding functor from C to its `Karoubi`{.Agda}
envelope. It has object part $x \mapsto (x, \mathrm{id})$; The morphism
part of the functor has to send $f : x \to y$ to some $f' : x \to y$
which absorbs $\mathrm{id}$ on either side; But this is just $f$ again.

```agda
Embed : Functor C Karoubi
Embed .F₀ x = x , C.id , C.idr _
Embed .F₁ f = f , C.idr _ , C.idl _
Embed .F-id = KH≡ {ai = C.idl _} {bi = C.idl _} refl
Embed .F-∘ f g = KH≡ {ai = C.idl _} {bi = C.idl _} refl
```

An elementary argument shows that the morphism part of `Embed`{.Agda}
has an inverse given by projecting the first component of the pair;
Hence, `Embed` is `fully faithful`{.Agda ident=isFf}.

```agda
Embed-ff : isFf Embed
Embed-ff = isIso→isEquiv (iso fst (λ _ → Σ≡Prop p refl) λ _ → refl) where
  p : (x : C.Hom _ _) → _
  p _ = isHLevel× 1 (C.Hom-set _ _ _ _) (C.Hom-set _ _ _ _)
```

## Idempotent-completeness

We now show that any idempotent $f : (A, e) \to (A, e)$ admits a
splitting in $\~\ca{C}$. First, note that since $f$ is (by assumption)
idempotent, we have an object given by $(A, f)$; We'll split $f$ as a
map

$$
(A, e) \to (A, f) \to (A, e)
$$

The first map is given by the underlying map of $f : A \to A$. We must
show that $f \circ e = f$, but we have this by the definition of maps in
$\~\ca{C}$. In the other direction, we can _again_ take $f : A \to A$,
which also satisfies $e \circ f = f$.

```agda
isIdempotentComplete-Karoubi : CI.isIdempotentComplete Karoubi
isIdempotentComplete-Karoubi {A = A , e , i} (f , p , q) idem = spl where
  open CI.IsSplit

  f-idem : f C.∘ f ≡ f
  f-idem i = idem i .fst

  spl : CI.IsSplit Karoubi (f , p , q)
  spl .F = A , f , f-idem
  spl .project = f , p , f-idem
  spl .inject = f , f-idem , q
```

For this to be a splitting of $f$, we must show that $f \circ f = f$ as
a map $(A, e) \to (A, e)$, which we have by assumption; And we must show
that $f \circ f = \mathrm{id}$ as a map $(A, f) \to (A, f)$. But recall
that the identity on $(A, f)$ is $f$, so we _also_ have this by
assumption!

```agda
  spl .p∘i = KH≡ {ai = f-idem} {bi = f-idem} f-idem
  spl .i∘p = KH≡ {ai = i} {bi = i} f-idem
```

Hence $\~\ca{C}$ is an idempotent-complete category which admits $C$ as
a full subcategory.
